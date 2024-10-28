use std::task;
use std::sync;

use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::model::listing::cursor;
use crate::model::listing::token;
use crate::model::listing::token::AsTokenRef;
use crate::model::listing::token::TokenKind;
use crate::model::selection;
use crate::view::helpers;
use crate::view::gsc;
use crate::view::listing;
use crate::view::listing::facet::cursor::CursorView;

use gtk::prelude::*;
use gtk::graphene;
use gtk::gsk;
use gtk::gdk;

// TODO: after Token-type refactor, make it so this can't represent hexdump tokens.
pub struct TokenView {
    token: token::Token,

    data: Option<datapath::Fetcher>,
    
    logical_bounds: Option<graphene::Rect>,
}

impl TokenView {
    pub fn from(token: token::Token) -> TokenView {
        TokenView {
            token,

            data: None,
            
            logical_bounds: None,
        }
    }

    pub fn into_token(self) -> token::Token {
        self.token
    }

    pub fn token(&self) -> token::TokenRef<'_> {
        self.token.as_token_ref()
    }
    
    pub fn get_indentation(&self) -> usize {
        self.token.common().depth
    }

    pub fn visible_address(&self) -> Option<addr::Address> {
        match &self.token {
            token::Token::Title(token) => Some(token.common.node_addr),
            token::Token::Hexdump(token) => Some(token.common.node_addr + token.line.begin.to_size()),
            token::Token::Hexstring(token) => Some(token.common.node_addr + token.extent.begin.to_size()),
            _ => None,
        }
    }

    pub fn contains_cursor(&self, cursor: &cursor::Cursor) -> bool {
        cursor.is_over(self.token.as_token_ref())
    }
    
    pub fn contains(&self, point: &graphene::Point) -> bool {
        self.logical_bounds.map(|lb| lb.contains_point(point)).unwrap_or(false)
    }

    pub fn logical_bounds(&self) -> Option<&graphene::Rect> {
        self.logical_bounds.as_ref()
    }
    
    pub fn render_logical_bounds(&self, snapshot: &gtk::Snapshot) {
        if let Some(lb) = self.logical_bounds {
            snapshot.append_border(
                &gsk::RoundedRect::new(
                    lb,
                    graphene::Size::zero(),
                    graphene::Size::zero(),
                    graphene::Size::zero(),
                    graphene::Size::zero()),
                &[1.0; 4],
                &[gdk::RGBA::GREEN; 4],
            );
        }
    }

    pub fn render(
        &mut self,
        snapshot: &gtk::Snapshot,
        cursor: &CursorView,
        selection: selection::listing::NodeIntersection,
        render: &listing::RenderDetail,
        origin: &graphene::Point
    ) -> graphene::Point {
        let lh = helpers::pango_unscale(render.metrics.height());

        snapshot.save();
        snapshot.translate(origin);

        let mut pos = graphene::Point::new(0.0, lh);
        
        let has_cursor = cursor.cursor.is_over(self.token.as_token_ref());
        
        match &self.token {
            token::Token::BlankLine(token) if token.accepts_cursor && has_cursor => {
                render.gsc_mono.begin(gsc::Entry::Punctuation(token::PunctuationKind::Space), render.config.text_color.rgba(), &mut pos)
                    .cursor(true, cursor, render.config.cursor_fg_color.rgba(), render.config.cursor_bg_color.rgba())
                    .selected(selection.is_total(), render.config.selection_color.rgba())
                    .render(snapshot);
            },
            token::Token::SummaryPunctuation(token) => {
                let selected = match token.kind {
                    token::PunctuationKind::Comma => selection.includes_child(token.node_child_index()) && selection.includes_child(token.node_child_index()+1),
                    _ => selection.is_total()
                };
                
                render.gsc_mono.begin(gsc::Entry::Punctuation(token.kind), render.config.text_color.rgba(), &mut pos)
                    .cursor(has_cursor, cursor, render.config.cursor_fg_color.rgba(), render.config.cursor_bg_color.rgba())
                    .selected(selected, render.config.selection_color.rgba())
                    .render(snapshot);

                /* need to render space separately so it doesn't draw cursor because that looks bad */
                match token.kind {
                    token::PunctuationKind::Comma => render.gsc_mono.begin(gsc::Entry::Space, render.config.text_color.rgba(), &mut pos)
                        .selected(selected, render.config.selection_color.rgba())
                        .render(snapshot),
                    
                    _ => {}
                }
            },
            token::Token::Title(token) => {
                gsc::begin_text(
                    &render.pango,
                    &render.font_bold,
                    render.config.text_color.rgba(),
                    &token.common.node.props.name,
                    &mut pos)
                    .cursor(has_cursor, cursor, render.config.cursor_fg_color.rgba(), render.config.cursor_bg_color.rgba())
                    .selected(selection.is_total(), render.config.selection_color.rgba())
                    .render(snapshot);

                render.gsc_bold.begin(
                    gsc::Entry::Colon,
                    render.config.text_color.rgba(),
                    &mut pos)
                    .selected(selection.is_total(), render.config.selection_color.rgba())
                    .render(snapshot);
            },
            token::Token::SummaryLabel(token) => {
                gsc::begin_text(
                    &render.pango,
                    &render.font_bold,
                    render.config.text_color.rgba(),
                    &token.common.node.props.name,
                    &mut pos)
                    .cursor(has_cursor, cursor, render.config.cursor_fg_color.rgba(), render.config.cursor_bg_color.rgba())
                    .selected(selection.is_total(), render.config.selection_color.rgba())
                    .render(snapshot);
                
                render.gsc_bold.begin(
                    gsc::Entry::Colon,
                    render.config.text_color.rgba(),
                    &mut pos)
                    .selected(selection.is_total(), render.config.selection_color.rgba())
                    .render(snapshot);
            },
            token::Token::Hexdump(_) => {
                // TODO: enforce this with type system?
                panic!("hexdump tokens should not be rendered via this codepath");
            },
            token::Token::Hexstring(token) => {
                let hexstring_cursor = match &cursor.cursor.class {
                    cursor::CursorClass::Hexstring(hxc) => Some(hxc),
                _ => None,
                };
                
                for i in 0..token.extent.length().bytes {
                    let (byte, flags) = self.data.as_ref().map(|fetcher| fetcher.byte_and_flags(i as usize)).unwrap_or_default();
                    let byte_extent = addr::Extent::sized(i.into(), addr::unit::BYTE).intersection(token.extent);
                    let selected = byte_extent.map_or(false, |be| selection.includes(be.begin));
                    
                    let mut text_color = render.config.text_color.rgba();
                    if flags.intersects(datapath::FetchFlags::HAS_DIRECT_EDIT) {
                        text_color = render.config.edit_color.rgba();
                    }
                    let pending = !flags.intersects(datapath::FetchFlags::HAS_ANY_DATA);

                    for low_nybble in [false, true] {
                        let nybble = if low_nybble { byte & 0xf } else { byte >> 4 };
                        let has_cursor = hexstring_cursor.map_or(false, |hxc| sync::Arc::ptr_eq(&hxc.token.common.node, &self.token.node()) && hxc.offset.bytes == i && hxc.low_nybble == low_nybble);
                        
                        let digit = if pending { gsc::Entry::Space } else { gsc::Entry::Digit(nybble) };
                    
                        render.gsc_mono.begin(digit, text_color, &mut pos)
                            .cursor(has_cursor, cursor, render.config.cursor_fg_color.rgba(), render.config.cursor_bg_color.rgba())
                            .selected(selected, render.config.selection_color.rgba())
                            .placeholder(pending, render.config.placeholder_color.rgba())
                            .render(snapshot);
                    }
                }

                if token.truncated {
                    render.gsc_mono.begin(
                        gsc::Entry::Punctuation(token::PunctuationKind::Ellipsis),
                        render.config.text_color.rgba(),
                        &mut pos)
                        .render(snapshot);
                }
            },

            /* Internal tokens that shouldn't be drawn. */
            token::Token::BlankLine(_) => {},
            token::Token::SummaryPreamble(_) => {},
            token::Token::SummaryEpilogue(_) => {},
        }
        
        self.logical_bounds = Some(graphene::Rect::new(origin.x(), origin.y() + helpers::pango_unscale(render.metrics.descent()), pos.x(), pos.y()));

        snapshot.restore();
        
        graphene::Point::new(origin.x() + pos.x(), 0.0)
    }

    pub fn pick(&self) -> Option<listing::pick::Triplet> {
        match &self.token {
            token::Token::Title(_) => Some(listing::pick::Triplet::all3(self.token.node_path().clone(), listing::pick::Part::Title)),

            /* Hexdump tokens can be picked, but that's done as part of HexdumpBucket picking logic and not done here. */
            
            _ => None
        }
    }
    
    pub fn invalidate_data(&mut self) {
        self.data = None;
    }
    
    pub fn work(&mut self, document: &document::Document, cx: &mut task::Context, did_work: &mut bool, work_needed: &mut bool) {
        let absolute_extent = match &self.token {
            token::Token::Hexdump(t) => t.absolute_extent(),
            token::Token::Hexstring(t) => t.extent.rebase(t.common().node_addr),
            _ => return,
        };
        
        let mut fetcher = match self.data.take() {
            Some(fetcher) => fetcher,
            None => {
                let (begin_byte, size) = absolute_extent.round_out();
                datapath::Fetcher::new(&document.datapath, begin_byte, size as usize)
            }
        };

        *did_work|= fetcher.work(cx);
        *work_needed|= !fetcher.finished();
        
        self.data = Some(fetcher);
    }
}
