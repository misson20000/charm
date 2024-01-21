use std::task;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::datapath::DataPathExt;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::cursor;
use crate::model::listing::token;
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
    
    data_cache: vec::Vec<datapath::ByteRecord>,
    data_pending: bool,
    
    logical_bounds: Option<graphene::Rect>,
}

impl TokenView {
    pub fn from(token: token::Token) -> TokenView {
        TokenView {
            token,

            data_cache: vec::Vec::new(),
            data_pending: true,
            
            logical_bounds: None,
        }
    }

    pub fn into_token(self) -> token::Token {
        self.token
    }

    pub fn token(&self) -> token::TokenRef<'_> {
        self.token.as_ref()
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
        cursor.is_over(self.token.as_ref())
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
        
        snapshot.translate(origin);

        let mut pos = graphene::Point::new(0.0, lh);
        
        let has_cursor = cursor.cursor.is_over(self.token.as_ref());
        
        match &self.token {
            token::Token::BlankLine(token) if token.accepts_cursor && has_cursor => {
                render.gsc_mono.begin(gsc::Entry::Punctuation(token::PunctuationKind::Space), &render.config.text_color, &mut pos)
                    .cursor(true, cursor, &render.config.cursor_fg_color, &render.config.cursor_bg_color)
                    .selected(selection.is_total(), &render.config.selection_color)
                    .render(snapshot);
            },
            token::Token::SummaryPunctuation(token) => {
                render.gsc_mono.begin(gsc::Entry::Punctuation(token.kind), &render.config.text_color, &mut pos)
                    .cursor(has_cursor, cursor, &render.config.cursor_fg_color, &render.config.cursor_bg_color)
                    .selected(selection.is_total(), &render.config.selection_color)
                    .render(snapshot);
            },
            token::Token::Title(token) => {
                gsc::begin_text(
                    &render.pango,
                    &render.font_bold,
                    &render.config.text_color,
                    &token.common.node.props.name,
                    &mut pos)
                    .cursor(has_cursor, cursor, &render.config.cursor_fg_color, &render.config.cursor_bg_color)
                    .selected(selection.is_total(), &render.config.selection_color)
                    .render(snapshot);

                render.gsc_bold.begin(
                    gsc::Entry::Colon,
                    &render.config.text_color,
                    &mut pos)
                    .selected(selection.is_total(), &render.config.selection_color)
                    .render(snapshot);
            },
            token::Token::SummaryLabel(token) => {
                gsc::begin_text(
                    &render.pango,
                    &render.font_bold,
                    &render.config.text_color,
                    &token.common.node.props.name,
                    &mut pos)
                    .cursor(has_cursor, cursor, &render.config.cursor_fg_color, &render.config.cursor_bg_color)
                    .selected(selection.is_total(), &render.config.selection_color)
                    .render(snapshot);
                
                render.gsc_bold.begin(
                    gsc::Entry::Colon,
                    &render.config.text_color,
                    &mut pos)
                    .selected(selection.is_total(), &render.config.selection_color)
                    .render(snapshot);
            },
            token::Token::Hexdump(_) => {
                // TODO: enforce this with type system?
                panic!("hexdump tokens should not be rendered via this codepath");
            },
            token::Token::Hexstring(token) => {
                for i in 0..token.extent.length().bytes {
                    let j = i as u8;
                    let byte_extent = addr::Extent::sized(i.into(), addr::unit::BYTE).intersection(token.extent);
                    let selected = byte_extent.map_or(false, |be| selection.includes(be.begin));
                    
                    render.gsc_mono.begin_iter([
                        gsc::Entry::Digit((j & 0xf0) >> 4),
                        gsc::Entry::Digit( j & 0x0f      ),
                    ].into_iter(), &render.config.text_color, &mut pos)
                        .selected(selected, &render.config.selection_color)
                    // TODO: cursor for hexstring
                        .render(snapshot);
                }
            },

            /* Internal tokens that shouldn't be drawn. */
            token::Token::BlankLine(_) => {},
            token::Token::SummaryPreamble(_) => {},
            token::Token::SummaryEpilogue(_) => {},
        }
        
        self.logical_bounds = Some(graphene::Rect::new(origin.x(), origin.y() + helpers::pango_unscale(render.metrics.descent()), pos.x(), pos.y()));

        graphene::Point::new(origin.x() + pos.x(), 0.0)
    }

    pub fn pick_position(&self, _x: f32, _y: f32) -> Option<(structure::Path, addr::Address, usize)> {
        // TODO
        None
    }
    
    pub fn invalidate_data(&mut self) {
        self.data_cache.clear();
        self.data_pending = true;
    }
    
    pub fn work(&mut self, document: &document::Document, cx: &mut task::Context) -> bool {
        if self.data_pending {
            let (begin_byte, size) = self.token.absolute_extent().round_out();
            
            self.data_cache.resize(size as usize, datapath::ByteRecord::default());
            document.datapath.fetch(datapath::ByteRecordRange::new(begin_byte, &mut self.data_cache), cx);
            
            self.data_pending = self.data_cache.iter().any(|b| b.pending);

            true
        } else {
            false
        }
    }
}