use std::iter;
use std::sync;
use std::task;
use std::vec;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::layout;
use crate::model::listing::token;
use crate::model::selection;
use crate::util;
use crate::view::gsc;
use crate::view::helpers;
use crate::view::listing;
use crate::view::listing::facet;
use crate::view::listing::token_view;

use gtk::prelude::*;
use gtk::graphene;
use gtk::gsk;

enum LineViewType {
    Empty,
    Blank(token_view::TokenView),
    Title(token_view::TokenView),
    Hexdump {
        title: Option<token_view::TokenView>,
        node: sync::Arc<structure::Node>,
        line_extent: addr::Extent,
        tokens: Vec<token_view::TokenView>,
    },
    Hexstring {
        title: Option<token_view::TokenView>,
        token: token_view::TokenView,
    },
    Summary {
        title: Option<token_view::TokenView>,
        tokens: Vec<token_view::TokenView>,
    },
}

pub struct Line {
    ev_draw: facet::Event,
    ev_work: facet::Event,

    current_document: Option<sync::Arc<document::Document>>,

    ty: LineViewType,
    
    render_serial: u64,
    selection_hash: u64,
    render_node: Option<gsk::RenderNode>,
}

impl layout::LineView for Line {
    type BorrowingTokenIterator<'a> = LineViewBorrowingTokenIterator<'a>;
    type TokenIterator = LineViewTokenIterator;
    
    fn from_line(line: layout::Line) -> Self {
        Line {
            ev_draw: facet::Event::new(),
            ev_work: facet::Event::new_wanted(),

            current_document: None,

            ty: LineViewType::from(line),
            
            render_serial: 0,
            selection_hash: 0,
            render_node: None,
        }
    }

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.ty.iter_tokens()
    }
    
    fn to_tokens(self) -> Self::TokenIterator {
        self.ty.to_tokens()
    }
}

type LineViewBorrowingTokenIterator<'a> = iter::Map
    <util::PhiIterator
     <&'a token_view::TokenView,
      iter::Empty<&'a token_view::TokenView>,
      iter::Once<&'a token_view::TokenView>,
      iter::Chain<std::option::Iter<'a, token_view::TokenView>, std::slice::Iter<'a, token_view::TokenView>>,
      iter::Chain<std::option::Iter<'a, token_view::TokenView>, iter::Once<&'a token_view::TokenView>>>,
     fn(&'a token_view::TokenView) -> &'a token::Token>;

type LineViewTokenIterator = iter::Map
    <util::PhiIterator
     <token_view::TokenView,
      iter::Empty<token_view::TokenView>,
      iter::Once<token_view::TokenView>,
      iter::Chain<std::option::IntoIter<token_view::TokenView>, vec::IntoIter<token_view::TokenView>>,
      iter::Chain<std::option::IntoIter<token_view::TokenView>, iter::Once<token_view::TokenView>>>,
     fn(token_view::TokenView) -> token::Token>;

impl LineViewType {
    fn from(line: layout::Line) -> Self {
        match line.ty {
            layout::LineType::Empty => Self::Empty,
            layout::LineType::Blank(tok) => Self::Blank(token_view::TokenView::from(tok)),
            layout::LineType::Title(tok) => Self::Title(token_view::TokenView::from(tok)),
            layout::LineType::Hexdump { title, node, line_extent, tokens } => Self::Hexdump {
                title: title.map(token_view::TokenView::from),
                node,
                line_extent,
                tokens: tokens.into_iter().map(token_view::TokenView::from).collect()
            },
            layout::LineType::Hexstring { title, token } => Self::Hexstring {
                title: title.map(token_view::TokenView::from),
                token: token_view::TokenView::from(token),
            },
            layout::LineType::Summary { title, tokens } => Self::Summary {
                title: title.map(token_view::TokenView::from),
                tokens: tokens.into_iter().map(token_view::TokenView::from).collect()
            },
        }
    }

    fn iter_tokens(&self) -> LineViewBorrowingTokenIterator<'_> {
        match &self {
            Self::Empty => util::PhiIterator::I1(iter::empty()),
            Self::Blank(t) | Self::Title(t) => util::PhiIterator::I2(iter::once(t)),
            Self::Hexdump { title, tokens, .. } => util::PhiIterator::I3(title.iter().chain(tokens.iter())),
            Self::Hexstring { title, token, .. } => util::PhiIterator::I4(title.iter().chain(iter::once(token))),
            Self::Summary { title, tokens, .. } => util::PhiIterator::I3(title.iter().chain(tokens.iter())),
        }.map(token_view::TokenView::token)
    }

    fn to_tokens(self) -> LineViewTokenIterator {
        match self {
            Self::Empty => util::PhiIterator::I1(iter::empty()),
            Self::Blank(t) | Self::Title(t) => util::PhiIterator::I2(iter::once(t)),
            Self::Hexdump { title, tokens, .. } => util::PhiIterator::I3(title.into_iter().chain(tokens.into_iter())),
            Self::Hexstring { title, token, .. } => util::PhiIterator::I4(title.into_iter().chain(iter::once(token))),
            Self::Summary { title, tokens, .. } => util::PhiIterator::I3(title.into_iter().chain(tokens.into_iter())),
        }.map(token_view::TokenView::into_token)
    }

    fn contains_cursor(&self, cursor: &cursor::Cursor) -> bool {
        match self {
            Self::Empty => false,
            Self::Blank(t) | Self::Title(t) => cursor.is_over(t.token()),
            Self::Hexdump { title, tokens, .. } => title.map_or(false, |tv| cursor.is_over(tv.token())) || tokens.iter().any(|tv| cursor.is_over(tv.token())),
            Self::Hexstring { title, token, .. } => title.map_or(false, |tv| cursor.is_over(tv.token())) || cursor.is_over(token.token()),
            Self::Summary { title, tokens, .. } => title.map_or(false, |tv| cursor.is_over(tv.token())) || tokens.iter().any(|tv| cursor.is_over(tv.token())),
        }
    }

    fn indentation(&self) -> usize {
        match self {
            Self::Empty => 0,
            Self::Blank(t) | Self::Title(t) => t.token().depth,
            
            Self::Hexdump { title: Some(t), .. } => t.token().depth,
            Self::Hexstring { title: Some(t), .. } => t.token().depth,
            Self::Summary { title: Some(t), .. } => t.token().depth,

            Self::Hexdump { tokens, .. } => tokens[0].token().depth,
            Self::Hexstring { token, .. } => token.token().depth,
            Self::SUmmary { tokens, .. } => tokens[0].token().depth,
        }
    }
}

impl Line {
    pub fn invalidate_render_node(&mut self) {
        self.render_node = None;
    }

    pub fn render(&mut self, cursor: &facet::cursor::CursorView, selection: &selection::listing::Mode, render: &listing::RenderDetail) -> Option<gsk::RenderNode> {
        /* check if the cursor is on any of the tokens on this line */
        let has_cursor = self.ty.contains_cursor(&cursor.cursor);

        let selection_hash = {
            let mut state = std::collections::hash_map::DefaultHasher::default();
            std::hash::Hash::hash(selection, &mut state);
            std::hash::Hasher::finish(&state)
        };
        
        /* if we rendered this line earlier, the parameters haven't been invalidated, and the cursor isn't on this line, just reuse the previous snapshot. */
        if let Some(rn) = self.render_node.as_ref() {
            /* if the cursor is on the line, we need to redraw it every time the cursor animates, which is hard to tell when that happens so we just redraw it every frame. */
            if self.render_serial == render.serial && self.selection_hash == selection_hash && !has_cursor {
                return Some(rn.clone());
            }
        }

        let snapshot = gtk::Snapshot::new();
        let mut main_position = graphene::Point::zero();
        let mut ascii_position = graphene::Point::zero();

        /* begin rendering main content to the right of the address pane */
        main_position.set_x(render.addr_pane_width + render.config.padding as f32);

        /* begin rendering asciidump content wherever configured */
        ascii_position.set_x(render.ascii_pane_position + render.config.padding as f32);

        /* indent  */
        main_position.set_x(
            main_position.x() +
                self.ty.indentation() as f32 *
                render.config.indentation_width *
                helpers::pango_unscale(render.gsc_mono.space_width()));
        }

        let mut visible_address = None;

        /* render tokens */
        for token in &mut self.tokens {
            let selection_intersection = selection.token_intersection(&token.token());
            
            snapshot.save();
            let main_advance = token.render(&snapshot, cursor, selection_intersection, render, &main_position);
            snapshot.restore();
            snapshot.save();
            let ascii_advance = token.render_asciidump(&snapshot, cursor, selection_intersection, render, &ascii_position);
            snapshot.restore();

            if render.config.show_token_bounds {
                token.render_logical_bounds(&snapshot);
            }
            
            main_position.set_x(main_position.x() + main_advance.x());
            ascii_position.set_x(ascii_position.x() + ascii_advance.x());

            /* pick the address from the first token that should display an address */
            if visible_address.is_none() {
                visible_address = token.visible_address();
            }
        }

        /* if any of our tokens wanted to show an address, render the first one into the address pane */
        if let Some(addr) = visible_address {
            let mut pos = graphene::Point::new(render.addr_pane_width - render.config.padding as f32, helpers::pango_unscale(render.metrics.height()));
            gsc::begin_text(&render.pango, &render.font_mono, &render.config.addr_color, &format!("{}", addr), &mut pos).render_right_aligned(&snapshot);
        }

        if !has_cursor {
            self.selection_hash = selection_hash;
            self.render_serial = render.serial;
            self.render_node = snapshot.to_node();
            self.render_node.clone()
        } else {
            /* don't store render snapshots when the cursor was on one of our
             * tokens, otherwise the appearance of the cursor will linger when
             * it moves off this line. */
            snapshot.to_node()
        }
    }

    /* This is really crummy and will have to change later, but I just want to get something working first. */
    pub fn pick_token(&self, x: f64, _y: f64) -> Option<(&token_view::TokenView, f32)> {
        let token = self.tokens.iter().find(
            |token| token.logical_bounds().map_or(
                false,
                |lb| lb.x() < x as f32))
            .or(self.tokens.first());
        token.and_then(|t| t.logical_bounds().map(|lb| (t, x as f32 - lb.x())))
    }
}

impl facet::Facet for Line {
    fn wants_draw(&mut self) -> &mut facet::Event {
        &mut self.ev_draw
    }

    fn wants_work(&mut self) -> &mut facet::Event {
        &mut self.ev_work
    }

    fn work(&mut self, document: &sync::Arc<document::Document>, cx: &mut task::Context) {
        let invalidate = self.current_document.as_ref().map_or(true, |current| !sync::Arc::ptr_eq(current, document));
        let mut updated = false;
        
        for token in &mut self.tokens {
            if invalidate {
                token.invalidate_data();
            }
            
            if token.work(document, cx) {
                updated = true;
            }
        }

        if updated {
            self.invalidate_render_node();
            self.ev_draw.want();
        }
        
        if invalidate {
            self.current_document = Some(document.clone());
        }
    }
}
