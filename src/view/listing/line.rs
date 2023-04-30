use std::iter;
use std::sync;
use std::task;
use std::vec;

use crate::model::document;
use crate::model::listing::layout;
use crate::model::listing::token;
use crate::model::selection;
use crate::view::gsc;
use crate::view::helpers;
use crate::view::listing;
use crate::view::listing::facet;
use crate::view::listing::token_view;

use gtk::prelude::*;
use gtk::graphene;
use gtk::gsk;

pub struct Line {
    ev_draw: facet::Event,
    ev_work: facet::Event,

    current_document: Option<sync::Arc<document::Document>>,
    
    tokens: vec::Vec<token_view::TokenView>,
    render_serial: u64,
    selection_hash: u64,
    render_node: Option<gsk::RenderNode>,
}

impl layout::Line for Line {
    type TokenIterator = iter::Map<vec::IntoIter<token_view::TokenView>, fn(token_view::TokenView) -> token::Token>;
    type BorrowingTokenIterator<'a> = iter::Map<std::slice::Iter<'a, token_view::TokenView>, fn(&'a token_view::TokenView) -> &'a token::Token>;
    
    fn from_tokens(tokens: vec::Vec<token::Token>) -> Self {
        Line {
            ev_draw: facet::Event::new(),
            ev_work: facet::Event::new_wanted(),

            current_document: None,
            
            tokens: tokens.into_iter().map(token_view::TokenView::from).collect(),
            render_serial: 0,
            selection_hash: 0,
            render_node: None,
        }
    }

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.tokens.iter().map(|t| t.token())
    }
    
    fn to_tokens(self) -> Self::TokenIterator {
        self.tokens.into_iter().map(|t| t.into_token())
    }
}

impl Line {
    pub fn invalidate_render_node(&mut self) {
        self.render_node = None;
    }

    pub fn render(&mut self, cursor: &facet::cursor::CursorView, selection: &selection::listing::Mode, render: &listing::RenderDetail) -> Option<gsk::RenderNode> {
        /* check if the cursor is on any of the tokens on this line */
        let has_cursor = self.tokens.iter().any(|t| t.contains_cursor(&cursor.cursor));

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

        /* indent by first token */
        if let Some(first) = self.tokens.get(0) {
            main_position.set_x(
                main_position.x() +
                render.config.indentation_width *
                    helpers::pango_unscale(render.gsc_mono.space_width()) *
                    first.get_indentation() as f32);
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
