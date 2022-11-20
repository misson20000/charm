use std::iter;
use std::sync;
use std::task;
use std::vec;

use crate::model::document;
use crate::model::listing::layout;
use crate::model::listing::token;
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
    render_node: Option<gsk::RenderNode>,
}

impl layout::Line for Line {
    type TokenIterator = iter::Map<vec::IntoIter<token_view::TokenView>, fn(token_view::TokenView) -> token::Token>;
    
    fn from_tokens(tokens: vec::Vec<token::Token>) -> Self {
        Line {
            ev_draw: facet::Event::new(),
            ev_work: facet::Event::new_wanted(),

            current_document: None,
            
            tokens: tokens.into_iter().map(|t| token_view::TokenView::from(t)).collect(),
            render_serial: 0,
            render_node: None,
        }
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.tokens.into_iter().map(|t| t.to_token())
    }
}

impl Line {
    pub fn invalidate_render_node(&mut self) {
        self.render_node = None;
    }

    pub fn render(&mut self, cursor: &facet::cursor::CursorView, render: &listing::RenderDetail) -> Option<gsk::RenderNode> {
        /* check if the cursor is on any of the tokens on this line */
        let has_cursor = self.tokens.iter().any(|t| t.contains_cursor(&cursor.cursor));
        
        /* if we rendered this line earlier, the parameters haven't been invalidated, and the cursor isn't on this line, just reuse the previous snapshot. */
        if let Some(rn) = self.render_node.as_ref() {
            /* if the cursor is on the line, we need to redraw it every time the cursor animates, which is hard to tell when that happens so we just redraw it every frame. */
            if self.render_serial == render.serial && !has_cursor {
                return Some(rn.clone());
            }
        }

        let snapshot = gtk::Snapshot::new();
        let mut position = graphene::Point::zero();

        /* begin rendering to the right of the address pane */
        position.set_x(render.addr_pane_width + render.config.padding as f32);

        /* indent by first token */
        if let Some(first) = self.tokens.get(0) {
            position.set_x(
                position.x() +
                render.config.indentation_width *
                    helpers::pango_unscale(render.gsc_mono.space_width()) *
                    first.get_indentation() as f32);
        }

        let mut visible_address = None;

        /* render tokens */
        for token in &mut self.tokens {
            snapshot.save();
            let advance = token.render(&snapshot, cursor, render, &position);
            snapshot.restore();
            position.set_x(position.x() + advance.x());

            /* pick the address from the first token that should display an address */
            if visible_address.is_none() {
                visible_address = token.visible_address();
            }
        }

        /* if any of our tokens wanted to show an address, render the first one into the address pane */
        if let Some(addr) = visible_address {
            let mut pos = graphene::Point::new(render.addr_pane_width - render.config.padding as f32, helpers::pango_unscale(render.metrics.height()));
            gsc::render_text_align_right(&snapshot, &render.pango, &render.font_mono, &render.config.addr_color, &format!("{}", addr), &mut pos);
        }

        if !has_cursor {
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
        let invalidate = self.current_document.as_ref().map_or(true, |current| !sync::Arc::ptr_eq(&current, document));
        let mut updated = false;
        
        for token in &mut self.tokens {
            if invalidate {
                token.invalidate_data();
            }
            
            if token.work(&*document, cx) {
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
