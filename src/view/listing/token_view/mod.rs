use std::task;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::datapath::DataPathExt;
use crate::model::document;
use crate::model::listing::cursor;
use crate::model::listing::token;
use crate::view::helpers;
use crate::view::gsc;
use crate::view::listing;
use crate::view::listing::facet::cursor::CursorView;

use gtk::prelude::*;
use gtk::graphene;

mod hexdump;

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

    pub fn to_token(self) -> token::Token {
        self.token
    }

    pub fn get_indentation(&self) -> usize {
        self.token.depth
    }

    pub fn visible_address(&self) -> Option<addr::Address> {
        match self.token.class {
            token::TokenClass::Title => Some(self.token.node_addr),
            token::TokenClass::Hexdump(e) => Some(self.token.node_addr + e.begin.to_size()),
            token::TokenClass::Hexstring(e) => Some(self.token.node_addr + e.begin.to_size()),
            _ => None,
        }
    }

    pub fn contains_cursor(&self, cursor: &cursor::Cursor) -> bool {
        cursor.is_over(&self.token)
    }
    
    pub fn contains(&self, point: &graphene::Point) -> bool {
        self.logical_bounds.map(|lb| lb.contains_point(point)).unwrap_or(false)
    }

    pub fn render(&mut self, snapshot: &gtk::Snapshot, cursor: &CursorView, render: &listing::RenderDetail, origin: &graphene::Point) -> graphene::Point {
        let lh = helpers::pango_unscale(render.metrics.height());
        
        snapshot.translate(origin);

        let mut pos = graphene::Point::new(0.0, lh);
        
        let has_cursor = cursor.cursor.is_over(&self.token);
        
        match self.token.class {
            token::TokenClass::Punctuation(punct) => {
                render.gsc_mono.print(&snapshot, gsc::Entry::Punctuation(punct), &render.config.text_color, &mut pos);
            },
            token::TokenClass::Title => {
                if has_cursor {
                    gsc::render_text_with_cursor(
                        &snapshot,
                        &render.pango,
                        &render.font_bold,
                        &render.config,
                        cursor,
                        &self.token.node.props.name,
                        &mut pos);
                } else {
                    gsc::render_text(
                        &snapshot,
                        &render.pango,
                        &render.font_bold,
                        &render.config.text_color,
                        &self.token.node.props.name,
                        &mut pos);
                }
                render.gsc_bold.print(&snapshot, gsc::Entry::Colon, &render.config.text_color, &mut pos);
            },
            token::TokenClass::SummaryLabel => {
                if has_cursor {
                    gsc::render_text_with_cursor(
                        &snapshot,
                        &render.pango,
                        &render.font_bold,
                        &render.config,
                        cursor,
                        &self.token.node.props.name,
                        &mut pos);
                    render.gsc_bold.print_with_cursor(&snapshot, gsc::Entry::Colon, &render.config, cursor, &mut pos);
                } else {
                    gsc::render_text(
                        &snapshot,
                        &render.pango,
                        &render.font_bold,
                        &render.config.text_color,
                        &self.token.node.props.name,
                        &mut pos);
                    render.gsc_bold.print(&snapshot, gsc::Entry::Colon, &render.config.text_color, &mut pos);
                }
            },
            token::TokenClass::Hexdump(extent) => {
                hexdump::render(self, extent, snapshot, cursor, has_cursor, render, &mut pos);
            },
            token::TokenClass::Hexstring(extent) => {
                for i in 0..extent.length().bytes {
                    let j = i as u8;
                    render.gsc_mono.print(&snapshot, gsc::Entry::Digit((j & 0xf0) >> 4), &render.config.text_color, &mut pos);
                    render.gsc_mono.print(&snapshot, gsc::Entry::Digit((j & 0x0f) >> 0), &render.config.text_color, &mut pos);
                }
            },
        }
        
        self.logical_bounds = Some(graphene::Rect::new(origin.x(), origin.y(), pos.x(), pos.y()));

        pos
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
