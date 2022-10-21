use crate::model::addr;
use crate::model::listing::cursor;
use crate::model::listing::token;
use crate::view::gsc;
use crate::view::listing;
use crate::view::listing::facet::cursor::CursorView;

use gtk::graphene;
use gtk::pango;

pub struct TokenView {
    token: token::Token,
    logical_bounds: Option<graphene::Rect>,
}

impl TokenView {
    pub fn from(token: token::Token) -> TokenView {
        TokenView {
            token,
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
        let lh = render.metrics.height() as f32 / pango::SCALE as f32;
        
        snapshot.translate(origin);

        let mut pos = graphene::Point::new(0.0, lh);
        
        let has_cursor = cursor.cursor.is_over(&self.token) && cursor.get_blink();
        
        match self.token.class {
            token::TokenClass::Punctuation(punct) => {
                render.gsc_mono.print(&snapshot, gsc::Entry::Punctuation(punct), &render.config.text_color, &mut pos);
            },
            token::TokenClass::Title => {
                if has_cursor {
                    snapshot.append_color(&render.config.cursor_bg_color, &graphene::Rect::new(0.0, 0.0, 10.0, render.metrics.height() as f32 / pango::SCALE as f32));
                }
                
                gsc::render_text(
                    &snapshot,
                    &render.pango,
                    &render.font_bold,
                    &render.config.text_color,
                    &self.token.node.props.name,
                    &mut pos);
                render.gsc_bold.print(&snapshot, gsc::Entry::Colon, &render.config.text_color, &mut pos);
            },
            token::TokenClass::SummaryLabel => {
                gsc::render_text(
                    &snapshot,
                    &render.pango,
                    &render.font_bold,
                    &render.config.text_color,
                    &self.token.node.props.name,
                    &mut pos);
                render.gsc_bold.print(&snapshot, gsc::Entry::Colon, &render.config.text_color, &mut pos);
            },
            token::TokenClass::Hexdump(extent) => {
                let hex_cursor = match &cursor.cursor.class {
                    cursor::CursorClass::Hexdump(hxc) if has_cursor => Some(hxc),
                    _ => None,
                };
                
                for i in 0..extent.length().bytes {
                    let j = i as u8;

                    /* high nybble */
                    {
                        let digit = gsc::Entry::Digit((j & 0xf0) >> 4);
                        if hex_cursor.map_or(false, |hxc| hxc.offset.bytes == i && !hxc.low_nybble) {
                            snapshot.append_color(&render.config.cursor_bg_color, &graphene::Rect::new(pos.x(), pos.y()-lh, render.gsc_mono.width(digit), lh));
                            render.gsc_mono.print(&snapshot, digit, &render.config.cursor_fg_color, &mut pos);
                        } else {
                            render.gsc_mono.print(&snapshot, digit, &render.config.text_color, &mut pos);
                        }
                    }

                    /* low nybble */
                    {
                        let digit = gsc::Entry::Digit((j & 0x0f) >> 0);
                        if hex_cursor.map_or(false, |hxc| hxc.offset.bytes == i && hxc.low_nybble) {
                            snapshot.append_color(&render.config.cursor_bg_color, &graphene::Rect::new(pos.x(), pos.y()-lh, render.gsc_mono.width(digit), lh));
                            render.gsc_mono.print(&snapshot, digit, &render.config.cursor_fg_color, &mut pos);
                        } else {
                            render.gsc_mono.print(&snapshot, digit, &render.config.text_color, &mut pos);
                        }
                    }
                }
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
}
