use crate::model::listing::token;
use crate::view::gsc;
use crate::view::listing;

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

    pub fn contains(&self, point: &graphene::Point) -> bool {
        self.logical_bounds.map(|lb| lb.contains_point(point)).unwrap_or(false)
    }

    pub fn render(&mut self, snapshot: &gtk::Snapshot, render: &listing::RenderDetail, origin: &graphene::Point) -> graphene::Point {
        let lh = render.metrics.height() as f32 / pango::SCALE as f32;
        
        snapshot.translate(origin);

        let mut pos = graphene::Point::new(0.0, lh);
        
        match self.token.class {
            token::TokenClass::Punctuation(punct) => {
                render.gsc_mono.print(&snapshot, gsc::Entry::Punctuation(punct), &render.config.text_color, &mut pos);
            },
            token::TokenClass::Title => {
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
                for i in 0..extent.length().bytes {
                    let j = i as u8;
                    render.gsc_mono.print(&snapshot, gsc::Entry::Digit((j & 0xf0) >> 4), &render.config.text_color, &mut pos);
                    render.gsc_mono.print(&snapshot, gsc::Entry::Digit((j & 0x0f) >> 0), &render.config.text_color, &mut pos);

                    if i + 1 < extent.length().bytes {
                        render.gsc_mono.print(&snapshot, gsc::Entry::Space, &render.config.text_color, &mut pos);
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
