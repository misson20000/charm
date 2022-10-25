const DIGIT_STRINGS: [&'static str; 16] = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"];

use crate::model::listing::token;
use crate::view::helpers;

use gtk::gdk;
use gtk::graphene;
use gtk::gsk;
use gtk::pango;
use gtk::prelude::*;

#[derive(Clone, Copy)]
pub enum Entry {
    Punctuation(token::PunctuationClass),
    Digit(u8),
    Colon,
    Space,
}

pub struct Cache {
    font: pango::Font,
    
    gs_space: pango::GlyphString, // " "
    gs_comma: pango::GlyphString, // ", "
    gs_open: pango::GlyphString, // "{"
    gs_close: pango::GlyphString, // "}"
    gs_digit: [pango::GlyphString; 16], // "0", "1", ..., "f"
    gs_colon: pango::GlyphString, // ": "

    space_width: i32,
}

impl Cache {
    pub fn new(pg: &pango::Context, font: &pango::Font) -> Cache {
        pg.set_font_description(&font.describe().unwrap());

        let mut gs_space = Self::shape(pg, " ");
        let space_width = gs_space.width();
        
        Cache {
            font: font.clone(),
            
            gs_space,
            gs_comma: Self::shape(pg, ", "),
            gs_open: Self::shape(pg, "{"),
            gs_close: Self::shape(pg, "}"),
            gs_digit: DIGIT_STRINGS.map(|d| Self::shape(pg, d)),
            gs_colon: Self::shape(pg, ": "),

            space_width,
        }
    }

    pub fn space_width(&self) -> i32 {
        self.space_width
    }
    
    fn shape(pg: &pango::Context, text: &str) -> pango::GlyphString {
        let items = pango::itemize(pg, text, 0, text.len() as i32, &pango::AttrList::new(), None);
        if items.len() != 1 {
            panic!("itemized '{}' into multiple items?", text);
        }

        let mut gs = pango::GlyphString::new();
        pango::shape(text, &items[0].analysis(), &mut gs);

        gs
    }

    fn get(&self, entry: Entry) -> Option<&pango::GlyphString> {
        match entry {
            Entry::Punctuation(punct) => match punct {
                token::PunctuationClass::Empty => None,
                token::PunctuationClass::Space => Some(&self.gs_space),
                token::PunctuationClass::Comma => Some(&self.gs_comma),
                token::PunctuationClass::OpenBracket => Some(&self.gs_open),
                token::PunctuationClass::CloseBracket => Some(&self.gs_close),
            },
            Entry::Digit(digit) => self.gs_digit.get(digit as usize),
            Entry::Colon => Some(&self.gs_colon),
            Entry::Space => Some(&self.gs_space),
        }
    }

    pub fn print(&self, snapshot: &gtk::Snapshot, entry: Entry, color: &gdk::RGBA, pos: &mut graphene::Point) {
        if let Some(gs) = self.get(entry) {
            let mut gs = gs.clone();
            if let Some(tn) = gsk::TextNode::new(
                &self.font,
                &mut gs,
                color,
                pos) {
                snapshot.append_node(tn);
            }

            let advance = helpers::pango_unscale(gs.width());
            pos.set_x(pos.x() + advance);
        }
    }

    pub fn width(&self, entry: Entry) -> f32 {
        self.get(entry).map_or(0.0, |e| e.clone().width() as f32 / pango::SCALE as f32)
    }
}

pub fn render_text(snapshot: &gtk::Snapshot, pg: &pango::Context, font: &pango::Font, color: &gdk::RGBA, text: &str, pos: &mut graphene::Point) {
    let items = pango::itemize(pg, text, 0, text.len() as i32, &pango::AttrList::new(), None);

    for item in items {
        let mut gs = pango::GlyphString::new();
        pango::shape(text, item.analysis(), &mut gs);
        snapshot.append_node(
            gsk::TextNode::new(
                font,
                &mut gs,
                color,
                pos)
                .unwrap());

        let advance = helpers::pango_unscale(gs.width());
        pos.set_x(pos.x() + advance);
    }
}

pub fn render_text_align_right(snapshot: &gtk::Snapshot, pg: &pango::Context, font: &pango::Font, color: &gdk::RGBA, text: &str, pos: &mut graphene::Point) {
    let items = pango::itemize(pg, text, 0, text.len() as i32, &pango::AttrList::new(), None);

    for item in items {
        let mut gs = pango::GlyphString::new();
        pango::shape(text, item.analysis(), &mut gs);

        let advance = helpers::pango_unscale(gs.width());
        pos.set_x(pos.x() - advance);
        
        snapshot.append_node(
            gsk::TextNode::new(
                font,
                &mut gs,
                color,
                pos)
                .unwrap());
    }
}
