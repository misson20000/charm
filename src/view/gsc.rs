//! GlyphString cache

const DIGIT_STRINGS: [&str; 16] = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"];

use crate::model::listing::token;
use crate::view::helpers;
use crate::view::listing::facet::cursor::CursorView;

use gtk::gdk;
use gtk::graphene;
use gtk::gsk;
use gtk::pango;
use gtk::prelude::*;

#[derive(Clone, Copy, Debug)]
pub enum Entry {
    Punctuation(token::PunctuationClass),
    Digit(u8),
    PrintableAscii(u8),
    Dot,
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
    gs_ascii: [pango::GlyphString; 0x7f-0x20], // ' ', '!', '"', ..., 'y', 'z', '{', '|', '}', '~'
    gs_dot: pango::GlyphString, // "."
    gs_colon: pango::GlyphString, // ": "

    space_width: i32,
}

struct CursorConfig<'a> {
    cursor: &'a CursorView,
    cursor_fg_color: &'a gdk::RGBA,
    cursor_bg_color: &'a gdk::RGBA,
}

#[must_use]
pub struct TextBuilder<'a, I: Iterator<Item = pango::GlyphString>> {
    iterator: I,
    pos: &'a mut graphene::Point,
    config: TextConfig<'a>,
}

struct TextConfig<'a> {
    font: &'a pango::Font,
    color: &'a gdk::RGBA,
    cursor: Option<CursorConfig<'a>>,
}

impl Cache {
    pub fn new(pg: &pango::Context, font: &pango::Font) -> Cache {
        pg.set_font_description(Some(&font.describe()));

        let gs_space = Self::shape(pg, " ");
        let space_width = gs_space.width();
        
        Cache {
            font: font.clone(),
            
            gs_space,
            gs_comma: Self::shape(pg, ", "),
            gs_open: Self::shape(pg, "{"),
            gs_close: Self::shape(pg, "}"),
            gs_digit: DIGIT_STRINGS.map(|d| Self::shape(pg, d)),
            gs_ascii: std::array::from_fn(|i| Self::shape(pg, std::str::from_utf8(&[0x20 + i as u8]).unwrap())),
            gs_dot: Self::shape(pg, "."),
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
        pango::shape(text, items[0].analysis(), &mut gs);

        gs
    }

    pub fn get(&self, entry: Entry) -> Option<&pango::GlyphString> {
        match entry {
            Entry::Punctuation(punct) => match punct {
                token::PunctuationClass::Empty => None,
                token::PunctuationClass::Space => Some(&self.gs_space),
                token::PunctuationClass::Comma => Some(&self.gs_comma),
                token::PunctuationClass::OpenBracket => Some(&self.gs_open),
                token::PunctuationClass::CloseBracket => Some(&self.gs_close),
            },
            Entry::Digit(digit) => self.gs_digit.get(digit as usize),
            Entry::PrintableAscii(ord) if (0x20..0x7f).contains(&ord) => Some(&self.gs_ascii[ord as usize - 0x20]),
            Entry::PrintableAscii(_) => Some(&self.gs_dot),
            Entry::Dot => Some(&self.gs_dot),
            Entry::Colon => Some(&self.gs_colon),
            Entry::Space => Some(&self.gs_space),
        }
    }

    pub fn begin<'a>(&'a self, entry: Entry, color: &'a gdk::RGBA, pos: &'a mut graphene::Point) -> TextBuilder<'a, impl Iterator<Item = pango::GlyphString>> {
        TextBuilder {
            iterator: self.get(entry).cloned().into_iter(),
            pos,
            config: TextConfig {
                font: &self.font,
                color,
                cursor: None,
            },
        }
    }

    pub fn begin_iter<'a, I: Iterator<Item = Entry> + 'a>(&'a self, entries: I, color: &'a gdk::RGBA, pos: &'a mut graphene::Point) -> TextBuilder<'a, impl Iterator<Item = pango::GlyphString> + 'a> {
        TextBuilder {
            iterator: entries.map(|e| self.get(e).cloned()).flatten(),
            pos,
            config: TextConfig {
                font: &self.font,
                color,
                cursor: None,
            },
        }
    }
}

pub fn begin_text<'a>(pg: &'a pango::Context, font: &'a pango::Font, color: &'a gdk::RGBA, text: &'a str, pos: &'a mut graphene::Point) -> TextBuilder<'a, impl std::iter::DoubleEndedIterator<Item = pango::GlyphString> + 'a> {
    let items = pango::itemize(pg, text, 0, text.len() as i32, &pango::AttrList::new(), None);

    TextBuilder {
        iterator: items.into_iter().map(|item| {
            let mut gs = pango::GlyphString::new();
            pango::shape(text, item.analysis(), &mut gs);
            gs
        }),
        pos,
        config: TextConfig {
            font,
            color,
            cursor: None,
        },
    }
}

impl<'a, I: Iterator<Item = pango::GlyphString>> TextBuilder<'a, I> {
    pub fn cursor(mut self, enable: bool, cursor: &'a CursorView, cursor_fg_color: &'a gdk::RGBA, cursor_bg_color: &'a gdk::RGBA) -> Self {
        if enable {
            self.config.cursor = Some(CursorConfig {
                cursor,
                cursor_fg_color,
                cursor_bg_color,
            });
        }
        self
    }
    
    pub fn render(mut self, snapshot: &gtk::Snapshot) {
        for gs in self.iterator {
            self.config.render_gs(self.pos, snapshot, &gs);

            let advance = helpers::pango_unscale(gs.width());
            self.pos.set_x(self.pos.x() + advance);
        }
    }
}

impl<'a, I: std::iter::DoubleEndedIterator<Item = pango::GlyphString>> TextBuilder<'a, I> {
    pub fn render_right_aligned(mut self, snapshot: &gtk::Snapshot) {
        for gs in self.iterator.rev() {
            let advance = helpers::pango_unscale(gs.width());
            self.pos.set_x(self.pos.x() - advance);

            self.config.render_gs(self.pos, snapshot, &gs);
        }
    }
}

impl<'a> TextConfig<'a> {
    fn render_gs(&mut self, pos: &mut graphene::Point, snapshot: &gtk::Snapshot, gs: &pango::GlyphString) {
        let (_ink, logical) = gs.clone().extents(self.font);
        
        if let Some(ccfg) = &self.cursor {
            if ccfg.cursor.has_focus && ccfg.cursor.get_blink() {
                /* Draw the cursor background */
                snapshot.append_color(&ccfg.cursor_bg_color, &graphene::Rect::new(
                    pos.x() + helpers::pango_unscale(logical.x()) + ccfg.cursor.get_bonk(),
                    pos.y() + helpers::pango_unscale(logical.y()),
                    helpers::pango_unscale(logical.width()),
                    helpers::pango_unscale(logical.height())));

                /* Override foreground color */
                self.color = ccfg.cursor_fg_color;
            } else if !ccfg.cursor.has_focus {
                /* Draw the cursor frame */
                snapshot.append_border(
                    &gsk::RoundedRect::new(
                        graphene::Rect::new(
                            pos.x() + helpers::pango_unscale(logical.x()) + ccfg.cursor.get_bonk(),
                            pos.y() + helpers::pango_unscale(logical.y()),
                            helpers::pango_unscale(logical.width()),
                            helpers::pango_unscale(logical.height())),
                        graphene::Size::zero(),
                        graphene::Size::zero(),
                        graphene::Size::zero(),
                        graphene::Size::zero()),
                    &[1.0; 4],
                    &[ccfg.cursor_bg_color.clone(); 4],
                );
            }
        }

        if let Some(tn) = gsk::TextNode::new(
            self.font,
            gs,
            self.color,
            pos
        ) {
            snapshot.append_node(tn);
        }
    }
}
