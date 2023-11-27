use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::datapath::DataPathExt;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::model::listing::cursor::CursorClass;
use crate::view::gsc;
use crate::view::helpers;
use crate::view::listing;
use crate::view::listing::bucket;
use crate::view::listing::layout::LayoutController;
use crate::view::listing::layout::LayoutProvider;

use gtk::prelude::*;
use gtk::graphene;

pub struct HexdumpBucket {
    hd_begin: graphene::Point,
    hd_end: graphene::Point,

    ascii_begin: graphene::Point,
    ascii_end: graphene::Point,

    space_width: f32,
    
    node: sync::Arc<structure::Node>,
    node_path: structure::Path,
    node_addr: addr::Address,
    
    line_extent: addr::Extent,
    line_cache: vec::Vec<datapath::ByteRecord>,
    line_data_pending: bool,
    
    toks: Vec<token::Token>
}

impl HexdumpBucket {
    pub fn new(node: sync::Arc<structure::Node>, node_path: structure::Path, node_addr: addr::Address, line_extent: addr::Extent, tokens: impl Iterator<Item = token::Token>) -> Self {
        HexdumpBucket {
            hd_begin: graphene::Point::zero(),
            hd_end: graphene::Point::zero(),
            
            ascii_begin: graphene::Point::zero(),
            ascii_end: graphene::Point::zero(),

            space_width: 1.0,
            
            node,
            node_path,
            node_addr,
            
            line_extent,
            line_cache: Vec::new(),
            line_data_pending: true,
            
            toks: tokens.collect(),
        }
    }
    fn gutter_pitch(&self) -> addr::Size {
        addr::Size::from(8)
    }
}

impl bucket::Bucket for HexdumpBucket {
    fn visible_address(&self) -> Option<addr::Address> {
        Some(self.line_extent.begin)
    }

    fn render(&mut self, ctx: bucket::RenderArgs<'_>, layout: &mut LayoutController) {
        let lh = helpers::pango_unscale(ctx.render.metrics.height());
        let selection = ctx.selection.node_intersection(&self.node, &self.node_path, self.node_addr);
        
        /* Hexdump */
        layout.allocate(std::marker::PhantomData::<bucket::HexdumpMarker>, |mut point| {
            self.hd_begin = point.clone();

            point.set_y(lh);
            
            let (_, logical_space) = ctx.render.gsc_mono.get(gsc::Entry::Space).unwrap().clone().extents(&ctx.render.font_mono);
            let space_width = helpers::pango_unscale(logical_space.width());
            self.space_width = space_width;
            
            let gutter_pitch = self.gutter_pitch();
            
            let mut offset = self.line_extent.begin;
            let mut next_gutter = offset + gutter_pitch;

            let mut token_iterator = self.toks.iter();
            let mut next_token = token_iterator.next();

            let hex_cursor = match &ctx.cursor.cursor.class {
                CursorClass::Hexdump(hxc) => Some(hxc),
                _ => None,
            };
            
            while offset < self.line_extent.end {
                let mut gutter_width = None;
                
                if offset != self.line_extent.begin {
                    /* put spaces between bytes */
                    gutter_width = Some(space_width);
                }

                while offset >= next_gutter {
                    /* additional gutters */
                    next_gutter+= gutter_pitch;
                    gutter_width = Some(gutter_width.unwrap_or(0.0) + space_width);
                }

                if let Some(w) = gutter_width {
                    /* if gutter is selected, draw it. */
                    if selection.includes(offset) {
                        ctx.snapshot.append_color(&ctx.render.config.selection_color, &graphene::Rect::new(
                            point.x() + helpers::pango_unscale(logical_space.x()),
                            point.y() + helpers::pango_unscale(logical_space.y()),
                            w as f32,
                            helpers::pango_unscale(logical_space.height()) - 1.0));
                    }

                    point.set_x(point.x() + w);
                }

                let next_offset = std::cmp::min(offset + addr::unit::BYTE, next_gutter);
                let selected = selection.includes(offset);
                
                while next_token.map_or(false, |t| match t.class {
                    token::TokenClass::Hexdump { extent, .. } => extent,
                    _ => panic!("attempted to render non-hexdump token through hexdump bucket")                    
                }.end <= offset) {
                    next_token = token_iterator.next();
                }

                if let Some(token) = next_token {
                    if match token.class {
                        token::TokenClass::Hexdump { extent, .. } => extent,
                        _ => panic!("attempted to render non-hexdump token through hexdump bucket")                    
                    }.includes(offset) {
                        // TODO: deal with bit-sized gutter pitches
                        let byte_record = self.line_cache.get((offset - self.line_extent.begin).bytes as usize).copied().unwrap_or_default();
                        let pending = byte_record.pending || !byte_record.loaded;

                        /* render nybbles */
                        for low_nybble in [false, true] {
                            let nybble = if low_nybble { byte_record.value & 0xf } else { byte_record.value >> 4 };
                            let has_cursor = hex_cursor.map_or(false, |hxc| sync::Arc::ptr_eq(&hxc.token.node, &self.node) && hxc.extent.begin + hxc.offset == offset && hxc.low_nybble == low_nybble);
                            
                            let digit = if pending { gsc::Entry::Space } else { gsc::Entry::Digit(nybble) };

                            ctx.render.gsc_mono.begin(digit, &ctx.render.config.text_color, &mut point)
                                .selected(selected, &ctx.render.config.selection_color)
                                .cursor(has_cursor, ctx.cursor, &ctx.render.config.cursor_fg_color, &ctx.render.config.cursor_bg_color)
                                .placeholder(pending, &ctx.render.config.placeholder_color)
                                .render(ctx.snapshot);
                        }
                    } else {
                        /* No token included this byte. */
                        if selection.includes(offset) {
                            ctx.snapshot.append_color(&ctx.render.config.selection_color, &graphene::Rect::new(
                                point.x() + helpers::pango_unscale(logical_space.x()),
                                point.y() + helpers::pango_unscale(logical_space.y()),
                                2.0 * space_width as f32,
                                helpers::pango_unscale(logical_space.height()) - 1.0));
                        }

                        point.set_x(point.x() + 2.0 * space_width);
                    }
                } else {
                    /* Out of tokens. */
                    break;
                }

                offset = next_offset;
            }

            self.hd_end = point.clone();

            point.set_y(self.hd_begin.y());
            
            point
        });

        /* Asciidump */
        layout.allocate(std::marker::PhantomData::<bucket::AsciidumpMarker>, |mut point| {
            self.ascii_begin = point.clone();

            point.set_y(lh);
            
            let mut token_iterator = self.toks.iter();
            let mut next_token = token_iterator.next();

            for i in 0..self.line_extent.round_out().1 {
                while next_token.map_or(false, |t| match t.class {
                    token::TokenClass::Hexdump { extent, .. } => extent,
                    _ => panic!("attempted to render non-hexdump token through hexdump bucket")                    
                }.end <= self.line_extent.begin + addr::Size::from(i)) {
                    next_token = token_iterator.next();
                }

                if let Some(_token) = next_token {
                    let byte_extent = addr::Extent::sized(i.into(), addr::unit::BYTE).rebase(self.line_extent.begin).intersection(self.line_extent);
                    let byte_record = self.line_cache.get(i as usize).copied().unwrap_or_default();
                    let selected = byte_extent.map_or(false, |be| selection.includes(be.begin));
                    let pending = byte_record.pending || !byte_record.loaded;
                    
                    let digit = if pending { gsc::Entry::Space } else { gsc::Entry::PrintableAscii(byte_record.value) };

                    ctx.render.gsc_mono.begin(digit, &ctx.render.config.text_color, &mut point)
                        .selected(selected, &ctx.render.config.selection_color)
                        .placeholder(pending, &ctx.render.config.placeholder_color)
                        .render(ctx.snapshot);
                } else {
                    break;
                }
            }

            self.ascii_end = point.clone();

            point.set_y(self.hd_begin.y());
            
            point
        });
    }

    fn pick(&self, pick_point: &graphene::Point) -> Option<listing::PickResult> {
        if pick_point.x() >= self.hd_begin.x() && pick_point.x() < self.hd_end.x() {
            let mut point = self.hd_begin.clone();
            
            let space_width = self.space_width;
            let gutter_pitch = self.gutter_pitch();
            
            let mut offset = self.line_extent.begin;
            let mut next_gutter = offset + gutter_pitch;

            let mut token_iterator = self.toks.iter();
            let mut next_token = token_iterator.next();

            let mut last_valid_offset = None;

            enum PickedPart {
                Gutter { right: bool },
                Octet,

                Previous
            }
            
            let mut has_picked = None;
            
            while offset < self.line_extent.end {
                let next_offset = std::cmp::min(offset + addr::unit::BYTE, next_gutter);
                
                let mut gutter_width = None;
                
                if offset != self.line_extent.begin {
                    /* put spaces between bytes */
                    gutter_width = Some(space_width);
                }

                while offset >= next_gutter {
                    /* additional gutters */
                    next_gutter+= gutter_pitch;
                    gutter_width = Some(gutter_width.unwrap_or(0.0) + space_width);
                }

                if let Some(w) = gutter_width {
                    if pick_point.x() >= point.x() && pick_point.x() < point.x() + w {
                        /* gutter is being picked */
                        has_picked = has_picked.or_else(|| Some(PickedPart::Gutter { right: pick_point.x() - point.x() > w/2.0 }));
                    }

                    point.set_x(point.x() + w);
                }

                if pick_point.x() >= point.x() && pick_point.x() < point.x() + 2.0 * space_width {
                    /* octet is being picked, but it may or may not be part of a token. */
                    has_picked = has_picked.or_else(|| Some(PickedPart::Octet));
                }

                while next_token.map_or(false, |t| match t.class {
                    token::TokenClass::Hexdump { extent, .. } => extent,
                    _ => panic!("attempted to render non-hexdump token through hexdump bucket")                    
                }.end <= offset) {
                    next_token = token_iterator.next();
                }

                if let Some(token) = next_token {
                    if match token.class {
                        token::TokenClass::Hexdump { extent, .. } => extent,
                        _ => panic!("attempted to render non-hexdump token through hexdump bucket")                    
                    }.includes(offset) {
                        /* Found token. */
                        if let Some(picked_part) = has_picked {
                            let begin = offset;
                            let middle = match picked_part {
                                PickedPart::Gutter { right: true } | PickedPart::Previous | PickedPart::Octet => offset,
                                PickedPart::Gutter { right: false } => last_valid_offset.unwrap_or(offset),
                            };
                            let end = match picked_part {
                                PickedPart::Gutter { .. } | PickedPart::Previous => offset,
                                PickedPart::Octet => next_offset,
                            };
                            
                            return Some(listing::PickResult {
                                begin: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(begin)),
                                middle: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(middle)),
                                end: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(end)),
                            });
                        }
                        
                        last_valid_offset = Some(offset);
                    } else {
                        /* No token included this byte. Continue. */
                    }
                    point.set_x(point.x() + 2.0 * space_width);
                } else {
                    /* Out of tokens. */
                    break;
                }

                has_picked = has_picked.map(|_| PickedPart::Previous);

                offset = next_offset;
            }

            return last_valid_offset.map(|lvo| listing::PickResult {
                begin: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(lvo)),
                middle: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(lvo)),
                end: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(lvo)),
            });
        } else if pick_point.x() >= self.ascii_begin.x() && pick_point.x() < self.ascii_end.x() {
            // TODO: asciidump picking
            None
        } else {
            None
        }
    }
}
    
impl bucket::TokenIterableBucket for HexdumpBucket {
    type TokenIterator = vec::IntoIter<token::Token>;
    type BorrowingTokenIterator<'a> = std::slice::Iter<'a, token::Token>;

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.toks.iter()
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.toks.into_iter()
    }
}

impl bucket::WorkableBucket for HexdumpBucket {
    fn work(&mut self, document: &sync::Arc<document::Document>, cx: &mut std::task::Context) -> bool {
        if self.line_data_pending {
            let (begin_byte, size) = self.line_extent.rebase(self.node_addr).round_out();
            
            self.line_cache.resize(size as usize, datapath::ByteRecord::default());
            document.datapath.fetch(datapath::ByteRecordRange::new(begin_byte, &mut self.line_cache), cx);
            
            self.line_data_pending = self.line_cache.iter().any(|b| b.pending);

            true
        } else {
            false
        }
    }

    fn invalidate_data(&mut self) {
        self.line_cache.clear();
        self.line_data_pending = true;        
    }
}
