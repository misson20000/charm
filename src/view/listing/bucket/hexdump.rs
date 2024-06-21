use std::iter;
use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::datapath::DataPathExt;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::model::listing::token::TokenKind;
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
    hd_begin: f32,
    hd_end: f32,

    ascii_begin: f32,
    ascii_end: f32,

    space_width: f32,
    
    node: sync::Arc<structure::Node>,
    node_path: structure::Path,
    node_addr: addr::Address,
    
    line_extent: addr::Extent,
    line_cache: vec::Vec<datapath::ByteRecord>,
    line_data_pending: bool,
    
    toks: Vec<token::HexdumpToken>
}

enum Part<'a> {
    Gap { width: usize, begin: Option<(addr::Address, usize)>, end: (addr::Address, usize) },
    Octet { offset: addr::Address, next_offset: addr::Address, token: &'a token::HexdumpToken },
}

impl HexdumpBucket {
    pub fn new(node: sync::Arc<structure::Node>, node_path: structure::Path, node_addr: addr::Address, line_extent: addr::Extent, tokens: impl Iterator<Item = token::HexdumpToken>) -> Self {
        HexdumpBucket {
            hd_begin: 0.0,
            hd_end: 0.0,
            
            ascii_begin: 0.0,
            ascii_end: 0.0,

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

    fn each_part<T, F: FnMut(usize, Part<'_>) -> Option<T>>(&self, mut cb: F) -> (usize, Option<T>) {
        let gutter_pitch = self.gutter_pitch();
        
        let mut offset = self.line_extent.begin;
        let mut next_gutter = offset + gutter_pitch;

        let mut token_iterator = self.toks.iter();
        let mut next_token = token_iterator.next();

        let mut gap_begin = None;
        let mut gap_width = 0;

        let mut column = 0;
        
        while offset < self.line_extent.end {
            if offset != self.line_extent.begin {
                /* put spaces between bytes */
                gap_width+= 1;
                column+= 1;
            }

            while offset >= next_gutter {
                /* additional gutters */
                gap_width+= 1;
                column+= 1;
                next_gutter+= gutter_pitch;
            }

            let next_offset = std::cmp::min(offset + addr::unit::BYTE, next_gutter);
            
            while next_token.map_or(false, |t| t.extent.end <= offset) {
                next_token = token_iterator.next();
            }

            if let Some(token) = next_token {
                if token.extent.includes(offset) {
                    if gap_width > 0 {
                        /* If there was a gap between this token and the last one, emit a Gap part */
                        if let Some(x) = cb(column - gap_width, Part::Gap { width: gap_width, begin: gap_begin, end: (offset, token.index) }) {
                            /* Callback requested early exit */
                            return (column, Some(x));
                        }
                    }

                    /* Emit an octet */
                    if let Some(x) = cb(column, Part::Octet { offset, next_offset, token }) {
                        /* Callback requested early exit */
                        return (column, Some(x));
                    }

                    column+= 2;
                    gap_width = 0;
                    gap_begin = Some((offset, token.index));
                } else {
                    /* No token included this octet. */
                    column+= 2;
                    gap_width+= 2;
                }
            } else {
                /* Out of tokens. */
                break;
            }

            offset = next_offset;
        }

        (column, None)
    }
}

impl bucket::Bucket for HexdumpBucket {
    fn visible_address(&self) -> Option<addr::Address> {
        Some(self.node_addr + self.line_extent.begin.to_size())
    }

    fn render(&mut self, ctx: bucket::RenderArgs<'_>, layout: &mut LayoutController) {
        let lh = helpers::pango_unscale(ctx.render.metrics.height());
        let selection = ctx.selection.node_intersection(&self.node, &self.node_path, self.node_addr);

        let (_, logical_space) = ctx.render.gsc_mono.get(gsc::Entry::Space).unwrap().clone().extents(&ctx.render.font_mono);
        
        let space_width = helpers::pango_unscale(logical_space.width());
        let space_height = helpers::pango_unscale(logical_space.height());
        let space_x = helpers::pango_unscale(logical_space.x());
        let space_y = helpers::pango_unscale(logical_space.y());
        
        self.space_width = space_width;
        
        /* Hexdump */
        layout.allocate(std::marker::PhantomData::<bucket::HexdumpMarker>, |mut x| {
            self.hd_begin = x;

            let hex_cursor = match &ctx.cursor.cursor.class {
                CursorClass::Hexdump(hxc) => Some(hxc),
                _ => None,
            };

            let column = self.each_part(|column, part| { match part {
                Part::Gap { width, begin, end } => if begin.map_or(false, |(o, _)| selection.includes(o)) && selection.includes(end.0) {
                    /* Draw gaps that are selected. */
                    ctx.snapshot.append_color(&ctx.render.config.selection_color, &graphene::Rect::new(
                        x + space_x + space_width * column as f32,
                        lh + space_y,
                        space_width * width as f32,
                        space_height - 1.0));
                },
                
                Part::Octet { offset, next_offset: _, token: _ } => {
                    // TODO: deal with bit-sized gutter pitches
                    let byte_record = self.line_cache.get((offset - self.line_extent.begin).bytes as usize).copied().unwrap_or_default();
                    let pending = byte_record.pending || !byte_record.loaded;
                    let selected = selection.includes(offset);
                    
                    let mut octet_point = graphene::Point::new(x + space_width * column as f32, lh);
                    
                    /* render nybbles */
                    for low_nybble in [false, true] {
                        let nybble = if low_nybble { byte_record.value & 0xf } else { byte_record.value >> 4 };
                        let has_cursor = hex_cursor.map_or(false, |hxc| sync::Arc::ptr_eq(&hxc.token.common.node, &self.node) && hxc.extent.begin + hxc.offset == offset && hxc.low_nybble == low_nybble);
                        
                        let digit = if pending { gsc::Entry::Space } else { gsc::Entry::Digit(nybble) };

                        ctx.render.gsc_mono.begin(digit, &ctx.render.config.text_color, &mut octet_point)
                            .selected(selected, &ctx.render.config.selection_color)
                            .cursor(has_cursor, ctx.cursor, &ctx.render.config.cursor_fg_color, &ctx.render.config.cursor_bg_color)
                            .placeholder(pending, &ctx.render.config.placeholder_color)
                            .render(ctx.snapshot);
                    }
                }
            }; None::<()> }).0;

            x+= space_width * column as f32;
            
            self.hd_end = x;

            x
        });

        /* Asciidump */
        layout.allocate(std::marker::PhantomData::<bucket::AsciidumpMarker>, |mut x| {
            self.ascii_begin = x;
            
            let mut token_iterator = self.toks.iter();
            let mut next_token = token_iterator.next();

            let num_bytes = self.line_extent.round_out().1;
            
            for i in 0..num_bytes {
                while next_token.map_or(false, |t| t.extent.end <= self.line_extent.begin + addr::Size::from(i)) {
                    next_token = token_iterator.next();
                }

                if let Some(token) = next_token {
                    if let Some(byte_extent) = addr::Extent::sized(i.into(), addr::unit::BYTE).rebase(self.line_extent.begin).intersection(self.line_extent) {
                        if token.extent.includes(byte_extent.begin) {
                            let byte_record = self.line_cache.get(i as usize).copied().unwrap_or_default();
                            let selected = selection.includes(byte_extent.begin);
                            let pending = byte_record.pending || !byte_record.loaded;
                    
                            let digit = if pending { gsc::Entry::Space } else { gsc::Entry::PrintableAscii(byte_record.value) };

                            let mut char_point = graphene::Point::new(x + space_width * i as f32, lh);
                        
                            ctx.render.gsc_mono.begin(digit, &ctx.render.config.text_color, &mut char_point)
                                .selected(selected, &ctx.render.config.selection_color)
                                .placeholder(pending, &ctx.render.config.placeholder_color)
                                .render(ctx.snapshot);
                        }
                    }
                } else {
                    break;
                }
            }

            x+= space_width * num_bytes as f32;
            
            self.ascii_end = x;
            
            x
        });
    }
}
    
impl bucket::TokenIterableBucket for HexdumpBucket {
    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        self.toks.iter().map(TokenKind::as_ref)
    }

    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        self.toks.into_iter().map(TokenKind::into_token)
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

impl bucket::PickableBucket for HexdumpBucket {
    fn contains(&self, pick_point: &graphene::Point) -> bool {
        let pick_x = pick_point.x();
        (pick_x >= self.hd_begin && pick_x < self.hd_end) ||
            (pick_x >= self.ascii_begin && pick_x < self.ascii_end)
    }
    
    fn pick(&self, pick_point: &graphene::Point) -> Option<listing::pick::Triplet> {
        let pick_x = pick_point.x();
        
        if pick_x < self.hd_begin {
            None
        } else if pick_x < self.hd_end {
            let pick_column   = ((pick_x - self.hd_begin) / self.space_width).trunc() as usize;
            let pick_fraction = (pick_x - self.hd_begin) / self.space_width;

            self.each_part(|column, part| match part {
                Part::Gap { width, begin, end } if pick_column >= column && pick_column < column + width => Some(listing::pick::Triplet {
                    begin: (self.node_path.clone(), listing::pick::Part::Hexdump {
                        index: end.1,
                        offset: end.0,
                        low_nybble: false,
                    }),
                    middle: (self.node_path.clone(), match (pick_fraction < column as f32 + width as f32 / 2.0, begin) {
                        (true, Some(begin)) => listing::pick::Part::Hexdump {
                            index: begin.1,
                            offset: begin.0,
                            low_nybble: true,
                        },
                        _ => listing::pick::Part::Hexdump {
                            index: end.1,
                            offset: end.0,
                            low_nybble: false,
                        }
                    }),
                    end: (self.node_path.clone(), listing::pick::Part::Hexdump {
                        index: end.1,
                        offset: end.0,
                        low_nybble: false,
                    }),
                }),

                Part::Octet { offset, next_offset, token } if pick_column >= column && pick_column < column + 2 => Some(listing::pick::Triplet {
                    begin: (self.node_path.clone(), listing::pick::Part::Hexdump {
                        index: token.index,
                        offset,
                        low_nybble: pick_column > column,
                    }),
                    middle: (self.node_path.clone(), listing::pick::Part::Hexdump {
                        index: token.index,
                        offset,
                        low_nybble: pick_column > column,
                    }),
                    end: (self.node_path.clone(), listing::pick::Part::Hexdump {
                        index: token.index,
                        offset: next_offset,
                        low_nybble: false,
                    }),
                }),

                _ => None,
            }).1
        } else if pick_x < self.ascii_begin {
            Some(listing::pick::Triplet::all3(self.node_path.clone(), listing::pick::Part::Hexdump {
                index: self.node.child_at_offset(self.line_extent.end),
                offset: self.line_extent.end,
                low_nybble: false,
            }))
        } else if pick_x < self.ascii_end {
            // TODO: asciidump picking
            None
        } else {
            Some(listing::pick::Triplet::all3(self.node_path.clone(), listing::pick::Part::Hexdump {
                index: self.node.child_at_offset(self.line_extent.end),
                offset: self.line_extent.end,
                low_nybble: false,
            }))
        }
    }
}
