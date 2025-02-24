use std::iter;
use std::sync;

use crate::model::addr;
use crate::model::datapath;
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
    node_addr: addr::AbsoluteAddress,
    
    line_extent: addr::Extent,
    line_data: Option<datapath::Fetcher>,
    
    toks: Vec<token::Hexdump>
}

enum Part<'a> {
    Gap { width: usize, begin: Option<(addr::Offset, usize)>, end: (addr::Offset, usize) },
    Octet { offset: addr::Offset, next_offset: addr::Offset, token: &'a token::Hexdump },
}

impl HexdumpBucket {
    pub fn new(node: sync::Arc<structure::Node>, node_path: structure::Path, node_addr: addr::AbsoluteAddress, line_extent: addr::Extent, tokens: impl Iterator<Item = token::Hexdump>) -> Self {
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
            line_data: None,
            
            toks: tokens.collect(),
        }
    }

    fn end(&self) -> addr::Offset {
        self.toks.last().unwrap().extent.end
    }
    
    fn gutter_pitch(&self) -> addr::Offset {
        addr::Offset::from(8)
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

            let next_offset = std::cmp::min(offset + addr::Offset::BYTE, next_gutter);
            
            while next_token.map_or(false, |t| t.extent.end <= offset) {
                next_token = token_iterator.next();
            }

            if let Some(token) = next_token {
                if token.extent.includes(offset) {
                    if gap_width > 0 {
                        /* If there was a gap between this token and the last one, emit a Gap part */
                        if let Some(x) = cb(column - gap_width, Part::Gap { width: gap_width, begin: gap_begin, end: (offset, token.node_child_index()) }) {
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
                    gap_begin = Some((offset, token.node_child_index()));
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
    fn visible_address(&self) -> Option<addr::AbsoluteAddress> {
        Some(self.node_addr + self.line_extent.begin)
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
                Part::Gap { width, begin, end } => if begin.map_or(false, |(o, i)| selection.includes(o, i)) && selection.includes(end.0, end.1) {
                    /* Draw gaps that are selected. */
                    ctx.snapshot.append_color(ctx.render.config.selection_color.rgba(), &graphene::Rect::new(
                        x + space_x + space_width * column as f32,
                        lh + space_y,
                        space_width * width as f32,
                        space_height - 1.0));
                },
                
                Part::Octet { offset, next_offset: _, token } => {
                    // TODO: deal with bit-sized gutter pitches
                    let (byte, flags) = self.line_data.as_ref().map(|fetcher| fetcher.byte_and_flags((offset - self.line_extent.begin).bytes() as usize)).unwrap_or_default();
                    
                    let mut text_color = ctx.render.config.text_color.rgba();
                    let pending = !flags.intersects(datapath::FetchFlags::HAS_ANY_DATA);
                    let selected = selection.includes(offset, token.common().node_child_index);

                    if flags.intersects(datapath::FetchFlags::HAS_DIRECT_EDIT) {
                        text_color = ctx.render.config.edit_color.rgba();
                    }
                    
                    let mut octet_point = graphene::Point::new(x + space_width * column as f32, lh);
                    
                    /* render nybbles */
                    for low_nybble in [false, true] {
                        let nybble = if low_nybble { byte & 0xf } else { byte >> 4 };
                        let has_cursor = hex_cursor.map_or(false, |hxc| sync::Arc::ptr_eq(&hxc.token.common.node, &self.node) && hxc.extent().begin + hxc.offset == offset && hxc.low_nybble == low_nybble);
                        
                        let digit = if pending { gsc::Entry::Space } else { gsc::Entry::Digit(nybble) };

                        ctx.render.gsc_mono.begin(digit, text_color, &mut octet_point)
                            .selected(selected, ctx.render.config.selection_color.rgba())
                            .cursor(has_cursor, ctx.cursor, ctx.render.config.cursor_fg_color.rgba(), ctx.render.config.cursor_bg_color.rgba())
                            .placeholder(pending, ctx.render.config.placeholder_color.rgba())
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
                while next_token.map_or(false, |t| t.extent.end <= self.line_extent.begin + addr::Offset::from(i)) {
                    next_token = token_iterator.next();
                }

                if let Some(token) = next_token {
                    if let Some(byte_extent) = addr::Extent::sized(i, addr::Offset::BYTE).offset(self.line_extent.begin).intersection(self.line_extent) {
                        if token.extent.includes(byte_extent.begin) {
                            let (byte, flags) = self.line_data.as_ref().map(|fetcher| fetcher.byte_and_flags(i as usize)).unwrap_or_default();

                            let mut text_color = ctx.render.config.text_color.rgba();
                            let pending = !flags.intersects(datapath::FetchFlags::HAS_ANY_DATA);
                            let selected = selection.includes(byte_extent.begin, token.common().node_child_index);

                            if flags.intersects(datapath::FetchFlags::HAS_DIRECT_EDIT) {
                                text_color = ctx.render.config.edit_color.rgba();
                            }

                            let digit = if pending { gsc::Entry::Space } else { gsc::Entry::PrintableAscii(byte) };

                            let mut char_point = graphene::Point::new(x + space_width * i as f32, lh);
                        
                            ctx.render.gsc_mono.begin(digit, text_color, &mut char_point)
                                .selected(selected, ctx.render.config.selection_color.rgba())
                                .placeholder(pending, ctx.render.config.placeholder_color.rgba())
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
        self.toks.iter().map(token::AsTokenRef::as_token_ref)
    }

    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        self.toks.into_iter().map(Into::into)
    }
}

impl bucket::WorkableBucket for HexdumpBucket {
    fn work(&mut self, document: &sync::Arc<document::Document>, cx: &mut std::task::Context, did_work: &mut bool, work_needed: &mut bool) {
        let mut fetcher = match self.line_data.take() {
            Some(fetcher) => fetcher,
            None => {
                let (begin_byte, size) = self.line_extent.absolute_from(self.node_addr).round_out();
                datapath::Fetcher::new(document.datapath.clone(), begin_byte, size as usize)
            }
        };

        *did_work|= fetcher.work(cx);
        *work_needed|= !fetcher.finished();
        
        self.line_data = Some(fetcher);
    }

    fn invalidate_data(&mut self) {
        self.line_data = None;
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
                        index: token.node_child_index(),
                        offset,
                        low_nybble: pick_column > column,
                    }),
                    middle: (self.node_path.clone(), listing::pick::Part::Hexdump {
                        index: token.node_child_index(),
                        offset,
                        low_nybble: pick_column > column,
                    }),
                    end: (self.node_path.clone(), listing::pick::Part::Hexdump {
                        index: token.node_child_index(),
                        offset: next_offset,
                        low_nybble: false,
                    }),
                }),

                _ => None,
            }).1
        } else if pick_x < self.ascii_begin {
            let last_tok = self.toks.last().unwrap();
            
            Some(listing::pick::Triplet::all3(self.node_path.clone(), listing::pick::Part::Hexdump {
                index: last_tok.node_child_index(),
                offset: last_tok.extent.end,
                low_nybble: false,
            }))
        } else if pick_x < self.ascii_end {
            // TODO: asciidump picking
            None
        } else {
            let last_tok = self.toks.last().unwrap();
            
            Some(listing::pick::Triplet::all3(self.node_path.clone(), listing::pick::Part::Hexdump {
                index: last_tok.node_child_index(),
                offset: last_tok.extent.end,
                low_nybble: false,
            }))
        }
    }
}
