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
    
    toks: Vec<token::HexdumpToken>
}

enum Part<'a> {
    Gap { width: usize, begin: Option<addr::Address>, end: addr::Address },
    Octet { offset: addr::Address, next_offset: addr::Address, token: &'a token::HexdumpToken },
}

impl HexdumpBucket {
    pub fn new(node: sync::Arc<structure::Node>, node_path: structure::Path, node_addr: addr::Address, line_extent: addr::Extent, tokens: impl Iterator<Item = token::HexdumpToken>) -> Self {
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
                        if let Some(x) = cb(column - gap_width, Part::Gap { width: gap_width, begin: gap_begin, end: offset }) {
                            return (column, Some(x));
                        }
                    }

                    if let Some(x) = cb(column, Part::Octet { offset, next_offset, token }) {
                        return (column, Some(x));
                    }

                    column+= 2;
                    gap_width = 0;
                    gap_begin = Some(offset);
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
        Some(self.line_extent.begin)
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
        layout.allocate(std::marker::PhantomData::<bucket::HexdumpMarker>, |mut point| {
            self.hd_begin = point.clone();

            point.set_y(lh);
                        
            let hex_cursor = match &ctx.cursor.cursor.class {
                CursorClass::Hexdump(hxc) => Some(hxc),
                _ => None,
            };

            let column = self.each_part(|column, part| { match part {
                Part::Gap { width, begin, end } => if begin.map_or(false, |o| selection.includes(o)) && selection.includes(end) {
                    /* Draw gaps that are selected. */
                    ctx.snapshot.append_color(&ctx.render.config.selection_color, &graphene::Rect::new(
                        point.x() + space_x + space_width * column as f32,
                        point.y() + space_y,
                        space_width * width as f32,
                        space_height - 1.0));
                },
                
                Part::Octet { offset, next_offset: _, token: _ } => {
                    // TODO: deal with bit-sized gutter pitches
                    let byte_record = self.line_cache.get((offset - self.line_extent.begin).bytes as usize).copied().unwrap_or_default();
                    let pending = byte_record.pending || !byte_record.loaded;
                    let selected = selection.includes(offset);
                    
                    let mut octet_point = graphene::Point::new(point.x() + space_width * column as f32, point.y());
                    
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

            point.set_x(point.x() + space_width * column as f32);
            
            self.hd_end = point.clone();

            point
        });

        /* Asciidump */
        layout.allocate(std::marker::PhantomData::<bucket::AsciidumpMarker>, |mut point| {
            self.ascii_begin = point.clone();

            point.set_y(lh);
            
            let mut token_iterator = self.toks.iter();
            let mut next_token = token_iterator.next();

            for i in 0..self.line_extent.round_out().1 {
                while next_token.map_or(false, |t| t.extent.end <= self.line_extent.begin + addr::Size::from(i)) {
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
            let pick_column   = ((pick_point.x() - self.hd_begin.x()) / self.space_width).trunc() as usize;
            let pick_fraction = (pick_point.x() - self.hd_begin.x()) / self.space_width;

            self.each_part(|column, part| match part {
                Part::Gap { width, begin, end } if pick_column >= column && pick_column < column + width => Some(listing::PickResult {
                    begin: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(end)),
                    middle: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(if pick_fraction < column as f32 + width as f32 / 2.0 {
                        begin.unwrap_or(end)
                    } else {
                        end
                    })),
                    end: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(end)),
                }),

                Part::Octet { offset, next_offset, token: _ } if pick_column >= column && pick_column < column + 2 => Some(listing::PickResult {
                    begin: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(offset)),
                    middle: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(offset)),
                    end: (self.node_path.clone(), listing::PickOffsetOrIndex::Offset(next_offset)),
                }),

                _ => None,
            }).1
        } else if pick_point.x() >= self.ascii_begin.x() && pick_point.x() < self.ascii_end.x() {
            // TODO: asciidump picking
            None
        } else {
            None
        }
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
