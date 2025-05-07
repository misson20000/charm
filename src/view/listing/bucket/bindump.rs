use std::iter;
use std::sync;

use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::model::listing::token::TokenKind;
use crate::view::gsc;
use crate::view::helpers;
use crate::view::listing;
use crate::view::listing::bucket;
use crate::view::listing::cursor::CursorClass;
use crate::view::listing::cursor::CursorClassExt;
use crate::view::listing::layout::LayoutController;
use crate::view::listing::layout::LayoutProvider;

use gtk::prelude::*;
use gtk::graphene;

pub struct BindumpBucket {
    bd_begin: f32,
    bd_end: f32,

    space_width: f32,
    
    node: sync::Arc<structure::Node>,
    node_path: structure::Path,
    node_addr: addr::AbsoluteAddress,
    
    line_extent: addr::Extent,
    line_data: Option<datapath::Fetcher>,
    
    toks: Vec<token::Bindump>
}

enum Part<'a> {
    Gap { width: usize, begin: Option<(addr::Offset, usize)>, end: (addr::Offset, usize) },
    Octet { offset: addr::Offset, next_offset: addr::Offset, num_bits: u8, token: &'a token::Bindump },
}

impl BindumpBucket {
    pub fn new(node: sync::Arc<structure::Node>, node_path: structure::Path, node_addr: addr::AbsoluteAddress, line_extent: addr::Extent, tokens: impl Iterator<Item = token::Bindump>) -> Self {
        BindumpBucket {
            bd_begin: 0.0,
            bd_end: 0.0,

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
    
    fn word_pitch(&self) -> addr::Offset {
        addr::Offset::from(4)
    }

    fn each_part<T, F: FnMut(usize, Part<'_>) -> Option<T>>(&self, mut cb: F) -> (usize, Option<T>) {
        let word_pitch = self.word_pitch();
        
        let mut offset = self.line_extent.begin;
        let mut next_word = offset + word_pitch;

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

            while offset >= next_word {
                /* additional words */
                gap_width+= 1;
                column+= 1;
                next_word+= word_pitch;
            }

            let next_offset = std::cmp::min(offset + addr::Offset::BYTE, next_word);
            
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
                    let mut num_bits = 8;
                    if offset.bytes() == token.extent.end.bytes() {
                        num_bits = token.extent.end.bits();
                    }
                    
                    if let Some(x) = cb(column, Part::Octet { offset, next_offset, num_bits, token }) {
                        /* Callback requested early exit */
                        return (column, Some(x));
                    }

                    column+= num_bits as usize;
                    gap_width = 0;
                    gap_begin = Some((offset, token.node_child_index()));
                } else {
                    /* No token included this octet. */
                    column+= 8;
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

impl bucket::Bucket for BindumpBucket {
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
        
        /* Bindump */
        layout.allocate(std::marker::PhantomData::<bucket::BindumpMarker>, |mut x| {
            self.bd_begin = x;

            let bin_cursor = match &ctx.cursor.cursor.class {
                CursorClass::Bindump(bdc) => Some(bdc),
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
                
                Part::Octet { offset, next_offset: _, num_bits, token } => {
                    // TODO: deal with bit-sized word pitches
                    let (byte, flags) = self.line_data.as_ref().map(|fetcher| fetcher.byte_and_flags((offset - self.line_extent.begin).bytes() as usize)).unwrap_or_default();
                    
                    let mut text_color = ctx.render.config.text_color.rgba();
                    let pending = !flags.intersects(datapath::FetchFlags::HAS_ANY_DATA);

                    if flags.intersects(datapath::FetchFlags::HAS_DIRECT_EDIT) {
                        text_color = ctx.render.config.edit_color.rgba();
                    }
                    
                    let mut octet_point = graphene::Point::new(x + space_width * column as f32, lh);
                    
                    /* render bits */
                    for bit_index in (0..num_bits).rev() {
                        let bit = (byte >> bit_index) & 1;
                        let selected = selection.includes(offset + addr::Offset::new(0, bit_index), token.common().node_child_index);
                        let has_cursor = bin_cursor.map_or(false, |bdc| sync::Arc::ptr_eq(&bdc.token.common.node, &self.node) && bdc.extent().begin + bdc.get_offset() == offset + addr::Offset::new(0, bit_index));
                        
                        let digit = if pending { gsc::Entry::Space } else { gsc::Entry::Digit(bit) };

                        ctx.render.gsc_mono.begin(digit, text_color, &mut octet_point)
                            .selected(selected, ctx.render.config.selection_color.rgba())
                            .cursor(has_cursor, ctx.cursor, ctx.render.config.cursor_fg_color.rgba(), ctx.render.config.cursor_bg_color.rgba())
                            .placeholder(pending, ctx.render.config.placeholder_color.rgba())
                            .render(ctx.snapshot);
                    }
                }
            }; None::<()> }).0;

            x+= space_width * column as f32;
            
            self.bd_end = x;

            x
        });
    }
}
    
impl bucket::TokenIterableBucket for BindumpBucket {
    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        self.toks.iter().map(token::AsTokenRef::as_token_ref)
    }

    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        self.toks.into_iter().map(Into::into)
    }
}

impl bucket::WorkableBucket for BindumpBucket {
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

impl bucket::PickableBucket for BindumpBucket {
    fn contains(&self, pick_point: &graphene::Point) -> bool {
        let pick_x = pick_point.x();
        pick_x >= self.bd_begin && pick_x < self.bd_end
    }
    
    fn pick(&self, pick_point: &graphene::Point) -> Option<listing::pick::Triplet> {
        let pick_x = pick_point.x();
        
        if pick_x < self.bd_begin {
            None
        } else if pick_x < self.bd_end {
            let pick_column   = ((pick_x - self.bd_begin) / self.space_width).trunc() as usize;
            let pick_fraction = (pick_x - self.bd_begin) / self.space_width;

            self.each_part(|column, part| match part {
                Part::Gap { width, begin, end } if pick_column >= column && pick_column < column + width => Some(listing::pick::Triplet {
                    begin: (self.node_path.clone(), listing::pick::Part::Bindump {
                        index: end.1,
                        offset: end.0,
                    }),
                    middle: (self.node_path.clone(), match (pick_fraction < column as f32 + width as f32 / 2.0, begin) {
                        (true, Some(begin)) => listing::pick::Part::Bindump {
                            index: begin.1,
                            offset: begin.0,
                        },
                        _ => listing::pick::Part::Bindump {
                            index: end.1,
                            offset: end.0,
                        }
                    }),
                    end: (self.node_path.clone(), listing::pick::Part::Bindump {
                        index: end.1,
                        offset: end.0,
                    }),
                }),

                Part::Octet { offset, next_offset: _, num_bits, token } if pick_column >= column && pick_column < column + num_bits as usize => Some(listing::pick::Triplet::all3(self.node_path.clone(), listing::pick::Part::Bindump {
                    index: token.node_child_index(),
                    offset: offset + addr::Offset::BIT * (num_bits as usize - (pick_column - column)) as u64,
                })),

                _ => None,
            }).1
        } else {
            let last_tok = self.toks.last().unwrap();
            
            Some(listing::pick::Triplet::all3(self.node_path.clone(), listing::pick::Part::Bindump {
                index: last_tok.node_child_index(),
                offset: last_tok.extent.end,
            }))
        }
    }
}
