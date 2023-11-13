use std::iter;
use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::datapath::DataPathExt;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::model::listing::cursor::CursorClass;
use crate::model::selection;
use crate::view::gsc;
use crate::view::helpers;
use crate::view::listing;
use crate::view::listing::facet::cursor;
use crate::view::listing::token_view;
use crate::view::listing::layout::LayoutController;
use crate::view::listing::layout::LayoutProvider;

use gtk::prelude::*;
use gtk::graphene;

#[derive(Clone, Copy)]
pub struct RenderArgs<'a> {
    pub snapshot: &'a gtk::Snapshot,
    pub cursor: &'a cursor::CursorView,
    pub selection: &'a selection::listing::Mode,
    pub render: &'a listing::RenderDetail,
}

pub trait Bucket: WorkableBucket {
    fn render(&mut self, ctx: RenderArgs<'_>, layout: &mut LayoutController);
    fn visible_address(&self) -> Option<addr::Address>;

    fn as_bucket(&self) -> &dyn Bucket where Self: Sized {
        self
    }

    fn as_bucket_mut(&mut self) -> &mut dyn Bucket where Self: Sized {
        self
    }
}

fn default_render<'a, Marker>(ctx: RenderArgs<'_>, layout: &mut LayoutController, begin: &mut graphene::Point, end: &mut graphene::Point, tvs: impl Iterator<Item = &'a mut token_view::TokenView>, marker: std::marker::PhantomData<Marker>) where LayoutController: LayoutProvider<Marker> {
    layout.allocate(marker, |mut point| {
        *begin = point.clone();

        for tv in tvs {
            let selection = ctx.selection.node_intersection(&tv.token().node, &tv.token().node_path, tv.token().node_addr);
            point = tv.render(ctx.snapshot, ctx.cursor, selection, ctx.render, &point);
        }

        *end = point.clone();

        point
    });
}

/* This trait is separate from Bucket so these functions can be impl'd automatically based on TokenViewIterableBucket. */
pub trait WorkableBucket {
    /// Returns true if any work was done and a redraw is required.
    fn work(&mut self, document: &sync::Arc<document::Document>, cx: &mut std::task::Context) -> bool;
    fn invalidate_data(&mut self);
}

/* This trait is separate from Bucket so we can take &dyn Bucket without specifying iterator types. */
pub trait TokenIterableBucket: Bucket {
    type TokenIterator: iter::Iterator<Item = token::Token>;
    type BorrowingTokenIterator<'a>: iter::Iterator<Item = &'a token::Token> where Self: 'a;

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_>;
    fn to_tokens(self) -> Self::TokenIterator;
}

pub trait TokenViewIterableBucket: Bucket {
    type BorrowingMutableTokenViewIterator<'a>: iter::Iterator<Item = &'a mut token_view::TokenView> where Self: 'a;
    
    fn iter_token_views_mut(&mut self) -> Self::BorrowingMutableTokenViewIterator<'_>;
}

impl<T: TokenViewIterableBucket> WorkableBucket for T {
    fn work(&mut self, document: &sync::Arc<document::Document>, cx: &mut std::task::Context) -> bool {
        let mut did_work = false;

        for tok in self.iter_token_views_mut() {
            /* don't short-circuit!!! */
            if tok.work(document, cx) {
                did_work = true;
            }
        }

        did_work
    }

    fn invalidate_data(&mut self) {
        for tok in self.iter_token_views_mut() {
            tok.invalidate_data()
        }
    }
}

pub struct BlankMarker;
pub struct TitleMarker;
pub struct HexstringMarker;
pub struct SummaryMarker;
pub struct HexdumpMarker;
pub struct AsciidumpMarker;

pub struct SingleTokenBucket<Marker> {
    begin: graphene::Point,
    end: graphene::Point,
    tv: token_view::TokenView,
    marker: std::marker::PhantomData<Marker>,
}

pub struct MaybeTokenBucket<Marker> {
    begin: graphene::Point,
    end: graphene::Point,
    tv: Option<token_view::TokenView>,
    marker: std::marker::PhantomData<Marker>,
}

pub struct MultiTokenBucket<Marker> {
    begin: graphene::Point,
    end: graphene::Point,
    tvs: Vec<token_view::TokenView>,
    marker: std::marker::PhantomData<Marker>,
}

pub struct HexdumpBucket {
    hd_begin: graphene::Point,
    hd_end: graphene::Point,

    ascii_begin: graphene::Point,
    ascii_end: graphene::Point,
    
    node: sync::Arc<structure::Node>,
    node_path: structure::Path,
    node_addr: addr::Address,
    
    line_extent: addr::Extent,
    line_cache: vec::Vec<datapath::ByteRecord>,
    line_data_pending: bool,
    
    toks: Vec<token::Token>
}

impl<Marker> SingleTokenBucket<Marker> {
}

impl<Marker> From<token::Token> for SingleTokenBucket<Marker> {
    fn from(token: token::Token) -> Self {
        SingleTokenBucket {
            begin: graphene::Point::zero(),
            end: graphene::Point::zero(),
            tv: token_view::TokenView::from(token),
            marker: std::marker::PhantomData
        }
    }
}

impl<Marker> Bucket for SingleTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    fn visible_address(&self) -> Option<addr::Address> {
        self.tv.visible_address()
    }

    fn render(&mut self, ctx: RenderArgs<'_>, layout: &mut LayoutController) {
        let mut begin = self.begin.clone();
        let mut end = self.end.clone();
        
        default_render(ctx, layout, &mut begin, &mut end, self.iter_token_views_mut(), std::marker::PhantomData::<Marker>);
        
        self.begin = begin;
        self.end = end;
    }
}

impl<Marker> TokenIterableBucket for SingleTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    type TokenIterator = iter::Once<token::Token>;
    type BorrowingTokenIterator<'a> = iter::Once<&'a token::Token> where Marker: 'a;

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        iter::once(self.tv.token())
    }

    fn to_tokens(self) -> Self::TokenIterator {
        iter::once(self.tv.into_token())
    }
}

impl<Marker> TokenViewIterableBucket for SingleTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    type BorrowingMutableTokenViewIterator<'a> = iter::Once<&'a mut token_view::TokenView> where Marker: 'a;
    
    fn iter_token_views_mut(&mut self) -> Self::BorrowingMutableTokenViewIterator<'_> {
        iter::once(&mut self.tv)
    }
}

impl<Marker> MaybeTokenBucket<Marker> {
    fn from_token(token: Option<token::Token>) -> Self {
        MaybeTokenBucket {
            begin: graphene::Point::zero(),
            end: graphene::Point::zero(),
            tv: token.map(token_view::TokenView::from),
            marker: std::marker::PhantomData
        }
    }
}

impl<Marker> From<Option<token::Token>> for MaybeTokenBucket<Marker> {
    fn from(token: Option<token::Token>) -> Self {
        MaybeTokenBucket {
            begin: graphene::Point::zero(),
            end: graphene::Point::zero(),
            tv: token.map(token_view::TokenView::from),
            marker: std::marker::PhantomData
        }
    }
}

impl<Marker> Bucket for MaybeTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    fn visible_address(&self) -> Option<addr::Address> {
        self.tv.as_ref().and_then(|tv| tv.visible_address())
    }

    fn render(&mut self, ctx: RenderArgs<'_>, layout: &mut LayoutController) {
        let mut begin = self.begin.clone();
        let mut end = self.end.clone();
        
        default_render(ctx, layout, &mut begin, &mut end, self.iter_token_views_mut(), std::marker::PhantomData::<Marker>);

        self.begin = begin;
        self.end = end;
    }
}

impl<Marker> TokenIterableBucket for MaybeTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    type TokenIterator = std::option::IntoIter<token::Token>;
    type BorrowingTokenIterator<'a> = std::option::IntoIter<&'a token::Token> where Marker: 'a;

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.tv.as_ref().map(token_view::TokenView::token).into_iter()
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.tv.map(token_view::TokenView::into_token).into_iter()
    }
}

impl<Marker> TokenViewIterableBucket for MaybeTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    type BorrowingMutableTokenViewIterator<'a> = std::option::IntoIter<&'a mut token_view::TokenView> where Marker: 'a;

    fn iter_token_views_mut(&mut self) -> Self::BorrowingMutableTokenViewIterator<'_> {
        self.tv.as_mut().into_iter()
    }
}

impl<Marker> MultiTokenBucket<Marker> {
    pub fn from_tokens(tokens: impl Iterator<Item = token::Token>) -> Self {
        MultiTokenBucket {
            begin: graphene::Point::zero(),
            end: graphene::Point::zero(),
            tvs: tokens.map(token_view::TokenView::from).collect(),
            marker: std::marker::PhantomData
        }
    }
}

impl<Marker> Bucket for MultiTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    fn visible_address(&self) -> Option<addr::Address> {
        self.tvs.get(0).and_then(|tv| tv.visible_address())
    }

    fn render(&mut self, ctx: RenderArgs<'_>, layout: &mut LayoutController) {
        let mut begin = self.begin.clone();
        let mut end = self.end.clone();
        
        default_render(ctx, layout, &mut begin, &mut end, self.iter_token_views_mut(), std::marker::PhantomData::<Marker>);

        self.begin = begin;
        self.end = end;
    }
}

impl<Marker> TokenIterableBucket for MultiTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    type TokenIterator = iter::Map<vec::IntoIter<token_view::TokenView>, fn(token_view::TokenView) -> token::Token>;
    type BorrowingTokenIterator<'a> = iter::Map<std::slice::Iter<'a, token_view::TokenView>, fn(&'a token_view::TokenView) -> &'a token::Token> where Marker: 'a;

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.tvs.iter().map(token_view::TokenView::token)
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.tvs.into_iter().map(token_view::TokenView::into_token)
    }
}

impl<Marker> TokenViewIterableBucket for MultiTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    type BorrowingMutableTokenViewIterator<'a> = std::slice::IterMut<'a, token_view::TokenView> where Marker: 'a;

    fn iter_token_views_mut(&mut self) -> Self::BorrowingMutableTokenViewIterator<'_> {
        self.tvs.iter_mut()
    }
}

impl HexdumpBucket {
    pub fn new(node: sync::Arc<structure::Node>, node_path: structure::Path, node_addr: addr::Address, line_extent: addr::Extent, tokens: impl Iterator<Item = token::Token>) -> Self {
        HexdumpBucket {
            hd_begin: graphene::Point::zero(),
            hd_end: graphene::Point::zero(),
            
            ascii_begin: graphene::Point::zero(),
            ascii_end: graphene::Point::zero(),
            
            node,
            node_path,
            node_addr,
            
            line_extent,
            line_cache: Vec::new(),
            line_data_pending: true,
            
            toks: tokens.collect(),
        }
    }
}

impl Bucket for HexdumpBucket {
    fn visible_address(&self) -> Option<addr::Address> {
        Some(self.line_extent.begin)
    }

    fn render(&mut self, ctx: RenderArgs<'_>, layout: &mut LayoutController) {
        let lh = helpers::pango_unscale(ctx.render.metrics.height());
        let selection = ctx.selection.node_intersection(&self.node, &self.node_path, self.node_addr);
        
        /* Hexdump */
        layout.allocate(std::marker::PhantomData::<HexdumpMarker>, |mut point| {
            self.hd_begin = point.clone();

            point.set_y(lh);
            
            let (_, logical_space) = ctx.render.gsc_mono.get(gsc::Entry::Space).unwrap().clone().extents(&ctx.render.font_mono);
            let space_width = helpers::pango_unscale(logical_space.width());
            
            let hex_cursor = match &ctx.cursor.cursor.class {
                CursorClass::Hexdump(hxc) => Some(hxc),
                _ => None,
            };

            let gutter_pitch = addr::Size::from(8);
            
            let mut offset = self.line_extent.begin;
            let mut next_gutter = offset + gutter_pitch;

            let mut token_iterator = self.toks.iter();
            let mut next_token = token_iterator.next();
            
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
        layout.allocate(std::marker::PhantomData::<AsciidumpMarker>, |mut point| {
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
}
    
impl TokenIterableBucket for HexdumpBucket {
    type TokenIterator = vec::IntoIter<token::Token>;
    type BorrowingTokenIterator<'a> = std::slice::Iter<'a, token::Token>;

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.toks.iter()
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.toks.into_iter()
    }
}

impl WorkableBucket for HexdumpBucket {
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
