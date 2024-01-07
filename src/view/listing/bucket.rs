use std::iter;
use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::listing::token;
use crate::model::listing::token::TokenKind;
use crate::model::selection;
use crate::view::listing;
use crate::view::listing::facet::cursor;
use crate::view::listing::token_view;
use crate::view::listing::layout::LayoutController;
use crate::view::listing::layout::LayoutProvider;

use gtk::graphene;

mod hexdump;

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
    
    fn pick(&self, point: &graphene::Point) -> Option<listing::PickResult> {
        let _ = point;
        None
    }

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
            let selection = ctx.selection.node_intersection(tv.token().node(), tv.token().node_path(), tv.token().node_addr());
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
    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>>;
    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token>;
}

pub trait TokenViewIterableBucket: Bucket {
    fn iter_token_views_mut(&mut self) -> impl iter::Iterator<Item = &mut token_view::TokenView>;
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

pub use hexdump::HexdumpBucket;

impl<Marker> SingleTokenBucket<Marker> {
}

impl<Marker, Token: token::TokenKind> From<Token> for SingleTokenBucket<Marker> {
    fn from(token: Token) -> Self {
        SingleTokenBucket {
            begin: graphene::Point::zero(),
            end: graphene::Point::zero(),
            tv: token_view::TokenView::from(token.into_token()),
            marker: std::marker::PhantomData
        }
    }
}

impl<Marker> Bucket for SingleTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    fn render(&mut self, ctx: RenderArgs<'_>, layout: &mut LayoutController) {
        let mut begin = self.begin.clone();
        let mut end = self.end.clone();
        
        default_render(ctx, layout, &mut begin, &mut end, self.iter_token_views_mut(), std::marker::PhantomData::<Marker>);
        
        self.begin = begin;
        self.end = end;
    }

    fn visible_address(&self) -> Option<addr::Address> {
        self.tv.visible_address()
    }
}

impl<Marker> TokenIterableBucket for SingleTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        iter::once(self.tv.token())
    }

    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        iter::once(self.tv.into_token())
    }
}

impl<Marker> TokenViewIterableBucket for SingleTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    fn iter_token_views_mut(&mut self) -> impl iter::Iterator<Item = &mut token_view::TokenView> {
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

impl<Marker, Token: token::TokenKind> From<Option<Token>> for MaybeTokenBucket<Marker> {
    fn from(token: Option<Token>) -> Self {
        MaybeTokenBucket {
            begin: graphene::Point::zero(),
            end: graphene::Point::zero(),
            tv: token.map(TokenKind::into_token).map(token_view::TokenView::from),
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
    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        self.tv.as_ref().map(token_view::TokenView::token).into_iter()
    }

    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        self.tv.map(token_view::TokenView::into_token).into_iter()
    }
}

impl<Marker> TokenViewIterableBucket for MaybeTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    fn iter_token_views_mut(&mut self) -> impl iter::Iterator<Item = &mut token_view::TokenView> {
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
    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        self.tvs.iter().map(token_view::TokenView::token)
    }

    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        self.tvs.into_iter().map(token_view::TokenView::into_token)
    }
}

impl<Marker> TokenViewIterableBucket for MultiTokenBucket<Marker> where LayoutController: LayoutProvider<Marker> {
    fn iter_token_views_mut(&mut self) -> impl iter::Iterator<Item = &mut token_view::TokenView> {
        self.tvs.iter_mut()
    }
}
