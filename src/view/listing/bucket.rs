use std::iter;
use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::model::selection;
use crate::view::listing;
use crate::view::listing::facet::cursor;
use crate::view::listing::token_view;

use gtk::graphene;

#[derive(Clone, Copy)]
pub struct RenderArgs<'a> {
    snapshot: &'a gtk::Snapshot,
    cursor: &'a cursor::CursorView,
    selection: &'a selection::listing::Mode,
    render: &'a listing::RenderDetail
}

pub trait Bucket {
    type TokenIterator;
    type BorrowingTokenIterator<'a> where Self: 'a;
    
    //fn render(&mut self, ctx: RenderArgs<'_>);
    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_>;
    fn to_tokens(self) -> Self::TokenIterator;
}

pub struct BlankMarker;
pub struct TitleMarker;
pub struct HexstringMarker;
pub struct SummaryMarker;

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
    begin: graphene::Point,
    end: graphene::Point,
    node: sync::Arc<structure::Node>,
    line_extent: addr::Extent,
    tvs: Vec<token_view::TokenView>
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

impl<Marker> Bucket for SingleTokenBucket<Marker> {
    type TokenIterator = iter::Once<token::Token>;
    type BorrowingTokenIterator<'a> = iter::Once<&'a token::Token> where Marker: 'a;

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        iter::once(self.tv.token())
    }

    fn to_tokens(self) -> Self::TokenIterator {
        iter::once(self.tv.into_token())
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

impl<Marker> Bucket for MaybeTokenBucket<Marker> {
    type TokenIterator = std::option::IntoIter<token::Token>;
    type BorrowingTokenIterator<'a> = std::option::IntoIter<&'a token::Token> where Marker: 'a;

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.tv.as_ref().map(token_view::TokenView::token).into_iter()
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.tv.map(token_view::TokenView::into_token).into_iter()
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

impl<Marker> Bucket for MultiTokenBucket<Marker> {
    type TokenIterator = iter::Map<vec::IntoIter<token_view::TokenView>, fn(token_view::TokenView) -> token::Token>;
    type BorrowingTokenIterator<'a> = iter::Map<std::slice::Iter<'a, token_view::TokenView>, fn(&'a token_view::TokenView) -> &'a token::Token> where Marker: 'a;

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.tvs.iter().map(token_view::TokenView::token)
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.tvs.into_iter().map(token_view::TokenView::into_token)
    }
}

impl HexdumpBucket {
    pub fn new(node: sync::Arc<structure::Node>, line_extent: addr::Extent, tokens: impl Iterator<Item = token::Token>) -> Self {
        HexdumpBucket {
            begin: graphene::Point::zero(),
            end: graphene::Point::zero(),
            node,
            line_extent,
            tvs: tokens.map(token_view::TokenView::from).collect(),
        }
    }
}

impl Bucket for HexdumpBucket {
    type TokenIterator = iter::Map<vec::IntoIter<token_view::TokenView>, fn(token_view::TokenView) -> token::Token>;
    type BorrowingTokenIterator<'a> = iter::Map<std::slice::Iter<'a, token_view::TokenView>, fn(&'a token_view::TokenView) -> &'a token::Token>;

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.tvs.iter().map(token_view::TokenView::token)
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.tvs.into_iter().map(token_view::TokenView::into_token)
    }
}
