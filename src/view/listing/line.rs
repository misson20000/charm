use std::iter;
use std::sync;
use std::task;

use crate::model::addr;
use crate::model::document;
use crate::model::listing::cursor;
use crate::model::listing::layout;
use crate::model::listing::token;
use crate::model::selection;
use crate::util;
use crate::view::gsc;
use crate::view::helpers;
use crate::view::listing;
use crate::view::listing::bucket;
use crate::view::listing::bucket::Bucket;
use crate::view::listing::bucket::TokenIterableBucket;
use crate::view::listing::bucket::WorkableBucket;
use crate::view::listing::facet;
use crate::view::listing::token_view;
use crate::view::listing::layout::LayoutController;

use gtk::prelude::*;
use gtk::graphene;
use gtk::gsk;

enum LineViewType {
    Empty,
    Blank(bucket::SingleTokenBucket<bucket::BlankMarker>),
    Title(bucket::SingleTokenBucket<bucket::TitleMarker>),
    Hexdump {
        title: bucket::MaybeTokenBucket<bucket::TitleMarker>,
        hexdump: bucket::HexdumpBucket,
    },
    Hexstring {
        title: bucket::MaybeTokenBucket<bucket::TitleMarker>,
        hexstring: bucket::SingleTokenBucket<bucket::HexstringMarker>,
    },
    Summary {
        title: bucket::MaybeTokenBucket<bucket::TitleMarker>,
        content: bucket::MultiTokenBucket<bucket::SummaryMarker>,
    },
}

pub struct Line {
    ev_draw: facet::Event,
    ev_work: facet::Event,

    current_document: Option<sync::Arc<document::Document>>,

    ty: LineViewType,
    
    render_serial: u64,
    selection_hash: u64,
    render_node: Option<gsk::RenderNode>,
}

impl layout::LineView for Line {
    type BorrowingTokenIterator<'a> = LineViewBorrowingTokenIterator<'a>;
    type TokenIterator = LineViewTokenIterator;
    
    fn from_line(line: layout::Line) -> Self {
        Line {
            ev_draw: facet::Event::new(),
            ev_work: facet::Event::new_wanted(),

            current_document: None,

            ty: LineViewType::from(line),
            
            render_serial: 0,
            selection_hash: 0,
            render_node: None,
        }
    }

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.ty.iter_tokens()
    }
    
    fn to_tokens(self) -> Self::TokenIterator {
        self.ty.to_tokens()
    }
}

type LineViewBorrowingTokenIterator<'a> = util::PhiIterator
    <&'a token::Token,
     iter::Empty<&'a token::Token>,
     <bucket::SingleTokenBucket<bucket::BlankMarker> as TokenIterableBucket>::BorrowingTokenIterator<'a>,
     <bucket::SingleTokenBucket<bucket::TitleMarker> as TokenIterableBucket>::BorrowingTokenIterator<'a>,
     iter::Chain
       <<bucket::MaybeTokenBucket<bucket::TitleMarker> as TokenIterableBucket>::BorrowingTokenIterator<'a>,
        <bucket::HexdumpBucket as TokenIterableBucket>::BorrowingTokenIterator<'a>>,
     iter::Chain
       <<bucket::MaybeTokenBucket<bucket::TitleMarker> as TokenIterableBucket>::BorrowingTokenIterator<'a>,
        <bucket::SingleTokenBucket<bucket::HexstringMarker> as TokenIterableBucket>::BorrowingTokenIterator<'a>>,
     iter::Chain
       <<bucket::MaybeTokenBucket<bucket::TitleMarker> as TokenIterableBucket>::BorrowingTokenIterator<'a>,
        <bucket::MultiTokenBucket<bucket::SummaryMarker> as TokenIterableBucket>::BorrowingTokenIterator<'a>>>;

type LineViewTokenIterator = util::PhiIterator
    <token::Token,
     iter::Empty<token::Token>,
     <bucket::SingleTokenBucket<bucket::BlankMarker> as TokenIterableBucket>::TokenIterator,
     <bucket::SingleTokenBucket<bucket::TitleMarker> as TokenIterableBucket>::TokenIterator,
     iter::Chain
       <<bucket::MaybeTokenBucket<bucket::TitleMarker> as TokenIterableBucket>::TokenIterator,
        <bucket::HexdumpBucket as TokenIterableBucket>::TokenIterator>,
     iter::Chain
       <<bucket::MaybeTokenBucket<bucket::TitleMarker> as TokenIterableBucket>::TokenIterator,
        <bucket::SingleTokenBucket<bucket::HexstringMarker> as TokenIterableBucket>::TokenIterator>,
     iter::Chain
       <<bucket::MaybeTokenBucket<bucket::TitleMarker> as TokenIterableBucket>::TokenIterator,
        <bucket::MultiTokenBucket<bucket::SummaryMarker> as TokenIterableBucket>::TokenIterator>>;

type LineViewBorrowingMutableBucketIterator<'a> = util::PhiIterator
    <&'a mut dyn Bucket,
     iter::Empty<&'a mut dyn Bucket>,
     iter::Once<&'a mut dyn Bucket>,
     std::array::IntoIter<&'a mut dyn Bucket, 2>>;

impl LineViewType {
    fn from(line: layout::Line) -> Self {
        match line.ty {
            layout::LineType::Empty => Self::Empty,
            layout::LineType::Blank(tok) => Self::Blank(tok.into()),
            layout::LineType::Title(tok) => Self::Title(tok.into()),
            layout::LineType::Hexdump { title, node, node_path, node_addr, line_extent, tokens } => Self::Hexdump {
                title: title.into(),
                hexdump: bucket::HexdumpBucket::new(node, node_path, node_addr, line_extent, tokens.into_iter())
            },
            layout::LineType::Hexstring { title, token } => Self::Hexstring {
                title: title.into(),
                hexstring: token.into()
            },
            layout::LineType::Summary { title, tokens } => Self::Summary {
                title: title.into(),
                content: bucket::MultiTokenBucket::from_tokens(tokens.into_iter())
            },
        }
    }

    fn iter_tokens(&self) -> LineViewBorrowingTokenIterator<'_> {
        match &self {
            Self::Empty => util::PhiIterator::I1(iter::empty()),
            Self::Blank(bucket) => util::PhiIterator::I2(bucket.iter_tokens()),
            Self::Title(bucket) => util::PhiIterator::I3(bucket.iter_tokens()),
            Self::Hexdump { title, hexdump } => util::PhiIterator::I4(title.iter_tokens().chain(hexdump.iter_tokens())),
            Self::Hexstring { title, hexstring } => util::PhiIterator::I5(title.iter_tokens().chain(hexstring.iter_tokens())),
            Self::Summary { title, content } => util::PhiIterator::I6(title.iter_tokens().chain(content.iter_tokens())),
        }
    }

    fn to_tokens(self) -> LineViewTokenIterator {
        match self {
            Self::Empty => util::PhiIterator::I1(iter::empty()),
            Self::Blank(bucket) => util::PhiIterator::I2(bucket.to_tokens()),
            Self::Title(bucket) => util::PhiIterator::I3(bucket.to_tokens()),
            Self::Hexdump { title, hexdump } => util::PhiIterator::I4(title.to_tokens().chain(hexdump.to_tokens())),
            Self::Hexstring { title, hexstring } => util::PhiIterator::I5(title.to_tokens().chain(hexstring.to_tokens())),
            Self::Summary { title, content } => util::PhiIterator::I6(title.to_tokens().chain(content.to_tokens())),
        }
    }

    fn iter_buckets_mut(&mut self) -> LineViewBorrowingMutableBucketIterator<'_> {
        match self {
            Self::Empty => util::PhiIterator::I1(iter::empty()),
            Self::Blank(bucket) => util::PhiIterator::I2(iter::once(bucket)),
            Self::Title(bucket) => util::PhiIterator::I2(iter::once(bucket)),
            Self::Hexdump { title, hexdump } => util::PhiIterator::I3([title.as_bucket_mut(), hexdump.as_bucket_mut()].into_iter()),
            Self::Hexstring { title, hexstring } => util::PhiIterator::I3([title.as_bucket_mut(), hexstring.as_bucket_mut()].into_iter()),
            Self::Summary { title, content } => util::PhiIterator::I3([title.as_bucket_mut(), content.as_bucket_mut()].into_iter()),
        }
    }
    
    fn contains_cursor(&self, cursor: &cursor::Cursor) -> bool {
        self.iter_tokens().any(|t| cursor.is_over(t))
    }

    fn indentation(&self) -> usize {
        self.iter_tokens().next().map_or(0, |t| t.depth)
    }

    fn visible_address(&self) -> Option<addr::Address> {
        match self {
            Self::Empty => None,
            Self::Blank(_) => None,
            Self::Title(bucket) => bucket.visible_address(),
            Self::Hexdump { title, hexdump } => title.visible_address().or(hexdump.visible_address()),
            Self::Hexstring { title, hexstring } => title.visible_address().or(hexstring.visible_address()),
            Self::Summary { title, content } => title.visible_address().or(content.visible_address()),
        }
    }
    
    fn invalidate_data(&mut self) {
        match self {
            Self::Empty => {},
            Self::Blank(_) => {},
            Self::Title(_) => {},
            Self::Hexdump { title: _, hexdump } => hexdump.invalidate_data(),
            Self::Hexstring { title: _, hexstring } => hexstring.invalidate_data(),
            Self::Summary { title: _, content } => content.invalidate_data(),
        }
    }
}

impl Line {
    pub fn invalidate_render_node(&mut self) {
        self.render_node = None;
    }

    pub fn render(&mut self, cursor: &facet::cursor::CursorView, selection: &selection::listing::Mode, render: &listing::RenderDetail) -> Option<gsk::RenderNode> {
        /* check if the cursor is on any of the tokens on this line */
        let has_cursor = self.ty.contains_cursor(&cursor.cursor);

        let selection_hash = {
            let mut state = std::collections::hash_map::DefaultHasher::default();
            std::hash::Hash::hash(selection, &mut state);
            std::hash::Hasher::finish(&state)
        };
        
        /* if we rendered this line earlier, the parameters haven't been invalidated, and the cursor isn't on this line, just reuse the previous snapshot. */
        if let Some(rn) = self.render_node.as_ref() {
            /* if the cursor is on the line, we need to redraw it every time the cursor animates, which is hard to tell when that happens so we just redraw it every frame. */
            if self.render_serial == render.serial && self.selection_hash == selection_hash && !has_cursor {
                return Some(rn.clone());
            }
        }

        let snapshot = gtk::Snapshot::new();

        /* draw address into address pane */
        if let Some(addr) = self.ty.visible_address() {
            let mut pos = graphene::Point::new(render.addr_pane_width - render.config.padding as f32, helpers::pango_unscale(render.metrics.height()));
            gsc::begin_text(&render.pango, &render.font_mono, &render.config.addr_color, &format!("{}", addr), &mut pos).render_right_aligned(&snapshot);
        }

        /* create structs for bucket render implementations to use */
        let mut layout = LayoutController::new(self.ty.indentation(), render);
        let args = bucket::RenderArgs {
            snapshot: &snapshot,
            cursor,
            selection,
            render,
        };

        /* have each bucket draw into the snapshot */
        for bucket in self.ty.iter_buckets_mut() {
            snapshot.save();
            bucket.render(args, &mut layout);
            snapshot.restore();
        }

        if !has_cursor {
            /* cache this snapshot and update cache keys */
            self.selection_hash = selection_hash;
            self.render_serial = render.serial;
            self.render_node = snapshot.to_node();
            self.render_node.clone()
        } else {
            /* don't store render snapshots when the cursor was on one of our
             * tokens, otherwise the appearance of the cursor will linger when
             * it moves off this line. */
            snapshot.to_node()
        }
    }

    /* This is really crummy and will have to change later, but I just want to get something working first. */
    pub fn pick_token(&self, _x: f64, _y: f64) -> Option<(&token_view::TokenView, f32)> {
        // TODO: picking
        /*
        let token = self.tokens.iter().find(
            |token| token.logical_bounds().map_or(
                false,
                |lb| lb.x() < x as f32))
            .or(self.tokens.first());
        token.and_then(|t| t.logical_bounds().map(|lb| (t, x as f32 - lb.x())))
            */
        None
    }
}

impl facet::Facet for Line {
    fn wants_draw(&mut self) -> &mut facet::Event {
        &mut self.ev_draw
    }

    fn wants_work(&mut self) -> &mut facet::Event {
        &mut self.ev_work
    }

    fn work(&mut self, document: &sync::Arc<document::Document>, cx: &mut task::Context) {
        let invalidate = self.current_document.as_ref().map_or(true, |current| !sync::Arc::ptr_eq(current, document));
        let mut updated = false;
        
        for bucket in self.ty.iter_buckets_mut() {
            if invalidate {
                bucket.invalidate_data();
            }

            if bucket.work(document, cx) {
                updated = true;
            }
        }
        
        if updated {
            self.invalidate_render_node();
            self.ev_work.want();
            self.ev_draw.want();
        }
        
        if invalidate {
            self.current_document = Some(document.clone());
        }
    }
}
