use std::iter;
use std::sync;
use std::task;

use crate::model::addr;
use crate::model::document;
use crate::model::listing::cursor;
use crate::model::listing::line as line_model;
use crate::model::listing::token;
use crate::model::listing::token::TokenKind;
use crate::model::listing::window;
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

impl std::fmt::Debug for LineViewType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LineViewType").finish()
    }
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

impl window::LineView for Line {
    fn from_line(line: line_model::Line) -> Self {
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

    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        self.ty.iter_tokens()
    }
    
    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        self.ty.to_tokens()
    }
}

impl LineViewType {
    fn from(line: line_model::Line) -> Self {
        match line.ty {
            line_model::LineType::Empty => Self::Empty,
            line_model::LineType::Blank(tok) => Self::Blank(tok.into()),
            line_model::LineType::Title(tok) => Self::Title(tok.into()),
            line_model::LineType::Hexdump { title, node, node_path, node_addr, line_extent, tokens } => Self::Hexdump {
                title: title.into(),
                hexdump: bucket::HexdumpBucket::new(node, node_path, node_addr, line_extent, tokens.into_iter())
            },
            line_model::LineType::Hexstring { title, token } => Self::Hexstring {
                title: title.into(),
                hexstring: token.into()
            },
            line_model::LineType::Summary { title, tokens } => Self::Summary {
                title: title.into(),
                content: bucket::MultiTokenBucket::from_tokens(tokens.into_iter())
            },
        }
    }

    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        match &self {
            Self::Empty => util::PhiIterator::I1(iter::empty()),
            Self::Blank(bucket) => util::PhiIterator::I2(bucket.iter_tokens()),
            Self::Title(bucket) => util::PhiIterator::I3(bucket.iter_tokens()),
            Self::Hexdump { title, hexdump } => util::PhiIterator::I4(title.iter_tokens().chain(hexdump.iter_tokens())),
            Self::Hexstring { title, hexstring } => util::PhiIterator::I5(title.iter_tokens().chain(hexstring.iter_tokens())),
            Self::Summary { title, content } => util::PhiIterator::I6(title.iter_tokens().chain(content.iter_tokens())),
        }
    }

    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        match self {
            Self::Empty => util::PhiIterator::I1(iter::empty()),
            Self::Blank(bucket) => util::PhiIterator::I2(bucket.to_tokens()),
            Self::Title(bucket) => util::PhiIterator::I3(bucket.to_tokens()),
            Self::Hexdump { title, hexdump } => util::PhiIterator::I4(title.to_tokens().chain(hexdump.to_tokens())),
            Self::Hexstring { title, hexstring } => util::PhiIterator::I5(title.to_tokens().chain(hexstring.to_tokens())),
            Self::Summary { title, content } => util::PhiIterator::I6(title.to_tokens().chain(content.to_tokens())),
        }
    }

    fn iter_buckets(&self) -> impl Iterator<Item = &dyn bucket::Bucket> {
        match self {
            Self::Empty => util::PhiIteratorOf3::I1(iter::empty()),
            Self::Blank(bucket) => util::PhiIteratorOf3::I2(iter::once(bucket.as_bucket())),
            Self::Title(bucket) => util::PhiIteratorOf3::I2(iter::once(bucket.as_bucket())),
            Self::Hexdump { title, hexdump } => util::PhiIteratorOf3::I3([title.as_bucket(), hexdump.as_bucket()].into_iter()),
            Self::Hexstring { title, hexstring } => util::PhiIteratorOf3::I3([title.as_bucket(), hexstring.as_bucket()].into_iter()),
            Self::Summary { title, content } => util::PhiIteratorOf3::I3([title.as_bucket(), content.as_bucket()].into_iter()),
        }
    }
    
    fn iter_buckets_mut(&mut self) -> impl Iterator<Item = &mut dyn bucket::Bucket> {
        match self {
            Self::Empty => util::PhiIteratorOf3::I1(iter::empty()),
            Self::Blank(bucket) => util::PhiIteratorOf3::I2(iter::once(bucket.as_bucket_mut())),
            Self::Title(bucket) => util::PhiIteratorOf3::I2(iter::once(bucket.as_bucket_mut())),
            Self::Hexdump { title, hexdump } => util::PhiIteratorOf3::I3([title.as_bucket_mut(), hexdump.as_bucket_mut()].into_iter()),
            Self::Hexstring { title, hexstring } => util::PhiIteratorOf3::I3([title.as_bucket_mut(), hexstring.as_bucket_mut()].into_iter()),
            Self::Summary { title, content } => util::PhiIteratorOf3::I3([title.as_bucket_mut(), content.as_bucket_mut()].into_iter()),
        }
    }
    
    fn contains_cursor(&self, cursor: &cursor::Cursor) -> bool {
        self.iter_tokens().any(|t| cursor.is_over(t))
    }

    fn indentation(&self) -> usize {
        self.iter_tokens().next().map_or(0, |t| t.common().depth)
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
            gsc::begin_text(&render.pango, &render.font_mono, render.config.addr_color.rgba(), &format!("{}", addr), &mut pos).render_right_aligned(&snapshot);
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

    pub fn pick(&self, x: f64, y: f64) -> Option<listing::pick::Triplet> {
        let point = graphene::Point::new(x as f32, y as f32);
        
        let bucket = self.ty.iter_buckets().find(|bucket| bucket.contains(&point)).or_else(|| self.ty.iter_buckets().last());
        bucket.and_then(|bucket| bucket.pick(&point))
    }
}

impl facet::Facet for Line {
    fn wants_draw(&self) -> &facet::Event {
        &self.ev_draw
    }

    fn wants_work(&self) -> &facet::Event {
        &self.ev_work
    }

    fn work(&mut self, document: &sync::Arc<document::Document>, cx: &mut task::Context) -> bool {
        let invalidate = self.current_document.as_ref().map_or(true, |current| !sync::Arc::ptr_eq(current, document));
        let mut updated = false;
        let mut work_needed = false;
        
        for bucket in self.ty.iter_buckets_mut() {
            if invalidate {
                bucket.invalidate_data();
            }

            bucket.work(document, cx, &mut updated, &mut work_needed);
        }
        
        if updated {
            self.invalidate_render_node();
            self.ev_work.want();
            self.ev_draw.want();
        }
        
        if invalidate {
            self.current_document = Some(document.clone());
        }

        work_needed
    }
}
