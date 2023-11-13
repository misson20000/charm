use crate::view::helpers;
use crate::view::listing::RenderDetail;
use crate::view::listing::bucket;

use gtk::graphene;

pub struct LayoutController {
    base_position: graphene::Point,
    indent_position: graphene::Point,
    main_position: graphene::Point,
    
    asciidump_origin: graphene::Point,
    asciidump_position: graphene::Point,
}

pub trait LayoutProvider<Marker> {
    fn allocate<F: FnOnce(graphene::Point) -> graphene::Point>(&mut self, marker: std::marker::PhantomData<Marker>, cb: F);
}

impl LayoutController {
    pub fn new(indentation: usize, render: &RenderDetail) -> LayoutController {
        let base_position = graphene::Point::new(render.addr_pane_width + render.config.padding as f32, 0.0);
        
        let mut indent_position = base_position;
        let indent_width = indentation as f32 * render.config.indentation_width * helpers::pango_unscale(render.gsc_mono.space_width());
        indent_position.set_x(indent_position.x() + indent_width);

        let main_position = indent_position;
        
        let asciidump_origin = graphene::Point::new(render.ascii_pane_position + render.config.padding as f32, 0.0);
        let asciidump_position = asciidump_origin;
        
        LayoutController {
            base_position,
            indent_position,
            main_position,
            asciidump_position,
            asciidump_origin,
        }
    }

    pub fn indent_rect(&self, lh: f32) -> graphene::Rect {
        graphene::Rect::new(self.base_position.x(), 0.0, self.indent_position.x() - self.base_position.x(), lh)
    }

    pub fn main_rect(&self, lh: f32) -> graphene::Rect {
        graphene::Rect::new(self.indent_position.x(), 0.0, self.main_position.x() - self.indent_position.x(), lh)
    }

    pub fn asciidump_rect(&self, lh: f32) -> graphene::Rect {
        graphene::Rect::new(self.asciidump_origin.x(), 0.0, self.asciidump_position.x() - self.asciidump_origin.x(), lh)
    }
    
    fn cascade(&mut self) {
        if self.base_position.x() > self.indent_position.x() {
            self.indent_position.set_x(self.base_position.x());
        }

        if self.indent_position.x() > self.main_position.x() {
            self.main_position.set_x(self.indent_position.x());
        }

        if self.main_position.x() > self.asciidump_position.x() {
            self.asciidump_origin.set_x(self.main_position.x());
            self.asciidump_position.set_x(self.main_position.x());
        }
    }
    
    fn allocate_main<F: FnOnce(graphene::Point) -> graphene::Point>(&mut self, cb: F) {
        self.cascade();
        self.main_position = cb(self.main_position);
    }

    fn allocate_asciidump<F: FnOnce(graphene::Point) -> graphene::Point>(&mut self, cb: F) {
        self.cascade();
        self.asciidump_position = cb(self.asciidump_position);
    }
}

impl LayoutProvider<bucket::BlankMarker> for LayoutController {
    fn allocate<F: FnOnce(graphene::Point) -> graphene::Point>(&mut self, _marker: std::marker::PhantomData<bucket::BlankMarker>, cb: F) {
        cb(self.base_position);
    }
}

impl LayoutProvider<bucket::TitleMarker> for LayoutController {
    fn allocate<F: FnOnce(graphene::Point) -> graphene::Point>(&mut self, _marker: std::marker::PhantomData<bucket::TitleMarker>, cb: F) {
        self.allocate_main(cb);
    }
}

impl LayoutProvider<bucket::HexstringMarker> for LayoutController {
    fn allocate<F: FnOnce(graphene::Point) -> graphene::Point>(&mut self, _marker: std::marker::PhantomData<bucket::HexstringMarker>, cb: F) {
        self.allocate_main(cb);
    }
}

impl LayoutProvider<bucket::SummaryMarker> for LayoutController {
    fn allocate<F: FnOnce(graphene::Point) -> graphene::Point>(&mut self, _marker: std::marker::PhantomData<bucket::SummaryMarker>, cb: F) {
        self.allocate_main(cb);
    }
}

impl LayoutProvider<bucket::HexdumpMarker> for LayoutController {
    fn allocate<F: FnOnce(graphene::Point) -> graphene::Point>(&mut self, _marker: std::marker::PhantomData<bucket::HexdumpMarker>, cb: F) {
        self.allocate_main(cb);
    }
}

impl LayoutProvider<bucket::AsciidumpMarker> for LayoutController {
    fn allocate<F: FnOnce(graphene::Point) -> graphene::Point>(&mut self, _marker: std::marker::PhantomData<bucket::AsciidumpMarker>, cb: F) {
        self.allocate_asciidump(cb);
    }
}
