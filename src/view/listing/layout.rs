use crate::view::helpers;
use crate::view::listing::RenderDetail;
use crate::view::listing::bucket;

use gtk::graphene;

pub struct LayoutController {
    base_position: f32,
    indent_position: f32,
    main_position: f32,

    /// Minimum distance between main bucket and asciidump bucket
    asciidump_padding: f32,
    asciidump_origin: f32,
    asciidump_position: f32,
}

pub trait LayoutProvider<Marker> {
    fn allocate<F: FnOnce(f32) -> f32>(&mut self, marker: std::marker::PhantomData<Marker>, cb: F);
}

impl LayoutController {
    pub fn new(indentation: usize, render: &RenderDetail) -> LayoutController {
        let indent_width = indentation as f32 * render.config.indentation_width * helpers::pango_unscale(render.gsc_mono.space_width());
        
        let base_position = render.addr_pane_width + render.config.padding as f32;
        let indent_position = base_position + indent_width;
        let main_position = indent_position;

        let normal_main_width = 3.0*render.config.indentation_width + 8.0*3.0 + 1.0 * 8.0*3.0 + 2.0;
        
        let asciidump_origin = base_position + normal_main_width * helpers::pango_unscale(render.gsc_mono.space_width()) + render.config.padding as f32;
        let asciidump_position = asciidump_origin;
        
        LayoutController {
            base_position,
            indent_position,
            main_position,
            asciidump_padding: 2.0 * helpers::pango_unscale(render.gsc_mono.space_width()),
            asciidump_position,
            asciidump_origin,
        }
    }

    pub fn indent_rect(&self, lh: f32) -> graphene::Rect {
        graphene::Rect::new(self.base_position, 0.0, self.indent_position - self.base_position, lh)
    }

    pub fn main_rect(&self, lh: f32) -> graphene::Rect {
        graphene::Rect::new(self.indent_position, 0.0, self.main_position - self.indent_position, lh)
    }

    pub fn asciidump_rect(&self, lh: f32) -> graphene::Rect {
        graphene::Rect::new(self.asciidump_origin, 0.0, self.asciidump_position - self.asciidump_origin, lh)
    }
    
    fn cascade(&mut self) {
        if self.base_position > self.indent_position {
            self.indent_position = self.base_position;
        }

        if self.indent_position > self.main_position {
            self.main_position = self.indent_position;
        }

        if self.main_position + self.asciidump_padding > self.asciidump_position {
            self.asciidump_position = self.main_position + self.asciidump_padding;
        }
    }
    
    fn allocate_main<F: FnOnce(f32) -> f32>(&mut self, cb: F) {
        self.cascade();
        self.main_position = cb(self.main_position);
    }

    fn allocate_asciidump<F: FnOnce(f32) -> f32>(&mut self, cb: F) {
        self.cascade();
        self.asciidump_position = cb(self.asciidump_position);
    }
}

impl LayoutProvider<bucket::BlankMarker> for LayoutController {
    fn allocate<F: FnOnce(f32) -> f32>(&mut self, _marker: std::marker::PhantomData<bucket::BlankMarker>, cb: F) {
        cb(self.base_position);
    }
}

impl LayoutProvider<bucket::TitleMarker> for LayoutController {
    fn allocate<F: FnOnce(f32) -> f32>(&mut self, _marker: std::marker::PhantomData<bucket::TitleMarker>, cb: F) {
        self.allocate_main(cb);
    }
}

impl LayoutProvider<bucket::HexstringMarker> for LayoutController {
    fn allocate<F: FnOnce(f32) -> f32>(&mut self, _marker: std::marker::PhantomData<bucket::HexstringMarker>, cb: F) {
        self.allocate_main(cb);
    }
}

impl LayoutProvider<bucket::SummaryMarker> for LayoutController {
    fn allocate<F: FnOnce(f32) -> f32>(&mut self, _marker: std::marker::PhantomData<bucket::SummaryMarker>, cb: F) {
        self.allocate_main(cb);
    }
}

impl LayoutProvider<bucket::HexdumpMarker> for LayoutController {
    fn allocate<F: FnOnce(f32) -> f32>(&mut self, _marker: std::marker::PhantomData<bucket::HexdumpMarker>, cb: F) {
        self.allocate_main(cb);
    }
}

impl LayoutProvider<bucket::AsciidumpMarker> for LayoutController {
    fn allocate<F: FnOnce(f32) -> f32>(&mut self, _marker: std::marker::PhantomData<bucket::AsciidumpMarker>, cb: F) {
        self.allocate_asciidump(cb);
    }
}
