use gtk::cairo;
use gtk::gdk;

pub trait CairoExt {
    fn set_source_gdk_rgba(&self, color: gdk::RGBA);
}

impl CairoExt for cairo::Context {
    fn set_source_gdk_rgba(&self, color: gdk::RGBA) {
        self.set_source_rgba(color.red().into(), color.green().into(), color.blue().into(), color.alpha().into());
    }
}

pub trait RGBAExt {
    fn bytes(bytes: [u8; 4]) -> gdk::RGBA;
}

impl RGBAExt for gdk::RGBA {
    fn bytes(bytes: [u8; 4]) -> gdk::RGBA {
        gdk::RGBA::new(
            bytes[0] as f32 / 255.0,
            bytes[1] as f32 / 255.0,
            bytes[2] as f32 / 255.0,
            bytes[3] as f32 / 255.0
        )
    }
}
