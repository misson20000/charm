pub trait CairoExt {
    fn set_source_gdk_rgba(&self, color: gdk::RGBA);
}

impl CairoExt for cairo::Context {
    fn set_source_gdk_rgba(&self, color: gdk::RGBA) {
        self.set_source_rgba(color.red, color.green, color.blue, color.alpha);
    }
}

pub trait RGBAExt {
    fn bytes(bytes: [u8; 4]) -> gdk::RGBA;
}

impl RGBAExt for gdk::RGBA {
    fn bytes(bytes: [u8; 4]) -> gdk::RGBA {
        gdk::RGBA {
            red: bytes[0] as f64 / 255.0,
            green: bytes[1] as f64 / 255.0,
            blue: bytes[2] as f64 / 255.0,
            alpha: bytes[3] as f64 / 255.0
        }
    }
}
