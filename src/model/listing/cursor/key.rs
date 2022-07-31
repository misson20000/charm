#[cfg(feature="gtk")]
use gtk::gdk;

#[allow(non_camel_case_types)]
pub enum Key {
    _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, a, b, c, d, e, f,
    Unrecognized
}

#[cfg(feature = "gtk")]
impl From<&gdk::KeyEvent> for Key {
    fn from(key: &gdk::KeyEvent) -> Key {
        match key.keyval() {
            _ if !key.modifier_state().is_empty() => Key::Unrecognized,
            gdk::Key::_0 => Key::_0,
            gdk::Key::_1 => Key::_1,
            gdk::Key::_2 => Key::_2,
            gdk::Key::_3 => Key::_3,
            gdk::Key::_4 => Key::_4,
            gdk::Key::_5 => Key::_5,
            gdk::Key::_6 => Key::_6,
            gdk::Key::_7 => Key::_7,
            gdk::Key::_8 => Key::_8,
            gdk::Key::_9 => Key::_9,
            gdk::Key::a => Key::a,
            gdk::Key::b => Key::b,
            gdk::Key::c => Key::c,
            gdk::Key::d => Key::d,
            gdk::Key::e => Key::e,
            gdk::Key::f => Key::f,
            _ => Key::Unrecognized
        }
    }
}
