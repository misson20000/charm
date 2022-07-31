#[allow(non_camel_case_types)]
pub enum Key {
    _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, a, b, c, d, e, f,
    Unrecognized
}

#[cfg(feature = "gtk")]
impl From<&gdk::EventKey> for Key {
    fn from(key: &gdk::EventKey) -> Key {
        match key.keyval() {
            _ if !key.state().is_empty() => Key::Unrecognized,
            gdk::keys::constants::_0 => Key::_0,
            gdk::keys::constants::_1 => Key::_1,
            gdk::keys::constants::_2 => Key::_2,
            gdk::keys::constants::_3 => Key::_3,
            gdk::keys::constants::_4 => Key::_4,
            gdk::keys::constants::_5 => Key::_5,
            gdk::keys::constants::_6 => Key::_6,
            gdk::keys::constants::_7 => Key::_7,
            gdk::keys::constants::_8 => Key::_8,
            gdk::keys::constants::_9 => Key::_9,
            gdk::keys::constants::a => Key::a,
            gdk::keys::constants::b => Key::b,
            gdk::keys::constants::c => Key::c,
            gdk::keys::constants::d => Key::d,
            gdk::keys::constants::e => Key::e,
            gdk::keys::constants::f => Key::f,
            _ => Key::Unrecognized
        }
    }
}
