use crate::catch_panic;
use crate::model::addr;

use std::cell;

use gtk::prelude::*;
use gtk::subclass::prelude::*;
use gtk::glib;
use gtk::glib::translate::IntoGlib;

glib::wrapper! {
    pub struct AddrEntry(ObjectSubclass<imp::AddrEntry>)
        @extends gtk::Entry, gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::CellEditable, gtk::ConstraintTarget, gtk::Editable;
}

impl AddrEntry {
    pub fn new() -> Self {
        glib::Object::builder().build()
    }

    pub fn addr(&self) -> Result<addr::Offset, addr::AddressParseError> {
        (*self.imp().addr.borrow()).clone()
    }

    pub fn connect_addr_changed<F: Fn(&Self, Result<addr::Offset, addr::AddressParseError>) + 'static>(&self, f: F) -> glib::SignalHandlerId {
        self.connect_closure(
            "addr-changed",
            false,
            glib::closure_local!(move |entry: &AddrEntry| {
                let a = entry.addr();
                f(entry, a)
            }))
    }
}

#[no_mangle]
pub unsafe extern "C" fn charm_addr_entry_get_type() -> <glib::Type as IntoGlib>::GlibType {
    let _ = gtk::init();
    AddrEntry::static_type().into_glib()
}

mod imp {
    use super::*;

    pub struct AddrEntry {
        pub addr: cell::RefCell<Result<addr::Offset, addr::AddressParseError>>,
    }

    impl Default for AddrEntry {
        fn default() -> Self {
            Self {
                addr: cell::RefCell::new(Err(addr::AddressParseError::EmptyString)),
            }
        }
    }

    #[glib::object_subclass]
    impl ObjectSubclass for AddrEntry {
        const NAME: &'static str = "CharmAddrEntry";
        type Type = super::AddrEntry;
        type ParentType = gtk::Entry;
    }

    impl ObjectImpl for AddrEntry {
        fn constructed(&self) {
            self.parent_constructed();
            self.obj().connect_changed(move |this| catch_panic! {
                this.imp().refresh();
            });
        }
        
        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: std::sync::OnceLock<Vec<glib::subclass::Signal>> = std::sync::OnceLock::new();
            SIGNALS.get_or_init(|| {
                vec![glib::subclass::Signal::builder("addr-changed")
                     .build()]
            })
        }
    }

    impl WidgetImpl for AddrEntry {
    }

    impl EntryImpl for AddrEntry {
    }

    impl AddrEntry {
        fn refresh(&self) {
            match addr::Offset::parse(&self.obj().text(), false) {
                Ok(addr) => {
                    self.obj().set_css_classes(&[]);
                    self.obj().set_icon_from_icon_name(gtk::EntryIconPosition::Secondary, None);
                    self.obj().set_icon_tooltip_text(gtk::EntryIconPosition::Secondary, None);
                    *self.addr.borrow_mut() = Ok(addr);
                }
                Err(e) => {
                    self.obj().set_css_classes(&["error"]);
                    self.obj().set_icon_from_icon_name(gtk::EntryIconPosition::Secondary, Some("dialog-error"));

                    let error_string = match &e {
                        addr::AddressParseError::EmptyString => "cannot be empty".to_string(),
                        addr::AddressParseError::MalformedBytes(pie) => format!("failed to parse bytes: {}", pie),
                        addr::AddressParseError::MalformedBits(pie) => format!("failed to parse bits: {}", pie),
                        addr::AddressParseError::TooManyBits => "bit offset too large".to_string(),
                    };
                    
                    self.obj().set_icon_tooltip_text(gtk::EntryIconPosition::Secondary, Some(&error_string));
                    *self.addr.borrow_mut() = Err(e);
                }
            }

            self.obj().emit_by_name::<()>("addr-changed", &[]);
        }
    }
}
