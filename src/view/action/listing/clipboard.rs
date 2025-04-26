use std::cell;
use std::rc;
use std::sync;

use crate::catch_panic;
use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::view::error;
use crate::view::helpers;
use crate::view::listing;
use crate::view::window;
use crate::view::window::ErrorReporter;

use glib::clone;
use gtk::prelude::*;
use glib::subclass::prelude::*;
use gtk::gdk;
use gtk::gio;

struct ClipboardActions {
    lw: listing::ListingWidget,
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::listing::Host>,
    selection: cell::RefCell<sync::Arc<selection::ListingSelection>>,
    subscriber: helpers::AsyncSubscriber,
    
    cut_action: glib::WeakRef<gio::SimpleAction>,
    copy_action: glib::WeakRef<gio::SimpleAction>,
    paste_action: glib::WeakRef<gio::SimpleAction>,
    
    clipboard: gdk::Clipboard,
    clipboard_signal_handler: Option<glib::signal::SignalHandlerId>,
}

glib::wrapper! {
    pub struct CharmStructureCopy(ObjectSubclass<imp::CharmStructureCopy>);
}

#[derive(Clone, Copy, Debug)]
enum SerializationError {
    Unimplemented
}

impl glib::error::ErrorDomain for SerializationError {
    fn domain() -> glib::Quark {
        glib::Quark::from_str("charm-serialization")
    }

    fn code(self) -> i32 {
        match self {
            Self::Unimplemented => 0,
        }
    }

    fn from(code: i32) -> Option<Self> {
        Some(match code {
            0 => Self::Unimplemented,
            _ => return None
        })
    }
}

pub fn init() {
    gdk::content_register_serializer(CharmStructureCopy::static_type(), "application/charm-structure", |serializer, _value: &mut Option<CharmStructureCopy>| {
        serializer.return_error(glib::Error::new(SerializationError::Unimplemented, "serialization is unimplemented"));
    });
}

pub fn add_actions(window_context: &window::WindowContext) {
    let gio_cut_action = gio::SimpleAction::new("cut", None);
    let gio_copy_action = gio::SimpleAction::new("copy", None);
    let gio_paste_action = gio::SimpleAction::new("paste", None);
    
    let action_impl = ClipboardActions::new(
        window_context,
        gio_cut_action.clone(),
        gio_copy_action.clone(),
        gio_paste_action.clone()
    );
    
    let window = window_context.window.clone();
    action_impl.update_is_enabled();
    
    gio_cut_action.connect_activate(clone!(#[strong] window, #[strong] action_impl, move |_, _| catch_panic! {
        let Some(window) = window.upgrade() else { return };
        
        if let Err(e) = action_impl.activate_cut() {
            window.report_error(e);
        }
    }));

    gio_copy_action.connect_activate(clone!(#[strong] window, #[strong] action_impl, move |_, _| catch_panic! {
        let Some(window) = window.upgrade() else { return };
        
        if let Err(e) = action_impl.activate_copy() {
            window.report_error(e);
        }
    }));

    gio_paste_action.connect_activate(clone!(#[strong] window, #[strong] action_impl, move |_, _| catch_panic! {
        let Some(window) = window.upgrade() else { return };
        
        action_impl.activate_paste(window)
    }));

    window_context.action_group.add_action(&gio_cut_action);
    window_context.action_group.add_action(&gio_copy_action);
    window_context.action_group.add_action(&gio_paste_action);
}

impl ClipboardActions {
    fn new(window_context: &window::WindowContext, cut_action: gio::SimpleAction, copy_action: gio::SimpleAction, paste_action: gio::SimpleAction) -> rc::Rc<Self> {
        let document_host = window_context.project.document_host.clone();
        let selection_host = window_context.listing_selection_host.clone();
        let selection = selection_host.get();
        let clipboard = window_context.window.upgrade().unwrap().window.clipboard();
        
        rc::Rc::new_cyclic(|weak: &rc::Weak<Self>| Self {
            subscriber: helpers::subscribe_to_updates(weak.clone(), selection_host.clone(), selection.clone(), |action, new_sel| {
                action.selection_updated(new_sel);
            }),

            lw: window_context.lw.clone(),
            document_host,
            selection_host,
            selection: cell::RefCell::new(selection),
            cut_action: cut_action.downgrade(),
            copy_action: copy_action.downgrade(),
            paste_action: paste_action.downgrade(),
            clipboard_signal_handler: Some(clipboard.connect_changed(clone!(#[strong] weak, move |_clipboard| catch_panic! {
                if let Some(actions) = weak.upgrade() {
                    actions.update_is_enabled()
                }
            }))),
            clipboard,
        })
    }

    fn selection_updated(&self, new_sel: &sync::Arc<selection::ListingSelection>) {
        *self.selection.borrow_mut() = new_sel.clone();
        self.update_is_enabled();
    }

    fn update_is_enabled(&self) {
        let cut_copy_enabled = match &self.selection.borrow().mode {
            selection::listing::Mode::Structure(selection::listing::StructureMode::Range(_)) => true,
            _ => false
        };
        
        if let Some(cut_action) = self.cut_action.upgrade() {
            cut_action.set_enabled(cut_copy_enabled);
        }

        if let Some(copy_action) = self.copy_action.upgrade() {
            copy_action.set_enabled(cut_copy_enabled);
        }

        if let Some(paste_action) = self.paste_action.upgrade() {
            paste_action.set_enabled(self.clipboard.formats().contains_type(CharmStructureCopy::static_type()));
        }
    }

    fn activate_cut(&self) -> Result<(), error::Error> {
        let selection = self.selection.borrow();
        
        self.clipboard.set(&CharmStructureCopy::from_selection(&*selection)?.to_value());

        match &selection.mode {
            selection::listing::Mode::Structure(selection::listing::StructureMode::Range(range)) if range.begin.1 != range.end.1 => {
                /* Only try to delete if there were actually any nodes selected */
                self.document_host.change(selection.document.delete_range(range.clone().to_sibling_range_and_extent().unwrap().0)).map_err(|(error, attempted_version)| error::Error {
                    while_attempting: error::Action::CutStructure,
                    trouble: error::Trouble::DocumentUpdateFailure {
                        error,
                        attempted_version
                    },
                    level: error::Level::Error,
                    is_bug: false,
                }).map(|_| {})
            },
            _ => Ok(())
        }
    }
    
    fn activate_copy(&self) -> Result<(), error::Error> {
        self.clipboard.set(&CharmStructureCopy::from_selection(&*self.selection.borrow())?.to_value());
        Ok(())
    }
    
    fn activate_paste(&self, window: rc::Rc<window::CharmWindow>) {
        let lw = self.lw.clone();
        let document_host = self.document_host.clone();
        
        self.clipboard.read_value_async(CharmStructureCopy::static_type(), glib::source::Priority::LOW, gio::Cancellable::NONE, move |result| catch_panic! {
            let copy: CharmStructureCopy = match result.map_err(error::Trouble::GlibError).and_then(|v| v.get().map_err(|e| match e {
                glib::value::ValueTypeMismatchOrNoneError::WrongValueType(_) => error::Trouble::ClipboardNotContainingStructure,
                glib::value::ValueTypeMismatchOrNoneError::UnexpectedNone => error::Trouble::ClipboardEmpty,
            })) {
                Ok(copy) => copy,
                Err(trouble) => { window.report_error(error::Error {
                    while_attempting: error::Action::PasteStructure,
                    trouble,
                    level: error::Level::Error,
                    is_bug: false,
                }); return }
            };

            let interior = copy.imp().interior.get().unwrap();
            let mut cursor = lw.cursor_mut();

            if let Err((error, attempted_version)) = cursor.paste(&document_host, interior.parent.clone(), interior.begin, interior.end) {
                window.report_error(error::Error {
                    while_attempting: error::Action::PasteStructure,
                    trouble: error::Trouble::DocumentUpdateFailure {
                        error,
                        attempted_version
                    },
                    level: error::Level::Error,
                    is_bug: false,
                });
            }
        });
    }
}

impl std::ops::Drop for ClipboardActions {
    fn drop(&mut self) {
        if let Some(handler) = self.clipboard_signal_handler.take() {
            glib::signal::signal_handler_disconnect(&self.clipboard, handler)
        }
    }
}

mod imp {
    use super::*;

    #[derive(Debug)]
    pub struct CharmStructureCopyInterior {
        pub parent: sync::Arc<structure::Node>,
        pub begin: (addr::Offset, usize),
        pub end: (addr::Offset, usize), //< exclusive
    }
    
    #[derive(Default)]
    pub struct CharmStructureCopy {
        pub interior: cell::OnceCell<CharmStructureCopyInterior>
    }

    #[glib::object_subclass]
    impl ObjectSubclass for CharmStructureCopy {
        const NAME: &'static str = "CharmStructureCopy";
        type Type = super::CharmStructureCopy;
        type ParentType = glib::Object;
    }

    impl ObjectImpl for CharmStructureCopy {
    }
}

impl CharmStructureCopy {
    fn from_selection(selection: &sync::Arc<selection::ListingSelection>) -> Result<Self, error::Error> {
        let range = match &selection.mode {
            selection::listing::Mode::Structure(selection::listing::StructureMode::Range(range)) => range,
            _ => return Err(error::Error {
                while_attempting: error::Action::CopyStructure,
                trouble: error::Trouble::UnsupportedListingSelectionMode {
                    selection: selection.clone()
                },
                level: error::Level::Warning,
                is_bug: true, /* should not be possible to activate action when the selection is invalid for it */
            })
        };

        let obj = glib::Object::new::<CharmStructureCopy>();
        obj.imp().interior.set(imp::CharmStructureCopyInterior {
            parent: selection.document.lookup_node(&range.path).0.clone(),
            begin: range.begin,
            end: range.end,
        }).unwrap();

        Ok(obj)
    }
}
