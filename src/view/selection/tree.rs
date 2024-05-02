use std::rc;
use std::sync;

use gtk::gio;
use gtk::glib;
use gtk::glib::clone;
use gtk::prelude::*;
use gtk::subclass::prelude::*;

use crate::model::document;
use crate::model::selection;
use crate::model::versioned::Versioned;
use crate::view::helpers;
use crate::view::hierarchy;
use crate::view::window;

mod imp {
    use std::cell;
    use std::rc;
    use std::sync;
    
    use gtk::gio;
    use gtk::glib;
    use gtk::subclass::prelude::*;
    use gtk::prelude::*;
    
    use crate::model::selection::tree as tree_model;
    use crate::model::versioned::Versioned;
    use crate::view::error;
    use crate::view::helpers;
    use crate::view::hierarchy::NodeItem;
    use crate::view::window;

    pub struct TreeSelectionModelInterior {
        pub gtk_model: gtk::TreeListModel,
        pub window: rc::Weak<window::CharmWindow>,
        
        pub selection: sync::Arc<tree_model::Selection>,
        pub selection_host: sync::Arc<tree_model::Host>,
        pub selection_subscriber: helpers::AsyncSubscriber,
    }

    impl TreeSelectionModelInterior {
        fn item(&self, position: u32) -> Option<NodeItem> {
            self.gtk_model.item(position).map(|item| item.downcast::<gtk::TreeListRow>().unwrap().item().unwrap().downcast().unwrap())
        }
    }
    
    #[derive(Default)]
    pub struct TreeSelectionModel {
        pub interior: cell::RefCell<Option<TreeSelectionModelInterior>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for TreeSelectionModel {
        const NAME: &'static str = "CharmTreeSelectionModel";
        type Type = super::TreeSelectionModel;
        type Interfaces = (gio::ListModel, gtk::SelectionModel,);
    }

    impl ObjectImpl for TreeSelectionModel {
    }

    impl TreeSelectionModel {
        pub fn borrow_interior_mut(&self) -> Option<std::cell::RefMut<'_, TreeSelectionModelInterior>> {
            std::cell::RefMut::filter_map(self.interior.borrow_mut(), Option::as_mut).ok()
        }

        pub fn borrow_interior(&self) -> Option<std::cell::Ref<'_, TreeSelectionModelInterior>> {
            std::cell::Ref::filter_map(self.interior.borrow(), Option::as_ref).ok()
        }

        fn change(&self, interior: std::cell::RefMut<'_, TreeSelectionModelInterior>, change: tree_model::Change) {
            if let Some(window) = interior.window.upgrade() {
                match interior.selection_host.change(change) {
                    Ok(new_selection) => self.obj().update_selection(interior, new_selection),
                    Err((error, attempted_version)) => {
                        window.report_error(error::Error {
                            while_attempting: error::Action::ModifyTreeSelection,
                            trouble: error::Trouble::TreeSelectionUpdateFailure {
                                error,
                                attempted_version
                            },
                            level: error::Level::Error,
                            is_bug: true,
                        });
                    }
                }
            }
        }

        fn change_multiple<F: FnOnce(&mut TreeSelectionModelInterior) -> Result<sync::Arc<tree_model::Selection>, (tree_model::ApplyError, sync::Arc<tree_model::Selection>)>>(&self, mut interior: std::cell::RefMut<'_, TreeSelectionModelInterior>, cb: F) {
            if let Some(window) = interior.window.upgrade() {
                match cb(&mut *interior) {
                    Ok(new_selection) => self.obj().update_selection(interior, new_selection),
                    Err((error, attempted_version)) => {
                        window.report_error(error::Error {
                            while_attempting: error::Action::ModifyTreeSelection,
                            trouble: error::Trouble::TreeSelectionUpdateFailure {
                                error,
                                attempted_version
                            },
                            level: error::Level::Error,
                            is_bug: true,
                        });
                    }
                }
            }
        }
    }

    impl SelectionModelImpl for TreeSelectionModel {
        fn selection_in_range(&self, _position: u32, _n_items: u32) -> gtk::Bitset {
            /* FFI CALLBACK */
            todo!();
        }

        fn is_selected(&self, position: u32) -> bool {
            /* FFI CALLBACK */
            self.borrow_interior_mut().map_or(false, |i| {
                i.item(position).map_or(false, |item| {
                    let info = item.imp().info.get().unwrap().borrow();
                    
                    if i.selection.document.is_outdated(&info.document) {
                        panic!("selection document was outdated!");
                    }
                    
                    i.selection.path_selected(&info.path)
                })
            })
        }

        fn select_all(&self) -> bool {
            /* FFI CALLBACK */
            if let Some(interior) = self.borrow_interior_mut() {
                self.change(interior, tree_model::Change::SelectAll);
                true
            } else {
                true
            }
        }

        fn select_item(&self, position: u32, unselect_rest: bool) -> bool {
            /* FFI CALLBACK */
            if let Some(interior) = self.borrow_interior_mut() {
                let item = match interior.item(position) {
                    Some(item) => item,
                    None if unselect_rest => {
                        self.change(interior, tree_model::Change::Clear);
                        return true
                    },
                    None => return true
                };

                let info = item.imp().info.get().unwrap().borrow();
                
                let document = info.document.clone();
                let path = info.path.clone();

                std::mem::drop(info);
                
                self.change(interior, if unselect_rest {
                    tree_model::Change::SetSingle(document, path)
                } else {
                    tree_model::Change::AddSingle(document, path)
                });
                
                true
            } else {
                true
            }
        }

        fn select_range(&self, position: u32, n_items: u32, unselect_rest: bool) -> bool {
            /* FFI CALLBACK */
            if let Some(interior) = self.borrow_interior_mut() {
                self.change_multiple(interior, |interior| {
                    let mut sel = if unselect_rest {
                        interior.selection_host.change(tree_model::Change::Clear)?
                    } else {
                        interior.selection.clone()
                    };
                    
                    for i in position..(position+n_items) {
                        let item = match interior.item(i) {
                            Some(item) => item,
                            None => continue
                        };

                        let info = item.imp().info.get().unwrap().borrow();

                        sel = interior.selection_host.change(tree_model::Change::AddSingle(info.document.clone(), info.path.clone()))?;
                    }

                    Ok(sel)
                });

                true
            } else {
                true
            }
        }

        fn set_selection(&self, _selected: &gtk::Bitset, _mask: &gtk::Bitset) -> bool {
            /* FFI CALLBACK: trivially panic-safe */
            false
        }

        fn unselect_all(&self) -> bool {
            /* FFI CALLBACK */
            if let Some(interior) = self.borrow_interior_mut() {
                self.change(interior, tree_model::Change::Clear);
                true
            } else {
                true
            }
        }

        fn unselect_item(&self, _position: u32) -> bool {
            /* FFI CALLBACK: trivially panic-safe */
            false
        }

        fn unselect_range(&self, _position: u32, _n_items: u32) -> bool {
            /* FFI CALLBACK: trivially panic-safe */
            false
        }
    }

    impl ListModelImpl for TreeSelectionModel {
        fn item_type(&self) -> glib::Type {
            /* FFI CALLBACK: assumed panic-safe */
            gtk::TreeListRow::static_type()
        }

        fn n_items(&self) -> u32 {
            /* FFI CALLBACK */
            self.borrow_interior().map_or(0, |i| i.gtk_model.n_items())
        }

        fn item(&self, position: u32) -> Option<glib::Object> {
            /* FFI CALLBACK */
            self.borrow_interior().and_then(|i| {
                i.gtk_model.item(position)
            })
        }
    }
}

glib::wrapper! {
    pub struct TreeSelectionModel(ObjectSubclass<imp::TreeSelectionModel>)
        @implements gio::ListModel, gtk::SelectionModel;
}

impl TreeSelectionModel {
    pub fn new(window: &rc::Rc<window::CharmWindow>, selection_host: sync::Arc<selection::tree::Host>, document_host: sync::Arc<document::DocumentHost>) -> TreeSelectionModel {
        let selection = selection_host.get();
        
        let model: TreeSelectionModel = glib::Object::builder().build();

        let gtk_model = hierarchy::create_tree_list_model(document_host.clone(), selection.document.clone(), true);
        gtk_model.connect_items_changed(clone!(@weak model => move |_, pos, removed, added| {
            /* FFI CALLBACK */
            model.items_changed(pos, removed, added)
        }));

        let selection_subscriber = helpers::subscribe_to_updates(model.downgrade(), selection_host.clone(), selection.clone(), move |model, new_selection| {
            if let Some(interior) = model.imp().borrow_interior_mut() {
                model.update_selection(interior, new_selection.clone());
            }
        });
        
        *model.imp().interior.borrow_mut() = Some(imp::TreeSelectionModelInterior {
            gtk_model,
            window: rc::Rc::downgrade(window),

            selection,
            selection_host,
            selection_subscriber,
        });

        model
    }
    
    pub fn selection(&self) -> sync::Arc<selection::TreeSelection> {
        self.imp().borrow_interior().unwrap().selection.clone()
    }
    
    fn update_selection(&self, mut interior: std::cell::RefMut<'_, imp::TreeSelectionModelInterior>, new_selection: sync::Arc<selection::TreeSelection>) {
        let mut selection_changed = false;
        
        new_selection.changes_since(&interior.selection.clone(), &mut |new_selection, record| {
            interior.selection = new_selection.clone();
            selection_changed = selection_changed || record.selection_changed;
        });

        let tlm = interior.gtk_model.clone();
        let rlm = interior.gtk_model.model().downcast::<hierarchy::RootListModel>().unwrap();
        let document = interior.selection.document.clone();
        drop(interior);
        
        rlm.update_document(&document);

        if selection_changed {
            self.selection_changed(0, tlm.n_items());
        }
    }
}
