use std::sync;

use gtk::gio;
use gtk::glib;
use gtk::glib::clone;
use gtk::prelude::*;
use gtk::subclass::prelude::*;

use crate::model::document;
use crate::model::selection;
use crate::view::helpers;
use crate::view::hierarchy;

mod imp {
    use std::cell;
    use std::sync;
    
    use gtk::gio;
    use gtk::glib;
    use gtk::subclass::prelude::*;
    use gtk::prelude::*;
    
    use crate::model::selection::hierarchy as hierarchy_model;
    use crate::model::selection::hierarchy::Mode;
    use crate::model::versioned::Versioned;
    use crate::view::helpers;
    use crate::view::hierarchy::NodeItem;

    pub struct HierarchySelectionModelInterior {
        pub tree_model: gtk::TreeListModel,

        pub selection: sync::Arc<hierarchy_model::Selection>,
        pub selection_host: sync::Arc<hierarchy_model::Host>,
        pub selection_subscriber: helpers::AsyncSubscriber,
    }

    impl HierarchySelectionModelInterior {
        fn item(&self, position: u32) -> Option<NodeItem> {
            self.tree_model.item(position).map(|item| item.downcast::<gtk::TreeListRow>().unwrap().item().unwrap().downcast().unwrap())
        }
    }
    
    #[derive(Default)]
    pub struct HierarchySelectionModel {
        pub interior: cell::RefCell<Option<HierarchySelectionModelInterior>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for HierarchySelectionModel {
        const NAME: &'static str = "CharmHierarchySelectionModel";
        type Type = super::HierarchySelectionModel;
        type Interfaces = (gio::ListModel, gtk::SelectionModel,);
    }

    impl ObjectImpl for HierarchySelectionModel {
    }

    impl HierarchySelectionModel {
        pub fn borrow_interior_mut(&self) -> Option<std::cell::RefMut<'_, HierarchySelectionModelInterior>> {
            std::cell::RefMut::filter_map(self.interior.borrow_mut(), Option::as_mut).ok()
        }

        pub fn borrow_interior(&self) -> Option<std::cell::Ref<'_, HierarchySelectionModelInterior>> {
            std::cell::Ref::filter_map(self.interior.borrow(), Option::as_ref).ok()
        }

        fn change(&self, interior: std::cell::RefMut<'_, HierarchySelectionModelInterior>, change: hierarchy_model::Change) {
            if let Ok(new_selection) = interior.selection_host.change(change) {
                self.obj().update(interior, new_selection)
            } else {
                // TODO: these shouldn't really fail... log this somewhere?
            }            
        }
    }

    impl SelectionModelImpl for HierarchySelectionModel {
        fn selection_in_range(&self, position: u32, n_items: u32) -> gtk::Bitset {
            self.borrow_interior().map_or_else(|| gtk::Bitset::new_empty(), |i| {
                match &i.selection.mode {
                    Mode::Empty => gtk::Bitset::new_empty(),
                    Mode::Single(path) => {
                        let matched_position = (position..position+n_items).into_iter().find_map(|position| match i.item(position) {
                            None => Some(None),
                            Some(item) => {
                                if &item.imp().info.get().unwrap().borrow().path == path {
                                    Some(Some(position))
                                } else {
                                    None
                                }
                            }
                        });

                        match matched_position {
                            Some(None) | None => gtk::Bitset::new_empty(),
                            Some(Some(pos)) => gtk::Bitset::new_range(pos-position, 1)
                        }
                    },
                    Mode::SiblingRange(_, _, _) => todo!(),
                    Mode::All => gtk::Bitset::new_range(0, n_items),
                }
            })
        }

        fn is_selected(&self, position: u32) -> bool {
            self.borrow_interior().map_or(false, |i| match &i.selection.mode {
                Mode::Empty => false,
                Mode::Single(path) => i.item(position).map_or(false, |item| {
                    &item.imp().info.get().unwrap().borrow().path == path
                }),
                Mode::SiblingRange(path, first, last) => i.item(position).map_or(false, |item| {
                    let query_path = &item.imp().info.get().unwrap().borrow().path;
                    query_path[0..std::cmp::min(path.len(), query_path.len())] == path[..] && query_path.get(path.len()).map_or(false, |sibling| sibling >= first && sibling <= last)
                }),
                Mode::All => true,
            })
        }

        fn select_all(&self) -> bool {
            if let Some(interior) = self.borrow_interior_mut() {
                self.change(interior, hierarchy_model::Change::SelectAll);
                true
            } else {
                true
            }
        }

        fn select_item(&self, position: u32, unselect_rest: bool) -> bool {
            if let Some(interior) = self.borrow_interior_mut() {
                let item = match interior.item(position) {
                    Some(item) => item,
                    None if unselect_rest => {
                        self.change(interior, hierarchy_model::Change::Clear);
                        return true
                    },
                    None => return true
                };

                let info = item.imp().info.get().unwrap().borrow();
                
                match (&interior.selection.mode, unselect_rest) {
                    (Mode::Empty, _) | (_, true) => {
                        self.change(interior, hierarchy_model::Change::SetSingle(info.document.clone(), info.path.clone()));
                    },
                    (Mode::All, false) => {},
                    (_, false) => {
                        self.change(interior, hierarchy_model::Change::AddSingle(info.document.clone(), info.path.clone()));
                    }
                }
                
                true
            } else {
                true
            }
        }

        fn select_range(&self, position: u32, n_items: u32, unselect_rest: bool) -> bool {            
            if let Some(interior) = self.borrow_interior_mut() {
                if n_items == 0 {
                    if unselect_rest {
                        self.change(interior, hierarchy_model::Change::Clear);
                    }
                    return true;
                }

                if n_items == 1 {
                    drop(interior);
                    return self.select_item(position, unselect_rest);
                }

                let first_item = match interior.item(position) {
                    Some(item) => item,
                    None => return true
                };

                let last_item = match interior.item(position + n_items - 1) {
                    Some(item) => item,
                    None => return true
                };

                let first_info = first_item.imp().info.get().unwrap().borrow();
                let last_info = last_item.imp().info.get().unwrap().borrow();

                if first_info.document.generation() != last_info.document.generation() {
                    println!("WARNING: node infos were on different outdated documents");
                    println!("  first_info: {:?}, gen {}", first_info, first_info.document.generation());
                    println!("  last_info: {:?}, gen {}", last_info, last_info.document.generation());
                    return false;
                }
                
                let begin_path = first_info.path.clone();
                let end_path = last_info.path.clone();
                    
                self.change(interior, match unselect_rest {
                    true  => hierarchy_model::Change::SetRange(first_info.document.clone(), begin_path, end_path),
                    false => hierarchy_model::Change::AddRange(first_info.document.clone(), begin_path, end_path),
                });
                
                true
            } else {
                true
            }
        }

        fn set_selection(&self, _selected: &gtk::Bitset, _mask: &gtk::Bitset) -> bool {
            false
        }

        fn unselect_all(&self) -> bool {
            if let Some(interior) = self.borrow_interior_mut() {
                self.change(interior, hierarchy_model::Change::Clear);
                true
            } else {
                true
            }
        }

        fn unselect_item(&self, _position: u32) -> bool {
            false
        }

        fn unselect_range(&self, _position: u32, _n_items: u32) -> bool {
            false
        }
    }

    impl ListModelImpl for HierarchySelectionModel {
        fn item_type(&self) -> glib::Type {
            gtk::TreeListRow::static_type()
        }

        fn n_items(&self) -> u32 {
            self.borrow_interior().map_or(0, |i| i.tree_model.n_items())
        }

        fn item(&self, position: u32) -> Option<glib::Object> {
            self.borrow_interior().and_then(|i| {
                i.tree_model.item(position)
            })
        }
    }
}

glib::wrapper! {
    pub struct HierarchySelectionModel(ObjectSubclass<imp::HierarchySelectionModel>)
        @implements gio::ListModel, gtk::SelectionModel;
}

impl HierarchySelectionModel {
    pub fn new(selection_host: sync::Arc<selection::hierarchy::Host>, document_host: sync::Arc<document::DocumentHost>) -> HierarchySelectionModel {
        let selection = selection_host.get();
        
        let model: HierarchySelectionModel = glib::Object::builder().build();

        let tree_model = hierarchy::create_tree_list_model(document_host.clone(), selection.document.clone(), true);
        tree_model.connect_items_changed(clone!(@weak model => move |_, pos, removed, added| model.items_changed(pos, removed, added)));

        let selection_subscriber = helpers::subscribe_to_updates(model.downgrade(), selection_host.clone(), selection.clone(), move |model, new_selection| {
            if let Some(interior) = model.imp().borrow_interior_mut() {
                model.update(interior, new_selection.clone());
            }
        });
        
        *model.imp().interior.borrow_mut() = Some(imp::HierarchySelectionModelInterior {
            tree_model,

            selection,
            selection_host,
            selection_subscriber,
        });

        model
    }
    
    pub fn selection(&self) -> sync::Arc<selection::HierarchySelection> {
        self.imp().borrow_interior().unwrap().selection.clone()
    }

    fn update(&self, mut interior: std::cell::RefMut<'_, imp::HierarchySelectionModelInterior>, new_selection: sync::Arc<selection::HierarchySelection>) {
        interior.selection = new_selection;
        
        let n_items = interior.tree_model.n_items();
        drop(interior);
        
        self.selection_changed(0, n_items);
    }
}
