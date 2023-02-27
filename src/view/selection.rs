use std::sync;

use gtk::gio;
use gtk::glib;
use gtk::glib::clone;
use gtk::prelude::*;
use gtk::subclass::prelude::*;

use crate::model::document;
use crate::model::document::structure;
use crate::view::helpers;
use crate::view::hierarchy;

#[derive(Debug, Clone)]
pub enum SelectionMode {
    Empty,
    Single(structure::Path),
    SiblingRange(structure::Path, usize, usize),
    All,
}

impl SelectionMode {
    fn new_range_between(begin: &Vec<usize>, end: &Vec<usize>) -> SelectionMode {
        if begin.is_empty() || end.is_empty() {
            return SelectionMode::All;
        }
        
        let shared_prefix: Vec<usize> = (&begin[0..begin.len()-1])
            .iter()
            .zip((&end[0..end.len()-1]).iter())
            .map_while(|(x, y)| if x == y { Some(*x) } else { None })
            .collect();
        
        let sp_len = shared_prefix.len();
        
        SelectionMode::SiblingRange(shared_prefix, begin[sp_len], end[sp_len])
    }
}

mod imp {
    use std::cell;
    use std::sync;
    
    use gtk::gio;
    use gtk::glib;
    use gtk::subclass::prelude::*;
    use gtk::prelude::*;
    
    use crate::model::document;
    use crate::model::document::change;
    use crate::model::versioned::Versioned;
    use crate::view::helpers;
    use crate::view::hierarchy::NodeItem;

    use super::SelectionMode;

    pub struct StructureSelectionModelInterior {
        pub tree_model: gtk::TreeListModel,
        pub mode: SelectionMode,
        pub document: sync::Arc<document::Document>,
        pub document_host: sync::Arc<document::DocumentHost>,
        pub subscriber: helpers::AsyncSubscriber,
    }

    impl StructureSelectionModelInterior {
        fn item(&self, position: u32) -> Option<NodeItem> {
            self.tree_model.item(position).map(|item| item.downcast::<gtk::TreeListRow>().unwrap().item().unwrap().downcast().unwrap())
        }

        fn port_change<'a>(&mut self, new_doc: &sync::Arc<document::Document>, change: &change::Change) {
            self.document = new_doc.clone();
            
            self.mode = match std::mem::replace(&mut self.mode, SelectionMode::Empty) {
                SelectionMode::Empty => SelectionMode::Empty,
                SelectionMode::Single(mut path) => match change.update_path(&mut path) {
                    change::UpdatePathResult::Moved | change::UpdatePathResult::Unmoved => SelectionMode::Single(path),
                    change::UpdatePathResult::Deleted => SelectionMode::Empty,
                },
                SelectionMode::SiblingRange(mut path, mut first_child, mut last_child) => match change.update_path(&mut path) {
                    change::UpdatePathResult::Moved | change::UpdatePathResult::Unmoved => match &change.ty {
                        change::ChangeType::AlterNode(_, _) => SelectionMode::SiblingRange(path, first_child, last_child),
                        change::ChangeType::InsertNode(affected_path, insertion_index, _, _) if affected_path == &path && (first_child..=last_child).contains(insertion_index) => SelectionMode::SiblingRange(path, first_child, last_child+1),
                        change::ChangeType::InsertNode(_, _, _, _) => SelectionMode::SiblingRange(path, first_child, last_child),
                        change::ChangeType::Nest(affected_path, _nested_first, _nested_last, _props) if affected_path == &path => {
                            // TODO: smarter logic here
                            path.push(first_child);
                            SelectionMode::Single(path)
                        },
                        change::ChangeType::Nest(_, _, _, _) => SelectionMode::SiblingRange(path, first_child, last_child),
                        change::ChangeType::DeleteRange(affected_path, deleted_first, deleted_last) if affected_path == &path => {
                            let deleted = *deleted_first..=*deleted_last;
                            if deleted.contains(&first_child) && deleted.contains(&last_child) {
                                SelectionMode::Empty
                            } else {
                                if deleted.contains(&first_child) {
                                    first_child = *deleted_first;
                                } else if first_child > *deleted_last {
                                    first_child-= (*deleted_last-*deleted_first)+1;
                                }
                                
                                if deleted.contains(&last_child) {
                                    last_child = *deleted_first-1;
                                } else if last_child > *deleted_last {
                                    last_child-= (*deleted_last-*deleted_first)+1;
                                }

                                SelectionMode::SiblingRange(path, first_child, last_child)
                            }
                        },
                        change::ChangeType::DeleteRange(_, _, _) => SelectionMode::SiblingRange(path, first_child, last_child),
                    },
                    change::UpdatePathResult::Deleted => SelectionMode::Empty,
                },
                SelectionMode::All => SelectionMode::All,
            };
        }
    }
    
    #[derive(Default)]
    pub struct StructureSelectionModel {
        pub interior: cell::RefCell<Option<StructureSelectionModelInterior>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for StructureSelectionModel {
        const NAME: &'static str = "CharmStructureSelectionModel";
        type Type = super::StructureSelectionModel;
        type Interfaces = (gio::ListModel, gtk::SelectionModel,);
    }

    impl ObjectImpl for StructureSelectionModel {
    }

    impl StructureSelectionModel {
        pub fn borrow_interior_mut(&self) -> Option<std::cell::RefMut<'_, StructureSelectionModelInterior>> {
            std::cell::RefMut::filter_map(self.interior.borrow_mut(), Option::as_mut).ok()
        }

        pub fn borrow_interior(&self) -> Option<std::cell::Ref<'_, StructureSelectionModelInterior>> {
            std::cell::Ref::filter_map(self.interior.borrow(), Option::as_ref).ok()
        }
        
        fn notify_all_changed(&self, interior: std::cell::RefMut<'_, StructureSelectionModelInterior>) {
            let n_items = interior.tree_model.n_items();
            drop(interior);
            self.obj().selection_changed(0, n_items);
        }
        
        pub fn update(&self, new_doc: &sync::Arc<document::Document>) {
            if let Some(mut i) = self.borrow_interior_mut() {
                new_doc.changes_since(&i.document.clone(), &mut |new_doc, change| i.port_change(new_doc, change));
                self.notify_all_changed(i);
            }
        }
    }
    
    impl SelectionModelImpl for StructureSelectionModel {
        fn selection_in_range(&self, position: u32, n_items: u32) -> gtk::Bitset {
            self.borrow_interior().map_or_else(|| gtk::Bitset::new_empty(), |i| {
                match &i.mode {
                    SelectionMode::Empty => gtk::Bitset::new_empty(),
                    SelectionMode::Single(path) => {
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
                    SelectionMode::SiblingRange(_, _, _) => todo!(),
                    SelectionMode::All => gtk::Bitset::new_range(0, n_items),
                }
            })
        }

        fn is_selected(&self, position: u32) -> bool {
            self.borrow_interior().map_or(false, |i| match &i.mode {
                SelectionMode::Empty => false,
                SelectionMode::Single(path) => i.item(position).map_or(false, |item| {
                    &item.imp().info.get().unwrap().borrow().path == path
                }),
                SelectionMode::SiblingRange(path, first, last) => i.item(position).map_or(false, |item| {
                    let query_path = &item.imp().info.get().unwrap().borrow().path;
                    query_path[0..std::cmp::min(path.len(), query_path.len())] == path[..] && query_path.get(path.len()).map_or(false, |sibling| sibling >= first && sibling <= last)
                }),
                SelectionMode::All => true,
            })
        }

        fn select_all(&self) -> bool {
            if let Some(mut interior) = self.borrow_interior_mut() {
                interior.mode = SelectionMode::All;
                self.notify_all_changed(interior);
                true
            } else {
                true
            }
        }

        fn select_item(&self, position: u32, unselect_rest: bool) -> bool {
            if let Some(mut interior) = self.borrow_interior_mut() {
                let item = match interior.item(position) {
                    Some(item) => item,
                    None if unselect_rest => {
                        interior.mode = SelectionMode::Empty;
                        self.notify_all_changed(interior);
                        return true
                    },
                    None => return true
                };

                match (&interior.mode, unselect_rest) {
                    (SelectionMode::Empty, _) | (_, true) => {
                        interior.mode = SelectionMode::Single(item.imp().info.get().unwrap().borrow().path.clone());
                        self.notify_all_changed(interior);
                    },
                    (SelectionMode::Single(current_path), _) => {
                        let item_path = &item.imp().info.get().unwrap().borrow().path;
                        
                        let begin = std::cmp::min(item_path, current_path);
                        let end = std::cmp::max(item_path, current_path);
                        interior.mode = SelectionMode::new_range_between(begin, end);
                        self.notify_all_changed(interior);
                    }
                    (SelectionMode::SiblingRange(_, _, _), _) => todo!(),
                    (SelectionMode::All, _) => {},
                }
                
                true
            } else {
                true
            }
        }

        fn select_range(&self, position: u32, n_items: u32, unselect_rest: bool) -> bool {            
            if let Some(mut interior) = self.borrow_interior_mut() {
                if n_items == 0 {
                    if unselect_rest {
                        interior.mode = SelectionMode::Empty;
                        self.notify_all_changed(interior);
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

                let mut begin_path = first_info.path.clone();
                let mut end_path = last_info.path.clone();
                
                if !unselect_rest {
                    match &interior.mode {
                        SelectionMode::Empty => {},
                        SelectionMode::Single(path) => {
                            begin_path = std::cmp::min(begin_path, path.clone());
                            end_path = std::cmp::max(end_path, path.clone());
                        },
                        SelectionMode::SiblingRange(prefix, begin_child, end_child) => {
                            let mut current_begin = prefix.clone();
                            let mut current_end = prefix.clone();
                            current_begin.push(*begin_child);
                            current_end.push(*end_child);
                            
                            begin_path = std::cmp::min(begin_path, current_begin);
                            end_path = std::cmp::max(end_path, current_end);
                        },
                        SelectionMode::All => return true,
                    }
                }
                
                interior.mode = SelectionMode::new_range_between(&begin_path, &end_path);
                self.notify_all_changed(interior);
                
                true
            } else {
                true
            }
        }

        fn set_selection(&self, _selected: &gtk::Bitset, _mask: &gtk::Bitset) -> bool {
            false
        }

        fn unselect_all(&self) -> bool {
            if let Some(mut interior) = self.borrow_interior_mut() {
                interior.mode = SelectionMode::Empty;
                self.notify_all_changed(interior);
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

    impl ListModelImpl for StructureSelectionModel {
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
    pub struct StructureSelectionModel(ObjectSubclass<imp::StructureSelectionModel>)
        @implements gio::ListModel, gtk::SelectionModel;
}

impl StructureSelectionModel {
    pub fn new(document_host: sync::Arc<document::DocumentHost>) -> StructureSelectionModel {
        let document = document_host.get();
        
        let model: StructureSelectionModel = glib::Object::builder().build();

        let tree_model = hierarchy::create_tree_list_model(document_host.clone(), document.clone(), true);
        tree_model.connect_items_changed(clone!(@weak model => move |_, pos, removed, added| model.items_changed(pos, removed, added)));

        let subscriber = helpers::subscribe_to_updates(model.downgrade(), document_host.clone(), document.clone(), move |model, new_document| {
            model.imp().update(new_document);
        });
        
        *model.imp().interior.borrow_mut() = Some(imp::StructureSelectionModelInterior {
            tree_model,
            mode: SelectionMode::Empty,
            document,
            document_host,
            subscriber,
        });

        model
    }
    
    pub fn selection_mode(&self) -> (SelectionMode, sync::Arc<document::Document>) {
        let i = self.imp().borrow_interior().unwrap();
        (i.mode.clone(), i.document.clone())
    }
}
