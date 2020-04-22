use std::cell;
use std::rc;
use std::sync;

use crate::CharmApplication;
use crate::space;
use crate::space::AddressSpace;
use crate::listing;
use crate::widget;

use gtk::prelude::*;
use gio::prelude::*;

pub struct WindowContext {
    window: rc::Weak<CharmWindow>,
    listing: sync::Arc<parking_lot::RwLock<listing::Listing>>,
    lw: sync::Arc<parking_lot::RwLock<widget::listing::ListingWidget>>,
}

pub struct CharmWindow {
    pub application: rc::Rc<CharmApplication>,
    window: gtk::ApplicationWindow,
    paned: gtk::Paned,
    context: cell::RefCell<Option<WindowContext>>,
}

impl CharmWindow {
    pub fn new(charm: &rc::Rc<CharmApplication>) -> rc::Rc<CharmWindow> {
        let window = gtk::ApplicationWindow::new(&charm.application);

        window.set_title("Charm");
        window.set_border_width(0);
        window.set_position(gtk::WindowPosition::Center);
        window.set_default_size(800, 600);

        let main_box = gtk::Box::new(gtk::Orientation::Vertical, 0);

        {
            let menu_bar = gio::Menu::new();
            {
                let file_menu = gio::Menu::new();
                file_menu.append(Some("New Window"), Some("app.new_window"));
                file_menu.append(Some("Open..."), Some("app.open"));
                file_menu.freeze();
                menu_bar.append_submenu(Some("File"), &file_menu);
            }
            {
                let edit_menu = gio::Menu::new();
                edit_menu.append(Some("Go to..."), Some("win-listing.goto"));
                edit_menu.append(Some("Insert or edit break..."), Some("win-listing.insert_break"));
                {
                    let mode_menu = gio::Menu::new();
                    mode_menu.append(Some("Command mode"), Some("win-listing.mode::command"));
                    mode_menu.append(Some("Hex entry"), Some("win-listing.mode::entry"));
                    mode_menu.append(Some("Text entry (UTF8)"), Some("win-listing.mode::utf8"));
                    edit_menu.append_section(Some("Edit Mode"), &mode_menu);
                    mode_menu.freeze();
                }
                edit_menu.freeze();
                menu_bar.append_submenu(Some("Edit"), &edit_menu);
            }
            {
                let help_menu = gio::Menu::new();
                help_menu.append(Some("About"), Some("app.about"));
                help_menu.freeze();
                menu_bar.append_submenu(Some("Help"), &help_menu);
            }
            menu_bar.freeze();
            
            let menu_bar_widget = gtk::MenuBar::new_from_model(&menu_bar);
            main_box.pack_start(&menu_bar_widget, false, false, 0);
        }

        let paned = gtk::Paned::new(gtk::Orientation::Horizontal);
        
        {
            let editor = widget::config_editor::build_config_editor();
            editor.set_size_request(400, -1);
            
            let frame = gtk::Frame::new(None);
            frame.set_shadow_type(gtk::ShadowType::In);
            frame.set_margin_top(10);
            frame.set_margin_bottom(10);
            frame.set_margin_start(10);
            frame.set_margin_end(10);
            frame.add(&editor);
            
            paned.pack2(&frame, false, true);
        }

        main_box.pack_start(&paned, true, true, 0);                
        
        window.add(&main_box);

        let w = rc::Rc::new(CharmWindow {
            application: charm.clone(),
            window,
            paned,
            
            context: cell::RefCell::new(None),
        });

        w
    }

    pub fn show(&self) {
        self.window.show_all();
    }

    pub fn open_file(self: &rc::Rc<Self>, file: &gio::File) {
        let attributes = file.query_info("standard::display-name", gio::FileQueryInfoFlags::NONE, Option::<&gio::Cancellable>::None).unwrap();
        let dn = attributes.get_attribute_as_string("standard::display-name").unwrap();

        let space = std::sync::Arc::new(
            space::file::FileAddressSpace::open(
                self.application.rt.handle().clone(),
                &file.get_path().unwrap(),
                &dn).unwrap(),
        );

        self.window.set_title(format!("Charm: {}", space.get_label()).as_str());
        let (context, da) = WindowContext::new(self, listing::Listing::new(space));
        self.paned.pack1(&da, true, false);
        
        std::mem::replace(&mut *self.context.borrow_mut(), Some(context));
    }
}

impl WindowContext {
    fn new(window: &rc::Rc<CharmWindow>, listing: listing::Listing) -> (WindowContext, gtk::DrawingArea) {
        let listing = sync::Arc::new(parking_lot::RwLock::new(listing));
        let (lw, da) = widget::listing::ListingWidget::new(window, &listing);
        
        (WindowContext {
            window: rc::Rc::downgrade(window),
            listing,
            lw,
        }, da)
    }
}
