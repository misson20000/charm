use std::sync;

use crate::catch_panic;
use crate::model::document;
use crate::model::document::structure;
use crate::model::document::structure::NodeKind;
use crate::model::listing::cursor;
use crate::view::error;
use crate::view::listing;
use crate::view::window;
use crate::view::window::ErrorReporter; 

use gtk::prelude::*;
use gtk::gio;

struct InsertCommentAction {
    document_host: sync::Arc<document::DocumentHost>,
    lw: listing::ListingWidget
}

pub fn add_action(window_context: &window::WindowContext) {
    let document_host = window_context.project.document_host.clone();
    let lw = window_context.lw.clone();
    let action = gio::SimpleAction::new("insert_comment", None);
    let window = window_context.window.clone();

    action.connect_activate(move |_, _| catch_panic! {
        let mut cursor = lw.cursor_mut();

        match match &cursor.class {
            /* Edit an existing comment */
            cursor::CursorClass::Comment(_) => {
                document_host.change(
                    cursor.document().edit_comment(
                        cursor.structure_path(),
                        "edited comment".to_string()
                    )
                )
            },

            /* Insert a new comment */
            _ => cursor.insert_node(&document_host, structure::CommentNode {
                text: "test comment".to_string(),
            }.into_arc())
        } {
            Ok(_) => {},
            Err((error, attempted_version)) => {
                let report_error = match &error.ty {
                    /* don't bother popping up the dialog for this */
                    document::change::ApplyErrorType::InvalidParameters(_) => false,
                    _ => true
                };

                if report_error {
                    if let Some(window) = window.upgrade() {
                        window.report_error(error::Error {
                            while_attempting: error::Action::InsertOrEditComment,
                            trouble: error::Trouble::DocumentUpdateFailure {
                                error,
                                attempted_version
                            },
                            level: error::Level::Error,
                            is_bug: false,
                        });
                    }
                }
                
                std::mem::drop(cursor);
                lw.bonk();
            },
        }
    });

    action.set_enabled(true);

    window_context.action_group.add_action(&action);
}
