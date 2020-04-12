use std::string;
use std::rc;
use std::cell;
use std::sync;
use std::collections;

use std::borrow::Borrow;
use gtk::prelude::*;
use gio::prelude::*;

pub fn nybble_to_hex(nyb: u8) -> u8 {
    if nyb < 10 {
        '0' as u8 + nyb
    } else {
        'a' as u8 + (nyb - 10)
    }
}

struct ActionForwarderSignals {
    // we should only ever be updating this from GTK thread...
    ag: Option<send_wrapper::SendWrapper<gio::ActionGroup>>,
    added_signal: Option<glib::signal::SignalHandlerId>,
    enabled_changed_signal: Option<glib::signal::SignalHandlerId>,
    removed_signal: Option<glib::signal::SignalHandlerId>,
    state_changed_signal: Option<glib::signal::SignalHandlerId>,
}

impl Drop for ActionForwarderSignals {
    fn drop(&mut self) {
        let ag = self.ag.take().unwrap().take();
        self.added_signal.take().map(|s| ag.disconnect(s));
        self.enabled_changed_signal.take().map(|s| ag.disconnect(s));
        self.removed_signal.take().map(|s| ag.disconnect(s));
        self.state_changed_signal.take().map(|s| ag.disconnect(s));
    }
}

/* nice hack to make it so we can trigger actions on the currently focused
 * widget from a menu that is elsewhere in the hierarchy */
pub struct ActionForwarder {
    group: string::String,
    forwarded_group: string::String,
    signals: sync::Mutex<Option<ActionForwarderSignals>>
}

impl ActionForwarder {
    pub fn new(window: &gtk::ApplicationWindow, group: string::String) -> sync::Arc<ActionForwarder> {
        let af = sync::Arc::new(ActionForwarder {
            forwarded_group: "win-".to_string() + &group,
            group,
            signals: sync::Mutex::new(None)
        });

        window.insert_action_group(
            &af.forwarded_group,
            af.clone().attach(
                window
                    .get_focus()
                    .and_then(|focus| focus.get_action_group(&af.group))
                    .as_ref()).as_ref());

        {
            let af_clone = af.clone();
            // TODO: make sure this works once focus-widget comes out
            // why does this require Send?!?!?
            window.connect_notify(Some("focus-widget"), move |window, _| {
                println!("notified focus-widget: {:?}", window.get_focus());
                if let Some(wid) = window.get_focus() {
                    window.insert_action_group(&af_clone.forwarded_group, af_clone.clone().attach(wid.get_action_group(&af_clone.group).as_ref()).as_ref());
                }
            });
        }

        af
    }

    fn setup_mirror(ag: &gio::ActionGroup, simple_map: &mut collections::HashMap<string::String, gio::SimpleAction>, action: &str) -> gio::SimpleAction {
        let parameter_type = ag.get_action_parameter_type(action);
        let simple_action = match (parameter_type, ag.get_action_state(action)) {
            (None, None) => gio::SimpleAction::new(action, None),
            (None, Some(state)) => gio::SimpleAction::new_stateful(action, None, &state),
            (Some(pt), None) => gio::SimpleAction::new(action, Some(pt.borrow())),
            (Some(pt), Some(state)) => gio::SimpleAction::new_stateful(action, Some(pt.borrow()), &state),
        };
        
        simple_action.set_enabled(ag.get_action_enabled(action));
        simple_action.set_state_hint(ag.get_action_state_hint(action).as_ref());

        { let agc = ag.clone(); let act = action.to_string();
          simple_action.connect_activate(
              move |_self, parameter| agc.activate_action(&act, parameter)); }

        { let agc = ag.clone(); let act = action.to_string();
          simple_action.connect_change_state(
              move |_self, parameter| agc.change_action_state(&act, parameter.expect("what to do if this is None?"))); }

        simple_map.insert(action.to_string(), simple_action.clone());

        simple_action
    }
    
    fn attach(self: sync::Arc<Self>, ago: Option<&gio::ActionGroup>) -> Option<gio::SimpleActionGroup> {
        println!("ActionForwarder reattaching to {:?}", ago);
        ago.map(|ag| {
            let action_map = gio::SimpleActionGroup::new();
            let simple_map = rc::Rc::new(cell::RefCell::new(collections::HashMap::new()));

            let mut simple_map_borrow = simple_map.borrow_mut();
            for action in ag.list_actions() {
                action_map.add_action(&Self::setup_mirror(ag, &mut simple_map_borrow, &action));
            }

            let signals = ActionForwarderSignals {
                ag: Some(send_wrapper::SendWrapper::new(ag.clone())),
                
                added_signal: Some(
                    { let smc = simple_map.clone();
                      let am = action_map.clone();
                      ag.connect_action_added(move |ag, action| {
                          am.add_action(&Self::setup_mirror(ag, &mut smc.borrow_mut(), &action));
                      }) }),

                enabled_changed_signal: Some(
                    { let smc = simple_map.clone();
                      ag.connect_action_enabled_changed(move |_ag, action, enabled| {
                          cell::RefCell::borrow(&smc).get(action).map(|sa| sa.set_enabled(enabled));
                      }) }),

                removed_signal: Some(
                    { let smc = simple_map.clone();
                      let am = action_map.clone();
                      ag.connect_action_removed(move |_ag, action| {
                          am.remove_action(action);
                          smc.borrow_mut().remove(action);
                      }) }),

                state_changed_signal: Some(
                    { let smc = simple_map.clone();
                      ag.connect_action_state_changed(move |_ag, action, state| {
                          cell::RefCell::borrow(&smc).get(action).map(|sa| sa.set_state(state));
                      }) }),
            };

            // the important part here is dropping and disconnecting the old signals
            std::mem::replace(&mut *self.signals.lock().unwrap(), Some(signals));
            
            action_map
        })
    }
}
