use crate::model::addr;
use crate::model::document::structure::NodeKind;

use std::iter;
use std::sync;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TitleDisplay {
    Inline,
    Major,
    Minor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChildrenDisplay {
    None, //< fully collapsed
    Summary,
    //Recolor,
    Full
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContentDisplay {
    None,
    Hexdump {
        line_pitch: addr::Offset,
        gutter_pitch: addr::Offset,
    },
    Bindump {
        line_pitch: addr::Offset,
        word_size: addr::Offset,
    },
    Hexstring
}

/// These are separated from the rest of the Node because they can be changed
/// without rearranging nodes, which makes it easier to port paths across
/// Changes if we can use a different ChangeType when only properties are
/// affected.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Properties {
    pub name: String,
    pub title_display: TitleDisplay,
    pub children_display: ChildrenDisplay,
    pub content_display: ContentDisplay,
    pub locked: bool,    
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct MaybeProperties {
    pub name: Option<String>,
    pub title_display: Option<TitleDisplay>,
    pub children_display: Option<ChildrenDisplay>,
    pub content_display: Option<ContentDisplay>,
    pub locked: Option<bool>,
}

impl TitleDisplay {
    pub fn has_blanks(&self) -> bool {
        match self {
            TitleDisplay::Inline => false,
            TitleDisplay::Major => true,
            TitleDisplay::Minor => false,
        }
    }

    pub fn is_inline(&self) -> bool {
        match self {
            TitleDisplay::Inline => true,
            TitleDisplay::Major => false,
            TitleDisplay::Minor => false,
        }
    }
}

impl Default for TitleDisplay {
    fn default() -> TitleDisplay {
        TitleDisplay::Major
    }
}

impl Default for ChildrenDisplay {
    fn default() -> ChildrenDisplay {
        ChildrenDisplay::Full
    }
}

impl ContentDisplay {
    pub fn preferred_pitch(&self) -> Option<addr::Offset> {
        match self {
            ContentDisplay::None => None,
            ContentDisplay::Hexdump { line_pitch, .. } => Some(*line_pitch),
            ContentDisplay::Bindump { line_pitch, .. } => Some(*line_pitch),
            ContentDisplay::Hexstring => Some(32.into()),
        }
    }

    /// An alternative to default() that explicitly returns a hexdump style.
    pub fn default_hexdump() -> ContentDisplay {
        ContentDisplay::Hexdump {
            line_pitch: addr::Offset::from(16),
            gutter_pitch: addr::Offset::from(8),
        }
    }

    /// An alternative to default() that explicitly returns a bindump style.
    pub fn default_bindump() -> ContentDisplay {
        ContentDisplay::Bindump {
            line_pitch: addr::Offset::from(8),
            word_size: addr::Offset::from(4),
        }
    }
}

impl Default for ContentDisplay {
    fn default() -> ContentDisplay {
        Self::default_hexdump()
    }
}

impl Default for Properties {
    fn default() -> Properties {
        Properties {
            name: "default".to_string(),
            title_display: TitleDisplay::default(),
            children_display: ChildrenDisplay::default(),
            content_display: ContentDisplay::default(),
            locked: false,
        }
    }
}

impl Properties {
    pub fn clone_rename(&self, new_name: String) -> Properties {
        Properties {
            name: new_name,
            title_display: self.title_display.clone(),
            children_display: self.children_display.clone(),
            content_display: self.content_display.clone(),
            locked: self.locked,
        }
    }

    pub fn apply_changes(&mut self, changes: MaybeProperties) {
        if let Some(name) = &changes.name { self.name = name.clone(); }
        if let Some(title_display) = &changes.title_display { self.title_display = title_display.clone(); }
        if let Some(children_display) = &changes.children_display { self.children_display = children_display.clone(); }
        if let Some(content_display) = &changes.content_display { self.content_display = content_display.clone(); }
        if let Some(locked) = &changes.locked { self.locked = locked.clone(); }
    }
}

impl MaybeProperties {
    pub fn new(props: Properties) -> MaybeProperties {
        MaybeProperties {
            name: Some(props.name),
            title_display: Some(props.title_display),
            children_display: Some(props.children_display),
            content_display: Some(props.content_display),
            locked: Some(props.locked),
        }
    }

    pub fn common_between<'a>(i: impl iter::Iterator<Item = &'a sync::Arc<super::Node>>) -> Option<MaybeProperties> {
        let mut current_props = Option::<MaybeProperties>::None;

        for node in i {
            if let Some(node_props) = node.props() {
                if let Some(current_props) = current_props.as_mut() {
                    if current_props.empty() {
                        /* Nothing in common anymore. Give up. */
                        break;
                    }

                    current_props.merge(node_props);
                } else {
                    current_props = Some(Self::new(node_props.clone()));
                }
            }
        }

        current_props
    }
    
    pub fn new_name(name: String) -> MaybeProperties {
        let mut props = Self::default();
        props.name = Some(name);
        props
    }

    pub fn new_title_display(title_display: TitleDisplay) -> MaybeProperties {
        let mut props = Self::default();
        props.title_display = Some(title_display);
        props
    }

    pub fn new_children_display(children_display: ChildrenDisplay) -> MaybeProperties {
        let mut props = Self::default();
        props.children_display = Some(children_display);
        props
    }

    pub fn new_content_display(content_display: ContentDisplay) -> MaybeProperties {
        let mut props = Self::default();
        props.content_display = Some(content_display);
        props
    }

    pub fn new_locked(locked: bool) -> MaybeProperties {
        let mut props = Self::default();
        props.locked = Some(locked);
        props
    }

    fn keep_if_eq<T: PartialEq>(option: &mut Option<T>, other: &T) {
        *option = match option.take() {
            Some(x) if x.eq(other) => Some(x),
            _ => None
        };
    }
    
    pub fn merge(&mut self, other: &Properties) {
        Self::keep_if_eq(&mut self.name, &other.name);
        Self::keep_if_eq(&mut self.title_display, &other.title_display);
        Self::keep_if_eq(&mut self.children_display, &other.children_display);
        Self::keep_if_eq(&mut self.content_display, &other.content_display);
        Self::keep_if_eq(&mut self.locked, &other.locked);
    }

    pub fn apply_changes(&mut self, changes: MaybeProperties) {
        if changes.name.is_some() { self.name = changes.name; }
        if changes.title_display.is_some() { self.title_display = changes.title_display; }
        if changes.children_display.is_some() { self.children_display = changes.children_display; }
        if changes.content_display.is_some() { self.content_display = changes.content_display; }
        if changes.locked.is_some() { self.locked = changes.locked; }
    }
    
    pub fn empty(&self) -> bool {
        self.name.is_none()
            && self.title_display.is_none()
            && self.children_display.is_none()
            && self.content_display.is_none()
            && self.locked.is_none()
    }
}
