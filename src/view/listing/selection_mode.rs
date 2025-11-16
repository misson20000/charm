use crate::model::selection;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum SelectionMode {
    #[default]
    Structure,
    Address,
}

impl SelectionMode {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Structure => "structure",
            Self::Address => "address",
        }
    }
    
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "structure" => Some(Self::Structure),
            "address" => Some(Self::Address),
            _ => None
        }
    }
    
    pub fn icon_name(&self) -> &'static str {
        match self {
            Self::Structure => "icon-selection-structure",
            Self::Address => "icon-selection-address",
        }
    }

    pub fn tooltip(&self) -> &'static str {
        match self {
            Self::Structure => "Structure selection",
            Self::Address => "Address selection",
        }
    }

    pub fn conversion_change(&self) -> selection::listing::Change {
        match self {
            Self::Structure => selection::listing::Change::ConvertToStructure,
            Self::Address => selection::listing::Change::ConvertToAddress,
        }
    }
}

impl glib::variant::ToVariant for SelectionMode {
    fn to_variant(&self) -> glib::Variant {
        self.name().to_variant()
    }
}
