#[derive(Clone, Debug, PartialEq, Eq)]
enum BranchState {
    Opener,
    Separator,
    Closer,
    End
};

struct BranchStackEntry {
    child: usize
};

#[derive(Clone, Debug, PartialEq, Eq)]
enum LeafState {
    Label,
    Value,
    End
};
