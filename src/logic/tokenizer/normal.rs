#[derive(Clone, Debug, PartialEq, Eq)]
enum State {
    PreBlank,
    Title,
    Content {
        offset: addr::Address,
        child: usize
    },
    PostBlank,
    End
}

enum StackEntry {
    ChildEntry { index: usize },
    SelfEntryForSummary,
}
