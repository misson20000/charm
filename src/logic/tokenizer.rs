//! This module includes the logic that converts from a document structure
//! hierarchy into a seekable stream of tokens.

use std::sync;

use crate::model::addr;
use crate::model::listing::token;
use crate::model::document::structure;

#[derive(Clone, Debug)]
enum TokenizerState {
    PreBlank, //< if going downward, would emit Null token.
    Title, //< if going downward, would emit Title token.
    Content(addr::Offset, usize), //< if going downward, would emit either the indexed child, or content starting at the offset.
    PostBlank, //< if going downward, would emit Null token.
    End, //< if going downward, we've hit the end of this node's content.
}

#[derive(Clone)]
pub struct Tokenizer {
    /* invariants:
       - stack should always contain a path all the way back to the root node
     */
    stack: Option<sync::Arc<Tokenizer>>,
    state: TokenizerState,
    depth: usize,
    node: sync::Arc<structure::Node>,
}

impl std::fmt::Debug for Tokenizer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "formatter(state: {:?}, node: {}, parent: {:?})", self.state, self.node.name, self.stack)
    }
}

impl Tokenizer {
    /// Creates a new tokenizer seeked to the root of the structure hierarchy and the beginning of the token stream.
    pub fn at_beginning(root: &sync::Arc<structure::Node>) -> Tokenizer {
        Tokenizer {
            stack: None,
            state: TokenizerState::PreBlank,
            depth: 0,
            node: root.clone()
        }
    }

    pub fn at_address(_root: &sync::Arc<structure::Node>, _addr: addr::Address) -> Tokenizer {
        todo!();
    }

    pub fn port(_old: &Tokenizer, _new_root: &sync::Arc<structure::Node>) -> Tokenizer {
        todo!();
    }

    /// Creates a new tokenizer seeked to the end of the token stream.
    pub fn at_end(root: &sync::Arc<structure::Node>) -> Tokenizer {
        Tokenizer {
            stack: None,
            state: TokenizerState::End,
            depth: 0,
            node: root.clone()
        }
    }
    
    /// Yield the token positioned before the seek head, and regress the seek head.
    pub fn prev(&mut self) -> Option<token::Token> {
        loop {
            match self.state {
                TokenizerState::PreBlank => {
                    /* try to ascend hierarchy by popping the stack */
                    if !self.try_ascend() {
                        break None;
                    }
                    /* retry using the parent's state. */
                    continue;
                },
                TokenizerState::Title => {
                    self.state = TokenizerState::PreBlank;
                    break Some(token::Token {
                        class: token::TokenClass::Null,
                        node: self.node.clone(),
                        depth: self.depth,
                        newline: true,
                    });
                },
                TokenizerState::Content(offset, index) => {
                    let prev_child_option = match index {
                        0 => None,
                        /* Something is seriously wrong if index was farther than one-past-the-end. */
                        i => Some(&self.node.children[i-1])
                    };

                    /* Should we descend? */
                    if let Some(prev_child) = prev_child_option {
                        if prev_child.end() >= offset {
                            self.descend(
                                prev_child.node.clone(),
                                /* By the time we ascend, we should have reached the beginning of the child. */
                                TokenizerState::Content(prev_child.offset, index - 1),
                                /* Descend to the end of the child. */
                                TokenizerState::End);
                            /* Re-enter loop */
                            continue;
                        }
                    }

                    /* Is there any content left? */
                    if offset > addr::unit::ZERO {
                        // TODO: wire up pitch
                        let pitch = addr::Size::from(16);
                        
                        /* Where would we *like* to begin, as decided by our hexdump pitch? */
                        let preferred_begin = pitch * ((offset - addr::unit::BIT) / pitch);

                        /* Where can we not begin before? */
                        let limit = match prev_child_option {
                            /* Can't include data from the child, so need to stop after its end. */
                            Some(prev_child) => prev_child.end(),
                            /* Can't include data that belongs to the parent, so need to stop before our begin. */
                            None => addr::unit::ZERO,
                        };

                        /* Pick a place to begin this line. */
                        let begin = std::cmp::max(preferred_begin, limit);

                        self.state = TokenizerState::Content(begin, index);
                        break Some(token::Token {
                            class: token::TokenClass::Hexdump(addr::Extent::between(begin.to_addr(), offset.to_addr())),
                            node: self.node.clone(),
                            depth: self.depth,
                            newline: true,
                        });
                    } else {
                        /* We're pointed at the beginning. Emit the title block. */
                        self.state = TokenizerState::Title;
                        break Some(token::Token {
                            class: token::TokenClass::Title,
                            node: self.node.clone(),
                            depth: self.depth,
                            newline: true,
                        });
                    }
                },
                TokenizerState::PostBlank => {
                    /* Bump state to content and retry. */
                    self.state = TokenizerState::Content(self.node.size, self.node.children.len());
                    continue;
                },
                TokenizerState::End => {
                    self.state = TokenizerState::PostBlank;
                    break Some(token::Token {
                        class: token::TokenClass::Null,
                        node: self.node.clone(),
                        depth: self.depth,
                        newline: true,
                    });
                },
            }
        }
    }

    /// Yield the token positioned after the seek head, and advance the seek head.
    pub fn next(&mut self) -> Option<token::Token> {
        loop {
            match self.state {
                TokenizerState::PreBlank => {
                    self.state = TokenizerState::Title;
                    break Some(token::Token {
                        class: token::TokenClass::Null,
                        node: self.node.clone(),
                        depth: self.depth,
                        newline: true,
                    });
                },
                TokenizerState::Title => {
                    self.state = TokenizerState::Content(addr::unit::ZERO, 0);
                    break Some(token::Token {
                        class: token::TokenClass::Title,
                        node: self.node.clone(),
                        depth: self.depth,
                        newline: true,
                    });
                },
                TokenizerState::Content(offset, index) => {
                    let next_child_option = self.node.children.get(index);
                    
                    /* Should we descend? */
                    if let Some(next_child) = next_child_option {
                        if next_child.offset <= offset {
                            self.descend(
                                next_child.node.clone(),
                                /* By the time we ascend, we should reach the end of the child. */
                                TokenizerState::Content(next_child.end(), index + 1),
                                /* Descend to the beginning of the child. */
                                TokenizerState::PreBlank);
                            /* Re-enter loop */
                            continue;
                        }
                    }

                    /* Is there any content left? */
                    if offset < self.node.size {
                        // TODO: wire up pitch
                        let pitch = addr::Size::from(16);

                        /* Where would we *like* to end, as decided by our hexdump pitch? */
                        let preferred_end = pitch * ((offset / pitch) + 1);

                        /* Where can we not end beyond? */
                        let limit = match next_child_option {
                            /* Can't include data from the child, so need to stop before it begins. */
                            Some(next_child) => next_child.offset,
                            /* Can't include data that belongs to the parent, so need to stop before we end. */
                            None => self.node.size,
                        };

                        /* Pick a place to end this line. */
                        let end = std::cmp::min(preferred_end, limit);

                        /* It's ok if this puts us past the end of the data,
                         * since we'll just go through the End state the next
                         * time next() is called. */
                        self.state = TokenizerState::Content(end, index);
                        
                        break Some(token::Token {
                            class: token::TokenClass::Hexdump(addr::Extent::between(offset.to_addr(), end.to_addr())),
                            node: self.node.clone(),
                            depth: self.depth,
                            newline: true,
                        });
                    } else {
                        /* We were pointed at (or past!) the end. Fixup state and retry. */
                        self.state = TokenizerState::PostBlank;
                        continue;
                    }
                },
                TokenizerState::PostBlank => {
                    self.state = TokenizerState::End;
                    break Some(token::Token {
                        class: token::TokenClass::Null,
                        node: self.node.clone(),
                        depth: self.depth,
                        newline: true,
                    });
                },
                TokenizerState::End => {
                    /* try to ascend hierarchy by popping the stack */
                    if !self.try_ascend() {
                        break None;
                    }
                    /* retry using the parent's state. */
                    continue;
                }
            }
        }
    }

    fn descend(&mut self, child_node: sync::Arc<structure::Node>, parent_state: TokenizerState, child_state: TokenizerState) {
        let parent_node = std::mem::replace(&mut self.node, child_node);
        
        let parent_tokenizer = Tokenizer {
            stack: self.stack.take(),
            state: parent_state,
            depth: self.depth,
            node: parent_node,
        };

        self.depth+= 1;
        self.stack = Some(sync::Arc::new(parent_tokenizer));
        self.state = child_state;
    }
    
    /// Replaces our context with the parent's context, returning false if there
    /// was no parent.
    fn try_ascend(&mut self) -> bool {
        match std::mem::replace(&mut self.stack, None) {
            Some(replacement) => {
                *self = (*replacement).clone();
                true
            },
            None => false
        }
    }
    
    pub fn hit_bottom(&self) -> bool {
        match self.state {
            TokenizerState::End => self.stack.is_none(),
            _ => false
        }
    }

    pub fn hit_top(&self) -> bool {
        match self.state {
            TokenizerState::PreBlank => self.stack.is_none(),
            _ => false
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    extern crate roxmltree;

    use std::collections;
    use std::iter;
    use std::vec;

    struct DownwardTokenizerIterator(Tokenizer);
    struct UpwardTokenizerIterator(Tokenizer);

    impl iter::Iterator for DownwardTokenizerIterator {
        type Item = token::Token;
        
        fn next(&mut self) -> Option<token::Token> {
            let a = self.0.next();
            if a.is_some() {
                assert_eq!(a, self.0.prev());
                assert_eq!(a, self.0.next());
            }
            a
        }
    }

    impl iter::Iterator for UpwardTokenizerIterator {
        type Item = token::Token;
        
        fn next(&mut self) -> Option<token::Token> {
            let a = self.0.prev();
            if a.is_some() {
                assert_eq!(a, self.0.next());
                assert_eq!(a, self.0.prev());
            }
            a
        }
    }

    pub struct Testcase {
        pub structure: sync::Arc<structure::Node>,
        pub expected_tokens: vec::Vec<token::Token>,
    }

    struct TokenDef {
        class: token::TokenClass,
        node_name: String,
        depth: usize,
        newline: bool,
    }
    
    impl Testcase {
        pub fn from_xml(xml: &[u8]) -> Testcase {
            let document = match roxmltree::Document::parse(std::str::from_utf8(xml).unwrap()) {
                Ok(document) => document,
                Err(e) => panic!("{}", e)
            };

            let re = document.root_element();
            assert!(re.has_tag_name("testcase"));

            let mut lookup = collections::HashMap::new();
            let mut structure = None;
            let mut expected_tokens: Option<Vec<TokenDef>> = None;
            
            for child in re.children() {
                if !child.is_element() { continue; }
                match child.tag_name().name() {
                    "node" => {
                        structure = match structure {
                            Some(_) => panic!("multiple structure definitions"),
                            None => Some(Self::inflate_structure(child, &mut lookup))
                        }
                    }
                    "tokens" => {
                        expected_tokens = match expected_tokens {
                            Some(_) => panic!("multiple expected tokens"),
                            None => {
                                let mut vec = vec::Vec::new();
                                Self::inflate_token_tree(child, &mut vec, 0);
                                Some(vec)
                            }
                        }
                    },
                    tn => panic!("unexpected tag '{}'", tn)
                }
            }

            Testcase {
                structure: structure.expect("should've had a structure definition"),
                expected_tokens: expected_tokens.expect("should've had expected tokens").into_iter().map(|c| c.to_token(&lookup)).collect(),
            }
        }

        fn inflate_token_tree(xml: roxmltree::Node, collection: &mut vec::Vec<TokenDef>, depth: usize) {
            for c in xml.children().filter(|c| c.is_element()) {
                if c.has_tag_name("indent") {
                    Self::inflate_token_tree(c, collection, depth + 1)
                } else {
                    collection.push(TokenDef {
                        class: match c.tag_name().name() {
                            "null" => token::TokenClass::Null,
                            "title" => token::TokenClass::Title,
                            "hexdump" => token::TokenClass::Hexdump(Self::inflate_extent(&c)),
                            "hexstring" => token::TokenClass::Hexstring(Self::inflate_extent(&c)),
                            tn => panic!("invalid token def: '{}'", tn)
                        },
                        node_name: c.attribute("node").unwrap().to_string(),
                        depth,
                        newline: c.attribute("nl").unwrap().eq("true"),
                    })
                }
            }
        }

        fn inflate_extent(xml: &roxmltree::Node) -> addr::Extent {
            addr::Extent::between(
                addr::Address::parse(xml.attribute("begin").unwrap()).unwrap(),
                addr::Address::parse(xml.attribute("end").unwrap()).unwrap()
            )
        }
        
        fn inflate_childhood(xml: roxmltree::Node, map: &mut collections::HashMap<String, sync::Arc<structure::Node>>) -> structure::Childhood {
            structure::Childhood {
                node: Self::inflate_structure(xml, map),
                offset: addr::Address::parse(xml.attribute("offset").unwrap()).unwrap().to_size(),
            }
        }
        
        fn inflate_structure(xml: roxmltree::Node, map: &mut collections::HashMap<String, sync::Arc<structure::Node>>) -> sync::Arc<structure::Node> {
            let node = structure::Node {
                name: xml.attribute("name").unwrap().to_string(),
                size: addr::Address::parse(xml.attribute("size").unwrap()).unwrap().to_size(),
                title_display: structure::TitleDisplay::Major,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump(16),
                locked: true,
                children: xml.children().filter(|c| c.is_element()).map(|c| Self::inflate_childhood(c, map)).collect()
            };
            let arc = sync::Arc::new(node);
            map.insert(arc.name.clone(), arc.clone());
            arc
        }

        fn test_forward(&self) {
            itertools::assert_equal(
                self.expected_tokens.iter().map(|x| x.clone()),
                &mut DownwardTokenizerIterator(Tokenizer::at_beginning(&self.structure)));
        }

        fn test_backward(&self) {
            itertools::assert_equal(
                self.expected_tokens.iter().rev().map(|x| x.clone()),
                &mut UpwardTokenizerIterator(Tokenizer::at_end(&self.structure)));
        }
    }

    impl TokenDef {
        fn to_token(self, lookup: &collections::HashMap<String, sync::Arc<structure::Node>>) -> token::Token {
            token::Token {
                class: self.class,
                node: lookup.get(&self.node_name).expect(&format!("expected a node named '{}'", self.node_name)).clone(),
                depth: self.depth,
                newline: self.newline
            }
        }
    }
    
    #[test]
    fn simple() {
        let tc = Testcase::from_xml(include_bytes!("tokenizer_tests/simple.xml"));
        tc.test_forward();
        tc.test_backward();
    }

    #[test]
    fn nesting() {
        let tc = Testcase::from_xml(include_bytes!("tokenizer_tests/nesting.xml"));
        tc.test_forward();
        tc.test_backward();
    }

    #[test]
    fn hardcoded() {
        let mut root = structure::Node {
            name: "root".to_string(),
            size: addr::Size::from(0x70),
            title_display: structure::TitleDisplay::Major,
            children_display: structure::ChildrenDisplay::Full,
            content_display: structure::ContentDisplay::Hexdump(16),
            locked: true,
            children: vec::Vec::new()
        };

        let child = sync::Arc::new(structure::Node {
            name: "child".to_string(),
            size: addr::Size::from(0x18),
            title_display: structure::TitleDisplay::Major,
            children_display: structure::ChildrenDisplay::Full,
            content_display: structure::ContentDisplay::Hexdump(16),
            locked: true,
            children: vec::Vec::new()
        });
        
        root.children.push(structure::Childhood {
            node: child.clone(),
            offset: addr::Size::from(0x32)
        });

        let root = sync::Arc::new(root);

        let expected_tokens = vec![
            /* root */
            token::Token {
                class: token::TokenClass::Null,
                node: root.clone(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Title,
                node: root.clone(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x0, 0x10)),
                node: root.clone(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x10, 0x20)),
                node: root.clone(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x20, 0x30)),
                node: root.clone(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x30, 0x32)),
                node: root.clone(), depth: 0, newline: true
            },
            /* child */
            token::Token {
                class: token::TokenClass::Null,
                node: child.clone(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Title,
                node: child.clone(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x0, 0x10)),
                node: child.clone(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x10, 0x18)),
                node: child.clone(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Null,
                node: child.clone(), depth: 1, newline: true
            },
            /* root */
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x4a, 0x50)),
                node: root.clone(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x50, 0x60)),
                node: root.clone(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x60, 0x70)),
                node: root.clone(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Null,
                node: root.clone(), depth: 0, newline: true
            },
        ];

        let testcase = Testcase {
            structure: root,
            expected_tokens,
        };

        testcase.test_forward();
        testcase.test_backward();
    }
}
