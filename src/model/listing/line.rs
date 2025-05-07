//! This module includes the logic that converts from a token stream
//! to lines that would be displayed in a window.

use std::collections;
use std::fmt;
use std::iter;
use std::sync;

use crate::model::addr;
use crate::model::document::structure;
use crate::model::listing::stream;
use crate::model::listing::token;
use crate::model::listing::token::AsTokenRef;
use crate::model::listing::token::TokenKind;
use crate::util;

#[derive(Clone)]
pub enum LineType {
    Empty,
    Blank(token::BlankLine),
    Title(token::Title),
    Hexdump {
        title: Option<token::Title>,
        node: sync::Arc<structure::Node>,
        node_path: structure::Path,
        node_addr: addr::AbsoluteAddress,
        line_extent: addr::Extent,
        tokens: collections::VecDeque<token::Hexdump>
    },
    Bindump {
        title: Option<token::Title>,
        node: sync::Arc<structure::Node>,
        node_path: structure::Path,
        node_addr: addr::AbsoluteAddress,
        line_extent: addr::Extent,
        tokens: collections::VecDeque<token::Bindump>
    },
    Hexstring {
        title: Option<token::Title>,
        token: token::Hexstring,
    },
    Summary {
        title: Option<token::Title>,
        tokens: collections::VecDeque<token::Token>
    },
    Ellipsis {
        title: Option<token::Title>,
        token: token::Ellipsis,
    },
}

#[derive(Debug, PartialEq, Eq)]
enum LinePushResult {
    Accepted,
    Completed,
    Rejected,
    BadPosition
}

#[derive(Clone)]
pub struct Line {
    pub ty: LineType,
}

impl Line {
    pub fn empty() -> Self {
        Line {
            ty: LineType::Empty
        }
    }

    /// Figures out what line contains the token immediately after the position. Moves the referenced position to the end of that line, and returns the line, a position pointing to the beginning of the line, and the index of the specified token within that line.
    pub fn containing_position<Position: stream::AbstractPosition>(position: &mut Position) -> (Self, Position, usize) {
        /* Put the first token on the line. */
        let mut line = Line::from_token(loop {
            match position.gen_token() {
                stream::TokenGenerationResult::Ok(token) => break token,
                /* If we hit the end, just return an empty line. */
                stream::TokenGenerationResult::Skip => if position.move_next() { continue } else { return (Self::empty(), position.clone(), 0) },
                stream::TokenGenerationResult::Boundary => return (Self::empty(), position.clone(), 0)
            }
        }, position.in_summary());

        let mut prev = position.clone();
        let mut index = 0;
        
        /* Walk `prev` back to the beginning of the line. */
        loop {
            if !prev.move_prev() {
                break;
            }

            if match line.push_front(match prev.gen_token() {
                stream::TokenGenerationResult::Ok(token) => token,
                stream::TokenGenerationResult::Skip => continue,
                stream::TokenGenerationResult::Boundary => break,
            }) {
                LinePushResult::Accepted => { index+= 1; continue },
                LinePushResult::Completed => { index+= 1; break },
                LinePushResult::Rejected => true,
                LinePushResult::BadPosition => false,
            } {
                /* roll the state back */
                assert!(prev.move_next());
                break;
            }
        }
        
        /* Walk `position` to the end of the line. */
        position.move_next();
        loop {
            match line.push_back(match position.gen_token() {
                stream::TokenGenerationResult::Ok(token) => token,
                stream::TokenGenerationResult::Skip => if position.move_next() { continue } else { break },
                stream::TokenGenerationResult::Boundary => break,
            }) {
                LinePushResult::Accepted => position.move_next(),
                LinePushResult::Completed => { position.move_next(); break },
                LinePushResult::Rejected => break,
                LinePushResult::BadPosition => position.move_next(),
            };
        }

        // TODO: move this to tests?
        assert_eq!(line, Self::next_from_position(&mut prev.clone()));

        (line, prev, index)
    }
    
    /// Returns the line ending at the position, and moves the position to the beginning of that line.
    pub fn prev_from_position(position: &mut impl stream::AbstractPosition) -> Self {
        let mut line = Self::empty();

        loop {
            if !position.move_prev() {
                break;
            }

            if match line.push_front(match position.gen_token() {
                stream::TokenGenerationResult::Ok(token) => token,
                stream::TokenGenerationResult::Skip => continue,
                stream::TokenGenerationResult::Boundary => break,
            }) {
                LinePushResult::Accepted => continue,
                LinePushResult::Completed => break,
                LinePushResult::Rejected => true,
                LinePushResult::BadPosition => false,
            } {
                /* roll the state back */
                assert!(position.move_next());
                break;
            }
        }

        line
    }

    /// Returns the line beginning at the position, and moves the position to the end of that line.
    pub fn next_from_position(position: &mut impl stream::AbstractPosition) -> Self {
        let mut line = Line::empty();

        loop {
            match line.push_back(match position.gen_token() {
                stream::TokenGenerationResult::Ok(token) => token,
                stream::TokenGenerationResult::Skip => if position.move_next() { continue } else { break },
                stream::TokenGenerationResult::Boundary => break,
            }) {
                LinePushResult::Accepted => position.move_next(),
                LinePushResult::Completed => { position.move_next(); break },
                LinePushResult::Rejected => break,
                LinePushResult::BadPosition => position.move_next(),
            };
        }

        line
    }

    fn from_token(token: token::Token, is_summary: bool) -> Self {
        Self {
            ty: match token {
                /* Even if we're in a summary, we can't accidentally count the title as part of the summary. */
                token::Token::Title(token) => LineType::Title(token),
                token::Token::BlankLine(token) => LineType::Blank(token),

                /* Value-like tokens such as Hexstring might appear in summaries. We need to know if we're in a summary so we make a Summary LineType instead of a Hexstring LineType. */
                token if is_summary => LineType::Summary { title: None, tokens: collections::VecDeque::from([token]) },
                
                token::Token::SummaryPreamble(_) |
                token::Token::SummaryEpilogue(_) |
                token::Token::SummaryPunctuation(_) |
                token::Token::SummaryLabel(_) => panic!("Got Summary-type token but wasn't told we were in a summary"),
                
                token::Token::Ellipsis(t) => LineType::Ellipsis { title: None, token: t },
                
                token::Token::Hexdump(token) => LineType::Hexdump {
                    title: None,
                    node: token.common.node.clone(),
                    node_path: token.common.node_path.clone(),
                    node_addr: token.common.node_addr,
                    line_extent: token.line.clone(),
                    tokens: collections::VecDeque::from([token])
                },
                token::Token::Bindump(token) => LineType::Bindump {
                    title: None,
                    node: token.common.node.clone(),
                    node_path: token.common.node_path.clone(),
                    node_addr: token.common.node_addr,
                    line_extent: token.line.clone(),
                    tokens: collections::VecDeque::from([token])
                },
                token::Token::Hexstring(token) => LineType::Hexstring { title: None, token },
            },
        }
    }
        
    /// Returns true on success
    fn push_front(&mut self, token: token::Token) -> LinePushResult {
        let (new_ty, result) = match (std::mem::replace(&mut self.ty, LineType::Empty), token) {
            /* A line with an Empty punctuation token on it can't have anything else on it. */
            (LineType::Blank(token), _) => (LineType::Blank(token), LinePushResult::Rejected),

            /* BlankLine punctuation tokens can go onto Empty lines to convert them to Blank lines, but nothing else. */
            (LineType::Empty, token::Token::BlankLine(token)) => (LineType::Blank(token), LinePushResult::Completed),
            (lt, token::Token::BlankLine(_)) => (lt, LinePushResult::Rejected),

            /* A title can end a line. */
            (LineType::Empty, token::Token::Title(token)) => (LineType::Title(token), LinePushResult::Accepted),

            /* A hexdump token can end a line. */
            (LineType::Empty, token::Token::Hexdump(token)) => (LineType::Hexdump {
                title: None,
                node: token.common.node.clone(),
                node_path: token.common.node_path.clone(),
                node_addr: token.common.node_addr,
                line_extent: token.line.clone(),
                tokens: collections::VecDeque::from([token])
            }, LinePushResult::Accepted),

            /* A title token can occur on the same line as a hexdump if the title is inline and there isn't already a title. */
            (LineType::Hexdump {
                title: None,
                node,
                node_path,
                node_addr,
                line_extent,
                tokens
            }, token::Token::Title(token))
                if sync::Arc::ptr_eq(&token.common.node, &node)
                && node_path == token.common.node_path
                && node_addr == token.common.node_addr
                && token.common.node.props.title_display.is_inline()
                => (LineType::Hexdump {
                    title: Some(token),
                    node,
                    node_path,
                    node_addr,
                    line_extent,
                    tokens
                }, LinePushResult::Accepted),

            /* Multiple hexdump tokens can coexist on a line under certain conditions. */
            (LineType::Hexdump { title, node: line_node, node_path, node_addr, line_extent, mut tokens },
             token::Token::Hexdump(token))
                if sync::Arc::ptr_eq(&line_node, &token.common.node)
                && node_path == token.common.node_path
                && node_addr == token.common.node_addr
                && token.line == line_extent
                => {
                    /* Must be monotonic and non-overlapping. */
                    let result = if tokens.front().expect("should have at least one token").extent.begin < token.extent.end {
                        // TODO: log properly
                        println!("Attempted to add a token to a LineType::Hexdump that would've broken monotonicity. This shouldn't really happen.");
                        LinePushResult::Rejected
                    } else {
                        tokens.push_front(token);
                        LinePushResult::Rejected
                    };

                    (LineType::Hexdump {
                        title,
                        node: line_node,
                        node_path,
                        node_addr,
                        line_extent,
                        tokens
                    }, result)
                },

            /* A bindump token can end a line. */
            (LineType::Empty, token::Token::Bindump(token)) => (LineType::Bindump {
                title: None,
                node: token.common.node.clone(),
                node_path: token.common.node_path.clone(),
                node_addr: token.common.node_addr,
                line_extent: token.line.clone(),
                tokens: collections::VecDeque::from([token])
            }, LinePushResult::Accepted),

            /* A title token can occur on the same line as a bindump if the title is inline and there isn't already a title. */
            (LineType::Bindump {
                title: None,
                node,
                node_path,
                node_addr,
                line_extent,
                tokens
            }, token::Token::Title(token))
                if sync::Arc::ptr_eq(&token.common.node, &node)
                && node_path == token.common.node_path
                && node_addr == token.common.node_addr
                && token.common.node.props.title_display.is_inline()
                => (LineType::Bindump {
                    title: Some(token),
                    node,
                    node_path,
                    node_addr,
                    line_extent,
                    tokens
                }, LinePushResult::Accepted),

            /* Multiple bindump tokens can coexist on a line under certain conditions. */
            (LineType::Bindump { title, node: line_node, node_path, node_addr, line_extent, mut tokens },
             token::Token::Bindump(token))
                if sync::Arc::ptr_eq(&line_node, &token.common.node)
                && node_path == token.common.node_path
                && node_addr == token.common.node_addr
                && token.line == line_extent
                => {
                    /* Must be monotonic and non-overlapping. */
                    let result = if tokens.front().expect("should have at least one token").extent.begin < token.extent.end {
                        // TODO: log properly
                        println!("Attempted to add a token to a LineType::Bindump that would've broken monotonicity. This shouldn't really happen.");
                        LinePushResult::Rejected
                    } else {
                        tokens.push_front(token);
                        LinePushResult::Rejected
                    };

                    (LineType::Bindump {
                        title,
                        node: line_node,
                        node_path,
                        node_addr,
                        line_extent,
                        tokens
                    }, result)
                },

            /* A hexstring token can end a line */
            (LineType::Empty, token::Token::Hexstring(token)) => (LineType::Hexstring {
                title: None,
                token
            }, LinePushResult::Accepted),

            /* A title token can occur on the same line as a hexstring if the title is inline and there isn't already a title. */
            (LineType::Hexstring { title: None, token: hexstring_token }, token::Token::Title(token))
                if sync::Arc::ptr_eq(&token.common.node, &hexstring_token.common.node)
                && token.common.node.props.title_display.is_inline()
                => (LineType::Hexstring {
                    title: Some(token),
                    token: hexstring_token,
                }, LinePushResult::Accepted),

            /* An ellipsis token can end a line */
            (LineType::Empty, token::Token::Ellipsis(token)) => (LineType::Ellipsis {
                title: None,
                token
            }, LinePushResult::Accepted),

            /* A title token can occur on the same line as an ellipsis if the title is inline and there isn't already a title. */
            (LineType::Ellipsis { title: None, token: ellipsis_token }, token::Token::Title(token))
                if sync::Arc::ptr_eq(&token.common.node, &ellipsis_token.common.node)
                && token.common.node.props.title_display.is_inline()
                => (LineType::Ellipsis {
                    title: Some(token),
                    token: ellipsis_token,
                }, LinePushResult::Accepted),

            /* Summaries... */
            (LineType::Empty, token::Token::SummaryEpilogue(token)) => (LineType::Summary {
                title: None,
                tokens: collections::VecDeque::from([token.into()]),
            }, LinePushResult::Accepted),

            (LineType::Summary { title: None, tokens }, token::Token::Title(token))
                if token.node().props.title_display.is_inline()
                => match tokens.front() {
                    Some(token::Token::SummaryPreamble(preamble)) if sync::Arc::ptr_eq(preamble.node(), token.node()) => (LineType::Summary { title: Some(token), tokens }, LinePushResult::Accepted),
                    Some(_) => (LineType::Summary { title: None, tokens }, LinePushResult::Rejected),
                    None => panic!("LineType::Summary should have at least one token")
                },
                
            (LineType::Summary { title, mut tokens }, token) => {
                let result = match (tokens.front(), token) {
                    (Some(token::Token::SummaryPreamble(_)), _) => LinePushResult::Rejected,
                    (Some(_), token::Token::SummaryPreamble(preamble)) => { tokens.push_front(preamble.into()); LinePushResult::Completed },
                    (Some(_), token) => { tokens.push_front(token); LinePushResult::Accepted },
                    (None, _) => panic!("LineType::Summary should have at least one token")
                };

                (LineType::Summary { title, tokens }, result)
            },

            /*
            (LineType::Empty, _) => {
                (LineType::Empty, LinePushResult::BadPosition)
        },
            */
            
            (ty, _) => (ty, LinePushResult::Rejected)
        };

        self.ty = new_ty;

        result
    }

    /// Returns true on success
    fn push_back(&mut self, token: token::Token) -> LinePushResult {
        let (new_ty, result) = match (std::mem::replace(&mut self.ty, LineType::Empty), token) {
            /* A line with an Empty punctuation token on it can't have anything else on it. */
            (LineType::Blank(tok), _) => (LineType::Blank(tok), LinePushResult::Rejected),

            /* BlankLine punctuation tokens can go onto Empty lines to convert them to Blank lines, but nothing else. */
            (LineType::Empty, token::Token::BlankLine(token)) => (LineType::Blank(token), LinePushResult::Completed),
            (lt, token::Token::BlankLine(_)) => (lt, LinePushResult::Rejected),
                
            /* A title can begin a line. */
            (LineType::Empty, token::Token::Title(token)) => (LineType::Title(token), LinePushResult::Accepted),

            /* A hexdump token can begin a line. */
            (LineType::Empty, token::Token::Hexdump(token)) => (LineType::Hexdump {
                title: None,
                node: token.node().clone(),
                node_path: token.node_path().clone(),
                node_addr: token.node_addr(),
                line_extent: token.line,
                tokens: collections::VecDeque::from([token])
            }, LinePushResult::Accepted),

            /* A hexdump token can occur on the same line as a title if the title is inline. */
            (LineType::Title(title_token), token::Token::Hexdump(token))
                if sync::Arc::ptr_eq(title_token.node(), token.node())
                && title_token.node().props.title_display.is_inline()
                => (LineType::Hexdump {
                    node: title_token.node().clone(),
                    node_path: token.node_path().clone(),
                    node_addr: token.node_addr(),
                    title: Some(title_token),
                    line_extent: token.line,
                    tokens: collections::VecDeque::from([token]),
                }, LinePushResult::Accepted),
                
            /* Multiple hexdump tokens can coexist on a line under certain conditions. */
            (LineType::Hexdump { title, node: line_node, node_path, node_addr, line_extent, mut tokens },
             token::Token::Hexdump(token))
                if sync::Arc::ptr_eq(&line_node, token.node())
                && node_path == token.common.node_path
                && node_addr == token.common.node_addr
                && line_extent == token.line
                => {
                    /* Must be monotonic and non-overlapping. */
                    let result = if tokens.back().expect("should have at least one token").extent.end > token.extent.begin {
                        // TODO: log properly
                        println!("Attempted to add a token to a LineType::Hexdump that would've broken monotonicity. This shouldn't really happen.");
                        LinePushResult::Rejected
                    } else {
                        tokens.push_back(token);
                        LinePushResult::Rejected
                    };

                    (LineType::Hexdump {
                        title,
                        node: line_node,
                        node_path,
                        node_addr,
                        line_extent,
                        tokens
                    }, result)
                },

            /* A bindump token can begin a line. */
            (LineType::Empty, token::Token::Bindump(token)) => (LineType::Bindump {
                title: None,
                node: token.node().clone(),
                node_path: token.node_path().clone(),
                node_addr: token.node_addr(),
                line_extent: token.line,
                tokens: collections::VecDeque::from([token])
            }, LinePushResult::Accepted),

            /* A bindump token can occur on the same line as a title if the title is inline. */
            (LineType::Title(title_token), token::Token::Bindump(token))
                if sync::Arc::ptr_eq(title_token.node(), token.node())
                && title_token.node().props.title_display.is_inline()
                => (LineType::Bindump {
                    node: title_token.node().clone(),
                    node_path: token.node_path().clone(),
                    node_addr: token.node_addr(),
                    title: Some(title_token),
                    line_extent: token.line,
                    tokens: collections::VecDeque::from([token]),
                }, LinePushResult::Accepted),
                
            /* Multiple bindump tokens can coexist on a line under certain conditions. */
            (LineType::Bindump { title, node: line_node, node_path, node_addr, line_extent, mut tokens },
             token::Token::Bindump(token))
                if sync::Arc::ptr_eq(&line_node, token.node())
                && node_path == token.common.node_path
                && node_addr == token.common.node_addr
                && line_extent == token.line
                => {
                    /* Must be monotonic and non-overlapping. */
                    let result = if tokens.back().expect("should have at least one token").extent.end > token.extent.begin {
                        // TODO: log properly
                        println!("Attempted to add a token to a LineType::Bindump that would've broken monotonicity. This shouldn't really happen.");
                        LinePushResult::Rejected
                    } else {
                        tokens.push_back(token);
                        LinePushResult::Rejected
                    };

                    (LineType::Bindump {
                        title,
                        node: line_node,
                        node_path,
                        node_addr,
                        line_extent,
                        tokens
                    }, result)
                },

            /* A hexstring token can begin a line */
            (LineType::Empty, token::Token::Hexstring(token)) => (LineType::Hexstring {
                title: None,
                token
            }, LinePushResult::Completed),

            /* A hexstring token can occur on the same line as a title if the title is inline. */
            (LineType::Title(title_token), token::Token::Hexstring(token))
                if sync::Arc::ptr_eq(title_token.node(), token.node())
                && title_token.node().props.title_display.is_inline()
                => (LineType::Hexstring {
                    title: Some(title_token),
                    token,
                }, LinePushResult::Accepted),

            /* An ellipsis token can begin a line */
            (LineType::Empty, token::Token::Ellipsis(token)) => (LineType::Ellipsis {
                title: None,
                token
            }, LinePushResult::Completed),

            /* An ellipsis token can occur on the same line as a title if the title is inline. */
            (LineType::Title(title_token), token::Token::Ellipsis(token))
                if sync::Arc::ptr_eq(title_token.node(), token.node())
                && title_token.node().props.title_display.is_inline()
                => (LineType::Ellipsis {
                    title: Some(title_token),
                    token,
                }, LinePushResult::Accepted),

            /* Summaries... */
            (LineType::Empty, token::Token::SummaryPreamble(token)) => (LineType::Summary {
                title: None,
                tokens: collections::VecDeque::from([token.into()]),
            }, LinePushResult::Accepted),
            
            (LineType::Title(title_token), token::Token::SummaryPreamble(token))
                if sync::Arc::ptr_eq(title_token.node(), token.node())
                && title_token.node().props.title_display.is_inline()
                => (LineType::Summary {
                    title: Some(title_token),
                    tokens: collections::VecDeque::from([token.into()]),
                }, LinePushResult::Accepted),

            (LineType::Summary { title, mut tokens }, token) => {
                let result = match (tokens.back(), token) {
                    (Some(token::Token::SummaryEpilogue(_)), _) => LinePushResult::Rejected,
                    (Some(_), token::Token::SummaryEpilogue(token)) => { tokens.push_back(token.into()); LinePushResult::Completed },
                    (Some(_), token) => { tokens.push_back(token); LinePushResult::Accepted },
                    (None, _) => panic!("LineType::Summary should have at least one token")
                };

                (LineType::Summary { title, tokens }, result)
            },

            (LineType::Empty, _class) => {
                println!("Attempted to start a line on a bad token");
                (LineType::Empty, LinePushResult::BadPosition)
            },
            
            (ty, _) => (ty, LinePushResult::Rejected)
        };

        self.ty = new_ty;

        result
    }

    pub fn is_empty(&self) -> bool {
        match &self.ty {
            LineType::Empty => true,
            _ => false,
        }
    }
    
    pub fn iter_tokens<'a>(&'a self) -> impl iter::Iterator<Item = token::TokenRef<'a>> {
        match &self.ty {
            LineType::Empty => util::PhiIteratorOf6::I1(iter::empty()),
            LineType::Blank(t) => util::PhiIteratorOf6::I2(iter::once(t.as_token_ref())),
            LineType::Title(t) => util::PhiIteratorOf6::I2(iter::once(t.as_token_ref())),
            LineType::Hexdump { title, tokens, .. } => util::PhiIteratorOf6::I3(title.as_ref().map(AsTokenRef::as_token_ref).into_iter().chain(tokens.iter().map(AsTokenRef::as_token_ref))),
            LineType::Bindump { title, tokens, .. } => util::PhiIteratorOf6::I6(title.as_ref().map(AsTokenRef::as_token_ref).into_iter().chain(tokens.iter().map(AsTokenRef::as_token_ref))),
            LineType::Hexstring { title, token, .. } => util::PhiIteratorOf6::I4(title.as_ref().map(AsTokenRef::as_token_ref).into_iter().chain(iter::once(token.as_token_ref()))),
            LineType::Summary { title, tokens, .. } => util::PhiIteratorOf6::I5(title.as_ref().map(AsTokenRef::as_token_ref).into_iter().chain(tokens.iter().map(AsTokenRef::as_token_ref))),
            LineType::Ellipsis { title, token } => util::PhiIteratorOf6::I4(title.as_ref().map(AsTokenRef::as_token_ref).into_iter().chain(iter::once(token.as_token_ref()))),
        }
    }

    pub fn into_iter(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        match self.ty {
            LineType::Empty => util::PhiIteratorOf6::I1(iter::empty()),
            LineType::Blank(t) => util::PhiIteratorOf6::I2(iter::once(t.into())),
            LineType::Title(t) => util::PhiIteratorOf6::I2(iter::once(t.into())),
            LineType::Hexdump { title, tokens, .. } => util::PhiIteratorOf6::I3(title.map(Into::into).into_iter().chain(tokens.into_iter().map(Into::into))),
            LineType::Bindump { title, tokens, .. } => util::PhiIteratorOf6::I6(title.map(Into::into).into_iter().chain(tokens.into_iter().map(Into::into))),
            LineType::Hexstring { title, token, .. } => util::PhiIteratorOf6::I4(title.map(Into::into).into_iter().chain(iter::once(token.into()))),
            LineType::Summary { title, tokens, .. } => util::PhiIteratorOf6::I5(title.map(Into::into).into_iter().chain(tokens.into_iter())),
            LineType::Ellipsis { title, token } => util::PhiIteratorOf6::I4(title.map(Into::into).into_iter().chain(iter::once(token.into()))),
        }
    }
}

impl PartialEq for Line {
    fn eq(&self, other: &Line) -> bool {
        match (&self.ty, &other.ty) {
            (LineType::Empty, LineType::Empty) => true,
            (LineType::Empty, _) => false,
            
            (LineType::Blank(tok1), LineType::Blank(tok2)) => tok1.eq(tok2),
            (LineType::Blank(_), _) => false,
            
            (LineType::Title(tok1), LineType::Title(tok2)) => tok1.eq(tok2),
            (LineType::Title(_), _) => false,
            
            (LineType::Hexdump {
                title: title1, node: node1, node_path: node_path1, node_addr: node_addr1, line_extent: line_extent1, tokens: tokens1
            }, LineType::Hexdump {
                title: title2, node: node2, node_path: node_path2, node_addr: node_addr2, line_extent: line_extent2, tokens: tokens2
            }) => title1.eq(title2)
                && sync::Arc::ptr_eq(node1, node2)
                && node_path1 == node_path2
                && node_addr1 == node_addr2
                && line_extent1.eq(line_extent2)
                && tokens1.iter().eq(tokens2.iter()),
            (LineType::Hexdump { .. }, _) => false,

            (LineType::Bindump {
                title: title1, node: node1, node_path: node_path1, node_addr: node_addr1, line_extent: line_extent1, tokens: tokens1
            }, LineType::Bindump {
                title: title2, node: node2, node_path: node_path2, node_addr: node_addr2, line_extent: line_extent2, tokens: tokens2
            }) => title1.eq(title2)
                && sync::Arc::ptr_eq(node1, node2)
                && node_path1 == node_path2
                && node_addr1 == node_addr2
                && line_extent1.eq(line_extent2)
                && tokens1.iter().eq(tokens2.iter()),
            (LineType::Bindump { .. }, _) => false,
            
            (LineType::Hexstring {
                title: title1, token: token1
            }, LineType::Hexstring {
                title: title2, token: token2
            }) => title1.eq(title2) && token1.eq(token2),
            (LineType::Hexstring { .. }, _) => false,

            (LineType::Summary {
                title: title1, tokens: tokens1
            }, LineType::Summary {
                title: title2, tokens: tokens2
            }) => title1.eq(title2) && tokens1.iter().eq(tokens2.iter()),
            (LineType::Summary { .. }, _) => false,

            (LineType::Ellipsis {
                title: title1, token: token1
            }, LineType::Ellipsis {
                title: title2, token: token2
            }) => title1.eq(title2) && token1.eq(token2),
            (LineType::Ellipsis { .. }, _) => false,
        }
    }
}

impl Eq for Line {
}

impl fmt::Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut toks = self.iter_tokens();
        if let Some(first) = toks.next() {
            for _ in 0..first.common().depth {
                write!(f, "  ")?;
            }

            write!(f, "{}", token::TokenTestFormat(first))?;
            
            for tok in toks {
                write!(f, "{}", token::TokenTestFormat(tok))?;
            }
        }

        Ok(())
    }
}

impl fmt::Debug for Line {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Line")
            .field("type", match &self.ty {
                LineType::Empty => &"empty",
                LineType::Blank(_) => &"blank",
                LineType::Title(_) => &"title",
                LineType::Hexdump { .. } => &"hexdump",
                LineType::Bindump { .. } => &"bindump",
                LineType::Hexstring { .. } => &"hexstring",
                LineType::Summary { .. } => &"summary",
                LineType::Ellipsis { .. } => &"ellipsis",
            })
            .field("tokens", &self.iter_tokens().map(|tok| token::TokenTestFormat(tok)).collect::<Vec<_>>())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::model::listing::token::AsTokenRef;
    
    #[test]
    fn test_hexstring_backwards_construction() {
        let node = structure::Node::builder()
            .name("foo")
            .size(0x8)
            .content_display(structure::ContentDisplay::Hexstring)
            .title_display(structure::TitleDisplay::Inline)
            .build();

        let mut line = Line::empty();

        assert_eq!(line.push_front(token::Token::Hexstring(token::Hexstring {
            common: token::TokenCommon {
                node: node.clone(),
                node_path: vec![],
                node_addr: addr::AbsoluteAddress::NULL,
                node_child_index: 0,
                depth: 0,
            },
            extent: addr::Extent::sized(0, 8),
        })), LinePushResult::Accepted);

        assert_eq!(line.push_front(token::Token::Title(token::Title {
            common: token::TokenCommon {
                node: node.clone(),
                node_path: vec![],
                node_addr: addr::AbsoluteAddress::NULL,
                node_child_index: 0,
                depth: 0,
            },
        })), LinePushResult::Accepted);
    }

    #[test]
    fn containing_position() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x78)
            .child(0x10, |b| b
                   .name("child0")
                   .size(0x20))
            .child(0x14, |b| b
                   .name("child1")
                   .size(0x64)
                   .children_display(structure::ChildrenDisplay::Summary)
                   .child(0x0, |b| b
                          .name("child1.0")
                          .size(0x18))
                   .child(0x20, |b| b
                          .name("child1.1")
                          .size(0x18))
                   .child(0x34, |b| b
                          .name("child1.2")
                          .size(0x18))
                   .child(0x48, |b| b
                          .name("child1.3")
                          .size(0x1c)))
            .child(0x60, |b| b
                   .name("child2")
                   .size(0x4))
            .build();

        /* Pregenerate all the lines from a simple forward walk through the whole document. */
        let mut position = stream::Position::at_beginning(root.clone());
        let mut lines = vec![];
        loop {
            let mut begin = position.clone();
            begin.canonicalize_next();
            
            let line = Line::next_from_position(&mut position);
            if line.is_empty() {
                break;
            }

            let mut end = position.clone();
            end.canonicalize_next();
            
            lines.push((begin, line, end));
        }

        let mut position = stream::Position::at_beginning(root.clone());
        let mut i = 0;
        loop {
            /* For every token in the stream, */
            let token = match position.gen_token() {
                stream::TokenGenerationResult::Ok(token) => token,
                stream::TokenGenerationResult::Skip => if position.move_next() { continue } else { break },
                stream::TokenGenerationResult::Boundary => break,
            };

            /* Find it in the array of lines we pregenerated. */
            let expected_index_in_line = loop {
                if let Some(index) = lines[i].1.iter_tokens().position(|t| t == token.as_token_ref()) {
                    break index;
                } else {
                    i+= 1;
                }
            };

            /* Make a new line containing just this token. */
            let mut line_end = position.clone();
            let (line, mut line_begin, index_in_line) = Line::containing_position(&mut line_end);
            line_begin.canonicalize_next();
            line_end.canonicalize_next();

            /* Check that the line matches the one from the pregenerated array. */
            if line != lines[i].1 || index_in_line != expected_index_in_line || line_begin != lines[i].0 || line_end != lines[i].2 {
                println!("seeked to {:?}", token);
                println!("line from forward walk       : {}", lines[i].1);
                println!("line from containing_position: {}", line);
                println!("expected index {}, got index {}", expected_index_in_line, index_in_line);
                
                println!("begin position [actual]  : {:#?}", line_begin);
                println!("begin position [expected]: {:#?}", lines[i].0);
                    
                println!("end position [actual]  : {:#?}", line_end);
                println!("end position [expected]: {:#?}", lines[i].2);
                
                panic!("mismatched");
            }

            position.move_next();
        }
    }
}
