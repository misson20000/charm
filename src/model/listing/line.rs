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
        node_addr: addr::Address,
        line_extent: addr::Extent,
        tokens: collections::VecDeque<token::Hexdump>
    },
    Hexstring {
        title: Option<token::Title>,
        token: token::Hexstring,
    },
    Summary {
        title: Option<token::Title>,
        tokens: collections::VecDeque<token::Token>
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
                
                token::Token::Hexdump(token) => LineType::Hexdump {
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

            (LineType::Empty, _) => {
                (LineType::Empty, LinePushResult::BadPosition)
            },
            
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
            LineType::Empty => util::PhiIteratorOf5::I1(iter::empty()),
            LineType::Blank(t) => util::PhiIteratorOf5::I2(iter::once(t.as_token_ref())),
            LineType::Title(t) => util::PhiIteratorOf5::I2(iter::once(t.as_token_ref())),
            LineType::Hexdump { title, tokens, .. } => util::PhiIteratorOf5::I3(title.as_ref().map(AsTokenRef::as_token_ref).into_iter().chain(tokens.iter().map(AsTokenRef::as_token_ref))),
            LineType::Hexstring { title, token, .. } => util::PhiIteratorOf5::I4(title.as_ref().map(AsTokenRef::as_token_ref).into_iter().chain(iter::once(token.as_token_ref()))),
            LineType::Summary { title, tokens, .. } => util::PhiIteratorOf5::I5(title.as_ref().map(AsTokenRef::as_token_ref).into_iter().chain(tokens.iter().map(AsTokenRef::as_token_ref))),
        }
    }

    pub fn into_iter(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        match self.ty {
            LineType::Empty => util::PhiIteratorOf5::I1(iter::empty()),
            LineType::Blank(t) => util::PhiIteratorOf5::I2(iter::once(t.into())),
            LineType::Title(t) => util::PhiIteratorOf5::I2(iter::once(t.into())),
            LineType::Hexdump { title, tokens, .. } => util::PhiIteratorOf5::I3(title.map(Into::into).into_iter().chain(tokens.into_iter().map(Into::into))),
            LineType::Hexstring { title, token, .. } => util::PhiIteratorOf5::I4(title.map(Into::into).into_iter().chain(iter::once(token.into()))),
            LineType::Summary { title, tokens, .. } => util::PhiIteratorOf5::I5(title.map(Into::into).into_iter().chain(tokens.into_iter())),
        }
    }
}

impl PartialEq for Line {
    fn eq(&self, other: &Line) -> bool {
        match (&self.ty, &other.ty) {
            (LineType::Empty, LineType::Empty) => true,
            (LineType::Blank(tok1), LineType::Blank(tok2)) => tok1.eq(tok2),
            (LineType::Title(tok1), LineType::Title(tok2)) => tok1.eq(tok2),
            
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

            (LineType::Hexstring {
                title: title1, token: token1
            }, LineType::Hexstring {
                title: title2, token: token2
            }) => title1.eq(title2) && token1.eq(token2),

            (LineType::Summary {
                title: title1, tokens: tokens1
            }, LineType::Summary {
                title: title2, tokens: tokens2
            }) => title1.eq(title2) && tokens1.iter().eq(tokens2.iter()),
            
            _ => false
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
                LineType::Hexstring { .. } => &"hexstring",
                LineType::Summary { .. } => &"summary",
            })
            .field("tokens", &self.iter_tokens().map(|tok| token::TokenTestFormat(tok)).collect::<Vec<_>>())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
                node_addr: addr::unit::NULL,
                depth: 0,
            },
            extent: addr::Extent::sized_u64(0, 8),
            truncated: false,
        })), LinePushResult::Accepted);

        assert_eq!(line.push_front(token::Token::Title(token::Title {
            common: token::TokenCommon {
                node: node.clone(),
                node_path: vec![],
                node_addr: addr::unit::NULL,
                depth: 0,
            },
        })), LinePushResult::Accepted);
    }
}
