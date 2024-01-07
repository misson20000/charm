use std::iter;
use std::sync;
use std::vec;

use charm::model::document;
use charm::model::listing::layout;
use charm::model::listing::token;
use charm::model::listing::token::TokenKind;

struct Line {
    indent: usize,
    tokens: vec::Vec<token::Token>
}

impl layout::LineView for Line {
    fn from_line(line: layout::Line) -> Self {
        let tokens: vec::Vec<token::Token> = line.to_tokens().collect();
        
        Line {
            indent: tokens[0].common().depth,
            tokens,
        }
    }

    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        self.tokens.iter().map(TokenKind::as_ref)
    }
    
    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        self.tokens.into_iter()
    }
}

fn main() {
    let mut args = std::env::args_os();
    args.next().expect("expected argv[0]");
    
    let xml_path = args.next().expect("expected path to xml");

    let document = sync::Arc::new(document::Document::load_from_testing_structure(xml_path).unwrap());
    let mut window = layout::Window::<Line>::new(document);

    window.resize(150);

    for line in window.line_views {
        for _ in 0..line.indent {
            print!("  ");
        }
        for token in line.tokens {
            print!("{}", token::TokenTestFormat(token.as_ref()));
        }
        println!();
    }
}
