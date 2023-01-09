use std::sync;
use std::vec;

use charm::model::document;
use charm::model::listing::layout;
use charm::model::listing::token;

struct Line {
    indent: usize,
    tokens: vec::Vec<token::Token>
}

impl layout::Line for Line {
    type TokenIterator = vec::IntoIter<token::Token>;
    
    fn from_tokens(tokens: vec::Vec<token::Token>) -> Self {
        Line {
            indent: tokens[0].depth,
            tokens,
        }
    }

    fn to_tokens(self) -> Self::TokenIterator {
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

    for line in window.lines {
        for _ in 0..line.indent {
            print!("  ");
        }
        for token in line.tokens {
            print!("{}", token::TokenTestFormat(&token));
        }
        println!();
    }
}
