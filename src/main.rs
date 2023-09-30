mod lexer;
mod ast;
mod parser;
mod generator;

use std::{fs, env, path};

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 {
        fs::read_to_string(&args[1]).unwrap()
    } else {
        "int main(){\nint a = 0; int b = 0; a && (b = 5); return b; }".to_string()
    };
    let tokens = lexer::lex(input.as_str());
    let ast = parser::parse(tokens);
    let output = generator::generate(ast);
    if args.len() == 2 {
        let file_path = path::Path::new(&args[1]);
        let output_file = file_path.with_extension("s");
        fs::write(output_file, output).unwrap();
    }
}