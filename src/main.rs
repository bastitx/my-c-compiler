mod lexer;
mod ast;
mod parser;
mod generator;

use std::{fs, env, path};

fn main() {
    let args: Vec<String> = env::args().collect();
    // println!("Got {:?} args", args);
    let input = if args.len() == 2 {
        fs::read_to_string(&args[1]).unwrap()
    } else {
        "int main(){\nreturn 2; }".to_string()
    };
    // println!("{}", input);
    let tokens = lexer::lex(input.as_str());
    // println!("{:?}", tokens);
    let ast = parser::parse(tokens);
    // println!("{:?}", ast);
    let output = generator::generate(ast);
    // println!("{}", output);
    if args.len() == 2 {
        let file_path = path::Path::new(&args[1]);
        let output_file = file_path.with_extension("s");
        // println!("Writing to {:?}", output_file);
        fs::write(output_file, output).unwrap();
    }
}