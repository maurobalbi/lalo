mod lexer;
mod token;
mod error;

use std::io;
use io::Write;

pub use crate::lexer::Lexer;

fn main() -> io::Result<()> {

    print!("> ");
    io::stdout().flush().unwrap();

    let mut input = String::new();

    io::stdin().read_line(&mut input)?;

    match Lexer::new(&input).next() {
        Ok(Some(token)) => println!("{:?}", token),
        _ => println!("whatwhatwhat"),
    }

   
    Ok(())
}
