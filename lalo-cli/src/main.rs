use lalo::*;

use io::Write;
use std::io;

fn main() -> io::Result<()> {
    print!("> ");
    io::stdout().flush().unwrap();

    let mut input = String::new();

    io::stdin().read_line(&mut input)?;

    let ast = parse_all::<lalo::ast::Expr>(&input);

    println!("{:?}", ast);

    Ok(())
}
