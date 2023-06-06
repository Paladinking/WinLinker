extern crate core;

use std::io::Result;

mod portable_executable;
mod language;

fn main() -> Result<()> {
    language::parser::read_program(String::from("if_prog.txt"));
    Ok(())
}
