use std::io::Result;

mod portable_executable;
mod language;

fn main() -> Result<()> {
    language::parser::read_program(String::from("test_prog.txt"));
    Ok(())
}
