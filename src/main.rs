use std::io::Result;

use portable_executable::types::PortableExecutable;
use portable_executable::serialize::FromBytes;
use portable_executable::types::{CoffFileHeader, SectionHeader};
use portable_executable::characteristics;

mod portable_executable;
mod language;

fn main() -> Result<()> {
    language::parser::read_program(String::from(""));
    Ok(())
}
