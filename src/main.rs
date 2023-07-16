extern crate core;
mod portable_executable;
mod language;



fn main() -> Result<(), String> {
    language::parse_and_compile("if_prog.txt", "out.bin")?;

    Ok(())
}
