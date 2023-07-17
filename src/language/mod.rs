mod parser;
mod amd_win64;
mod types;
mod operator;

pub fn parse_and_compile(path : &str) -> Result<Vec<u8>, String>{
    let data = std::fs::read_to_string(path).map_err(|e|
        format!("Could not open file '{}', ", path) + &e.to_string())?;
    let program = parser::read_program(data).map_err(|e| e.to_string())?;
    let program = amd_win64::compiler::InstructionBuilder::new(&program.variables)
        .with(&program.statements).compile();
    return Ok(program);
}