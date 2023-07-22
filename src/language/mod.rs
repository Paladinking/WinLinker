mod parser;
mod amd_win64;
mod types;
mod operator;

pub fn parse_and_compile(data : String) -> Result<Vec<u8>, String>{
    let program = parser::read_program(data).map_err(|e| e.to_string())?;
    let program = amd_win64::compiler::InstructionBuilder::new(&program.variables)
        .with(&program.statements).compile();
    return Ok(program);
}