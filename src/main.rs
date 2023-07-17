extern crate core;
mod portable_executable;
mod language;



fn main() -> Result<(), String> {
    let res = language::parse_and_compile("if_prog.txt")?;
    let out  = portable_executable::CoffObject::amd_x64("if_prog.obj")
        .add_function(res, "main")
        .build();
    std::fs::write("if_prog.obj", &out).map_err(|e| e.to_string())?;
    Ok(())
}
