extern crate core;

use crate::portable_executable::SubSystem;

mod portable_executable;
mod language;



fn main() -> Result<(), String> {
    let res = language::parse_and_compile("if_prog.txt")?;
    std::fs::write("if_prog.bin", &res).map_err(|e| e.to_string())?;
    let obj  = portable_executable::CoffObject::amd_x64("if_prog.obj")
        .add_function(res, "main")
        .build();
    let obj_out = obj.write();
    let img = portable_executable::CoffImage::amd_x64(SubSystem::Console)
        .add_object(obj).build();

    let img_out = img.write();

    std::fs::write("if_prog.obj", &obj_out).map_err(|e| e.to_string())?;
    std::fs::write("if_prog.exe", &img_out).map_err(|e| e.to_string())?;
    Ok(())
}
