extern crate core;

use crate::portable_executable::SubSystem;

mod portable_executable;
mod language;

fn main() -> Result<(), isize> {
    let mut file_name = std::env::args().skip(1)
        .next().unwrap_or("if_prog.txt".to_owned());
    let file_base = file_name.rsplitn(2, '.').last().unwrap();
    let data = std::fs::read_to_string(&file_name).map_err(|e| {
        println!("Could not open file '{}', {}", &file_name, e);
        -1_isize
    })?;
    let res = language::parse_and_compile(data).map_err(|e| {
        println!("{}", e);
        -2_isize
    })?;
    let bin_file = file_base.to_owned() + ".bin";
    std::fs::write(&bin_file, &res).map_err(|e| {
        println!("Could not write {}, {}", &bin_file, e);
        -3_isize
    })?;
    let obj_file = file_base.to_owned() + ".obj";
    let obj= portable_executable::CoffObject::amd_x64(&obj_file)
        .add_function(res, "main")
        .build();
    let obj_out = obj.write();
    let img = portable_executable::CoffImage::amd_x64(SubSystem::Console)
        .add_object(obj).build();

    let img_out = img.write();

    std::fs::write(&obj_file, &obj_out).map_err(|e| {
        println!("Could not create file {}, {}", obj_file, e);
        -4_isize
    })?;
    let exe_file = file_base.to_owned() + ".exe";
    std::fs::write(&exe_file, &img_out).map_err(|e| {
        println!("Could not create file {}, {}", exe_file, e);
        -5_isize
    })?;
    Ok(())
}
