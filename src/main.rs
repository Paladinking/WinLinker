use std::io::Result;

use portable_executable::types::PortableExecutable;
use portable_executable::serialize::FromBytes;
use portable_executable::types::{CoffFileHeader, SectionHeader};
use portable_executable::characteristics;

mod portable_executable;

fn main() -> Result<()> {
    let data = std::fs::read("grapple.exe")?;
    let header = PortableExecutable::from_bytes_iter(&mut data.iter());

    let h = header.unwrap();

    println!("Alignment : {}, {}",h.optional_header.as_ref().unwrap().section_alignment, h.optional_header.as_ref().unwrap().file_alignment);
    for dd in &h.optional_header.as_ref().unwrap().data_directories {
        if dd.virtual_address != 0 {
            println!("{:?}", dd);
        }
    }
    println!("{:?}", characteristics::get_all(&CoffFileHeader::CHARACTERISTICS, h.coff_file_header.characteristics));
    for sh  in h.section_table {
        println!("{:?}, {:?}", sh, characteristics::get_all(&SectionHeader::CHARACTERISTICS, sh.characteristics));
    }
    println!("{:?}", h.import_section);
    if let Some(reloc_blocks) = h.reloc_section {
        for block in reloc_blocks {
            println!("{:?}", block);
        }
    }
    Ok(())
}
