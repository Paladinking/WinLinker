mod pe_types;

use std::io::Result;
use pe_types::DosStub;
use pe_types::FromBytes;
use pe_types::PortableExecutable;


fn main() -> Result<()> {
    let data = std::fs::read("main.exe")?;
    let header = PortableExecutable::from_bytes(&mut data.iter());

    let header_data : Vec<u8> = DosStub {offset: 0xd0}.into();
    let h = header.unwrap();

    println!("Alignment : {}, {}",h.optional_header.as_ref().unwrap().section_alignment, h.optional_header.as_ref().unwrap().file_alignment);
    println!("{:?}", h.optional_header.as_ref().unwrap().data_directories);
    println!("{:?}", h.section_table);

    std::fs::write("test.exe", header_data)?;
    Ok(())
}
