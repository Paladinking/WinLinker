use std::io::Write;
use crate::portable_executable::sections::{Section, SectionHeader, TextSection};
use crate::portable_executable::symbol_table::{CoffSymbol, StringTable, SymbolTable};
use std::time::{SystemTime, UNIX_EPOCH};

#[allow(dead_code)]
pub mod coff_header_characteristic {
    pub const IMAGE_FILE_RELOCS_STRIPPED : u16 = 0x1;
    pub const IMAGE_FILE_EXECUTABLE_IMAGE : u16 = 0x2;
    pub const IMAGE_FILE_LINE_NUMS_STRIPPED : u16 = 0x4;
    pub const IMAGE_FILE_LOCAL_SYMS_STRIPPED : u16 = 0x8;
    pub const IMAGE_FILE_AGGRESSIVE_WS_TRIM : u16 = 0x10;
    pub const IMAGE_FILE_LARGE_ADDRESS_AWARE : u16 = 0x20;
    pub const IMAGE_FILE_BYTES_REVERSED_LO : u16 = 0x80;
    pub const IMAGE_FILE_32BIT_MACHINE : u16 = 0x100;
    pub const IMAGE_FILE_DEBUG_STRIPPED : u16 = 0x200;
    pub const IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP : u16 = 0x400;
    pub const IMAGE_FILE_NET_RUN_FROM_SWAP : u16 = 0x800;
    pub const IMAGE_FILE_SYSTEM : u16 = 0x1000;
    pub const IMAGE_FILE_DLL : u16 = 0x2000;
    pub const IMAGE_FILE_UP_SYSTEM_ONLY : u16 = 0x4000;
    pub const IMAGE_FILE_BYTES_REVERSED_HI : u16 = 0x8000;
}

mod machine_type {
    pub const IMAGE_FILE_MACHINE_UNKNOWN : u16 = 0x0;
    pub const IMAGE_FILE_MACHINE_AMD64 : u16 = 0x8664;
}

#[repr(u16)]
#[derive(Clone, Copy)]
pub enum MachineType {
    Unknown = machine_type::IMAGE_FILE_MACHINE_UNKNOWN,
    Amd64 = machine_type::IMAGE_FILE_MACHINE_AMD64
}

impl Into<u16> for MachineType {
    fn into(self) -> u16 {
        self as u16
    }
}

impl Into<MachineType> for u16 {
    fn into(self) -> MachineType {
        match self {
            machine_type::IMAGE_FILE_MACHINE_AMD64 => MachineType::Amd64,
            _ => MachineType::Unknown
        }
    }
}

pub struct CoffFileHeader {
    pub machine : MachineType,
    pub number_of_sections : u16,
    pub time_date_stamp : u32,
    pub pointer_to_symbol_table : u32,
    pub number_of_symbols : u32,
    pub size_of_optional_header : u16,
    pub characteristics : u16
}

impl CoffFileHeader {
    pub const SIZE : usize = 20;

    fn new(machine : MachineType, characteristics : u16) -> CoffFileHeader {
        CoffFileHeader {
            machine, number_of_sections : 0, time_date_stamp : 0,
            pointer_to_symbol_table : 0, number_of_symbols : 0,
            size_of_optional_header : 0, characteristics
        }
    }

    fn write(&self, data : &mut Vec<u8>) {
        let machine_type : u16 = self.machine.into();
        data.extend_from_slice(&machine_type.to_le_bytes());
        data.extend_from_slice(&self.number_of_sections.to_le_bytes());
        data.extend_from_slice(&self.time_date_stamp.to_le_bytes());
        data.extend_from_slice(&self.pointer_to_symbol_table.to_le_bytes());
        data.extend_from_slice(&self.number_of_symbols.to_le_bytes());
        data.extend_from_slice(&self.size_of_optional_header.to_le_bytes());
        data.extend_from_slice(&self.characteristics.to_le_bytes());
    }
}

pub struct CoffObject {
    header : CoffFileHeader,
    symbol_table : SymbolTable,
    section_headers : Vec<SectionHeader>,
    text_section : Option<TextSection>
}

impl CoffObject {

    pub fn amd_x64(file_name : &str) -> CoffObject {
        let header = CoffFileHeader::new(MachineType::Amd64, 0);
        let mut symbol_table = SymbolTable::new();
        symbol_table.add_file(file_name.to_string());
        CoffObject {
            header, symbol_table, section_headers : Vec::new(), text_section : None
        }
    }

    pub fn add_function(mut self, data : Vec<u8>, name : &str) -> Self {
        if self.text_section.is_none() {
            self.text_section.replace(TextSection::new(data));
        } else {
            self.text_section.as_mut().unwrap().data.extend_from_slice(&data);
        }
        self.symbol_table.add_external(name, 1);
        self
    }

    pub fn build(&mut self) -> Vec<u8> {
        let text_section_size;
        if let Some(text_section) = &mut self.text_section {
            self.symbol_table.add_section(".text".to_owned(), 1,
            text_section.data.len() as u32, 0);
            text_section.header = self.section_headers.len();
            text_section_size = text_section.data.len();
            self.section_headers.push(text_section.get_object_header());

        } else {
            text_section_size = 0;
        }
        self.header.number_of_sections = self.section_headers.len() as u16;
        self.header.time_date_stamp = SystemTime::now().duration_since(UNIX_EPOCH)
            .expect("Time is before UNIX_EPOCH").as_secs() as u32;
        self.header.pointer_to_symbol_table = (CoffFileHeader::SIZE +
            self.section_headers.len() * SectionHeader::SIZE + text_section_size).try_into()
            .expect("To big object file");
        self.header.number_of_symbols = self.symbol_table.symbol_count;

        let mut out = Vec::with_capacity(CoffFileHeader::SIZE +
                self.section_headers.len() * SectionHeader::SIZE + text_section_size +
                self.symbol_table.symbol_count as usize * CoffSymbol::SIZE
        );
        self.header.write(&mut out);

        if let Some(text_section) = &self.text_section {
            self.section_headers[text_section.header].pointer_to_raw_data =
                (out.len() + self.section_headers.len() * SectionHeader::SIZE) as u32;
        }

        let mut string_table = StringTable::new();
        for header in &self.section_headers {
            header.write(&mut out, &mut string_table);
        }

        debug_assert!(out.len() == CoffFileHeader::SIZE +
            self.section_headers.len() * SectionHeader::SIZE);

        if let Some(text_section) = &self.text_section {
            out.extend_from_slice(&text_section.data);
        }
        self.symbol_table.write(&mut out, &mut string_table);
        string_table.write(&mut out);

        return out;
    }

}
