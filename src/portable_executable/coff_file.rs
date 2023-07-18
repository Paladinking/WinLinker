use crate::portable_executable::sections::{SectionHeader, SectionHeaderCharacteristic, TextSection};
use crate::portable_executable::symbol_table::{CoffSymbol, StringTable, SymbolTable, storage_class};
use std::time::{SystemTime, UNIX_EPOCH};

const TEXT_SECTION_INDEX : usize = 0;

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
    text_section : Option<TextSection>,

    section_size : usize
}

impl CoffObject {
    pub fn amd_x64(file_name : &str) -> CoffObject {
        let header = CoffFileHeader::new(MachineType::Amd64, 0);
        let mut symbol_table = SymbolTable::new();
        symbol_table.add_file(file_name.to_string());
        CoffObject {
            header, symbol_table, section_headers : Vec::new(), text_section : None,
            section_size : 0
        }
    }

    pub fn add_function(mut self, data : Vec<u8>, name : &str) -> Self {
        if self.text_section.is_none() {
            self.text_section.replace(TextSection::new(data));
        } else {
            self.text_section.as_mut().unwrap().data.extend_from_slice(&data);
        }
        self.symbol_table.add_external(name, 1, 0);
        self
    }

    pub fn build(mut self) -> Self {
        self.section_size = 0;
        if let Some(text_section) = &mut self.text_section {
            self.section_size += text_section.data.len();
            let text_section_size = text_section.data.len().try_into()
                .expect("To big .text section");
            self.symbol_table.add_section(".text".to_owned(), 1,
                                          text_section_size, 0);
            let ptr_to_raw_data=
                (CoffFileHeader::SIZE + (self.section_headers.len() + 1) * SectionHeader::SIZE)
                    .try_into().expect("To big file");
            self.section_headers.push(TextSection::get_object_header(
                text_section_size,
                 ptr_to_raw_data
            ));
        }
        self.header.number_of_sections = self.section_headers.len() as u16;
        self.header.time_date_stamp = SystemTime::now().duration_since(UNIX_EPOCH)
            .expect("Time is before UNIX_EPOCH").as_secs() as u32;
        self.header.pointer_to_symbol_table = (CoffFileHeader::SIZE +
            self.section_headers.len() * SectionHeader::SIZE + self.section_size).try_into()
            .expect("To big object file");
        self.header.number_of_symbols = self.symbol_table.symbol_count;
        return self;
    }

    pub fn write(&self) -> Vec<u8> {

        let mut out = Vec::with_capacity(CoffFileHeader::SIZE +
            self.section_headers.len() * SectionHeader::SIZE + self.section_size +
            self.symbol_table.symbol_count as usize * CoffSymbol::SIZE
        );

        let mut string_table = StringTable::new();

        self.header.write(&mut out);

        for header in &self.section_headers {
            header.write(&mut out, Some(&mut string_table));
        }

        if let Some(text_section) = &self.text_section {
            text_section.write(&mut out);
        }

        self.symbol_table.write(&mut out, &mut string_table);
        string_table.write(&mut out);
        return out;
    }
}

#[derive(Copy, Clone)]
#[repr(u16)]
pub enum SubSystem {
    Unknown = 0,
    Windows = 2,
    Console = 3
}

impl From<u16> for SubSystem {
    fn from(val: u16) -> Self {
        if val > 3 {
            return  SubSystem::Unknown;
        }
        return unsafe {std::mem::transmute(val)};
    }
}

#[derive(Copy, Clone)]
#[repr(u32)]
#[allow(dead_code)]
pub enum DataDirectory {
    ExportTable = 0,
    ImportTable = 1,
    ResourceTable = 2,
    ExceptionTable = 3,
    CertificateTable =  4,
    BaseRelocationTable = 5,
    Debug = 6,
    Architecture = 7,
    GlobalPtr = 8,
    TLSTable = 9,
    LoadConfigTable = 10,
    BoundImport = 11,
    IAT = 12,
    DelayImportDescriptor = 13,
    CLRRuntimeHeader = 14,
    Reserved = 15,
    Unknown = 16
}

impl From<u32> for DataDirectory {
    fn from(val: u32) -> Self {
        if val >= 16 {
            return DataDirectory::Unknown;
        }
        return unsafe {std::mem::transmute(val)} ;
    }
}

pub struct ImageDataDirectory {
    pub name : DataDirectory,
    pub virtual_address: u32,
    pub size : u32
}

impl ImageDataDirectory {
    fn write(&self, data : &mut Vec<u8>) {
        data.extend_from_slice(&self.virtual_address.to_le_bytes());
        data.extend_from_slice(&self.size.to_le_bytes());
    }
}

pub struct OptionalHeader {
    // Standard fields
    pub magic : u16,
    pub major_linker_version : u8,
    pub minor_linker_version : u8,
    pub size_of_code : u32,
    pub size_of_initialized_data : u32,
    pub size_of_uninitialized_data : u32,
    pub address_of_entry_point : u32,
    pub base_of_code : u32,

    // Windows specific fields
    pub image_base : u64,
    pub section_alignment : u32,
    pub file_alignment : u32,
    pub major_os_version : u16,
    pub minor_os_version : u16,
    pub major_image_version : u16,
    pub minor_image_version : u16,
    pub major_subsystem_version : u16,
    pub minor_subsystem_version : u16,
    pub win_32_version_value : u32,
    pub size_of_image : u32,
    pub size_of_headers : u32,
    pub check_sum : u32,
    pub subsystem : u16,
    pub dll_characteristics : u16,
    pub size_of_stack_reserve : u64,
    pub size_of_stack_commit : u64,
    pub size_of_heap_reserve : u64,
    pub size_of_heap_commit : u64,
    pub loader_flags : u32,
    pub number_of_rva_and_sizes : u32,

    pub data_directories : Vec<ImageDataDirectory>
}

impl OptionalHeader {
    const SIZE : usize = 240;

    fn new(subsystem : SubSystem) -> OptionalHeader {
        OptionalHeader {
            magic : 0x20B, major_linker_version : 0, minor_linker_version: 1,
            size_of_code : 0, size_of_initialized_data : 0, size_of_uninitialized_data : 0,
            address_of_entry_point : 0, base_of_code : 0, image_base : 0x140000000,
            section_alignment : 0x1000, file_alignment : 0x200, major_os_version : 6,
            minor_os_version : 0, major_image_version : 0, minor_image_version : 0,
            major_subsystem_version : 6, minor_subsystem_version : 0,
            win_32_version_value : 0, size_of_image : 0, size_of_headers : 0,
            check_sum : 0, subsystem : subsystem as u16,
            dll_characteristics : 0x0020 | 0x0100 | 0x8000, size_of_stack_reserve : 0x100000,
            size_of_stack_commit : 0x1000, size_of_heap_reserve : 0x100000,
            size_of_heap_commit : 0x1000, loader_flags : 0, 
            number_of_rva_and_sizes : DataDirectory::Reserved as u32 + 1,
            data_directories : (0_u32..=DataDirectory::Reserved as u32).map(|i|
                ImageDataDirectory {
                    name: i.into(),
                    virtual_address: 0,
                    size: 0,
                }
            ).collect()
        }
    }

    fn write(&self, data : &mut Vec<u8>) {
        data.extend_from_slice(&self.magic.to_le_bytes());
        data.extend_from_slice(&self.major_linker_version.to_le_bytes());
        data.extend_from_slice(&self.minor_linker_version.to_le_bytes());
        data.extend_from_slice(&self.size_of_code.to_le_bytes());
        data.extend_from_slice(&self.size_of_initialized_data.to_le_bytes());
        data.extend_from_slice(&self.size_of_uninitialized_data.to_le_bytes());
        data.extend_from_slice(&self.address_of_entry_point.to_le_bytes());
        data.extend_from_slice(&self.base_of_code.to_le_bytes());
        data.extend_from_slice(&self.image_base.to_le_bytes());
        data.extend_from_slice(&self.section_alignment.to_le_bytes());
        data.extend_from_slice(&self.file_alignment.to_le_bytes());
        data.extend_from_slice(&self.major_os_version.to_le_bytes());
        data.extend_from_slice(&self.minor_os_version.to_le_bytes());
        data.extend_from_slice(&self.major_image_version.to_le_bytes());
        data.extend_from_slice(&self.minor_image_version.to_le_bytes());
        data.extend_from_slice(&self.major_subsystem_version.to_le_bytes());
        data.extend_from_slice(&self.minor_subsystem_version.to_le_bytes());
        data.extend_from_slice(&self.win_32_version_value.to_le_bytes());
        data.extend_from_slice(&self.size_of_image.to_le_bytes());
        data.extend_from_slice(&self.size_of_headers.to_le_bytes());
        data.extend_from_slice(&self.check_sum.to_le_bytes());
        data.extend_from_slice(&self.subsystem.to_le_bytes());
        data.extend_from_slice(&self.dll_characteristics.to_le_bytes());
        data.extend_from_slice(&self.size_of_stack_reserve.to_le_bytes());
        data.extend_from_slice(&self.size_of_stack_commit.to_le_bytes());
        data.extend_from_slice(&self.size_of_heap_reserve.to_le_bytes());
        data.extend_from_slice(&self.size_of_heap_commit.to_le_bytes());
        data.extend_from_slice(&self.loader_flags.to_le_bytes());
        data.extend_from_slice(&self.number_of_rva_and_sizes.to_le_bytes());
        for data_dir in &self.data_directories {
            data_dir.write(data);
        }
    }
}

pub struct CoffImage {
    coff_header : CoffFileHeader,
    optional_header : OptionalHeader,
    section_headers : Vec<SectionHeader>,
    text_section : Option<TextSection>,

    main_offset : Option<u32>,
    section_size : usize
}

impl CoffImage {
    const DOS_STUB : [u8; 128] = [
        0x4d, 0x5a, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00,
        0x04, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00,
        0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x80, 0, 0, 0,
        0x0e, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD,
        0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68,

        0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72,
        0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F,

        0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6E,
        0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20,

        0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A,
        0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ];

    pub fn amd_x64(subsystem : SubSystem) -> CoffImage {
        CoffImage {
            coff_header : CoffFileHeader::new(MachineType::Amd64,
                                              0x0020 | 0x0002),
            optional_header : OptionalHeader::new(subsystem),
            section_headers : Vec::new(),
            text_section : None, main_offset : None,
            section_size : 0
        }
    }

    pub fn add_object(mut self, mut object : CoffObject) -> Self {
        self.text_section = object.text_section.take();
        self.section_headers.append(&mut object.section_headers);

        for symbol in &object.symbol_table.symbols {
            if symbol.storage_class == storage_class::IMAGE_SYM_CLASS_EXTERNAL &&
                symbol.name == "main" {
                if self.main_offset.is_some() {
                    panic!("Multiple main functions");
                }
                self.main_offset.replace(u32::from_le_bytes(symbol.value.clone()));
            }
        }
        return self;
    }

    pub fn build(mut self) -> Self {
        self.coff_header.size_of_optional_header = OptionalHeader::SIZE as u16;
        self.optional_header.size_of_headers = (Self::DOS_STUB.len() +
            4 + CoffFileHeader::SIZE + OptionalHeader::SIZE +
            self.section_headers.len() * SectionHeader::SIZE) as u32;
        let margin = self.optional_header.size_of_headers % self.optional_header.file_alignment;
        if margin > 0 {
            self.optional_header.size_of_headers += self.optional_header.file_alignment - margin;
        }
        let mut sections_rva = self.optional_header.size_of_headers;
        let margin = sections_rva % self.optional_header.section_alignment;
        if margin > 0 {
            sections_rva += self.optional_header.section_alignment - margin;
        }

        if let Some(text_section) = &mut self.text_section {
            let size : u32 = text_section.data.len().try_into()
                .expect("To big .text section");

            self.section_headers[TEXT_SECTION_INDEX].size_of_raw_data = size;
            let margin = size % self.optional_header.file_alignment;
            if margin > 0 {
                self.section_headers[TEXT_SECTION_INDEX].size_of_raw_data +=
                    self.optional_header.file_alignment - margin;
            }
            self.section_size += self.section_headers[TEXT_SECTION_INDEX].size_of_raw_data as usize;
            self.optional_header.size_of_code =
                self.section_headers[TEXT_SECTION_INDEX].size_of_raw_data;
            text_section.data.resize(
                self.section_headers[TEXT_SECTION_INDEX].size_of_raw_data as usize, 0);
            self.section_headers[TEXT_SECTION_INDEX].pointer_to_raw_data =
                self.optional_header.size_of_headers;
            self.section_headers[TEXT_SECTION_INDEX].virtual_address = sections_rva;
            self.optional_header.base_of_code = sections_rva;
            self.optional_header.address_of_entry_point = sections_rva + self.main_offset.unwrap();
            self.section_headers[TEXT_SECTION_INDEX].virtual_size = size;
            self.section_headers[TEXT_SECTION_INDEX].characteristics =
                SectionHeaderCharacteristic::ImageScnCntCode as u32 |
                    SectionHeaderCharacteristic::ImageScnMemExecute as u32 |
                    SectionHeaderCharacteristic::ImageScnMemRead as u32;
            sections_rva += size;
            let margin = sections_rva % self.optional_header.section_alignment;
            if margin > 0 {
                sections_rva += self.optional_header.section_alignment - margin;
            }
        } else {
            panic!("No .text section");
        }

        self.optional_header.size_of_image = sections_rva;

        self.coff_header.time_date_stamp = SystemTime::now().duration_since(UNIX_EPOCH)
            .expect("Time is before UNIX_EPOCH").as_secs() as u32;
        self.coff_header.number_of_sections = self.section_headers.len() as u16;
        return self;
    }

    pub fn write(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(
            Self::DOS_STUB.len() + 4 + OptionalHeader::SIZE +
                self.section_headers.len() * SectionHeader::SIZE + self.section_size
        );

        out.extend_from_slice(&Self::DOS_STUB);
        out.extend_from_slice(b"PE\0\0");
        self.coff_header.write(&mut out);
        self.optional_header.write(&mut out);
        for section_header in &self.section_headers {
            section_header.write(&mut out, None);
        }
        if out.len() % self.optional_header.file_alignment as usize != 0 {
            out.resize(out.len() +
                self.optional_header.file_alignment as usize -
                out.len() % self.optional_header.file_alignment as usize, 0)
        }
        self.text_section.as_ref().unwrap().write(&mut out);

        return out;
    }

}
