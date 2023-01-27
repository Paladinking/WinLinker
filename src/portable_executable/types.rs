use std::collections::HashMap;
use std::fmt::{Debug, Formatter, LowerHex, Display};

#[derive(Debug)]
pub struct DosStub {
    pub offset : u32
}


#[derive(Debug)]
pub struct CoffFileHeader {
    pub machine : u16,
    pub number_of_sections : u16,
    pub time_date_stamp : u32,
    pub pointer_to_symbol_table : u32,
    pub number_of_symbols : u32,
    pub size_of_optional_header : u16,
    pub characteristics : u16
}

#[derive(Debug, Copy, Clone)]
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

impl DataDirectory {
    fn from(index : u32) -> DataDirectory {
        match index {
            0 => DataDirectory::ExportTable,
            1 => DataDirectory::ImportTable,
            2 => DataDirectory::ResourceTable,
            3 => DataDirectory::ExceptionTable,
            4 => DataDirectory::CertificateTable,
            5 => DataDirectory::BaseRelocationTable,
            6 => DataDirectory::Debug,
            7 => DataDirectory::Architecture,
            8 => DataDirectory::GlobalPtr,
            9 => DataDirectory::TLSTable,
            10 => DataDirectory::LoadConfigTable,
            11 => DataDirectory::BoundImport,
            12 => DataDirectory::IAT,
            13 => DataDirectory::DelayImportDescriptor,
            14 => DataDirectory::CLRRuntimeHeader,
            15 => DataDirectory::Reserved,
            _ => DataDirectory::Unknown
        }
    }
}

pub struct ImageDataDirectory {
    pub name : u32,
    pub virtual_address: u32,
    pub size : u32
}

pub struct Hex<T>(pub T, pub usize);

impl <T : LowerHex> Display for Hex<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.1 {
            1 => f.write_fmt(format_args!("{:#03x}", self.0)),
            2 => f.write_fmt(format_args!("{:#04x}", self.0)),
            3 => f.write_fmt(format_args!("{:#05x}", self.0)),
            4 => f.write_fmt(format_args!("{:#06x}", self.0)),
            5 => f.write_fmt(format_args!("{:#07x}", self.0)),
            6 => f.write_fmt(format_args!("{:#08x}", self.0)),
            7 => f.write_fmt(format_args!("{:#09x}", self.0)),
            8 => f.write_fmt(format_args!("{:#010x}", self.0)),
            _ => f.write_fmt(format_args!("{:#0x}", self.0))
        }
    }
}

impl <T : LowerHex> Debug for Hex<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl Debug for ImageDataDirectory {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ImageDataDirectory")
            .field("name", &DataDirectory::from(self.name))
            .field("virtual_address", &Hex(self.virtual_address, 4))
            .field("size", &self.size)
            .finish()
    }
}


#[derive(Debug)]
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
    pub base_of_data : Option<u32>,

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

pub struct SectionHeader {
    pub name : u64,
    pub virtual_size : u32,
    pub virtual_address : u32,
    pub size_of_raw_data : u32,
    pub pointer_to_raw_data : u32,
    pub pointer_to_relocations : u32,
    pub pointer_to_line_numbers : u32,
    pub number_of_relocations : u16,
    pub number_of_line_numbers : u16,
    pub characteristics : u32
}

impl Debug for SectionHeader {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name_bytes = self.name.to_le_bytes();
        let name = std::str::from_utf8(&name_bytes).unwrap();
        f.debug_struct("SectionHeader")
            .field("name", &name)
            .field("virtual_size", &self.virtual_size)
            .field("virtual_address", &Hex(&self.virtual_address, 4))
            .field("size_of_raw_data", &self.size_of_raw_data)
            .field("pointer_to_raw_data", &Hex(self.pointer_to_raw_data, 4))
            .field("pointer_to_relocations", &Hex(self.pointer_to_relocations, 4))
            .field("pointer_to_line_numbers", &self.pointer_to_line_numbers)
            .field("number_of_relocations", &self.number_of_relocations)
            .field("number_of_line_numbers", &self.number_of_line_numbers)
            .field("characteristics", &self.characteristics)
            .finish()
    }
}

pub enum ImportLookupEntry {
    Ordinal(u16), NameTableRva(u32, String)
}

impl Debug for ImportLookupEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportLookupEntry::Ordinal(o) => f.write_fmt(format_args!("Ordinal({})", o)),
            ImportLookupEntry::NameTableRva(rva, name) => f.debug_struct("NameTableRva")
                .field("rva", &Hex(rva, 4)).field("name", name).finish()
        }
    }
}

pub struct ImportDirectoryEntry {
    pub import_lookup_table_rva : u32,
    pub time_date_stamp : u32,
    pub forwarder_chain : u32,
    pub name_rva : u32,
    pub import_address_table_rva : u32,
    pub import_lookup_table : Vec<ImportLookupEntry>,
    pub name : String
}

impl Debug for ImportDirectoryEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ImportDirectoryEntry")
            .field("name", &self.name)
            .field("import_lookup_table_rva", &Hex(self.import_lookup_table_rva, 4))
            .field("time_date_stamp", &self.time_date_stamp)
            .field("forwarder_chain", &self.forwarder_chain)
            .field("name_rva", &Hex(self.name_rva, 4))
            .field("import_address_table_rva", &Hex(self.import_address_table_rva, 4))
            .field("import_lookup_table", &self.import_lookup_table)
            .finish()
    }
}



#[derive(Debug)]
pub struct ImportSection {
    pub import_directory_table : Vec<ImportDirectoryEntry>
}

#[derive(Debug)]
pub enum BaseRelocType {
    ImageRelBasedAbsolute = 0,
    ImageRelBasedHigh = 1,
    ImageRelBasedLow = 2,
    ImageRelBasedHighlow = 3,
    ImageRelBasedHighadj = 4,
    //IMAGE_REL_BASED_MIPS_JMPADDR = 5,
    ImageRelBasedArmMov32 = 5,
    //IMAGE_REL_BASED_RISCV_HIGH20 = 5,
    //IMAGE_REL_BASED_RISCV_HIGH20 = 5,
    ImageRelBasedThumbMov32 = 7,
    //IMAGE_REL_BASED_RISCV_LOW12I = 7,
    ImageRelBasedRiscvLow12s = 8,
    //IMAGE_REL_BASED_LOONGARCH32_MARK_LA = 8,
    //IMAGE_REL_BASED_LOONGARCH64_MARK_LA = 8,
    ImageRelBasedMipsJmpaddr16 = 9,
    ImageRelBasedDir64 = 10,
    Unknown
}

impl BaseRelocType {
    pub fn from(val : u16) -> BaseRelocType {
        match (val & 0xF000) >> 12 {
            0 => BaseRelocType::ImageRelBasedAbsolute,
            1 => BaseRelocType::ImageRelBasedHigh,
            2 => BaseRelocType::ImageRelBasedLow,
            3 => BaseRelocType::ImageRelBasedHighlow,
            4 => BaseRelocType::ImageRelBasedHighadj,
            5 => BaseRelocType::ImageRelBasedArmMov32,
            7 => BaseRelocType::ImageRelBasedThumbMov32,
            8 => BaseRelocType::ImageRelBasedRiscvLow12s,
            9 => BaseRelocType::ImageRelBasedMipsJmpaddr16,
            10 => BaseRelocType::ImageRelBasedDir64,
            _ => BaseRelocType::Unknown
        }
    }
}
#[derive(Debug)]
pub struct BaseRelocationEntry {
    pub reloc_type : BaseRelocType,
    pub offset : u16 // Note: only lower 12 bits are used
}


pub struct BaseRelocationBlock {
    pub page_rva : u32,
    pub block_size : u32,
    pub entries : Vec<BaseRelocationEntry>
}

impl Debug for BaseRelocationBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BaseRelocationBlock")
            .field("page_rva", &Hex(self.page_rva, 4))
            .field("block_size", &self.block_size)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone)]
pub enum Identifier {
    Name(String),
    Id(u32)
}

pub struct Resource {
   pub identifiers : Vec<Identifier>,
   pub data_rva : u32,
   pub code_page : u32,
   pub data : Vec<u8>
}

impl Debug for Resource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Resource")
            .field("identifiers", &self.identifiers)
            .field("data_rva", &Hex(self.data_rva, 4))
            .field("code_page", &self.code_page)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub struct ResourceDirectoryTable {
    pub characteristics : u32,
    pub time_date_stamp : u32,
    pub major_version : u16,
    pub minor_version : u16,
    pub number_of_name_entries : u16,
    pub number_of_id_entries : u16
}

#[derive(Debug)]
pub struct ResourceSection {
    pub resource_tables : HashMap<u32, ResourceDirectoryTable>,
    pub resources : Vec<Resource>
}

#[derive(Debug)]
pub struct PortableExecutable {
    pub dos_stub : DosStub,
    pub coff_file_header : CoffFileHeader,
    pub optional_header : Option<OptionalHeader>,
    pub section_table : Vec<SectionHeader>,
    pub import_section : Option<ImportSection>,
    pub reloc_section : Option<Vec<BaseRelocationBlock>>,
    pub resource_section : Option<ResourceSection>
}


/*
impl CoffFileHeader {
    fn x64_64bit(sections : u16, optional_header_size : u16) -> CoffFileHeader {
        use std::time::{SystemTime, UNIX_EPOCH};
        CoffFileHeader {
            machine : 0x8664,
            sections,
            time_stamp : SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs() as u32,
            symbol_table_ptr : 0,
            number_of_symbols : 0,
            optional_header_size,
            characteristics : 0x2 | 0x20 | 0x200
        }
    }
}*/






/*
impl Into<Vec<u8>> for &DosStub {
    fn into(self) ->  Vec<u8> {
         vec![
            0x4d, 0x5a, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00,
            0x04, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00,
            0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
            self.offset as u8,
            (self.offset >> 8) as u8,
            (self.offset >> 16) as u8,
            (self.offset >> 24) as u8,
            0x0e, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD,
            0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68,
            0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72,
            0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F,
            0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6E,
            0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20,
            0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A,
            0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        ]
    }
}

impl Into<Vec<u8>> for DosStub {
    fn into(self) -> Vec<u8> {
        (&self).into()
    }
}


impl Into<Vec<u8>> for CoffFileHeader {
    fn into(self) -> Vec<u8> {
        vec![
            self.machine as u8, (self.machine >> 8) as u8,
            self.number_of_sections as u8, (self.number_of_sections >> 8) as u8,
            self.time_date_stamp as u8, (self.time_date_stamp >> 8) as u8, (self.time_date_stamp >> 16) as u8, (self.time_date_stamp >> 24) as u8,
            self.pointer_to_symbol_table as u8, (self.pointer_to_symbol_table >> 8) as u8, (self.pointer_to_symbol_table >> 16) as u8, (self.pointer_to_symbol_table >> 24) as u8,
            self.number_of_symbols as u8, (self.number_of_symbols >> 8) as u8, (self.number_of_symbols >> 16) as u8, (self.number_of_symbols >> 24) as u8,
            self.size_of_optional_header as u8, (self.size_of_optional_header >> 8) as u8,
            self.characteristics as u8, (self.characteristics >> 8) as u8
        ]
    }
}

impl Into<Vec<u8>> for PortableExecutable {
    fn into(self) -> Vec<u8> {
        let dos_stub : Vec<u8> = self.dos_stub.into();
        let coff_header : Vec<u8> = self.coff_file_header.into();
        return dos_stub.into_iter()
            .chain(coff_header.into_iter())
            .collect();
    }
}*/