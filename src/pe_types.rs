use std::fmt::{Debug, Formatter, LowerHex};
use std::mem::size_of;
macro_rules! read_u16{
    ($iter : expr) => {
        (*$iter.next()?) as u16 | ((*$iter.next()? as u16) << 8)
    };
}

macro_rules! read_u32{
    ($iter : expr) => {
        (*$iter.next()?) as u32 |
        ((*$iter.next()? as u32) << 8) |
        ((*$iter.next()? as u32) << 16) |
        ((*$iter.next()? as u32) << 24)
    };
}

macro_rules! read_u64{
    ($iter : expr) => {
        (*$iter.next()?) as u64 |
        ((*$iter.next()? as u64) << 8) |
        ((*$iter.next()? as u64) << 16) |
        ((*$iter.next()? as u64) << 24) |
        ((*$iter.next()? as u64) << 32) |
        ((*$iter.next()? as u64) << 40) |
        ((*$iter.next()? as u64) << 48) |
        ((*$iter.next()? as u64) << 56)
    };
}


pub trait FromBytes where Self : Sized {
    fn from_bytes<'a, T : Iterator<Item=&'a u8>> (iter : &mut T) -> Option<Self>;
}

#[derive(Debug)]
pub struct DosStub {
    pub offset : u32
}


#[derive(Debug)]
pub struct CoffFileHeader {
    machine : u16,
    number_of_sections : u16,
    time_date_stamp : u32,
    pointer_to_symbol_table : u32,
    number_of_symbols : u32,
    size_of_optional_header : u16,
    characteristics : u16
}

#[derive(Debug)]
enum DataDirectory {
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

struct Hex<T>(T, usize);

impl <T : LowerHex> Debug for Hex<T> {
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
            .field("virtual_address", &self.virtual_address)
            .field("size_of_raw_data", &self.size_of_raw_data)
            .field("pointer_to_raw_data", &self.pointer_to_raw_data)
            .field("pointer_to_relocations", &self.pointer_to_relocations)
            .field("pointer_to_line_numbers", &self.pointer_to_line_numbers)
            .field("number_of_relocations", &self.number_of_relocations)
            .field("number_of_line_numbers", &self.number_of_line_numbers)
            .field("characteristics", &self.characteristics)
            .finish()
    }
}

#[derive(Debug)]
pub struct PortableExecutable {
    pub dos_stub : DosStub,
    pub coff_file_header : CoffFileHeader,
    pub optional_header : Option<OptionalHeader>,
    pub section_table : Vec<SectionHeader>
}

impl FromBytes for SectionHeader {
    fn from_bytes<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Option<Self> {
         Some(SectionHeader {
            name : read_u64!(iter),
            virtual_size : read_u32!(iter),
            virtual_address : read_u32!(iter),
            size_of_raw_data : read_u32!(iter),
            pointer_to_raw_data : read_u32!(iter),
            pointer_to_relocations : read_u32!(iter),
            pointer_to_line_numbers : read_u32!(iter),
            number_of_relocations : read_u16!(iter),
            number_of_line_numbers : read_u16!(iter),
            characteristics : read_u32!(iter)
        })
    }
}

impl FromBytes for OptionalHeader {
    fn from_bytes<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Option<Self> {
        let magic = read_u16!(iter);
        if magic != 0x10b && magic != 0x20b {
            return None;
        }
        let mut optional_header = OptionalHeader {
            magic,
            major_linker_version : *iter.next()?,
            minor_linker_version : *iter.next()?,
            size_of_code : read_u32!(iter),
            size_of_initialized_data : read_u32!(iter),
            size_of_uninitialized_data : read_u32!(iter),
            address_of_entry_point : read_u32!(iter),
            base_of_code : read_u32!(iter),
            base_of_data : if magic == 0x10b {Some(read_u32!(iter))} else {None},

            image_base: if magic == 0x10b {read_u32!(iter) as u64} else {read_u64!(iter)},
            section_alignment : read_u32!(iter),
            file_alignment : read_u32!(iter),
            major_os_version : read_u16!(iter),
            minor_os_version : read_u16!(iter),
            major_image_version : read_u16!(iter),
            minor_image_version : read_u16!(iter),
            major_subsystem_version : read_u16!(iter),
            minor_subsystem_version : read_u16!(iter),
            win_32_version_value : read_u32!(iter),
            size_of_image : read_u32!(iter),
            size_of_headers : read_u32!(iter),
            check_sum : read_u32!(iter),
            subsystem : read_u16!(iter),
            dll_characteristics : read_u16!(iter),
            size_of_stack_reserve : if magic == 0x10b {read_u32!(iter) as u64} else {read_u64!(iter)},
            size_of_stack_commit : if magic == 0x10b {read_u32!(iter) as u64} else {read_u64!(iter)},
            size_of_heap_reserve : if magic == 0x10b {read_u32!(iter) as u64} else {read_u64!(iter)},
            size_of_heap_commit : if magic == 0x10b {read_u32!(iter) as u64} else {read_u64!(iter)},
            loader_flags : read_u32!(iter),
            number_of_rva_and_sizes : read_u32!(iter),

            data_directories : Vec::new()
        };
        for i in 0..optional_header.number_of_rva_and_sizes {
            optional_header.data_directories.push(ImageDataDirectory {
                name : i,
                virtual_address : read_u32!(iter),
                size : read_u32!(iter)
            });
        }
        Some(optional_header)
    }
}

impl FromBytes for DosStub {
    fn from_bytes<'a, T : Iterator<Item=&'a u8>> (iter : &mut T) -> Option<DosStub> {
        let mut iter = iter.skip(0x3c);
        let offset = read_u32!(iter);
        Some(DosStub {
            offset
        })
    }
}


impl FromBytes for CoffFileHeader {
    fn from_bytes<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Option<Self> {
        Some(CoffFileHeader {
            machine : read_u16!(iter),
            number_of_sections : read_u16!(iter),
            time_date_stamp : read_u32!(iter),
            pointer_to_symbol_table : read_u32!(iter),
            number_of_symbols : read_u32!(iter),
            size_of_optional_header : read_u16!(iter),
            characteristics : read_u16!(iter)
        })
    }
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



impl FromBytes for PortableExecutable {
    fn from_bytes<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Option<Self> {
        let dos_stub = DosStub::from_bytes(&mut iter.by_ref().take(64))?;
        iter.by_ref().take(dos_stub.offset as usize - 64).for_each(drop);
        if *iter.next()? != 0x50 || *iter.next()? != 0x45 || *iter.next()? != 0x00 || *iter.next()? != 0x00 {
            return None;
        }
        let coff_file_header = CoffFileHeader::from_bytes(iter)?;
        let optional_header = if coff_file_header.size_of_optional_header == 0 {None} else {
            Some(OptionalHeader::from_bytes(iter)?)
        };
        let mut section_table = Vec::with_capacity(coff_file_header.number_of_sections as usize);
        for _ in 0..coff_file_header.number_of_sections {
            section_table.push(SectionHeader::from_bytes(&mut iter.by_ref().take(40))?);
        }
        Some(PortableExecutable {
            dos_stub, coff_file_header, optional_header, section_table
        })
    }
}



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
}