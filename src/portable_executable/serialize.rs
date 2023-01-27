use crate::portable_executable::types::*;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Debug};

macro_rules! read_u8 {
    ($iter : expr) => {
        $iter.next().copied().ok_or(ParseError::OutOfDataError)
    };
}

macro_rules! read_u16 {
    ($iter : expr) => {
        match ($iter.next(), $iter.next()) {
            (Some(&b1), Some(&b2)) => Ok(b1 as u16 | ((b2 as u16) << 8)),
            _ => Err(ParseError::OutOfDataError)
        }
    };
}

macro_rules! read_u32 {
    ($iter : expr) => {
        match ($iter.next(), $iter.next(), $iter.next(), $iter.next()) {
            (Some(&b1), Some(&b2), Some(&b3), Some(&b4)) =>
                Ok(b1 as u32 | ((b2 as u32) << 8) | ((b3 as u32) << 16)  | ((b4 as u32) << 24)),
            _ => Err(ParseError::OutOfDataError)
        }
    };
}

macro_rules! read_u64 {
    ($iter : expr) => {
        match ($iter.next(), $iter.next(), $iter.next(), $iter.next(), $iter.next(), $iter.next(), $iter.next(), $iter.next()) {
            (Some(&b1), Some(&b2), Some(&b3), Some(&b4), Some(&b5), Some(&b6), Some(&b7), Some(&b8)) =>
                Ok(
                    b1 as u64 | ((b2 as u64) << 8) | ((b3 as u64) << 16)  | ((b4 as u64) << 24) |
                    ((b5 as u64) << 32) | ((b6 as u64) << 40) | ((b7 as u64) << 48) | ((b8 as u64) << 56)
                ),
            _ => Err(ParseError::OutOfDataError)
        }
    };
}

macro_rules! bytes_u16 {
    ($bytes : expr, $addr : expr) => {
        match $bytes.get(($addr)..($addr + 2)) {
            Some(fixed) =>  Ok(u16::from_le_bytes(fixed.try_into().unwrap())),
            None => Err(ParseError::OutOfDataError)
        }
    };
}

macro_rules! bytes_u32 {
    ($bytes : expr, $addr : expr) => {
        match $bytes.get(($addr)..($addr + 4)) {
            Some(fixed) =>  Ok(u32::from_le_bytes(fixed.try_into().unwrap())),
            None => Err(ParseError::OutOfDataError)
        }
    };
}

macro_rules! bytes_u64 {
    ($bytes : expr, $addr : expr) => {
        match $bytes.get(($addr)..($addr + 8)) {
            Some(fixed) =>  Ok(u64::from_le_bytes(fixed.try_into().unwrap())),
            None => Err(ParseError::OutOfDataError)
        }
    };
}

#[derive(Debug)]
pub enum ParseError {
    OutOfDataError,
    BadOptionalHeader,
    InvalidRVA,
    InvalidNameTableEntry,
    BadUtf16String,
    InvalidHeader
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

pub trait FromBytes where Self : Sized {
    fn from_bytes_iter<'a, T : Iterator<Item=&'a u8>> (iter : &mut T) -> Result<Self, ParseError>;

    fn from_bytes(bytes : &[u8]) -> Result<Self, ParseError>;
}

impl FromBytes for SectionHeader {
    fn from_bytes_iter<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Result<SectionHeader, ParseError> {
        Ok(SectionHeader {
            name : read_u64!(iter)?,
            virtual_size : read_u32!(iter)?,
            virtual_address : read_u32!(iter)?,
            size_of_raw_data : read_u32!(iter)?,
            pointer_to_raw_data : read_u32!(iter)?,
            pointer_to_relocations : read_u32!(iter)?,
            pointer_to_line_numbers : read_u32!(iter)?,
            number_of_relocations : read_u16!(iter)?,
            number_of_line_numbers : read_u16!(iter)?,
            characteristics : read_u32!(iter)?
        })
    }

    fn from_bytes(bytes: &[u8]) -> Result<SectionHeader, ParseError> {
        Ok(SectionHeader {
            name: bytes_u64!(bytes, 0)?,
            virtual_size: bytes_u32!(bytes, 8)?,
            virtual_address: bytes_u32!(bytes, 12)?,
            size_of_raw_data: bytes_u32!(bytes, 16)?,
            pointer_to_raw_data: bytes_u32!(bytes, 20)?,
            pointer_to_relocations: bytes_u32!(bytes, 24)?,
            pointer_to_line_numbers: bytes_u32!(bytes, 28)?,
            number_of_relocations: bytes_u16!(bytes, 32)?,
            number_of_line_numbers: bytes_u16!(bytes, 34)?,
            characteristics: bytes_u32!(bytes, 36)?
        })
    }
}

impl FromBytes for OptionalHeader {
    fn from_bytes_iter<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) ->  Result<OptionalHeader, ParseError> {
        let magic = read_u16!(iter)?;
        if magic != 0x10b && magic != 0x20b {
            return Err(ParseError::BadOptionalHeader);
        }
        let mut optional_header = OptionalHeader {
            magic,
            major_linker_version : read_u8!(iter)?,
            minor_linker_version : read_u8!(iter)?,
            size_of_code : read_u32!(iter)?,
            size_of_initialized_data : read_u32!(iter)?,
            size_of_uninitialized_data : read_u32!(iter)?,
            address_of_entry_point : read_u32!(iter)?,
            base_of_code : read_u32!(iter)?,
            base_of_data : if magic == 0x10b {Some(read_u32!(iter)?)} else {None},

            image_base: if magic == 0x10b {read_u32!(iter)? as u64} else {read_u64!(iter)?},
            section_alignment : read_u32!(iter)?,
            file_alignment : read_u32!(iter)?,
            major_os_version : read_u16!(iter)?,
            minor_os_version : read_u16!(iter)?,
            major_image_version : read_u16!(iter)?,
            minor_image_version : read_u16!(iter)?,
            major_subsystem_version : read_u16!(iter)?,
            minor_subsystem_version : read_u16!(iter)?,
            win_32_version_value : read_u32!(iter)?,
            size_of_image : read_u32!(iter)?,
            size_of_headers : read_u32!(iter)?,
            check_sum : read_u32!(iter)?,
            subsystem : read_u16!(iter)?,
            dll_characteristics : read_u16!(iter)?,
            size_of_stack_reserve : if magic == 0x10b {read_u32!(iter)? as u64} else {read_u64!(iter)?},
            size_of_stack_commit : if magic == 0x10b {read_u32!(iter)? as u64} else {read_u64!(iter)?},
            size_of_heap_reserve : if magic == 0x10b {read_u32!(iter)? as u64} else {read_u64!(iter)?},
            size_of_heap_commit : if magic == 0x10b {read_u32!(iter)? as u64} else {read_u64!(iter)?},
            loader_flags : read_u32!(iter)?,
            number_of_rva_and_sizes : read_u32!(iter)?,

            data_directories : Vec::new()
        };
        for i in 0..optional_header.number_of_rva_and_sizes {
            optional_header.data_directories.push(ImageDataDirectory {
                name : i,
                virtual_address : read_u32!(iter)?,
                size : read_u32!(iter)?
            });
        }
        Ok(optional_header)
    }

    fn from_bytes(bytes: &[u8]) -> Result<OptionalHeader, ParseError> {
        OptionalHeader::from_bytes_iter(&mut bytes.iter())
    }
}

impl FromBytes for DosStub {
    fn from_bytes_iter<'a, T : Iterator<Item=&'a u8>> (iter : &mut T) -> Result<DosStub, ParseError> {
        let mut it = iter.skip(0x3c);
        let offset = read_u32!(it)?;
        Ok(DosStub {
            offset
        })
    }

    fn from_bytes(bytes: &[u8]) -> Result<Self, ParseError> {
        DosStub::from_bytes_iter(&mut bytes.iter())
    }
}


impl FromBytes for CoffFileHeader {
    fn from_bytes_iter<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Result<Self, ParseError> {
        Ok(CoffFileHeader {
            machine : read_u16!(iter)?,
            number_of_sections : read_u16!(iter)?,
            time_date_stamp : read_u32!(iter)?,
            pointer_to_symbol_table : read_u32!(iter)?,
            number_of_symbols : read_u32!(iter)?,
            size_of_optional_header : read_u16!(iter)?,
            characteristics : read_u16!(iter)?
        })
    }

    fn from_bytes(bytes: &[u8]) -> Result<Self, ParseError> {
        CoffFileHeader::from_bytes_iter(&mut bytes.iter())
    }
}

fn to_raw_address(sections : &Vec<SectionHeader>, virtual_address : u32) -> Result<usize, ParseError> {
    for sec in sections {
        if sec.virtual_address <= virtual_address && sec.virtual_address + sec.virtual_size > virtual_address {
            return Ok((virtual_address - sec.virtual_address + sec.pointer_to_raw_data) as usize);
        }
    }
    Err(ParseError::InvalidRVA)
}

fn data_directory_address(id : DataDirectory, sections : &Vec<SectionHeader>, optional_header : &OptionalHeader) -> Result<Option<usize>, ParseError> {
    if optional_header.data_directories.len() <= id as usize {
        return Ok(None)
    } else if optional_header.data_directories[id as usize].virtual_address == 0 {
        return Ok(None)
    }
    let virtual_address = optional_header.data_directories[id as usize].virtual_address;
    Ok(Some(to_raw_address(sections, virtual_address)?))
}

fn name_table_entry(sections : &Vec<SectionHeader>, rva : u32, bytes : &[u8]) -> Result<String, ParseError> {
    let raw_addr = to_raw_address(&sections, rva)?;
    let nul_range_end = (&bytes[raw_addr..]).iter()
        .position(|&c| c == b'\0').ok_or(ParseError::InvalidNameTableEntry)? + raw_addr;
    let escaped = (&bytes[raw_addr..nul_range_end]).iter().flat_map(|&b | std::ascii::escape_default(b)).collect();
    String::from_utf8(escaped).map_err(|_| ParseError::InvalidNameTableEntry)
}

fn read_utf16_string(bytes : &[u8], index : usize, string_len : usize) -> Result<String, ParseError> {
    if index + string_len > bytes.len() {
        return Err(ParseError::BadUtf16String);
    }
    let iter = (index..(index + 2 * string_len)).step_by(2)
        .map(|i| bytes[i] as u16 | ((bytes[i + 1] as u16) << 8));
    Ok(std::char::decode_utf16(iter)
        .map_while(|r| r.ok()).collect()
    )
}

fn read_name_table(name_table_rva : u32, section_table : &Vec<SectionHeader>, image64 : bool, bytes : &[u8]) -> Result<Vec<ImportLookupEntry>, ParseError> {
    let raw_address = to_raw_address(section_table, name_table_rva)?;
    let mut i = 0;
    let (increment, mask) = if image64 {
        (8, 0x8000000000000000)
    } else {
        (4, 0x80000000)
    };
    let mut import_lookup_table = Vec::new();
    loop {
        let val = if image64 {
            bytes_u64!(bytes, raw_address + i)?
        } else {
            bytes_u32!(bytes, raw_address + i)? as u64
        };
        if val == 0 {
            break
        }
        if (val & mask) != 0 {
            import_lookup_table.push(ImportLookupEntry::Ordinal(val as u16));
        } else {
            let rva = (val & 0x7fffffff) as u32;
            let name = name_table_entry(section_table, rva + 2, bytes)?;
            import_lookup_table.push(ImportLookupEntry::NameTableRva(rva, name));
        }
        i += increment;
    }
    Ok(import_lookup_table)
}

fn read_delay_import_section(bytes : &[u8], optional_header : &OptionalHeader, section_table : &Vec<SectionHeader>) -> Result<Option<DelayImportSection>, ParseError> {
    let address = data_directory_address(DataDirectory::DelayImportDescriptor, &section_table, optional_header)?;
    if address.is_none() {
        return Ok(None)
    }
    let mut index = address.unwrap();
    let mut delay_load_directory_table = Vec::new();
    loop {
        let remaining = bytes.get(index..(index + 40)).ok_or(ParseError::OutOfDataError)?;
        let attributes = bytes_u32!(remaining, 0).unwrap();
        let name_rva = bytes_u32!(remaining, 4).unwrap();
        let module_handle = bytes_u32!(remaining, 8).unwrap();
        let import_address_table_rva = bytes_u32!(remaining, 12).unwrap();
        let import_name_table_rva = bytes_u32!(remaining, 16).unwrap();
        let bound_import_table_rva = bytes_u32!(remaining, 24).unwrap();
        let unload_import_table_rva = bytes_u32!(remaining, 32).unwrap();
        let time_stamp = bytes_u32!(remaining, 36).unwrap();
        if name_rva == 0 {
            break;
        }
        let name = name_table_entry(&section_table, name_rva, bytes)?;
        let import_lookup_table = read_name_table(
            import_name_table_rva,
            section_table,
            optional_header.magic == 0x20b,
            bytes
        )?;
        delay_load_directory_table.push(DelayLoadDirectoryTable {
            attributes,
            name_rva,
            module_handle,
            import_address_table_rva,
            import_name_table_rva,
            bound_import_table_rva,
            unload_import_table_rva,
            time_stamp,
            import_lookup_table,
            name
        });
        index += 32;
    }

    Ok(Some(DelayImportSection {
        delay_load_directory_table
    }))
}

fn read_import_section(bytes : &[u8], optional_header : &OptionalHeader, section_table : &Vec<SectionHeader>) -> Result<Option<ImportSection>, ParseError> {
    let address = data_directory_address(DataDirectory::ImportTable, &section_table, optional_header)?;
    if address.is_none() {
        return Ok(None);
    }
    let address = address.unwrap();
    let mut import_table = Vec::new();
    let iter = &mut bytes[address..].iter();
    loop {
        let import_lookup_table_rva =  read_u32!(iter)?;
        let time_date_stamp = read_u32!(iter)?;
        let forwarder_chain = read_u32!(iter)?;
        let name_rva = read_u32!(iter)?;
        let import_address_table_rva = read_u32!(iter)?;
        if import_lookup_table_rva == 0 {
            break;
        }
        let name = name_table_entry(&section_table, name_rva, bytes)?;
        let import_lookup_table = read_name_table(
            import_lookup_table_rva, section_table,
            optional_header.magic == 0x20b, bytes
        )?;
        import_table.push(ImportDirectoryEntry {
            import_lookup_table_rva,
            time_date_stamp,
            forwarder_chain,
            name_rva,
            import_address_table_rva,
            import_lookup_table,
            name
        });
    }
    Ok(Some(ImportSection {
        import_directory_tables: import_table
    }))
}

fn read_reloc_section(bytes : &[u8], optional_header : &OptionalHeader, section_table : &Vec<SectionHeader>) -> Result<Option<Vec<BaseRelocationBlock>>, ParseError> {
    let address = data_directory_address(DataDirectory::BaseRelocationTable, &section_table, optional_header)?;
    if address.is_none() {
        return Ok(None);
    }
    let address = address.unwrap();
    let mut base_reloc_blocks = Vec::new();
    let mut i = address;
    while i < (address + optional_header.data_directories[DataDirectory::BaseRelocationTable as usize].size as usize) {
        let remaining : &[u8] = &bytes[i..];
        let iter = &mut remaining.iter();
        let rva = read_u32!(iter)?;
        let size = read_u32!(iter)?;
        if size % 2 != 0 {
            return Err(ParseError::InvalidRVA);
        }
        if remaining.len() < size as usize {
            return Err(ParseError::OutOfDataError);
        }
        let entries = (8..size).step_by(2).map_while(|index| {
            let val = remaining[index as usize] as u16 + ((remaining[index as usize + 1] as u16) << 8);
            if val == 0 {
                None
            } else {
                Some(BaseRelocationEntry {
                    reloc_type: BaseRelocType::from(val),
                    offset: val & 0x0FFF
                })
            }
        }).collect();
        base_reloc_blocks.push(BaseRelocationBlock {
            page_rva : rva,
            block_size : size,
            entries
        });
        i += size as usize;
    }
    Ok(Some(base_reloc_blocks))
}

fn read_resource_table(bytes : &[u8], address : usize) -> Result<ResourceDirectoryTable, ParseError> {
    let iter = &mut (bytes[address..]).iter();
    Ok(ResourceDirectoryTable {
        characteristics: read_u32!(iter)?,
        time_date_stamp: read_u32!(iter)?,
        major_version: read_u16!(iter)?,
        minor_version: read_u16!(iter)?,
        number_of_name_entries: read_u16!(iter)?,
        number_of_id_entries: read_u16!(iter)?
    })
}

fn read_resource_section(bytes : &[u8], optional_header : &OptionalHeader, section_table : &Vec<SectionHeader>) -> Result<Option<ResourceSection>, ParseError> {
    let address = data_directory_address(DataDirectory::ResourceTable, &section_table, optional_header)?;
    if address.is_none() {
        return Ok(None);
    }
    let address = address.unwrap();
    let mut resource_tables : HashMap<u32, ResourceDirectoryTable> = HashMap::new();
    let first_table = read_resource_table(bytes, address)?;
    let mut to_visit : Vec<(u32, u16, Vec<Identifier>, bool)> = (0..first_table.number_of_name_entries)
        .map(|i| (0, i * 8, Vec::new(), true))
        .chain((0..first_table.number_of_id_entries)
            .map(|i| (0, (first_table.number_of_name_entries + i) * 8, Vec::new(), false))
        ).collect();
    resource_tables.insert(0, first_table);
    let mut resources = Vec::new();
    while !to_visit.is_empty() {
        let (table, i, mut entries, by_name) = to_visit.pop().unwrap();
        let index = address + table as usize + 16 + i as usize;
        let val = bytes_u32!(bytes, index)? as usize;
        if by_name {
            let string_addr = address + val & 0x7fffffff;
            let string_len = bytes_u16!(bytes, string_addr)? as usize;
            let entry = read_utf16_string(bytes, string_addr + 2, string_len)?;
            entries.push(Identifier::Name(entry));
        } else {
            entries.push(Identifier::Id(val as u32))
        }
        let next = bytes_u32!(bytes, index + 4)?;
        if (next & 0x80000000) != 0 {
            let next = next & 0x7fffffff;
            if !resource_tables.contains_key(&next) {
                let new_table = read_resource_table(bytes, address + next as usize)?;
                let iter = (0..new_table.number_of_name_entries)
                    .map(|i| (next, i * 8, entries.clone(), true))
                    .chain((0..new_table.number_of_id_entries)
                        .map(|i| (next, (new_table.number_of_name_entries + i) * 8, entries.clone(), false))
                    );
                for e in iter {
                    to_visit.push(e);
                }
                resource_tables.insert(next, new_table);
            }
        } else {
            let addr = address + next as usize;
            let data_rva = bytes_u32!(bytes, addr)?;
            let size = bytes_u32!(bytes, addr + 4)?;
            let code_page = bytes_u32!(bytes, addr + 8)?;
            let data_addr = to_raw_address(section_table, data_rva)?;
            let data = Vec::from(bytes.get(data_addr..(data_addr + size as usize)).ok_or(ParseError::OutOfDataError)?);
            let resource = Resource {
                identifiers: entries,
                data_rva,
                code_page,
                data
            };
            resources.push(resource);
        }
    }
    Ok(Some(ResourceSection { resource_tables, resources }))
}

impl FromBytes for PortableExecutable {
    fn from_bytes_iter<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Result<Self, ParseError> {
        PortableExecutable::from_bytes(&iter.cloned().collect::<Vec<u8>>())
    }

    fn from_bytes(bytes: &[u8]) -> Result<Self, ParseError> {
        let mut iter = bytes.iter();
        let dos_stub = DosStub::from_bytes_iter(&mut iter.by_ref().take(64))?;
        iter.by_ref().take(dos_stub.offset as usize - 64).for_each(drop);
        if iter.next() != Some(&0x50) || iter.next() != Some(&0x45) || iter.next() != Some(&0x00) || iter.next() != Some(&0x00) {
            return Err(ParseError::InvalidHeader);
        }
        let coff_file_header = CoffFileHeader::from_bytes_iter(&mut iter)?;
        let optional_header = if coff_file_header.size_of_optional_header == 0 {None} else {
            Some(OptionalHeader::from_bytes_iter(&mut iter)?)
        };
        let mut section_table = Vec::with_capacity(coff_file_header.number_of_sections as usize);
        for _ in 0..coff_file_header.number_of_sections {
            section_table.push(SectionHeader::from_bytes_iter(&mut iter.by_ref().take(40))?);
        }
        let (import_section, reloc_section, resource_section, delay_import_section) = if let Some(optional_header) = &optional_header {

            (
                read_import_section(bytes, &optional_header, &section_table)?,
                read_reloc_section(bytes, &optional_header, &section_table)?,
                read_resource_section(bytes, &optional_header, &section_table)?,
                read_delay_import_section(bytes, &optional_header, &section_table)?
            )
        } else {
            (None, None, None, None)
        };
        Ok(PortableExecutable {
            dos_stub, coff_file_header, optional_header, section_table, import_section, reloc_section, resource_section, delay_import_section
        })
    }
}