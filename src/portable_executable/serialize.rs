use crate::portable_executable::types::*;

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
    fn from_bytes_iter<'a, T : Iterator<Item=&'a u8>> (iter : &mut T) -> Option<Self>;

    fn from_bytes(bytes : &[u8]) -> Option<Self>;
}

impl FromBytes for SectionHeader {
    fn from_bytes_iter<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Option<Self> {
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

    fn from_bytes(bytes: &[u8]) -> Option<Self> {
        SectionHeader::from_bytes_iter(&mut bytes.iter())
    }
}

impl FromBytes for OptionalHeader {
    fn from_bytes_iter<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Option<Self> {
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

    fn from_bytes(bytes: &[u8]) -> Option<Self> {
        OptionalHeader::from_bytes_iter(&mut bytes.iter())
    }
}

impl FromBytes for DosStub {
    fn from_bytes_iter<'a, T : Iterator<Item=&'a u8>> (iter : &mut T) -> Option<DosStub> {
        let mut iter = iter.skip(0x3c);
        let offset = read_u32!(iter);
        Some(DosStub {
            offset
        })
    }

    fn from_bytes(bytes: &[u8]) -> Option<Self> {
        DosStub::from_bytes_iter(&mut bytes.iter())
    }
}


impl FromBytes for CoffFileHeader {
    fn from_bytes_iter<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Option<Self> {
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

    fn from_bytes(bytes: &[u8]) -> Option<Self> {
        CoffFileHeader::from_bytes_iter(&mut bytes.iter())
    }
}

fn to_raw_address(sections : &Vec<SectionHeader>, virtual_address : u32) -> Option<usize> {
    for sec in sections {
        if sec.virtual_address <= virtual_address && sec.virtual_address + sec.virtual_size > virtual_address {
            return Some((virtual_address - sec.virtual_address + sec.pointer_to_raw_data) as usize);
        }
    }
    None
}

fn data_directory_address(id : DataDirectory, sections : &Vec<SectionHeader>, optional_header : &OptionalHeader) -> Option<usize> {
    if optional_header.data_directories.len() <= id as usize {
        return None
    } else if optional_header.data_directories[id as usize].virtual_address == 0 {
        return None
    }
    let virtual_address = optional_header.data_directories[id as usize].virtual_address;
    to_raw_address(sections, virtual_address)
}

fn name_table_entry(sections : &Vec<SectionHeader>, rva : u32, bytes : &[u8]) -> Option<String> {
    let raw_addr = to_raw_address(&sections, rva)?;
    let nul_range_end = (&bytes[raw_addr..]).iter()
        .position(|&c| c == b'\0')? + raw_addr;
    let escaped = (&bytes[raw_addr..nul_range_end]).iter().flat_map(|&b | std::ascii::escape_default(b)).collect();
    String::from_utf8(escaped).ok()
}

fn read_import_section(bytes : &[u8], optional_header : &OptionalHeader, section_table : &Vec<SectionHeader>) -> Option<ImportSection> {
    let address = data_directory_address(DataDirectory::ImportTable, &section_table, optional_header)?;
    let mut import_table = Vec::new();
    let iter = &mut (bytes[address..]).iter();
    loop {
        let import_lookup_table_rva =  read_u32!(iter);
        let time_date_stamp = read_u32!(iter);
        let forwarder_chain = read_u32!(iter);
        let name_rva = read_u32!(iter);
        let import_address_table_rva = read_u32!(iter);
        if (import_lookup_table_rva | time_date_stamp | forwarder_chain | name_rva | import_address_table_rva) == 0 {
            break;
        }
        let name = name_table_entry(&section_table, name_rva, bytes)?;
        let mut ide = ImportDirectoryEntry {
            import_lookup_table_rva,
            time_date_stamp,
            forwarder_chain,
            name_rva,
            import_address_table_rva,
            import_lookup_table: Vec::new(),
            name
        };
        if ide.time_date_stamp == 0 &&
            ide.forwarder_chain == 0 &&
            ide.import_address_table_rva == 0 &&
            ide.import_lookup_table_rva == 0 &&
            ide.name_rva == 0
        {
            break;
        }
        let lookup_bytes : &[u8] = &bytes[(to_raw_address(&section_table, ide.import_lookup_table_rva)?)..];
        let mut i = 0;
        let (increment, mask) = if optional_header.magic == 0x20b {
            (8, 0x8000000000000000)
        } else {
            (4, 0x80000000)
        };
        loop {
            if i + increment > lookup_bytes.len() {
                return None; // File is invalid, lookup table should be null-terminated.
            }
            let mut it = lookup_bytes[i..].iter();
            let val = if optional_header.magic == 0x20b {
                read_u64!(it)
            } else {
                read_u32!(it) as u64
            };
            if val == 0 {
                break
            }

            if (val & mask) != 0 {
                ide.import_lookup_table.push(ImportLookupEntry::Ordinal(val as u16));
            } else {
                let rva = (val & 0x7fffffff) as u32;
                let name = name_table_entry(&section_table, rva + 2, bytes)?;
                ide.import_lookup_table.push(ImportLookupEntry::NameTableRva(rva, name));
            }
            i += increment;
        }
        import_table.push(ide);
    }
    Some(ImportSection {
        import_directory_table: import_table
    })
}

fn read_reloc_section(bytes : &[u8], optional_header : &OptionalHeader, section_table : &Vec<SectionHeader>) -> Option<Vec<BaseRelocationBlock>> {
    let address = data_directory_address(DataDirectory::BaseRelocationTable, &section_table, optional_header)?;
    let mut base_reloc_blocks = Vec::new();
    let mut i = address;
    while i < (address + optional_header.data_directories[DataDirectory::BaseRelocationTable as usize].size as usize) {
        let remaining : &[u8] = &bytes[i..];
        let iter = &mut remaining.iter();
        let rva = read_u32!(iter);
        let size = read_u32!(iter);
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
    Some(base_reloc_blocks) 
}

impl FromBytes for PortableExecutable {
    fn from_bytes_iter<'a, T: Iterator<Item=&'a u8>>(iter: &mut T) -> Option<Self> {
        PortableExecutable::from_bytes(&iter.cloned().collect::<Vec<u8>>())
    }

    fn from_bytes(bytes: &[u8]) -> Option<Self> {
        let mut iter = bytes.iter();
        let dos_stub = DosStub::from_bytes_iter(&mut iter.by_ref().take(64))?;
        iter.by_ref().take(dos_stub.offset as usize - 64).for_each(drop);
        if *iter.next()? != 0x50 || *iter.next()? != 0x45 || *iter.next()? != 0x00 || *iter.next()? != 0x00 {
            return None;
        }
        let coff_file_header = CoffFileHeader::from_bytes_iter(&mut iter)?;
        let optional_header = if coff_file_header.size_of_optional_header == 0 {None} else {
            Some(OptionalHeader::from_bytes_iter(&mut iter)?)
        };
        let mut section_table = Vec::with_capacity(coff_file_header.number_of_sections as usize);
        for _ in 0..coff_file_header.number_of_sections {
            section_table.push(SectionHeader::from_bytes_iter(&mut iter.by_ref().take(40))?);
        }
        let import_section = if let Some(optional_header) = &optional_header {
            read_import_section(bytes, &optional_header, &section_table)
        } else {
            None
        };
        let reloc_section = if let Some(optional_header) = &optional_header {
            read_reloc_section(bytes, &optional_header, &section_table)
        } else {
            None
        };

        Some(PortableExecutable {
            dos_stub, coff_file_header, optional_header, section_table, import_section, reloc_section
        })
    }
}