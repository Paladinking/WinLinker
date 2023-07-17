use crate::portable_executable::symbol_table::StringTable;

#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
pub enum SectionHeaderCharacteristic {
    ImageScnTypeNoPad = 0x00000008,
    ImageScnCntCode = 0x00000020,
    ImageScnCntInitializedData = 0x00000040,
    ImageScnCntUninitializedData = 0x00000080,
    ImageScnLnkOther = 0x00000100,
    ImageScnLnkInfo = 0x00000200,
    ImageScnLnkRemove = 0x00000800,
    ImageScnLnkComdat = 0x00001000,
    ImageScnGprel = 0x00008000,
    //ImageScnMemPurgeable = 0x00020000,
    //ImageScnMem16bit = 0x00020000,
    //ImageScnMemLocked = 0x00040000,
    //ImageScnMemPreload = 0x00080000,
    ImageScnAlign1bytes = 0x00100000,
    ImageScnAlign2bytes = 0x00200000,
    ImageScnAlign4bytes = 0x00300000,
    ImageScnAlign8bytes = 0x00400000,
    ImageScnAlign16bytes = 0x00500000,
    ImageScnAlign32bytes = 0x00600000,
    ImageScnAlign64bytes = 0x00700000,
    ImageScnAlign128bytes = 0x00800000,
    ImageScnAlign256bytes = 0x00900000,
    ImageScnAlign512bytes = 0x00A00000,
    ImageScnAlign1024bytes = 0x00B00000,
    ImageScnAlign2048bytes = 0x00C00000,
    ImageScnAlign4096bytes = 0x00D00000,
    ImageScnAlign8192bytes = 0x00E00000,
    ImageScnLnkNrelocOvfl = 0x01000000,
    ImageScnMemDiscardable = 0x02000000,
    ImageScnMemNotCached = 0x04000000,
    ImageScnMemNotPaged =0x08000000,
    ImageScnMemShared = 0x10000000,
    ImageScnMemExecute = 0x20000000,
    ImageScnMemRead = 0x40000000,
    ImageScnMemWrite = 0x80000000,
}

pub struct SectionHeader {
    pub name : String,
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

impl SectionHeader {
    pub const SIZE : usize = 40;

    pub fn new(name : String, raw_size : u32, characteristics : u32) -> SectionHeader {
        SectionHeader {
            name, virtual_size : 0, virtual_address : 0, size_of_raw_data : raw_size,
            pointer_to_raw_data : 0, pointer_to_relocations : 0, pointer_to_line_numbers : 0,
            number_of_relocations : 0, number_of_line_numbers : 0, characteristics
        }
    }

    pub fn write(&self, data : &mut Vec<u8>, string_table : &mut StringTable) {
        let bytes = self.name.clone().into_bytes();
        if bytes.len() > 8 {
            let index = string_table.add_string(bytes);
            let index_ascii = index.to_string().into_bytes();
            debug_assert!(index_ascii.is_ascii() && index_ascii.len() < 8);
            data.push(b'/');
            data.resize(data.len() + 7 - index_ascii.len(), 0);
            data.extend_from_slice(&index_ascii);
        } else {
            data.extend_from_slice(&bytes);
            data.resize(data.len() + 8 - bytes.len(), 0);
        }
        data.extend_from_slice(&self.virtual_size.to_le_bytes());
        data.extend_from_slice(&self.virtual_address.to_le_bytes());
        data.extend_from_slice(&self.size_of_raw_data.to_le_bytes());
        data.extend_from_slice(&self.pointer_to_raw_data.to_le_bytes());
        data.extend_from_slice(&self.pointer_to_relocations.to_le_bytes());
        data.extend_from_slice(&self.pointer_to_line_numbers.to_le_bytes());
        data.extend_from_slice(&self.number_of_relocations.to_le_bytes());
        data.extend_from_slice(&self.number_of_line_numbers.to_le_bytes());
        data.extend_from_slice(&self.characteristics.to_le_bytes());
    }
}

pub trait Section {
    fn get_object_header(&self) -> SectionHeader;

    fn write(&self, data: &mut Vec<u8>);
}

pub struct TextSection {
    pub data : Vec<u8>,
    pub header : usize
}

impl TextSection {
    pub fn new(data : Vec<u8>) -> TextSection {
        TextSection {
            data, header : 0
        }
    }
}

impl Section for TextSection {
    fn get_object_header(&self) -> SectionHeader {
        let mut raw_size = self.data.len().try_into().expect("Too big text section");
        SectionHeader::new(String::from(".text"), raw_size,
            SectionHeaderCharacteristic::ImageScnCntCode as u32 |
                        SectionHeaderCharacteristic::ImageScnAlign16bytes as u32 |
                        SectionHeaderCharacteristic::ImageScnMemExecute as u32 |
                        SectionHeaderCharacteristic::ImageScnMemRead as u32)
    }

    fn write(&self, data: &mut Vec<u8>) {
        data.extend_from_slice(&self.data);
    }
}