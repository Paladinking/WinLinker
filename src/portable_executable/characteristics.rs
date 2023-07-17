use crate::portable_executable::types::{CoffFileHeader, SectionHeader};
use std::ops::BitAnd;



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
    ImageScnMem16bit = 0x00020000,
    ImageScnMemLocked = 0x00040000,
    ImageScnMemPreload = 0x00080000,
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

impl Into<u16> for CoffHeaderCharacteristic {
    fn into(self) -> u16 {
        self as u16
    }
}

impl Into<u32> for SectionHeaderCharacteristic {
    fn into(self) -> u32 {
        self as u32
    }
}



impl SectionHeader {
    pub const CHARACTERISTICS : [SectionHeaderCharacteristic; 34] = [
        SectionHeaderCharacteristic::ImageScnTypeNoPad,
        SectionHeaderCharacteristic::ImageScnCntCode,
        SectionHeaderCharacteristic::ImageScnCntInitializedData,
        SectionHeaderCharacteristic::ImageScnCntUninitializedData,
        SectionHeaderCharacteristic::ImageScnLnkOther,
        SectionHeaderCharacteristic::ImageScnLnkInfo,
        SectionHeaderCharacteristic::ImageScnLnkRemove,
        SectionHeaderCharacteristic::ImageScnLnkComdat,
        SectionHeaderCharacteristic::ImageScnGprel,
        SectionHeaderCharacteristic::ImageScnMem16bit,
        SectionHeaderCharacteristic::ImageScnMemLocked,
        SectionHeaderCharacteristic::ImageScnMemPreload,
        SectionHeaderCharacteristic::ImageScnAlign1bytes,
        SectionHeaderCharacteristic::ImageScnAlign2bytes,
        SectionHeaderCharacteristic::ImageScnAlign4bytes,
        SectionHeaderCharacteristic::ImageScnAlign8bytes,
        SectionHeaderCharacteristic::ImageScnAlign16bytes,
        SectionHeaderCharacteristic::ImageScnAlign32bytes,
        SectionHeaderCharacteristic::ImageScnAlign64bytes,
        SectionHeaderCharacteristic::ImageScnAlign128bytes,
        SectionHeaderCharacteristic::ImageScnAlign256bytes,
        SectionHeaderCharacteristic::ImageScnAlign512bytes,
        SectionHeaderCharacteristic::ImageScnAlign1024bytes,
        SectionHeaderCharacteristic::ImageScnAlign2048bytes,
        SectionHeaderCharacteristic::ImageScnAlign4096bytes,
        SectionHeaderCharacteristic::ImageScnAlign8192bytes,
        SectionHeaderCharacteristic::ImageScnLnkNrelocOvfl,
        SectionHeaderCharacteristic::ImageScnMemDiscardable,
        SectionHeaderCharacteristic::ImageScnMemNotCached,
        SectionHeaderCharacteristic::ImageScnMemNotPaged,
        SectionHeaderCharacteristic::ImageScnMemShared,
        SectionHeaderCharacteristic::ImageScnMemExecute,
        SectionHeaderCharacteristic::ImageScnMemRead,
        SectionHeaderCharacteristic::ImageScnMemWrite,
    ];
}

pub fn get_all<N: BitAnd<Output=N> + From<u8> + Eq + Copy, T: Into<N> + Copy, const L: usize>(flags: &[T; L], val: N) -> Vec<&T> {
    return flags.iter().filter(|&flag| ((*flag).into() & val) != 0_u8.into()).collect();
}


