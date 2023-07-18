pub mod storage_class {
    pub const IMAGE_SYM_CLASS_NULL : u8 = 0;
    pub const IMAGE_SYM_CLASS_EXTERNAL : u8 = 2;
    pub const IMAGE_SYM_CLASS_STATIC : u8 = 3;
    pub const IMAGE_SYM_CLASS_FILE : u8 = 103;
}

pub struct StringTable {
    pub data : Vec<u8>
}

impl StringTable {
    pub fn new() -> StringTable {
        StringTable {
            data : Vec::new()
        }
    }

    pub fn add_string(&mut self, s : Vec<u8>) -> u32 {
        let index = self.data.len();
        self.data.reserve(s.len() + 1);
        self.data.extend_from_slice(&s);
        self.data.push(0);
        return index as u32;
    }

    pub fn write(&self, data : &mut Vec<u8>) {
        let size : u32 = (self.data.len() + 4).try_into().expect("To big string table");
        data.extend_from_slice(&size.to_le_bytes());
        data.extend_from_slice(&self.data);
    }
}

pub enum AuxiliarySymbol {
    Unknown,
    File(String),
    SectionDefinition {
        length : u32,
        relocations : u16
        // Rest is unused
    }
}

impl AuxiliarySymbol {
    pub fn write(&self, data: &mut Vec<u8>) {
        match self {
            AuxiliarySymbol::Unknown => {
                data.resize(data.len() + 8, 0);
            }
            AuxiliarySymbol::File(path) => {
                let mut bytes = path.clone().into_bytes();
                bytes.resize(18, 0);
                data.extend_from_slice(&bytes);
            }
            AuxiliarySymbol::SectionDefinition { length, relocations } => {
                data.extend_from_slice(&length.to_le_bytes());
                data.extend_from_slice(&relocations.to_le_bytes());
                data.resize(data.len() + 12, 0);
            }
        }
    }
}

pub struct CoffSymbol {
    pub name : String,
    pub value : [u8; 4],
    pub section_number : i16,
    pub symbol_type : u16,
    pub storage_class : u8,
    pub aux_symbols : Vec<AuxiliarySymbol>
}

impl CoffSymbol {
    pub const SIZE : usize = 18;
}

pub struct SymbolTable {
    pub symbols : Vec<CoffSymbol>,
    pub symbol_count : u32
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            symbols : Vec::new(), symbol_count : 0
        }
    }

    pub fn write(&self, data: &mut Vec<u8>, string_table : &mut StringTable) {
        for symbol in &self.symbols {
            let name_data = symbol.name.clone().into_bytes();
            if name_data.len() > 8 {
                let index = string_table.add_string(name_data);
                data.extend_from_slice(&0_u32.to_le_bytes());
                data.extend_from_slice(&index.to_le_bytes());
            } else {
                data.extend_from_slice(&name_data);
                data.resize(data.len() + 8 - name_data.len(), 0);
            }
            data.extend_from_slice(&symbol.value);
            data.extend_from_slice(&symbol.section_number.to_le_bytes());
            data.extend_from_slice(&symbol.symbol_type.to_le_bytes());
            data.push(symbol.storage_class);
            data.push(symbol.aux_symbols.len().try_into().expect(
                "Nothing should have 256 aux symbols"));
            for aux_symbol in &symbol.aux_symbols {
                aux_symbol.write(data);
            }
        }
    }

    pub fn add_file(&mut self, file_name : String) {
        self.symbols.push(CoffSymbol {
            name : ".file".to_owned(),
            value : [0, 0, 0, 0],
            section_number : -2,
            symbol_type : 0,
            storage_class : storage_class::IMAGE_SYM_CLASS_FILE,
            aux_symbols : vec![AuxiliarySymbol::File(file_name)]
        });
        self.symbol_count += 2;
    }

    pub fn add_external(&mut self, symbol_name : &str, section : i16, offset : u32) {
        self.symbols.push(CoffSymbol {
            name : symbol_name.to_owned(),
            value : offset.to_le_bytes(),
            section_number : section,
            symbol_type : 0,
            storage_class : storage_class::IMAGE_SYM_CLASS_EXTERNAL,
            aux_symbols : vec![]
        });
        self.symbol_count += 1;
    }

    pub fn add_section(&mut self, section_name : String, section : i16, size : u32, relocations : u16) {
        self.symbols.push(CoffSymbol {
            name : section_name,
            value : [0, 0, 0, 0],
            section_number : section,
            symbol_type : 0,
            storage_class : storage_class::IMAGE_SYM_CLASS_STATIC,
            aux_symbols : vec![
                AuxiliarySymbol::SectionDefinition {
                    length : size, relocations
                }
            ]
        });
        self.symbol_count += 2;
    }
}