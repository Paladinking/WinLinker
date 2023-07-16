

mod storage_class {
    pub const IMAGE_SYM_CLASS_NULL : u8 = 0;
    pub const IMAGE_SYM_CLASS_EXTERNAL : u8 = 2;
    pub const IMAGE_SYM_CLASS_STATIC : u8 = 3;
    pub const IMAGE_SYM_CLASS_FILE : u8 = 103;
}

enum AuxiliarySymbol {
    Unknown,
    File(String)
}



struct CoffSymbol {
    name : String,
    value : [u8; 4],
    section_number : i16,
    symbol_type : u16,
    storage_class : u8,
    aux_symbols : Vec<AuxiliarySymbol>
}

struct SymbolTable {
    symbols : Vec<CoffSymbol>
}

impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable {
            symbols : Vec::new()
        }
    }

    fn add_file(&mut self, file_name : String) {
        self.symbols.push(CoffSymbol {
            name : ".file".to_owned(),
            value : [0, 0, 0, 0],
            section_number : -2,
            symbol_type : 0,
            storage_class : storage_class::IMAGE_SYM_CLASS_FILE,
            aux_symbols : vec![AuxiliarySymbol::File(file_name)]
        });
    }

    fn add_external(&mut self, symbol_name : String, section : i16) {
        self.symbols.push(CoffSymbol {
            name : symbol_name,
            value : [0, 0, 0, 0],
            section_number : section,
            symbol_type : 0,
            storage_class : storage_class::IMAGE_SYM_CLASS_EXTERNAL,
            aux_symbols : vec![]
        });
    }

    fn add_section(&mut self, section_name : String, section : i16, size : usize) {
        
    }
}