# wasmbin
Collection of WebAssembly binary utils

## wasmtool
wasmtool is an utility that allows to modify wasm binary linking information. wat2wasm util is capable of generating binaries with relocation information. However the symbol flags are setup in the manner that final binaries cannot be linked by wasm-ld utility. (i.e. non setting weak symbols).

### options
    --exportrename inputfile
The option allow to change the export name. The reasoning for that is wasm-ld behavior of renaming the export from the declared name to the symbol name. 

The input file can be either a text file, containing a list of oldname new name values:

    OldExportName=NewExportName
or the input file can point to an object file. The object file's export section is parsed for the desired export names. 

    --symbolflag inputfile
The action allows to modify flags for the specified symbols.
The input file specified must contain the pairs of symbolname + desired flags.
  
    $TESTUNIT_$$_ADD$LONGINT$LONGINT$$LONGINT=WH
The desired flag is a string (or a character) that should consists of the following characters. Each character corresponds to a certain symbol linking flag:

* W - WASM_SYM_BINDING_WEAK

* H - WASM_SYM_VISIBILITY_HIDDEN

* L - WASM_SYM_BINDING_LOCAL
 
* D - removes flag WASM_SYM_BINDING_LOCAL
    
    --symbolauto
The flags for each symbol updated and  is determined based of the symbol use:
