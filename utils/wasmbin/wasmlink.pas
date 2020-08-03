unit wasmlink;
// The unit covers the WebAssembly static linking convention
// as described at https://github.com/WebAssembly/tool-conventions/blob/master/Linking.md

interface

uses
  Classes, SysUtils, lebutils, wasmbin;

const
  SectionName_Linking = 'linking';
  SectionNamePfx_Reloc = 'reloc.';

type
  TRelocationSection = record
    section : UInt32; // the index of the target section
    count   : Uint32; // count of entries to follow
  end;

  TRelocationEntry = record
    reltype : UInt8;  // the relocation type (see R_WASM constants)
    offset  : UInt32; // offset of the value to rewrite
    index   : Uint32; // the index of the symbol used (or, for R_WASM_TYPE_INDEX_LEB relocations, the index of the type)
  end;
  TRelocationEntryEx = record
    entry  : TRelocationEntry;
    addend : UInt32;
  end;

const
  // A relocation type can be one of the following:
  R_WASM_FUNCTION_INDEX_LEB  = 0;  // a function index encoded as a 5-byte varuint32. Used for the immediate argument of a call instruction.
  R_WASM_TABLE_INDEX_SLEB    = 1;  // a function table index encoded as a 5-byte varint32. Used to refer to the immediate argument of a i32.const instruction, e.g. taking the address of a function.
  R_WASM_TABLE_INDEX_I32     = 2;  // a function table index encoded as a uint32, e.g. taking the address of a function in a static data initializer.
  R_WASM_MEMORY_ADDR_LEB     = 3;  // a linear memory index encoded as a 5-byte varuint32. Used for the immediate argument of a load or store instruction, e.g. directly loading from or storing to a C++ global.
  R_WASM_MEMORY_ADDR_SLEB    = 4;  // a linear memory index encoded as a 5-byte varint32. Used for the immediate argument of a i32.const instruction, e.g. taking the address of a C++ global.
  R_WASM_MEMORY_ADDR_I32     = 5;  // a linear memory index encoded as a uint32, e.g. taking the address of a C++ global in a static data initializer.
  R_WASM_TYPE_INDEX_LEB      = 6;  // a type table index encoded as a 5-byte varuint32, e.g. the type immediate in a call_indirect.
  R_WASM_GLOBAL_INDEX_LEB    = 7;  // a global index encoded as a 5-byte varuint32, e.g. the index immediate in a get_global.
  R_WASM_FUNCTION_OFFSET_I32 = 8;  // a byte offset within code section for the specic function encoded as a uint32. The offsets start at the actual function code excluding its size field.
  R_WASM_SECTION_OFFSET_I32  = 9;  // an byte offset from start of the specified section encoded as a uint32.
  R_WASM_EVENT_INDEX_LEB     = 10; // an event index encoded as a 5-byte varuint32. Used for the immediate argument of a throw and if_except instruction.
  R_WASM_TABLE_NUMBER_LEB    = 13; // a table number encoded as a 5-byte varuint32. Used for the table immediate argument in the table.* instructions.

type
  TLinkingMetadata = record
    version : UInt32; // the version of linking metadata contained in this section. Currently: 2
  end;

  TLinkinSubSection = record
    sectype : UInt8;   // code identifying type of subsection
    length  : UInt32;  // size of this subsection in bytes
  end;

const
  LINKING_VERSION = 2;

  // The current list of valid TLinkinSubSection.sectype codes are:
  WASM_SEGMENT_INFO = 5; // Extra metadata about the data segments.
  WASM_INIT_FUNCS   = 6; // Specifies a list of constructor functions to be called at startup.
                         // These constructors will be called in priority order after memory
                         // has been initialized.
  WASM_COMDAT_INFO  = 7; // Specifies the COMDAT groups of associated linking objects,
                         // which are linked only once and all together.
  WASM_SYMBOL_TABLE = 8; // Specifies extra information about the symbols present in the module


type
  TSymInfo = record
    kind        : UInt8;
    flags       : UInt32;

    hasSymIndex : Boolean; // always true for Kind non Data
                           // for Data it's true for defined symbols (see flags);
    symindex    : UInt32;  // either symbol or data symbol offset
    hasSymName  : Boolean;
    symname     : string;
    dataofs     : integer; // only if Kind is Data and hasSymIndex = true;
    datasize    : integer;

  end;

// The symbol type. One of:
const
  SYMTAB_FUNCTION = 0;
  SYMTAB_DATA     = 1;
  SYMTAB_GLOBAL   = 2;
  SYMTAB_SECTION  = 3;
  SYMTAB_EVENT    = 4;
  SYMTAB_TABLE    = 5;

//  The current set of valid flags for symbols are:
const
  // Indicating that this is a weak symbol.  When linking multiple modules
  // defining the same symbol, all weak definitions are discarded if
  // any strong definitions exist; then if multiple weak definitions
  // exist all but one (unspecified) are discarded; and finally it is an error
  // if more than one definition remains.
  WASM_SYM_BINDING_WEAK      = $01;

  // Indicating that this is a local symbol (this is exclusive
  // with WASM_SYM_BINDING_WEAK). Local symbols are not to be exported,
  // or linked to other modules/sections. The names of all non-local
  // symbols must be unique, but the names of local symbols are
  // not considered for uniqueness. A local function or global
  // symbol cannot reference an import.
  WASM_SYM_BINDING_LOCAL     = $02;

  // Indicating that this is a hidden symbol. Hidden symbols are not to be
  // exported when performing the final link, but may be linked to other modules.
  WASM_SYM_VISIBILITY_HIDDEN = $04;

  // Indicating that this symbol is not defined. For non-data symbols,
  // this must match whether the symbol is an import or is defined;
  // for data symbols, determines whether a segment is specified.
  WASM_SYM_UNDEFINED         = $10;
  WASM_SYM_IMPORTED          = WASM_SYM_UNDEFINED;

  // The symbol is intended to be exported from the wasm module to the host
  // environment. This differs from the visibility flags in that it effects
  // the static linker.
  WASM_SYM_EXPORTED          = $20;

  // The symbol uses an explicit symbol name, rather than reusing the name
  // from a wasm import. This allows it to remap imports from foreign WebAssembly
  // modules into local symbols with different names.
  WASM_SYM_EXPLICIT_NAME     = $40;

  // The symbol is intended to be included in the linker output,
  // regardless of whether it is used by the program.
  WASM_SYM_NO_STRIP          = $80;

function ReadMetaData(st: TStream; out m:TLinkingMetadata): Boolean;
function ReadLinkSubSect(st: TStream; out m: TLinkinSubSection): Boolean;
function ReadSymInfo(st: TStream; out m: TSymInfo): Boolean;

procedure WriteSymInfo(st: TStream; const m: TSymInfo);

// dumps linking information. Note: that the name of the "Linking" section
// must have already been read
procedure DumpLinking(st: TStream; secsize: integer);

function SubSecTypeToStr(b: Byte): string;
function SymKindToStr(b: Byte): string;

implementation

function ReadMetaData(st: TStream; out m:TLinkingMetadata): Boolean;
begin
  FillChar(m, sizeof(m), 0);
  m.version := st.ReadByte;
  Result:=true;
end;

function ReadLinkSubSect(st: TStream; out m: TLinkinSubSection): Boolean;
begin
  FillChar(m, sizeof(m), 0);
  m.sectype := ReadU(st);
  m.length := ReadU(st);
  Result := true;
end;

function ReadSymInfo(st: TStream; out m: TSymInfo): Boolean;
begin
  FillChar(m, sizeof(m), 0);
  m.kind := st.ReadByte;
  m.flags := ReadU(st);

  if m.kind = SYMTAB_DATA then begin
    m.hasSymName := true; // always exist
    m.symname := ReadName(st);

    m.hasSymIndex := (m.flags and WASM_SYM_UNDEFINED)=0;
    if m.hasSymIndex then begin
      m.symindex := ReadU(st);
      m.dataofs := ReadU(st);
      m.datasize := ReadU(st);
    end;

  end else begin
    m.hasSymIndex := true; // always exists
    m.symindex := ReadU(st);

    m.hasSymName := ((m.flags and WASM_SYM_IMPORTED) = 0) or ((m.flags and WASM_SYM_EXPLICIT_NAME) > 0); // imported
    if m.hasSymName then
      m.symname:=ReadName(st);
  end;

  Result := true;
end;

procedure WriteSymInfo(st: TStream; const m: TSymInfo);
begin
  st.WriteByte(m.kind);
  WriteU32(st, m.flags);

  if m.kind = SYMTAB_DATA then begin
    WriteName(st, m.symname);
    if (m.flags and WASM_SYM_UNDEFINED)=0 then begin
      WriteU32(st, m.symindex);
      WriteU32(st, m.dataofs);
      WriteU32(st, m.datasize);
    end;

  end else begin
    WriteU32(st, m.symindex);
    if ((m.flags and WASM_SYM_IMPORTED) = 0) or ((m.flags and WASM_SYM_EXPLICIT_NAME) > 0) then
      WriteName(st, m.symname);
  end;
end;

procedure DumpLinking(st: TStream; secsize: integer);
var
  mt  : TLinkingMetadata;
  en  : Int64;
  sub : TLinkinSubSection;
  cnt : LongWord;
  nx  : Int64;
  i   : integer;
  si  : TSymInfo;
begin
  en := st.Position+secsize;
  ReadMetadata(st, mt);
  writeln('version: ', mt.version);
  while st.Position<en do begin
    ReadLinkSubSect(st, sub);
    nx := st.Position+sub.length;

    writeln('subsec=',SubSecTypeToStr(sub.sectype),' ',sub.sectype);
    cnt := ReadU(st);
    writeln('- symbol table [count=', cnt,']');
    for i:=0 to cnt-1 do begin
      write('  - ',i,' ');
      ReadSymInfo(st, si);
      write(SymKindToStr(si.kind),' ',IntToHex(si.flags,8));
      if si.hasSymName then write(' ',si.symname);
      writeln;
      //writeln(si.symname);
    end;

    st.Position:=nx;
  end;
end;

function SubSecTypeToStr(b: Byte): string;
begin
  case b of
    WASM_SEGMENT_INFO: Result := 'WASM_SEGMENT_INFO';
    WASM_INIT_FUNCS:   Result := 'WASM_INIT_FUNCS';
    WASM_COMDAT_INFO:  Result := 'WASM_COMDAT_INFO';
    WASM_SYMBOL_TABLE: Result := 'WASM_SYMBOL_TABLE';
  else
    Result := Format('UNKNOWN %d',[b]);
  end;
end;

function SymKindToStr(b: Byte): string;
begin
  case b of
    SYMTAB_FUNCTION: Result := 'F';
    SYMTAB_DATA:     Result := 'D';
    SYMTAB_GLOBAL:   Result := 'G';
    SYMTAB_SECTION:  Result := 'S';
    SYMTAB_EVENT:    Result := 'E';
    SYMTAB_TABLE:    Result := 'T';
  else
    Result := 'U'+IntToStR(b);
  end;
end;


end.
