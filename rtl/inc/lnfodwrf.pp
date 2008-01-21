{
    This file is part of the Free Pascal run time library.

    Copyright (c) 2006 by Thomas Schatzl, member of the FreePascal
    Development team
    Parts (c) 2000 Peter Vreman (adapted from original stabs line
    reader)

    Dwarf LineInfo Retriever

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit lnfodwrf;
interface

{ disable stack checking }
{$S-}

procedure GetLineInfo(addr:ptruint;var func,source:string;var line:longint);

implementation

{ Note to implementors of other OS loaders:

  - add a LoadXXX() function which has no parameters and returns a Boolean
   in the "OS loaders section" enclosing it using the OS specific define.
   This method should set the

     DwarfOpened,
     DwarfOffset and
     DwarfSize

   global variables properly (see comments at variable definition for more
   information).
   Additionally this method should return true if DWARF line info information
   could be found.

   The file variable which can be used for file I/O is the global "infile"
   variable.

  - in OpenDwarf(), add a call to this initializer function after the
   "run OS specific initializer" comment, again enclosed in the system
   specific define.
}

{ Current issues:

  - ignores DW_LNS_SET_FILE
  - slow
}

{ some type definitions }
type
{$IFDEF CPU32}
  UInt = DWord;
  Int = Longint;
{$ENDIF}
{$IFDEF CPU64}
  UInt = QWord;
  Int = Int64;
{$ENDIF}
  Bool8 = ByteBool;

var
  { the input file to read DWARF debug info from, i.e. paramstr(0) }
  infile : File;
  { size of the current file, cached }
  DwarfFilesize : Int64;

{ these variables should be set by the LoadXXX() methods for proper function }

  { set to true if DWARF debug info could be found in the file.
    The DwarfOffset and DwarfSize variables must be valid after setting this }
  DwarfOpened : Boolean;
  { the offset to the DWARF debug_line section in the file }
  DwarfOffset : Int64;
  { the size of the DWARF .debug_line section in the file in bytes }
  DwarfSize : SizeInt;

{$MACRO ON}

//{$DEFINE DEBUG_DWARF_PARSER}
{$ifdef DEBUG_DWARF_PARSER}
{$define DEBUG_WRITELN := WriteLn}
{$define DEBUG_COMMENT :=  }
{$else}
{$define DEBUG_WRITELN := //}
{$define DEBUG_COMMENT := //}
{$endif}

{---------------------------------------------------------------------------
 I/O utility functions
---------------------------------------------------------------------------}

var
  base, limit : SizeInt;
  index : SizeInt;

function Init(aBase, aLimit : Int64) : Boolean;
begin
  base := aBase;
  limit := aLimit;
  Init := (aBase + limit) <= DwarfFilesize;
  seek(infile, base);
  index := 0;
end;

function Init(aBase : Int64) : Boolean;
begin
  Init := Init(aBase, limit - (aBase - base));
end;

function Pos() : Int64;
begin
  Pos := index;
end;

procedure Seek(const newIndex : Int64);
begin
  index := newIndex;
  system.seek(infile, base + index);
end;

{ Returns the next Byte from the input stream, or -1 if there has been
  an error }
function ReadNext() : Int;
var
  bytesread : SizeInt;
  b : Byte;
begin
  ReadNext := -1;
  if (index < limit) then begin
    blockread(infile, b, 1, bytesread);
    ReadNext := b;
    inc(index);
  end;
  if (bytesread <> 1) then
    ReadNext := -1;
end;

{ Reads the next size bytes into dest. Returns true if successful,
  false otherwise. Note that dest may be partially overwritten after
  returning false. }
function ReadNext(var dest; size : SizeInt) : Boolean;
var
  bytesread : SizeInt;
begin
  bytesread := 0;
  if ((index + size) < limit) then begin
    blockread(infile, dest, size, bytesread);
    inc(index, size);
  end;
  ReadNext := (bytesread = size);
end;


{---------------------------------------------------------------------------
 OS specific loaders
---------------------------------------------------------------------------}

{$ifdef LINUX}
{$packrecords c}

{ ELF Header structures types}
type
  Elf32_Half = Word;
  Elf64_Half = Word;
  { Types for signed and unsigned 32-bit quantities.   }
  Elf32_Word = DWord;
  Elf32_Sword = Longint;
  Elf64_Word = DWord;
  Elf64_Sword = Longint;
  { Types for signed and unsigned 64-bit quantities.   }
  Elf32_Xword = QWord;
  Elf32_Sxword = Int64;
  Elf64_Xword = QWord;
  Elf64_Sxword = Int64;
  { Type of addresses.   }
  Elf32_Addr = DWord;
  Elf64_Addr = QWord;
  { Type of file offsets.   }
  Elf32_Off = DWord;
  Elf64_Off = QWord;
  { Type for section indices, which are 16-bit quantities.   }
  Elf32_Section = Word;
  Elf64_Section = Word;
  { Type for version symbol information.   }
  Elf32_Versym = Elf32_Half;
  Elf64_Versym = Elf64_Half;
{ some constants from the corresponding header files }
const
  El_NIDENT = 16;
  { some important indices into the e_ident signature of an ELF file }
  EI_MAG0 = 0;
  EI_MAG1 = 1;
  EI_MAG2 = 2;
  EI_MAG3 = 3;
  EI_CLASS = 4;
  { the first byte of the e_ident array must be of this value }
  ELFMAG0 = $7f;
  { the second byte of the e_ident array must be of this value }
  ELFMAG1 = Byte('E');
  { the third byte of the e_ident array must be of this value }
  ELFMAG2 = Byte('L');
  { the fourth byte of the e_ident array must be of this value }
  ELFMAG3 = Byte('F');

  { the fifth byte specifies the bitness of the header; all other values are invalid }
  ELFCLASS32 = 1;
  ELFCLASS64 = 2;

  ELFCLASS = {$IFDEF CPU32}ELFCLASS32{$ENDIF}{$IFDEF CPU64}ELFCLASS64{$ENDIF};

type
  { The ELF file header.  This appears at the start of every ELF file, 32 bit version }
  TElf32_Ehdr = record
    e_ident : array[0..El_NIDENT-1] of Byte; { file identification }
    e_type : Elf32_Half; { file type }
    e_machine : Elf32_Half; { machine architecture }
    e_version : Elf32_Word; { ELF format version }
    e_entry : Elf32_Addr; { entry point }
    e_phoff : Elf32_Off; { program header file offset }
    e_shoff : Elf32_Off; { section header file offset }
    e_flags : Elf32_Word; { architecture specific flags }
    e_ehsize : Elf32_Half; { size of ELF header in bytes }
    e_phentsize : Elf32_Half; { size of program header entry }
    e_phnum : Elf32_Half; { number of program header entries }
    e_shentsize : Elf32_Half; { size of section header entry }
    e_shnum : Elf32_Half; { number of section header entry }
    e_shstrndx : Elf32_Half; { section name strings section index }
  end;

  { ELF32 Section header }
  TElf32_Shdr = record
    sh_name : Elf32_Word; { section name }
    sh_type : Elf32_Word; { section type }
    sh_flags : Elf32_Word; { section flags }
    sh_addr : Elf32_Addr; { virtual address }
    sh_offset : Elf32_Off; { file offset }
    sh_size : Elf32_Word; { section size }
    sh_link : Elf32_Word; { misc info }
    sh_info : Elf32_Word; { misc info }
    sh_addralign : Elf32_Word; { memory alignment }
    sh_entsize : Elf32_Word; { entry size if table }
  end;

  { The ELF file header.  This appears at the start of every ELF file, 64 bit version }
  TElf64_Ehdr = record
    e_ident : array[0..El_NIDENT-1] of Byte;
    e_type : Elf64_Half;
    e_machine : Elf64_Half;
    e_version : Elf64_Word;
    e_entry : Elf64_Addr;
    e_phoff : Elf64_Off;
    e_shoff : Elf64_Off;
    e_flags : Elf64_Word;
    e_ehsize : Elf64_Half;
    e_phentsize : Elf64_Half;
    e_phnum : Elf64_Half;
    e_shentsize : Elf64_Half;
    e_shnum : Elf64_Half;
    e_shstrndx : Elf64_Half;
  end;

  { ELF64 Section header }
  TElf64_Shdr = record
    sh_name : Elf64_Word;
    sh_type : Elf64_Word;
    sh_flags : Elf64_Xword;
    sh_addr : Elf64_Addr;
    sh_offset : Elf64_Off;
    sh_size : Elf64_Xword;
    sh_link : Elf64_Word;
    sh_info : Elf64_Word;
    sh_addralign : Elf64_Xword;
    sh_entsize : Elf64_Xword;
  end;

  TElf_Shdr = {$ifdef cpu32}TElf32_Shdr{$endif}{$ifdef cpu64}TElf64_Shdr{$endif};
  TElf_Ehdr = {$ifdef cpu32}TElf32_Ehdr{$endif}{$ifdef cpu64}TElf64_Ehdr{$endif};

{ use globals to save stack space }
var
  header : TElf_Ehdr;
  strtab_header : TElf_Shdr;
  cursec_header : TElf_Shdr;

  buf : array[0..20] of char;

function LoadLinux() : Boolean;
var
  i : Integer;
begin
  LoadLinux := false;

  Init(0, DwarfFilesize);

  if (not ReadNext(header, sizeof(header))) then begin
    DEBUG_WRITELN('Could not read header');
    exit;
  end;

  { more paranoia checks }
  if ((header.e_ident[EI_MAG0] <> ELFMAG0) or (header.e_ident[EI_MAG1] <> ELFMAG1) or
    (header.e_ident[EI_MAG2] <> ELFMAG2) or (header.e_ident[EI_MAG3] <> ELFMAG3)) then begin
    DEBUG_WRITELN('Invalid ELF magic header. Exiting');
    exit;
  end;

  if (header.e_ident[EI_CLASS] <> ELFCLASS) then begin
    DEBUG_WRITELN('Invalid ELF header bitness. Exiting');
    exit;
  end;

  { check e_version = , e_shentsize > 0, e_shnum > 0 }


  { seek to the start of section headers }

  { first get string section header }
  Init(header.e_shoff + (header.e_shstrndx * header.e_shentsize));
  if (not ReadNext(strtab_header, sizeof(strtab_header))) then begin
    DEBUG_WRITELN('Could not read string section header');
    exit;
  end;

  for i := 0 to (header.e_shnum-1) do begin
    Init(header.e_shoff + (i * header.e_shentsize));
    if (not ReadNext(cursec_header, sizeof(cursec_header))) then begin
      DEBUG_WRITELN('Could not read next section header');
      exit;
    end;
    { paranoia TODO: check cursec_header.e_shentsize }

    Init(strtab_header.sh_offset + cursec_header.sh_name);
    if (not ReadNext(buf, sizeof(buf))) then begin
      DEBUG_WRITELN('Could not read section name');
      exit;
    end;
    buf[sizeof(buf)-1] := #0;

    DEBUG_WRITELN('This section is "', pchar(@buf[0]), '", offset ', cursec_header.sh_offset, ' size ', cursec_header.sh_size);
    if (pchar(@buf[0]) = '.debug_line') then begin
      DEBUG_WRITELN('.debug_line section found');
      DwarfOffset := cursec_header.sh_offset;
      DwarfSize := cursec_header.sh_size;
      { more checks }
      LoadLinux := (DwarfOffset >= 0) and (DwarfSize > 0);
    end;
  end;
end;
{$endif LINUX}


{---------------------------------------------------------------------------

 Generic Dwarf lineinfo reader

 The line info reader is based on the information contained in

   DWARF Debugging Information Format Version 3
   Chapter 6.2 "Line Number Information"

 from the

   DWARF Debugging Information Format Workgroup.

 For more information on this document see also

   http://dwarf.freestandards.org/

---------------------------------------------------------------------------}

procedure CloseDwarf();
begin
  if (DwarfOpened) then
    close(infile);
  DwarfOpened := false;
end;

function OpenDwarf() : Boolean;
var
  oldfilemode : Word;
begin
  OpenDwarf := false;
  { open input file }
  assign(infile, paramstr(0));
  {$I-}
  oldfilemode := filemode;
  filemode := $40;
  reset(infile, 1);
  filemode := oldfilemode;
  {$I+}
  if (ioresult <> 0) then begin
    DEBUG_WRITELN('Could not open file');
    exit;
  end;
  DwarfFilesize := filesize(infile);
  DwarfOpened := true;
  { run OS specific initializer }
  {$ifdef linux}
  if (LoadLinux()) then begin
    OpenDwarf := true;
    exit;
  end;
  {$endif}
  CloseDwarf();
end;

{$packrecords default}
{ DWARF 2 default opcodes}
const
  { Extended opcodes }
  DW_LNE_END_SEQUENCE = 1;
  DW_LNE_SET_ADDRESS = 2;
  DW_LNE_DEFINE_FILE = 3;
  { Standard opcodes }
  DW_LNS_COPY = 1;
  DW_LNS_ADVANCE_PC = 2;
  DW_LNS_ADVANCE_LINE = 3;
  DW_LNS_SET_FILE = 4;
  DW_LNS_SET_COLUMN = 5;
  DW_LNS_NEGATE_STMT = 6;
  DW_LNS_SET_BASIC_BLOCK = 7;
  DW_LNS_CONST_ADD_PC = 8;
  DW_LNS_FIXED_ADVANCE_PC = 9;
  DW_LNS_SET_PROLOGUE_END = 10;
  DW_LNS_SET_EPILOGUE_BEGIN = 11;
  DW_LNS_SET_ISA = 12;

type
  { state record for the line info state machine }
  TMachineState = record
    address : QWord;
    file_id : DWord;
    line : QWord;
    column : DWord;
    is_stmt : Boolean;
    basic_block : Boolean;
    end_sequence : Boolean;
    prolouge_end : Boolean;
    epilouge_begin : Boolean;
    isa : DWord;
    append_row : Boolean;
  end;

{ DWARF line number program header preceding the line number program, 64 bit version }
  TLineNumberProgramHeader64 = packed record
    magic : DWord;
    unit_length : QWord;
    version : Word;
    length : QWord;
    minimum_instruction_length : Byte;
    default_is_stmt : Bool8;
    line_base : ShortInt;
    line_range : Byte;
    opcode_base : Byte;
  end;

{ DWARF line number program header preceding the line number program, 32 bit version }
  TLineNumberProgramHeader32 = packed record
    unit_length : DWord;
    version : Word;
    length : DWord;
    minimum_instruction_length : Byte;
    default_is_stmt : Bool8;
    line_base : ShortInt;
    line_range : Byte;
    opcode_base : Byte;
  end;

{ initializes the line info state to the default values }
procedure InitStateRegisters(var state : TMachineState; const aIs_Stmt : Bool8);
begin
  with state do begin
    address := 0;
    file_id := 1;
    line := 1;
    column := 0;
    is_stmt := aIs_Stmt;
    basic_block := false;
    end_sequence := false;
    prolouge_end := false;
    epilouge_begin := false;
    isa := 0;
    append_row := false;
  end;
end;

{ Reads an unsigned LEB encoded number from the input stream }
function ReadULEB128() : QWord;
var
  shift : Byte;
  data : Int;
  val : QWord;
  result : QWord;
begin
  shift := 0;
  result := 0;
  data := ReadNext();
  while (data <> -1) do begin
    val := data and $7f;
    result := result or (val shl shift);
    inc(shift, 7);
    if ((data and $80) = 0) then
      break;
    data := ReadNext();
  end;
  ReadULEB128 := result;
end;

{ Reads a signed LEB encoded number from the input stream }
function ReadLEB128() : Int64;
var
  shift : Byte;
  data : Int;
  val : Int64;
  result : Int64;
begin
  shift := 0;
  result := 0;
  data := ReadNext();
  while (data <> -1) do begin
    val := data and $7f;
    result := result or (val shl shift);
    inc(shift, 7);
    if ((data and $80) = 0) then
      break;
    data := ReadNext();
  end;
  { extend sign. Note that we can not use shl/shr since the latter does not
    translate to arithmetic shifting for signed types }
  result := (not ((result and (1 shl (shift-1)))-1)) or result;
  ReadLEB128 := result;
end;

{ Reads an address from the current input stream }
function ReadAddress() : PtrUInt;
var
  result : PtrUInt;
begin
  ReadNext(result, sizeof(result));
  ReadAddress := result;
end;

{ Reads a zero-terminated string from the current input stream. If the
  string is larger than 255 chars (maximum allowed number of elements in
  a ShortString, excess characters will be chopped off. }
function ReadString() : ShortString;
var
  temp : Int;
  i : UInt;
  result : ShortString;
begin
  i := 1;
  temp := ReadNext();
  while (temp > 0) do begin
    result[i] := char(temp);
    if (i = 255) then begin
      { skip remaining characters }
      repeat
        temp := ReadNext();
      until (temp <= 0);
      break;
    end;
    inc(i);
    temp := ReadNext();
  end;
  { unexpected end of file occurred? }
  if (temp = -1) then
    result := ''
  else
    Byte(result[0]) := i-1;
  ReadString := result;
end;

{ Reads an unsigned Half from the current input stream }
function ReadUHalf() : Word;
var
  result : Word;
begin
  ReadNext(result, sizeof(result));
  ReadUHalf := result;
end;

{ Skips all line info directory entries }
procedure SkipDirectories();
var s : ShortString;
begin
  while (true) do begin
    s := ReadString();
    if (s = '') then break;
    DEBUG_WRITELN('Skipping directory : ', s);
  end;
end;

{ Skips an LEB128 }
procedure SkipLEB128();
{$ifdef DEBUG_DWARF_PARSER}
var temp : QWord;
{$endif}
begin
  {$ifdef DEBUG_DWARF_PARSER}temp := {$endif}ReadLEB128();
  DEBUG_WRITELN('Skipping LEB128 : ', temp);
end;

{ Skips the filename section from the current file stream }
procedure SkipFilenames();
var s : ShortString;
begin
  while (true) do begin
    s := ReadString();
    if (s = '') then break;
    DEBUG_WRITELN('Skipping filename : ', s);
    SkipLEB128(); { skip the directory index for the file }
    SkipLEB128(); { skip last modification time for file }
    SkipLEB128(); { skip length of file }
  end;
end;

function CalculateAddressIncrement(opcode : Byte; const header : TLineNumberProgramHeader64) : Int64;
var
  result : Int64;
begin
  result := (Int64(opcode) - header.opcode_base) div header.line_range * header.minimum_instruction_length;
  CalculateAddressIncrement := result;
end;

function GetFullFilename(const filenameStart, directoryStart : Int64; const file_id : DWord) : ShortString;
var
  i : DWord;
  filename, directory : ShortString;
  dirindex : Int64;
begin
  filename := '';
  directory := '';
  i := 1;
  Seek(filenameStart);
  while (i <= file_id) do begin
    filename := ReadString();
    DEBUG_WRITELN('Found "', filename, '"');
    if (filename = '') then break;
    dirindex := ReadLEB128(); { read the directory index for the file }
    SkipLEB128(); { skip last modification time for file }
    SkipLEB128(); { skip length of file }
    inc(i);
  end;
  { if we could not find the file index, exit }
  if (filename = '') then begin
    GetFullFilename := '(Unknown file)';
    exit;
  end;

  Seek(directoryStart);
  i := 1;
  while (i <= dirindex) do begin
    directory := ReadString();
    if (directory = '') then break;
    inc(i);
  end;
  if (directory<>'') and (directory[length(directory)]<>'/') then
    directory:=directory+'/';
  GetFullFilename := directory + filename;
end;

function ParseCompilationUnit(const addr : PtrUInt; const file_offset : QWord;
  var source : String; var line : longint; var found : Boolean) : QWord;
var
  state : TMachineState;
  { we need both headers on the stack, although we only use the 64 bit one internally }
  header64 : TLineNumberProgramHeader64;
  header32 : TLineNumberProgramHeader32;

  adjusted_opcode : Int64;

  opcode : Int;
  extended_opcode : Byte;
  extended_opcode_length : Int;
  i, addrIncrement, lineIncrement : Int;

  {$ifdef DEBUG_DWARF_PARSER}
  s : ShortString;
  {$endif}

  numoptable : array[1..255] of Byte;
  { the offset into the file where the include directories are stored for this compilation unit }
  include_directories : QWord;
  { the offset into the file where the file names are stored for this compilation unit }
  file_names : Int64;

  temp_length : DWord;
  unit_length : QWord;
  header_length : SizeInt;

  first_row : Boolean;

  prev_line : QWord;
  prev_file : DWord;

begin
  prev_line := 0;
  prev_file := 0;
  first_row := true;

  found := false;

  ReadNext(temp_length, sizeof(temp_length));
  if (temp_length <> $ffffffff) then begin
    unit_length := temp_length + sizeof(temp_length)
  end else begin
    ReadNext(unit_length, sizeof(unit_length));
    inc(unit_length, 12);
  end;

  ParseCompilationUnit := file_offset + unit_length;

  Init(file_offset, unit_length);

  DEBUG_WRITELN('Unit length: ', unit_length);
  if (temp_length <> $ffffffff) then begin
    DEBUG_WRITELN('32 bit DWARF detected');
    ReadNext(header32, sizeof(header32));
    header64.magic := $ffffffff;
    header64.unit_length := header32.unit_length;
    header64.version := header32.version;
    header64.length := header32.length;
    header64.minimum_instruction_length := header32.minimum_instruction_length;
    header64.default_is_stmt := header32.default_is_stmt;
    header64.line_base := header32.line_base;
    header64.line_range := header32.line_range;
    header64.opcode_base := header32.opcode_base;
    header_length :=
      sizeof(header32.length) + sizeof(header32.version) +
      sizeof(header32.unit_length);
  end else begin
    DEBUG_WRITELN('64 bit DWARF detected');
    ReadNext(header64, sizeof(header64));
    header_length :=
      sizeof(header64.magic) + sizeof(header64.version) +
      sizeof(header64.length) + sizeof(header64.unit_length);
  end;

  inc(header_length, header64.length);

  fillchar(numoptable, sizeof(numoptable), #0);
  ReadNext(numoptable, header64.opcode_base-1);
  DEBUG_WRITELN('Opcode parameter count table');
  for i := 1 to header64.opcode_base-1 do begin
    DEBUG_WRITELN('Opcode[', i, '] - ', numoptable[i], ' parameters');
  end;

  DEBUG_WRITELN('Reading directories...');
  include_directories := Pos();
  SkipDirectories();
  DEBUG_WRITELN('Reading filenames...');
  file_names := Pos();
  SkipFilenames();

  Seek(header_length);

  with header64 do begin
    InitStateRegisters(state, default_is_stmt);
  end;
  opcode := ReadNext();
  while (opcode <> -1) and (not found) do begin
    DEBUG_WRITELN('Next opcode: ');
    case (opcode) of
      { extended opcode }
      0 : begin
        extended_opcode_length := ReadULEB128();
        extended_opcode := ReadNext();
        case (extended_opcode) of
          DW_LNE_END_SEQUENCE : begin
            state.end_sequence := true;
            state.append_row := true;
            DEBUG_WRITELN('DW_LNE_END_SEQUENCE');
          end;
          DW_LNE_SET_ADDRESS : begin
            state.address := ReadAddress();
            DEBUG_WRITELN('DW_LNE_SET_ADDRESS (', hexstr(state.address, sizeof(state.address)*2), ')');
          end;
          DW_LNE_DEFINE_FILE : begin
            {$ifdef DEBUG_DWARF_PARSER}s := {$endif}ReadString();
            SkipLEB128();
            SkipLEB128();
            SkipLEB128();
            DEBUG_WRITELN('DW_LNE_DEFINE_FILE (', s, ')');
          end;
          else begin
            DEBUG_WRITELN('Unknown extended opcode (opcode ', extended_opcode, ' length ', extended_opcode_length, ')');
            for i := 0 to extended_opcode_length-2 do
              ReadNext();
          end;
        end;
      end;
      DW_LNS_COPY : begin
        state.basic_block := false;
        state.prolouge_end := false;
        state.epilouge_begin := false;
        state.append_row := true;
        DEBUG_WRITELN('DW_LNS_COPY');
      end;
      DW_LNS_ADVANCE_PC : begin
        inc(state.address, ReadULEB128() * header64.minimum_instruction_length);
        DEBUG_WRITELN('DW_LNS_ADVANCE_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
      end;
      DW_LNS_ADVANCE_LINE : begin
        inc(state.line, ReadLEB128());
        DEBUG_WRITELN('DW_LNS_ADVANCE_LINE (', state.line, ')');
      end;
      DW_LNS_SET_FILE : begin
        state.file_id := ReadULEB128();
        DEBUG_WRITELN('DW_LNS_SET_FILE (', state.file_id, ')');
      end;
      DW_LNS_SET_COLUMN : begin
        state.column := ReadULEB128();
        DEBUG_WRITELN('DW_LNS_SET_COLUMN (', state.column, ')');
      end;
      DW_LNS_NEGATE_STMT : begin
        state.is_stmt := not state.is_stmt;
        DEBUG_WRITELN('DW_LNS_NEGATE_STMT (', state.is_stmt, ')');
      end;
      DW_LNS_SET_BASIC_BLOCK : begin
        state.basic_block := true;
        DEBUG_WRITELN('DW_LNS_SET_BASIC_BLOCK');
      end;
      DW_LNS_CONST_ADD_PC : begin
        inc(state.address, CalculateAddressIncrement(255, header64));
        DEBUG_WRITELN('DW_LNS_CONST_ADD_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
      end;
      DW_LNS_FIXED_ADVANCE_PC : begin
        inc(state.address, ReadUHalf());
        DEBUG_WRITELN('DW_LNS_FIXED_ADVANCE_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
      end;
      DW_LNS_SET_PROLOGUE_END : begin
        state.prolouge_end := true;
        DEBUG_WRITELN('DW_LNS_SET_PROLOGUE_END');
      end;
      DW_LNS_SET_EPILOGUE_BEGIN : begin
        state.epilouge_begin := true;
        DEBUG_WRITELN('DW_LNS_SET_EPILOGUE_BEGIN');
      end;
      DW_LNS_SET_ISA : begin
        state.isa := ReadULEB128();
        DEBUG_WRITELN('DW_LNS_SET_ISA (', state.isa, ')');
      end;
      else begin { special opcode }
        if (opcode < header64.opcode_base) then begin
          DEBUG_WRITELN('Unknown standard opcode $', hexstr(opcode, 2), '; skipping');
          for i := 1 to numoptable[opcode] do
            SkipLEB128();
        end else begin
          adjusted_opcode := opcode - header64.opcode_base;
          addrIncrement := CalculateAddressIncrement(opcode, header64);
          inc(state.address, addrIncrement);
          lineIncrement := header64.line_base + (adjusted_opcode mod header64.line_range);
          inc(state.line, lineIncrement);
          DEBUG_WRITELN('Special opcode $', hexstr(opcode, 2), ' address increment: ', addrIncrement, ' new line: ', lineIncrement);
          state.basic_block := false;
          state.prolouge_end := false;
          state.epilouge_begin := false;
          state.append_row := true;
        end;
      end;
    end;

    if (state.append_row) then begin
      DEBUG_WRITELN('Current state : address = ', hexstr(state.address, sizeof(state.address) * 2),
      DEBUG_COMMENT ' file_id = ', state.file_id, ' line = ', state.line, ' column = ', state.column,
      DEBUG_COMMENT  ' is_stmt = ', state.is_stmt, ' basic_block = ', state.basic_block,
      DEBUG_COMMENT  ' end_sequence = ', state.end_sequence, ' prolouge_end = ', state.prolouge_end,
      DEBUG_COMMENT  ' epilouge_begin = ', state.epilouge_begin, ' isa = ', state.isa);

      if (first_row) then begin
        if (state.address > addr) then
          break;
        first_row := false;
      end;

      { when we have found the address we need to return the previous
        line because that contains the call instruction }
      if (state.address >= addr) then
        found:=true
      else
        begin
          { save line information }
          prev_file := state.file_id;
          prev_line := state.line;
        end;

      state.append_row := false;
      if (state.end_sequence) then begin
        InitStateRegisters(state, header64.default_is_stmt);
      end;
    end;

    opcode := ReadNext();
  end;

  if (found) then begin
    line := prev_line;
    source := GetFullFilename(file_names, include_directories, prev_file);
  end;
end;

procedure GetLineInfo(addr : ptruint; var func, source : string; var line : longint);
var
  current_offset : QWord;
  end_offset : QWord;

  found : Boolean;

begin
  func := '';
  source := '';
  found := false;

  if (not DwarfOpened) and (not OpenDwarf()) then
    exit;

  current_offset := DwarfOffset;
  end_offset := DwarfOffset + DwarfSize;

  while (current_offset < end_offset) and (not found) do begin
    Init(current_offset, end_offset - current_offset);
    current_offset := ParseCompilationUnit(addr, current_offset,
      source, line, found);
  end;
end;


function DwarfBackTraceStr(addr : Pointer) : shortstring;
var
  func,
  source : string;
  hs     : string[32];
  line   : longint;
  Store  : TBackTraceStrFunc;
begin
  { reset to prevent infinite recursion if problems inside the code }
  Store := BackTraceStrFunc;
  BackTraceStrFunc := @SysBackTraceStr;
  GetLineInfo(ptruint(addr), func, source, line);
  { create string }
  DwarfBackTraceStr :='  $' + HexStr(ptruint(addr), sizeof(ptruint) * 2);
  if func<>'' then
   DwarfBackTraceStr := DwarfBackTraceStr + '  ' + func;

  if source<>'' then begin
    if func<>'' then
      DwarfBackTraceStr := DwarfBackTraceStr + ', ';
    if line<>0 then begin
      str(line, hs);
      DwarfBackTraceStr := DwarfBackTraceStr + ' line ' + hs;
    end;
    DwarfBackTraceStr := DwarfBackTraceStr + ' of ' + source;
  end;
  if (DwarfOpened) then
    BackTraceStrFunc := Store;
end;


initialization
  DwarfOpened := false;
  BackTraceStrFunc := @DwarfBacktraceStr;

finalization
  CloseDwarf();
end.
