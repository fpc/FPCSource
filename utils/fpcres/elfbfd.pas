{$ifdef fpc}
{$mode objfpc}
{$endif}
{$H+}
unit elfbfd;

{ELF Binary Format Description. 32/64 bit definitions }

interface

const
  SHT_NULL     = 0;
  SHT_PROGBITS = 1;
  SHT_SYMTAB   = 2;
  SHT_STRTAB   = 3;
  SHT_RELA     = 4;
  SHT_HASH     = 5;
  SHT_DYNAMIC  = 6;
  SHT_NOTE     = 7;
  SHT_NOBITS   = 8;
  SHT_REL      = 9;
  SHT_SHLIB    = 10;
  SHT_DYNSYM   = 11;
  SHF_WRITE     = 1;
  SHF_ALLOC     = 2;
  SHF_EXECINSTR = 4;

  PT_NULL       = 0;
  PT_LOAD 	= 1;
  PT_DYNAMIC 	= 2;
  PT_INTERP 	= 3;
  PT_NOTE 	= 4;
  PT_SHLIB 	= 5;
  PT_PHDR 	= 6;
  PT_LOOS 	= $60000000;
  PT_HIOS 	= $6fffffff;
  PT_LOPROC 	= $70000000;
  PT_HIPROC 	= $7fffffff;

Type
  TElf32header = packed record
    magic0123: longint;
    file_class: byte;
    data_encoding: byte;
    file_version: byte;
    padding: array[$07..$0F] of byte;
    e_type: word;
    e_machine: word;
    e_version: longint;
    e_entry: longint; { entrypoint }
    e_phoff: longint; { program header offset }
    e_shoff: longint; { sections header offset }
    e_flags: longint;
    e_ehsize: word; { elf header size in bytes }
    e_phentsize: word; { size of an entry in the program header array }
    e_phnum: word; { 0..e_phnum-1 of entrys }
    e_shentsize: word; { size of an entry in sections header array }
    e_shnum: word; { 0..e_shnum-1 of entrys }
    e_shstrndx: word; { index of string section header }
  end;

  TElf64header = packed record
    magic0123: longint;
    file_class: byte;
    data_encoding: byte;
    file_version: byte;
    padding: array[$07..$0F] of byte;
    e_type: word;
    e_machine: word;
    e_version: longint;
    e_entry: int64; { entrypoint }
    e_phoff: int64; { program header offset }
    e_shoff: int64; { sections header offset }
    e_flags: longint;
    e_ehsize: word; { elf header size in bytes }
    e_phentsize: word; { size of an entry in the program header array }
    e_phnum: word; { 0..e_phnum-1 of entrys }
    e_shentsize: word; { size of an entry in sections header array }
    e_shnum: word; { 0..e_shnum-1 of entrys }
    e_shstrndx: word; { index of string section header }
  end;

  TElf32sechdr = packed record
    sh_name: longint;
    sh_type: longint;
    sh_flags: longint;
    sh_addr: longint;
    sh_offset: longint;
    sh_size: longint;
    sh_link: longint;
    sh_info: longint;
    sh_addralign: longint;
    sh_entsize: longint;
  end;

  TElf64sechdr = packed record
    sh_name: longint;
    sh_type: longint;
    sh_flags: longint;
    sh_addr: int64;
    sh_offset: int64;
    sh_size: int64;
    sh_link: longint;
    sh_info: longint;
    sh_addralign: int64;
    sh_entsize: int64;
  end;
  
  { FPC resources }
  
  TELF32ResourceSectionInfo = packed record
    ptr: longint;
    size: longint;
  end;

  TELF64ResourceSectionInfo = packed record
    ptr: int64;
    size: int64;
  end;

  TELF32ResourceInfo = packed record
    reshash: longint; // always 32bit, contains an ELF hash of the resource entries name
    restype: longint; // always 32bit, contains the resource type ID compatible with Windows RES IDs
    ptr:     longint; // Byte offset to the resource inside the resdata section.
    name:    longint; // Byte offset to the the resource name inside the ressym section.
    size:    longint; // The size of the resource entry
  end;

  TELF64ResourceInfo = packed record
    reshash: longint; // always 32bit, contains an ELF hash of the resource entries name
    restype: longint; // always 32bit, contains the resource type ID compatible with Windows RES IDs
    ptr:     int64;   // Byte offset to the resource inside the resdata section.
    name:    int64;   // Byte offset to the the resource name inside the ressym section.
    size:    int64;   // The size of the resource entry
  end;

  TELF32ResourceSectionTable = packed record
    version: integer;
    resentries: integer;
    ressym: TELF32ResourceSectionInfo;
    reshash: TELF32ResourceSectionInfo;
    resdata: TELF32ResourceSectionInfo;
    resspare: TELF32ResourceSectionInfo;
    resstr: TELF32ResourceSectionInfo;
  end;

implementation

end.

