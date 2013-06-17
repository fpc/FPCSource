{
    Copyright (c) 1998-2006 by Peter Vreman

    Contains ELF definitions

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit elfbase;

{$i fpcdefs.inc}

interface

  const
    EI_MAG0    = 0;
      ELFMAG0  = $7F;
    EI_MAG1    = 1;
      ELFMAG1  = ord('E');
    EI_MAG2    = 2;
      ELFMAG2  = ord('L');
    EI_MAG3    = 3;
      ELFMAG3  = ord('F');
    EI_CLASS   = 4;
      ELFCLASSNONE  = 0;
      ELFCLASS32    = 1;
      ELFCLASS64    = 2;
    EI_DATA    = 5;
      ELFDATANONE   = 0;
      ELFDATA2LSB   = 1;
      ELFDATA2MSB   = 2;
    EI_VERSION = 6;
    EI_OSABI   = 7;
      ELFOSABI_NONE    = 0;  { UNIX System V ABI }
      ELFOSABI_HPUX    = 1;  { HP-UX operating system }
      ELFOSABI_NETBSD  = 2;  { NetBSD }
      ELFOSABI_LINUX   = 3;  { GNU/Linux }
      ELFOSABI_HURD    = 4;  { GNU/Hurd }
      ELFOSABI_SOLARIS = 6;  { Solaris }
      ELFOSABI_AIX     = 7;  { AIX }
      ELFOSABI_IRIX    = 8;  { IRIX }
      ELFOSABI_FREEBSD = 9;  { FreeBSD }
      ELFOSABI_TRU64   = 10; { TRU64 UNIX }
      ELFOSABI_MODESTO = 11; { Novell Modesto }
      ELFOSABI_OPENBSD = 12; { OpenBSD }
      ELFOSABI_OPENVMS = 13; { OpenVMS }
      ELFOSABI_NSK     = 14; { Hewlett-Packard Non-Stop Kernel }
      ELFOSABI_AROS    = 15; { AROS }
      ELFOSABI_FENIXOS = 16; { FenixOS }
      ELFOSABI_C6000_ELFABI = 64; { Bare-metal TMS320C6000 }
      ELFOSABI_C6000_LINUX  = 65; { Linux TMS320C6000 }
      ELFOSABI_ARM     = 97; { ARM }
      ELFOSABI_STANDALONE = 255; { Standalone (embedded) application }

    EI_ABIVERSION = 8;
    EI_PAD     = 9;

    { ELFHeader.e_type }
    ET_NONE       = 0;
    ET_REL        = 1;
    ET_EXEC       = 2;
    ET_DYN        = 3;
    ET_CORE       = 4;

    { ELFHeader.e_machine }
    EM_SPARC      = 2;
    EM_386        = 3;
    EM_M68K       = 4;
    EM_MIPS       = 8;
    EM_PPC        = 20;
    EM_ARM        = 40;
    EM_X86_64     = 62;

    { ElfSechdr.sh_num }
    SHN_UNDEF     = 0;
    SHN_LORESERVE = $FF00;
    SHN_ABS       = $fff1;
    SHN_COMMON    = $fff2;

    { ElfSechdr.sh_type }
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
    SHT_INIT_ARRAY = 14;
    SHT_FINI_ARRAY = 15;
    SHT_PREINIT_ARRAY = 16;
    SHT_GROUP    = 17;
    SHT_SYMTAB_SHNDX = 18;
    SHT_GNU_ATTRIBUTES = $6ffffff5;
    SHT_GNU_HASH = $6ffffff6;
    SHT_GNU_LIBLIST = $6ffffff7;
    SHT_GNU_verdef = $6ffffffd;
    SHT_GNU_verneed = $6ffffffe;
    SHT_GNU_versym = $6fffffff;

    { ElfSechdr.sh_flags }
    SHF_WRITE     = 1;
    SHF_ALLOC     = 2;
    SHF_EXECINSTR = 4;
    SHF_MERGE     = 16;
    SHF_STRINGS   = 32;
    SHF_INFO_LINK = 64;
    SHF_LINK_ORDER = 128;
    SHF_OS_NONCONFORMING = 256;
    SHF_GROUP     = 512;
    SHF_TLS       = 1024;

    STB_LOCAL   = 0;
    STB_GLOBAL  = 1;
    STB_WEAK    = 2;

    STT_NOTYPE  = 0;
    STT_OBJECT  = 1;
    STT_FUNC    = 2;
    STT_SECTION = 3;
    STT_FILE    = 4;
    STT_COMMON  = 5;
    STT_TLS     = 6;
    STT_GNU_IFUNC = 10;

    { program header types }
    PT_NULL     = 0;
    PT_LOAD     = 1;
    PT_DYNAMIC  = 2;
    PT_INTERP   = 3;
    PT_NOTE     = 4;
    PT_SHLIB    = 5;
    PT_PHDR     = 6;
    PT_TLS      = 7;
    PT_LOOS     = $60000000;
    PT_HIOS     = $6FFFFFFF;
    PT_LOPROC   = $70000000;
    PT_HIPROC   = $7FFFFFFF;
    PT_GNU_EH_FRAME = PT_LOOS + $474e550;   { Frame unwind information }
    PT_GNU_STACK = PT_LOOS + $474e551;      { Stack flags }
    PT_GNU_RELRO = PT_LOOS + $474e552;      { Read-only after relocation }

    { program header flags }
    PF_X = 1;
    PF_W = 2;
    PF_R = 4;
    PF_MASKOS   = $0FF00000;   { OS-specific reserved bits }
    PF_MASKPROC = $F0000000;   { Processor-specific reserved bits }

    { .dynamic tags  }
    DT_NULL     = 0;
    DT_NEEDED   = 1;
    DT_PLTRELSZ = 2;
    DT_PLTGOT   = 3;
    DT_HASH     = 4;
    DT_STRTAB   = 5;
    DT_SYMTAB   = 6;
    DT_RELA     = 7;
    DT_RELASZ   = 8;
    DT_RELAENT  = 9;
    DT_STRSZ    = 10;
    DT_SYMENT   = 11;
    DT_INIT     = 12;
    DT_FINI     = 13;
    DT_SONAME   = 14;
    DT_RPATH    = 15;
    DT_SYMBOLIC = 16;
    DT_REL      = 17;
    DT_RELSZ    = 18;
    DT_RELENT   = 19;
    DT_PLTREL   = 20;
    DT_DEBUG    = 21;
    DT_TEXTREL  = 22;
    DT_JMPREL   = 23;
    DT_BIND_NOW = 24;
    DT_INIT_ARRAY = 25;
    DT_FINI_ARRAY = 26;
    DT_INIT_ARRAYSZ = 27;
    DT_FINI_ARRAYSZ = 28;
    DT_RUNPATH  = 29;
    DT_FLAGS    = 30;
    DT_ENCODING = 32;
    DT_PREINIT_ARRAY   = 32;
    DT_PREINIT_ARRAYSZ = 33;
    DT_NUM      = 34;
    DT_LOOS     = $6000000D;
    DT_HIOS     = $6ffff000;
    DT_LOPROC   = $70000000;
    DT_HIPROC   = $7fffffff;

    DT_RELACOUNT = $6ffffff9;
    DT_RELCOUNT  = $6ffffffa;
    DT_FLAGS_1   = $6ffffffb;
    DT_VERDEF    = $6ffffffc;
    DT_VERDEFNUM = $6ffffffd;
    DT_VERNEED   = $6ffffffe;
    DT_VERNEEDNUM = $6fffffff;
    { GNU extension to Solaris versioning scheme }
    DT_VERSYM     = $6ffffff0;

    GRP_COMDAT = 1;


    { DT_FLAGS }
    DF_ORIGIN     = 1;
    DF_SYMBOLIC   = 2;         // supersedes DT_SYMBOLIC
    DF_TEXTREL    = 4;         // supersedes DT_TEXTREL
    DF_BIND_NOW   = 8;         // supersedes DT_BIND_NOW
    DF_STATIC_TLS = 16;

    { DT_FLAGS_1 }
    DF_1_NOW       = $01;
    DF_1_GLOBAL    = $02;
    DF_1_GROUP     = $04;
    DF_1_NODELETE  = $08;
    DF_1_LOADFLTR  = $10;
    DF_1_INITFIRST = $20;
    DF_1_NOOPEN    = $40;
    DF_1_ORIGIN    = $80;
    DF_1_DIRECT    = $100;
    DF_1_TRANS     = $200;
    DF_1_INTERPOSE = $400;
    DF_1_NODEFLIB  = $800;
    DF_1_NODUMP    = $1000;
    DF_1_CONFALT   = $2000;

  type
    TElfIdent = array[0..15] of byte;

    { Structures which are written directly to the output file }
    TElf32header=record
      e_ident           : TElfIdent;
      e_type            : word;
      e_machine         : word;
      e_version         : longword;
      e_entry           : longword;         { entrypoint }
      e_phoff           : longword;         { program header offset }
      e_shoff           : longword;         { sections header offset }
      e_flags           : longword;
      e_ehsize          : word;             { elf header size in bytes }
      e_phentsize       : word;             { size of an entry in the program header array }
      e_phnum           : word;             { 0..e_phnum-1 of entrys }
      e_shentsize       : word;             { size of an entry in sections header array }
      e_shnum           : word;             { 0..e_shnum-1 of entrys }
      e_shstrndx        : word;             { index of string section header }
    end;

    TElf32sechdr=record
      sh_name           : longword;
      sh_type           : longword;
      sh_flags          : longword;
      sh_addr           : longword;
      sh_offset         : longword;
      sh_size           : longword;
      sh_link           : longword;
      sh_info           : longword;
      sh_addralign      : longword;
      sh_entsize        : longword;
    end;

    TElf32proghdr=record
      p_type            : longword;
      p_offset          : longword;
      p_vaddr           : longword;
      p_paddr           : longword;
      p_filesz          : longword;
      p_memsz           : longword;
      p_flags           : longword;
      p_align           : longword;
    end;

    TElf32reloc=record
      address : longword;
      info    : longword; { bit 0-7: type, 8-31: symbol }
      addend  : longint;
    end;

    TElf32symbol=record
      st_name  : longword;
      st_value : longword;
      st_size  : longword;
      st_info  : byte; { bit 0-3: type, 4-7: bind }
      st_other : byte;
      st_shndx : word;
    end;

    TElf32Dyn=record
      d_tag: longword;
      case integer of
        0: (d_val: longword);
        1: (d_ptr: longword);
    end;

    telf64header=record
      e_ident           : TElfIdent;
      e_type            : word;
      e_machine         : word;
      e_version         : longword;
      e_entry           : qword;            { entrypoint }
      e_phoff           : qword;            { program header offset }
      e_shoff           : qword;            { sections header offset }
      e_flags           : longword;
      e_ehsize          : word;             { elf header size in bytes }
      e_phentsize       : word;             { size of an entry in the program header array }
      e_phnum           : word;             { 0..e_phnum-1 of entrys }
      e_shentsize       : word;             { size of an entry in sections header array }
      e_shnum           : word;             { 0..e_shnum-1 of entrys }
      e_shstrndx        : word;             { index of string section header }
    end;

    TElf64sechdr=record
      sh_name           : longword;
      sh_type           : longword;
      sh_flags          : qword;
      sh_addr           : qword;
      sh_offset         : qword;
      sh_size           : qword;
      sh_link           : longword;
      sh_info           : longword;
      sh_addralign      : qword;
      sh_entsize        : qword;
    end;

    telf64proghdr=record
      p_type            : longword;
      p_flags           : longword;
      p_offset          : qword;
      p_vaddr           : qword;
      p_paddr           : qword;
      p_filesz          : qword;
      p_memsz           : qword;
      p_align           : qword;
    end;

    telf64reloc=record
      address : qword;
      info    : qword; { bit 0-31: type, 32-63: symbol }
      addend  : int64; { signed! }
    end;

    telf64symbol=record
      st_name  : longword;
      st_info  : byte; { bit 0-3: type, 4-7: bind }
      st_other : byte;
      st_shndx : word;
      st_value : qword;
      st_size  : qword;
    end;

    TElf64Dyn=record
      d_tag: qword;
      case integer of
        0: (d_val: qword);
        1: (d_ptr: qword);
    end;

    { The following records are same for 32 and 64 bit ELF files }
    TElfVerdef=record
      vd_version: word;      { =VER_DEF_CURRENT }
      vd_flags:   word;
      vd_ndx:     word;
      vd_cnt:     word;      { number of verdaux records }
      vd_hash:    longword;  { ELF hash of version name }
      vd_aux:     longword;  { offset to verdaux records }
      vd_next:    longword;  { offset to next verdef record }
    end;

    TElfVerdaux=record
      vda_name: longword;
      vda_next: longword;
    end;

    TElfVerneed=record
      vn_version: word;      { =VER_NEED_CURRENT }
      vn_cnt:     word;
      vn_file:    longword;
      vn_aux:     longword;
      vn_next:    longword;
    end;

    TElfVernaux=record
      vna_hash:  longword;
      vna_flags: word;
      vna_other: word;
      vna_name:  longword;
      vna_next:  longword;
    end;

  const
    VERSYM_VERSION = $7FFF;
    VERSYM_HIDDEN  = $8000;
    VER_NDX_LOCAL = 0;
    VER_NDX_GLOBAL = 1;

    { TElfverdef.vd_version }
    VER_DEF_CURRENT = 1;
    { TElfverneed.vn_version }
    VER_NEED_CURRENT = 1;
    { TElfverdef.vn_flags }
    VER_FLG_BASE = 1;
    VER_FLG_WEAK = 2;
    VER_FLG_INFO = 4;


implementation

end.
