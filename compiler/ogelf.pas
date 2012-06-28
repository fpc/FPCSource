{
    Copyright (c) 1998-2006 by Peter Vreman

    Contains the binary elf writer

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
unit ogelf;

{$i fpcdefs.inc}

interface

    uses
       { common }
       cclasses,globtype,
       { target }
       systems,
       { assembler }
       cpuinfo,cpubase,aasmbase,aasmtai,aasmdata,assemble,
       { output }
       ogbase,
       owbase;

    type
       TElfObjSection = class(TObjSection)
       public
          secshidx  : longint; { index for the section in symtab }
          shstridx,
          shtype,
          shflags,
          shlink,
          shinfo,
          shentsize : longint;
          constructor create(AList:TFPHashObjectList;const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);override;
          constructor create_ext(aobjdata:TObjData;const Aname:string;Ashtype,Ashflags,Ashlink,Ashinfo:longint;Aalign:shortint;Aentsize:longint);
       end;

       TElfSymtabKind = (esk_obj,esk_exe,esk_dyn);

       TElfSymtab = class(TElfObjSection)
       public
         kind: TElfSymtabKind;
         fstrsec: TObjSection;
         symidx: longint;
         constructor create(aObjData:TObjData;aKind:TElfSymtabKind);reintroduce;
         procedure writeSymbol(objsym:TObjSymbol);
         procedure writeInternalSymbol(astridx:longint;ainfo:byte;ashndx:word);
       end;

       TElfObjData = class(TObjData)
       public
         constructor create(const n:string);override;
         function  sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;override;
         procedure CreateDebugSections;override;
         procedure writereloc(data:aint;len:aword;p:TObjSymbol;reltype:TObjRelocationType);override;
       end;

       TElfObjectOutput = class(tObjOutput)
       private
         symtabsect: TElfSymtab;
         shstrtabsect: TElfObjSection;
         procedure createrelocsection(s:TElfObjSection;data:TObjData);
         procedure createshstrtab(data:TObjData);
         procedure createsymtab(data: TObjData);
         procedure writesectionheader(s:TElfObjSection);
         procedure section_write_symbol(p:TObject;arg:pointer);
         procedure section_write_sh_string(p:TObject;arg:pointer);
         procedure section_count_sections(p:TObject;arg:pointer);
         procedure section_create_relocsec(p:TObject;arg:pointer);
         procedure section_write_sechdr(p:TObject;arg:pointer);
       protected
         function writedata(data:TObjData):boolean;override;
       public
         constructor Create(AWriter:TObjectWriter);override;
       end;

       TElfAssembler = class(tinternalassembler)
         constructor create(smart:boolean);override;
       end;


implementation

      uses
        SysUtils,
        verbose,
        cutils,globals,fmodule;

    const
      symbolresize = 200*18;

    const
      { Relocation types }
{$ifdef i386}
      R_386_32 = 1;                    { ordinary absolute relocation }
      R_386_PC32 = 2;                  { PC-relative relocation }
      R_386_GOT32 = 3;                 { an offset into GOT }
      R_386_PLT32 = 4;                 { a PC-relative offset into PLT }
      R_386_GOTOFF = 9;                { an offset from GOT base }
      R_386_GOTPC = 10;                { a PC-relative offset _to_ GOT }
      R_386_GNU_VTINHERIT = 250;
      R_386_GNU_VTENTRY = 251;
{$endif i386}
{$ifdef sparc}
      R_SPARC_32 = 3;
      R_SPARC_WDISP30 = 7;
      R_SPARC_HI22 = 9;
      R_SPARC_LO10 = 12;
      R_SPARC_GNU_VTINHERIT = 250;
      R_SPARC_GNU_VTENTRY = 251;
{$endif sparc}
{$ifdef x86_64}
      R_X86_64_NONE = 0;
      R_X86_64_64 = 1;                 { Direct 64 bit   }
      R_X86_64_PC32 = 2;               { PC relative 32 bit signed  }
      R_X86_64_GOT32 = 3;              { 32 bit GOT entry  }
      R_X86_64_PLT32 = 4;              { 32 bit PLT address  }
      R_X86_64_COPY = 5;               { Copy symbol at runtime  }
      R_X86_64_GLOB_DAT = 6;           { Create GOT entry  }
      R_X86_64_JUMP_SLOT = 7;          { Create PLT entry  }
      R_X86_64_RELATIVE = 8;           { Adjust by program base  }
      R_X86_64_GOTPCREL = 9;           { 32 bit signed PC relative offset to GOT  }
      R_X86_64_32 = 10;                { Direct 32 bit zero extended  }
      R_X86_64_32S = 11;               { Direct 32 bit sign extended  }
      R_X86_64_16 = 12;                { Direct 16 bit zero extended  }
      R_X86_64_PC16 = 13;              { 16 bit sign extended PC relative  }
      R_X86_64_8 = 14;                 { Direct 8 bit sign extended   }
      R_X86_64_PC8 = 15;               { 8 bit sign extended PC relative  }
      R_X86_64_DTPMOD64 = 16;          { ID of module containing symbol  }
      R_X86_64_DTPOFF64 = 17;          { Offset in module's TLS block  }
      R_X86_64_TPOFF64 = 18;           { Offset in initial TLS block  }
      { 32 bit signed PC relative offset to two GOT entries for GD symbol  }
      R_X86_64_TLSGD = 19;
      { 32 bit signed PC relative offset to two GOT entries for LD symbol  }
      R_X86_64_TLSLD = 20;
      R_X86_64_DTPOFF32 = 21;          { Offset in TLS block  }
      { 32 bit signed PC relative offset to GOT entry for IE symbol  }
      R_X86_64_GOTTPOFF = 22;
      R_X86_64_TPOFF32 = 23;           { Offset in initial TLS block  }
      R_X86_64_PC64 = 24;              { PC relative 64-bit signed }
      R_X86_64_GOTOFF64 = 25;          { 64-bit offset from GOT base }
      R_X86_64_GOTPC32 = 26;           { PC-relative offset GOT }
      R_X86_64_GOT64  = 27;            { 64-bit GOT entry offset }
      R_X86_64_GOTPCREL64 = 28;        { 64-bit PC relative offset to GOT entry }
      R_X86_64_GOTPC64 = 29;           { 64-bit PC relative offset to GOT }
      R_X86_64_GOTPLT64 = 30;          { Like GOT64, indicates that PLT entry needed }
      R_X86_64_PLTOFF64 = 31;          { 64-bit GOT relative offset to PLT entry }
      R_X86_64_SIZE32 = 32;
      R_X86_64_SIZE64 = 33;
      R_X86_64_GOTPC32_TLSDESC = 34;
      R_X86_64_TLSDESC_CALL = 35;
      R_X86_64_TLSDESC = 36;
      R_X86_64_IRELATIVE = 37;
      R_X86_64_GNU_VTINHERIT = 250;    { GNU extension to record C++ vtable hierarchy }
      R_X86_64_GNU_VTENTRY = 251;      { GNU extension to record C++ vtable member usage }
{$endif x86_64}

      { ELFHeader.file_class }
      ELFCLASSNONE  = 0;
      ELFCLASS32    = 1;
      ELFCLASS64    = 2;

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
      EM_PPC        = 20;
      EM_ARM        = 40;
      EM_X86_64     = 62;

{$ifdef sparc}
      ELFMACHINE = EM_SPARC;
{$endif sparc}
{$ifdef i386}
      ELFMACHINE = EM_386;
{$endif i386}
{$ifdef m68k}
      ELFMACHINE = EM_M68K;
{$endif m68k}
{$ifdef powerpc}
      ELFMACHINE = EM_PPC;
{$endif powerpc}
{$ifdef arm}
      ELFMACHINE = EM_ARM;
{$endif arm}
{$ifdef x86_64}
      ELFMACHINE = EM_X86_64;
{$endif x86_64}

      SHN_UNDEF     = 0;
      SHN_ABS       = $fff1;
      SHN_COMMON    = $fff2;

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

      STB_LOCAL   = 0;
      STB_GLOBAL  = 1;
      STB_WEAK    = 2;

      STT_NOTYPE  = 0;
      STT_OBJECT  = 1;
      STT_FUNC    = 2;
      STT_SECTION = 3;
      STT_FILE    = 4;

      { program header types }
      PT_NULL     = 0;
      PT_LOAD     = 1;
      PT_DYNAMIC  = 2;
      PT_INTERP   = 3;
      PT_NOTE     = 4;
      PT_SHLIB    = 5;
      PT_PHDR     = 6;
      PT_LOPROC   = $70000000;
      PT_HIPROC   = $7FFFFFFF;

      { program header flags }
      PF_X = 1;
      PF_W = 2;
      PF_R = 4;
      PF_MASKPROC = $F0000000;

      { .dynamic tags  }
      DT_NULL     = 0;
      DT_NEEDED   = 1;
      DT_PLTRELSZ = 2;
      DT_PLTGOT   = 3;
      DT_HASH     = 4;
      DT_STRTAB   = 5;
      DT_SYMTAB	  = 6;
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

      type
      { Structures which are written directly to the output file }
        TElf32header=packed record
          magic             : array[0..3] of byte;
          file_class        : byte;
          data_encoding     : byte;
          file_version      : byte;
          padding           : array[$07..$0f] of byte;
          e_type            : word;
          e_machine         : word;
          e_version         : longint;
          e_entry           : longint;          { entrypoint }
          e_phoff           : longint;          { program header offset }
          e_shoff           : longint;          { sections header offset }
          e_flags           : longint;
          e_ehsize          : word;             { elf header size in bytes }
          e_phentsize       : word;             { size of an entry in the program header array }
          e_phnum           : word;             { 0..e_phnum-1 of entrys }
          e_shentsize       : word;             { size of an entry in sections header array }
          e_shnum           : word;             { 0..e_shnum-1 of entrys }
          e_shstrndx        : word;             { index of string section header }
        end;
        TElf32sechdr=packed record
          sh_name           : longint;
          sh_type           : longint;
          sh_flags          : longint;
          sh_addr           : longint;
          sh_offset         : longint;
          sh_size           : longint;
          sh_link           : longint;
          sh_info           : longint;
          sh_addralign      : longint;
          sh_entsize        : longint;
        end;
        TElf32proghdr=packed record
          p_type            : longword;
          p_offset          : longword;
          p_vaddr           : longword;
          p_paddr           : longword;
          p_filesz          : longword;
          p_memsz           : longword;
          p_flags           : longword;
          p_align           : longword;
        end;
        TElf32reloc=packed record
          address : longint;
          info    : longint; { bit 0-7: type, 8-31: symbol }
          addend  : longint;
        end;
        TElf32symbol=packed record
          st_name  : longint;
          st_value : longint;
          st_size  : longint;
          st_info  : byte; { bit 0-3: type, 4-7: bind }
          st_other : byte;
          st_shndx : word;
        end;
        TElf32Dyn=packed record
          d_tag: longword;
          case integer of
            0: (d_val: longword);
            1: (d_ptr: longword);
        end;


        telf64header=packed record
          magic             : array[0..3] of byte;
          file_class        : byte;
          data_encoding     : byte;
          file_version      : byte;
          padding           : array[$07..$0f] of byte;
          e_type            : word;
          e_machine         : word;
          e_version         : longint;
          e_entry           : qword;            { entrypoint }
          e_phoff           : qword;            { program header offset }
          e_shoff           : qword;            { sections header offset }
          e_flags           : longint;
          e_ehsize          : word;             { elf header size in bytes }
          e_phentsize       : word;             { size of an entry in the program header array }
          e_phnum           : word;             { 0..e_phnum-1 of entrys }
          e_shentsize       : word;             { size of an entry in sections header array }
          e_shnum           : word;             { 0..e_shnum-1 of entrys }
          e_shstrndx        : word;             { index of string section header }
        end;
        telf64sechdr=packed record
          sh_name           : longint;
          sh_type           : longint;
          sh_flags          : qword;
          sh_addr           : qword;
          sh_offset         : qword;
          sh_size           : qword;
          sh_link           : longint;
          sh_info           : longint;
          sh_addralign      : qword;
          sh_entsize        : qword;
        end;
        telf64proghdr=packed record
          p_type            : longword;
          p_flags           : longword;
          p_offset          : qword;
          p_vaddr           : qword;
          p_paddr           : qword;
          p_filesz          : qword;
          p_memsz           : qword;
          p_align           : qword;
        end;
        telf64reloc=packed record
          address : qword;
          info    : qword; { bit 0-31: type, 32-63: symbol }
          addend  : int64; { signed! }
        end;
        telf64symbol=packed record
          st_name  : longint;
          st_info  : byte; { bit 0-3: type, 4-7: bind }
          st_other : byte;
          st_shndx : word;
          st_value : qword;
          st_size  : qword;
        end;
        TElf64Dyn=packed record
          d_tag: qword;
          case integer of
            0: (d_val: qword);
            1: (d_ptr: qword);
        end;


{$ifdef cpu64bitaddr}
      const
        ELFCLASS = ELFCLASS64;
      type
        telfheader = telf64header;
        telfreloc = telf64reloc;
        telfsymbol = telf64symbol;
        telfsechdr = telf64sechdr;
        telfproghdr = telf64proghdr;
        telfdyn = telf64dyn;
{$else cpu64bitaddr}
      const
        ELFCLASS = ELFCLASS32;
      type
        telfheader = telf32header;
        telfreloc = telf32reloc;
        telfsymbol = telf32symbol;
        telfsechdr = telf32sechdr;
        telfproghdr = telf32proghdr;
        telfdyn = telf32dyn;
{$endif cpu64bitaddr}

{$ifdef x86_64}
      const
        relocs_use_addend:Boolean=True;
{$else x86_64}
      const
        relocs_use_addend:Boolean=False;
{$endif x86_64}

      procedure MayBeSwapHeader(var h : telf32header);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                e_type:=swapendian(e_type);
                e_machine:=swapendian(e_machine);
                e_version:=swapendian(e_version);
                e_entry:=swapendian(e_entry);
                e_phoff:=swapendian(e_phoff);
                e_shoff:=swapendian(e_shoff);
                e_flags:=swapendian(e_flags);
                e_ehsize:=swapendian(e_ehsize);
                e_phentsize:=swapendian(e_phentsize);
                e_phnum:=swapendian(e_phnum);
                e_shentsize:=swapendian(e_shentsize);
                e_shnum:=swapendian(e_shnum);
                e_shstrndx:=swapendian(e_shstrndx);
              end;
        end;


      procedure MayBeSwapHeader(var h : telf64header);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                e_type:=swapendian(e_type);
                e_machine:=swapendian(e_machine);
                e_version:=swapendian(e_version);
                e_entry:=swapendian(e_entry);
                e_phoff:=swapendian(e_phoff);
                e_shoff:=swapendian(e_shoff);
                e_flags:=swapendian(e_flags);
                e_ehsize:=swapendian(e_ehsize);
                e_phentsize:=swapendian(e_phentsize);
                e_phnum:=swapendian(e_phnum);
                e_shentsize:=swapendian(e_shentsize);
                e_shnum:=swapendian(e_shnum);
                e_shstrndx:=swapendian(e_shstrndx);
              end;
        end;


      procedure MayBeSwapHeader(var h : telf32proghdr);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                p_align:=swapendian(p_align);
                p_filesz:=swapendian(p_filesz);
                p_flags:=swapendian(p_flags);
                p_memsz:=swapendian(p_memsz);
                p_offset:=swapendian(p_offset);
                p_paddr:=swapendian(p_paddr);
                p_type:=swapendian(p_type);
                p_vaddr:=swapendian(p_vaddr);
              end;
        end;


      procedure MayBeSwapHeader(var h : telf64proghdr);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                p_align:=swapendian(p_align);
                p_filesz:=swapendian(p_filesz);
                p_flags:=swapendian(p_flags);
                p_memsz:=swapendian(p_memsz);
                p_offset:=swapendian(p_offset);
                p_paddr:=swapendian(p_paddr);
                p_type:=swapendian(p_type);
                p_vaddr:=swapendian(p_vaddr);
              end;
        end;


      procedure MaybeSwapSecHeader(var h : telf32sechdr);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                sh_name:=swapendian(sh_name);
                sh_type:=swapendian(sh_type);
                sh_flags:=swapendian(sh_flags);
                sh_addr:=swapendian(sh_addr);
                sh_offset:=swapendian(sh_offset);
                sh_size:=swapendian(sh_size);
                sh_link:=swapendian(sh_link);
                sh_info:=swapendian(sh_info);
                sh_addralign:=swapendian(sh_addralign);
                sh_entsize:=swapendian(sh_entsize);
              end;
        end;


      procedure MaybeSwapSecHeader(var h : telf64sechdr);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                sh_name:=swapendian(sh_name);
                sh_type:=swapendian(sh_type);
                sh_flags:=swapendian(sh_flags);
                sh_addr:=swapendian(sh_addr);
                sh_offset:=swapendian(sh_offset);
                sh_size:=swapendian(sh_size);
                sh_link:=swapendian(sh_link);
                sh_info:=swapendian(sh_info);
                sh_addralign:=swapendian(sh_addralign);
                sh_entsize:=swapendian(sh_entsize);
              end;
        end;


      procedure MaybeSwapElfSymbol(var h : telf32symbol);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                st_name:=swapendian(st_name);
                st_value:=swapendian(st_value);
                st_size:=swapendian(st_size);
                st_shndx:=swapendian(st_shndx);
              end;
        end;


      procedure MaybeSwapElfSymbol(var h : telf64symbol);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                st_name:=swapendian(st_name);
                st_value:=swapendian(st_value);
                st_size:=swapendian(st_size);
                st_shndx:=swapendian(st_shndx);
              end;
        end;


      procedure MaybeSwapElfReloc(var h : telf32reloc);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                address:=swapendian(address);
                info:=swapendian(info);
                addend:=swapendian(addend);
              end;
        end;


      procedure MaybeSwapElfReloc(var h : telf64reloc);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                address:=swapendian(address);
                info:=swapendian(info);
                addend:=swapendian(addend);
              end;
        end;


      procedure MaybeSwapElfDyn(var h : telf32dyn);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                d_tag:=swapendian(d_tag);
                d_val:=swapendian(d_val);
              end;
        end;


      procedure MaybeSwapElfDyn(var h : telf64dyn);
        begin
          if source_info.endian<>target_info.endian then
            with h do
              begin
                d_tag:=swapendian(d_tag);
                d_val:=swapendian(d_val);
              end;
        end;


{****************************************************************************
                                Helpers
****************************************************************************}

    procedure encodesechdrflags(aoptions:TObjSectionOptions;out AshType:longint;out Ashflags:longint);
      begin
        { Section Type }
        AshType:=SHT_PROGBITS;
        if oso_strings in aoptions then
          AshType:=SHT_STRTAB
        else if not(oso_data in aoptions) then
          AshType:=SHT_NOBITS;
        { Section Flags }
        Ashflags:=0;
        if oso_load in aoptions then
          Ashflags:=Ashflags or SHF_ALLOC;
        if oso_executable in aoptions then
          Ashflags:=Ashflags or SHF_EXECINSTR;
        if oso_write in aoptions then
          Ashflags:=Ashflags or SHF_WRITE;
      end;


    procedure decodesechdrflags(AshType:longint;Ashflags:longint;out aoptions:TObjSectionOptions);
      begin
        aoptions:=[];
        { Section Type }
        if AshType<>SHT_NOBITS then
          include(aoptions,oso_data);
        if AshType=SHT_STRTAB then
          include(aoptions,oso_strings);
        { Section Flags }
        if Ashflags and SHF_ALLOC<>0 then
          include(aoptions,oso_load)
        else
          include(aoptions,oso_noload);
        if Ashflags and SHF_WRITE<>0 then
          include(aoptions,oso_write)
        else
          include(aoptions,oso_readonly);
        if Ashflags and SHF_EXECINSTR<>0 then
          include(aoptions,oso_executable);
      end;


{****************************************************************************
                               TElfObjSection
****************************************************************************}

    constructor TElfObjSection.create(AList:TFPHashObjectList;const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);
      begin
        inherited create(AList,Aname,Aalign,aoptions);
        secshidx:=0;
        shstridx:=0;
        encodesechdrflags(aoptions,shtype,shflags);
        shlink:=0;
        shinfo:=0;
        if name='.stab' then
          shentsize:=sizeof(TObjStabEntry);
      end;


    constructor TElfObjSection.create_ext(aobjdata:TObjData;const Aname:string;Ashtype,Ashflags,Ashlink,Ashinfo:longint;Aalign:shortint;Aentsize:longint);
      var
        aoptions : TObjSectionOptions;
      begin
        decodesechdrflags(Ashtype,Ashflags,aoptions);
        inherited create(aobjdata.ObjSectionList,Aname,Aalign,aoptions);
        objdata:=aobjdata;
        secshidx:=0;
        shstridx:=0;
        shtype:=AshType;
        shflags:=AshFlags;
        shlink:=Ashlink;
        shinfo:=Ashinfo;
        shentsize:=Aentsize;
      end;


{****************************************************************************
                            TElfObjData
****************************************************************************}

    constructor TElfObjData.create(const n:string);
      begin
        inherited create(n);
        CObjSection:=TElfObjSection;
        { we need at least the following sections }
        createsection(sec_code);
        { always a non-PIC data section (will remain empty if doing PIC) }
        createsection('.data',sectiontype2align(sec_data),sectiontype2options(sec_data));
        createsection(sec_bss);
        if (cs_create_pic in current_settings.moduleswitches) and
           not(target_info.system in systems_darwin) then
          createsection(sec_data);
        if tf_section_threadvars in target_info.flags then
          createsection(sec_threadvar);
        if (tf_needs_dwarf_cfi in target_info.flags) and
           (af_supports_dwarf in target_asm.flags) then
             createsection(sec_debug_frame);
      end;


    function TElfObjData.sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;
      const
        secnames : array[TAsmSectiontype] of string[length('__DATA, __datacoal_nt,coalesced')] = ('','',
{$ifdef userodata}
          '.text','.data','.data','.rodata','.bss','.threadvar',
{$else userodata}
          '.text','.data','.data','.data','.bss','.threadvar',
{$endif userodata}
          '.pdata',
          '.text', { darwin stubs }
          '__DATA,__nl_symbol_ptr',
          '__DATA,__la_symbol_ptr',
          '__DATA,__mod_init_func',
          '__DATA,__mod_term_func',
          '.stab','.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame','.debug_info','.debug_line','.debug_abbrev',
          '.fpc',
          '.toc',
          '.init',
          '.fini',
          '.objc_class',
          '.objc_meta_class',
          '.objc_cat_cls_meth',
          '.objc_cat_inst_meth',
          '.objc_protocol',
          '.objc_string_object',
          '.objc_cls_meth',
          '.objc_inst_meth',
          '.objc_cls_refs',
          '.objc_message_refs',
          '.objc_symbols',
          '.objc_category',
          '.objc_class_vars',
          '.objc_instance_vars',
          '.objc_module_info',
          '.objc_class_names',
          '.objc_meth_var_types',
          '.objc_meth_var_names',
          '.objc_selector_strs',
          '.objc_protocol_ext',
          '.objc_class_ext',
          '.objc_property',
          '.objc_image_info',
          '.objc_cstring_object',
          '.objc_sel_fixup',
          '__DATA,__objc_data',
          '__DATA,__objc_const',
          '.objc_superrefs',
          '__DATA, __datacoal_nt,coalesced',
          '.objc_classlist',
          '.objc_nlclasslist',
          '.objc_catlist',
          '.obcj_nlcatlist',
          '.objc_protolist'
        );
        secnames_pic : array[TAsmSectiontype] of string[length('__DATA, __datacoal_nt,coalesced')] = ('','',
          '.text',
          '.data.rel',
          '.data.rel',
          '.data.rel',
          '.bss',
          '.threadvar',
          '.pdata',
          '', { stubs }
          '__DATA,__nl_symbol_ptr',
          '__DATA,__la_symbol_ptr',
          '__DATA,__mod_init_func',
          '__DATA,__mod_term_func',
          '.stab',
          '.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame','.debug_info','.debug_line','.debug_abbrev',
          '.fpc',
          '.toc',
          '.init',
          '.fini',
          '.objc_class',
          '.objc_meta_class',
          '.objc_cat_cls_meth',
          '.objc_cat_inst_meth',
          '.objc_protocol',
          '.objc_string_object',
          '.objc_cls_meth',
          '.objc_inst_meth',
          '.objc_cls_refs',
          '.objc_message_refs',
          '.objc_symbols',
          '.objc_category',
          '.objc_class_vars',
          '.objc_instance_vars',
          '.objc_module_info',
          '.objc_class_names',
          '.objc_meth_var_types',
          '.objc_meth_var_names',
          '.objc_selector_strs',
          '.objc_protocol_ext',
          '.objc_class_ext',
          '.objc_property',
          '.objc_image_info',
          '.objc_cstring_object',
          '.objc_sel_fixup',
          '__DATA,__objc_data',
          '__DATA,__objc_const',
          '.objc_superrefs',
          '__DATA, __datacoal_nt,coalesced',
          '.objc_classlist',
          '.objc_nlclasslist',
          '.objc_catlist',
          '.obcj_nlcatlist',
          '.objc_protolist'
        );
      var
        sep : string[3];
        secname : string;
      begin
        { section type user gives the user full controll on the section name }
        if atype=sec_user then
          result:=aname
        else
          begin
            if (cs_create_pic in current_settings.moduleswitches) and
               not(target_info.system in systems_darwin) then
              secname:=secnames_pic[atype]
            else
              secname:=secnames[atype];
            if (atype=sec_fpc) and (Copy(aname,1,3)='res') then
              begin
                result:=secname+'.'+aname;
                exit;
              end;
            if create_smartlink_sections and (aname<>'') then
              begin
                case aorder of
                  secorder_begin :
                    sep:='.b_';
                  secorder_end :
                    sep:='.z_';
                  else
                    sep:='.n_';
                end;
                result:=secname+sep+aname
              end
            else
              result:=secname;
          end;
      end;


    procedure TElfObjData.CreateDebugSections;
      begin
        if target_dbg.id=dbg_stabs then
          begin
            stabssec:=createsection(sec_stab);
            stabstrsec:=createsection(sec_stabstr);
          end;
      end;


    procedure TElfObjData.writereloc(data:aint;len:aword;p:TObjSymbol;reltype:TObjRelocationType);
      var
        symaddr : aint;
        objreloc: TObjRelocation;
      begin
        if CurrObjSec=nil then
          internalerror(200403292);
        objreloc:=nil;
        if assigned(p) then
         begin
           { real address of the symbol }
           symaddr:=p.address;
           { Local ObjSymbols can be resolved already or need a section reloc }
           if (p.bind=AB_LOCAL) and
              (reltype in [RELOC_RELATIVE,RELOC_ABSOLUTE{$ifdef x86_64},RELOC_ABSOLUTE32{$endif x86_64}]) then
             begin
               { For a reltype relocation in the same section the
                 value can be calculated }
               if (p.objsection=CurrObjSec) and
                  (reltype=RELOC_RELATIVE) then
                 inc(data,symaddr-len-CurrObjSec.Size)
               else
                 begin
                   objreloc:=TObjRelocation.CreateSection(CurrObjSec.Size,p.objsection,reltype);
                   CurrObjSec.ObjRelocations.Add(objreloc);
                   inc(data,symaddr);
                 end;
             end
           else
             begin
               objreloc:=TObjRelocation.CreateSymbol(CurrObjSec.Size,p,reltype);
               CurrObjSec.ObjRelocations.Add(objreloc);
            end;
         end;
        if assigned(objreloc) then
          begin
            objreloc.size:=len;
            if reltype in [RELOC_RELATIVE,RELOC_PLT32{$ifdef x86_64},RELOC_GOTPCREL{$endif}] then
              dec(data,len);
            if relocs_use_addend then
              begin
                objreloc.orgsize:=data;
                data:=0;
              end;
          end;
        CurrObjSec.write(data,len);
      end;


{****************************************************************************
                            TElfSymtab
****************************************************************************}

    const
      symsecnames: array[boolean] of string[8] = ('.symtab','.dynsym');
      strsecnames: array[boolean] of string[8] = ('.strtab','.dynstr');
      symsectypes: array[boolean] of longint   = (SHT_SYMTAB,SHT_DYNSYM);
      symsecattrs: array[boolean] of longint   = (0,SHF_ALLOC);


    constructor TElfSymtab.create(aObjData:TObjData;aKind:TElfSymtabKind);
      var
        dyn:boolean;
      begin
        dyn:=(aKind=esk_dyn);
        create_ext(aObjData,symsecnames[dyn],symsectypes[dyn],symsecattrs[dyn],0,0,sizeof(pint),sizeof(TElfSymbol));
        fstrsec:=TElfObjSection.create_ext(aObjData,strsecnames[dyn],SHT_STRTAB,symsecattrs[dyn],0,0,1,0);
        fstrsec.writestr(#0);
        writezeros(sizeof(TElfSymbol));
        symidx:=1;
        shinfo:=1;
        kind:=aKind;
      end;

    procedure TElfSymtab.writeInternalSymbol(astridx:longint;ainfo:byte;ashndx:word);
      var
        elfsym:TElfSymbol;
      begin
        fillchar(elfsym,sizeof(elfsym),0);
        elfsym.st_name:=astridx;
        elfsym.st_info:=ainfo;
        elfsym.st_shndx:=ashndx;
        inc(symidx);
        inc(shinfo);
        MaybeSwapElfSymbol(elfsym);
        write(elfsym,sizeof(elfsym));
      end;

    procedure TElfSymtab.writeSymbol(objsym:TObjSymbol);
      var
        elfsym:TElfSymbol;
      begin
        fillchar(elfsym,sizeof(elfsym),0);
        { symbolname, write the #0 separate to overcome 255+1 char not possible }
        elfsym.st_name:=fstrsec.writestr(objsym.name);
        fstrsec.writestr(#0);
        elfsym.st_size:=objsym.size;
        case objsym.bind of
          AB_LOCAL :
            begin
              elfsym.st_value:=objsym.address;
              elfsym.st_info:=STB_LOCAL shl 4;
              inc(shinfo);
            end;
          AB_COMMON :
            begin
              elfsym.st_value:=$10;            { ?? should not be hardcoded }
              elfsym.st_info:=STB_GLOBAL shl 4;
              elfsym.st_shndx:=SHN_COMMON;
            end;
          AB_EXTERNAL :
            elfsym.st_info:=STB_GLOBAL shl 4;
          AB_WEAK_EXTERNAL :
            elfsym.st_info:=STB_WEAK shl 4;
          AB_GLOBAL :
            begin
              elfsym.st_value:=objsym.address;
              elfsym.st_info:=STB_GLOBAL shl 4;
            end;
        end;
        if (objsym.bind<>AB_EXTERNAL) {and
           not(assigned(objsym.objsection) and
           not(oso_data in objsym.objsection.secoptions))} then
          begin
            case objsym.typ of
              AT_FUNCTION :
                elfsym.st_info:=elfsym.st_info or STT_FUNC;
              AT_DATA :
                elfsym.st_info:=elfsym.st_info or STT_OBJECT;
            end;
          end;
        if objsym.bind<>AB_COMMON then
          begin
            if kind<>esk_obj then
              begin
                { TODO }
              end
            else
              begin
                if assigned(objsym.objsection) then
                  elfsym.st_shndx:=TElfObjSection(objsym.objsection).secshidx
                else
                  elfsym.st_shndx:=SHN_UNDEF;
                objsym.symidx:=symidx;
              end;
          end;
        inc(symidx);
        MaybeSwapElfSymbol(elfsym);
        write(elfsym,sizeof(TElfSymbol));
      end;

{****************************************************************************
                            TElfObjectOutput
****************************************************************************}

    constructor TElfObjectOutput.create(AWriter:TObjectWriter);
      begin
        inherited Create(AWriter);
        CObjData:=TElfObjData;
      end;


    procedure TElfObjectOutput.createrelocsection(s:TElfObjSection;data:TObjData);
      var
        i    : longint;
        rel  : telfreloc;
        objreloc : TObjRelocation;
        relsym,
        reltyp   : longint;
        relocsect : TElfObjSection;
      begin
        with data do
         begin
           { create the reloc section }
           if relocs_use_addend then
             relocsect:=TElfObjSection.create_ext(data,'.rela'+s.name,SHT_RELA,0,symtabsect.secshidx,s.secshidx,4,3*sizeof(pint))
           else
             relocsect:=TElfObjSection.create_ext(data,'.rel'+s.name,SHT_REL,0,symtabsect.secshidx,s.secshidx,4,2*sizeof(pint));
           { add the relocations }
           for i:=0 to s.Objrelocations.count-1 do
             begin
               objreloc:=TObjRelocation(s.Objrelocations[i]);
               fillchar(rel,sizeof(rel),0);
               rel.address:=objreloc.dataoffset;
               rel.addend:=objreloc.orgsize;

               { when things settle down, we can create processor specific
                 derived classes }
               case objreloc.typ of
{$ifdef i386}
                 RELOC_RELATIVE :
                   reltyp:=R_386_PC32;
                 RELOC_ABSOLUTE :
                   reltyp:=R_386_32;
                 RELOC_GOT32 :
                   reltyp:=R_386_GOT32;
                 RELOC_GOTPC :
                   reltyp:=R_386_GOTPC;
                 RELOC_PLT32 :
                   reltyp:=R_386_PLT32;
{$endif i386}
{$ifdef sparc}
                 RELOC_ABSOLUTE :
                   reltyp:=R_SPARC_32;
{$endif sparc}
{$ifdef x86_64}
    { Note: 8 and 16-bit relocations are known to be non-conformant with
      AMD64 ABI, so they aren't handled. }
                 RELOC_RELATIVE :
                   if objreloc.size=8 then
                     reltyp:=R_X86_64_PC64
                   else if objreloc.size=4 then
                     reltyp:=R_X86_64_PC32
                   else
                     InternalError(2012061900);
                 RELOC_ABSOLUTE :
                   if objreloc.size=8 then
                     reltyp:=R_X86_64_64
                   else if objreloc.size=4 then
                     reltyp:=R_X86_64_32
                   else
                     InternalError(2012061901);
                 RELOC_ABSOLUTE32 :
                   reltyp:=R_X86_64_32S;
                 RELOC_GOTPCREL :
                   reltyp:=R_X86_64_GOTPCREL;
                 RELOC_PLT32 :
                   reltyp:=R_X86_64_PLT32;
{$endif x86_64}
                 else
                   internalerror(200602261);
               end;

               { Symbol }
               if assigned(objreloc.symbol) then
                 begin
                   if objreloc.symbol.symidx=-1 then
                     begin
                       writeln(objreloc.symbol.Name);
                       internalerror(200603012);
                     end;
                   relsym:=objreloc.symbol.symidx;
                 end
               else
                 begin
                   if objreloc.objsection<>nil then
                     relsym:=objreloc.objsection.secsymidx
                   else
                     relsym:=SHN_UNDEF;
                 end;
{$ifdef cpu64bitaddr}
               rel.info:=(qword(relsym) shl 32) or reltyp;
{$else cpu64bitaddr}
               rel.info:=(relsym shl 8) or reltyp;
{$endif cpu64bitaddr}
               { write reloc }
               { ElfXX_Rel is essentially ElfXX_Rela without the addend field. }
               MaybeSwapElfReloc(rel);
               relocsect.write(rel,relocsect.shentsize);
             end;
         end;
      end;


    procedure TElfObjectOutput.section_write_symbol(p:TObject;arg:pointer);
      begin
        { Must not write symbols for internal sections like .symtab }
        { TODO: maybe use inclusive list of section types instead }
        if (TElfObjSection(p).shtype in [SHT_SYMTAB,SHT_STRTAB,SHT_REL,SHT_RELA]) then
          exit;
        TObjSection(p).secsymidx:=symtabsect.symidx;
        symtabsect.writeInternalSymbol(0,STT_SECTION,TElfObjSection(p).secshidx);
      end;


    procedure TElfObjectOutput.createsymtab(data: TObjData);
      var
        i      : longint;
        objsym : TObjSymbol;
      begin
        with data do
         begin
           { filename entry }
           symtabsect.writeInternalSymbol(1,STT_FILE,SHN_ABS);
           { section }
           ObjSectionList.ForEachCall(@section_write_symbol,nil);
           { First the Local Symbols, this is required by ELF. The localsyms
             count stored in shinfo is used to skip the local symbols
             when traversing the symtab }
           for i:=0 to ObjSymbolList.Count-1 do
             begin
               objsym:=TObjSymbol(ObjSymbolList[i]);
               if (objsym.bind=AB_LOCAL) and (objsym.typ<>AT_LABEL) then
                 symtabsect.WriteSymbol(objsym);
             end;
           { Global Symbols }
           for i:=0 to ObjSymbolList.Count-1 do
             begin
               objsym:=TObjSymbol(ObjSymbolList[i]);
               if (objsym.bind<>AB_LOCAL) then
                 symtabsect.WriteSymbol(objsym);
             end;
           { update the .symtab section header }
           symtabsect.shlink:=TElfObjSection(symtabsect.fstrsec).secshidx;
         end;
      end;


    procedure TElfObjectOutput.section_write_sh_string(p:TObject;arg:pointer);
      begin
        TElfObjSection(p).shstridx:=shstrtabsect.writestr(TObjSection(p).name+#0);
      end;


    procedure TElfObjectOutput.createshstrtab(data: TObjData);
      begin
        with data do
         begin
           shstrtabsect.writestr(#0);
           ObjSectionList.ForEachCall(@section_write_sh_string,nil);
         end;
      end;


    procedure TElfObjectOutput.writesectionheader(s:TElfObjSection);
      var
        sechdr : telfsechdr;
      begin
        fillchar(sechdr,sizeof(sechdr),0);
        sechdr.sh_name:=s.shstridx;
        sechdr.sh_type:=s.shtype;
        sechdr.sh_flags:=s.shflags;
        sechdr.sh_offset:=s.datapos;
        sechdr.sh_size:=s.Size;
        sechdr.sh_link:=s.shlink;
        sechdr.sh_info:=s.shinfo;
        sechdr.sh_addralign:=s.secalign;
        sechdr.sh_entsize:=s.shentsize;
        MaybeSwapSecHeader(sechdr);
        writer.write(sechdr,sizeof(sechdr));
      end;


    procedure TElfObjectOutput.section_count_sections(p:TObject;arg:pointer);
      begin
        TElfObjSection(p).secshidx:=pword(arg)^;
        inc(pword(arg)^);
      end;


    procedure TElfObjectOutput.section_create_relocsec(p:TObject;arg:pointer);
      begin
        if (TElfObjSection(p).ObjRelocations.count>0) then
          createrelocsection(TElfObjSection(p),TObjData(arg));
      end;


    procedure TElfObjectOutput.section_write_sechdr(p:TObject;arg:pointer);
      begin
        writesectionheader(TElfObjSection(p));
      end;


    function TElfObjectOutput.writedata(data:TObjData):boolean;
      var
        header : telfheader;
        shoffset,
        datapos   : aword;
        nsections : word;
      begin
        result:=false;
        with data do
         begin
           { default sections }
           symtabsect:=TElfSymtab.create(data,esk_obj);
           shstrtabsect:=TElfObjSection.create_ext(data,'.shstrtab',SHT_STRTAB,0,0,0,1,0);
           { "no executable stack" marker for Linux }
           if (target_info.system in systems_linux) and
              not(cs_executable_stack in current_settings.moduleswitches) then
             TElfObjSection.create_ext(data,'.note.GNU-stack',SHT_PROGBITS,0,0,0,1,0);
           { insert filename as first in strtab }
           symtabsect.fstrsec.writestr(ExtractFileName(current_module.mainsource));
           symtabsect.fstrsec.writestr(#0);
           { calc amount of sections we have }
           nsections:=1;
           { also create the index in the section header table }
           ObjSectionList.ForEachCall(@section_count_sections,@nsections);
           { create .symtab and .strtab }
           createsymtab(data);
           { Create the relocation sections, this needs valid secidx and symidx }
           ObjSectionList.ForEachCall(@section_create_relocsec,data);
           { recalc nsections to incude the reloc sections }
           nsections:=1;
           ObjSectionList.ForEachCall(@section_count_sections,@nsections);
           { create .shstrtab }
           createshstrtab(data);

           { Calculate the filepositions }
           datapos:=$40; { elfheader + alignment }
           { section data }
           layoutsections(datapos);
           { section headers }
           shoffset:=datapos;
           inc(datapos,(nsections+1)*sizeof(telfsechdr));

           { Write ELF Header }
           fillchar(header,sizeof(header),0);
           header.magic[0]:=$7f; { = #127'ELF' }
           header.magic[1]:=$45;
           header.magic[2]:=$4c;
           header.magic[3]:=$46;
           header.file_class:=ELFCLASS;
           if target_info.endian=endian_big then
             header.data_encoding:=2
           else
             header.data_encoding:=1;

           header.file_version:=1;
           header.e_type:=ET_REL;
           header.e_machine:=ELFMACHINE;
{$ifdef arm}
           if (current_settings.fputype=fpu_soft) then
             header.e_flags:=$600;
{$endif arm}
           header.e_version:=1;
           header.e_shoff:=shoffset;
           header.e_shstrndx:=shstrtabsect.secshidx;

           header.e_shnum:=nsections;
           header.e_ehsize:=sizeof(telfheader);
           header.e_shentsize:=sizeof(telfsechdr);
           MaybeSwapHeader(header);
           writer.write(header,sizeof(header));
           writer.writezeros($40-sizeof(header)); { align }
           { Sections }
           WriteSectionContent(data);
           { section headers, start with an empty header for sh_undef }
           writer.writezeros(sizeof(telfsechdr));
           ObjSectionList.ForEachCall(@section_write_sechdr,nil);
         end;
        result:=true;
      end;


{****************************************************************************
                               TELFAssembler
****************************************************************************}

    constructor TElfAssembler.Create(smart:boolean);
      begin
        inherited Create(smart);
        CObjOutput:=TElfObjectOutput;
      end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

{$ifdef i386}
     const
       as_i386_elf32_info : tasminfo =
          (
            id     : as_i386_elf32;
            idtxt  : 'ELF';
            asmbin : '';
            asmcmd : '';
            supported_targets : [system_i386_linux,system_i386_beos,
                                 system_i386_freebsd,system_i386_haiku,
                                 system_i386_openbsd,system_i386_netbsd,
                                 system_i386_Netware,system_i386_netwlibc,
	                         system_i386_solaris,system_i386_embedded];
            flags : [af_outputbinary,af_smartlink_sections,af_supports_dwarf];
            labelprefix : '.L';
            comment : '';
            dollarsign: '$';
          );
{$endif i386}
{$ifdef x86_64}
     const
       as_x86_64_elf64_info : tasminfo =
          (
            id     : as_x86_64_elf64;
            idtxt  : 'ELF';
            asmbin : '';
            asmcmd : '';
            supported_targets : [system_x86_64_linux,system_x86_64_freebsd,
                                 system_x86_64_openbsd,system_x86_64_netbsd];
            flags : [af_outputbinary,af_smartlink_sections,af_supports_dwarf];
            labelprefix : '.L';
            comment : '';
            dollarsign: '$';
          );
{$endif x86_64}
{$ifdef sparc}
     const
       as_sparc_elf32_info : tasminfo =
          (
            id     : as_sparc_elf32;
            idtxt  : 'ELF';
            asmbin : '';
            asmcmd : '';
            supported_targets : [];
//            flags : [af_outputbinary,af_smartlink_sections];
            flags : [af_outputbinary,af_supports_dwarf];
            labelprefix : '.L';
            comment : '';
            dollarsign: '$';
          );
{$endif sparc}


initialization
{$ifdef i386}
  RegisterAssembler(as_i386_elf32_info,TElfAssembler);
{$endif i386}
{$ifdef sparc}
  RegisterAssembler(as_sparc_elf32_info,TElfAssembler);
{$endif sparc}
{$ifdef x86_64}
  RegisterAssembler(as_x86_64_elf64_info,TElfAssembler);
{$endif x86_64}
end.
