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
          shstridx,
          shtype,
          shflags,
          shlink,
          shinfo,
          shentsize : longint;
          constructor create(AList:TFPHashObjectList;const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);override;
          constructor create_ext(aobjdata:TObjData;const Aname:string;Ashtype,Ashflags:longint;Aalign:shortint;Aentsize:longint);
          constructor create_reloc(aobjdata:TObjData;const Aname:string;allocflag:boolean);
          procedure writeReloc_internal(aTarget:TObjSection;offset:aword;len:byte;reltype:TObjRelocationType);override;
       end;

       TElfSymtabKind = (esk_obj,esk_exe,esk_dyn);

       TElfSymtab = class(TElfObjSection)
       public
         kind: TElfSymtabKind;
         fstrsec: TObjSection;
         symidx: longint;
         constructor create(aObjData:TObjData;aKind:TElfSymtabKind);reintroduce;
         procedure writeSymbol(objsym:TObjSymbol;nameidx:longword=0);
         procedure writeInternalSymbol(avalue:aword;astridx:longword;ainfo:byte;ashndx:word);
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

       TElfTarget=class(TObject)
       public
         class function encodereloc(objrel:TObjRelocation):byte;virtual;abstract;
         class procedure loadreloc(objrel:TObjRelocation);virtual;abstract;
       end;

       TElfTargetClass=class of TElfTarget;

       PSectionRec=^TSectionRec;
       TSectionRec=record
         sec: TObjSection;
         relocpos: aword;
         relocs: longint;
         relentsize: longint;
       end;

       TElfObjInput=class(TObjInput)
       private
         FSecTbl: PSectionRec;
         FSymTbl: PPointer;
         FLoaded: PBoolean;
         nsects: longword;
         shentsize: longword;
         shoffset: aword;
         shstrndx: longword;
         shstrtab: PChar;
         strtab: PChar;
         shstrtablen: longword;
         strtablen: longword;
         symtaboffset: aword;
         syms: longword;
         localsyms: longword;
         symversions: PWord;
         dynobj: boolean;
         function LoadHeader:word;
         procedure LoadSection(const hdr;index:longint;objdata:TObjData);
         procedure LoadRelocations(const secrec:TSectionRec);
         procedure LoadSymbols(objdata:TObjData;count,locals:longword);
         procedure LoadDynamic(const hdr;objdata:TObjData);
       public
         constructor Create;override;
         destructor Destroy;override;
         function  ReadObjData(AReader:TObjectreader;objdata:TObjData):boolean;override;
         class function CanReadObjData(AReader:TObjectreader):boolean;override;
       end;

       TElfExeSection=class(TExeSection)
       public
         secshidx  : longword; { index of the section header }
         shstridx,
         shtype,
         shflags,
         shlink,
         shinfo,
         shentsize : longword;
         procedure AddObjSection(objsec:TObjSection;ignoreprops:boolean=false);override;
       end;

       TElfSegment=class
       public
         ptype: longword;
         pflags: longword;
         DataPos: aword;
         DataSize: aword;
         MemPos: aword;
         MemSize: aword;
         align: longword;
         //physaddr: aword;
         FSectionList: TFPObjectList;
         constructor Create(atype,aflags,aalign:longword);
         destructor Destroy; override;
         procedure Add(exesec:TExeSection);
       end;

       TElfExeOutput=class(TExeOutput)
       private
         segmentlist: TFPObjectList;
         textseg,
         dataseg,
         noteseg,
         phdrseg: TElfSegment;
         shstrtabsect: TElfObjSection;
         symtab: TElfSymtab;
         shoffset: longint;
         gotwritten: boolean;
         { dynamic linking }
         dynamiclink: boolean;
         dynsymnames: Plongword;
         dynsymtable: TElfSymtab;
         interpobjsec: TObjSection;
         dynamicsec,
         hashobjsec: TElfObjSection;
         neededlist: TFPHashList;
         gotsize: aword;
         dynrelsize: aword;

         function AttachSection(objsec:TObjSection):TElfExeSection;
         function CreateSegment(atype,aflags,aalign:longword):TElfSegment;
         procedure CreatePLT;
         procedure WriteHeader;
         procedure WriteDynamicSymbolsHash;
         procedure WriteDynamicTags;
         procedure FinishDynamicTags;
         procedure exesection_write_header(p:TObject;arg:Pointer);
         procedure segment_write_header(p:TObject;arg:Pointer);
         procedure mempos_segment(seg:TElfSegment);
         procedure datapos_segment(seg:TElfSegment);
         procedure MapSectionsToSegments;
         procedure WriteStaticSymtable;
         procedure InitDynlink;
         procedure PrepareGOT;
         procedure WriteDynTag(aTag:longword;aValue:longword);
         procedure WriteDynTag(aTag:longword;aSection:TObjSection;aOffs:aword=0);
       protected
         hastextrelocs: boolean;
         gotsymbol: TObjSymbol;
         dynsymlist: TFPObjectList;
         gotobjsec: TObjSection;
         pltobjsec,
         gotpltobjsec,
         pltrelocsec,
         ipltrelocsec,
         dynrelocsec: TElfObjSection;
         tlsseg: TElfSegment;
         procedure WriteDynRelocEntry(dataofs:aword;typ:byte;symidx:aword;addend:aword);
         procedure WriteFirstPLTEntry;virtual;abstract;
         procedure WritePLTEntry(exesym:TExeSymbol);virtual;
         procedure WriteIndirectPLTEntry(exesym:TExeSymbol);virtual;
         procedure GOTRelocPass1(objsec:TObjSection;ObjReloc:TObjRelocation);virtual;abstract;
       public
         constructor Create;override;
         destructor Destroy;override;
         procedure Load_Start;override;
         procedure Load_DynamicObject(ObjData:TObjData);override;
         procedure Order_Start;override;
         procedure Order_end;override;
         procedure AfterUnusedSectionRemoval;override;
         procedure MemPos_Start;override;
         procedure MemPos_ExeSection(const aname:string);override;
         procedure DataPos_Start;override;
         procedure DataPos_ExeSection(const aname:string);override;
         function writeData:boolean;override;
         procedure GenerateLibraryImports(ImportLibraryList:TFPHashObjectList);override;
       end;

     var
       ElfExeOutputClass: TExeOutputClass;
       ElfTarget: TElfTargetClass;

     const
       { Bits of TObjSymbol.refs field }
       symref_plt = 1;

{TODO: should become property of back-end }
{$ifdef x86_64}
     const
       relocs_use_addend:Boolean=True;
{$else x86_64}
     const
       relocs_use_addend:Boolean=False;
{$endif x86_64}


implementation

      uses
        SysUtils,
        verbose,
        export,expunix,
        cutils,globals,fmodule;

    const
      symbolresize = 200*18;

    const
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
      EM_MIPS       = 8;
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
{$ifdef powerpc64}
      ELFMACHINE = EM_PPC; // TODO
{$endif}
{$ifdef mips}
      ELFMACHINE = EM_MIPS;
{$endif}
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
      SHT_INIT_ARRAY = 14;
      SHT_FINI_ARRAY = 15;
      SHT_PREINIT_ARRAY = 16;
      SHT_GROUP    = 17;
      SHT_SYMTAB_SHNDX = 18;
      SHT_GNU_verdef = $6ffffffd;
      SHT_GNU_verneed = $6ffffffe;
      SHT_GNU_versym = $6fffffff;

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

      DT_RELACOUNT = $6ffffff9;
      DT_RELCOUNT  = $6ffffffa;
      DT_FLAGS_1   = $6ffffffb;
      DT_VERDEF    = $6ffffffc;
      DT_VERDEFNUM = $6ffffffd;
      DT_VERNEED   = $6ffffffe;
      DT_VERNEEDNUM = $6fffffff;

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
        TElf32sechdr=packed record
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
          address : longword;
          info    : longword; { bit 0-7: type, 8-31: symbol }
          addend  : longint;
        end;
        TElf32symbol=packed record
          st_name  : longword;
          st_value : longword;
          st_size  : longword;
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
        telf64sechdr=packed record
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
          st_name  : longword;
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

        TElfVerdef=record        { same for 32 and 64 bits }
          vd_version: word;      { =1 }
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

      function ELF_R_INFO(sym:longword;typ:byte):qword;inline;
        begin
          result:=(qword(sym) shl 32) or typ;
        end;

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

      function ELF_R_INFO(sym:longword;typ:byte):longword;inline;
        begin
          result:=(sym shl 8) or typ;
        end;
{$endif cpu64bitaddr}

{$ifdef x86_64}
      const
        ELF_MAXPAGESIZE:longint=$200000;
        TEXT_SEGMENT_START:longint=$400000;
{$else x86_64}
      const
        ELF_MAXPAGESIZE:longint=$1000;
        TEXT_SEGMENT_START:longint=$8048000;
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
          include(aoptions,oso_load);
        if Ashflags and SHF_WRITE<>0 then
          include(aoptions,oso_write);
        if Ashflags and SHF_EXECINSTR<>0 then
          include(aoptions,oso_executable);
      end;


{****************************************************************************
                               TElfObjSection
****************************************************************************}

    constructor TElfObjSection.create(AList:TFPHashObjectList;const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);
      begin
        inherited create(AList,Aname,Aalign,aoptions);
        index:=0;
        shstridx:=0;
        encodesechdrflags(aoptions,shtype,shflags);
        shlink:=0;
        shinfo:=0;
        if name='.stab' then
          shentsize:=sizeof(TObjStabEntry);
      end;


    constructor TElfObjSection.create_ext(aobjdata:TObjData;const Aname:string;Ashtype,Ashflags:longint;Aalign:shortint;Aentsize:longint);
      var
        aoptions : TObjSectionOptions;
      begin
        decodesechdrflags(Ashtype,Ashflags,aoptions);
        inherited create(aobjdata.ObjSectionList,Aname,Aalign,aoptions);
        objdata:=aobjdata;
        index:=0;
        shstridx:=0;
        shtype:=AshType;
        shflags:=AshFlags;
        shentsize:=Aentsize;
      end;


    const
      relsec_prefix:array[boolean] of string[5] = ('.rel','.rela');
      relsec_shtype:array[boolean] of longword = (SHT_REL,SHT_RELA);

    constructor TElfObjSection.create_reloc(aobjdata:TObjData;const Aname:string;allocflag:boolean);
      begin
        create_ext(aobjdata,
          relsec_prefix[relocs_use_addend]+aname,
          relsec_shtype[relocs_use_addend],
          SHF_ALLOC*ord(allocflag),
          sizeof(pint),
          (2+ord(relocs_use_addend))*sizeof(pint));
      end;


    procedure TElfObjSection.writeReloc_internal(aTarget:TObjSection;offset:aword;len:byte;reltype:TObjRelocationType);
      var
        reloc: TObjRelocation;
      begin
        reloc:=TObjRelocation.CreateSection(Size,aTarget,reltype);
        reloc.size:=len;
        ObjRelocations.Add(reloc);
        if reltype=RELOC_RELATIVE then
          dec(offset,len)
        else if reltype<>RELOC_ABSOLUTE then
          InternalError(2012062401);
        if relocs_use_addend then
          begin
            reloc.orgsize:=offset;
            offset:=0;
          end;
        write(offset,len);
      end;


{****************************************************************************
                            TElfObjData
****************************************************************************}

    constructor TElfObjData.create(const n:string);
      begin
        inherited create(n);
        CObjSection:=TElfObjSection;
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
               { If target is a local label and it isn't handled above,
                 patch its type in order to get it written to symtable.
                 This may happen e.g. when taking address of Pascal label in PIC mode. }
               if (p.bind=AB_LOCAL) and (p.typ=AT_LABEL) then
                 p.typ:=AT_ADDR;
            end;
         end;
        if assigned(objreloc) then
          begin
            objreloc.size:=len;
            if reltype in [RELOC_RELATIVE{$ifdef x86},RELOC_PLT32{$endif}{$ifdef x86_64},RELOC_GOTPCREL{$endif}] then
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
      symsectypes: array[boolean] of longword  = (SHT_SYMTAB,SHT_DYNSYM);
      symsecattrs: array[boolean] of longword  = (0,SHF_ALLOC);


    constructor TElfSymtab.create(aObjData:TObjData;aKind:TElfSymtabKind);
      var
        dyn:boolean;
      begin
        dyn:=(aKind=esk_dyn);
        create_ext(aObjData,symsecnames[dyn],symsectypes[dyn],symsecattrs[dyn],sizeof(pint),sizeof(TElfSymbol));
        fstrsec:=TElfObjSection.create_ext(aObjData,strsecnames[dyn],SHT_STRTAB,symsecattrs[dyn],1,0);
        fstrsec.writezeros(1);
        writezeros(sizeof(TElfSymbol));
        symidx:=1;
        shinfo:=1;
        kind:=aKind;
      end;

    procedure TElfSymtab.writeInternalSymbol(avalue:aword;astridx:longword;ainfo:byte;ashndx:word);
      var
        elfsym:TElfSymbol;
      begin
        fillchar(elfsym,sizeof(elfsym),0);
        elfsym.st_value:=avalue;
        elfsym.st_name:=astridx;
        elfsym.st_info:=ainfo;
        elfsym.st_shndx:=ashndx;
        inc(symidx);
        inc(shinfo);
        MaybeSwapElfSymbol(elfsym);
        write(elfsym,sizeof(elfsym));
      end;

    procedure TElfSymtab.writeSymbol(objsym:TObjSymbol;nameidx:longword);
      var
        elfsym:TElfSymbol;
      begin
        fillchar(elfsym,sizeof(elfsym),0);
        if nameidx=0 then
          elfsym.st_name:=fstrsec.writestr(objsym.name)
        else
          elfsym.st_name:=nameidx;
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
              elfsym.st_value:=var_align(objsym.size);
              elfsym.st_info:=STB_GLOBAL shl 4;
              elfsym.st_shndx:=SHN_COMMON;
            end;
          AB_EXTERNAL :
            elfsym.st_info:=STB_GLOBAL shl 4;
          AB_WEAK_EXTERNAL :
            begin
              elfsym.st_info:=STB_WEAK shl 4;
              elfsym.st_value:=objsym.address;
            end;
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
              AT_TLS:
                elfsym.st_info:=elfsym.st_info or STT_TLS;
              AT_GNU_IFUNC:
                elfsym.st_info:=elfsym.st_info or STT_GNU_IFUNC;
            end;
          end;
        if objsym.bind<>AB_COMMON then
          begin
            if kind<>esk_obj then
              begin
                if assigned(objsym.objsection) and assigned(objsym.objsection.ExeSection) then
                  begin
                    if (oso_plt in objsym.objsection.SecOptions) then
                      elfsym.st_value:=0
                    else
                      elfsym.st_shndx:=TElfExeSection(objsym.objsection.ExeSection).secshidx;
                  end;
              end
            else
              begin
                if assigned(objsym.objsection) then
                  elfsym.st_shndx:=objsym.objsection.index
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
        relsym   : longint;
        relocsect : TElfObjSection;
      begin
        { create the reloc section }
        relocsect:=TElfObjSection.create_reloc(data,s.name,false);
        relocsect.shlink:=symtabsect.index;
        relocsect.shinfo:=s.index;
        { add the relocations }
        for i:=0 to s.Objrelocations.count-1 do
          begin
            objreloc:=TObjRelocation(s.Objrelocations[i]);

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

            rel.address:=objreloc.dataoffset;
            rel.info:=ELF_R_INFO(relsym,ElfTarget.encodereloc(objreloc));
            rel.addend:=objreloc.orgsize;

            { write reloc }
            { ElfXX_Rel is essentially ElfXX_Rela without the addend field. }
            MaybeSwapElfReloc(rel);
            relocsect.write(rel,relocsect.shentsize);
          end;
      end;


    procedure TElfObjectOutput.section_write_symbol(p:TObject;arg:pointer);
      begin
        { Must not write symbols for internal sections like .symtab }
        { TODO: maybe use inclusive list of section types instead }
        if (TElfObjSection(p).shtype in [SHT_SYMTAB,SHT_STRTAB,SHT_REL,SHT_RELA]) then
          exit;
        TObjSection(p).secsymidx:=symtabsect.symidx;
        symtabsect.writeInternalSymbol(0,0,STT_SECTION,TObjSection(p).index);
      end;


    procedure TElfObjectOutput.createsymtab(data: TObjData);
      var
        i      : longint;
        objsym : TObjSymbol;
      begin
        with data do
         begin
           { filename entry }
           symtabsect.writeInternalSymbol(0,1,STT_FILE,SHN_ABS);
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
           symtabsect.shlink:=symtabsect.fstrsec.index;
         end;
      end;


    procedure TElfObjectOutput.createshstrtab(data: TObjData);
      var
        i,prefixlen:longint;
        objsec,target:TElfObjSection;
      begin
        shstrtabsect.writezeros(1);
        prefixlen:=length('.rel')+ord(relocs_use_addend);
        for i:=0 to data.ObjSectionList.Count-1 do
          begin
            objsec:=TElfObjSection(data.ObjSectionList[i]);
            { Alias section names into names of corresponding reloc sections,
              this is allowed by ELF specs and saves good half of .shstrtab space. }
            if objsec.shtype=relsec_shtype[relocs_use_addend] then
              begin
                target:=TElfObjSection(data.ObjSectionList[objsec.shinfo-1]);
                if (target.ObjRelocations.Count=0) or
                   (target.shstridx<prefixlen) then
                  InternalError(2012101204);
                objsec.shstridx:=target.shstridx-prefixlen;
              end
            else
              begin
                if objsec.ObjRelocations.Count<>0 then
                  shstrtabsect.write(relsec_prefix[true][1],prefixlen);
                objsec.shstridx:=shstrtabsect.writestr(objsec.name);
              end;
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
        TElfObjSection(p).index:=pword(arg)^;
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
           shstrtabsect:=TElfObjSection.create_ext(data,'.shstrtab',SHT_STRTAB,0,1,0);
           { "no executable stack" marker for Linux }
           if (target_info.system in systems_linux) and
              not(cs_executable_stack in current_settings.moduleswitches) then
             TElfObjSection.create_ext(data,'.note.GNU-stack',SHT_PROGBITS,0,1,0);
           { insert filename as first in strtab }
           symtabsect.fstrsec.writestr(ExtractFileName(current_module.mainsource));
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
           header.e_shstrndx:=shstrtabsect.index;

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


{****************************************************************************
                               TELFObjectInput
****************************************************************************}

    constructor TElfObjInput.Create;
      begin
        inherited Create;
        CObjData:=TElfObjData;
      end;


    destructor TElfObjInput.Destroy;
      begin
        if Assigned(FSymTbl) then
          FreeMem(FSymTbl);
        if Assigned(FSecTbl) then
          FreeMem(FSecTbl);
        if Assigned(strtab) then
          FreeMem(strtab);
        if Assigned(shstrtab) then
          FreeMem(shstrtab);
        if Assigned(symversions) then
          FreeMem(symversions);
        inherited Destroy;
      end;


    procedure TElfObjInput.LoadRelocations(const secrec:TSectionRec);
      var
        i: longint;
        rel: TElfReloc;
        reltyp: byte;
        relsym: longint;
        objrel: TObjRelocation;
        p: TObjSymbol;
      begin
        FReader.Seek(secrec.relocpos);
        if secrec.sec=nil then
          InternalError(2012060203);
        for i:=0 to secrec.relocs-1 do
          begin
            FReader.Read(rel,secrec.relentsize);
            MaybeSwapElfReloc(rel);
            reltyp:=rel.info and $FF;
{$ifdef cpu64bitaddr}
            relsym:=rel.info shr 32;
{$else cpu64bitaddr}
            relsym:=(rel.info shr 8) and $FFFFFF;
{$endif cpu64bitaddr}
            if relsym>=syms then
              InternalError(2012060204);
            p:=TObjSymbol(FSymTbl[relsym]);
            if assigned(p) then
              begin
                objrel:=TObjRelocation.CreateRaw(rel.address-secrec.sec.mempos,p,reltyp);
                if relocs_use_addend then
                  objrel.orgsize:=rel.addend;
                { perform target-specific actions }
                ElfTarget.loadreloc(objrel);
                secrec.sec.ObjRelocations.add(objrel);
              end
            else
              begin
                InputError('Unable to resolve symbol of relocation');
                exit;
              end;
          end;
      end;


    procedure TElfObjInput.LoadSymbols(objdata:TObjData;count,locals:longword);
      var
        i: longint;
        sym: TElfSymbol;
        bind: TAsmSymBind;
        typ: TAsmSymType;
        objsym: TObjSymbol;
        ver: word;
      begin
        FSymTbl:=AllocMem(count*sizeof(Pointer));
        for i:=1 to count-1 do
          begin
            FReader.Read(sym,sizeof(TElfSymbol));
            MaybeSwapElfSymbol(sym);
            if sym.st_name>=strtablen then
              InternalError(2012060205);

            if sym.st_shndx=SHN_ABS then    { ignore absolute symbols (should we really do it???) }
              Continue
            else if sym.st_shndx=SHN_COMMON then
              bind:=AB_COMMON
            else if (sym.st_shndx>=nsects) or
              (
                (sym.st_shndx>0) and
                (FSecTbl[sym.st_shndx].sec=nil) and
                (not dynobj)
              ) then
              begin
                writeln(objdata.name,' ',i);
                InternalError(2012060206)
              end
            else
              case (sym.st_info shr 4) of
                STB_LOCAL:
                  bind:=AB_LOCAL;
                STB_GLOBAL:
                  if sym.st_shndx=SHN_UNDEF then
                    bind:=AB_EXTERNAL
                  else
                    bind:=AB_GLOBAL;
                STB_WEAK:
                  bind:=AB_WEAK_EXTERNAL;
              else
                InternalError(2012060207);
              end;

            case (sym.st_info and $0F) of
              STT_NOTYPE:
                typ:=AT_NONE;
              STT_OBJECT:
                typ:=AT_DATA;
              STT_FUNC:
                typ:=AT_FUNCTION;
              STT_SECTION:
                typ:=AT_SECTION;
              STT_FILE:
                continue;
              STT_TLS:
                typ:=AT_TLS;
              STT_GNU_IFUNC:
                typ:=AT_GNU_IFUNC;
            else
              writeln(objdata.name,' ',sym.st_info and $0F);
              InternalError(2012060208);
            end;
            { If reading DSO, we're interested only in global symbols defined there.
              Symbols with non-current version should also be ignored. }
            if dynobj then
              begin
                if assigned(symversions) then
                  begin
                    ver:=symversions[i];
                    if (ver=0) or (ver > $7FFF) then
                      continue;
                  end;
                if (bind= AB_LOCAL) or (sym.st_shndx=SHN_UNDEF) then
                  continue;
              end;
            { validity of name and objsection has been checked above }
            { !! all AT_SECTION symbols have duplicate (null) name,
              therefore TObjSection.CreateSymbol cannot be used here }
            objsym:=TObjSymbol.Create(objdata.ObjSymbolList,string(PChar(@strtab[sym.st_name])));
            objsym.bind:=bind;
            objsym.typ:=typ;
            if bind<>AB_COMMON then
              objsym.objsection:=FSecTbl[sym.st_shndx].sec;
            objsym.offset:=sym.st_value;
            objsym.size:=sym.st_size;
            FSymTbl[i]:=objsym;
          end;
      end;


    procedure TElfObjInput.LoadSection(const hdr;index:longint;objdata:tobjdata);
      var
        shdr: TElfsechdr absolute hdr;
        sec: TElfObjSection;
        secname: string;
      begin
        case shdr.sh_type of
          SHT_NULL:
            {ignore};

          { SHT_STRTAB may appear for .stabstr and other debug sections.
            .shstrtab and .strtab are processed separately and don't appear here. }
          SHT_PROGBITS,SHT_NOBITS,SHT_NOTE,SHT_STRTAB:
            begin
              if shdr.sh_name>=shstrtablen then
                InternalError(2012060210);
              secname:=string(PChar(@shstrtab[shdr.sh_name]));

              sec:=TElfObjSection.create_ext(objdata,secname,
                shdr.sh_type,shdr.sh_flags,shdr.sh_addralign,shdr.sh_entsize);

              if (Length(secname)>3) and (secname[2] in ['d','f','n','s']) then
                begin
                  if (Pos('.stub',secname)=1) or
                    (Pos('.fpc',secname)=1) then
                    sec.SecOptions:=[oso_keep]
                  { ELF does not have any flags specific to debug sections,
                    but reserves names starting with '.debug' for this purpose }
                  else if (Pos('.debug',secname)=1) or
                    (secname='.stab') or
                    (secname='.stabstr') then
                    sec.SecOptions:=[oso_debug]
                  else if (secname='.note.GNU-stack') and (shdr.sh_type=SHT_PROGBITS) then
                    begin
                      if (shdr.sh_flags and SHF_EXECINSTR)=0 then
                        objdata.ExecStack:=False;
                    end;
                end;

              if (shdr.sh_type=SHT_NOTE) and (shdr.sh_size<>0) then
                sec.SecOptions:=[oso_keep];

              sec.index:=index;
              sec.DataPos:=shdr.sh_offset;
              sec.MemPos:=shdr.sh_addr;
              sec.Size:=shdr.sh_size;
              FSecTbl[index].sec:=sec;
            end;

          SHT_REL,SHT_RELA:
            begin
              if shdr.sh_info>=nsects then
                InternalError(2012060211);
              if shdr.sh_entsize<>longword((2+ord(shdr.sh_type=SHT_RELA))*sizeof(pint)) then
                InternalError(2012060212);

              with FSecTbl[shdr.sh_info] do
                begin
                  relocpos:=shdr.sh_offset;
                  relocs:=shdr.sh_size div shdr.sh_entsize;
                  relentsize:=shdr.sh_entsize;
                end;
            end;

          SHT_GROUP:
            if (shdr.sh_size>=2*sizeof(longword)) and
              (shdr.sh_entsize=sizeof(longword)) and
              ((shdr.sh_size mod shdr.sh_entsize)=0) then
              begin
                { we need a dummy section to load the corresponding symbol later on }
                secname:=string(PChar(@shstrtab[shdr.sh_name]));
                sec:=TElfObjSection.create_ext(objdata,secname,
                  shdr.sh_type,shdr.sh_flags,shdr.sh_addralign,shdr.sh_entsize);
                sec.index:=index;
                FSecTbl[index].sec:=sec;
              end;
        else
          InternalError(2012072603);
        end;
        FLoaded[index]:=True;
      end;


    function TElfObjInput.LoadHeader:word;
      var
        header:TElfHeader;
      begin
        result:=ET_NONE;
        if not FReader.read(header,sizeof(header)) then
          begin
            InputError('Can''t read ELF header');
            exit;
          end;
        if (header.magic[0]<>$7f) or (header.magic[1]<>$45) or
           (header.magic[2]<>$4c) or (header.magic[3]<>$46) then
          begin
            InputError('Illegal ELF magic');
            exit;
          end;
        if (header.file_version<>1) then
          begin
            InputError('Unknown ELF file version');
            exit;
          end;
        if (header.file_class<>ELFCLASS) then
          begin
            InputError('Wrong ELF file class (32/64 bit mismatch)');
            exit;
          end;
        if (header.data_encoding<>1+ord(target_info.endian=endian_big)) then
          begin
            InputError('ELF endianness does not match target');
            exit;
          end;
        MaybeSwapHeader(header);
        if (header.e_version<>1) then
          begin
            InputError('Unknown ELF data version');
            exit;
          end;
        if (header.e_machine<>ELFMACHINE) then
          begin
            InputError('ELF file is for different CPU');
            exit;
          end;

        nsects:=header.e_shnum;
        shentsize:=header.e_shentsize;
        shoffset:=header.e_shoff;
        shstrndx:=header.e_shstrndx;
        result:=header.e_type;
      end;


    procedure TElfObjInput.LoadDynamic(const hdr;objdata:TObjData);
      var
        shdr: TElfsechdr absolute hdr;
        dt: TElfDyn;
        i: longint;
      begin
        if (shdr.sh_entsize<>sizeof(TElfDyn)) then
          InternalError(2012071403);

        FReader.Seek(shdr.sh_offset);
        for i:=0 to (shdr.sh_size div shdr.sh_entsize)-1 do
          begin
            FReader.Read(dt,sizeof(TElfDyn));
            MaybeSwapElfDyn(dt);
            case dt.d_tag of
              DT_NULL:
                break;
              DT_SONAME:
                TElfObjData(objdata).FName:=string(PChar(@strtab[dt.d_ptr]));
              DT_NEEDED:
                ;
            end;
          end;
      end;


    function TElfObjInput.ReadObjData(AReader:TObjectreader;objdata:TObjData):boolean;
      var
        i,j,symtabndx,strndx,dynndx,
        versymndx,verdefndx,verneedndx: longint;
        objsec: TElfObjSection;
        shdrs: array of TElfsechdr;
      begin
        FReader:=AReader;
        InputFileName:=AReader.FileName;
        result:=false;

        i:=LoadHeader;
        if (i=ET_NONE) then    { error message already given in this case }
          exit;
        if (i<>ET_REL) and (i<>ET_DYN) then
          begin
            InputError('Not a relocatable or dynamic ELF file');
            exit;
          end;
        dynobj:=(i=ET_DYN);

        if shentsize<>sizeof(TElfsechdr) then
          InternalError(2012062701);

        FSecTbl:=AllocMem(nsects*sizeof(TSectionRec));
        FLoaded:=AllocMem(nsects*sizeof(boolean));
        SetLength(shdrs,nsects);

        FReader.Seek(shoffset);
        if not FReader.Read(shdrs[0],nsects*sizeof(TElfsechdr)) then
          begin
            InputError('Can''t read ELF section headers');
            exit;
          end;
        if source_info.endian<>target_info.endian then
          for i:=0 to nsects-1 do
            MaybeSwapSecHeader(shdrs[i]);

        { First, load the .shstrtab section }
        if shstrndx>=nsects then
          InternalError(2012060201);
        if shdrs[shstrndx].sh_type<>SHT_STRTAB then
          InternalError(2012060202);
        shstrtablen:=shdrs[shstrndx].sh_size;
        GetMem(shstrtab,shstrtablen);
        FReader.seek(shdrs[shstrndx].sh_offset);
        FReader.read(shstrtab^,shstrtablen);
        FLoaded[shstrndx]:=True;

        { Locate the symtable, it is typically at the end so loop backwards.
          Load the strings, postpone symtable itself until done with sections.
          Note that is is legal to have no symtable.
          For DSO, locate .dynsym instead, this one is near the beginning, but
          overall number of sections won't be big. }
        symtabndx:=-1;
        for i:=nsects-1 downto 1 do
          begin
            if (shdrs[i].sh_type<>symsectypes[dynobj]) then
              continue;
            if (shdrs[i].sh_entsize<>sizeof(TElfSymbol)) then
              InternalError(2012060213);
            if shdrs[i].sh_link>=nsects then
              InternalError(2012062702);
            strndx:=shdrs[i].sh_link;
            if shdrs[strndx].sh_type<>SHT_STRTAB then
              InternalError(2012062703);
            strtablen:=shdrs[strndx].sh_size;
            GetMem(strtab,strtablen);
            FReader.seek(shdrs[strndx].sh_offset);
            FReader.read(strtab^,strtablen);

            symtaboffset:=shdrs[i].sh_offset;
            syms:=shdrs[i].sh_size div sizeof(TElfSymbol);
            localsyms:=shdrs[i].sh_info;
            FLoaded[i]:=True;
            FLoaded[strndx]:=True;
            symtabndx:=i;
            break;
          end;

        if dynobj then
          begin
            { Locate .dynamic and version sections. Expect a single one of a kind. }
            dynndx:=0;
            versymndx:=0;
            verdefndx:=0;
            verneedndx:=0;
            for i:=nsects-1 downto 0 do
              begin
                case shdrs[i].sh_type of
                  SHT_DYNAMIC:
                    begin
                      if dynndx<>0 then
                        InternalError(2012102001);
                      dynndx:=i;
                      if (shdrs[dynndx].sh_link<>strndx) then
                        InternalError(2012071402);
                      LoadDynamic(shdrs[dynndx],objdata);
                    end;

                  SHT_GNU_versym:
                    begin
                      if versymndx<>0 then
                        InternalError(2012102002);
                      versymndx:=i;
                      if shdrs[i].sh_entsize<>sizeof(word) then
                        InternalError(2012102003);
                      if shdrs[i].sh_link<>symtabndx then
                        InternalError(2012102004);
                      if shdrs[i].sh_size<>syms*sizeof(word) then
                        InternalError(2012102005);
                      GetMem(symversions,shdrs[i].sh_size);
                      FReader.seek(shdrs[i].sh_offset);
                      FReader.read(symversions^,shdrs[i].sh_size);
                      if source_info.endian<>target_info.endian then
                        for j:=0 to syms-1 do
                          SwapEndian(symversions[j]);
                    end;

                  SHT_GNU_verdef:
                    begin
                      if verdefndx<>0 then
                        InternalError(2012102006);
                      verdefndx:=i;
                      //sh_link->.dynstr
                      //sh_info->.hash
                    end;

                  SHT_GNU_verneed:
                    begin
                      if verneedndx<>0 then
                        InternalError(2012102007);
                      verneedndx:=i;
                      //sh_link->.dynstr
                      //sh_info->hash
                    end;
                end;
             end;

            if dynndx=0 then
              InternalError(2012071401);

            { for DSO, we aren't interested in actual sections, but need to a dummy one
              to maintain integrity. }
            objsec:=TElfObjSection.create_ext(objdata,'*DSO*',SHT_PROGBITS,0,0,0);
            for i:=1 to nsects-1 do
              FSecTbl[i].sec:=objsec;

            { load the symtable }
            FReader.Seek(symtaboffset+sizeof(TElfSymbol));
            LoadSymbols(objdata,syms,localsyms);

            result:=True;
            exit;
          end;

        { assume stack is executable until proven otherwise }
        objdata.ExecStack:=True;

        { Process section headers }
        for i:=1 to nsects-1 do
          if not FLoaded[i] then
            LoadSection(shdrs[i],i,objdata);

        { load the content }
        ReadSectionContent(objdata);

        { load the symtable }
        FReader.Seek(symtaboffset+sizeof(TElfSymbol));
        LoadSymbols(objdata,syms,localsyms);

        { finish relocations }
        for i:=0 to objdata.ObjSectionList.Count-1 do
          begin
            objsec:=TElfObjSection(objdata.ObjsectionList[i]);
            { skip debug sections }
            if (oso_debug in objsec.SecOptions) and
               (cs_link_strip in current_settings.globalswitches) and
               not(cs_link_separate_dbg_file in current_settings.globalswitches) then
              continue;

            if FSecTbl[objsec.index].relocpos>0 then
              LoadRelocations(FSecTbl[objsec.index]);
          end;

        result:=True;
      end;


    class function TElfObjInput.CanReadObjData(AReader:TObjectreader):boolean;
      var
        header: TElfHeader;
      begin
        result:=false;
        if AReader.Read(header,sizeof(header)) then
          begin;
            if (header.magic[0]=$7f) and (header.magic[1]=$45) and
               (header.magic[2]=$4c) and (header.magic[3]=$46) then
            { TODO: check additional fields }
              result:=true;
          end;
        AReader.Seek(0);
      end;

{*****************************************************************************
                                  TElfExeOutput
*****************************************************************************}

    constructor TElfExeOutput.Create;
      begin
        inherited Create;
        CObjData:=TElfObjData;
        CExeSection:=TElfExeSection;
{$ifdef cpu64}
        MaxMemPos:=Qword($FFFFFFFFFFFFFFFF);
        //MaxMemPos:=$7EFFFFFF;   { As specified by SysV AMD64 ABI for small memory model }
{$else cpu64}
        MaxMemPos:=$7FFFFFFF;
{$endif cpu64}
        SectionMemAlign:=$20;
        SectionDataAlign:=$20;
        segmentlist:=TFPObjectList.Create(True);
        neededlist:=TFPHashList.Create;
      end;


    destructor TElfExeOutput.Destroy;
      begin
        neededlist.Free;
        segmentlist.Free;
        dynsymlist.Free;
        if assigned(dynsymnames) then
          FreeMem(dynsymnames);
        inherited Destroy;
      end;


    function TElfExeOutput.AttachSection(objsec:TObjSection):TElfExeSection;
      begin
        objsec.SecOptions:=[oso_keep];
        result:=TElfExeSection(FindExeSection(objsec.name));
        if result=nil then
          result:=TElfExeSection.Create(ExeSectionList,objsec.name);
        result.AddObjSection(objsec);
      end;


    function TElfExeOutput.CreateSegment(atype,aflags,aalign:longword):TElfSegment;
      begin
        result:=TElfSegment.Create(atype,aflags,aalign);
        segmentlist.add(result);
      end;


    procedure TElfExeOutput.WriteHeader;
      var
        header: TElfHeader;
      begin
        FillChar(header,sizeof(header),0);
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
        if IsSharedLibrary then
          header.e_type:=ET_DYN
        else
          header.e_type:=ET_EXEC;
        header.e_machine:=ELFMACHINE;
{$ifdef arm}
        if (current_settings.fputype=fpu_soft) then
          header.e_flags:=$600;
{$endif arm}
        header.e_version:=1;
        header.e_phoff:=sizeof(TElfHeader);
        header.e_shoff:=shoffset;
        header.e_shstrndx:=ExeSectionList.IndexOf(shstrtabsect.ExeSection)+1;

        header.e_shnum:=ExeSectionList.Count+1;
        header.e_phnum:=segmentlist.count;
        header.e_ehsize:=sizeof(telfheader);
        if assigned(EntrySym) then
          header.e_entry:=EntrySym.Address;
        header.e_shentsize:=sizeof(telfsechdr);
        header.e_phentsize:=sizeof(telfproghdr);
        MaybeSwapHeader(header);
        FWriter.Write(header,sizeof(header));
      end;


    procedure TElfExeOutput.exesection_write_header(p:TObject;arg:Pointer);
      var
        shdr: TElfsechdr;
        exesec: TElfExeSection absolute p;
      begin
        FillChar(shdr,sizeof(shdr),0);
        shdr.sh_name:=exesec.shstridx;
        if (ExeWriteMode=ewm_dbgonly) and
           (exesec.SecOptions*[oso_debug,oso_debug_copy]=[]) then
          shdr.sh_type:=SHT_NOBITS
        else
          shdr.sh_type:=exesec.shtype;
        shdr.sh_flags:=exesec.shflags;
        if (oso_load in exesec.SecOptions) then
          shdr.sh_addr:=exesec.MemPos;
        shdr.sh_offset:=exesec.DataPos;
        shdr.sh_size:=exesec.Size;
        shdr.sh_link:=exesec.shlink;
        shdr.sh_info:=exesec.shinfo;
        shdr.sh_addralign:=exesec.SecAlign;
        shdr.sh_entsize:=exesec.shentsize;
        MaybeSwapSecHeader(shdr);
        FWriter.Write(shdr,sizeof(shdr));
      end;


    procedure TElfExeOutput.segment_write_header(p:TObject;arg:Pointer);
      var
        phdr: TElfproghdr;
        seg: TElfSegment absolute p;
      begin
        FillChar(phdr,sizeof(phdr),0);
        phdr.p_type:=seg.ptype;
        phdr.p_flags:=seg.pflags;
        phdr.p_align:=seg.align;
        phdr.p_offset:=seg.DataPos;
        phdr.p_filesz:=seg.DataSize;
        phdr.p_memsz:=seg.MemSize;
        phdr.p_vaddr:=seg.MemPos;
        phdr.p_paddr:=seg.MemPos;

        MaybeSwapHeader(phdr);
        FWriter.Write(phdr,sizeof(phdr));
      end;


    procedure TElfExeOutput.WriteStaticSymtable;
      var
        i: longint;
        sec: TElfExeSection;
        exesym: TExeSymbol;
      begin
        for i:=0 to ExeSectionList.Count-1 do
          begin
            sec:=TElfExeSection(ExeSectionList[i]);
            { Must not write symbols for internal sections like .symtab }
            if (sec.shtype in [SHT_SYMTAB,SHT_STRTAB,SHT_REL,SHT_RELA]) then
              continue;
            sec.secsymidx:=symtab.symidx;
            symtab.writeInternalSymbol(sec.mempos,0,STT_SECTION,sec.secshidx);
          end;
        { local symbols first }
        for i:=0 to ExeSymbolList.Count-1 do
          begin
            exesym:=TExeSymbol(ExeSymbolList[i]);
            if (exesym.objsymbol.bind=AB_LOCAL) and (exesym.objsymbol.typ<>AT_LABEL) then
              symtab.WriteSymbol(exesym.objsymbol);
          end;
        { Global Symbols }
        for i:=0 to ExeSymbolList.Count-1 do
          begin
            exesym:=TExeSymbol(ExeSymbolList[i]);
            if (exesym.objsymbol.bind<>AB_LOCAL) then
              symtab.WriteSymbol(exesym.objsymbol);
          end;
        { update exe section properties }
        symtab.ExeSection.size:=symtab.size;
        TElfExeSection(symtab.ExeSection).shinfo:=symtab.shinfo;
        TElfExeSection(symtab.ExeSection).shlink:=ExeSectionList.IndexOf(symtab.fstrsec.ExeSection)+1;
        symtab.fstrsec.ExeSection.Size:=symtab.fstrsec.size;
      end;


    procedure TElfExeOutput.MapSectionsToSegments;
      var
        seg: TElfSegment;
        exesec: TExeSection;
        i: longint;
      begin
        if (not IsSharedLibrary) and assigned(interpobjsec) then
          begin
            phdrseg:=CreateSegment(PT_PHDR,PF_R or PF_X,sizeof(pint));
            seg:=CreateSegment(PT_INTERP,PF_R,1);
            seg.Add(interpobjsec.ExeSection);
          end;

        textseg:=CreateSegment(PT_LOAD,PF_X or PF_R,ELF_MAXPAGESIZE);
        dataseg:=CreateSegment(PT_LOAD,PF_R or PF_W,ELF_MAXPAGESIZE);
        for i:=0 to ExeSectionList.Count-1 do
          begin
            exesec:=TExeSection(ExeSectionList[i]);
            if (oso_load in exesec.SecOptions) then
              begin
                if (TElfExeSection(exesec).shflags and SHF_TLS)<>0 then
                  begin
                    if tlsseg=nil then
                      tlsseg:=CreateSegment(PT_TLS,PF_R,sizeof(pint));
                    tlsseg.add(exesec);
                  end;

                { TODO: at least on Linux, ld seems to drop .note.ABI-tag for static executables.
                  (Logic is as follows: there is no .note.ABI-tag section in ld script, so it
                  is processed as orphan section. As such, it is placed after .interp.
                  For static executables .interp is dropped, and it looks like there's nowhere to
                  place .note.ABI-tag in this case)
                  Always including it doesn't harm though (except increasing file size). }
                if TElfExeSection(exesec).shtype=SHT_NOTE then
                  begin
                    if noteseg=nil then
                      noteseg:=CreateSegment(PT_NOTE,PF_R,4);
                    noteseg.Add(exesec);
                    Include(exesec.SecOptions,oso_debug_copy);
                  end;
                if (oso_executable in exesec.SecOptions) or
                  not (oso_write in exesec.SecOptions) then
                  textseg.add(exesec)
                else
                  dataseg.add(exesec);
              end;
          end;

        if dynamiclink then
          begin
            seg:=CreateSegment(PT_DYNAMIC,PF_R or PF_W,sizeof(pint));
            seg.add(dynamicsec.ExeSection);
          end;

        { stack flags }
        CreateSegment(PT_GNU_STACK,PF_R or PF_W or (PF_X*ord(ExecStack)),sizeof(pint));
      end;


    procedure TElfExeOutput.PrepareGOT;
      var
        i,j,k: longint;
        objsec: TElfObjSection;
        exesec: TExeSection;
      begin
        for i:=0 to ExeSectionList.Count-1 do
          begin
            exesec:=TExeSection(ExeSectionList[i]);
            for j:=0 to exesec.ObjSectionlist.count-1 do
              begin
                objsec:=TElfObjSection(exesec.ObjSectionlist[j]);
                { ignore linker-generated and debug sections }
                if (objsec.objdata=internalobjdata) or (oso_debug in objsec.SecOptions) then
                  continue;
                if not objsec.Used then
                  internalerror(2012060901);
                for k:=0 to objsec.ObjRelocations.Count-1 do
                  GOTRelocPass1(objsec,TObjRelocation(objsec.ObjRelocations[k]));
              end;
          end;
        { remember sizes for sanity checking }
        gotsize:=gotobjsec.size;
        if assigned(dynrelocsec) then
          dynrelsize:=dynrelocsec.size
        else
          dynrelsize:=0;
      end;


    procedure TElfExeOutput.Load_Start;
      begin
        inherited Load_Start;
        dynsymlist:=TFPObjectList.Create(False);

        gotpltobjsec:=TElfObjSection.create_ext(internalObjData,'.got.plt',
          SHT_PROGBITS,SHF_ALLOC or SHF_WRITE,sizeof(pint),sizeof(pint));

        gotobjsec:=TElfObjSection.create_ext(internalObjData,'.got',
            SHT_PROGBITS,SHF_ALLOC or SHF_WRITE,sizeof(pint),sizeof(pint));
        gotobjsec.SecOptions:=[oso_keep];

        { GOT symbol and reserved .got.plt entries }
        internalObjData.SetSection(gotpltobjsec);
        gotsymbol:=internalObjData.SymbolDefine('_GLOBAL_OFFSET_TABLE_',AB_GLOBAL,AT_DATA);
        gotpltobjsec.writeZeros(3*sizeof(pint));
      end;


    procedure TElfExeOutput.Load_DynamicObject(objdata:TObjData);
      var
        i: longint;
        exesym: TExeSymbol;
        objsym: TObjSymbol;
      begin
        Comment(v_debug,'Dynamic object: '+objdata.name);
        if neededlist.Find(objdata.name)=nil then
          neededlist.Add(objdata.name,objdata);
        for i:=0 to UnresolvedExeSymbols.Count-1 do
          begin
            exesym:=TExeSymbol(UnresolvedExeSymbols[i]);
            if exesym.State<>symstate_undefined then
              continue;
            objsym:=TObjSymbol(objdata.ObjSymbolList.Find(exesym.name));
            if assigned(objsym) then
              begin
                exesym.State:=symstate_defined;
                exesym.dynindex:=dynsymlist.Add(exesym)+1;
              end;
          end;
      end;


    procedure TElfExeOutput.Order_Start;
      begin
        inherited Order_Start;
        dynamiclink:=IsSharedLibrary or (dynsymlist.count>0) or
          (
            (UnresolvedExeSymbols.Count>0) and
            not (cs_link_staticflag in current_settings.globalswitches)
          );
        if dynamiclink then
          InitDynlink;
        if dynamiclink or (IndirectObjSymbols.Count>0) then
          CreatePLT;
      end;


    procedure TElfExeOutput.Order_end;

      procedure set_oso_keep(const s:string);
        var
          exesec:TExeSection;
          objsec:TObjSection;
          i:longint;
        begin
          exesec:=TExeSection(ExeSectionList.Find(s));
          if assigned(exesec) then
            begin
              for i:=0 to exesec.ObjSectionList.Count-1 do
                begin
                  objsec:=TObjSection(exesec.ObjSectionList[i]);
                  { ignore sections used for symbol definition }
                  if oso_data in objsec.SecOptions then
                    objsec.SecOptions:=[oso_keep];
                end;
            end;
        end;

      begin
        inherited Order_end;
        set_oso_keep('.init');
        set_oso_keep('.fini');
        set_oso_keep('.jcr');
        set_oso_keep('.ctors');
        set_oso_keep('.dtors');
        set_oso_keep('.preinit_array');
        set_oso_keep('.init_array');
        set_oso_keep('.fini_array');
        set_oso_keep('.eh_frame');

        { let .dynamic reference other dynamic sections so they aren't marked
          for removal as unused }
        if dynamiclink then
          WriteDynamicTags;
      end;


    procedure TElfExeOutput.AfterUnusedSectionRemoval;
      var
        i:longint;
        exesym:TExeSymbol;
        objsym:TObjSymbol;
        objsec: TObjSection;
      begin
        { Unused section removal changes state of referenced exesymbols
          to symstate_dynamic. Remaining ones can be removed. }
        for i:=0 to dynsymlist.count-1 do
          begin
            exesym:=TExeSymbol(dynsymlist[i]);
            if assigned(exesym.ObjSymbol.ObjSection) then  // an exported symbol
              continue;
            if exesym.state<>symstate_dynamic then
              begin
                dynsymlist[i]:=nil;
                exesym.dynindex:=0;
              end;
          end;
        dynsymlist.Pack;
        { reindex }
        for i:=0 to dynsymlist.count-1 do
          TExeSymbol(dynsymlist[i]).dynindex:=i+1;

        { Drop unresolved symbols that aren't referenced, assign dynamic
          indices to remaining ones. }
        for i:=0 to UnresolvedExeSymbols.Count-1 do
          begin
            exesym:=TExeSymbol(UnresolvedExeSymbols[i]);
            if exesym.state=symstate_dynamic then
              begin
                if exesym.dynindex<>0 then
                  InternalError(2012062301);
                exesym.dynindex:=dynsymlist.add(exesym)+1;
                exesym.state:=symstate_defined;
              end
            else
              UnresolvedExeSymbols[i]:=nil;
          end;
        UnresolvedExeSymbols.Pack;

        { Scan relocations to determine size of GOT, dynamic reloc section, etc. }
        PrepareGOT;

        { Write required PLT entries }
        for i:=0 to dynsymlist.Count-1 do
          begin
            exesym:=TExeSymbol(dynsymlist[i]);
            if assigned(exesym.ObjSymbol.objsection) then  // an exported symbol
              continue;

            { if there's no PLT references to symbol, then PLT entry isn't needed }
            { !! Does not work correctly yet !! }
//            if (exesym.objsymbol.refs and symref_plt)=0 then
//              continue;

            { This symbol has a valid address to which relocations are resolved,
              but it remains (weak)external when written to dynamic symtable. }
            objsym:=internalobjdata.CreateSymbol(exesym.name);
            objsym.typ:=AT_FUNCTION;
            objsym.bind:=exesym.ObjSymbol.bind;  { AB_EXTERNAL or AB_WEAK_EXTERNAL }
            objsym.offset:=pltobjsec.size;
            objsym.objsection:=pltobjsec;
            exesym.ObjSymbol:=objsym;

            WritePLTEntry(exesym);
          end;

        { Handle indirect symbols }
        for i:=0 to IndirectObjSymbols.Count-1 do
          begin
            objsym:=TObjSymbol(IndirectObjSymbols[i]);
            objsec:=objsym.ExeSymbol.ObjSymbol.objsection;
            objsym.bind:=AB_EXTERNAL;  { cheat FixupSymbols }
            if (oso_plt in objsec.SecOptions) then
              continue;
            WriteIndirectPLTEntry(objsym.ExeSymbol);
          end;

        FixupSymbols;

        if dynamiclink then
          WriteDynamicSymbolsHash;
      end;


    procedure TElfExeOutput.MemPos_Start;
      var
        i,j: longint;
        seg: TElfSegment;
        exesec: TElfExeSection;
        objsec: TObjSection;
        tempmempos: qword;
      begin
        { If using -Xg, .shstrtab needs oso_debug flag, otherwise it won't be written
          to .dbg file. RemoveDebugInfo will then destroy ExeSection named .shstrtab,
          objsection remains intact but its contents must be rewritten. }
        if Assigned(shstrtabsect) then
          begin
            shstrtabsect.ReleaseData;
            shstrtabsect.Size:=0;
            shstrtabsect.SecOptions:=[oso_data];
          end
        else
          shstrtabsect:=TElfObjSection.create_ext(internalObjData,'.shstrtab',SHT_STRTAB,0,1,0);
        if (cs_link_separate_dbg_file in current_settings.globalswitches) then
          shstrtabsect.SecOptions:=[oso_debug];
        AttachSection(shstrtabsect);
        shstrtabsect.writezeros(1);

        if (not gotwritten) then
          begin
            { Create the static symtable (.symtab and .strtab) }
            if (cs_link_separate_dbg_file in current_settings.globalswitches) or
              not(cs_link_strip in current_settings.globalswitches) then
              begin
                symtab:=TElfSymtab.Create(internalObjData,esk_exe);
                symtab.SecOptions:=[oso_debug];
                symtab.fstrsec.SecOptions:=[oso_debug];
                AttachSection(symtab);
                AttachSection(symtab.fstrsec);
              end;

            { Re-enable sections which end up to contain some data
              (.got, .rel[a].dyn, .rel[a].plt (includes .rel[a].iplt) and .hash }
            if gotobjsec.size<>0 then
              Exclude(gotobjsec.ExeSection.SecOptions,oso_disabled);
            if assigned(dynrelocsec) and (dynrelocsec.size<>0) then
              Exclude(dynrelocsec.ExeSection.SecOptions,oso_disabled);
            if assigned(pltrelocsec) and (pltrelocsec.size>0) then
              Exclude(pltrelocsec.ExeSection.SecOptions,oso_disabled);
            if assigned(ipltrelocsec) and (ipltrelocsec.size>0) then
              Exclude(ipltrelocsec.ExeSection.SecOptions,oso_disabled);
            if assigned(hashobjsec) then
              Exclude(hashobjsec.ExeSection.SecOptions,oso_disabled);
          end;

        RemoveDisabledSections;

        SegmentList.Clear;
        textseg:=nil;
        dataseg:=nil;
        phdrseg:=nil;
        noteseg:=nil;
        MapSectionsToSegments;

        { Assign section indices and fill .shstrtab
          List of sections cannot be modified after this point. }
        for i:=0 to ExeSectionList.Count-1 do
          begin
            exesec:=TElfExeSection(ExeSectionList[i]);
            exesec.shstridx:=shstrtabsect.writestr(exesec.Name);
            exesec.secshidx:=i+1;
          end;

        if dynamiclink then
          begin
            if (not gotwritten) then
              FinishDynamicTags;

            { fixup sh_link/sh_info members of various dynamic sections }
            TElfExeSection(hashobjsec.ExeSection).shlink:=TElfExeSection(dynsymtable.ExeSection).secshidx;
            i:=TElfExeSection(dynsymtable.fstrsec.ExeSection).secshidx;
            TElfExeSection(dynamicsec.ExeSection).shlink:=i;
            TElfExeSection(dynsymtable.ExeSection).shlink:=i;

            if assigned(pltrelocsec) then
              begin
                TElfExeSection(pltrelocsec.ExeSection).shlink:=TElfExeSection(dynsymtable.ExeSection).secshidx;
                TElfExeSection(pltrelocsec.ExeSection).shinfo:=TElfExeSection(pltobjsec.ExeSection).secshidx;
              end;

            if assigned(dynrelocsec) and assigned(dynrelocsec.ExeSection) then
              TElfExeSection(dynrelocsec.ExeSection).shlink:=TElfExeSection(dynsymtable.ExeSection).secshidx;
          end
        else if assigned(ipltrelocsec) then
          TElfExeSection(ipltrelocsec.ExeSection).shinfo:=TElfExeSection(pltobjsec.ExeSection).secshidx;

        { The actual layout }
        if IsSharedLibrary then
          CurrMemPos:=0
        else
          CurrMemPos:=TEXT_SEGMENT_START;
        textseg.MemPos:=CurrMemPos;
        if assigned(phdrseg) then
          begin
            phdrseg.Mempos:=CurrMemPos+sizeof(TElfHeader);
            phdrseg.Memsize:=sizeof(TElfproghdr)*segmentlist.count;
          end;
        CurrMemPos:=CurrMemPos+sizeof(TElfHeader)+segmentlist.count*sizeof(TElfproghdr);
        MemPos_Segment(textseg);
        CurrMemPos:=Align(CurrMemPos,SectionDataAlign); {! Data,not MemAlign}
        CurrMemPos:=CurrMemPos+ELF_MAXPAGESIZE;
        dataseg.MemPos:=CurrMemPos;
        MemPos_Segment(dataseg);
        { Mempos of unmapped sections is forced to zero, but we have to set positions
          of its objsections and update sizes }
        for i:=0 to ExeSectionList.Count-1 do
          begin
            exesec:=TElfExeSection(ExeSectionList[i]);
            if not (oso_load in exesec.SecOptions) then
              begin
                tempmempos:=0;
                exesec.MemPos:=tempmempos;
                for j:=0 to exesec.ObjSectionList.Count-1 do
                  begin
                    objsec:=TObjSection(exesec.ObjSectionList[j]);
                    tempmempos:=objsec.setmempos(tempmempos);
                  end;
                exesec.Size:=tempmempos;
              end;
          end;

        { Update MemPos and MemSize of non-load segments,
          in particular, TLS sizes are needed to resolve relocations }
        for i:=0 to segmentlist.count-1 do
          begin
            seg:=TElfSegment(segmentlist[i]);
            if (seg.ptype=PT_LOAD) or (seg.FSectionList.Count=0) then
              continue;
            seg.MemPos:=TExeSection(seg.FSectionList.First).MemPos;
            for j:=0 to seg.FSectionList.Count-1 do
              begin
                exesec:=TElfExeSection(seg.FSectionList[j]);
                seg.MemSize:=exesec.MemPos+exesec.Size-seg.MemPos;
              end;
          end;

        if (not gotwritten) then
          begin
            { reset size of .got and .rel[a].dyn, they will be refilled while fixing up relocations }
            if assigned(gotobjsec) then
              gotobjsec.size:=0;
            if assigned(dynrelocsec) then
              begin
                dynrelocsec.size:=0;
                { write actual .dynsym content (needs valid symbol addresses) }
                dynsymtable.size:=sizeof(TElfsymbol);
                for i:=0 to dynsymlist.count-1 do
                  dynsymtable.writeSymbol(TExeSymbol(dynsymlist[i]).objsymbol,dynsymnames[i]);
              end;
          end;
      end;


    procedure TElfExeOutput.MemPos_Segment(seg:TElfSegment);
      var
        i: longint;
        exesec: TElfExeSection;
      begin
        for i:=0 to seg.FSectionList.Count-1 do
          begin
            exesec:=TElfExeSection(seg.FSectionList[i]);
            inherited MemPos_ExeSection(exesec);
            { .tbss should not contribute to address space }
            if (exesec.shtype=SHT_NOBITS) and ((exesec.shflags and SHF_TLS)<>0) then
              CurrMemPos:=exesec.MemPos;
          end;
        { calculate size of the segment }
        seg.MemSize:=CurrMemPos-seg.MemPos;
      end;


    procedure TElfExeOutput.MemPos_ExeSection(const aname:string);
      begin
        // Ignore. All layout is done in mempos_start
      end;


    procedure TElfExeOutput.DataPos_Start;
      var
        i,j: longint;
        exesec: TExeSection;
        seg: TElfSegment;
      begin
        gotwritten:=true;
        { sanity checks }
        if assigned(gotobjsec) and (gotsize<>gotobjsec.size) then
          InternalError(2012092501);
        if assigned(dynrelocsec) and (dynrelsize<>dynrelocsec.size) then
          InternalError(2012092502);

        if (ExeWriteMode=ewm_dbgonly) or
          (
            (ExeWriteMode=ewm_exefull) and
             not(cs_link_strip in current_settings.globalswitches)
          ) then
          WriteStaticSymtable;

        { first handle primary segments }
        textseg.DataPos:=0;
        CurrDataPos:=sizeof(TElfHeader)+sizeof(TElfproghdr)*segmentlist.count;
        if assigned(phdrseg) then
          begin
            phdrseg.DataPos:=sizeof(TElfHeader);
            phdrseg.DataSize:=sizeof(TElfproghdr)*segmentlist.count;
          end;
        DataPos_Segment(textseg);
        CurrDataPos:=align(CurrDataPos,SectionDataAlign);
        dataseg.DataPos:=CurrDataPos;
        DataPos_Segment(dataseg);
        { then unmapped sections }
        for i:=0 to ExeSectionList.Count-1 do
          begin
            exesec:=TExeSection(ExeSectionList[i]);
            if not (oso_load in exesec.SecOptions) then
              inherited DataPos_ExeSection(exesec);
          end;
        { finally, update size/position of non-load segments }
        for i:=0 to segmentlist.count-1 do
          begin
            seg:=TElfSegment(segmentlist[i]);
            if (seg.ptype=PT_LOAD) or (seg.FSectionList.Count=0) then
              continue;
            seg.DataPos:=TExeSection(seg.FSectionList.First).DataPos;
            for j:=0 to seg.FSectionList.Count-1 do
              begin
                exesec:=TExeSection(seg.FSectionList[j]);
                if oso_data in exesec.SecOptions then
                  seg.DataSize:=exesec.DataPos+exesec.Size-seg.DataPos;
              end;
          end;
        { place section headers after the data }
        shoffset:=CurrDataPos;
        CurrDataPos:=CurrDataPos+ExeSectionList.Count*sizeof(TElfsechdr);
      end;


    procedure TElfExeOutput.DataPos_Segment(seg:TElfSegment);
      var
        i: longint;
        exesec: TElfExeSection;
      begin
        for i:=0 to seg.FSectionList.Count-1 do
          begin
            exesec:=TElfExeSection(seg.FSectionList[i]);
            { ELF needs DataPos set to 'would-be' value for sections that
              don't have data, and for non-debug sections in .dbg file, too.
              This slightly differs from generic approach. }
            if not (oso_data in exesec.SecOptions) or
              (
                (ExeWriteMode=ewm_dbgonly) and
                (exesec.SecOptions*[oso_debug,oso_debug_copy]=[])
              ) then
              begin
                CurrDataPos:=align(CurrDataPos,SectionDataAlign);
                exesec.DataPos:=CurrDataPos;
              end
            else
              inherited DataPos_ExeSection(exesec);

          end;
        { calculate size of the segment }
        seg.DataSize:=CurrDataPos-seg.DataPos;
      end;


    procedure TElfExeOutput.DataPos_ExeSection(const aname:string);
      begin
        // Ignore. Work is done entirely in datapos_start.
      end;


    procedure TElfExeOutput.InitDynlink;
      begin
        if not IsSharedLibrary then
          begin
            interpobjsec:=internalObjData.createsection('.interp',1,[oso_data,oso_load,oso_keep]);
            { TODO: supply target-specific default }
{$ifdef x86_64}
            interpobjsec.writestr('/lib64/ld-linux-x86-64.so.2');
{$else}
            interpobjsec.writestr('/lib/ld-linux.so.2');
{$endif x86_64}
          end;

        hashobjsec:=TElfObjSection.create_ext(internalObjData,'.hash',
          SHT_HASH,SHF_ALLOC,sizeof(pint),4);
        hashobjsec.secoptions:=[oso_keep];

        dynsymtable:=TElfSymtab.create(internalObjData,esk_dyn);

        dynamicsec:=TElfObjSection.create_ext(internalObjData,'.dynamic',
          SHT_DYNAMIC,SHF_ALLOC or SHF_WRITE,sizeof(pint),sizeof(TElfDyn));
        dynamicsec.SecOptions:=[oso_keep];

        dynrelocsec:=TElfObjSection.create_reloc(internalObjData,'.dyn',true);
        dynrelocsec.SecOptions:=[oso_keep];
      end;


    const
      hashbuckets: array[0..15] of longint=(
        1, 3, 17, 37, 67, 97, 131, 197, 263, 521, 1031, 2053, 4099, 8209,
        16411, 32771);

{$push}{$r-,q-}
    function elfhash(const name:string):longword;
      var
        g: longword;
        i: longint;
      begin
        result:=0;
        for i:=1 to length(name) do
        begin
          result:=(result shl 4)+ord(name[i]);
          g:=result and $F0000000;
          if g>0 then
            result:=result xor (g shr 24);
          result:=result and (not g);
        end;
      end;
{$pop}

    procedure TElfExeOutput.WriteDynamicSymbolsHash;
      var
        nchains,nbuckets: longint;
        i,j: longint;
        hashdata: plongint;
        sym: TExeSymbol;
      begin
        dynsymnames:=AllocMem(dynsymlist.count*sizeof(longword));
        nchains:=dynsymlist.Count+1;
        { determine suitable bucket count }
        i:=high(hashbuckets);
        while (i>=0) and (nchains<hashbuckets[i]) do
          dec(i);
        nbuckets:=hashbuckets[i];

        hashdata:=AllocMem((2+nchains+nbuckets)*sizeof(longint));
        hashdata[0]:=nbuckets;
        hashdata[1]:=nchains;
        { The contents of .dynsym can be written only after mempos pass
          because it needs valid symbol virtual addresses and section indices.
          Here we preset .dynsym size and write names, in order to get
          correct size of .dynstr section. }
        dynsymtable.size:=(dynsymlist.count+1)*sizeof(TElfsymbol);
        for i:=0 to dynsymlist.Count-1 do
          begin
            sym:=TExeSymbol(dynsymlist[i]);
            dynsymnames[i]:=dynsymtable.fstrsec.writestr(sym.objsymbol.name);
            j:=(elfhash(sym.objsymbol.name) mod nbuckets)+2;

            while hashdata[j]<>0 do
              j:=2+nbuckets+hashdata[j];
            hashdata[j]:=i+1;
          end;
        if source_info.endian<>target_info.endian then
          for i:=0 to nchains+nbuckets+1 do
            hashdata[i]:=swapendian(hashdata[i]);
        hashobjsec.write(hashdata^,(2+nchains+nbuckets)*sizeof(longint));
        freemem(hashdata);
      end;


    procedure TElfExeOutput.WriteDynRelocEntry(dataofs:aword;typ:byte;symidx:aword;addend:aword);
      var
        rel:telfreloc;
      begin
        rel.address:=dataofs;
        rel.info:=ELF_R_INFO(symidx,typ);
        rel.addend:=addend;
        MaybeSwapElfReloc(rel);
        dynrelocsec.write(rel,dynrelocsec.shentsize);
      end;


    procedure TElfExeOutput.WriteDynTag(aTag:longword;aValue:longword);
      var
        d: TElfDyn;
      begin
        d.d_tag:=aTag;
        d.d_val:=aValue;
        MaybeSwapElfDyn(d);
        dynamicsec.write(d,sizeof(TElfDyn));
      end;


    procedure TElfExeOutput.WriteDynTag(aTag:longword;aSection:TObjSection;aOffs:aword);
      var
        d: TElfDyn;
      begin
        d.d_tag:=aTag;
        if source_info.endian<>target_info.endian then
          d.d_tag:=swapendian(d.d_tag);
        dynamicsec.write(d.d_tag,sizeof(d.d_tag));
        { TODO: ignores endianness! }
        dynamicsec.writeReloc_internal(aSection,aOffs,sizeof(d.d_ptr),RELOC_ABSOLUTE);
      end;


    procedure TElfExeOutput.WriteDynamicTags;
      var
        s: aword;
        i: longint;
        sym: TExeSymbol;
        hs:string;
      begin
        for i:=0 to neededlist.Count-1 do
          begin
            s:=dynsymtable.fstrsec.writestr(neededlist.NameOfIndex(i));
            WriteDynTag(DT_NEEDED,s);
          end;

        if IsSharedLibrary then
          begin
            s:=dynsymtable.fstrsec.writestr(ExtractFileName(current_module.sharedlibfilename));
            WriteDynTag(DT_SONAME,s);
            { TODO: names hardcoded here }
            sym:=TExeSymbol(ExeSymbolList.Find('FPC_SHARED_LIB_START'));
            if assigned(sym) then
              WriteDynTag(DT_INIT,sym.objsymbol.objsection,sym.objsymbol.offset);
            sym:=TExeSymbol(ExeSymbolList.Find('FPC_LIB_EXIT'));
            if assigned(sym) then
              WriteDynTag(DT_FINI,sym.objsymbol.objsection,sym.objsymbol.offset);
          end;

        { TODO: we need a dedicated parameter to pass runpath, instead of this hack
          (-Xr is a different thing, it passes "-rpath-link"). }
        if (ParaLinkOptions<>'') then
          begin
            hs:=ParaLinkOptions;
            while (hs<>'') do
              begin
                if (GetToken(hs,' ')='-rpath') then
                  begin
                    s:=dynsymtable.fstrsec.writestr(GetToken(hs,' '));
                    WriteDynTag(DT_RPATH,s);
                  end;
              end;
          end;

        writeDynTag(DT_HASH,hashobjsec);
        writeDynTag(DT_STRTAB,dynsymtable.fstrsec);
        writeDynTag(DT_SYMTAB,dynsymtable);

        writeDynTag(DT_SYMENT,sizeof(TElfSymbol));

        if Assigned(gotpltobjsec) then
          writeDynTag(DT_PLTGOT,gotpltobjsec);
      end;


    const
      pltreltags: array[boolean] of longword=(DT_REL,DT_RELA);
      relsztags:  array[boolean] of longword=(DT_RELSZ,DT_RELASZ);
      relenttags: array[boolean] of longword=(DT_RELENT,DT_RELAENT);

    procedure TElfExeOutput.FinishDynamicTags;
      begin
        if assigned(dynsymtable) then
          writeDynTag(DT_STRSZ,dynsymtable.fstrsec.size);

        if hastextrelocs then
          writeDynTag(DT_TEXTREL,0);

        if Assigned(pltrelocsec) and (pltrelocsec.size>0) then
          begin
            writeDynTag(DT_PLTRELSZ,pltrelocsec.Size);
            writeDynTag(DT_PLTREL,pltreltags[relocs_use_addend]);
            writeDynTag(DT_JMPREL,pltrelocsec);
          end;

        if Assigned(dynrelocsec) and (dynrelocsec.size>0) then
          begin
            writeDynTag(pltreltags[relocs_use_addend],dynrelocsec);
            writeDynTag(relsztags[relocs_use_addend],dynrelocsec.Size);
            writeDynTag(relenttags[relocs_use_addend],dynrelocsec.shentsize);
          end;
        writeDynTag(DT_NULL,0);
      end;


    procedure TElfExeOutput.CreatePLT;
      var
        reloc: TObjRelocation;
      begin
        pltobjsec:=TElfObjSection.create_ext(internalObjData,'.plt',
          SHT_PROGBITS,SHF_ALLOC or SHF_EXECINSTR,4,16);
        pltobjsec.SecOptions:=[oso_keep,oso_plt];

        pltrelocsec:=TElfObjSection.create_reloc(internalObjData,'.plt',true);
        pltrelocsec.SecOptions:=[oso_keep];

        ipltrelocsec:=TElfObjSection.create_reloc(internalObjData,'.iplt',true);
        ipltrelocsec.SecOptions:=[oso_keep];

        { reference .dynamic from .got.plt, this isn't necessary if linking statically }
        { TODO: maybe move writing initial .got.plt entries here completely
          (needs testing --- GOT symbol may get lost if .got.plt is empty)}
        if dynamiclink then
          begin
            reloc:=TObjRelocation.CreateSection(0,dynamicsec,RELOC_ABSOLUTE);
            reloc.size:=sizeof(pint);
            gotpltobjsec.ObjRelocations.Add(reloc);
          end;

        { Initial PLT entry, CPU-specific }
        WriteFirstPLTEntry;
      end;


    procedure TElfExeOutput.WritePLTEntry(exesym:TExeSymbol);
      begin
        // must be implemented by CPU-specific descendant
        InternalError(2012092102);
      end;


    procedure TElfExeOutput.WriteIndirectPLTEntry(exesym:TExeSymbol);
      begin
        // must be implemented by CPU-specific descendant
        InternalError(2012092101);
      end;


    function TElfExeOutput.WriteData:boolean;
      begin
        WriteHeader;
        segmentlist.ForEachCall(@segment_write_header,nil);
        WriteExeSectionContent;
        FWriter.WriteZeros(sizeof(TElfsechdr));
        ExeSectionList.ForEachCall(@exesection_write_header,nil);
        result:=true;
      end;


    procedure TElfExeOutput.GenerateLibraryImports(ImportLibraryList:TFPHashObjectList);
      var
        i:longint;
        exportlist: TCmdStrList;
        sym: TExeSymbol;
      begin
        { add exported symbols to dynamic list }
        exportlist:=texportlibunix(exportlib).exportedsymnames;
        if not exportlist.empty then
          repeat
            sym:=TExeSymbol(ExeSymbolList.Find(exportlist.getfirst));
            if assigned(sym) then
              begin
                if assigned(sym.objsymbol.objsection) then
                  sym.objsymbol.objsection.SecOptions:=[oso_keep];
                sym.dynindex:=dynsymlist.add(sym)+1
              end
            else
              InternalError(2012071801);
          until exportlist.empty;

        { Mark unresolved weak symbols as defined, they will become dynamic further on.
          If compiling a .so, make all symbols dynamic, since shared library may reference
          symbols from executable which does not participate in linking. }
        for i:=0 to UnresolvedExeSymbols.Count-1 do
          begin
            sym:=TExeSymbol(UnresolvedExeSymbols[i]);
            if (sym.objsymbol.bind=AB_WEAK_EXTERNAL) or IsSharedLibrary then
              sym.state:=symstate_defined;
          end;
      end;


{****************************************************************************
                               TElfExeSection
****************************************************************************}

    procedure TElfExeSection.AddObjSection(objsec:TObjSection;ignoreprops:boolean);
      begin
        inherited AddObjSection(objsec,ignoreprops);
        if ignoreprops then
          exit;
        if (shtype=SHT_NULL) then
        begin
          shtype:=TElfObjSection(objsec).shtype;
          shflags:=TElfObjSection(objsec).shflags;
          shentsize:=TElfObjSection(objsec).shentsize;
        end;
      end;

{****************************************************************************
                                TElfSegment
****************************************************************************}

    constructor TElfSegment.Create(atype,aflags,aalign:longword);
      begin
        ptype:=atype;
        pflags:=aflags;
        align:=aalign;
        FSectionList:=TFPObjectList.Create(false);
      end;

    destructor TElfSegment.Destroy;
      begin
        FSectionList.Free;
        inherited Destroy;
      end;

    procedure TElfSegment.Add(exesec:TExeSection);
      begin
        FSectionList.Add(exesec);
      end;


end.
