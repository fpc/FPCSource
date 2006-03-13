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
       cpuinfo,cpubase,aasmbase,aasmtai,assemble,
       { output }
       ogbase;

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
          { relocation }
          relocsect : TElfObjSection;
          constructor create(const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);override;
          constructor create_ext(const Aname:string;Ashtype,Ashflags,Ashlink,Ashinfo:longint;Aalign:shortint;Aentsize:longint);
          destructor  destroy;override;
       end;

       TElfObjData = class(TObjData)
       public
         symtabsect,
         strtabsect,
         shstrtabsect,
         gotpcsect,
         gotoffsect,
         goTSect,
         plTSect,
         symsect  : TElfObjSection;
         syms     : Tdynamicarray;
         constructor create(const n:string);override;
         destructor  destroy;override;
         function  sectionname(atype:TAsmSectiontype;const aname:string):string;override;
         function  sectiontype2align(atype:TAsmSectiontype):shortint;override;
         procedure CreateDebugSections;override;
         procedure writereloc(data,len:aint;p:TObjSymbol;relative:TObjRelocationType);override;
         procedure writestab(offset:aint;ps:TObjSymbol;nidx,nother:byte;ndesc:word;p:pchar);override;
       end;

       TElfObjectOutput = class(tObjOutput)
       private
         elf32data : TElfObjData;
         symidx,
         localsyms : longint;
         procedure createrelocsection(s:TElfObjSection);
         procedure createshstrtab;
         procedure createsymtab;
         procedure writesectionheader(s:TElfObjSection);
         procedure writesectiondata(s:TElfObjSection);
         procedure write_internal_symbol(astridx:longint;ainfo:byte;ashndx:word);
         procedure section_write_symbol(p:TObject;arg:pointer);
         procedure section_write_sh_string(p:TObject;arg:pointer);
         procedure section_count_sections(p:TObject;arg:pointer);
         procedure section_create_relocsec(p:TObject;arg:pointer);
         procedure section_set_datapos(p:TObject;arg:pointer);
         procedure section_relocsec_set_datapos(p:TObject;arg:pointer);
         procedure section_write_data(p:TObject;arg:pointer);
         procedure section_write_sechdr(p:TObject;arg:pointer);
         procedure section_write_relocsec(p:TObject;arg:pointer);
       protected
         function writedata(data:TObjData):boolean;override;
       public
         constructor Create(smart:boolean);override;
       end;

       TElfAssembler = class(tinternalassembler)
         constructor create(smart:boolean);override;
       end;


implementation

      uses
        strings,
        verbose,
        cutils,globals,fmodule;

    const
      symbolresize = 200*18;

    const
      R_386_32 = 1;                    { ordinary absolute relocation }
      R_386_PC32 = 2;                  { PC-relative relocation }
      R_386_GOT32 = 3;                 { an offset into GOT }
      R_386_PLT32 = 4;                 { a PC-relative offset into PLT }
      R_386_GOTOFF = 9;                { an offset from GOT base }
      R_386_GOTPC = 10;                { a PC-relative offset _to_ GOT }

      R_SPARC_32 = 3;
      R_SPARC_WDISP30 = 7;
      R_SPARC_HI22 = 9;
      R_SPARC_LO10 = 12;

      { AMD64 relocations }
      R_X86_64_NONE = 0;
      { Direct 64 bit   }
      R_X86_64_64 = 1;
      { PC relative 32 bit signed  }
      R_X86_64_PC32 = 2;
      { 32 bit GOT entry  }
      R_X86_64_GOT32 = 3;
      { 32 bit PLT address  }
      R_X86_64_PLT32 = 4;
      { Copy symbol at runtime  }
      R_X86_64_COPY = 5;
      { Create GOT entry  }
      R_X86_64_GLOB_DAT = 6;
      { Create PLT entry  }
      R_X86_64_JUMP_SLOT = 7;
      { Adjust by program base  }
      R_X86_64_RELATIVE = 8;
      { 32 bit signed PC relative offset to GOT  }
      R_X86_64_GOTPCREL = 9;
      { Direct 32 bit zero extended  }
      R_X86_64_32 = 10;
      { Direct 32 bit sign extended  }
      R_X86_64_32S = 11;
      { Direct 16 bit zero extended  }
      R_X86_64_16 = 12;
      { 16 bit sign extended pc relative  }
      R_X86_64_PC16 = 13;
      { Direct 8 bit sign extended   }
      R_X86_64_8 = 14;
      { 8 bit sign extended pc relative  }
      R_X86_64_PC8 = 15;
      { ID of module containing symbol  }
      R_X86_64_DTPMOD64 = 16;
      { Offset in module's TLS block  }
      R_X86_64_DTPOFF64 = 17;
      { Offset in initial TLS block  }
      R_X86_64_TPOFF64 = 18;
      { 32 bit signed PC relative offset to two GOT entries for GD symbol  }
      R_X86_64_TLSGD = 19;
      { 32 bit signed PC relative offset to two GOT entries for LD symbol  }
      R_X86_64_TLSLD = 20;
      { Offset in TLS block  }
      R_X86_64_DTPOFF32 = 21;
      { 32 bit signed PC relative offset to GOT entry for IE symbol  }
      R_X86_64_GOTTPOFF = 22;
      { Offset in initial TLS block  }
      R_X86_64_TPOFF32 = 23;
      R_X86_64_NUM = 24;

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

      type
      { Structures which are written directly to the output file }
        TElf32header=packed record
          magic0123         : longint;
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
        TElf32reloc=packed record
          address : longint;
          info    : longint; { bit 0-7: type, 8-31: symbol }
        end;
        TElf32symbol=packed record
          st_name  : longint;
          st_value : longint;
          st_size  : longint;
          st_info  : byte; { bit 0-3: type, 4-7: bind }
          st_other : byte;
          st_shndx : word;
        end;


        telf64header=packed record
          magic0123         : longint;
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
        telf64reloc=packed record
          address : qword;
          info    : qword; { bit 0-7: type, 8-31: symbol }
          addend  : qword;
        end;
        telf64symbol=packed record
          st_name  : longint;
          st_info  : byte; { bit 0-3: type, 4-7: bind }
          st_other : byte;
          st_shndx : word;
          st_value : qword;
          st_size  : qword;
        end;
        telf64stab=packed record
          strpos  : longint;
          ntype   : byte;
          nother  : byte;
          ndesc   : word;
          nvalue  : longint;
        end;


{$ifdef cpu64bit}
        telfheader = telf64header;
        telfreloc = telf64reloc;
        telfsymbol = telf64symbol;
        telfsechdr = telf64sechdr;
{$else cpu64bit}
        telfheader = telf32header;
        telfreloc = telf32reloc;
        telfsymbol = telf32symbol;
        telfsechdr = telf32sechdr;
{$endif cpu64bit}


      function MayBeSwapHeader(h : telf32header) : telf32header;
        begin
          result:=h;
          if source_info.endian<>target_info.endian then
            with h do
              begin
                result.e_type:=swapword(e_type);
                result.e_machine:=swapword(e_machine);
                result.e_version:=swaplong(e_version);
                result.e_entry:=swaplong(e_entry);
                result.e_phoff:=swaplong(e_phoff);
                result.e_shoff:=swaplong(e_shoff);
                result.e_flags:=swaplong(e_flags);
                result.e_ehsize:=swapword(e_ehsize);
                result.e_phentsize:=swapword(e_phentsize);
                result.e_phnum:=swapword(e_phnum);
                result.e_shentsize:=swapword(e_shentsize);
                result.e_shnum:=swapword(e_shnum);
                result.e_shstrndx:=swapword(e_shstrndx);
              end;
        end;


      function MayBeSwapHeader(h : telf64header) : telf64header;
        begin
          result:=h;
          if source_info.endian<>target_info.endian then
            with h do
              begin
                result.e_type:=swapword(e_type);
                result.e_machine:=swapword(e_machine);
                result.e_version:=swaplong(e_version);
                result.e_entry:=swapqword(e_entry);
                result.e_phoff:=swapqword(e_phoff);
                result.e_shoff:=swapqword(e_shoff);
                result.e_flags:=swaplong(e_flags);
                result.e_ehsize:=swapword(e_ehsize);
                result.e_phentsize:=swapword(e_phentsize);
                result.e_phnum:=swapword(e_phnum);
                result.e_shentsize:=swapword(e_shentsize);
                result.e_shnum:=swapword(e_shnum);
                result.e_shstrndx:=swapword(e_shstrndx);
              end;
        end;


      function MaybeSwapSecHeader(h : telf32sechdr) : telf32sechdr;
        begin
          result:=h;
          if source_info.endian<>target_info.endian then
            with h do
              begin
                result.sh_name:=SwapLong(sh_name);
                result.sh_type:=SwapLong(sh_type);
                result.sh_flags:=SwapLong(sh_flags);
                result.sh_addr:=SwapLong(sh_addr);
                result.sh_offset:=SwapLong(sh_offset);
                result.sh_size:=SwapLong(sh_size);
                result.sh_link:=SwapLong(sh_link);
                result.sh_info:=SwapLong(sh_info);
                result.sh_addralign:=SwapLong(sh_addralign);
                result.sh_entsize:=SwapLong(sh_entsize);
              end;
        end;


      function MaybeSwapSecHeader(h : telf64sechdr) : telf64sechdr;
        begin
          result:=h;
          if source_info.endian<>target_info.endian then
            with h do
              begin
                result.sh_name:=SwapLong(sh_name);
                result.sh_type:=SwapLong(sh_type);
                result.sh_flags:=SwapQWord(sh_flags);
                result.sh_addr:=SwapQWord(sh_addr);
                result.sh_offset:=SwapQWord(sh_offset);
                result.sh_size:=SwapQWord(sh_size);
                result.sh_link:=SwapLong(sh_link);
                result.sh_info:=SwapLong(sh_info);
                result.sh_addralign:=SwapQWord(sh_addralign);
                result.sh_entsize:=SwapQWord(sh_entsize);
              end;
        end;


      function MaybeSwapElfSymbol(h : telf32symbol) : telf32symbol;
        begin
          result:=h;
          if source_info.endian<>target_info.endian then
            with h do
              begin
                result.st_name:=SwapLong(st_name);
                result.st_value:=SwapLong(st_value);
                result.st_size:=SwapLong(st_size);
                result.st_shndx:=SwapWord(st_shndx);
              end;
        end;


      function MaybeSwapElfSymbol(h : telf64symbol) : telf64symbol;
        begin
          result:=h;
          if source_info.endian<>target_info.endian then
            with h do
              begin
                result.st_name:=SwapLong(st_name);
                result.st_value:=SwapQWord(st_value);
                result.st_size:=SwapQWord(st_size);
                result.st_shndx:=SwapWord(st_shndx);
              end;
        end;


      function MaybeSwapElfReloc(h : telf32reloc) : telf32reloc;
        begin
          result:=h;
          if source_info.endian<>target_info.endian then
            with h do
              begin
                result.address:=SwapLong(address);
                result.info:=SwapLong(info);
              end;
        end;


      function MaybeSwapElfReloc(h : telf64reloc) : telf64reloc;
        begin
          result:=h;
          if source_info.endian<>target_info.endian then
            with h do
              begin
                result.address:=SwapQWord(address);
                result.info:=SwapQWord(info);
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
      end;


{****************************************************************************
                               TSection
****************************************************************************}

    constructor TElfObjSection.create(const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);
      begin
        inherited create(Aname,Aalign,aoptions);
        secshidx:=0;
        shstridx:=0;
        encodesechdrflags(aoptions,shtype,shflags);
        shlink:=0;
        shinfo:=0;
        if name='.stab' then
          shentsize:=sizeof(TObjStabEntry);
        relocsect:=nil;
      end;


    constructor TElfObjSection.create_ext(const Aname:string;Ashtype,Ashflags,Ashlink,Ashinfo:longint;Aalign:shortint;Aentsize:longint);
      var
        aoptions : TObjSectionOptions;
      begin
        decodesechdrflags(Ashtype,Ashflags,aoptions);
        inherited create(Aname,Aalign,aoptions);
        secshidx:=0;
        shstridx:=0;
        shtype:=AshType;
        shflags:=AshFlags;
        shlink:=Ashlink;
        shinfo:=Ashinfo;
        shentsize:=Aentsize;
        relocsect:=nil;
      end;


    destructor TElfObjSection.destroy;
      begin
        if assigned(relocsect) then
          relocsect.free;
        inherited destroy;
      end;


{****************************************************************************
                            TElfObjData
****************************************************************************}

    constructor TElfObjData.create(const n:string);
      begin
        inherited create(n);
        CObjSection:=TElfObjSection;
        { reset }
        Syms:=TDynamicArray.Create(symbolresize);
        { default sections }
        symtabsect:=TElfObjSection.create_ext('.symtab',2,0,0,0,4,sizeof(telfsymbol));
        strtabsect:=TElfObjSection.create_ext('.strtab',3,0,0,0,1,0);
        shstrtabsect:=TElfObjSection.create_ext('.shstrtab',3,0,0,0,1,0);
        { insert the empty and filename as first in strtab }
        strtabsect.writestr(#0);
        strtabsect.writestr(SplitFileName(current_module.mainsource^)+#0);
        { we need at least the following sections }
        createsection(sec_code,'');
        createsection(sec_data,'');
        createsection(sec_bss,'');
        if tf_section_threadvars in target_info.flags then
          createsection(sec_threadvar,'');
        if (tf_needs_dwarf_cfi in target_info.flags) and
           (af_supports_dwarf in target_asm.flags) then
             createsection(sec_debug_frame,'');
      end;


    destructor TElfObjData.destroy;
      begin
        Syms.Free;
        symtabsect.free;
        strtabsect.free;
        shstrtabsect.free;
        inherited destroy;
      end;


    function TElfObjData.sectionname(atype:TAsmSectiontype;const aname:string):string;
      const
        secnames : array[TAsmSectiontype] of string[13] = ('',
{$ifdef userodata}
          '.text','.data','.rodata','.bss','.threadvar',
{$else userodata}
          '.text','.data','.data','.bss','.threadvar',
{$endif userodata}
          '.text', { darwin stubs }
          '.stab','.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame','.debug_info','.debug_line','.debug_abbrev',
          'fpc',
          ''
        );
      begin
        if (use_smartlink_section and
           (aname<>'')) or (atype=sec_fpc) then
          result:=secnames[atype]+'.'+aname
        else
          result:=secnames[atype];
      end;


    function TElfObjData.sectiontype2align(atype:TAsmSectiontype):shortint;
      begin
        if atype=sec_stabstr then
          result:=1
        else
          result:=sizeof(aint);
      end;


    procedure TElfObjData.CreateDebugSections;
      begin
        stabssec:=createsection(sec_stab,'');
        stabstrsec:=createsection(sec_stabstr,'');
      end;


    procedure TElfObjData.writereloc(data,len:aint;p:TObjSymbol;relative:TObjRelocationType);
      var
        symaddr : longint;
      begin
        if CurrObjSec=nil then
          internalerror(200403292);
{$ifdef userodata}
        if CurrObjSec.sectype in [sec_rodata,sec_bss,sec_threadvar] then
          internalerror(200408252);
{$endif userodata}
        if assigned(p) then
         begin
           { real address of the symbol }
           symaddr:=p.address;
           { Local ObjSymbols can be resolved already or need a section reloc }
           if (p.bind=AB_LOCAL) then
             begin
               { For a relative relocation in the same section the
                 value can be calculated }
               if (p.objsection=CurrObjSec) and
                  (relative=RELOC_RELATIVE) then
                 inc(data,symaddr-len-CurrObjSec.Size)
               else
                 begin
                   CurrObjSec.addsectionreloc(CurrObjSec.Size,p.objsection,relative);
                   inc(data,symaddr);
                 end;
             end
           else
             begin
               CurrObjSec.addsymreloc(CurrObjSec.Size,p,relative);
{$ifndef x86_64}
               if relative=RELOC_RELATIVE then
                 dec(data,len);
{$endif x86_64}
            end;
         end;
        CurrObjSec.write(data,len);
      end;


    procedure TElfObjData.writestab(offset:aint;ps:TObjSymbol;nidx,nother:byte;ndesc:word;p:pchar);
      var
        stab : TObjStabEntry;
      begin
        if not assigned(StabsSec) then
          internalerror(200602271);
        fillchar(stab,sizeof(TObjStabEntry),0);
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=stabstrsec.Size;
           stabstrsec.write(p^,strlen(p)+1);
         end;
        stab.ntype:=nidx;
        stab.ndesc:=ndesc;
        stab.nother:=nother;
        stab.nvalue:=offset;
        stabssec.write(stab,sizeof(stab));
        if assigned(ps) then
          stabssec.addsymreloc(stabssec.Size-4,ps,RELOC_ABSOLUTE);
      end;


{****************************************************************************
                            TElfObjectOutput
****************************************************************************}

    constructor TElfObjectOutput.create(smart:boolean);
      begin
        inherited Create(smart);
        CObjData:=TElfObjData;
      end;


    procedure TElfObjectOutput.createrelocsection(s:TElfObjSection);
      var
{$ifdef ver2_0_0}
        relnative,
{$endif ver2_0_0}
        rel  : telfreloc;
        r    : TObjRelocation;
        relsym,reltyp : longint;
      begin
        with elf32data do
         begin
{$ifdef userodata}
           { rodata can't have relocations }
           if s.sectype=sec_rodata then
             begin
               if assigned(s.relocations.first) then
                 internalerror(200408251);
               exit;
             end;
{$endif userodata}
           { create the reloc section }
{$ifdef i386}
           s.relocsect:=TElfObjSection.create_ext('.rel'+s.name,9,0,symtabsect.secshidx,s.secshidx,4,8);
{$endif i386}
{$ifdef x86_64}
           s.relocsect:=TElfObjSection.create_ext('.rela'+s.name,4,0,symtabsect.secshidx,s.secshidx,4,3*8);
{$endif x86_64}
{$ifdef sparc}
           s.relocsect:=TElfObjSection.create_ext('.rel'+s.name,4,0,symtabsect.secshidx,s.secshidx,4,8);
{$endif sparc}
           { add the relocations }
           r:=TObjRelocation(s.relocations.first);
           while assigned(r) do
            begin
              rel.address:=r.dataoffset;
              if assigned(r.symbol) then
               begin
                 if (r.symbol.bind=AB_LOCAL) then
                  relsym:=r.symbol.objsection.secsymidx
                 else
                  begin
                    if r.symbol.symidx=-1 then
                      internalerror(200603012);
                    relsym:=r.symbol.symidx;
                  end;
               end
              else
               if r.objsection<>nil then
                relsym:=r.objsection.secsymidx
               else
                relsym:=SHN_UNDEF;

              { when things settle down, we can create processor specific
                derived classes
              }
{$ifdef i386}
              case r.typ of
                RELOC_RELATIVE :
                  reltyp:=R_386_PC32;
                RELOC_ABSOLUTE :
                  reltyp:=R_386_32;
              end;
{$endif i386}
{$ifdef sparc}
              case r.typ of
{                RELOC_RELATIVE :
                  reltyp:=R_386_PC32;
}
                RELOC_ABSOLUTE :
                  reltyp:=R_SPARC_32;
                else
                  internalerror(200410201);
              end;
{$endif sparc}
{$ifdef x86_64}
              case r.typ of
                RELOC_RELATIVE :
                  begin
                    reltyp:=R_X86_64_PC32;
                    { length of the relocated location is handled here }
                    rel.addend:=-4;
                  end;
              RELOC_ABSOLUTE :
                  reltyp:=R_X86_64_64;
                else
                  internalerror(200602261);
              end;
{$endif x86_64}
{$ifdef cpu64bit}
              rel.info:=(qword(relsym) shl 32) or reltyp;
{$else cpu64bit}
              rel.info:=(relsym shl 8) or reltyp;
{$endif cpu64bit}
              { write reloc }
{$ifdef ver2_0_0}
              relnative:=MaybeSwapElfReloc(rel);
              s.relocsect.write(relnative,sizeof(rel));
{$else}
              s.relocsect.write(MaybeSwapElfReloc(rel),sizeof(rel));
{$endif ver2_0_0}
              r:=TObjRelocation(r.next);
            end;
         end;
      end;


    procedure TElfObjectOutput.write_internal_symbol(astridx:longint;ainfo:byte;ashndx:word);
      var
{$ifdef ver2_0_0}
        elfsymnative,
{$endif ver2_0_0}
        elfsym : telfsymbol;
      begin
        fillchar(elfsym,sizeof(elfsym),0);
        elfsym.st_name:=astridx;
        elfsym.st_info:=ainfo;
        elfsym.st_shndx:=ashndx;
        inc(symidx);
        inc(localsyms);
{$ifdef ver2_0_0}
        elfsymnative:=MaybeSwapElfSymbol(elfsym);
        elf32data.symtabsect.write(elfsymnative,sizeof(elfsym));
{$else}
        elf32data.symtabsect.write(MaybeSwapElfSymbol(elfsym),sizeof(elfsym));
{$endif ver2_0_0}
      end;


    procedure TElfObjectOutput.section_write_symbol(p:TObject;arg:pointer);
      begin
        TObjSection(p).secsymidx:=symidx;
        write_internal_symbol(TElfObjSection(p).shstridx,STT_SECTION,TElfObjSection(p).secshidx);
      end;


    procedure TElfObjectOutput.createsymtab;
      var
{$ifdef ver2_0_0}
        elfsymnative,
{$endif}
        elfsym : telfsymbol;
        i      : longint;
        objsym : TObjSymbol;
      begin
        with elf32data do
         begin
           symidx:=0;
           localsyms:=0;
           { empty entry }
           write_internal_symbol(0,0,0);
           { filename entry }
           write_internal_symbol(1,STT_FILE,SHN_ABS);
           { section }
           ObjSectionList.ForEachCall(@section_write_symbol,nil);
           { ObjSymbols }
           for i:=0 to ObjSymbolList.Count-1 do
             begin
               objsym:=TObjSymbol(ObjSymbolList[i]);
               if objsym.bind<>AB_LOCAL then
                 begin
                   fillchar(elfsym,sizeof(elfsym),0);
                   { symbolname, write the #0 separate to overcome 255+1 char not possible }
                   elfsym.st_name:=strtabsect.Size;
                   strtabsect.writestr(objsym.name);
                   strtabsect.writestr(#0);
                   elfsym.st_size:=objsym.size;
                   case objsym.bind of
                     AB_LOCAL :
                       begin
                         elfsym.st_value:=objsym.address;
                         elfsym.st_info:=STB_LOCAL shl 4;
                         inc(localsyms);
                       end;
                     AB_COMMON :
                       begin
                         elfsym.st_value:=$10;
                         elfsym.st_info:=STB_GLOBAL shl 4;
                       end;
                     AB_EXTERNAL :
                       elfsym.st_info:=STB_GLOBAL shl 4;
                     AB_GLOBAL :
                       begin
                         elfsym.st_value:=objsym.address;
                         elfsym.st_info:=STB_GLOBAL shl 4;
                       end;
                   end;
                   if (objsym.bind<>AB_EXTERNAL) and
                      not(assigned(objsym.objsection) and
                      not(oso_data in objsym.objsection.secoptions)) then
                     begin
                       case objsym.typ of
                         AT_FUNCTION :
                           elfsym.st_info:=elfsym.st_info or STT_FUNC;
                         AT_DATA :
                           elfsym.st_info:=elfsym.st_info or STT_OBJECT;
                       end;
                     end;
                   if objsym.bind=AB_COMMON then
                     elfsym.st_shndx:=SHN_COMMON
                   else
                     begin
                       if assigned(objsym.objsection) then
                         elfsym.st_shndx:=TElfObjSection(objsym.objsection).secshidx
                       else
                         elfsym.st_shndx:=SHN_UNDEF;
                     end;
                   objsym.symidx:=symidx;
                   inc(symidx);
{$ifdef ver2_0_0}
                   elfsymnative:=MaybeSwapElfSymbol(elfsym);
                   symtabsect.write(elfsymnative,sizeof(elfsym));
{$else}
                   symtabsect.write(MaybeSwapElfSymbol(elfsym),sizeof(elfsym));
{$endif ver2_0_0}
                 end;
             end;
           { update the .symtab section header }
           symtabsect.shlink:=strtabsect.secshidx;
           symtabsect.shinfo:=localsyms;
         end;
      end;


    procedure TElfObjectOutput.section_write_sh_string(p:TObject;arg:pointer);
      begin
        TElfObjSection(p).shstridx:=elf32data.shstrtabsect.writestr(TObjSection(p).name+#0);
        if assigned(TElfObjSection(p).relocsect) then
          TElfObjSection(p).relocsect.shstridx:=elf32data.shstrtabsect.writestr(TElfObjSection(p).relocsect.name+#0);
      end;


    procedure TElfObjectOutput.createshstrtab;
      begin
        with elf32data do
         begin
           with shstrtabsect do
            begin
              writestr(#0);
              symtabsect.shstridx:=writestr('.symtab'#0);
              strtabsect.shstridx:=writestr('.strtab'#0);
              shstrtabsect.shstridx:=writestr('.shstrtab'#0);
              ObjSectionList.ForEachCall(@section_write_sh_string,nil);
            end;
         end;
      end;


    procedure TElfObjectOutput.writesectionheader(s:TElfObjSection);
      var
{$ifdef ver2_0_0}
        sechdrnative,
{$endif ver2_0_0}
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
{$ifdef ver2_0_0}
        sechdrnative:=MaybeSwapSecHeader(sechdr);
        writer.write(sechdrnative,sizeof(sechdr));
{$else}
        writer.write(MaybeSwapSecHeader(sechdr),sizeof(sechdr));
{$endif ver2_0_0}
      end;


    procedure TElfObjectOutput.writesectiondata(s:TElfObjSection);
      begin
        FWriter.writezeros(s.dataalignbytes);
        FWriter.writearray(s.data);
      end;


    procedure TElfObjectOutput.section_count_sections(p:TObject;arg:pointer);
      begin
        TElfObjSection(p).secshidx:=pword(arg)^;
        inc(pword(arg)^);
        if TElfObjSection(p).relocations.count>0 then
          inc(pword(arg)^);
      end;


    procedure TElfObjectOutput.section_create_relocsec(p:TObject;arg:pointer);
      begin
        if (TElfObjSection(p).relocations.count>0) then
          createrelocsection(TElfObjSection(p));
      end;


    procedure TElfObjectOutput.section_set_datapos(p:TObject;arg:pointer);
      begin
        TObjSection(p).setdatapos(paint(arg)^);
      end;


    procedure TElfObjectOutput.section_relocsec_set_datapos(p:TObject;arg:pointer);
      begin
        if assigned(TElfObjSection(p).relocsect) then
          TElfObjSection(p).relocsect.setdatapos(paint(arg)^);
      end;


    procedure TElfObjectOutput.section_write_data(p:TObject;arg:pointer);
      begin
        if (oso_data in TObjSection(p).secoptions) then
          begin
            if TObjSection(p).data=nil then
              internalerror(200403073);
            writesectiondata(TElfObjSection(p));
          end;
      end;


    procedure TElfObjectOutput.section_write_sechdr(p:TObject;arg:pointer);
      begin
        writesectionheader(TElfObjSection(p));
        if assigned(TElfObjSection(p).relocsect) then
          writesectionheader(TElfObjSection(p).relocsect);
      end;


    procedure TElfObjectOutput.section_write_relocsec(p:TObject;arg:pointer);
      begin
        if assigned(TElfObjSection(p).relocsect) then
          writesectiondata(TElfObjSection(p).relocsect);
      end;



    function TElfObjectOutput.writedata(data:TObjData):boolean;
      var
{$ifdef ver2_0_0}
        headernative,
{$endif ver2_0_0}
        header : telfheader;
        shoffset,
        datapos   : aint;
        nsections : word;
      begin
        result:=false;
        elf32data:=TElfObjData(data);
        with elf32data do
         begin
           { calc amount of sections we have }
           nsections:=1;
           { also create the index in the section header table }
           ObjSectionList.ForEachCall(@section_count_sections,@nsections);
           { add default sections }
           shstrtabsect.secshidx:=nsections;
           inc(nsections);
           symtabsect.secshidx:=nsections;
           inc(nsections);
           strtabsect.secshidx:=nsections;
           inc(nsections);
           { create .symtab and .strtab }
           createsymtab;
           { Create the relocation sections }
           ObjSectionList.ForEachCall(@section_create_relocsec,nil);
           { create .shstrtab }
           createshstrtab;

           { Calculate the filepositions }
           datapos:=$40; { elfheader + alignment }
           { sections first }
           ObjSectionList.ForEachCall(@section_set_datapos,@datapos);
           { shstrtab }
           shstrtabsect.setdatapos(datapos);
           { section headers }
           shoffset:=datapos;
           inc(datapos,nsections*sizeof(telfsechdr));
           { symtab }
           symtabsect.setdatapos(datapos);
           { strtab }
           strtabsect.setdatapos(datapos);
           { .rel sections }
           ObjSectionList.ForEachCall(@section_relocsec_set_datapos,@datapos);

           { Write ELF Header }
           fillchar(header,sizeof(header),0);
           header.magic0123:=$464c457f; { = #127'ELF' }
{$ifdef cpu64bit}
           header.file_class:=2;
{$else cpu64bit}
           header.file_class:=1;
{$endif cpu64bit}
           if target_info.endian=endian_big then
             header.data_encoding:=2
           else
           header.data_encoding:=1;

           header.file_version:=1;
           header.e_type:=1;
{$ifdef sparc}
           header.e_machine:=2;
{$endif sparc}
{$ifdef i386}
           header.e_machine:=3;
{$endif i386}
{$ifdef m68k}
           header.e_machine:=4;
{$endif m68k}
{$ifdef powerpc}
           header.e_machine:=20;
{$endif powerpc}
{$ifdef arm}
           header.e_machine:=40;
{$endif arm}
{$ifdef x86_64}
           header.e_machine:=62;
{$endif x86_64}
           header.e_version:=1;
           header.e_shoff:=shoffset;
           header.e_shstrndx:=shstrtabsect.secshidx;

           header.e_shnum:=nsections;
           header.e_ehsize:=sizeof(telfheader);
           header.e_shentsize:=sizeof(telfsechdr);
{$ifdef ver2_0_0}
           headernative:=MaybeSwapHeader(header);
           writer.write(headernative,sizeof(header));
{$else}
           writer.write(MaybeSwapHeader(header),sizeof(header));
{$endif ver2_0_0}
           writer.writezeros($40-sizeof(header)); { align }
         { Sections }
           ObjSectionList.ForEachCall(@section_write_data,nil);
         { .shstrtab }
           writesectiondata(shstrtabsect);
         { section headers, start with an empty header for sh_undef }
           writer.writezeros(sizeof(telfsechdr));
           ObjSectionList.ForEachCall(@section_write_sechdr,nil);
           writesectionheader(shstrtabsect);
           writesectionheader(symtabsect);
           writesectionheader(strtabsect);
         { .symtab }
           writesectiondata(symtabsect);
         { .strtab }
           writesectiondata(strtabsect);
         { .rel sections }
           ObjSectionList.ForEachCall(@section_write_relocsec,nil);
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
            supported_target : system_any;  //target_i386_linux;
            flags : [af_outputbinary,af_smartlink_sections];
            labelprefix : '.L';
            comment : '';
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
            supported_target : system_any;  //target_i386_linux;
            flags : [af_outputbinary,af_smartlink_sections];
            labelprefix : '.L';
            comment : '';
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
            supported_target : system_any;  //target_i386_linux;
//            flags : [af_outputbinary,af_smartlink_sections];
            flags : [af_outputbinary];
            labelprefix : '.L';
            comment : '';
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
