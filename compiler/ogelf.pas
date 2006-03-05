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
       TElf32ObjSection = class(TObjSection)
       public
          secshidx  : longint; { index for the section in symtab }
          shstridx,
          shtype,
          shflags,
          shlink,
          shinfo,
          shentsize : longint;
          { relocation }
          relocsect : TElf32ObjSection;
          constructor create(const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);override;
          constructor create_ext(const Aname:string;Ashtype,Ashflags,Ashlink,Ashinfo:longint;Aalign:shortint;Aentsize:longint);
          destructor  destroy;override;
       end;

       TElf32ObjData = class(TObjData)
       public
         symtabsect,
         strtabsect,
         shstrtabsect,
         gotpcsect,
         gotoffsect,
         goTSect,
         plTSect,
         symsect  : TElf32ObjSection;
         syms     : Tdynamicarray;
         constructor create(const n:string);override;
         destructor  destroy;override;
         function  sectionname(atype:TAsmSectiontype;const aname:string):string;override;
         function  sectiontype2align(atype:TAsmSectiontype):shortint;override;
         procedure CreateDebugSections;override;
         procedure writereloc(data,len:aint;p:TObjSymbol;relative:TObjRelocationType);override;
         procedure writestab(offset:aint;ps:TObjSymbol;nidx,nother:byte;ndesc:word;p:pchar);override;
       end;

       TElf32ObjectOutput = class(tObjOutput)
       private
         elf32data : TElf32ObjData;
         symidx,
         localsyms : longint;
         procedure createrelocsection(s:TElf32ObjSection);
         procedure createshstrtab;
         procedure createsymtab;
         procedure writesectionheader(s:TElf32ObjSection);
         procedure writesectiondata(s:TElf32ObjSection);
         procedure section_write_symbol(p,arg:pointer);
         procedure section_write_sh_string(p,arg:pointer);
         procedure section_count_sections(p,arg:pointer);
         procedure section_create_relocsec(p,arg:pointer);
         procedure section_set_datapos(p,arg:pointer);
         procedure section_relocsec_set_datapos(p,arg:pointer);
         procedure section_write_data(p,arg:pointer);
         procedure section_write_sechdr(p,arg:pointer);
         procedure section_write_relocsec(p,arg:pointer);
       protected
         function writedata(data:TObjData):boolean;override;
       public
         constructor Create(smart:boolean);override;
       end;

       TElf32assembler = class(tinternalassembler)
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

    constructor TElf32ObjSection.create(const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);
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


    constructor TElf32ObjSection.create_ext(const Aname:string;Ashtype,Ashflags,Ashlink,Ashinfo:longint;Aalign:shortint;Aentsize:longint);
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


    destructor TElf32ObjSection.destroy;
      begin
        if assigned(relocsect) then
          relocsect.free;
        inherited destroy;
      end;


{****************************************************************************
                            TElf32ObjData
****************************************************************************}

    constructor TElf32ObjData.create(const n:string);
      begin
        inherited create(n);
        CObjSection:=TElf32ObjSection;
        { reset }
        Syms:=TDynamicArray.Create(symbolresize);
        { default sections }
        symtabsect:=TElf32ObjSection.create_ext('.symtab',2,0,0,0,4,16);
        strtabsect:=TElf32ObjSection.create_ext('.strtab',3,0,0,0,1,0);
        shstrtabsect:=TElf32ObjSection.create_ext('.shstrtab',3,0,0,0,1,0);
        { insert the empty and filename as first in strtab }
        strtabsect.writestr(#0);
        strtabsect.writestr(SplitFileName(current_module.mainsource^)+#0);
        { we need at least the following sections }
        createsection(sec_code,'');
        createsection(sec_data,'');
        createsection(sec_bss,'');
        if tf_section_threadvars in target_info.flags then
          createsection(sec_threadvar,'');
      end;


    destructor TElf32ObjData.destroy;
      begin
        Syms.Free;
        symtabsect.free;
        strtabsect.free;
        shstrtabsect.free;
        inherited destroy;
      end;


    function TElf32ObjData.sectionname(atype:TAsmSectiontype;const aname:string):string;
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


    function TElf32ObjData.sectiontype2align(atype:TAsmSectiontype):shortint;
      begin
        if atype=sec_stabstr then
          result:=1
        else
          result:=sizeof(aint);
      end;


    procedure TElf32ObjData.CreateDebugSections;
      begin
        stabssec:=createsection(sec_stab,'');
        stabstrsec:=createsection(sec_stabstr,'');
      end;


    procedure TElf32ObjData.writereloc(data,len:aint;p:TObjSymbol;relative:TObjRelocationType);
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
               if relative=RELOC_RELATIVE then
                 dec(data,len);
            end;
         end;
        CurrObjSec.write(data,len);
      end;


    procedure TElf32ObjData.writestab(offset:aint;ps:TObjSymbol;nidx,nother:byte;ndesc:word;p:pchar);
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
                            TElf32ObjectOutput
****************************************************************************}

    constructor TElf32ObjectOutput.create(smart:boolean);
      begin
        inherited Create(smart);
        CObjData:=TElf32ObjData;
      end;


    procedure TElf32ObjectOutput.createrelocsection(s:TElf32ObjSection);
      var
        rel  : TElf32reloc;
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
           s.relocsect:=TElf32ObjSection.create_ext('.rel'+s.name,9,0,symtabsect.secshidx,s.secshidx,4,8);
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
              case r.typ of
                RELOC_RELATIVE :
                  reltyp:=R_386_PC32;
                RELOC_ABSOLUTE :
                  reltyp:=R_386_32;
              end;
              rel.info:=(relsym shl 8) or reltyp;
              { write reloc }
              s.relocsect.write(rel,sizeof(rel));
              r:=TObjRelocation(r.next);
            end;
         end;
      end;


    procedure TElf32ObjectOutput.section_write_symbol(p,arg:pointer);
      var
        elfsym : TElf32symbol;
      begin
        fillchar(elfsym,sizeof(elfsym),0);
        elfsym.st_name:=TElf32ObjSection(p).shstridx;
        elfsym.st_info:=STT_SECTION;
        elfsym.st_shndx:=TElf32ObjSection(p).secshidx;
        TObjSection(p).secsymidx:=symidx;
        inc(symidx);
        inc(localsyms);
        elf32data.symtabsect.write(elfsym,sizeof(elfsym));
      end;


    procedure TElf32ObjectOutput.createsymtab;
      var
        elfsym : TElf32symbol;
        i       : longint;
        objsym : TObjSymbol;
      begin
        with elf32data do
         begin
           symidx:=0;
           localsyms:=0;
           { empty entry }
           fillchar(elfsym,sizeof(elfsym),0);
           symtabsect.write(elfsym,sizeof(elfsym));
           inc(symidx);
           inc(localsyms);
           { filename entry }
           elfsym.st_name:=1;
           elfsym.st_info:=STT_FILE;
           elfsym.st_shndx:=SHN_ABS;
           symtabsect.write(elfsym,sizeof(elfsym));
           inc(symidx);
           inc(localsyms);
           { section }
           ObjSectionList.foreach(@section_write_symbol,nil);
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
                         elfsym.st_shndx:=TElf32ObjSection(objsym.objsection).secshidx
                       else
                         elfsym.st_shndx:=SHN_UNDEF;
                     end;
                   objsym.symidx:=symidx;
                   inc(symidx);
                   symtabsect.write(elfsym,sizeof(elfsym));
                 end;
             end;
           { update the .symtab section header }
           symtabsect.shlink:=strtabsect.secshidx;
           symtabsect.shinfo:=localsyms;
         end;
      end;


    procedure TElf32ObjectOutput.section_write_sh_string(p,arg:pointer);
      begin
        TElf32ObjSection(p).shstridx:=elf32data.shstrtabsect.writestr(TObjSection(p).name+#0);
        if assigned(TElf32ObjSection(p).relocsect) then
          TElf32ObjSection(p).relocsect.shstridx:=elf32data.shstrtabsect.writestr(TElf32ObjSection(p).relocsect.name+#0);
      end;


    procedure TElf32ObjectOutput.createshstrtab;
      begin
        with elf32data do
         begin
           with shstrtabsect do
            begin
              writestr(#0);
              symtabsect.shstridx:=writestr('.symtab'#0);
              strtabsect.shstridx:=writestr('.strtab'#0);
              shstrtabsect.shstridx:=writestr('.shstrtab'#0);
              ObjSectionList.foreach(@section_write_sh_string,nil);
            end;
         end;
      end;


    procedure TElf32ObjectOutput.writesectionheader(s:TElf32ObjSection);
      var
        sechdr : TElf32sechdr;
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
        writer.write(sechdr,sizeof(sechdr));
      end;



    procedure TElf32ObjectOutput.writesectiondata(s:TElf32ObjSection);
      begin
        FWriter.writezeros(s.dataalignbytes);
        FWriter.writearray(s.data);
      end;


    procedure TElf32ObjectOutput.section_count_sections(p,arg:pointer);
      begin
        TElf32ObjSection(p).secshidx:=pword(arg)^;
        inc(pword(arg)^);
        if TElf32ObjSection(p).relocations.count>0 then
          inc(pword(arg)^);
      end;


    procedure TElf32ObjectOutput.section_create_relocsec(p,arg:pointer);
      begin
        if (TElf32ObjSection(p).relocations.count>0) then
          createrelocsection(TElf32ObjSection(p));
      end;


    procedure TElf32ObjectOutput.section_set_datapos(p,arg:pointer);
      begin
        TObjSection(p).setdatapos(paint(arg)^);
      end;


    procedure TElf32ObjectOutput.section_relocsec_set_datapos(p,arg:pointer);
      begin
        if assigned(TElf32ObjSection(p).relocsect) then
          TElf32ObjSection(p).relocsect.setdatapos(paint(arg)^);
      end;


    procedure TElf32ObjectOutput.section_write_data(p,arg:pointer);
      begin
        if (oso_data in TObjSection(p).secoptions) then
          begin
            if TObjSection(p).data=nil then
              internalerror(200403073);
            writesectiondata(TElf32ObjSection(p));
          end;
      end;


    procedure TElf32ObjectOutput.section_write_sechdr(p,arg:pointer);
      begin
        writesectionheader(TElf32ObjSection(p));
        if assigned(TElf32ObjSection(p).relocsect) then
          writesectionheader(TElf32ObjSection(p).relocsect);
      end;


    procedure TElf32ObjectOutput.section_write_relocsec(p,arg:pointer);
      begin
        if assigned(TElf32ObjSection(p).relocsect) then
          writesectiondata(TElf32ObjSection(p).relocsect);
      end;



    function TElf32ObjectOutput.writedata(data:TObjData):boolean;
      var
        header    : TElf32header;
        shoffset,
        datapos   : aint;
        nsections : word;
      begin
        result:=false;
        elf32data:=TElf32ObjData(data);
        with elf32data do
         begin
           { calc amount of sections we have }
           nsections:=1;
           { also create the index in the section header table }
           ObjSectionList.foreach(@section_count_sections,@nsections);
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
           ObjSectionList.foreach(@section_create_relocsec,nil);
           { create .shstrtab }
           createshstrtab;

           { Calculate the filepositions }
           datapos:=$40; { elfheader + alignment }
           { sections first }
           ObjSectionList.foreach(@section_set_datapos,@datapos);
           { shstrtab }
           shstrtabsect.setdatapos(datapos);
           { section headers }
           shoffset:=datapos;
           inc(datapos,nsections*sizeof(TElf32sechdr));
           { symtab }
           symtabsect.setdatapos(datapos);
           { strtab }
           strtabsect.setdatapos(datapos);
           { .rel sections }
           ObjSectionList.foreach(@section_relocsec_set_datapos,@datapos);

           { Write ELF Header }
           fillchar(header,sizeof(header),0);
           header.magic0123:=$464c457f; { = #127'ELF' }
           header.file_class:=1;
           header.data_encoding:=1;
           header.file_version:=1;
           header.e_type:=1;
           header.e_machine:=3;
           header.e_version:=1;
           header.e_shoff:=shoffset;
           header.e_shstrndx:=shstrtabsect.secshidx;
           header.e_shnum:=nsections;
           header.e_ehsize:=sizeof(TElf32header);
           header.e_shentsize:=sizeof(TElf32sechdr);
           writer.write(header,sizeof(header));
           writer.writezeros($40-sizeof(header)); { align }
         { Sections }
           ObjSectionList.foreach(@section_write_data,nil);
         { .shstrtab }
           writesectiondata(shstrtabsect);
         { section headers, start with an empty header for sh_undef }
           writer.writezeros(sizeof(TElf32sechdr));
           ObjSectionList.foreach(@section_write_sechdr,nil);
           writesectionheader(shstrtabsect);
           writesectionheader(symtabsect);
           writesectionheader(strtabsect);
         { .symtab }
           writesectiondata(symtabsect);
         { .strtab }
           writesectiondata(strtabsect);
         { .rel sections }
           ObjSectionList.foreach(@section_write_relocsec,nil);
         end;
        result:=true;
      end;


{****************************************************************************
                               TELFAssembler
****************************************************************************}

    constructor TElf32Assembler.Create(smart:boolean);
      begin
        inherited Create(smart);
        CObjOutput:=TElf32ObjectOutput;
      end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

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


initialization
  RegisterAssembler(as_i386_elf32_info,TElf32Assembler);
end.
