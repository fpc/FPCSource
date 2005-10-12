{
    Copyright (c) 1998-2002 by Peter Vreman

    Contains the binary elf writer

    * This code was inspired by the NASM sources
      The Netwide Assembler is Copyright (c) 1996 Simon Tatham and
      Julian Hall. All rights reserved.

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
       telf32section = class(TAsmSection)
       public
          secshidx  : longint; { index for the section in symtab }
          shstridx,
          shtype,
          shflags,
          shlink,
          shinfo,
          entsize   : longint;
          { relocation }
          relocsect : telf32Section;
          constructor create(const Aname:string;Atype:TAsmSectionType;Aalign:longint;Aoptions:TAsmSectionOptions);override;
          constructor create_ext(const Aname:string;Atype:TAsmSectionType;Ashtype,Ashflags,Ashlink,Ashinfo,Aalign,Aentsize:longint);
          destructor  destroy;override;
       end;

       telf32objectdata = class(TAsmObjectData)
       public
         symtabsect,
         strtabsect,
         shstrtabsect,
         gotpcsect,
         gotoffsect,
         goTSect,
         plTSect,
         symsect  : telf32Section;
         syms     : Tdynamicarray;
         constructor create(const n:string);
         destructor  destroy;override;
         function  sectionname(atype:tasmsectiontype;const aname:string):string;override;
         procedure writereloc(data,len:aint;p:tasmsymbol;relative:TAsmRelocationType);override;
         procedure writesymbol(p:tasmsymbol);override;
         procedure writestab(offset:aint;ps:tasmsymbol;nidx,nother,line:longint;p:pchar);override;
         procedure beforealloc;override;
         procedure beforewrite;override;
       end;

       telf32objectoutput = class(tobjectoutput)
       private
         elf32data : telf32objectdata;
         initsym   : longint;
         procedure createrelocsection(s:telf32section);
         procedure createshstrtab;
         procedure createsymtab;
         procedure writesectionheader(s:telf32section);
         procedure writesectiondata(s:telf32section);
         procedure section_write_symbol(p:tnamedindexitem;arg:pointer);
         procedure section_write_sh_string(p:tnamedindexitem;arg:pointer);
         procedure section_number_symbol(p:tnamedindexitem;arg:pointer);
         procedure section_count_sects(p:tnamedindexitem;arg:pointer);
         procedure section_create_relocsec(p:tnamedindexitem;arg:pointer);
         procedure section_set_datapos(p:tnamedindexitem;arg:pointer);
         procedure section_relocsec_set_datapos(p:tnamedindexitem;arg:pointer);
         procedure section_write_data(p:tnamedindexitem;arg:pointer);
         procedure section_write_sechdr(p:tnamedindexitem;arg:pointer);
         procedure section_write_relocsec(p:tnamedindexitem;arg:pointer);
       protected
         function writedata(data:TAsmObjectData):boolean;override;
       public
         function newobjectdata(const n:string):TAsmObjectData;override;
       end;

       telf32assembler = class(tinternalassembler)
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
        telf32header=packed record
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
        telf32sechdr=packed record
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
        telf32reloc=packed record
          address : longint;
          info    : longint; { bit 0-7: type, 8-31: symbol }
        end;
        telf32symbol=packed record
          st_name  : longint;
          st_value : longint;
          st_size  : longint;
          st_info  : byte; { bit 0-3: type, 4-7: bind }
          st_other : byte;
          st_shndx : word;
        end;
        telf32stab=packed record
          strpos  : longint;
          ntype   : byte;
          nother  : byte;
          ndesc   : word;
          nvalue  : longint;
        end;


{****************************************************************************
                               TSection
****************************************************************************}

    constructor telf32section.create(const Aname:string;Atype:TAsmSectionType;Aalign:longint;Aoptions:TAsmSectionOptions);
      var
        Ashflags,Ashtype,Aentsize : longint;
      begin
        Ashflags:=0;
        Ashtype:=0;
        Aentsize:=0;
        case Atype of
          sec_code :
            begin
              Ashflags:=SHF_ALLOC or SHF_EXECINSTR;
              AshType:=SHT_PROGBITS;
              AAlign:=max(sizeof(aint),AAlign);
            end;
          sec_data :
            begin
              Ashflags:=SHF_ALLOC or SHF_WRITE;
              AshType:=SHT_PROGBITS;
              AAlign:=max(sizeof(aint),AAlign);
            end;
          sec_rodata :
            begin
{$warning TODO Remove rodata hack}
              Ashflags:=SHF_ALLOC or SHF_WRITE;
              AshType:=SHT_PROGBITS;
              AAlign:=max(sizeof(aint),AAlign);
            end;
          sec_bss,sec_threadvar :
            begin
              Ashflags:=SHF_ALLOC or SHF_WRITE;
              AshType:=SHT_NOBITS;
              AAlign:=max(sizeof(aint),AAlign);
            end;
          sec_stab :
            begin
              AshType:=SHT_PROGBITS;
              AAlign:=max(sizeof(aint),AAlign);
              Aentsize:=sizeof(telf32stab);
            end;
          sec_stabstr :
            begin
              AshType:=SHT_STRTAB;
              AAlign:=1;
            end;
          sec_fpc :
            begin
              AshFlags:=SHF_ALLOC;
              AshType:=SHT_PROGBITS ;
              AAlign:=4;// max(sizeof(aint),AAlign);
            end;
          else
            internalerror(200509122);
        end;
        create_ext(Aname,Atype,Ashtype,Ashflags,0,0,Aalign,Aentsize);
      end;


    constructor telf32section.create_ext(const Aname:string;Atype:TAsmSectionType;Ashtype,Ashflags,Ashlink,Ashinfo,Aalign,Aentsize:longint);
      var
        aoptions : TAsmSectionOptions;
      begin
        aoptions:=[];
        if (AshType=SHT_NOBITS) then
          include(aoptions,aso_alloconly);
        inherited create(Aname,Atype,Aalign,aoptions);
        secshidx:=0;
        shstridx:=0;
        shtype:=AshType;
        shflags:=AshFlags;
        shlink:=Ashlink;
        shinfo:=Ashinfo;
        entsize:=Aentsize;
        relocsect:=nil;
      end;


    destructor telf32section.destroy;
      begin
        if assigned(relocsect) then
          relocsect.free;
        inherited destroy;
      end;


{****************************************************************************
                            telf32objectdata
****************************************************************************}

    constructor telf32objectdata.create(const n:string);
      begin
        inherited create(n);
        CAsmSection:=TElf32Section;
        { reset }
        Syms:=TDynamicArray.Create(symbolresize);
        { default sections }
        symtabsect:=telf32section.create_ext('.symtab',sec_custom,2,0,0,0,4,16);
        strtabsect:=telf32section.create_ext('.strtab',sec_custom,3,0,0,0,1,0);
        shstrtabsect:=telf32section.create_ext('.shstrtab',sec_custom,3,0,0,0,1,0);
        { insert the empty and filename as first in strtab }
        strtabsect.writestr(#0);
        strtabsect.writestr(SplitFileName(current_module.mainsource^)+#0);
        { we need at least the following sections }
        createsection(sec_code,'',0,[]);
        createsection(sec_data,'',0,[]);
        createsection(sec_bss,'',0,[]);
{$ifdef segment_threadvars}
        createsection(sec_threadvar,'',0,[]);
{$endif}
        { create stabs sections if debugging }
        if (cs_debuginfo in aktmoduleswitches) then
         begin
           stabssec:=createsection(sec_stab,'',0,[]);
           stabstrsec:=createsection(sec_stabstr,'',0,[]);
         end;
      end;


    destructor telf32objectdata.destroy;
      begin
        Syms.Free;
        symtabsect.free;
        strtabsect.free;
        shstrtabsect.free;
        inherited destroy;
      end;


    function telf32objectdata.sectionname(atype:tasmsectiontype;const aname:string):string;
      const
        secnames : array[tasmsectiontype] of string[12] = ('',
{$ifdef userodata}
          '.text','.data','.rodata','.bss','.threadvar',
{$else userodata}
          '.text','.data','.data','.bss','.threadvar',
{$endif userodata}
          'common',
          '.note',
          '.text', { darwin stubs }
          '.stab','.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame',
          'fpc'
        );
      begin
        if (use_smartlink_section and
           (aname<>'')) or (atype=sec_fpc) then
          result:=secnames[atype]+'.'+aname
        else
          result:=secnames[atype];
      end;


    procedure telf32objectdata.writesymbol(p:tasmsymbol);
      begin
        if currsec=nil then
          internalerror(200403291);
        { already written ? }
        if p.indexnr<>-1 then
         exit;
        { calculate symbol index }
        if (p.currbind<>AB_LOCAL) then
         begin
           { insert the symbol in the local index, the indexarray
             will take care of the numbering }
           symbols.insert(p);
         end
        else
         p.indexnr:=-2; { local }
      end;


    procedure telf32objectdata.writereloc(data,len:aint;p:tasmsymbol;relative:TAsmRelocationType);
      var
        symaddr : longint;
      begin
        if currsec=nil then
          internalerror(200403292);
{$ifdef userodata}
        if currsec.sectype in [sec_rodata,sec_bss,sec_threadvar] then
          internalerror(200408252);
{$endif userodata}
        if assigned(p) then
         begin
           { real address of the symbol }
           symaddr:=p.address;
           { Local symbols can be resolved already or need a section reloc }
           if (p.currbind=AB_LOCAL) then
             begin
               { For a relative relocation in the same section the
                 value can be calculated }
               if (p.section=currsec) and
                  (relative=RELOC_RELATIVE) then
                 inc(data,symaddr-len-currsec.datasize)
               else
                 begin
                   currsec.addsectionreloc(currsec.datasize,p.section,relative);
                   inc(data,symaddr);
                 end;
             end
           else
             begin
               writesymbol(p);
               currsec.addsymreloc(currsec.datasize,p,relative);
               if relative=RELOC_RELATIVE then
                 dec(data,len);
            end;
         end;
        currsec.write(data,len);
      end;


    procedure telf32objectdata.writestab(offset:aint;ps:tasmsymbol;nidx,nother,line:longint;p:pchar);
      var
        stab : telf32stab;
      begin
        fillchar(stab,sizeof(telf32stab),0);
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=stabstrsec.datasize;
           stabstrsec.write(p^,strlen(p)+1);
         end;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        stabssec.write(stab,sizeof(stab));
        if assigned(ps) then
          begin
            writesymbol(ps);
            stabssec.addsymreloc(stabssec.datasize-4,ps,RELOC_ABSOLUTE);
          end;
      end;


    procedure telf32objectdata.beforealloc;
      begin
        { create stabs sections if debugging }
        if (cs_debuginfo in aktmoduleswitches) then
          begin
            StabsSec.Alloc(sizeof(telf32stab));
            StabStrSec.Alloc(length(SplitFileName(current_module.mainsource^))+2);
          end;
      end;


    procedure telf32objectdata.beforewrite;
      var
        s : string;
      begin
        { create stabs sections if debugging }
        if (cs_debuginfo in aktmoduleswitches) then
         begin
           writestab(0,nil,0,0,0,nil);
           { write zero pchar and name together (PM) }
           s:=#0+SplitFileName(current_module.mainsource^)+#0;
           stabstrsec.write(s[1],length(s));
         end;
      end;


{****************************************************************************
                            telf32objectoutput
****************************************************************************}

    function telf32objectoutput.newobjectdata(const n:string):TAsmObjectData;
      begin
        result:=telf32objectdata.create(n);
      end;


    procedure telf32objectoutput.createrelocsection(s:telf32section);
      var
        rel  : telf32reloc;
        r    : tasmrelocation;
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
           s.relocsect:=telf32section.create_ext('.rel'+s.name,sec_custom,9,0,symtabsect.secshidx,s.secshidx,4,8);
           { add the relocations }
           r:=TasmRelocation(s.relocations.first);
           while assigned(r) do
            begin
              rel.address:=r.address;
              if assigned(r.symbol) then
               begin
                 if (r.symbol.currbind=AB_LOCAL) then
                  relsym:=r.symbol.section.secsymidx
                 else
                  begin
                    if r.symbol.indexnr=-1 then
                      internalerror(4321);
                    { indexnr starts with 1, ELF starts with 0 }
                    relsym:=r.symbol.indexnr+initsym-1;
                  end;
               end
              else
               if r.section<>nil then
                relsym:=r.section.secsymidx
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
              r:=TAsmRelocation(r.next);
            end;
         end;
      end;


    procedure telf32objectoutput.section_write_symbol(p:tnamedindexitem;arg:pointer);
      var
        elfsym : telf32symbol;
      begin
        fillchar(elfsym,sizeof(elfsym),0);
        elfsym.st_name:=telf32section(p).shstridx;
        elfsym.st_info:=STT_SECTION;
        elfsym.st_shndx:=telf32section(p).secshidx;
        elf32data.symtabsect.write(elfsym,sizeof(elfsym));
        { increase locals count }
        inc(plongint(arg)^);
      end;


    procedure telf32objectoutput.createsymtab;
      var
        elfsym : telf32symbol;
        locals : longint;
        sym : tasmsymbol;
      begin
        with elf32data do
         begin
           locals:=2;
           { empty entry }
           fillchar(elfsym,sizeof(elfsym),0);
           symtabsect.write(elfsym,sizeof(elfsym));
           { filename entry }
           elfsym.st_name:=1;
           elfsym.st_info:=STT_FILE;
           elfsym.st_shndx:=SHN_ABS;
           symtabsect.write(elfsym,sizeof(elfsym));
           { section }
           sects.foreach(@section_write_symbol,@locals);
           { symbols }
           sym:=Tasmsymbol(symbols.First);
           while assigned(sym) do
            begin
              fillchar(elfsym,sizeof(elfsym),0);
              { symbolname, write the #0 separate to overcome 255+1 char not possible }
              elfsym.st_name:=strtabsect.datasize;
              strtabsect.writestr(sym.name);
              strtabsect.writestr(#0);
              case sym.currbind of
                AB_LOCAL,
                AB_GLOBAL :
                 elfsym.st_value:=sym.address;
                AB_COMMON :
                 elfsym.st_value:=$10;
              end;
              elfsym.st_size:=sym.size;
              case sym.currbind of
                AB_LOCAL :
                  begin
                    elfsym.st_info:=STB_LOCAL shl 4;
                    inc(locals);
                  end;
                AB_COMMON,
                AB_EXTERNAL,
                AB_GLOBAL :
                  elfsym.st_info:=STB_GLOBAL shl 4;
              end;
              if (sym.currbind<>AB_EXTERNAL) and
                 not(assigned(sym.section) and
                     (sym.section.sectype=sec_bss)) then
               begin
                 case sym.typ of
                   AT_FUNCTION :
                     elfsym.st_info:=elfsym.st_info or STT_FUNC;
                   AT_DATA :
                     elfsym.st_info:=elfsym.st_info or STT_OBJECT;
                 end;
               end;
              if sym.currbind=AB_COMMON then
               elfsym.st_shndx:=SHN_COMMON
              else
               if assigned(sym.section) then
                elfsym.st_shndx:=telf32section(sym.section).secshidx
               else
                elfsym.st_shndx:=SHN_UNDEF;
              symtabsect.write(elfsym,sizeof(elfsym));
              sym:=tasmsymbol(sym.indexnext);
            end;
           { update the .symtab section header }
           symtabsect.shlink:=strtabsect.secshidx;
           symtabsect.shinfo:=locals;
         end;
      end;


    procedure telf32objectoutput.section_write_sh_string(p:tnamedindexitem;arg:pointer);
      begin
        telf32section(p).shstridx:=elf32data.shstrtabsect.writestr(tasmsection(p).name+#0);
        if assigned(telf32section(p).relocsect) then
          telf32section(p).relocsect.shstridx:=elf32data.shstrtabsect.writestr(telf32section(p).relocsect.name+#0);
      end;


    procedure telf32objectoutput.createshstrtab;
      begin
        with elf32data do
         begin
           with shstrtabsect do
            begin
              writestr(#0);
              symtabsect.shstridx:=writestr('.symtab'#0);
              strtabsect.shstridx:=writestr('.strtab'#0);
              shstrtabsect.shstridx:=writestr('.shstrtab'#0);
              sects.foreach(@section_write_sh_string,nil);
            end;
         end;
      end;


    procedure telf32objectoutput.writesectionheader(s:telf32section);
      var
        sechdr : telf32sechdr;
      begin
        fillchar(sechdr,sizeof(sechdr),0);
        sechdr.sh_name:=s.shstridx;
        sechdr.sh_type:=s.shtype;
        sechdr.sh_flags:=s.shflags;
        sechdr.sh_offset:=s.datapos;
        sechdr.sh_size:=s.datasize;
        sechdr.sh_link:=s.shlink;
        sechdr.sh_info:=s.shinfo;
        sechdr.sh_addralign:=s.addralign;
        sechdr.sh_entsize:=s.entsize;
        writer.write(sechdr,sizeof(sechdr));
      end;



    procedure telf32objectoutput.writesectiondata(s:telf32section);
      var
        hp : pdynamicblock;
      begin
        FWriter.writezeros(s.dataalignbytes);
        s.alignsection;
        hp:=s.data.firstblock;
        while assigned(hp) do
          begin
            FWriter.write(hp^.data,hp^.used);
            hp:=hp^.next;
          end;
      end;


    procedure telf32objectoutput.section_number_symbol(p:tnamedindexitem;arg:pointer);
      begin
        tasmsection(p).secsymidx:=plongint(arg)^;
        inc(plongint(arg)^);
      end;


    procedure telf32objectoutput.section_count_sects(p:tnamedindexitem;arg:pointer);
      begin
        telf32section(p).secshidx:=plongint(arg)^;
        inc(plongint(arg)^);
        if telf32section(p).relocations.count>0 then
          inc(plongint(arg)^);
      end;


    procedure telf32objectoutput.section_create_relocsec(p:tnamedindexitem;arg:pointer);
      begin
        if (telf32section(p).relocations.count>0) then
          createrelocsection(telf32section(p));
      end;


    procedure telf32objectoutput.section_set_datapos(p:tnamedindexitem;arg:pointer);
      begin
        if (aso_alloconly in tasmsection(p).secoptions) then
          tasmsection(p).datapos:=paint(arg)^
        else
          tasmsection(p).setdatapos(paint(arg)^);
      end;


    procedure telf32objectoutput.section_relocsec_set_datapos(p:tnamedindexitem;arg:pointer);
      begin
        if assigned(telf32section(p).relocsect) then
          telf32section(p).relocsect.setdatapos(paint(arg)^);
      end;


    procedure telf32objectoutput.section_write_data(p:tnamedindexitem;arg:pointer);
      begin
        if (aso_alloconly in tasmsection(p).secoptions) then
          exit;
        if tasmsection(p).data=nil then
          internalerror(200403073);
        writesectiondata(telf32section(p));
      end;


    procedure telf32objectoutput.section_write_sechdr(p:tnamedindexitem;arg:pointer);
      begin
        writesectionheader(telf32section(p));
        if assigned(telf32section(p).relocsect) then
          writesectionheader(telf32section(p).relocsect);
      end;


    procedure telf32objectoutput.section_write_relocsec(p:tnamedindexitem;arg:pointer);
      begin
        if assigned(telf32section(p).relocsect) then
          writesectiondata(telf32section(p).relocsect);
      end;



    function telf32objectoutput.writedata(data:TAsmObjectData):boolean;
      var
        header : telf32header;
        datapos : aint;
        shoffset,
        nsects : longint;
        hstab  : telf32stab;
        empty  : array[0..63] of byte;
      begin
        result:=false;
        elf32data:=telf32objectdata(data);
        with elf32data do
         begin
           { calc amount of sections we have }
           initsym:=2;
           nsects:=1;
           fillchar(empty,sizeof(empty),0);
           { each section requires a symbol for relocation }
           sects.foreach(@section_number_symbol,@initsym);
           { also create the index in the section header table }
           sects.foreach(@section_count_sects,@nsects);
           { add default sections follows }
           shstrtabsect.secshidx:=nsects;
           inc(nsects);
           symtabsect.secshidx:=nsects;
           inc(nsects);
           strtabsect.secshidx:=nsects;
           inc(nsects);
         { For the stab section we need an HdrSym which can now be
           calculated more easily }
           if assigned(stabssec) then
            begin
              hstab.strpos:=1;
              hstab.ntype:=0;
              hstab.nother:=0;
              hstab.ndesc:=(stabssec.datasize div sizeof(telf32stab))-1{+1 according to gas output PM};
              hstab.nvalue:=stabstrsec.datasize;
              stabssec.Data.seek(0);
              stabssec.Data.write(hstab,sizeof(hstab));
            end;
         { Create the relocation sections }
           sects.foreach(@section_create_relocsec,nil);
         { create .symtab and .strtab }
           createsymtab;
         { create .shstrtab }
           createshstrtab;
         { Calculate the filepositions }
           datapos:=$40; { elfheader + alignment }
           { sections first }
           sects.foreach(@section_set_datapos,@datapos);
           { shstrtab }
           shstrtabsect.setdatapos(datapos);
           { section headers }
           shoffset:=datapos;
           inc(datapos,nsects*sizeof(telf32sechdr));
           { symtab }
           symtabsect.setdatapos(datapos);
           { strtab }
           strtabsect.setdatapos(datapos);
           { .rel sections }
           sects.foreach(@section_relocsec_set_datapos,@datapos);
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
           header.e_shnum:=nsects;
           header.e_ehsize:=sizeof(telf32header);
           header.e_shentsize:=sizeof(telf32sechdr);
           writer.write(header,sizeof(header));
           writer.write(empty,$40-sizeof(header)); { align }
         { Sections }
           sects.foreach(@section_write_data,nil);
         { .shstrtab }
           writesectiondata(shstrtabsect);
         { section headers, start with an empty header for sh_undef }
           writer.write(empty,sizeof(telf32sechdr));
           sects.foreach(@section_write_sechdr,nil);
           writesectionheader(shstrtabsect);
           writesectionheader(symtabsect);
           writesectionheader(strtabsect);
         { .symtab }
           writesectiondata(symtabsect);
         { .strtab }
           writesectiondata(strtabsect);
         { .rel sections }
           sects.foreach(@section_write_relocsec,nil);
         end;
        result:=true;
      end;


{****************************************************************************
                               TELFAssembler
****************************************************************************}

    constructor TELF32Assembler.Create(smart:boolean);
      begin
        inherited Create(smart);
        objectoutput:=telf32objectoutput.create(smart);
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
