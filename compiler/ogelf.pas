{
    $Id$
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
       cclasses,
       { target }
       systems,
       { assembler }
       cpubase,aasmbase,assemble,
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
          constructor createsec(sec:TSection);
          constructor createname(const Aname:string;Atype,Aflags,Alink,Ainfo,Aalign,Aentsize:longint);
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
         procedure createsection(sec:TSection);override;
         procedure seTSectionsizes(var s:TAsmSectionSizes);override;
         procedure writereloc(data,len:longint;p:tasmsymbol;relative:TAsmRelocationType);override;
         procedure writesymbol(p:tasmsymbol);override;
         procedure writestabs(section:TSection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);override;
         procedure writesymstabs(section:TSection;offset:longint;p:pchar;ps:tasmsymbol;
                                 nidx,nother,line:longint;reloc:boolean);override;
       end;

       telf32objectoutput = class(tobjectoutput)
       private
         elf32data : telf32objectdata;
         initsym   : longint;
         procedure createrelocsection(s:telf32section);
         procedure createshstrtab;
         procedure createsymtab;
         procedure writesectionheader(s:telf32section);
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
{$ifdef delphi}
        sysutils,
{$else}
        strings,
{$endif}
        verbose,
        globtype,cutils,globals,fmodule;

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

    constructor telf32section.createsec(sec:TSection);
      var
        Aflags,Atype,Aalign,Aentsize : longint;
      begin
        Aflags:=0;
        Atype:=0;
        Aalign:=0;
        Aentsize:=0;
        case sec of
          sec_code :
            begin
              Aflags:=SHF_ALLOC or SHF_EXECINSTR;
              AType:=SHT_PROGBITS;
              AAlign:=16;
            end;
          sec_data :
            begin
              Aflags:=SHF_ALLOC or SHF_WRITE;
              AType:=SHT_PROGBITS;
              AAlign:=4;
            end;
          sec_bss :
            begin
              Aflags:=SHF_ALLOC or SHF_WRITE;
              AType:=SHT_NOBITS;
              AAlign:=4;
            end;
          sec_stab :
            begin
              AType:=SHT_PROGBITS;
              AAlign:=4;
              Aentsize:=sizeof(telf32stab);
            end;
          sec_stabstr :
            begin
              AType:=SHT_STRTAB;
              AAlign:=1;
            end;
        end;
        createname(target_asm.secnames[sec],Atype,Aflags,0,0,Aalign,Aentsize);
      end;


    constructor telf32section.createname(const Aname:string;Atype,Aflags,Alink,Ainfo,Aalign,Aentsize:longint);
      begin
        inherited create(Aname,Aalign,(AType=SHT_NOBITS));
        secshidx:=0;
        shstridx:=0;
        shtype:=AType;
        shflags:=AFlags;
        shlink:=Alink;
        shinfo:=Ainfo;
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
      var
        s : string;
      begin
        inherited create(n);
        { reset }
        Syms:=TDynamicArray.Create(symbolresize);
        { default sections }
        symtabsect:=telf32section.createname('.symtab',2,0,0,0,4,16);
        strtabsect:=telf32section.createname('.strtab',3,0,0,0,1,0);
        shstrtabsect:=telf32section.createname('.shstrtab',3,0,0,0,1,0);
        { insert the empty and filename as first in strtab }
        strtabsect.writestr(#0);
        strtabsect.writestr(SplitFileName(current_module.mainsource^)+#0);
        { we need at least the following sections }
        createsection(sec_code);
        createsection(sec_data);
        createsection(sec_bss);
        { create stabs sections if debugging }
        if (cs_debuginfo in aktmoduleswitches) then
         begin
           createsection(sec_stab);
           createsection(sec_stabstr);
           writestabs(sec_none,0,nil,0,0,0,false);
           { write zero pchar and name together (PM) }
           s:=#0+SplitFileName(current_module.mainsource^)+#0;
           sects[sec_stabstr].write(s[1],length(s));
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


    procedure telf32objectdata.createsection(sec:TSection);
      begin
        sects[sec]:=telf32Section.createsec(Sec);
      end;


    procedure telf32objectdata.writesymbol(p:tasmsymbol);
      begin
        { already written ? }
        if p.indexnr<>-1 then
         exit;
        { be sure that the section will exists }
        if (p.section<>sec_none) and not(assigned(sects[p.section])) then
          createsection(p.section);
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


    procedure telf32objectdata.writereloc(data,len:longint;p:tasmsymbol;relative:TAsmRelocationType);
      var
        symaddr : longint;
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        if assigned(p) then
         begin
           { real address of the symbol }
           symaddr:=p.address;
           { no symbol relocation need inside a section }
           if p.section=currsec then
             begin
               case relative of
                 RELOC_ABSOLUTE :
                   begin
                     sects[currsec].addsectionreloc(sects[currsec].datasize,currsec,RELOC_ABSOLUTE);
                     inc(data,symaddr);
                   end;
                 RELOC_RELATIVE :
                   begin
                     inc(data,symaddr-len-sects[currsec].datasize);
                   end;
                 RELOC_RVA :
                   internalerror(3219583);
               end;
             end
           else
             begin
               writesymbol(p);
               if (p.section<>sec_none) and (relative<>RELOC_RELATIVE) then
                begin
                  sects[currsec].addsectionreloc(sects[currsec].datasize,p.section,relative);
                  inc(data,symaddr);
                end
               else
                sects[currsec].addsymreloc(sects[currsec].datasize,p,relative);
               if relative=RELOC_RELATIVE then
                begin
                  if p.currbind=AB_EXTERNAL then
                   dec(data,len)
                  else
                   dec(data,len+sects[currsec].datasize);
                end;
            end;
         end;
        sects[currsec].write(data,len);
      end;


    procedure telf32objectdata.writestabs(section:TSection;offset:longint;p:pchar;nidx,nother,line:longint;reloc : boolean);
      var
        stab : telf32stab;
      begin
        if reloc then
         begin
           if (offset=-1) then
            begin
              if section=sec_none then
               offset:=0
              else
               offset:=sects[section].datasize;
            end;
         end;
        fillchar(stab,sizeof(telf32stab),0);
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr].datasize;
           sects[sec_stabstr].write(p^,strlen(p)+1);
         end;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        sects[sec_stab].write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
         sects[sec_stab].addsectionreloc(sects[sec_stab].datasize-4,section,RELOC_ABSOLUTE);
      end;


    procedure telf32objectdata.writesymstabs(section:TSection;offset:longint;p:pchar;ps:tasmsymbol;
                                             nidx,nother,line:longint;reloc:boolean);
      var
        stab : telf32stab;
      begin
        fillchar(stab,sizeof(telf32stab),0);
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr].datasize;
           sects[sec_stabstr].write(p^,strlen(p)+1);
         end;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=0;
        sects[sec_stab].write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
         sects[sec_stab].addsymreloc(sects[sec_stab].datasize-4,ps,RELOC_ABSOLUTE);
      end;


    procedure telf32objectdata.seTSectionsizes(var s:TAsmSectionSizes);
      begin
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
           { create the reloc section }
           s.relocsect:=telf32section.createname('.rel'+s.name,9,0,symtabsect.secshidx,s.secshidx,4,8);
           { add the relocations }
           r:=TasmRelocation(s.relocations.first);
           while assigned(r) do
            begin
              rel.address:=r.address;
              if assigned(r.symbol) then
               begin
                 if (r.symbol.currbind=AB_LOCAL) then
                  relsym:=sects[r.symbol.section].secsymidx
                 else
                  begin
                    if r.symbol.indexnr=-1 then
                      internalerror(4321);
                    { indexnr starts with 1, ELF starts with 0 }
                    relsym:=r.symbol.indexnr+initsym-1;
                  end;
               end
              else
               if r.section<>sec_none then
                relsym:=sects[r.section].secsymidx
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


    procedure telf32objectoutput.createsymtab;
      var
        elfsym : telf32symbol;
        sec : TSection;
        locals,
        i : longint;
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
           for sec:=low(TSection) to high(TSection) do
            if assigned(sects[sec]) then
             begin
               fillchar(elfsym,sizeof(elfsym),0);
               elfsym.st_name:=telf32section(sects[sec]).shstridx;
               elfsym.st_info:=STT_SECTION;
               elfsym.st_shndx:=telf32section(sects[sec]).secshidx;
               symtabsect.write(elfsym,sizeof(elfsym));
               inc(locals);
             end;
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
              if sym.currbind<>AB_EXTERNAL then
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
               if assigned(sects[sym.section]) then
                elfsym.st_shndx:=telf32section(sects[sym.section]).secshidx
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


    procedure telf32objectoutput.createshstrtab;
      var
        sec : TSection;
      begin
        with elf32data do
         begin
           with shstrtabsect do
            begin
              writestr(#0);
              symtabsect.shstridx:=writestr('.symtab'#0);
              strtabsect.shstridx:=writestr('.strtab'#0);
              shstrtabsect.shstridx:=writestr('.shstrtab'#0);
              for sec:=low(TSection) to high(TSection) do
               if assigned(sects[sec]) then
                begin
                  telf32section(sects[sec]).shstridx:=writestr(sects[sec].name+#0);
                  if assigned(telf32section(sects[sec]).relocsect) then
                   telf32section(sects[sec]).relocsect.shstridx:=writestr(telf32section(sects[sec]).relocsect.name+#0);
                end;
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


    function telf32objectoutput.writedata(data:TAsmObjectData):boolean;
      var
        header : telf32header;
        datapos,
        shoffset,
        nsects : longint;
        hstab  : telf32stab;
        sec    : TSection;
        empty  : array[0..63] of byte;
        hp     : pdynamicblock;
      begin
        result:=false;
        elf32data:=telf32objectdata(data);
        with elf32data do
         begin
         { calc amount of sections we have }
           fillchar(empty,sizeof(empty),0);
           nsects:=1;
           initsym:=2;
           for sec:=low(TSection) to high(TSection) do
            if assigned(sects[sec]) then
             begin
               { each section requires a symbol for relocation }
               sects[sec].secsymidx:=initsym;
               inc(initsym);
               { also create the index in the section header table }
               telf32section(sects[sec]).secshidx:=nsects;
               inc(nsects);
               if sects[sec].relocations.count>0 then
                inc(nsects);
             end;
           { add default sections follows }
           shstrtabsect.secshidx:=nsects;
           inc(nsects);
           symtabsect.secshidx:=nsects;
           inc(nsects);
           strtabsect.secshidx:=nsects;
           inc(nsects);
         { For the stab section we need an HdrSym which can now be
           calculated more easily }
           if assigned(sects[sec_stab]) then
            begin
              hstab.strpos:=1;
              hstab.ntype:=0;
              hstab.nother:=0;
              hstab.ndesc:=(sects[sec_stab].datasize div sizeof(telf32stab))-1{+1 according to gas output PM};
              hstab.nvalue:=sects[sec_stabstr].datasize;
              sects[sec_stab].Data.seek(0);
              sects[sec_stab].Data.write(hstab,sizeof(hstab));
            end;
         { Create the relocation sections }
           for sec:=low(TSection) to high(TSection) do
            if assigned(sects[sec]) and
               (sects[sec].relocations.count>0) then
              createrelocsection(telf32section(sects[sec]));
         { create .symtab and .strtab }
           createsymtab;
         { create .shstrtab }
           createshstrtab;
         { Calculate the filepositions }
           datapos:=$40; { elfheader + alignment }
           { sections first }
           for sec:=low(TSection) to high(TSection) do
            if assigned(sects[sec]) then
             begin
               sects[sec].datapos:=datapos;
               if assigned(sects[sec].data) then
                 inc(datapos,sects[sec].aligneddatasize);
             end;
           { shstrtab }
           shstrtabsect.datapos:=datapos;
           inc(datapos,shstrtabsect.aligneddatasize);
           { section headers }
           shoffset:=datapos;
           inc(datapos,nsects*sizeof(telf32sechdr));
           { symtab }
           symtabsect.datapos:=datapos;
           inc(datapos,symtabsect.aligneddatasize);
           { strtab }
           strtabsect.datapos:=datapos;
           inc(datapos,align(strtabsect.datasize,4));
           { .rel sections }
           for sec:=low(TSection) to high(TSection) do
            if assigned(sects[sec]) and
               assigned(telf32section(sects[sec]).relocsect) then
             begin
               telf32section(sects[sec]).relocsect.datapos:=datapos;
               inc(datapos,telf32section(sects[sec]).relocsect.aligneddatasize);
             end;
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
           for sec:=low(TSection) to high(TSection) do
            if assigned(sects[sec]) and
               assigned(sects[sec].data) then
             begin
               sects[sec].alignsection;
               hp:=sects[sec].Data.firstblock;
               while assigned(hp) do
                begin
                  writer.write(hp^.data,hp^.used);
                  hp:=hp^.next;
                end;
             end;
         { .shstrtab }
           shstrtabsect.alignsection;
           hp:=shstrtabsect.Data.firstblock;
           while assigned(hp) do
            begin
              writer.write(hp^.data,hp^.used);
              hp:=hp^.next;
            end;
         { section headers, start with an empty header for sh_undef }
           writer.write(empty,sizeof(telf32sechdr));
           for sec:=low(TSection) to high(TSection) do
            if assigned(sects[sec]) then
             begin
               writesectionheader(telf32section(sects[sec]));
               if assigned(telf32section(sects[sec]).relocsect) then
                writesectionheader(telf32section(sects[sec]).relocsect);
             end;
           writesectionheader(shstrtabsect);
           writesectionheader(symtabsect);
           writesectionheader(strtabsect);
         { .symtab }
           symtabsect.alignsection;
           hp:=symtabsect.Data.firstblock;
           while assigned(hp) do
            begin
              writer.write(hp^.data,hp^.used);
              hp:=hp^.next;
            end;
         { .strtab }
           strtabsect.writealign(4);
           hp:=strtabsect.Data.firstblock;
           while assigned(hp) do
            begin
              writer.write(hp^.data,hp^.used);
              hp:=hp^.next;
            end;
         { .rel sections }
           for sec:=low(TSection) to high(TSection) do
            if assigned(sects[sec]) and
               assigned(telf32section(sects[sec]).relocsect) then
             begin
               telf32section(sects[sec]).relocsect.alignsection;
               hp:=telf32section(sects[sec]).relocsect.Data.firstblock;
               while assigned(hp) do
                begin
                  writer.write(hp^.data,hp^.used);
                  hp:=hp^.next;
                end;
             end;
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
            outputbinary : true;
            allowdirect : false;
            externals : true;
            needar : false;
            labelprefix_only_inside_procedure: false;
            labelprefix : '.L';
            comment : '';
            secnames : ('',
              '.text','.data','.bss',
              '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
              '.stab','.stabstr','')
          );


initialization
  RegisterAssembler(as_i386_elf32_info,TElf32Assembler);
end.
{
  $Log$
  Revision 1.17  2002-07-26 21:15:39  florian
    * rewrote the system handling

  Revision 1.16  2002/07/01 18:46:24  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.15  2002/05/18 13:34:10  peter
    * readded missing revisions

  Revision 1.14  2002/05/16 19:46:39  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.12  2002/05/12 16:53:08  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.11  2002/04/04 19:05:58  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

}
