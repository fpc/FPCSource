{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    Contains the binary elf writer

    * This code was inspired by the NASM sources
      The Netwide Assembler is copyright (C) 1996 Simon Tatham and
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
unit og386elf;

  interface

    uses
       cobjects,
       systems,cpubase,aasm,og386;

    type
       preloc = ^treloc;
       treloc = packed record
          next     : preloc;
          address  : longint;
          symbol   : pasmsymbol;
          section  : tsection; { only used if symbol=nil }
          typ      : relative_type;
       end;

       psymbol = ^tsymbol;
       tsymbol = packed record
         name    : longint;
         section : tsection;
         value   : longint;
         bind    : TAsmsymbind;
         typ     : TAsmsymtype;
         size    : longint;
       end;

       pelf32section = ^telf32section;
       telf32section = object
          name      : string[16];
          secshidx,
          secsymidx : longint; { index for the section in symtab }
          shstridx,
          shtype,
          shflags,
          shlink,
          shinfo,
          addralign,
          entsize   : longint;
          { size of the data and in the file }
          data  : PDynamicArray;
          datalen,
          datapos   : longint;
          { settings after setsectionsize }
          size      : longint;
          fillsize  : longint;
          { relocation }
          nrelocs   : longint;
          relochead : PReloc;
          reloctail : ^PReloc;
          relocsect : PElf32Section;
          constructor init(sec:TSection);
          constructor initname(const Aname:string;Atype,Aflags,Alink,Ainfo,Aalign,Aentsize:longint);
          destructor  done;
          function  write(var d;l:longint):longint;
          function  writestr(const s:string):longint;
          procedure writealign(l:longint);
          function  aligneddatalen:longint;
          procedure alignsection;
          procedure alloc(l:longint);
          procedure addsymreloc(ofs:longint;p:pasmsymbol;relative:relative_type);
          procedure addsectionreloc(ofs:longint;sec:tsection;relative:relative_type);
       end;

       pelf32output = ^telf32output;
       telf32output = object(tobjectoutput)
         sects   : array[TSection] of pelf32Section;
         symtabsect,
         strtabsect,
         shstrtabsect,
         gotpcsect,
         gotoffsect,
         gotsect,
         pltsect,
         symsect  : pelf32Section;
         strs,
         syms    : Pdynamicarray;
         initsym : longint;
         constructor init(smart:boolean);
         destructor  done;virtual;
         procedure initwriting(Aplace:tcutplace);virtual;
         procedure donewriting;virtual;
         function  sectionsize(s:tsection):longint;virtual;
         procedure setsectionsizes(var s:tsecsize);virtual;
         procedure writebytes(var data;len:longint);virtual;
         procedure writealloc(len:longint);virtual;
         procedure writealign(len:longint);virtual;
         procedure writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);virtual;
         procedure writesymbol(p:pasmsymbol);virtual;
         procedure writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);virtual;
         procedure writesymstabs(section:tsection;offset:longint;p:pchar;ps:pasmsymbol;
           nidx,nother,line:longint;reloc:boolean);virtual;
       private
         procedure createsection(sec:tsection);
         procedure createrelocsection(s:pelf32section);
         procedure createshstrtab;
         procedure createsymtab;
         procedure writesectionheader(s:pelf32section);
         procedure writetodisk;
       end;


  implementation

      uses
        strings,verbose,
        globtype,globals,files;

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
        pelf32reloc=^telf32reloc;
        telf32reloc=packed record
          address : longint;
          info    : longint; { bit 0-7: type, 8-31: symbol }
        end;
        pelf32symbol=^telf32symbol;
        telf32symbol=packed record
          st_name  : longint;
          st_value : longint;
          st_size  : longint;
          st_info  : byte; { bit 0-3: type, 4-7: bind }
          st_other : byte;
          st_shndx : word;
        end;
        pelf32stab=^telf32stab;
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

    constructor telf32section.init(sec:TSection);
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
        initname(target_asm.secnames[sec],Atype,Aflags,0,0,Aalign,Aentsize);
      end;


    constructor telf32section.initname(const Aname:string;Atype,Aflags,Alink,Ainfo,Aalign,Aentsize:longint);
      begin
        name:=Aname;
        secshidx:=0;
        secsymidx:=0;
        shstridx:=0;
        shtype:=AType;
        shflags:=AFlags;
        shlink:=Alink;
        shinfo:=Ainfo;
        addralign:=Aalign;
        entsize:=Aentsize;
        { setsectionsize data }
        fillsize:=0;
        size:=0;
        { data }
        dataLen:=0;
        dataPos:=0;
        if shtype=SHT_NOBITS then
         data:=nil
        else
         new(Data,Init(1,8192));
        { relocation }
        NRelocs:=0;
        relocHead:=nil;
        relocTail:=@relocHead;
        relocsect:=nil;
      end;


    destructor telf32section.done;
      begin
        if assigned(Data) then
          dispose(Data,done);
      end;


    function telf32section.write(var d;l:longint):longint;
      begin
        write:=datalen;
        if not assigned(Data) then
         Internalerror(3334441);
        Data^.write(d,l);
        inc(datalen,l);
      end;


    function telf32section.writestr(const s:string):longint;
      begin
        writestr:=datalen;
        if not assigned(Data) then
         Internalerror(3334441);
        Data^.write(s[1],length(s));
        inc(datalen,length(s));
      end;


    procedure telf32section.writealign(l:longint);
      var
        i : longint;
        empty : array[0..63] of char;
      begin
        { no alignment needed for 0 or 1 }
        if l<=1 then
         exit;
        i:=datalen mod l;
        if i>0 then
         begin
           if assigned(data) then
            begin
              fillchar(empty,sizeof(empty),0);
              data^.write(empty,l-i);
            end;
           inc(datalen,l-i);
         end;
      end;


    function telf32section.aligneddatalen:longint;
      begin
        aligneddatalen:=align(datalen,addralign);
      end;


    procedure telf32section.alignsection;
      begin
        writealign(addralign);
      end;


    procedure telf32section.alloc(l:longint);
      begin
        if assigned(Data) then
         Internalerror(3334442);
        inc(datalen,l);
      end;


    procedure telf32section.addsymreloc(ofs:longint;p:pasmsymbol;relative:relative_type);
      var
        r : PReloc;
      begin
        new(r);
        reloctail^:=r;
        reloctail:=@r^.next;
        r^.next:=nil;
        r^.address:=ofs;
        r^.symbol:=p;
        r^.section:=sec_none;
        r^.typ:=relative;
        inc(nrelocs);
      end;


    procedure telf32section.addsectionreloc(ofs:longint;sec:tsection;relative:relative_type);
      var
        r : PReloc;
      begin
        new(r);
        reloctail^:=r;
        reloctail:=@r^.next;
        r^.next:=nil;
        r^.address:=ofs;
        r^.symbol:=nil;
        r^.section:=sec;
        r^.typ:=relative;
        inc(nrelocs);
      end;


{****************************************************************************
                            TElf32Output
****************************************************************************}

    const
{$ifdef TP}
      symbolresize = 50;
      strsresize   = 200;
{$else}
      symbolresize = 200;
      strsresize   = 8192;
{$endif}

    constructor telf32output.init(smart:boolean);
      begin
        inherited init(smart);
      end;


    destructor telf32output.done;
      begin
        inherited done;
      end;


    procedure telf32output.initwriting(Aplace:tcutplace);
      var
        s : string;
      begin
        inherited initwriting(Aplace);
        { reset }
        initsym:=0;
        new(syms,init(sizeof(TSymbol),symbolresize));
        FillChar(Sects,sizeof(Sects),0);
        { default sections }
        new(symtabsect,initname('.symtab',2,0,0,0,4,16));
        new(strtabsect,initname('.strtab',3,0,0,0,1,0));
        new(shstrtabsect,initname('.shstrtab',3,0,0,0,1,0));
        { insert the empty and filename as first in strtab }
        strtabsect^.writestr(#0);
        strtabsect^.writestr(SplitFileName(current_module^.mainsource^)+#0);
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
           s:=#0+SplitFileName(current_module^.mainsource^)+#0;
           sects[sec_stabstr]^.write(s[1],length(s));
         end;
      end;


    procedure telf32output.donewriting;
      var
        sec : tsection;
      begin
        writetodisk;
        dispose(syms,done);
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          dispose(sects[sec],done);
        inherited donewriting;
      end;


    function telf32output.sectionsize(s:tsection):longint;
      begin
        if assigned(sects[s]) then
         sectionsize:=sects[s]^.datalen
        else
         sectionsize:=0;
      end;


    procedure telf32output.createsection(sec:tsection);
      begin
        sects[sec]:=new(pelf32Section,init(Sec));
      end;


    procedure telf32output.writesymbol(p:pasmsymbol);
      var
        sym : tsymbol;
      begin
        { already written ? }
        if p^.idx<>-1 then
         exit;
        { be sure that the section will exists }
        if (p^.section<>sec_none) and not(assigned(sects[p^.section])) then
          createsection(p^.section);
        FillChar(sym,sizeof(sym),0);
        sym.size:=p^.size;
        sym.bind:=p^.bind;
        sym.typ:=p^.typ;
        { if local of global then set the section value to the address
          of the symbol }
        case sym.bind of
          AB_LOCAL,
          AB_GLOBAL :
            begin
              sym.section:=p^.section;
              sym.value:=p^.address;
            end;
          AB_COMMON :
            begin
              sym.value:=$10;
            end;
        end;
        { update the asmsymbol index }
        p^.idx:=syms^.count;
        { store the symbol, but not the local ones (PM) }
        if (sym.bind<>AB_LOCAL) then
         begin
           { symbolname, write the #0 separate to overcome 255+1 char not possible }
           sym.name:=strtabsect^.writestr(p^.name);
           strtabsect^.writestr(#0);
           { symbol }
           syms^.write(sym,1);
         end;
        { make the exported syms known to the objectwriter
          (needed for .a generation) }
        if (sym.bind in [AB_GLOBAL,AB_COMMON]) then
          writer^.writesym(p^.name);
      end;


    procedure telf32output.writebytes(var data;len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec]^.write(data,len);
      end;


    procedure telf32output.writealloc(len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec]^.alloc(len);
      end;


    procedure telf32output.writealign(len:longint);
      var
        modulo : longint;
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        modulo:=sects[currsec]^.datalen mod len;
        if modulo > 0 then
          sects[currsec]^.alloc(len-modulo);
      end;


    procedure telf32output.writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);
      var
        symaddr : longint;
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        if assigned(p) then
         begin
           { real address of the symbol }
           symaddr:=p^.address;
           { no symbol relocation need inside a section }
           if p^.section=currsec then
             begin
               case relative of
                 relative_false :
                   begin
                     sects[currsec]^.addsectionreloc(sects[currsec]^.datalen,currsec,relative_false);
                     inc(data,symaddr);
                   end;
                 relative_true :
                   begin
                     inc(data,symaddr-len-sects[currsec]^.datalen);
                   end;
                 relative_rva :
                   internalerror(3219583);
               end;
             end
           else
             begin
               writesymbol(p);
               if (p^.section<>sec_none) and (relative<>relative_true) then
                begin
                  sects[currsec]^.addsectionreloc(sects[currsec]^.datalen,p^.section,relative);
                  inc(data,symaddr);
                end
               else
                sects[currsec]^.addsymreloc(sects[currsec]^.datalen,p,relative);
               if relative=relative_true then
                begin
                  if p^.bind=AB_EXTERNAL then
                   dec(data,len)
                  else
                   dec(data,len+sects[currsec]^.datalen);
                end;
            end;
         end;
        sects[currsec]^.write(data,len);
      end;


    procedure telf32output.writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc : boolean);
      var
        stab : telf32stab;
        s : tsection;
      begin
        s:=section;
        if reloc then
         begin
           if (offset=-1) then
            begin
              if s=sec_none then
               offset:=0
              else
               offset:=sects[s]^.datalen;
            end;
         end;
        fillchar(stab,sizeof(telf32stab),0);
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr]^.datalen;
           sects[sec_stabstr]^.write(p^,strlen(p)+1);
         end;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        sects[sec_stab]^.write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
         sects[sec_stab]^.addsectionreloc(sects[sec_stab]^.datalen-4,s,relative_false);
      end;


    procedure telf32output.writesymstabs(section:tsection;offset:longint;p:pchar;ps:pasmsymbol;
                                                 nidx,nother,line:longint;reloc:boolean);
      var
        stab : telf32stab;
        s : tsection;
      begin
        s:=section;
        if reloc then
         begin
           if (offset=-1) then
            begin
              if s=sec_none then
               offset:=0
              else
               offset:=sects[s]^.datalen;
            end;
         end;
        fillchar(stab,sizeof(telf32stab),0);
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr]^.datalen;
           sects[sec_stabstr]^.write(p^,strlen(p)+1);
         end;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        sects[sec_stab]^.write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
         sects[sec_stab]^.addsymreloc(sects[sec_stab]^.datalen-4,ps,relative_false);
      end;


    procedure telf32output.setsectionsizes(var s:tsecsize);
      var
        align : longint;
        sec : tsection;
      begin
        { multiply stab with real size }
        s[sec_stab]:=s[sec_stab]*sizeof(telf32stab);
        { if debug then also count header stab }
        if (cs_debuginfo in aktmoduleswitches) then
         begin
           inc(s[sec_stab],sizeof(telf32stab));
           inc(s[sec_stabstr],length(SplitFileName(current_module^.mainsource^))+2);
         end;
        { fix all section }
        for sec:=low(tsection) to high(tsection) do
         begin
           if (s[sec]>0) and (not assigned(sects[sec])) then
             createsection(sec);
           if assigned(sects[sec]) then
            begin
              sects[sec]^.size:=s[sec];
              { calculate the alignment }
              align:=sects[sec]^.addralign;
              sects[sec]^.fillsize:=align-(sects[sec]^.size and (align-1));
              if sects[sec]^.fillsize=align then
               sects[sec]^.fillsize:=0;
            end;
         end;
      end;


{***********************************************
             Writing to disk
***********************************************}

    procedure telf32output.createrelocsection(s:pelf32section);
      var
        rel  : telf32reloc;
        hr,r : preloc;
        relsym,reltyp : longint;
      begin
        { create the reloc section }
        new(s^.relocsect,initname('.rel'+s^.name,9,0,symtabsect^.secshidx,s^.secshidx,4,8));
        { add the relocations }
        r:=s^.relochead;
        while assigned(r) do
         begin
           rel.address:=r^.address;
           if assigned(r^.symbol) then
            begin
              if (r^.symbol^.bind=AB_LOCAL) then
               relsym:=sects[r^.symbol^.section]^.secsymidx
              else
               begin
                 if r^.symbol^.idx=-1 then
                   internalerror(4321);
                 relsym:=(r^.symbol^.idx+initsym);
               end;
            end
           else
            if r^.section<>sec_none then
             relsym:=sects[r^.section]^.secsymidx
            else
             relsym:=SHN_UNDEF;
           case r^.typ of
             relative_true :
               reltyp:=R_386_PC32;
             relative_false :
               reltyp:=R_386_32;
           end;
           rel.info:=(relsym shl 8) or reltyp;
           { write reloc }
           s^.relocsect^.write(rel,sizeof(rel));
           { goto next and dispose this reloc }
           hr:=r;
           r:=r^.next;
           dispose(hr);
         end;
      end;


    procedure telf32output.createsymtab;
      var
        elfsym : telf32symbol;
        sym : tsymbol;
        sec : tsection;
        locals,
        i : longint;
      begin
        locals:=2;
      { empty entry }
        fillchar(elfsym,sizeof(elfsym),0);
        symtabsect^.write(elfsym,sizeof(elfsym));
      { filename entry }
        elfsym.st_name:=1;
        elfsym.st_info:=STT_FILE;
        elfsym.st_shndx:=SHN_ABS;
        symtabsect^.write(elfsym,sizeof(elfsym));
      { section }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            fillchar(elfsym,sizeof(elfsym),0);
            elfsym.st_name:=sects[sec]^.shstridx;
            elfsym.st_info:=STT_SECTION;
            elfsym.st_shndx:=sects[sec]^.secshidx;
            symtabsect^.write(elfsym,sizeof(elfsym));
            inc(locals);
          end;
      { symbols }
        syms^.seek(0);
        for i:=1 to syms^.count do
         begin
           syms^.read(sym,1);
           fillchar(elfsym,sizeof(elfsym),0);
           elfsym.st_name:=sym.name;
           elfsym.st_value:=sym.value;
           elfsym.st_size:=sym.size;
           case sym.bind of
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
           if sym.bind<>AB_EXTERNAL then
            begin
              case sym.typ of
                AT_FUNCTION :
                  elfsym.st_info:=elfsym.st_info or STT_FUNC;
                AT_DATA :
                  elfsym.st_info:=elfsym.st_info or STT_OBJECT;
              end;
            end;
           if sym.bind=AB_COMMON then
            elfsym.st_shndx:=SHN_COMMON
           else
            if assigned(sects[sym.section]) then
             elfsym.st_shndx:=sects[sym.section]^.secshidx
            else
             elfsym.st_shndx:=SHN_UNDEF;
           symtabsect^.write(elfsym,sizeof(elfsym));
         end;
      { update the .symtab section header }
        symtabsect^.shlink:=strtabsect^.secshidx;
        symtabsect^.shinfo:=locals;
      end;


    procedure telf32output.createshstrtab;
      var
        sec : tsection;
      begin
        with shstrtabsect^ do
         begin
           writestr(#0);
           symtabsect^.shstridx:=writestr('.symtab'#0);
           strtabsect^.shstridx:=writestr('.strtab'#0);
           shstrtabsect^.shstridx:=writestr('.shstrtab'#0);
           for sec:=low(tsection) to high(tsection) do
            if assigned(sects[sec]) then
             begin
               sects[sec]^.shstridx:=writestr(sects[sec]^.name+#0);
               if assigned(sects[sec]^.relocsect) then
                sects[sec]^.relocsect^.shstridx:=writestr(sects[sec]^.relocsect^.name+#0);
             end;
         end;
      end;


    procedure telf32output.writesectionheader(s:pelf32section);
      var
        sechdr : telf32sechdr;
      begin
        fillchar(sechdr,sizeof(sechdr),0);
        sechdr.sh_name:=s^.shstridx;
        sechdr.sh_type:=s^.shtype;
        sechdr.sh_flags:=s^.shflags;
        sechdr.sh_offset:=s^.datapos;
        sechdr.sh_size:=s^.datalen;
        sechdr.sh_link:=s^.shlink;
        sechdr.sh_info:=s^.shinfo;
        sechdr.sh_addralign:=s^.addralign;
        sechdr.sh_entsize:=s^.entsize;
        writer^.write(sechdr,sizeof(sechdr));
      end;


    procedure telf32output.writetodisk;
      var
        header : telf32header;
        datapos,
        shoffset,
        nsects : longint;
        sec    : tsection;
        empty  : array[0..63] of byte;
      begin
      { calc amount of sections we have and align sections at 4 bytes }
        fillchar(empty,sizeof(empty),0);
        nsects:=1;
        initsym:=2;
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            { each section requires a symbol for relocation }
            sects[sec]^.secsymidx:=initsym;
            inc(initsym);
            { also create the index in the section header table }
            sects[sec]^.secshidx:=nsects;
            inc(nsects);
            if assigned(sects[sec]^.relochead) then
             inc(nsects);
          end;
        { add default sections follows }
        shstrtabsect^.secshidx:=nsects;
        inc(nsects);
        symtabsect^.secshidx:=nsects;
        inc(nsects);
        strtabsect^.secshidx:=nsects;
        inc(nsects);
      { Create the relocation sections }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) and
            (sects[sec]^.nrelocs>0) then
           createrelocsection(sects[sec]);
      { create .symtab }
        createsymtab;
      { create .shstrtab }
        createshstrtab;
      { Calculate the filepositions }
        datapos:=$40; { elfheader + alignment }
        { sections first }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            sects[sec]^.datapos:=datapos;
            if assigned(sects[sec]^.data) then
              inc(datapos,sects[sec]^.aligneddatalen);
          end;
        { shstrtab }
        shstrtabsect^.datapos:=datapos;
        inc(datapos,shstrtabsect^.aligneddatalen);
        { section headers }
        shoffset:=datapos;
        inc(datapos,nsects*sizeof(telf32sechdr));
        { symtab }
        symtabsect^.datapos:=datapos;
        inc(datapos,symtabsect^.aligneddatalen);
        { strtab }
        strtabsect^.datapos:=datapos;
        inc(datapos,align(strtabsect^.datalen,4));
        { .rel sections }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) and
            assigned(sects[sec]^.relocsect) then
          begin
            sects[sec]^.relocsect^.datapos:=datapos;
            inc(datapos,sects[sec]^.relocsect^.aligneddatalen);
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
        header.e_shstrndx:=shstrtabsect^.secshidx;
        header.e_shnum:=nsects;
        header.e_ehsize:=sizeof(telf32header);
        header.e_shentsize:=sizeof(telf32sechdr);
        writer^.write(header,sizeof(header));
        writer^.write(empty,$40-sizeof(header)); { align }
      { Sections }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) and
            assigned(sects[sec]^.data) then
          begin
            { For the stab section we need an HdrSym which can now be
                calculated more easily }
            if sec=sec_stab then
             begin
               pelf32stab(sects[sec_stab]^.data^.data)^.nvalue:=sects[sec_stabstr]^.datalen;
               pelf32stab(sects[sec_stab]^.data^.data)^.strpos:=1;
               pelf32stab(sects[sec_stab]^.data^.data)^.ndesc:=
                 (sects[sec_stab]^.datalen div sizeof(telf32stab))-1{+1 according to gas output PM};
             end;
            { save the original section length }
            sects[sec]^.alignsection;
            writer^.write(sects[sec]^.data^.data^,sects[sec]^.data^.usedsize);
          end;
      { .shstrtab }
        shstrtabsect^.alignsection;
        writer^.write(shstrtabsect^.data^.data^,shstrtabsect^.data^.usedsize);
      { section headers, start with an empty header for sh_undef }
        writer^.write(empty,sizeof(telf32sechdr));
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            writesectionheader(sects[sec]);
            if assigned(sects[sec]^.relocsect) then
             writesectionheader(sects[sec]^.relocsect);
          end;
        writesectionheader(shstrtabsect);
        writesectionheader(symtabsect);
        writesectionheader(strtabsect);
      { .symtab }
        symtabsect^.alignsection;
        writer^.write(symtabsect^.data^.data^,symtabsect^.data^.usedsize);
      { .strtab }
        strtabsect^.writealign(4);
        writer^.write(strtabsect^.data^.data^,strtabsect^.data^.usedsize);
      { .rel sections }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) and
            assigned(sects[sec]^.relocsect) then
          begin
            sects[sec]^.relocsect^.alignsection;
            writer^.write(sects[sec]^.relocsect^.data^.data^,sects[sec]^.relocsect^.data^.usedsize);
          end;
      end;


end.
{
  $Log$
  Revision 1.4  2000-08-12 19:14:58  peter
    * ELF writer works now also with -g
    * ELF writer is default again for linux

  Revision 1.3  2000/07/13 12:08:26  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:43  michael
  + removed logs

}
