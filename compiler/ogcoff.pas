{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman and Pierre Muller

    Contains the binary coff reader and writer

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
unit ogcoff;

{$i defines.inc}

interface

    uses
       { common }
       cclasses,
       { target }
       systems,
       { assembler }
       cpubase,aasm,assemble,
       { output }
       ogbase;

    type
       tcoffsection = class(tobjectsection)
       public
         flags    : cardinal;
         coffrelocs,
         coffrelocpos : longint;
         constructor createsec(sec:TSection;AAlign,AFlags:cardinal);
       end;

       tcoffdata = class(tobjectdata)
       private
         Fstrs,
         Fsyms  : Tdynamicarray;
         win32   : boolean;
         procedure reset;
       public
         constructor createdjgpp;
         constructor createwin32;
         destructor  destroy;override;
         procedure setsectionsizes(var s:tsecsize);override;
         procedure createsection(sec:tsection);override;
         procedure writereloc(data,len:longint;p:tasmsymbol;relative:relative_type);override;
         procedure writesymbol(p:tasmsymbol);override;
         procedure writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);override;
         procedure writesymstabs(section:tsection;offset:longint;p:pchar;ps:tasmsymbol;nidx,nother,line:longint;reloc:boolean);override;
       end;

       tcoffobjectoutput = class(tobjectoutput)
       private
         win32   : boolean;
         initsym : longint;
         procedure write_relocs(s:tobjectsection);
         procedure write_symbol(const name:string;strpos,value,section,typ,aux:longint);
         procedure write_symbols;
       protected
         procedure writetodisk;override;
       public
         constructor createdjgpp(smart:boolean);
         constructor createwin32(smart:boolean);
         function  initwriting(const fn:string):boolean;override;
       end;

       ttasmsymbolarray = array[0..high(word)] of tasmsymbol;

       tcoffobjectinput = class(tobjectinput)
       private
         Fidx2sec  : array[0..255] of tsection;
         FCoffsyms : tdynamicarray;
         FSymTbl   : ^ttasmsymbolarray;
         win32     : boolean;
         procedure read_relocs(s:tcoffsection);
         procedure handle_symbols;
       public
         constructor createdjgpp(const fn:string);
         constructor createwin32(const fn:string);
         function  initreading:boolean;override;
         procedure donereading;override;
         procedure readfromdisk;override;
       end;

       tcoffassembler = class(tinternalassembler)
         constructor create(smart:boolean);override;
       end;

       tpecoffassembler = class(tinternalassembler)
         constructor create(smart:boolean);override;
       end;


implementation

    uses
{$ifdef delphi}
       sysutils,
{$else}
       strings,
{$endif}
       cutils,verbose,
       globtype,globals,fmodule;

    const
       symbolresize = 200*sizeof(toutputsymbol);
       strsresize   = 8192;

    const
       COFF_SYM_EXTERNAL = 2;
       COFF_SYM_STATIC   = 3;
       COFF_SYM_LABEL    = 6;
       COFF_SYM_FUNCTION = 101;
       COFF_SYM_FILE     = 103;
       COFF_SYM_SECTION  = 104;

    type
       { Structures which are written directly to the output file }
       coffheader=packed record
         mach   : word;
         nsects : word;
         time   : longint;
         sympos : longint;
         syms   : longint;
         opthdr : word;
         flag   : word;
       end;
       coffsechdr=packed record
         name     : array[0..7] of char;
         vsize    : longint;
         rvaofs   : longint;
         datasize  : longint;
         datapos  : longint;
         relocpos : longint;
         lineno1  : longint;
         nrelocs  : word;
         lineno2  : word;
         flags    : cardinal;
       end;
       coffsectionrec=packed record
         len     : longint;
         nrelocs : word;
         empty   : array[0..11] of char;
       end;
       coffreloc=packed record
         address  : longint;
         sym      : longint;
         relative : word;
       end;
       coffsymbol=packed record
         name    : array[0..3] of char; { real is [0..7], which overlaps the strpos ! }
         strpos  : longint;
         value   : longint;
         section : smallint;
         empty   : smallint;
         typ     : byte;
         aux     : byte;
       end;
       coffstab=packed record
         strpos  : longint;
         ntype   : byte;
         nother  : byte;
         ndesc   : word;
         nvalue  : longint;
       end;


{****************************************************************************
                               TCoffSection
****************************************************************************}

    constructor tcoffsection.createsec(sec:TSection;AAlign,AFlags:cardinal);
      begin
        inherited create(target_asm.secnames[sec],AAlign,(sec=sec_bss));
        Flags:=AFlags;
      end;


{****************************************************************************
                                TCoffData
****************************************************************************}

    constructor tcoffdata.createdjgpp;
      begin
        inherited create;
        win32:=false;
        reset;
      end;


    constructor tcoffdata.createwin32;
      begin
        inherited create;
        win32:=true;
        reset;
      end;


    destructor tcoffdata.destroy;
      begin
        FSyms.Free;
        FStrs.Free;
        inherited destroy;
      end;


    procedure tcoffdata.reset;
      var
        s : string;
      begin
        FSyms:=TDynamicArray.Create(symbolresize);
        FStrs:=TDynamicArray.Create(strsresize);
        { we need at least the following 3 sections }
        createsection(sec_code);
        createsection(sec_data);
        createsection(sec_bss);
        if (cs_gdb_lineinfo in aktglobalswitches) or
           (cs_debuginfo in aktmoduleswitches) then
         begin
           createsection(sec_stab);
           createsection(sec_stabstr);
           writestabs(sec_none,0,nil,0,0,0,false);
           { write zero pchar and name together (PM) }
           s:=#0+SplitFileName(current_module.mainsource^)+#0;
           sects[sec_stabstr].write(s[1],length(s));
         end;
      end;


    procedure tcoffdata.createsection(sec:TSection);
      var
        Flags,
        AAlign : cardinal;
      begin
        { defaults }
        Flags:=0;
        Aalign:=1;
        { alignment after section }
        case sec of
          sec_code :
            begin
              if win32 then
               Flags:=$60000020
              else
               Flags:=$20;
              Aalign:=16;
            end;
          sec_data :
            begin
              if win32 then
               Flags:=$c0300040
              else
               Flags:=$40;
              Aalign:=16;
            end;
          sec_bss :
            begin
              if win32 then
               Flags:=$c0300080
              else
               Flags:=$80;
              Aalign:=16;
            end;
          sec_idata2,
          sec_idata4,
          sec_idata5,
          sec_idata6,
          sec_idata7 :
            begin
              if win32 then
               Flags:=$40000000;
            end;
          sec_edata :
            begin
              if win32 then
               Flags:=$c0300040;
            end;
        end;
        sects[sec]:=tcoffSection.createsec(Sec,AAlign,Flags);
      end;


    procedure tcoffdata.writesymbol(p:tasmsymbol);
      var
        sym : toutputsymbol;
        s   : string;
      begin
        { already written ? }
        if p.idx<>-1 then
         exit;
        { be sure that the section will exists }
        if (p.section<>sec_none) and not(assigned(sects[p.section])) then
          createsection(p.section);
        FillChar(sym,sizeof(sym),0);
        sym.value:=p.size;
        sym.bind:=p.bind;
        sym.typ:=AT_NONE;
        { if local of global then set the section value to the address
          of the symbol }
        if sym.bind in [AB_LOCAL,AB_GLOBAL] then
         begin
           sym.section:=p.section;
           sym.value:=p.address+sects[sym.section].mempos;
         end;
        { store the symbol, but not the local ones }
        if (sym.bind<>AB_LOCAL) then
         begin
           { symbolname }
           s:=p.name;
           if length(s)>8 then
            begin
              sym.nameidx:=FStrs.size+4;
              FStrs.writestr(s);
              FStrs.writestr(#0);
            end
           else
            begin
              sym.nameidx:=-1;
              sym.namestr:=s;
            end;
           { update the asmsymbol index }
           p.idx:=FSyms.size div sizeof(TOutputSymbol);
           { write the symbol }
           FSyms.write(sym,sizeof(toutputsymbol));
         end
        else
         begin
           p.idx:=-2; { local }
         end;
      end;


    procedure tcoffdata.writereloc(data,len:longint;p:tasmsymbol;relative:relative_type);
      var
        curraddr,
        symaddr : longint;
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        if assigned(p) then
         begin
           { current address }
           curraddr:=sects[currsec].mempos+sects[currsec].datasize;
           { real address of the symbol }
           symaddr:=p.address;
           if p.section<>sec_none then
            inc(symaddr,sects[p.section].mempos);
           { no symbol relocation need inside a section }
           if p.section=currsec then
             begin
               case relative of
                 relative_false :
                   begin
                     sects[currsec].addsectionreloc(curraddr,currsec,relative_false);
                     inc(data,symaddr);
                   end;
                 relative_true :
                   begin
                     inc(data,symaddr-len-sects[currsec].datasize);
                   end;
                 relative_rva :
                   begin
                     sects[currsec].addsectionreloc(curraddr,currsec,relative_rva);
                     inc(data,symaddr);
                   end;
               end;
             end
           else
             begin
               writesymbol(p);
               if (p.section<>sec_none) and (relative<>relative_true) then
                 sects[currsec].addsectionreloc(curraddr,p.section,relative)
               else
                 sects[currsec].addsymreloc(curraddr,p,relative);
               if not win32 then {seems wrong to me (PM) }
                inc(data,symaddr)
               else
                if (relative<>relative_true) and (p.section<>sec_none) then
                 inc(data,symaddr);
               if relative=relative_true then
                begin
                  if win32 then
                    dec(data,len-4)
                  else
                    dec(data,len+sects[currsec].datasize);
                end;
            end;
         end;
        sects[currsec].write(data,len);
      end;


    procedure tcoffdata.writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc : boolean);
      var
        stab : coffstab;
        s : tsection;
        curraddr : longint;
      begin
        s:=section;
        { local var can be at offset -1 !! PM }
        if reloc then
         begin
           if (offset=-1) then
            begin
              if s=sec_none then
               offset:=0
              else
               offset:=sects[s].datasize;
            end;
           if (s<>sec_none) then
            inc(offset,sects[s].datapos);
         end;
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr].datasize;
           sects[sec_stabstr].write(p^,strlen(p)+1);
         end
        else
         stab.strpos:=0;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        sects[sec_stab].write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
         begin
           { current address }
           curraddr:=sects[sec_stab].mempos+sects[sec_stab].datasize;
           if DLLSource and RelocSection then
           { avoid relocation in the .stab section
             because it ends up in the .reloc section instead }
             sects[sec_stab].addsectionreloc(curraddr-4,s,relative_rva)
           else
             sects[sec_stab].addsectionreloc(curraddr-4,s,relative_false);
         end;
      end;


    procedure tcoffdata.writesymstabs(section:tsection;offset:longint;p:pchar;ps:tasmsymbol;
                                                 nidx,nother,line:longint;reloc:boolean);
      var
        stab : coffstab;
        curraddr : longint;
      begin
        { do not use the size stored in offset field
         this is DJGPP specific ! PM }
        if win32 then
          offset:=0;
        { local var can be at offset -1 !! PM }
        if reloc then
         begin
           if (offset=-1) then
            begin
              if section=sec_none then
               offset:=0
              else
               offset:=sects[section].datasize;
            end;
           if (section<>sec_none) then
            inc(offset,sects[section].mempos);
         end;
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr].datasize;
           sects[sec_stabstr].write(p^,strlen(p)+1);
         end
        else
         stab.strpos:=0;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        sects[sec_stab].write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
         begin
           { current address }
           curraddr:=sects[sec_stab].mempos+sects[sec_stab].datasize;
           if DLLSource and RelocSection then
            { avoid relocation in the .stab section
              because it ends up in the .reloc section instead }
            sects[sec_stab].addsymreloc(curraddr-4,ps,relative_rva)
           else
            sects[sec_stab].addsymreloc(curraddr-4,ps,relative_false);
         end;
      end;


    procedure tcoffdata.setsectionsizes(var s:tsecsize);
      var
        mempos : longint;
        sec    : tsection;
      begin
        { multiply stab with real size }
        s[sec_stab]:=s[sec_stab]*sizeof(coffstab);
        { if debug then also count header stab }
        if (cs_debuginfo in aktmoduleswitches) then
         begin
           inc(s[sec_stab],sizeof(coffstab));
           inc(s[sec_stabstr],length(SplitFileName(current_module.mainsource^))+2);
         end;
        { calc mempos }
        mempos:=0;
        for sec:=low(tsection) to high(tsection) do
         begin
           if (s[sec]>0) and
              (not assigned(sects[sec])) then
             createsection(sec);
           if assigned(sects[sec]) then
            begin
              sects[sec].memsize:=s[sec];
              { memory position }
              if not win32 then
               begin
                 sects[sec].mempos:=mempos;
                 inc(mempos,align(sects[sec].memsize,sects[sec].addralign));
               end;
            end;
         end;
      end;


{****************************************************************************
                                tcoffobjectoutput
****************************************************************************}

    constructor tcoffobjectoutput.createdjgpp(smart:boolean);
      begin
        inherited create(smart);
        win32:=false;
      end;


    constructor tcoffobjectoutput.createwin32(smart:boolean);
      begin
        inherited create(smart);
        win32:=true;
      end;


    function tcoffobjectoutput.initwriting(const fn:string):boolean;
      begin
        result:=inherited initwriting(fn);
        if result then
         begin
           initsym:=0;
           if win32 then
            FData:=tcoffdata.createwin32
           else
            FData:=tcoffdata.createdjgpp;
         end;
      end;


    procedure tcoffobjectoutput.write_relocs(s:tobjectsection);
      var
        rel  : coffreloc;
        hr,r : poutputreloc;
      begin
        r:=s.relochead;
        while assigned(r) do
         begin
           rel.address:=r^.address;
           if assigned(r^.symbol) then
            begin
              if (r^.symbol.bind=AB_LOCAL) then
               rel.sym:=2*data.sects[r^.symbol.section].secsymidx
              else
               begin
                 if r^.symbol.idx=-1 then
                   internalerror(4321);
                 rel.sym:=r^.symbol.idx+initsym;
               end;
            end
           else
            begin
              if r^.section<>sec_none then
               rel.sym:=2*data.sects[r^.section].secsymidx
              else
               rel.sym:=0;
            end;
           case r^.typ of
             relative_true  : rel.relative:=$14;
             relative_false : rel.relative:=$6;
             relative_rva   : rel.relative:=$7;
           end;
           FWriter.write(rel,sizeof(rel));
           { goto next and dispose this reloc }
           hr:=r;
           r:=r^.next;
           dispose(hr);
         end;
      end;


    procedure tcoffobjectoutput.write_symbol(const name:string;strpos,value,section,typ,aux:longint);
      var
        sym : coffsymbol;
      begin
        FillChar(sym,sizeof(sym),0);
        if strpos=-1 then
         move(name[1],sym.name,length(name))
        else
         sym.strpos:=strpos;
        sym.value:=value;
        sym.section:=section;
        sym.typ:=typ;
        sym.aux:=aux;
        FWriter.write(sym,sizeof(sym));
      end;


    procedure tcoffobjectoutput.write_symbols;
      var
        filename  : string[18];
        sec       : tsection;
        sectionval,
        i         : longint;
        globalval : byte;
        secrec    : coffsectionrec;
        sym       : toutputsymbol;
      begin
        with tcoffdata(data) do
         begin
           { The `.file' record, and the file name auxiliary record }
           write_symbol ('.file', -1, 0, -2, $67, 1);
           fillchar(filename,sizeof(filename),0);
           filename:=SplitFileName(current_module.mainsource^);
           FWriter.write(filename[1],sizeof(filename)-1);
           { The section records, with their auxiliaries, also store the
             symbol index }
           for sec:=low(tsection) to high(tsection) do
            if assigned(sects[sec]) then
             begin
               write_symbol(target_asm.secnames[sec],-1,sects[sec].mempos,sects[sec].secsymidx,3,1);
               fillchar(secrec,sizeof(secrec),0);
               secrec.len:=sects[sec].aligneddatasize;
               secrec.nrelocs:=sects[sec].nrelocs;
               FWriter.write(secrec,sizeof(secrec));
             end;
           { The real symbols }
           FSyms.seek(0);
           for i:=1 to FSyms.size div sizeof(TOutputSymbol) do
            begin
              FSyms.read(sym,sizeof(TOutputSymbol));
              if sym.bind=AB_LOCAL then
                globalval:=3
              else
                globalval:=2;
              if assigned(sects[sym.section]) then
               sectionval:=sects[sym.section].secsymidx
              else
               sectionval:=0;
              write_symbol(sym.namestr,sym.nameidx,sym.value,sectionval,globalval,0);
            end;
         end;
      end;


    procedure tcoffobjectoutput.writetodisk;
      var
        datapos,
        secsymidx,
        nsects,
        sympos,i : longint;
        hstab    : coffstab;
        gotreloc : boolean;
        sec      : tsection;
        header   : coffheader;
        sechdr   : coffsechdr;
        empty    : array[0..15] of byte;
        hp       : pdynamicblock;
      begin
        with tcoffdata(data) do
         begin
         { calc amount of sections we have }
           fillchar(empty,sizeof(empty),0);
           nsects:=0;
           initsym:=2;   { 2 for the file }
           secsymidx:=0;
           for sec:=low(tsection) to high(tsection) do
            if assigned(sects[sec]) then
             begin
               inc(nsects);
               inc(secsymidx);
               sects[sec].secsymidx:=secsymidx;
               inc(initsym,2); { 2 for each section }
             end;
         { For the stab section we need an HdrSym which can now be
           calculated more easily }
           if assigned(sects[sec_stab]) then
            begin
              hstab.strpos:=1;
              hstab.ntype:=0;
              hstab.nother:=0;
              hstab.ndesc:=(sects[sec_stab].datasize div sizeof(coffstab))-1{+1 according to gas output PM};
              hstab.nvalue:=sects[sec_stabstr].datasize;
              sects[sec_stab].data.seek(0);
              sects[sec_stab].data.write(hstab,sizeof(hstab));
            end;
         { Calculate the filepositions }
           datapos:=sizeof(coffheader)+sizeof(coffsechdr)*nsects;
           { sections first }
           for sec:=low(tsection) to high(tsection) do
            if assigned(sects[sec]) then
             begin
               sects[sec].datapos:=datapos;
               if assigned(sects[sec].data) then
                 inc(datapos,sects[sec].aligneddatasize);
             end;
           { relocs }
           gotreloc:=false;
           for sec:=low(tsection) to high(tsection) do
            if assigned(sects[sec]) then
             begin
               tcoffsection(sects[sec]).coffrelocpos:=datapos;
               inc(datapos,10*sects[sec].nrelocs);
               if (not gotreloc) and (sects[sec].nrelocs>0) then
                gotreloc:=true;
             end;
           { symbols }
           sympos:=datapos;
         { COFF header }
           fillchar(header,sizeof(coffheader),0);
           header.mach:=$14c;
           header.nsects:=nsects;
           header.sympos:=sympos;
           header.syms:=(FSyms.size div sizeof(TOutputSymbol))+initsym;
           if gotreloc then
            header.flag:=$104
           else
            header.flag:=$105;
           FWriter.write(header,sizeof(header));
         { Section headers }
           for sec:=low(tsection) to high(tsection) do
            if assigned(sects[sec]) then
             begin
               fillchar(sechdr,sizeof(sechdr),0);
               move(target_asm.secnames[sec][1],sechdr.name,length(target_asm.secnames[sec]));
               if not win32 then
                 begin
                   sechdr.rvaofs:=sects[sec].mempos;
                   sechdr.vsize:=sects[sec].mempos;
                 end
               else
                 begin
                   if sec=sec_bss then
                     sechdr.vsize:=sects[sec].aligneddatasize;
                 end;
               sechdr.datasize:=sects[sec].aligneddatasize;
               if (sects[sec].datasize>0) and assigned(sects[sec].data) then
                 sechdr.datapos:=sects[sec].datapos;
               sechdr.nrelocs:=sects[sec].nrelocs;
               sechdr.relocpos:=TCoffSection(sects[sec]).coffrelocpos;
               sechdr.flags:=TCoffSection(sects[sec]).flags;
               FWriter.write(sechdr,sizeof(sechdr));
             end;
         { Sections }
           for sec:=low(tsection) to high(tsection) do
            if assigned(sects[sec]) and
               assigned(sects[sec].data) then
             begin
               sects[sec].alignsection;
               hp:=sects[sec].data.firstblock;
               while assigned(hp) do
                begin
                  FWriter.write(hp^.data,hp^.used);
                  hp:=hp^.next;
                end;
             end;
         { Relocs }
           for sec:=low(tsection) to high(tsection) do
            if assigned(sects[sec]) then
             write_relocs(sects[sec]);
         { Symbols }
           write_symbols;
         { Strings }
           i:=FStrs.size+4;
           FWriter.write(i,4);
           hp:=FStrs.firstblock;
           while assigned(hp) do
            begin
              FWriter.write(hp^.data,hp^.used);
              hp:=hp^.next;
            end;
         end;
      end;

{****************************************************************************
                                tcoffobjectinput
****************************************************************************}

    constructor tcoffobjectinput.createdjgpp(const fn:string);
      begin
        inherited create(fn);
        win32:=false;
      end;


    constructor tcoffobjectinput.createwin32(const fn:string);
      begin
        inherited create(fn);
        win32:=true;
      end;


    function tcoffobjectinput.initreading:boolean;
      begin
        result:=inherited initreading;
        if result then
         begin
           if win32 then
            FData:=tcoffdata.createwin32
           else
            FData:=tcoffdata.createdjgpp;
           FCoffSyms:=TDynamicArray.Create(symbolresize);
         end;
      end;


    procedure tcoffobjectinput.donereading;
      begin
        FCoffSyms.Free;
      end;


    procedure tcoffobjectinput.read_relocs(s:tcoffsection);
      var
        rel  : coffreloc;
        rel_type : relative_type;
        i        : longint;
        p        : tasmsymbol;
      begin
        for i:=1 to s.coffrelocs do
         begin
           FReader.read(rel,sizeof(rel));
           case rel.relative of
             $14 : rel_type:=relative_true;
             $06 : rel_type:=relative_false;
             $07 : rel_type:=relative_rva;
           else
             begin
               Comment(V_Error,'Error reading coff file');
               exit;
             end;
           end;

           p:=FSymTbl^[rel.sym];
           if assigned(p) then
            begin
              s.addsymreloc(rel.address,p,rel_type);
            end
           else
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
         end;
      end;


    procedure tcoffobjectinput.handle_symbols;
      var
        sec       : tsection;
        i,nsyms,
        symidx    : longint;
        sym       : coffsymbol;
        strname   : string;
        p         : tasmsymbol;
        auxrec    : array[0..17] of byte;
      begin
        with tcoffdata(data) do
         begin
           nsyms:=FCoffSyms.Size div sizeof(CoffSymbol);
           { Allocate memory for symidx -> tasmsymbol table }
           GetMem(FSymTbl,nsyms*sizeof(pointer));
           FillChar(FSymTbl^,nsyms*sizeof(pointer),0);
           { Loop all symbols }
           FCoffSyms.Seek(0);
           symidx:=0;
           while (symidx<nsyms) do
            begin
              FCoffSyms.Read(sym,sizeof(sym));
              if plongint(@sym.name)^<>0 then
               begin
                 move(sym.name,strname[1],8);
                 strname[9]:=#0;
               end
              else
               begin
                 FStrs.Seek(sym.strpos-4);
                 FStrs.Read(strname[1],255);
                 strname[255]:=#0;
               end;
              strname[0]:=chr(strlen(@strname[1]));
              if strname='' then
               Internalerror(341324310);
              case sym.typ of
                COFF_SYM_EXTERNAL :
                  begin
                    if sym.section=0 then
                     begin
                       p:=tasmsymbol.create(strname,AB_EXTERNAL,AT_FUNCTION);
                     end
                    else
                     begin
                       p:=tasmsymbol.create(strname,AB_GLOBAL,AT_FUNCTION);
                       sec:=Fidx2sec[sym.section];
                       if assigned(sects[sec]) then
                        begin
                          p.section:=sec;
                          if sym.value>=sects[sec].mempos then
                           p.address:=sym.value-sects[sec].mempos
                          else
                           internalerror(432432432);
                        end
                       else
                        internalerror(34243214);
                     end;
                    AddSymbol(p);
                    FSymTbl^[symidx]:=p
                  end;
                COFF_SYM_STATIC :
                  begin
                    p:=tasmsymbol.create(strname,AB_LOCAL,AT_FUNCTION);
                    sec:=Fidx2sec[sym.section];
                    if assigned(sects[sec]) then
                     begin
                       p.section:=sec;
                       if sym.value>=sects[sec].mempos then
                        p.address:=sym.value-sects[sec].mempos
                       else
                        begin
                          if Str2Sec(strname)<>sec then
                           internalerror(432432432);
                        end;
                     end
                    else
                     internalerror(34243214);
                    AddSymbol(p);
                    FSymTbl^[symidx]:=p;
                  end;
                COFF_SYM_SECTION,
                COFF_SYM_LABEL,
                COFF_SYM_FUNCTION,
                COFF_SYM_FILE :
                  ;
                else
                  internalerror(4342343);
              end;
              { read aux records }
              for i:=1 to sym.aux do
               begin
                 FCoffSyms.Read(auxrec,sizeof(auxrec));
                 inc(symidx);
               end;
              inc(symidx);
            end;
         end;
      end;


    procedure tcoffobjectinput.readfromdisk;
      var
        strsize,
        i        : longint;
        sec      : tsection;
        header   : coffheader;
        sechdr   : coffsechdr;
      begin
        with tcoffdata(data) do
         begin
           FillChar(Fidx2sec,sizeof(Fidx2sec),0);
         { COFF header }
           if not reader.read(header,sizeof(coffheader)) then
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
           if header.mach<>$14c then
            begin
              Comment(V_Error,'Not a coff file');
              exit;
            end;
           if header.nsects>255 then
            begin
              Comment(V_Error,'To many sections');
              exit;
            end;
{           header.mach:=$14c;
           header.nsects:=nsects;
           header.sympos:=sympos;
           header.syms:=(Syms.size div sizeof(TOutputSymbol))+initsym;
           if gotreloc then
            header.flag:=$104
           else
            header.flag:=$105 }
         { Section headers }
           for i:=1 to header.nsects do
            begin
              if not reader.read(sechdr,sizeof(sechdr)) then
               begin
                 Comment(V_Error,'Error reading coff file');
                 exit;
               end;
              sec:=str2sec(strpas(sechdr.name));
              if sec<>sec_none then
               begin
                 Fidx2sec[i]:=sec;
                 createsection(sec);
                 if not win32 then
                  sects[sec].mempos:=sechdr.rvaofs;
                 tcoffsection(sects[sec]).coffrelocs:=sechdr.nrelocs;
                 tcoffsection(sects[sec]).coffrelocpos:=sechdr.relocpos;
                 sects[sec].datapos:=sechdr.datapos;
                 sects[sec].datasize:=sechdr.datasize;
                 tcoffsection(sects[sec]).flags:=sechdr.flags;
               end
              else
               Comment(V_Warning,'skipping unsupported section '+strpas(sechdr.name));
            end;
         { Symbols }
           Reader.Seek(header.sympos);
           if not Reader.ReadArray(FCoffSyms,header.syms*sizeof(CoffSymbol)) then
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
         { Strings }
           if not Reader.Read(strsize,4) then
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
           if strsize<4 then
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
           if not Reader.ReadArray(FStrs,Strsize-4) then
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
         { Insert all symbols }
           handle_symbols;
         { Sections }
           for sec:=low(tsection) to high(tsection) do
            if assigned(sects[sec]) and
               (sec<>sec_bss) then
             begin
               Reader.Seek(sects[sec].datapos);
               if not Reader.ReadArray(sects[sec].data,sects[sec].datasize) then
                begin
                  Comment(V_Error,'Error reading coff file');
                  exit;
                end;
             end;
         { Relocs }
           for sec:=low(tsection) to high(tsection) do
            if assigned(sects[sec]) and
               (tcoffsection(sects[sec]).coffrelocs>0) then
             begin
               Reader.Seek(tcoffsection(sects[sec]).coffrelocpos);
               read_relocs(tcoffsection(sects[sec]));
             end;
         end;
      end;


{****************************************************************************
                                 TCoffAssembler
****************************************************************************}

    constructor TCoffAssembler.Create(smart:boolean);
      begin
        inherited Create(smart);
        objectoutput:=tcoffobjectoutput.createdjgpp(smart);
      end;


{****************************************************************************
                               TPECoffAssembler
****************************************************************************}

    constructor TPECoffAssembler.Create(smart:boolean);
      begin
        inherited Create(smart);
        objectoutput:=tcoffobjectoutput.createwin32(smart);
      end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

    const
       as_i386_coff_info : tasminfo =
          (
            id     : as_i386_coff;
            idtxt  : 'COFF';
            asmbin : '';
            asmcmd : '';
            supported_target : target_i386_go32v2;
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
              '.stab','.stabstr')
          );

    const
       as_i386_pecoff_info : tasminfo =
          (
            id     : as_i386_pecoff;
            idtxt  : 'PECOFF';
            asmbin : '';
            asmcmd : '';
            supported_target : target_i386_win32;
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
              '.stab','.stabstr')
          );

       as_i386_pecoffwdosx_info : tasminfo =
          (
            id     : as_i386_pecoffwdosx;
            idtxt  : 'PECOFFWDOSX';
            asmbin : '';
            asmcmd : '';
            supported_target : target_i386_wdosx;
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
              '.stab','.stabstr')
          );


initialization
  RegisterAssembler(as_i386_coff_info,TCoffAssembler);
  RegisterAssembler(as_i386_pecoff_info,TPECoffAssembler);
  RegisterAssembler(as_i386_pecoffwdosx_info,TPECoffAssembler);
end.
{
  $Log$
  Revision 1.19  2002-05-14 19:34:43  peter
    * removed old logs and updated copyright year

  Revision 1.18  2002/04/04 19:05:58  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.17  2002/04/04 18:38:30  carl
  + added wdosx support (patch from Pavel)

}
