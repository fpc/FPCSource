{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman and Pierre Muller

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
       cobjects,
       { target }
       systems,
       { assembler }
       cpubase,aasm,
       { output }
       ogbase;

    type
       pcoffsection = ^tcoffsection;
       tcoffsection = object(toutputsection)
          flags    : longint;
          relocpos : longint;
          constructor initsec(sec:TSection;AAlign,AFlags:longint);
       end;

       pcoffoutput = ^tcoffoutput;
       tcoffoutput = object(tobjectoutput)
         win32   : boolean;
         strs,
         syms    : Pdynamicarray;
         initsym : longint;
         constructor initdjgpp(smart:boolean);
         constructor initwin32(smart:boolean);
         destructor  done;virtual;
         procedure initwriting(Aplace:tcutplace);virtual;
         procedure donewriting;virtual;
         procedure setsectionsizes(var s:tsecsize);virtual;
         procedure createsection(sec:tsection);virtual;
         procedure writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);virtual;
         procedure writesymbol(p:pasmsymbol);virtual;
         procedure writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);virtual;
         procedure writesymstabs(section:tsection;offset:longint;p:pchar;ps:pasmsymbol;
                                 nidx,nother,line:longint;reloc:boolean);virtual;
       private
         procedure write_relocs(s:poutputsection);
         procedure write_symbol(const name:string;strpos,value,section,typ,aux:longint);
         procedure write_symbols;
         procedure writetodisk;
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
       symbolresize = 200*18;
       strsresize   = 8192;
       DataResize   = 8192;

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
         flags    : longint;
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
       pcoffstab=^coffstab;
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

    constructor tcoffsection.initsec(sec:TSection;AAlign,AFlags:longint);
      begin
        inherited init(target_asm.secnames[sec],AAlign,(sec=sec_bss));
        Flags:=AFlags;
      end;


{****************************************************************************
                                TCoffOutput
****************************************************************************}

    constructor tcoffoutput.initdjgpp(smart:boolean);
      begin
        inherited init(smart);
        win32:=false;
      end;


    constructor tcoffoutput.initwin32(smart:boolean);
      begin
        inherited init(smart);
        win32:=true;
      end;


    destructor tcoffoutput.done;
      begin
        inherited done;
      end;


    procedure tcoffoutput.initwriting(Aplace:tcutplace);
      var
        s : string;
      begin
        inherited initwriting(Aplace);
        { reset }
        initsym:=0;
        new(syms,init(symbolresize));
        new(strs,init(strsresize));
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
           s:=#0+SplitFileName(current_module^.mainsource^)+#0;
           sects[sec_stabstr]^.write(s[1],length(s));
         end;
      end;


    procedure tcoffoutput.donewriting;
      begin
        { Only write the .o if there are no errors }
        if errorcount=0 then
          writetodisk;
        dispose(syms,done);
        dispose(strs,done);
        inherited donewriting;
      end;


    procedure tcoffoutput.createsection(sec:TSection);
      var
        Flags,
        AAlign : longint;
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
              Aalign:=4;
            end;
          sec_data :
            begin
              if win32 then
               Flags:=$c0300040
              else
               Flags:=$40;
              Aalign:=4;
            end;
          sec_bss :
            begin
              if win32 then
               Flags:=$c0300080
              else
               Flags:=$80;
              Aalign:=4;
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
        sects[sec]:=new(PcoffSection,InitSec(Sec,AAlign,Flags));
      end;


    procedure tcoffoutput.writesymbol(p:pasmsymbol);
      var
        sym : toutputsymbol;
        s   : string;
      begin
        { already written ? }
        if p^.idx<>-1 then
         exit;
        { be sure that the section will exists }
        if (p^.section<>sec_none) and not(assigned(sects[p^.section])) then
          createsection(p^.section);
        FillChar(sym,sizeof(sym),0);
        sym.value:=p^.size;
        sym.bind:=p^.bind;
        sym.typ:=AT_NONE;
        { if local of global then set the section value to the address
          of the symbol }
        if sym.bind in [AB_LOCAL,AB_GLOBAL] then
         begin
           sym.section:=p^.section;
           sym.value:=p^.address+sects[sym.section]^.mempos;
         end;
        { store the symbol, but not the local ones }
        if (sym.bind<>AB_LOCAL) then
         begin
           { symbolname }
           s:=p^.name;
           if length(s)>8 then
            begin
              sym.nameidx:=strs^.size+4;
              strs^.writestr(s);
              strs^.writestr(#0);
            end
           else
            begin
              sym.nameidx:=-1;
              sym.namestr:=s;
            end;
           { update the asmsymbol index }
           p^.idx:=syms^.size div sizeof(TOutputSymbol);
           { write the symbol }
           syms^.write(sym,sizeof(toutputsymbol));
         end
        else
         begin
           p^.idx:=-2; { local }
         end;
        { make the exported syms known to the objectwriter
          (needed for .a generation) }
        if (sym.bind in [AB_GLOBAL,AB_COMMON]) then
          writer^.writesym(p^.name);
      end;


    procedure tcoffoutput.writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);
      var
        curraddr,
        symaddr : longint;
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        if assigned(p) then
         begin
           { current address }
           curraddr:=sects[currsec]^.mempos+sects[currsec]^.datasize;
           { real address of the symbol }
           symaddr:=p^.address;
           if p^.section<>sec_none then
            inc(symaddr,sects[p^.section]^.mempos);
           { no symbol relocation need inside a section }
           if p^.section=currsec then
             begin
               case relative of
                 relative_false :
                   begin
                     sects[currsec]^.addsectionreloc(curraddr,currsec,relative_false);
                     inc(data,symaddr);
                   end;
                 relative_true :
                   begin
                     inc(data,symaddr-len-sects[currsec]^.datasize);
                   end;
                 relative_rva :
                   begin
                     sects[currsec]^.addsectionreloc(curraddr,currsec,relative_rva);
                     inc(data,symaddr);
                   end;
               end;
             end
           else
             begin
               writesymbol(p);
               if (p^.section<>sec_none) and (relative<>relative_true) then
                 sects[currsec]^.addsectionreloc(curraddr,p^.section,relative)
               else
                 sects[currsec]^.addsymreloc(curraddr,p,relative);
               if not win32 then {seems wrong to me (PM) }
                inc(data,symaddr)
               else
                if (relative<>relative_true) and (p^.section<>sec_none) then
                 inc(data,symaddr);
               if relative=relative_true then
                begin
                  if win32 then
                    dec(data,len-4)
                  else
                    dec(data,len+sects[currsec]^.datasize);
                end;
            end;
         end;
        sects[currsec]^.write(data,len);
      end;


    procedure tcoffoutput.writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc : boolean);
      var
        stab : coffstab;
        s : tsection;
      begin
        { This is wrong because
          sec_none is used only for external bss
        if section=sec_none then
         s:=currsec
        else }
         s:=section;
        { local var can be at offset -1 !! PM }
        if reloc then
         begin
           if (offset=-1) then
            begin
              if s=sec_none then
               offset:=0
              else
               offset:=sects[s]^.datasize;
            end;
           if (s<>sec_none) then
            inc(offset,sects[s]^.datapos);
         end;
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr]^.datasize;
           sects[sec_stabstr]^.write(p^,strlen(p)+1);
         end
        else
         stab.strpos:=0;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        sects[sec_stab]^.write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
          if DLLSource and RelocSection then
          { avoid relocation in the .stab section
            because it ends up in the .reloc section instead }
            sects[sec_stab]^.addsectionreloc(sects[sec_stab]^.datasize-4,s,relative_rva)
          else
            sects[sec_stab]^.addsectionreloc(sects[sec_stab]^.datasize-4,s,relative_false);
      end;


    procedure tcoffoutput.writesymstabs(section:tsection;offset:longint;p:pchar;ps:pasmsymbol;
                                                 nidx,nother,line:longint;reloc:boolean);
      var
        stab : coffstab;
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
               offset:=sects[section]^.datasize;
            end;
           if (section<>sec_none) then
            inc(offset,sects[section]^.mempos);
         end;
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr]^.datasize;
           sects[sec_stabstr]^.write(p^,strlen(p)+1);
         end
        else
         stab.strpos:=0;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        sects[sec_stab]^.write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
         begin
           if DLLSource and RelocSection then
            { avoid relocation in the .stab section
              because it ends up in the .reloc section instead }
            sects[sec_stab]^.addsymreloc(sects[sec_stab]^.datasize-4,ps,relative_rva)
           else
            sects[sec_stab]^.addsymreloc(sects[sec_stab]^.datasize-4,ps,relative_false);
         end;
      end;


    procedure tcoffoutput.setsectionsizes(var s:tsecsize);
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
           inc(s[sec_stabstr],length(SplitFileName(current_module^.mainsource^))+2);
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
              sects[sec]^.memsize:=s[sec];
              { memory position }
              if not win32 then
               begin
                 sects[sec]^.mempos:=mempos;
                 inc(mempos,align(sects[sec]^.memsize,sects[sec]^.addralign));
               end;
            end;
         end;
      end;


{***********************************************
             Writing to disk
***********************************************}

    procedure tcoffoutput.write_relocs(s:poutputsection);
      var
        rel  : coffreloc;
        hr,r : poutputreloc;
      begin
        r:=s^.relochead;
        while assigned(r) do
         begin
           rel.address:=r^.address;
           if assigned(r^.symbol) then
            begin
              if (r^.symbol^.bind=AB_LOCAL) then
               rel.sym:=2*sects[r^.symbol^.section]^.secsymidx
              else
               begin
                 if r^.symbol^.idx=-1 then
                   internalerror(4321);
                 rel.sym:=r^.symbol^.idx+initsym;
               end;
            end
           else
            begin
              if r^.section<>sec_none then
               rel.sym:=2*sects[r^.section]^.secsymidx
              else
               rel.sym:=0;
            end;
           case r^.typ of
             relative_true  : rel.relative:=$14;
             relative_false : rel.relative:=$6;
             relative_rva   : rel.relative:=$7;
           end;
           writer^.write(rel,sizeof(rel));
           { goto next and dispose this reloc }
           hr:=r;
           r:=r^.next;
           dispose(hr);
         end;
      end;


    procedure tcoffoutput.write_symbol(const name:string;strpos,value,section,typ,aux:longint);
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
        writer^.write(sym,sizeof(sym));
      end;


    procedure tcoffoutput.write_symbols;
      var
        filename  : string[18];
        sec       : tsection;
        value,
        sectionval,
        i         : longint;
        globalval : byte;
        secrec    : coffsectionrec;
        sym       : toutputsymbol;
      begin
        { The `.file' record, and the file name auxiliary record }
        write_symbol ('.file', -1, 0, -2, $67, 1);
        fillchar(filename,sizeof(filename),0);
        filename:=SplitFileName(current_module^.mainsource^);
        writer^.write(filename[1],sizeof(filename)-1);
        { The section records, with their auxiliaries, also store the
          symbol index }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            write_symbol(target_asm.secnames[sec],-1,sects[sec]^.mempos,sects[sec]^.secsymidx,3,1);
            fillchar(secrec,sizeof(secrec),0);
            secrec.len:=sects[sec]^.aligneddatasize;
            secrec.nrelocs:=sects[sec]^.nrelocs;
            writer^.write(secrec,sizeof(secrec));
          end;
        { The real symbols }
        syms^.seek(0);
        for i:=1 to syms^.size div sizeof(TOutputSymbol) do
         begin
           syms^.read(sym,sizeof(TOutputSymbol));
           if sym.bind=AB_LOCAL then
             globalval:=3
           else
             globalval:=2;
           if assigned(sects[sym.section]) then
            sectionval:=sects[sym.section]^.secsymidx
           else
            sectionval:=0;
           write_symbol(sym.namestr,sym.nameidx,sym.value,sectionval,globalval,0);
         end;
      end;


    procedure tcoffoutput.writetodisk;
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
            sects[sec]^.secsymidx:=secsymidx;
            inc(initsym,2); { 2 for each section }
          end;
      { For the stab section we need an HdrSym which can now be
        calculated more easily }
        if assigned(sects[sec_stab]) then
         begin
           hstab.strpos:=1;
           hstab.ntype:=0;
           hstab.nother:=0;
           hstab.ndesc:=(sects[sec_stab]^.datasize div sizeof(coffstab))-1{+1 according to gas output PM};
           hstab.nvalue:=sects[sec_stabstr]^.datasize;
           sects[sec_stab]^.data^.seek(0);
           sects[sec_stab]^.data^.write(hstab,sizeof(hstab));
         end;
      { Calculate the filepositions }
        datapos:=sizeof(coffheader)+sizeof(coffsechdr)*nsects;
        { sections first }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            sects[sec]^.datapos:=datapos;
            if assigned(sects[sec]^.data) then
              inc(datapos,sects[sec]^.aligneddatasize);
          end;
        { relocs }
        gotreloc:=false;
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            PCoffSection(sects[sec])^.relocpos:=datapos;
            inc(datapos,10*sects[sec]^.nrelocs);
            if (not gotreloc) and (sects[sec]^.nrelocs>0) then
             gotreloc:=true;
          end;
        { symbols }
        sympos:=datapos;
      { COFF header }
        fillchar(header,sizeof(coffheader),0);
        header.mach:=$14c;
        header.nsects:=nsects;
        header.sympos:=sympos;
        header.syms:=(syms^.size div sizeof(TOutputSymbol))+initsym;
        if gotreloc then
         header.flag:=$104
        else
         header.flag:=$105;
        writer^.write(header,sizeof(header));
      { Section headers }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            fillchar(sechdr,sizeof(sechdr),0);
            move(target_asm.secnames[sec][1],sechdr.name,length(target_asm.secnames[sec]));
            if not win32 then
              begin
                sechdr.rvaofs:=sects[sec]^.mempos;
                sechdr.vsize:=sects[sec]^.mempos;
              end
            else
              begin
                if sec=sec_bss then
                  sechdr.vsize:=sects[sec]^.aligneddatasize;
              end;
            sechdr.datasize:=sects[sec]^.aligneddatasize;
            if (sects[sec]^.datasize>0) and assigned(sects[sec]^.data) then
              sechdr.datapos:=sects[sec]^.datapos;
            sechdr.nrelocs:=sects[sec]^.nrelocs;
            sechdr.relocpos:=PCoffSection(sects[sec])^.relocpos;
            sechdr.flags:=PCoffSection(sects[sec])^.flags;
            writer^.write(sechdr,sizeof(sechdr));
          end;
      { Sections }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) and
            assigned(sects[sec]^.data) then
          begin
            sects[sec]^.alignsection;
            hp:=sects[sec]^.data^.firstblock;
            while assigned(hp) do
             begin
               writer^.write(hp^.data,hp^.used);
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
        i:=strs^.size+4;
        writer^.write(i,4);
        hp:=strs^.firstblock;
        while assigned(hp) do
         begin
           writer^.write(hp^.data,hp^.used);
           hp:=hp^.next;
         end;
      end;

end.
{
  $Log$
  Revision 1.1  2000-11-12 22:20:37  peter
    * create generic toutputsection for binary writers

}
