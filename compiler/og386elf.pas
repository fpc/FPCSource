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

{
  Notes on COFF:

  (0) When I say `standard COFF' below, I mean `COFF as output and
  used by DJGPP'. I assume DJGPP gets it right.

  (1) Win32 appears to interpret the term `relative relocation'
  differently from standard COFF. Standard COFF understands a
  relative relocation to mean that during relocation you add the
  address of the symbol you're referencing, and subtract the base
  address of the section you're in. Win32 COFF, by contrast, seems
  to add the address of the symbol and then subtract the address
  of THE BYTE AFTER THE RELOCATED DWORD. Hence the two formats are
  subtly incompatible.

  (2) Win32 doesn't bother putting any flags in the header flags
  field (at offset 0x12 into the file).

  (3) Win32 uses some extra flags into the section header table:
  it defines flags 0x80000000 (writable), 0x40000000 (readable)
  and 0x20000000 (executable), and uses them in the expected
  combinations. It also defines 0x00100000 through 0x00700000 for
  section alignments of 1 through 64 bytes.

  (4) Both standard COFF and Win32 COFF seem to use the DWORD
  field directly after the section name in the section header
  table for something strange: they store what the address of the
  section start point _would_ be, if you laid all the sections end
  to end starting at zero. Dunno why. Microsoft's documentation
  lists this field as "Virtual Size of Section", which doesn't
  seem to fit at all. In fact, Win32 even includes non-linked
  sections such as .drectve in this calculation.

  (5) Standard COFF does something very strange to common
  variables: the relocation point for a common variable is as far
  _before_ the variable as its size stretches out _after_ it. So
  we must fix up common variable references. Win32 seems to be
  sensible on this one.
}
  interface

    uses
       cobjects,og386,cpubase,aasm;

    type
       preloc = ^treloc;
       treloc = packed record
          next     : preloc;
          address  : longint;
          symbol   : pasmsymbol;
          section  : tsection; { only used if symbol=nil }
          relative : relative_type;
       end;

       psymbol = ^tsymbol;
       tsymbol = packed record
         name    : string[8];
         strpos  : longint;
         section : longint;
         value   : longint;
         typ     : TAsmsymtype;
       end;

       pcoffsection = ^tcoffsection;
       tcoffsection = object
          index : tsection;
          data  : PDynamicArray;
          len,
          pos,
          datapos,
          relocpos,
          nrelocs,
          flags     : longint;
          relochead : PReloc;
          reloctail : ^PReloc;
          constructor init(sec:TSection;Aflags:longint);
          destructor  done;
          procedure  write(var d;l:longint);
          procedure  alloc(l:longint);
          procedure  addsymreloc(ofs:longint;p:pasmsymbol;relative:relative_type);
          procedure  addsectionreloc(ofs:longint;sec:tsection);
       end;

       pgenericcoffoutput = ^tgenericcoffoutput;
       tgenericcoffoutput = object(tobjectoutput)
         win32   : boolean;
         sects   : array[TSection] of PCoffSection;
         strs,
         syms    : Pdynamicarray;
         initsym : longint;
         constructor init;
         destructor  done;virtual;
         procedure initwriting;virtual;
         procedure donewriting;virtual;
         procedure writebytes(var data;len:longint);virtual;
         procedure writealloc(len:longint);virtual;
         procedure writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);virtual;
         procedure writesymbol(p:pasmsymbol);virtual;
         procedure writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);virtual;

         function  text_flags : longint;virtual;
         function  data_flags : longint;virtual;
         function  bss_flags : longint;virtual;
         function  info_flags : longint;virtual;
       private
         procedure createsection(sec:tsection);
         procedure write_relocs(s:pcoffsection);
         procedure write_symbol(const name:string;strpos,value,section,typ,aux:longint);
         procedure write_symbols;
         procedure writetodisk;
       end;

       pdjgppcoffoutput = ^tdjgppcoffoutput;
       tdjgppcoffoutput = object(tgenericcoffoutput)
         constructor init;
         function text_flags : longint;virtual;
         function data_flags : longint;virtual;
         function bss_flags : longint;virtual;
         function info_flags : longint;virtual;
       end;

       pwin32coffoutput = ^twin32coffoutput;
       twin32coffoutput = object(tgenericcoffoutput)
         constructor init;
         function text_flags : longint;virtual;
         function data_flags : longint;virtual;
         function bss_flags : longint;virtual;
         function info_flags : longint;virtual;
       end;

  implementation

      uses
        strings,verbose,
        globtype,globals,files;

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
          datalen  : longint;
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
          section : integer;
          empty   : integer;
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


      const
        sec_2_str : array[tsection] of string[8]=('',
          '.text','.data','.bss',
          '.stab','.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          ''
        );


{****************************************************************************
                               TSection
****************************************************************************}

    constructor tcoffsection.init(sec:TSection;Aflags:longint);
      begin
        index:=sec;
        flags:=AFlags;
        relocHead:=nil;
        relocTail:=@relocHead;
        Len:=0;
        NRelocs:=0;
        if sec=sec_bss then
         data:=nil
        else
         new(Data,Init(1,8192));
      end;


    destructor tcoffsection.done;
      begin
        if assigned(Data) then
          dispose(Data,done);
      end;


    procedure  tcoffsection.write(var d;l:longint);
      begin
        if not assigned(Data) then
         Internalerror(3334441);
        Data^.write(d,l);
        inc(len,l);
      end;


    procedure  tcoffsection.alloc(l:longint);
      begin
        if assigned(Data) then
         Internalerror(3334442);
        inc(len,l);
      end;


    procedure tcoffsection.addsymreloc(ofs:longint;p:pasmsymbol;relative:relative_type);
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
        r^.relative:=relative;
        inc(nrelocs);
      end;


    procedure tcoffsection.addsectionreloc(ofs:longint;sec:tsection);
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
        r^.relative:=relative_false;
        inc(nrelocs);
      end;


{****************************************************************************
                            Genericcoffoutput
****************************************************************************}

    const
{$ifdef TP}
      symbolresize = 50;
      strsresize   = 200;
{$else}
      symbolresize = 200;
      strsresize   = 8192;
{$endif}

    constructor tgenericcoffoutput.init;
      begin
        inherited init;
      end;


    destructor tgenericcoffoutput.done;
      begin
        inherited done;
      end;


    procedure tgenericcoffoutput.initwriting;
      var
        s : string;
      begin
        inherited initwriting;
        { reset }
        initsym:=0;
        new(syms,init(sizeof(TSymbol),symbolresize));
        new(strs,init(1,strsresize));
        FillChar(Sects,sizeof(Sects),0);
        { we need at least the following 3 sections }
        createsection(sec_code);
        createsection(sec_data);
        createsection(sec_bss);
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


    procedure tgenericcoffoutput.donewriting;
      var
        sec : tsection;
      begin
        writetodisk;
        dispose(syms,done);
        dispose(strs,done);
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          dispose(sects[sec],done);
        inherited donewriting;
      end;


    function tgenericcoffoutput.text_flags : longint;
      begin
        text_flags:=0;
      end;

    function tgenericcoffoutput.data_flags : longint;
      begin
        data_flags:=0;
      end;

    function tgenericcoffoutput.bss_flags : longint;
      begin
        bss_flags:=0;
      end;

    function tgenericcoffoutput.info_flags : longint;
      begin
        info_flags:=0;
      end;


    procedure tgenericcoffoutput.createsection(sec:TSection);
      var
        Aflags : longint;
      begin
        Aflags:=0;
        case sec of
          sec_code :
            Aflags:=text_flags;
          sec_data :
            Aflags:=data_flags;
          sec_bss :
            Aflags:=bss_flags;
        { sec_info :
            Aflags:=info_flags; }
          else
            Aflags:=0;
        end;
        sects[sec]:=new(PcoffSection,init(Sec,Aflags));
      end;


    procedure tgenericcoffoutput.writesymbol(p:pasmsymbol);
      var
        pos : longint;
        sym : tsymbol;
        c   : char;
        s   : string;
      begin
        { already written ? }
        if p^.idx<>-1 then
         exit;
        { be sure that the section will exists }
        if (p^.section<>sec_none) and not(assigned(sects[p^.section])) then
          createsection(p^.section);
        { symbolname }
        pos:=strs^.usedsize+4;
        c:=#0;
        s:=p^.name;
        if length(s)>8 then
         begin
           s:=s+#0;
           strs^.write(s[1],length(s));
         end
        else
         pos:=-1;
        FillChar(sym,sizeof(sym),0);
        sym.strpos:=pos;
        if pos=-1 then
         sym.name:=s;
        sym.value:=p^.size;
        sym.typ:=p^.typ;
        { if local of global then set the section value to the address
          of the symbol }
        if p^.typ in [AS_LOCAL,AS_GLOBAL] then
         begin
           sym.section:=ord(p^.section);
           sym.value:=p^.address;
         end;
        { update the asmsymbol index }
        p^.idx:=syms^.count;
        { store the symbol, but not the local ones (PM) }
        if (p^.typ<>AS_LOCAL) or ((copy(s,1,2)<>'.L') and
          ((copy(s,1,1)<>'L') or not win32)) then
          syms^.write(sym,1);
        { make the exported syms known to the objectwriter
          (needed for .a generation) }
        if (p^.typ=AS_GLOBAL) or
           ((p^.typ=AS_EXTERNAL) and (sym.value=p^.size) and (sym.value>0)) then
          writer^.writesym(p^.name);
      end;


    procedure tgenericcoffoutput.writebytes(var data;len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec]^.write(data,len);
      end;


    procedure tgenericcoffoutput.writealloc(len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec]^.alloc(len);
      end;


    procedure tgenericcoffoutput.writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        if assigned(p) then
         begin
           { no symbol relocation need inside a section }
           if p^.section=currsec then
             begin
               if relative=relative_false then
                 begin
                   sects[currsec]^.addsectionreloc(sects[currsec]^.len,currsec);
                   inc(data,p^.address);
                 end
               else if relative=relative_true then
                 begin
                   inc(data,p^.address-len-sects[currsec]^.len);
                 end
               else if relative=relative_rva then
                 begin
                   { don't know if this can happens !! }
                   { does this work ?? }
                   sects[currsec]^.addsectionreloc(sects[currsec]^.len,currsec);
                   inc(data,p^.address);
                 end;
             end
           else
             begin
               writesymbol(p);
               if (p^.section<>sec_none) and (relative=relative_false) then
                 begin
                   sects[currsec]^.addsectionreloc(sects[currsec]^.len,p^.section);
                 end
               else
                 sects[currsec]^.addsymreloc(sects[currsec]^.len,p,relative);
               if not win32 then {seems wrong to me (PM) }
                begin
                  {if p^.section<>sec_none then
                    this is the cause of the strange
                    feature see Note (5) before
                    address contains the size for
                    global vars switched to common }
                    inc(data,p^.address);
                end
               else
                if (relative<>relative_true) and (p^.section<>sec_none) then
                 inc(data,p^.address);
               if relative=relative_true then
                begin
                  if win32 then
                    {inc(data,4-len)}
                    dec(data,len-4{+p^.address})
                  else
                    dec(data,len+sects[currsec]^.len);
                end;
            end;
         end;
        sects[currsec]^.write(data,len);
      end;


    procedure tgenericcoffoutput.writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc : boolean);
      var
        stab : coffstab;
        s : tsection;
      begin
        if section=sec_none then
         s:=currsec
        else
         s:=section;
        { local var can be at offset -1 !! PM }
        if (offset=-1) and reloc then
         begin
           if s=sec_none then
            offset:=0
           else
            offset:=sects[s]^.len;
         end;
        fillchar(stab,sizeof(coffstab),0);
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=sects[sec_stabstr]^.len;
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
          sects[sec_stab]^.addsectionreloc(sects[sec_stab]^.len-4,s);
      end;


    procedure tgenericcoffoutput.write_relocs(s:pcoffsection);
      var
        rel  : coffreloc;
        hr,r : preloc;
      begin
        r:=s^.relochead;
        while assigned(r) do
         begin
           rel.address:=r^.address;
           if assigned(r^.symbol) then
            begin
              if (r^.symbol^.typ=AS_LOCAL) then
               rel.sym:=2*ord(r^.symbol^.section)
              else
               rel.sym:=r^.symbol^.idx+initsym;
            end
           else
            rel.sym:=2*ord(r^.section);
           case r^.relative of
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


    procedure tgenericcoffoutput.write_symbol(const name:string;strpos,value,section,typ,aux:longint);
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


    procedure tgenericcoffoutput.write_symbols;
      var
        filename : string[18];
        sec : tsection;
        i   : longint;
        globalval : byte;
        secrec : coffsectionrec;
        sym : tsymbol;
      begin
        { The `.file' record, and the file name auxiliary record. }
        write_symbol ('.file', -1, 0, -2, $67, 1);
        fillchar(filename,sizeof(filename),0);
        filename:=SplitFileName(current_module^.mainsource^);
        writer^.write(filename[1],sizeof(filename)-1);
        { The section records, with their auxiliaries }
        i:=0;
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            inc(i);
            write_symbol(sec_2_str[sec],-1,{sects[sec]^.pos}0,i,3,1);
            fillchar(secrec,sizeof(secrec),0);
            secrec.len:=sects[sec]^.len;
            secrec.nrelocs:=sects[sec]^.nrelocs;
            writer^.write(secrec,sizeof(secrec));
          end;
        { The real symbols. }
        syms^.seek(0);
        for i:=1 to syms^.count do
         begin
           syms^.read(sym,1);
           if sym.typ=AS_LOCAL then
             globalval:=3
           else
             globalval:=2;
           write_symbol(sym.name,sym.strpos,sym.value,sym.section,globalval,0);
         end;
      end;


    procedure tgenericcoffoutput.writetodisk;
      var
        datapos,
        nsects,pos,sympos,i,fillsize : longint;
        sec    : tsection;
        header : coffheader;
        sechdr : coffsechdr;
        empty  : array[0..15] of byte;
      begin
      { calc amount of sections we have and align sections at 4 bytes }
        fillchar(empty,sizeof(empty),0);
        nsects:=0;
        for sec:=low(tsection) to high(tsection) do
        { .stabstr section length must be without alignment !! }
         if assigned(sects[sec]) then
          begin
          { fill with zero }
            fillsize:=4-(sects[sec]^.len and 3);
            if fillsize<>4 then
             begin
               if assigned(sects[sec]^.data) then
                 sects[sec]^.write(empty,fillsize)
               else
                 sects[sec]^.alloc(fillsize);
               { .stabstr section length must be without alignment !! }
               if (sec=sec_stabstr) then
                 dec(sects[sec]^.len,fillsize);
             end;
            inc(nsects);
          end;
      { Calculate the filepositions }
        datapos:=sizeof(coffheader)+sizeof(coffsechdr)*nsects;
        pos:=0;
        initsym:=2; { 2 for the file }
        { sections first }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            sects[sec]^.pos:=pos;
            sects[sec]^.datapos:=datapos;
            inc(pos,sects[sec]^.len);
            if assigned(sects[sec]^.data) then
              inc(datapos,sects[sec]^.len);
            { align after stabstr section !! }
            if (sec=sec_stabstr) and ((sects[sec]^.len and 3)<>0) then
              inc(datapos,4-(sects[sec]^.len and 3));
            inc(initsym,2); { 2 for each section }
          end;
        { relocs }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            sects[sec]^.relocpos:=datapos;
            inc(datapos,10*sects[sec]^.nrelocs);
          end;
        { symbols }
        sympos:=datapos;
      { COFF header }
        fillchar(header,sizeof(coffheader),0);
        header.mach:=$14c;
        header.nsects:=nsects;
        header.sympos:=sympos;
        header.syms:=syms^.count+initsym;
        if not win32 then
         header.flag:=$104;
        writer^.write(header,sizeof(header));
      { Section headers }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          begin
            fillchar(sechdr,sizeof(sechdr),0);
            move(sec_2_str[sec][1],sechdr.name,length(sec_2_str[sec]));
            if not win32 then
              sechdr.vsize:=sects[sec]^.pos
            else if sec=sec_bss then
              sechdr.vsize:=sects[sec]^.len;
            sechdr.datalen:=sects[sec]^.len;
            { apparently win32 asw leaves section at datapos zero }
            { this was an error by me (PM) }
            if (sects[sec]^.len>0) and assigned(sects[sec]^.data) then
              sechdr.datapos:=sects[sec]^.datapos;
            sechdr.relocpos:=sects[sec]^.relocpos;
            sechdr.nrelocs:=sects[sec]^.nrelocs;
            sechdr.flags:=sects[sec]^.flags;
            writer^.write(sechdr,sizeof(sechdr));
          end;
      { Sections }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) and
            assigned(sects[sec]^.data) then
          begin
            { For the stab section we need an HdrSym which can now be
              calculated more easily }
            if sec=sec_stab then
             begin
               pcoffstab(sects[sec_stab]^.data^.data)^.nvalue:=sects[sec_stabstr]^.len;
               pcoffstab(sects[sec_stab]^.data^.data)^.strpos:=1;
               pcoffstab(sects[sec_stab]^.data^.data)^.ndesc:=
                 (sects[sec_stab]^.len div sizeof(coffstab))-1{+1 according to gas output PM};
             end;
            writer^.write(sects[sec]^.data^.data^,sects[sec]^.data^.usedsize);
          end;
      { Relocs }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          write_relocs(sects[sec]);
      { Symbols }
        write_symbols;
      { Strings }
        i:=strs^.usedsize+4;
        writer^.write(i,4);
        writer^.write(strs^.data^,strs^.usedsize);
      end;


{****************************************************************************
                            DJGppcoffoutput
****************************************************************************}

    constructor tdjgppcoffoutput.init;
      begin
        inherited init;
        win32:=false;
      end;

    function tdjgppcoffoutput.text_flags : longint;
      begin
        text_flags:=$20;
      end;

    function tdjgppcoffoutput.data_flags : longint;
      begin
        data_flags:=$40;
      end;

    function tdjgppcoffoutput.bss_flags : longint;
      begin
        bss_flags:=$80;
      end;

    function tdjgppcoffoutput.info_flags : longint;
      begin
        writeln('djgpp coff doesn''t support info sections');
        info_flags:=$40;
      end;


{****************************************************************************
                            Win32coffoutput
****************************************************************************}

    constructor twin32coffoutput.init;
      begin
        inherited init;
        win32:=true;
      end;

    function twin32coffoutput.text_flags : longint;
      begin
        text_flags:={ $60500020}$60300020{changed to get same as asw.exe (PM)};
      end;

    function twin32coffoutput.data_flags : longint;
      begin
        data_flags:=$c0300040;
      end;

    function twin32coffoutput.bss_flags : longint;
      begin
        bss_flags:=$c0300080;
      end;

    function twin32coffoutput.info_flags : longint;
      begin
        info_flags:=$100a00;
      end;


end.
{
  $Log$
  Revision 1.4  2000-02-09 13:22:54  peter
    * log truncated

  Revision 1.3  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.2  1999/08/04 00:23:07  florian
    * renamed i386asm and i386base to cpuasm and cpubase

}
