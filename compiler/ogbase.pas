{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    Contains the base stuff for binary object file writers

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
unit ogbase;

{$i defines.inc}

interface
    uses
{$ifdef Delphi}
       sysutils,
       dmisc,
{$else Delphi}
       strings,
       dos,
{$endif Delphi}
       { common }
       cclasses,
       { targets }
       systems,
       { outputwriters }
       owbase,owar,
       { assembler }
       cpubase,aasm;

    type
       tsecsize = array[tsection] of longint;

       relative_type = (relative_false,relative_true,relative_rva);

       poutputreloc = ^toutputreloc;
       toutputreloc = packed record
          next     : poutputreloc;
          address  : longint;
          symbol   : tasmsymbol;
          section  : tsection; { only used if symbol=nil }
          typ      : relative_type;
       end;

       poutputsymbol = ^toutputsymbol;
       toutputsymbol = packed record
         namestr : string[8];    { namestr or nameidx will be used }
         nameidx : longint;
         section : tsection;
         value   : longint;
         bind    : TAsmsymbind;
         typ     : TAsmsymtype;
         size    : longint;
       end;

       tobjectsection = class
         name      : string[32];
         secsymidx : longint; { index for the section in symtab }
         addralign : longint;
         { size of the data and in the file }
         data      : TDynamicArray;
         datasize  : longint;
         datapos   : longint;
         { size and position in memory, set by setsectionsize }
         memsize,
         mempos    : longint;
         { relocation }
         nrelocs   : longint;
         relochead : POutputReloc;
         reloctail : ^POutputReloc;
         constructor create(const Aname:string;Aalign:longint;alloconly:boolean);
         destructor  destroy;override;
         function  write(var d;l:longint):longint;
         function  writestr(const s:string):longint;
         procedure writealign(l:longint);
         function  aligneddatasize:longint;
         procedure alignsection;
         procedure alloc(l:longint);
         procedure addsymreloc(ofs:longint;p:tasmsymbol;relative:relative_type);
         procedure addsectionreloc(ofs:longint;sec:tsection;relative:relative_type);
       end;

       tobjectdata = class
         { section }
         currsec   : tsection;
         sects     : array[TSection] of tobjectsection;
         localsyms : tdictionary;
         constructor create;
         destructor  destroy;override;
         procedure createsection(sec:tsection);virtual;
         procedure defaultsection(sec:tsection);
         function  sectionsize(s:tsection):longint;
         function  currsectionsize:longint;
         procedure setsectionsizes(var s:tsecsize);virtual;
         procedure alloc(len:longint);
         procedure allocalign(len:longint);
         procedure writebytes(var data;len:longint);
         procedure writereloc(data,len:longint;p:tasmsymbol;relative:relative_type);virtual;abstract;
         procedure writesymbol(p:tasmsymbol);virtual;abstract;
         procedure writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);virtual;abstract;
         procedure writesymstabs(section:tsection;offset:longint;p:pchar;ps:tasmsymbol;nidx,nother,line:longint;reloc:boolean);virtual;abstract;
         procedure addsymbol(p:tasmsymbol);
       end;

       tobjectalloc = class
         currsec : tsection;
         secsize : tsecsize;
         constructor create;
         destructor  destroy;override;
         procedure setsection(sec:tsection);
         function  sectionsize:longint;
         procedure sectionalloc(l:longint);
         procedure sectionalign(l:longint);
         procedure staballoc(p:pchar);
         procedure resetsections;
       end;

       tobjectoutput = class
       protected
         { writer }
         FWriter    : tobjectwriter;
         { section }
         FData      : tobjectdata;
         procedure writetodisk;virtual;
       public
         constructor create(smart:boolean);
         destructor  destroy;override;
         function  initwriting(const fn:string):boolean;virtual;
         procedure donewriting;virtual;
         procedure exportsymbol(p:tasmsymbol);
         property Data:TObjectData read FData write FData;
         property Writer:TObjectWriter read FWriter;
       end;

       tobjectinput = class
       protected
         FObjFile   : string;
         { writer }
         FReader    : tobjectreader;
       protected
         { section }
         FData      : tobjectdata;
         function  str2sec(const s:string):tsection;
       public
         constructor create(const fn:string);
         destructor  destroy;override;
         function  initreading:boolean;virtual;
         procedure donereading;virtual;
         procedure readfromdisk;virtual;
         property Data:TObjectData read FData write FData;
         property Reader:TObjectReader read FReader;
       end;

    var
      { current object data, used in ag386bin/cpuasm }
      objectdata   : tobjectdata;
      { current object allocator }
      objectalloc  : tobjectalloc;
      { current object writer used }
      objectoutput : tobjectoutput;

      { globals }
      globalsyms : tdictionary;


implementation

    uses
      cutils,globtype,globals,verbose,fmodule;


{****************************************************************************
                                tobjectalloc
****************************************************************************}

    constructor tobjectalloc.create;
      begin
      end;


    destructor tobjectalloc.destroy;
      begin
      end;


    procedure tobjectalloc.setsection(sec:tsection);
      begin
        currsec:=sec;
      end;


    procedure tobjectalloc.resetsections;
      begin
        FillChar(secsize,sizeof(secsize),0);
      end;


    procedure tobjectalloc.sectionalloc(l:longint);
      begin
        inc(secsize[currsec],l);
      end;


    procedure tobjectalloc.sectionalign(l:longint);
      begin
        if (secsize[currsec] mod l)<>0 then
          inc(secsize[currsec],l-(secsize[currsec] mod l));
      end;


    procedure tobjectalloc.staballoc(p:pchar);
      begin
        inc(secsize[sec_stab]);
        if assigned(p) and (p[0]<>#0) then
          inc(secsize[sec_stabstr],strlen(p)+1);
      end;


    function tobjectalloc.sectionsize:longint;
      begin
        sectionsize:=secsize[currsec];
      end;


{****************************************************************************
                              TSectionOutput
****************************************************************************}

    constructor tobjectsection.create(const Aname:string;Aalign:longint;alloconly:boolean);
      begin
        name:=Aname;
        secsymidx:=0;
        addralign:=Aalign;
        { data }
        datasize:=0;
        datapos:=0;
        if alloconly then
         data:=nil
        else
         Data:=TDynamicArray.Create(8192);
        { position }
        mempos:=0;
        memsize:=0;
        { relocation }
        NRelocs:=0;
        relocHead:=nil;
        relocTail:=@relocHead;
      end;


    destructor tobjectsection.destroy;
      begin
        if assigned(Data) then
          Data.Free;
      end;


    function tobjectsection.write(var d;l:longint):longint;
      begin
        write:=datasize;
        if not assigned(Data) then
         Internalerror(3334441);
        Data.write(d,l);
        inc(datasize,l);
      end;


    function tobjectsection.writestr(const s:string):longint;
      begin
        writestr:=datasize;
        if not assigned(Data) then
         Internalerror(3334441);
        Data.write(s[1],length(s));
        inc(datasize,length(s));
      end;


    procedure tobjectsection.writealign(l:longint);
      var
        i : longint;
        empty : array[0..63] of char;
      begin
        { no alignment needed for 0 or 1 }
        if l<=1 then
         exit;
        i:=datasize mod l;
        if i>0 then
         begin
           if assigned(data) then
            begin
              fillchar(empty,sizeof(empty),0);
              Data.write(empty,l-i);
            end;
           inc(datasize,l-i);
         end;
      end;


    function tobjectsection.aligneddatasize:longint;
      begin
        aligneddatasize:=align(datasize,addralign);
      end;


    procedure tobjectsection.alignsection;
      begin
        writealign(addralign);
      end;


    procedure tobjectsection.alloc(l:longint);
      begin
        if assigned(Data) then
         Internalerror(3334442);
        inc(datasize,l);
      end;


    procedure tobjectsection.addsymreloc(ofs:longint;p:tasmsymbol;relative:relative_type);
      var
        r : POutputReloc;
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


    procedure tobjectsection.addsectionreloc(ofs:longint;sec:tsection;relative:relative_type);
      var
        r : POutputReloc;
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
                                tobjectdata
****************************************************************************}

    constructor tobjectdata.create;
      begin
        { reset }
        FillChar(Sects,sizeof(Sects),0);
        localsyms:=tdictionary.create;
        localsyms.usehash;
      end;


    destructor tobjectdata.destroy;
      var
        sec : tsection;
      begin
        { free memory }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          sects[sec].free;
        localsyms.free;
      end;


    procedure tobjectdata.createsection(sec:tsection);
      begin
        sects[sec]:=tobjectsection.create(target_asm.secnames[sec],1,(sec=sec_bss));
      end;


    function tobjectdata.sectionsize(s:tsection):longint;
      begin
        if assigned(sects[s]) then
         sectionsize:=sects[s].datasize
        else
         sectionsize:=0;
      end;


    function tobjectdata.currsectionsize:longint;
      begin
        if assigned(sects[currsec]) then
         currsectionsize:=sects[currsec].datasize
        else
         currsectionsize:=0;
      end;


    procedure tobjectdata.setsectionsizes(var s:tsecsize);
      begin
      end;


    procedure tobjectdata.defaultsection(sec:tsection);
      begin
        currsec:=sec;
      end;


    procedure tobjectdata.writebytes(var data;len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec].write(data,len);
      end;


    procedure tobjectdata.alloc(len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec].alloc(len);
      end;


    procedure tobjectdata.allocalign(len:longint);
      var
        modulo : longint;
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        modulo:=sects[currsec].datasize mod len;
        if modulo > 0 then
          sects[currsec].alloc(len-modulo);
      end;


    procedure tobjectdata.addsymbol(p:tasmsymbol);
      begin
        if (p.bind=AB_LOCAL) then
         localsyms.insert(p)
        else
         globalsyms.insert(p);
      end;


{****************************************************************************
                                tobjectoutput
****************************************************************************}

    constructor tobjectoutput.create(smart:boolean);
      begin
      { init writer }
        if smart and
           not(cs_asm_leave in aktglobalswitches) then
          FWriter:=tarobjectwriter.create(current_module.staticlibfilename^)
        else
          FWriter:=tobjectwriter.create;
      end;


    destructor tobjectoutput.destroy;
      begin
        FWriter.free;
      end;


    procedure tobjectoutput.writetodisk;
      begin
      end;


    function tobjectoutput.initwriting(const fn:string):boolean;
      begin
        { the data should be set by the real output like coffoutput }
        FData:=nil;
        initwriting:=FWriter.createfile(fn);
      end;


    procedure tobjectoutput.donewriting;
      begin
        { Only write the .o if there are no errors }
        if errorcount=0 then
          writetodisk;
        { close the writer }
        FWriter.closefile;
        { free data }
        FData.free;
        FData:=nil;
      end;


    procedure tobjectoutput.exportsymbol(p:tasmsymbol);
      begin
        { export globals and common symbols, this is needed
          for .a files }
        if p.bind in [AB_GLOBAL,AB_COMMON] then
         FWriter.writesym(p.name);
      end;


{****************************************************************************
                                tobjectinput
****************************************************************************}

    constructor tobjectinput.create(const fn:string);
      begin
        FObjfile:=fn;
        FData:=nil;
      { init reader }
        FReader:=tobjectreader.create;
      end;


    destructor tobjectinput.destroy;
      begin
        FReader.free;
      end;


    function tobjectinput.initreading:boolean;
      begin
        { the data should be set by the real output like coffoutput }
        FData:=nil;
        { open the reader }
        initreading:=FReader.openfile(FObjfile);
      end;


    procedure tobjectinput.donereading;
      begin
        { close the writer }
        FReader.closefile;
        { free data }
        FData.free;
        FData:=nil;
      end;


    procedure tobjectinput.readfromdisk;
      begin
      end;


    function tobjectinput.str2sec(const s:string):tsection;
      var
        t : tsection;
      begin
        for t:=low(tsection) to high(tsection) do
         begin
           if (s=target_asm.secnames[t]) then
            begin
              str2sec:=t;
              exit;
            end;
         end;
        str2sec:=sec_none;
      end;



end.
{
  $Log$
  Revision 1.9  2002-05-14 19:34:43  peter
    * removed old logs and updated copyright year

}
