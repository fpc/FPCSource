{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

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
       cclasses,cobjects,
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
          symbol   : pasmsymbol;
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
          procedure addsymreloc(ofs:longint;p:pasmsymbol;relative:relative_type);
          procedure addsectionreloc(ofs:longint;sec:tsection;relative:relative_type);
       end;

       tobjectdata = class
         { section }
         currsec   : tsection;
         sects     : array[TSection] of tobjectsection;
         constructor create;
         destructor  destroy;override;
         procedure createsection(sec:tsection);virtual;
         procedure defaultsection(sec:tsection);
         function  sectionsize(s:tsection):longint;
         procedure setsectionsizes(var s:tsecsize);virtual;
         procedure alloc(len:longint);
         procedure allocalign(len:longint);
         procedure writebytes(var data;len:longint);
         procedure writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);virtual;abstract;
         procedure writesymbol(p:pasmsymbol);virtual;abstract;
         procedure writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);virtual;abstract;
         procedure writesymstabs(section:tsection;offset:longint;p:pchar;ps:pasmsymbol;nidx,nother,line:longint;reloc:boolean);virtual;abstract;
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
         path      : pathstr;
         ObjFile   : string;
         { smartlinking }
         objsmart  : boolean;
         place     : tcutplace;
         SmartFilesCount,
         SmartHeaderCount : longint;
         { writer }
         writer    : tobjectwriter;
         { section }
         data      : tobjectdata;
         { Writing }
         procedure NextSmartName;
       protected
         procedure writetodisk;virtual;
       public
         constructor create(smart:boolean);
         destructor  destroy;override;
         function  initwriting(Aplace:tcutplace):tobjectdata;virtual;
         procedure donewriting;virtual;
         procedure exportsymbol(p:pasmsymbol);
       end;

    var
      { current object data, used in ag386bin/cpuasm }
      objectdata   : tobjectdata;
      { current object allocator }
      objectalloc  : tobjectalloc;
      { current object writer used }
      objectoutput : tobjectoutput;


implementation

    uses
      comphook,
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


    procedure tobjectsection.addsymreloc(ofs:longint;p:pasmsymbol;relative:relative_type);
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
      end;


    destructor tobjectdata.destroy;
      var
        sec : tsection;
      begin
        { free memory }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          sects[sec].free;
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


{****************************************************************************
                                tobjectoutput
****************************************************************************}

    constructor tobjectoutput.create(smart:boolean);
      begin
        SmartFilesCount:=0;
        SmartHeaderCount:=0;
        objsmart:=smart;
        objfile:=current_module.objfilename^;
      { Which path will be used ? }
        if objsmart and
           (cs_asm_leave in aktglobalswitches) then
         begin
           path:=current_module.path^+FixFileName(current_module.modulename^)+target_info.smartext;
           {$I-}
            mkdir(path);
           {$I+}
           if ioresult<>0 then;
           path:=FixPath(path,false);
         end
        else
         path:=current_module.path^;
      { init writer }
        if objsmart and
           not(cs_asm_leave in aktglobalswitches) then
          writer:=tarobjectwriter.create(current_module.staticlibfilename^)
        else
          writer:=tobjectwriter.create;
      end;


    destructor tobjectoutput.destroy;
      begin
        writer.free;
      end;


    procedure tobjectoutput.NextSmartName;
      var
        s : string;
      begin
        inc(SmartFilesCount);
        if SmartFilesCount>999999 then
         Message(asmw_f_too_many_asm_files);
        if (cs_asm_leave in aktglobalswitches) then
         s:=current_module.asmprefix^
        else
         s:=current_module.modulename^;
        case place of
          cut_begin :
            begin
              inc(SmartHeaderCount);
              s:=s+tostr(SmartHeaderCount)+'h';
            end;
          cut_normal :
            s:=s+tostr(SmartHeaderCount)+'s';
          cut_end :
            s:=s+tostr(SmartHeaderCount)+'t';
        end;
        ObjFile:=FixFileName(s+tostr(SmartFilesCount)+target_info.objext);
      end;


    procedure tobjectoutput.writetodisk;
      begin
      end;


    function tobjectoutput.initwriting(Aplace:tcutplace):tobjectdata;
      begin
        place:=Aplace;
        { the data should be set by the real output like coffoutput }
        data:=nil;
        initwriting:=nil;
        { open the writer }
        if objsmart then
         NextSmartName;
        writer.createfile(objfile);
      end;


    procedure tobjectoutput.donewriting;
      begin
        { Only write the .o if there are no errors }
        if errorcount=0 then
          writetodisk;
        { close the writer }
        writer.closefile;
        { free data }
        data.free;
        data:=nil;
      end;


    procedure tobjectoutput.exportsymbol(p:pasmsymbol);
      begin
        { export globals and common symbols, this is needed
          for .a files }
        if p^.bind in [AB_GLOBAL,AB_COMMON] then
         writer.writesym(p^.name);
      end;

end.
{
  $Log$
  Revision 1.5  2000-12-25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.4  2000/12/24 12:25:31  peter
    + cstreams unit
    * dynamicarray object to class

  Revision 1.3  2000/12/23 19:59:35  peter
    * object to class for ow/og objects
    * split objectdata from objectoutput

  Revision 1.2  2000/11/13 21:56:07  peter
    * removed some virtual from methods
    * sectionsize method implemented (fixes lineinfo stabs)

  Revision 1.1  2000/11/12 22:20:37  peter
    * create generic tobjectsection for binary writers

}
