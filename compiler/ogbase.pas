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
       cobjects,
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
         namestr : string[8]; { namestr or nameidx is used }
         nameidx : longint;
         section : tsection;
         value   : longint;
         bind    : TAsmsymbind;
         typ     : TAsmsymtype;
         size    : longint;
       end;

       poutputsection = ^toutputsection;
       toutputsection = object
          name      : string[32];
          secsymidx : longint; { index for the section in symtab }
          addralign : longint;
          { size of the data and in the file }
          data      : PDynamicArray;
          datasize   : longint;
          datapos   : longint;
          { size and position in memory, set by setsectionsize }
          memsize,
          mempos    : longint;
          { relocation }
          nrelocs   : longint;
          relochead : POutputReloc;
          reloctail : ^POutputReloc;
          constructor init(const Aname:string;Aalign:longint;alloconly:boolean);
          destructor  done;
          function  write(var d;l:longint):longint;
          function  writestr(const s:string):longint;
          procedure writealign(l:longint);
          function  aligneddatasize:longint;
          procedure alignsection;
          procedure alloc(l:longint);
          procedure addsymreloc(ofs:longint;p:pasmsymbol;relative:relative_type);
          procedure addsectionreloc(ofs:longint;sec:tsection;relative:relative_type);
       end;

       pobjectalloc = ^tobjectalloc;
       tobjectalloc = object
         currsec : tsection;
         secsize : tsecsize;
         constructor init;
         destructor  done;
         procedure setsection(sec:tsection);
         function  sectionsize:longint;
         procedure sectionalloc(l:longint);
         procedure sectionalign(l:longint);
         procedure staballoc(p:pchar);
         procedure resetsections;
       end;

       pobjectoutput = ^tobjectoutput;
       tobjectoutput = object
         writer    : pobjectwriter;
         path      : pathstr;
         ObjFile   : string;
         { smartlinking }
         objsmart  : boolean;
         place     : tcutplace;
         SmartFilesCount,
         SmartHeaderCount : longint;
         { section }
         currsec   : tsection;
         sects     : array[TSection] of POutputSection;
         constructor init(smart:boolean);
         destructor  done;virtual;
         { Writing }
         procedure NextSmartName;
         procedure initwriting(Aplace:tcutplace);virtual;
         procedure donewriting;virtual;
         procedure createsection(sec:tsection);virtual;
         procedure defaultsection(sec:tsection);
         function  sectionsize(s:tsection):longint;virtual;
         procedure setsectionsizes(var s:tsecsize);virtual;
         procedure alloc(len:longint);virtual;
         procedure allocalign(len:longint);virtual;
         procedure writebytes(var data;len:longint);virtual;
         procedure writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);virtual;
         procedure writesymbol(p:pasmsymbol);virtual;
         procedure writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);virtual;
         procedure writesymstabs(section:tsection;offset:longint;p:pchar;ps:pasmsymbol;
           nidx,nother,line:longint;reloc:boolean);virtual;
       end;

    var
      objectalloc  : pobjectalloc;
      objectoutput : pobjectoutput;

  implementation

    uses
      comphook,
      cutils,globtype,globals,verbose,fmodule;


{****************************************************************************
                                tobjectalloc
****************************************************************************}

    constructor tobjectalloc.init;
      begin
      end;


    destructor tobjectalloc.done;
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

    constructor toutputsection.init(const Aname:string;Aalign:longint;alloconly:boolean);
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
         new(Data,Init(8192));
        { position }
        mempos:=0;
        memsize:=0;
        { relocation }
        NRelocs:=0;
        relocHead:=nil;
        relocTail:=@relocHead;
      end;


    destructor toutputsection.done;
      begin
        if assigned(Data) then
          dispose(Data,done);
      end;


    function toutputsection.write(var d;l:longint):longint;
      begin
        write:=datasize;
        if not assigned(Data) then
         Internalerror(3334441);
        Data^.write(d,l);
        inc(datasize,l);
      end;


    function toutputsection.writestr(const s:string):longint;
      begin
        writestr:=datasize;
        if not assigned(Data) then
         Internalerror(3334441);
        Data^.write(s[1],length(s));
        inc(datasize,length(s));
      end;


    procedure toutputsection.writealign(l:longint);
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
              data^.write(empty,l-i);
            end;
           inc(datasize,l-i);
         end;
      end;


    function toutputsection.aligneddatasize:longint;
      begin
        aligneddatasize:=align(datasize,addralign);
      end;


    procedure toutputsection.alignsection;
      begin
        writealign(addralign);
      end;


    procedure toutputsection.alloc(l:longint);
      begin
        if assigned(Data) then
         Internalerror(3334442);
        inc(datasize,l);
      end;


    procedure toutputsection.addsymreloc(ofs:longint;p:pasmsymbol;relative:relative_type);
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


    procedure toutputsection.addsectionreloc(ofs:longint;sec:tsection;relative:relative_type);
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
                                tobjectoutput
****************************************************************************}

    constructor tobjectoutput.init(smart:boolean);
      begin
        SmartFilesCount:=0;
        SmartHeaderCount:=0;
        objsmart:=smart;
        objfile:=current_module^.objfilename^;
      { Which path will be used ? }
        if objsmart and
           (cs_asm_leave in aktglobalswitches) then
         begin
           path:=current_module^.path^+FixFileName(current_module^.modulename^)+target_info.smartext;
           {$I-}
            mkdir(path);
           {$I+}
           if ioresult<>0 then;
           path:=FixPath(path,false);
         end
        else
         path:=current_module^.path^;
      { init writer }
        if objsmart and
           not(cs_asm_leave in aktglobalswitches) then          writer:=New(parobjectwriter,Init(current_module^.staticlibfilename^))
        else
          writer:=New(pobjectwriter,Init);
      end;


    destructor tobjectoutput.done;
      begin
        Dispose(writer,done);
      end;


    procedure tobjectoutput.NextSmartName;
      var
        s : string;
      begin
        inc(SmartFilesCount);
        if SmartFilesCount>999999 then
         Message(asmw_f_too_many_asm_files);
        if (cs_asm_leave in aktglobalswitches) then
         s:=current_module^.asmprefix^
        else
         s:=current_module^.modulename^;
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


    procedure tobjectoutput.initwriting(Aplace:tcutplace);
      begin
        place:=Aplace;
        { open the writer }
        if objsmart then
         NextSmartName;
        writer^.create(objfile);
        { reset }
        FillChar(Sects,sizeof(Sects),0);
      end;


    procedure tobjectoutput.donewriting;
      var
        sec : tsection;
      begin
        { free memory }
        for sec:=low(tsection) to high(tsection) do
         if assigned(sects[sec]) then
          dispose(sects[sec],done);
        { close the writer }
        writer^.close;
      end;


    procedure tobjectoutput.createsection(sec:tsection);
      begin
        sects[sec]:=new(poutputsection,init(target_asm.secnames[sec],1,(sec=sec_bss)));
      end;


    function tobjectoutput.sectionsize(s:tsection):longint;
      begin
        sectionsize:=0;
      end;


    procedure tobjectoutput.setsectionsizes(var s:tsecsize);
      begin
      end;


    procedure tobjectoutput.defaultsection(sec:tsection);
      begin
        currsec:=sec;
      end;


    procedure tobjectoutput.writebytes(var data;len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec]^.write(data,len);
      end;


    procedure tobjectoutput.alloc(len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec]^.alloc(len);
      end;


    procedure tobjectoutput.allocalign(len:longint);
      var
        modulo : longint;
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        modulo:=sects[currsec]^.datasize mod len;
        if modulo > 0 then
          sects[currsec]^.alloc(len-modulo);
      end;


    procedure tobjectoutput.writesymbol(p:pasmsymbol);
      begin
        Do_halt(211);
      end;


    procedure tobjectoutput.writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);
      begin
        Do_halt(211);
      end;


   procedure tobjectoutput.writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);
      begin
        Do_halt(211);
      end;


   procedure tobjectoutput.writesymstabs(section:tsection;offset:longint;p:pchar;ps:pasmsymbol;
                                         nidx,nother,line:longint;reloc:boolean);
      begin
        Do_halt(211);
      end;

end.
{
  $Log$
  Revision 1.1  2000-11-12 22:20:37  peter
    * create generic toutputsection for binary writers

}
