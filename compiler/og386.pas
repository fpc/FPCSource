{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    Contains the base stuff for 386 binary object file writers

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
unit og386;

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
       owbase,owar,
       systems,cpubase,aasm;

    type
       tsecsize = array[tsection] of longint;

       relative_type = (relative_false,relative_true,relative_rva);

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
         smarthcount : longint;
         objsmart  : boolean;
         writer    : pobjectwriter;
         path      : pathstr;
         ObjFile   : string;
         place     : tcutplace;
         currsec   : tsection;
         constructor init(smart:boolean);
         destructor  done;virtual;
         { Writing }
         procedure NextSmartName;
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
         procedure defaultsection(sec:tsection);
       end;

    var
      objectalloc  : pobjectalloc;
      objectoutput : pobjectoutput;

  implementation

    uses
      comphook,
      cutils,globtype,globals,verbose,fmodule,
      assemble;


{****************************************************************************
                                tobjectoutput
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
                                tobjectoutput
****************************************************************************}

    constructor tobjectoutput.init(smart:boolean);
      begin
        smarthcount:=0;
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
           not(cs_asm_leave in aktglobalswitches) then
          writer:=New(parobjectwriter,Init(current_module^.staticlibfilename^))
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
        inc(SmartLinkFilesCnt);
        if SmartLinkFilesCnt>999999 then
         Message(asmw_f_too_many_asm_files);
        if (cs_asm_leave in aktglobalswitches) then
         s:=current_module^.asmprefix^
        else
         s:=current_module^.modulename^;
        case place of
          cut_begin :
            begin
              inc(smarthcount);
              s:=s+tostr(smarthcount)+'h';
            end;
          cut_normal :
            s:=s+tostr(smarthcount)+'s';
          cut_end :
            s:=s+tostr(smarthcount)+'t';
        end;
        ObjFile:=FixFileName(s+tostr(SmartLinkFilesCnt)+target_info.objext);
      end;


    procedure tobjectoutput.initwriting(Aplace:tcutplace);
      begin
        place:=Aplace;
        if objsmart then
         NextSmartName;
        writer^.create(objfile);
      end;


    procedure tobjectoutput.donewriting;
      begin
        writer^.close;
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

    procedure tobjectoutput.writesymbol(p:pasmsymbol);
      begin
        Do_halt(211);
      end;

    procedure tobjectoutput.writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);
      begin
        Do_halt(211);
      end;

    procedure tobjectoutput.writebytes(var data;len:longint);
      begin
        Do_halt(211);
      end;

    procedure tobjectoutput.writealloc(len:longint);
      begin
        Do_halt(211);
      end;

    procedure tobjectoutput.writealign(len:longint);
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
  Revision 1.6  2000-09-24 15:06:19  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/06 10:42:29  peter
    * merged patches name generation in lib and asm constant eval

  Revision 1.3  2000/07/13 12:08:26  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:43  michael
  + removed logs

}
