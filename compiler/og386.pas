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

  interface
    uses
{$ifdef Delphi}
       dmisc,
{$else Delphi}
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
      strings,comphook,
      globtype,globals,verbose,files,
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
      var
        i : longint;
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
           i:=ioresult;
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
        ObjFile:=Path+FixFileName(s+tostr(SmartLinkFilesCnt)+target_info.objext)
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
  Revision 1.16  2000-01-07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.15  1999/11/08 10:37:12  peter
    * filename fixes for win32 imports for units with multiple needed dll's

  Revision 1.14  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.13  1999/11/02 15:06:57  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.12  1999/09/07 15:22:20  pierre
   * runerror => do_halt

  Revision 1.11  1999/08/04 00:23:04  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.10  1999/07/18 10:19:56  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.9  1999/07/03 00:27:03  peter
    * better smartlinking support

  Revision 1.8  1999/05/19 12:41:48  florian
    * made source compilable with TP (too long line)
    * default values for set properties fixed

  Revision 1.7  1999/05/19 11:54:18  pierre
   + experimental code for externalbss and stabs problem

  Revision 1.6  1999/05/07 00:36:56  pierre
    * added alignment code for .bss
    * stabs correct but externalbss disabled
      would need a special treatment in writestabs

  Revision 1.5  1999/05/05 22:21:57  peter
    * updated messages

  Revision 1.4  1999/05/05 17:34:30  peter
    * output is more like as 2.9.1
    * stabs really working for go32v2

  Revision 1.3  1999/05/04 21:44:50  florian
    * changes to compile it with Delphi 4.0

  Revision 1.2  1999/05/02 22:41:54  peter
    * moved section names to systems
    * fixed nasm,intel writer

  Revision 1.1  1999/05/01 13:24:23  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.8  1999/03/18 20:30:48  peter
    + .a writer

  Revision 1.7  1999/03/10 13:41:09  pierre
   + partial implementation for win32 !
     winhello works but pp still does not !

  Revision 1.6  1999/03/08 14:51:08  peter
    + smartlinking for ag386bin

  Revision 1.5  1999/03/05 13:09:51  peter
    * first things for tai_cut support for ag386bin

  Revision 1.4  1999/03/03 01:36:45  pierre
    + stabs output working (though not really tested)
      for a simple file the only difference to GAS output is due
      to the VMA of the different sections

  Revision 1.3  1999/03/02 02:56:26  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.2  1999/02/25 21:03:09  peter
    * ag386bin updates
    + coff writer

}
