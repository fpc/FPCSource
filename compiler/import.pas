{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    This unit implements an uniform import object

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

 ****************************************************************************}
unit import;
interface

uses
  cobjects;

type
   pimported_item = ^timported_item;
   timported_item = object(tlinkedlist_item)
      ordnr  : word;
      name,
      func   : pstring;
      lab    : pointer; { should be plabel, but this gaves problems with circular units }
      is_var : boolean;
      constructor init(const n,s : string;o : word);
      constructor init_var(const n,s : string);
      destructor done;virtual;
   end;

   pimportlist = ^timportlist;
   timportlist = object(tlinkedlist_item)
      dllname : pstring;
      imported_items : plinkedlist;
      constructor init(const n : string);
      destructor done;virtual;
   end;

   pimportlib=^timportlib;
   timportlib=object
      constructor Init;
      destructor Done;
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure importvariable(const varname,module:string;const name:string);virtual;
      procedure generatelib;virtual;
      procedure generatesmartlib;virtual;
   end;

var
  importlib : pimportlib;

procedure InitImport;
procedure DoneImport;

implementation

uses
  systems,verbose,globals
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
  {$ifndef NOTARGETOS2}
    ,t_os2
  {$endif}
  {$ifndef NOTARGETWIN32}
    ,t_win32
  {$endif}
  {$ifndef NOTARGETGO32V2}
    ,t_go32v2
  {$endif}
{$endif}
{$ifdef m68k}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
{$ifdef powerpc}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
{$ifdef alpha}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
  ;

{****************************************************************************
                           Timported_item
****************************************************************************}

constructor timported_item.init(const n,s : string;o : word);
begin
  inherited init;
  func:=stringdup(n);
  name:=stringdup(s);
  ordnr:=o;
  lab:=nil;
  is_var:=false;
end;


constructor timported_item.init_var(const n,s : string);
begin
  inherited init;
  func:=stringdup(n);
  name:=stringdup(s);
  ordnr:=0;
  lab:=nil;
  is_var:=true;
end;


destructor timported_item.done;
begin
  stringdispose(name);
  stringdispose(func);
  inherited done;
end;


{****************************************************************************
                              TImportlist
****************************************************************************}

constructor timportlist.init(const n : string);
begin
  inherited init;
  dllname:=stringdup(n);
  imported_items:=new(plinkedlist,init);
end;


destructor timportlist.done;
begin
  dispose(imported_items,done);
  stringdispose(dllname);
end;


{****************************************************************************
                              TImportLib
****************************************************************************}

constructor timportlib.Init;
begin
end;


destructor timportlib.Done;
begin
end;


procedure timportlib.preparelib(const s:string);
begin
  Message(exec_e_dll_not_supported);
end;


procedure timportlib.importprocedure(const func,module:string;index:longint;const name:string);
begin
  Message(exec_e_dll_not_supported);
end;


procedure timportlib.importvariable(const varname,module:string;const name:string);
begin
  Message(exec_e_dll_not_supported);
end;


procedure timportlib.generatelib;
begin
  Message(exec_e_dll_not_supported);
end;


procedure timportlib.generatesmartlib;
begin
  Message(exec_e_dll_not_supported);
end;


procedure DoneImport;
begin
  if assigned(importlib) then
    dispose(importlib,done);
end;


procedure InitImport;
begin
  case target_info.target of
{$ifdef i386}
    target_i386_Linux :
      importlib:=new(pimportliblinux,Init);
    target_i386_Win32 :
      importlib:=new(pimportlibwin32,Init);
    target_i386_OS2 :
      importlib:=new(pimportlibos2,Init);
{$endif i386}
{$ifdef m68k}
    target_m68k_Linux :
      importlib:=new(pimportliblinux,Init);
{$endif m68k}
{$ifdef alpha}
    target_alpha_Linux :
      importlib:=new(pimportliblinux,Init);
{$endif alpha}
{$ifdef powerpc}
    target_alpha_Linux :
      importlib:=new(pimportliblinux,Init);
{$endif powerpc}
    else
      importlib:=new(pimportlib,Init);
  end;
end;


end.
{
  $Log$
  Revision 1.16  2000-01-07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.15  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.14  1999/11/02 15:06:57  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.13  1999/10/21 14:29:34  peter
    * redesigned linker object
    + library support for linux (only procedures can be exported)

  Revision 1.12  1999/08/04 13:02:44  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.11  1999/08/03 13:50:16  michael
  + Changes for alpha

  Revision 1.10  1999/05/17 14:33:01  pierre
   * func was not disposed in timported_item

  Revision 1.9  1998/11/28 16:20:50  peter
    + support for dll variables

  Revision 1.8  1998/10/19 18:07:12  peter
    + external dll_name name func support for linux

  Revision 1.7  1998/10/19 15:41:02  peter
    * better splitname to support glib-1.1.dll alike names

  Revision 1.6  1998/10/13 13:10:17  peter
    * new style for m68k/i386 infos and enums

  Revision 1.5  1998/10/06 17:16:51  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.4  1998/09/30 12:16:47  peter
    * remove extension if one is specified

  Revision 1.3  1998/06/04 23:51:43  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.2  1998/04/27 23:10:28  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.1.1.1  1998/03/25 11:18:12  root
  * Restored version

  Revision 1.3  1998/03/10 01:17:19  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.2  1998/03/06 00:52:21  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

}
