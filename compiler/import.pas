{
    $Id$
    Copyright (c) 1998 by Peter Vreman

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
   pimported_procedure = ^timported_procedure;
   timported_procedure = object(tlinkedlist_item)
      ordnr : word;
      name,func : pstring;
      lab : pointer; { should be plabel, but this gaves problems with circular units }
      constructor init(const n,s : string;o : word);
      destructor done;virtual;
   end;

   pimportlist = ^timportlist;
   timportlist = object(tlinkedlist_item)
      dllname : pstring;
      imported_procedures : plinkedlist;
      constructor init(const n : string);
      destructor done;virtual;
   end;

   pimportlib=^timportlib;
   timportlib=object
      constructor Init;
      destructor Done;
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure generatelib;virtual;
   end;

var
  importlib : pimportlib;

procedure InitImport;

implementation

uses
  systems,verbose,globals
{$ifdef i386}
  ,os2_targ
  ,win_targ
{$endif}
  ;

{****************************************************************************
                           TImported_procedure
****************************************************************************}

constructor timported_procedure.init(const n,s : string;o : word);
begin
  inherited init;
  func:=stringdup(n);
  name:=stringdup(s);
  ordnr:=o;
  lab:=nil;
end;

destructor timported_procedure.done;
begin
  stringdispose(name);
  inherited done;
end;


{****************************************************************************
                              TImportlist
****************************************************************************}

constructor timportlist.init(const n : string);
begin
  inherited init;
  dllname:=stringdup(SplitName(n));
  imported_procedures:=new(plinkedlist,init);
end;

destructor timportlist.done;
begin
  dispose(imported_procedures,done);
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


procedure timportlib.generatelib;
begin
  Message(exec_e_dll_not_supported);
end;


procedure InitImport;
begin
{$ifdef i386}

  case target_info.target of
 target_Win32 : importlib:=new(pimportlibwin32,Init);
   target_OS2 : importlib:=new(pimportlibos2,Init);
  else
   importlib:=new(pimportlib,Init);
  end;
{$endif i386}
{$ifdef m68k}
  importlib:=new(pimportlib,Init);
{$endif m68k}
end;

end.
{
  $Log$
  Revision 1.4  1998-09-30 12:16:47  peter
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