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

{$i defines.inc}

interface

uses
  cutils,cclasses;

type
   timported_item = class(tlinkedlistitem)
      ordnr  : word;
      name,
      func   : pstring;
      lab    : pointer; { should be plabel, but this gaves problems with circular units }
      is_var : boolean;
      constructor Create(const n,s : string;o : word);
      constructor Create_var(const n,s : string);
      destructor Destroy;override;
   end;

   timportlist = class(tlinkedlistitem)
      dllname : pstring;
      imported_items : tlinkedlist;
      constructor Create(const n : string);
      destructor Destroy;Override;
   end;

   timportlib=class
   private
      notsupmsg : boolean;
      procedure NotSupported;
   public
      constructor Create;
      destructor Destroy;override;
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure importvariable(const varname,module:string;const name:string);virtual;
      procedure generatelib;virtual;
      procedure generatesmartlib;virtual;
   end;

var
  importlib : timportlib;

procedure InitImport;
procedure DoneImport;

implementation

uses
  systems,verbose,globals
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
  {$ifndef NOTARGETFREEBSD}
   ,t_fbsd
  {$endif}
  {$ifndef NOTARGETOS2}
    ,t_os2
  {$endif}
  {$ifndef NOTARGETWIN32}
    ,t_win32
  {$endif}
  {$ifndef NOTARGETNETWARE}
    ,t_nwm
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

constructor timported_item.Create(const n,s : string;o : word);
begin
  inherited Create;
  func:=stringdup(n);
  name:=stringdup(s);
  ordnr:=o;
  lab:=nil;
  is_var:=false;
end;


constructor timported_item.create_var(const n,s : string);
begin
  inherited Create;
  func:=stringdup(n);
  name:=stringdup(s);
  ordnr:=0;
  lab:=nil;
  is_var:=true;
end;


destructor timported_item.destroy;
begin
  stringdispose(name);
  stringdispose(func);
  inherited destroy;
end;


{****************************************************************************
                              TImportlist
****************************************************************************}

constructor timportlist.Create(const n : string);
begin
  inherited Create;
  dllname:=stringdup(n);
  imported_items:=Tlinkedlist.Create;
end;


destructor timportlist.destroy;
begin
  imported_items.free;
  stringdispose(dllname);
end;


{****************************************************************************
                              TImportLib
****************************************************************************}

constructor timportlib.Create;
begin
  notsupmsg:=false;
end;


destructor timportlib.Destroy;
begin
end;


procedure timportlib.NotSupported;
begin
  { show the message only once }
  if not notsupmsg then
   begin
     Message(exec_e_dll_not_supported);
     notsupmsg:=true;
   end;
end;


procedure timportlib.preparelib(const s:string);
begin
  NotSupported;
end;


procedure timportlib.importprocedure(const func,module:string;index:longint;const name:string);
begin
  NotSupported;
end;


procedure timportlib.importvariable(const varname,module:string;const name:string);
begin
  NotSupported;
end;


procedure timportlib.generatelib;
begin
  NotSupported;
end;


procedure timportlib.generatesmartlib;
begin
  NotSupported;
end;


procedure DoneImport;
begin
  if assigned(importlib) then
    importlib.free;
end;


procedure InitImport;
begin
  case target_info.target of
{$ifdef i386}
    target_i386_Linux :
      importlib:=Timportliblinux.Create;
    target_i386_freebsd:
      importlib:=Timportlibfreebsd.Create;
         target_i386_Win32 :
      importlib:=Timportlibwin32.Create;
    target_i386_OS2 :
      importlib:=Timportlibos2.Create;
    target_i386_Netware :
      importlib:=Timportlibnetware.Create;
{$endif i386}
{$ifdef m68k}
    target_m68k_Linux :
      importlib:=Timportliblinux.Create;
{$endif m68k}
{$ifdef alpha}
    target_alpha_Linux :
      importlib:=Timportliblinux.Create;
{$endif alpha}
{$ifdef powerpc}
    target_alpha_Linux :
      importlib:=Timportliblinux.Create;
{$endif powerpc}
    else
      importlib:=Timportlib.Create;
  end;
end;


end.
{
  $Log$
  Revision 1.7  2000-12-25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.6  2000/09/24 15:06:18  peter
    * use defines.inc

  Revision 1.5  2000/09/16 12:22:52  peter
    * freebsd support merged

  Revision 1.4  2000/09/11 17:00:23  florian
    + first implementation of Netware Module support, thanks to
      Armin Diehl (diehl@nordrhein.de) for providing the patches

  Revision 1.3  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:43  michael
  + removed logs

}
