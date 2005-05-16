{
    $Id: import.pas,v 1.26 2005/02/14 17:13:06 peter Exp $
    Copyright (c) 1998-2002 by Peter Vreman

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

{$i fpcdefs.inc}

interface

uses
  cutils,cclasses,
  systems,
  aasmbase,
  symdef,symsym;

type
   timported_item = class(TLinkedListItem)
      ordnr  : word;
      name,
      func   : pstring;
      lab    : tasmlabel;
      is_var : boolean;
      constructor Create(const n,s : string;o : word);
      constructor Create_var(const n,s : string);
      destructor Destroy;override;
   end;

   timportlist = class(TLinkedListItem)
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
      constructor Create;virtual;
      destructor Destroy;override;
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);virtual;
      procedure importvariable(vs:tglobalvarsym;const name,module:string);virtual;
      procedure generatelib;virtual;
      procedure generatesmartlib;virtual;
   end;

   TDLLScanner=class
   public
     f:file;
     impname:string;
     TheWord:array[0..1]of char;
     HeaderOffset:cardinal;
     loaded:integer;
     function isSuitableFileType(x:cardinal):longbool;virtual;abstract;
     function GetEdata(HeaderEntry:cardinal):longbool;virtual;abstract;
     function Scan(const binname:string):longbool;virtual;abstract;
   end;

   TImportLibClass=class of TImportLib;
   TDLLScannerClass=class of TDLLScanner;

var
  CImportLib  : array[tsystem] of TImportLibClass;
  CDLLScanner : array[tsystem] of TDLLScannerClass;
  ImportLib   : TImportLib;

procedure RegisterImport(t:tsystem;c:TImportLibClass);
procedure RegisterDLLScanner(t:tsystem;c:TDLLScannerClass);
procedure InitImport;
procedure DoneImport;


implementation

uses
  verbose,globals;

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


procedure timportlib.importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);
begin
  NotSupported;
end;


procedure timportlib.importvariable(vs:tglobalvarsym;const name,module:string);
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


{*****************************************************************************
                                 Init/Done
*****************************************************************************}

procedure RegisterImport(t:tsystem;c:TImportLibClass);
begin
  CImportLib[t]:=c;
end;


procedure RegisterDLLScanner(t:tsystem;c:TDLLScannerClass);
begin
  CDLLScanner[t]:=c;
end;


procedure InitImport;
begin
  if assigned(CImportLib[target_info.system]) then
   importlib:=CImportLib[target_info.system].Create
  else
   importlib:=TImportLib.Create;
end;


procedure DoneImport;
begin
  if assigned(importlib) then
    importlib.free;
end;

end.
{
  $Log: import.pas,v $
  Revision 1.26  2005/02/14 17:13:06  peter
    * truncate log

}
