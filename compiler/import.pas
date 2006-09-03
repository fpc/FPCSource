{
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
   timportlib=class
   private
      notsupmsg : boolean;
      procedure NotSupported;
   public
      constructor Create;virtual;
      destructor Destroy;override;
      procedure generatelib;virtual;
   end;

   TDLLScanner=class
     function Scan(const binname:string):boolean;virtual;abstract;
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


procedure timportlib.generatelib;
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
