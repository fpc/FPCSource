{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements an uniform export object

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
unit export;

{$i fpcdefs.inc}

interface

uses
  cutils,cclasses,
  systems,
  symtype,
  aasmbase;

const
   { export options }
   eo_resident = $1;
   eo_index    = $2;
   eo_name     = $4;

type
   texported_item = class(TLinkedListItem)
      sym : tsym;
      index : longint;
      name : pshortstring;
      options : word;
      is_var : boolean;
      constructor create;
      destructor destroy;override;
   end;

   texportlib=class
   private
      notsupmsg : boolean;
      procedure NotSupported;
   public
      constructor Create;virtual;
      destructor Destroy;override;
      procedure preparelib(const s : string);virtual;
      procedure exportprocedure(hp : texported_item);virtual;
      procedure exportvar(hp : texported_item);virtual;
      procedure generatelib;virtual;
   end;

   TExportLibClass=class of TExportLib;

var
  CExportLib : array[tsystem] of TExportLibClass;
  ExportLib  : TExportLib;

procedure RegisterExport(t:tsystem;c:TExportLibClass);
procedure InitExport;
procedure DoneExport;

implementation

uses
  verbose,globals;

{****************************************************************************
                           TExported_procedure
****************************************************************************}

constructor texported_item.Create;
begin
  inherited Create;
  sym:=nil;
  index:=-1;
  name:=nil;
  options:=0;
  is_var:=false;
end;


destructor texported_item.destroy;
begin
  stringdispose(name);
  inherited destroy;
end;


{****************************************************************************
                              TExportLib
****************************************************************************}

constructor texportlib.Create;
begin
  notsupmsg:=false;
end;


destructor texportlib.Destroy;
begin
end;


procedure texportlib.NotSupported;
begin
  { show the message only once }
  if not notsupmsg then
   begin
     Message(exec_e_dll_not_supported);
     notsupmsg:=true;
   end;
end;


procedure texportlib.preparelib(const s:string);
begin
  NotSupported;
end;


procedure texportlib.exportprocedure(hp : texported_item);
begin
  NotSupported;
end;


procedure texportlib.exportvar(hp : texported_item);
begin
  NotSupported;
end;


procedure texportlib.generatelib;
begin
  NotSupported;
end;


{*****************************************************************************
                                 Init/Done
*****************************************************************************}

procedure RegisterExport(t:tsystem;c:TExportLibClass);
begin
  CExportLib[t]:=c;
end;


procedure InitExport;
begin
  if assigned(CExportLib[target_info.system]) then
   exportlib:=CExportLib[target_info.system].Create
  else
   exportlib:=TExportLib.Create;
end;


procedure DoneExport;
begin
  if assigned(Exportlib) then
    Exportlib.free;
end;


end.
