{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

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

 ****************************************************************************}
unit export;

interface

uses
  cobjects;

type
   pexported_procedure = ^texported_procedure;
   texported_procedure = object(tlinkedlist_item)
      ordnr : word;
      name,func : pstring;
      lab : pointer; { should be plabel, but this gaves problems with circular units }
      constructor init(const n,s : string;o : word);
      destructor done;virtual;
   end;

   pexportlib=^texportlib;
   texportlib=object
      constructor Init;
      destructor Done;
      procedure preparelib(const s:string);virtual;
      procedure exportprocedure(const func:string;index:longint;const name:string);virtual;
      procedure generatelib;virtual;
   end;

var
   exportlib : pexportlib;

procedure InitExport;
procedure DoneExport;

implementation

uses
  systems,verbose,globals
{$ifdef i386}
  ,os2_targ
  ,win_targ
{$endif}
  ,lin_targ
  ;

{****************************************************************************
                           TImported_procedure
****************************************************************************}

constructor texported_procedure.init(const n,s : string;o : word);
begin
  inherited init;
  func:=stringdup(n);
  name:=stringdup(s);
  ordnr:=o;
  lab:=nil;
end;


destructor texported_procedure.done;
begin
  stringdispose(name);
  inherited done;
end;


{****************************************************************************
                              TImportLib
****************************************************************************}

constructor texportlib.Init;
begin
end;


destructor texportlib.Done;
begin
end;


procedure texportlib.preparelib(const s:string);
begin
  Message(exec_e_dll_not_supported);
end;


procedure texportlib.exportprocedure(const func : string;index:longint;const name:string);
begin
  Message(exec_e_dll_not_supported);
end;


procedure texportlib.generatelib;
begin
  Message(exec_e_dll_not_supported);
end;


procedure DoneExport;
begin
  if assigned(exportlib) then
    dispose(exportlib,done);
end;


procedure InitExport;
begin
  case target_info.target of
{$ifdef i386}
{    target_i386_Linux :
      importlib:=new(pimportliblinux,Init);
}
    target_i386_Win32 :
      exportlib:=new(pexportlibwin32,Init);
{
    target_i386_OS2 :
      exportlib:=new(pexportlibos2,Init);
}
{$endif i386}
{$ifdef m68k}
{
    target_m68k_Linux :
      importlib:=new(pimportliblinux,Init);
}
{$endif m68k}
    else
      exportlib:=new(pexportlib,Init);
  end;
end;


end.
{
  $Log$
  Revision 1.1  1998-10-27 10:22:34  florian
    + First things for win32 export sections

}