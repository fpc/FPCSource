{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

uses
  cutils,cobjects,
  symtable;

const
   { export options }
   eo_resident = $1;
   eo_index    = $2;
   eo_name     = $4;

type
   pexported_item = ^texported_item;
   texported_item = object(tlinkedlist_item)
      sym : psym;
      index : longint;
      name : pstring;
      options : word;
      is_var : boolean;
      constructor init;
      destructor done;virtual;
   end;

   pexportlib=^texportlib;
   texportlib=object
   private
      notsupmsg : boolean;
      procedure NotSupported;
   public
      constructor Init;
      destructor Done;
      procedure preparelib(const s : string);virtual;
      procedure exportprocedure(hp : pexported_item);virtual;
      procedure exportvar(hp : pexported_item);virtual;
      procedure generatelib;virtual;
   end;

var
   exportlib : pexportlib;

procedure InitExport;
procedure DoneExport;

implementation

uses
  systems,verbose,globals,fmodule
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
                           TImported_procedure
****************************************************************************}

constructor texported_item.init;
begin
  inherited init;
  sym:=nil;
  index:=-1;
  name:=nil;
  options:=0;
  is_var:=false;
end;


destructor texported_item.done;
begin
  stringdispose(name);
  inherited done;
end;


{****************************************************************************
                              TImportLib
****************************************************************************}

constructor texportlib.Init;
begin
  notsupmsg:=false;
end;


destructor texportlib.Done;
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


procedure texportlib.exportprocedure(hp : pexported_item);
begin
  NotSupported;
end;


procedure texportlib.exportvar(hp : pexported_item);
begin
  NotSupported;
end;


procedure texportlib.generatelib;
begin
  NotSupported;
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
    target_i386_Linux :
      exportlib:=new(pexportliblinux,Init);
    target_i386_freebsd:
      exportlib:=new(pexportlibfreebsd,Init);
    target_i386_Win32 :
      exportlib:=new(pexportlibwin32,Init);
    target_i386_Netware :
      exportlib:=new(pexportlibnetware,Init);
{
    target_i386_OS2 :
      exportlib:=new(pexportlibos2,Init);
}
{$endif i386}
{$ifdef m68k}
    target_m68k_Linux :
      exportlib:=new(pexportlib,Init);
{$endif m68k}
{$ifdef alpha}
    target_alpha_Linux :
      exportlib:=new(pexportlib,Init);
{$endif alpha}
{$ifdef powerpc}
    target_alpha_Linux :
      exportlib:=new(pexportlib,Init);
{$endif powerpc}
    else
      exportlib:=new(pexportlib,Init);
  end;
end;


end.
{
  $Log$
  Revision 1.6  2000-09-24 15:06:16  peter
    * use defines.inc

  Revision 1.5  2000/09/16 12:22:52  peter
    * freebsd support merged

  Revision 1.4  2000/09/11 17:00:22  florian
    + first implementation of Netware Module support, thanks to
      Armin Diehl (diehl@nordrhein.de) for providing the patches

  Revision 1.3  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:41  michael
  + removed logs

}
