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

type
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
  systems,verbose,
  os2_targ,win_targ;

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
  case target_info.target of
 target_Win32 : importlib:=new(pimportlibwin32,Init);
   target_OS2 : importlib:=new(pimportlibos2,Init);
  else
   importlib:=new(pimportlib,Init);
  end;
end;

end.
{
  $Log$
  Revision 1.1  1998-03-25 11:18:12  root
  Initial revision

  Revision 1.3  1998/03/10 01:17:19  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.2  1998/03/06 00:52:21  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

}