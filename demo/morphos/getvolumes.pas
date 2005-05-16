{
    $Id: getvolumes.pas,v 1.2 2005/02/14 17:13:10 peter Exp $

    Getting list of DOS volumes and assigns
    Free Pascal for MorphOS example

    Copyright (C) 2005 by Karoly Balogh

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ * 2005.01.10 * }
{ * Needs MorphOS RTL 2005.01.10 or later! * }

program getvolumes;

uses doslib;


{ * this function converts a BCPL-style string pointer to * }
{ * normal PChar type. * }
function BStr2PChar(bstr: DWord): PChar;
begin
  BStr2PChar:=Pointer((bstr shl 2)+1);
end;


procedure dosList(flags: DWord);
var
  dosList: PDosList;
begin
  { * fetch a list of volumes * }
  dosList:=LockDosList(flags or LDF_READ);
  { * parse the volumes * }
  repeat
    dosList:=NextDosEntry(dosList,flags);
    if dosList<>NIL then
      writeln(BStr2PChar(dosList^.dol_Name));
  until dosList=NIL;
  UnLockDosList(flags or LDF_READ);
end;


begin
  { * dos.library is opened by the RTL startup code, * }
  { * so we don't need to open it by ourselves. * }

  writeln('Current Volumes: ==========');
  dosList(LDF_VOLUMES);
  writeln('Current Assigns: ==========');
  dosList(LDF_ASSIGNS);
end.


{
  $Log: getvolumes.pas,v $
  Revision 1.2  2005/02/14 17:13:10  peter
    * truncate log

  Revision 1.1  2005/01/10 06:00:47  karoly
    * initial revision

}
