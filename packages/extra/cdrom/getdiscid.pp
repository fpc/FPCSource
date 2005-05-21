{
    Copyright (c) 1999-2000 by Michael Van Canneyt

    Demonstrates DiscID unit usage.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program getdiscid;

uses cdrom,discid;

Var
  TheDiscID : cardinal;
  Tracks, i : Integer;
  Entries : Array[1..100] of TTocEntry;
  Device : string;

begin
  Case ParamCount of
    0 : Device:='/dev/cdrom';
    1 : Device:=Paramstr(1);
  else
    Writeln('Usage: getdiscid [devicefile]');
    halt(1);
  end;
  Tracks := ReadCDTOC(Device,Entries);
  If Tracks<0 then
    Writeln('Error reading TOC of device ',device)
  else
    begin
    Writeln('Disk has ',tracks,' tracks.');
    TheDiscID := CDDBDiscID(Entries,Tracks);
    Writeln('Disc ID : ',lowercase(HexStr(theDiscID,8)));
    Writeln('CDDB Query : ',GetCDDBQueryString(Entries,Tracks));
    end;
end.
