{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Disk functions from Delphi's sysutils.pas

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

   function GetDiskFreeSpace(drive:pchar;var sector_cluster,bytes_sector,
                             freeclusters,totalclusters:longint):longbool;
     external 'kernel32' name 'GetDiskFreeSpaceA';

function diskfree(drive : byte) : longint;
var
  disk : array[1..4] of char;
  secs,bytes,
  free,total : longint;
begin
  if drive=0 then
   begin
     disk[1]:='\';
     disk[2]:=#0;
   end
  else
   begin
     disk[1]:=chr(drive+64);
     disk[2]:=':';
     disk[3]:='\';
     disk[4]:=#0;
   end;
  if GetDiskFreeSpace(@disk,secs,bytes,free,total) then
   result:=free*secs*bytes
  else
   result:=-1;
end;


function disksize(drive : byte) : longint;
var
  disk : array[1..4] of char;
  secs,bytes,
  free,total : longint;
begin
  if drive=0 then
   begin
     disk[1]:='\';
     disk[2]:=#0;
   end
  else
   begin
     disk[1]:=chr(drive+64);
     disk[2]:=':';
     disk[3]:='\';
     disk[4]:=#0;
   end;
  if GetDiskFreeSpace(@disk,secs,bytes,free,total) then
   result:=total*secs*bytes
  else
   result:=-1;
end;


Function GetCurrentDir : String;
begin
  GetDir(0, result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
  {$I-}
  ChDir(NewDir);
  result := (IOResult = 0);
  {$I+}
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
  {$I-}
  MkDir(NewDir);
  result := (IOResult = 0);
  {$I+}
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
  {$I-}
  RmDir(Dir);
  result := (IOResult = 0);
  {$I+}
end;

{
  $Log$
  Revision 1.3  2000-01-07 16:41:52  daniel
    * copyright 2000

  Revision 1.2  1999/04/08 12:23:06  peter
    * removed os.inc

  Revision 1.1  1998/10/11 13:42:55  michael
  Added disk functions
}
