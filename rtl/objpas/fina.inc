{
    *********************************************************************
    $Id$
    Copyright (C) 1997, 1998 Gertjan Schouten

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
    *********************************************************************

    System Utilities For Free Pascal
}


type
   PByte=^Byte;
   PWord=^Word;
   PLongint=^Longint;

const
   DayTable:array[Boolean,1..12] of longint =
      ((0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
       (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335));
   HexDigits: array[0..15] of char = '0123456789ABCDEF';

function ChangeFileExt(FileName, Extension: string): string;
var i: longint;
begin
I := Length(FileName);
while (I > 0) and not (FileName[I] in ['.', '\', ':']) do Dec(I);
if (I = 0) or (FileName[I] <> '.') then I := 255;
ChangeFileExt := Copy(FileName, 1, I - 1) + Extension;
end;

function ExtractFilePath(FileName: string): string;
var i: longint;
begin
i := Length(FileName);
while (I > 0) and not (FileName[I] in ['\', ':']) do Dec(I);
ExtractFilePath := Copy(FileName, 1, I);
end;

function ExtractFileDir(FileName: string): string;
var i: longint;
begin
I := Length(FileName);
while (I > 0) and not (FileName[I] in ['\', ':']) do Dec(I);
if (I > 1) and (FileName[I] = '\') and
    not (FileName[I - 1] in ['\', ':']) then Dec(I);
ExtractFileDir := Copy(FileName, 1, I);
end;

function ExtractFileDrive(FileName: string): string;
var i, j: longint;
begin
if (Length(FileName) >= 3) and (FileName[2] = ':') then
   ExtractFileDrive := Copy(FileName, 1, 2)
else if (Length(FileName) >= 2) and (FileName[1] = '\') and
   (FileName[2] = '\') then begin
   J := 0;
   I := 3;
   While (I < Length(FileName)) and (J < 2) do begin
      if FileName[I] = '\' then Inc(J);
      if J < 2 then Inc(I);
      end;
   if FileName[I] = '\' then Dec(I);
   ExtractFileDrive := Copy(FileName, 1, I);
   end else ExtractFileDrive := '';
end;

function ExtractFileName(FileName: string): string;
var i: longint;
begin
I := Length(FileName);
while (I > 0) and not (FileName[I] in ['\', ':']) do Dec(I);
ExtractFileName := Copy(FileName, I + 1, 255);
end;

function ExtractFileExt(FileName: string): string;
var i: longint;
begin
I := Length(FileName);
while (I > 0) and not (FileName[I] in ['.', '\', ':']) do Dec(I);
if (I > 0) and (FileName[I] = '.') then
   ExtractFileExt := Copy(FileName, I, 255)
else ExtractFileExt := '';
end;


{
  $Log$
  Revision 1.1  1998-04-10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.

}