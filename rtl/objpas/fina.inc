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

function ChangeFileExt(const FileName, Extension: string): string;
var i: longint;
begin
I := Length(FileName);
while (I > 0) and not(FileName[I] in ['/', '.', '\', ':']) do Dec(I);
if (I = 0) or (FileName[I] <> '.') then I := 255;
Result := Copy(FileName, 1, I - 1) + Extension;
end;

function ExtractFilePath(const FileName: string): string;
var i: longint;
begin
i := Length(FileName);
while (i > 0) and not (FileName[i] in ['/', '\', ':']) do Dec(i);
Result := Copy(FileName, 1, i);
end;

function ExtractFileDir(const FileName: string): string;
var i: longint;
begin
I := Length(FileName);
while (I > 0) and not (FileName[I] in ['/', '\', ':']) do Dec(I);
if (I > 1) and (FileName[I] in ['\', '/']) and
   not (FileName[I - 1] in ['/', '\', ':']) then Dec(I);
Result := Copy(FileName, 1, I);
end;

function ExtractFileDrive(const FileName: string): string;
var i: longint;
begin
if (Length(FileName) >= 3) and (FileName[2] = ':') then
   result := Copy(FileName, 1, 2)
else if (Length(FileName) >= 2) and (FileName[1] in ['/', '\']) and
   (FileName[2] in ['/', '\']) then begin
   i := 2;
   While (i < Length(Filename)) do begin
	   if Filename[i + 1] in ['/', '\'] then break;
      inc(i);
      end ;
   Result := Copy(FileName, 1, i);
   end
else Result := '';
end;

function ExtractFileName(const FileName: string): string;
var i: longint;
begin
I := Length(FileName);
while (I > 0) and not (FileName[I] in ['/', '\', ':']) do Dec(I);
Result := Copy(FileName, I + 1, 255);
end;

function ExtractFileExt(const FileName: string): string;
var i: longint;
begin
I := Length(FileName);
while (I > 0) and not (FileName[I] in ['.', '/', '\', ':']) do Dec(I);
if (I > 0) and (FileName[I] = '.') then
   Result := Copy(FileName, I, 255)
else Result := '';
end;

{
  $Log$
  Revision 1.2  1998-09-16 08:28:38  michael
  Update from gertjan Schouten, plus small fix for linux

  Revision 1.1  1998/04/10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.
}
