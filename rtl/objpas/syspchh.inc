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

function StrLen(Str: PChar): cardinal;
function StrEnd(Str: PChar): PChar;
function StrMove(Dest, Source: PChar; Count: cardinal): PChar;
function StrCopy(Dest, Source: PChar): PChar;
function StrECopy(Dest, Source: PChar): PChar;
function StrLCopy(Dest, Source: PChar; MaxLen: cardinal): PChar;
function StrPCopy(Dest: PChar; Source: string): PChar;
function StrPLCopy(Dest: PChar; Source: string; MaxLen: cardinal): PChar;
function StrCat(Dest, Source: PChar): PChar;
function StrLCat(Dest, Source: PChar; MaxLen: cardinal): PChar;
function StrComp(Str1, Str2: PChar): integer;
function StrIComp(Str1, Str2: PChar): integer;
function StrLComp(Str1, Str2: PChar; MaxLen: cardinal): integer;
function StrLIComp(Str1, Str2: PChar; MaxLen: cardinal): integer;
function StrScan(Str: PChar; Chr: char): PChar;
function StrRScan(Str: PChar; Chr: char): PChar;
function StrPos(Str1, Str2: PChar): PChar;
function StrUpper(Str: PChar): PChar;
function StrLower(Str: PChar): PChar;
function StrPas(Str: PChar): string;
function StrAlloc(Size: cardinal): PChar;
function StrBufSize(var Str: PChar): cardinal;
function StrNew(Str: PChar): PChar;
procedure StrDispose(var Str: PChar);

{
  $Log$
  Revision 1.2  1998-09-16 08:28:41  michael
  Update from gertjan Schouten, plus small fix for linux

  Revision 1.1  1998/04/10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.

}

