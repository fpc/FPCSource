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


function  NewStr(s:string):pchar;
function  StrAlloc(Size:longint):pchar;
procedure StrDispose(var p:pchar);
function  StrPas(p:pchar):string;
function  StrLen(p:pchar):longint;
function  StrEnd(p:pchar):pchar;
function  StrCat(Dest, Source:pchar): PChar;
function  StrCat(Dest:pchar; Source: string): PChar;
function  StrCat(var Dest:string; Source: pchar): String;
function  StrIns(Dest:pchar; Source: string): PChar;
function  StrMove(Dest, Source: PChar; Count: longint): PChar;
function  StrCopy(Dest, Source: PChar): Pchar;
function  StrLCopy(Dest, Source: PChar; MaxLen: longint): PChar;
function  StrScan(str:pchar;ch:char):PChar;
function  StrRScan(str:pchar;ch:char):PChar;
function  StrTer(str:pchar;l:longint):pchar;


{
  $Log$
  Revision 1.1  1998-04-10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.

}
