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

{ shared with strings unit }
function strlen(p:pchar):sizeint;external name 'FPC_PCHAR_LENGTH';
function strcopy(dest,source : pchar) : pchar;
function strlcopy(dest,source : pchar;maxlen : SizeInt) : pchar;
function strecopy(dest,source : pchar) : pchar;
function strend(p : pchar) : pchar;
function strcat(dest,source : pchar) : pchar;
function strcomp(str1,str2 : pchar) : SizeInt;
function strlcomp(str1,str2 : pchar;l : SizeInt) : SizeInt;
function stricomp(str1,str2 : pchar) : SizeInt;
function strmove(dest,source : pchar;l : SizeInt) : pchar;
function strlcat(dest,source : pchar;l : SizeInt) : pchar;
function strscan(p : pchar;c : char) : pchar;
function strrscan(p : pchar;c : char) : pchar;
function strlower(p : pchar) : pchar;
function strupper(p : pchar) : pchar;
function strlicomp(str1,str2 : pchar;l : SizeInt) : SizeInt;
function strpos(str1,str2 : pchar) : pchar;
function strnew(p : pchar) : pchar;

{ Different from strings unit - ansistrings or different behaviour }
function StrPas(Str: PChar): string;
function StrPCopy(Dest: PChar; Source: string): PChar;
function StrPLCopy(Dest: PChar; Source: string; MaxLen: SizeUInt): PChar;
function StrAlloc(Size: cardinal): PChar;
function StrBufSize(Str: PChar): SizeUInt;
procedure StrDispose(Str: PChar);

{
  $Log$
  Revision 1.4  2004-11-21 15:35:23  peter
    * float routines all use internproc and compilerproc helpers

  Revision 1.3  2004/05/01 23:55:18  peter
    * replace strlenint with sizeint

  Revision 1.2  2004/02/20 22:15:16  florian
    + x86_64 dependend sysutils part added
    * some 64 bit adaptions

  Revision 1.1  2003/10/06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.3  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed

}
