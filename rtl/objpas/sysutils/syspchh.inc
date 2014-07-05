{
    *********************************************************************
    Copyright (C) 1997, 1998 Gertjan Schouten

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

    System Utilities For Free Pascal
}

{ shared with strings unit }
function strlen(p:pchar):sizeint;external name 'FPC_PCHAR_LENGTH';overload;
function strcopy(dest,source : pchar) : pchar;overload;
function strlcopy(dest,source : pchar;maxlen : SizeInt) : pchar;overload;
function strecopy(dest,source : pchar) : pchar;
function strend(p : pchar) : pchar;
function strcat(dest,source : pchar) : pchar;
function strcomp(str1,str2 : pchar) : SizeInt;
function strlcomp(str1,str2 : pchar;l : SizeInt) : SizeInt;
function stricomp(str1,str2 : pchar) : SizeInt;
function strmove(dest,source : pchar;l : SizeInt) : pchar; overload;
function strlcat(dest,source : pchar;l : SizeInt) : pchar;
function strscan(p : pchar;c : char) : pchar; overload;
function strrscan(p : pchar;c : char) : pchar;
function strlower(p : pchar) : pchar;
function strupper(p : pchar) : pchar;
function strlicomp(str1,str2 : pchar;l : SizeInt) : SizeInt;
function strpos(str1,str2 : pchar) : pchar;
function strnew(p : pchar) : pchar; overload;

{ Different from strings unit - ansistrings or different behaviour }
function StrPas(Str: PChar): string;overload;
function StrPCopy(Dest: PChar; Const Source: string): PChar;overload;
function StrPLCopy(Dest: PChar; Const Source: string; MaxLen: SizeUInt): PChar;overload;
function StrAlloc(Size: cardinal): PChar;
function StrBufSize(Str: PChar): Cardinal;
procedure StrDispose(Str: PChar);

