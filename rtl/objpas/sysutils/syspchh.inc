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
function StrLen(p:pchar):sizeint;external name 'FPC_PCHAR_LENGTH';overload;
function StrCopy(dest,source : pchar) : pchar;overload;
function StrLCopy(dest,source : pchar;maxlen : SizeInt) : pchar;overload;
function StrECopy(dest,source : pchar) : pchar;
function StrEnd(p : pchar) : pchar;
function StrCat(dest,source : pchar) : pchar;
function StrComp(str1,str2 : pchar) : SizeInt;
function StrLComp(str1,str2 : pchar;l : SizeInt) : SizeInt;
function StrIComp(str1,str2 : pchar) : SizeInt;
function StrMove(dest,source : pchar;l : SizeInt) : pchar; overload;
function StrLCat(dest,source : pchar;l : SizeInt) : pchar;
function StrScan(p : pchar;c : char) : pchar; overload;
function StrRScan(p : pchar;c : char) : pchar;
function StrLower(p : pchar) : pchar;
function StrUpper(p : pchar) : pchar;
function StrLIComp(str1,str2 : pchar;l : SizeInt) : SizeInt;
function StrPos(str1,str2 : pchar) : pchar;
function StrNew(p : pchar) : pchar; overload;

{ Different from strings unit - ansistrings or different behaviour }
function StrPas(Str: PChar): string;overload;
function StrPCopy(Dest: PChar; Const Source: string): PChar;overload;
function StrPLCopy(Dest: PChar; Const Source: string; MaxLen: SizeUInt): PChar;overload;
function StrAlloc(Size: cardinal): PChar;
function StrBufSize(Str: PChar): Cardinal;
procedure StrDispose(Str: PChar);

