{%MainUnit sysutils.pp}
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
function StrLen(p:PAnsiChar):sizeint;external name 'FPC_PCHAR_LENGTH';overload;
function StrCopy(dest,source : PAnsiChar) : PAnsiChar;overload;
function StrLCopy(dest,source : PAnsiChar;maxlen : SizeInt) : PAnsiChar;overload;
function StrECopy(dest,source : PAnsiChar) : PAnsiChar;
function StrEnd(p : PAnsiChar) : PAnsiChar;
function StrCat(dest,source : PAnsiChar) : PAnsiChar;
function StrComp(str1,str2 : PAnsiChar) : SizeInt;
function StrLComp(str1,str2 : PAnsiChar;l : SizeInt) : SizeInt;
function StrIComp(str1,str2 : PAnsiChar) : SizeInt;
function StrMove(dest,source : PAnsiChar;l : SizeInt) : PAnsiChar; overload;
function StrLCat(dest,source : PAnsiChar;l : SizeInt) : PAnsiChar;
function StrScan(p : PAnsiChar;c : AnsiChar) : PAnsiChar; overload;
function StrRScan(p : PAnsiChar;c : AnsiChar) : PAnsiChar;
function StrLower(p : PAnsiChar) : PAnsiChar;
function StrUpper(p : PAnsiChar) : PAnsiChar;
function StrLIComp(str1,str2 : PAnsiChar;l : SizeInt) : SizeInt;
function StrPos(str1,str2 : PAnsiChar) : PAnsiChar;
function StrNew(p : PAnsiChar) : PAnsiChar; overload;

{ Different from strings unit - ansistrings or different behaviour }
function StrPas(Str: PAnsiChar): string;overload;
function StrPCopy(Dest: PAnsiChar; Const Source: RawByteString): PAnsiChar;overload;
function StrPLCopy(Dest: PAnsiChar; Const Source: RawByteString; MaxLen: SizeUInt): PAnsiChar;overload;
function StrAlloc(Size: cardinal): PAnsiChar;
procedure StrDispose(Str: PAnsiChar);
function StrBufSize(Str: PAnsiChar): Cardinal;

