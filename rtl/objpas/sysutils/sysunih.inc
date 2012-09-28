{
    *********************************************************************
    Copyright (C) 2002 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    *********************************************************************
}

type
  TUnicodeCharArray = array of UnicodeChar;

function Trim(const S: unicodestring): unicodestring;
function TrimLeft(const S: unicodestring): unicodestring;
function TrimRight(const S: unicodestring): unicodestring;

function UnicodeUpperCase(const s : UnicodeString) : UnicodeString;{$ifdef SYSUTILSINLINE}inline;{$endif}
function UnicodeLowerCase(const s : UnicodeString) : UnicodeString;{$ifdef SYSUTILSINLINE}inline;{$endif}
function UnicodeCompareStr(const s1, s2 : UnicodeString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
function UnicodeSameStr(const s1, s2 : UnicodeString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
function UnicodeCompareText(const s1, s2 : UnicodeString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
function UnicodeSameText(const s1, s2 : UnicodeString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}

Function UnicodeFormat (Const Fmt : UnicodeString; const Args : Array of const) : UnicodeString;
Function UnicodeFormat (Const Fmt : UnicodeString; const Args : Array of const; Const FormatSettings: TFormatSettings) : UnicodeString;
Function UnicodeFormatBuf (Var Buffer; BufLen : Cardinal; Const Fmt; fmtLen : Cardinal; Const Args : Array of const) : Cardinal;
Function UnicodeFormatBuf (Var Buffer; BufLen : Cardinal; Const Fmt; fmtLen : Cardinal; Const Args : Array of const; Const FormatSettings: TFormatSettings) : Cardinal;
Procedure UnicodeFmtStr(Var Res: UnicodeString; Const Fmt : UnicodeString; Const args: Array of const);
Procedure UnicodeFmtStr(Var Res: UnicodeString; Const Fmt : UnicodeString; Const args: Array of const; Const FormatSettings: TFormatSettings);

function StrPLCopy(Dest: PWideChar; const Source: UnicodeString; MaxLen: SizeInt): PWideChar; overload;
function StrPCopy(Dest: PWideChar; const Source: UnicodeString): PWideChar; overload;
{$IFNDEF VER2_4}
function BytesOf(const Val: UnicodeString): TBytes; overload;
function BytesOf(const Val: WideChar): TBytes; overload;
function StringOf(const Bytes: TBytes): UnicodeString;
{$ENDIF VER2_4}
function WideBytesOf(const Value: UnicodeString): TBytes;
function WideStringOf(const Value: TBytes): UnicodeString;
function ByteLength(const S: UnicodeString): Integer;
