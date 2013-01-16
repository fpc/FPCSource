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

function Trim(const S: widestring): widestring;
function TrimLeft(const S: widestring): widestring;
function TrimRight(const S: widestring): widestring;

function WideUpperCase(const s : WideString) : WideString;{$ifdef SYSUTILSINLINE}inline;{$endif}
function WideLowerCase(const s : WideString) : WideString;{$ifdef SYSUTILSINLINE}inline;{$endif}
function WideCompareStr(const s1, s2 : WideString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
function WideSameStr(const s1, s2 : WideString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
function WideCompareText(const s1, s2 : WideString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
function WideSameText(const s1, s2 : WideString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}

Function WideFormat (Const Fmt : WideString; const Args : Array of const) : WideString;
Function WideFormat (Const Fmt : WideString; const Args : Array of const; Const FormatSettings: TFormatSettings) : WideString;
Function WideFormatBuf (Var Buffer; BufLen : Cardinal; Const Fmt; fmtLen : Cardinal; Const Args : Array of const) : Cardinal;
Function WideFormatBuf (Var Buffer; BufLen : Cardinal; Const Fmt; fmtLen : Cardinal; Const Args : Array of const; Const FormatSettings: TFormatSettings) : Cardinal;
Procedure WideFmtStr(Var Res: WideString; Const Fmt : WideString; Const args: Array of const);
Procedure WideFmtStr(Var Res: WideString; Const Fmt : WideString; Const args: Array of const; Const FormatSettings: TFormatSettings);

function StrLen(p: pwidechar): sizeint; external name 'FPC_PWIDECHAR_LENGTH'; overload;

function StrCopy(Dest, Source: PWideChar): PWideChar; overload;
function StrLCopy(Dest,Source: PWideChar; MaxLen: SizeInt): PWideChar; overload;
Function CharInSet(Ch:WideChar;Const CSet : TSysCharSet) : Boolean;
