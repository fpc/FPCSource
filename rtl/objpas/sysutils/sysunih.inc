{
    *********************************************************************
    Copyright (C) 2002 by Florian Klaempfl

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
}

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
