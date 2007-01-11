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
