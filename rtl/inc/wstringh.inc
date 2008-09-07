{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl,
    member of the Free Pascal development team.

    This file implements support routines for WideStrings/Unicode with FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Procedure UniqueString (Var S : WideString);external name 'FPC_WIDESTR_UNIQUE';
Function Pos (Const Substr : WideString; Const Source : WideString) : SizeInt;
Function Pos (c : Char; Const s : WideString) : SizeInt;
Function Pos (c : WideChar; Const s : WideString) : SizeInt;
Function Pos (c : WideChar; Const s : AnsiString) : SizeInt;
Function Pos (c : AnsiString; Const s : WideString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Pos (c : WideString; Const s : AnsiString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Pos (c : ShortString; Const s : WideString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}

Function UpCase(const s : WideString) : WideString;

Procedure Insert (Const Source : WideString; Var S : WideString; Index : SizeInt);
Procedure Delete (Var S : WideString; Index,Size: SizeInt);
Procedure SetString (Out S : WideString; Buf : PWideChar; Len : SizeInt);
Procedure SetString (Out S : WideString; Buf : PChar; Len : SizeInt);

procedure DefaultWide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
procedure DefaultAnsi2WideMove(source:pchar;var dest:widestring;len:SizeInt);

type
  TWideStringManager = TUnicodeStringManager;

function UnicodeToUtf8(Dest: PChar; Source: PWideChar; MaxBytes: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function UnicodeToUtf8(Dest: PChar; MaxDestBytes: SizeUInt; Source: PWideChar; SourceChars: SizeUInt): SizeUInt;
function Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: SizeUInt; Source: PChar; SourceBytes: SizeUInt): SizeUInt;
function UTF8Encode(const s : WideString) : UTF8String;

{$ifdef MSWINDOWS}
const
  winwidestringalloc : boolean = true;
{$endif MSWINDOWS}

