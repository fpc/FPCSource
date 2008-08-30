{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl,
    member of the Free Pascal development team.

    This file implements support routines for UnicodeStrings with FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Procedure UniqueString (Var S : UnicodeString);external name 'FPC_UNICODESTR_UNIQUE';
Function Pos (Const Substr : UnicodeString; Const Source : UnicodeString) : SizeInt;
Function Pos (c : Char; Const s : UnicodeString) : SizeInt;
Function Pos (c : UnicodeChar; Const s : UnicodeString) : SizeInt;
Function Pos (c : AnsiString; Const s : UnicodeString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Pos (c : UnicodeString; Const s : AnsiString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Pos (c : ShortString; Const s : UnicodeString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}

Function UpCase(const s : UnicodeString) : UnicodeString;
Function  UpCase(c:UnicodeChar):UnicodeChar;

Procedure Insert (Const Source : UnicodeString; Var S : UnicodeString; Index : SizeInt);
Procedure Delete (Var S : UnicodeString; Index,Size: SizeInt);
Procedure SetString (Out S : UnicodeString; Buf : PUnicodeChar; Len : SizeInt);
Procedure SetString (Out S : UnicodeString; Buf : PChar; Len : SizeInt);

function UnicodeCharToString(S : PUnicodeChar) : AnsiString;
function StringToUnicodeChar(const Src : AnsiString;Dest : PUnicodeChar;DestSize : SizeInt) : PUnicodeChar;
function UnicodeCharLenToString(S : PUnicodeChar;Len : SizeInt) : AnsiString;
procedure UnicodeCharLenToStrVar(Src : PUnicodeChar;Len : SizeInt;out Dest : AnsiString);
procedure UnicodeCharToStrVar(S : PUnicodeChar;out Dest : AnsiString);

procedure DefaultUnicode2AnsiMove(source:punicodechar;var dest:ansistring;len:SizeInt);
procedure DefaultAnsi2UnicodeMove(source:pchar;var dest:unicodestring;len:SizeInt);

