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


Procedure UniqueString (Var S : WideString);{$ifdef SYSTEMINLINE}inline;{$endif}
Function Pos (Const Substr : WideString; Const Source : WideString; Offset : SizeInt = 1) : SizeInt;
Function Pos (c : AnsiChar; Const s : WideString; Offset : SizeInt = 1) : SizeInt;
Function Pos (c : WideChar; Const s : WideString; Offset : SizeInt = 1) : SizeInt;
Function Pos (c : WideChar; Const s : RawByteString; Offset : SizeInt = 1) : SizeInt;
Function Pos (const c : RawByteString; Const s : WideString; Offset : SizeInt = 1) : SizeInt;
Function Pos (const c : WideString; Const s : RawByteString; Offset : SizeInt = 1) : SizeInt;
Function Pos (const c : ShortString; Const s : WideString; Offset : SizeInt = 1) : SizeInt;

Function UpCase(const s : WideString) : WideString;

Procedure fpc_setstring_widestr_pwidechar(Out S : WideString; Buf : PWideChar; Len : SizeInt); compilerproc;
Procedure fpc_setstring_widestr_pansichar(Out S : WideString; Buf : PAnsiChar; Len : SizeInt); compilerproc;

procedure DefaultAnsi2WideMove(source:PAnsiChar;cp : TSystemCodePage;var dest:widestring;len:SizeInt);

type
  TWideStringManager = TUnicodeStringManager;

function UTF8Encode(const s : WideString) : RawByteString;

{$ifdef MSWINDOWS}
const
  winwidestringalloc : boolean = true deprecated;
{$endif MSWINDOWS}

