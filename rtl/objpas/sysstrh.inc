{
    *********************************************************************
    $Id$
    Copyright (C) 1997, 1998 Gertjan Schouten

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

    System Utilities For Free Pascal
}

{==============================================================================}
{   standard functions                                                         }
{==============================================================================}

type
   PString = ^String;

function NewStr(const S: string): PString;
procedure DisposeStr(S: PString);
procedure AssignStr(var P: PString; const S: string);
procedure AppendStr(var Dest: PString; const S: string);
function UpperCase(const s: string): string;
function LowerCase(const s: string): string;
function CompareStr(const S1, S2: string): Integer;
function CompareMem(P1, P2: Pointer; Length: cardinal): integer;
function CompareText(const S1, S2: string): integer;

function AnsiUpperCase(const s: string): string;
function AnsiLowerCase(const s: string): string;
function AnsiCompareStr(const S1, S2: string): integer;
function AnsiCompareText(const S1, S2: string): integer;
function AnsiStrComp(S1, S2: PChar): integer;
function AnsiStrIComp(S1, S2: PChar): integer;
function AnsiStrLComp(S1, S2: PChar; MaxLen: cardinal): integer;
function AnsiStrLIComp(S1, S2: PChar; MaxLen: cardinal): integer;
function AnsiStrLower(Str: PChar): PChar;
function AnsiStrUpper(Str: PChar): PChar;
function AnsiLastChar(const S: string): PChar;
function AnsiStrLastChar(Str: PChar): PChar;

function Trim(const S: string): string;
function TrimLeft(const S: string): string;
function TrimRight(const S: string): string;
function QuotedStr(const S: string): string;
function AnsiQuotedStr(const S: string; Quote: char): string;
function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;
function AdjustLineBreaks(const S: string): string;
function IsValidIdent(const Ident: string): boolean;
function IntToStr(Value: integer): string;
// function IntToStr(Value: Int64): string;
function IntToHex(Value: integer; Digits: integer): string;
// function IntToHex(Value: Int64; Digits: integer): string;
function StrToInt(const s: string): integer;
// function StrToInt64(const s: string): int64;
function StrToIntDef(const S: string; Default: integer): integer;
// function StrToInt64Def(const S: string; Default: int64): int64;
function LoadStr(Ident: integer): string;
// function FmtLoadStr(Ident: integer; const Args: array of const): string;
{$ifdef autoobjpas}
Function Format (Const Fmt : String; const Args : Array of const) : String;
{$endif}

{==============================================================================}
{   extra functions                                                            }
{==============================================================================}

function LeftStr(const S: string; Count: integer): string;
function RightStr(const S: string; Count: integer): string;
function BCDToInt(Value: integer): integer;

{
  $Log$
  Revision 1.4  1998-11-04 10:20:53  peter
    * ansistring fixes

  Revision 1.3  1998/11/02 12:53:53  michael
  + Added format function

  Revision 1.2  1998/09/16 08:28:43  michael
  Update from gertjan Schouten, plus small fix for linux

  Revision 1.1  1998/04/10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.

}

