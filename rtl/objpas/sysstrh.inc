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

   { For FloatToText }
   TFloatFormat = (ffGeneral, ffExponent, ffFixed, ffNumber, ffCurrency);
   TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

const
  { For floattodatetime }
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }



function NewStr(const S: string): PString;
procedure DisposeStr(S: PString);
procedure AssignStr(var P: PString; const S: string);
procedure AppendStr(var Dest: String; const S: string);
function UpperCase(const s: string): string;
function LowerCase(const s: string): string; overload;
function CompareStr(const S1, S2: string): Integer;
function CompareMemRange(P1, P2: Pointer; Length: cardinal): integer;
function CompareMem(P1, P2: Pointer; Length: cardinal): Boolean;
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
function AnsiExtractQuotedStr(Const Src: PChar; Quote: Char): string;
function AdjustLineBreaks(const S: string): string;
function IsValidIdent(const Ident: string): boolean;
function IntToStr(Value: integer): string;
function IntToStr(Value: Int64): string;
function IntToStr(Value: QWord): string;
function IntToHex(Value: integer; Digits: integer): string;
function IntToHex(Value: Int64; Digits: integer): string;
function StrToInt(const s: string): integer;
function StrToInt64(const s: string): int64;
function StrToIntDef(const S: string; Default: integer): integer;
function StrToInt64Def(const S: string; Default: int64): int64;
function LoadStr(Ident: integer): string;
// function FmtLoadStr(Ident: integer; const Args: array of const): string;
Function Format (Const Fmt : String; const Args : Array of const) : String;
Function FormatBuf (Var Buffer; BufLen : Cardinal; Const Fmt; fmtLen : Cardinal; Const Args : Array of const) : Cardinal;
Function StrFmt(Buffer,Fmt : PChar; Const args: Array of const) : Pchar;
Function StrLFmt(Buffer : PCHar; Maxlen : Cardinal;Fmt : PChar; Const args: Array of const) : Pchar;
Procedure FmtStr(Var Res: String; Const Fmt : String; Const args: Array of const);
Function FloatToStrF(Value: Extended; format: TFloatFormat; Precision, Digits: Integer): String;
Function FloatToStr(Value: Extended): String;
Function StrToFloat(Value : String) : Extended;
Function FloatToText(Buffer: PChar; Value: Extended; format: TFloatFormat; Precision, Digits: Integer): Longint;
Function FloatToDateTime (Const Value : Extended) : TDateTime;
Function FloattoCurr (Const Value : Extended) : Currency;
Function CurrToStr(Value: Currency): string;
function StrToCurr(const S: string): Currency;
function StrToBool(const S: string): Boolean;
function BoolToStr(B: Boolean): string;
function LastDelimiter(const Delimiters, S: string): Integer;
function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;

{==============================================================================}
{   extra functions                                                            }
{==============================================================================}

function LeftStr(const S: string; Count: integer): string;
function RightStr(const S: string; Count: integer): string;
function BCDToInt(Value: integer): integer;

{
  $Log$
  Revision 1.11  2002-01-24 18:33:58  peter
    * overload for lowercase()

  Revision 1.10  2002/01/24 12:33:54  jonas
    * adapted ranges of native types to int64 (e.g. high cardinal is no
      longer longint($ffffffff), but just $fffffff in psystem)
    * small additional fix in 64bit rangecheck code generation for 32 bit
      processors
    * adaption of ranges required the matching talgorithm used for selecting
      which overloaded procedure to call to be adapted. It should now always
      select the closest match for ordinal parameters.
    + inttostr(qword) in sysstr.inc/sysstrh.inc
    + abs(int64), sqr(int64), sqr(qword) in systemh.inc/generic.inc (previous
      fixes were required to be able to add them)
    * is_in_limit() moved from ncal to types unit, should always be used
      instead of direct comparisons of low/high values of orddefs because
      qword is a special case

  Revision 1.9  2001/09/20 14:42:34  michael
  + Implemented missing StringReplace function

  Revision 1.6  2000/12/09 10:39:50  florian
    * fixed merging problem

  Revision 1.1.2.3  2001/09/20 14:35:34  michael
   Implemented missing StringReplace function

  Revision 1.1.2.2  2000/12/07 21:48:58  michael
  + Added LastDelimiter function

  Revision 1.1.2.1  2000/08/09 19:31:03  peter
    * int64 updates from Marco

  Revision 1.5  2000/12/07 21:58:30  michael
  + Merged lastdelimiter from fixbranch

  Revision 1.4  2000/08/13 17:55:38  michael
  + Added some missing functions needed for variant support

  Revision 1.3  2000/08/09 07:48:05  marco
   * Uncommented some int64 functions, now that int64 support is ok

  Revision 1.2  2000/07/13 11:33:51  michael
  + removed logs

}
