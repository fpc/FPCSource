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

   TFloatRec = Record
      Exponent: Integer;
      Negative: Boolean;
      Digits: Array[0..18] Of Char;
   End;
   
const
  { For floattodatetime }
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }
                      
                
Const
  LeadBytes: set of Char = [];
  EmptyStr : string = '';
  NullStr : PString = @EmptyStr;
      
  EmptyWideStr : WideString = '';
//  NullWideStr : PWideString = @EmptyWideStr;

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
Function StrToFloat(Const S : String) : Extended;
Function TextToFloat(Buffer: PChar; Var Value: Extended): Boolean;
Function FloatToText(Buffer: PChar; Value: Extended; format: TFloatFormat; Precision, Digits: Integer): Longint;
Function FloatToDateTime (Const Value : Extended) : TDateTime;
Function FloattoCurr (Const Value : Extended) : Currency;
Function CurrToStr(Value: Currency): string;
function StrToCurr(const S: string): Currency;
function StrToBool(const S: string): Boolean;
function BoolToStr(B: Boolean): string;
function LastDelimiter(const Delimiters, S: string): Integer;
function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;
Function FloatToTextFmt(Buffer: PChar; Value: Extended; format: PChar): Integer;
Procedure FloatToDecimal(Var Result: TFloatRec; Value: Extended; Precision, Decimals : integer);
Function FormatFloat(Const Format : String; Value : Extended) : String;
Function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;

{// MBCS Functions. No MBCS yet, so mostly these are calls to the regular counterparts.
Type
  TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);
  
Function AnsiCompareFileName(const S1, S2: string): Integer;
Function SameFileName(const S1, S2: string): Boolean;
Function AnsiLowerCaseFileName(const S: string): string;
Function AnsiUpperCaseFileName(const S: string): string;
Function AnsiPos(const Substr, S: string): Integer;
Function AnsiStrPos(Str, SubStr: PChar): PChar;
Function AnsiStrRScan(Str: PChar; Chr: Char): PChar;
Function AnsiStrScan(Str: PChar; Chr: Char): PChar;
Function ByteType(const S: string; Index: Integer): TMbcsByteType;
Function StrByteType(Str: PChar; Index: Cardinal): TMbcsByteType;
Function ByteToCharLen(const S: string; MaxLen: Integer): Integer;
Function CharToByteLen(const S: string; MaxLen: Integer): Integer;
Function ByteToCharIndex(const S: string; Index: Integer): Integer;
}
{==============================================================================}
{   extra functions                                                            }
{==============================================================================}

function LeftStr(const S: string; Count: integer): string;
function RightStr(const S: string; Count: integer): string;
function BCDToInt(Value: integer): integer;

{
  $Log$
  Revision 1.14  2002-11-28 22:26:30  michael
  + Fixed float<>string conversion routines

  Revision 1.13  2002/11/28 20:29:26  michael
  + made it compile again

  Revision 1.12  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed
  Revision 1.1.2.6  2002/11/28 22:25:01  michael
  + Fixed float<>string conversion routines

  Revision 1.1.2.5  2002/11/28 20:24:12  michael
  + merged some fixes from mainbranch

  Revision 1.1.2.4  2002/10/29 23:41:06  michael
  + Added lots of D4 functions

  Revision 1.11  2002/01/24 18:33:58  peter
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

}
