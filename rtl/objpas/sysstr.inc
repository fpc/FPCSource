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

{   NewStr creates a new PString and assigns S to it
    if length(s) = 0 NewStr returns Nil   }

function NewStr(const S: string): PString;
begin
result := Nil;
{
if Length(S) <> 0 then begin
   result := New(PString);
   result^ := S;
   end ;
}
end ;

{   DisposeStr frees the memory occupied by S   }

procedure DisposeStr(S: PString);
begin
{
if S <> Nil then begin
   Dispose(S);
   S := Nil;
   end ;
}
end ;

{   AssignStr assigns S to P^   }

procedure AssignStr(var P: PString; const S: string);
begin
P^ := s;
end ;

{   AppendStr appends S to Dest   }

procedure AppendStr(var Dest: PString; const S: string);
begin
Dest^ := Dest^ + S;
end ;

{   UpperCase returns a copy of S where all lowercase characters ( from a to z )
    have been converted to uppercase   }

function UpperCase(const S: string): string;
var i: integer;
begin
result := S;
i := Length(S);
while i <> 0 do begin
   if (result[i] in ['a'..'z']) then result[i] := char(byte(result[i]) - 32);
   Dec(i);
   end;
end;

{   LowerCase returns a copy of S where all uppercase characters ( from A to Z )
    have been converted to lowercase  }

function LowerCase(const S: string): string;
var i: integer;
begin
result := S;
i := Length(result);
while i <> 0 do begin
   if (result[i] in ['A'..'Z']) then result[i] := char(byte(result[i]) + 32);
   dec(i);
   end;
end;

{   CompareStr compares S1 and S2, the result is the based on
    substraction of the ascii values of the characters in S1 and S2
    case     result
    S1 < S2  < 0
    S1 > S2  > 0
    S1 = S2  = 0     }

function CompareStr(const S1, S2: string): Integer;
var i, count, count1, count2: integer;
begin
result := 0;
Count1 := Length(S1);
Count2 := Length(S2);
if Count1 > Count2 then Count := Count2
else Count := Count1;
result := CompareMem(@S1[1], @S2[1], Count);
if (result = 0) and (Count1 <> Count2) then begin
   if Count1 > Count2 then result := ord(s1[Count1 + 1])
   else result := -ord(s2[Count2 + 1]);
   end ;
end ;

{   CompareMem returns the result of comparison of Length bytes at P1 and P2
    case       result
    P1 < P2    < 0
    P1 > P2    > 0
    P1 = P2    = 0    }

function CompareMem(P1, P2: Pointer; Length: cardinal): integer;
var i: integer;
begin
i := 0;
result := 0;
while (result = 0) and (i < length) do begin
   result := byte(P1^) - byte(P2^);
   P1 := P1 + 1;
   P2 := P2 + 1;
   i := i + 1;
   end ;
end ;

{   CompareText compares S1 and S2, the result is the based on
    substraction of the ascii values of characters in S1 and S2
    comparison is case-insensitive
    case     result
    S1 < S2  < 0
    S1 > S2  > 0
    S1 = S2  = 0     }

function CompareText(const S1, S2: string): integer;
var i, count, count1, count2: integer; Chr1, Chr2: byte;
begin
result := 0;
Count1 := Length(S1);
Count2 := Length(S2);
if Count1 > Count2 then Count := Count2
else Count := Count1;
i := 0;
while (result = 0) and (i < count) do begin
   i := i + 1;
   Chr1 := byte(s1[i]);
   Chr2 := byte(s2[i]);
   if Chr1 in [97..122] then Chr1 := Chr1 - 32;
   if Chr2 in [97..122] then Chr2 := Chr2 - 32;
   result := Chr1 - Chr2;
   end ;
if (result = 0) and (Count1 <> Count2) then begin
   if Count1 > Count2 then result := byte(UpCase(s1[Count1 + 1]))
   else result := -byte(UpCase(s2[Count2 + 1]));
   end ;
end ;

{==============================================================================}
{   Ansi string functions                                                      }
{   these functions rely on the character set loaded by the OS                 }
{==============================================================================}

type
   TCaseTranslationTable = array[0..255] of char;

var
   UpperCaseTable: TCaseTranslationTable;
   LowerCaseTable: TCaseTranslationTable;

function AnsiUpperCase(const s: string): string;
var len, i: integer;
begin
len := length(s);
SetLength(result, len);
for i := 1 to len do
   result[i] := UpperCaseTable[ord(s[i])];
end ;

function AnsiLowerCase(const s: string): string;
var len, i: integer;
begin
len := length(s);
SetLength(result, len);
for i := 1 to len do
   result[i] := LowerCaseTable[ord(s[i])];
end ;

function AnsiCompareStr(const S1, S2: string): integer;
begin
end ;

function AnsiCompareText(const S1, S2: string): integer;
begin
end ;

function AnsiStrComp(S1, S2: PChar): integer;
begin
end ;

function AnsiStrIComp(S1, S2: PChar): integer;
begin
end ;

function AnsiStrLComp(S1, S2: PChar; MaxLen: cardinal): integer;
begin
end ;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: cardinal): integer;
begin
end ;

function AnsiStrLower(Str: PChar): PChar;
begin
if Str <> Nil then begin
   while Str^ <> #0 do begin
      Str^ := LowerCaseTable[byte(Str^)];
      Str := Str + 1;
      end ;
   end ;
result := Str;
end ;

function AnsiStrUpper(Str: PChar): PChar;
begin
if Str <> Nil then begin
   while Str^ <> #0 do begin
      Str^ := UpperCaseTable[byte(Str^)];
      Str := Str + 1;
      end ;
   end ;
result := Str;
end ;

function AnsiLastChar(const S: string): PChar;
begin
end ;

function AnsiStrLastChar(Str: PChar): PChar;
begin
end ;

{==============================================================================}
{  End of Ansi functions                                                       }
{==============================================================================}

{   Trim returns a copy of S with blanks characters on the left and right stripped off   }

function Trim(const S: string): string;
var Ofs, Len: integer;
begin
len := Length(S);
while (S[Len] = ' ') and (Len > 0) do
   dec(Len);
Ofs := 1;
while (S[Ofs] = ' ') and (Ofs <= Len) do
   Inc(Ofs);
result := Copy(S, Ofs, 1 + Len - Ofs);
end ;

{   TrimLeft returns a copy of S with all blank characters on the left stripped off  }

function TrimLeft(const S: string): string;
var i,l:integer;
begin
l := length(s);
i := 1;
while (s[i] = ' ') and (i <= l) do inc(i);
Result := copy(s, i, l);
end ;

{   TrimRight returns a copy of S with all blank characters on the right stripped off  }

function TrimRight(const S: string): string;
var l:integer;
begin
l := length(s);
while (s[l] = ' ') and (l > 0) do dec(l);
result := copy(s,1,l);
end ;

{   QuotedStr returns S quoted left and right and every single quote in S
    replaced by two quotes   }

function QuotedStr(const S: string): string;
begin
result := AnsiQuotedStr(s, '''');
end ;

{   AnsiQuotedStr returns S quoted left and right by Quote,
    and every single occurance of Quote replaced by two   }

function AnsiQuotedStr(const S: string; Quote: char): string;
var i, j, count: integer;
begin
result := '' + Quote;
count := length(s);
i := 0;
j := 0;
while i < count do begin
   i := i + 1;
   if S[i] = Quote then begin
      result := result + copy(S, 1 + j, i - j) + Quote;
      j := i;
      end ;
   end ;
if i <> j then
   result := result + copy(S, 1 + j, i - j);
result := result + Quote;
end ;

{   AnsiExtractQuotedStr returns a copy of Src with quote characters
    deleted to the left and right and double occurances
    of Quote replaced by a single Quote   }

function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;
var i: integer; P, Q: PChar;
begin
P := Src;
if Src^ = Quote then P := P + 1;
Q := StrEnd(P);
if PChar(Q - 1)^ = Quote then Q := Q - 1;
SetLength(result, Q - P);
i := 0;
while P <> Q do begin
   i := i + 1;
   result[i] := P^;
   if (P^ = Quote) and (PChar(P + 1)^ = Quote) then
      P := P + 1;
   P := P + 1;
   end ;
SetLength(result, i);
end ;

{   AdjustLineBreaks returns S with all CR characters not followed by LF
    replaced with CR/LF  }
//  under Linux all CR characters or CR/LF combinations should be replaced with LF

function AdjustLineBreaks(const S: string): string;
var i, j, count: integer;
begin
result := '';
i := 0;
j := 0;
count := Length(S);
while i < count do begin
   i := i + 1;
   if (S[i] = #13) and ((i = count) or (S[i + 1] <> #10)) then begin
      result := result + Copy(S, 1 + j, i - j) + #10;
      j := i;
      end ;
   end ;
if j <> i then
   result := result + copy(S, 1 + j, i - j);
end ;

{   IsValidIdent returns true if the first character of Ident is in:
    'A' to 'Z', 'a' to 'z' or '_' and the following characters are
    on of: 'A' to 'Z', 'a' to 'z', '0'..'9' or '_'    }

function IsValidIdent(const Ident: string): boolean;
var i, len: integer;
begin
result := false;
len := length(Ident);
if len <> 0 then begin
   result := Ident[1] in ['A'..'Z', 'a'..'z', '_'];
   i := 1;
   while (result) and (i < len) do begin
      i := i + 1;
      result := result and (Ident[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
      end ;
   end ;
end ;

{   IntToStr returns a string representing the value of Value    }

function IntToStr(Value: integer): string;
begin
System.Str(Value, result);
end ;

{   IntToHex returns a string representing the hexadecimal value of Value   }

const
   HexDigits: array[0..15] of char = '0123456789ABCDEF';

function IntToHex(Value: integer; Digits: integer): string;
var i: integer;
begin
SetLength(result, digits);
for i := 0 to digits - 1 do begin
   result[digits - i] := HexDigits[value and 15];
   value := value shr 4;
   end ;
end ;

{   StrToInt converts the string S to an integer value,
    if S does not represent a valid integer value EConvertError is raised  }

function StrToInt(const S: string): integer;
var Error: word;
begin
 Val(S, result, Error);
 if Error <> 0 then raise EConvertError.create(s + ' is not a valid integer');
end ;

{   StrToIntDef converts the string S to an integer value,
    Default is returned in case S does not represent a valid integer value  }

function StrToIntDef(const S: string; Default: integer): integer;
var Error: word;
begin
Val(S, result, Error);
if Error <> 0 then result := Default;
end ;

{   LoadStr returns the string resource Ident.   }

function LoadStr(Ident: integer): string;
begin
end ;

{   FmtLoadStr returns the string resource Ident and formats it accordingly   }


function FmtLoadStr(Ident: integer; const Args: array of const): string;
begin
end;


Function Format (Const Fmt : String; Const Args: Array of const) : string;

begin
end;


{==============================================================================}
{   extra functions                                                            }
{==============================================================================}

{   SetLength sets the length of S to NewLength   }
//  SetLength should be in the system unit
//  which lacks the ShortString version of SetLength

function SetLength(var S: string; NewLength: integer): integer;
begin
if (NewLength > 255) then
   NewLength := 255;
S[0] := char(NewLength);
Result := Ord(S[0]);
end ;

{   LeftStr returns Count left-most characters from S   }

function LeftStr(const S: string; Count: integer): string;
begin
result := Copy(S, 1, Count);
end ;

{    RightStr returns Count right-most characters from S   }

function RightStr(const S: string; Count: integer): string;
begin
result := Copy(S, 1 + Length(S) - Count, Count);
end ;

{    BCDToInt converts the BCD value Value to an integer   }

function BCDToInt(Value: integer): integer;
var i, j: integer;
begin
result := 0;
j := 1;
for i := 0 to SizeOf(Value) shr 1 - 1 do begin
   result := result + j * (Value and 15);
   j := j * 10;
   Value := Value shr 4;
   end ;
end ;

{  Case Translation Tables  }

   {  Although these tables can be obtained through system calls  }
   {  it is better to not use those, since most implementation are not 100%  }

   {  WARNING:  }
   {  before modifying a translation table make sure that the current codepage  }
   {  of the OS corresponds to the one you make changes to  }

const
   { upper case translation table for character set 850 }
   CP850UCT: array[128..255] of char =
   ('Ä', 'ö', 'ê', '∂', 'é', '∂', 'è', 'Ä', '“', '”', '‘', 'ÿ', '◊', 'ﬁ', 'é', 'è',
    'ê', 'í', 'í', '‚', 'ô', '„', 'Í', 'Î', 'Y', 'ô', 'ö', 'ù', 'ú', 'ù', 'û', 'ü',
    'µ', '÷', '‡', 'È', '•', '•', '¶', 'ß', '®', '©', '™', '´', '¨', '≠', 'Æ', 'Ø',
    '∞', '±', '≤', '≥', '¥', 'µ', '∂', '∑', '∏', 'π', '∫', 'ª', 'º', 'Ω', 'æ', 'ø',
    '¿', '¡', '¬', '√', 'ƒ', '≈', '«', '«', '»', '…', ' ', 'À', 'Ã', 'Õ', 'Œ', 'œ',
    '–', '—', '“', '”', '‘', '’', '÷', '◊', 'ÿ', 'Ÿ', '⁄', '€', '‹', '›', 'ﬁ', 'ﬂ',
    '‡', '·', '‚', '„', 'Â', 'Â', 'Ê', 'Ì', 'Ë', 'È', 'Í', 'Î', 'Ì', 'Ì', 'Ó', 'Ô',
    '', 'Ò', 'Ú', 'Û', 'Ù', 'ı', 'ˆ', '˜', '¯', '˘', '˙', '˚', '¸', '˝', '˛', 'ˇ');

   { lower case translation table for character set 850 }
   CP850LCT: array[128..255] of char =
   ('á', 'Å', 'Ç', 'É', 'Ñ', 'Ö', 'Ü', 'á', 'à', 'â', 'ä', 'ã', 'å', 'ç', 'Ñ', 'Ü',
    'Ç', 'ë', 'ë', 'ì', 'î', 'ï', 'ñ', 'ó', 'ò', 'î', 'Å', 'õ', 'ú', 'õ', 'û', 'ü',
    '†', '°', '¢', '£', '§', '§', '¶', 'ß', '®', '©', '™', '´', '¨', '≠', 'Æ', 'Ø',
    '∞', '±', '≤', '≥', '¥', '†', 'É', 'Ö', '∏', 'π', '∫', 'ª', 'º', 'Ω', 'æ', 'ø',
    '¿', '¡', '¬', '√', 'ƒ', '≈', '∆', '∆', '»', '…', ' ', 'À', 'Ã', 'Õ', 'Œ', 'œ',
    '–', '—', 'à', 'â', 'ä', '’', '°', 'å', 'ã', 'Ÿ', '⁄', '€', '‹', '›', 'ç', 'ﬂ',
    '¢', '·', 'ì', 'ï', '‰', '‰', 'Ê', 'Ì', 'Ë', '£', 'ñ', 'ó', 'Ï', 'Ï', 'Ó', 'Ô',
    '', 'Ò', 'Ú', 'Û', 'Ù', 'ı', 'ˆ', '˜', '¯', '˘', '˙', '˚', '¸', '˝', '˛', 'ˇ');

   { upper case translation table for character set ISO 8859/1  Latin 1  }
   CPISO88591UCT: array[192..255] of char =
   ( #192, #193, #194, #195, #196, #197, #198, #199,
     #200, #201, #202, #203, #204, #205, #206, #207,
     #208, #209, #210, #211, #212, #213, #214, #215,
     #216, #217, #218, #219, #220, #221, #222, #223,
     #192, #193, #194, #195, #196, #197, #198, #199,
     #200, #201, #202, #203, #204, #205, #206, #207,
     #208, #209, #210, #211, #212, #213, #214, #247,
     #216, #217, #218, #219, #220, #221, #222, #89 );

   { lower case translation table for character set ISO 8859/1  Latin 1  }
   CPISO88591LCT: array[192..255] of char =
   ( #224, #225, #226, #227, #228, #229, #230, #231,
     #232, #233, #234, #235, #236, #237, #238, #239,
     #240, #241, #242, #243, #244, #245, #246, #215,
     #248, #249, #250, #251, #252, #253, #254, #223,
     #224, #225, #226, #227, #228, #229, #230, #231,
     #232, #233, #234, #235, #236, #237, #238, #239,
     #240, #241, #242, #243, #244, #245, #246, #247,
     #248, #249, #250, #251, #252, #253, #254, #255 );

{$IFDEF GO32V2}

{  Codepage constants  }

const
   CP_US = 437;
   CP_MultiLingual = 850;
   CP_SlavicLatin2 = 852;
   CP_Turkish = 857;
   CP_Portugal = 860;
   CP_IceLand = 861;
   CP_Canada = 863;
   CP_NorwayDenmark = 865;

{  CountryInfo   }

{$PACKRECORDS 1}
type
   TCountryInfo = record
      InfoId: byte;
      case integer of
         1: ( Size: word;
              CountryId: word;
              CodePage: word;
              CountryInfo: array[0..33] of byte );
         2: ( UpperCaseTable: longint );
         4: ( FilenameUpperCaseTable: longint );
         5: ( FilecharacterTable: longint );
         6: ( CollatingTable: longint );
         7: ( DBCSLeadByteTable: longint );
   end ;

{$PACKRECORDS NORMAL}

procedure GetExtendedCountryInfo(InfoId: integer; CodePage, CountryId: word; var CountryInfo: TCountryInfo);
var Regs: Registers;
begin
Regs.AH := $65;
Regs.AL := InfoId;
Regs.BX := CodePage;
Regs.DX := CountryId;
Regs.ES := transfer_buffer div 16;
Regs.DI := transfer_buffer and 15;
Regs.CX := SizeOf(TCountryInfo);
RealIntr($21, Regs);
DosMemGet(transfer_buffer shr 16, transfer_buffer and 65535, CountryInfo, Regs.CX );
end ;

procedure InitAnsi;
var CountryInfo: TCountryInfo; i: integer;
begin
{  Fill table entries 0 to 127  }
for i := 0 to 96 do
   UpperCaseTable[i] := chr(i);
for i := 97 to 122 do
   UpperCaseTable[i] := chr(i - 32);
for i := 123 to 127 do
   UpperCaseTable[i] := chr(i);
for i := 0 to 64 do
   LowerCaseTable[i] := chr(i);
for i := 65 to 90 do
   LowerCaseTable[i] := chr(i + 32);
for i := 91 to 255 do
   LowerCaseTable[i] := chr(i);
{  Get country and codepage info  }
GetExtendedCountryInfo(1, $FFFF, $FFFF, CountryInfo);
if CountryInfo.CodePage = 850 then begin
   Move(CP850UCT, UpperCaseTable[128], 128);
   Move(CP850LCT, LowerCaseTable[128], 128);
   end
else begin
{ this needs to be checked !!
  this is correct only if UpperCaseTable is
  and Offset:Segment word record (PM) }
   {  get the uppercase table from dosmemory  }
   GetExtendedCountryInfo(2, $FFFF, $FFFF, CountryInfo);
   DosMemGet(CountryInfo.UpperCaseTable shr 16, 2 + CountryInfo.UpperCaseTable and 65535, UpperCaseTable[128], 128);
   for i := 128 to 255 do begin
      if UpperCaseTable[i] <> chr(i) then
         LowerCaseTable[ord(UpperCaseTable[i])] := chr(i);
      end ;
   end ;
end ;

{$ELSE}
// {$IFDEF LINUX}

procedure InitAnsi;
begin
end ;

// {$ENDIF}
{$ENDIF}

{
  $Log$
  Revision 1.5  1998-10-01 16:05:37  michael
  Added (empty) format function

  Revision 1.4  1998/09/17 12:39:52  michael
  + Further fixes from GertJan Schouten

  Revision 1.3  1998/09/16 14:34:37  pierre
    * go32v2 did not compile
    * wrong code in systr.inc corrected

  Revision 1.2  1998/09/16 08:28:42  michael
  Update from gertjan Schouten, plus small fix for linux

  $Log$
  Revision 1.5  1998-10-01 16:05:37  michael
  Added (empty) format function

  Revision 1.4  1998/09/17 12:39:52  michael
  + Further fixes from GertJan Schouten

  Revision 1.1  1998/04/10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.
}

