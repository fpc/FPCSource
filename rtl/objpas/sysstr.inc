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
  if Error <> 0 then raise EConvertError.createfmt(SInValidInteger,[S]);
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

Const
  feInvalidFormat   = 1;
  feMissingArgument = 2;
  feInvalidArgIndex = 3;

Procedure Log (Const S: String);

begin
 {$ifdef debug}
 Writeln (S);
 {$endif}
end;

Procedure DoFormatError (ErrCode : Longint);

Var S : String;

begin
  //!! must be changed to contain format string...
  S:='';
  Case ErrCode of
   feInvalidFormat : EConvertError.Createfmt(SInvalidFormat,[s]);
   feMissingArgument : EConvertError.Createfmt(SArgumentMissing,[s]);
   feInvalidArgIndex : EConvertError.Createfmt(SInvalidArgIndex,[s]);
 end;
end;


Function Format (Const Fmt : String; const Args : Array of const) : String;

Var ChPos,OldPos,ArgPos,DoArg,Len : Longint;
    ToAdd : String;
    Index,Width,Prec : Longint;
    Left : Boolean;
    ExtVal: Extended;
    Fchar : char;

  {
    ReadFormat reads the format string. It returns the type character in
    uppercase, and sets index, Width, Prec to their correct values,
    or -1 if not set. It sets Left to true if left alignment was requested.
    In case of an error, DoFormatError is called.
  }

  Function ReadFormat : Char;

  Var Value : longint;

    Procedure ReadInteger;

    Var Code : Word;

    begin
      If Value<>-1 then exit; // Was already read.
      OldPos:=chPos;
      While (Chpos<Len) and
            (Pos(Fmt[chpos],'1234567890')<>0) do inc(chpos);
      If Chpos=len then DoFormatError(feInvalidFormat);
      If Fmt[Chpos]='*' then
        begin
        If (Chpos>OldPos) or (ArgPos>High(Args))
           or (Args[ArgPos].Vtype<>vtInteger) then
          DoFormatError(feInvalidFormat);
        Value:=Args[ArgPos].VInteger;
        Inc(ArgPos);
        Inc(chPos);
        end
      else
        begin
        If (OldPos<chPos) Then
          begin
          Val (Copy(Fmt,OldPos,ChPos-OldPos),value,code);
          // This should never happen !!
          If Code>0 then DoFormatError (feInvalidFormat);
          end
        else
          Value:=-1;
        end;
    end;

    Procedure ReadIndex;

    begin
      ReadInteger;
      If Fmt[ChPos]=':' then
        begin
        If Value=-1 then DoFormatError(feMissingArgument);
        Index:=Value;
        Value:=-1;
        Inc(Chpos);
        end;
      Log ('Read index');
    end;

    Procedure ReadLeft;

    begin
      If Fmt[chpos]='-' then
        begin
        left:=True;
        Inc(chpos);
        end
      else
        Left:=False;
      Log ('Read Left');
    end;

    Procedure ReadWidth;

    begin
      ReadInteger;
      If Value<>-1 then
        begin
        Width:=Value;
        Value:=-1;
        end;
      Log ('Read width');
    end;

    Procedure ReadPrec;

    begin
      If Fmt[chpos]='.' then
        begin
        inc(chpos);
        ReadInteger;
        If Value=-1 then DoFormaterror(feMissingArgument);
        prec:=Value;
        end;
      Log ('Read precision');
    end;

  begin
    Log ('Start format');
    Index:=-1;
    Width:=-1;
    Prec:=-1;
    Value:=-1;
    inc(chpos);
    If Fmt[Chpos]='%' then exit('%');
    ReadIndex;
    ReadLeft;
    ReadWidth;
    ReadPrec;
    ReadFormat:=Upcase(Fmt[ChPos]);
    Log ('End format');
end;

Procedure DumpFormat (C : char);

begin
  Write ('Fmt : ',fmt:10);
  Write (' Index : ',Index:3);
  Write (' Left  : ',left:5);
  Write (' Width : ',Width:3);
  Write (' Prec  : ',prec:3);
  Writeln (' Type  : ',C);
end;

Procedure Checkarg (AT : Longint);
{
  Check if argument INDEX is of correct type (AT)
  If Index=-1, ArgPos is used, and argpos is augmented with 1
  DoArg is set to the argument that must be used.
}
begin
  If Index=-1 then
    begin
    DoArg:=Argpos;
    inc(ArgPos);
    end
  else
    DoArg:=Index;
  If (Doarg>High(Args)) or (Args[Doarg].Vtype<>AT) then
    DoFormatError(feInvalidArgindex);
end;

Const Zero = '000000000000000000000000000000000000000000000000000000000000000';

begin
  Result:='';
  Len:=Length(Fmt)+1;
  Chpos:=1;
  OldPos:=1;
  ArgPos:=0;
  While chpos<len do
    begin
    // uses shortcut evaluation !!
    While (ChPos<=Len) and (Fmt[chpos]<>'%') do inc(chpos);
    If ChPos>OldPos Then
      Result:=Result+Copy(Fmt,OldPos,Chpos-Oldpos);
    If ChPos<Len then
      begin
      FChar:=ReadFormat;
      {$ifdef debug}
      DumpFormat(FCHar);
      {$endif}
      Case FChar of
        'D' : begin
              Checkarg(vtinteger);
              Width:=Abs(width);
              Str(Args[Doarg].VInteger,ToAdd);
              While Length(ToAdd)<Prec do
                begin
                Index:=Prec-Length(ToAdd);
                If Index>64 then Index:=64;
                ToAdd:=Copy(Zero,1,Index)+ToAdd;
                end;
              end;
        'E' : begin
              CheckArg(vtExtended);
              If Prec=-1 then prec:=15;
              ExtVal:=Args[doarg].VExtended^;
              Prec:=Prec+5;   // correct dot, eXXX
              If ExtVal<0 then Inc(Prec); // Corect for minus sign
              If Abs(Extval)<1 then Inc(Prec); // correct for - in E
              Writeln('STRING ',prec);
              Str(Args[doarg].VExtended^:prec,ToAdd);
              WRITELN('DID');
              end;
        'F' : begin
              end;
        'S' : begin
              CheckArg(vtString);
              Index:=Length(Args[doarg].VString^);
              If (Prec<>-1) and (Index>Prec) then
                Index:=Prec;
              ToAdd:=Copy(Args[DoArg].VString^,1,Index);
              end;
        'P' : Begin
              CheckArg(vtpointer);
              ToAdd:=HexStr(Longint(Args[DoArg].VPointer),8);
              // Insert ':'. Is this needed in 32 bit ? No it isn't.
              // Insert(':',ToAdd,5);
              end;
        'X' : begin
              Checkarg(vtinteger);
              If Prec>32 then
                ToAdd:=HexStr(Args[Doarg].VInteger,Prec)
              else
                begin
                // determine minimum needed number of hex digits.
                Index:=1;
                While (1 shl (Index*4))<Args[DoArg].VInteger do
                  inc(Index);
                If Index>Prec then
                  Prec:=Index;
                ToAdd:=HexStr(Args[DoArg].VInteger,Prec);
                end;
              end;

        '%': ToAdd:='%';
      end;
      If Width<>-1 then
        If Length(ToAdd)<Width then
          If not Left then
            ToAdd:=Space(Width-Length(ToAdd))+ToAdd
          else
            ToAdd:=ToAdd+space(Width-Length(ToAdd));
      Result:=Result+ToAdd;
      end;
    inc(chpos);
    Oldpos:=chpos;
    end;
end;

{==============================================================================}
{   extra functions                                                            }
{==============================================================================}

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
   ('€', 'š', '', '¶', 'Ž', '¶', '', '€', 'Ò', 'Ó', 'Ô', 'Ø', '×', 'Þ', 'Ž', '',
    '', '’', '’', 'â', '™', 'ã', 'ê', 'ë', 'Y', '™', 'š', '', 'œ', '', 'ž', 'Ÿ',
    'µ', 'Ö', 'à', 'é', '¥', '¥', '¦', '§', '¨', '©', 'ª', '«', '¬', '­', '®', '¯',
    '°', '±', '²', '³', '´', 'µ', '¶', '·', '¸', '¹', 'º', '»', '¼', '½', '¾', '¿',
    'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Ç', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
    'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß',
    'à', 'á', 'â', 'ã', 'å', 'å', 'æ', 'í', 'è', 'é', 'ê', 'ë', 'í', 'í', 'î', 'ï',
    'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ');

   { lower case translation table for character set 850 }
   CP850LCT: array[128..255] of char =
   ('‡', '', '‚', 'ƒ', '„', '…', '†', '‡', 'ˆ', '‰', 'Š', '‹', 'Œ', '', '„', '†',
    '‚', '‘', '‘', '“', '”', '•', '–', '—', '˜', '”', '', '›', 'œ', '›', 'ž', 'Ÿ',
    ' ', '¡', '¢', '£', '¤', '¤', '¦', '§', '¨', '©', 'ª', '«', '¬', '­', '®', '¯',
    '°', '±', '²', '³', '´', ' ', 'ƒ', '…', '¸', '¹', 'º', '»', '¼', '½', '¾', '¿',
    'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Æ', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
    'Ð', 'Ñ', 'ˆ', '‰', 'Š', 'Õ', '¡', 'Œ', '‹', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', '', 'ß',
    '¢', 'á', '“', '•', 'ä', 'ä', 'æ', 'í', 'è', '£', '–', '—', 'ì', 'ì', 'î', 'ï',
    'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ');

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
type
   TCountryInfo = packed record
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
  Revision 1.10  1998-12-15 22:43:09  peter
    * removed temp symbols

  Revision 1.9  1998/11/04 10:20:52  peter
    * ansistring fixes

  Revision 1.8  1998/10/02 13:57:38  michael
  Format error now causes exception

  Revision 1.7  1998/10/02 12:17:17  michael
  + Made sure it compiles with official 0.99.8

  Revision 1.6  1998/10/02 10:42:17  michael
  + Initial implementation of format

  Revision 1.5  1998/10/01 16:05:37  michael
  Added (empty) format function

  Revision 1.4  1998/09/17 12:39:52  michael
  + Further fixes from GertJan Schouten

  Revision 1.3  1998/09/16 14:34:37  pierre
    * go32v2 did not compile
    * wrong code in systr.inc corrected

  Revision 1.2  1998/09/16 08:28:42  michael
  Update from gertjan Schouten, plus small fix for linux

  Revision 1.1  1998/04/10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.
}

