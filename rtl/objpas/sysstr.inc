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
  if (S='') then
   Result:=nil
  else
   begin
     getmem(Result,length(s)+1);
     if (Result<>nil) then
      Result^:=s;
   end;
end;

{   DisposeStr frees the memory occupied by S   }

procedure DisposeStr(S: PString);
begin
  if S <> Nil then
   begin
     Freemem(S,Length(S^)+1);
     S:=nil;
   end;
end;

{   AssignStr assigns S to P^   }

procedure AssignStr(var P: PString; const S: string);
begin
  P^ := s;
end ;

{   AppendStr appends S to Dest   }

procedure AppendStr(var Dest: String; const S: string);
begin
Dest := Dest + S;
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
var count, count1, count2: integer;
begin
  result := 0;
  Count1 := Length(S1);
  Count2 := Length(S2);
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  result := CompareMemRange(Pointer(S1),Pointer(S2), Count);
  if (result=0) and (Count1<>Count2) then
    begin
    if Count1>Count2 then
      result:=ord(s1[Count+1])
    else
      result:=-ord(s2[Count+1]);
    end;
end;

{   CompareMemRange returns the result of comparison of Length bytes at P1 and P2
    case       result
    P1 < P2    < 0
    P1 > P2    > 0
    P1 = P2    = 0    }

function CompareMemRange(P1, P2: Pointer; Length: cardinal): integer;

var
  i: cardinal;

begin
  i := 0;
  result := 0;
  while (result=0) and (I<length) do
    begin
    result:=byte(P1^)-byte(P2^);
    P1:=P1+1;
    P2:=P2+1;
    i := i + 1;
   end ;
end ;

function CompareMem(P1, P2: Pointer; Length: cardinal): Boolean;
var
  i: cardinal;
begin
  for i := 0 to Length - 1 do
  begin
    if Byte(P1^) <> Byte(P2^) then
    begin
      Result := False;
      exit;
    end;
    Inc(P1);
    Inc(P2);
  end;
  Result := True;
end;


{   CompareText compares S1 and S2, the result is the based on
    substraction of the ascii values of characters in S1 and S2
    comparison is case-insensitive
    case     result
    S1 < S2  < 0
    S1 > S2  > 0
    S1 = S2  = 0     }

function CompareText(const S1, S2: string): integer;

var
  i, count, count1, count2: integer; Chr1, Chr2: byte;
begin
  result := 0;
  Count1 := Length(S1);
  Count2 := Length(S2);
  if (Count1>Count2) then
    Count := Count2
  else
    Count := Count1;
  i := 0;
  while (result=0) and (i<count) do
    begin
    inc (i);
     Chr1 := byte(s1[i]);
     Chr2 := byte(s2[i]);
     if Chr1 in [97..122] then
       dec(Chr1,32);
     if Chr2 in [97..122] then
       dec(Chr2,32);
     result := Chr1 - Chr2;
     end ;
  if (result = 0) then
    result:=(count1-count2);
end;

{==============================================================================}
{   Ansi string functions                                                      }
{   these functions rely on the character set loaded by the OS                 }
{==============================================================================}


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

Var I,L1,L2 : Longint;

begin
  Result:=0;
  L1:=Length(S1);
  L2:=Length(S2);
  I:=1;
  While (Result=0) and ((I<=L1) and (I<=L2)) do
    begin
    Result:=Ord(S1[I])-Ord(S2[I]); //!! Must be replaced by ansi characters !!
    Inc(I);
    end;
  If Result=0 Then
    Result:=L1-L2;
end;

function AnsiCompareText(const S1, S2: string): integer;
Var I,L1,L2 : Longint;

begin
  Result:=0;
  L1:=Length(S1);
  L2:=Length(S2);
  I:=1;
  While (Result=0) and ((I<=L1) and (I<=L2)) do
    begin
    Result:=Ord(LowerCaseTable[Ord(S1[I])])-Ord(LowerCaseTable[Ord(S2[I])]); //!! Must be replaced by ansi characters !!
    Inc(I);
    end;
  If Result=0 Then
    Result:=L1-L2;
end;

function AnsiStrComp(S1, S2: PChar): integer;

begin
  Result:=0;
  If S1=Nil then
    begin
    If S2=Nil Then Exit;
    result:=-1;
    exit;
    end;
  If S2=Nil then
    begin
    Result:=1;
    exit;
    end;
  Repeat
    Result:=Ord(S1[0])-Ord(S2[0]); //!! Must be replaced by ansi characters !!
    Inc(S1);
    Inc(S2);
  Until (Result<>0) or ((S1[0]=#0) or (S2[0]=#0))
end;

function AnsiStrIComp(S1, S2: PChar): integer;

begin
  Result:=0;
  If S1=Nil then
    begin
    If S2=Nil Then Exit;
    result:=-1;
    exit;
    end;
  If S2=Nil then
    begin
    Result:=1;
    exit;
    end;
  Repeat
    Result:=Ord(LowerCaseTable[Ord(S1[0])])-Ord(LowerCaseTable[Ord(S2[0])]); //!! Must be replaced by ansi characters !!
    Inc(S1);
    Inc(S2);
  Until (Result<>0) or ((S1[0]=#0) or (S2[0]=#0))
end;

function AnsiStrLComp(S1, S2: PChar; MaxLen: cardinal): integer;

Var I : cardinal;

begin
  Result:=0;
  If MaxLen=0 then exit;
  If S1=Nil then
    begin
    If S2=Nil Then Exit;
    result:=-1;
    exit;
    end;
  If S2=Nil then
    begin
    Result:=1;
    exit;
    end;
  I:=0;
  Repeat
    Result:=Ord(S1[0])-Ord(S2[0]); //!! Must be replaced by ansi characters !!
    Inc(S1);
    Inc(S2);
    Inc(I);
  Until (Result<>0) or ((S1[0]=#0) or (S2[0]=#0)) or (I=MaxLen)
end ;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: cardinal): integer;

Var I : cardinal;

begin
  Result:=0;
  If MaxLen=0 then exit;
  If S1=Nil then
    begin
    If S2=Nil Then Exit;
    result:=-1;
    exit;
    end;
  If S2=Nil then
    begin
    Result:=1;
    exit;
    end;
  I:=0;
  Repeat
    Result:=Ord(LowerCaseTable[Ord(S1[0])])-Ord(LowerCaseTable[Ord(S2[0])]); //!! Must be replaced by ansi characters !!
    Inc(S1);
    Inc(S2);
    Inc(I);
  Until (Result<>0) or ((S1[0]=#0) or (S2[0]=#0)) or (I=MaxLen)
end ;

function AnsiStrLower(Str: PChar): PChar;
begin
result := Str;
if Str <> Nil then begin
   while Str^ <> #0 do begin
      Str^ := LowerCaseTable[byte(Str^)];
      Str := Str + 1;
      end ;
   end ;
end ;

function AnsiStrUpper(Str: PChar): PChar;
begin
result := Str;
if Str <> Nil then begin
   while Str^ <> #0 do begin
      Str^ := UpperCaseTable[byte(Str^)];
      Str := Str + 1;
      end ;
   end ;
end ;

function AnsiLastChar(const S: string): PChar;

begin
  //!! No multibyte yet, so we return the last one.
  result:=StrEnd(Pchar(S));
  Dec(Result);
end ;

function AnsiStrLastChar(Str: PChar): PChar;
begin
  //!! No multibyte yet, so we return the last one.
  result:=StrEnd(Str);
  Dec(Result);
end ;

{==============================================================================}
{  End of Ansi functions                                                       }
{==============================================================================}

{   Trim returns a copy of S with blanks characters on the left and right stripped off   }

Const WhiteSpace = [' ',#10,#13,#9];

function Trim(const S: string): string;
var Ofs, Len: integer;
begin
  len := Length(S);
  while (Len>0) and (S[Len] in WhiteSpace) do
   dec(Len);
  Ofs := 1;
  while (Ofs<=Len) and (S[Ofs] in WhiteSpace) do
   Inc(Ofs);
  result := Copy(S, Ofs, 1 + Len - Ofs);
end ;

{   TrimLeft returns a copy of S with all blank characters on the left stripped off  }

function TrimLeft(const S: string): string;
var i,l:integer;
begin
  l := length(s);
  i := 1;
  while (i<=l) and (s[i] in whitespace) do
   inc(i);
  Result := copy(s, i, l);
end ;

{   TrimRight returns a copy of S with all blank characters on the right stripped off  }

function TrimRight(const S: string): string;
var l:integer;
begin
  l := length(s);
  while (l>0) and (s[l] in whitespace) do
   dec(l);
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

function AnsiExtractQuotedStr(Const Src: PChar; Quote: Char): string;
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
{$ifndef Unix}
   if (S[i] = #13) and ((i = count) or (S[i + 1] <> #10)) then
     begin
     result := result + Copy(S, 1 + j, i - j) + #10;
     j := i;
     end;
{$else}
   If S[i]=#13 then
     begin
     Result:= Result+Copy(S,J+1,i-j-1)+#10;
     If I<>Count Then
       If S[I+1]=#10 then inc(i);
     J :=I;
     end;
{$endif}
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


function IntToStr(Value: int64): string;
begin
 System.Str(Value, result);
end ;

function IntToStr(Value: QWord): string;
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
 for i := 0 to digits - 1 do
  begin
   result[digits - i] := HexDigits[value and 15];
   value := value shr 4;
  end ;
end ;


function IntToHex(Value: int64; Digits: integer): string;
var i: integer;
begin
 SetLength(result, digits);
 for i := 0 to digits - 1 do
  begin
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


function StrToInt64(const S: string): int64;

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

{   StrToIntDef converts the string S to an integer value,
    Default is returned in case S does not represent a valid integer value  }

function StrToInt64Def(const S: string; Default: int64): int64;
var Error: word;
begin
Val(S, result, Error);
if Error <> 0 then result := Default;
end ;


{   LoadStr returns the string resource Ident.   }

function LoadStr(Ident: integer): string;
begin
  result:='';
end ;

{   FmtLoadStr returns the string resource Ident and formats it accordingly   }


function FmtLoadStr(Ident: integer; const Args: array of const): string;
begin
  result:='';
end;

Const
  feInvalidFormat   = 1;
  feMissingArgument = 2;
  feInvalidArgIndex = 3;

{$ifdef fmtdebug}
Procedure Log (Const S: String);
begin
 Writeln (S);
end;
{$endif}


Procedure DoFormatError (ErrCode : Longint);
Var
  S : String;
begin
  //!! must be changed to contain format string...
  S:='';
  Case ErrCode of
   feInvalidFormat : raise EConvertError.Createfmt(SInvalidFormat,[s]);
   feMissingArgument : raise EConvertError.Createfmt(SArgumentMissing,[s]);
   feInvalidArgIndex : raise EConvertError.Createfmt(SInvalidArgIndex,[s]);
 end;
end;


Function Format (Const Fmt : String; const Args : Array of const) : String;

Var ChPos,OldPos,ArgPos,DoArg,Len : Longint;
    Hs,ToAdd : String;
    Index,Width,Prec : Longint;
    Left : Boolean;
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
      While (Chpos<=Len) and
            (Pos(Fmt[chpos],'1234567890')<>0) do inc(chpos);
      If Chpos>len then
        DoFormatError(feInvalidFormat);
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
{$ifdef fmtdebug}
      Log ('Read index');
{$endif}
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
{$ifdef fmtdebug}
      Log ('Read Left');
{$endif}
    end;

    Procedure ReadWidth;

    begin
      ReadInteger;
      If Value<>-1 then
        begin
        Width:=Value;
        Value:=-1;
        end;
{$ifdef fmtdebug}
      Log ('Read width');
{$endif}
    end;

    Procedure ReadPrec;

    begin
      If Fmt[chpos]='.' then
        begin
        inc(chpos);
        ReadInteger;
        If Value=-1 then
         Value:=0;
        prec:=Value;
        end;
{$ifdef fmtdebug}
      Log ('Read precision');
{$endif}
    end;

  begin
{$ifdef fmtdebug}
    Log ('Start format');
{$endif}
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
{$ifdef fmtdebug}
    Log ('End format');
{$endif}
end;


{$ifdef fmtdebug}
Procedure DumpFormat (C : char);
begin
  Write ('Fmt : ',fmt:10);
  Write (' Index : ',Index:3);
  Write (' Left  : ',left:5);
  Write (' Width : ',Width:3);
  Write (' Prec  : ',prec:3);
  Writeln (' Type  : ',C);
end;
{$endif}


function Checkarg (AT : Longint;err:boolean):boolean;
{
  Check if argument INDEX is of correct type (AT)
  If Index=-1, ArgPos is used, and argpos is augmented with 1
  DoArg is set to the argument that must be used.
}
begin
  result:=false;
  if Index=-1 then
    begin
    DoArg:=Argpos;
    inc(ArgPos);
    end
  else
    DoArg:=Index;
  If (Doarg>High(Args)) or (Args[Doarg].Vtype<>AT) then
   begin
     if err then
      DoFormatError(feInvalidArgindex);
     dec(ArgPos);
     exit;
   end;
  result:=true;
end;

Const Zero = '000000000000000000000000000000000000000000000000000000000000000';

begin
  Result:='';
  Len:=Length(Fmt);
  Chpos:=1;
  OldPos:=1;
  ArgPos:=0;
  While chpos<=len do
    begin
    While (ChPos<=Len) and (Fmt[chpos]<>'%') do
      inc(chpos);
    If ChPos>OldPos Then
      Result:=Result+Copy(Fmt,OldPos,Chpos-Oldpos);
    If ChPos<Len then
      begin
      FChar:=ReadFormat;
{$ifdef fmtdebug}
      DumpFormat(FCHar);
{$endif}
      Case FChar of
        'D' : begin
              if Checkarg(vtinteger,false) then
                Str(Args[Doarg].VInteger,ToAdd)
              else if CheckArg(vtInt64,true) then
                Str(Args[DoArg].VInt64^,toadd);
              Width:=Abs(width);
              Index:=Prec-Length(ToAdd);
              If ToAdd[1]<>'-' then
                ToAdd:=StringOfChar('0',Index)+ToAdd
              else
                // + 1 to accomodate for - sign in length !!
                Insert(StringOfChar('0',Index+1),toadd,2);
              end;
        'E' : begin
              CheckArg(vtExtended,true);
              ToAdd:=FloatToStrF(Args[doarg].VExtended^,ffexponent,Prec,3);
              end;
        'F' : begin
              CheckArg(vtExtended,true);
              ToAdd:=FloatToStrF(Args[doarg].VExtended^,ffFixed,9999,Prec);
              end;
        'G' : begin
              CheckArg(vtExtended,true);
              ToAdd:=FloatToStrF(Args[doarg].VExtended^,ffGeneral,Prec,3);
              end;
        'N' : begin
              CheckArg(vtExtended,true);
              ToAdd:=FloatToStrF(Args[doarg].VExtended^,ffNumber,9999,Prec);
              end;
        'M' : begin
              CheckArg(vtExtended,true);
              ToAdd:=FloatToStrF(Args[doarg].VExtended^,ffCurrency,9999,Prec);
              end;
        'S' : begin
                if CheckArg(vtString,false) then
                  hs:=Args[doarg].VString^
                else
                  if CheckArg(vtChar,false) then
                    hs:=Args[doarg].VChar
                else
                  if CheckArg(vtPChar,false) then
                    hs:=Args[doarg].VPChar
                else
                  if CheckArg(vtPWideChar,false) then
                    hs:=char(Args[doarg].VPWideChar^)
                else
                  if CheckArg(vtWideChar,false) then
                    hs:=char(Args[doarg].VWideChar)
                else
                  if CheckArg(vtWidestring,false) then
                    hs:=ansistring(Args[doarg].VWideString)
                else
                  if CheckArg(vtAnsiString,true) then
                    hs:=ansistring(Args[doarg].VAnsiString);
                Index:=Length(hs);
                If (Prec<>-1) and (Index>Prec) then
                  Index:=Prec;
                ToAdd:=Copy(hs,1,Index);
              end;
        'P' : Begin
              CheckArg(vtpointer,true);
              ToAdd:=HexStr(Longint(Args[DoArg].VPointer),8);
              // Insert ':'. Is this needed in 32 bit ? No it isn't.
              // Insert(':',ToAdd,5);
              end;
        'X' : begin
              Checkarg(vtinteger,true);
              If Prec>15 then
                ToAdd:=HexStr(Args[Doarg].VInteger,15)
              else
                begin
                // determine minimum needed number of hex digits.
                Index:=1;
                While (DWord(1 shl (Index*4))<=DWord(Args[DoArg].VInteger)) and (index<8) do
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

Function FormatBuf (Var Buffer; BufLen : Cardinal;
                     Const Fmt; fmtLen : Cardinal;
                     Const Args : Array of const) : Cardinal;

Var S,F : String;

begin
  Setlength(F,fmtlen);
  if fmtlen > 0 then
    Move(fmt,F[1],fmtlen);
  S:=Format (F,Args);
  If Length(S)<Buflen then
    Result:=Length(S)
  else
    Result:=Buflen;
  Move(S[1],Buffer,Result);
end;

Procedure FmtStr(Var Res: String; Const Fmt : String; Const args: Array of const);

begin
  Res:=Format(fmt,Args);
end;

Function StrFmt(Buffer,Fmt : PChar; Const args: Array of const) : Pchar;

begin
  Buffer[FormatBuf(Buffer^,Maxint,Fmt^,strlen(fmt),args)]:=#0;
  Result:=Buffer;
end;

Function StrLFmt(Buffer : PCHar; Maxlen : Cardinal;Fmt : PChar; Const args: Array of const) : Pchar;

begin
  Buffer[FormatBuf(Buffer^,MaxLen,Fmt^,strlen(fmt),args)]:=#0;
  Result:=Buffer;
end;

Function StrToFloat(Const S: String): Extended;

Begin
  If Not TextToFloat(Pchar(S),Result) then
    Raise EConvertError.createfmt(SInValidFLoat,[S]);
End;

function StrToFloatDef(const S: string; const Default: Extended): Extended;

begin
   if not TextToFloat(PChar(S),Result,fvExtended) then
     Result:=Default;
end;

Function TextToFloat(Buffer: PChar; Var Value: Extended): Boolean;

Var
  E,P : Integer;
  S : String;

Begin
  S:=StrPas(Buffer);
  P:=Pos(DecimalSeparator,S);
  If (P<>0) Then
    S[P] := '.';
  Val(S,Value,E);
  Result:=(E=0);
End;

Function TextToFloat(Buffer: PChar; Var Value; ValueType: TFloatValue): Boolean;

Var
  E,P : Integer;
  S : String;
  C : Currency;
  Ext : Extended;

Begin
  S:=StrPas(Buffer);
  P:=Pos(DecimalSeparator,S);
  If (P<>0) Then
    S[P] := '.';
  case ValueType of
    fvCurrency:
      Val(S,Currency(Value),E);
    fvExtended:
      Val(S,Extended(Value),E);
  end;
  Result:=(E=0);
End;

Function FloatToStr(Value: Extended): String;
Begin
  Result := FloatToStrF(Value, ffGeneral, 15, 0);
End;

Function FloatToText(Buffer: PChar; Value: Extended; format: TFloatFormat; Precision, Digits: Integer): Longint;
Var
  Tmp: String[40];
Begin
  Tmp := FloatToStrF(Value, format, Precision, Digits);
  Result := Length(Tmp);
  Move(Tmp[1], Buffer[0], Result);
End;


Function FloatToStrF(Value: Extended; format: TFloatFormat; Precision, Digits: Integer): String;
Var
  P: Integer;
  Negative, TooSmall, TooLarge: Boolean;


Begin
  Case format Of

    ffGeneral:

      Begin
        If (Precision = -1) Or (Precision > 15) Then Precision := 15;
        TooSmall := (Abs(Value) < 0.00001) and (Value>0.0);
        If Not TooSmall Then
        Begin
          Str(Value:0:999, Result);
          P := Pos('.', Result);
          Result[P] := DecimalSeparator;
          TooLarge := P > Precision + 1;
        End;

        If TooSmall Or TooLarge Then
          begin
          Result := FloatToStrF(Value, ffExponent, Precision, Digits);
          // Strip unneeded zeroes.
          P:=Pos('E',result)-1;
          If P<>-1 then
             While (P>1) and (Result[P]='0') do
               begin
               system.Delete(Result,P,1);
               Dec(P);
               end;
          end
        else
          begin
          P := Length(Result);
          While Result[P] = '0' Do Dec(P);
          If Result[P] = DecimalSeparator Then Dec(P);
          SetLength(Result, P);
          end;
      End;

    ffExponent:

      Begin
        If (Precision = -1) Or (Precision > 15) Then Precision := 15;
        Str(Value:Precision + 8, Result);
        Result[3] := DecimalSeparator;
        P:=4;
        While (P>0) and (Digits < P) And (Result[Precision + 5] = '0') do
          Begin
          If P<>1 then
            system.Delete(Result, Precision + 5, 1)
          else
            system.Delete(Result, Precision + 3, 3);
          Dec(P);
          end;
        If Result[1] = ' ' Then
          System.Delete(Result, 1, 1);
      End;

    ffFixed:

      Begin
        If Digits = -1 Then Digits := 2
        Else If Digits > 15 Then Digits := 15;
        Str(Value:0:Digits, Result);
        If Result[1] = ' ' Then
          System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 Then Result[P] := DecimalSeparator;
      End;

    ffNumber:

      Begin
        If Digits = -1 Then Digits := 2
        Else If Digits > 15 Then Digits := 15;
        Str(Value:0:Digits, Result);
        If Result[1] = ' ' Then System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 Then
          Result[P] := DecimalSeparator
        else
          P := Length(Result)+1;
        Dec(P, 3);
        While (P > 1) Do
        Begin
          If Result[P - 1] <> '-' Then Insert(ThousandSeparator, Result, P);
          Dec(P, 3);
        End;
      End;

    ffCurrency:

      Begin
        If Value < 0 Then
        Begin
          Negative := True;
          Value := -Value;
        End
        Else Negative := False;

        If Digits = -1 Then Digits := CurrencyDecimals
        Else If Digits > 18 Then Digits := 18;
        Str(Value:0:Digits, Result);
        If Result[1] = ' ' Then System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 Then Result[P] := DecimalSeparator;
        Dec(P, 3);
        While (P > 1) Do
        Begin
          Insert(ThousandSeparator, Result, P);
          Dec(P, 3);
        End;

        If Not Negative Then
        Begin
          Case CurrencyFormat Of
            0: Result := CurrencyString + Result;
            1: Result := Result + CurrencyString;
            2: Result := CurrencyString + ' ' + Result;
            3: Result := Result + ' ' + CurrencyString;
          End
        End
        Else
        Begin
          Case NegCurrFormat Of
            0: Result := '(' + CurrencyString + Result + ')';
            1: Result := '-' + CurrencyString + Result;
            2: Result := CurrencyString + '-' + Result;
            3: Result := CurrencyString + Result + '-';
            4: Result := '(' + Result + CurrencyString + ')';
            5: Result := '-' + Result + CurrencyString;
            6: Result := Result + '-' + CurrencyString;
            7: Result := Result + CurrencyString + '-';
            8: Result := '-' + Result + ' ' + CurrencyString;
            9: Result := '-' + CurrencyString + ' ' + Result;
            10: Result := CurrencyString + ' ' + Result + '-';
          End;
        End;
      End;
  End;
End;

Function FloatToDateTime (Const Value : Extended) : TDateTime;
begin
  If (Value<MinDateTime) or (Value>MaxDateTime) then
    Raise EConvertError.CreateFmt (SInvalidDateTime,[Value]);
  Result:=Value;
end;

Function FloatToCurr (Const Value : Extended) : Currency;
begin

end;

Function CurrToStr(Value: Currency): string;
begin
end;

function StrToCurr(const S: string): Currency;
begin
end;

function StrToBool(const S: string): Boolean;

Var
  Temp : String;
  D : Double;
  Code : word;

begin
  Temp:=upcase(S);
  Val(temp,D,code);
  If Code=0 then
    Result:=(D<>0.0)
  else If Temp='TRUE' then
    result:=true
  else if Temp='FALSE' then
    result:=false
  else
    Raise EConvertError.CreateFmt(SInvalidBoolean,[S]);
end;

function BoolToStr(B: Boolean): string;
begin
  If B then
    Result:='TRUE'
  else
    Result:='FALSE';
end;

Function FloatToTextFmt(Buffer: PChar; Value: Extended; format: PChar): Integer;

Var
  Digits: String[40];                         { String Of Digits                 }
  Exponent: String[8];                        { Exponent strin                   }
  FmtStart, FmtStop: PChar;                   { Start And End Of relevant part   }
                                              { Of format String                 }
  ExpFmt, ExpSize: Integer;                   { Type And Length Of               }
                                              { exponential format chosen        }
  Placehold: Array[1..4] Of Integer;          { Number Of placeholders In All    }
                                              { four Sections                    }
  thousand: Boolean;                          { thousand separators?             }
  UnexpectedDigits: Integer;                  { Number Of unexpected Digits that }
                                              { have To be inserted before the   }
                                              { First placeholder.               }
  DigitExponent: Integer;                     { Exponent Of First digit In       }
                                              { Digits Array.                    }

  { Find end of format section starting at P. False, if empty }

  Function GetSectionEnd(Var P: PChar): Boolean;
  Var
    C: Char;
    SQ, DQ: Boolean;
  Begin
    Result := False;
    SQ := False;
    DQ := False;
    C := P[0];
    While (C<>#0) And ((C<>';') Or SQ Or DQ) Do
      Begin
      Result := True;
      Case C Of
        #34: If Not SQ Then DQ := Not DQ;
        #39: If Not DQ Then SQ := Not SQ;
      End;
      Inc(P);
      C := P[0];
      End;
  End;

  { Find start and end of format section to apply. If section doesn't exist,
    use section 1. If section 2 is used, the sign of value is ignored.       }

  Procedure GetSectionRange(section: Integer);
  Var
    Sec: Array[1..3] Of PChar;
    SecOk: Array[1..3] Of Boolean;
  Begin
    Sec[1] := format;
    SecOk[1] := GetSectionEnd(Sec[1]);
    If section > 1 Then
      Begin
      Sec[2] := Sec[1];
      If Sec[2][0] <> #0 Then
        Inc(Sec[2]);
      SecOk[2] := GetSectionEnd(Sec[2]);
      If section > 2 Then
        Begin
        Sec[3] := Sec[2];
        If Sec[3][0] <> #0 Then
          Inc(Sec[3]);
        SecOk[3] := GetSectionEnd(Sec[3]);
        End;
      End;
    If Not SecOk[1] Then
      FmtStart := Nil
    Else
      Begin
      If Not SecOk[section] Then
        section := 1
      Else If section = 2 Then
        Value := -Value;   { Remove sign }
      If section = 1 Then FmtStart := format Else
        Begin
        FmtStart := Sec[section - 1];
        Inc(FmtStart);
        End;
      FmtStop := Sec[section];
      End;
  End;

  { Find format section ranging from FmtStart to FmtStop. }

  Procedure GetFormatOptions;
  Var
    Fmt: PChar;
    SQ, DQ: Boolean;
    area: Integer;
  Begin
    SQ := False;
    DQ := False;
    Fmt := FmtStart;
    ExpFmt := 0;
    area := 1;
    thousand := False;
    Placehold[1] := 0;
    Placehold[2] := 0;
    Placehold[3] := 0;
    Placehold[4] := 0;
    While Fmt < FmtStop Do
      Begin
      Case Fmt[0] Of
        #34:
          Begin
          If Not SQ Then
            DQ := Not DQ;
          Inc(Fmt);
          End;
        #39:
          Begin
          If Not DQ Then
            SQ := Not SQ;
          Inc(Fmt);
          End;
      Else
        { This was 'if not SQ or DQ'. Looked wrong... }
        If Not SQ Or DQ Then
          Begin
          Case Fmt[0] Of
            '0':
              Begin
              Case area Of
                1:
                  area := 2;
                4:
                  Begin
                  area := 3;
                  Inc(Placehold[3], Placehold[4]);
                  Placehold[4] := 0;
                  End;
              End;
              Inc(Placehold[area]);
              Inc(Fmt);
              End;

            '#':
              Begin
              If area=3 Then
                area:=4;
              Inc(Placehold[area]);
              Inc(Fmt);
              End;
            '.':
              Begin
              If area<3 Then
                area:=3;
              Inc(Fmt);
              End;
            ',':
              Begin
              thousand := True;
              Inc(Fmt);
              End;
            'e', 'E':
              If ExpFmt = 0 Then
                Begin
                If (Fmt[0]='E') Then
                  ExpFmt:=1
                Else
                  ExpFmt := 3;
                Inc(Fmt);
                If (Fmt<FmtStop) Then
                  Begin
                  Case Fmt[0] Of
                    '+':
                      Begin
                      End;
                    '-':
                      Inc(ExpFmt);
                  Else
                    ExpFmt := 0;
                  End;
                  If ExpFmt <> 0 Then
                    Begin
                    Inc(Fmt);
                    ExpSize := 0;
                    While (Fmt<FmtStop) And
                          (ExpSize<4) And
                          (Fmt[0] In ['0'..'9']) Do
                      Begin
                      Inc(ExpSize);
                      Inc(Fmt);
                      End;
                    End;
                  End;
                End
              Else
                Inc(Fmt);
          Else { Case }
            Inc(Fmt);
          End; { Case }
          End; { Begin }
      End; { Case }
      End; { While .. Begin }
  End;

  Procedure FloatToStr;

  Var
    I, J, Exp, Width, Decimals, DecimalPoint, len: Integer;

  Begin
    If ExpFmt = 0 Then
      Begin
      { Fixpoint }
      Decimals:=Placehold[3]+Placehold[4];
      Width:=Placehold[1]+Placehold[2]+Decimals;
      If (Decimals=0) Then
        Str(Value:Width:0,Digits)
      Else
        Str(Value:Width+1:Decimals,Digits);
      len:=Length(Digits);
      { Find the decimal point }
      If (Decimals=0) Then
        DecimalPoint:=len+1
      Else
        DecimalPoint:=len-Decimals;
      { If value is very small, and no decimal places
        are desired, remove the leading 0.            }
      If (Abs(Value) < 1) And (Placehold[2] = 0) Then
        Begin
        If (Placehold[1]=0) Then
          Delete(Digits,DecimalPoint-1,1)
        Else
          Digits[DecimalPoint-1]:=' ';
        End;

      { Convert optional zeroes to spaces. }
      I:=len;
      J:=DecimalPoint+Placehold[3];
      While (I>J) And (Digits[I]='0') Do
        Begin
        Digits[I] := ' ';
        Dec(I);
        End;
      { If integer value and no obligatory decimal
        places, remove decimal point. }
      If (DecimalPoint < len) And (Digits[DecimalPoint + 1] = ' ') Then
          Digits[DecimalPoint] := ' ';
      { Convert spaces left from obligatory decimal point to zeroes. }
      I:=DecimalPoint-Placehold[2];
      While (I<DecimalPoint) And (Digits[I]=' ') Do
        Begin
        Digits[I] := '0';
        Inc(I);
        End;
      Exp := 0;
      End
    Else
      Begin
      { Scientific: exactly <Width> Digits With <Precision> Decimals
        And adjusted Exponent. }
      If Placehold[1]+Placehold[2]=0 Then
        Placehold[1]:=1;
      Decimals := Placehold[3] + Placehold[4];
      Width:=Placehold[1]+Placehold[2]+Decimals;
      Str(Value:Width+8,Digits);
      { Find and cut out exponent. Always the
        last 6 characters in the string.
        -> 0000E+0000                         }
      I:=Length(Digits)-5;
      Val(Copy(Digits,I+1,5),Exp,J);
      Exp:=Exp+1-(Placehold[1]+Placehold[2]);
      Delete(Digits, I, 6);
      { Str() always returns at least one digit after the decimal point.
        If we don't want it, we have to remove it. }
      If (Decimals=0) And (Placehold[1]+Placehold[2]<= 1) Then
        Begin
        If (Digits[4]>='5') Then
          Begin
          Inc(Digits[2]);
          If (Digits[2]>'9') Then
            Begin
            Digits[2] := '1';
            Inc(Exp);
            End;
          End;
        Delete(Digits, 3, 2);
        DecimalPoint := Length(Digits) + 1;
        End
      Else
        Begin
        { Move decimal point at the desired position }
        Delete(Digits, 3, 1);
        DecimalPoint:=2+Placehold[1]+Placehold[2];
        If (Decimals<>0) Then
          Insert('.',Digits,DecimalPoint);
        End;

      { Convert optional zeroes to spaces. }
      I := Length(Digits);
      J := DecimalPoint + Placehold[3];
      While (I > J) And (Digits[I] = '0') Do
        Begin
        Digits[I] := ' ';
        Dec(I);
        End;

      { If integer number and no obligatory decimal paces, remove decimal point }

      If (DecimalPoint<Length(Digits)) And
         (Digits[DecimalPoint+1]=' ') Then
          Digits[DecimalPoint]:=' ';
      If (Digits[1]=' ') Then
        Begin
        Delete(Digits, 1, 1);
        Dec(DecimalPoint);
        End;
      { Calculate exponent string }
      Str(Abs(Exp), Exponent);
      While Length(Exponent)<ExpSize Do
        Insert('0',Exponent,1);
      If Exp >= 0 Then
        Begin
        If (ExpFmt In [1,3]) Then
          Insert('+', Exponent, 1);
        End
      Else
        Insert('-',Exponent,1);
      If (ExpFmt<3) Then
        Insert('E',Exponent,1)
      Else
        Insert('e',Exponent,1);
      End;
    DigitExponent:=DecimalPoint-2;
    If (Digits[1]='-') Then
      Dec(DigitExponent);
    UnexpectedDigits:=DecimalPoint-1-(Placehold[1]+Placehold[2]);
  End;

  Function PutResult: LongInt;

  Var
    SQ, DQ: Boolean;
    Fmt, Buf: PChar;
    Dig, N: Integer;

  Begin
    SQ := False;
    DQ := False;
    Fmt := FmtStart;
    Buf := Buffer;
    Dig := 1;
    While (Fmt<FmtStop) Do
      Begin
      //Write(Fmt[0]);
      Case Fmt[0] Of
        #34:
          Begin
          If Not SQ Then
            DQ := Not DQ;
          Inc(Fmt);
          End;
        #39:
          Begin
          If Not DQ Then
            SQ := Not SQ;
          Inc(Fmt);
          End;
      Else
        If Not (SQ Or DQ) Then
          Begin
          Case Fmt[0] Of
            '0', '#', '.':
              Begin
              If (Dig=1) And (UnexpectedDigits>0) Then
                Begin
                { Everything unexpected is written before the first digit }
                For N := 1 To UnexpectedDigits Do
                  Begin
                  Buf[0] := Digits[N];
                  Inc(Buf);
                  If thousand And (Digits[N]<>'-') Then
                    Begin
                    If (DigitExponent Mod 3 = 0) And (DigitExponent>0) Then
                      Begin
                      Buf[0] := ThousandSeparator;
                      Inc(Buf);
                      End;
                    Dec(DigitExponent);
                    End;
                  End;
                Inc(Dig, UnexpectedDigits);
                End;
              If (Digits[Dig]<>' ') Then
                Begin
                If (Digits[Dig]='.') Then
                  Buf[0] := DecimalSeparator
                Else
                  Buf[0] := Digits[Dig];
                Inc(Buf);
                If thousand And (DigitExponent Mod 3 = 0) And (DigitExponent > 0) Then
                  Begin
                  Buf[0] := ThousandSeparator;
                  Inc(Buf);
                  End;
                End;
              Inc(Dig);
              Dec(DigitExponent);
              Inc(Fmt);
              End;
            'e', 'E':
              Begin
              If ExpFmt <> 0 Then
                Begin
                Inc(Fmt);
                If Fmt < FmtStop Then
                  Begin
                  If Fmt[0] In ['+', '-'] Then
                    Begin
                    Inc(Fmt, ExpSize);
                    For N:=1 To Length(Exponent) Do
                      Buf[N-1] := Exponent[N];
                    Inc(Buf,Length(Exponent));
                    ExpFmt:=0;
                    End;
                  Inc(Fmt);
                  End;
                End
              Else
                Begin
                { No legal exponential format.
                  Simply write the 'E' to the result. }
                Buf[0] := Fmt[0];
                Inc(Buf);
                Inc(Fmt);
                End;
              End;
          Else { Case }
            { Usual character }
            If (Fmt[0]<>',') Then
              Begin
              Buf[0] := Fmt[0];
              Inc(Buf);
              End;
            Inc(Fmt);
          End; { Case }
          End
        Else { IF }
          Begin
          { Character inside single or double quotes }
          Buf[0] := Fmt[0];
          Inc(Buf);
          Inc(Fmt);
          End;
      End; { Case }
    End; { While .. Begin }
    Result:=LongInt(Buf)-LongInt(Buffer);
  End;

Begin
  If (Value>0) Then
    GetSectionRange(1)
  Else If (Value<0) Then
    GetSectionRange(2)
  Else
    GetSectionRange(3);
  If FmtStart = Nil Then
    Begin
    Result := FloatToText(Buffer, Value, ffGeneral, 15, 4);
    End
  Else
    Begin
    GetFormatOptions;
    If (ExpFmt = 0) And (Abs(Value) >= 1E18) Then
      Result := FloatToText(Buffer, Value, ffGeneral, 15, 4)
    Else
      Begin
      FloatToStr;
      Result := PutResult;
      End;
    End;
End;



Procedure FloatToDecimal(Var Result: TFloatRec; Value: Extended; Precision, Decimals : integer);

Var
  Buffer: String[24];
  Error, N: Integer;

Begin
  Str(Value:23, Buffer);
  Result.Negative := (Buffer[1] = '-');
  Val(Copy(Buffer, 19, 5), Result.Exponent, Error);
  Inc(Result. Exponent);
  Result.Digits[0] := Buffer[2];
  Move(Buffer[4], Result.Digits[1], 14);
  If Decimals + Result.Exponent < Precision Then
    N := Decimals + Result.Exponent
  Else
    N := Precision;
  If N > 15 Then
    N := 15;
  If N = 0 Then
    Begin
    If Result.Digits[0] >= '5' Then
      Begin
      Result.Digits[0] := '1';
      Result.Digits[1] := #0;
      Inc(Result.Exponent);
      End
    Else
      Result.Digits[0] := #0;
    End
  Else If N > 0 Then
    Begin
    If Result.Digits[N] >= '5' Then
      Begin
      Repeat
        Result.Digits[N] := #0;
        Dec(N);
        Inc(Result.Digits[N]);
      Until (N = 0) Or (Result.Digits[N] < ':');
      If Result.Digits[0] = ':' Then
        Begin
        Result.Digits[0] := '1';
        Inc(Result.Exponent);
        End;
      End
    Else
      Begin
      Result.Digits[N] := '0';
      While (Result.Digits[N] = '0') And (N > -1) Do
        Begin
        Result.Digits[N] := #0;
        Dec(N);
        End;
      End;
    End
  Else
    Result.Digits[0] := #0;
  If Result.Digits[0] = #0 Then
    Begin
    Result.Exponent := 0;
    Result.Negative := False;
    End;
End;

Function FormatFloat(Const format: String; Value: Extended): String;

Var
  Temp: ShortString;
  buf : Array[0..1024] of char;

Begin
  Buf[FloatToTextFmt(@Buf[0],Value,Pchar(Format))]:=#0;
  Result:=StrPas(@Buf);
End;



{==============================================================================}
{   extra functions                                                            }
{==============================================================================}

{   LeftStr returns Count left-most characters from S }

function LeftStr(const S: string; Count: integer): string;
begin
  result := Copy(S, 1, Count);
end ;

{ RightStr returns Count right-most characters from S }

function RightStr(const S: string; Count: integer): string;
begin
   If Count>Length(S) then
     Count:=Length(S);
   result := Copy(S, 1 + Length(S) - Count, Count);
end;

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

Function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;

begin
  Result:=False;
  If Index<=Length(S) then
    Result:=Pos(S[Index],Delimiters)<>0; // Note we don't do MBCS yet
end;



Function LastDelimiter(const Delimiters, S: string): Integer;

begin
  Result:=Length(S);
  While (Result>0) and (Pos(S[Result],Delimiters)=0) do
    Dec(Result);
end;

function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;

var
  Srch,OldP,RemS: string; // Srch and Oldp can contain uppercase versions of S,OldPattern
  P : Integer;

begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then
    begin
    Srch:=UpperCase(Srch);
    OldP:=UpperCase(OldP);
    end;
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
    begin
    P:=Pos(OldP, Srch);
    if P=0 then
      begin
      Result:=Result+RemS;
      Srch:='';
      end
    else
      begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
        begin
        Result:=Result+RemS;
        Srch:='';
        end
      else
         Srch:=Copy(Srch,P,Length(Srch)-P+1);
      end;
    end;
end;


{
   Case Translation Tables
   Can be used in internationalization support.

   Although these tables can be obtained through system calls
   it is better to not use those, since most implementation are not 100%
   WARNING:
   before modifying a translation table make sure that the current codepage
   of the OS corresponds to the one you make changes to
}



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

{
  $Log$
  Revision 1.24  2002-12-23 23:12:34  florian
    + TextToFloat: D6 compatibility added

  Revision 1.23  2002/11/28 22:26:30  michael
  + Fixed float<>string conversion routines

  Revision 1.22  2002/11/28 20:29:26  michael
  + made it compile again

  Revision 1.21  2002/11/28 20:15:37  michael
  + Fixed comparestr (merge from fix)

  Revision 1.20  2002/09/15 17:50:35  peter
    * Fixed AnsiStrComp crashes
  Revision 1.1.2.16  2002/11/28 22:25:01  michael
  + Fixed float<>string conversion routines

  Revision 1.1.2.15  2002/11/28 20:24:11  michael
  + merged some fixes from mainbranch

  Revision 1.19  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed
  Revision 1.1.2.14  2002/11/28 20:13:10  michael
  + Fixed comparestr

  Revision 1.1.2.13  2002/10/29 23:41:06  michael
  + Added lots of D4 functions

  Revision 1.18  2002/09/02 06:07:16  michael
  + Fix for formatbuf not applied correct

  Revision 1.17  2002/08/29 10:04:48  michael
  + Fix for bug report 2097 in formatbuf

  Revision 1.16  2002/08/29 10:04:25  michael
  + Fix for bug report 2097 in formatbuf

  Revision 1.15  2002/07/06 12:14:03  daniel
  - Changes from Strasbourg

  Revision 1.14  2002/01/24 12:33:53  jonas
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