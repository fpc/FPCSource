{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                    subroutines for string handling
****************************************************************************}

{$I real2str.inc}

function copy(const s : shortstring;index : StrLenInt;count : StrLenInt): shortstring;
begin
  if count<0 then
   count:=0;
  if index>1 then
   dec(index)
  else
   index:=0;
  if index>length(s) then
   count:=0
  else
   if index+count>length(s) then
    count:=length(s)-index;
  Copy[0]:=chr(Count);
  Move(s[Index+1],Copy[1],Count);
end;


procedure delete(var s : shortstring;index : StrLenInt;count : StrLenInt);
begin
  if index<=0 then
   begin
     inc(count,index-1);
     index:=1;
   end;
  if (Index<=Length(s)) and (Count>0) then
   begin
     if Count+Index>length(s) then
      Count:=length(s)-Index+1;
     s[0]:=Chr(length(s)-Count);
     if Index<=Length(s) then
      Move(s[Index+Count],s[Index],Length(s)-Index+1);
   end;
end;


procedure insert(const source : shortstring;var s : shortstring;index : StrLenInt);
var
  cut,srclen,indexlen : longint;
begin
  if index<1 then
   index:=1;
  if index>length(s) then
   index:=length(s)+1;
  indexlen:=Length(s)-Index+1;
  srclen:=length(Source);
  if length(source)+length(s)>=sizeof(s) then
   begin
     cut:=length(source)+length(s)-sizeof(s)+1;
     if cut>indexlen then
      begin
        dec(srclen,cut-indexlen);
        indexlen:=0;
      end
     else
      dec(indexlen,cut);
   end;
  move(s[Index],s[Index+srclen],indexlen);
  move(Source[1],s[Index],srclen);
  s[0]:=chr(index+srclen+indexlen-1);
end;


procedure insert(source : Char;var s : shortstring;index : StrLenInt);
var
  indexlen : longint;
begin
  if index<1 then
   index:=1;
  if index>length(s) then
   index:=length(s)+1;
  indexlen:=Length(s)-Index+1;
  if (length(s)+1=sizeof(s)) and (indexlen>0) then
   dec(indexlen);
  move(s[Index],s[Index+1],indexlen);
  s[Index]:=Source;
  s[0]:=chr(index+indexlen);
end;


function pos(const substr : shortstring;const s : shortstring):StrLenInt;
var
  i,j : StrLenInt;
  e   : boolean;
begin
  i := 0;
  j := 0;
  e:=(length(SubStr)>0);
  while e and (i<=Length(s)-Length(SubStr)) do
   begin
     inc(i);
     if (SubStr[1]=s[i]) and (Substr=Copy(s,i,Length(SubStr))) then
      begin
        j:=i;
        e:=false;
      end;
   end;
  Pos:=j;
end;


{Faster when looking for a single char...}
function pos(c:char;const s:shortstring):StrLenInt;
var
  i : StrLenInt;
begin
  for i:=1 to length(s) do
   if s[i]=c then
    begin
      pos:=i;
      exit;
    end;
  pos:=0;
end;


procedure SetLength(var s:shortstring;len:StrLenInt);
begin
  if Len>255 then
   Len:=255;
  s[0]:=chr(len);
end;


function copy(c:char;index : StrLenInt;count : StrLenInt): shortstring;
begin
  if (index=1) and (Count>0) then
   Copy:=c
  else
   Copy:='';
end;


function pos(const substr : shortstring;c:char): StrLenInt;
begin
  if (length(substr)=1) and (substr[1]=c) then
   Pos:=1
  else
   Pos:=0;
end;


{ removed must be internal to be accepted in const expr !! PM
function length(c:char):StrLenInt;
begin
  Length:=1;
end;
}

{$ifdef IBM_CHAR_SET}
const
  UpCaseTbl : shortstring[7]=#154#142#153#144#128#143#165;
  LoCaseTbl : shortstring[7]=#129#132#148#130#135#134#164;
{$endif}

function upcase(c : char) : char;
{$IFDEF IBM_CHAR_SET}
var
  i : longint;
{$ENDIF}
begin
  if (c in ['a'..'z']) then
    upcase:=char(byte(c)-32)
  else
{$IFDEF IBM_CHAR_SET}
    begin
      i:=Pos(c,LoCaseTbl);
      if i>0 then
       upcase:=UpCaseTbl[i]
      else
       upcase:=c;
    end;
{$ELSE}
   upcase:=c;
{$ENDIF}
end;


function upcase(const s : shortstring) : shortstring;
var
  i : longint;
begin
  upcase[0]:=s[0];
  for i := 1 to length (s) do
    upcase[i] := upcase (s[i]);
end;


{$ifndef RTLLITE}

function lowercase(c : char) : char;
{$IFDEF IBM_CHAR_SET}
var
  i : longint;
{$ENDIF}
begin
  if (c in ['A'..'Z']) then
   lowercase:=char(byte(c)+32)
  else
{$IFDEF IBM_CHAR_SET}
   begin
     i:=Pos(c,UpCaseTbl);
     if i>0 then
      lowercase:=LoCaseTbl[i]
     else
      lowercase:=c;
   end;
 {$ELSE}
   lowercase:=c;
 {$ENDIF}
end;


function lowercase(const s : shortstring) : shortstring;
var
  i : longint;
begin
  lowercase [0]:=s[0];
  for i:=1 to length(s) do
   lowercase[i]:=lowercase (s[i]);
end;


function hexstr(val : longint;cnt : byte) : shortstring;
const
  HexTbl : array[0..15] of char='0123456789ABCDEF';
var
  i : longint;
begin
  hexstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     hexstr[i]:=hextbl[val and $f];
     val:=val shr 4;
   end;
end;


function binstr(val : longint;cnt : byte) : shortstring;
var
  i : longint;
begin
  binstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     binstr[i]:=char(48+val and 1);
     val:=val shr 1;
   end;
end;

{$endif RTLLITE}


function space (b : byte): shortstring;
begin
  space[0] := chr(b);
  FillChar (Space[1],b,' ');
end;


{*****************************************************************************
                              Str() Helpers
*****************************************************************************}

procedure ShortStr_Float(d : ValReal;len,fr,rt : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_FLOAT'];
begin
  str_real(len,fr,d,treal_type(rt),s);
end;


procedure int_str_longint(v : longint;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_LONGINT'];
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;


procedure int_str_cardinal(v : cardinal;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_CARDINAL'];
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;


{*****************************************************************************
                           Val() Functions
*****************************************************************************}

Function InitVal(const s:shortstring;var negativ:boolean;var base:byte):ValSInt;
var
  Code : Longint;
begin
{Skip Spaces and Tab}
  code:=1;
  while (code<=length(s)) and (s[code] in [' ',#9]) do
   inc(code);
{Sign}
  negativ:=false;
  case s[code] of
   '-' : begin
           negativ:=true;
           inc(code);
         end;
   '+' : inc(code);
  end;
{Base}
  base:=10;
  if code<=length(s) then
   begin
     case s[code] of
      '$' : begin
              base:=16;
              repeat
                inc(code);
              until (code>=length(s)) or (s[code]<>'0');
            end;
      '%' : begin
              base:=2;
              inc(code);
            end;
     end;
  end;
  InitVal:=code;
end;


Function ValSignedInt(DestSize: longint; Const S: ShortString; var Code: ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_SHORTSTR'];
var
  u, temp, prev: ValUInt;
  base : byte;
  negative : boolean;
begin
  ValSignedInt := 0;
  Temp:=0;
  Code:=InitVal(s,negative,base);
  if Code>length(s) then
   exit;
  if negative and (s='-2147483648') then
   begin
     Code:=0;
     ValSignedInt:=$80000000;
     exit;
   end;

  while Code<=Length(s) do
   begin
     case s[Code] of
       '0'..'9' : u:=Ord(S[Code])-Ord('0');
       'A'..'F' : u:=Ord(S[Code])-(Ord('A')-10);
       'a'..'f' : u:=Ord(S[Code])-(Ord('a')-10);
     else
      u:=16;
     end;
     Prev := Temp;
     Temp := Temp*ValUInt(base);
     If ((base = 10) and
         (prev > MaxSIntValue div ValUInt(Base))) or
        (Temp < prev) Then
       Begin
         ValSignedInt := 0;
         Exit
       End;
     if (u>=base) or
        ((base = 10) and
         (MaxSIntValue < u+temp)) or
        ((base <> 10) and
         (ValUInt(MaxUIntValue-Temp) < u)) then
       begin
         ValSignedInt:=0;
         exit;
       end;
     Temp:=Temp+u;
     inc(code);
   end;
  code := 0;
  ValSignedInt := ValSInt(Temp);
  If Negative Then
    ValSignedInt := -ValSignedInt;
  If Not(Negative) and (base <> 10) Then
   {sign extend the result to allow proper range checking}
    Case DestSize of
      1: If (ValSignedInt > High(ShortInt)) and (ValSignedInt <= High(Byte)) Then
           ValSignedInt := ValSignedInt or (MaxUIntValue xor High(Byte));
      2: If (ValSignedInt > High(Integer)) and (ValSignedInt <= High(Word)) Then
           ValSignedInt := ValSignedInt or (MaxUIntValue xor High(Word));
{     Uncomment the folling once full 64bit support is in place
      4: If (ValSignedInt > High(Longint)) and (ValSignedInt <= High(Cardinal)) Then
           ValSignedInt := ValSignedInt or (MaxUIntValue xor High(Cardinal));}
    End;
end;


Function ValUnsignedInt(Const S: ShortString; var Code: ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_SHORTSTR'];
var
  u, prev: ValUInt;
  base : byte;
  negative : boolean;
begin
  ValUnSignedInt:=0;
  Code:=InitVal(s,negative,base);
  If Negative or (Code>length(s)) Then
    Exit;
  while Code<=Length(s) do
   begin
     case s[Code] of
       '0'..'9' : u:=Ord(S[Code])-Ord('0');
       'A'..'F' : u:=Ord(S[Code])-(Ord('A')-10);
       'a'..'f' : u:=Ord(S[Code])-(Ord('a')-10);
     else
      u:=16;
     end;
     prev := ValUnsignedInt;
     ValUnsignedInt:=ValUnsignedInt*ValUInt(base);
     If prev > ValUnsignedInt Then
      {we've had an overflow. Can't check this with
       "If ValUnsignedInt <= (MaxUIntValue div ValUInt(Base)) Then"
       because this division always overflows! (JM)}
       Begin
         ValUnsignedInt := 0;
         Exit
       End;
     if (u>=base) or (ValUInt(MaxUIntValue-ValUnsignedInt) < u) then
      begin
        ValUnsignedInt:=0;
        exit;
      end;
     ValUnsignedInt:=ValUnsignedInt+u;
     inc(code);
   end;
  code := 0;
end;


Function ValFloat(const s : shortstring; var code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_SHORTSTR'];
var
  hd,
  esign,sign : valreal;
  exponent,i : longint;
  flags      : byte;
begin
  ValFloat:=0.0;
  code:=1;
  exponent:=0;
  esign:=1;
  flags:=0;
  sign:=1;
  while (code<=length(s)) and (s[code] in [' ',#9]) do
   inc(code);
  case s[code] of
   '+' : inc(code);
   '-' : begin
           sign:=-1;
           inc(code);
         end;
  end;
  while (Code<=Length(s)) and (s[code] in ['0'..'9']) do
   begin
   { Read integer part }
      flags:=flags or 1;
      valfloat:=valfloat*10+(ord(s[code])-ord('0'));
      inc(code);
   end;
{ Decimal ? }
  if (s[code]='.') and (length(s)>=code) then
   begin
      hd:=1.0;
      inc(code);
      while (s[code] in ['0'..'9']) and (length(s)>=code) do
        begin
           { Read fractional part. }
           flags:=flags or 2;
           valfloat:=valfloat*10+(ord(s[code])-ord('0'));
           hd:=hd*10.0;
           inc(code);
        end;
      valfloat:=valfloat/hd;
   end;
 { Again, read integer and fractional part}
  if flags=0 then
   begin
      valfloat:=0.0;
      exit;
   end;
 { Exponent ? }
  if (upcase(s[code])='E') and (length(s)>=code) then
   begin
      inc(code);
      if s[code]='+' then
        inc(code)
      else
        if s[code]='-' then
         begin
           esign:=-1;
           inc(code);
         end;
      if not(s[code] in ['0'..'9']) or (length(s)<code) then
        begin
           valfloat:=0.0;
           exit;
        end;
      while (s[code] in ['0'..'9']) and (length(s)>=code) do
        begin
           exponent:=exponent*10;
           exponent:=exponent+ord(s[code])-ord('0');
           inc(code);
        end;
   end;
{ Calculate Exponent }
{
  if esign>0 then
    for i:=1 to exponent do
      valfloat:=valfloat*10
    else
      for i:=1 to exponent do
        valfloat:=valfloat/10; }
  hd:=1.0;
  for i:=1 to exponent do
    hd:=hd*10.0;
  if esign>0 then
    valfloat:=valfloat*hd
  else
    valfloat:=valfloat/hd;
{ Not all characters are read ? }
  if length(s)>=code then
   begin
     valfloat:=0.0;
     exit;
   end;
{ evaluate sign }
  valfloat:=valfloat*sign;
{ success ! }
  code:=0;
end;


{$ifdef SUPPORT_FIXED}
Function ValFixed(const s : shortstring;var code : ValSInt): Fixed; [public, alias:'FPC_VAL_FIXED_SHORTSTR'];
begin
  ValFixed := Fixed(ValFloat(s,code));
end;
{$endif SUPPORT_FIXED}


Procedure SetString (Var S : Shortstring; Buf : PChar; Len : Longint);
begin
  Move (Buf[0],S[1],Len);
  S[0]:=chr(len);
end;

{
  $Log$
  Revision 1.1  2000-07-13 06:30:48  michael
  + Initial import

  Revision 1.35  2000/04/06 11:51:47  pierre
   * fix for extended constants

  Revision 1.34  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.33  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.32  2000/01/07 16:32:25  daniel
    * copyright 2000 added

  Revision 1.31  1999/12/11 19:07:44  jonas
    * avoid unwanted type conversion from cardinal to longint in val for
      signed and unsigned 32bit int

  Revision 1.30  1999/11/06 14:35:39  peter
    * truncated log

}