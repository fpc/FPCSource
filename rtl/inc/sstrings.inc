{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

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

procedure int_str_real(d : real;len,fr : longint;var s : shortstring);[public,alias:'FPC_'+{$ifdef NOSTRANSI}'SHORT'+{$endif}'STR_REAL'];
begin
{$ifdef i386}
   str_real(len,fr,d,rt_s64real,s);
{$else}
   str_real(len,fr,d,rt_s32real,s);
{$endif}
end;


{$ifdef SUPPORT_SINGLE}
procedure int_str_single(d : single;len,fr : longint;var s : shortstring);[public,alias:'FPC_'+{$ifdef NOSTRANSI}'SHORT'+{$endif}'STR_SINGLE'];
begin
   str_real(len,fr,d,rt_s32real,s);
end;
{$endif SUPPORT_SINGLE}


{$ifdef SUPPORT_EXTENDED}
procedure int_str_extended(d : extended;len,fr : longint;var s : shortstring);[public,alias:'FPC_'+{$ifdef NOSTRANSI}'SHORT'+{$endif}'STR_EXTENDED'];
begin
   str_real(len,fr,d,rt_s80real,s);
end;
{$endif SUPPORT_EXTENDED}


{$ifdef SUPPORT_COMP}
procedure int_str_comp(d : comp;len,fr : longint;var s : shortstring);[public,alias:'FPC_'+{$ifdef NOSTRANSI}'SHORT'+{$endif}'STR_COMP'];
begin
   str_real(len,fr,d,rt_s64bit,s);
end;
{$endif SUPPORT_COMP}


{$ifdef SUPPORT_FIXED}
procedure int_str_fixed(d : fixed;len,fr : longint;var s : shortstring);[public,alias:'FPC_'+{$ifdef NOSTRANSI}'SHORT'+{$endif}'STR_FIXED'];
begin
   str_real(len,fr,d,rt_f32bit,s);
end;
{$endif SUPPORT_FIXED}


procedure int_str_longint(v : longint;len : longint;var s : shortstring);[public,alias:'FPC_'+{$ifdef NOSTRANSI}'SHORT'+{$endif}'STR_LONGINT'];
begin
   int_str(v,s);
   if length(s)<len then
     s:=space(len-length(s))+s;
end;


procedure int_str_cardinal(v : cardinal;len : longint;var s : shortstring);[public,alias:'FPC_'+{$ifdef NOSTRANSI}'SHORT'+{$endif}'STR_CARDINAL'];
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
{The following isn't correct anymore for 64 bit integers! (JM)}
{$IfNDef ValInternCompiled}
              if length(s)-code>7 then
               code:=code+8;
{$EndIf ValInternCompiled}
            end;
      '%' : begin
              base:=2;
              inc(code);
            end;
     end;
  end;
  InitVal:=code;
end;


{$IfDef ValInternCompiled}

Function ValSignedInt(DestSize: longint; Const S: ShortString; var Code: ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_SHORTSTR'];
var
  u: ValSInt;
  base : byte;
  negative : boolean;
  temp, prev: ValUInt;
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
         (MaxSIntValue-Temp < u)) or
        ((base <> 10) and
         (MaxUIntValue-Temp < u)) then
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
  u: ValUInt;
  base : byte;
  negative : boolean;
  prev: ValUInt;
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
     if (u>=base) or (MaxUIntValue-ValUnsignedInt < u) then
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
      valfloat:=valfloat*10;
      valfloat:=valfloat+(ord(s[code])-ord('0'));
      inc(code);
   end;
{ Decimal ? }
  if (s[code]='.') and (length(s)>=code) then
   begin
      hd:=0.1;
      inc(code);
      while (s[code] in ['0'..'9']) and (length(s)>=code) do
        begin
           { Read fractional part. }
           flags:=flags or 2;
           valfloat:=valfloat+hd*(ord(s[code])-ord('0'));
           hd:=hd/10.0;
           inc(code);
        end;
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
  if esign>0 then
    for i:=1 to exponent do
      valfloat:=valfloat*10
    else
      for i:=1 to exponent do
        valfloat:=valfloat/10;
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


{$Else ValInternCompiled}


procedure val(const s : shortstring;var l : longint;var code : word);
var
  base,u  : byte;
  negativ : boolean;
begin
  l:=0;
  Code:=InitVal(s,negativ,base);
  if Code>length(s) then
   exit;
  if negativ and (s='-2147483648') then
   begin
     Code:=0;
     l:=$80000000;
     exit;
   end;
  while Code<=Length(s) do
   begin
     u:=ord(s[code]);
     case u of
       48..57 : u:=u-48;
       65..70 : u:=u-55;
      97..104 : u:=u-87;
     else
      u:=16;
     end;
     l:=l*longint(base);
     if (u>=base) or ((base=10) and (2147483647-l<longint(u))) then
      begin
        l:=0;
        exit;
      end;
     l:=l+u;
     inc(code);
   end;
  code := 0;
  if negativ then
   l:=0-l;
end;


procedure val(const s : shortstring;var l : longint;var code : integer);
begin
  val(s,l,word(code));
end;


procedure val(const s : shortstring;var l : longint;var code : longint);
var
  cw : word;
begin
  val (s,l,cw);
  code:=cw;
end;


procedure val(const s : shortstring;var l : longint);
var
  code : word;
begin
  val (s,l,code);
end;


procedure val(const s : shortstring;var b : byte);
var
  l : longint;
begin
  val(s,l);
  b:=l;
end;


procedure val(const s : shortstring;var b : byte;var code : word);
var
  l : longint;
begin
  val(s,l,code);
  b:=l;
end;


procedure val(const s : shortstring;var b : byte;var code : Integer);
begin
  val(s,b,word(code));
end;


procedure val(const s : shortstring;var b : byte;var code : longint);
var
  l : longint;
begin
  val(s,l,code);
  b:=l;
end;


procedure val(const s : shortstring;var b : shortint);
var
  l : longint;
begin
  val(s,l);
  b:=l;
end;


procedure val(const s : shortstring;var b : shortint;var code : word);
var
  l : longint;
begin
  val(s,l,code);
  b:=l;
end;


procedure val(const s : shortstring;var b : shortint;var code : Integer);
begin
  val(s,b,word(code));
end;


procedure val(const s : shortstring;var b : shortint;var code : longint);
var
  l : longint;
begin
  val(s,l,code);
  b:=l;
end;


procedure val(const s : shortstring;var b : word);
var
  l : longint;
begin
  val(s,l);
  b:=l;
end;


procedure val(const s : shortstring;var b : word;var code : word);
var
  l : longint;
begin
  val(s,l,code);
  b:=l;
end;


procedure val(const s : shortstring;var b : word;var code : Integer);
begin
  val(s,b,word(code));
end;


procedure val(const s : shortstring;var b : word;var code : longint);
var
  l : longint;
begin
  val(s,l,code);
  b:=l;
end;


procedure val(const s : shortstring;var b : integer);
var
  l : longint;
begin
   val(s,l);
   b:=l;
end;


procedure val(const s : shortstring;var b : integer;var code : word);
var
  l : longint;
begin
   val(s,l,code);
   b:=l;
end;


procedure val(const s : shortstring;var b : integer;var code : Integer);
begin
  val(s,b,word(code));
end;


procedure val(const s : shortstring;var b : integer;var code : longint);
var
  l : longint;
begin
  val(s,l,code);
  b:=l;
end;


procedure val(const s : shortstring;var v : cardinal;var code : word);
var
  negativ : boolean;
  base,u  : byte;
begin
  v:=0;
  code:=InitVal(s,negativ,base);
  if (Code>length(s)) or negativ then
   exit;
  while Code<=Length(s) do
   begin
     u:=ord(s[code]);
     case u of
       48..57 : u:=u-48;
       65..70 : u:=u-55;
      97..104 : u:=u-87;
     else
      u:=16;
     end;
     cardinal(v):=cardinal(v)*cardinal(longint(base));
     if (u>base) or (cardinal($ffffffff)-cardinal(v)>cardinal(longint(u))) then
      begin
        v:=0;
        exit;
      end;
     v:=v+u;
     inc(code);
   end;
  code:=0;
end;


procedure val(const s : shortstring;var v : cardinal);
var
  code : word;
begin
  val(s,v,code);
end;


procedure val(const s : shortstring;var v : cardinal;var code : integer);
begin
  val(s,v,word(code));
end;


procedure val(const s : shortstring;var v : cardinal;var code : longint);
var
  cw : word;
begin
  val(s,v,cw);
  code:=cw;
end;


procedure val(const s : shortstring;var d : valreal;var code : word);
var
  hd,
  esign,sign : valreal;
  exponent,i : longint;
  flags      : byte;
const
  i10 = 10;
begin
  d:=0;
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
      d:=d*i10;
      d:=d+(ord(s[code])-ord('0'));
      inc(code);
   end;
{ Decimal ? }
  if (s[code]='.') and (length(s)>=code) then
   begin
      hd:=extended(i1)/extended(i10);
      inc(code);
      while (s[code] in ['0'..'9']) and (length(s)>=code) do
        begin
           { Read fractional part. }
           flags:=flags or 2;
           d:=d+hd*(ord(s[code])-ord('0'));
           hd:=hd/i10;
           inc(code);
        end;
   end;
 { Again, read integer and fractional part}
  if flags=0 then
   begin
      d:=0;
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
           d:=0;
           exit;
        end;
      while (s[code] in ['0'..'9']) and (length(s)>=code) do
        begin
           exponent:=exponent*i10;
           exponent:=exponent+ord(s[code])-ord('0');
           inc(code);
        end;
   end;
{ Calculate Exponent }
  if esign>0 then
    for i:=1 to exponent do
      d:=d*i10
    else
      for i:=1 to exponent do
        d:=d/i10;
{ Not all characters are read ? }
  if length(s)>=code then
   begin
     d:=0.0;
     exit;
   end;
{ evalute sign }
  d:=d*sign;
{ success ! }
  code:=0;
end;


procedure val(const s : shortstring;var d : valreal;var code : integer);
begin
  val(s,d,word(code));
end;


procedure val(const s : shortstring;var d : valreal;var code : longint);
var
  cw : word;
begin
  val(s,d,cw);
  code:=cw;
end;


procedure val(const s : shortstring;var d : valreal);
var
  code : word;
begin
  val(s,d,code);
end;


{$ifdef SUPPORT_SINGLE}
procedure val(const s : shortstring;var d : single;var code : word);
var
  e : valreal;
begin
  val(s,e,code);
  d:=e;
end;


procedure val(const s : shortstring;var d : single;var code : integer);
var
  e : valreal;
begin
  val(s,e,word(code));
  d:=e;
end;


procedure val(const s : shortstring;var d : single;var code : longint);
var
  cw : word;
  e  : valreal;
begin
  val(s,e,cw);
  d:=e;
  code:=cw;
end;


procedure val(const s : shortstring;var d : single);
var
  code : word;
  e    : valreal;
begin
  val(s,e,code);
  d:=e;
end;
{$endif SUPPORT_SINGLE}


{$ifdef DEFAULT_EXTENDED}

  { with extended as default the valreal is extended so for real there need
    to be a new val }

  procedure val(const s : shortstring;var d : real;var code : word);
  var
    e : valreal;
  begin
    val(s,e,code);
    d:=e;
  end;


  procedure val(const s : shortstring;var d : real;var code : integer);
  var
    e : valreal;
  begin
    val(s,e,word(code));
    d:=e;
  end;


  procedure val(const s : shortstring;var d : real;var code : longint);
  var
    cw : word;
    e  : valreal;
  begin
    val(s,e,cw);
    d:=e;
    code:=cw;
  end;


  procedure val(const s : shortstring;var d : real);
  var
    code : word;
    e    : valreal;
  begin
    val(s,e,code);
    d:=e;
  end;

{$else DEFAULT_EXTENDED}

  { when extended is not the default it could still be supported }

  {$ifdef SUPPORT_EXTENDED}

  procedure val(const s : shortstring;var d : extended;var code : word);
  var
    e : valreal;
  begin
    val(s,e,code);
    d:=e;
  end;

  procedure val(const s : shortstring;var d : extended;var code : integer);
  var
    e : valreal;
  begin
    val(s,e,word(code));
    d:=e;
  end;

  procedure val(const s : shortstring;var d : extended;var code : longint);
  var
    cw : word;
    e  : valreal;
  begin
    val(s,e,cw);
    d:=e;
    code:=cw;
  end;

  procedure val(const s : shortstring;var d : extended);
  var
    code : word;
    e    : valreal;
  begin
    val(s,e,code);
    d:=e;
  end;

  {$endif SUPPORT_EXTENDED}

{$endif DEFAULT_EXTENDED}


{$ifdef SUPPORT_COMP}
procedure val(const s : shortstring;var d : comp;var code : word);
var
  e : valreal;
begin
  val(s,e,code);
  d:=comp(e);
end;


procedure val(const s : shortstring;var d : comp;var code : integer);
var
  e : valreal;
begin
  val(s,e,word(code));
  d:=comp(e);
end;


procedure val(const s : shortstring;var d : comp;var code : longint);
var
  cw : word;
  e  : valreal;
begin
  val(s,e,cw);
  d:=comp(e);
  code:=cw;
end;


procedure val(const s : shortstring;var d : comp);
var
  code : word;
  e    : valreal;
begin
  val(s,e,code);
  d:=comp(e);
end;
{$endif SUPPORT_COMP}


{$ifdef SUPPORT_FIXED}
procedure val(const s : shortstring;var d : fixed;var code : word);
var
  e : valreal;
begin
  val(s,e,code);
  d:=fixed(e);
end;


procedure val(const s : shortstring;var d : fixed;var code : integer);
var
  e : valreal;
begin
  val(s,e,word(code));
  d:=fixed(e);
end;


procedure val(const s : shortstring;var d : fixed;var code : longint);
var
  cw : word;
  e  : valreal;
begin
  val(s,e,cw);
  d:=fixed(e);
  code:=cw;
end;


procedure val(const s : shortstring;var d : fixed);
var
  code : word;
  e    : valreal;
begin
  val(s,e,code);
  d:=fixed(e);
end;
{$endif SUPPORT_FIXED}
{$EndIf ValInternCompiled}

Procedure SetString (Var S : Shortstring; Buf : PChar; Len : Longint);

begin
  Move (Buf[0],S[1],Len);
  S[0]:=chr(len);
end;

{
  $Log$
  Revision 1.27  1999-04-08 15:57:54  peter
    + subrange checking for readln()

  Revision 1.26  1999/04/05 12:28:27  michael
  + Fixed insert with char. length byte wrapped around in some cases.

  Revision 1.25  1999/04/01 22:11:50  peter
    * fixed '1.' parsing of val

  Revision 1.24  1999/04/01 22:00:49  peter
    * universal names for str/val (ansistr instead of stransi)
    * '1.' support for val() this is compatible with tp7

  Revision 1.23  1999/03/26 00:24:16  peter
    * last para changed to long for easier pushing with 4 byte aligns

  Revision 1.22  1999/03/16 17:49:36  jonas
    * changes for internal Val code (do a "make cycle OPT=-dvalintern" to test)
    * in text.inc: changed RTE 106 when read integer values are out of bounds to RTE 201
    * in systemh.inc: disabled "support_fixed" for the i386 because it gave internal errors,

  Revision 1.21  1999/03/10 21:49:03  florian
    * str and val for extended use now int constants to minimize
      rounding error

  Revision 1.20  1999/03/03 15:23:57  michael
  + Added setstring for Delphi compatibility

  Revision 1.19  1999/01/25 20:24:28  peter
    * fixed insert to support again the max string length

  Revision 1.18  1999/01/11 19:26:55  jonas
    * made inster(string,string,index) a bit faster
    + overloaded insert(char,string,index)

  Revision 1.17  1998/12/15 22:43:02  peter
    * removed temp symbols

  Revision 1.16  1998/11/05 10:29:34  pierre
   * fix for length(char) in const expressions

  Revision 1.15  1998/11/04 10:20:50  peter
    * ansistring fixes

  Revision 1.14  1998/10/11 14:30:19  peter
    * small typo :(

  Revision 1.13  1998/10/10 15:28:46  peter
    + read single,fixed
    + val with code:longint
    + val for fixed

  Revision 1.12  1998/09/14 10:48:19  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.11  1998/08/11 21:39:07  peter
    * splitted default_extended from support_extended

  Revision 1.10  1998/08/08 12:28:13  florian
    * a lot small fixes to the extended data type work

  Revision 1.9  1998/07/18 17:14:23  florian
    * strlenint type implemented

  Revision 1.8  1998/07/10 11:02:38  peter
    * support_fixed, becuase fixed is not 100% yet for the m68k

  Revision 1.7  1998/07/02 12:14:19  carl
    * No SINGLE type for non-intel processors!!

  Revision 1.6  1998/06/25 09:44:19  daniel
  + RTLLITE directive to compile minimal RTL.

  Revision 1.5  1998/06/04 23:45:59  peter
    * comp,extended are only i386 added support_comp,support_extended

  Revision 1.4  1998/05/31 14:14:52  peter
    * removed warnings using comp()

  Revision 1.3  1998/05/12 10:42:45  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}
