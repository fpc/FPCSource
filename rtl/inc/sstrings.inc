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

function copy(const s : string;index : StrLenInt;count : StrLenInt): string;

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

procedure delete(var s : string;index : StrLenInt;count : StrLenInt);

begin
  if index<=0 then
    begin
    count:=count+index-1;
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

procedure insert(const source : string;var s : string;index : StrLenInt);

begin
  if index>1 then
   dec(index)
  else
   index:=0;
  s:=Copy(s,1,Index)+source+Copy(s,Index+1,length(s));
end;

function pos(const substr : string;const s : string): byte;

var i,j : longint;
    e : boolean;

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

function pos(c:char;const s:string):byte;

var i:longint;

begin
    for i:=1 to length(s) do
        if s[i]=c then
            begin
                pos:=i;
                exit;
            end;
    pos:=0;
end;

{$ifdef IBM_CHAR_SET}
const
  UpCaseTbl : string[7]=#154#142#153#144#128#143#165;
  LoCaseTbl : string[7]=#129#132#148#130#135#134#164;
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

function upcase(const s : string) : string;

var i : longint;

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

function lowercase(const s : string) : string;

var i : longint;

begin
  lowercase [0] := s[0];
  for i := 1 to length (s) do
     lowercase[i] := lowercase (s[i]);
end;

function hexstr(val : longint;cnt : byte) : string;

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



 function binstr(val : longint;cnt : byte) : string;

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

 function space (b : byte): string;

 begin
    space[0] := chr(b);
    FillChar (Space[1],b,' ');
 end;

{*****************************************************************************
                              Str() Helpers
*****************************************************************************}

procedure int_str_real(d : real;len,fr : longint;var s : string);[public, alias : 'STR_REAL'];
begin
{$ifdef i386}
   str_real(len,fr,d,rt_s64real,s);
{$else}
   str_real(len,fr,d,rt_s32real,s);
{$endif}
end;

{$ifdef SUPPORT_SINGLE}
procedure int_str_single(d : single;len,fr : longint;var s : string);[public, alias : 'STR_SINGLE'];
begin
   str_real(len,fr,d,rt_s32real,s);
end;
{$endif SUPPORT_SINGLE}


{$ifdef SUPPORT_EXTENDED}
procedure int_str_extended(d : extended;len,fr : longint;var s : string);[public, alias : 'STR_EXTENDED'];
begin
   str_real(len,fr,d,rt_s80real,s);
end;
{$endif SUPPORT_EXTENDED}


{$ifdef SUPPORT_COMP}
procedure int_str_comp(d : comp;len,fr : longint;var s : string);[public, alias : 'STR_COMP'];
begin
   str_real(len,fr,d,rt_s64bit,s);
end;
{$endif SUPPORT_COMP}


{$ifdef SUPPORT_FIXED}
procedure int_str_fixed(d : fixed;len,fr : longint;var s : string);[public, alias : 'STR_FIXED'];
begin
   str_real(len,fr,d,rt_f32bit,s);
end;
{$endif SUPPORT_FIXED}


procedure int_str_longint(v : longint;len : longint;var s : string);[public, alias : 'STR_LONGINT'];
begin
   int_str(v,s);
   if length(s)<len then
     s:=space(len-length(s))+s;
end;


procedure int_str_cardinal(v : cardinal;len : longint;var s : string);[public, alias : 'STR_CARDINAL'];
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;


{*****************************************************************************
                           Val() Functions
*****************************************************************************}

Function InitVal(const s:string;var negativ:boolean;var base:byte):Word;
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
              if length(s)-code>7 then
               code:=code+8;
            end;
      '%' : begin
              base:=2;
              inc(code);
            end;
     end;
  end;
  InitVal:=code;
end;


procedure val(const s : string;var l : longint;var code : word);
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


procedure val(const s : string;var l : longint;var code : integer);
begin
  val(s,l,word(code));
end;


procedure val(const s : string;var l : longint);
var
  code : word;
begin
   val (s,l,code);
end;


procedure val(const s : string;var b : byte);
var
  l : longint;
begin
  val(s,l);
  b:=l;
end;


procedure val(const s : string;var b : byte;var code : word);
var
  l : longint;
begin
  val(s,l,code);
  b:=l;
end;


procedure val(const s : string;var b : byte;var code : Integer);
begin
  val(s,b,word(code));
end;


procedure val(const s : string;var b : shortint);
var
  l : longint;
begin
  val(s,l);
  b:=l;
end;


procedure val(const s : string;var b : shortint;var code : word);
var
  l : longint;
begin
  val(s,l,code);
  b:=l;
end;


procedure val(const s : string;var b : shortint;var code : Integer);
begin
  val(s,b,word(code));
end;


procedure val(const s : string;var b : word);
var
  l : longint;
begin
  val(s,l);
  b:=l;
end;


procedure val(const s : string;var b : word;var code : word);
var
  l : longint;
begin
  val(s,l,code);
  b:=l;
end;


procedure val(const s : string;var b : word;var code : Integer);
begin
  val(s,b,word(code));
end;


procedure val(const s : string;var b : integer);
var
   l : longint;
begin
   val(s,l);
   b:=l;
end;


procedure val(const s : string;var b : integer;var code : word);
var
   l : longint;
begin
   val(s,l,code);
   b:=l;
end;


procedure val(const s : string;var b : integer;var code : Integer);
begin
  val(s,b,word(code));
end;


procedure val(const s : string;var d : real;var code : word);
var
  hd,
  esign,sign : real;
  exponent,i : longint;
  flags      : byte;
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
           sign:=-1.0;
           inc(code);
         end;
  end;
  while (Code<=Length(s)) and (s[code] in ['0'..'9']) do
   begin
   { Read integer part }
      flags:=flags or 1;
      d:=d*10;
      d:=d+(ord(s[code])-ord('0'));
      inc(code);
   end;
{ Decimal ? }
  if (s[code]='.') and (length(s)>=code) then
   begin
      hd:=0.1;
      inc(code);
      { After dot, a number is required. }
      if not(s[code] in ['0'..'9']) or (length(s)<code) then
        begin
           d:=0.0;
           exit;
        end;
      while (s[code] in ['0'..'9']) and (length(s)>=code) do
        begin
           { Read fractional part. }
           flags:=flags or 2;
           d:=d+hd*(ord(s[code])-ord('0'));
           hd:=hd/10.0;
           inc(code);
        end;
   end;
 { Again, read integer and fractional part}
  if flags=0 then
   begin
      d:=0.0;
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
           d:=0.0;
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
      d:=d*10
    else
      for i:=1 to exponent do
        d:=d/10;
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


procedure val(const s : string;var d : real;var code : integer);
begin
  val(s,d,word(code));
end;


procedure val(const s : string;var d : real);
var
  code : word;
begin
  val(s,d,code);
end;


{$ifdef SUPPORT_SINGLE}
procedure val(const s : string;var d : single;var code : word);
var
  e : double;
begin
  val(s,e,code);
  d:=e;
end;


procedure val(const s : string;var d : single;var code : integer);
var
  e : double;
begin
  val(s,e,word(code));
  d:=e;
end;


procedure val(const s : string;var d : single);
var
  code : word;
  e    : double;
begin
  val(s,e,code);
  d:=e;
end;
{$endif SUPPORT_SINGLE}

{$ifdef SUPPORT_EXTENDED}
procedure val(const s : string;var d : extended;var code : word);
var
  e : double;
begin
  val(s,e,code);
  d:=e;
end;


procedure val(const s : string;var d : extended;var code : integer);
var
  e : double;
begin
  val(s,e,word(code));
  d:=e;
end;


procedure val(const s : string;var d : extended);
var
  code : word;
  e    : double;
begin
  val(s,e,code);
  d:=e;
end;
{$endif SUPPORT_EXTENDED}


{$ifdef SUPPORT_COMP}
procedure val(const s : string;var d : comp;var code : word);
var
  e : double;
begin
  val(s,e,code);
  d:=comp(e);
end;


procedure val(const s : string;var d : comp;var code : integer);
var
  e : double;
begin
  val(s,e,word(code));
  d:=comp(e);
end;


procedure val(const s : string;var d : comp);
var
  code : word;
  e    : double;
begin
  val(s,e,code);
  d:=comp(e);
end;
{$endif SUPPORT_COMP}

procedure val(const s : string;var v : cardinal;var code : word);
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


procedure val(const s : string;var v : cardinal);
var
  code : word;
begin
  val(s,v,code);
end;


procedure val(const s : string;var v : cardinal;var code : integer);
begin
  val(s,v,word(code));
end;

{
  $Log$
  Revision 1.9  1998-07-18 17:14:23  florian
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
