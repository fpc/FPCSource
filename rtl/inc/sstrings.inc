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

{$ifndef INTERNSETLENGTH}
procedure SetLength(var s:shortstring;len:StrLenInt);
{$else INTERNSETLENGTH}
procedure fpc_Shortstr_SetLength(var s:shortstring;len:StrLenInt);[Public,Alias : 'FPC_SHORTSTR_SETLENGTH']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$endif INTERNSETLENGTH}
begin
  if Len>255 then
   Len:=255;
  s[0]:=chr(len);
end;

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
   if count>length(s)-index then
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
     if Count>length(s)-Index then
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
  i,MaxLen : StrLenInt;
  pc : pchar;
begin
  Pos:=0;
  if Length(SubStr)>0 then
   begin
     MaxLen:=Length(s)-Length(SubStr);
     i:=0;
     pc:=@s[1];
     while (i<=MaxLen) do
      begin
        inc(i);
        if (SubStr[1]=pc^) and
           (CompareChar(Substr[1],pc^,Length(SubStr))=0) then
         begin
           Pos:=i;
           exit;
         end;
        inc(pc);
      end;
   end;
end;


{Faster when looking for a single char...}
function pos(c:char;const s:shortstring):StrLenInt;
var
  i : StrLenInt;
  pc : pchar;
begin
  pc:=@s[1];
  for i:=1 to length(s) do
   begin
     if pc^=c then
      begin
        pos:=i;
        exit;
      end;
     inc(pc);
   end;
  pos:=0;
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


function lowercase(c : char) : char;overload;
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


function lowercase(const s : shortstring) : shortstring; overload;
var
  i : longint;
begin
  lowercase [0]:=s[0];
  for i:=1 to length(s) do
   lowercase[i]:=lowercase (s[i]);
end;


const
  HexTbl : array[0..15] of char='0123456789ABCDEF';

function hexstr(val : longint;cnt : byte) : shortstring;
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

function octstr(val : longint;cnt : byte) : shortstring;
var
  i : longint;
begin
  octstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     octstr[i]:=hextbl[val and 7];
     val:=val shr 3;
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


function hexstr(val : int64;cnt : byte) : shortstring;
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


function octstr(val : int64;cnt : byte) : shortstring;
var
  i : longint;
begin
  octstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     octstr[i]:=hextbl[val and 7];
     val:=val shr 3;
   end;
end;


function binstr(val : int64;cnt : byte) : shortstring;
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


function space (b : byte): shortstring;
begin
  space[0] := chr(b);
  FillChar (Space[1],b,' ');
end;


{*****************************************************************************
                              Str() Helpers
*****************************************************************************}

procedure fpc_shortstr_longint(v : longint;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_LONGINT']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

{$ifdef ver1_0}
procedure fpc_shortstr_cardinal(v : longword;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_CARDINAL']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$else}
procedure fpc_shortstr_longword(v : longword;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_LONGWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$endif}
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

{ fpc_shortstr_longint must appear before this file is included, because }
{ it's used inside real2str.inc and otherwise the searching via the      }
{ compilerproc name will fail (JM)                                       }

{$I real2str.inc}

procedure fpc_ShortStr_Float(d : ValReal;len,fr,rt : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_FLOAT']; {$ifdef hascompilerproc}compilerproc;{$endif hascompilerproc}
begin
  str_real(len,fr,d,treal_type(rt),s);
end;


{
   Array Of Char Str() helpers
}

procedure fpc_chararray_longint(v : longint;len : longint;var a:array of char);{$ifdef hascompilerproc} compilerproc; {$endif}
var
  ss : shortstring;
  maxlen : longint;
begin
  int_str(v,ss);
  if length(ss)<len then
    ss:=space(len-length(ss))+ss;
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  move(ss[1],pchar(@a)^,maxlen);
end;

procedure fpc_chararray_longword(v : longword;len : longint;var a : array of char);{$ifdef hascompilerproc} compilerproc; {$endif}
var
  ss : shortstring;
  maxlen : longint;
begin
  int_str(v,ss);
  if length(ss)<len then
    ss:=space(len-length(ss))+ss;
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  move(ss[1],pchar(@a)^,maxlen);
end;


procedure fpc_chararray_Float(d : ValReal;len,fr,rt : longint;var a : array of char);{$ifdef hascompilerproc}compilerproc;{$endif hascompilerproc}
var
  ss : shortstring;
  maxlen : longint;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  move(ss[1],pchar(@a)^,maxlen);
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
      '&' : begin
              Base:=8;
              repeat
                inc(code);
              until (code>=length(s)) or (s[code]<>'0');
            end;
     end;
  end;
  InitVal:=code;
end;

Function fpc_Val_SInt_ShortStr(DestSize: longint; Const S: ShortString; var Code: ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  u, temp, prev, maxPrevValue, maxNewValue: ValUInt;
  base : byte;
  negative : boolean;
begin
  fpc_Val_SInt_ShortStr := 0;
  Temp:=0;
  Code:=InitVal(s,negative,base);
  if Code>length(s) then
   exit;
  maxPrevValue := ValUInt(MaxUIntValue) div ValUInt(Base);
  if (base = 10) then
    maxNewValue := MaxSIntValue + ord(negative)
  else
    maxNewValue := MaxUIntValue;
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
     If (u >= base) or
        (ValUInt(maxNewValue-u) < Temp) or
        (prev > maxPrevValue) Then
       Begin
         fpc_Val_SInt_ShortStr := 0;
         Exit
       End;
     Temp:=Temp+u;
     inc(code);
   end;
  code := 0;
  fpc_Val_SInt_ShortStr := ValSInt(Temp);
  If Negative Then
    fpc_Val_SInt_ShortStr := -fpc_Val_SInt_ShortStr;
  If Not(Negative) and (base <> 10) Then
   {sign extend the result to allow proper range checking}
    Case DestSize of
      1: fpc_Val_SInt_ShortStr := shortint(fpc_Val_SInt_ShortStr);
      2: fpc_Val_SInt_ShortStr := smallint(fpc_Val_SInt_ShortStr);
{     Uncomment the folling once full 64bit support is in place
      4: fpc_Val_SInt_ShortStr := longint(fpc_Val_SInt_ShortStr);}
    End;
end;

{$ifdef hascompilerproc}
{ we need this for fpc_Val_SInt_Ansistr and fpc_Val_SInt_WideStr because }
{ we have to pass the DestSize parameter on (JM)                         }
Function fpc_Val_SInt_ShortStr(DestSize: longint; Const S: ShortString; var Code: ValSInt): ValSInt; [external name 'FPC_VAL_SINT_SHORTSTR'];
{$endif hascompilerproc}


Function fpc_Val_UInt_Shortstr(Const S: ShortString; var Code: ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  u, prev : ValUInt;
  base : byte;
  negative : boolean;
begin
  fpc_Val_UInt_Shortstr:=0;
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
     prev := fpc_Val_UInt_Shortstr;
     If (u>=base) or
        (ValUInt(MaxUIntValue-u) div ValUInt(Base)<prev) then
      begin
        fpc_Val_UInt_Shortstr:=0;
        exit;
      end;
     fpc_Val_UInt_Shortstr:=fpc_Val_UInt_Shortstr*ValUInt(base) + u;
     inc(code);
   end;
  code := 0;
end;


Function fpc_Val_Real_ShortStr(const s : shortstring; var code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  hd,
  esign,sign : valreal;
  exponent,i : longint;
  flags      : byte;
begin
  fpc_Val_Real_ShortStr:=0.0;
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

fpc_Val_Real_ShortStr:=fpc_Val_Real_ShortStr*10+(ord(s[code])-ord('0'));
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
           fpc_Val_Real_ShortStr:=fpc_Val_Real_ShortStr*10+(ord(s[code])-ord('0'));
           hd:=hd*10.0;
           inc(code);
        end;
      fpc_Val_Real_ShortStr:=fpc_Val_Real_ShortStr/hd;
   end;
 { Again, read integer and fractional part}
  if flags=0 then
   begin
      fpc_Val_Real_ShortStr:=0.0;
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
           fpc_Val_Real_ShortStr:=0.0;
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
      fpc_Val_Real_ShortStr:=Val_Real_ShortStr*10
    else
      for i:=1 to exponent do
        fpc_Val_Real_ShortStr:=Val_Real_ShortStr/10; }
  hd:=1.0;
  for i:=1 to exponent do
    hd:=hd*10.0;
  if esign>0 then
    fpc_Val_Real_ShortStr:=fpc_Val_Real_ShortStr*hd
  else
    fpc_Val_Real_ShortStr:=fpc_Val_Real_ShortStr/hd;
{ Not all characters are read ? }
  if length(s)>=code then
   begin
     fpc_Val_Real_ShortStr:=0.0;
     exit;
   end;
{ evaluate sign }
  fpc_Val_Real_ShortStr:=fpc_Val_Real_ShortStr*sign;
{ success ! }
  code:=0;
end;


Procedure SetString (Var S : Shortstring; Buf : PChar; Len : Longint);
begin
  Move (Buf[0],S[1],Len);
  S[0]:=chr(len);
end;

{
  $Log$
  Revision 1.22  2002-09-07 21:19:00  carl
    * cardinal -> longword

  Revision 1.21  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

  Revision 1.20  2002/09/02 19:24:41  peter
    * array of char support for Str()

  Revision 1.19  2002/08/06 20:53:38  michael
    + Added support for octal strings (using &)

  Revision 1.18  2002/01/24 18:27:06  peter
    * lowercase() overloaded

}
