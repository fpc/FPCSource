{
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

procedure fpc_Shortstr_SetLength(var s:shortstring;len:SizeInt);[Public,Alias : 'FPC_SHORTSTR_SETLENGTH']; compilerproc;
begin
  if Len>255 then
   Len:=255;
  s[0]:=chr(len);
end;

function fpc_shortstr_copy(const s : shortstring;index : SizeInt;count : SizeInt): shortstring;compilerproc;
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
  fpc_shortstr_Copy[0]:=chr(Count);
  Move(s[Index+1],fpc_shortstr_Copy[1],Count);
end;


procedure delete(var s : shortstring;index : SizeInt;count : SizeInt);
begin
  if index<=0 then
     exit;
  if (Index<=Length(s)) and (Count>0) then
   begin
     if Count>length(s)-Index then
      Count:=length(s)-Index+1;
     s[0]:=Chr(length(s)-Count);
     if Index<=Length(s) then
      Move(s[Index+Count],s[Index],Length(s)-Index+1);
   end;
end;


procedure insert(const source : shortstring;var s : shortstring;index : SizeInt);
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


procedure insert(source : Char;var s : shortstring;index : SizeInt);
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


function pos(const substr : shortstring;const s : shortstring):SizeInt;
var
  i,MaxLen : SizeInt;
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
function pos(c:char;const s:shortstring):SizeInt;
var
  i : SizeInt;
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


{$ifdef interncopy}
function fpc_char_copy(c:char;index : SizeInt;count : SizeInt): shortstring;compilerproc;
begin
  if (index=1) and (Count>0) then
   fpc_char_Copy:=c
  else
   fpc_char_Copy:='';
end;
{$else}
function copy(c:char;index : SizeInt;count : SizeInt): shortstring;
begin
  if (index=1) and (Count>0) then
   Copy:=c
  else
   Copy:='';
end;
{$endif}


function pos(const substr : shortstring;c:char): SizeInt;
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


function hexstr(val : pointer) : shortstring;
var
  i : longint;
  v : ptrint;
begin
  v:=ptrint(val);
  hexstr[0]:=chr(sizeof(pointer)*2);
  for i:=sizeof(pointer)*2 downto 1 do
   begin
     hexstr[i]:=hextbl[v and $f];
     v:=v shr 4;
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

{$ifdef STR_USES_VALINT}
procedure fpc_shortstr_SInt(v : valSInt;len : SizeInt;var s : shortstring);[public,alias:'FPC_SHORTSTR_SINT']; compilerproc;
{$else}
procedure fpc_shortstr_longint(v : longint;len : SizeInt;var s : shortstring);[public,alias:'FPC_SHORTSTR_LONGINT']; compilerproc;
{$endif}
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

{$ifdef STR_USES_VALINT}
procedure fpc_shortstr_UInt(v : valUInt;len : SizeInt;var s : shortstring);[public,alias:'FPC_SHORTSTR_UINT']; compilerproc;
{$else}
  procedure fpc_shortstr_longword(v : longword;len : SizeInt;var s : shortstring);[public,alias:'FPC_SHORTSTR_LONGWORD']; compilerproc;
{$endif}
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

{$ifndef CPU64}

  procedure fpc_shortstr_qword(v : qword;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_QWORD']; compilerproc;
    begin
       int_str(v,s);
       if length(s)<len then
         s:=space(len-length(s))+s;
    end;


  procedure fpc_shortstr_int64(v : int64;len : longint;var s : shortstring);[public,alias:'FPC_SHORTSTR_INT64'];  compilerproc;
    begin
       int_str(v,s);
       if length(s)<len then
         s:=space(len-length(s))+s;
    end;

{$endif CPU64}


{ fpc_shortstr_sInt must appear before this file is included, because }
{ it's used inside real2str.inc and otherwise the searching via the      }
{ compilerproc name will fail (JM)                                       }

{$I real2str.inc}

procedure fpc_ShortStr_Float(d : ValReal;len,fr,rt : SizeInt;var s : shortstring);[public,alias:'FPC_SHORTSTR_FLOAT']; compilerproc;
begin
  str_real(len,fr,d,treal_type(rt),s);
end;


{
   Array Of Char Str() helpers
}

{$ifdef STR_USES_VALINT}
procedure fpc_chararray_sint(v : valsint;len : SizeInt;var a:array of char);compilerproc;
{$else}
procedure fpc_chararray_longint(v : longint;len : SizeInt;var a:array of char);compilerproc;
{$endif}
var
  ss : shortstring;
  maxlen : SizeInt;
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


{$ifdef STR_USES_VALINT}
procedure fpc_chararray_uint(v : valuint;len : SizeInt;var a : array of char);compilerproc;
{$else}
procedure fpc_chararray_longword(v : longword;len : SizeInt;var a : array of char);compilerproc;
{$endif}
var
  ss : shortstring;
  maxlen : SizeInt;
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


{$ifndef CPU64}

procedure fpc_chararray_qword(v : qword;len : SizeInt;var a : array of char);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
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


procedure fpc_chararray_int64(v : int64;len : SizeInt;var a : array of char);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
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

{$endif CPU64}


procedure fpc_chararray_Float(d : ValReal;len,fr,rt : SizeInt;var a : array of char);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
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
  Code : SizeInt;
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
              inc(code);
            end;
      '%' : begin
              base:=2;
              inc(code);              
            end;
      '&' : begin
              Base:=8;
              inc(code);              
            end;
      '0' : begin
              if (code < length(s)) and (s[code+1] in ['x', 'X']) then 
              begin
                inc(code, 2);
                base := 16;
              end;
            end;
     end;
  end;
  { strip leading zeros }
  while ((code < length(s)) and (s[code] = '0')) do begin
    inc(code);
  end;
  InitVal:=code;
end;


Function fpc_Val_SInt_ShortStr(DestSize: SizeInt; Const S: ShortString; var Code: ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_SHORTSTR']; compilerproc;
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
      4: fpc_Val_SInt_ShortStr := SizeInt(fpc_Val_SInt_ShortStr);}
    End;
end;

{ we need this for fpc_Val_SInt_Ansistr and fpc_Val_SInt_WideStr because }
{ we have to pass the DestSize parameter on (JM)                         }
Function int_Val_SInt_ShortStr(DestSize: SizeInt; Const S: ShortString; var Code: ValSInt): ValSInt; [external name 'FPC_VAL_SINT_SHORTSTR'];


Function fpc_Val_UInt_Shortstr(Const S: ShortString; var Code: ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_SHORTSTR']; compilerproc;
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


{$ifndef CPU64}

  Function fpc_val_int64_shortstr(Const S: ShortString; var Code: ValSInt): Int64; [public, alias:'FPC_VAL_INT64_SHORTSTR']; compilerproc;
   type
     QWordRec = packed record
       l1,l2: longint;
     end;

    var
       u, temp, prev, maxint64, maxqword : qword;
       base : byte;
       negative : boolean;

  begin
    fpc_val_int64_shortstr := 0;
    Temp:=0;
    Code:=InitVal(s,negative,base);
    if Code>length(s) then
     exit;
    { high(int64) produces 0 in version 1.0 (JM) }
    with qwordrec(maxint64) do
      begin
{$ifdef ENDIAN_LITTLE}
        l1 := longint($ffffffff);
        l2 := $7fffffff;
{$else ENDIAN_LITTLE}
        l1 := $7fffffff;
        l2 := longint($ffffffff);
{$endif ENDIAN_LITTLE}
      end;
    with qwordrec(maxqword) do
      begin
        l1 := longint($ffffffff);
        l2 := longint($ffffffff);
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
       Prev:=Temp;
       Temp:=Temp*Int64(base);
     If (u >= base) or
        ((base = 10) and
         (maxint64-temp+ord(negative) < u)) or
        ((base <> 10) and
         (qword(maxqword-temp) < u)) or
        (prev > maxqword div qword(base)) Then
       Begin
         fpc_val_int64_shortstr := 0;
         Exit
       End;
       Temp:=Temp+u;
       inc(code);
     end;
    code:=0;
    fpc_val_int64_shortstr:=int64(Temp);
    If Negative Then
      fpc_val_int64_shortstr:=-fpc_val_int64_shortstr;
  end;


  Function fpc_val_qword_shortstr(Const S: ShortString; var Code: ValSInt): QWord; [public, alias:'FPC_VAL_QWORD_SHORTSTR']; compilerproc;
    type qwordrec = packed record
      l1,l2: longint;
    end;
    var
       u, prev, maxqword: QWord;
       base : byte;
       negative : boolean;
  begin
    fpc_val_qword_shortstr:=0;
    Code:=InitVal(s,negative,base);
    If Negative or (Code>length(s)) Then
      Exit;
    with qwordrec(maxqword) do
      begin
        l1 := longint($ffffffff);
        l2 := longint($ffffffff);
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
       prev := fpc_val_qword_shortstr;
       If (u>=base) or
         ((QWord(maxqword-u) div QWord(base))<prev) then
         Begin
           fpc_val_qword_shortstr := 0;
           Exit
         End;
       fpc_val_qword_shortstr:=fpc_val_qword_shortstr*QWord(base) + u;
       inc(code);
     end;
    code := 0;
  end;

{$endif CPU64}


Function fpc_Val_Real_ShortStr(const s : shortstring; var code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_SHORTSTR']; compilerproc;
var
  hd,
  esign,sign : valreal;
  exponent,i : SizeInt;
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
  if (length(s)>=code) and (s[code]='.') then
   begin
      hd:=1.0;
      inc(code);
      while (length(s)>=code) and (s[code] in ['0'..'9']) do
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
  if (length(s)>=code) and (upcase(s[code])='E') then
   begin
      inc(code);
      if Length(s) >= code then
        if s[code]='+' then
          inc(code)
        else
          if s[code]='-' then
           begin
             esign:=-1;
             inc(code);
           end;
      if (length(s)<code) or not(s[code] in ['0'..'9']) then
        begin
           fpc_Val_Real_ShortStr:=0.0;
           exit;
        end;
      while (length(s)>=code) and (s[code] in ['0'..'9']) do
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


Procedure SetString (Var S : Shortstring; Buf : PChar; Len : SizeInt);
begin
  If Len > High(S) then
    Len := High(S);
  SetLength(S,Len);
  If Buf<>Nil then
    begin
      Move (Buf[0],S[1],Len);
    end;
end;

