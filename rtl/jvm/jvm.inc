{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                           JVM specific stuff
****************************************************************************}

{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    softfloat_exception_mask:=float_flag_underflow or float_flag_inexact or float_flag_denormal;
  end;
  
{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    softfloat_exception_flags:=0;
  end;


procedure fpc_cpuinit;
  begin
    SysResetFPU;
    if not(IsLibrary) then
      SysInitFPU;
  end;


{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;
  begin
    result:=nil;
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;
  begin
    result:=nil;
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;
  begin
    result:=nil;
  end;


{$define FPC_SYSTEM_HAS_SPTR}
function Sptr:Pointer;
   begin
     result:=nil;
   end;

{****************************************************************************
                               Primitives
****************************************************************************}

{ lie so that the non-compilable generic versions will be skipped }
{$define FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLWORD}
{$define FPC_SYSTEM_HAS_FILLDWORD}
{$define FPC_SYSTEM_HAS_FILLQWORD}
{$define FPC_SYSTEM_HAS_INDEXBYTE}
{$define FPC_SYSTEM_HAS_INDEXWORD}
{$define FPC_SYSTEM_HAS_INDEXDWORD}
{$define FPC_SYSTEM_HAS_INDEXQWORD}
{$define FPC_SYSTEM_HAS_COMPAREBYTE}
{$define FPC_SYSTEM_HAS_COMPAREWORD}
{$define FPC_SYSTEM_HAS_COMPAREDWORD}
{$define FPC_SYSTEM_HAS_MOVECHAR0}
{$define FPC_SYSTEM_HAS_INDEXCHAR0}
{$define FPC_SYSTEM_HAS_COMPARECHAR0}

{****************************************************************************
                                 String
****************************************************************************}

{ more lies }
{$define FPC_STRTOSHORTSTRINGPROC}
{$define FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
{$define FPC_SYSTEM_HAS_FPC_PWIDECHAR_LENGTH}

{$define FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}
procedure fpc_pchar_to_shortstr(out res : shortstring;p:pchar); compilerproc;
var
  i, len: longint;
  arr: TAnsiCharArray;
begin
  arr:=TAnsiCharArray(p);
  i:=0;
  while arr[i]<>#0 do
    inc(i);
  if i<>0 then
    res:=pshortstring(ShortStringClass.create(arr,min(i,high(res))))^
  else
    res:=''
end;

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}
procedure fpc_shortstr_to_shortstr(out res:shortstring; const sstr: shortstring); compilerproc;
var
  len: longint;
begin
  len:=length(sstr);
  if len>high(res) then
    len:=high(res);
  ShortstringClass(@res).curlen:=len;
  JLSystem.ArrayCopy(JLObject(ShortstringClass(@sstr).fdata),0,JLObject(ShortstringClass(@res).fdata),0,len);
end;


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}
procedure fpc_shortstr_concat(var dests:shortstring;const s1,s2:shortstring);compilerproc;
var
  tmpres: ShortstringClass;
  s1l, s2l: longint;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if (s1l+s2l)>high(dests) then
    begin
      if s1l>high(dests) then
        s1l:=high(dests);
      s2l:=high(dests)-s1l;
    end;
  if ShortstringClass(@dests)=ShortstringClass(@s1) then
    JLSystem.ArrayCopy(JLObject(ShortstringClass(@s2).fdata),0,JLObject(ShortstringClass(@dests).fdata),s1l,s2l)
  else if ShortstringClass(@dests)=ShortstringClass(@s2) then
    begin
      JLSystem.ArrayCopy(JLObject(ShortstringClass(@dests).fdata),0,JLObject(ShortstringClass(@dests).fdata),s1l,s2l);
      JLSystem.ArrayCopy(JLObject(ShortstringClass(@s1).fdata),0,JLObject(ShortstringClass(@dests).fdata),0,s1l);
    end
  else
    begin
      JLSystem.ArrayCopy(JLObject(ShortstringClass(@s1).fdata),0,JLObject(ShortstringClass(@dests).fdata),0,s1l);
      JLSystem.ArrayCopy(JLObject(ShortstringClass(@s2).fdata),0,JLObject(ShortstringClass(@dests).fdata),s1l,s2l)
    end;
  ShortstringClass(@dests).curlen:=s1l+s2l;
end;


procedure fpc_shortstr_concat_multi(var dests:shortstring;const sarr:array of ShortstringClass);compilerproc;
var
  s2l : byte;
  LowStart,i,
  Len : longint;
  needtemp : boolean;
  tmpstr  : shortstring;
  p,pdest  : ShortstringClass;
begin
  if high(sarr)=0 then
    begin
      DestS:='';
      exit;
    end;
  lowstart:=low(sarr);
  if ShortstringClass(@DestS)=sarr[lowstart] then
    inc(lowstart);
  { Check for another reuse, then we can't use
    the append optimization and need to use a temp }
  needtemp:=false;
  for i:=lowstart to high(sarr) do
    begin
      if ShortstringClass(@DestS)=sarr[i] then
        begin
          needtemp:=true;
          break;
        end;
    end;
  if needtemp then
    begin
      lowstart:=low(sarr);
      tmpstr:='';
      pdest:=ShortstringClass(@tmpstr)
    end
  else
    begin
      { Start with empty DestS if we start with concatting
        the first array element }
      if lowstart=low(sarr) then
        DestS:='';
      pdest:=ShortstringClass(@DestS);
    end;
  { Concat all strings, except the string we already
    copied in DestS }
  Len:=pdest.curlen;
  for i:=lowstart to high(sarr) do
    begin
      p:=sarr[i];
      if assigned(p) then
        begin
          s2l:=p.curlen;
          if Len+s2l>high(dests) then
            s2l:=high(dests)-Len;
          JLSystem.ArrayCopy(JLObject(p.fdata),0,JLObject(pdest.fdata),len,s2l);
          inc(Len,s2l);
        end;
    end;
  pdest.curlen:=len;
  if needtemp then
    DestS:=TmpStr;
end;


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}
procedure fpc_shortstr_append_shortstr(var s1:shortstring;const s2:shortstring); compilerproc;
var
  s1l, s2l : integer;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>high(s1) then
    s2l:=high(s1)-s1l;
  JLSystem.ArrayCopy(JLObject(ShortstringClass(@s2).fdata),0,JLObject(ShortstringClass(@s1).fdata),s1l,s2l);
  s1[0]:=chr(s1l+s2l);
end;


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}
function fpc_shortstr_compare(const left,right:shortstring) : longint; compilerproc;
Var
  MaxI,Temp, i : SizeInt;
begin
  if ShortstringClass(@left)=ShortstringClass(@right) then
    begin
      result:=0;
      exit;
    end;
  Maxi:=Length(left);
  temp:=Length(right);
  If MaxI>Temp then
    MaxI:=Temp;
  if MaxI>0 then
    begin
      for i:=0 to MaxI-1 do
        begin
          result:=ord(ShortstringClass(@left).fdata[i])-ord(ShortstringClass(@right).fdata[i]);
          if result<>0 then
            exit;
        end;
      result:=Length(left)-Length(right);
    end
  else
    result:=Length(left)-Length(right);
end;


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE_EQUAL}

function fpc_shortstr_compare_intern(const left,right:shortstring) : longint; external name 'fpc_shortstr_compare';

function fpc_shortstr_compare_equal(const left,right:shortstring) : longint; compilerproc;
begin
  { perform normal comparsion, because JUArrays.equals() only returns true if
    the arrays have equal length, while we only want to compare curlen bytes }
  result:=fpc_shortstr_compare_intern(left,right);
end;


{$define FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}
procedure fpc_chararray_to_shortstr(out res : shortstring;const arr: array of AnsiChar; zerobased: boolean = true); compilerproc;
var
 l: longint;
 index: longint;
 len: byte;
 foundnull: boolean;
begin
  l:=high(arr)+1;
  if l>=high(res)+1 then
    l:=high(res)
  else if l<0 then
    l:=0;
  if zerobased then
    begin
      foundnull:=false;
      for index:=low(arr) to l-1 do
        if arr[index]=#0 then
          begin
            foundnull:=true;
            break;
          end;
      if not foundnull then
        len:=l
      else
        len:=index;
    end
  else
    len:=l;
  JLSystem.ArrayCopy(JLObject(@arr),0,JLObject(ShortstringClass(@res).fdata),0,len);
  ShortstringClass(@res).curlen:=len;
end;


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_TO_CHARARRAY}
procedure fpc_shortstr_to_chararray(out res: array of AnsiChar; const src: ShortString); compilerproc;
var
  len: longint;
begin
  len:=length(src);
  if len>length(res) then
    len:=length(res);
  { make sure we don't access char 1 if length is 0 (JM) }
  if len>0 then
    JLSystem.ArrayCopy(JLObject(ShortstringClass(@src).fdata),0,JLObject(@res),0,len);
  if len<=high(res) then
    JUArrays.fill(TJByteArray(@res),len,high(res),0);
end;


{****************************************************************************
                                 Str()
****************************************************************************}

{$define FPC_SYSTEM_HAS_INT_STR_LONGINT}
procedure int_str(l:longint;out s:shortstring);
  begin
    s:=unicodestring(JLInteger.valueOf(l).toString);
  end;


{$define FPC_SYSTEM_HAS_INT_STR_LONGWORD}
procedure int_str_unsigned(l:longword;out s:shortstring);
  begin
    s:=unicodestring(JLLong.valueOf(l).toString);
  end;


{$define FPC_SYSTEM_HAS_INT_STR_INT64}
procedure int_str(l:int64;out s:shortstring);
  begin
    s:=unicodestring(JLLong.valueOf(l).toString);
  end;


{$define FPC_SYSTEM_HAS_INT_STR_QWORD}
procedure int_str_unsigned(l:qword;out s:shortstring);
var
  tmp: int64;
  tmpstr: JLString;
  bi: JMBigInteger;
begin
  tmp:=int64(l);
  tmpstr:=JLLong.valueOf(tmp and $7fffffffffffffff).toString;
  if tmp<0 then
    begin
      { no unsigned 64 bit types in Java -> use big integer to add
        high(int64) to the string representation }
      bi:=JMBigInteger.Create(tmpstr);
      bi:=bi.add(JMBigInteger.Create('9223372036854775808'));
      tmpstr:=bi.toString;
    end;
  s:=unicodestring(tmpstr);
end;


{ lies... }
{$define FPC_SYSTEM_HAS_ODD_LONGWORD}
{$define FPC_SYSTEM_HAS_ODD_QWORD}
{$define FPC_SYSTEM_HAS_SQR_QWORD}

