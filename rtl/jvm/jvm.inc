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
    softfloat_exception_mask:=[float_flag_underflow, float_flag_inexact, float_flag_denormal];
  end;

{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    softfloat_exception_flags:=[];
  end;


procedure fpc_cpuinit;
  begin
    SysResetFPU;
    if not(IsLibrary) then
      SysInitFPU;
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;
  begin
    result:=nil;
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;
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

{$push}
{$q-,r-}

procedure fillchar(var arr: array of jbyte; len: sizeint; val: byte);
  begin
    JUArrays.fill(arr,0,len,jbyte(val));
  end;

{ boolean maps to a different signature }
procedure fillchar(var arr: array of jbyte; len: sizeint; val: jboolean);
  begin
    JUArrays.fill(arr,0,len,jbyte(val));
  end;

{ don't define since the signature would be the same as the one above (well,
  we could cheat by changing the case since the JVM is case-sensitive, but
  this way we also save on code size) -> map it to the byte version via
  "external" }

procedure fillchar(var arr: array of boolean; len: sizeint; val: byte);
  begin
    JUArrays.fill(TJBooleanArray(@arr),0,len,jboolean(val));
  end;

procedure fillchar(var arr: array of boolean; len: sizeint; val: boolean);
  begin
    JUArrays.fill(TJBooleanArray(@arr),0,len,val);
  end;


procedure fillchar(var arr: array of jshort; len: sizeint; val: byte);
  var
    w: jshort;
  begin
    w:=(val shl 8) or val;
    JUArrays.fill(arr,0,len div 2,w);
    if (len and 1) <> 0 then
      arr[len div 2 + 1]:=(arr[len div 2 + 1] and $ff) or (val shl 8);
  end;

procedure fillchar(var arr: array of jshort; len: sizeint; val: boolean);
  begin
    fillchar(arr,len,jbyte(val));
  end;

{ widechar maps to a different signature }
procedure fillchar(var arr: array of widechar; len: sizeint; val: byte);
  var
    w: widechar;
  begin
    w:=widechar((val shl 8) or val);
    JUArrays.fill(arr,0,len div 2,w);
    { jvm is big endian -> set top byte of last word }
    if (len and 1) <> 0 then
      arr[len shr 1+1]:=widechar((ord(arr[len shr 1+1]) and $ff) or (val shl 8));
  end;

procedure fillchar(var arr: array of widechar; len: sizeint; val: boolean);
  begin
    fillchar(arr,len,byte(val));
  end;

procedure fillchar(var arr: array of jint; len: sizeint; val: byte);
  var
    d, dmask: jint;
  begin
    d:=(val shl 8) or val;
    d:=(d shl 16) or d;
    JUArrays.fill(arr,0,len div 4,d);
    len:=len and 3;
    if len<>0 then
      begin
        dmask:=not((1 shl (32-8*len))-1);
        d:=d and dmask;
        arr[len shr 2+1]:=(arr[len shr 2+1] and not(dmask)) or d;
      end;
  end;

procedure fillchar(var arr: array of jint; len: sizeint; val: boolean);
  begin
    fillchar(arr,len,jbyte(val));
  end;


procedure fillchar(var arr: array of jlong; len: sizeint; val: byte);
  var
    i, imask: jlong;
  begin
    i:=(val shl 8) or val;
    i:=cardinal(i shl 16) or i;
    i:=(i shl 32) or i;
    JUArrays.fill(arr,0,len shr 3,i);
    len:=len and 7;
    if len<>0 then
      begin
        imask:=not((jlong(1) shl (64-8*len))-1);
        i:=i and imask;
        arr[len shr 3+1]:=(arr[len shr 3+1] and not(imask)) or i;
      end;
  end;

procedure fillchar(var arr: array of jlong; len: sizeint; val: boolean);
  begin
    fillchar(arr,len,jbyte(val));
  end;

{$pop}

{$define FPC_SYSTEM_HAS_FILLWORD}
{$define FPC_SYSTEM_HAS_FILLDWORD}
{$define FPC_SYSTEM_HAS_FILLQWORD}

{$define FPC_SYSTEM_HAS_INDEXBYTE}

function  IndexByte(const buf: array of jbyte;len:SizeInt;b:jbyte):SizeInt;
  var
    i: SizeInt;
  begin
    if len<0 then
      len:=high(buf);
    for i:=0 to len do
      if buf[i]=b then
        exit(i);
    IndexByte:=-1;
  end;


function  IndexByte(const buf: array of boolean;len:SizeInt;b:jbyte):SizeInt;
var
  i: SizeInt;
begin
  if len<0 then
    len:=high(buf);
  for i:=0 to len do
    if jbyte(buf[i])=b then
      exit(i);
  IndexByte:=-1;
end;


function  IndexChar(const buf: array of boolean;len:SizeInt;b:ansichar):SizeInt;
  begin
    IndexChar:=IndexByte(buf,len,jbyte(b));
  end;


function  IndexChar(const buf: array of jbyte;len:SizeInt;b:ansichar):SizeInt;
begin
  IndexChar:=IndexByte(buf,len,jbyte(b));
end;

{$define FPC_SYSTEM_HAS_INDEXWORD}

function  IndexWord(const buf: array of jshort;len:SizeInt;b:jshort):SizeInt;
  var
    i: SizeInt;
  begin
    if len<0 then
      len:=high(buf);
    for i:=0 to len do
      if buf[i]=b then
        exit(i);
    IndexWord:=-1;
  end;


function  IndexWord(const buf: array of jchar;len:SizeInt;b:jchar):SizeInt;
  var
    i: SizeInt;
  begin
    if len<0 then
      len:=high(buf);
    for i:=0 to len do
      if buf[i]=b then
        exit(i);
    IndexWord:=-1;
  end;

function  IndexWord(const buf: array of jchar;len:SizeInt;b:jshort):SizeInt;
  var
    i: SizeInt;
    c: jchar;
  begin
    c:=jchar(b);
    if len<0 then
      len:=high(buf);
    for i:=0 to len do
      if buf[i]=c then
        exit(i);
    IndexWord:=-1;
  end;

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
  if len>0 then
    JLSystem.ArrayCopy(JLObject(ShortstringClass(@sstr).fdata),0,JLObject(ShortstringClass(@res).fdata),0,len);
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
  if s2l>0 then
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
  if len>0 then
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

