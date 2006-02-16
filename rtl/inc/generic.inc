{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Processor independent implementation for the system unit
    (adapted for intel i386.inc file)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                               Primitives
****************************************************************************}
type
  pstring = ^shortstring;

{$ifndef FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE'];
type
  bytearray    = array [0..high(sizeint)-1] of byte;
var
  i:longint;
begin
  if count <= 0 then exit;
  Dec(count);
  if @source<@dest then
    begin
      for i:=count downto 0 do
        bytearray(dest)[i]:=bytearray(source)[i];
    end
  else
    begin
      for i:=0 to count do
        bytearray(dest)[i]:=bytearray(source)[i];
    end;
end;
{$endif not FPC_SYSTEM_HAS_MOVE}


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:SizeInt;value:byte);
type
  longintarray = array [0..high(sizeint) div 4-1] of longint;
  bytearray    = array [0..high(sizeint)-1] of byte;
var
  i,v : longint;
begin
  if count <= 0 then exit;
  v := 0;
  { aligned? }
  if (PtrUInt(@x) mod sizeof(PtrUInt))<>0 then
    begin
      for i:=0 to count-1 do
        bytearray(x)[i]:=value;
    end
  else
    begin
      v:=(value shl 8) or (value and $FF);
      v:=(v shl 16) or (v and $ffff);
      for i:=0 to (count div 4)-1 do
        longintarray(x)[i]:=v;
      for i:=(count div 4)*4 to count-1 do
        bytearray(x)[i]:=value;
    end;
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef FPC_SYSTEM_HAS_FILLBYTE}
procedure FillByte (var x;count : SizeInt;value : byte );
begin
  FillChar (X,Count,CHR(VALUE));
end;
{$endif not FPC_SYSTEM_HAS_FILLBYTE}


{$ifndef FPC_SYSTEM_HAS_FILLWORD}
procedure fillword(var x;count : SizeInt;value : word);
type
  longintarray = array [0..high(sizeint) div 4-1] of longint;
  wordarray    = array [0..high(sizeint) div 2-1] of word;
var
  i,v : longint;
begin
  if Count <= 0 then exit;
  { aligned? }
  if (PtrUInt(@x) mod sizeof(PtrUInt))<>0 then
    begin
      for i:=0 to count-1 do
        wordarray(x)[i]:=value;
    end
  else
    begin
      v:=value*$10000+value;
      for i:=0 to (count div 2) -1 do
        longintarray(x)[i]:=v;
      for i:=(count div 2)*2 to count-1 do
        wordarray(x)[i]:=value;
    end;
end;
{$endif not FPC_SYSTEM_HAS_FILLWORD}


{$ifndef FPC_SYSTEM_HAS_FILLDWORD}
procedure FillDWord(var x;count : SizeInt;value : DWord);
type
  longintarray = array [0..high(sizeint) div 4-1] of longint;
begin
  if count <= 0 then exit;
  while Count<>0 do
   begin
     { range checking must be disabled here }
     longintarray(x)[count-1]:=longint(value);
     Dec(count);
   end;
end;
{$endif FPC_SYSTEM_HAS_FILLDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXCHAR}
function IndexChar(Const buf;len:SizeInt;b:char):SizeInt;
begin
  IndexChar:=IndexByte(Buf,Len,byte(B));
end;
{$endif not FPC_SYSTEM_HAS_INDEXCHAR}


{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(Const buf;len:SizeInt;b:byte):SizeInt;
type
  bytearray    = array [0..high(sizeint)-1] of byte;
var
  I : longint;
begin
  I:=0;
  { simulate assembler implementations behaviour, which is expected }
  { fpc_pchar_to_ansistr in astrings.inc                            }
  if (len < 0) then
    len := high(longint);
  while (I<Len) and (bytearray(buf)[I]<>b) do
   inc(I);
  if (i=Len) then
   i:=-1;                      {Can't use 0, since it is a possible value}
  IndexByte:=I;
end;
{$endif not FPC_SYSTEM_HAS_INDEXBYTE}


{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
function Indexword(Const buf;len:SizeInt;b:word):SizeInt;
type
  wordarray    = array [0..high(sizeint) div 2-1] of word;
var
  I : longint;
begin
  I:=0;
  if (len < 0) then
    len := high(longint);
  while (I<Len) and (wordarray(buf)[I]<>b) do
   inc(I);
  if (i=Len) then
   i:=-1;           {Can't use 0, since it is a possible value for index}
  Indexword:=I;
end;
{$endif not FPC_SYSTEM_HAS_INDEXWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(Const buf;len:SizeInt;b:DWord):SizeInt;
type
  dwordarray = array [0..high(sizeint) div 4-1] of dword;
var
  I : longint;
begin
  I:=0;
  if (len < 0) then
    len := high(longint);
  while (I<Len) and (dwordarray(buf)[I]<>b) do
    inc(I);
  if (i=Len) then
   i:=-1;           {Can't use 0, since it is a possible value for index}
  IndexDWord:=I;
end;
{$endif not FPC_SYSTEM_HAS_INDEXDWORD}


{$ifndef FPC_SYSTEM_HAS_COMPARECHAR}
function CompareChar(Const buf1,buf2;len:SizeInt):SizeInt;
begin
  CompareChar:=CompareByte(buf1,buf2,len);
end;
{$endif not FPC_SYSTEM_HAS_COMPARECHAR}


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(Const buf1,buf2;len:SizeInt):SizeInt;
type
  bytearray = array [0..high(sizeint)-1] of byte;
var
  I : longint;
begin
  I:=0;
  if (Len<>0) and (@Buf1<>@Buf2) then
   begin
     while (bytearray(Buf1)[I]=bytearray(Buf2)[I]) and (I<Len) do
      inc(I);
     if I=Len then  {No difference}
      I:=0
     else
      begin
        I:=bytearray(Buf1)[I]-bytearray(Buf2)[I];
        if I>0 then
         I:=1
        else
         if I<0 then
          I:=-1;
      end;
   end;
  CompareByte:=I;
end;
{$endif not FPC_SYSTEM_HAS_COMPAREBYTE}


{$ifndef FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(Const buf1,buf2;len:SizeInt):SizeInt;
type
  wordarray = array [0..high(sizeint) div 2-1] of word;
var
  I : longint;
begin
  I:=0;
  if (Len<>0) and (@Buf1<>@Buf2) then
   begin
     while (wordarray(Buf1)[I]=wordarray(Buf2)[I]) and (I<Len) do
      inc(I);
     if I=Len then  {No difference}
      I:=0
     else
      begin
        I:=wordarray(Buf1)[I]-wordarray(Buf2)[I];
        if I>0 then
         I:=1
        else
         if I<0 then
          I:=-1;
      end;
   end;
  CompareWord:=I;
end;
{$endif not FPC_SYSTEM_HAS_COMPAREWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(Const buf1,buf2;len:SizeInt):SizeInt;
type
  longintarray = array [0..high(sizeint) div 4-1] of longint;
var
  I : longint;
begin
  I:=0;
  if (Len<>0) and (@Buf1<>@Buf2) then
   begin
     while (longintarray(Buf1)[I]=longintarray(Buf2)[I]) and (I<Len) do
      inc(I);
     if I=Len then  {No difference}
      I:=0
     else
      begin
        I:=longintarray(Buf1)[I]-longintarray(Buf2)[I];
        if I>0 then
         I:=1
        else
         if I<0 then
          I:=-1;
      end;
   end;
  CompareDWord:=I;
end;
{$endif ndef FPC_SYSTEM_HAS_COMPAREDWORD}


{$ifndef FPC_SYSTEM_HAS_MOVECHAR0}
procedure MoveChar0(Const buf1;var buf2;len:SizeInt);
var
  I : longint;
begin
  if Len = 0 then exit;
  I:=IndexByte(Buf1,Len,0);
  if I<>-1 then
    Move(Buf1,Buf2,I)
  else
    Move(Buf1,Buf2,len);
end;
{$endif ndef FPC_SYSTEM_HAS_MOVECHAR0}


{$ifndef FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(Const buf;len:SizeInt;b:Char):SizeInt;
var
  I : longint;
begin
  if Len<>0 then
   begin
     I:=IndexByte(Buf,Len,0);
     If (I=-1) then
       I:=Len;
     IndexChar0:=IndexByte(Buf,I,byte(b));
   end
  else
   IndexChar0:=0;
end;
{$endif ndef FPC_SYSTEM_HAS_INDEXCHAR0}


{$ifndef FPC_SYSTEM_HAS_COMPARECHAR0}
function CompareChar0(Const buf1,buf2;len:SizeInt):SizeInt;
type
  bytearray = array [0..high(sizeint)-1] of byte;
var
  i : longint;
begin
  I:=0;
  if (Len<>0) and (@Buf1<>@Buf2) then
   begin
     while (I<Len) And
           ((Pbyte(@Buf1)[i]<>0) and (PByte(@buf2)[i]<>0)) and
           (pbyte(@Buf1)[I]=pbyte(@Buf2)[I])  do
      inc(I);
     if (I=Len) or
        (PByte(@Buf1)[i]=0) or
        (PByte(@buf2)[I]=0) then  {No difference or 0 reached }
      I:=0
     else
      begin
        I:=bytearray(Buf1)[I]-bytearray(Buf2)[I];
        if I>0 then
         I:=1
        else
         if I<0 then
          I:=-1;
      end;
   end;
  CompareChar0:=I;
end;
{$endif not FPC_SYSTEM_HAS_COMPARECHAR0}


{****************************************************************************
                              Object Helpers
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}
{ Note: _vmt will be reset to -1 when memory is allocated,
  this is needed for fpc_help_fail }
function fpc_help_constructor(_self:pointer;var _vmt:pointer;_vmt_pos:cardinal):pointer;[public,alias:'FPC_HELP_CONSTRUCTOR'];compilerproc;
type
  ppointer = ^pointer;
  pvmt = ^tvmt;
  tvmt=packed record
    size,msize:ptrint;
    parent:pointer;
  end;
var
  vmtcopy : pointer;
begin
  { Inherited call? }
  if _vmt=nil then
    begin
      fpc_help_constructor:=_self;
      exit;
    end;
  vmtcopy:=_vmt;

  if (_self=nil) and
     (pvmt(_vmt)^.size>0) then
    begin
      getmem(_self,pvmt(_vmt)^.size);
      { reset vmt needed for fail }
      _vmt:=pointer(-1);
    end;
  if _self<>nil then
    begin
      fillchar(_self^,pvmt(vmtcopy)^.size,#0);
      ppointer(_self+_vmt_pos)^:=vmtcopy;
    end;
  fpc_help_constructor:=_self;
end;
{$endif FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}


{$ifndef FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}
{ Note: _self will not be reset, the compiler has to generate the reset }
procedure fpc_help_destructor(_self,_vmt:pointer;vmt_pos:cardinal);[public,alias:'FPC_HELP_DESTRUCTOR'];  compilerproc;
type
  ppointer = ^pointer;
  pvmt = ^tvmt;
  tvmt = packed record
    size,msize : ptrint;
    parent : pointer;
  end;
begin
   { already released? }
   if (_self=nil) or
      (_vmt=nil) or
      (ppointer(_self+vmt_pos)^=nil) then
     exit;
   if (pvmt(ppointer(_self+vmt_pos)^)^.size=0) or
      (pvmt(ppointer(_self+vmt_pos)^)^.size+pvmt(ppointer(_self+vmt_pos)^)^.msize<>0) then
     RunError(210);
   { reset vmt to nil for protection }
   ppointer(_self+vmt_pos)^:=nil;
   freemem(_self);
end;
{$endif FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}


{$ifndef FPC_SYSTEM_HAS_FPC_HELP_FAIL}
{ Note: _self will not be reset, the compiler has to generate the reset }
procedure fpc_help_fail(_self:pointer;var _vmt:pointer;vmt_pos:cardinal);[public,alias:'FPC_HELP_FAIL'];compilerproc;
type
  ppointer = ^pointer;
  pvmt = ^tvmt;
  tvmt = packed record
    size,msize : ptrint;
    parent : pointer;
  end;
begin
   if (_self=nil) or (_vmt=nil) then
     exit;
   { vmt=-1 when memory was allocated }
   if ptrint(_vmt)=-1 then
     begin
       if (_self=nil) or (ppointer(_self+vmt_pos)^=nil) then
         HandleError(210)
       else
         begin
           ppointer(_self+vmt_pos)^:=nil;
           freemem(_self);
           { reset _vmt to nil so it will not be freed a
             second time }
           _vmt:=nil;
         end;
     end
   else
     ppointer(_self+vmt_pos)^:=nil;
end;
{$endif FPC_SYSTEM_HAS_FPC_HELP_FAIL}

{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}
procedure fpc_check_object(_vmt : pointer); [public,alias:'FPC_CHECK_OBJECT'];  compilerproc;
type
  pvmt = ^tvmt;
  tvmt = packed record
    size,msize : ptrint;
    parent : pointer;
  end;
begin
  if (_vmt=nil) or
     (pvmt(_vmt)^.size=0) or
     (pvmt(_vmt)^.size+pvmt(_vmt)^.msize<>0) then
    RunError(210);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}


{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}
{ checks for a correct vmt pointer }
{ deeper check to see if the current object is }
{ really related to the true }
procedure fpc_check_object_ext(vmt, expvmt : pointer); [public,alias:'FPC_CHECK_OBJECT_EXT']; compilerproc;
type
  pvmt = ^tvmt;
  tvmt = packed record
    size,msize : ptrint;
    parent : pointer;
  end;
begin
   if (vmt=nil) or
      (pvmt(vmt)^.size=0) or
      (pvmt(vmt)^.size+pvmt(vmt)^.msize<>0) then
        RunError(210);
   while assigned(vmt) do
     if vmt=expvmt then
       exit
     else
       vmt:=pvmt(vmt)^.parent;
   RunError(219);
end;
{$endif not FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}


{****************************************************************************
                                 String
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}

function fpc_shortstr_to_shortstr(len:longint;const sstr:shortstring): shortstring;[public,alias:'FPC_SHORTSTR_TO_SHORTSTR']; compilerproc;
var
  slen : byte;
begin
  slen:=length(sstr);
  if slen<len then
    len:=slen;
  move(sstr[0],result[0],len+1);
  if slen>len then
    result[0]:=chr(len);
end;

procedure fpc_shortstr_assign(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_ASSIGN']; {$ifdef HAS_COMPILER_PROC} compilerproc; {$endif}
var
  slen : byte;
type
  pstring = ^string;
begin
  slen:=length(pstring(sstr)^);
  if slen<len then
    len:=slen;
  move(sstr^,dstr^,len+1);
  if slen>len then
    pchar(dstr)^:=chr(len);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

function fpc_shortstr_concat(const s1,s2:shortstring): shortstring;[public,alias:'FPC_SHORTSTR_CONCAT']; compilerproc;
var
  s1l, s2l : byte;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>255 then
    s2l:=255-s1l;
  move(s1[1],fpc_shortstr_concat[1],s1l);
  move(s2[1],fpc_shortstr_concat[s1l+1],s2l);
  fpc_shortstr_concat[0]:=chr(s1l+s2l);
end;
{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}


{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}
procedure fpc_shortstr_append_shortstr(var s1:shortstring;const s2:shortstring);compilerproc;
    [public,alias:'FPC_SHORTSTR_APPEND_SHORTSTR'];
var
  s1l, s2l : byte;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>high(s1) then
    s2l:=high(s1)-s1l;
  move(s2[1],s1[s1l+1],s2l);
  s1[0]:=chr(s1l+s2l);
end;
{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}


{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}
function fpc_shortstr_compare(const left,right:shortstring) : longint;[public,alias:'FPC_SHORTSTR_COMPARE']; compilerproc;
var
   s1,s2,max,i : byte;
   d : longint;
begin
  s1:=length(left);
  s2:=length(right);
  if s1<s2 then
    max:=s1
  else
    max:=s2;
  for i:=1 to max do
    begin
     d:=byte(left[i])-byte(right[i]);
     if d>0 then
       exit(1)
     else if d<0 then
       exit(-1);
    end;
  if s1>s2 then
    exit(1)
  else if s1<s2 then
    exit(-1)
  else
    exit(0);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}

{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}

function fpc_pchar_to_shortstr(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
var
  l : longint;
  s: shortstring;
begin
  if p=nil then
    l:=0
  else
    l:=strlen(p);
  if l>255 then
    l:=255;
  if l>0 then
    move(p^,s[1],l);
  s[0]:=chr(l);
  fpc_pchar_to_shortstr := s;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}


{$ifndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}

function fpc_chararray_to_shortstr(const arr: array of char; zerobased: boolean = true):shortstring;[public,alias:'FPC_CHARARRAY_TO_SHORTSTR']; compilerproc;
var
 l: longint;
 index: longint;
 len: byte;
begin
  l := high(arr)+1;
  if l>=256 then
    l:=255
  else if l<0 then
    l:=0;
  if (zerobased) then
    begin
      index:=IndexByte(arr[0],l,0);
      if (index < 0) then
        len := l
      else
        len := index;
    end
  else
    len := l;
  move(arr[0],fpc_chararray_to_shortstr[1],len);
  fpc_chararray_to_shortstr[0]:=chr(len);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}


{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_TO_CHARARRAY}

{ inside the compiler, the resulttype is modified to that of the actual }
{ chararray we're converting to (JM)                                    }
function fpc_shortstr_to_chararray(arraysize: longint; const src: ShortString): fpc_big_chararray;[public,alias: 'FPC_SHORTSTR_TO_CHARARRAY']; compilerproc;
var
  len: longint;
begin
  len := length(src);
  if len > arraysize then
    len := arraysize;
  { make sure we don't access char 1 if length is 0 (JM) }
  if len > 0 then
    move(src[1],fpc_shortstr_to_chararray[0],len);
  fillchar(fpc_shortstr_to_chararray[len],arraysize-len,0);
end;

{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_TO_CHARARRAY}

{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}

function fpc_pchar_length(p:pchar):longint;[public,alias:'FPC_PCHAR_LENGTH']; compilerproc;
var i : longint;
begin
  i:=0;
  while p[i]<>#0 do inc(i);
  exit(i);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}

{$ifndef FPC_SYSTEM_HAS_FPC_PWIDECHAR_LENGTH}

function fpc_pwidechar_length(p:pwidechar):longint;[public,alias:'FPC_PWIDECHAR_LENGTH']; compilerproc;
var i : longint;
begin
  i:=0;
  while p[i]<>#0 do inc(i);
  exit(i);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PWIDECHAR_LENGTH}

{****************************************************************************
                       Caller/StackFrame Helpers
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_GET_FRAME}
{_$error Get_frame must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_FRAME}

{$ifndef FPC_SYSTEM_HAS_GET_CALLER_ADDR}
{_$error Get_caller_addr must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_CALLER_ADDR}

{$ifndef FPC_SYSTEM_HAS_GET_CALLER_FRAME}
{_$error Get_caller_frame must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_CALLER_FRAME}

{****************************************************************************
                                 Math
****************************************************************************}

{****************************************************************************
                          Software longint/dword division
****************************************************************************}
{$ifdef FPC_INCLUDE_SOFTWARE_MOD_DIV}

function count_leading_zeros_32bit(l : longint) : longint;
  var
    i : longint;
  begin
     for i:=0 to 31 do
       begin
          if (l and (longint($80000000) shr i))<>0 then
            begin
               result:=i;
               exit;
            end;
       end;
     result:=i;
  end;


{$ifndef FPC_SYSTEM_HAS_DIV_DWORD}
function fpc_div_dword(n,z : dword) : dword; [public,alias: 'FPC_DIV_DWORD']; compilerproc;
  var
     shift,lzz,lzn : longint;
  begin
     result:=0;
     if n=0 then
       HandleErrorFrame(200,get_frame);
     lzz:=count_leading_zeros_32bit(z);
     lzn:=count_leading_zeros_32bit(n);
     { if the denominator contains less zeros
       then the numerator
       the d is greater than the n }
     if lzn<lzz then
       exit;
     shift:=lzn-lzz;
     n:=n shl shift;
     repeat
       if z>=n then
         begin
            z:=z-n;
            result:=result+dword(1 shl shift);
         end;
       dec(shift);
       n:=n shr 1;
     until shift<0;
  end;
{$endif FPC_SYSTEM_HAS_DIV_DWORD}


{$ifndef FPC_SYSTEM_HAS_MOD_DWORD}
function fpc_mod_dword(n,z : dword) : dword; [public,alias: 'FPC_MOD_DWORD']; compilerproc;
  var
     shift,lzz,lzn : longint;
  begin
    result:=0;
    if n=0 then
      HandleErrorFrame(200,get_frame);
    lzz:=count_leading_zeros_32bit(z);
    lzn:=count_leading_zeros_32bit(n);
    { if the denominator contains less zeros
      then the numerator
      the d is greater than the n }
    if lzn<lzz then
      begin
         result:=z;
         exit;
      end;
    shift:=lzn-lzz;
    n:=n shl shift;
    repeat
      if z>=n then
        z:=z-n;
      dec(shift);
      n:=n shr 1;
    until shift<0;
    result:=z;
  end;
{$endif FPC_SYSTEM_HAS_MOD_DWORD}


{$ifndef FPC_SYSTEM_HAS_DIV_LONGINT}
function fpc_div_longint(n,z : longint) : longint; [public,alias: 'FPC_DIV_LONGINT']; compilerproc;
  var
     sign : boolean;
     d1,d2 : dword;
  begin
     if n=0 then
       HandleErrorFrame(200,get_frame);
     sign:=false;
     if z<0 then
       begin
          sign:=not(sign);
          d1:=dword(-z);
       end
     else
       d1:=z;
     if n<0 then
       begin
          sign:=not(sign);
          d2:=dword(-n);
       end
     else
       d2:=n;

     { the div is coded by the compiler as call to divdword }
     if sign then
       result:=-(d1 div d2)
     else
       result:=d1 div d2;
  end;
{$endif FPC_SYSTEM_HAS_DIV_LONGINT}


{$ifndef FPC_SYSTEM_HAS_MOD_LONGINT}
function fpc_mod_longint(n,z : longint) : longint; [public,alias: 'FPC_MOD_LONGINT']; compilerproc;
  var
     signed : boolean;
     r,nq,zq : dword;
  begin
     if n=0 then
       HandleErrorFrame(200,get_frame);
     nq:=abs(n);

     if z<0 then
       begin
          zq:=dword(-z);
          signed:=true;
       end
     else
       begin
         zq:=z;
         signed:=false;
       end;

     r:=zq mod nq;
     if signed then
       result:=-longint(r)
     else
       result:=r;
  end;
{$endif FPC_SYSTEM_HAS_MOD_LONGINT}

{$endif FPC_INCLUDE_SOFTWARE_MOD_DIV}


{****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_ABS_LONGINT}
function abs(l:longint):longint;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   if l<0 then
     abs:=-l
   else
     abs:=l;
end;

{$endif not FPC_SYSTEM_HAS_ABS_LONGINT}

{$ifndef FPC_SYSTEM_HAS_ODD_LONGINT}

function odd(l:longint):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   odd:=boolean(l and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_LONGINT}

{$ifndef FPC_SYSTEM_HAS_ODD_LONGWORD}

function odd(l:longword):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   odd:=boolean(l and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_LONGWORD}


{$ifndef FPC_SYSTEM_HAS_ODD_INT64}

function odd(l:int64):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   odd:=boolean(longint(l) and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_INT64}

{$ifndef FPC_SYSTEM_HAS_ODD_QWORD}

function odd(l:qword):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   odd:=boolean(longint(l) and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_QWORD}

{$ifndef FPC_SYSTEM_HAS_SQR_LONGINT}

function sqr(l:longint):longint;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   sqr:=l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_LONGINT}


{$ifndef FPC_SYSTEM_HAS_ABS_INT64}

function abs(l: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  if l < 0 then
    abs := -l
  else
    abs := l;
end;

{$endif ndef FPC_SYSTEM_HAS_ABS_INT64}


{$ifndef FPC_SYSTEM_HAS_SQR_INT64}

function sqr(l: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  sqr := l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_INT64}


{$ifndef FPC_SYSTEM_HAS_SQR_QWORD}

function sqr(l: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  sqr := l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_INT64}

{$ifndef FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
function declocked(var l:longint):boolean;
  begin
    Dec(l);
    declocked:=(l=0);
  end;
{$endif FPC_SYSTEM_HAS_DECLOCKED_LONGINT}


{$ifndef FPC_SYSTEM_HAS_DECLOCKED_INT64}
function declocked(var l:int64):boolean;
  begin
    Dec(l);
    declocked:=(l=0);
  end;
{$endif FPC_SYSTEM_HAS_DECLOCKED_INT64}


{$ifndef FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure inclocked(var l:longint);
  begin
    Inc(l);
  end;
{$endif FPC_SYSTEM_HAS_INCLOCKED_LONGINT}


{$ifndef FPC_SYSTEM_HAS_INCLOCKED_INT64}
procedure inclocked(var l:int64);
  begin
    Inc(l);
  end;
{$endif FPC_SYSTEM_HAS_INCLOCKED_INT64}


{$ifndef FPC_SYSTEM_HAS_SPTR}
{_$error Sptr must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_SPTR}



function align(addr : PtrInt;alignment : PtrInt) : PtrInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    if addr mod alignment<>0 then
      result:=addr+(alignment-(addr mod alignment))
    else
      result:=addr;
  end;


function align(addr : Pointer;alignment : PtrInt) : Pointer;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    if PtrInt(addr) mod alignment<>0 then
      result:=pointer(addr+(alignment-(PtrInt(addr) mod alignment)))
    else
      result:=addr;
  end;


{****************************************************************************
                                 Str()
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_INT_STR_LONGINT}

procedure int_str(l:longint;out s:string);

var m:cardinal;
    p:byte;

begin
  s[1]:='-';     {Will be overwritten if positive.}
  p:=byte(l<0);  {Number of characters to start with.}
  {Count number of characters.}
  m:=abs(l);
  repeat
    inc(p);
    l:=l div 10;
  until l=0;
  {Generate string.}
  s[0]:=char(p);
  repeat
    s[p]:=char(m mod 10+byte('0'));
    m:=m div 10;
    dec(p);
  until m=0;
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_LONGINT}

{$ifndef FPC_SYSTEM_HAS_INT_STR_LONGWORD}

procedure int_str(l:longword;out s:string);

var m:longword;
    p:byte;

begin
  p:=0;
  {Count number of characters.}
  m:=l;
  repeat
    inc(p);
    m:=m div 10;
  until m=0;
  {Generate string.}
  s[0]:=char(p);
  repeat
    s[p]:=char((l mod 10)+byte('0'));
    l:=l div 10;
    dec(p);
  until l=0;
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_LONGWORD}

{$ifndef FPC_SYSTEM_HAS_INT_STR_INT64}

procedure int_str(l:int64;out s:string);

var m:qword;
    p:byte;

begin
  s[1]:='-';     {Will be overwritten if positive.}
  p:=byte(l<0);  {Number of characters to start with.}
  {Count number of characters.}
  m:=abs(l);
  repeat
    inc(p);
    l:=l div 10;
  until l=0;
  {Generate string.}
  s[0]:=char(p);
  repeat
    s[p]:=char(m mod 10+byte('0'));
    m:=m div 10;
    dec(p);
  until m=0;
end;
{$endif ndef FPC_SYSTEM_HAS_INT_STR_INT64}

{$ifndef FPC_SYSTEM_HAS_INT_STR_QWORD}

procedure int_str(l:qword;out s:string);

var m:qword;
    p:byte;

begin
  p:=0;
  {Count number of characters.}
  m:=l;
  repeat
    inc(p);
    m:=m div 10;
  until m=0;
  {Generate string.}
  s[0]:=char(p);
  repeat
    s[p]:=char((l mod 10)+byte('0'));
    l:=l div 10;
    dec(p);
  until l=0;
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_QWORD}

{$ifndef FPC_SYSTEM_HAS_SYSRESETFPU}

procedure SysResetFpu;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  { nothing todo }
end;

{$endif FPC_SYSTEM_HAS_SYSRESETFPU}

