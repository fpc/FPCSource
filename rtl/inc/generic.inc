{
    $Id$
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
  bytearray    = array [0..maxlongint-1] of byte;
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
  longintarray = array [0..maxlongint div 4] of longint;
  bytearray    = array [0..maxlongint-1] of byte;
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
  longintarray = array [0..maxlongint div 4] of longint;
  wordarray    = array [0..maxlongint div 2] of word;
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
  longintarray = array [0..maxlongint div 4] of longint;
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
  bytearray    = array [0..maxlongint-1] of byte;
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
  wordarray    = array [0..maxlongint div 2] of word;
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
  dwordarray = array [0..maxlongint div 4] of dword;
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
  bytearray = array [0..maxlongint-1] of byte;
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
  wordarray = array [0..maxlongint div 2] of word;
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
  longintarray = array [0..maxlongint div 4] of longint;
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
     IndexChar0:=IndexByte(Buf,I,0);
   end
  else
   IndexChar0:=0;
end;
{$endif ndef FPC_SYSTEM_HAS_INDEXCHAR0}


{$ifndef FPC_SYSTEM_HAS_COMPARECHAR0}
function CompareChar0(Const buf1,buf2;len:SizeInt):SizeInt;
type
  bytearray = array [0..maxlongint-1] of byte;
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
function fpc_help_constructor(_self:pointer;var _vmt:pointer;_vmt_pos:cardinal):pointer;[public,alias:'FPC_HELP_CONSTRUCTOR'];{$ifdef hascompilerproc}compilerproc;{$endif}
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
procedure fpc_help_destructor(_self,_vmt:pointer;vmt_pos:cardinal);[public,alias:'FPC_HELP_DESTRUCTOR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
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


{$ifndef NOCLASSHELPERS}

{$ifndef FPC_SYSTEM_HAS_FPC_NEW_CLASS}
function fpc_new_class(_self,_vmt:pointer):pointer;[public,alias:'FPC_NEW_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  { Inherited call? }
  if _vmt=nil then
    begin
      fpc_new_class:=_self;
      exit;
    end;

  fpc_new_class := tclass(_vmt).NewInstance
end;
{$endif FPC_SYSTEM_HAS_FPC_NEW_CLASS}


{$ifndef FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}
procedure fpc_dispose_class(_self: pointer; flag : longint);[public,alias:'FPC_DISPOSE_CLASS'];compilerproc;
begin
  { inherited -> flag = 0 -> no destroy }
  { normal -> flag = 1 -> destroy }
  if (_self <> nil) and (flag = 1) then
    tobject(_self).FreeInstance;
end;
{$endif ndef FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}

{$endif NOCLASSHELPERS}


{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}
procedure fpc_check_object(_vmt : pointer);saveregisters;[public,alias:'FPC_CHECK_OBJECT'];  {$ifdef hascompilerproc} compilerproc; {$endif}
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
procedure fpc_check_object_ext(vmt, expvmt : pointer);saveregisters;[public,alias:'FPC_CHECK_OBJECT_EXT']; {$ifdef hascompilerproc} compilerproc; {$endif}
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

function fpc_shortstr_to_shortstr(len:longint;const sstr:shortstring): shortstring;[public,alias:'FPC_SHORTSTR_TO_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
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

{$ifdef interncopy}
procedure fpc_shortstr_assign(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_ASSIGN']; {$ifdef HAS_COMPILER_PROC} compilerproc; {$endif}
{$else}
procedure fpc_shortstr_copy(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_COPY']; {$ifdef HAS_COMPILER_PROC} compilerproc; {$endif}
{$endif}
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

function fpc_shortstr_concat(const s1,s2:shortstring): shortstring;[public,alias:'FPC_SHORTSTR_CONCAT']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
procedure fpc_shortstr_append_shortstr(var s1:shortstring;const s2:shortstring);{$ifdef hascompilerproc} compilerproc; {$endif}
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
function fpc_shortstr_compare(const left,right:shortstring) : longint;[public,alias:'FPC_SHORTSTR_COMPARE']; {$ifdef hascompilerproc} compilerproc; {$endif}
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

function fpc_pchar_to_shortstr(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
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

{ also add a strpas alias for internal use in the system unit (JM) }
function strpas(p:pchar):shortstring; [external name 'FPC_PCHAR_TO_SHORTSTR'];

{$ifndef FPC_UNIT_HAS_STRLEN}

{ if strlen is not yet defined, we need a forward declaration here }
function strlen(p:pchar):longint; [external name 'FPC_PCHAR_LENGTH'];
{$endif FPC_UNIT_HAS_STRLEN}

{$ifndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}

{$ifdef hascompilerproc}
function fpc_chararray_to_shortstr(const arr: array of char):shortstring;[public,alias:'FPC_CHARARRAY_TO_SHORTSTR']; compilerproc;
var
  l: longint;
{$else hascompilerproc}
function fpc_chararray_to_shortstr(arr:pchar; l : longint):shortstring;[public,alias:'FPC_CHARARRAY_TO_SHORTSTR'];
var
{$endif hascompilerproc}
 index: longint;
 len: byte;
begin
{$ifdef hascompilerproc}
  l := high(arr)+1;
{$endif hascompilerproc}
  if l>=256 then
    l:=255
  else if l<0 then
    l:=0;
  index:=IndexByte(arr[0],l,0);
  if (index < 0) then
    len := l
  else
    len := index;
  move(arr[0],fpc_chararray_to_shortstr[1],len);
  fpc_chararray_to_shortstr[0]:=chr(len);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}

{$ifdef hascompilerproc}

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

{$else hascompilerproc}

{$ifopt r+}
{$define rangeon}
{$r-}
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_STR_TO_CHARARRAY}
procedure fpc_str_to_chararray(strtyp, arraysize: longint; src,dest: pchar);[public,alias:'FPC_STR_TO_CHARARRAY'];  {$ifdef hascompilerproc} compilerproc; {$endif}
type
  plongint = ^longint;
var
  len: longint;
begin
  case strtyp of
    { shortstring }
    0:
      begin
        len := byte(src[0]);
        inc(src);
      end;
{$ifdef SUPPORT_ANSISTRING}
    { ansistring}
    1: len := length(ansistring(pointer(src)));
{$endif SUPPORT_ANSISTRING}
    { longstring }
    2:;
    { widestring }
    3: ;
  end;
  if len > arraysize then
    len := arraysize;
  { make sure we don't dereference src if it can be nil (JM) }
  if len > 0 then
    move(src^,dest^,len);
  fillchar(dest[len],arraysize-len,0);
end;

{$endif FPC_SYSTEM_HAS_FPC_STR_TO_CHARARRAY}

{$ifdef rangeon}
{$r+}
{undef rangeon}
{$endif rangeon}

{$endif hascompilerproc}

{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}

function fpc_pchar_length(p:pchar):longint;[public,alias:'FPC_PCHAR_LENGTH']; {$ifdef hascompilerproc} compilerproc; {$endif}
var i : longint;
begin
  i:=0;
  while p[i]<>#0 do inc(i);
  exit(i);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}

{$ifdef HASWIDESTRING}
{$ifndef FPC_SYSTEM_HAS_FPC_PWIDECHAR_LENGTH}

function fpc_pwidechar_length(p:pwidechar):longint;[public,alias:'FPC_PWIDECHAR_LENGTH']; {$ifdef hascompilerproc} compilerproc; {$endif}
var i : longint;
begin
  i:=0;
  while p[i]<>#0 do inc(i);
  exit(i);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PWIDECHAR_LENGTH}
{$endif HASWIDESTRING}

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
function fpc_div_dword(n,z : dword) : dword; [public,alias: 'FPC_DIV_DWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
function fpc_mod_dword(n,z : dword) : dword; [public,alias: 'FPC_MOD_DWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
function fpc_div_longint(n,z : longint) : longint; [public,alias: 'FPC_DIV_LONGINT']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
function fpc_mod_longint(n,z : longint) : longint; [public,alias: 'FPC_MOD_LONGINT']; {$ifdef hascompilerproc} compilerproc; {$endif}
  var
     signed : boolean;
     r,nq,zq : dword;
  begin
     if n=0 then
       HandleErrorFrame(200,get_frame);
     if n<0 then
       begin
          nq:=-n;
          signed:=true;
       end
     else
       begin
          signed:=false;
          nq:=n;
       end;
     if z<0 then
       begin
          zq:=dword(-z);
          signed:=not(signed);
       end
     else
       zq:=z;
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
function abs(l:longint):longint;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_abs];
begin
   if l<0 then
     abs:=-l
   else
     abs:=l;
end;

{$endif not FPC_SYSTEM_HAS_ABS_LONGINT}

{$ifndef FPC_SYSTEM_HAS_ODD_LONGINT}

function odd(l:longint):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_odd];
begin
   odd:=boolean(l and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_LONGINT}

{$ifndef FPC_SYSTEM_HAS_ODD_LONGWORD}

function odd(l:longword):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_odd];
begin
   odd:=boolean(l and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_LONGWORD}


{$ifndef FPC_SYSTEM_HAS_ODD_INT64}

function odd(l:int64):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_odd];
begin
   odd:=boolean(longint(l) and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_INT64}

{$ifndef FPC_SYSTEM_HAS_ODD_QWORD}

function odd(l:qword):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_odd];
begin
   odd:=boolean(longint(l) and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_QWORD}

{$ifndef FPC_SYSTEM_HAS_SQR_LONGINT}

function sqr(l:longint):longint;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_sqr];
begin
   sqr:=l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_LONGINT}


{$ifndef FPC_SYSTEM_HAS_ABS_INT64}

function abs(l: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_abs];
begin
  if l < 0 then
    abs := -l
  else
    abs := l;
end;

{$endif ndef FPC_SYSTEM_HAS_ABS_INT64}


{$ifndef FPC_SYSTEM_HAS_SQR_INT64}

function sqr(l: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_sqr];
begin
  sqr := l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_INT64}


{$ifndef FPC_SYSTEM_HAS_SQR_QWORD}

function sqr(l: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_sqr];
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


procedure prefetch(const mem);[internproc:in_prefetch_var];


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

procedure int_str(l : longint;var s : string);
var
  value: longint;
  negative: boolean;

  begin
     negative := false;
     s:='';
     { Workaround: }
     if l=longint($80000000) then
       begin
         s:='-2147483648';
         exit;
       end;
     { handle case where l = 0 }
     if l = 0 then
       begin
         s:='0';
         exit;
       end;
     If l < 0 then
       begin
         negative := true;
         value:=abs(l);
       end
     else
         value:=l;
     { handle non-zero case }
     while value>0 do
       begin
         s:=char((value mod 10)+ord('0'))+s;
         value := value div 10;
       end;
     if negative then
       s := '-' + s;
  end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_LONGINT}

{$ifndef FPC_SYSTEM_HAS_INT_STR_LONGWORD}

procedure int_str(l : longword;var s : string);
begin
  s:='';
  if l = 0 then
  begin
    s := '0';
    exit;
  end;
  while l>0 do
    begin
       s:=char(ord('0')+(l mod 10))+s;
       l:=l div 10;
    end;
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_LONGWORD}

{$ifndef FPC_SYSTEM_HAS_INT_STR_INT64}

procedure int_str(l : int64;var s : string);
var
  value: int64;
  negative: boolean;

  begin
     negative := false;
     s:='';
     { Workaround: }
{$ifdef ver1_0}
     if (l shr 32=$80000000) and ((l and $ffffffff)=0) then
{$else}
     if l=int64($8000000000000000) then
{$endif}
       begin
         s:='-9223372036854775808';
         exit;
       end;
     { handle case where l = 0 }
     if l = 0 then
       begin
         s:='0';
         exit;
       end;
     If l < 0 then
       begin
         negative := true;
         value:=abs(l);
       end
     else
         value:=l;
     { handle non-zero case }
     while value>0 do
       begin
         s:=char((value mod 10)+ord('0'))+s;
         value := value div 10;
       end;
     if negative then
       s := '-' + s;
  end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_INT64}

{$ifndef FPC_SYSTEM_HAS_INT_STR_QWORD}

procedure int_str(l : qword;var s : string);
begin
  s:='';
  if l = 0 then
  begin
    s := '0';
    exit;
  end;
  while l>0 do
    begin
       s:=char(ord('0')+(l mod 10))+s;
       l:=l div 10;
    end;
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_QWORD}

{$ifndef FPC_SYSTEM_HAS_SYSRESETFPU}

procedure SysResetFpu;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  { nothing todo }
end;

{$endif FPC_SYSTEM_HAS_SYSRESETFPU}

{
  $Log$
  Revision 1.82  2004-10-14 17:39:33  florian
    + added system.align
    + threadvars are now aligned

  Revision 1.81  2004/10/09 21:00:46  jonas
    + cgenmath with libc math functions. Faster than the routines in genmath
      and also have full double support (exp() only has support for values in
      the single range in genmath, for example). Used in FPC_USE_LIBC is
      defined
    * several fixes to allow compilation with -dHASINLINE, but internalerrors
      because of missing support for inlining assembler code

  Revision 1.80  2004/07/18 20:21:44  florian
    + several unicode (to/from utf-8 conversion) stuff added
    * some longint -> SizeInt changes

  Revision 1.79  2004/05/31 20:25:04  peter
    * removed warnings

  Revision 1.78  2004/05/02 15:15:45  peter
    * fix arguments for shortstr_compare

  Revision 1.77  2004/05/02 00:31:03  peter
    * fixed compile with 1.0.x

  Revision 1.76  2004/05/01 23:55:18  peter
    * replace strlenint with sizeint

  Revision 1.75  2004/05/01 20:52:50  peter
    * ValSInt fixed for 64 bit

  Revision 1.74  2004/05/01 15:26:33  jonas
    * use some more string routines from libc if FPC_USE_LIBC is used

  Revision 1.73  2004/04/29 19:50:13  peter
    * x86-64 fixes

  Revision 1.72  2004/04/28 21:01:29  florian
    * tvmt fixed (longint -> ptrint)

  Revision 1.71  2004/04/28 20:48:20  peter
    * ordinal-pointer conversions fixed

  Revision 1.70  2004/04/26 15:55:01  peter
    * FPC_MOVE alias

  Revision 1.69  2004/02/02 20:39:27  florian
    + added prefetch(const mem)

  Revision 1.68  2004/01/31 16:14:24  florian
    * alignment handling of generic fillbyte/word fixed

  Revision 1.66  2004/01/21 01:25:02  florian
    * improved generic int. div routines

  Revision 1.65  2004/01/20 23:16:56  florian
    + created generic versions of software dword/longint mod/div

  Revision 1.64  2004/01/10 17:01:29  jonas
    * changed index* to conform to the assembler implementations (interpret
      negative upper bound as maximum)

  Revision 1.63  2003/12/16 09:43:04  daniel
    * Use of 0 instead of nil fixed

  Revision 1.62  2003/12/06 13:25:30  jonas
    * fixed longint/cardinal comparison in int_str

  Revision 1.61  2003/09/03 14:09:37  florian
    * arm fixes to the common rtl code
    * some generic math code fixed
    * ...

  Revision 1.60  2003/06/01 14:50:17  jonas
    * fpc_shortstr_append_shortstr has to use high(s1) instead of 255 as
      maxlen
    + ppc version of fpc_shortstr_append_shortstr

  Revision 1.59  2003/05/26 21:18:13  peter
    * FPC_SHORTSTR_APPEND_SHORTSTR public added

  Revision 1.58  2003/05/26 19:36:46  peter
    * fpc_shortstr_concat is now the same for all targets
    * fpc_shortstr_append_shortstr added for optimized code generation

  Revision 1.57  2003/05/16 22:40:11  florian
    * fixed generic shortstr_compare

  Revision 1.56  2003/05/13 20:52:50  peter
    * extra check for self and empty objects

  Revision 1.55  2003/05/13 19:18:08  peter
    * fpc_help_fail compilerproc
    * fpc_new_class, fpc_dispose_class not needed by latest compiler

  Revision 1.54  2003/04/23 13:10:09  peter
    * remvoe objectsize loading from help_destructor
    * implement fpc_check_object
    * saveregistrers for check_object

  Revision 1.53  2003/04/02 14:05:45  peter
    * undo previous commit

  Revision 1.51  2003/03/26 00:17:34  peter
    * generic constructor/destructor fixes

  Revision 1.50  2003/02/18 17:56:06  jonas
    - removed buggy i386-specific FPC_CHARARRAY_TO_SHORTSTR
    * fixed generic FPC_CHARARRAY_TO_SHORTSTR (web bug 2382)
    * fixed some potential range errors in indexchar/word/dword

  Revision 1.49  2003/01/20 22:21:36  mazen
  * many stuff related to RTL fixed

  Revision 1.48  2003/01/09 20:14:20  florian
    * fixed helper declarations

  Revision 1.47  2003/01/07 22:04:12  mazen
  - space removed

  Revision 1.46  2003/01/06 23:04:21  mazen
  * functions headers modified in generic.inc to make it possible compiling sparc
    RTL based on generic code

  Revision 1.45  2003/01/05 21:32:35  mazen
  * fixing several bugs compiling the RTL

  Revision 1.44  2002/12/23 21:27:13  peter
    * fix wrong var names for shortstr_compare

  Revision 1.43  2002/10/20 11:51:54  carl
   * avoid crashes with negative len counts on fills/moves
   * movechar0 was wrong and did not do the behavior as
     described in docs

  Revision 1.42  2002/10/14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.41  2002/10/12 20:32:41  carl
    * RunError 220 -> RunError 219 to be more consistent with as operator

  Revision 1.40  2002/10/10 16:08:50  florian
    + several widestring/pwidechar related helpers added

  Revision 1.39  2002/10/05 14:20:16  peter
    * fpc_pchar_length compilerproc and strlen alias

  Revision 1.38  2002/10/02 18:21:51  peter
    * Copy() changed to internal function calling compilerprocs
    * FPC_SHORTSTR_COPY renamed to FPC_SHORTSTR_ASSIGN because of the
      new copy functions

  Revision 1.37  2002/09/27 21:10:40  carl
    * fix 2GB limit problem

  Revision 1.36  2002/09/13 19:13:06  carl
    * FPC_HELP_FAIL : reset _self to nil

  Revision 1.35  2002/09/10 21:29:44  jonas
    * added some missing compilerproc directives

  Revision 1.34  2002/09/07 21:08:42  carl
    * cardinal -> longword
    - remove generic boundcheck (does not exist in v1.1)

  Revision 1.33  2002/09/07 15:07:45  peter
    * old logs removed and tabs fixed

  Revision 1.32  2002/08/19 19:34:02  peter
    * SYSTEMINLINE define that will add inline directives for small
      functions and wrappers. This will be defined automaticly when
      the compiler defines the HASINLINE directive

  Revision 1.31  2002/07/29 21:28:16  florian
    * several fixes to get further with linux/ppc system unit compilation

  Revision 1.30  2002/07/29 09:23:11  jonas
    * fixed some datastructures > 2GB

  Revision 1.29  2002/07/28 21:39:28  florian
    * made abs a compiler proc if it is generic

  Revision 1.28  2002/07/28 20:43:47  florian
    * several fixes for linux/powerpc
    * several fixes to MT

  Revision 1.27  2002/06/16 08:19:03  carl
  * bugfix of FPC_NEW_CLASS (was not creating correct instance)
  + FPC_HELP_FAIL_CLASS now tested (no longer required)

  Revision 1.25  2002/05/16 19:58:05  carl
  * generic constructor implemented

  Revision 1.24  2002/03/30 13:08:54  carl
  * memory corruption bugfix in FPC_HELP_CONSTRUCTOR if object cannot be allocated

  Revision 1.23  2002/01/25 17:38:55  peter
    * add internconst for all overloaded types of Odd/Abs/Sqr

  Revision 1.22  2002/01/24 12:33:53  jonas
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
