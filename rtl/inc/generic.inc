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
procedure Move(const source;var dest;count:longint);
type
  bytearray    = array [0..maxlongint-1] of byte;
var
  i,size : longint;
begin
  if count <= 0 then exit;
  Dec(count);
  for i:=0 to count do
         bytearray(dest)[i]:=bytearray(source)[i];
end;
{$endif not FPC_SYSTEM_HAS_MOVE}


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);
type
  longintarray = array [0..maxlongint div 4] of longint;
  bytearray    = array [0..maxlongint-1] of byte;
var
  i,v : longint;
begin
  if count <= 0 then exit;
  v := 0;
  v:=(value shl 8) or (value and $FF);
  v:=(v shl 16) or (v and $ffff);
  for i:=0 to (count div 4) -1 do
    longintarray(x)[i]:=v;
  for i:=(count div 4)*4 to count-1 do
    bytearray(x)[i]:=value;
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef FPC_SYSTEM_HAS_FILLBYTE}
procedure FillByte (var x;count : longint;value : byte );
begin
  FillChar (X,Count,CHR(VALUE));
end;
{$endif not FPC_SYSTEM_HAS_FILLBYTE}


{$ifndef FPC_SYSTEM_HAS_FILLWORD}
procedure fillword(var x;count : longint;value : word);
type
  longintarray = array [0..maxlongint div 4] of longint;
  wordarray    = array [0..maxlongint div 2] of word;
var
  i,v : longint;
begin
  if Count <= 0 then exit;
  v:=value*$10000+value;
  for i:=0 to (count div 2) -1 do
    longintarray(x)[i]:=v;
  for i:=(count div 2)*2 to count-1 do
    wordarray(x)[i]:=value;
end;
{$endif not FPC_SYSTEM_HAS_FILLWORD}


{$ifndef FPC_SYSTEM_HAS_FILLDWORD}
procedure FillDWord(var x;count : longint;value : DWord);
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
function IndexChar(Const buf;len:longint;b:char):longint;
begin
  IndexChar:=IndexByte(Buf,Len,byte(B));
end;
{$endif not FPC_SYSTEM_HAS_INDEXCHAR}


{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(Const buf;len:longint;b:byte):longint;
type
  bytearray    = array [0..maxlongint-1] of byte;
var
  I : longint;
begin
  I:=0;
  while (bytearray(buf)[I]<>b) and (I<Len) do
   inc(I);
  if (i=Len) then
   i:=-1;                                {Can't use 0, since it is a possible value}
  IndexByte:=I;
end;
{$endif not FPC_SYSTEM_HAS_INDEXBYTE}


{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
function Indexword(Const buf;len:longint;b:word):longint;
type
  wordarray    = array [0..maxlongint div 2] of word;
var
  I : longint;
begin
  I:=0;
  while (wordarray(buf)[I]<>b) and (I<Len) do
   inc(I);
  if (i=Len) then
   i:=-1;           {Can't use 0, since it is a possible value for index}
  Indexword:=I;
end;
{$endif not FPC_SYSTEM_HAS_INDEXWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(Const buf;len:longint;b:DWord):longint;
type
  longintarray = array [0..maxlongint div 4] of longint;
var
  I : longint;
begin
  I:=0;
  while (longintarray(buf)[I]<>b) and (I<Len) do inc(I);
  if (i=Len) then
   i:=-1;           {Can't use 0, since it is a possible value for index}
  IndexDWord:=I;
end;
{$endif not FPC_SYSTEM_HAS_INDEXDWORD}


{$ifndef FPC_SYSTEM_HAS_COMPARECHAR}
function CompareChar(Const buf1,buf2;len:longint):longint;
begin
  CompareChar:=CompareByte(buf1,buf2,len);
end;
{$endif not FPC_SYSTEM_HAS_COMPARECHAR}


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(Const buf1,buf2;len:longint):longint;
type
  bytearray    = array [0..maxlongint-1] of byte;
var
  I,J : longint;
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
function CompareWord(Const buf1,buf2;len:longint):longint;
type
  wordarray    = array [0..maxlongint div 2] of word;
var
  I,J : longint;
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
function CompareDWord(Const buf1,buf2;len:longint):longint;
type
  longintarray    = array [0..maxlongint div 4] of longint;
var
  I,J : longint;
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
procedure MoveChar0(Const buf1;var buf2;len:longint);
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
function IndexChar0(Const buf;len:longint;b:Char):longint;
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
function CompareChar0(Const buf1,buf2;len:longint):longint;
type
  bytearray    = array [0..maxlongint-1] of byte;

Var i : longint;

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

{ Generic code does not set the register used for self !
  So this needs to be done by the compiler after calling
  FPC_HELP_CONSTRUCTOR : generic allways means aa little less efficient (PM) }
{ I don't think we really need to save any registers here      }
{ since this is called at the start of the constructor (CEC)   }
function fpc_help_constructor(var _self : pointer; var vmt : pointer; vmt_pos : cardinal) : pointer; [public,alias:'FPC_HELP_CONSTRUCTOR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
   type
     ppointer = ^pointer;
     pvmt = ^tvmt;
     tvmt = packed record
        size,msize : longint;
        parent : pointer;
        end;
   var
      objectsize : longint;
      vmtcopy : pointer;
begin
   if vmt=nil then
     begin
       fpc_help_constructor:=_self;
       exit;
     end;
   vmtcopy:=vmt;
   objectsize:=pvmt(vmtcopy)^.size;
   if _self=nil then
     begin
       getmem(_self,objectsize);
       longint(vmt):=-1; { needed for fail }
     end;
   if _self<>nil then
     begin
       fillchar(_self^,objectsize,#0);
       ppointer(_self+vmt_pos)^:=vmtcopy;
     end;
   fpc_help_constructor:=_self;
end;

{$endif FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}

{$ifndef FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}

procedure fpc_help_destructor(var _self : pointer; vmt : pointer; vmt_pos : cardinal);saveregisters;[public,alias:'FPC_HELP_DESTRUCTOR'];  {$ifdef hascompilerproc} compilerproc; {$endif}
   type
     ppointer = ^pointer;
     pvmt = ^tvmt;
     tvmt = packed record
        size,msize : longint;
        parent : pointer;
        end;
   var
      objectsize : longint;
begin
   if (_self=nil) then
     exit;
   if (pvmt(ppointer(_self+vmt_pos)^)^.size=0) or
      (pvmt(ppointer(_self+vmt_pos)^)^.size+pvmt(ppointer(_self+vmt_pos)^)^.msize<>0) then
     RunError(210);
   if (vmt = nil) then
     exit;
   objectsize:=pvmt(vmt)^.size;
   { reset vmt to nil for protection }
   ppointer(_self+vmt_pos)^:=nil;
   freemem(_self,objectsize);
   _self:=nil;
end;

{$endif FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}

{$ifndef FPC_SYSTEM_HAS_FPC_HELP_FAIL}
procedure fpc_help_fail(var _self : pointer; var vmt : pointer; vmt_pos : cardinal);safecall; [public,alias:'FPC_HELP_FAIL'];
   type
     ppointer = ^pointer;
     pvmt = ^tvmt;
     tvmt = packed record
        size,msize : longint;
        parent : pointer;
        end;
   var
      objectsize : longint;
begin
   if vmt=nil then
     exit;
   if longint(vmt)=-1 then
     begin
       if (_self=nil) or (ppointer(_self+vmt_pos)^=nil) then
         HandleError(210)
       else
         begin
           ppointer(_self+vmt_pos)^:=nil;
           freemem(_self);
           _self:=nil;
           vmt:=nil;
         end;
     end
   else
     ppointer(_self+vmt_pos)^:=nil;
   _self := nil;
end;
{$endif FPC_SYSTEM_HAS_FPC_HELP_FAIL}

{$ifndef FPC_SYSTEM_HAS_FPC_NEW_CLASS}
{ the constructor receives as first parameter a pointer        }
{ to the vmt or nil, if called when class already instanciated }
{ RETURNS SELF                                                 }
{ IMPORTANT: SELF REGISTER should be pre-loaded before call to }
{ constructor for this to work!                                }
function fpc_new_class(_vmt: pointer; _self : pointer):pointer;saveregisters;[public,alias:'FPC_NEW_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
 begin
   if _vmt <> nil then
     begin
        fpc_new_class := tclass(_vmt).NewInstance;
     end
   else
     begin
       { calling when class already instanciated  }
       { then simply returna a boolean value <> 0 }
       fpc_new_class := _self;
     end;
 end;
{$endif FPC_SYSTEM_HAS_FPC_NEW_CLASS}

{$ifndef FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}
procedure fpc_dispose_class(_self: tobject; flag : longint);saveregisters;[public,alias:'FPC_DISPOSE_CLASS'];
 begin
   if (_self <> nil) and (flag = 1) then
      _self.FreeInstance;
 end;
{$endif ndef FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}

{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}

procedure fpc_check_object(obj : pointer);saveregisters;[public,alias:'FPC_CHECK_OBJECT'];  {$ifdef hascompilerproc} compilerproc; {$endif}
   type
     pvmt = ^tvmt;
     tvmt = packed record
        size,msize : longint;
        parent : pointer;
        end;
begin
(*   if (vmt=nil) or
      (pvmt(vmt)^.size=0) or
      (pvmt(vmt)^.size+pvmt(vmt)^.msize<>0) then
        RunError(210);*)
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}

{ checks for a correct vmt pointer }
{ deeper check to see if the current object is }
{ really related to the true }

{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}

procedure fpc_check_object_ext(vmt, expvmt : pointer);saveregisters;[public,alias:'FPC_CHECK_OBJECT_EXT']; {$ifdef hascompilerproc} compilerproc; {$endif}
   type
     pvmt = ^tvmt;
     tvmt = packed record
        size,msize : longint;
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
{ these are shortstrings, not pointers! (JM)
  if dstr=nil then
    exit;
  if sstr=nil then
    begin
      if dstr<>nil then
        pstring(dstr)^[0]:=#0;
      exit;
    end;
}
  slen:=length(sstr);
  if slen<len then
    len:=slen;
  { don't forget the length character }
  if len <> 0 then
    move(sstr[0],result[0],len+1);
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
{ these are shortstrings, not pointers! (JM)
  if dstr=nil then
    exit;
  if sstr=nil then
    begin
      if dstr<>nil then
        pstring(dstr)^[0]:=#0;
      exit;
    end;
}
  slen:=length(pstring(sstr)^);
  if slen<len then
    len:=slen;
  { don't forget the length character }
  if len <> 0 then
      move(sstr^,dstr^,len+1);
 { already done by the move above (JM)
  pstring(dstr)^[0]:=chr(len);
 }
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

{ note: this routine is *DIFFERENT* from the routine in i386.inc and as such you }
{ cannot use it with the i386 compiler, unless you remove the                    }
{ ti386addnode.first_string method (JM)                                          }
function fpc_shortstr_concat(const s1,s2:shortstring): shortstring;[public,alias:'FPC_SHORTSTR_CONCAT']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  s1l, s2l : byte;
begin
{ these are shortstrings, they can't be nil! (JM)
  if (s1=nil) or (s2=nil) then
    exit;
}
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>255 then
    s2l:=255-s1l;
  fpc_shortstr_concat := s1;
  move(s2[1],fpc_shortstr_concat[s1l+1],s2l);
  fpc_shortstr_concat[0]:=chr(s1l+s2l);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}

function fpc_shortstr_compare(const dstr,sstr:shortstring) : longint;[public,alias:'FPC_SHORTSTR_COMPARE']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
   s1,s2,max,i : byte;
   d : longint;
begin
  s1:=length(dstr);
  s2:=length(sstr);
  if s1<s2 then
    max:=s1
  else
    max:=s2;
  for i:=1 to max do
    begin
     d:=byte(sstr[i])-byte(dstr[i]);
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

{ also add a strlen alias for internal use in the system unit (JM) }
function strlen(p:pchar):longint; [external name 'FPC_PCHAR_LENGTH'];

{$ifndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}

{$ifdef hascompilerproc}
function fpc_chararray_to_shortstr(const arr: array of char):shortstring;[public,alias:'FPC_CHARARRAY_TO_SHORTSTR']; compilerproc;
var
  l: longint;
{$else hascompilerproc}
function fpc_chararray_to_shortstr(p:pchar; l : longint):shortstring;[public,alias:'FPC_CHARARRAY_TO_SHORTSTR'];
{$endif hascompilerproc}
begin
{$ifdef hascompilerproc}
  l := high(arr)+1;
{$endif hascompilerproc}
  if l>=256 then
    l:=255
  else if l<0 then
    l:=0;
  move(arr[0],fpc_chararray_to_shortstr[1],l);
  fpc_chararray_to_shortstr[0]:=chr(l);
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
{$error Get_frame must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_FRAME}

{$ifndef FPC_SYSTEM_HAS_GET_CALLER_ADDR}
{$error Get_caller_addr must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_CALLER_ADDR}

{$ifndef FPC_SYSTEM_HAS_GET_CALLER_FRAME}
{$error Get_caller_frame must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_CALLER_FRAME}

{****************************************************************************
                                 Math
****************************************************************************}

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

{$ifndef FPC_SYSTEM_HAS_SPTR}
{$error Sptr must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_SPTR}

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
     if l=$80000000 then
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

{$ifndef FPC_SYSTEM_HAS_SYSRESETFPU}

procedure SysResetFpu;
begin
  { nothing todo }
end;

{$endif FPC_SYSTEM_HAS_SYSRESETFPU}

{
  $Log$
  Revision 1.44  2002-12-23 21:27:13  peter
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
