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
  bytearray    = array [0..maxlongint] of byte;
var
  i,size : longint;
begin
  Dec(count);
  for i:=0 to count do
         bytearray(dest)[i]:=bytearray(source)[i];
end;
{$endif ndef FPC_SYSTEM_HAS_MOVE}


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);
type
  longintarray = array [0..maxlongint] of longint;
  bytearray    = array [0..maxlongint] of byte;
var
  i,v : longint;
begin
  if count = 0 then exit;
  v := 0;
  v:=(value shl 8) or (value and $FF);
  v:=(v shl 16) or (v and $ffff);
  for i:=0 to (count div 4) -1 do
    longintarray(x)[i]:=v;
  for i:=(count div 4)*4 to count-1 do
    bytearray(x)[i]:=value;
end;
{$endif ndef FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef FPC_SYSTEM_HAS_FILLBYTE}
procedure FillByte (var x;count : longint;value : byte );
begin
  FillChar (X,Count,CHR(VALUE));
end;
{$endif ndef FPC_SYSTEM_HAS_FILLBYTE}


{$ifndef FPC_SYSTEM_HAS_FILLWORD}
procedure fillword(var x;count : longint;value : word);
type
  longintarray = array [0..maxlongint] of longint;
  wordarray    = array [0..maxlongint] of word;
var
  i,v : longint;
begin
  v:=value*$10000+value;
  for i:=0 to (count div 2) -1 do
    longintarray(x)[i]:=v;
  for i:=(count div 2)*2 to count-1 do
    wordarray(x)[i]:=value;
end;
{$endif ndef FPC_SYSTEM_HAS_FILLWORD}


{$ifndef FPC_SYSTEM_HAS_FILLDWORD}
procedure FillDWord(var x;count : longint;value : DWord);
type
  longintarray = array [0..maxlongint] of longint;
var
  I : longint;
begin
  if Count<>0 then
   begin
     I:=Count;
     while I<>0 do
      begin
        longintarray(X)[I-1]:=Value;
        Dec(I);
      end;
   end;
end;
{$endif ndef FPC_SYSTEM_HAS_FILLDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXCHAR}
function IndexChar(var buf;len:longint;b:char):longint;
begin
  IndexChar:=IndexByte(Buf,Len,byte(B));
end;
{$endif ndef FPC_SYSTEM_HAS_INDEXCHAR}


{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(var buf;len:longint;b:byte):longint;
type
  bytearray    = array [0..maxlongint] of byte;
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
{$endif ndef FPC_SYSTEM_HAS_INDEXBYTE}


{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
function Indexword(var buf;len:longint;b:word):longint;
type
  wordarray    = array [0..maxlongint] of word;
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
{$endif ndef FPC_SYSTEM_HAS_INDEXWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(var buf;len:longint;b:DWord):longint;
type
  longintarray = array [0..maxlongint] of longint;
var
  I : longint;
begin
  I:=0;
  while (longintarray(buf)[I]<>b) and (I<Len) do inc(I);
  if (i=Len) then
   i:=-1;           {Can't use 0, since it is a possible value for index}
  IndexDWord:=I;
end;
{$endif ndef FPC_SYSTEM_HAS_INDEXDWORD}


{$ifndef FPC_SYSTEM_HAS_COMPARECHAR}
function CompareChar(var buf1,buf2;len:longint):longint;
begin
  CompareChar:=CompareByte(buf1,buf2,len);
end;
{$endif ndef FPC_SYSTEM_HAS_COMPARECHAR}


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(var buf1,buf2;len:longint):longint;
type
  bytearray    = array [0..maxlongint] of byte;
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
{$endif ndef FPC_SYSTEM_HAS_COMPAREBYTE}


{$ifndef FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(var buf1,buf2;len:longint):longint;
type
  wordarray    = array [0..maxlongint] of word;
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
{$endif ndef FPC_SYSTEM_HAS_COMPAREWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(var buf1,buf2;len:longint):longint;
type
  longintarray    = array [0..maxlongint] of longint;
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
procedure MoveChar0(var buf1,buf2;len:longint);
var
  I : longint;
begin
  if Len<> 0 then
   begin
     I:=IndexByte(Buf1,Len,0);
     if I<>0 then
      Move(Buf1,Buf2,I);
   end;
end;
{$endif ndef FPC_SYSTEM_HAS_MOVECHAR0}


{$ifndef FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(var buf;len:longint;b:Char):longint;
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
function CompareChar0(var buf1,buf2;len:longint):longint;
type
  bytearray    = array [0..maxlongint] of byte;

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
{$endif ndef FPC_SYSTEM_HAS_COMPARECHAR0}


{****************************************************************************
                              Object Helpers
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}

{ Generic code does not set the register used for self !
  So this needs to be done by the compiler after calling
  FPC_HELP_CONSTRUCTOR : generic allways means aa little less efficient (PM) }
{ I don't think we really need to save any registers here      }
{ since this is called at the start of the constructor (CEC)   }
procedure int_help_constructor(var _self : pointer; vmt : pointer; vmt_pos : cardinal); [public,alias:'FPC_HELP_CONSTRUCTOR'];
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
   objectsize:=pvmt(vmt)^.size;
   getmem(_self,objectsize);
   fillchar(_self,objectsize,#0);
   ppointer(_self+vmt_pos)^:=vmt;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}

{$ifndef FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}

procedure int_help_destructor(var _self : pointer; vmt : pointer; vmt_pos : cardinal);saveregisters;[public,alias:'FPC_HELP_DESTRUCTOR'];
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
   objectsize:=pvmt(vmt)^.size;
   { reset vmt to nil for protection }
   ppointer(_self+vmt_pos)^:=nil;
   freemem(_self,objectsize);
   _self:=nil;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}


{$ifndef FPC_SYSTEM_HAS_FPC_NEW_CLASS}
{$error No pascal version of Int_new_class}
(* procedure int_new_class;assembler;saveregisters;[public,alias:'FPC_NEW_CLASS'];
asm
        { to be sure in the future, we save also edit }
        pushl   %edi
        { create class ? }
        movl    8(%ebp),%edi
        orl     %edi,%edi
        jz      .LNEW_CLASS1
        { save registers !! }
        pushl   %ebx
        pushl   %ecx
        pushl   %edx
        { esi contains the vmt }
        pushl   %esi
        { call newinstance (class method!) }
        call    *16(%esi)
        popl    %edx
        popl    %ecx
        popl    %ebx
        { newinstance returns a pointer to the new created }
        { instance in eax                                  }
        { load esi and insert self                         }
        movl    %eax,%esi
.LNEW_CLASS1:
        movl    %esi,8(%ebp)
        orl     %eax,%eax
        popl   %edi
end; *)

{$endif ndef FPC_SYSTEM_HAS_FPC_NEW_CLASS}

{$ifndef FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}

{$error No pascal version of Int_dispose_class}
(* procedure int_dispose_class;assembler;saveregisters;[public,alias:'FPC_DISPOSE_CLASS'];
asm
        { to be sure in the future, we save also edit }
        pushl   %edi
        { destroy class ? }
        movl    12(%ebp),%edi
        orl     %edi,%edi
        jz      .LDISPOSE_CLASS1
        { no inherited call }
        movl    (%esi),%edi
        { save registers !! }
        pushl   %eax
        pushl   %ebx
        pushl   %ecx
        pushl   %edx
        { push self }
        pushl   %esi
        { call freeinstance }
        call    *20(%edi)
        popl    %edx
        popl    %ecx
        popl    %ebx
        popl    %eax
.LDISPOSE_CLASS1:
        popl   %edi
end; *)

{$endif ndef FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}

{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}

procedure int_check_object(vmt : pointer);saveregisters;[public,alias:'FPC_CHECK_OBJECT'];
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
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}

{ checks for a correct vmt pointer }
{ deeper check to see if the current object is }
{ really related to the true }

{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}

procedure int_check_object_ext(vmt, expvmt : pointer);saveregisters;[public,alias:'FPC_CHECK_OBJECT_EXT'];
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
   RunError(220);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}


{****************************************************************************
                                 String
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}

procedure int_strcopy(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_COPY'];
var
  slen : byte;
begin
  if dstr=nil then
    exit;
  if sstr=nil then
    begin
      if dstr<>nil then
        pstring(dstr)^[0]:=#0;
      exit;
    end;
  slen:=length(pstring(sstr)^);
  if slen<len then
    len:=slen;
  { don't forget the length character }
  if len <> 0 then
      move(sstr^,dstr^,len+1);
  pstring(dstr)^[0]:=chr(len);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

procedure int_strconcat(s1,s2:pointer);[public,alias:'FPC_SHORTSTR_CONCAT'];
var
  s1l, s2l : byte;
begin
  if (s1=nil) or (s2=nil) then
    exit;
  s1l:=length(pstring(s1)^);
  s2l:=length(pstring(s2)^);
  if s1l+s2l>255 then
    s1l:=255-s2l;
  move(pstring(s1)^[1],pstring(s2)^[s2l+1],s1l);
  pstring(s2)^[0]:=chr(s1l+s2l);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}

function int_strcmp(dstr,sstr:pointer) : longint;[public,alias:'FPC_SHORTSTR_COMPARE'];
var
   s1,s2,max,i : byte;
   d : longint;
begin
  s1:=length(pstring(dstr)^);
  s2:=length(pstring(sstr)^);
  if s1<s2 then
    max:=s1
  else
    max:=s2;
  for i:=1 to max do
    begin
     d:=byte(pstring(dstr)^[i])-byte(pstring(sstr)^[i]);
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

function strpas(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR'];
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
  strpas := s;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}

{$ifndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}

function strchararray(p:pchar; l : longint):shortstring;[public,alias:'FPC_CHARARRAY_TO_SHORTSTR'];
var
 s: shortstring;
begin
  if l>=256 then
    l:=255
  else if l<0 then
    l:=0;
  move(p^,s[1],l);
  s[0]:=chr(l);
  strchararray := s;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}

{$ifopt r+}
{$define rangeon}
{$r-}
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_STR_TO_CHARARRAY}
procedure str_to_chararray(strtyp, arraysize: longint; src,dest: pchar);[public,alias:'FPC_STR_TO_CHARARRAY'];
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
    { ansistring}
    1: len := length(ansistring(pointer(src)));
    { longstring }
    2:;
    { widestring }
    3:;
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

{$ifndef FPC_SYSTEM_HAS_STRLEN}

function strlen(p:pchar):longint;
var i : longint;
begin
  i:=0;
  while p[i]<>#0 do inc(i);
  exit(i);
end;

{$endif ndef FPC_SYSTEM_HAS_STRLEN}

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
function abs(l:longint):longint;[internconst:in_const_abs];
begin
   if l<0 then
     abs:=-l
   else
     abs:=l;
end;

{$endif ndef FPC_SYSTEM_HAS_ABS_LONGINT}

{$ifndef FPC_SYSTEM_HAS_ODD_LONGINT}

function odd(l:longint):boolean;
begin
   odd:=boolean(l and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_LONGINT}

{$ifndef FPC_SYSTEM_HAS_ODD_CARDINAL}

function odd(l:cardinal):boolean;
begin
   odd:=boolean(l and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_CARDINAL}


{$ifndef FPC_SYSTEM_HAS_ODD_INT64}

function odd(l:int64):boolean;[internconst:in_const_odd];
begin
   odd:=boolean(longint(l) and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_INT64}

{$ifndef FPC_SYSTEM_HAS_ODD_QWORD}

function odd(l:qword):boolean;
begin
   odd:=boolean(longint(l) and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_QWORD}

{$ifndef FPC_SYSTEM_HAS_SQR_LONGINT}

function sqr(l:longint):longint;[internconst:in_const_sqr];
begin
   sqr:=l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_LONGINT}


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

{$ifndef FPC_SYSTEM_HAS_INT_STR_CARDINAL}

procedure int_str(l : cardinal;var s : string);
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

{$endif ndef FPC_SYSTEM_HAS_INT_STR_CARDINAL}

{****************************************************************************
                               Bounds Check
****************************************************************************}

{$ifndef NOBOUNDCHECK}
{$ifndef FPC_SYSTEM_HAS_FPC_BOUNDCHECK}

procedure int_boundcheck(l : longint; range : pointer);[public,alias: 'FPC_BOUNDCHECK'];
  type
    prange = ^trange;
    trange = packed record
               min,max : longint;
             end;
begin
   if (l < prange(range)^.min) or
      (l > prange(range)^.max) then
     HandleError(201);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_BOUNDCHECK}
{$endif NOBOUNDCHECK}


{
  $Log$
  Revision 1.13  2001-05-28 20:43:17  peter
    * more saveregisters added (merged)

  Revision 1.12  2001/05/18 22:59:59  peter
    * merged fixes branch fixes

  Revision 1.11  2001/05/16 17:44:25  jonas
    + odd() for cardinal, int64 and qword (merged)

  Revision 1.10  2001/05/09 19:57:07  peter
  *** empty log message ***

  Revision 1.9  2001/04/21 12:16:28  peter
    * int_str cardinal fix (merged)

  Revision 1.8  2001/04/13 18:06:28  peter
    * removed rtllite define

  Revision 1.7  2001/03/05 17:10:40  jonas
    * changed typecast in FPC_STR_TO_CHARARRAY so that no temp ansistring is
      generated anymore (merged)

  Revision 1.6  2001/03/03 12:41:22  jonas
    * simplified and optimized range checking code, FPC_BOUNDCHECK is no longer necessary

  Revision 1.5  2000/10/01 13:17:35  michael
  + Merged from fixbranch

  Revision 1.4  2000/08/09 11:29:01  jonas

  Revision 1.1.2.2  2000/10/01 13:14:50  michael
  + Corrected and (hopefully) improved compare0

  Revision 1.1.2.1  2000/08/09 11:21:32  jonas
    + FPC_STR_TO_CHARARRAY routine necessary for string -> chararray
      conversions fix (merged fropm fixes branch)

  Revision 1.3  2000/07/14 10:33:10  michael
  + Conditionals fixed

  Revision 1.2  2000/07/13 11:33:43  michael
  + removed logs

}
