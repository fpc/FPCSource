{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    Processor independent implementation for the system unit
    (adapted for intel i386.inc file)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                                Move / Fill
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_MOVE}
procedure Move(var source;var dest;count:longint);
  type
     longintarray = array [0..maxlongint] of longint;
     bytearray    = array [0..maxlongint] of byte;
  var
     i,size : longint;
begin
   size:=count div sizeof(longint);

   if (@dest)<@source) or
      (@dest>@source+count) then
     begin
        for i:=0 to size-1 do
          longintarray(dest)[i]:=longintarray(source)[i];
        for i:=size*sizeof(longint) to count-1 do
          bytearray(dest)[i]:=bytearray(source)[i];
     end
   else
     begin
        for i:=count-1 downto size*sizeof(longint) do
          bytearray(dest)[i]:=bytearray(source)[i];
        for i:=size-1 downto 0 do
          longintarray(dest)[i]:=longintarray(source)[i];
     end;
end;
{$endif ndef FPC_SYSTEM_HAS_MOVE}


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);
  type
     longintarray = array [0..maxlongint] of longint;
     bytearray    = array [0..maxlongint] of byte;
var i,v : longint;
begin
   v:=value*256+value;
   v:=v*$10000+v;
   for i:=0 to (count div 4) -1 do
      longintarray(x)[i]:=v;
   for i:=(count div 4)*4 to count-1 do
      bytearray(x)[i]:=value;
end;
{$endif ndef FPC_SYSTEM_HAS_FILLCHAR}

{$ifndef FPC_SYSTEM_HAS_FILLWORD}
procedure fillword(var x;count : longint;value : word);
  type
     longintarray = array [0..maxlongint] of longint;
     wordarray    = array [0..maxlongint] of word;
var i,v : longint;
begin
   v:=value*$10000+value;
   for i:=0 to (count div 2) -1 do
      longintarray(x)[i]:=v;
   for i:=(count div 2)*2 to count-1 do
      wordarray(x)[i]:=value;
end;
{$endif ndef FPC_SYSTEM_HAS_FILLWORD}

{****************************************************************************
                              Object Helpers
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}

{ Generic code does not set the register used for self !
  So this needs to be done by the compiler after calling
  FPC_HELP_CONSTRUCTOR : generic allways means aa little less efficient (PM) }
procedure int_help_constructor(var _self : pointer; vmt : pointer; vmt_pos : cardinal); [public,alias:'FPC_HELP_CONSTRUCTOR'];
   type
     ppointer = ^pointer;
     pvmt = ^tvmt;
     tvmt = record
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

procedure int_help_destructor(var _self : pointer; vmt : pointer; vmt_pos : cardinal);[public,alias:'FPC_HELP_DESTRUCTOR'];
   type
     ppointer = ^pointer;
     pvmt = ^tvmt;
     tvmt = record
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

procedure int_new_class;assembler;[public,alias:'FPC_NEW_CLASS'];
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
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_NEW_CLASS}

{$ifndef FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}

procedure int_dispose_class;assembler;[public,alias:'FPC_DISPOSE_CLASS'];
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
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}

{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}

procedure int_check_object(vmt : pointer);[public,alias:'FPC_CHECK_OBJECT'];
   type
     pvmt = ^tvmt;
     tvmt = record
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

{$ifdef FPC_TESTOBJEXT}
{ checks for a correct vmt pointer }
{ deeper check to see if the current object is }
{ really related to the true }

{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}

procedure int_check_object_ext(vmt, expvmt : pointer);[public,alias:'FPC_CHECK_OBJECT_EXT'];
   type
     pvmt = ^tvmt;
     tvmt = record
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

{$endif  FPC_TESTOBJEXT}


{****************************************************************************
                                 String
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}

procedure int_strcopy(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_COPY'];
{
  this procedure must save all modified registers except EDI and ESI !!!
}
begin
  asm
        pushl   %eax
        pushl   %ecx
        cld
        movl    16(%ebp),%edi
        movl    12(%ebp),%esi
        xorl    %eax,%eax
        movl    8(%ebp),%ecx
        lodsb
        cmpl    %ecx,%eax
        jbe     .LStrCopy1
        movl    %ecx,%eax
.LStrCopy1:
        stosb
        cmpl    $7,%eax
        jl      .LStrCopy2
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        rep
        movsb
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        rep
        movsl
.LStrCopy2:
        movl    %eax,%ecx
        rep
        movsb
        popl    %ecx
        popl    %eax
  end ['ESI','EDI'];
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

procedure int_strconcat(s1,s2:pointer);[public,alias:'FPC_SHORTSTR_CONCAT'];
begin
  asm
        xorl    %ecx,%ecx
        movl    12(%ebp),%edi
        movl    8(%ebp),%esi
        movl    %edi,%ebx
        movb    (%edi),%cl
        lea     1(%edi,%ecx),%edi
        negl    %ecx
        xor     %eax,%eax
        addl    $0xff,%ecx
        lodsb
        cmpl    %ecx,%eax
        jbe     .LStrConcat1
        movl    %ecx,%eax
.LStrConcat1:
        addb    %al,(%ebx)
        cmpl    $7,%eax
        jl      .LStrConcat2
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        rep
        movsb
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        rep
        movsl
.LStrConcat2:
        movl    %eax,%ecx
        rep
        movsb
  end ['EBX','ECX','EAX','ESI','EDI'];
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}

procedure int_strcmp(dstr,sstr:pointer);[public,alias:'FPC_SHORTSTR_COMPARE'];
begin
  asm
        cld
        xorl    %ebx,%ebx
        xorl    %eax,%eax
        movl    12(%ebp),%esi
        movl    8(%ebp),%edi
        movb    (%esi),%al
        movb    (%edi),%bl
        movl    %eax,%edx
        incl    %esi
        incl    %edi
        cmpl    %ebx,%eax
        jbe     .LStrCmp1
        movl    %ebx,%eax
.LStrCmp1:
        cmpl    $7,%eax
        jl      .LStrCmp2
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        orl     %ecx,%ecx
        rep
        cmpsb
        jne     .LStrCmp3
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        orl     %ecx,%ecx
        rep
        cmpsl
        je      .LStrCmp2
        movl    $4,%eax
        sub     %eax,%esi
        sub     %eax,%edi
.LStrCmp2:
        movl    %eax,%ecx
        orl     %eax,%eax
        rep
        cmpsb
        jne     .LStrCmp3
        cmp     %ebx,%edx
.LStrCmp3:
  end ['EDX','ECX','EBX','EAX','ESI','EDI'];
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}

{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}

function strpas(p:pchar):string;[public,alias:'FPC_PCHAR_TO_SHORTSTR'];
begin
{$ifndef NEWATT}
  { remove warning }
  strpas:='';
{$endif}
  asm
        cld
        movl    p,%edi
        movl    $0xff,%ecx
        orl     %edi,%edi
        jnz     .LStrPasNotNil
        decl    %ecx
        jmp     .LStrPasNil
.LStrPasNotNil:
        xorl    %eax,%eax
        movl    %edi,%esi
        repne
        scasb
.LStrPasNil:
        movl    %ecx,%eax
{$ifdef NEWATT}
        movl    __RESULT,%edi
{$else}
        movl    8(%ebp),%edi
{$endif}
        notb    %al
        decl    %eax
        stosb
        cmpl    $7,%eax
        jl      .LStrPas2
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        rep
        movsb
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        rep
        movsl
.LStrPas2:
        movl    %eax,%ecx
        rep
        movsb
  end ['ECX','EAX','ESI','EDI'];
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}


{$ifndef FPC_SYSTEM_HAS_STRLEN}

function strlen(p:pchar):longint;assembler;
asm
        movl    p,%edi
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        cld
        repne
        scasb
        movl    $0xfffffffe,%eax
        subl    %ecx,%eax
end ['EDI','ECX','EAX'];

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

function odd(l:longint):boolean;[internconst:in_const_odd];
begin
   odd:=((l and 1)<>0);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_LONGINT}

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
   sign : boolean;
begin
  { Workaround: }
  if l=$80000000 then
   begin
     s:='-2147483648';
     exit;
   end;
  if l<0 then
    begin
       sign:=true;
       l:=-l;
    end
  else
    sign:=false;
  s:='';
  while l>0 do
    begin
       s:=char(ord('0')+(l mod 10))+s;
       l:=l div 10;
    end;
  if sign then
    s:='-'+s;
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_LONGINT}

{$ifndef FPC_SYSTEM_HAS_INT_STR_CARDINAL}

procedure int_str(l : cardinal;var s : string);
begin
  s:='';
  while l>0 do
    begin
       s:=char(ord('0')+(l mod 10))+s;
       l:=l div 10;
    end;
  if sign then
    s:='-'+s;
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_CARDINAL}

{****************************************************************************
                               Bounds Check
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_BOUNDCHECK}

procedure int_boundcheck(l : longint; range : pointer);[public,alias: 'FPC_BOUNDCHECK'];
  type
    prange = ^trange;
    trange = record
               min,max : longint;
             end;
begin
   if (l < prange(range)^.min) or
      (l > prange(range)^.max) then
     HandleError(201);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_BOUNDCHECK}


{****************************************************************************
                                 IoCheck
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_IOCHECK}

procedure int_iocheck(addr : longint);[public,alias:'FPC_IOCHECK'];
var
  l : longint;
begin
  if InOutRes<>0 then
   begin
     l:=InOutRes;
     InOutRes:=0;
     HandleErrorFrame(l,get_frame);
   end;
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_IOCHECK}


{
  $Log$
  Revision 1.1  1999-05-31 21:59:58  pierre
   + generic.inc added

}
