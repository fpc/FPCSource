{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    intel i386+

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode ATT}


{****************************************************************************
                                Move / Fill
****************************************************************************}

{$define FPC_SYSTEM_HAS_MOVE}

procedure Move(var source;var dest;count:longint);
begin
        asm
        movl    dest,%edi
        movl    source,%esi
        movl    %edi,%eax
        movl    count,%ebx
{ Check for back or forward }
        sub     %esi,%eax
        jz      .LMoveEnd               { Do nothing when source=dest }
        jc      .LFMove                 { Do forward, dest<source }
        cmp     %ebx,%eax
        jb      .LBMove                 { Dest is in range of move, do backward }
{ Forward Copy }
.LFMove:
        cld
        cmpl    $15,%ebx
        jl      .LFMove1
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%ebx
        rep
        movsb
        movl    %ebx,%ecx
        andl    $3,%ebx
        shrl    $2,%ecx
        rep
        movsl
.LFMove1:
        movl    %ebx,%ecx
        rep
        movsb
        jmp .LMoveEnd
{ Backward Copy }
.LBMove:
        std
        addl    %ebx,%esi
        addl    %ebx,%edi
        movl    %edi,%ecx
        decl    %esi
        decl    %edi
        cmpl    $15,%ebx
        jl      .LBMove1
        negl    %ecx            { Align on 32bits }
        andl    $3,%ecx
        subl    %ecx,%ebx
        rep
        movsb
        movl    %ebx,%ecx
        andl    $3,%ebx
        shrl    $2,%ecx
        subl    $3,%esi
        subl    $3,%edi
        rep
        movsl
        addl    $3,%esi
        addl    $3,%edi
.LBMove1:
        movl    %ebx,%ecx
        rep
        movsb
        cld
.LMoveEnd:
        end;
end;


{$define FPC_SYSTEM_HAS_FILLCHAR}

Procedure FillChar(var x;count:longint;value:byte);
begin
        asm
        cld
        movl    x,%edi
        { movl    value,%eax      Only lower 8 bits will be used }
        movb    value,%al
        movl    count,%ecx
        cmpl    $7,%ecx
        jl      .LFill1
        movb    %al,%ah
        movl    %eax,%ebx
        shll    $16,%eax
        movl    %ecx,%edx
        movw    %bx,%ax
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%edx
        rep
        stosb
        movl    %edx,%ecx
        andl    $3,%edx
        shrl    $2,%ecx
        rep
        stosl
        movl    %edx,%ecx
.LFill1:
        rep
        stosb
        end;
end;


{$define FPC_SYSTEM_HAS_FILLWORD}

procedure fillword(var x;count : longint;value : word);
begin
  asm
        movl 8(%ebp),%edi
        movl 12(%ebp),%ecx
        movl 16(%ebp),%eax
        movl %eax,%edx
        shll $16,%eax
        movw %dx,%ax
        movl %ecx,%edx
        shrl $1,%ecx
        cld
        rep
        stosl
        movl %edx,%ecx
        andl $1,%ecx
        rep
        stosw
  end ['EAX','ECX','EDX','EDI'];
end;



{****************************************************************************
                              Object Helpers
****************************************************************************}

{$define FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}

procedure int_help_constructor;assembler; [public,alias:'FPC_HELP_CONSTRUCTOR'];
asm
{ Entry without preamble, since we need the ESP of the constructor
  Stack (relative to %ebp):
    12 Self
     8 VMT-Address
     4 main programm-Addr
     0 %ebp
     edi contains the vmt position
}
      { eax isn't touched anywhere, so it doesn't have to reloaded }
        movl    8(%ebp),%eax
      { initialise self ? }
        orl     %esi,%esi
        jne     .LHC_4
      { get memory, but save register first temporary variable }
        subl    $4,%esp
        movl    %esp,%esi
      { Save Register}
        pushal
      { Memory size }
        pushl   (%eax)
        pushl   %esi
        call    AsmGetMem
        movl    $-1,8(%ebp)
        popal
      { Memory position to %esi }
        movl    (%esi),%esi
        addl    $4,%esp
      { If no memory available : fail() }
        orl     %esi,%esi
        jz      .LHC_5
      { init self for the constructor }
        movl    %esi,12(%ebp)
        jmp     .LHC_6
      { Why was the VMT reset to zero here ????
        I need it fail to know if I should
        zero the VMT field in static objects PM }
.LHC_4:
      {  movl    $0,8(%ebp)                      }
.LHC_6:
      { is there a VMT address ? }
        orl     %eax,%eax
        jnz     .LHC_7
      { In case the constructor doesn't do anything, the Zero-Flag }
      { can't be put, because this calls Fail() }
        incl    %eax
        ret
.LHC_7:
      { set zero inside the object }
        pushal
        cld
        movl    (%eax),%ecx
        movl    %esi,%edi
        xorl    %eax,%eax
        shrl    $1,%ecx
        jnc     .LHCFill1
        stosb
.LHCFill1:
        shrl    $1,%ecx
        jnc     .LHCFill2
        stosw
.LHCFill2:
        rep
        stosl
        popal
      { set the VMT address for the new created object }
      { the offset is in %edi since the calling and has not been changed !! }
        movl    %eax,(%esi,%edi,1)
        orl     %eax,%eax
.LHC_5:
end;
{$define FPC_SYSTEM_HAS_FPC_HELP_FAIL}

procedure int_help_fail;assembler;[public,alias:'FPC_HELP_FAIL'];
{ should be called with a object that needs to be
  freed if VMT field is at -1
  %edi contains VMT offset in object again }
asm
        orl    %esi,%esi
        je     .LHF_1
        cmpl   $-1,8(%ebp)
        je     .LHF_2
        { reset vmt field to zero for static instances }
        cmpl    $0,8(%ebp)
        je      .LHF_3
        { main constructor, we can zero the VMT field now }
        movl    $0,(%esi,%edi,1)
.LHF_3:
        { we zero esi to indicate failure }
        xorl    %esi,%esi
        jmp    .LHF_1
.LHF_2:
        { get vmt address in eax }
        movl    (%esi,%edi,1),%eax
        movl    %esi,12(%ebp)
        { push object position }
        leal    12(%ebp),%eax
        pushl   %eax
        call    AsmFreeMem
        { set both object places to zero }
        xorl    %esi,%esi
        movl    %esi,12(%ebp)
.LHF_1:
end;

{$define FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}

procedure int_help_destructor;assembler;[public,alias:'FPC_HELP_DESTRUCTOR'];
asm
{ Stack (relative to %ebp):
    12 Self
     8 VMT-Address
     4 Main program-Addr
     0 %ebp
     edi contains the vmt position
}
        pushal
      { Should the object be resolved ? }
        movl    8(%ebp),%eax
        orl     %eax,%eax
        jz      .LHD_3
      { Yes, get size from SELF! }
        movl    12(%ebp),%eax
      { get VMT-pointer (from Self) to %ebx }
      { the offset is in %edi since the calling and has not been changed !! }
        movl    (%eax,%edi,1),%ebx
      { I think for precaution }
      { that we should clear the VMT here }
        movl    $0,(%eax,%edi,1)
      { temporary Variable }
        subl    $4,%esp
        movl    %esp,%edi
      { SELF }
        movl    %eax,(%edi)
        pushl   %edi
        call    AsmFreeMem
        addl    $4,%esp
.LHD_3:
        popal
end;

{$define FPC_SYSTEM_HAS_FPC_NEW_CLASS}

procedure int_new_class;assembler;[public,alias:'FPC_NEW_CLASS'];
asm
        { to be sure in the future, we save also edit }
        pushl   %edi
        { create class ? }
        movl    8(%ebp),%edi
        { if we test eax later without calling newinstance }
        { it must have a value <>0                         }
        movl    $1,%eax
        orl     %edi,%edi
        jz      .LNEW_CLASS1
        { save registers !! }
        pushl   %ebx
        pushl   %ecx
        pushl   %edx
        { esi contains the vmt }
        pushl   %esi
        { call newinstance (class method!) }
{$ifdef NEWVMTOFFSET}
        call    *52{vmtNewInstance}(%esi)
{$else}
        call    *16(%esi)
{$endif}
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
        popl    %edi
end;


{$define FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}

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
{$ifdef NEWVMTOFFSET}
        call    *56{vmtFreeInstance}(%edi)
{$else}
        call    *20(%edi)
{$endif}
        popl    %edx
        popl    %ecx
        popl    %ebx
        popl    %eax
.LDISPOSE_CLASS1:
        popl    %edi
end;

{$define FPC_SYSTEM_HAS_FPC_HELP_FAIL_CLASS}

procedure int_help_fail_class;assembler;[public,alias:'FPC_HELP_FAIL_CLASS'];
{ a non zero class must allways be disposed
  VMT is allways at pos 0 }
asm
        orl    %esi,%esi
        je     .LHFC_1
        call   INT_DISPOSE_CLASS
        { set both object places to zero }
        xorl    %esi,%esi
        movl    %esi,8(%ebp)
.LHFC_1:
end;



{$define FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}

{$ifdef SYSTEMDEBUG}
{ we want the stack for debugging !! PM }
procedure int_check_object(obj : pointer);[public,alias:'FPC_CHECK_OBJECT'];
begin
{$else not SYSTEMDEBUG}
procedure int_check_object;assembler;[public,alias:'FPC_CHECK_OBJECT'];
{$endif not SYSTEMDEBUG}
asm
        pushl   %edi
{$ifdef SYSTEMDEBUG}
        movl    obj,%edi
{$else not SYSTEMDEBUG}
        movl    8(%esp),%edi
{$endif not SYSTEMDEBUG}
        pushl   %eax
        { Here we must check if the VMT pointer is nil before  }
        { accessing it...                                      }
        orl     %edi,%edi
        jz      .Lco_re
        movl    (%edi),%eax
        addl    4(%edi),%eax
        jz      .Lco_ok
.Lco_re:
        pushl   $210
        call    HandleError
.Lco_ok:
        popl    %eax
        popl    %edi
        { the adress is pushed : it needs to be removed from stack !! PM }
{$ifdef SYSTEMDEBUG}
end;{ of asm }
end;
{$else SYSTEMDEBUG}
        ret     $4
end;
{$endif not SYSTEMDEBUG}

{ checks for a correct vmt pointer }
{ deeper check to see if the current object is }
{ really related to the true }

{$define FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}

procedure int_check_object_ext;assembler;[public,alias:'FPC_CHECK_OBJECT_EXT'];
asm
        pushl   %ebp
        movl    %esp,%ebp
        pushl   %edi
        movl    8(%ebp),%edi
        pushl   %ebx
        movl    12(%ebp),%ebx
        pushl   %eax
        { Here we must check if the VMT pointer is nil before  }
        { accessing it...                                      }
.Lcoext_obj:
        orl     %edi,%edi
        jz      .Lcoext_re
        movl    (%edi),%eax
        addl    4(%edi),%eax
        jnz     .Lcoext_re
        cmpl    %edi,%ebx
        je      .Lcoext_ok
.Lcoext_vmt:
        movl    8(%edi),%eax
        cmpl    %ebx,%eax
        je      .Lcoext_ok
        movl    %eax,%edi
        jmp     .Lcoext_obj
.Lcoext_re:
        pushl   $220
        call    HandleError
.Lcoext_ok:
        popl    %eax
        popl    %ebx
        popl    %edi
        { the adress and vmt were pushed : it needs to be removed from stack !! PM }
        popl    %ebp
        ret     $8
end;


{****************************************************************************
                                 String
****************************************************************************}

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}

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

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

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

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}

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

{$define FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}

function strpas(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR'];
begin
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
        movl    __RESULT,%edi
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

{$define FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}

function strchararray(p:pchar; l : longint):shortstring;[public,alias:'FPC_CHARARRAY_TO_SHORTSTR'];
begin
  asm
        cld
        movl    p,%esi
        movl    l,%ecx
        orl     %esi,%esi
        jnz     .LStrCharArrayNotNil
        movl    $0,%ecx
.LStrCharArrayNotNil:
        movl    %ecx,%eax
        movl    __RESULT,%edi
        stosb
        cmpl    $7,%eax
        jl      .LStrCharArray2
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
.LStrCharArray2:
        movl    %eax,%ecx
        rep
        movsb
  end ['ECX','EAX','ESI','EDI'];
end;

{$define FPC_SYSTEM_HAS_STRLEN}

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


{****************************************************************************
                       Caller/StackFrame Helpers
****************************************************************************}

{$define FPC_SYSTEM_HAS_GET_FRAME}

function get_frame:longint;assembler;
asm
        movl    %ebp,%eax
end ['EAX'];

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}

function get_caller_addr(framebp:longint):longint;assembler;
asm
        movl    framebp,%eax
        orl     %eax,%eax
        jz      .Lg_a_null
        movl    4(%eax),%eax
.Lg_a_null:
end ['EAX'];

{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}

function get_caller_frame(framebp:longint):longint;assembler;
asm
        movl    framebp,%eax
        orl     %eax,%eax
        jz      .Lgnf_null
        movl    (%eax),%eax
.Lgnf_null:
end ['EAX'];


{****************************************************************************
                                 Math
****************************************************************************}

{$define FPC_SYSTEM_HAS_ABS_LONGINT}

function abs(l:longint):longint; assembler;[internconst:in_const_abs];
asm
        movl    l,%eax
        cltd
        xorl    %edx,%eax
        subl    %edx,%eax
end ['EAX','EDX'];

{$define FPC_SYSTEM_HAS_ODD_LONGINT}

function odd(l:longint):boolean;assembler;[internconst:in_const_odd];
asm
       movl     l,%eax
       andl     $1,%eax
       setnz    %al
end ['EAX'];

{$define FPC_SYSTEM_HAS_SQR_LONGINT}

function sqr(l:longint):longint;assembler;[internconst:in_const_sqr];
asm
        mov     l,%eax
        imull   %eax,%eax
end ['EAX'];


{$define FPC_SYSTEM_HAS_SPTR}

Function Sptr : Longint;assembler;
asm
        movl    %esp,%eax
end;


{****************************************************************************
                                 Str()
****************************************************************************}

{$define FPC_SYSTEM_HAS_INT_STR_LONGINT}

procedure int_str(l : longint;var s : string);
var
  buffer : array[0..11] of byte;
begin
  { Workaround: }
  if l=$80000000 then
   begin
     s:='-2147483648';
     exit;
   end;
  asm
        movl    l,%eax          // load Integer
        movl    s,%edi          // Load String address
        xorl    %ecx,%ecx       // String length=0
        xorl    %ebx,%ebx       // Buffer length=0
        movl    $0x0a,%esi      // load 10 as dividing constant.
        orl     %eax,%eax       // Sign ?
        jns     .LM2
        neg     %eax
        movb    $0x2d,1(%edi)   // put '-' in String
        incl    %ecx
.LM2:
        cltd
        idivl   %esi
        addb    $0x30,%dl       // convert Rest to ASCII.
        movb    %dl,-12(%ebp,%ebx)
        incl    %ebx
        cmpl    $0,%eax
        jnz     .LM2
        { copy String }
.LM3:
        movb    -13(%ebp,%ebx),%al    // -13 because EBX is decreased only later
        movb    %al,1(%edi,%ecx)
        incl    %ecx
        decl    %ebx
        jnz     .LM3
        movb    %cl,(%edi)      // Copy String length
  end;
end;

{$define FPC_SYSTEM_HAS_INT_STR_CARDINAL}

procedure int_str(c : cardinal;var s : string);
var
  buffer : array[0..14] of byte;
begin
  asm
        movl    c,%eax          // load CARDINAL
        movl    s,%edi          // Load String address
        xorl    %ecx,%ecx       // String length=0
        xorl    %ebx,%ebx       // Buffer length=0
        movl    $0x0a,%esi      // load 10 as dividing constant.
.LM4:
        xorl    %edx,%edx
        divl    %esi
        addb    $0x30,%dl       // convert Rest to ASCII.
        movb    %dl,-12(%ebp,%ebx)
        incl    %ebx
        cmpl    $0,%eax
        jnz     .LM4
        { now copy the string }
.LM5:
        movb    -13(%ebp,%ebx),%al    // -13 because EBX is decreased only later
        movb    %al,1(%edi,%ecx)
        incl    %ecx
        decl    %ebx
        jnz     .LM5
        movb    %cl,(%edi)      // Copy String length
  end;
end;


{****************************************************************************
                               Bounds Check
****************************************************************************}

{$define FPC_SYSTEM_HAS_FPC_BOUNDCHECK}

{$ifdef SYSTEMDEBUG}
{ we want the stack for debugging !! PM }
procedure int_boundcheck;[public,alias: 'FPC_BOUNDCHECK'];
begin
{$else not SYSTEMDEBUG}
procedure int_boundcheck;assembler;[public,alias: 'FPC_BOUNDCHECK'];
var dummy_to_force_stackframe_generation_for_trace: Longint;
{$endif not SYSTEMDEBUG}
{
  called with:
    %ecx - value
    %edi - pointer to the ranges
}
asm
        cmpl    (%edi),%ecx
        jl      .Lbc_err
        cmpl    4(%edi),%ecx
        jle     .Lbc_ok
.Lbc_err:
        pushl   $201
        call    HandleError
.Lbc_ok:
end;
{$ifdef SYSTEMDEBUG}
end;
{$endif def SYSTEMDEBUG}


{$ifndef HASSAVEREGISTERS}

{****************************************************************************
                                 IoCheck
****************************************************************************}
{$define FPC_SYSTEM_HAS_FPC_IOCHECK}

procedure int_iocheck(addr : longint);[public,alias:'FPC_IOCHECK'];
var
  l : longint;
begin
  asm
        pushal
  end;
  if InOutRes<>0 then
   begin
     l:=InOutRes;
     InOutRes:=0;
     HandleErrorFrame(l,get_frame);
   end;
  asm
        popal
  end;
end;

{$endif not HASSAVEREGISTERS}

{
  $Log$
  Revision 1.63  1999-12-21 11:48:09  pierre
   * typo error if previous commit

  Revision 1.62  1999/12/21 11:13:34  pierre
   + FPC_CHARARRAY_TO_SHORTSTRING added

  Revision 1.61  1999/12/11 18:59:44  jonas
    * faster abs() function (no jump anymore)

  Revision 1.60  1999/11/20 12:48:09  jonas
    * reinstated old random generator, but modified it so the integer
      one now has a much longer period

  Revision 1.59  1999/11/09 20:14:12  daniel
  * Committed new random generator.

  Revision 1.58  1999/10/30 17:39:05  peter
    * memorymanager expanded with allocmem/reallocmem

  Revision 1.57  1999/10/08 14:40:54  pierre
   * fix for FPC_HELP_FAIL_CLASS

  Revision 1.56  1999/10/05 20:50:06  pierre
   + code for fail for class

  Revision 1.55  1999/09/17 17:14:11  peter
    + new heap manager supporting delphi freemem(pointer)

  Revision 1.54  1999/09/15 13:04:04  jonas
    * added dummy local var to boundcheck to force stackframe generation

  Revision 1.53  1999/08/19 12:50:08  pierre
   * changes for fail support

  Revision 1.52  1999/08/18 10:43:31  pierre
   + VMT reset to -1 if getmem called, neede for fail

  Revision 1.51  1999/08/09 22:20:02  peter
    * classes vmt changed to only positive addresses
    * sharedlib creation is working

  Revision 1.50  1999/08/05 23:45:12  peter
    * saveregister is now working and used for assert and iocheck (which has
      been moved to system.inc because it's now system independent)

  Revision 1.49  1999/07/06 10:53:52  peter
    * removed temp defines

  Revision 1.48  1999/07/03 09:03:47  florian
    * int_new_class returned with zero flag=1, if at the entrance eax was zero
      and newinstance wasn't called, fixed

  Revision 1.47  1999/05/31 22:03:37  pierre
   * added $define FPC_SYSTEM_HAS_... for use with generic.inc

  Revision 1.46  1999/05/31 12:49:19  peter
    * smaller sptr function

  Revision 1.45  1999/04/22 10:52:40  peter
    * nil check for strpas

  Revision 1.44  1999/04/19 06:13:28  florian
    * the class helper routines doesn't store the registers properly,
      fixed

  Revision 1.43  1999/04/08 11:30:57  peter
    * removed warnings

  Revision 1.42  1999/04/07 16:21:10  pierre
   + no assembler if systemdebug defined

  Revision 1.41  1999/03/01 15:40:55  peter
    * use external names
    * removed all direct assembler modes

  Revision 1.40  1999/02/22 13:23:22  pierre
   * VMT field zeroed at destructor forgot offset !!

  Revision 1.39  1999/02/05 12:26:25  pierre
   + code for FPC_TESTOBJEXT conditionnal

  Revision 1.38  1999/02/02 11:04:27  florian
    * class destructor helper routine for the new calling copnventions fixed

  Revision 1.37  1998/12/21 14:28:20  pierre
    * HandleError -> HandleErrorFrame to avoid problem in
      assembler code in i386.inc
      (call to overloaded function in assembler block !)

  Revision 1.36  1998/12/18 17:21:32  peter
    * fixed io-error handling

  Revision 1.35  1998/12/15 22:42:53  peter
    * removed temp symbols

  Revision 1.34  1998/11/30 15:27:28  pierre
   * vmt address pushed for CHECK_OBJECT was not removed from stack

  Revision 1.33  1998/11/28 14:09:49  peter
    * NOATTCDQ define

  Revision 1.32  1998/11/26 23:15:08  jonas
    * changed cdq to cltd in AT&T assembler block

  Revision 1.31  1998/11/26 21:33:58  peter
    + FPC_BOUNDCHECK

  Revision 1.30  1998/11/17 00:41:08  peter
    * renamed string functions

  Revision 1.29  1998/10/19 08:49:16  pierre
   * removed old code forgotten about vmtoffset

  Revision 1.28  1998/10/16 13:37:46  pierre
    * added code for vmt_offset in destructors

  Revision 1.27  1998/10/16 08:53:50  peter
    * fill_object in constructor changed to 'inline' code to overcome
      pushw/pushl problem

  Revision 1.26  1998/10/15 11:35:02  pierre
   + first step of variable vmt offset
     offset is stored in R_EDI (R_D0)
     if objectvmtoffset is defined

  Revision 1.25  1998/09/28 11:02:34  peter
    * added some more $ifdef FPCNAMES

  Revision 1.24  1998/09/28 10:23:43  florian
    * FPC_NEW_CLASS optimized: addl $0,%eax => orl %eax,%eax

  Revision 1.23  1998/09/28 08:40:47  michael
  + Bugreport from Gertjan Schouten

  Revision 1.22  1998/09/22 15:32:00  peter
    + fpc_pchar_to_str alias for strpas

  Revision 1.21  1998/09/14 10:48:08  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.20  1998/09/11 17:38:48  pierre
     merge for fixes branch

  Revision 1.19.2.1  1998/09/11 17:37:24  pierre
    * correction respective to stricter as v2.9.1 syntax

  Revision 1.19  1998/09/01 17:36:17  peter
    + internconst

  Revision 1.18  1998/08/11 00:04:47  peter
    * $ifdef ver0_99_5 updates

  Revision 1.17  1998/07/30 13:26:20  michael
  + Added support for ErrorProc variable. All internal functions are required
    to call HandleError instead of runerror from now on.
    This is necessary for exception support.

  Revision 1.16  1998/07/02 12:55:04  carl
    * Put back DoError, DO NOT TOUCH!

  Revision 1.15  1998/07/02 12:19:32  carl
    + IO-Error and Overflow now print address in hex

  Revision 1.14  1998/07/01 15:29:58  peter
    * better readln/writeln

  Revision 1.13  1998/06/26 08:20:57  daniel
  - Doerror removed.

  Revision 1.12  1998/05/31 14:15:47  peter
    * force to use ATT or direct parsing

  Revision 1.11  1998/05/30 14:30:21  peter
    * force att reading

  Revision 1.10  1998/05/25 10:40:49  peter
    * remake3 works again on tflily

  Revision 1.5  1998/04/29 13:28:19  peter
    * some cleanup and i386_att usage

  Revision 1.4  1998/04/10 15:41:54  florian
    + some small comments added

  Revision 1.3  1998/04/10 15:25:23  michael
  - Removed so-called better random function

  Revision 1.2  1998/04/08 07:53:31  michael
  + Changed Random() function. Moved from system to processor dependent files (from Pedro Gimeno)
}
