{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

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
                               Primitives
****************************************************************************}

{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(var source;var dest;count:longint);assembler;
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
end ['EAX','EBX','ECX','ESI','EDI'];


{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);
  [public,alias: 'FPC_FILLCHAR'];assembler;
asm
        cld
        movl    x,%edi
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


{$define FPC_SYSTEM_HAS_FILLWORD}
procedure fillword(var x;count : longint;value : word);assembler;
asm
        movl    x,%edi
        movl    count,%ecx
        movzwl  value,%eax
        movl    %eax,%edx
        shll    $16,%eax
        orl     %edx,%eax
        movl    %ecx,%edx
        shrl    $1,%ecx
        cld
        rep
        stosl
        movl    %edx,%ecx
        andl    $1,%ecx
        rep
        stosw
end ['EAX','ECX','EDX','EDI'];


{$define FPC_SYSTEM_HAS_FILLDWORD}
procedure filldword(var x;count : longint;value : dword);assembler;
asm
        movl    x,%edi
        movl    count,%ecx
        movl    value,%eax
        cld
        rep
        stosl
end ['EAX','ECX','EDX','EDI'];


{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(var buf;len:longint;b:byte):longint; assembler;
asm
        movl    Len,%ecx       // Load len
        movl    Buf,%edi       // Load String
        testl   %ecx,%ecx
        jz      .Lready
        cld
        movl    %ecx,%ebx      // Copy for easy manipulation
        movb    b,%al
        repne
        scasb
        jne     .Lcharposnotfound
        incl    %ecx
        subl    %ecx,%ebx
        movl    %ebx,%eax
        jmp     .Lready
.Lcharposnotfound:
        movl    $-1,%eax
.Lready:
end ['EAX','EBX','ECX','EDI'];


{$define FPC_SYSTEM_HAS_INDEXWORD}
function Indexword(var buf;len:longint;b:word):longint; assembler;
asm
        movl    Len,%ecx       // Load len
        movl    Buf,%edi       // Load String
        testl   %ecx,%ecx
        jz      .Lready
        cld
        movl    %ecx,%ebx      // Copy for easy manipulation
        movw    b,%ax
        repne
        scasw
        jne     .Lcharposnotfound
        incl    %ecx
        subl    %ecx,%ebx
        movl    %ebx,%eax
        jmp     .Lready
.Lcharposnotfound:
        movl    $-1,%eax
.Lready:
end ['EAX','EBX','ECX','EDI'];


{$define FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(var buf;len:longint;b:DWord):longint; assembler;
asm
        movl    Len,%ecx       // Load len
        movl    Buf,%edi       // Load String
        testl   %ecx,%ecx
        jz      .Lready
        cld
        movl    %ecx,%ebx      // Copy for easy manipulation
        movl    b,%eax
        repne
        scasd
        jne     .Lcharposnotfound
        incl    %ecx
        subl    %ecx,%ebx
        movl    %ebx,%eax
        jmp     .Lready
.Lcharposnotfound:
        movl    $-1,%eax
.Lready:
end ['EAX','EBX','ECX','EDI'];


{$define FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(var buf1,buf2;len:longint):longint; assembler;
asm
        cld
        movl    len,%eax
        movl    buf2,%esi       { Load params}
        movl    buf1,%edi
        testl   %eax,%eax       {We address -1(%esi), so we have to deal with len=0}
        je      .LCmpbyteExit
        cmpl    $7,%eax         {<7 not worth aligning and go through all trouble}
        jl      .LCmpbyte2
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx            { calc bytes to align   (%edi and 3) xor 3= -%edi and 3}
        andl    $3,%ecx
        subl    %ecx,%eax       { Subtract from number of bytes to go}
        orl     %ecx,%ecx
        rep
        cmpsb                   {The actual 32-bit Aligning}
        jne     .LCmpbyte3
        movl    %eax,%ecx       {bytes to do, divide by 4}
        andl    $3,%eax         {remainder}
        shrl    $2,%ecx         {The actual division}
        orl     %ecx,%ecx       {Sets zero flag if ecx=0 -> no cmp}
        rep
        cmpsl
        je      .LCmpbyte2       { All equal? then to the left over bytes}
        movl    $4,%eax         { Not equal. Rescan the last 4 bytes bytewise}
        subl    %eax,%esi
        subl    %eax,%edi
.LCmpbyte2:
        movl    %eax,%ecx       {bytes still to (re)scan}
        orl     %eax,%eax       {prevent disaster in case %eax=0}
        rep
        cmpsb
.LCmpbyte3:
        movzbl  -1(%esi),%ecx
        movzbl  -1(%edi),%eax      // Compare failing (or equal) position
        subl    %ecx,%eax
.LCmpbyteExit:
end ['ECX','EAX','ESI','EDI'];



{$define FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(var buf1,buf2;len:longint):longint; assembler;
asm
        cld
        movl    len,%eax
        movl    buf2,%esi       { Load params}
        movl    buf1,%edi
        testl   %eax,%eax       {We address -2(%esi), so we have to deal with len=0}
        je      .LCmpwordExit
        cmpl    $5,%eax         {<5 (3 bytes align + 4 bytes cmpsl = 4 words}
        jl      .LCmpword2      { not worth aligning and go through all trouble}
        movl    (%edi),%ebx     // Compare alignment bytes.
        cmpl    (%esi),%ebx
        jne     .LCmpword2      // Aligning will go wrong already. Max 2 words will be scanned Branch NOW
        shll    $1,%eax         {Convert word count to bytes}
        movl    %edi,%edx       { Align comparing is already done, so simply add}
        negl    %edx            { calc bytes to align  -%edi and 3}
        andl    $3,%edx
        addl    %edx,%esi       { Skip max 3 bytes alignment}
        addl    %edx,%edi
        subl    %edx,%eax       { Subtract from number of bytes to go}
        movl    %eax,%ecx       { Make copy of bytes to go}
        andl    $3,%eax         { Calc remainder (mod 4) }
        andl    $1,%edx         { %edx is 1 if array not 2-aligned, 0 otherwise}
        shrl    $2,%ecx         { divide bytes to go by 4, DWords to go}
        orl     %ecx,%ecx       { Sets zero flag if ecx=0 -> no cmp}
        rep                     { Compare entire DWords}
        cmpsl
        je      .LCmpword2a     { All equal? then to the left over bytes}
        movl    $4,%eax         { Not equal. Rescan the last 4 bytes bytewise}
        subl    %eax,%esi       { Go back one DWord}
        subl    %eax,%edi
        incl    %eax            {if not odd then this does nothing, else it makes
                                  sure that adding %edx increases from 2 to 3 words}
.LCmpword2a:
        subl    %edx,%esi       { Subtract alignment}
        subl    %edx,%edi
        addl    %edx,%eax
        shrl    $1,%eax
.LCmpword2:
        movl    %eax,%ecx       {words still to (re)scan}
        orl     %eax,%eax       {prevent disaster in case %eax=0}
        rep
        cmpsw
.LCmpword3:
        movzwl  -2(%esi),%ecx
        movzwl  -2(%edi),%eax    // Compare failing (or equal) position
        subl    %ecx,%eax        // calculate end result.
.LCmpwordExit:
end ['EBX','EDX','ECX','EAX','ESI','EDI'];


{$define FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(var buf1,buf2;len:longint):longint; assembler;
asm
        cld
        movl    len,%eax
        movl    buf2,%esi       { Load params}
        movl    buf1,%edi
        testl   %eax,%eax       {We address -2(%esi), so we have to deal with len=0}
        je      .LCmpDwordExit
        cmpl    $3,%eax         {<3 (3 bytes align + 4 bytes cmpsl) = 2 DWords}
        jl      .LCmpDword2      { not worth aligning and go through all trouble}
        movl    (%edi),%ebx     // Compare alignment bytes.
        cmpl    (%esi),%ebx
        jne     .LCmpDword2      // Aligning will go wrong already. Max 2 words will be scanned Branch NOW
        shll    $2,%eax         {Convert word count to bytes}
        movl    %edi,%edx       { Align comparing is already done, so simply add}
        negl    %edx            { calc bytes to align  -%edi and 3}
        andl    $3,%edx
        addl    %edx,%esi       { Skip max 3 bytes alignment}
        addl    %edx,%edi
        subl    %edx,%eax       { Subtract from number of bytes to go}
        movl    %eax,%ecx       { Make copy of bytes to go}
        andl    $3,%eax         { Calc remainder (mod 4) }
        shrl    $2,%ecx         { divide bytes to go by 4, DWords to go}
        orl     %ecx,%ecx       { Sets zero flag if ecx=0 -> no cmp}
        rep                     { Compare entire DWords}
        cmpsl
        je      .LCmpDword2a     { All equal? then to the left over bytes}
        movl    $4,%eax         { Not equal. Rescan the last 4 bytes bytewise}
        subl    %eax,%esi       { Go back one DWord}
        subl    %eax,%edi
        addl    $3,%eax         {if align<>0 this causes repcount to be 2}
.LCmpDword2a:
        subl    %edx,%esi       { Subtract alignment}
        subl    %edx,%edi
        addl    %edx,%eax
        shrl    $2,%eax
.LCmpDword2:
        movl    %eax,%ecx       {words still to (re)scan}
        orl     %eax,%eax       {prevent disaster in case %eax=0}
        rep
        cmpsl
.LCmpDword3:
        movzwl  -4(%esi),%ecx
        movzwl  -4(%edi),%eax    // Compare failing (or equal) position
        subl    %ecx,%eax        // calculate end result.
.LCmpDwordExit:
end ['EBX','EDX','ECX','EAX','ESI','EDI'];


{$define FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(var buf;len:longint;b:Char):longint; assembler;
asm
// Can't use scasb, or will have to do it twice, think this
//   is faster for small "len"
        movl    Buf,%esi        // Load address
        movl    len,%edx        // load maximal searchdistance
        movzbl  b,%ebx          // Load searchpattern
        testl   %edx,%edx
        je      .LFound
        xorl    %ecx,%ecx       // zero index in Buf
        xorl    %eax,%eax       // To make DWord compares possible
.LLoop:
        movb    (%esi),%al      // Load byte
        cmpb    %al,%bl
        je      .LFound         //  byte the same?
        incl    %ecx
        incl    %esi
        cmpl    %edx,%ecx       // Maximal distance reached?
        je      .LNotFound
        testl   %eax,%eax       // Nullchar = end of search?
        jne     .LLoop
.LNotFound:
        movl    $-1,%ecx        // Not found return -1
.LFound:
        movl    %ecx,%eax
end['EAX','EBX','ECX','EDX','ESI'];


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


{$define FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}
procedure int_check_object_ext;assembler;[public,alias:'FPC_CHECK_OBJECT_EXT'];
{ checks for a correct vmt pointer }
{ deeper check to see if the current object is }
{ really related to the true }
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
        movl    dstr,%edi
        movl    sstr,%esi
        xorl    %eax,%eax
        movl    len,%ecx
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
procedure int_strconcat(s1,s2:pointer);
  [public,alias:'FPC_SHORTSTR_CONCAT'];
begin
  asm
        movl    s2,%edi
        movl    s1,%esi
        movl    %edi,%ebx
        movzbl  (%edi),%ecx
        xor     %eax,%eax
        lea     1(%edi,%ecx),%edi
        negl    %ecx
        addl    $0x0ff,%ecx
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

{$ifdef newoptimizations}
procedure int_strconcatlen(s1,s2:pointer;maxlen: longint);
  [public,alias:'FPC_SHORTSTR_CONCAT_LEN'];
begin
  asm
        movl    s2,%edi
        movl    s1,%esi
        movl    %edi,%ebx
        movzbl  (%edi),%ecx
        xor     %eax,%eax
        lea     1(%edi,%ecx),%edi
        negl    %ecx
        addl    maxlen,%ecx
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
{$endif newoptimizations}


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}
procedure int_strcmp(dstr,sstr:pointer);[public,alias:'FPC_SHORTSTR_COMPARE'];
begin
  asm
        cld
        xorl    %ebx,%ebx
        xorl    %eax,%eax
        movl    sstr,%esi
        movl    dstr,%edi
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
        pushl   %ebp
        pushl   $201
        call    HandleErrorFrame
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
  Revision 1.72  2000-04-23 09:26:51  jonas
    + FPC_SHORTSTR_CONCAT_LEN (temporary, for -dnewoptimizations)

  Revision 1.71  2000/04/10 09:47:15  jonas
    + added destroyed registers list for move procedure (it doesn't destroy
      edx)

  Revision 1.70  2000/04/06 08:39:22  florian
    * the bounds check error gets now a correct stack frame

  Revision 1.69  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.68  2000/01/13 13:06:03  jonas
    * fixed warning

  Revision 1.67  2000/01/11 21:11:04  marco
   * Changed some direct asm params to real params

  Revision 1.66  2000/01/10 09:54:30  peter
    * primitives added

  Revision 1.65  2000/01/07 16:41:32  daniel
    * copyright 2000

  Revision 1.64  2000/01/07 16:32:24  daniel
    * copyright 2000 added

  Revision 1.63  1999/12/21 11:48:09  pierre
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

}