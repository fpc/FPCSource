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
procedure Move(const source;var dest;count:longint);assembler;
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
{ alias seems to be nowhere used? (JM)
   [public,alias: 'FPC_FILLCHAR']; }
assembler;
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
function IndexByte(Const buf;len:longint;b:byte):longint; assembler;
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
function Indexword(Const buf;len:longint;b:word):longint; assembler;
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
function IndexDWord(Const buf;len:longint;b:DWord):longint; assembler;
asm
        movl    Len,%ecx       // Load len
        movl    Buf,%edi       // Load String
        testl   %ecx,%ecx
        jz      .Lready
        cld
        movl    %ecx,%ebx      // Copy for easy manipulation
        movl    b,%eax
        repne
        scasl
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
function CompareByte(Const buf1,buf2;len:longint):longint; assembler;
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
function CompareWord(Const buf1,buf2;len:longint):longint; assembler;
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
function CompareDWord(Const buf1,buf2;len:longint):longint; assembler;
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
function IndexChar0(Const buf;len:longint;b:Char):longint; assembler;
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
{$ifndef TEST_GENERIC}
{$define FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}
procedure fpc_help_constructor; assembler; [public,alias:'FPC_HELP_CONSTRUCTOR']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
{$ifdef valuegetmem}
        { esi can be destroyed in fpc_getmem!!! (JM) }
        pushl   %esi
{$endif valuegetmem}
      { Memory size }
        pushl   (%eax)
{$ifdef valuegetmem}
        call    fpc_getmem
        popl    %esi
        movl    %eax,(%esi)
{$else valuegetmem}
        pushl   %esi
        call    AsmGetMem
{$endif valuegetmem}
        movl    $-1,8(%ebp)
        popal
      { Avoid 80386DX bug }
        nop
      { Memory position to %esi }
        movl    (%esi),%esi
        addl    $4,%esp
      { If no memory available : fail() }
        orl     %esi,%esi
        jz      .LHC_5
      { init self for the constructor }
        movl    %esi,12(%ebp)
      { jmp not necessary anymore because next instruction is disabled (JM)
        jmp     .LHC_6                          }
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
        movl    %ecx,%ebx
        xorl    %eax,%eax
        shrl    $2,%ecx
        andl    $3,%ebx
        rep
        stosl
        movl    %ebx,%ecx
        rep
        stosb
        popal
        { avoid the 80386DX bug }
        nop
      { set the VMT address for the new created object }
      { the offset is in %edi since the calling and has not been changed !! }
        movl    %eax,(%esi,%edi,1)
        testl   %eax,%eax
.LHC_5:
end;


{$define FPC_SYSTEM_HAS_FPC_HELP_FAIL}
procedure fpc_help_fail;assembler;[public,alias:'FPC_HELP_FAIL']; {$ifdef hascompilerproc} compilerproc; {$endif}
{ should be called with a object that needs to be
  freed if VMT field is at -1
  %edi contains VMT offset in object again }
asm
        testl  %esi,%esi
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
{$ifdef valuefreemem}
        pushl   %esi
        call    fpc_freemem
{$else valuefreemem}
        leal    12(%ebp),%eax
        pushl   %eax
        call    AsmFreeMem
{$endif valuefreemem}
        { set both object places to zero }
        xorl    %esi,%esi
        movl    %esi,12(%ebp)
.LHF_1:
end;


{$define FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}
procedure fpc_help_destructor;assembler;[public,alias:'FPC_HELP_DESTRUCTOR']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
{$ifdef valuefreemem}
      { Freemem }
        pushl   %eax
        call    fpc_freemem
{$else valuefreemem}
      { temporary Variable }
        subl    $4,%esp
        movl    %esp,%edi
      { SELF }
        movl    %eax,(%edi)
        pushl   %edi
        call    AsmFreeMem
        addl    $4,%esp
{$endif valuefreemem}
.LHD_3:
        popal
        { avoid the 80386DX bug }
        nop
end;


{$define FPC_SYSTEM_HAS_FPC_NEW_CLASS}
procedure fpc_new_class;assembler;[public,alias:'FPC_NEW_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
asm
        { to be sure in the future, we save also edit }
        pushl   %edi
        { create class ? }
        movl    8(%ebp),%edi
        { if we test eax later without calling newinstance }
        { it must have a value <>0                         }
        movl    $1,%eax
        testl   %edi,%edi
        jz      .LNEW_CLASS1
        { save registers !! }
        pushl   %ebx
        pushl   %ecx
        pushl   %edx
        { esi contains the vmt }
        pushl   %esi
        { call newinstance (class method!) }
        call    *52{vmtNewInstance}(%esi)
        popl    %edx
        popl    %ecx
        popl    %ebx
        { newinstance returns a pointer to the new created }
        { instance in eax                                  }
        { load esi and insert self                         }
        movl    %eax,%esi
.LNEW_CLASS1:
        movl    %esi,8(%ebp)
        testl   %eax,%eax
        popl    %edi
end;


{$define FPC_SYSTEM_HAS_FPC_DISPOSE_CLASS}
procedure fpc_dispose_class;assembler;[public,alias:'FPC_DISPOSE_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
asm
        { to be sure in the future, we save also edit }
        pushl   %edi
        { destroy class ? }
        movl    12(%ebp),%edi
        testl   %edi,%edi
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
        call    *56{vmtFreeInstance}(%edi)
        popl    %edx
        popl    %ecx
        popl    %ebx
        popl    %eax
.LDISPOSE_CLASS1:
        popl    %edi
end;
{$endif TEST_GENERIC}

{$define FPC_SYSTEM_HAS_FPC_HELP_FAIL_CLASS}
procedure fpc_help_fail_class;assembler;[public,alias:'FPC_HELP_FAIL_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
{ a non zero class must allways be disposed
  VMT is allways at pos 0 }
asm
        testl  %esi,%esi
        je     .LHFC_1
        call   FPC_DISPOSE_CLASS
        { set both object places to zero }
        xorl    %esi,%esi
        movl    %esi,8(%ebp)
.LHFC_1:
end;


{$ifndef TEST_GENERIC}
{$define FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}

{ we want the stack for debugging !! PM }
procedure fpc_check_object(obj : pointer);[public,alias:'FPC_CHECK_OBJECT']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
asm
        pushl   %edi
        movl    obj,%edi
        pushl   %eax
        { Here we must check if the VMT pointer is nil before  }
        { accessing it...                                      }
        testl   %edi,%edi
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
end;{ of asm }
end;


{$define FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}
procedure fpc_check_object_ext;assembler;[public,alias:'FPC_CHECK_OBJECT_EXT']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
        testl   %edi,%edi
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
{$endif TEST_GENERIC}

{****************************************************************************
                                 String
****************************************************************************}

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COPY}

function fpc_shortstr_to_shortstr(len:longint; const sstr: shortstring): shortstring; [public,alias: 'FPC_SHORTSTR_TO_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  asm
        cld
        movl    __RESULT,%edi
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
  end ['ESI','EDI','EAX','ECX'];
end;

procedure fpc_shortstr_copy(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_COPY'];
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

{$ifdef hascompilerproc}
{ define a dummy fpc_shortstr_concat for i386. Only the next one }
{ is really used by the compiler, but the compilerproc forward   }
{ definition must still be fulfilled (JM)                        }
function fpc_shortstr_concat(const s1,s2: shortstring): shortstring; compilerproc;
begin
  { avoid warning }
  fpc_shortstr_concat := '';
  runerror(216);
end;
{$endif hascompilerproc}


procedure fpc_shortstr_concat_intern(const s1, s2:shortstring);
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


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}
function fpc_shortstr_compare(const dstr,sstr:shortstring): longint; [public,alias:'FPC_SHORTSTR_COMPARE']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
function fpc_pchar_to_shortstr(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$include strpas.inc}

{$define FPC_SYSTEM_HAS_STRLEN}
function strlen(p:pchar):longint;assembler;
{$include strlen.inc}

{$define FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}
function fpc_chararray_to_shortstr(const arr: array of char):shortstring;[public,alias:'FPC_CHARARRAY_TO_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  asm
        cld
        movl    arr,%esi
        movl    arr+4,%ecx
{$ifdef hascompilerproc}
        { previous implementations passed length(arr), with compilerproc }
        { we only have high(arr), so add one (JM)                        }
        incl    %ecx
{$endif hascompilerproc}
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

{$ifndef NOBOUNDCHECK}

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

{$endif NOBOUNDCHECK}

{ do a thread save inc/dec }

function declocked(var l : longint) : boolean;assembler;

  asm
     movl       l,%edi
{$ifdef MTRTL}
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     cmpb       $0,IsMultithread
     jz         .Ldeclockednolock
     lock
     decl       (%edi)
     jmp        .Ldeclockedend
.Ldeclockednolock:
{$endif MTRTL}
     decl       (%edi);
.Ldeclockedend:
     setzb      %al
  end ['EDI','EAX'];

procedure inclocked(var l : longint);assembler;

  asm
     movl       l,%edi
{$ifdef MTRTL}
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     cmpb       $0,IsMultithread
     jz         .Linclockednolock
     lock
     incl       (%edi)
     jmp        .Linclockedend
.Linclockednolock:
{$endif MTRTL}
     incl       (%edi)
.Linclockedend:
  end ['EDI'];

{
  $Log$
  Revision 1.25  2002-07-06 20:31:59  carl
  + added TEST_GENERIC to test generic version

  Revision 1.24  2002/06/16 08:21:26  carl
  + TEST_GENERIC to test generic versions of code

  Revision 1.23  2002/06/09 12:54:37  jonas
    * fixed memory corruption bug in fpc_help_constructor

  Revision 1.22  2002/04/21 18:56:59  peter
    * fpc_freemem and fpc_getmem compilerproc

  Revision 1.21  2002/04/01 14:23:17  carl
  - no need for runerror 203, already fixed!

  Revision 1.20  2002/03/30 14:52:04  carl
  * cause runtime error 203 on failed class creation

  Revision 1.19  2001/12/03 21:39:19  peter
    * freemem(var) -> freemem(value)

  Revision 1.18  2001/10/09 02:43:58  carl
  * bugfix #1639 (IsMultiThread varialbe setting)

  Revision 1.17  2001/08/30 15:43:14  jonas
    * converted adding/comparing of strings to compileproc. Note that due
      to the way the shortstring helpers for i386 are written, they are
      still handled by the old code (reason: fpc_shortstr_compare returns
      results in the flags instead of in eax and fpc_shortstr_concat
      has wierd parameter conventions). The compilerproc stuff should work
      fine with the generic implementations though.
    * removed some nested comments warnings

  Revision 1.16  2001/08/29 19:49:04  jonas
    * some fixes in compilerprocs for chararray to string conversions
    * conversion from string to chararray is now also done via compilerprocs

  Revision 1.15  2001/08/28 13:24:47  jonas
    + compilerproc implementation of most string-related type conversions
    - removed all code from the compiler which has been replaced by
      compilerproc implementations (using (ifdef hascompilerproc) is not
      necessary in the compiler)

  Revision 1.14  2001/08/01 15:00:09  jonas
    + "compproc" helpers
    * renamed several helpers so that their name is the same as their
      "public alias", which should facilitate the conversion of processor
      specific code in the code generator to processor independent code
    * some small fixes to the val_ansistring and val_widestring helpers
      (always immediately exit if the source string is longer than 255
       chars)
    * fixed fpc_dynarray_high and fpc_dynarray_length if the dynarray is
      still nil (used to crash, now return resp -1 and 0)

  Revision 1.13  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.12  2001/05/31 22:42:56  florian
    * some fixes for widestrings and variants

  Revision 1.11  2001/04/21 12:18:09  peter
    * add nop after popa (merged)

  Revision 1.9  2001/04/08 13:19:28  jonas
    * optimized FPC_HELP_CONSTRUCTOR a bit

  Revision 1.8  2001/03/05 17:10:04  jonas
    * moved implementations of strlen and strpas to separate include files
      (they were duplicated in i386.inc and strings.inc/stringss.inc)
    * strpas supports 'nil' pchars again (returns an empty string)
      (both merged)

  Revision 1.7  2001/03/04 17:31:34  jonas
    * fixed all implementations of strpas

  Revision 1.5  2000/11/12 23:23:34  florian
    * interfaces basically running

  Revision 1.4  2000/11/07 23:42:21  florian
    + AfterConstruction and BeforeDestruction implemented
    + TInterfacedObject implemented

  Revision 1.3  2000/07/14 10:33:09  michael
  + Conditionals fixed

  Revision 1.2  2000/07/13 11:33:41  michael
  + removed logs
}
