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
{ check for zero or negative count }
        cmpl    $0,%ebx
        jle     .LMoveEnd
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
{ check for zero or negative count }
        cmpl    $0,%ecx
        jle     .LFillEnd
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
.LFillEnd:
end;


{$define FPC_SYSTEM_HAS_FILLWORD}
procedure fillword(var x;count : longint;value : word);assembler;
asm
        movl    x,%edi
        movl    count,%ecx
{ check for zero or negative count }
        cmpl    $0,%ecx
        jle     .LFillWordEnd
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
.LFillWordEnd:
end ['EAX','ECX','EDX','EDI'];


{$define FPC_SYSTEM_HAS_FILLDWORD}
procedure filldword(var x;count : longint;value : dword);assembler;
asm
        movl    x,%edi
        movl    count,%ecx
{ check for zero or negative count }
        cmpl    $0,%ecx
        jle     .LFillDWordEnd
        movl    value,%eax
        cld
        rep
        stosl
.LFillDWordEnd:
end ['EAX','ECX','EDX','EDI'];


{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(Const buf;len:longint;b:byte):longint; assembler;
asm
        xorl    %eax,%eax
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
        xorl    %eax,%eax
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
        xorl    %eax,%eax
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
{$ifndef HAS_GENERICCONSTRUCTOR}
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


{ Internal alias that can be reference from asm code }
procedure int_dispose_class;external name 'FPC_DISPOSE_CLASS';

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

{$define FPC_SYSTEM_HAS_FPC_HELP_FAIL_CLASS}
procedure fpc_help_fail_class;assembler;[public,alias:'FPC_HELP_FAIL_CLASS']; {$ifdef hascompilerproc} compilerproc; {$endif}
{ a non zero class must allways be disposed
  VMT is allways at pos 0 }
asm
        testl  %esi,%esi
        je     .LHFC_1
        { can't use the compilerproc version as that will generate a
          reference instead of a symbol }
        call   int_dispose_class
        { set both object places to zero }
        xorl    %esi,%esi
        movl    %esi,8(%ebp)
.LHFC_1:
end;

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
        pushl   $219
        call    HandleError
.Lcoext_ok:
        popl    %eax
        popl    %ebx
        popl    %edi
        { the adress and vmt were pushed : it needs to be removed from stack !! PM }
        popl    %ebp
        ret     $8
end;

{$endif HAS_GENERICCONSTRUCTOR}


{****************************************************************************
                                 String
****************************************************************************}

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}

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

{$ifdef interncopy}
procedure fpc_shortstr_assign(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_ASSIGN'];
{$else}
procedure fpc_shortstr_copy(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_COPY'];
{$endif}
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

function fpc_shortstr_concat(const s1,s2:shortstring):shortstring;{$ifdef hascompilerproc}compilerproc;{$endif}
begin
  asm
        movl    __RESULT,%edi
        movl    %edi,%ebx
        movl    s1,%esi         { first string }
        lodsb
        andl    $0x0ff,%eax
        stosb
        cmpl    $7,%eax
        jl      .LStrConcat1
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
.LStrConcat1:
        movl    %eax,%ecx
        rep
        movsb
        movl    s2,%esi       { second string }
        movzbl  (%ebx),%ecx
        negl    %ecx
        addl    $0x0ff,%ecx
        lodsb
        cmpl    %ecx,%eax
        jbe     .LStrConcat2
        movl    %ecx,%eax
.LStrConcat2:
        addb    %al,(%ebx)
        cmpl    $7,%eax
        jl      .LStrConcat3
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
.LStrConcat3:
        movl    %eax,%ecx
        rep
        movsb
  end ['EBX','ECX','EAX','ESI','EDI'];
end;


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}

{$ifdef hascompilerproc}
procedure fpc_shortstr_append_shortstr(var s1:shortstring;const s2:shortstring);compilerproc;
begin
  asm
        movl    s1,%edi
        movl    s2,%esi
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
{$else hascompilerproc}
procedure fpc_shortstr_concat_int(const s1,s2:shortstring);[public,alias:'FPC_SHORTSTR_CONCAT'];
begin
  asm
        movl    s1,%esi
        movl    s2,%edi
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
{$endif hascompilerproc}


{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}
function fpc_shortstr_compare(const left,right:shortstring): longint; [public,alias:'FPC_SHORTSTR_COMPARE']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  asm
        cld
        xorl    %ebx,%ebx
        xorl    %eax,%eax
        movl    right,%esi
        movl    left,%edi
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

{$define FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
function fpc_pchar_length(p:pchar):longint;assembler;[public,alias:'FPC_PCHAR_LENGTH']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$include strlen.inc}


{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        movl    %ebp,%eax
end ['EAX'];


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        movl    framebp,%eax
        orl     %eax,%eax
        jz      .Lg_a_null
        movl    4(%eax),%eax
.Lg_a_null:
end ['EAX'];


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
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
function abs(l:longint):longint; assembler;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_abs];
asm
        movl    l,%eax
        cltd
        xorl    %edx,%eax
        subl    %edx,%eax
end ['EAX','EDX'];


{$define FPC_SYSTEM_HAS_ODD_LONGINT}
function odd(l:longint):boolean;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_odd];
asm
       movl     l,%eax
       andl     $1,%eax
       setnz    %al
end ['EAX'];


{$define FPC_SYSTEM_HAS_SQR_LONGINT}
function sqr(l:longint):longint;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_sqr];
asm
        mov     l,%eax
        imull   %eax,%eax
end ['EAX'];


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Longint;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
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


{$define FPC_SYSTEM_HAS_INT_STR_LONGWORD}
procedure int_str(c : longword;var s : string);
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

procedure int_boundcheck;assembler;[public,alias: 'FPC_BOUNDCHECK'];
var dummy_to_force_stackframe_generation_for_trace: Longint;
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

{$endif NOBOUNDCHECK}

{ do a thread save inc/dec }
{$define FPC_SYSTEM_HAS_DECLOCKED}
function declocked(var l : longint) : boolean;assembler;

  asm
     movl       l,%edi
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     cmpb       $0,IsMultithread
     jz         .Ldeclockednolock
     lock
     decl       (%edi)
     jmp        .Ldeclockedend
.Ldeclockednolock:
     decl       (%edi);
.Ldeclockedend:
     setzb      %al
  end ['EDI','EAX'];
{$define FPC_SYSTEM_HAS_INCLOCKED}
procedure inclocked(var l : longint);assembler;

  asm
     movl       l,%edi
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     cmpb       $0,IsMultithread
     jz         .Linclockednolock
     lock
     incl       (%edi)
     jmp        .Linclockedend
.Linclockednolock:
     incl       (%edi)
.Linclockedend:
  end ['EDI'];

{****************************************************************************
                                  FPU
****************************************************************************}

const
  fpucw : word = $1332;
  { Internal constants for use in system unit }
  FPU_Invalid = 1;
  FPU_Denormal = 2;
  FPU_DivisionByZero = 4;
  FPU_Overflow = 8;
  FPU_Underflow = $10;
  FPU_StackUnderflow = $20;
  FPU_StackOverflow = $40;
  FPU_ExceptionMask = $ff;

{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
    fninit
    fldcw   fpucw
end;



{
  $Log$
  Revision 1.43  2003-05-26 19:36:46  peter
    * fpc_shortstr_concat is now the same for all targets
    * fpc_shortstr_append_shortstr added for optimized code generation

  Revision 1.42  2003/05/16 22:40:11  florian
    * fixed generic shortstr_compare

  Revision 1.41  2003/03/26 00:19:10  peter
    * ifdef HAS_GENERICCONSTRUCTOR

  Revision 1.40  2003/03/17 14:30:11  peter
    * changed address parameter/return values to pointer instead
      of longint

  Revision 1.39  2003/02/18 17:56:06  jonas
    - removed buggy i386-specific FPC_CHARARRAY_TO_SHORTSTR
    * fixed generic FPC_CHARARRAY_TO_SHORTSTR (web bug 2382)
    * fixed some potential range errors in indexchar/word/dword

  Revision 1.38  2003/01/06 23:03:13  mazen
  + defining FPC_SYSTEM_HAS_DECLOCKED and FPC_SYSTEM_HAS_INCLOCKED to avoid
    compilation error on generic.inc

  Revision 1.37  2003/01/03 17:14:54  peter
    * fix possible overflow when array len > 255 when converting to
      shortstring

  Revision 1.36  2002/12/15 22:32:25  peter
    * fixed return value when len=0 for indexchar,indexword

  Revision 1.35  2002/10/20 11:50:57  carl
   * avoid crashes with negative len counts on fills/moves

  Revision 1.34  2002/10/15 19:24:47  carl
    * Replace 220 -> 219

  Revision 1.33  2002/10/14 19:39:16  peter
    * threads unit added for thread support

  Revision 1.32  2002/10/05 14:20:16  peter
    * fpc_pchar_length compilerproc and strlen alias

  Revision 1.31  2002/10/02 18:21:51  peter
    * Copy() changed to internal function calling compilerprocs
    * FPC_SHORTSTR_COPY renamed to FPC_SHORTSTR_ASSIGN because of the
      new copy functions

  Revision 1.30  2002/09/07 21:33:35  carl
    - removed unused defines

  Revision 1.29  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed

  Revision 1.28  2002/09/03 15:43:36  peter
    * add alias for fpc_dispose_class so it can be called from
      fpc_help_fail_class

  Revision 1.27  2002/08/19 19:34:02  peter
    * SYSTEMINLINE define that will add inline directives for small
      functions and wrappers. This will be defined automaticly when
      the compiler defines the HASINLINE directive

  Revision 1.26  2002/07/26 15:45:33  florian
    * changed multi threading define: it's MT instead of MTRTL

  Revision 1.25  2002/07/06 20:31:59  carl
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

}
