{
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

{****************************************************************************
                               Primitives
****************************************************************************}
var
  os_supports_sse : boolean;
  { this variable is set to true, if currently an sse check is executed and no sig ill should be generated }
  sse_check : boolean;

{$asmmode intel}

function cpuid_support : boolean;assembler;
  {
    Check if the ID-flag can be changed, if changed then CpuID is supported.
    Tested under go32v1 and Linux on c6x86 with CpuID enabled and disabled (PFV)
  }
  asm
    push   ebx
    pushf
    pushf
    pop     eax
    mov     ebx,eax
    xor     eax,200000h
    push    eax
    popf
    pushf
    pop     eax
    popf
    and     eax,200000h
    and     ebx,200000h
    cmp     eax,ebx
    setnz   al
    pop     ebx
  end;

{$asmmode ATT}

function sse_support : boolean;
  var
     _edx : longint;
  begin
    if cpuid_support then
     begin
        asm
          pushl %ebx
          movl $1,%eax
          cpuid
          movl %edx,_edx
          popl %ebx
        end;
        sse_support:=((_edx and $2000000)<>0) and os_supports_sse;
     end
    else
     { a cpu with without cpuid instruction supports never sse }
     sse_support:=false;
  end;


{ returns true, if the processor supports the mmx instructions }
function mmx_support : boolean;

  var
     _edx : longint;

  begin
     if cpuid_support then
       begin
          asm
             pushl %ebx
             movl $1,%eax
             cpuid
             movl %edx,_edx
             popl %ebx
          end;
          mmx_support:=(_edx and $800000)<>0;
       end
     else
       { a cpu with without cpuid instruction supports never mmx }
       mmx_support:=false;
  end;

{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define USE_FASTMOVE}
{$i fastmove.inc}
{$endif FPC_SYSTEM_HAS_MOVE}

procedure fpc_cpuinit;
  begin
    { because of the brain dead sse detection on x86, this test is post poned to fpc_cpucodeinit which
      must be implemented OS dependend (FK)
    has_sse_support:=sse_support;
    has_mmx_support:=mmx_support;
    setup_fastmove;
    }
    os_supports_sse:=false;
  end;


{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}

procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE'];assembler;
var
  saveesi,saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
{$ifdef REGCALL}
        movl    %eax,%esi
        movl    %edx,%edi
        movl    %ecx,%edx
{$else}
        movl    dest,%edi
        movl    source,%esi
        movl    count,%edx
{$endif}
        movl    %edi,%eax
{ check for zero or negative count }
        cmpl    $0,%edx
        jle     .LMoveEnd
{ Check for back or forward }
        sub     %esi,%eax
        jz      .LMoveEnd               { Do nothing when source=dest }
        jc      .LFMove                 { Do forward, dest<source }
        cmp     %edx,%eax
        jb      .LBMove                 { Dest is in range of move, do backward }
{ Forward Copy }
.LFMove:
        cld
        cmpl    $15,%edx
        jl      .LFMove1
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%edx
        rep
        movsb
        movl    %edx,%ecx
        andl    $3,%edx
        shrl    $2,%ecx
        rep
        movsl
.LFMove1:
        movl    %edx,%ecx
        rep
        movsb
        jmp .LMoveEnd
{ Backward Copy }
.LBMove:
        std
        addl    %edx,%esi
        addl    %edx,%edi
        movl    %edi,%ecx
        decl    %esi
        decl    %edi
        cmpl    $15,%edx
        jl      .LBMove1
        negl    %ecx            { Align on 32bits }
        andl    $3,%ecx
        subl    %ecx,%edx
        rep
        movsb
        movl    %edx,%ecx
        andl    $3,%edx
        shrl    $2,%ecx
        subl    $3,%esi
        subl    $3,%edi
        rep
        movsl
        addl    $3,%esi
        addl    $3,%edi
.LBMove1:
        movl    %edx,%ecx
        rep
        movsb
        cld
.LMoveEnd:
        movl    saveedi,%edi
        movl    saveesi,%esi
end;

{$endif FPC_SYSTEM_HAS_MOVE}


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:SizeInt;value:byte);assembler;
asm
        {A push is prefered over a local variable because a local
         variable causes the compiler to generate a stackframe.}
        cld
{$ifdef REGCALL}
        push    %edi
        movl    %eax,%edi
        movzbl  %cl,%eax
        movl    %edx,%ecx
{$else}
        movl    x,%edi
        movl    count,%ecx
        movzbl  value,%eax
        movl    %ecx,%edx
{$endif}
{ check for zero or negative count }
        or      %ecx,%ecx
        jle     .LFillEnd
        cmpl    $7,%ecx
        jl      .LFill1
        imul    $0x01010101,%eax { Expand al into a 4 subbytes of eax}
        shrl    $2,%ecx
        andl    $3,%edx
        rep
        stosl
        movl    %edx,%ecx
.LFill1:
        rep
        stosb
.LFillEnd:
{$ifdef REGCALL}
        pop %edi
{$endif}
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef FPC_SYSTEM_HAS_FILLWORD}
{$define FPC_SYSTEM_HAS_FILLWORD}
procedure fillword(var x;count : SizeInt;value : word);assembler;
var
  saveedi : longint;
asm
        movl    %edi,saveedi
{$ifdef REGCALL}
        movl    %eax,%edi
        movzwl  %cx,%eax
        movl    %edx,%ecx
{$else}
        movl    x,%edi
        movl    count,%ecx
        movzwl  value,%eax
{$endif}
{ check for zero or negative count }
        cmpl    $0,%ecx
        jle     .LFillWordEnd
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
        movl    saveedi,%edi
end;
{$endif FPC_SYSTEM_HAS_FILLWORD}


{$ifndef FPC_SYSTEM_HAS_FILLDWORD}
{$define FPC_SYSTEM_HAS_FILLDWORD}
procedure filldword(var x;count : SizeInt;value : dword);assembler;
var
  saveedi : longint;
asm
        movl    %edi,saveedi
{$ifdef REGCALL}
        movl    %eax,%edi
        movl    %ecx,%eax
        movl    %edx,%ecx
{$else}
        movl    x,%edi
        movl    count,%ecx
        movl    value,%eax
{$endif}
{ check for zero or negative count }
        cmpl    $0,%ecx
        jle     .LFillDWordEnd
        cld
        rep
        stosl
.LFillDWordEnd:
        movl    saveedi,%edi
end;
{$endif FPC_SYSTEM_HAS_FILLDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(Const buf;len:SizeInt;b:byte):SizeInt; assembler;
var
  saveedi,saveebx : longint;
asm
        movl    %edi,saveedi
        movl    %ebx,saveebx
        movl    buf,%edi       // Load String
        movb    b,%bl
        movl    len,%ecx       // Load len
        xorl    %eax,%eax
        testl   %ecx,%ecx
        jz      .Lcharposnotfound
        cld
        movl    %ecx,%edx      // Copy for easy manipulation
        movb    %bl,%al
        repne
        scasb
        jne     .Lcharposnotfound
        incl    %ecx
        subl    %ecx,%edx
        movl    %edx,%eax
        jmp     .Lready
.Lcharposnotfound:
        movl    $-1,%eax
.Lready:
        movl    saveedi,%edi
        movl    saveebx,%ebx
end;
{$endif FPC_SYSTEM_HAS_FILLDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
{$define FPC_SYSTEM_HAS_INDEXWORD}
function Indexword(Const buf;len:SizeInt;b:word):SizeInt; assembler;
var
  saveedi,saveebx : longint;
asm
        movl    %edi,saveedi
        movl    %ebx,saveebx
        movl    Buf,%edi       // Load String
        movw    b,%bx
        movl    Len,%ecx       // Load len
        xorl    %eax,%eax
        testl   %ecx,%ecx
        jz      .Lcharposnotfound
        cld
        movl    %ecx,%edx      // Copy for easy manipulation
        movw    %bx,%ax
        repne
        scasw
        jne     .Lcharposnotfound
        incl    %ecx
        subl    %ecx,%edx
        movl    %edx,%eax
        jmp     .Lready
.Lcharposnotfound:
        movl    $-1,%eax
.Lready:
        movl    saveedi,%edi
        movl    saveebx,%ebx
end;
{$endif FPC_SYSTEM_HAS_INDEXWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
{$define FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(Const buf;len:SizeInt;b:DWord):SizeInt; assembler;
var
  saveedi,saveebx : longint;
asm
        movl    %edi,saveedi
        movl    %ebx,saveebx
{$ifdef REGCALL}
        movl    %eax,%edi
        movl    %ecx,%ebx
        movl    %edx,%ecx
{$else}
        movl    Len,%ecx       // Load len
        movl    Buf,%edi       // Load String
        movl    b,%ebx
{$endif}
        xorl    %eax,%eax
        testl   %ecx,%ecx
        jz      .Lcharposnotfound
        cld
        movl    %ecx,%edx      // Copy for easy manipulation
        movl    %ebx,%eax
        repne
        scasl
        jne     .Lcharposnotfound
        incl    %ecx
        subl    %ecx,%edx
        movl    %edx,%eax
        jmp     .Lready
.Lcharposnotfound:
        movl    $-1,%eax
.Lready:
        movl    saveedi,%edi
        movl    saveebx,%ebx
end;
{$endif FPC_SYSTEM_HAS_INDEXDWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
{$define FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(Const buf1,buf2;len:SizeInt):SizeInt; assembler;
var
  saveesi,saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        cld
{$ifdef REGCALL}
        movl    %eax,%edi
        movl    %edx,%esi
        movl    %ecx,%eax
{$else}
        movl    buf2,%esi       { Load params}
        movl    buf1,%edi
        movl    len,%eax
{$endif}
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
        movl    saveedi,%edi
        movl    saveesi,%esi
end;
{$endif FPC_SYSTEM_HAS_COMPAREBYTE}



{$ifndef FPC_SYSTEM_HAS_COMPAREWORD}
{$define FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(Const buf1,buf2;len:SizeInt):SizeInt; assembler;
var
  saveesi,saveedi,saveebx : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        movl    %ebx,saveebx
        cld
{$ifdef REGCALL}
        movl    %eax,%edi
        movl    %edx,%esi
        movl    %ecx,%eax
{$else}
        movl    buf2,%esi       { Load params}
        movl    buf1,%edi
        movl    len,%eax
{$endif}
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
        movl    saveedi,%edi
        movl    saveesi,%esi
        movl    saveebx,%ebx
end;
{$endif FPC_SYSTEM_HAS_COMPAREWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREDWORD}
{$define FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(Const buf1,buf2;len:SizeInt):SizeInt; assembler;
var
  saveesi,saveedi,saveebx : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        cld
{$ifdef REGCALL}
        movl    %eax,%edi
        movl    %edx,%esi
        movl    %ecx,%eax
{$else}
        movl    buf2,%esi       { Load params}
        movl    buf1,%edi
        movl    len,%eax
        movl    %eax,%ecx
{$endif}
        testl   %eax,%eax
        je      .LCmpDwordExit
        movl    %eax,%ecx
        xorl    %eax,%eax
        rep                     { Compare entire DWords}
        cmpsl
        movl    -4(%edi),%edi        // Compare failing (or equal) position
        subl    -4(%esi),%edi        // calculate end result.
        setb    %dl
        seta    %cl
        addb    %cl,%al
        subb    %dl,%al
        movsbl  %al,%eax
.LCmpDwordExit:
        movl    saveedi,%edi
        movl    saveesi,%esi
end;
{$endif FPC_SYSTEM_HAS_COMPAREDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXCHAR0}
{$define FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(Const buf;len:SizeInt;b:Char):SizeInt; assembler;
var
  saveesi,saveebx : longint;
asm
        movl    %esi,saveesi
        movl    %ebx,saveebx
// Can't use scasb, or will have to do it twice, think this
//   is faster for small "len"
{$ifdef REGCALL}
        movl    %eax,%esi        // Load address
        movzbl  %cl,%ebx          // Load searchpattern
{$else}
        movl    Buf,%esi        // Load address
        movl    len,%edx        // load maximal searchdistance
        movzbl  b,%ebx          // Load searchpattern
{$endif}
        testl   %edx,%edx
        je      .LFound
        xorl    %ecx,%ecx       // zero index in Buf
        xorl    %eax,%eax       // To make DWord compares possible
        .balign 4
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
        movl    saveesi,%esi
        movl    saveebx,%ebx
end;
{$endif FPC_SYSTEM_HAS_INDEXCHAR0}


{****************************************************************************
                                 String
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}

{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_shortstr_to_shortstr(len:longint; const sstr: shortstring): shortstring; [public,alias: 'FPC_SHORTSTR_TO_SHORTSTR']; compilerproc;
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


{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_shortstr_to_shortstr(out res:shortstring; const sstr: shortstring);assembler;[public,alias:'FPC_SHORTSTR_TO_SHORTSTR']; compilerproc;
  var
   saveesi,saveedi : longint;
  asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        cld
        movl    %eax,%edi
        movl    %ecx,%esi
        movl    %edx,%ecx
        xorl    %eax,%eax
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
        movl    saveedi,%edi
        movl    saveesi,%esi
  end;
{$endif FPC_STRTOSHORTSTRINGPROC}


procedure fpc_shortstr_assign(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_ASSIGN'];
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
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}


{$ifndef STR_CONCAT_PROCS}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

function fpc_shortstr_concat(const s1,s2:shortstring):shortstring;compilerproc;
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
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}


{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}

procedure fpc_shortstr_append_shortstr(var s1:shortstring;const s2:shortstring);compilerproc;
    [public,alias:'FPC_SHORTSTR_APPEND_SHORTSTR'];
begin
  asm
        movl    s1,%edi
        movl    s2,%esi
        movl    %edi,%ebx
        movzbl  (%edi),%ecx
        movl    __HIGH(s1),%eax
        lea     1(%edi,%ecx),%edi
        negl    %ecx
        addl    %eax,%ecx
        // no need to zero eax, high(s1) <= 255
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
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}

{$endif STR_CONCAT_PROCS}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}

function fpc_shortstr_compare(const left,right:shortstring): longint;assembler; [public,alias:'FPC_SHORTSTR_COMPARE']; compilerproc;
var
  saveesi,saveedi,saveebx : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        movl    %ebx,saveebx
        cld
        movl    right,%esi
        movl    left,%edi
        movzbl  (%esi),%eax
        movzbl  (%edi),%ebx
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
        subl    %eax,%esi
        subl    %eax,%edi
.LStrCmp2:
        movl    %eax,%ecx
        orl     %eax,%eax
        rep
        cmpsb
        je      .LStrCmp4
.LStrCmp3:
        movzbl  -1(%esi),%edx      // Compare failing (or equal) position
        movzbl  -1(%edi),%ebx
.LStrCmp4:
        movl    %ebx,%eax          // Compare length or position
        subl    %edx,%eax
        movl    saveedi,%edi
        movl    saveesi,%esi
        movl    saveebx,%ebx
end;
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}


{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}
{$define FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}
{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_pchar_to_shortstr(p:pchar):shortstring;assembler;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
{$include strpas.inc}
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_pchar_to_shortstr(out res : shortstring;p:pchar);assembler;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
var
  saveres,saveebx,saveesi,saveedi : longint;
asm
        movl    %ebx,saveebx
        movl    %esi,saveesi
        movl    %edi,saveedi
{$ifdef regcall}
        movl    %ecx,%esi
        movl    %eax,%edi
        movl    %edi,saveres
{$else}
        movl    p,%esi
{$endif}
        movl    $1,%ecx
        testl   %esi,%esi
        movl    %esi,%eax
        jz      .LStrPasDone
{$ifndef REGCALL}
        movl    res,%edi
{$endif}
        leal    3(%esi),%edx
        andl    $-4,%edx
        // skip length byte
        incl    %edi
        subl    %esi,%edx
        jz      .LStrPasAligned
        // align source to multiple of 4 (not dest, because we can't read past
        // the end of the source, since that may be past the end of the heap
        // -> sigsegv!!)
.LStrPasAlignLoop:
        movb    (%esi),%al
        incl    %esi
        testb   %al,%al
        jz      .LStrPasDone
        incl    %edi
        incb    %cl
        decb    %dl
        movb    %al,-1(%edi)
        jne     .LStrPasAlignLoop
        .balign  16
.LStrPasAligned:
        movl    (%esi),%ebx
        addl    $4,%edi
        leal    0x0fefefeff(%ebx),%eax
        movl    %ebx,%edx
        addl    $4,%esi
        notl    %edx
        andl    %edx,%eax
        addl    $4,%ecx
        andl    $0x080808080,%eax
        movl    %ebx,-4(%edi)
        jnz     .LStrPasEndFound
        cmpl    $252,%ecx
        ja      .LStrPasPreEndLoop
        jmp     .LStrPasAligned
.LStrPasEndFound:
        subl    $4,%ecx
        // this won't overwrite data since the result = 255 char string
        // and we never process more than the first 255 chars of p
        shrl    $8,%eax
        jc      .LStrPasDone
        incl    %ecx
        shrl    $8,%eax
        jc      .LStrPasDone
        incl    %ecx
        shrl    $8,%eax
        jc      .LStrPasDone
        incl    %ecx
        jmp     .LStrPasDone
.LStrPasPreEndLoop:
        testb   %cl,%cl
        jz      .LStrPasDone
        movl    (%esi),%eax
.LStrPasEndLoop:
        testb   %al,%al
        jz      .LStrPasDone
        movb    %al,(%edi)
        shrl    $8,%eax
        incl    %edi
        incb    %cl
        jnz     .LStrPasEndLoop
.LStrPasDone:
{$ifdef REGCALL}
        movl    saveres,%edi
{$else}
        movl    __RESULT,%edi
{$endif}
        addb    $255,%cl
        movb    %cl,(%edi)
        movl    saveesi,%esi
        movl    saveedi,%edi
        movl    saveebx,%ebx
end;
{$endif FPC_STRTOSHORTSTRINGPROC}
{$endif FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}


{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
{$define FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
function fpc_pchar_length(p:pchar):longint;assembler;[public,alias:'FPC_PCHAR_LENGTH']; compilerproc;

var
  saveedi : longint;
asm
        movl    %edi,saveedi
{$ifdef REGCALL}
        movl    %eax,%edi
{$else}
        movl    p,%edi
{$endif}
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        cld
        repne
        scasb
        movl    $0xfffffffe,%eax
        subl    %ecx,%eax
        movl    saveedi,%edi
end;
{$endif FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        movl    %ebp,%eax
end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;nostackframe;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
{$ifndef REGCALL}
        movl    framebp,%eax
{$endif}
        orl     %eax,%eax
        jz      .Lg_a_null
        movl    4(%eax),%eax
.Lg_a_null:
end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;nostackframe;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
{$ifndef REGCALL}
        movl    framebp,%eax
{$endif}
        orl     %eax,%eax
        jz      .Lgnf_null
        movl    (%eax),%eax
.Lgnf_null:
end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Pointer;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        movl    %esp,%eax
end;

{****************************************************************************
                                 Str()
****************************************************************************}

{$ifndef darwin}
{$if not (defined(VER2_0) or defined(VER2_2))}
{$define FPC_SYSTEM_HAS_INT_STR_LONGWORD}
{$define FPC_SYSTEM_HAS_INT_STR_LONGINT}

label str_int_shortcut;


procedure int_str(l:longword;out s:string);assembler;nostackframe;

asm
  pushl %esi
  pushl %edi
  pushl %ebx
  mov %edx,%edi
  xor %edx,%edx
  jmp str_int_shortcut
end;

procedure int_str(l:longint;out s:string);assembler;nostackframe;

{Optimized for speed, but balanced with size.}

const digits:array[0..9] of cardinal=(0,10,100,1000,10000,
                                      100000,1000000,10000000,
                                      100000000,1000000000);

asm
  push %esi
  push %edi
  push %ebx
  movl %edx,%edi

  { Calculate absolute value and put sign in edx}
  cltd
  xorl %edx,%eax
  subl %edx,%eax
  negl %edx
str_int_shortcut:
  movl %ecx,%esi
  {Calculate amount of digits in ecx.}
  xorl %ecx,%ecx
  bsrl %eax,%ecx
  incl %ecx
  imul $1233,%ecx
  shr $12,%ecx
{$ifdef FPC_PIC}
  call fpc_geteipasebx
  {$ifdef darwin}
  movl digits-.Lpic(%ebx),%ebx
  {$else}
  addl $_GLOBAL_OFFSET_TABLE_,%ebx
  movl digits@GOT(%ebx),%ebx
  {$endif}
  cmpl (%ebx,%ecx,4),%eax
{$else}
  cmpl digits(,%ecx,4),%eax
{$endif}
  cmc
  adcl $0,%ecx               {Nr. digits ready in ecx.}

  {Write length & sign.}
  lea (%edx,%ecx),%ebx
  movb $45,%bh               {movb $'-,%bh   Not supported by our ATT reader.}
  movw %bx,(%edi)
  addl %edx,%edi
  subl %edx,%esi

  {Skip digits beyond string length.}
  movl %eax,%edx
  subl %ecx,%esi
  jae .Lloop_write
	.balign 4
.Lloop_skip:
  movl $0xcccccccd,%eax      {Divide by 10 using mul+shr}
  mull %edx
  shrl $3,%edx
  decl %ecx
  jz .Ldone                  {If (l<0) and (high(s)=1) this jump is taken.}
  incl %esi
  jnz .Lloop_skip

  {Write out digits.}
	.balign 4
.Lloop_write:
  movl $0xcccccccd,%eax      {Divide by 10 using mul+shr}
  {Pre-add '0'}
  leal 48(%edx),%ebx         {leal $'0(,%edx),%ebx Not supported by our ATT reader.}
  mull %edx
  shrl $3,%edx
  leal (%edx,%edx,8),%eax    {x mod 10 = x-10*(x div 10)}
  subl %edx,%ebx
  subl %eax,%ebx
  movb %bl,(%edi,%ecx)
  decl %ecx
  jnz .Lloop_write
.Ldone:
  popl %ebx
  popl %edi
  popl %esi
end;
{$endif not (defined(VER2_0) or defined(VER2_2))}
{$endif not darwin}

{****************************************************************************
                               Bounds Check
****************************************************************************}


{ do a thread-safe inc/dec }
{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
function cpudeclocked(var l : longint) : boolean;assembler;nostackframe;

  asm
{$ifndef REGCALL}
     movl       l,%eax
{$endif}
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     lock
     decl       (%eax)
     setzb      %al
  end;

{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure cpuinclocked(var l : longint);assembler;nostackframe;

  asm
{$ifndef REGCALL}
     movl       l,%eax
{$endif}
     lock
     incl       (%eax)
  end;

// inline SMP check and normal lock.
// the locked one is so slow, inlining doesn't matter.
function declocked(var l : longint) : boolean; inline;

begin
  if not ismultithread then
    begin
     dec(l);
     declocked:=l=0;
    end
   else
    declocked:=cpudeclocked(l);
end;

procedure inclocked(var l : longint); inline;

begin
  if not ismultithread then
    inc(l)
   else
    cpuinclocked(l);
end;



function InterLockedDecrement (var Target: longint) : longint; assembler;
asm
{$ifdef REGCALL}
        movl    $-1,%edx
        xchgl   %edx,%eax
{$else}
        movl    Target, %edx
        movl    $-1, %eax
{$endif}
        lock
        xaddl   %eax, (%edx)
        decl    %eax
end;


function InterLockedIncrement (var Target: longint) : longint; assembler;
asm
{$ifdef REGCALL}
        movl    $1,%edx
        xchgl   %edx,%eax
{$else}
        movl    Target, %edx
        movl    $1, %eax
{$endif}
        lock
        xaddl   %eax, (%edx)
        incl    %eax
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler;
asm
{$ifdef REGCALL}
        xchgl   (%eax),%edx
        movl    %edx,%eax
{$else}
        movl    Target,%ecx
        movl    Source,%eax
        xchgl   (%ecx),%eax
{$endif}
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler;
asm
{$ifdef REGCALL}
        xchgl   %eax,%edx
{$else}
        movl    Target,%edx
        movl    Source,%eax
{$endif}
        lock
        xaddl   %eax, (%edx)
end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint; assembler;
asm
{$ifdef REGCALL}
        xchgl   %eax,%ecx
{$else}
        movl    Target,%ecx
        movl    NewValue,%edx
        movl    Comparand,%eax
{$endif}
        lock
        cmpxchgl   %edx, (%ecx)
end;


function InterlockedCompareExchange64(var Target: int64; NewValue: int64; Comperand: int64): int64; assembler;
asm
        pushl       %ebx
        pushl       %edi
{$ifdef REGCALL}
        movl        %eax,%edi
{$else}
        movl        Target,%edi
{$endif}
        movl        Comperand+4,%edx
        movl        Comperand+0,%eax
        movl        NewValue+4,%ecx
        movl        NewValue+0,%ebx
        lock cmpxchg8b (%edi)
        pop         %edi
        pop         %ebx
end;




{****************************************************************************
                                  FPU
****************************************************************************}

const
  { Internal constants for use in system unit }
  FPU_Invalid = 1;
  FPU_Denormal = 2;
  FPU_DivisionByZero = 4;
  FPU_Overflow = 8;
  FPU_Underflow = $10;
  FPU_StackUnderflow = $20;
  FPU_StackOverflow = $40;
  FPU_ExceptionMask = $ff;

  { use Default8087CW instead
  fpucw : word = $1300 or FPU_StackUnderflow or FPU_Underflow or FPU_Denormal;
  }

  MM_MaskInvalidOp = %0000000010000000;
  MM_MaskDenorm    = %0000000100000000;
  MM_MaskDivZero   = %0000001000000000;
  MM_MaskOverflow  = %0000010000000000;
  MM_MaskUnderflow = %0000100000000000;
  MM_MaskPrecision = %0001000000000000;

  mxcsr : dword = MM_MaskUnderflow or MM_MaskPrecision or MM_MaskDenorm;


{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
  var
    { these locals are so we don't have to hack pic code in the assembler }
    localmxcsr: dword;
    localfpucw: word;
  begin
    localfpucw:=Default8087CW;
    asm
      fldcw   localfpucw
      fwait
    end;
    if has_sse_support then
      begin
        localmxcsr:=mxcsr;
        asm
          { setup sse exceptions }
          ldmxcsr localmxcsr
        end;
      end;
    softfloat_exception_mask:=float_flag_underflow or float_flag_inexact or float_flag_denormal;
  end;


{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    asm
      fnclex
      fwait
    end;
    softfloat_exception_flags:=0;
  end;


{ because of the brain dead sse detection on x86, this test is post poned }
procedure fpc_cpucodeinit;
  begin
    os_supports_sse:=true;
    os_supports_sse:=sse_support;
    if os_supports_sse then
      begin
        sse_check:=true;
        asm
          { force an sse exception if no sse is supported, the exception handler sets
            os_supports_sse to false then }
          { don't change this instruction, the code above depends on its size }
          movaps %xmm7, %xmm6
        end;
        sse_check:=false;
      end;
    has_sse_support:=os_supports_sse;
    has_mmx_support:=mmx_support;
    SysResetFPU;
    if not(IsLibrary) then
      SysInitFPU;
{$ifdef USE_FASTMOVE}
    setup_fastmove;
{$endif}
  end;


{$ifndef darwin}
{ darwin requires that the stack is aligned to 16 bytes when calling another function }

{$define FPC_SYSTEM_HAS_ANSISTR_DECR_REF}
function fpc_freemem_x(p:pointer):ptrint; [external name 'FPC_FREEMEM_X'];

Procedure fpc_AnsiStr_Decr_Ref (Var S : Pointer); [Public,Alias:'FPC_ANSISTR_DECR_REF']; compilerproc; nostackframe; assembler;
asm
        cmpl $0,(%eax)
        jne .Ldecr_ref_continue
        ret
.Ldecr_ref_continue:
// Temps allocated between ebp-24 and ebp+0
        subl    $4,%esp
// Var S located in register
// Var l located in register
        movl    %eax,(%esp)
// [101] l:=@PAnsiRec(S-FirstOff)^.Ref;
        movl    (%eax),%edx
        subl    $8,%edx
// [102] If l^<0 then exit;
        cmpl    $0,(%edx)
        jl      .Lj3596
.Lj3603:
// [104] If declocked(l^) then
        cmpl    $0,ismultithread
        jne     .Lj3610
        decl    (%edx)
        je      .Lj3620
        addl    $4,%esp
        ret
.Lj3610:
        movl    %edx,%eax
        call    cpudeclocked
        testb   %al,%al
        je      .Lj3605
.Lj3620:
        movl    (%esp),%eax
        movl    (%eax),%eax
        subl    $8,%eax
        call    FPC_FREEMEM_X
        movl    (%esp),%eax
        movl    $0,(%eax)
.Lj3618:
.Lj3605:
.Lj3596:
// [107] end;
        addl $4,%esp
end;

function fpc_truely_ansistr_unique(Var S : Pointer): Pointer; forward;

{$define FPC_SYSTEM_HAS_ANSISTR_UNIQUE}
Function fpc_ansistr_Unique(Var S : Pointer): Pointer; [Public,Alias : 'FPC_ANSISTR_UNIQUE']; compilerproc; nostackframe;assembler;
asm
// Var S located in register
// Var $result located in register
        movl    %eax,%edx
// [437] pointer(result) := pointer(s);
        movl    (%eax),%eax
// [438] If Pointer(S)=Nil then
        testl   %eax,%eax
        je      .Lj4031
.Lj4036:
// [440] if PAnsiRec(Pointer(S)-Firstoff)^.Ref<>1 then
        movl    -8(%eax),%ecx
        cmpl    $1,%ecx
        je      .Lj4038
// [441] result:=fpc_truely_ansistr_unique(s);
        movl    %edx,%eax
        call    fpc_truely_ansistr_unique
.Lj4038:
.Lj4031:
// [442] end;
end;

{$endif darwin}

{$ifndef FPC_SYSTEM_HAS_MEM_BARRIER}
{$define FPC_SYSTEM_HAS_MEM_BARRIER}

procedure ReadBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  lock
  addl $0,0(%esp)
  { alternative: lfence on SSE capable CPUs }
end;

procedure ReadDependencyBarrier;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  { reads imply barrier on earlier reads depended on }
end;

procedure ReadWriteBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  lock
  addl $0,0(%esp)
  { alternative: mfence on SSE capable CPUs }
end;

procedure WriteBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  { no write reordering on intel CPUs (yet) }
end;

{$endif}
