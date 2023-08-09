{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Include file with set operations called by the compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}
procedure fpc_varset_add_sets(const set1,set2; var dest;size : ptrint); compilerproc; assembler; nostackframe;
{ eax = set1, edx = set2, ecx = dest, [esp + 4] = size }
asm
    push   %ebx
    push   %esi
    mov    12(%esp), %esi { esi = size }
    sub    $4, %esi
    jl     .LBytewise_Prepare { probably dead branch... }

.L4x_Loop:
    mov    (%eax,%esi), %ebx
    or     (%edx,%esi), %ebx
    mov    %ebx, (%ecx,%esi)
    sub    $4, %esi
    ja     .L4x_Loop

    mov    (%eax), %ebx { Tail, just in case (if size is always divisible by 4, 4x_Loop can be altered to handle everything instead). }
    or     (%edx), %ebx
    mov    %ebx, (%ecx)
    pop    %esi
    pop    %ebx
    ret    $4

.LBytewise_Prepare:
    add    $3, %esi
.LBytewise_Loop:
    movzbl (%eax,%esi), %ebx
    or     (%edx,%esi), %bl
    mov    %bl, (%ecx,%esi)
    sub    $1, %esi
    jae    .LBytewise_Loop
    pop    %esi
    pop    %ebx
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}
procedure fpc_varset_mul_sets(const set1,set2; var dest;size : ptrint); compilerproc; assembler; nostackframe;
{ Same as fpc_varset_add_sets but with 'and' instead of 'or'. }
asm
    push   %ebx
    push   %esi
    mov    12(%esp), %esi { esi = size }
    sub    $4, %esi
    jl     .LBytewise_Prepare { probably dead branch... }

.L4x_Loop:
    mov    (%eax,%esi), %ebx
    and    (%edx,%esi), %ebx
    mov    %ebx, (%ecx,%esi)
    sub    $4, %esi
    ja     .L4x_Loop

    mov    (%eax), %ebx { Tail, just in case (if size is always divisible by 4, 4x_Loop can be altered to handle everything instead). }
    and    (%edx), %ebx
    mov    %ebx, (%ecx)
    pop    %esi
    pop    %ebx
    ret    $4

.LBytewise_Prepare:
    add    $3, %esi
.LBytewise_Loop:
    movzbl (%eax,%esi), %ebx
    and    (%edx,%esi), %bl
    mov    %bl, (%ecx,%esi)
    sub    $1, %esi
    jae    .LBytewise_Loop
    pop    %esi
    pop    %ebx
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}
procedure fpc_varset_sub_sets(const set1,set2; var dest;size : ptrint); compilerproc; assembler; nostackframe;
{ eax = set1, edx = set2, ecx = dest, [esp + 4] = size }
asm
    push   %ebx
    push   %esi
    mov    12(%esp), %esi { esi = size }
    sub    $4, %esi
    jl     .LBytewise_Prepare { probably dead branch... }

    mov    (%edx), %ebx { Tail, just in case (if size is always divisible by 4, 4x_Loop can be altered to handle everything instead). }
    not    %ebx         { Precalculated because operation is not idempotent and dest can be equal to set1/set2. }
    and    (%eax), %ebx
    push   %ebx
.L4x_Loop:
    mov    (%edx,%esi), %ebx
    not    %ebx
    and    (%eax,%esi), %ebx
    mov    %ebx, (%ecx,%esi)
    sub    $4, %esi
    ja     .L4x_Loop

    pop    %ebx
    mov    %ebx, (%ecx) { Write precalculated tail. }
    pop    %esi
    pop    %ebx
    ret    $4

.LBytewise_Prepare:
    add    $3, %esi
.LBytewise_Loop:
    movzbl (%edx,%esi), %ebx
    not    %ebx
    and    (%eax,%esi), %bl
    mov    %bl, (%ecx,%esi)
    sub    $1, %esi
    jae    .LBytewise_Loop
    pop    %esi
    pop    %ebx
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}
procedure fpc_varset_symdif_sets(const set1,set2; var dest;size : ptrint); compilerproc; assembler; nostackframe;
{ Same as fpc_varset_mul_sets but with 'xor' instead of 'and not'.
  eax = set1, edx = set2, ecx = dest, [esp + 4] = size }
asm
    push   %ebx
    push   %esi
    mov    12(%esp), %esi { esi = size }
    sub    $4, %esi
    jl     .LBytewise_Prepare { probably dead branch... }

    mov    (%eax), %ebx { Tail, just in case (if size is always divisible by 4, 4x_Loop can be altered to handle everything instead). }
    xor    (%edx), %ebx { Precalculated because operation is not idempotent and dest can be equal to set1/set2. }
    push   %ebx
.L4x_Loop:
    mov    (%eax,%esi), %ebx
    xor    (%edx,%esi), %ebx
    mov    %ebx, (%ecx,%esi)
    sub    $4, %esi
    ja     .L4x_Loop

    pop    %ebx
    mov    %ebx, (%ecx) { Write precalculated tail. }
    pop    %esi
    pop    %ebx
    ret    $4

.LBytewise_Prepare:
    add    $3, %esi
.LBytewise_Loop:
    movzbl (%eax,%esi), %ebx
    xor    (%edx,%esi), %bl
    mov    %bl, (%ecx,%esi)
    sub    $1, %esi
    jae    .LBytewise_Loop
    pop    %esi
    pop    %ebx
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}
function fpc_varset_contains_sets(const set1,set2;size : ptrint):boolean; compilerproc; assembler; nostackframe;
{ eax = set1, edx = set2, ecx = size }
asm
    push   %ebx
    sub    $4, %ecx
    jl     .LBytewise_Prepare { probably dead branch... }
    add    %ecx, %eax
    add    %ecx, %edx
    neg    %ecx { Now ecx = -(size - 4), eax points to set1 + size - 4, edx points to set2 + size - 4. Loop ends on size >= 0, leaving up to 4 tail bytes. }

.L4x_Loop:
    mov    (%edx,%ecx), %ebx
    not    %ebx
    test   %ebx, (%eax,%ecx)
    jnz    .LNo
    add    $4, %ecx
    js     .L4x_Loop

    mov    (%edx), %ebx { Tail. }
    not    %ebx
    mov    %eax, %ecx { eax value is still required to access set1 tail, but eax is going to be xor-zeroed for setz. }
    xor    %eax, %eax
    test   %ebx, (%ecx)
    setz   %al
    pop    %ebx
    ret

.LNo:
    xor    %eax, %eax
    pop    %ebx
    ret

.LBytewise_Prepare:
    add    $4, %ecx
    neg    %ecx
    sub    %ecx, %eax
    sub    %ecx, %edx
.LBytewise_Loop:
    movzbl (%edx,%ecx), %ebx
    not    %ebx
    test   %bl, (%eax,%ecx)
    jnz    .LNo
    inc    %ecx
    jnz    .LBytewise_Loop
    mov    $1, %eax
    pop    %ebx
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}

{ the following code is exactly big endian set-related, but specific to the old
  scheme whereby sets were either 4 or 32 bytes. I've left the routines here
  so if someone wants to, they can create equivalents of the new varset helpers
  from rtl/inc/genset.inc
}

{$ifdef FPC_OLD_BIGENDIAN_SETS}

{$define FPC_SYSTEM_HAS_FPC_SET_LOAD_SMALL}
function fpc_set_load_small(l: fpc_small_set): fpc_normal_set;assembler;[public,alias:'FPC_SET_LOAD_SMALL']; compilerproc;
{
  load a normal set p from a smallset l
}
var
  saveedi : longint;
asm
        movl    %edi,saveedi
        movl    __RESULT,%edi
        movl    l,%eax
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        stosl
        xorl    %eax,%eax
        movl    $7,%ecx
        rep
        stosl
        movl    saveedi,%edi
end;

{$define FPC_SYSTEM_HAS_FPC_SET_CREATE_ELEMENT}

function fpc_set_create_element(b : byte): fpc_normal_set;assembler;[public,alias:'FPC_SET_CREATE_ELEMENT']; compilerproc;
{
  create a new set in p from an element b
}
var
  saveedi : longint;
asm
        movl    %edi,saveedi
        movl    __RESULT,%edi
        movzbl  b,%edx
        xorl    %eax,%eax
        movl    $8,%ecx
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        rep
        stosl
        leal    -32(%edi),%eax
        btsl    %edx,(%eax)
        movl    saveedi,%edi
end;


{$define FPC_SYSTEM_HAS_FPC_SET_SET_BYTE}
function fpc_set_set_byte(const source: fpc_normal_set; b : byte): fpc_normal_set;assembler; compilerproc;
{
  add the element b to the set pointed by source
}
var
  saveesi,saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        movl    source,%esi
        movl    __RESULT,%edi
        movzbl  b,%edx
        movl    $8,%ecx
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        rep
        movsl
        leal    -32(%edi),%eax
        btsl    %edx,(%eax)
        movl    saveedi,%edi
        movl    saveesi,%esi
end;

{$define FPC_SYSTEM_HAS_FPC_SET_UNSET_BYTE}
function fpc_set_unset_byte(const source: fpc_normal_set; b : byte): fpc_normal_set;assembler; compilerproc;
{
  add the element b to the set pointed by source
}
var
  saveesi,saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        movl    source,%esi
        movl    __RESULT,%edi
        movzbl  b,%edx
        movl    $8,%ecx
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        rep
        movsl
        leal    -32(%edi),%eax
        btrl    %edx,(%eax)
        movl    saveedi,%edi
        movl    saveesi,%esi
end;

{$define FPC_SYSTEM_HAS_FPC_SET_SET_RANGE}

function fpc_set_set_range(const orgset: fpc_normal_set; l,h : byte): fpc_normal_set;assembler; compilerproc;
{
  adds the range [l..h] to the set pointed to by p
}
var
  saveh : byte;
  saveesi,saveedi,saveebx : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        movl    %ebx,saveebx
        movl   __RESULT,%edi        // target set address in edi
        movl   orgset, %esi         // source set address in esi
        movzbl l,%eax               // lowest bit to be set in eax
        movzbl h,%ebx               // highest in ebx
        movb   %bl,saveh
        movl   $8,%ecx              // we have to copy 32 bytes
        cmpl   %eax,%ebx            // high < low?
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        rep                         // copy source to dest (it's possible to do the range
        movsl                       // setting and copying simultanuously of course, but
                                    // that would result in many more jumps and code)
        movl   %eax,%ecx            // lowest also in ecx
        jb     .Lset_range_done     // if high > low, then dest := source
        shrl   $3,%eax              // divide by 8 to get starting and ending byte
        shrl   $3,%ebx              // address
        andb   $31,%cl              // low five bits of lo determine start of bit mask
        andl   $0x0fffffffc,%eax    // clear two lowest bits to get start/end longint
        subl   $32,%edi             // get back to start of dest
        andl   $0x0fffffffc,%ebx    // address * 4
        movl   $0x0ffffffff,%edx    // edx = bitmask to be inserted
        shll   %cl,%edx             // shift bitmask to clear bits below lo
        addl   %eax,%edi            // go to starting pos in set
        subl   %eax,%ebx            // are bit lo and hi in the same longint?
        jz     .Lset_range_hi       // yes, keep current mask and adjust for hi bit
        orl    %edx,(%edi)          // no, store current mask
        movl   $0x0ffffffff,%edx    // new mask
        addl   $4,%edi              // next longint of set
        subl   $4,%ebx              // bit hi in this longint?
        jz     .Lset_range_hi       // yes, keep full mask and adjust for hi bit
.Lset_range_loop:
        movl   %edx,(%edi)          // no, fill longints in between with full mask
        addl   $4,%edi
        subl   $4,%ebx
        jnz    .Lset_range_loop
.Lset_range_hi:
        movb   saveh,%cl                // this is ok, h is on the stack
        movl   %edx,%ebx            // save current bitmask
        andb   $31,%cl
        subb   $31,%cl              // cl := (31 - (hi and 31)) = shift count to
        negb   %cl                  // adjust bitmask for hi bit
        shrl   %cl,%edx             // shift bitmask to clear bits higher than hi
        andl   %edx,%ebx            // combine both bitmasks
        orl    %ebx,(%edi)          // store to set
.Lset_range_done:
        movl    saveedi,%edi
        movl    saveesi,%esi
        movl    saveebx,%ebx
end;

{$define FPC_SYSTEM_HAS_FPC_SET_IN_BYTE}

function fpc_set_in_byte(const p: fpc_normal_set; b: byte): boolean; assembler; [public,alias:'FPC_SET_IN_BYTE']; compilerproc;
{
  tests if the element b is in the set p the carryflag is set if it present
}
asm
{$ifdef REGCALL}
        xchgl %edx,%eax
        andl $0xff,%eax
{$else}
       movl   p,%edx
       movzbl b,%eax
{$endif}
       btl %eax,(%edx)
end;


{$define FPC_SYSTEM_HAS_FPC_SET_COMP_SETS}

function fpc_set_comp_sets(const set1,set2: fpc_normal_set): boolean;assembler;[public,alias:'FPC_SET_COMP_SETS']; compilerproc;
{
  compares set1 and set2 zeroflag is set if they are equal
}
var
  saveesi,saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        movl set1,%esi
        movl set2,%edi
        movl $8,%ecx
    .LMCOMPSETS1:
        movl (%esi),%eax
        movl (%edi),%edx
        cmpl %edx,%eax
        jne  .LMCOMPSETEND
        addl $4,%esi
        addl $4,%edi
        decl %ecx
        jnz .LMCOMPSETS1
        { we are here only if the two sets are equal
          we have zero flag set, and that what is expected }
    .LMCOMPSETEND:
        seteb %al
        movl    saveedi,%edi
        movl    saveesi,%esi
end;


{$ifdef LARGESETS}

{$error Needs to be fixed for register calling first!}

procedure fpc_largeset_set_word(p : pointer;b : word);assembler;[public,alias:'FPC_LARGESET_SET_WORD']; compilerproc;
{
  sets the element b in set p works for sets larger than 256 elements
  not yet use by the compiler so
}
asm
       pushl %eax
       movl p,%edi
       movw b,%ax
       andl $0xfff8,%eax
       shrl $3,%eax
       addl %eax,%edi
       movb 12(%ebp),%al
       andl $7,%eax
       btsl %eax,(%edi)
       popl %eax
end;


procedure fpc_largeset_in_word(p : pointer;b : word);assembler;[public,alias:'FPC_LARGESET_IN_WORD']; compilerproc;
{
  tests if the element b is in the set p the carryflag is set if it present
  works for sets larger than 256 elements
}
asm
        pushl %eax
        movl p,%edi
        movw b,%ax
        andl $0xfff8,%eax
        shrl $3,%eax
        addl %eax,%edi
        movb 12(%ebp),%al
        andl $7,%eax
        btl %eax,(%edi)
        popl %eax
end;


procedure fpc_largeset_comp_sets(set1,set2 : pointer;size : longint);assembler;[public,alias:'FPC_LARGESET_COMP_SETS']; compilerproc;
asm
      movl set1,%esi
      movl set2,%edi
      movl size,%ecx
{$ifdef FPC_ENABLED_CLD}
      cld
{$endif FPC_ENABLED_CLD}
  .LMCOMPSETSIZES1:
      lodsl
      movl (%edi),%edx
      cmpl %edx,%eax
      jne  .LMCOMPSETSIZEEND
      addl $4,%edi
      decl %ecx
      jnz .LMCOMPSETSIZES1
      { we are here only if the two sets are equal
        we have zero flag set, and that what is expected }
  .LMCOMPSETSIZEEND:
end;


{$endif LARGESET}

{$endif FPC_OLD_BIGENDIAN_SETS}
