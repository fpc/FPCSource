{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Include file with set operations called by the compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure do_load_small(p : pointer;l:longint);assembler;[public,alias:'FPC_SET_LOAD_SMALL'];
{
  load a normal set p from a smallset l

  on entry: p in r3, l in r4
}
asm
        stw     r4,(r3)
        li      r4,0
        stw     r4,4(r3)
        stw     r4,8(r3)
        stw     r4,12(r3)
        stw     r4,16(r3)
        stw     r4,20(r3)
        stw     r4,24(r3)
        stw     r4,28(r3)
end ['R4'];


procedure do_create_element(p : pointer;b : byte);assembler;[public,alias:'FPC_SET_CREATE_ELEMENT'];
{
  create a new set in p from an element b

  on entry: p in r3, b in r4
}
var
  saveR5, saveR6: longint;
asm
        stw     r5,saveR5
        li      r5,0
        stw     r6,saveR6
        stw     r5,(r3)
        stw     r5,4(r3)
        stw     r5,8(r3)
        stw     r5,12(r3)
        li      r6,1
        stw     r5,16(r3)
        stw     r5,20(r3)
        stw     r5,24(r3)
        stw     r5,28(r3)
        // get the index of the correct *dword* in the set
        // (((b div 8) div 4)*4= (b div 8) and not(3))
        rlwinm  r5,r4,29,3,31     // r5 := (r4 rotl(32-3)) and (0x0fffffff8)
        // r4 := 1 shl r4[27-31] -> bit index in dword (rlw* instructions with
        // shift count in register only consider lower 5 bits of this register)
        rotlw   r4,r6,r4          // equivalent to rlwnm r4,r6,r4,0,31
        // store the result
        stwx    r4,r3,r5
        lwz     r5,saveR5
        lwz     r6,saveR6
end ['R4'];

procedure do_set_byte(p : pointer;b : byte);assembler;[public,alias:'FPC_SET_SET_BYTE'];
{
  add the element b to the set pointed by p

  on entry: p in r3, b in r4
}
var
  saveR5, saveR6: longint;
asm
       stw      r5,saveR5
       stw      r6,saveR6
       // get the index of the correct *dword* in the set
       rlwinm   r5,r4,29,3,31     // r5 := (r4 rotl(32-3)) and (0x0fffffff8)
       // load dword in which the bit has to be set (and update r3 to this address)
       lwzxu    r6,r3,r5
       li       r5,1
       // generate bit which has to be inserted
       rotlw    r4,r5,r4          // equivalent to rlwnm r4,r5,r4,0,31
       // insert it
       lwz      r5,saveR5
       or       r7,r7,r4
       lwz      r6,saveR6
       // store result
       stw      r7,(r3)
end ['R3','R4'];


procedure do_unset_byte(p : pointer;b : byte);assembler;[public,alias:'FPC_SET_UNSET_BYTE'];
{
  suppresses the element b to the set pointed by p
  used for exclude(set,element)

  on entry: p in r3, b in r4
}
var
  saveR5, saveR6: longint;
asm
       stw      r5,saveR5
       stw      r6,saveR6
       // get the index of the correct *dword* in the set
       rlwinm   r5,r4,29,3,31     // r5 := (r4 rotl(32-3)) and (0x0fffffff8)
       // load dword in which the bit has to be set (and update r3 to this address)
       lwzxu    r6,r3,r5
       li       r5,1
       // generate bit which has to be inserted
       rotlw    r4,r5,r4          // equivalent to rlwnm r4,r5,r4,0,31
       // insert it
       lwz      r5,saveR5
       nor      r7,r7,r4
       lwz      r6,saveR6
       // store result
       stw      r7,(r3)
end ['R3','R4'];


procedure do_set_range(p : pointer;l,h : byte);assembler;[public,alias:'FPC_SET_SET_RANGE'];
{
  bad implementation, but it's very seldom used

  on entry: p in r3, l in r4, h in r5
}
var
  saveR6, saveR7, saveR8: longint;
asm
       cmplw    cr0,r4,r5
       bg       cr0,.LSET_RANGE_EXIT
       stw      r6,saveR6
       stw      r7,saveR7
       stw      r8,saveR8
       
       
       
       lwz      r7,(r3)
       
       

        pushl   %eax
        movl    p,%edi
        xorl    %eax,%eax
        xorl    %ecx,%ecx
        movb    h,%al
        movb    l,%cl
.LSET_SET_RANGE_LOOP:
        cmpl    %ecx,%eax
        jl      .LSET_SET_RANGE_EXIT
        movl    %eax,%ebx
        movl    %eax,%edx
        andl    $0xf8,%ebx
        andl    $7,%edx
        shrl    $3,%ebx
        btsl    %edx,(%edi,%ebx)
        dec     %eax
        jmp     .LSET_SET_RANGE_LOOP
.LSET_SET_RANGE_EXIT:
        popl %eax
end ['R4'];


procedure do_in_byte(p : pointer;b : byte);assembler;[public,alias:'FPC_SET_IN_BYTE'];
{
  tests if the element b is in the set p, the **zero** flag is cleared if it's present

  on entry: p in r3, b in r4
}
var
  saveR5, saveR6: longint;
asm
       stw      r5,saveR5
       stw      r6,saveR6
       // get the index of the correct *dword* in the set
       rlwinm   r5,r4,29,3,31     // r5 := (r4 rotl(32-3)) and (0x0fffffff8)
       // load dword in which the bit has to be set (and update r3 to this address)
       lwzx     r6,r3,r5
       li       r5,1
       // generate bit which has to be inserted
       rotlw    r4,r5,r4          // equivalent to rlwnm r4,r5,r4,0,31
       // insert it
       lwz      r5,saveR5
       and.     r7,r7,r4
       lwz      r6,saveR6
       // store result
       stw      r7,(r3)
end ['R4'];



procedure do_add_sets(set1,set2,dest : pointer);assembler;[public,alias:'FPC_SET_ADD_SETS'];
{
  adds set1 and set2 into set dest

  on entry: set1 in r3, set2 in r4, dest in r5
}
var
  saveR6, saveR7, saveR8: longint;
asm
       stw      r6,saveR6
       stw      r7,saveR7
       subi     r5,r5,4
       li       r6,8
       stw      r8,saveR8
       lwz      r7,(r3)
       lwz      r8,(r4)
   .LMADDSETS1:
      subi.     r6,r6,1
      or        r7,r7,r8
      lwzu      r8,4(r4)
      stwu      r7,4(r5)
      lwzu      r7,4(r3)
      bne       cr0,.LMADDSETS1
      lwz       r6,saveR6
      lwz       r7,saveR7
      lwz       r8,saveR8
end ['R3','R4','R5'];



procedure do_mul_sets(set1,set2,dest:pointer);assembler;[public,alias:'FPC_SET_MUL_SETS'];
{
  multiplies (takes common elements of) set1 and set2 result put in dest
  on entry: set1 in r3, set2 in r4, dest in r5
}
var
  saveR6, saveR7, saveR8: longint;
asm
       stw      r6,saveR6
       stw      r7,saveR7
       subi     r5,r5,4
       li       r6,8
       stw      r8,saveR8
       lwz      r7,(r3)
       lwz      r8,(r4)
   .LMADDSETS1:
       subi.    r6,r6,1
       and      r7,r7,r8
       lwzu     r8,4(r4)
       stwu     r7,4(r5)
       lwzu     r7,4(r3)
       bne      cr0,.LMADDSETS1
       lwz      r6,saveR6
       lwz      r7,saveR7
       lwz      r8,saveR8
end ['R3','R4','R5'];


procedure do_sub_sets(set1,set2,dest:pointer);assembler;[public,alias:'FPC_SET_SUB_SETS'];
{
  computes the diff from set1 to set2 result in dest

  on entry: set1 in r3, set2 in r4, dest in r5
}
var
  saveR6, saveR7, saveR8: longint;
asm
       stw      r6,saveR6
       stw      r7,saveR7
       subi     r5,r5,4
       li       r6,8
       stw      r8,saveR8
       lwz      r7,(r3)
       lwz      r8,(r4)
   .LMSUBSETS1:
       subi.    r6,r6,1
       andc     r8,r8,r7
       lwzu     r7,4(r3)
       stwu     r8,4(r5)
       lwzu     r8,4(r4)
       bne      cr0,.LMSUBSETS1
       lwz      r6,saveR6
       lwz      r7,saveR7
       lwz      r8,saveR8
end ['R3','R4','R5'];


procedure do_symdif_sets(set1,set2,dest:pointer);assembler;[public,alias:'FPC_SET_SYMDIF_SETS'];
{
   computes the symetric diff from set1 to set2 result in dest

  on entry: set1 in r3, set2 in r4, dest in r5
}
var
  saveR6, saveR7, saveR8: longint;
asm
       stw      r6,saveR6
       stw      r7,saveR7
       subi     r5,r5,4
       li       r6,8
       stw      r8,saveR8
       lwz      r7,(r3)
       lwz      r8,(r4)
   .LMSYMDIFSETS1:
       subi.    r6,r6,1
       xor      r7,r7,r8
       lwzu     r8,4(r4)
       stwu     r7,4(r5)
       lwzu     r7,4(r3)
       bne      cr0,.LMSYMDIFSETS1
       lwz      r6,saveR6
       lwz      r7,saveR7
       lwz      r8,saveR8
end ['R3','R4','R5'];


procedure do_comp_sets(set1,set2 : pointer);assembler;[public,alias:'FPC_SET_COMP_SETS'];
{
  compares set1 and set2 zeroflag is set if they are equal

  on entry: set1 in r3, set2 in r4
}
var
  saveR5, saveR6, saveR7: longint;
asm
       stw      r5,saveR5
       mfctr    r5
       stw      r6,saveR6
       li       r6,8
       stw      r7,saveR7
       mtctr    r6
       lwz      r6,(r3)
       lwz      r7,(r4)
    .LMCOMPSETS1:
       cmplw    cr0,r6,r7
       lwzu     r6,4(r3)
       lwzu     r7,4(r4)
       bdnzeq   cr0,.LMCOMPSETS1
       mtctr    r5
       lwz      r5,saveR5
       lwz      r6,saveR6
       lwz      r7,saveR7
end ['R3','R4'];

{$IfNDef NoSetInclusion}
procedure do_contains_sets(set1,set2 : pointer);assembler;[public,alias:'FPC_SET_CONTAINS_SETS'];
{
  on exit, zero flag is set if set1 <= set2 (set2 contains set1)
  on entry: set1 in r3, set2 in r4
}
var
  saveR5, saveR6, saveR7: longint;
asm
       stw      r5,saveR5
       mfctr    r5
       stw      r6,saveR6
       li       r6,8
       stw      r7,saveR7
       mtctr    r6
       lwz      r6,(r3)
       lwz      r7,(r4)
    .LMCOMPSETS1:
       andc.    r7,r6,r7
       lwzu     r6,4(r3)
       lwzu     r7,4(r4)
       bdnzeq   cr0,.LMCOMPSETS1
       mtctr    r5
       lwz      r5,saveR5
       lwz      r6,saveR6
       lwz      r7,saveR7
end ['R3','R4'];
{$EndIf SetInclusion}

{$ifdef LARGESETS}

procedure do_set(p : pointer;b : word);assembler;[public,alias:'FPC_SET_SET_WORD'];
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


procedure do_in(p : pointer;b : word);assembler;[public,alias:'FPC_SET_IN_WORD'];
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


procedure add_sets(set1,set2,dest : pointer;size : longint);assembler;[public,alias:'FPC_SET_ADD_SETS_SIZE'];
{
  adds set1 and set2 into set dest size is the number of bytes in the set
}
asm
      movl set1,%esi
      movl set2,%ebx
      movl dest,%edi
      movl size,%ecx
  .LMADDSETSIZES1:
      lodsl
      orl (%ebx),%eax
      stosl
      addl $4,%ebx
      decl %ecx
      jnz .LMADDSETSIZES1
end;


procedure mul_sets(set1,set2,dest : pointer;size : longint);assembler;[public,alias:'FPC_SET_MUL_SETS_SIZE'];
{
  multiplies (i.E. takes common elements of) set1 and set2 result put in
  dest size is the number of bytes in the set
}
asm
         movl set1,%esi
         movl set2,%ebx
         movl dest,%edi
         movl size,%ecx
     .LMMULSETSIZES1:
         lodsl
         andl (%ebx),%eax
         stosl
         addl $4,%ebx
         decl %ecx
         jnz .LMMULSETSIZES1
end;


procedure sub_sets(set1,set2,dest : pointer;size : longint);assembler;[public,alias:'FPC_SET_SUB_SETS_SIZE'];
asm
         movl set1,%esi
         movl set2,%ebx
         movl dest,%edi
         movl size,%ecx
     .LMSUBSETSIZES1:
         lodsl
         movl (%ebx),%edx
         notl %edx
         andl %edx,%eax
         stosl
         addl $4,%ebx
         decl %ecx
         jnz .LMSUBSETSIZES1
end;


procedure sym_sub_sets(set1,set2,dest : pointer;size : longint);assembler;[public,alias:'FPC_SET_SYMDIF_SETS_SIZE'];
{
   computes the symetric diff from set1 to set2 result in dest
}
asm
      movl set1,%esi
      movl set2,%ebx
      movl dest,%edi
      movl size,%ecx
  .LMSYMDIFSETSIZE1:
      lodsl
      movl (%ebx),%edx
      xorl %edx,%eax
      stosl
      addl $4,%ebx
      decl %ecx
      jnz .LMSYMDIFSETSIZE1
end;


procedure comp_sets(set1,set2 : pointer;size : longint);assembler;[public,alias:'FPC_SET_COMP_SETS_SIZE'];
asm
      movl set1,%esi
      movl set2,%edi
      movl size,%ecx
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

{$IfNDef NoSetInclusion}
procedure contains_sets(set1,set2 : pointer; size: longint);assembler;[public,alias:'FPC_SET_CONTAINS_SETS'];
{
  on exit, zero flag is set if set1 <= set2 (set2 contains set1)
}
asm
        movl set1,%esi
        movl set2,%edi
        movl size,%ecx
    .LMCONTAINSSETS2:
        movl (%esi),%eax
        movl (%edi),%edx
        andl %eax,%edx
        cmpl %edx,%eax  {set1 and set2 = set1?}
        jne  .LMCONTAINSSETEND2
        addl $4,%esi
        addl $4,%edi
        decl %ecx
        jnz .LMCONTAINSSETS2
        { we are here only if set2 contains set1
          we have zero flag set, and that what is expected }
    .LMCONTAINSSETEND2:
end;
{$EndIf NoSetInclusion}


{$endif LARGESET}

{
  $Log$
  Revision 1.3  2000-06-30 10:32:43  jonas
    * some optimizations suggested by Anton Rang in c.s.powerpc.misc

  Revision 1.1  2000/06/28 13:43:29  jonas
    * inital version, everything not yet implemented
}
