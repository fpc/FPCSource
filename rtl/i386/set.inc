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
}
asm
        movl    p,%edi
        movl    l,%eax
        movl    %eax,(%edi)
        addl    $4,%edi
        movl    $7,%ecx
        xorl    %eax,%eax
        rep
        stosl
end;


procedure do_create_element(p : pointer;b : byte);assembler;[public,alias:'FPC_SET_CREATE_ELEMENT'];
{
  create a new set in p from an element b
}
asm
        pushl   %eax
        pushl   %ecx
        movl    p,%edi
        xorl    %eax,%eax
        movl    $8,%ecx
        rep
        stosl
        movb    b,%al
        movl    p,%edi
        movl    %eax,%ecx
        shrl    $3,%eax
        andl    $7,%ecx
        addl    %eax,%edi
        btsl    %ecx,(%edi)
        popl    %ecx
        popl    %eax
end;

procedure do_set_byte(p : pointer;b : byte);assembler;[public,alias:'FPC_SET_SET_BYTE'];
{
  add the element b to the set pointed by p
}
asm
       pushl %eax
       movl p,%edi
       movb b,%al
       andl $0xf8,%eax
       shrl $3,%eax
       addl %eax,%edi
       movb b,%al
       andl $7,%eax
       btsl %eax,(%edi)
       popl %eax
end;


procedure do_unset_byte(p : pointer;b : byte);assembler;[public,alias:'FPC_SET_UNSET_BYTE'];
{
  suppresses the element b to the set pointed by p
  used for exclude(set,element)
}
asm
       pushl %eax
       movl p,%edi
       movb b,%al
       andl $0xf8,%eax
       shrl $3,%eax
       addl %eax,%edi
       movb b,%al
       andl $7,%eax
       btrl %eax,(%edi)
       popl %eax
end;


procedure do_set_range(p : pointer;l,h : byte);assembler;[public,alias:'FPC_SET_SET_RANGE'];
{
  bad implementation, but it's very seldom used
}
asm
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
end;


procedure do_in_byte(p : pointer;b : byte);assembler;[public,alias:'FPC_SET_IN_BYTE'];
{
  tests if the element b is in the set p the carryflag is set if it present
}
asm
       pushl %eax
       movl p,%edi
       movb b,%al
       andl $0xf8,%eax
       shrl $3,%eax
       addl %eax,%edi
       movb b,%al
       andl $7,%eax
       btl %eax,(%edi)
       popl %eax
end;



procedure do_add_sets(set1,set2,dest : pointer);assembler;[public,alias:'FPC_SET_ADD_SETS'];
{
  adds set1 and set2 into set dest
}
asm
      movl set1,%esi
      movl set2,%ebx
      movl dest,%edi
      movl $8,%ecx
   .LMADDSETS1:
      lodsl
      orl (%ebx),%eax
      stosl
      addl $4,%ebx
      decl %ecx
      jnz .LMADDSETS1
end;



procedure do_mul_sets(set1,set2,dest:pointer);assembler;[public,alias:'FPC_SET_MUL_SETS'];
{
  multiplies (takes common elements of) set1 and set2 result put in dest
}
asm
      movl set1,%esi
      movl set2,%ebx
      movl dest,%edi
      movl $8,%ecx
  .LMMULSETS1:
      lodsl
      andl (%ebx),%eax
      stosl
      addl $4,%ebx
      decl %ecx
      jnz .LMMULSETS1
end;


procedure do_sub_sets(set1,set2,dest:pointer);assembler;[public,alias:'FPC_SET_SUB_SETS'];
{
  computes the diff from set1 to set2 result in dest
}
asm
        movl set1,%esi
        movl set2,%ebx
        movl dest,%edi
        movl $8,%ecx
    .LMSUBSETS1:
        lodsl
        movl (%ebx),%edx
        notl %edx
        andl %edx,%eax
        stosl
        addl $4,%ebx
        decl %ecx
        jnz .LMSUBSETS1
end;


procedure do_symdif_sets(set1,set2,dest:pointer);assembler;[public,alias:'FPC_SET_SYMDIF_SETS'];
{
   computes the symetric diff from set1 to set2 result in dest
}
asm
        movl set1,%esi
        movl set2,%ebx
        movl dest,%edi
        movl $8,%ecx
    .LMSYMDIFSETS1:
        lodsl
        movl (%ebx),%edx
        xorl %edx,%eax
        stosl
        addl $4,%ebx
        decl %ecx
        jnz .LMSYMDIFSETS1
end;


procedure do_comp_sets(set1,set2 : pointer);assembler;[public,alias:'FPC_SET_COMP_SETS'];
{
  compares set1 and set2 zeroflag is set if they are equal
}
asm
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
end;

{$IfNDef NoSetInclusion}
procedure do_contains_sets(set1,set2 : pointer);assembler;[public,alias:'FPC_SET_CONTAINS_SETS'];
{
  on exit, zero flag is set if set1 <= set2 (set2 contains set1)
}
asm
        movl set1,%esi
        movl set2,%edi
        movl $8,%ecx
    .LMCONTAINSSETS1:
        movl (%esi),%eax
        movl (%edi),%edx
        andl %eax,%edx
        cmpl %edx,%eax  {set1 and set2 = set1?}
        jne  .LMCONTAINSSETEND
        addl $4,%esi
        addl $4,%edi
        decl %ecx
        jnz .LMCONTAINSSETS1
        { we are here only if set2 contains set1
          we have zero flag set, and that what is expected }
    .LMCONTAINSSETEND:
end;
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
  Revision 1.2  2000-07-13 11:33:41  michael
  + removed logs
 
}
