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

{$define FPC_SYSTEM_HAS_FPC_SET_LOAD_SMALL}
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

{$define FPC_SYSTEM_HAS_FPC_SET_CREATE_ELEMENT}
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


{$define FPC_SYSTEM_HAS_FPC_SET_SET_BYTE}
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


{$define FPC_SYSTEM_HAS_FPC_SET_UNSET_BYTE}
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


{$define FPC_SYSTEM_HAS_FPC_SET_SET_RANGE}
procedure do_set_range(p : pointer;l,h : byte);assembler;[public,alias:'FPC_SET_SET_RANGE'];
{
  adds the range [l..h] to the set pointed to by p
}
asm
        pushl   %eax
        movzbl l,%eax              // lowest bit to be set in eax
        movzbl h,%ebx              // highest in ebx
        cmpl   %eax,%ebx
        jb     .Lset_range_done
        movl   p,%edi               // set address in edi
        movl   %eax,%ecx            // lowest also in ecx
        shrl   $3,%eax              // divide by 8 to get starting and ending byte
        shrl   $3,%ebx              // address
        andb   $31,%cl              // low five bits of lo determine start of bit mask
        movl   $0x0ffffffff,%edx    // edx = bitmask to be inserted
        andl   $0x0fffffffc,%eax    // clear two lowest bits to get start/end longint
        andl   $0x0fffffffc,%ebx    // address * 4
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
        movb   h,%cl
        movl   %edx,%ebx            // save current bitmask
        andb   $31,%cl
        subb   $31,%cl              // cl := (31 - (hi and 31)) = shift count to
        negb   %cl                  // adjust bitmask for hi bit
        shrl   %cl,%edx             // shift bitmask to clear bits higher than hi
        andl   %edx,%ebx            // combine both bitmasks
        orl    %ebx,(%edi)          // store to set
.Lset_range_done:
        popl %eax
end;


{$define FPC_SYSTEM_HAS_FPC_SET_IN_BYTE}
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


{$define FPC_SYSTEM_HAS_FPC_SET_ADD_SETS}
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


{$define FPC_SYSTEM_HAS_FPC_SET_MUL_SETS}
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


{$define FPC_SYSTEM_HAS_FPC_SET_SUB_SETS}
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


{$define FPC_SYSTEM_HAS_FPC_SET_SYMDIF_SETS}
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


{$define FPC_SYSTEM_HAS_FPC_SET_COMP_SETS}
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
{$define FPC_SYSTEM_HAS_FPC_SET_CONTAINS_SET}
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
  Revision 1.4  2001-05-09 19:57:07  peter
  *** empty log message ***

  Revision 1.3  2000/09/21 16:09:19  jonas
    + new, much faster do_set_range based on the PowerPC version (which
      will be committed tomorrow)

  Revision 1.2  2000/07/13 11:33:41  michael
  + removed logs

}
