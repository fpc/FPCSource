{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    Include file with set operations called by the compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ASMMODE ATT}

procedure do_load_small(p : pointer;l:longint);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_LOAD_SMALL'];
{
  load a set from an
}
begin
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
end;


procedure do_set_byte(p : pointer;b : byte); [public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_SET_BYTE'];
{
  add the element b to the set pointed by p
}
begin
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
       { own exit, the compiler generates a ret $8, IMHO }
       leave
       ret $6
    end;
end;


procedure do_set_range(p : pointer;l,h : byte);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_SET_RANGE'];
{
  bad implementation, but it's very seldom used
}
begin
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
end;


procedure do_in_byte(p : pointer;b : byte);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_IN_BYTE'];
{
  tests if the element b is in the set p the carryflag is set if it present
}
begin
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
       { own exit, the compiler generates a ret $8, IMHO }
       leave
       ret $6
    end;
end;



procedure do_add_sets(set1,set2,dest : pointer);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_ADD_SETS'];
{
  adds set1 and set2 into set dest
}
begin
   asm
      movl 8(%ebp),%esi
      movl 12(%ebp),%ebx
      movl 16(%ebp),%edi
      movl $8,%ecx
   .LMADDSETS1:
      lodsl
      orl (%ebx),%eax
      stosl
      addl $4,%ebx
      decl %ecx
      jnz .LMADDSETS1
   end;
end;


{ multiplies (i.E. takes common elements of) set1 and set2 }
{ result put in dest                                       }

procedure do_mul_sets(set1,set2,dest : pointer);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_MUL_SETS'];
begin
   asm
      movl 8(%ebp),%esi
      movl 12(%ebp),%ebx
      movl 16(%ebp),%edi
      movl $8,%ecx
  .LMMULSETS1:
      lodsl
      andl (%ebx),%eax
      stosl
      addl $4,%ebx
      decl %ecx
      jnz .LMMULSETS1
   end;
end;


procedure do_sub_sets(set1,set2,dest : pointer);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_SUB_SETS'];
{
  computes the diff from set1 to set2 result in dest
}
begin
     asm
        movl 8(%ebp),%esi
        movl 12(%ebp),%ebx
        movl 16(%ebp),%edi
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
end;


procedure do_symdif_sets(set1,set2,dest : pointer);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_SYMDIF_SETS'];
{
   computes the symetric diff from set1 to set2 result in dest
}
begin
     asm
        movl 8(%ebp),%esi
        movl 12(%ebp),%ebx
        movl 16(%ebp),%edi
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
end;

procedure do_comp_sets(set1,set2 : pointer);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_COMP_SETS'];
{
  compares set1 and set2 zeroflag is set if they are equal
}
begin
     asm
        movl 8(%ebp),%esi
        movl 12(%ebp),%edi
        movl $8,%ecx
    .LMCOMPSETS1:
        lodsl
        movl (%edi),%edx
        cmpl %edx,%eax
        jne  .LMCOMPSETEND
        addl $4,%edi
        decl %ecx
        jnz .LMCOMPSETS1
        { we are here only if the two sets are equal
          we have zero flag set, and that what is expected }
        cmpl %eax,%eax
    .LMCOMPSETEND:
     end;
end;


{$ifdef LARGESETS}

procedure do_set(p : pointer;b : word);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_SET_WORD'];
{
  sets the element b in set p works for sets larger than 256 elements
  not yet use by the compiler so
}
begin
    asm
       pushl %eax
       movl 8(%ebp),%edi
       movw 12(%ebp),%ax
       andl $0xfff8,%eax
       shrl $3,%eax
       addl %eax,%edi
       movb 12(%ebp),%al
       andl $7,%eax
       btsl %eax,(%edi)
       popl %eax
    end;
end;


procedure do_in(p : pointer;b : word);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_IN_WORD'];
{
  tests if the element b is in the set p the carryflag is set if it present
  works for sets larger than 256 elements
}
begin
     asm
        pushl %eax
        movl 8(%ebp),%edi
        movw 12(%ebp),%ax
        andl $0xfff8,%eax
        shrl $3,%eax
        addl %eax,%edi
        movb 12(%ebp),%al
        andl $7,%eax
        btl %eax,(%edi)
        popl %eax
     end;
end;


procedure add_sets(set1,set2,dest : pointer;size : longint);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_ADD_SETS_SIZE'];
{
  adds set1 and set2 into set dest size is the number of bytes in the set
}

begin
   asm
      movl 8(%ebp),%esi
      movl 12(%ebp),%ebx
      movl 16(%ebp),%edi
      movl 20(%ebp),%ecx
  .LMADDSETSIZES1:
      lodsl
      orl (%ebx),%eax
      stosl
      addl $4,%ebx
      decl %ecx
      jnz .LMADDSETSIZES1
   end;
end;


procedure mul_sets(set1,set2,dest : pointer;size : longint);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_MUL_SETS_SIZE'];
{
  multiplies (i.E. takes common elements of) set1 and set2 result put in
  dest size is the number of bytes in the set
}
begin
      asm
         movl 8(%ebp),%esi
         movl 12(%ebp),%ebx
         movl 16(%ebp),%edi
         movl 20(%ebp),%ecx
     .LMMULSETSIZES1:
         lodsl
         andl (%ebx),%eax
         stosl
         addl $4,%ebx
         decl %ecx
         jnz .LMMULSETSIZES1
      end;
end;


procedure sub_sets(set1,set2,dest : pointer;size : longint);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_SUB_SETS_SIZE'];
begin
      asm
         movl 8(%ebp),%esi
         movl 12(%ebp),%ebx
         movl 16(%ebp),%edi
         movl 20(%ebp),%ecx
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
end;


procedure sym_sub_sets(set1,set2,dest : pointer;size : longint);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_SYMDIF_SETS_SIZE'];
{
   computes the symetric diff from set1 to set2 result in dest
}
begin
   asm
      movl 8(%ebp),%esi
      movl 12(%ebp),%ebx
      movl 16(%ebp),%edi
      movl 20(%ebp),%ecx
  .LMSYMDIFSETSIZE1:
      lodsl
      movl (%ebx),%edx
      xorl %edx,%eax
      stosl
      addl $4,%ebx
      decl %ecx
      jnz .LMSYMDIFSETSIZE1
   end;
end;


procedure comp_sets(set1,set2 : pointer;size : longint);[public,alias: {$ifdef FPCNAMES}'FPC_'+{$endif}'SET_COMP_SETS_SIZE'];
begin
   asm
      movl 8(%ebp),%esi
      movl 12(%ebp),%edi
      movl 16(%ebp),%ecx
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
      cmpl %eax,%eax
  .LMCOMPSETSIZEEND:
   end;
end;

{$endif LARGESET}

{
  $Log$
  Revision 1.4  1998-09-14 10:48:12  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.3  1998/08/14 18:13:44  peter
    + set_load_small
    * fixed set_set_range

  Revision 1.2  1998/05/31 14:15:51  peter
    * force to use ATT or direct parsing

}
