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
function fpc_set_load_small(l: fpc_small_set): fpc_normal_set;assembler;[public,alias:'FPC_SET_LOAD_SMALL']; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  load a normal set p from a smallset l
}
asm
        movl    __RESULT,%edi
        movl    l,%eax
        stosl
        xorl    %eax,%eax
        movl    $7,%ecx
        rep
        stosl
end ['EAX','ECX','EDI'];

{$define FPC_SYSTEM_HAS_FPC_SET_CREATE_ELEMENT}

function fpc_set_create_element(b : byte): fpc_normal_set;assembler;[public,alias:'FPC_SET_CREATE_ELEMENT']; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  create a new set in p from an element b
}
asm
{$ifndef hascompilerproc}
        pushl   %eax
        pushl   %ecx
{$endif not hascompilerproc}
        movl    __RESULT,%edi
        xorl    %eax,%eax
        movl    $8,%ecx
        rep
        stosl
        leal    -32(%edi),%eax
        movzbl  b,%edi
        btsl    %edi,(%eax)
{$ifndef hascompilerproc}
        popl    %ecx
        popl    %eax
{$endif hascompilerproc}
end ['EAX','ECX','EDI'];


{$define FPC_SYSTEM_HAS_FPC_SET_SET_BYTE}
{$ifdef hascompilerproc}
function fpc_set_set_byte(const source: fpc_normal_set; b : byte): fpc_normal_set;assembler; compilerproc;
{
  add the element b to the set pointed by source
}
asm
       movl $8,%ecx
       movl source,%esi
       movl __RESULT,%edi
       rep
       movsl
       leal    -32(%edi),%eax
       movzbl  b,%edi
       btsl    %edi,(%eax)
end ['EAX','ECX','ESI','EDI'];
{$else hascompilerproc}
function fpc_set_set_byte(b : byte): fpc_normal_set;assembler;[public,alias:'FPC_SET_SET_BYTE'];
{
  add the element b to the set pointed by p
}
asm
       pushl %eax
       movl __RESULT,%edi
       movb b,%al
       andl $0xf8,%eax
       shrl $3,%eax
       addl %eax,%edi
       movb b,%al
       andl $7,%eax
       btsl %eax,(%edi)
       popl %eax
end;
{$endif hascompilerproc}


{$define FPC_SYSTEM_HAS_FPC_SET_UNSET_BYTE}
{$ifdef hascompilerproc}
function fpc_set_unset_byte(const source: fpc_normal_set; b : byte): fpc_normal_set;assembler; compilerproc;
{
  add the element b to the set pointed by source
}
asm
       movl $8,%ecx
       movl source,%esi
       movl __RESULT,%edi
       rep
       movsl
       leal    -32(%edi),%eax
       movzbl  b,%edi
       btrl    %edi,(%eax)
end ['EAX','ECX','ESI','EDI'];
{$else hascompilerproc}
function fpc_set_unset_byte(b : byte): fpc_normal_set;assembler;[public,alias:'FPC_SET_UNSET_BYTE']; {$ifdef hascompilerproc} compilerproc; {$endif}
{
  suppresses the element b to the set pointed by p
  used for exclude(set,element)
}
asm
       pushl %eax
       movl __RESULT,%edi
       movb b,%al
       andl $0xf8,%eax
       shrl $3,%eax
       addl %eax,%edi
       movb b,%al
       andl $7,%eax
       btrl %eax,(%edi)
       popl %eax
end;
{$endif hascompilerproc}


{$define FPC_SYSTEM_HAS_FPC_SET_SET_RANGE}

{$ifdef hascompilerproc}
function fpc_set_set_range(const orgset: fpc_normal_set; l,h : byte): fpc_normal_set;assembler; compilerproc;
{
  adds the range [l..h] to the set pointed to by p
}
asm
        movzbl l,%eax               // lowest bit to be set in eax
        movzbl h,%ebx               // highest in ebx
        movl   $8,%ecx              // we have to copy 32 bytes
        movl   __RESULT,%edi        // target set address in edi
        movl   orgset, %esi         // source set address in esi
        cmpl   %eax,%ebx            // high < low?
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
        movb   h,%cl
        movl   %edx,%ebx            // save current bitmask
        andb   $31,%cl
        subb   $31,%cl              // cl := (31 - (hi and 31)) = shift count to
        negb   %cl                  // adjust bitmask for hi bit
        shrl   %cl,%edx             // shift bitmask to clear bits higher than hi
        andl   %edx,%ebx            // combine both bitmasks
        orl    %ebx,(%edi)          // store to set
.Lset_range_done:
end;

{$else hascompilerproc}

function fpc_set_set_range(l,h : byte): fpc_normal_set;assembler;[public,alias:'FPC_SET_SET_RANGE'];
{
  adds the range [l..h] to the set pointed to by p
}
asm
        movzbl l,%eax              // lowest bit to be set in eax
        movzbl h,%ebx              // highest in ebx
        cmpl   %eax,%ebx
        jb     .Lset_range_done
        movl   __RESULT,%edi               // set address in edi
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
end;
{$endif hascompilerproc}


{$define FPC_SYSTEM_HAS_FPC_SET_IN_BYTE}


{$ifdef hascompilerproc}
{ it's inlined in the code generator }
function fpc_set_in_byte(const p: fpc_normal_set; b: byte): boolean; compilerproc;
begin
  { make sure we won't accidentally call it }
  runerror(216);
end;
{$endif hascompilerproc}

function fpc_set_in_byte_i386(p: pointer; b : byte): boolean;assembler;[public,alias:'FPC_SET_IN_BYTE'];
{
  tests if the element b is in the set p the carryflag is set if it present
}
asm
{ it's inlined in the code generator }
{$ifndef hascompilerproc}
       pushl %eax
       movl   p,%edi
       movzbl b,%eax
       btl %eax,(%edi)
       popl %eax
{$endif not hascompilerproc}
end;


{$define FPC_SYSTEM_HAS_FPC_SET_ADD_SETS}

{$ifdef hascompilerproc}
function fpc_set_add_sets(const set1,set2: fpc_normal_set): fpc_normal_set;assembler;[public,alias:'FPC_SET_ADD_SETS']; compilerproc;
{$else hascompilerproc}
procedure fpc_set_add_sets(set1,set2,dest : pointer);assembler;[public,alias:'FPC_SET_ADD_SETS'];
{$endif hascompilerproc}
{
  adds set1 and set2 into set dest
}
asm
      movl set1,%esi
      movl set2,%ebx
{$ifdef hascompilerproc}
      movl __RESULT,%edi
{$else hascompilerproc}
      movl dest,%edi
{$endif hascompilerproc}
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

{$ifdef hascompilerproc}
function fpc_set_mul_sets(const set1,set2: fpc_normal_set): fpc_normal_set;assembler;[public,alias:'FPC_SET_MUL_SETS']; compilerproc;
{$else hascompilerproc}
procedure fpc_set_mul_sets(set1,set2,dest:pointer);assembler;[public,alias:'FPC_SET_MUL_SETS'];
{$endif hascompilerproc}
{
  multiplies (takes common elements of) set1 and set2 result put in dest
}
asm
      movl set1,%esi
      movl set2,%ebx
{$ifdef hascompilerproc}
      movl __RESULT,%edi
{$else hascompilerproc}
      movl dest,%edi
{$endif hascompilerproc}
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

{$ifdef hascompilerproc}
function fpc_set_sub_sets(const set1,set2: fpc_normal_set): fpc_normal_set;assembler;[public,alias:'FPC_SET_SUB_SETS']; compilerproc;
{$else hascompilerproc}
procedure fpc_set_sub_sets(set1,set2,dest:pointer);assembler;[public,alias:'FPC_SET_SUB_SETS'];
{$endif hascompilerproc}
{
  computes the diff from set1 to set2 result in dest
}
asm
        movl set1,%esi
        movl set2,%ebx
{$ifdef hascompilerproc}
      movl __RESULT,%edi
{$else hascompilerproc}
      movl dest,%edi
{$endif hascompilerproc}
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

{$ifdef hascompilerproc}
function fpc_set_symdif_sets(const set1,set2: fpc_normal_set): fpc_normal_set;assembler;[public,alias:'FPC_SET_SYMDIF_SETS']; compilerproc;
{$else hascompilerproc}
procedure fpc_set_symdif_sets(set1,set2,dest:pointer);assembler;[public,alias:'FPC_SET_SYMDIF_SETS'];
{$endif hascompilerproc}
{
   computes the symetric diff from set1 to set2 result in dest
}
asm
        movl set1,%esi
        movl set2,%ebx
{$ifdef hascompilerproc}
      movl __RESULT,%edi
{$else hascompilerproc}
      movl dest,%edi
{$endif hascompilerproc}
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

function fpc_set_comp_sets(const set1,set2: fpc_normal_set): boolean;assembler;[public,alias:'FPC_SET_COMP_SETS']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
{$ifdef hascompilerproc}
        seteb %al
{$endif hascompilerproc}
end;


{$define FPC_SYSTEM_HAS_FPC_SET_CONTAINS_SET}

function fpc_set_contains_sets(const set1,set2: fpc_normal_set): boolean;assembler;[public,alias:'FPC_SET_CONTAINS_SETS']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
{$ifdef hascompilerproc}
        seteb %al
{$endif hascompilerproc}
end;


{$ifdef LARGESETS}

procedure fpc_largeset_set_word(p : pointer;b : word);assembler;[public,alias:'FPC_LARGESET_SET_WORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


procedure fpc_largeset_in_word(p : pointer;b : word);assembler;[public,alias:'FPC_LARGESET_IN_WORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


procedure fpc_largeset_add_sets(set1,set2,dest : pointer;size : longint);assembler;[public,alias:'FPC_LARGESET_ADD_SETS']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


procedure fpc_largeset_mul_sets(set1,set2,dest : pointer;size : longint);assembler;[public,alias:'FPC_LARGESET_MUL_SETS']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


procedure fpc_largeset_sub_sets(set1,set2,dest : pointer;size : longint);assembler;[public,alias:'FPC_LARGESET_SUB_SETS']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


procedure fpc_largeset_symdif_sets(set1,set2,dest : pointer;size : longint);assembler;[public,alias:'FPC_LARGESET_SYMDIF_SETS']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


procedure fpc_largeset_comp_sets(set1,set2 : pointer;size : longint);assembler;[public,alias:'FPC_LARGESET_COMP_SETS']; {$ifdef hascompilerproc} compilerproc; {$endif}
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

procedure fpc_largeset_contains_sets(set1,set2 : pointer; size: longint);assembler;[public,alias:'FPC_LARGESET_CONTAINS_SETS']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


{$endif LARGESET}

{
  $Log$
  Revision 1.8  2002-03-29 20:15:44  peter
    * fixed load_smallset

  Revision 1.7  2001/09/04 11:38:55  jonas
    + searchsystype() and searchsystype() functions in symtable
    * changed ninl and nadd to use these functions
    * i386 set comparison functions now return their results in al instead
      of in the flags so that they can be sued as compilerprocs
    - removed all processor specific code from n386add.pas that has to do
      with set handling, it's now all done in nadd.pas
    * fixed fpc_set_contains_sets in genset.inc
    * fpc_set_in_byte is now coded inline in n386set.pas and doesn't use a
      helper anymore
    * some small fixes in compproc.inc/set.inc regarding the declaration of
      internal helper types (fpc_small_set and fpc_normal_set)

  Revision 1.6  2001/09/03 13:27:43  jonas
    * compilerproc implementation of set addition/substraction/...
    * changed the declaration of some set helpers somewhat to accomodate the
      above change
    * i386 still uses the old code for comparisons of sets, because its
      helpers return the results in the flags
    * dummy tc_normal_2_small_set type conversion because I need the original
      resulttype of the set add nodes
    NOTE: you have to start a cycle with 1.0.5!

  Revision 1.5  2001/08/01 15:00:10  jonas
    + "compproc" helpers
    * renamed several helpers so that their name is the same as their
      "public alias", which should facilitate the conversion of processor
      specific code in the code generator to processor independent code
    * some small fixes to the val_ansistring and val_widestring helpers
      (always immediately exit if the source string is longer than 255
       chars)
    * fixed fpc_dynarray_high and fpc_dynarray_length if the dynarray is
      still nil (used to crash, now return resp -1 and 0)

  Revision 1.4  2001/05/09 19:57:07  peter
  *** empty log message ***

  Revision 1.3  2000/09/21 16:09:19  jonas
    + new, much faster do_set_range based on the PowerPC version (which
      will be committed tomorrow)

  Revision 1.2  2000/07/13 11:33:41  michael
  + removed logs

}
