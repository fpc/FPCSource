{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Florian Klaempfl.
    Member of the Free Pascal development team

    Parts of this code are derived from the x86-64 linux port
    Copyright 2002 Andi Kleen

    Processor dependent implementation for the system unit for
    the x86-64 architecture

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode DIRECT}

{****************************************************************************
                               Primitives
****************************************************************************}

{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:longint);assembler;
  asm
     { rdi destination
       rsi source
       rdx count
     }
     pushq %rbx
     prefetcht0 (%rsi)	// for more hopefully the hw prefetch will kick in
     movq %rdi,%rax

     movl %edi,%ecx
     andl $7,%ecx
     jnz  bad_alignment
after_bad_alignment:
     movq %rdx,%rcx
     movl $64,%ebx
     shrq $6,%rcx
     jz handle_tail

loop_64:
     { no prefetch because we assume the hw prefetcher does it already
       and we have no specific temporal hint to give. XXX or give a nta
       hint for the source? }
     movq (%rsi),%r11
     movq 8(%rsi),%r8
     movq 2*8(%rsi),%r9
     movq 3*8(%rsi),%r10
     movnti %r11,(%rdi)
     movnti %r8,1*8(%rdi)
     movnti %r9,2*8(%rdi)
     movnti %r10,3*8(%rdi)

     movq 4*8(%rsi),%r11
     movq 5*8(%rsi),%r8
     movq 6*8(%rsi),%r9
     movq 7*8(%rsi),%r10
     movnti %r11,4*8(%rdi)
     movnti %r8,5*8(%rdi)
     movnti %r9,6*8(%rdi)
     movnti %r10,7*8(%rdi)

     addq %rbx,%rsi
     addq %rbx,%rdi
     loop loop_64

handle_tail:
     movl %edx,%ecx
     andl $63,%ecx
     shrl $3,%ecx
     jz   handle_7
     movl $8,%ebx
loop_8:
     movq (%rsi),%r8
     movnti %r8,(%rdi)
     addq %rbx,%rdi
     addq %rbx,%rsi
     loop loop_8

handle_7:
     movl %edx,%ecx
     andl $7,%ecx
     jz ende
loop_1:
     movb (%rsi),%r8b
     movb %r8b,(%rdi)
     incq %rdi
     incq %rsi
     loop loop_1

ende:
     sfence
     popq %rbx
     ret

     /* align destination */
     /* This is simpleminded. For bigger blocks it may make sense to align
        src and dst to their aligned subset and handle the rest separately */
bad_alignment:
     movl $8,%r9d
     subl %ecx,%r9d
     movl %r9d,%ecx
     subq %r9,%rdx
     js   small_alignment
     jz   small_alignment
align_1:
     movb (%rsi),%r8b
     movb %r8b,(%rdi)
     incq %rdi
     incq %rsi
     loop align_1
     jmp after_bad_alignment
small_alignment:
     addq %r9,%rdx
     jmp handle_7
  end;

{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);
  asm
    { rdi   destination
      rsi   value (char)
      rdx   count (bytes)
    }
    movq %rdi,%r10
    movq %rdx,%r11

    /* expand byte value  */
    movzbl %sil,%ecx
    movabs $0x0101010101010101,%rax
    mul    %rcx		/* with rax, clobbers rdx */

    /* align dst */
    movl  %edi,%r9d
    andl  $7,%r9d
    jnz  bad_alignment
after_bad_alignment:

     movq %r11,%rcx
     movl $64,%r8d
     shrq $6,%rcx
     jz	 handle_tail

loop_64:
     movnti  %rax,(%rdi)
     movnti  %rax,8(%rdi)
     movnti  %rax,16(%rdi)
     movnti  %rax,24(%rdi)
     movnti  %rax,32(%rdi)
     movnti  %rax,40(%rdi)
     movnti  %rax,48(%rdi)
     movnti  %rax,56(%rdi)
     addq    %r8,%rdi
     loop	loop_64

     { Handle tail in loops. The loops should be faster than hard
        to predict jump tables. }
handle_tail:
     movl	%r11d,%ecx
     andl    $63&(~7),%ecx
     jz 		handle_7
     shrl	$3,%ecx
loop_8:
     movnti  %rax,(%rdi)
     addq    $8,%rdi
     loop 	loop_8
handle_7:
     movl	%r11d,%ecx
     andl	$7,%ecx
     jz      ende
loop_1:
     movb 	%al,(%rdi)
     addq	$1,%rdi
     loop	loop_1

ende:
     movq	%r10,%rax
     ret
bad_alignment:
     cmpq $7,%r11
     jbe handle_7
     movnti %rax,(%rdi)	/* unaligned store */
     movq $8,%r8
     subq %r9,%r8
     addq %r8,%rdi
     subq %r8,%r11
     jmp after_bad_alignment
  end;

{ does a thread save inc/dec }
function declocked(var l : longint) : boolean;assembler;
  asm
     {
       l: %rdi
     }
{$ifdef MT}
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     cmpb       $0,IsMultithread
     jz         .Ldeclockednolock
     lock
     decl       (%rdi)
     jmp        .Ldeclockedend
.Ldeclockednolock:
{$endif MT}
     decl       (%rdi);
.Ldeclockedend:
     setzb      %al
  end;

procedure inclocked(var l : longint);assembler;

  asm
     {
       l: %rdi
     }
{$ifdef MT}
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     cmpb       $0,IsMultithread
     jz         .Linclockednolock
     lock
     incl       (%rdi)
     jmp        .Linclockedend
.Linclockednolock:
{$endif MT}
     incl       (%rdi)
.Linclockedend:
  end;

{
  $Log$
  Revision 1.1  2003-01-06 19:40:18  florian
    + initial revision
}
