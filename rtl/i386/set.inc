{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ include file with procedures used for set operations }
{ these procedures should never be called directly     }
{ the compiler calls them                              }

  { add the element b to the set pointed by p }
  procedure do_set(p : pointer;b : byte);
    [public,alias: 'SET_SET_BYTE'];

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

  { bad implementation, but it's very seldom used }
  procedure do_set(p : pointer;l,h : byte);[public,alias: 'SET_SET_RANGE'];

    begin
       asm
          pushl %eax
          xorl %eax,%eax
          movb h,%al
       .LSET_SET_RANGE_LOOP:
          cmpb %al,l
          jb .LSET_SET_RANGE_EXIT
          pushw %ax
          pushl p
          call SET_SET_BYTE
          dec %al
          jmp .LSET_SET_RANGE_LOOP
       .LSET_SET_RANGE_EXIT:
          popl %eax
       end;
    end;

  { tests if the element b is in the set p      }
  { the carryflag is set if it present          }

  procedure do_in(p : pointer;b : byte);
    [public,alias: 'SET_IN_BYTE'];

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
   { adds set1 and set2 into set dest }

   procedure add_sets(set1,set2,dest : pointer);[public,alias: 'SET_ADD_SETS'];

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

   procedure mul_sets(set1,set2,dest : pointer);[public,alias: 'SET_MUL_SETS'];

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

   { computes the diff from set1 to set2    }
   { result in dest                         }

   procedure sub_sets(set1,set2,dest : pointer);[public,alias: 'SET_SUB_SETS'];

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

   { computes the symetric diff from set1 to set2    }
   { result in dest                                  }

   procedure sym_sub_sets(set1,set2,dest : pointer);[public,alias: 'SET_SYMDIF_SETS'];

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

   { compares set1 and set2                             }
   { zeroflag is set if they are equal                  }
   procedure comp_sets(set1,set2 : pointer);[public,alias: 'SET_COMP_SETS'];

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
           // we are here only if the two sets are equal
           // we have zero flag set, and that what is expected
           cmpl %eax,%eax
       .LMCOMPSETEND:
        end;
     end;

  { sets the element b in set p }
  { works for sets larger than 256 elements }
  { not yet use by the compiler so }
{$ifdef ver_above without the number }
  procedure do_set(p : pointer;b : word);[public,alias: 'SET_SET_WORD'];
  
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

  { tests if the element b is in the set p      }
  { the carryflag is set if it present          }
  { works for sets larger than 256 elements }

  procedure do_in(p : pointer;b : word);[public,alias: 'SET_IN_WORD'];

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

   { adds set1 and set2 into set dest       }
   { size is the number of bytes in the set }

   procedure add_sets(set1,set2,dest : pointer;size : longint);
             [public,alias: 'SET_ADD_SETS_SIZE'];

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

   { multiplies (i.E. takes common elements of) set1 and set2 }
   { result put in dest                                       }
   { size is the number of bytes in the set                   }

   procedure mul_sets(set1,set2,dest : pointer;size : longint);
             [public,alias: 'SET_MUL_SETS_SIZE'];

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


   procedure sub_sets(set1,set2,dest : pointer;size : longint);[public,alias: 'SET_SUB_SETS_SIZE'];

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

   { computes the symetric diff from set1 to set2    }
   { result in dest                                  }

   procedure sym_sub_sets(set1,set2,dest : pointer;size : longint);[public,alias: 'SET_SYMDIF_SETS_SIZE'];

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


   procedure comp_sets(set1,set2 : pointer;size : longint);[public,alias: 'SET_COMP_SETS_SIZE'];

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
           // we are here only if the two sets are equal
           // we have zero flag set, and that what is expected
           cmpl %eax,%eax
       .LMCOMPSETSIZEEND:
        end;
     end;

{$endif ver_above without the number }

{
  $Log$
  Revision 1.1  1998-03-25 11:18:42  root
  Initial revision

  Revision 1.7  1998/03/03 12:07:11  florian
    * undid the change of some procedures to plain assembler procedures

  Revision 1.6  1998/03/02 23:10:33  florian
    * SET_* are now assembler procedures

  Revision 1.5  1998/02/11 18:37:01  florian
    * stupid typing mistake fixed (I though it compiles, but the assembler
      wrote an error message)

  Revision 1.4  1998/02/11 16:17:45  florian
    + helper routine for "dynamic" set constructors with ranges added

  Revision 1.3  1998/01/26 11:59:09  michael
  + Added log at the end


  
  Working file: rtl/i386/set.inc
  description:
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:34:37;  author: michael;  state: Exp;  lines: +11 -4
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:48;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:48;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
