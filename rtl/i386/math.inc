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

{ Implementation of mathamatical Routines (only for real) }

    function abs(d : real) : real;
    
      begin
         asm
            fldl 8(%ebp)
            fabs
            leave
            ret $8
         end [];
      end;
      
    function sqr(d : real) : real;
    
      begin
         asm
            fldl 8(%ebp)
            fldl 8(%ebp)
            fmulp
            leave
            ret $8
         end [];
      end;
      
    function sqrt(d : real) : real;
    
      begin
         asm
            fldl 8(%ebp)
            fsqrtl
            leave
            ret $8
         end [];
      end;
      
    function arctan(d : real) : real;
    
      begin
         asm
            fldl 8(%ebp)
            fld1
            fpatan
            leave
            ret $8
         end [];
      end;
      
    function cos(d : real) : real;
    
      begin
         asm
            fldl 8(%ebp)
            fcos
            fstsw
            sahf
            jnp .LCOS1
            fstp %st(0)
            fldl .LCOS0
         .LCOS1:
            leave
            ret	$8
         .LCOS0:
            .quad	0xffffffffffffffff
         end ['EAX'];
      end;
      
    function exp(d : real) : real;
    
      begin
         asm
            // comes from DJ GPP
	    fldl	8(%ebp)
	    fldl2e
	    fmulp
	    fstcww	.LCW1
	    fstcww	.LCW2
	    fwait
	    andw	$0xf3ff,.LCW2
	    orw	$0x0400,.LCW2
	    fldcww	.LCW2
	    fldl	%st(0)
	    frndint
	    fldcww	.LCW1
	    fxch	%st(1)
	    fsub	%st(1),%st
	    f2xm1
	    faddl	.LC0
	    fscale
	    fstp	%st(1)
            leave
	    ret $8

            // store some help data in the data segment
	    .data
    .LCW1:
	    .word	0
    .LCW2:
	    .word	0
    .LC0:
	    .double	0d1.0e+00

            // do not forget to switch back to text
            .text
         end;
      end;

    function frac(d : real) : real;

      begin
         asm
            subl $16,%esp
            fnstcw -4(%ebp)
            fwait
            movw -4(%ebp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%ebp)
            fldcw -8(%ebp)
            fwait
            fldl 8(%ebp)
            frndint
            fldl 8(%ebp)
            fsub %st(1)
            fstp %st(1)
            fclex
            fldcw -4(%ebp)
            leave
            ret $8
         end ['ECX']; 
      end;
    
    function int(d : real) : real;
    
      begin
         asm
            subl $16,%esp
            fnstcw -4(%ebp)
            fwait
            movw -4(%ebp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%ebp)
            fldcw -8(%ebp)
            fwait
            fldl 8(%ebp)
            frndint
            fclex
            fldcw -4(%ebp)
            leave
            ret $8
         end ['ECX']; 
      end;
      
    function trunc(d : real) : longint;
    
      begin
         asm
            subl $16,%esp
            fnstcw -4(%ebp)
            fwait
            movw -4(%ebp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%ebp)
            fldcw -8(%ebp)
            fwait
            fldl 8(%ebp)
            fistpl -8(%ebp)
            movl -8(%ebp),%eax
            fldcw -4(%ebp)
            leave
            ret $8
         end ['EAX','ECX']; 
      end;

    function round(d : real) : longint;
    
      begin
         asm
            subl $8,%esp
            fnstcw -4(%ebp)
            fwait
            movw $0x1372,-8(%ebp)
            fldcw -8(%ebp)
            fwait
            fldl 8(%ebp)
            fistpl -8(%ebp)
            movl -8(%ebp),%eax
            fldcw -4(%ebp)
            leave
            ret $8
         end ['EAX','ECX']; 
      end;
      
    function ln(d : real) : real;
    
      begin
         asm
            fldln2
            fldl 8(%ebp)
            fyl2x
            leave
            ret $8
         end [];
      end;
      
    function pi : real;
    
      begin
         asm
            fldpi
            leave
            ret
         end [];
      end;
      
    function sin(d : real) : real;
    
      begin
         asm
            fldl 8(%ebp)
            fsin
            fstsw
            sahf
            jnp .LSIN1
            fstp %st(0)
            fldl .LSIN0
         .LSIN1:
            leave
            ret	$8
         .LSIN0:
            .quad	0xffffffffffffffff
         end ['EAX'];
      end;

   function power(bas,expo : real) : real;
     begin
        power:=exp(ln(bas)*expo);
     end;

   function power(bas,expo : longint) : longint;
     begin
        power:=round(exp(ln(bas)*expo));
     end;

{$ifdef fixed}

    function sqrt(d : fixed) : fixed;
    
      begin
	 asm
	    movl d,%eax
	    movl %eax,%ebx
	    movl %eax,%ecx
	    jecxz .L_kl
	    xorl %esi,%esi
	 .L_it:
	    xorl %edx,%edx
	    idivl %ebx
	    addl %ebx,%eax
	    shrl $1,%eax
	    subl %eax,%esi
	    cmpl $1,%esi
	    jbe .L_kl
	    movl %eax,%esi
	    movl %eax,%ebx
	    movl %ecx,%eax
	    jmp .L_it
	 .L_kl:
	    shl $8,%eax
	    leave
	    ret $4
 	 end;
      end;

    function int(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the integral part of d                                  }
    {*****************************************************************}
    begin
      int:=d and $ffff0000;       { keep only upper bits      }
    end;


    function trunc(d : fixed) : longint;
    {*****************************************************************}
    { Returns the Truncated integral part of d                        }
    {*****************************************************************}
    begin
      trunc:=longint(integer(d shr 16));   { keep only upper 16 bits  }
    end;

    function frac(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Fractional part of d                                }
    {*****************************************************************}
    begin
      frac:=d AND $ffff;         { keep only decimal parts - lower 16 bits }
    end;

    function abs(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Absolute value of d                                 }
    {*****************************************************************}
    begin
       asm
           movl d,%eax
           rol $16,%eax		    { Swap high & low word.}
	   {Absolute value: Invert all bits and increment when <0 .}
	   cwd                      { When ax<0, dx contains $ffff}
	   xorw %dx,%ax		    { Inverts all bits when dx=$ffff.}
           subw %dx,%ax             { Increments when dx=$ffff.}
	   rol $16,%eax             { Swap high & low word.}
           leave
	   ret $4
       end;
    end;


    function sqr(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Absolute squared value of d                         }
    {*****************************************************************}
    begin
            {16-bit precision needed, not 32 =)}
       sqr := d*d;
{       sqr := (d SHR 8 * d) SHR 8; }
    end;


    function Round(x: fixed): longint;
    {*****************************************************************}
    { Returns the Rounded value of d as a longint                     }
    {*****************************************************************}
    var
     lowf:integer;
     highf:integer;
    begin
      lowf:=x and $ffff;       { keep decimal part ... }
      highf :=integer(x shr 16);
      if lowf > 5 then
        highf:=highf+1
      else
      if lowf = 5 then
      begin
        { here we must check the sign ...       }
        { if greater or equal to zero, then     }
        { greater value will be found by adding }
        { one...                                }
         if highf >= 0 then
           Highf:=Highf+1;
      end;
      Round:= longint(highf);
    end;

{$endif}

{
  $Log$
  Revision 1.1  1998-03-25 11:18:42  root
  Initial revision

  Revision 1.9  1998/02/04 14:40:31  daniel
  * Translated abs for fixed to assembler.

  Revision 1.8  1998/01/27 12:44:48  peter
    * removed comment level 2 warning

  Revision 1.7  1998/01/26 11:59:04  michael
  + Added log at the end


  
  Working file: rtl/i386/math.inc
  description:
  ----------------------------
  revision 1.6
  date: 1998/01/20 15:12:27;  author: peter;  state: Exp;  lines: +4 -3
    * fixes bug 65
  ----------------------------
  revision 1.5
  date: 1997/12/01 12:34:37;  author: michael;  state: Exp;  lines: +11 -4
  + added copyright reference in header.
  ----------------------------
  revision 1.4
  date: 1997/11/28 23:26:44;  author: florian;  state: Exp;  lines: +34 -33
  $ifdef fixed added
  ----------------------------
  revision 1.3
  date: 1997/11/28 19:46:11;  author: pierre;  state: Exp;  lines: +360 -358
    + fixed math in define (does not compile yet)
  ----------------------------
  revision 1.2
  date: 1997/11/28 16:50:04;  author: carl;  state: Exp;  lines: +358 -278
  + added fixes point routines.
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
