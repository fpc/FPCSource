{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file contains some helper routines for int64 and qword

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$Q- no overflow checking }
{$R- no range checking }

{$define FPC_SYSTEM_HAS_DIV_QWORD}
    function fpc_div_qword(n,z : qword) : qword;assembler;[public,alias: 'FPC_DIV_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
      var
         shift,lzz,lzn : longint;
	 saveebx,saveedi : longint;
      asm
            movl %ebx,saveebx
            movl %edi,saveedi
            { the following piece of code is taken from the     }
            { AMD Athlon Processor x86 Code Optimization manual }
            movl n+4,%ecx
            movl n,%ebx
	    movl %ecx,%eax
	    orl %ebx,%eax
	    jnz .Lnodivzero
	    pushl %ebp
	    pushl $200
	    call HandleErrorFrame
	    jmp .Lexit
.Lnodivzero:
            movl z+4,%edx
            movl z,%eax
            testl %ecx,%ecx
            jnz .Lqworddivbigdivisor
            cmpl %ebx,%edx
            jae .Lqworddivtwo_divs
            divl %ebx
            movl %ecx,%edx
	    jmp .Lexit

         .Lqworddivtwo_divs:
            movl %eax,%ecx
            movl %edx,%eax
            xorl %edx,%edx
            divl %ebx
            xchgl %ecx,%eax
            divl %ebx
            movl %ecx,%edx
	    jmp .Lexit

         .Lqworddivbigdivisor:
            movl %ecx,%edi
            shrl $1,%edx
            rcrl $1,%eax
            rorl $1,%edi
            rcrl $1,%ebx
            bsrl %ecx,%ecx
            shrdl %cl,%edi,%ebx
            shrdl %cl,%edx,%eax
            shrl %cl,%edx
            roll $1,%edi
            divl %ebx
            movl z,%ebx
            movl %eax,%ecx
            imull %eax,%edi
            mull n
            addl %edi,%edx
            subl %eax,%ebx
            movl %ecx,%eax
            movl z+4,%ecx
            sbbl %edx,%ecx
            sbbl $0,%eax
            xorl %edx,%edx
.Lexit:
            movl saveebx,%ebx
            movl saveedi,%edi
      end;


(*
  This does not work correctly

{$define FPC_SYSTEM_HAS_MOD_QWORD}
    function fpc_mod_qword(n,z : qword) : qword;[public,alias: 'FPC_MOD_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
      var
         shift,lzz,lzn : longint;
	 saveebx,saveedi : longint;
      asm
            movl %ebx,saveebx
            movl %edi,saveedi
            { the following piece of code is taken from the     }
            { AMD Athlon Processor x86 Code Optimization manual }
            movl n+4,%ecx
            movl n,%ebx
	    movl %ecx,%eax
	    orl %ebx,%eax
	    jnz .Lnodivzero
	    pushl %ebp
	    pushl $200
	    call HandleErrorFrame
	    jmp .Lexit
            movl z+4,%edx
            movl z,%eax
            testl %ecx,%ecx
            jnz .Lqwordmodr_big_divisior
            cmpl %ebx,%edx
            jae .Lqwordmodr_two_divs
            divl %ebx
            movl %edx,%eax
            movl %ecx,%edx
	    jmp .Lexit

         .Lqwordmodr_two_divs:
            movl %eax,%ecx
            movl %edx,%eax
            xorl %edx,%edx
            divl %ebx
            movl %ecx,%eax
            divl %ebx
            movl %edx,%eax
            xorl %edx,%edx
	    jmp .Lexit

         .Lqwordmodr_big_divisior:
            movl %ecx,%edi
            shrl $1,%edx
            rcrl $1,%eax
            rorl $1,%edi
            rcrl $1,%ebx
            bsrl %ecx,%ecx
            shrdl %cl,%edi,%ebx
            shrdl %cl,%edx,%eax
            shrl %cl,%edx
            rorl $1,%edi
            divl %ebx
            movl z,%ebx
            movl %eax,%ecx
            imull %eax,%edi
            mull n
            addl %edi,%edx
            subl %eax,%ebx
            movl z+4,%ecx
            movl n,%eax
            sbbl %edx,%ecx
            sbbl %edx,%edx
            andl %edx,%eax
            andl n+4,%edx
            addl %ebx,%eax
            adcl %ecx,%edx
.Lexit:
            movl saveebx,%ebx
            movl saveedi,%edi
      end;
*)


{$define FPC_SYSTEM_HAS_MUL_QWORD}
    { multiplies two qwords
      the longbool for checkoverflow avoids a misaligned stack
    }
    function fpc_mul_qword(f1,f2 : qword;checkoverflow : longbool) : qword;[public,alias: 'FPC_MUL_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}

      var
         _f1,bitpos : qword;
         l : longint;
         r : qword;

      begin
         if not(checkoverflow) then
           begin
              { the following piece of code is taken from the     }
              { AMD Athlon Processor x86 Code Optimization manual }
              asm
                 movl f1+4,%edx
                 movl f2+4,%ecx
                 orl %ecx,%edx
                 movl f2,%edx
                 movl f1,%eax
                 jnz .Lqwordmultwomul
                 mull %edx
                 jmp .Lqwordmulready
              .Lqwordmultwomul:
                 imul f1+4,%edx
                 imul %eax,%ecx
                 addl %edx,%ecx
                 mull f2
                 add %ecx,%edx
              .Lqwordmulready:
                 movl %eax,r
                 movl %edx,r+4
              end [ 'eax','edx','ecx' ];
              fpc_mul_qword:=r;
           end
         else
           begin
              fpc_mul_qword:=0;
              bitpos:=1;

              // store f1 for overflow checking
              _f1:=f1;

              for l:=0 to 63 do
                begin
                   if (f2 and bitpos)<>0 then
                     fpc_mul_qword:=fpc_mul_qword+f1;

                   f1:=f1 shl 1;
                   bitpos:=bitpos shl 1;
                end;

              { if one of the operands is greater than the result an }
              { overflow occurs                                      }
              if checkoverflow and (_f1 <> 0) and (f2 <>0) and
                 ((_f1>fpc_mul_qword) or (f2>fpc_mul_qword)) then
                HandleErrorFrame(215,get_frame);
           end;
      end;

{
  $Log$
  Revision 1.1  2003-09-14 11:34:13  peter
    * moved int64 asm code to int64p.inc
    * save ebx,esi

}
