{
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

{$define FPC_SYSTEM_HAS_DIV_INT64}
    function fpc_div_int64(n,z : int64) : int64;assembler;[public,alias: 'FPC_DIV_INT64']; compilerproc;
      var
         saveebx,saveedi,saveesi : longint;
      asm
            movl %ebx,saveebx
            movl %esi,saveesi
            movl %edi,saveedi
            { the following piece of code is taken from the     }
            { AMD Athlon Processor x86 Code Optimization manual }
            movl n+4,%ecx
            movl n,%ebx
            movl %ecx,%eax
            orl %ebx,%eax
            jnz .Lnodivzero
            movl  %ebp,%edx
            movl  $200,%eax
            call HandleErrorFrame
            jmp .Lexit
.Lnodivzero:
            movl z+4,%edx
            movl z,%eax
            movl %ecx,%esi
            xorl %edx,%esi
            sarl $31,%esi
            movl %edx,%edi
            sarl $31,%edi
            xorl %edi,%eax
            xorl %edi,%edx
            subl %edi,%eax
            sbbl %edi,%edx
            movl %ecx,%edi
            sarl $31,%edi
            xorl %edi,%ebx
            xorl %edi,%ecx
            subl %edi,%ebx
            sbbl %edi,%ecx
            jnz .Lbigdivisor
            cmpl %ebx,%edx
            jae .Ltwo_divs
            divl %ebx
            movl %ecx,%edx
            xorl %esi,%eax
            xorl %esi,%edx
            subl %esi,%eax
            sbbl %esi,%edx
            jmp .Lexit
.Ltwo_divs:
            movl %eax,%ecx
            movl %edx,%eax
            xorl %edx,%edx
            divl %ebx
            xchgl %ecx,%eax
            divl %ebx
            movl %ecx,%edx
            jmp .Lmake_sign
.Lbigdivisor:
            subl $12,%esp
            movl %eax,(%esp)
            movl %ebx,4(%esp)
            movl %edx,8(%esp)
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
            movl (%esp),%ebx
            movl %eax,%ecx
            imull %eax,%edi
            mull 4(%esp)
            addl %edi,%edx
            subl %eax,%ebx
            movl %ecx,%eax
            movl 8(%esp),%ecx
            sbbl %edx,%ecx
            sbbl $0,%eax
            xorl %edx,%edx
            addl $12,%esp
.Lmake_sign:
            xorl %esi,%eax
            xorl %esi,%edx
            subl %esi,%eax
            sbbl %esi,%edx
.Lexit:
            movl saveebx,%ebx
            movl saveesi,%esi
            movl saveedi,%edi
      end;

{$define FPC_SYSTEM_HAS_MOD_INT64}
    function fpc_mod_int64(n,z : int64) : int64;assembler;[public,alias: 'FPC_MOD_INT64']; compilerproc;
      var
         saveebx,saveedi,saveesi : longint;
      asm
            movl %ebx,saveebx
            movl %esi,saveesi
            movl %edi,saveedi
            { the following piece of code is taken from the     }
            { AMD Athlon Processor x86 Code Optimization manual }
            movl n+4,%ecx
            movl n,%ebx
            movl %ecx,%eax
            orl %ebx,%eax
            jnz .Lnodivzero
            movl  %ebp,%edx
            movl  $200,%eax
            call HandleErrorFrame
            jmp .Lexit
.Lnodivzero:
            movl z+4,%edx
            movl z,%eax
            movl %edx,%esi
            sarl $31,%esi
            movl %edx,%edi
            sarl $31,%edi
            xorl %edi,%eax
            xorl %edi,%edx
            subl %edi,%eax
            sbbl %edi,%edx
            movl %ecx,%edi
            sarl $31,%edi
            xorl %edi,%ebx
            xorl %edi,%ecx
            subl %edi,%ebx
            sbbl %edi,%ecx
            jnz .Lbig_divisor
            cmpl %ebx,%edx
            jae .Ltwo_divs
            divl %ebx
            movl %edx,%eax
            movl %ecx,%edx
            xorl %esi,%eax
            xorl %esi,%edx
            subl %esi,%eax
            sbbl %esi,%edx
            jmp .Lexit
.Ltwo_divs:
            movl %eax,%ecx
            movl %edx,%eax
            xorl %edx,%edx
            divl %ebx
            movl %ecx,%eax
            divl %ebx
            movl %edx,%eax
            xorl %edx,%edx
            jmp .Lmake_sign
.Lbig_divisor:
            subl $16,%esp
            movl %eax,(%esp)
            movl %ebx,4(%esp)
            movl %edx,8(%esp)
            movl %ecx,12(%esp)
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
            movl (%esp),%ebx
            movl %eax,%ecx
            imull %eax,%edi
            mull 4(%esp)
            addl %edi,%edx
            subl %eax,%ebx
            movl 8(%esp),%ecx
            sbbl %edx,%ecx
            sbbl %eax,%eax
            movl 12(%esp),%edx
            andl %eax,%edx
            andl 4(%esp),%eax
            addl %ebx,%eax
            adcl %ecx,%edx
            addl $16,%esp

.Lmake_sign:
            xorl %esi,%eax
            xorl %esi,%edx
            subl %esi,%eax
            sbbl %esi,%edx

.Lexit:
            movl saveebx,%ebx
            movl saveesi,%esi
            movl saveedi,%edi
      end;

{$define FPC_SYSTEM_HAS_DIV_QWORD}
    function fpc_div_qword(n,z : qword) : qword;assembler;[public,alias: 'FPC_DIV_QWORD']; compilerproc;
      var
         saveebx,saveedi,saveesi : longint;
      asm
            movl %ebx,saveebx
            movl %esi,saveesi
            movl %edi,saveedi
            { the following piece of code is taken from the     }
            { AMD Athlon Processor x86 Code Optimization manual }
            movl n+4,%ecx
            movl n,%ebx
            movl %ecx,%eax
            orl %ebx,%eax
            jnz .Lnodivzero
            movl  %ebp,%edx
            movl  $200,%eax
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
            movl %eax,%esi             // save quotient to esi
            imull %eax,%edi
            mull n
            addl %edi,%edx
            setcb %cl                  // cl:edx:eax = 65 bits quotient*divisor

            movl z+4,%edi              // edi:ebx = dividend
            subl %eax,%ebx
            movb $0,%al
            sbbl %edx,%edi
            sbbb %cl,%al
            sbbl $0,%esi
            xorl %edx,%edx
            movl %esi,%eax
.Lexit:
            movl saveebx,%ebx
            movl saveesi,%esi
            movl saveedi,%edi
      end;


{$define FPC_SYSTEM_HAS_MOD_QWORD}
    function fpc_mod_qword(n,z : qword) : qword;assembler;[public,alias: 'FPC_MOD_QWORD']; compilerproc;
      var
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
            movl  %ebp,%edx
            movl  $200,%eax
            call HandleErrorFrame
            jmp .Lexit
.Lnodivzero:
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
            roll $1,%edi
            divl %ebx
            movl z,%ebx
            imull %eax,%edi
            mull n
            addl %edi,%edx
            setcb %cl                  // cl:edx:eax = 65 bits quotient*divisor
            movl z+4,%edi
            subl %eax,%ebx             // subtract (quotient*divisor) from dividend
            movb $0,%al
            sbbl %edx,%edi
            sbbb %cl,%al               // if carry is set now, the quotient was off by 1,
                                       // and we need to add divisor to result
            movl n,%eax
            sbbl %edx,%edx
            andl %edx,%eax
            andl n+4,%edx
            addl %ebx,%eax
            adcl %edi,%edx
.Lexit:
            movl saveebx,%ebx
            movl saveedi,%edi
      end;

{$define FPC_SYSTEM_HAS_MUL_QWORD}
    { multiplies two qwords
      the longbool for checkoverflow avoids a misaligned stack
    }
    function fpc_mul_qword(f1,f2 : qword;checkoverflow : longbool) : qword;[public,alias: 'FPC_MUL_QWORD']; compilerproc;
      var
        overflowed : boolean;
      begin
        overflowed:=false;
        { the following piece of code is taken from the
          AMD Athlon Processor x86 Code Optimization manual }
        asm
           movl f1+4,%edx
           movl f2+4,%ecx
           orl %ecx,%edx
           movl f2,%edx
           movl f1,%eax
           jnz .Lqwordmultwomul
           { if both upper dwords are =0 then it cannot overflow }
           mull %edx
           jmp .Lqwordmulready
        .Lqwordmultwomul:
           cmpl $0,checkoverflow
           jnz  .Loverflowchecked
           imul f1+4,%edx
           imul %eax,%ecx
           addl %edx,%ecx
           mull f2
           add %ecx,%edx
        .Lqwordmulready:
           movl %eax,__RESULT
           movl %edx,__RESULT+4
           jmp .Lend

        .Loverflowchecked:
           { if both upper dwords are <>0 then it overflows always }
           or %ecx,%ecx
           jz .Loverok1
           cmpl $0,f1+4
           jnz .Loverflowed
        .Loverok1:
           { overflow checked code }
           movl f1+4,%eax
           mull f2
           movl %eax,%ecx
           jc  .Loverflowed

           movl f1,%eax
           mull f2+4
           jc  .Loverflowed

           addl %eax,%ecx
           jc  .Loverflowed

           movl f2,%eax
           mull f1
           addl %ecx,%edx
           movl %eax,__RESULT
           movl %edx,__RESULT+4
           jnc  .Lend

        .Loverflowed:
           movb $1,overflowed

        .Lend:
        end [ 'eax','edx','ecx'];

        if overflowed then
          HandleErrorFrame(215,get_frame);
      end;

