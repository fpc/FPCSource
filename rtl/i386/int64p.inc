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

    function div_qword_throwdivbyzero(n,z : qword) : qword;
      begin
        HandleErrorFrame(200,get_frame);
      end;

{$define FPC_SYSTEM_HAS_DIV_INT64}
    function fpc_div_int64(n,z : int64) : int64;assembler;nostackframe;[public,alias: 'FPC_DIV_INT64']; compilerproc;
      { n = [esp + 12], z = [esp + 4]. }
      asm
            push %ebx
            push %esi
            push %edi
            { the following piece of code is taken from the     }
            { AMD Athlon Processor x86 Code Optimization manual }
            movl 12+16(%esp),%ecx { ecx = hi(n) }
            movl 12+12(%esp),%ebx { ebx = lo(n) }
            movl 12+8(%esp),%edx { edx = hi(z) }
            movl 12+4(%esp),%eax { eax = lo(z) }
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
.Lmake_sign_zero_hi:
            xorl %edx,%edx
.Lmake_sign:
            xorl %esi,%eax
            xorl %esi,%edx
            subl %esi,%eax
            sbbl %esi,%edx
            pop %edi
            pop %esi
            pop %ebx
            ret $16

.Ltwo_divs:
            test %ebx,%ebx { Zero division ends up here with ebx = 0. }
            jz .Ldivzero
            movl %eax,%ecx
            movl %edx,%eax
            xorl %edx,%edx
            divl %ebx
            xchgl %ecx,%eax
            divl %ebx
            movl %ecx,%edx
            jmp .Lmake_sign

.Lbigdivisor:
            movl %eax,12+4(%esp) { Reuse n~z stack space. }
            movl %ebx,12+8(%esp)
            movl %edx,12+12(%esp)
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
            movl 12+4(%esp),%ebx
            movl %eax,%ecx
            imull %eax,%edi
            mull 12+8(%esp)
            addl %edi,%edx
            subl %eax,%ebx
            movl %ecx,%eax
            movl 12+12(%esp),%ecx
            sbbl %edx,%ecx
            sbbl $0,%eax
            jmp .Lmake_sign_zero_hi

.Ldivzero:
            pop %edi
            pop %esi
            pop %ebx
            jmp div_qword_throwdivbyzero
      end;

{$define FPC_SYSTEM_HAS_MOD_INT64}
    function fpc_mod_int64(n,z : int64) : int64;assembler;nostackframe;[public,alias: 'FPC_MOD_INT64']; compilerproc;
      { n = [esp + 12], z = [esp + 4]. }
      asm
            push %ebx
            push %esi
            push %edi
            { the following piece of code is taken from the     }
            { AMD Athlon Processor x86 Code Optimization manual }
            movl 12+16(%esp),%ecx
            movl 12+12(%esp),%ebx
            movl 12+8(%esp),%edx
            movl 12+4(%esp),%eax
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
.Lmake_sign:
            xorl %esi,%eax
            xorl %esi,%edx
            subl %esi,%eax
            sbbl %esi,%edx
            pop %edi
            pop %esi
            pop %ebx
            ret $16

.Ltwo_divs:
            test %ebx,%ebx { Zero division ends up here with ebx = 0. }
            jz .Ldivzero
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
            movl %eax,12+4(%esp)  { Reuse n~z stack space. }
            movl %ebx,12+8(%esp)
            movl %edx,12+12(%esp)
            movl %ecx,12+16(%esp)
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
            movl 12+4(%esp),%ebx
            movl %eax,%ecx
            imull %eax,%edi
            mull 12+8(%esp)
            addl %edi,%edx
            subl %eax,%ebx
            movl 12+12(%esp),%ecx
            sbbl %edx,%ecx
            sbbl %eax,%eax
            movl 12+16(%esp),%edx
            andl %eax,%edx
            andl 12+8(%esp),%eax
            addl %ebx,%eax
            adcl %ecx,%edx
            jmp .Lmake_sign

.Ldivzero:
            pop %edi
            pop %esi
            pop %ebx
            jmp div_qword_throwdivbyzero
      end;

{$define FPC_SYSTEM_HAS_DIV_QWORD}
    function fpc_div_qword(n,z : qword) : qword;assembler;nostackframe;[public,alias: 'FPC_DIV_QWORD']; compilerproc;
      { n = [esp + 12], z = [esp + 4]. }
      asm
            { the following piece of code is taken from the     }
            { AMD Athlon Processor x86 Code Optimization manual }
            movl 16(%esp),%ecx { ecx = hi(n) }
            test %ecx,%ecx
            jnz .Lqworddivbigdivisor

            movl 12(%esp),%ecx { ecx = lo(n) }
            movl 8(%esp),%edx { edx = hi(z) }
            cmpl %ecx,%edx
            jae .Lqworddivtwo_divs

            movl 4(%esp),%eax { eax = lo(z) }
            divl %ecx
            xorl %edx,%edx
            ret $16

         .Lqworddivtwo_divs:
            test %ecx,%ecx { Zero division ends up here with ecx = 0. }
            jz div_qword_throwdivbyzero
            movl %edx,%eax
            xorl %edx,%edx
            divl %ecx
            push %eax { eax = future hi(result); remember }
            movl 4+4(%esp),%eax { eax = lo(z) }
            divl %ecx
            pop %edx
            ret $16

         .Lqworddivbigdivisor:
            push %ebx
            push %esi
            push %edi
            movl 12+12(%esp),%ebx { ebx = lo(n) }
            movl 12+8(%esp),%edx { edx = hi(z) }
            movl 12+4(%esp),%eax { eax = lo(z) }
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
            movl 12+4(%esp),%ebx
            movl %eax,%esi             // save quotient to esi
            imull %eax,%edi
            mull 12+12(%esp)
            addl %edi,%edx
            setcb %cl                  // cl:edx:eax = 65 bits quotient*divisor

            movl 12+8(%esp),%edi       // edi:ebx = dividend
            subl %eax,%ebx
            movb $0,%al
            sbbl %edx,%edi
            sbbb %cl,%al
            sbbl $0,%esi
            xorl %edx,%edx
            movl %esi,%eax
            pop %edi
            pop %esi
            pop %ebx
      end;


{$define FPC_SYSTEM_HAS_MOD_QWORD}
    function fpc_mod_qword(n,z : qword) : qword;assembler;nostackframe;[public,alias: 'FPC_MOD_QWORD']; compilerproc;
      { n = [esp + 12], z = [esp + 4]. }
      asm
            { the following piece of code is taken from the     }
            { AMD Athlon Processor x86 Code Optimization manual }
            movl 16(%esp),%ecx { ecx = hi(n) }
            movl 8(%esp),%edx { edx = hi(z) }
            test %ecx,%ecx
            jnz .Lqwordmodr_big_divisior

            movl 12(%esp),%ecx { ecx = lo(n) }
            movl 4(%esp),%eax { eax = lo(z) }
            cmpl %ecx,%edx
            jae .Lqwordmodr_two_divs

            divl %ecx
            movl %edx,%eax
            xorl %edx,%edx
            ret $16

         .Lqwordmodr_two_divs:
            test %ecx,%ecx { Zero division ends up here with ecx = 0. }
            jz div_qword_throwdivbyzero
            movl %edx,%eax
            xorl %edx,%edx
            divl %ecx
            movl 4(%esp),%eax { eax = lo(z) }
            divl %ecx
            movl %edx,%eax
            xorl %edx,%edx
            ret $16

         .Lqwordmodr_big_divisior:
            push %ebx
            push %edi
            movl 8+12(%esp),%ebx { ebx = lo(n) }
            movl 8+4(%esp),%eax { eax = lo(z) }
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
            movl 8+4(%esp),%ebx { lo(z) }
            imull %eax,%edi
            mull 8+12(%esp) { lo(n) }
            addl %edi,%edx
            setcb %cl                  // cl:edx:eax = 65 bits quotient*divisor
            movl 8+8(%esp),%edi { hi(z) }
            subl %eax,%ebx             // subtract (quotient*divisor) from dividend
            movb $0,%al
            sbbl %edx,%edi
            sbbb %cl,%al               // if carry is set now, the quotient was off by 1,
                                       // and we need to add divisor to result
            movl 8+12(%esp),%eax { lo(n) }
            sbbl %edx,%edx
            andl %edx,%eax
            andl 8+16(%esp),%edx { hi(n) }
            addl %ebx,%eax
            adcl %edi,%edx
            pop %edi
            pop %ebx
      end;

{$ifndef VER3_0}
{$define FPC_SYSTEM_HAS_MUL_QWORD}
    function fpc_mul_qword(f1,f2 : qword) : qword;[public,alias: 'FPC_MUL_QWORD']; compilerproc;
      begin
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
           imul f1+4,%edx
           imul %eax,%ecx
           addl %edx,%ecx
           mull f2
           add %ecx,%edx
        .Lqwordmulready:
           movl %eax,__RESULT
           movl %edx,__RESULT+4
        .Lend:
        end [ 'eax','edx','ecx'];
      end;


    function mul_qword_throwoverflow(f1,f2 : qword) : qword;
      begin
        HandleErrorFrame(215,get_frame);
      end;


    function fpc_mul_qword_checkoverflow(f1,f2 : qword) : qword;assembler;nostackframe;[public,alias: 'FPC_MUL_QWORD_CHECKOVERFLOW']; compilerproc;
      { f1 = [esp + 12], f2 = [esp + 4]. }
      asm
        { the following piece of code is taken from the
          AMD Athlon Processor x86 Code Optimization manual }
        movl 16(%esp),%edx { edx = hi(f1) }
        movl 8(%esp),%ecx { ecx = hi(f2) }
        orl %ecx,%edx
        movl 4(%esp),%edx { edx = lo(f2) }
        movl 12(%esp),%eax { eax = lo(f1) }
        jnz .Loverflowchecked
        { if both upper dwords are =0 then it cannot overflow }
        mull %edx
        ret $16

.Loverflowed:
        jmp mul_qword_throwoverflow

.Loverflowchecked:
        { if both upper dwords are <>0 then it overflows always }
        test %ecx,%ecx
        jz .Loverok1
        cmpl $0,16(%esp)
        jnz .Loverflowed
.Loverok1:
        { overflow checked code }
        movl 16(%esp),%eax { eax = hi(f1) }
        mull 4(%esp)
        movl %eax,%ecx
        jc  .Loverflowed

        movl 12(%esp),%eax { eax = lo(f1) }
        mull 8(%esp)
        jc  .Loverflowed

        addl %eax,%ecx
        jc  .Loverflowed

        movl 4(%esp),%eax
        mull 12(%esp)
        addl %ecx,%edx
        jc  .Loverflowed
      end;
{$endif VER3_0}