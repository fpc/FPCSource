{******************************************************************************
        $Id$
 *****************************************************************************}
          'none',
          'abcd',
          'add','addcc','addx','addxcc',
          'and','andcc','andn','andncc',
          'jmp',
          'jmpl',
          'call',
          'ba','b','fba','fb',
          'cbccc',
          'flush',
          'ldsb','ldsh','ldstub',
          'ldub','lduh','ld','ldd','ld','ldfsr','ldd','ldc','ldcsr','lddc',
          'ldsba','ldsha','lduba','lduha','lda','ldda',
          'ldstuba',
          'mulscc',
          'nop',
          'or','orcc','orn','orncc',
          'rd','rd','rd','rd','rd',
          'restore',
          'ret','retl',
          'save',
          'sdiv','sdivcc',
          'smul','smulcc',
          'sethi',
          'sll','srl','sra',
          'stb','sth','st','std','st','std','stfsr','stdfq',
          'stc','stdc','stcsr','stdcq',
          'stba','stha','sta','stda',
          'sub','subcc','subx','subxcc',
          'swap','swapa','ticc','taddcc','tsubcc','taddcctv','tsubcctv',
          'ta','t',
          'udiv','udivcc',
          'umul','umulcc',
          'unimp',
          'wrasr','wry','wrpsr','wrwim','wrtbr',
          'xnor','xnorcc',
          'xor','xorcc',
          {floating-point instructions}
          'fitos','fitod','fitoq',
          'fstoi','fdtoi','fqtoi',
          'fstod','fstoq',
          'fdtos','fdtoq',
          'fqtod','fqtos',
          'fmovs','fnegs','fabss',
          'fsqrts','fsqrtd','fsqrtq',
          'fadds','faddd','faddq',
          'fsubs','fsubd','fsubq',
          'fmuls','fmuld','fmulq',
          'fdmulq','fsmuld',
          'fdivs','fdivd','fdivq',
          'fcmps','fcmpd','fcmpq',
          'fcpop1','cpop2',
          {synthetic instructions}
          'btst','bset','bclr','btog',
          'clr','clrb','clrh',
          'cmp',
          'dec','deccc',
          'inc','inccc',
          'mov',
          'neg',
          'not',
          'set',
          'skipz','skipnz',
          'tst',
          { internal instructions }
          'fmovd',
          'fabsd','fabsq'
{
        $Log$
        Revision 1.15  2005-01-07 16:22:54  florian
          + implemented abi compliant handling of strucutured functions results on sparc platform

        Revision 1.14  2004/10/03 12:42:22  florian
          * made sqrt, sqr and abs internal for the sparc

        Revision 1.13  2004/06/20 08:55:32  florian
          * logs truncated

        Revision 1.12  2004/06/16 20:07:11  florian
          * dwarf branch merged

        Revision 1.11.2.3  2004/05/27 23:34:59  peter
          * retl added

        Revision 1.11.2.2  2004/05/25 21:38:53  peter
          * assembler reader/writer updates

        Revision 1.11.2.1  2004/05/13 20:58:47  florian
          * fixed register addressed jumps in interface wrappers

        Revision 1.11  2004/01/12 22:11:39  peter
          * use localalign info for alignment for locals and temps
          * sparc fpu flags branching added
          * moved powerpc copy_valye_openarray to generic

}
