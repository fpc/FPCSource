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
          'unmp',
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
          'fmovd'
{
        $Log$
        Revision 1.12  2004-06-16 20:07:11  florian
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

        Revision 1.10  2004/01/12 16:39:41  peter
          * sparc updates, mostly float related

        Revision 1.9  2003/12/08 13:02:21  mazen
        + support for native sparc assembler reader

        Revision 1.8  2003/07/02 22:18:04  peter
          * paraloc splitted in callerparaloc,calleeparaloc
          * sparc calling convention updates

        Revision 1.7  2003/06/01 21:38:07  peter
          * getregisterfpu size parameter added
          * op_const_reg size parameter added
          * sparc updates

        Revision 1.6  2003/05/26 21:31:27  mazen
        * mnemonic of A_LDF is ld, ldf does not exist!

        Revision 1.5  2003/05/06 20:23:44  mazen
        * A_BI ==> A_BL (bi ==> bl) instructions renamed

        Revision 1.4  2003/04/29 10:00:31  mazen
        * instruction are moved lowercase to allow using old assmeblers

        Revision 1.3  2002/10/17 14:48:34  mazen
        * branch instructions are now contigous

        Revision 1.2  2002/10/01 21:07:48  mazen
        attinst.inc --> strinst.inc
}
