{******************************************************************************
        $Id$
 *****************************************************************************}
          'none',
          'abcd',
          'add','addcc','addx','addxcc',
          'and','andcc','andn','addncc',
          'jmpl',
          'call',
          'ba','b',
          'fbn','fbu','fbg','fbug','fbl','fbul','fblg','fbne',
          'fbe','fbue','fbge','fbuge','fble','fbule','fbo','fba',
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
          'ret',
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
          'tst'
{
        $Log$
        Revision 1.8  2003-07-02 22:18:04  peter
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
