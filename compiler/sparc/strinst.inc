{******************************************************************************
	$Id$
 *****************************************************************************}
          'none',
          'abcd',
          'add','addcc','addx','addxcc',
          'and','andcc','andn','addncc',
          'jmpl',
          'call',
          'bn','bne','be','bg','ble','bge','bl','bgu','bleu','bcc',
          'bcs','bpos','neg','bvc','bvs','ba',
          'fbn','fbu','fbg','fbug','fbl','fbul','fblg','fbne',
          'fbe','fbue','fbge','fbuge','fble','fbule','fbo','fba',
          'cbccc',
          'flush',
          'ldsb','ldsh','ldstub',
          'ldub','lduh','ld','ldd','ldf','ldfsr','lddf','ldc','ldcsr','lddc',
          'ldsba','ldsha','lduba','lduha','lda','ldda',
          'ldstuba',
          'mulscc',
          'nop',
          'or','orcc','orn','orncc',
          'rdasr','rdy','rdpsr','rdwim','rdtbr',
          'restore',
          'rett',
          'save',
          'sdiv','sdivcc',
          'smul','smulcc',
          'sethi',
          'sll','srl','sra',
          'stb','sth','st','std','stf','stdf','stfsr','stdfq',
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
          'movs','negs','fabss',
          'fsqrts','fsqrtd','fsqrtq',
          'fadds','faddd','faddq',
          'fsubs','fsubd','fsubq',
          'fmuls','fmuld','fmulq',
          'fdmulq','fsmuld',
          'divs','divd','fdivq',
          'fcmps','fcmpd','fcmpq',
          'fcpop1','cpop2',
          {synthetic instructions}
          'btst','bset','bclr','btog',
          'clr','clrb','clrh',
          'cmp',
          'dec','deccc',
          'inc','inccc',
          'not',
          'set',
          'skipz','skipnz',
          'tst'
{
	$Log$
	Revision 1.5  2003-05-06 20:23:44  mazen
	* A_BI ==> A_BL (bi ==> bl) instructions renamed

	Revision 1.4  2003/04/29 10:00:31  mazen
	* instruction are moved lowercase to allow using old assmeblers
	
	Revision 1.3  2002/10/17 14:48:34  mazen
	* branch instructions are now contigous
	
	Revision 1.2  2002/10/01 21:07:48  mazen
	attinst.inc --> strinst.inc
}
