{******************************************************************************
	$Id$
 *****************************************************************************}
          'NONE',
          'ABCD',
          'ADD','ADDcc','ADDX','ADDXcc',
          'AND','ANDcc','ANDN','ADDNcc',
          'JMPL',
          'CALL',
          'BN','BNE','BE','BG','BLE','BGE','BI','BGU','BLEU','BCC',
          'BCS','BPOS','NEG','BVC','BVS','BA',
          'FBN','FBU','FBG','FBUG','FBL','FBUL','FBLG','FBNE',
          'FBE','FBUE','FBGE','FBUGE','FBLE','FBULE','FBO','FBA',
          'CBccc',
          'FLUSH',
          'LDSB','LDSH','LDSTUB',
          'LDUB','LDUH','LD','LDD','LDF','LDFSR','LDDF','LDC','LDCSR','LDDC',
          'LDSBA','LDSHA','LDUBA','LDUHA','LDA','LDDA',
          'LDSTUBA',
          'MULScc',
          'NOP',
          'OR','ORcc','ORN','ORNcc',
          'RDASR','RDY','RDPSR','RDWIM','RDTBR',
          'RESTORE',
          'RETT',
          'SAVE',
          'SDIV','SDIVcc',
          'SMUL','SMULcc',
          'SETHI',
          'SLL','SRL','SRA',
          'STB','STH','ST','STD','STF','STDF','STFSR','STDFQ',
          'STC','STDC','STCSR','STDCQ',
          'STBA','STHA','STA','STDA',
          'SUB','SUBcc','SUBX','SUBXcc',
          'SWAP','SWAPA','Ticc','TADDcc','TSUBcc','TADDccTV','TSUBccTV',
          'UDIV','UDIVcc',
          'UMUL','UMULcc',
          'UNMP',
          'WRASR','WRY','WRPSR','WRWIM','WRTBR',
          'XNOR','XNORcc',
          'XOR','XORcc',
          {Floating-point instructions}
          'FiTOs','FiTOd','FiTOq',
          'FsTOi','FdTOi','FqTOi',
          'FsTOd','FsTOq',
          'FdTOs','FdTOq',
          'FqTOd','FqTOs',
          'MOVs','NEGs','FABSs',
          'FSQRTs','FSQRTd','FSQRTq',
          'FADDs','FADDd','FADDq',
          'FSUBs','FSUBd','FSUBq',
          'FMULs','FMULd','FMULQ',
          'FdMULq','FsMULd',
          'DIVs','DIVd','FDIVq',
          'FCMPs','FCMPd','FCMPq',
          'FCPop1','CPop2',
          {Synthetic instructions}
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
	Revision 1.3  2002-10-17 14:48:34  mazen
	* branch instructions are now contigous

	Revision 1.2  2002/10/01 21:07:48  mazen
	attinst.inc --> strinst.inc
	
}
