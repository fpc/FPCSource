{******************************************************************************
        $Id$
 *****************************************************************************}
A_NONE,
A_ABCD,
A_ADD,A_ADDcc,A_ADDX,A_ADDXcc,
A_AND,A_ANDcc,A_ANDN,A_ADDNcc,
{Branching instructions}
A_JMPL,
A_CALL,
A_BN,A_BNE,A_BE,A_BG,A_BLE,A_BGE,A_BL,A_BGU,A_BLEU,A_BCC,
A_BCS,A_BPOS,A_NEG,A_BVC,A_BVS,A_BA,
A_FBN,A_FBU,A_FBG,A_FBUG,A_FBL,A_FBUL,A_FBLG,A_FBNE,
A_FBE,A_FBUE,A_FBGE,A_FBUGE,A_FBLE,A_FBULE,A_FBO,A_FBA,
A_CBccc,
A_FLUSH,
{Load instructions}
A_LDSB,A_LDSH,A_LDSTUB,
A_LDUB,A_LDUH,A_LD,A_LDD,A_LDF,A_LDFSR,A_LDDF,A_LDC,A_LDCSR,A_LDDC,
A_LDSBA,A_LDSHA,A_LDUBA,A_LDUHA,A_LDA,A_LDDA,
A_LDSTUBA,
A_MULScc,
A_NOP,
A_OR,A_ORcc,A_ORN,A_ORNcc,
A_RDASR,A_RDY,A_RDPSR,A_RDWIM,A_RDTBR,
A_RESTORE,
A_RET,
A_SAVE,
A_SDIV,A_SDIVcc,
A_SMUL,A_SMULcc,
A_SETHI,
A_SLL,A_SRL,A_SRA,
A_STB,A_STH,A_ST,A_STD,A_STF,A_STDF,A_STFSR,A_STDFQ,
A_STC,A_STDC,A_STCSR,A_STDCQ,
A_STBA,A_STHA,A_STA,A_STDA,
A_SUB,A_SUBcc,A_SUBX,A_SUBXcc,
A_SWAP,A_SWAPA,A_Ticc,A_TADDcc,A_TSUBcc,A_TADDccTV,A_TSUBccTV,
A_UDIV,A_UDIVcc,
A_UMUL,A_UMULcc,
A_UNMP,
A_WRASR,A_WRY,A_WRPSR,A_WRWIM,A_WRTBR,
A_XNOR,A_XNORcc,
A_XOR,A_XORcc,
          {Floating-point instructions}
A_FiTOs,A_FiTOd,A_FiTOq,
A_FsTOi,A_FdTOi,A_FqTOi,
A_FsTOd,A_FsTOq,
A_FdTOs,A_FdTOq,
A_FqTOd,A_FqTOs,
A_FMOVs,A_FNEGs,A_FABSs,
A_FSQRTs,A_FSQRTd,A_FSQRTq,
A_FADDs,A_FADDd,A_FADDq,
A_FSUBs,A_FSUBd,A_FSUBq,
A_FMULs,A_FMULd,A_FMULQ,
A_FdMULq,A_FsMULd,
A_FDIVs,A_FDIVd,A_FDIVq,
A_FCMPs,A_FCMPd,A_FCMPq,
A_FCPop1,A_CPop2,
          {Synthetic instructions}
A_btst,A_bset,A_bclr,A_btog,
A_clr,A_clrb,A_clrh,
A_cmp,
A_dec,A_deccc,
A_inc,A_inccc,
A_MOV,
A_not,
A_set,
A_skipz,A_skipnz,
A_tst
{
        $Log$
        Revision 1.8  2003-06-01 21:38:07  peter
          * getregisterfpu size parameter added
          * op_const_reg size parameter added
          * sparc updates

        Revision 1.7  2003/06/01 01:03:53  peter
          * FMOVs fixed

        Revision 1.6  2003/05/06 20:23:44  mazen
        * A_BI ==> A_BL (bi ==> bl) instructions renamed

        Revision 1.5  2003/01/20 20:15:44  mazen
        * A_CMPrs --> A_FCMPs and A_CMPd --> A_FCMPd

        Revision 1.4  2002/11/10 17:49:14  mazen
        * A_RET --> A_RETT (RET from Trap)

        Revision 1.3  2002/10/17 14:48:34  mazen
        * branch instructions are now contigous

}
