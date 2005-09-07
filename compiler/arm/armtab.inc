{ don't edit, this file is generated from armins.dat }
(
  (
    opcode  : A_ADC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #4#0#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_ADC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #5#0#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_ADC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate);
    code    : #6#0#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_ADC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #7#2#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #4#0#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_ADD;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #5#0#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_ADD;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate);
    code    : #6#0#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #7#2#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_AND;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #4#0#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_AND;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #5#0#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_AND;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate);
    code    : #6#0#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_AND;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #7#2#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_memory or ot_bits32,ot_none,ot_none,);
    code    : #1#10;
    flags   : if_arm7
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_immediate or ot_bits32,ot_none,ot_none,);
    code    : #1#10;
    flags   : if_arm7
  ),
  (
    opcode  : A_BIC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #4#1#192;
    flags   : if_arm7
  ),
  (
    opcode  : A_BIC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #5#1#192;
    flags   : if_arm7
  ),
  (
    opcode  : A_BIC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate);
    code    : #6#1#192;
    flags   : if_arm7
  ),
  (
    opcode  : A_BIC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #7#3#192;
    flags   : if_arm7
  ),
  (
    opcode  : A_BL;
    ops     : 1;
    optypes : (ot_memory or ot_bits32,ot_none,ot_none,);
    code    : #1#11;
    flags   : if_arm7
  ),
  (
    opcode  : A_BL;
    ops     : 1;
    optypes : (ot_immediate or ot_bits32,ot_none,ot_none,);
    code    : #1#11;
    flags   : if_arm7
  ),
  (
    opcode  : A_BX;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,);
    code    : #3#1#47#255#16;
    flags   : if_arm7
  ),
  (
    opcode  : A_CDP;
    ops     : 2;
    optypes : (ot_reg8,ot_reg8,ot_none,);
    code    : #192#1#16#65;
    flags   : if_8086
  ),
  (
    opcode  : A_CMN;
    ops     : 2;
    optypes : (ot_reg32 or ot_signed,ot_reg32,ot_none,);
    code    : #12#1#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMN;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32 or ot_signed,ot_reg32,);
    code    : #13#1#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMN;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_signed,);
    code    : #14#1#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMN;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate,ot_none,);
    code    : #15#3#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : (ot_reg32 or ot_signed,ot_reg32,ot_none,);
    code    : #12#1#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMP;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32 or ot_signed,ot_reg32,);
    code    : #13#1#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMP;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_signed,);
    code    : #14#1#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate,ot_none,);
    code    : #15#3#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_DCB;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,);
    code    : #0;
    flags   : if_ignore
  ),
  (
    opcode  : A_DCD;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,);
    code    : #0;
    flags   : if_ignore
  ),
  (
    opcode  : A_DCW;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,);
    code    : #0;
    flags   : if_ignore
  ),
  (
    opcode  : A_DQ;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,);
    code    : #0;
    flags   : if_ignore
  ),
  (
    opcode  : A_DT;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,);
    code    : #0;
    flags   : if_ignore
  ),
  (
    opcode  : A_EOR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #4#0#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_EOR;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #5#0#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_EOR;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate);
    code    : #6#0#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_EOR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #7#2#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDC;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #209#192#1#17#65;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDMDA;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#129;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDMDB;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#145;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDMEA;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#145;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDMED;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#153;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDMFA;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#129;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDMFD;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#137;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDMIA;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#137;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDMIB;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#153;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDR;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate or ot_bits32,ot_none,);
    code    : #23#5#16;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDR;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #24#4#16;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_bits32,);
    code    : #25#4#16;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #32#6#16;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDR;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate or ot_bits32);
    code    : #33#6#16;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRH;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate or ot_bits32,ot_none,);
    code    : #34#80#176;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRH;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #35#80#176;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_bits32,);
    code    : #36#80#176;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #37#16#176;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSB;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate or ot_bits32,ot_none,);
    code    : #34#80#208;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSB;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #35#80#208;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_bits32,);
    code    : #36#80#208;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #37#16#208;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSH;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate or ot_bits32,ot_none,);
    code    : #34#80#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSH;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #35#80#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_bits32,);
    code    : #36#80#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #37#16#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_MCR;
    ops     : 2;
    optypes : (ot_reg32,ot_memory or ot_bits32,ot_none,);
    code    : #208#193#1#19#72;
    flags   : if_8086
  ),
  (
    opcode  : A_MLA;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #21#0#32#144;
    flags   : if_arm7
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #8#1#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_MOV;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #9#1#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_MOV;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #10#1#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate,ot_none,);
    code    : #11#3#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_MRC;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #209#193#1#19#72;
    flags   : if_arm7
  ),
  (
    opcode  : A_MRS;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #16#1#15;
    flags   : if_arm7
  ),
  (
    opcode  : A_MSR;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #17#1#41#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_MSR;
    ops     : 2;
    optypes : (ot_regf,ot_reg32,ot_none,);
    code    : #18#1#40#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_MSR;
    ops     : 2;
    optypes : (ot_regf,ot_immediate,ot_none,);
    code    : #19#3#40#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_MUL;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #20#0#0#144;
    flags   : if_arm7
  ),
  (
    opcode  : A_MVN;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #8#1#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_MVN;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #9#1#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_MVN;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #10#1#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_MVN;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate,ot_none,);
    code    : #11#3#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_ORR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #4#1#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_ORR;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #5#1#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_ORR;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate);
    code    : #6#1#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_ORR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #7#3#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #4#0#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #5#0#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate);
    code    : #6#0#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #7#2#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #4#0#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #5#0#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate);
    code    : #6#0#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #7#2#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_SBC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #4#0#192;
    flags   : if_arm7
  ),
  (
    opcode  : A_SBC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #5#0#192;
    flags   : if_arm7
  ),
  (
    opcode  : A_SBC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate);
    code    : #6#0#192;
    flags   : if_arm7
  ),
  (
    opcode  : A_SBC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #7#2#192;
    flags   : if_arm7
  ),
  (
    opcode  : A_SMLAL;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #22#0#224#144;
    flags   : if_arm7
  ),
  (
    opcode  : A_SMULL;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #22#0#192#144;
    flags   : if_arm7
  ),
  (
    opcode  : A_STMDA;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_STMDB;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#144;
    flags   : if_arm7
  ),
  (
    opcode  : A_STMEA;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#136;
    flags   : if_arm7
  ),
  (
    opcode  : A_STMED;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_STMFA;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#152;
    flags   : if_arm7
  ),
  (
    opcode  : A_STMFD;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#144;
    flags   : if_arm7
  ),
  (
    opcode  : A_STMIA;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#136;
    flags   : if_arm7
  ),
  (
    opcode  : A_STMIB;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,);
    code    : #38#152;
    flags   : if_arm7
  ),
  (
    opcode  : A_STR;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate or ot_bits32,ot_none,);
    code    : #23#5#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_STR;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #24#4#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_STR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_bits32,);
    code    : #25#4#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_STR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #32#6#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_STR;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate or ot_bits32);
    code    : #33#6#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_STRH;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate or ot_bits32,ot_none,);
    code    : #34#64#176;
    flags   : if_arm7
  ),
  (
    opcode  : A_STRH;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,);
    code    : #35#64#176;
    flags   : if_arm7
  ),
  (
    opcode  : A_STRH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_bits32,);
    code    : #36#64#176;
    flags   : if_arm7
  ),
  (
    opcode  : A_STRH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #37#0#176;
    flags   : if_arm7
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #4#0#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_SUB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #5#0#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_SUB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate);
    code    : #6#0#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,);
    code    : #7#2#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_SWI;
    ops     : 1;
    optypes : (ot_immediate,ot_none,ot_none,);
    code    : #2#15;
    flags   : if_8086
  ),
  (
    opcode  : A_SWP;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,);
    code    : #39#1#144;
    flags   : if_arm7
  ),
  (
    opcode  : A_TEQ;
    ops     : 2;
    optypes : (ot_reg32 or ot_signed,ot_reg32,ot_none,);
    code    : #12#1#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_TEQ;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32 or ot_signed,ot_reg32,);
    code    : #13#1#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_TEQ;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_signed,);
    code    : #14#1#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_TEQ;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate,ot_none,);
    code    : #15#3#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_TST;
    ops     : 2;
    optypes : (ot_reg32 or ot_signed,ot_reg32,ot_none,);
    code    : #12#1#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_TST;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32 or ot_signed,ot_reg32,);
    code    : #13#1#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_TST;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_signed,);
    code    : #14#1#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_TST;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate,ot_none,);
    code    : #15#3#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_UMLAL;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #22#0#160#144;
    flags   : if_arm7
  ),
  (
    opcode  : A_UMULL;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32);
    code    : #22#0#128#144;
    flags   : if_arm7
  )
);
