{ don't edit, this file is generated from armins.dat }
(
  (
    opcode  : A_NONE;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none);
    code    : #0;
    flags   : if_none
  ),
  (
    opcode  : A_ADC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
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
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none);
    code    : #7#2#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
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
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop);
    code    : #6#0#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none);
    code    : #7#2#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_AND;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #4#0#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_AND;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop);
    code    : #6#0#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_AND;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none);
    code    : #7#2#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_memory or ot_bits32,ot_none,ot_none,ot_none);
    code    : #1#10;
    flags   : if_arm7
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_immediate24,ot_none,ot_none,ot_none);
    code    : #1#10;
    flags   : if_arm7
  ),
  (
    opcode  : A_BIC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none);
    code    : #7#3#192;
    flags   : if_arm7
  ),
  (
    opcode  : A_BL;
    ops     : 1;
    optypes : (ot_memory or ot_bits32,ot_none,ot_none,ot_none);
    code    : #1#11;
    flags   : if_arm7
  ),
  (
    opcode  : A_BL;
    ops     : 1;
    optypes : (ot_immediate24,ot_none,ot_none,ot_none);
    code    : #1#11;
    flags   : if_arm7
  ),
  (
    opcode  : A_BLX;
    ops     : 1;
    optypes : (ot_memory or ot_bits32,ot_none,ot_none,ot_none);
    code    : #15#15;
    flags   : if_arm7
  ),
  (
    opcode  : A_BLX;
    ops     : 1;
    optypes : (ot_immediate24,ot_none,ot_none,ot_none);
    code    : #15#15;
    flags   : if_arm7
  ),
  (
    opcode  : A_BX;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none);
    code    : #3#1#47#255#16;
    flags   : if_arm7
  ),
  (
    opcode  : A_CDP;
    ops     : 2;
    optypes : (ot_reg8,ot_reg8,ot_none,ot_none);
    code    : #192#1#16#65;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMN;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #12#1#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMN;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #13#1#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMN;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,ot_none);
    code    : #14#1#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMN;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none);
    code    : #15#1#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #12#1#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMP;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #13#1#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMP;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none);
    code    : #14#1#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none);
    code    : #15#3#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_CLZ;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #39#1#1;
    flags   : if_arm7
  ),
  (
    opcode  : A_EOR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #4#0#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_EOR;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop);
    code    : #6#0#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_EOR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none);
    code    : #7#2#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDC;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #209#192#1#17#65;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDM;
    ops     : 2;
    optypes : (ot_memoryam4,ot_reglist,ot_none,ot_none);
    code    : #38#129;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDM;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,ot_none);
    code    : #38#129;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRB;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none);
    code    : #23#4#80;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDR;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none);
    code    : #23#4#16;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRH;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none);
    code    : #34#16#176;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSB;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none);
    code    : #34#16#208;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSB;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #35#80#208;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_bits32,ot_none);
    code    : #36#80#208;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #37#16#208;
    flags   : if_arm7
  ),
  (
    opcode  : A_LDRSH;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none);
    code    : #34#16#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_LFM;
    ops     : 3;
    optypes : (ot_reg32,ot_immediate or ot_bits8,ot_fpureg,ot_none);
    code    : #240#2#1;
    flags   : if_fpa
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
    optypes : (ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #8#1#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_MOV;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none);
    code    : #10#1#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none);
    code    : #11#1#160;
    flags   : if_arm7
  ),
  (
    opcode  : A_MRS;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #16#1#15;
    flags   : if_arm7
  ),
  (
    opcode  : A_MSR;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #17#1#41#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_MSR;
    ops     : 2;
    optypes : (ot_regf,ot_reg32,ot_none,ot_none);
    code    : #18#1#40#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_MSR;
    ops     : 2;
    optypes : (ot_regf,ot_immediate,ot_none,ot_none);
    code    : #19#3#40#240;
    flags   : if_arm7
  ),
  (
    opcode  : A_MUL;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #20#0#0#144;
    flags   : if_arm7
  ),
  (
    opcode  : A_MVF;
    ops     : 2;
    optypes : (ot_fpureg,ot_fpureg,ot_none,ot_none);
    code    : #242;
    flags   : if_fpa
  ),
  (
    opcode  : A_MVF;
    ops     : 2;
    optypes : (ot_fpureg,ot_immediatefpu,ot_none,ot_none);
    code    : #242;
    flags   : if_fpa
  ),
  (
    opcode  : A_MVN;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #8#1#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_MVN;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none);
    code    : #10#1#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_MVN;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none);
    code    : #11#1#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_ORR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
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
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop);
    code    : #6#1#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_ORR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none);
    code    : #7#3#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop);
    code    : #6#0#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none);
    code    : #7#0#96;
    flags   : if_arm7
  ),
  (
    opcode  : A_RSC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
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
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none);
    code    : #7#2#224;
    flags   : if_arm7
  ),
  (
    opcode  : A_SBC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
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
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none);
    code    : #7#2#192;
    flags   : if_arm7
  ),
  (
    opcode  : A_SFM;
    ops     : 3;
    optypes : (ot_reg32,ot_immediate or ot_bits8,ot_fpureg,ot_none);
    code    : #240#2#0;
    flags   : if_fpa
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
    opcode  : A_STM;
    ops     : 2;
    optypes : (ot_memoryam4,ot_reglist,ot_none,ot_none);
    code    : #38#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_STM;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,ot_none);
    code    : #38#128;
    flags   : if_arm7
  ),
  (
    opcode  : A_STR;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none);
    code    : #23#4#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_STRB;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none);
    code    : #23#4#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_STRH;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none);
    code    : #34#0#176;
    flags   : if_arm7
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none);
    code    : #4#0#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none);
    code    : #4#0#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #4#0#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_SUB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop);
    code    : #6#0#64;
    flags   : if_arm7
  ),
  (
    opcode  : A_SWI;
    ops     : 1;
    optypes : (ot_immediate,ot_none,ot_none,ot_none);
    code    : #2#15;
    flags   : if_arm7
  ),
  (
    opcode  : A_SWP;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_memoryam2,ot_none);
    code    : #39#16#9;
    flags   : if_arm7
  ),
  (
    opcode  : A_SWPB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #39#20#9;
    flags   : if_arm7
  ),
  (
    opcode  : A_TEQ;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #12#1#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_TEQ;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #13#1#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_TEQ;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate,ot_none);
    code    : #14#1#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_TEQ;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate,ot_none,ot_none);
    code    : #15#3#32;
    flags   : if_arm7
  ),
  (
    opcode  : A_TST;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #12#1#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_TST;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #13#1#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_TST;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none);
    code    : #14#1#0;
    flags   : if_arm7
  ),
  (
    opcode  : A_TST;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none);
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
