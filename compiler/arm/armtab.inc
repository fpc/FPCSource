{ don't edit, this file is generated from armins.dat }
(
  (
    opcode  : A_NONE;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #0;
    flags   : if_none
  ),
  (
    opcode  : A_ADC;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#65#64;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #4#0#160;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ADC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #6#0#160;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ADC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #7#2#160;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #97#68#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reglo,ot_reglo,ot_reglo,ot_none,ot_none,ot_none);
    code    : #96#24#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reglo,ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #96#28#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #96#48#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reglo,ot_regsp,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #100#168#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_regsp,ot_regsp,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #100#176#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reg32,ot_regsp,ot_reg32,ot_none,ot_none,ot_none);
    code    : #100#68#104;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (ot_regsp,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #100#68#133;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #4#0#128;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ADD;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #6#0#128;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #7#2#128;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ADR;
    ops     : 2;
    optypes : (ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #103#160#0#2;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADR;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam6,ot_none,ot_none,ot_none,ot_none);
    code    : #103#160#0#2;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ADR;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #51#2#15;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#64#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_AND;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #4#0#0;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_AND;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #6#0#0;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_AND;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #7#2#0;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_immediate or ot_bits32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #98#224#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #98#224#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_memory or ot_bits32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #98#224#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_immediate or ot_bits32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #99#208#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #99#208#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_memory or ot_bits32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #99#208#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_immediate24,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #1#10;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_B;
    ops     : 1;
    optypes : (ot_memory or ot_bits32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #1#10;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BIC;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#67#128;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_BIC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #6#1#192;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BIC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #6#1#192;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BIC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #7#3#192;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BL;
    ops     : 1;
    optypes : (ot_immediate24,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #1#11;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BL;
    ops     : 1;
    optypes : (ot_memory or ot_bits32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #1#11;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BLX;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #98#71#128;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_BLX;
    ops     : 1;
    optypes : (ot_immediate24,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #40#250;
    flags   : if_arm32 or if_armv5t
  ),
  (
    opcode  : A_BLX;
    ops     : 1;
    optypes : (ot_memory or ot_bits32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #40#250;
    flags   : if_arm32 or if_armv5t
  ),
  (
    opcode  : A_BLX;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #3#1#47#255#48;
    flags   : if_arm32 or if_armv5t
  ),
  (
    opcode  : A_BKPT;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #96#190#0;
    flags   : if_thumb or if_armv5t
  ),
  (
    opcode  : A_BKPT;
    ops     : 1;
    optypes : (ot_immediate,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #49#1#32#112;
    flags   : if_arm32 or if_armv5t
  ),
  (
    opcode  : A_BKPT;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #49#1#32#112;
    flags   : if_arm32 or if_armv5t
  ),
  (
    opcode  : A_BX;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #98#71#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_BX;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #3#1#47#255#16;
    flags   : if_arm32 or if_armv4t
  ),
  (
    opcode  : A_CDP;
    ops     : 2;
    optypes : (ot_reg8,ot_reg8,ot_none,ot_none,ot_none,ot_none);
    code    : #192#1#16#65;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_CMN;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#66#192;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_CMN;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #12#1#96;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_CMN;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #14#1#96;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_CMN;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #15#1#96;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#66#128;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #97#69#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : (ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #96#40#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #12#1#64;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_CMP;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #14#1#64;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #15#3#64;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LFM;
    ops     : 3;
    optypes : (ot_reg32,ot_immediate or ot_bits8,ot_fpureg,ot_none,ot_none,ot_none);
    code    : #240#2#1;
    flags   : if_fpa
  ),
  (
    opcode  : A_CLZ;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #50#1#111#15#16;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_EOR;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#64#64;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_EOR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #4#0#32;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_EOR;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #6#0#32;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_EOR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #7#2#32;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDC;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #209#192#1#17#65;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDM;
    ops     : 2;
    optypes : (ot_memoryam4,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #105#200;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDM;
    ops     : 2;
    optypes : (ot_reglo,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #105#200;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDM;
    ops     : 2;
    optypes : (ot_memoryam4,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #38#129;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDM;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #38#129;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRBT;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #23#4#112;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRBT;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #23#4#112;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRB;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam3,ot_none,ot_none,ot_none,ot_none);
    code    : #101#92#0#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDRB;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam4,ot_none,ot_none,ot_none,ot_none);
    code    : #102#120#0#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDRB;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #23#4#80;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDR;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam3,ot_none,ot_none,ot_none,ot_none);
    code    : #101#88#0#2;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDR;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam4,ot_none,ot_none,ot_none,ot_none);
    code    : #102#104#0#2;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDR;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam5,ot_none,ot_none,ot_none,ot_none);
    code    : #103#152#0#2;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDR;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam6,ot_none,ot_none,ot_none,ot_none);
    code    : #103#72#0#2;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDR;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #23#4#16;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRH;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam3,ot_none,ot_none,ot_none,ot_none);
    code    : #101#90#0#1;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDRH;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam4,ot_none,ot_none,ot_none,ot_none);
    code    : #102#136#0#1;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDRH;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #34#16#176;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRSB;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam3,ot_none,ot_none,ot_none,ot_none);
    code    : #101#86#0#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDRSB;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #34#16#208;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRSB;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #35#80#208;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediate or ot_bits32,ot_none,ot_none,ot_none);
    code    : #36#80#208;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #37#16#208;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRSH;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam3,ot_none,ot_none,ot_none,ot_none);
    code    : #101#94#0#1;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LDRSH;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #34#16#240;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRT;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #23#4#48;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MCR;
    ops     : 5;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_regf,ot_regf,ot_none);
    code    : #28#14#0#1;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MCR;
    ops     : 6;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_regf,ot_regf,ot_immediateshifter);
    code    : #28#14#0#1;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MCR2;
    ops     : 5;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_regf,ot_regf,ot_none);
    code    : #28#254#0#1;
    flags   : if_arm32 or if_armv5t
  ),
  (
    opcode  : A_MCR2;
    ops     : 6;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_regf,ot_regf,ot_immediateshifter);
    code    : #28#254#0#1;
    flags   : if_arm32 or if_armv5t
  ),
  (
    opcode  : A_MRC;
    ops     : 5;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_regf,ot_regf,ot_none);
    code    : #28#14#16#1;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MRC;
    ops     : 6;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_regf,ot_regf,ot_immediateshifter);
    code    : #28#14#16#1;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MRC2;
    ops     : 5;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_regf,ot_regf,ot_none);
    code    : #28#254#16#1;
    flags   : if_arm32 or if_armv5t
  ),
  (
    opcode  : A_MRC2;
    ops     : 6;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_regf,ot_regf,ot_immediateshifter);
    code    : #28#254#16#1;
    flags   : if_arm32 or if_armv5t
  ),
  (
    opcode  : A_MCRR;
    ops     : 5;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_reg32,ot_regf,ot_none);
    code    : #29#12#64#0;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_MCRR2;
    ops     : 5;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_reg32,ot_regf,ot_none);
    code    : #29#252#64#0;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_MRRC;
    ops     : 5;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_reg32,ot_regf,ot_none);
    code    : #29#12#80#0;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_MRRC2;
    ops     : 5;
    optypes : (ot_regf,ot_immediateshifter,ot_reg32,ot_reg32,ot_regf,ot_none);
    code    : #29#252#80#0;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_MLA;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#0#32#9;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#0#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #97#70#192;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : (ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #96#32#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : (ot_reg32,ot_shifterop,ot_none,ot_none,ot_none,ot_none);
    code    : #8#1#160;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MOV;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #10#1#160;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #11#1#160;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MRS;
    ops     : 2;
    optypes : (ot_reg32,ot_regf,ot_none,ot_none,ot_none,ot_none);
    code    : #16#1#15;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MSR;
    ops     : 2;
    optypes : (ot_regf,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #18#1#40#240;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MSR;
    ops     : 2;
    optypes : (ot_regf,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #19#3#40#240;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MUL;
    ops     : 3;
    optypes : (ot_reglo,ot_reglo,ot_reglo,ot_none,ot_none,ot_none);
    code    : #100#67#64;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_MUL;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #20#0#0#144;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MVF;
    ops     : 2;
    optypes : (ot_fpureg,ot_fpureg,ot_none,ot_none,ot_none,ot_none);
    code    : #242;
    flags   : if_fpa
  ),
  (
    opcode  : A_MVF;
    ops     : 2;
    optypes : (ot_fpureg,ot_immediatefpu,ot_none,ot_none,ot_none,ot_none);
    code    : #242;
    flags   : if_fpa
  ),
  (
    opcode  : A_MVN;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#67#192;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_MVN;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #8#1#224;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MVN;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #10#1#224;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MVN;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #11#1#224;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_VMOV;
    ops     : 2;
    optypes : (ot_vreg,ot_vreg,ot_none,ot_none,ot_none,ot_none);
    code    : #64#14#176#10#64;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMOV;
    ops     : 2;
    optypes : (ot_reg32,ot_vreg,ot_none,ot_none,ot_none,ot_none);
    code    : #64#14#16#10#16;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMOV;
    ops     : 2;
    optypes : (ot_vreg,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #64#14#0#10#16;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMOV;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_vreg,ot_vreg,ot_none,ot_none);
    code    : #64#12#80#10#16;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMOV;
    ops     : 4;
    optypes : (ot_vreg,ot_vreg,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #64#12#64#10#16;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMOV;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_vreg,ot_none,ot_none,ot_none);
    code    : #64#12#80#11#16;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMOV;
    ops     : 3;
    optypes : (ot_vreg,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #64#12#64#11#16;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_NOP;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #97#191#0;
    flags   : if_thumb or if_armv6t2
  ),
  (
    opcode  : A_NOP;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #47#3#32#240#0;
    flags   : if_arm32 or if_armv6k
  ),
  (
    opcode  : A_ORR;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#67#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ORR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #4#1#128;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ORR;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #5#1#128;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ORR;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #6#1#128;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ORR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #7#3#128;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_RSB;
    ops     : 3;
    optypes : (ot_reglo,ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #96#66#64;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_RSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #6#0#96;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_RSB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #6#0#96;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_RSB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #7#0#96;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_RSC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #4#0#224;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_RSC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #5#0#224;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_RSC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #6#0#224;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_RSC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #7#2#224;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SBC;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#65#128;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_SBC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #4#0#192;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SBC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #5#0#192;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SBC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_immediate,ot_none,ot_none);
    code    : #6#0#192;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SBC;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #6#0#192;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SBC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #7#2#192;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SFM;
    ops     : 3;
    optypes : (ot_reg32,ot_immediate or ot_bits8,ot_fpureg,ot_none,ot_none,ot_none);
    code    : #240#2#0;
    flags   : if_fpa
  ),
  (
    opcode  : A_SMLAL;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#0#224#9;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SMULL;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#0#192#9;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STM;
    ops     : 2;
    optypes : (ot_memoryam4,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #105#192;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_STM;
    ops     : 2;
    optypes : (ot_reglo,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #105#192;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_STM;
    ops     : 2;
    optypes : (ot_memoryam4,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #38#128;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STM;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #38#128;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STR;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam3,ot_none,ot_none,ot_none,ot_none);
    code    : #101#80#0#2;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_STR;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam4,ot_none,ot_none,ot_none,ot_none);
    code    : #102#96#0#2;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_STR;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam5,ot_none,ot_none,ot_none,ot_none);
    code    : #103#144#0#2;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_STR;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #23#4#0;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STRB;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam3,ot_none,ot_none,ot_none,ot_none);
    code    : #101#84#0#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_STRB;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam4,ot_none,ot_none,ot_none,ot_none);
    code    : #102#112#0#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_STRB;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #23#4#64;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STRBT;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #23#4#96;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STRBT;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #23#4#96;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STRH;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam3,ot_none,ot_none,ot_none,ot_none);
    code    : #101#82#0#1;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_STRH;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam4,ot_none,ot_none,ot_none,ot_none);
    code    : #102#128#0#1;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_STRH;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #34#0#176;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STRT;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #23#4#32;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_regsp,ot_regsp,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #100#176#128;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reglo,ot_reglo,ot_reglo,ot_none,ot_none,ot_none);
    code    : #96#26#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reglo,ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #96#30#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : (ot_reglo,ot_immediate or ot_bits8,ot_none,ot_none,ot_none,ot_none);
    code    : #96#56#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #4#0#64;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #4#0#64;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #4#0#64;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SUB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #6#0#64;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SWI;
    ops     : 1;
    optypes : (ot_immediate,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #2#15;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SWI;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #2#15;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SWP;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none);
    code    : #39#16#9;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SWPB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none);
    code    : #39#20#9;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_TEQ;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #12#1#32;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_TEQ;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #13#1#32;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_TEQ;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #14#1#32;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_TEQ;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #15#3#32;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_TST;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#66#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_TST;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #12#1#0;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_TST;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #13#1#0;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_TST;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #14#1#0;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_TST;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #15#3#0;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_UMLAL;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#0#160#9;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_UMULL;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#0#128#9;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none);
    code    : #25#0#0#0#208;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_PLD;
    ops     : 1;
    optypes : (ot_memoryam2,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #37#245#80#240#0;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_PLDW;
    ops     : 1;
    optypes : (ot_memoryam2,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #37#245#16#240#0;
    flags   : if_arm32 or if_armv7
  ),
  (
    opcode  : A_QADD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #26#1#0#5;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_QDADD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #26#1#64#5;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_QDSUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #26#1#96#5;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_QSUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #26#1#32#5;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMLABB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#1#0#8;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMLABT;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#1#0#12;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMLATB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#1#0#10;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMLATT;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#1#0#14;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMLALBB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#1#64#8;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMLALBT;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#1#64#12;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMLALTB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#1#64#10;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMLALTT;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#1#64#14;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_VLDM;
    ops     : 2;
    optypes : (ot_memoryam4,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #68#12#16#10;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VLDM;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #68#12#16#10;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VSTM;
    ops     : 2;
    optypes : (ot_memoryam4,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #68#12#0#10;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VSTM;
    ops     : 2;
    optypes : (ot_reg32,ot_reglist,ot_none,ot_none,ot_none,ot_none);
    code    : #68#12#0#10;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VPOP;
    ops     : 1;
    optypes : (ot_reglist,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #68#12#189#10;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VPUSH;
    ops     : 1;
    optypes : (ot_reglist,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #68#13#45#10;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VLDR;
    ops     : 2;
    optypes : (ot_vreg,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #69#13#16#10;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VSTR;
    ops     : 2;
    optypes : (ot_vreg,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #69#13#0#10;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_SMULBB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #21#1#96#8#0;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMULBT;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #21#1#96#12#0;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMULTB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #21#1#96#10#0;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMULTT;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #21#1#96#14#0;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMULWB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #20#1#32#160;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_SMULWT;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #20#1#32#224;
    flags   : if_arm32 or if_armv5te
  ),
  (
    opcode  : A_STRD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none);
    code    : #25#0#0#0#240;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRHT;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #25#0#48#0#176;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STRHT;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #30#0#32#0#176;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRSBT;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #30#0#48#0#208;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STRSBT;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #30#0#48#0#208;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDRSHT;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #30#0#48#0#240;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STRSHT;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #30#0#48#0#240;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BFC;
    ops     : 3;
    optypes : (ot_reg32,ot_immediateshifter,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #45#7#192#0#31;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BFC;
    ops     : 3;
    optypes : (ot_reg32,ot_immediateshifter,ot_immediate or ot_bits32,ot_none,ot_none,ot_none);
    code    : #45#7#192#0#31;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BFI;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_immediateshifter,ot_none,ot_none);
    code    : #45#7#192#0#16;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BFI;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_immediate or ot_bits32,ot_none,ot_none);
    code    : #45#7#192#0#16;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_CLREX;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #47#245#127#240#31;
    flags   : if_arm32 or if_armv6k
  ),
  (
    opcode  : A_LDREX;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam6,ot_none,ot_none,ot_none,ot_none);
    code    : #24#1#144#15#159;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDREXB;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam6,ot_none,ot_none,ot_none,ot_none);
    code    : #24#1#208#15#159;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDREXD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_memoryam6,ot_none,ot_none,ot_none);
    code    : #24#1#176#15#159;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LDREXH;
    ops     : 2;
    optypes : (ot_reg32,ot_memoryam6,ot_none,ot_none,ot_none,ot_none);
    code    : #24#1#240#15#159;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STREX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_memoryam6,ot_none,ot_none,ot_none);
    code    : #24#1#128#15#144;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STREXB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_memoryam6,ot_none,ot_none,ot_none);
    code    : #24#1#192#15#144;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STREXD;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_memoryam6,ot_none,ot_none);
    code    : #24#1#160#15#144;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_STREXH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_memoryam6,ot_none,ot_none,ot_none);
    code    : #24#1#224#15#144;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MLS;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#0#96#9;
    flags   : if_arm32 or if_armv6t2
  ),
  (
    opcode  : A_PKHBT;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#128#1;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_PKHBT;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #22#6#128#1;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_PKHTB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#128#1;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_PKHTB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #22#6#128#5;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_PLI;
    ops     : 1;
    optypes : (ot_memoryam2,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #37#244#80#240#0;
    flags   : if_arm32 or if_armv7
  ),
  (
    opcode  : A_QADD16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#32#241;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_QADD8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#32#249;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_QASX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#32#243;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_QSAX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#32#245;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_QSUB16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#32#247;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_QSUB8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#32#255;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_RBIT;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #50#6#255#15#48;
    flags   : if_arm32 or if_armv6t2
  ),
  (
    opcode  : A_REV;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #97#186#0;
    flags   : if_thumb or if_armv6
  ),
  (
    opcode  : A_REV;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #50#6#191#15#48;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_REV16;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #97#186#64;
    flags   : if_thumb or if_armv6
  ),
  (
    opcode  : A_REV16;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #50#6#191#15#176;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_REVSH;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #97#186#192;
    flags   : if_thumb or if_armv6
  ),
  (
    opcode  : A_REVSH;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #50#6#255#15#176;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SADD16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#16#241;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SADD8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#16#249;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SASX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#16#243;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SBFX;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_immediateshifter,ot_none,ot_none);
    code    : #45#7#160#0#80;
    flags   : if_arm32 or if_armv6t2
  ),
  (
    opcode  : A_SEL;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#128#251;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SETEND;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #43#241#1#0#0;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SEV;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #100#191#64;
    flags   : if_thumb or if_armv7
  ),
  (
    opcode  : A_SEV;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #47#3#32#240#4;
    flags   : if_arm32 or if_armv6k
  ),
  (
    opcode  : A_ASR;
    ops     : 3;
    optypes : (ot_reglo,ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #96#1#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ASR;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#65#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ASR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #48#1#160#0#80;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ASR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #48#1#160#0#64;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LSR;
    ops     : 3;
    optypes : (ot_reglo,ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #96#8#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LSR;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#64#192;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LSR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #48#1#160#0#48;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LSR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #48#1#160#0#32;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LSL;
    ops     : 3;
    optypes : (ot_reglo,ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #96#0#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LSL;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#64#128;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_LSL;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #48#1#160#0#16;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_LSL;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #48#1#160#0#0;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ROR;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #96#65#192;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_ROR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #48#1#160#0#112;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ROR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #48#1#160#0#96;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_RRX;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #48#1#160#0#96;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_UMAAL;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#0#64#9;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SHADD16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#48#241;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SHADD8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#48#249;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SHASX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#48#243;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SHSAX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#48#245;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SHSUB16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#48#247;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SHSUB8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#48#255;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SMLAD;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#7#0#1;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SMLALD;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#7#64#1;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SMLSD;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#7#0#5;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SMLSLD;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #22#7#64#5;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SMMLA;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#7#80#1;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SMMLS;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#7#80#13;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SMMUL;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #21#7#80#1#15;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SMUAD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #21#7#0#1#15;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SMUSD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #21#7#0#5#15;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SSAT;
    ops     : 3;
    optypes : (ot_reg32,ot_immediateshifter,ot_reg32,ot_none,ot_none,ot_none);
    code    : #42#6#160#0#16;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SSAT;
    ops     : 4;
    optypes : (ot_reg32,ot_immediateshifter,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #42#6#160#0#16;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SSAT16;
    ops     : 3;
    optypes : (ot_reg32,ot_immediateshifter,ot_reg32,ot_none,ot_none,ot_none);
    code    : #42#6#160#15#48;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SSAX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#16#245;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SSUB16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#16#247;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SSUB8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#16#255;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTAB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#160#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTAB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #22#6#160#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTAB16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#128#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTAB16;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #22#6#128#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTAH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#176#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTAH;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #22#6#176#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UBFX;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_immediateshifter,ot_immediateshifter,ot_none,ot_none);
    code    : #45#7#224#0#80;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_UXTAB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#224#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTAB;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #22#6#224#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTAB16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#192#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTAB16;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #22#6#192#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTAH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#240#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTAH;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #22#6#240#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTB;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #97#178#64;
    flags   : if_thumb or if_armv6
  ),
  (
    opcode  : A_SXTB;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #27#6#175#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #27#6#175#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTB16;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #27#6#143#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTB16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #27#6#143#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTH;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #97#178#0;
    flags   : if_thumb or if_armv6
  ),
  (
    opcode  : A_SXTH;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #27#6#191#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_SXTH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #27#6#191#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTB;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #97#178#192;
    flags   : if_thumb or if_armv6
  ),
  (
    opcode  : A_UXTB;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #27#6#239#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #27#6#239#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTB16;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #27#6#207#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTB16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #27#6#207#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTH;
    ops     : 2;
    optypes : (ot_reglo,ot_reglo,ot_none,ot_none,ot_none,ot_none);
    code    : #97#178#128;
    flags   : if_thumb or if_armv6
  ),
  (
    opcode  : A_UXTH;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #27#6#255#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UXTH;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none,ot_none,ot_none);
    code    : #27#6#255#7;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UADD16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#80#241;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UADD8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#80#249;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UASX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#80#243;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UHADD16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#112#241;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UHADD8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#112#249;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UHASX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#112#243;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UHSAX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#112#245;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UHSUB16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#112#247;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UHSUB8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#112#255;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UQADD16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#96#241;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UQADD8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#96#249;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UQASX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#96#243;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UQSAX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#96#245;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UQSUB16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#96#247;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_UQSUB8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#96#255;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_USAD8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #21#7#128#1#15;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_USADA8;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #21#7#128#1;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_USAT;
    ops     : 3;
    optypes : (ot_reg32,ot_immediateshifter,ot_reg32,ot_none,ot_none,ot_none);
    code    : #42#6#224#0#16;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_USAT;
    ops     : 4;
    optypes : (ot_reg32,ot_immediateshifter,ot_reg32,ot_shifterop,ot_none,ot_none);
    code    : #42#6#224#0#16;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_USAT16;
    ops     : 3;
    optypes : (ot_reg32,ot_immediateshifter,ot_reg32,ot_none,ot_none,ot_none);
    code    : #42#6#224#15#48;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_USAX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#80#245;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_USUB16;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#80#247;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_USUB8;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none,ot_none,ot_none);
    code    : #22#6#80#255;
    flags   : if_arm32 or if_armv6
  ),
  (
    opcode  : A_WFE;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #100#191#32;
    flags   : if_thumb or if_armv7
  ),
  (
    opcode  : A_WFE;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #47#3#32#240#2;
    flags   : if_arm32 or if_armv6k
  ),
  (
    opcode  : A_WFI;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #100#191#48;
    flags   : if_thumb or if_armv7
  ),
  (
    opcode  : A_WFI;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #47#3#32#240#3;
    flags   : if_arm32 or if_armv6k
  ),
  (
    opcode  : A_YIELD;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #100#191#16;
    flags   : if_thumb or if_armv7
  ),
  (
    opcode  : A_YIELD;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #47#3#32#240#1;
    flags   : if_arm32 or if_armv6k
  ),
  (
    opcode  : A_POP;
    ops     : 1;
    optypes : (ot_reglist,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #105#188;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_POP;
    ops     : 1;
    optypes : (ot_reglist,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #38#139;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_PUSH;
    ops     : 1;
    optypes : (ot_reglist,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #105#180;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_PUSH;
    ops     : 1;
    optypes : (ot_reglist,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #38#128;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MOVT;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate,ot_none,ot_none,ot_none,ot_none);
    code    : #44#3#64;
    flags   : if_arm32 or if_armv6t2
  ),
  (
    opcode  : A_MOVT;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #44#3#64;
    flags   : if_arm32 or if_armv6t2
  ),
  (
    opcode  : A_IT;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITE;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITT;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITEE;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITTE;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITET;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITTT;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITEEE;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITTEE;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITETE;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITTTE;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITEET;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITTET;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITETT;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_ITTTT;
    ops     : 1;
    optypes : (ot_condition,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #254;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_MOVW;
    ops     : 2;
    optypes : (ot_reg32,ot_immediate,ot_none,ot_none,ot_none,ot_none);
    code    : #44#3#0;
    flags   : if_arm32 or if_armv6t2
  ),
  (
    opcode  : A_MOVW;
    ops     : 2;
    optypes : (ot_reg32,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #44#3#0;
    flags   : if_arm32 or if_armv6t2
  ),
  (
    opcode  : A_CBZ;
    ops     : 2;
    optypes : (ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #104#177;
    flags   : if_thumb or if_armv6t2
  ),
  (
    opcode  : A_CBZ;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #104#177;
    flags   : if_thumb or if_armv6t2
  ),
  (
    opcode  : A_CBNZ;
    ops     : 2;
    optypes : (ot_reglo,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #104#185;
    flags   : if_thumb or if_armv6t2
  ),
  (
    opcode  : A_CBNZ;
    ops     : 2;
    optypes : (ot_reglo,ot_memoryam2,ot_none,ot_none,ot_none,ot_none);
    code    : #104#185;
    flags   : if_thumb or if_armv6t2
  ),
  (
    opcode  : A_VABS;
    ops     : 2;
    optypes : (ot_vreg,ot_vreg,ot_none,ot_none,ot_none,ot_none);
    code    : #66#14#176#10#192;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VADD;
    ops     : 3;
    optypes : (ot_vreg,ot_vreg,ot_vreg,ot_none,ot_none,ot_none);
    code    : #66#14#48#10#0;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VCMP;
    ops     : 2;
    optypes : (ot_vreg,ot_vreg,ot_none,ot_none,ot_none,ot_none);
    code    : #66#14#180#10#64;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VCMP;
    ops     : 2;
    optypes : (ot_vreg,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #66#14#181#10#64;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VCMPE;
    ops     : 2;
    optypes : (ot_vreg,ot_vreg,ot_none,ot_none,ot_none,ot_none);
    code    : #66#14#180#10#192;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VCMPE;
    ops     : 2;
    optypes : (ot_vreg,ot_immediateshifter,ot_none,ot_none,ot_none,ot_none);
    code    : #66#14#181#10#192;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VCVT;
    ops     : 2;
    optypes : (ot_vreg,ot_vreg,ot_none,ot_none,ot_none,ot_none);
    code    : #67#14#184#10#192;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VCVT;
    ops     : 3;
    optypes : (ot_vreg,ot_vreg,ot_immediateshifter,ot_none,ot_none,ot_none);
    code    : #67#14#186#10#64;
    flags   : if_arm32 or if_vfpv3
  ),
  (
    opcode  : A_VCVTR;
    ops     : 2;
    optypes : (ot_vreg,ot_vreg,ot_none,ot_none,ot_none,ot_none);
    code    : #67#14#184#10#64;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VDIV;
    ops     : 3;
    optypes : (ot_vreg,ot_vreg,ot_vreg,ot_none,ot_none,ot_none);
    code    : #66#14#128#10#0;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMRS;
    ops     : 2;
    optypes : (ot_reg32,ot_regf,ot_none,ot_none,ot_none,ot_none);
    code    : #65#14#240#10#16;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMRS;
    ops     : 2;
    optypes : (ot_regf,ot_regf,ot_none,ot_none,ot_none,ot_none);
    code    : #65#14#240#10#16;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMSR;
    ops     : 2;
    optypes : (ot_regf,ot_reg32,ot_none,ot_none,ot_none,ot_none);
    code    : #65#14#224#10#16;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMLA;
    ops     : 3;
    optypes : (ot_vreg,ot_vreg,ot_vreg,ot_none,ot_none,ot_none);
    code    : #66#14#0#10#0;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMLS;
    ops     : 3;
    optypes : (ot_vreg,ot_vreg,ot_vreg,ot_none,ot_none,ot_none);
    code    : #66#14#0#10#64;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VMUL;
    ops     : 3;
    optypes : (ot_vreg,ot_vreg,ot_vreg,ot_none,ot_none,ot_none);
    code    : #66#14#32#10#0;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VNMLA;
    ops     : 3;
    optypes : (ot_vreg,ot_vreg,ot_vreg,ot_none,ot_none,ot_none);
    code    : #66#14#16#10#64;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VNMLS;
    ops     : 3;
    optypes : (ot_vreg,ot_vreg,ot_vreg,ot_none,ot_none,ot_none);
    code    : #66#14#16#10#0;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VNMUL;
    ops     : 3;
    optypes : (ot_vreg,ot_vreg,ot_vreg,ot_none,ot_none,ot_none);
    code    : #66#14#32#10#64;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VNEG;
    ops     : 2;
    optypes : (ot_vreg,ot_vreg,ot_none,ot_none,ot_none,ot_none);
    code    : #66#14#177#10#64;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VSQRT;
    ops     : 2;
    optypes : (ot_vreg,ot_vreg,ot_none,ot_none,ot_none,ot_none);
    code    : #66#14#177#10#192;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_VSUB;
    ops     : 3;
    optypes : (ot_vreg,ot_vreg,ot_vreg,ot_none,ot_none,ot_none);
    code    : #66#14#48#10#64;
    flags   : if_arm32 or if_vfpv2
  ),
  (
    opcode  : A_DMB;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #46#245#127#240#80;
    flags   : if_arm32 or if_armv7
  ),
  (
    opcode  : A_ISB;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #46#245#127#240#96;
    flags   : if_arm32 or if_armv7
  ),
  (
    opcode  : A_DSB;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #46#245#127#240#64;
    flags   : if_arm32 or if_armv7
  ),
  (
    opcode  : A_SMC;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #46#1#96#0#112;
    flags   : if_arm32 or if_armv7
  ),
  (
    opcode  : A_SVC;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #97#223#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_SVC;
    ops     : 1;
    optypes : (ot_immediate or ot_bits32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #2#15;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_SVC;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #2#15;
    flags   : if_arm32 or if_armv4
  ),
  (
    opcode  : A_BXJ;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #3#1#47#255#32;
    flags   : if_arm32 or if_armv5tej
  ),
  (
    opcode  : A_UDF;
    ops     : 1;
    optypes : (ot_immediateshifter,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #97#222#0;
    flags   : if_thumb or if_armv4t
  ),
  (
    opcode  : A_UDF;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none,ot_none,ot_none);
    code    : #0;
    flags   : if_arm32 or if_armv4t
  )
);
