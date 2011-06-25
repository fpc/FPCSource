{ don't edit, this file is generated from avr32ins.dat }
(
  (
    opcode  : A_NONE;
    ops     : 0;
    optypes : (ot_none,ot_none,ot_none,ot_none);
    code    : #0;
    flags   : if_none
  ),
  (
    opcode  : A_ABS;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none);
    code    : #2#4;
    flags   : if_1
  ),
  (
    opcode  : A_ACR;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none);
    code    : #2#0;
    flags   : if_1
  ),
  (
    opcode  : A_ADC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#4;
    flags   : if_1
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #1#0;
    flags   : if_1
  ),
  (
    opcode  : A_ADD;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none);
    code    : #17#0;
    flags   : if_1
  ),
  (
    opcode  : A_ADDABS;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#228;
    flags   : if_1
  ),
  (
    opcode  : A_CPC;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #23#3;
    flags   : if_1
  ),
  (
    opcode  : A_CPC;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none);
    code    : #2#2;
    flags   : if_1
  ),
  (
    opcode  : A_MAX;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#196;
    flags   : if_1
  ),
  (
    opcode  : A_MIN;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#212;
    flags   : if_1
  ),
  (
    opcode  : A_NEG;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none);
    code    : #2#3;
    flags   : if_1
  ),
  (
    opcode  : A_RSUB;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #1#2;
    flags   : if_1
  ),
  (
    opcode  : A_RSUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_imm or ot_bits8,ot_none);
    code    : #19#1;
    flags   : if_1
  ),
  (
    opcode  : A_SBC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#20;
    flags   : if_1
  ),
  (
    opcode  : A_SCR;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits5,ot_none,ot_none);
    code    : #9#13;
    flags   : if_1
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #1#1;
    flags   : if_1
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none);
    code    : #17#1;
    flags   : if_1
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits8,ot_none,ot_none);
    code    : #4#0;
    flags   : if_1
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits21,ot_none,ot_none);
    code    : #24#1;
    flags   : if_1
  ),
  (
    opcode  : A_SUB;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_imm or ot_bits16,ot_none);
    code    : #26#12;
    flags   : if_1
  ),
  (
    opcode  : A_TBNZ;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none);
    code    : #2#14;
    flags   : if_1
  ),
  (
    opcode  : A_DIVS;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#192;
    flags   : if_1
  ),
  (
    opcode  : A_DIVU;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#208;
    flags   : if_1
  ),
  (
    opcode  : A_MAC;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#52;
    flags   : if_1
  ),
  (
    opcode  : A_MACS;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#84;
    flags   : if_1
  ),
  (
    opcode  : A_MACU;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#116;
    flags   : if_1
  ),
  (
    opcode  : A_MUL;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #1#179;
    flags   : if_1
  ),
  (
    opcode  : A_MUL;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#36;
    flags   : if_1
  ),
  (
    opcode  : A_MUL;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_imm or ot_bits8,ot_none);
    code    : #19#0;
    flags   : if_1
  ),
  (
    opcode  : A_MULS;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#68;
    flags   : if_1
  ),
  (
    opcode  : A_MULU;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_reg32,ot_none);
    code    : #16#100;
    flags   : if_1
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #1#6;
    flags   : if_1
  ),
  (
    opcode  : A_AND;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none);
    code    : #41#0;
    flags   : if_1
  ),
  (
    opcode  : A_ANDN;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #1#8;
    flags   : if_1
  ),
  (
    opcode  : A_ANDH;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits16,ot_none,ot_none);
    code    : #39#2;
    flags   : if_1
  ),
  (
    opcode  : A_ANDH;
    ops     : 3;
    optypes : (ot_reg32,ot_imm or ot_bits16,ot_coh,ot_none);
    code    : #39#3;
    flags   : if_1
  ),
  (
    opcode  : A_ANDL;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits16,ot_none,ot_none);
    code    : #39#0;
    flags   : if_1
  ),
  (
    opcode  : A_ANDL;
    ops     : 3;
    optypes : (ot_reg32,ot_imm or ot_bits16,ot_coh,ot_none);
    code    : #39#1;
    flags   : if_1
  ),
  (
    opcode  : A_COM;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none);
    code    : #2#11;
    flags   : if_1
  ),
  (
    opcode  : A_EOR;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #1#5;
    flags   : if_1
  ),
  (
    opcode  : A_EOR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none);
    code    : #41#32;
    flags   : if_1
  ),
  (
    opcode  : A_EORH;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits16,ot_none,ot_none);
    code    : #24#0#225;
    flags   : if_1
  ),
  (
    opcode  : A_EORL;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits16,ot_none,ot_none);
    code    : #24#0#193;
    flags   : if_1
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #1#4;
    flags   : if_1
  ),
  (
    opcode  : A_OR;
    ops     : 3;
    optypes : (ot_reg32,ot_reg32,ot_shifterop,ot_none);
    code    : #41#128;
    flags   : if_1
  ),
  (
    opcode  : A_ORH;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits16,ot_none,ot_none);
    code    : #24#0#161;
    flags   : if_1
  ),
  (
    opcode  : A_ORL;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits16,ot_none,ot_none);
    code    : #24#0#129;
    flags   : if_1
  ),
  (
    opcode  : A_TST;
    ops     : 2;
    optypes : (ot_reg32,ot_reg32,ot_none,ot_none);
    code    : #1#7;
    flags   : if_1
  ),
  (
    opcode  : A_BFEXTS;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_imm or ot_bits5,ot_imm or ot_bits5);
    code    : #21#44;
    flags   : if_1
  ),
  (
    opcode  : A_BFEXTU;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_imm or ot_bits5,ot_imm or ot_bits5);
    code    : #21#48;
    flags   : if_1
  ),
  (
    opcode  : A_BFINS;
    ops     : 4;
    optypes : (ot_reg32,ot_reg32,ot_imm or ot_bits5,ot_imm or ot_bits5);
    code    : #21#52;
    flags   : if_1
  ),
  (
    opcode  : A_BLD;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits5,ot_none,ot_none);
    code    : #34#6;
    flags   : if_1
  ),
  (
    opcode  : A_BREV;
    ops     : 1;
    optypes : (ot_reg32,ot_none,ot_none,ot_none);
    code    : #2#73;
    flags   : if_1
  ),
  (
    opcode  : A_BST;
    ops     : 2;
    optypes : (ot_reg32,ot_imm or ot_bits5,ot_none,ot_none);
    code    : #34#7;
    flags   : if_1
  )
);
