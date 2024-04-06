{ don't edit, this file is generated from mos6502ins.dat; to regenerate, run 'make insdat' in the compiler directory }
(
  (
    opcode  : A_None;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : 'void';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 1;
    optypes : (OT_INDEXED_INDIRECT,OT_NONE);
    code    : '$61';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$65';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$69';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$6D';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 1;
    optypes : (OT_INDIRECT_INDEXED,OT_NONE);
    code    : '$71';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$75';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 1;
    optypes : (OT_ABSOLUTE_Y,OT_NONE);
    code    : '$79';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$7D';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 1;
    optypes : (OT_INDEXED_INDIRECT,OT_NONE);
    code    : '$21';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$25';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$29';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$2D';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 1;
    optypes : (OT_INDIRECT_INDEXED,OT_NONE);
    code    : '$31';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$35';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 1;
    optypes : (OT_ABSOLUTE_Y,OT_NONE);
    code    : '$39';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$3D';
    flags   : 0
  ),
  (
    opcode  : A_ASL;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$06';
    flags   : 0
  ),
  (
    opcode  : A_ASL;
    ops     : 1;
    optypes : (OT_ACCUMULATOR,OT_NONE);
    code    : '$0A';
    flags   : 0
  ),
  (
    opcode  : A_ASL;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$0E';
    flags   : 0
  ),
  (
    opcode  : A_ASL;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$16';
    flags   : 0
  ),
  (
    opcode  : A_ASL;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$1E';
    flags   : 0
  ),
  (
    opcode  : A_BCC;
    ops     : 1;
    optypes : (OT_RELATIVE,OT_NONE);
    code    : '$90';
    flags   : 0
  ),
  (
    opcode  : A_BCS;
    ops     : 1;
    optypes : (OT_RELATIVE,OT_NONE);
    code    : '$B0';
    flags   : 0
  ),
  (
    opcode  : A_BEQ;
    ops     : 1;
    optypes : (OT_RELATIVE,OT_NONE);
    code    : '$F0';
    flags   : 0
  ),
  (
    opcode  : A_BIT;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$24';
    flags   : 0
  ),
  (
    opcode  : A_BIT;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$2C';
    flags   : 0
  ),
  (
    opcode  : A_BMI;
    ops     : 1;
    optypes : (OT_RELATIVE,OT_NONE);
    code    : '$30';
    flags   : 0
  ),
  (
    opcode  : A_BNE;
    ops     : 1;
    optypes : (OT_RELATIVE,OT_NONE);
    code    : '$D0';
    flags   : 0
  ),
  (
    opcode  : A_BPL;
    ops     : 1;
    optypes : (OT_RELATIVE,OT_NONE);
    code    : '$10';
    flags   : 0
  ),
  (
    opcode  : A_BRK;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$00';
    flags   : 0
  ),
  (
    opcode  : A_BVC;
    ops     : 1;
    optypes : (OT_RELATIVE,OT_NONE);
    code    : '$50';
    flags   : 0
  ),
  (
    opcode  : A_BVS;
    ops     : 1;
    optypes : (OT_RELATIVE,OT_NONE);
    code    : '$70';
    flags   : 0
  ),
  (
    opcode  : A_CLC;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$18';
    flags   : 0
  ),
  (
    opcode  : A_CLD;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$D8';
    flags   : 0
  ),
  (
    opcode  : A_CLI;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$58';
    flags   : 0
  ),
  (
    opcode  : A_CLV;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$B8';
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    ops     : 1;
    optypes : (OT_INDEXED_INDIRECT,OT_NONE);
    code    : '$C1';
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$C5';
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$C9';
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$CD';
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    ops     : 1;
    optypes : (OT_INDIRECT_INDEXED,OT_NONE);
    code    : '$D1';
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$D5';
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    ops     : 1;
    optypes : (OT_ABSOLUTE_Y,OT_NONE);
    code    : '$D9';
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$DD';
    flags   : 0
  ),
  (
    opcode  : A_CPX;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$E0';
    flags   : 0
  ),
  (
    opcode  : A_CPX;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$E4';
    flags   : 0
  ),
  (
    opcode  : A_CPX;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$EC';
    flags   : 0
  ),
  (
    opcode  : A_CPY;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$C0';
    flags   : 0
  ),
  (
    opcode  : A_CPY;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$C4';
    flags   : 0
  ),
  (
    opcode  : A_CPY;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$CC';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$C6';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$CE';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$D6';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$DE';
    flags   : 0
  ),
  (
    opcode  : A_DEX;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$CA';
    flags   : 0
  ),
  (
    opcode  : A_DEY;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$88';
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    ops     : 1;
    optypes : (OT_INDEXED_INDIRECT,OT_NONE);
    code    : '$41';
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$45';
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$49';
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$4D';
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    ops     : 1;
    optypes : (OT_INDIRECT_INDEXED,OT_NONE);
    code    : '$51';
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$55';
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    ops     : 1;
    optypes : (OT_ABSOLUTE_Y,OT_NONE);
    code    : '$59';
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$5D';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$E6';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$EE';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$F6';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$FE';
    flags   : 0
  ),
  (
    opcode  : A_INX;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$E8';
    flags   : 0
  ),
  (
    opcode  : A_INY;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$C8';
    flags   : 0
  ),
  (
    opcode  : A_JMP;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$4C';
    flags   : 0
  ),
  (
    opcode  : A_JMP;
    ops     : 1;
    optypes : (OT_INDIRECT,OT_NONE);
    code    : '$6C';
    flags   : 0
  ),
  (
    opcode  : A_JSR;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$20';
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    ops     : 1;
    optypes : (OT_INDEXED_INDIRECT,OT_NONE);
    code    : '$A1';
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$A5';
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$A9';
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$AD';
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    ops     : 1;
    optypes : (OT_INDIRECT_INDEXED,OT_NONE);
    code    : '$B1';
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$B5';
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    ops     : 1;
    optypes : (OT_ABSOLUTE_Y,OT_NONE);
    code    : '$B9';
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$BD';
    flags   : 0
  ),
  (
    opcode  : A_LDX;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$A2';
    flags   : 0
  ),
  (
    opcode  : A_LDX;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$A6';
    flags   : 0
  ),
  (
    opcode  : A_LDX;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$AE';
    flags   : 0
  ),
  (
    opcode  : A_LDX;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_Y,OT_NONE);
    code    : '$B6';
    flags   : 0
  ),
  (
    opcode  : A_LDX;
    ops     : 1;
    optypes : (OT_ABSOLUTE_Y,OT_NONE);
    code    : '$BE';
    flags   : 0
  ),
  (
    opcode  : A_LDY;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$A0';
    flags   : 0
  ),
  (
    opcode  : A_LDY;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$A4';
    flags   : 0
  ),
  (
    opcode  : A_LDY;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$AC';
    flags   : 0
  ),
  (
    opcode  : A_LDY;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$B4';
    flags   : 0
  ),
  (
    opcode  : A_LDY;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$BC';
    flags   : 0
  ),
  (
    opcode  : A_LSR;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$46';
    flags   : 0
  ),
  (
    opcode  : A_LSR;
    ops     : 1;
    optypes : (OT_ACCUMULATOR,OT_NONE);
    code    : '$4A';
    flags   : 0
  ),
  (
    opcode  : A_LSR;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$4E';
    flags   : 0
  ),
  (
    opcode  : A_LSR;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$56';
    flags   : 0
  ),
  (
    opcode  : A_LSR;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$5E';
    flags   : 0
  ),
  (
    opcode  : A_NOP;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$EA';
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    ops     : 1;
    optypes : (OT_INDEXED_INDIRECT,OT_NONE);
    code    : '$01';
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$05';
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$09';
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$0D';
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    ops     : 1;
    optypes : (OT_INDIRECT_INDEXED,OT_NONE);
    code    : '$11';
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$15';
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    ops     : 1;
    optypes : (OT_ABSOLUTE_Y,OT_NONE);
    code    : '$19';
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$1D';
    flags   : 0
  ),
  (
    opcode  : A_PHA;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$48';
    flags   : 0
  ),
  (
    opcode  : A_PHP;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$08';
    flags   : 0
  ),
  (
    opcode  : A_PLA;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$68';
    flags   : 0
  ),
  (
    opcode  : A_PLP;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$28';
    flags   : 0
  ),
  (
    opcode  : A_ROL;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$26';
    flags   : 0
  ),
  (
    opcode  : A_ROL;
    ops     : 1;
    optypes : (OT_ACCUMULATOR,OT_NONE);
    code    : '$2A';
    flags   : 0
  ),
  (
    opcode  : A_ROL;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$2E';
    flags   : 0
  ),
  (
    opcode  : A_ROL;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$36';
    flags   : 0
  ),
  (
    opcode  : A_ROL;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$3E';
    flags   : 0
  ),
  (
    opcode  : A_ROR;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$66';
    flags   : 0
  ),
  (
    opcode  : A_ROR;
    ops     : 1;
    optypes : (OT_ACCUMULATOR,OT_NONE);
    code    : '$6A';
    flags   : 0
  ),
  (
    opcode  : A_ROR;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$6E';
    flags   : 0
  ),
  (
    opcode  : A_ROR;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$76';
    flags   : 0
  ),
  (
    opcode  : A_ROR;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$7E';
    flags   : 0
  ),
  (
    opcode  : A_RTI;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$40';
    flags   : 0
  ),
  (
    opcode  : A_RTS;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$60';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 1;
    optypes : (OT_INDEXED_INDIRECT,OT_NONE);
    code    : '$E1';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$E5';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 1;
    optypes : (OT_IMMEDIATE,OT_NONE);
    code    : '$E9';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$ED';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 1;
    optypes : (OT_INDIRECT_INDEXED,OT_NONE);
    code    : '$F1';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$F5';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 1;
    optypes : (OT_ABSOLUTE_Y,OT_NONE);
    code    : '$F9';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$FD';
    flags   : 0
  ),
  (
    opcode  : A_SEC;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$38';
    flags   : 0
  ),
  (
    opcode  : A_SED;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$F8';
    flags   : 0
  ),
  (
    opcode  : A_SEI;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$78';
    flags   : 0
  ),
  (
    opcode  : A_STA;
    ops     : 1;
    optypes : (OT_INDEXED_INDIRECT,OT_NONE);
    code    : '$81';
    flags   : 0
  ),
  (
    opcode  : A_STA;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$85';
    flags   : 0
  ),
  (
    opcode  : A_STA;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$8D';
    flags   : 0
  ),
  (
    opcode  : A_STA;
    ops     : 1;
    optypes : (OT_INDIRECT_INDEXED,OT_NONE);
    code    : '$91';
    flags   : 0
  ),
  (
    opcode  : A_STA;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$95';
    flags   : 0
  ),
  (
    opcode  : A_STA;
    ops     : 1;
    optypes : (OT_ABSOLUTE_Y,OT_NONE);
    code    : '$99';
    flags   : 0
  ),
  (
    opcode  : A_STA;
    ops     : 1;
    optypes : (OT_ABSOLUTE_X,OT_NONE);
    code    : '$9D';
    flags   : 0
  ),
  (
    opcode  : A_STX;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$86';
    flags   : 0
  ),
  (
    opcode  : A_STX;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$8E';
    flags   : 0
  ),
  (
    opcode  : A_STX;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_Y,OT_NONE);
    code    : '$96';
    flags   : 0
  ),
  (
    opcode  : A_STY;
    ops     : 1;
    optypes : (OT_ZERO_PAGE,OT_NONE);
    code    : '$84';
    flags   : 0
  ),
  (
    opcode  : A_STY;
    ops     : 1;
    optypes : (OT_ABSOLUTE,OT_NONE);
    code    : '$8C';
    flags   : 0
  ),
  (
    opcode  : A_STY;
    ops     : 1;
    optypes : (OT_ZERO_PAGE_X,OT_NONE);
    code    : '$94';
    flags   : 0
  ),
  (
    opcode  : A_TAX;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$AA';
    flags   : 0
  ),
  (
    opcode  : A_TAY;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$A8';
    flags   : 0
  ),
  (
    opcode  : A_TSX;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$BA';
    flags   : 0
  ),
  (
    opcode  : A_TXA;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$8A';
    flags   : 0
  ),
  (
    opcode  : A_TXS;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$9A';
    flags   : 0
  ),
  (
    opcode  : A_TYA;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$98';
    flags   : 0
  )
);
