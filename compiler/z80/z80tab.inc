{ don't edit, this file is generated from z80ins.dat; to regenerate, run 'make insdat' in the compiler directory }
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
    ops     : 2;
    optypes : (OT_REG8_A,OT_REG8);
    code    : '%10001rrr';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 2;
    optypes : (OT_REG8_A,OT_IMM8);
    code    : '$CE,n';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_HL);
    code    : '$8E';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IX_d);
    code    : '$DD,$8E,d';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IY_d);
    code    : '$FD,$8E,d';
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    ops     : 2;
    optypes : (OT_REG16_HL,OT_REG16_BC_DE_HL_SP);
    code    : '$ED,%01dd1010';
    flags   : 0
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REG8);
    code    : '%10000rrr';
    flags   : 0
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (OT_REG8_A,OT_IMM8);
    code    : '$C6,n';
    flags   : 0
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_HL);
    code    : '$86';
    flags   : 0
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IX_d);
    code    : '$DD,$86,d';
    flags   : 0
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IY_d);
    code    : '$FD,$86,d';
    flags   : 0
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (OT_REG16_HL,OT_REG16_BC_DE_HL_SP);
    code    : '%00dd1001';
    flags   : 0
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (OT_REG16_IX,OT_REG16_BC_DE_IX_SP);
    code    : '$DD,%00pp1001';
    flags   : 0
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : (OT_REG16_IY,OT_REG16_BC_DE_IY_SP);
    code    : '$FD,%00rr1001';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REG8);
    code    : '%10100rrr';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : (OT_REG8_A,OT_IMM8);
    code    : '$E6,n';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_HL);
    code    : '$A6';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IX_d);
    code    : '$DD,$A6,d';
    flags   : 0
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IY_d);
    code    : '$FD,$A6,d';
    flags   : 0
  ),
  (
    opcode  : A_BIT;
    ops     : 2;
    optypes : (OT_IMM3,OT_REG8);
    code    : '$CB,%01bbbrrr';
    flags   : 0
  ),
  (
    opcode  : A_BIT;
    ops     : 2;
    optypes : (OT_IMM3,OT_REF_HL);
    code    : '$CB,%01bbb110';
    flags   : 0
  ),
  (
    opcode  : A_BIT;
    ops     : 2;
    optypes : (OT_IMM3,OT_REF_IX_d);
    code    : '$DD,$CB,d,%01bbb110';
    flags   : 0
  ),
  (
    opcode  : A_BIT;
    ops     : 2;
    optypes : (OT_IMM3,OT_REF_IY_d);
    code    : '$FD,$CB,d,%01bbb110';
    flags   : 0
  ),
  (
    opcode  : A_CALL;
    ops     : 1;
    optypes : (OT_IMM16,OT_NONE);
    code    : '$CD,nn';
    flags   : 0
  ),
  (
    opcode  : A_CALL;
    ops     : 2;
    optypes : (OT_COND,OT_IMM16);
    code    : '%11ccc100,nn';
    flags   : 0
  ),
  (
    opcode  : A_CCF;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$3F';
    flags   : 0
  ),
  (
    opcode  : A_CP;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REG8);
    code    : '%10111rrr';
    flags   : 0
  ),
  (
    opcode  : A_CP;
    ops     : 2;
    optypes : (OT_REG8_A,OT_IMM8);
    code    : '$FE,n';
    flags   : 0
  ),
  (
    opcode  : A_CP;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_HL);
    code    : '$BE';
    flags   : 0
  ),
  (
    opcode  : A_CP;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IX_d);
    code    : '$DD,$BE,d';
    flags   : 0
  ),
  (
    opcode  : A_CP;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IY_d);
    code    : '$FD,$BE,d';
    flags   : 0
  ),
  (
    opcode  : A_CPD;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$A9';
    flags   : 0
  ),
  (
    opcode  : A_CPDR;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$B9';
    flags   : 0
  ),
  (
    opcode  : A_CPI;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$A1';
    flags   : 0
  ),
  (
    opcode  : A_CPIR;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$B1';
    flags   : 0
  ),
  (
    opcode  : A_CPL;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$2F';
    flags   : 0
  ),
  (
    opcode  : A_DAA;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$27';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_REG8,OT_NONE);
    code    : '%00rrr101';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_REF_HL,OT_NONE);
    code    : '$35';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_REF_IX_d,OT_NONE);
    code    : '$DD,$35,d';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_REF_IY_d,OT_NONE);
    code    : '$FD,$35,d';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_REG16_BC_DE_HL_SP,OT_NONE);
    code    : '%00dd1011';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_REG16_IX,OT_NONE);
    code    : '$DD,$2B';
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    ops     : 1;
    optypes : (OT_REG16_IY,OT_NONE);
    code    : '$FD,$2B';
    flags   : 0
  ),
  (
    opcode  : A_DI;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$F3';
    flags   : 0
  ),
  (
    opcode  : A_DJNZ;
    ops     : 1;
    optypes : (OT_RELJMP8,OT_NONE);
    code    : '$10,e';
    flags   : 0
  ),
  (
    opcode  : A_EI;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$FB';
    flags   : 0
  ),
  (
    opcode  : A_EX;
    ops     : 2;
    optypes : (OT_REG16_DE,OT_REG16_HL);
    code    : '$EB';
    flags   : 0
  ),
  (
    opcode  : A_EX;
    ops     : 2;
    optypes : (OT_REG16_AF,OT_REG16_AF_);
    code    : '$08';
    flags   : 0
  ),
  (
    opcode  : A_EX;
    ops     : 2;
    optypes : (OT_REF_SP,OT_REG16_HL);
    code    : '$E3';
    flags   : 0
  ),
  (
    opcode  : A_EX;
    ops     : 2;
    optypes : (OT_REF_SP,OT_REG16_IX);
    code    : '$DD,$E3';
    flags   : 0
  ),
  (
    opcode  : A_EX;
    ops     : 2;
    optypes : (OT_REF_SP,OT_REG16_IY);
    code    : '$FD,$E3';
    flags   : 0
  ),
  (
    opcode  : A_EXX;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$D9';
    flags   : 0
  ),
  (
    opcode  : A_HALT;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$76';
    flags   : 0
  ),
  (
    opcode  : A_IM;
    ops     : 1;
    optypes : (OT_IMM_VAL0,OT_NONE);
    code    : '$ED,$46';
    flags   : 0
  ),
  (
    opcode  : A_IM;
    ops     : 1;
    optypes : (OT_IMM_VAL1,OT_NONE);
    code    : '$ED,$56';
    flags   : 0
  ),
  (
    opcode  : A_IM;
    ops     : 1;
    optypes : (OT_IMM_VAL2,OT_NONE);
    code    : '$ED,$5E';
    flags   : 0
  ),
  (
    opcode  : A_IN;
    ops     : 2;
    optypes : (OT_REG8_A,OT_IMM_PORT);
    code    : '$DB,n';
    flags   : 0
  ),
  (
    opcode  : A_IN;
    ops     : 2;
    optypes : (OT_REG8,OT_REG8_C_PORT);
    code    : '$ED,%01rrr000';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_REG8,OT_NONE);
    code    : '%00rrr100';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_REF_HL,OT_NONE);
    code    : '$34';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_REF_IX_d,OT_NONE);
    code    : '$DD,$34,d';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_REF_IY_d,OT_NONE);
    code    : '$FD,$34,d';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_REG16_BC_DE_HL_SP,OT_NONE);
    code    : '%00dd0011';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_REG16_IX,OT_NONE);
    code    : '$DD,$23';
    flags   : 0
  ),
  (
    opcode  : A_INC;
    ops     : 1;
    optypes : (OT_REG16_IY,OT_NONE);
    code    : '$FD,$23';
    flags   : 0
  ),
  (
    opcode  : A_IND;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$AA';
    flags   : 0
  ),
  (
    opcode  : A_INDR;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$BA';
    flags   : 0
  ),
  (
    opcode  : A_INI;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$A2';
    flags   : 0
  ),
  (
    opcode  : A_INIR;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$B2';
    flags   : 0
  ),
  (
    opcode  : A_JP;
    ops     : 1;
    optypes : (OT_IMM16,OT_NONE);
    code    : '$C3,nn';
    flags   : 0
  ),
  (
    opcode  : A_JP;
    ops     : 2;
    optypes : (OT_COND,OT_IMM16);
    code    : '%11ccc010,nn';
    flags   : 0
  ),
  (
    opcode  : A_JP;
    ops     : 1;
    optypes : (OT_REF_HL,OT_NONE);
    code    : '$E9';
    flags   : 0
  ),
  (
    opcode  : A_JP;
    ops     : 1;
    optypes : (OT_REF_IX,OT_NONE);
    code    : '$DD,$E9';
    flags   : 0
  ),
  (
    opcode  : A_JP;
    ops     : 1;
    optypes : (OT_REF_IY,OT_NONE);
    code    : '$FD,$E9';
    flags   : 0
  ),
  (
    opcode  : A_JR;
    ops     : 1;
    optypes : (OT_RELJMP8,OT_NONE);
    code    : '$18,e';
    flags   : 0
  ),
  (
    opcode  : A_JR;
    ops     : 2;
    optypes : (OT_COND_C,OT_RELJMP8);
    code    : '$38,e';
    flags   : 0
  ),
  (
    opcode  : A_JR;
    ops     : 2;
    optypes : (OT_COND_NC,OT_RELJMP8);
    code    : '$30,e';
    flags   : 0
  ),
  (
    opcode  : A_JR;
    ops     : 2;
    optypes : (OT_COND_Z,OT_RELJMP8);
    code    : '$28,e';
    flags   : 0
  ),
  (
    opcode  : A_JR;
    ops     : 2;
    optypes : (OT_COND_NZ,OT_RELJMP8);
    code    : '$20,e';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8,OT_REG8);
    code    : '%01rrrr''r''r''';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8,OT_IMM8);
    code    : '%00rrr110,n';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8,OT_REF_HL);
    code    : '%01rrr110';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8,OT_REF_IX_d);
    code    : '$DD,%01rrr110,d';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8,OT_REF_IY_d);
    code    : '$FD,%01rrr110,d';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_HL,OT_REG8);
    code    : '%01110rrr';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_IX_d,OT_REG8);
    code    : '$DD,%01110rrr,d';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_IY_d,OT_REG8);
    code    : '$FD,%01110rrr,d';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_HL,OT_IMM8);
    code    : '$36,n';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_IX_d,OT_IMM8);
    code    : '$DD,$36,d,n';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_IY_d,OT_IMM8);
    code    : '$FD,$36,d,n';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_BC);
    code    : '$0A';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_DE);
    code    : '$1A';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_ADDR16);
    code    : '$3A,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_BC,OT_REG8_A);
    code    : '$02';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_DE,OT_REG8_A);
    code    : '$12';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_ADDR16,OT_REG8_A);
    code    : '$32,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REG8_I);
    code    : '$ED,$57';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REG8_R);
    code    : '$ED,$5F';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8_I,OT_REG8_A);
    code    : '$ED,$47';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG8_R,OT_REG8_A);
    code    : '$ED,$4F';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG16_BC_DE_HL_SP,OT_IMM16);
    code    : '%00dd0001,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG16_IX,OT_IMM16);
    code    : '$DD,$21,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG16_IY,OT_IMM16);
    code    : '$FD,$21,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG16_HL,OT_REF_ADDR16);
    code    : '$2A,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG16_BC_DE_HL_SP,OT_REF_ADDR16);
    code    : '$ED,%01dd1011,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG16_IX,OT_REF_ADDR16);
    code    : '$DD,$2A,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG16_IY,OT_REF_ADDR16);
    code    : '$FD,$2A,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_ADDR16,OT_REG16_HL);
    code    : '$22,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_ADDR16,OT_REG16_BC_DE_HL_SP);
    code    : '$ED,%01dd0011,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_ADDR16,OT_REG16_IX);
    code    : '$DD,$22,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REF_ADDR16,OT_REG16_IY);
    code    : '$FD,$22,nn';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG16_SP,OT_REG16_HL);
    code    : '$F9';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG16_SP,OT_REG16_IX);
    code    : '$DD,$F9';
    flags   : 0
  ),
  (
    opcode  : A_LD;
    ops     : 2;
    optypes : (OT_REG16_SP,OT_REG16_IY);
    code    : '$FD,$F9';
    flags   : 0
  ),
  (
    opcode  : A_LDD;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$A8';
    flags   : 0
  ),
  (
    opcode  : A_LDDR;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$B8';
    flags   : 0
  ),
  (
    opcode  : A_LDI;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$A0';
    flags   : 0
  ),
  (
    opcode  : A_LDIR;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$B0';
    flags   : 0
  ),
  (
    opcode  : A_NEG;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$44';
    flags   : 0
  ),
  (
    opcode  : A_NOP;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$00';
    flags   : 0
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REG8);
    code    : '%10110rrr';
    flags   : 0
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : (OT_REG8_A,OT_IMM8);
    code    : '$F6,n';
    flags   : 0
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_HL);
    code    : '$B6';
    flags   : 0
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IX_d);
    code    : '$DD,$B6,d';
    flags   : 0
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IY_d);
    code    : '$FD,$B6,d';
    flags   : 0
  ),
  (
    opcode  : A_OTDR;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$BB';
    flags   : 0
  ),
  (
    opcode  : A_OTIR;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$B3';
    flags   : 0
  ),
  (
    opcode  : A_OUT;
    ops     : 2;
    optypes : (OT_IMM_PORT,OT_REG8_A);
    code    : '$D3,n';
    flags   : 0
  ),
  (
    opcode  : A_OUT;
    ops     : 2;
    optypes : (OT_REG8_C_PORT,OT_REG8);
    code    : '$ED,%01rrr001';
    flags   : 0
  ),
  (
    opcode  : A_OUTD;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$AB';
    flags   : 0
  ),
  (
    opcode  : A_OUTI;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$A3';
    flags   : 0
  ),
  (
    opcode  : A_POP;
    ops     : 1;
    optypes : (OT_REG16_BC_DE_HL_AF,OT_NONE);
    code    : '%11qq0001';
    flags   : 0
  ),
  (
    opcode  : A_POP;
    ops     : 1;
    optypes : (OT_REG16_IX,OT_NONE);
    code    : '$DD,$E1';
    flags   : 0
  ),
  (
    opcode  : A_POP;
    ops     : 1;
    optypes : (OT_REG16_IY,OT_NONE);
    code    : '$FD,$E1';
    flags   : 0
  ),
  (
    opcode  : A_PUSH;
    ops     : 1;
    optypes : (OT_REG16_BC_DE_HL_AF,OT_NONE);
    code    : '%11qq0101';
    flags   : 0
  ),
  (
    opcode  : A_PUSH;
    ops     : 1;
    optypes : (OT_REG16_IX,OT_NONE);
    code    : '$DD,$E5';
    flags   : 0
  ),
  (
    opcode  : A_PUSH;
    ops     : 1;
    optypes : (OT_REG16_IY,OT_NONE);
    code    : '$FD,$E5';
    flags   : 0
  ),
  (
    opcode  : A_RES;
    ops     : 2;
    optypes : (OT_IMM3,OT_REG8);
    code    : '$CB,%10bbbrrr';
    flags   : 0
  ),
  (
    opcode  : A_RES;
    ops     : 2;
    optypes : (OT_IMM3,OT_REF_HL);
    code    : '$CB,%10bbb110';
    flags   : 0
  ),
  (
    opcode  : A_RES;
    ops     : 2;
    optypes : (OT_IMM3,OT_REF_IX_d);
    code    : '$DD,$CB,d,%10bbb110';
    flags   : 0
  ),
  (
    opcode  : A_RES;
    ops     : 2;
    optypes : (OT_IMM3,OT_REF_IY_d);
    code    : '$FD,$CB,d,%10bbb110';
    flags   : 0
  ),
  (
    opcode  : A_RET;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$C9';
    flags   : 0
  ),
  (
    opcode  : A_RET;
    ops     : 1;
    optypes : (OT_COND,OT_NONE);
    code    : '%11ccc000';
    flags   : 0
  ),
  (
    opcode  : A_RETI;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$4D';
    flags   : 0
  ),
  (
    opcode  : A_RETN;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$45';
    flags   : 0
  ),
  (
    opcode  : A_RL;
    ops     : 1;
    optypes : (OT_REG8,OT_NONE);
    code    : '$CB,%00010rrr';
    flags   : 0
  ),
  (
    opcode  : A_RL;
    ops     : 1;
    optypes : (OT_REF_HL,OT_NONE);
    code    : '$CB,$16';
    flags   : 0
  ),
  (
    opcode  : A_RL;
    ops     : 1;
    optypes : (OT_REF_IX_d,OT_NONE);
    code    : '$DD,$CB,d,$16';
    flags   : 0
  ),
  (
    opcode  : A_RL;
    ops     : 1;
    optypes : (OT_REF_IY_d,OT_NONE);
    code    : '$FD,$CB,d,$16';
    flags   : 0
  ),
  (
    opcode  : A_RLA;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$17';
    flags   : 0
  ),
  (
    opcode  : A_RLC;
    ops     : 1;
    optypes : (OT_REG8,OT_NONE);
    code    : '$CB,%00000rrr';
    flags   : 0
  ),
  (
    opcode  : A_RLC;
    ops     : 1;
    optypes : (OT_REF_HL,OT_NONE);
    code    : '$CB,$06';
    flags   : 0
  ),
  (
    opcode  : A_RLC;
    ops     : 1;
    optypes : (OT_REF_IX_d,OT_NONE);
    code    : '$DD,$CB,d,$06';
    flags   : 0
  ),
  (
    opcode  : A_RLC;
    ops     : 1;
    optypes : (OT_REF_IY_d,OT_NONE);
    code    : '$FD,$CB,d,$06';
    flags   : 0
  ),
  (
    opcode  : A_RLCA;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$07';
    flags   : 0
  ),
  (
    opcode  : A_RLD;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$6F';
    flags   : 0
  ),
  (
    opcode  : A_RR;
    ops     : 1;
    optypes : (OT_REG8,OT_NONE);
    code    : '$CB,%00001rrr';
    flags   : 0
  ),
  (
    opcode  : A_RR;
    ops     : 1;
    optypes : (OT_REF_HL,OT_NONE);
    code    : '$CB,$1E';
    flags   : 0
  ),
  (
    opcode  : A_RR;
    ops     : 1;
    optypes : (OT_REF_IX_d,OT_NONE);
    code    : '$DD,$CB,d,$1E';
    flags   : 0
  ),
  (
    opcode  : A_RR;
    ops     : 1;
    optypes : (OT_REF_IY_d,OT_NONE);
    code    : '$FD,$CB,d,$1E';
    flags   : 0
  ),
  (
    opcode  : A_RRA;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$1F';
    flags   : 0
  ),
  (
    opcode  : A_RRC;
    ops     : 1;
    optypes : (OT_REG8,OT_NONE);
    code    : '$CB,%00001rrr';
    flags   : 0
  ),
  (
    opcode  : A_RRC;
    ops     : 1;
    optypes : (OT_REF_HL,OT_NONE);
    code    : '$CB,$0E';
    flags   : 0
  ),
  (
    opcode  : A_RRC;
    ops     : 1;
    optypes : (OT_REF_IX_d,OT_NONE);
    code    : '$DD,$CB,d,$0E';
    flags   : 0
  ),
  (
    opcode  : A_RRC;
    ops     : 1;
    optypes : (OT_REF_IY_d,OT_NONE);
    code    : '$FD,$CB,d,$0E';
    flags   : 0
  ),
  (
    opcode  : A_RRCA;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$0F';
    flags   : 0
  ),
  (
    opcode  : A_RRD;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$ED,$67';
    flags   : 0
  ),
  (
    opcode  : A_RST;
    ops     : 1;
    optypes : (OT_IMM_RST,OT_NONE);
    code    : '%11ppp111';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REG8);
    code    : '%10011rrr';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 2;
    optypes : (OT_REG8_A,OT_IMM8);
    code    : '$DE,n';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_HL);
    code    : '$9E';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IX_d);
    code    : '$DD,$9E,d';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IY_d);
    code    : '$FD,$9E,d';
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    ops     : 2;
    optypes : (OT_REG16_HL,OT_REG16_BC_DE_HL_SP);
    code    : '$ED,%01dd0010';
    flags   : 0
  ),
  (
    opcode  : A_SCF;
    ops     : 0;
    optypes : (OT_NONE,OT_NONE);
    code    : '$37';
    flags   : 0
  ),
  (
    opcode  : A_SET;
    ops     : 2;
    optypes : (OT_IMM3,OT_REG8);
    code    : '$CB,%11bbbrrr';
    flags   : 0
  ),
  (
    opcode  : A_SET;
    ops     : 2;
    optypes : (OT_IMM3,OT_REF_HL);
    code    : '$CB,%11bbb110';
    flags   : 0
  ),
  (
    opcode  : A_SET;
    ops     : 2;
    optypes : (OT_IMM3,OT_REF_IX_d);
    code    : '$DD,$CB,d,%11bbb110';
    flags   : 0
  ),
  (
    opcode  : A_SET;
    ops     : 2;
    optypes : (OT_IMM3,OT_REF_IY_d);
    code    : '$FD,$CB,d,%11bbb110';
    flags   : 0
  ),
  (
    opcode  : A_SLA;
    ops     : 1;
    optypes : (OT_REG8,OT_NONE);
    code    : '$CB,%00100rrr';
    flags   : 0
  ),
  (
    opcode  : A_SLA;
    ops     : 1;
    optypes : (OT_REF_HL,OT_NONE);
    code    : '$CB,$26';
    flags   : 0
  ),
  (
    opcode  : A_SLA;
    ops     : 1;
    optypes : (OT_REF_IX_d,OT_NONE);
    code    : '$DD,$CB,d,$26';
    flags   : 0
  ),
  (
    opcode  : A_SLA;
    ops     : 1;
    optypes : (OT_REF_IY_d,OT_NONE);
    code    : '$FD,$CB,d,$26';
    flags   : 0
  ),
  (
    opcode  : A_SRA;
    ops     : 1;
    optypes : (OT_REG8,OT_NONE);
    code    : '$CB,%00101rrr';
    flags   : 0
  ),
  (
    opcode  : A_SRA;
    ops     : 1;
    optypes : (OT_REF_HL,OT_NONE);
    code    : '$CB,$2E';
    flags   : 0
  ),
  (
    opcode  : A_SRA;
    ops     : 1;
    optypes : (OT_REF_IX_d,OT_NONE);
    code    : '$DD,$CB,d,$2E';
    flags   : 0
  ),
  (
    opcode  : A_SRA;
    ops     : 1;
    optypes : (OT_REF_IY_d,OT_NONE);
    code    : '$FD,$CB,d,$2E';
    flags   : 0
  ),
  (
    opcode  : A_SRL;
    ops     : 1;
    optypes : (OT_REG8,OT_NONE);
    code    : '$CB,%00111rrr';
    flags   : 0
  ),
  (
    opcode  : A_SRL;
    ops     : 1;
    optypes : (OT_REF_HL,OT_NONE);
    code    : '$CB,$3E';
    flags   : 0
  ),
  (
    opcode  : A_SRL;
    ops     : 1;
    optypes : (OT_REF_IX_d,OT_NONE);
    code    : '$DD,$CB,d,$3E';
    flags   : 0
  ),
  (
    opcode  : A_SRL;
    ops     : 1;
    optypes : (OT_REF_IY_d,OT_NONE);
    code    : '$FD,$CB,d,$3E';
    flags   : 0
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REG8);
    code    : '%10010rrr';
    flags   : 0
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : (OT_REG8_A,OT_IMM8);
    code    : '$D6,n';
    flags   : 0
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_HL);
    code    : '$96';
    flags   : 0
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IX_d);
    code    : '$DD,$96,d';
    flags   : 0
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IY_d);
    code    : '$FD,$96,d';
    flags   : 0
  ),
  (
    opcode  : A_XOR;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REG8);
    code    : '%10101rrr';
    flags   : 0
  ),
  (
    opcode  : A_XOR;
    ops     : 2;
    optypes : (OT_REG8_A,OT_IMM8);
    code    : '$EE,n';
    flags   : 0
  ),
  (
    opcode  : A_XOR;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_HL);
    code    : '$AE';
    flags   : 0
  ),
  (
    opcode  : A_XOR;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IX_d);
    code    : '$DD,$AE,d';
    flags   : 0
  ),
  (
    opcode  : A_XOR;
    ops     : 2;
    optypes : (OT_REG8_A,OT_REF_IY_d);
    code    : '$FD,$AE,d';
    flags   : 0
  )
);
