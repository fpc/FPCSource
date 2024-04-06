{ don't edit, this file is generated from mos6502ins.dat; to regenerate, run 'make insdat' in the compiler directory }
(
  (
    opcode  : A_None;
    adrmode : AM_IMPLICIT;
    code    : 0;
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    adrmode : AM_INDEXED_INDIRECT;
    code    : $61;
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    adrmode : AM_ZERO_PAGE;
    code    : $65;
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    adrmode : AM_IMMEDIATE;
    code    : $69;
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    adrmode : AM_ABSOLUTE;
    code    : $6D;
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    adrmode : AM_INDIRECT_INDEXED;
    code    : $71;
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    adrmode : AM_ZERO_PAGE_X;
    code    : $75;
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    adrmode : AM_ABSOLUTE_Y;
    code    : $79;
    flags   : 0
  ),
  (
    opcode  : A_ADC;
    adrmode : AM_ABSOLUTE_X;
    code    : $7D;
    flags   : 0
  ),
  (
    opcode  : A_AND;
    adrmode : AM_INDEXED_INDIRECT;
    code    : $21;
    flags   : 0
  ),
  (
    opcode  : A_AND;
    adrmode : AM_ZERO_PAGE;
    code    : $25;
    flags   : 0
  ),
  (
    opcode  : A_AND;
    adrmode : AM_IMMEDIATE;
    code    : $29;
    flags   : 0
  ),
  (
    opcode  : A_AND;
    adrmode : AM_ABSOLUTE;
    code    : $2D;
    flags   : 0
  ),
  (
    opcode  : A_AND;
    adrmode : AM_INDIRECT_INDEXED;
    code    : $31;
    flags   : 0
  ),
  (
    opcode  : A_AND;
    adrmode : AM_ZERO_PAGE_X;
    code    : $35;
    flags   : 0
  ),
  (
    opcode  : A_AND;
    adrmode : AM_ABSOLUTE_Y;
    code    : $39;
    flags   : 0
  ),
  (
    opcode  : A_AND;
    adrmode : AM_ABSOLUTE_X;
    code    : $3D;
    flags   : 0
  ),
  (
    opcode  : A_ASL;
    adrmode : AM_ZERO_PAGE;
    code    : $06;
    flags   : 0
  ),
  (
    opcode  : A_ASL;
    adrmode : AM_ACCUMULATOR;
    code    : $0A;
    flags   : 0
  ),
  (
    opcode  : A_ASL;
    adrmode : AM_ABSOLUTE;
    code    : $0E;
    flags   : 0
  ),
  (
    opcode  : A_ASL;
    adrmode : AM_ZERO_PAGE_X;
    code    : $16;
    flags   : 0
  ),
  (
    opcode  : A_ASL;
    adrmode : AM_ABSOLUTE_X;
    code    : $1E;
    flags   : 0
  ),
  (
    opcode  : A_BCC;
    adrmode : AM_RELATIVE;
    code    : $90;
    flags   : 0
  ),
  (
    opcode  : A_BCS;
    adrmode : AM_RELATIVE;
    code    : $B0;
    flags   : 0
  ),
  (
    opcode  : A_BEQ;
    adrmode : AM_RELATIVE;
    code    : $F0;
    flags   : 0
  ),
  (
    opcode  : A_BIT;
    adrmode : AM_ZERO_PAGE;
    code    : $24;
    flags   : 0
  ),
  (
    opcode  : A_BIT;
    adrmode : AM_ABSOLUTE;
    code    : $2C;
    flags   : 0
  ),
  (
    opcode  : A_BMI;
    adrmode : AM_RELATIVE;
    code    : $30;
    flags   : 0
  ),
  (
    opcode  : A_BNE;
    adrmode : AM_RELATIVE;
    code    : $D0;
    flags   : 0
  ),
  (
    opcode  : A_BPL;
    adrmode : AM_RELATIVE;
    code    : $10;
    flags   : 0
  ),
  (
    opcode  : A_BRK;
    adrmode : AM_IMPLICIT;
    code    : $00;
    flags   : 0
  ),
  (
    opcode  : A_BVC;
    adrmode : AM_RELATIVE;
    code    : $50;
    flags   : 0
  ),
  (
    opcode  : A_BVS;
    adrmode : AM_RELATIVE;
    code    : $70;
    flags   : 0
  ),
  (
    opcode  : A_CLC;
    adrmode : AM_IMPLICIT;
    code    : $18;
    flags   : 0
  ),
  (
    opcode  : A_CLD;
    adrmode : AM_IMPLICIT;
    code    : $D8;
    flags   : 0
  ),
  (
    opcode  : A_CLI;
    adrmode : AM_IMPLICIT;
    code    : $58;
    flags   : 0
  ),
  (
    opcode  : A_CLV;
    adrmode : AM_IMPLICIT;
    code    : $B8;
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    adrmode : AM_INDEXED_INDIRECT;
    code    : $C1;
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    adrmode : AM_ZERO_PAGE;
    code    : $C5;
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    adrmode : AM_IMMEDIATE;
    code    : $C9;
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    adrmode : AM_ABSOLUTE;
    code    : $CD;
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    adrmode : AM_INDIRECT_INDEXED;
    code    : $D1;
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    adrmode : AM_ZERO_PAGE_X;
    code    : $D5;
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    adrmode : AM_ABSOLUTE_Y;
    code    : $D9;
    flags   : 0
  ),
  (
    opcode  : A_CMP;
    adrmode : AM_ABSOLUTE_X;
    code    : $DD;
    flags   : 0
  ),
  (
    opcode  : A_CPX;
    adrmode : AM_IMMEDIATE;
    code    : $E0;
    flags   : 0
  ),
  (
    opcode  : A_CPX;
    adrmode : AM_ZERO_PAGE;
    code    : $E4;
    flags   : 0
  ),
  (
    opcode  : A_CPX;
    adrmode : AM_ABSOLUTE;
    code    : $EC;
    flags   : 0
  ),
  (
    opcode  : A_CPY;
    adrmode : AM_IMMEDIATE;
    code    : $C0;
    flags   : 0
  ),
  (
    opcode  : A_CPY;
    adrmode : AM_ZERO_PAGE;
    code    : $C4;
    flags   : 0
  ),
  (
    opcode  : A_CPY;
    adrmode : AM_ABSOLUTE;
    code    : $CC;
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    adrmode : AM_ZERO_PAGE;
    code    : $C6;
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    adrmode : AM_ABSOLUTE;
    code    : $CE;
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    adrmode : AM_ZERO_PAGE_X;
    code    : $D6;
    flags   : 0
  ),
  (
    opcode  : A_DEC;
    adrmode : AM_ABSOLUTE_X;
    code    : $DE;
    flags   : 0
  ),
  (
    opcode  : A_DEX;
    adrmode : AM_IMPLICIT;
    code    : $CA;
    flags   : 0
  ),
  (
    opcode  : A_DEY;
    adrmode : AM_IMPLICIT;
    code    : $88;
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    adrmode : AM_INDEXED_INDIRECT;
    code    : $41;
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    adrmode : AM_ZERO_PAGE;
    code    : $45;
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    adrmode : AM_IMMEDIATE;
    code    : $49;
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    adrmode : AM_ABSOLUTE;
    code    : $4D;
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    adrmode : AM_INDIRECT_INDEXED;
    code    : $51;
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    adrmode : AM_ZERO_PAGE_X;
    code    : $55;
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    adrmode : AM_ABSOLUTE_Y;
    code    : $59;
    flags   : 0
  ),
  (
    opcode  : A_EOR;
    adrmode : AM_ABSOLUTE_X;
    code    : $5D;
    flags   : 0
  ),
  (
    opcode  : A_INC;
    adrmode : AM_ZERO_PAGE;
    code    : $E6;
    flags   : 0
  ),
  (
    opcode  : A_INC;
    adrmode : AM_ABSOLUTE;
    code    : $EE;
    flags   : 0
  ),
  (
    opcode  : A_INC;
    adrmode : AM_ZERO_PAGE_X;
    code    : $F6;
    flags   : 0
  ),
  (
    opcode  : A_INC;
    adrmode : AM_ABSOLUTE_X;
    code    : $FE;
    flags   : 0
  ),
  (
    opcode  : A_INX;
    adrmode : AM_IMPLICIT;
    code    : $E8;
    flags   : 0
  ),
  (
    opcode  : A_INY;
    adrmode : AM_IMPLICIT;
    code    : $C8;
    flags   : 0
  ),
  (
    opcode  : A_JMP;
    adrmode : AM_ABSOLUTE;
    code    : $4C;
    flags   : 0
  ),
  (
    opcode  : A_JMP;
    adrmode : AM_INDIRECT;
    code    : $6C;
    flags   : 0
  ),
  (
    opcode  : A_JSR;
    adrmode : AM_ABSOLUTE;
    code    : $20;
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    adrmode : AM_INDEXED_INDIRECT;
    code    : $A1;
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    adrmode : AM_ZERO_PAGE;
    code    : $A5;
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    adrmode : AM_IMMEDIATE;
    code    : $A9;
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    adrmode : AM_ABSOLUTE;
    code    : $AD;
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    adrmode : AM_INDIRECT_INDEXED;
    code    : $B1;
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    adrmode : AM_ZERO_PAGE_X;
    code    : $B5;
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    adrmode : AM_ABSOLUTE_Y;
    code    : $B9;
    flags   : 0
  ),
  (
    opcode  : A_LDA;
    adrmode : AM_ABSOLUTE_X;
    code    : $BD;
    flags   : 0
  ),
  (
    opcode  : A_LDX;
    adrmode : AM_IMMEDIATE;
    code    : $A2;
    flags   : 0
  ),
  (
    opcode  : A_LDX;
    adrmode : AM_ZERO_PAGE;
    code    : $A6;
    flags   : 0
  ),
  (
    opcode  : A_LDX;
    adrmode : AM_ABSOLUTE;
    code    : $AE;
    flags   : 0
  ),
  (
    opcode  : A_LDX;
    adrmode : AM_ZERO_PAGE_Y;
    code    : $B6;
    flags   : 0
  ),
  (
    opcode  : A_LDX;
    adrmode : AM_ABSOLUTE_Y;
    code    : $BE;
    flags   : 0
  ),
  (
    opcode  : A_LDY;
    adrmode : AM_IMMEDIATE;
    code    : $A0;
    flags   : 0
  ),
  (
    opcode  : A_LDY;
    adrmode : AM_ZERO_PAGE;
    code    : $A4;
    flags   : 0
  ),
  (
    opcode  : A_LDY;
    adrmode : AM_ABSOLUTE;
    code    : $AC;
    flags   : 0
  ),
  (
    opcode  : A_LDY;
    adrmode : AM_ZERO_PAGE_X;
    code    : $B4;
    flags   : 0
  ),
  (
    opcode  : A_LDY;
    adrmode : AM_ABSOLUTE_X;
    code    : $BC;
    flags   : 0
  ),
  (
    opcode  : A_LSR;
    adrmode : AM_ZERO_PAGE;
    code    : $46;
    flags   : 0
  ),
  (
    opcode  : A_LSR;
    adrmode : AM_ACCUMULATOR;
    code    : $4A;
    flags   : 0
  ),
  (
    opcode  : A_LSR;
    adrmode : AM_ABSOLUTE;
    code    : $4E;
    flags   : 0
  ),
  (
    opcode  : A_LSR;
    adrmode : AM_ZERO_PAGE_X;
    code    : $56;
    flags   : 0
  ),
  (
    opcode  : A_LSR;
    adrmode : AM_ABSOLUTE_X;
    code    : $5E;
    flags   : 0
  ),
  (
    opcode  : A_NOP;
    adrmode : AM_IMPLICIT;
    code    : $EA;
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    adrmode : AM_INDEXED_INDIRECT;
    code    : $01;
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    adrmode : AM_ZERO_PAGE;
    code    : $05;
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    adrmode : AM_IMMEDIATE;
    code    : $09;
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    adrmode : AM_ABSOLUTE;
    code    : $0D;
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    adrmode : AM_INDIRECT_INDEXED;
    code    : $11;
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    adrmode : AM_ZERO_PAGE_X;
    code    : $15;
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    adrmode : AM_ABSOLUTE_Y;
    code    : $19;
    flags   : 0
  ),
  (
    opcode  : A_ORA;
    adrmode : AM_ABSOLUTE_X;
    code    : $1D;
    flags   : 0
  ),
  (
    opcode  : A_PHA;
    adrmode : AM_IMPLICIT;
    code    : $48;
    flags   : 0
  ),
  (
    opcode  : A_PHP;
    adrmode : AM_IMPLICIT;
    code    : $08;
    flags   : 0
  ),
  (
    opcode  : A_PLA;
    adrmode : AM_IMPLICIT;
    code    : $68;
    flags   : 0
  ),
  (
    opcode  : A_PLP;
    adrmode : AM_IMPLICIT;
    code    : $28;
    flags   : 0
  ),
  (
    opcode  : A_ROL;
    adrmode : AM_ZERO_PAGE;
    code    : $26;
    flags   : 0
  ),
  (
    opcode  : A_ROL;
    adrmode : AM_ACCUMULATOR;
    code    : $2A;
    flags   : 0
  ),
  (
    opcode  : A_ROL;
    adrmode : AM_ABSOLUTE;
    code    : $2E;
    flags   : 0
  ),
  (
    opcode  : A_ROL;
    adrmode : AM_ZERO_PAGE_X;
    code    : $36;
    flags   : 0
  ),
  (
    opcode  : A_ROL;
    adrmode : AM_ABSOLUTE_X;
    code    : $3E;
    flags   : 0
  ),
  (
    opcode  : A_ROR;
    adrmode : AM_ZERO_PAGE;
    code    : $66;
    flags   : 0
  ),
  (
    opcode  : A_ROR;
    adrmode : AM_ACCUMULATOR;
    code    : $6A;
    flags   : 0
  ),
  (
    opcode  : A_ROR;
    adrmode : AM_ABSOLUTE;
    code    : $6E;
    flags   : 0
  ),
  (
    opcode  : A_ROR;
    adrmode : AM_ZERO_PAGE_X;
    code    : $76;
    flags   : 0
  ),
  (
    opcode  : A_ROR;
    adrmode : AM_ABSOLUTE_X;
    code    : $7E;
    flags   : 0
  ),
  (
    opcode  : A_RTI;
    adrmode : AM_IMPLICIT;
    code    : $40;
    flags   : 0
  ),
  (
    opcode  : A_RTS;
    adrmode : AM_IMPLICIT;
    code    : $60;
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    adrmode : AM_INDEXED_INDIRECT;
    code    : $E1;
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    adrmode : AM_ZERO_PAGE;
    code    : $E5;
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    adrmode : AM_IMMEDIATE;
    code    : $E9;
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    adrmode : AM_ABSOLUTE;
    code    : $ED;
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    adrmode : AM_INDIRECT_INDEXED;
    code    : $F1;
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    adrmode : AM_ZERO_PAGE_X;
    code    : $F5;
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    adrmode : AM_ABSOLUTE_Y;
    code    : $F9;
    flags   : 0
  ),
  (
    opcode  : A_SBC;
    adrmode : AM_ABSOLUTE_X;
    code    : $FD;
    flags   : 0
  ),
  (
    opcode  : A_SEC;
    adrmode : AM_IMPLICIT;
    code    : $38;
    flags   : 0
  ),
  (
    opcode  : A_SED;
    adrmode : AM_IMPLICIT;
    code    : $F8;
    flags   : 0
  ),
  (
    opcode  : A_SEI;
    adrmode : AM_IMPLICIT;
    code    : $78;
    flags   : 0
  ),
  (
    opcode  : A_STA;
    adrmode : AM_INDEXED_INDIRECT;
    code    : $81;
    flags   : 0
  ),
  (
    opcode  : A_STA;
    adrmode : AM_ZERO_PAGE;
    code    : $85;
    flags   : 0
  ),
  (
    opcode  : A_STA;
    adrmode : AM_ABSOLUTE;
    code    : $8D;
    flags   : 0
  ),
  (
    opcode  : A_STA;
    adrmode : AM_INDIRECT_INDEXED;
    code    : $91;
    flags   : 0
  ),
  (
    opcode  : A_STA;
    adrmode : AM_ZERO_PAGE_X;
    code    : $95;
    flags   : 0
  ),
  (
    opcode  : A_STA;
    adrmode : AM_ABSOLUTE_Y;
    code    : $99;
    flags   : 0
  ),
  (
    opcode  : A_STA;
    adrmode : AM_ABSOLUTE_X;
    code    : $9D;
    flags   : 0
  ),
  (
    opcode  : A_STX;
    adrmode : AM_ZERO_PAGE;
    code    : $86;
    flags   : 0
  ),
  (
    opcode  : A_STX;
    adrmode : AM_ABSOLUTE;
    code    : $8E;
    flags   : 0
  ),
  (
    opcode  : A_STX;
    adrmode : AM_ZERO_PAGE_Y;
    code    : $96;
    flags   : 0
  ),
  (
    opcode  : A_STY;
    adrmode : AM_ZERO_PAGE;
    code    : $84;
    flags   : 0
  ),
  (
    opcode  : A_STY;
    adrmode : AM_ABSOLUTE;
    code    : $8C;
    flags   : 0
  ),
  (
    opcode  : A_STY;
    adrmode : AM_ZERO_PAGE_X;
    code    : $94;
    flags   : 0
  ),
  (
    opcode  : A_TAX;
    adrmode : AM_IMPLICIT;
    code    : $AA;
    flags   : 0
  ),
  (
    opcode  : A_TAY;
    adrmode : AM_IMPLICIT;
    code    : $A8;
    flags   : 0
  ),
  (
    opcode  : A_TSX;
    adrmode : AM_IMPLICIT;
    code    : $BA;
    flags   : 0
  ),
  (
    opcode  : A_TXA;
    adrmode : AM_IMPLICIT;
    code    : $8A;
    flags   : 0
  ),
  (
    opcode  : A_TXS;
    adrmode : AM_IMPLICIT;
    code    : $9A;
    flags   : 0
  ),
  (
    opcode  : A_TYA;
    adrmode : AM_IMPLICIT;
    code    : $98;
    flags   : 0
  )
);
