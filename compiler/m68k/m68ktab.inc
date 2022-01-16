{ don't edit, this file is generated from m68kins.dat; to regenerate, run 'make insdat' in the compiler directory }
(
  (
    opcode  : A_NONE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 0;
    code    : ($0000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_ABCD;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c100,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_ABCD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR_PREDEC],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c108,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($d000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($d000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($d100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($d0c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ADD;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0600,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ADDA;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($d0c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ADDI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0600,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ADDI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0600,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ADDQ;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($5000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ADDQ;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($5000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ADDX;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($d100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ADDX;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR_PREDEC],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($d108,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0200,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($023c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_AND;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($027c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_ANDI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0200,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ANDI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0200,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ANDI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($023c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_ANDI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($027c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_ASL;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e1c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_ASL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e120,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ASL;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($e100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ASL;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e300,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ASR;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e0c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_ASR;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e020,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ASR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($e000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ASR;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e200,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_BHS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BHS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BLO;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BLO;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BHI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6200,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BHI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6200,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BLS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6300,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BLS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6300,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BCC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BCC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BCS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BCS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BNE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6600,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BNE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6600,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6700,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6700,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BVC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6800,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BVC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6800,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BVS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6900,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BVS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6900,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BPL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6a00,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BPL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6a00,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BMI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6b00,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BMI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6b00,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6c00,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6c00,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6d00,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6d00,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6e00,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6e00,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6f00,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6f00,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BRA;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BRA;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6000,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BSR;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BSR;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6100,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_BCHG;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0140,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_BCHG;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0140,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BCHG;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($0840,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_BCHG;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($0840,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BCHG;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($0840,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BCLR;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0180,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_BCLR;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0180,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BCLR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($0880,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_BCLR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($0880,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BCLR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($0880,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BSET;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($01c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_BSET;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($01c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BSET;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($08c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_BSET;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($08c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BSET;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($08c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BTST;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_BTST;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BTST;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($0800,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_BTST;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($0800,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BTST;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($0800,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_BFCHG;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BITFIELD],[],[],[],[],[]);
    codelen : 2;
    code    : ($eac0,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_BFCLR;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BITFIELD],[],[],[],[],[]);
    codelen : 2;
    code    : ($ecc0,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_BFEXTS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[OT_DATA],[],[],[],[]);
    opflags : ([OF_BITFIELD],[],[],[],[],[]);
    codelen : 2;
    code    : ($ebc0,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_BFEXTU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[OT_DATA],[],[],[],[]);
    opflags : ([OF_BITFIELD],[],[],[],[],[]);
    codelen : 2;
    code    : ($e9c0,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_BFFFO;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[OT_DATA],[],[],[],[]);
    opflags : ([OF_BITFIELD],[],[],[],[],[]);
    codelen : 2;
    code    : ($edc0,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_BFINS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BITFIELD],[],[],[],[]);
    codelen : 2;
    code    : ($efc0,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_BFSET;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BITFIELD],[],[],[],[],[]);
    codelen : 2;
    code    : ($eec0,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_BFTST;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[],[]);
    opflags : ([OF_BITFIELD],[],[],[],[],[]);
    codelen : 2;
    code    : ($e8c0,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_BGND;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4afa,$0000);
    support : [OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_BITREV;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($00c0,$0000);
    support : [OS_CF_ISA_APL, OS_CF_ISA_C];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_BKPT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($4848,$0000);
    support : [OS_M68010UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_BYTEREV;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($02c0,$0000);
    support : [OS_CF_ISA_APL, OS_CF_ISA_C];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_CALLM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($06c0,$0000);
    support : [OS_M68020];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_CAS;
    ops     : 3;
    optypes : ([OT_DATA],[OT_DATA],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($08c0,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CAS2;
    ops     : 3;
    optypes : ([OT_DATA],[OT_DATA],[OT_ADDR_INDIR],[],[],[]);
    opflags : ([OF_DOUBLE_REG],[OF_DOUBLE_REG],[OF_DOUBLE_REG],[],[],[]);
    codelen : 3;
    code    : ($08fc,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CHK;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4180,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_CHK;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4100,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_CHK2;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[OT_DATA, OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($00c0,$0800);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CLR;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4200,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b000,$0000);
    support : [OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b000,$0000);
    support : [OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b0c0,$0000);
    support : [OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b0c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0c00,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0c00,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMP;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR_POSTINC],[OT_ADDR_INDIR_POSTINC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b108,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMPA;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b0c0,$0000);
    support : [OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMPA;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b0c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_CMPI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0c00,$0000);
    support : [OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMPI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0c00,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_CMPI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0c00,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMPI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0c00,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMPM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR_POSTINC],[OT_ADDR_INDIR_POSTINC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b108,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CMP2;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[OT_DATA, OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($00c0,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_CINVL;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 1;
    code    : ($f408,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_CINVP;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 1;
    code    : ($f410,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_CINVA;
    ops     : 1;
    optypes : ([OT_SPECIALREG],[],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 1;
    code    : ($f418,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_CPUSHL;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f4e8,$0000);
    support : [OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_CPUSHL;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 1;
    code    : ($f428,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_CPUSHP;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 1;
    code    : ($f430,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_CPUSHA;
    ops     : 1;
    optypes : ([OT_SPECIALREG],[],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 1;
    code    : ($f438,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_DBT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($50c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBF;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($51c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBRA;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($51c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBHI;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($52c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBLS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($53c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBCC;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($54c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBHS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($54c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBCS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($55c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBLO;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($55c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBNE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($56c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBEQ;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($57c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBVC;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($58c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBVS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($59c8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBPL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($5ac8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBMI;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($5bc8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBGE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($5cc8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBLT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($5dc8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBGT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($5ec8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DBLE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 1;
    code    : ($5fc8,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DIVS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($81c0,$0000);
    support : [OS_M68000UP, OS_CF_HWDIV];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DIVS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c40,$0800);
    support : [OS_M68020UP, OS_CPU32, OS_CF_HWDIV];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_DIVS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($81c0,$0000);
    support : [OS_M68000UP, OS_CF_HWDIV];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DIVS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c40,$0800);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_DIVS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($4c40,$0c00);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_DIVSL;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($4c40,$0800);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_DIVU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($80c0,$0000);
    support : [OS_M68000UP, OS_CF_HWDIV];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DIVU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c40,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_HWDIV];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_DIVU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($80c0,$0000);
    support : [OS_M68000UP, OS_CF_HWDIV];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_DIVU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c40,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_DIVU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($4c40,$0400);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_DIVUL;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($4c40,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_EOR;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($b100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_EOR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0a00,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_EOR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0a00,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_EOR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($0a3c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_EOR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($0a7c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_EORI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0a00,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_EORI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0a00,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_EORI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($0a3c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_EORI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($0a7c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_EXG;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c140,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_EXG;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c148,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_EXG;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c188,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_EXG;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c188,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_EXT;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4800,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_EXTB;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4900,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FABS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4018);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FABS;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4018);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FABS;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4018);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FABS;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0018);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FABS;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0018);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FABS;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0018);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FABS;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0018);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSABS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4058);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSABS;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4058);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FSABS;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4058);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSABS;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0058);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSABS;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0058);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSABS;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0058);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSABS;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0058);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDABS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$405c);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FDABS;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$405c);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FDABS;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$405c);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FDABS;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005c);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDABS;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005c);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDABS;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005c);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDABS;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005c);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FACOS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401c);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FACOS;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401c);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FACOS;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001c);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FACOS;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001c);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FADD;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4022);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FADD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4022);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FADD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4022);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FADD;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0022);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FADD;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0022);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSADD;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4062);
    support : [OS_M68040UP];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSADD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4062);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FSADD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4062);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSADD;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0062);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSADD;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0062);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDADD;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4066);
    support : [OS_M68040UP];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FDADD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4066);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FDADD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4066);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FDADD;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0066);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDADD;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0066);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FASIN;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$400c);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FASIN;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$400c);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FASIN;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$000c);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FASIN;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$000c);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FATAN;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$400a);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FATAN;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$400a);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FATAN;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$000a);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FATAN;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$000a);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FATANH;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$400d);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FATANH;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$400d);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FATANH;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$000d);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FATANH;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$000d);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FBF;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f080,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f081,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBOGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f082,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBOGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f083,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBOLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f084,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBOLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f085,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBOGL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f086,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBOR;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f087,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBUN;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f088,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBUEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f089,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBUGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08a,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBUGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08b,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBULT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08c,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBULE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08d,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBNE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08e,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08f,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBSF;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f090,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBSEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f091,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f092,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f093,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f094,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f095,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBGL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f096,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBGLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f097,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBNGLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f098,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBNGL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f099,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBNLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09a,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBNLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09b,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBNGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09c,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBNGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09d,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBSNE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09e,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FBST;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09f,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FCMP;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4038);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FCMP;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4038);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FCMP;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4038);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FCMP;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0038);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FCMP;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0038);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FCOS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401d);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FCOS;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401d);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FCOS;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001d);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FCOS;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001d);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FCOSH;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4019);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FCOSH;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4019);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FCOSH;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0019);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FCOSH;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0019);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDBF;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0000);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBEQ;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0001);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBOGT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0002);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBOGE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0003);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBOLT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0004);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBOLE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0005);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBOGL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0006);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBOR;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0007);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBUN;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0008);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBUEQ;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0009);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBUGT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000a);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBUGE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000b);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBULT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000c);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBULE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000d);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBNE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000e);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000f);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBSF;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0010);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBSEQ;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0011);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBGT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0012);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBGE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0013);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBLT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0014);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBLE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0015);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBGL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0016);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBGLE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0017);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBNGLE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0018);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBNGL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0019);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBNLE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$001a);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBNLT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$001b);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBNGE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$001c);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBNGT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$001d);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBSNE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$001e);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDBST;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$001f);
    support : [OS_M68881];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FDIV;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4020);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FDIV;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4020);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FDIV;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4020);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FDIV;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0020);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDIV;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0020);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSDIV;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4060);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSDIV;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4060);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FSDIV;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4060);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSDIV;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0060);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSDIV;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0060);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDDIV;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4064);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FDDIV;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4064);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FDDIV;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4064);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FDDIV;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0064);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDDIV;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0064);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FETOX;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4010);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FETOX;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4010);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FETOX;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0010);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FETOX;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0010);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FETOXM1;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4008);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FETOXM1;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4008);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FETOXM1;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0008);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FETOXM1;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0008);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FGETEXP;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401e);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FGETEXP;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401e);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FGETEXP;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001e);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FGETEXP;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001e);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FGETMAN;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401f);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FGETMAN;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401f);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FGETMAN;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001f);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FGETMAN;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001f);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FINT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4001);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FINT;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4001);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FINT;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4001);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FINT;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0001);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FINT;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0001);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FINT;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0001);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FINT;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0001);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FINTRZ;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4003);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FINTRZ;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4003);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FINTRZ;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4003);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FINTRZ;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0003);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FINTRZ;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0003);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FINTRZ;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0003);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FINTRZ;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0003);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FJF;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f080,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f081,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJOGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f082,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJOGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f083,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJOLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f084,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJOLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f085,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJOGL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f086,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJOR;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f087,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJUN;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f088,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJUEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f089,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJUGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08a,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJUGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08b,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJULT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08c,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJULE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08d,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJNE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08e,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08f,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJSF;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f090,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJSEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f091,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f092,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f093,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f094,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f095,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJGL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f096,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJGLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f097,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJNGLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f098,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJNGL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f099,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJNLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09a,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJNLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09b,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJNGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09c,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJNGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09d,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJSNE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09e,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FJST;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f09f,$0000);
    support : [OS_GNU_AS, OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FLOG10;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4015);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FLOG10;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4015);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FLOG10;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0015);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FLOG10;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0015);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FLOG2;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4016);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FLOG2;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4016);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FLOG2;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0016);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FLOG2;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0016);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FLOGN;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4014);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FLOGN;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4014);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FLOGN;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0014);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FLOGN;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0014);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FLOGNP1;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4006);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FLOGNP1;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4006);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FLOGNP1;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0006);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FLOGNP1;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0006);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOD;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4021);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FMOD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4021);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FMOD;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0021);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0000);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0000);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4000);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4000);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$6000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_KFACTOR],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$6000);
    support : [OS_M68881];
    sizes   : [OPS_PACKED];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$6000);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$6000);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_FPIAR],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$8000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_FPU],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$8000);
    support : [OS_CF_FPU];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_FPU],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$8000);
    support : [OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_FPIAR],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$a000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_FPU],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$a000);
    support : [OS_CF_FPU];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_FPU],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$a000);
    support : [OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FSMOVE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0040);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSMOVE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0040);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSMOVE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4040);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4040);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FSMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4040);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FDMOVE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0044);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDMOVE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0044);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDMOVE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4044);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FDMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4044);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FDMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4044);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FMOVECR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$5c00);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_FPUREG_LIST],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$e000);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_FPUREG_LIST],[OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$f000);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_FPUREG_LIST],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$f000);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$e000);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$f000);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$f000);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$e800);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$f800);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG_LIST],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$d000);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_FPUREG_LIST],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$d000);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_PC_DISP16],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$d000);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$d000);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$d800);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_FPU],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$a000);
    support : [OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_FPIAR],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$a000);
    support : [OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_FPUREG_LIST],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_FPU],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$a000);
    support : [OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_DATA],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_FPU],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$8000);
    support : [OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_FPIAR],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$8000);
    support : [OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVEM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG_LIST],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_FPU],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$8000);
    support : [OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_FPUREG_LIST],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$e000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_FPUREG_LIST],[OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$f000);
    support : [OS_GNU_AS, OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_FPUREG_LIST],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$f000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$e000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$f000);
    support : [OS_GNU_AS, OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$f000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$e800);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$f800);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG_LIST],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$d000);
    support : [OS_GNU_AS, OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_FPUREG_LIST],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$d000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_PC_DISP16],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$d000);
    support : [OS_GNU_AS, OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$d000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$d800);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_FPU],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$a000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_FPIAR],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$a000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_FPUREG_LIST],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_FPU],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$a000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_DATA],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_FPU],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$8000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_FPIAR],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$8000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMOVM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG_LIST],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_FPU],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$8000);
    support : [OS_GNU_AS, OS_M68881];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_FMUL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4023);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FMUL;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4023);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FMUL;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4023);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FMUL;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0023);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FMUL;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0023);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSMUL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4063);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSMUL;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4063);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FSMUL;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4063);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSMUL;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0063);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSMUL;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0063);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDMUL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4067);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FDMUL;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4067);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FDMUL;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4067);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FDMUL;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0067);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDMUL;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0067);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FNEG;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401a);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FNEG;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401a);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FNEG;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$401a);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FNEG;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001a);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FNEG;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001a);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FNEG;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001a);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FNEG;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$001a);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSNEG;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$405a);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSNEG;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$405a);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FSNEG;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$405a);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSNEG;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005a);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSNEG;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005a);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSNEG;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005a);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSNEG;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005a);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDNEG;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$405e);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FDNEG;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$405e);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FDNEG;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$405e);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FDNEG;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005e);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDNEG;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005e);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDNEG;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005e);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDNEG;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$005e);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FNOP;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f080,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FREM;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4025);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FREM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4025);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FREM;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0025);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FRESTORE;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_PC_DISP16],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f140,$0000);
    support : [OS_CF_FPU];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FRESTORE;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f140,$0000);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FSAVE;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f100,$0000);
    support : [OS_CF_FPU];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FSAVE;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f100,$0000);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FSCALE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4026);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSCALE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4026);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSCALE;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0026);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSF;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0000);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSEQ;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0001);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSOGT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0002);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSOGE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0003);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSOLT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0004);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSOLE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0005);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSOGL;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0006);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSOR;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0007);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSUN;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0008);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSUEQ;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0009);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSUGT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000a);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSUGE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000b);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSULT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000c);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSULE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000d);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSNE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000e);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FST;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000f);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSSF;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0010);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSSEQ;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0011);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSGT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0012);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSGE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0013);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSLT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0014);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSLE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0015);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSGL;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0016);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSGLE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0017);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSNGLE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0018);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSNGL;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0019);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSNLE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$001a);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSNLT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$001b);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSNGE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$001c);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSNGT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$001d);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSSNE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$001e);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSST;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$001f);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_FSGLDIV;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4024);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSGLDIV;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4024);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSGLDIV;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0024);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSGLMUL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4027);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSGLMUL;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4027);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSGLMUL;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0027);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSIN;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$400e);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSIN;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$400e);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSIN;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$000e);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSIN;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$000e);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSINCOS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4030);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSINCOS;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4030);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSINCOS;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0030);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSINH;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4002);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSINH;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4002);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSINH;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0002);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSINH;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0002);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSQRT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4004);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSQRT;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4004);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FSQRT;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4004);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSQRT;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0004);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSQRT;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0004);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSQRT;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0004);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSQRT;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0004);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSSQRT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4041);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSSQRT;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4041);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FSSQRT;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4041);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSSQRT;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0041);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSSQRT;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0041);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSSQRT;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0041);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSSQRT;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0041);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDSQRT;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4045);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FDSQRT;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4045);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FDSQRT;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4045);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FDSQRT;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0045);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDSQRT;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0045);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDSQRT;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0045);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDSQRT;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0045);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSUB;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4028);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSUB;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4028);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FSUB;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4028);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSUB;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0028);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSUB;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0028);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FSSUB;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4068);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSSUB;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$4068);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FSSUB;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0068);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FSSUB;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$0068);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FDSUB;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$406c);
    support : [OS_M68040UP, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FDSUB;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$406c);
    support : [OS_M68040UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FDSUB;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$006c);
    support : [OS_M68040UP];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FDSUB;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f200,$006c);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FTAN;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$400f);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTAN;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$400f);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FTAN;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$000f);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FTAN;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$000f);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FTANH;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4009);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTANH;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4009);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FTANH;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0009);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FTANH;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0009);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FTENTOX;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4012);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTENTOX;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4012);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FTENTOX;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0012);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FTENTOX;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0012);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FTRAPF;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0000);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPF;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0000);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPEQ;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0001);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPEQ;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0001);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPOGT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0002);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPOGT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0002);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPOGE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0003);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPOGE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0003);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPOLT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0004);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPOLT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0004);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPOLE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0005);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPOLE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0005);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPOGL;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0006);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPOGL;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0006);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPOR;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0007);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPOR;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0007);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPUN;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0008);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPUN;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0008);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPUEQ;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0009);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPUEQ;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0009);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPUGT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000a);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPUGT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000a);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPUGE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000b);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPUGE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000b);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPULT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000c);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPULT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000c);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPULE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000d);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPULE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000d);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPNE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000e);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPNE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000e);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000f);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000f);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPSF;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0010);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPSF;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0010);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPSEQ;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0011);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPSEQ;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0011);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPGT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0012);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPGT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0012);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPGE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0013);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPGE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0013);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPLT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0014);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPLT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0014);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPLE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0015);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPLE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0015);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPGL;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0016);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPGL;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0016);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPGLE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0017);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPGLE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0017);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPNGLE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0018);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPNGLE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0018);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPNGL;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0019);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPNGL;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0019);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPNLE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$001a);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPNLE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$001a);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPNLT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$001b);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPNLT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$001b);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPNGE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$001c);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPNGE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$001c);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPNGT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$001d);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPNGT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$001d);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPSNE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$001e);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPSNE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$001e);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTRAPST;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$001f);
    support : [OS_M68881];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTRAPST;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$001f);
    support : [OS_M68881];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_FTST;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$403a);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTST;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_PC_DISP16],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$403a);
    support : [OS_CF_FPU];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_COLDFIRE];
  ),
  (
    opcode  : A_FTST;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$403a);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FTST;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$003a);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FTST;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$003a);
    support : [OS_CF_FPU];
    sizes   : [OPS_DOUBLE];
  ),
  (
    opcode  : A_FTWOTOX;
    ops     : 2;
    optypes : ([OT_DATA],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4011);
    support : [OS_M68881];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FTWOTOX;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_FPUREG],[],[],[],[]);
    opflags : ([OF_IMM_FLOAT],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4011);
    support : [OS_M68881];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_SINGLE, OPS_DOUBLE, OPS_EXTENDED, OPS_PACKED];
  ),
  (
    opcode  : A_FTWOTOX;
    ops     : 2;
    optypes : ([OT_FPUREG],[OT_FPUREG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0011);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FTWOTOX;
    ops     : 1;
    optypes : ([OT_FPUREG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0011);
    support : [OS_M68881];
    sizes   : [OPS_EXTENDED];
  ),
  (
    opcode  : A_FF1;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($04c0,$0000);
    support : [OS_CF_ISA_APL, OS_CF_ISA_C];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_HALT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4ac8,$0000);
    support : [OS_M68060, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_ILLEGAL;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4afc,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_INTOUCH;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f428,$0000);
    support : [OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_JHS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JHS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JLO;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JLO;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JHI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6200,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JHI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6200,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JLS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6300,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JLS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6300,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JCC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JCC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JCS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JCS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JNE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6600,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JNE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6600,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6700,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6700,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JVC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6800,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JVC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6800,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JVS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6900,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JVS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6900,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JPL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6a00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JPL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6a00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JMI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6b00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JMI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6b00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6c00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6c00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6d00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6d00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6e00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6e00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6f00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6f00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBHS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBHS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBLO;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBLO;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBHI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6200,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBHI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6200,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBLS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6300,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBLS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6300,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBCC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBCC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6400,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBCS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBCS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6500,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBNE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6600,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBNE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6600,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6700,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBEQ;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6700,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBVC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6800,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBVC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6800,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBVS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6900,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBVS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6900,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBPL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6a00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBPL;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6a00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBMI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6b00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBMI;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6b00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6c00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBGE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6c00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6d00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBLT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6d00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6e00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBGT;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6e00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6f00,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_JBLE;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6f00,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_JBRA;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6000,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_JBRA;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4ec0,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_JRA;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6000,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_JRA;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4ec0,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_JMP;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4ec0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_JBSR;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($6100,$0000);
    support : [OS_GNU_AS, OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_JBSR;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e80,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_JSR;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e80,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_LEA;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($41c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_LINEA;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($a000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_LINE_A;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($a000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_LINEF;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($f000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_LINE_F;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($f000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_LINK;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e50,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_LINK;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4808,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_LPSTOP;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f800,$01c0);
    support : [OS_M68060, OS_CPU32];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_LSL;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e3c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_LSL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e128,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_LSL;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($e108,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_LSL;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e308,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_LSR;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e2c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_LSR;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e028,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_LSR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($e008,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_LSR;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e208,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0008,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0040,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0000,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_CCR],[],[],[],[],[]);
    codelen : 1;
    code    : ($42c0,$0000);
    support : [OS_GNU_AS, OS_M68010UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_CCR],[],[],[],[],[]);
    codelen : 1;
    code    : ($42c0,$0000);
    support : [OS_GNU_AS, OS_M68010UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_SR],[],[],[],[],[]);
    codelen : 1;
    code    : ($40c0,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_SR],[],[],[],[],[]);
    codelen : 1;
    code    : ($40c0,$0000);
    support : [OS_GNU_AS, OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_DATA],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($44c0,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($44c0,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($44c0,$0000);
    support : [OS_GNU_AS, OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_DATA],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($46c0,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($46c0,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($46c0,$0000);
    support : [OS_GNU_AS, OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_USP],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e68,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF_USP];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MOV;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_USP],[],[],[],[]);
    codelen : 1;
    code    : ($4e60,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF_USP];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MOV3Q;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($a140,$0000);
    support : [OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0008,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0040,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_CCR],[],[],[],[],[]);
    codelen : 1;
    code    : ($42c0,$0000);
    support : [OS_M68010UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_CCR],[],[],[],[],[]);
    codelen : 1;
    code    : ($42c0,$0000);
    support : [OS_M68010UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_SR],[],[],[],[],[]);
    codelen : 1;
    code    : ($40c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_SR],[],[],[],[],[]);
    codelen : 1;
    code    : ($40c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($44c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($44c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($44c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_DATA],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($46c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($46c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($46c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR],[],[],[],[]);
    opflags : ([OF_SPECREG, OF_SPECREG_USP],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e68,$0000);
    support : [OS_M68000UP, OS_CF_USP];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MOVE;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_USP],[],[],[],[]);
    codelen : 1;
    code    : ($4e60,$0000);
    support : [OS_M68000UP, OS_CF_USP];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MOVEA;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0040,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVEC;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($4e7a,$0000);
    support : [OS_M68010UP];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MOVEC;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($4e7b,$0000);
    support : [OS_M68010UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MOVEM;
    ops     : 2;
    optypes : ([OT_REG_LIST],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVEM;
    ops     : 2;
    optypes : ([OT_REG_LIST],[OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_MOVEM;
    ops     : 2;
    optypes : ([OT_REG_LIST],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVEM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16],[OT_REG_LIST],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c80,$0000);
    support : [OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_MOVEM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_REG_LIST],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c80,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVEM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVEM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_MOVEM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVEM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($4c80,$0000);
    support : [OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_MOVEM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($4c80,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVEP;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0108,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVEP;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0188,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVEQ;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($7000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MOVES;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_DATA, OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($0e00,$0000);
    support : [OS_M68010UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVES;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($0e00,$0800);
    support : [OS_M68010UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVE16;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR_POSTINC],[OT_ADDR_INDIR_POSTINC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f620,$8000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_MOVE16;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR_POSTINC],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f600,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_MOVE16;
    ops     : 2;
    optypes : ([OT_ABS_LONG],[OT_ADDR_INDIR_POSTINC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f608,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_MOVE16;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f610,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_MOVE16;
    ops     : 2;
    optypes : ([OT_ABS_LONG],[OT_ADDR_INDIR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f618,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_MOVM;
    ops     : 2;
    optypes : ([OT_REG_LIST],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_GNU_AS, OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVM;
    ops     : 2;
    optypes : ([OT_REG_LIST],[OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_MOVM;
    ops     : 2;
    optypes : ([OT_REG_LIST],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_GNU_AS, OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16],[OT_REG_LIST],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c80,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_MOVM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_REG_LIST],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c80,$0000);
    support : [OS_GNU_AS, OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_GNU_AS, OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_MOVM;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($4880,$0000);
    support : [OS_GNU_AS, OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MOVM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($4c80,$0000);
    support : [OS_GNU_AS, OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_MOVM;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($4c80,$0000);
    support : [OS_GNU_AS, OS_M68000UP];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_MULS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c1c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MULS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c00,$0800);
    support : [OS_M68020UP, OS_CPU32, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MULS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c1c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MULS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c00,$0800);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MULS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($4c00,$0c00);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MULU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c0c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MULU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c00,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MULU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($c0c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_MULU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($4c00,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MULU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($4c00,$0400);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_MVS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($7100,$0000);
    support : [OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_MVZ;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($7180,$0000);
    support : [OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_NBCD;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4800,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_NEG;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4400,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_NEG;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4400,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_NEGX;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_NEGX;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4000,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_NOP;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e71,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_NOT;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4600,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_NOT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4600,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($8000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($8100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0000,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($003c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_OR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($007c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_ORI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_ORI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0000,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ORI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_CCR],[],[],[],[]);
    codelen : 1;
    code    : ($003c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_ORI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG, OF_SPECREG_SR],[],[],[],[]);
    codelen : 1;
    code    : ($007c,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PACK;
    ops     : 3;
    optypes : ([OT_DATA],[OT_DATA],[OT_IMMEDIATE],[],[],[]);
    opflags : ([],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($8140,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PACK;
    ops     : 3;
    optypes : ([OT_ADDR_INDIR_PREDEC],[OT_ADDR_INDIR_PREDEC],[OT_IMMEDIATE],[],[],[]);
    opflags : ([],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($8148,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PBBS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f080,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBBC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f081,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBLS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f082,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBLC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f083,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBSS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f084,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBSC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f085,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBAS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f086,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBAC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f087,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBWS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f088,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBWC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f089,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBIS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08a,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBIC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08b,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBGS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08c,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBGC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08d,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBCS;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08e,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PBCC;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 1;
    code    : ($f08f,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PDBBS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBBC;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0001);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBLS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0002);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBLC;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0003);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBSS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0004);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBSC;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0005);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBAS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0006);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBAC;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0007);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBWS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0008);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBWC;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$0009);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBIS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000a);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBIC;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000b);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBGS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000c);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBGC;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000d);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBCS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000e);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PDBCC;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 2;
    code    : ($f048,$000f);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PEA;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4840,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f508,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3010);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3010);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 2;
    optypes : ([OT_DATA],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3008);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 2;
    optypes : ([OT_DATA],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3008);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([OF_SPECREG],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3000);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([OF_SPECREG],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3000);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 3;
    optypes : ([OT_IMMEDIATE],[OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3810);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 3;
    optypes : ([OT_IMMEDIATE],[OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3810);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 3;
    optypes : ([OT_DATA],[OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[]);
    opflags : ([],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3808);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 3;
    optypes : ([OT_DATA],[OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[]);
    opflags : ([],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3808);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 3;
    optypes : ([OT_SPECIALREG],[OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[]);
    opflags : ([OF_SPECREG],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3800);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSH;
    ops     : 3;
    optypes : ([OT_SPECIALREG],[OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[]);
    opflags : ([OF_SPECREG],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3800);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHA;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f518,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHA;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2400);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHAN;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f510,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHN;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f500,$0000);
    support : [OS_M68040UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHR;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$a000);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHS;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3410);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3408);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHS;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_IMMEDIATE],[],[],[],[]);
    opflags : ([OF_SPECREG],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3400);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHS;
    ops     : 3;
    optypes : ([OT_IMMEDIATE],[OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3c10);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHS;
    ops     : 3;
    optypes : ([OT_DATA],[OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[]);
    opflags : ([],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3c08);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PFLUSHS;
    ops     : 3;
    optypes : ([OT_SPECIALREG],[OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[]);
    opflags : ([OF_SPECREG],[OF_IMM_QUICK, OF_NOSIZE],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$3c00);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PLOADR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2210);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PLOADR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2210);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PLOADR;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2208);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PLOADR;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2200);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PLOADW;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2010);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PLOADW;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2010);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PLOADW;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2008);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PLOADW;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2000);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PLPAR;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f5c8,$0000);
    support : [OS_M68060];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PLPAW;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f588,$0000);
    support : [OS_M68060];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4000);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_QUAD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([OF_IMM_64BIT],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4000);
    support : [OS_M68851];
    sizes   : [OPS_QUAD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4000);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4000);
    support : [OS_M68851];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4000);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4000);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$7000);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$7400);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$6000);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$6000);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0000);
    support : [OS_M68030];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4200);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_QUAD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4200);
    support : [OS_M68851];
    sizes   : [OPS_QUAD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4200);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4200);
    support : [OS_M68851];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4200);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4200);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$7200);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$7600);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$6200);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$6200);
    support : [OS_M68851];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_PMOVE;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0200);
    support : [OS_M68030];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_PMOVEFD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4100);
    support : [OS_M68030];
    sizes   : [OPS_QUAD];
  ),
  (
    opcode  : A_PMOVEFD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$4100);
    support : [OS_M68030];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_PMOVEFD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_SPECIALREG],[],[],[],[]);
    opflags : ([],[OF_SPECREG],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$0100);
    support : [OS_M68030];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_PRESTORE;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f140,$0000);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PSAVE;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f100,$0000);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PSBS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0000);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSBC;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0001);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSLS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0002);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSLC;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0003);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSSS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0004);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSSC;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0005);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSAS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0006);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSAC;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0007);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSWS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0008);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSWC;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$0009);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSIS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000a);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSIC;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000b);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSGS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000c);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSGC;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000d);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSCS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000e);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PSCC;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f040,$000f);
    support : [OS_M68851];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_PTESTR;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f568,$0000);
    support : [OS_M68040];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTR;
    ops     : 3;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8210);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTR;
    ops     : 3;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8210);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTR;
    ops     : 3;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[]);
    opflags : ([],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8208);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTR;
    ops     : 3;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[]);
    opflags : ([OF_SPECREG],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8200);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTR;
    ops     : 4;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[OT_ADDR],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8310);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTR;
    ops     : 4;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[OT_ADDR],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8310);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTR;
    ops     : 4;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[OT_ADDR],[],[]);
    opflags : ([],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8308);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTR;
    ops     : 4;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[OT_ADDR],[],[]);
    opflags : ([OF_SPECREG],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8300);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTW;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($f548,$0000);
    support : [OS_M68040];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTW;
    ops     : 3;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8010);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTW;
    ops     : 3;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8010);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTW;
    ops     : 3;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[]);
    opflags : ([],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8008);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTW;
    ops     : 3;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[],[],[]);
    opflags : ([OF_SPECREG],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8000);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTW;
    ops     : 4;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[OT_ADDR],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8110);
    support : [OS_M68030];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTW;
    ops     : 4;
    optypes : ([OT_IMMEDIATE],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[OT_ADDR],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8110);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTW;
    ops     : 4;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[OT_ADDR],[],[]);
    opflags : ([],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8108);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTESTW;
    ops     : 4;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[OT_IMMEDIATE],[OT_ADDR],[],[]);
    opflags : ([OF_SPECREG],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($f000,$8100);
    support : [OS_M68030, OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPBS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0000);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPBS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0000);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPBC;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0001);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPBC;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0001);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPLS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0002);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPLS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0002);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPLC;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0003);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPLC;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0003);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPSS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0004);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPSS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0004);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPSC;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0005);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPSC;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0005);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPAS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0006);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPAS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0006);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPAC;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0007);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPAC;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0007);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPWS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0008);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPWS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0008);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPWC;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$0009);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPWC;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$0009);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPIS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000a);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPIS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000a);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPIC;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000b);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPIC;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000b);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPGS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000c);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPGS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000c);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPGC;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000d);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPGC;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000d);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPCS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000e);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPCS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000e);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PTRAPCC;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f078,$000f);
    support : [OS_M68851];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_PTRAPCC;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f07c,$000f);
    support : [OS_M68851];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PULSE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4acc,$0000);
    support : [OS_M68060, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_PVALID;
    ops     : 2;
    optypes : ([OT_SPECIALREG],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_SPECREG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2800);
    support : [OS_M68851];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_PVALID;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f000,$2c00);
    support : [OS_M68851];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_REMS;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($4c40,$0800);
    support : [OS_CF_HWDIV];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_REMU;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16],[OT_DATA],[],[],[],[]);
    opflags : ([],[OF_DOUBLE_REG],[],[],[],[]);
    codelen : 2;
    code    : ($4c40,$0000);
    support : [OS_CF_HWDIV];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_RESET;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e70,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_ROL;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e7c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_ROL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e138,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROL;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($e118,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROL;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e318,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROR;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e6c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_ROR;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e038,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($e018,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROR;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e218,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROXL;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e5c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_ROXL;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e130,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROXL;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($e110,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROXL;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e310,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROXR;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e4c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_ROXR;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e030,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROXR;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($e010,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_ROXR;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($e210,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_RTD;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($4e74,$0000);
    support : [OS_M68010UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_RTE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e73,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_RTM;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($06c0,$0000);
    support : [OS_M68020];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_RTR;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e77,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_RTS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e75,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_SATS;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4c80,$0000);
    support : [OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_SBCD;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($8100,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SBCD;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR_PREDEC],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($8108,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_ST;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($50c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_ST;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($50c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SF;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($51c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SF;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($51c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SHI;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($52c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SHI;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($52c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SLS;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($53c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SLS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($53c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SCC;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($54c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SCC;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($54c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SHS;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($54c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SHS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($54c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SCS;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($55c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SCS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($55c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SLO;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($55c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SLO;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($55c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SNE;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($56c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SNE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($56c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SEQ;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($57c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SEQ;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($57c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SVC;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($58c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SVC;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($58c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SVS;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($59c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SVS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($59c0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SPL;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5ac0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SPL;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5ac0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SMI;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5bc0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SMI;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5bc0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SGE;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5cc0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SGE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5cc0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SLT;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5dc0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SLT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5dc0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SGT;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5ec0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SGT;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5ec0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SLE;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5fc0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SLE;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5fc0,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_STOP;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 2;
    code    : ($4e72,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_STRLDSR;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($40e7,$46fc);
    support : [OS_CF_ISA_APL, OS_CF_ISA_C];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($9000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : ([OT_ADDR],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($9000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($9100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($90c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_SUB;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0400,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_SUBA;
    ops     : 2;
    optypes : ([OT_DATA, OT_ADDR, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($90c0,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_SUBI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0400,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_SUBI;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($0400,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_SUBQ;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_ADDR],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($5100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_SUBQ;
    ops     : 2;
    optypes : ([OT_IMMEDIATE],[OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($5100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_SUBX;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($9100,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG, OPS_COLDFIRE];
  ),
  (
    opcode  : A_SUBX;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR_PREDEC],[OT_ADDR_INDIR_PREDEC],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($9108,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_SWAP;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4840,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_TAS;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4ac0,$0000);
    support : [OS_M68000UP, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_TBLS;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f800,$0900);
    support : [OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TBLS;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([OF_DOUBLE_REG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f800,$0800);
    support : [OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TBLSN;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f800,$0d00);
    support : [OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TBLSN;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([OF_DOUBLE_REG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f800,$0c00);
    support : [OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TBLU;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f800,$0100);
    support : [OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TBLU;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([OF_DOUBLE_REG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f800,$0000);
    support : [OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TBLUN;
    ops     : 2;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8],[OT_DATA],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($f800,$0500);
    support : [OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TBLUN;
    ops     : 2;
    optypes : ([OT_DATA],[OT_DATA],[],[],[],[]);
    opflags : ([OF_DOUBLE_REG],[],[],[],[],[]);
    codelen : 2;
    code    : ($f800,$0400);
    support : [OS_CPU32];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TPF;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($51f8,$0000);
    support : [OS_CF];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TPF;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($51fc,$0000);
    support : [OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAP;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([OF_IMM_QUICK, OF_NOSIZE],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e40,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPV;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e76,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($50f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($50fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPF;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($51f8,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPF;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($51fc,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPHI;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($52f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPHI;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($52fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPLS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($53f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPLS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($53fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPCC;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($54f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPCC;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($54fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPHS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($54f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPHS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($54fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPCS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($55f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPCS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($55fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPLO;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($55f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPLO;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($55fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPNE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($56f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPNE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($56fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPEQ;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($57f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPEQ;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($57fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPVC;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($58f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPVC;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($58fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPVS;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($59f8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPVS;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($59fc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPPL;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5af8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPPL;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5afc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPMI;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5bf8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPMI;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5bfc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPGE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5cf8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPGE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5cfc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPLT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5df8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPLT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5dfc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPGT;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5ef8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPGT;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5efc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TRAPLE;
    ops     : 1;
    optypes : ([OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5ff8,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TRAPLE;
    ops     : 0;
    optypes : ([],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($5ffc,$0000);
    support : [OS_M68020UP, OS_CPU32];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_TST;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4a00,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TST;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG, OT_PC_DISP16, OT_PC_IDX_DISP8, OT_IMMEDIATE],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4a00,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_TST;
    ops     : 1;
    optypes : ([OT_ADDR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4a00,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_UNLK;
    ops     : 1;
    optypes : ([OT_ADDR],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($4e58,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_UNPK;
    ops     : 3;
    optypes : ([OT_DATA],[OT_DATA],[OT_IMMEDIATE],[],[],[]);
    opflags : ([],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($8180,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_UNPK;
    ops     : 3;
    optypes : ([OT_ADDR_INDIR_PREDEC],[OT_ADDR_INDIR_PREDEC],[OT_IMMEDIATE],[],[],[]);
    opflags : ([],[],[OF_IMM_QUICK, OF_NOSIZE],[],[],[]);
    codelen : 2;
    code    : ($8188,$0000);
    support : [OS_M68020UP];
    sizes   : [OPS_UNSIZED];
  ),
  (
    opcode  : A_WDDATA;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 1;
    code    : ($fb00,$0000);
    support : [OS_CF];
    sizes   : [OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_WDEBUG;
    ops     : 1;
    optypes : ([OT_ADDR_INDIR, OT_ADDR_DISP16],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 2;
    code    : ($fbc0,$0003);
    support : [OS_CF];
    sizes   : [OPS_LONG];
  ),
  (
    opcode  : A_BXX;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 0;
    code    : ($0000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD];
  ),
  (
    opcode  : A_BXX;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 0;
    code    : ($0000,$0000);
    support : [OS_M68020UP, OS_CPU32, OS_CF_ISA_B, OS_CF_ISA_C];
    sizes   : [OPS_SHORT, OPS_BYTE, OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_DBXX;
    ops     : 2;
    optypes : ([OT_DATA],[OT_ABS_LONG],[],[],[],[]);
    opflags : ([],[OF_BRANCH],[],[],[],[]);
    codelen : 0;
    code    : ($0000,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_WORD];
  ),
  (
    opcode  : A_FBXX;
    ops     : 1;
    optypes : ([OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([OF_BRANCH],[],[],[],[],[]);
    codelen : 0;
    code    : ($0000,$0000);
    support : [OS_M68881, OS_CF_FPU];
    sizes   : [OPS_WORD, OPS_LONG];
  ),
  (
    opcode  : A_FSXX;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 0;
    code    : ($0000,$0000);
    support : [OS_M68881];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SXX;
    ops     : 1;
    optypes : ([OT_DATA],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 0;
    code    : ($0000,$0000);
    support : [OS_M68000UP, OS_CF];
    sizes   : [OPS_BYTE];
  ),
  (
    opcode  : A_SXX;
    ops     : 1;
    optypes : ([OT_DATA, OT_ADDR_INDIR, OT_ADDR_INDIR_POSTINC, OT_ADDR_INDIR_PREDEC, OT_ADDR_DISP16, OT_ADDR_IDX_DISP8, OT_ABS_SHORT, OT_ABS_LONG],[],[],[],[],[]);
    opflags : ([],[],[],[],[],[]);
    codelen : 0;
    code    : ($0000,$0000);
    support : [OS_M68000UP];
    sizes   : [OPS_BYTE];
  )
);
