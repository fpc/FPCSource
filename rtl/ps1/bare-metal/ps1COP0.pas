{$mode objfpc}
unit ps1COP0;
interface

const
   { Register definitions } 
    COP0_BPC      =  3;  // Breakpoint program counter
    COP0_BDA      =  5;  // Breakpoint data address
    COP0_DCIC     =  7;  // Debug and cache invalidation control
    COP0_BADVADDR =  8;  // Bad virtual address
    COP0_BDAM     =  9;  // Breakpoint data address mask
    COP0_BPCM     = 11;  // Breakpoint program counter mask
    COP0_STATUS   = 12;  // Status register
    COP0_CAUSE    = 13;  // Exception cause
    COP0_EPC      = 14;  // Exception program counter
    COP0_PRID     = 15;   // Processor identifier

    { DCIC Flag }
    COP0_DCIC_DB  = 1 shl  0;  // Debug event pending
    COP0_DCIC_PC  = 1 shl  1;  // Program counter breakpoint pending
    COP0_DCIC_DA  = 1 shl  2;  // Data address breakpoint pending
    COP0_DCIC_R   = 1 shl  3;  // Data address read breakpoint pending
    COP0_DCIC_W   = 1 shl  4;  // Data address write breakpoint pending
    COP0_DCIC_T   = 1 shl  5;  // Trace event pending
    COP0_DCIC_DE  = 1 shl 23;  // Debug enable
    COP0_DCIC_PCE = 1 shl 24;  // Program counter breakpoint enable
    COP0_DCIC_DAE = 1 shl 25;  // Data address breakpoint enable
    COP0_DCIC_DR  = 1 shl 26;  // Data address read breakpoint enable
    COP0_DCIC_DW  = 1 shl 27;  // Data address write breakpoint enable
    COP0_DCIC_TE  = 1 shl 28;  // Trace enable
    COP0_DCIC_KD  = 1 shl 29;  // Kernel debug enable
    COP0_DCIC_UD  = 1 shl 30;  // User debug enable
    COP0_DCIC_TR  = 1 shl 31;   // Debug event trap enable

    { Status Flag}
    COP0_STATUS_IEc = 1 shl  0; // Current interrupt enable
    COP0_STATUS_KUc = 1 shl  1; // Current privilege level
    COP0_STATUS_IEp = 1 shl  2; // Previous interrupt enable
    COP0_STATUS_KUp = 1 shl  3; // Previous privilege level
    COP0_STATUS_IEo = 1 shl  4; // Old interrupt enable
    COP0_STATUS_KUo = 1 shl  5; // Old privilege level
    COP0_STATUS_Im0 = 1 shl  8; // IRQ mask 0 (software interrupt)
    COP0_STATUS_Im1 = 1 shl  9; // IRQ mask 1 (software interrupt)
    COP0_STATUS_Im2 = 1 shl 10; // IRQ mask 2 (hardware interrupt)
    COP0_STATUS_IsC = 1 shl 16; // Isolate cache
    COP0_STATUS_BEV = 1 shl 22; // Boot exception vector location
    COP0_STATUS_CU0 = 1 shl 28; // Coprocessor 0 privilege level
    COP0_STATUS_CU2 = 1 shl 30;  // Coprocessor 2 enable

    { Cause Flag }
    COP0_CAUSE_EXC_BITMASK = 31 shl  2;
    COP0_CAUSE_EXC_INT     =  0 shl  2; // Interrupt
    COP0_CAUSE_EXC_AdEL    =  4 shl  2; // Load address error
    COP0_CAUSE_EXC_AdES    =  5 shl  2; // Store address error
    COP0_CAUSE_EXC_IBE     =  6 shl  2; // Instruction bus error
    COP0_CAUSE_EXC_DBE     =  7 shl  2; // Data bus error
    COP0_CAUSE_EXC_SYS     =  8 shl  2; // Syscall
    COP0_CAUSE_EXC_BP      =  9 shl  2; // Breakpoint or break instruction
    COP0_CAUSE_EXC_RI      = 10 shl  2; // Reserved instruction
    COP0_CAUSE_EXC_CpU     = 11 shl  2; // Coprocessor unusable
    COP0_CAUSE_EXC_Ov      = 12 shl  2; // Arithmetic overflow
    COP0_CAUSE_Ip0         =  1 shl  8; // IRQ 0 pending (software interrupt)
    COP0_CAUSE_Ip1         =  1 shl  9; // IRQ 1 pending (software interrupt)
    COP0_CAUSE_Ip2         =  1 shl 10; // IRQ 2 pending (hardware interrupt)
    COP0_CAUSE_CE_BITMASK  =  3 shl 28;
    COP0_CAUSE_BD          =  1 shl 30;  // Exception occurred in delay slot

procedure cop0_SetBPC(value: dword);      // Breakpoint program counter
procedure cop0_SetBDA(value: dword);      // Breakpoint data address
procedure cop0_SetDCIC(value: dword);     // Debug and cache invalidation control
procedure cop0_SetBADVADDR(value: dword); // Bad virtual address
procedure cop0_SetBDAM(value: dword);     // Breakpoint data address mask
procedure cop0_SetBPCM(value: dword);     // Breakpoint program counter mask
procedure cop0_SetSTATUS(value: dword);   // Status register
procedure cop0_SetCAUSE(value: dword);    // Exception cause
procedure cop0_SetEPC(value: dword);      // Exception program counter
procedure cop0_SetPRID(value: dword);     // Processor identifier

function cop0_GetBPC: dword;              // Breakpoint program counter
function cop0_GetBDA: dword;              // Breakpoint data address
function cop0_GetDCIC: dword;             // Debug and cache invalidation control
function cop0_GetBADVADDR: dword;         // Bad virtual address
function cop0_GetBDAM: dword;             // Breakpoint data address mask
function cop0_GetBPCM: dword;             // Breakpoint program counter mask
function cop0_GetSTATUS: dword;           // Status register
function cop0_GetCAUSE: dword;            // Exception cause
function cop0_GetEPC: dword;              // Exception program counter
function cop0_GetPRID: dword;             // Processor identifier


implementation

procedure cop0_SetBPC(value: dword); assembler;
asm
  mtc0 $a0, $BPC
end;


procedure cop0_SetBDA(value: dword); assembler;
asm
  mtc0 $a0, $BDA
end;


procedure cop0_SetDCIC(value: dword); assembler;
asm
  mtc0 $a0, $DCIC
end;


procedure cop0_SetBADVADDR(value: dword); assembler;
asm
  mtc0 $a0, $BADVADDR
end;


procedure cop0_SetBDAM(value: dword); assembler;
asm
  mtc0 $a0, $BDAM
end;


procedure cop0_SetBPCM(value: dword); assembler;
asm
  mtc0 $a0, $BPCM
end;


procedure cop0_SetSTATUS(value: dword); assembler;
asm
  mtc0 $a0, $STATUS
end;


procedure cop0_SetCAUSE(value: dword); assembler;
asm
  mtc0 $a0, $CAUSE
end;



procedure cop0_SetEPC(value: dword); assembler;
asm
  mtc0 $a0, $EPC
end;


procedure cop0_SetPRID(value: dword); assembler;
asm
  mtc0 $a0, $PRID
end;


function cop0_GetBPC: dword; assembler;
asm
  mfc0 $t0, $BPC
  sw $t0, result
end;


function cop0_GetBDA: dword; assembler;
asm
  mfc0 $t0, $BDA
  sw $t0, result
end;


function cop0_GetDCIC: dword; assembler;
asm
  mfc0 $t0, $DCIC
  sw $t0, result
end;


function cop0_GetBADVADDR: dword; assembler;
asm
  mfc0 $t0, $BADVADDR
  sw $t0, result
end;


function cop0_GetBDAM: dword; assembler;
asm
  mfc0 $t0, $BDAM
  sw $t0, result
end;


function cop0_GetBPCM: dword; assembler;
asm
  mfc0 $t0, $BPCM
  sw $t0, result
end;


function cop0_GetSTATUS: dword; assembler;
asm
  mfc0 $t0, $STATUS
  sw $t0, result
end;


function cop0_GetCAUSE: dword; assembler;
asm
  mfc0 $t0, $CAUSE
  sw $t0, result
end;


function cop0_GetEPC: dword; assembler;
asm
  mfc0 $t0, $EPC
  sw $t0, result
end;


function cop0_GetPRID: dword; assembler;
asm
  mfc0 $t0, $PRID
  sw $t0, result
end;


end.
