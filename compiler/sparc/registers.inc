{******************************************************************************
  $Id$
  Author                 : Mazen NEIFER
  Project                : Free Pascal Compiler (FPC)
  Creation date          : 2003\04\29
  Licence                : GPL
  Bug report             : mazen.neifer.01@supaero.org
******************************************************************************}
(enum:R_NONE;number:NR_NONE)
{General purpose global registers}
,(enum:R_G0;number:NR_G0){This register is usually set to zero and used as a scratch register}
,(enum:R_G1;number:NR_G1),(enum:R_G2;number:NR_G2),(enum:R_G3;number:NR_G3),(enum:R_G4;number:NR_G4),(enum:R_G5;number:NR_G5),(enum:R_G6;number:NR_G6),(enum:R_G7;number:NR_G7)
{General purpose out registers}
,(enum:R_O0;number:NR_O0),(enum:R_O1;number:NR_O1),(enum:R_O2;number:NR_O2),(enum:R_O3;number:NR_O3),(enum:R_O4;number:NR_O4),(enum:R_O5;number:NR_O5),(enum:R_O6;number:NR_O6)
,(enum:R_O7;number:NR_O7){This register is used to save the address of the last CALL instruction}
{General purpose local registers}
,(enum:R_L0;number:NR_L0)
,(enum:R_L1;number:NR_L1){This register is used to save the Program Counter (PC) after a Trap}
,(enum:R_L2;number:NR_L2){This register is used to save the Program Counter (nPC) after a Trap}
,(enum:R_L3;number:NR_L3),(enum:R_L4;number:NR_L4),(enum:R_L5;number:NR_L5),(enum:R_L6;number:NR_L6),(enum:R_L7;number:NR_L7)
{General purpose in registers}
,(enum:R_I0;number:NR_I0),(enum:R_I1;number:NR_I1),(enum:R_I2;number:NR_I2),(enum:R_I3;number:NR_I3),(enum:R_I4;number:NR_I4),(enum:R_I5;number:NR_I5),(enum:R_I6;number:NR_I6),(enum:R_I7;number:NR_I7)
{Floating point registers}
,(enum:R_F0;number:NR_F0),(enum:R_F1;number:NR_F1),(enum:R_F2;number:NR_F2),(enum:R_F3;number:NR_F3),(enum:R_F4;number:NR_F4),(enum:R_F5;number:NR_F5),(enum:R_F6;number:NR_F6),(enum:R_F7;number:NR_F7)
,(enum:R_F8;number:NR_F8),(enum:R_F9;number:NR_F9),(enum:R_F10;number:NR_F10),(enum:R_F11;number:NR_F11),(enum:R_F12;number:NR_F12),(enum:R_F13;number:NR_F13),(enum:R_F14;number:NR_F14),(enum:R_F15;number:NR_F15)
,(enum:R_F16;number:NR_F16),(enum:R_F17;number:NR_F17),(enum:R_F18;number:NR_F18),(enum:R_F19;number:NR_F19),(enum:R_F20;number:NR_F20),(enum:R_F21;number:NR_F21),(enum:R_F22;number:NR_F22),(enum:R_F23;number:NR_F23)
,(enum:R_F24;number:NR_F24),(enum:R_F25;number:NR_F25),(enum:R_F26;number:NR_F26),(enum:R_F27;number:NR_F27),(enum:R_F28;number:NR_F28),(enum:R_F29;number:NR_F29),(enum:R_F30;number:NR_F30),(enum:R_F31;number:NR_F31)
{Floating point status/"front of queue" registers}
,(enum:R_FSR;number:NR_FSR),(enum:R_FQ;number:NR_FQ)
{Coprocessor registers}
,(enum:R_C0;number:NR_C0),(enum:R_C1;number:NR_C1),(enum:R_C2;number:NR_C2),(enum:R_C3;number:NR_C3),(enum:R_C4;number:NR_C4),(enum:R_C5;number:NR_C5),(enum:R_C6;number:NR_C6),(enum:R_C7;number:NR_C7)
,(enum:R_C8;number:NR_C8),(enum:R_C9;number:NR_C9),(enum:R_C10;number:NR_C10),(enum:R_C11;number:NR_C11),(enum:R_C12;number:NR_C12),(enum:R_C13;number:NR_C13),(enum:R_C14;number:NR_C14),(enum:R_C15;number:NR_C15)
,(enum:R_C16;number:NR_C16),(enum:R_C17;number:NR_C17),(enum:R_C18;number:NR_C18),(enum:R_C19;number:NR_C19),(enum:R_C20;number:NR_C20),(enum:R_C21;number:NR_C21),(enum:R_C22;number:NR_C22),(enum:R_C23;number:NR_C23)
,(enum:R_C24;number:NR_C24),(enum:R_C25;number:NR_C25),(enum:R_C26;number:NR_C26),(enum:R_C27;number:NR_C27),(enum:R_C28;number:NR_C28),(enum:R_C29;number:NR_C29),(enum:R_C30;number:NR_C30),(enum:R_C31;number:NR_C31)
{Coprocessor status/queue registers}
,(enum:R_CSR;number:NR_CSR)
,(enum:R_CQ;number:NR_CQ)
{Integer Unit control & status registers}
,(enum:R_PSR;number:NR_PSR){Processor Status Register : informs upon the program status}
,(enum:R_TBR;number:NR_TBR){Trap Base Register : saves the Trap vactor base address}
,(enum:R_WIM;number:NR_WIM){Window Invalid Mask : }
,(enum:R_Y;number:NR_Y){Multiply/Devide Register : }
{Ancillary State Registers : these are implementation dependent registers and
thus, are not specified by the SPARC Reference Manual. I did choose the SUN's
implementation according to the Assembler Refernce Manual.(MN)}
,(enum:R_ASR0;number:NR_ASR0),(enum:R_ASR1;number:NR_ASR1),(enum:R_ASR2;number:NR_ASR2),(enum:R_ASR3;number:NR_ASR3),(enum:R_ASR4;number:NR_ASR4),(enum:R_ASR5;number:NR_ASR5),(enum:R_ASR6;number:NR_ASR6),(enum:R_ASR7;number:NR_ASR7)
,(enum:R_ASR8;number:NR_ASR8),(enum:R_ASR9;number:NR_ASR9),(enum:R_ASR10;number:NR_ASR10),(enum:R_ASR11;number:NR_ASR11),(enum:R_ASR12;number:NR_ASR12),(enum:R_ASR13;number:NR_ASR13),(enum:R_ASR14;number:NR_ASR14),(enum:R_ASR15;number:NR_ASR15)
,(enum:R_ASR16;number:NR_ASR16),(enum:R_ASR17;number:NR_ASR17),(enum:R_ASR18;number:NR_ASR18),(enum:R_ASR19;number:NR_ASR19),(enum:R_ASR20;number:NR_ASR20),(enum:R_ASR21;number:NR_ASR21),(enum:R_ASR22;number:NR_ASR22),(enum:R_ASR23;number:NR_ASR23)
,(enum:R_ASR24;number:NR_ASR24),(enum:R_ASR25;number:NR_ASR25),(enum:R_ASR26;number:NR_ASR26),(enum:R_ASR27;number:NR_ASR27),(enum:R_ASR28;number:NR_ASR28),(enum:R_ASR29;number:NR_ASR29),(enum:R_ASR30;number:NR_ASR30),(enum:R_ASR31;number:NR_ASR31)
{The following registers are just used with the new register allocator}
,(enum:R_INTREGISTER;number:NR_NONE),(enum:R_FLOATREGISTER;number:NR_NONE),(enum:R_MMXREGISTER;number:NR_NONE),(enum:R_KNIREGISTER;number:NR_NONE)

{
  $Log$
  Revision 1.5  2003-05-23 21:10:50  florian
    * fixed sparc compiler compilation

  Revision 1.4  2003/04/29 11:52:52  mazen
  * signification of ths faile has changed.
  * It was used to declare the cpu registers physically available
  * Now, it is used to define the register allocators paris (enum,number)
}
