{*****************************************************************************}
{ File                   : registers.inc                                      }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\05\08                                         }
{ Last modification date : 2002\07\24                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
{
    $Id$
}
R_NONE
{General purpose global registers}
,R_G0{This register is usually set to zero and used as a scratch register}
,R_G1,R_G2,R_G3,R_G4,R_G5,R_G6,R_G7
{General purpose out registers}
,R_O0,R_O1,R_O2,R_O3,R_O4,R_O5,R_O6
,R_O7{This register is used to save the address of the last CALL instruction}
{General purpose local registers}
,R_L0
,R_L1{This register is used to save the Program Counter (PC) after a Trap}
,R_L2{This register is used to save the Program Counter (nPC) after a Trap}
,R_L3,R_L4,R_L5,R_L6,R_L7
{General purpose in registers}
,R_I0,R_I1,R_I2,R_I3,R_I4,R_I5,R_I6,R_I7
{Floating point registers}
,R_F0,R_F1,R_F2,R_F3,R_F4,R_F5,R_F6,R_F7
,R_F8,R_F9,R_F10,R_F11,R_F12,R_F13,R_F14,R_F15
,R_F16,R_F17,R_F18,R_F19,R_F20,R_F21,R_F22,R_F23
,R_F24,R_F25,R_F26,R_F27,R_F28,R_F29,R_F30,R_F31
{Floating point status/"front of queue" registers}
,R_FSR,R_FQ
{Coprocessor registers}
,R_C0,R_C1,R_C2,R_C3,R_C4,R_C5,R_C6,R_C7
,R_C8,R_C9,R_C10,R_C11,R_C12,R_C13,R_C14,R_C15
,R_C16,R_C17,R_C18,R_C19,R_C20,R_C21,R_C22,R_C23
,R_C24,R_C25,R_C26,R_C27,R_C28,R_C29,R_C30,R_C31
{Coprocessor status/queue registers}
,R_CSR
,R_CQ
{Integer Unit control & status registers}
,R_PSR{Processor Status Register : informs upon the program status}
,R_TBR{Trap Base Register : saves the Trap vactor base address}
,R_WIM{Window Invalid Mask : }
,R_Y{Multiply/Devide Register : }
{Ancillary State Registers : these are implementation dependent registers and
thus, are not specified by the SPARC Reference Manual. I did choose the SUN's
implementation according to the Assembler Refernce Manual.(MN)}
,R_ASR0,R_ASR1,R_ASR2,R_ASR3,R_ASR4,R_ASR5,R_ASR6,R_ASR7
,R_ASR8,R_ASR9,R_ASR10,R_ASR11,R_ASR12,R_ASR13,R_ASR14,R_ASR15
,R_ASR16,R_ASR17,R_ASR18,R_ASR19,R_ASR20,R_ASR21,R_ASR22,R_ASR23
,R_ASR24,R_ASR25,R_ASR26,R_ASR27,R_ASR28,R_ASR29,R_ASR30,R_ASR31
{The following registers are just used with the new register allocator}
,R_INTREGISTER,R_FLOATREGISTER,R_MMXREGISTER,R_KNIREGISTER
{
  $Log$
  Revision 1.3  2003-01-20 22:21:36  mazen
  * many stuff related to RTL fixed

  Revision 1.2  2002/10/02 22:20:28  mazen
  + out registers allocator for the first 6 scalar parameters which must be passed into %o0..%o5

}
