{*****************************************************************************}
{ File                   : strregs.inc                                        }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\05\08                                         }
{ Last modification date : 2002\06\01                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
'NO',
  {General purpose global registers}
'G0','G1','G2','G3','G4','G5','G6','G7',
  {General purpose out registers}
'O0','O1','O2','O3','O4','O5','O6','O7',
  {General purpose local registers}
'L0','L1','L2','L3','L4','L5','L6','L7',
  {General purpose in registers}
'I0','I1','I2','I3','I4','I5','I6','I7',
  {Floating point registers}
'F0','F1','F2','F3','F4','F5','F6','F7',
'F8','F9','F10','F11','F12','F13','F14','F15',
'F16','F17','F18','F19','F20','F21','F22','F23',
'F24','F25','F26','F27','F28','F29','F30','F31',
  {Floating point status/"front of queue" registers}
'FSR','FQ',
  {Coprocessor registers}
'C0','C1','C2','C3','C4','C5','C6','C7',
'C8','C9','C10','C11','C12','C13','C14','C15',
'C16','C17','C18','C19','C20','C21','C22','C23',
'C24','C25','C26','C27','C28','C29','C30','C31',
  {Coprocessor status/queue registers}
'CSR','CQ',
  {"Program status"/"Trap vactor base address register"/"Window invalid mask"/Y registers}
'PSR','TBR','WIM','Y',
  {Ancillary state registers}
'ASR0','ASR1','ASR2','ASR3','ASR4','ASR5','ASR6','ASR7',
'ASR8','ASR9','ASR10','ASR11','ASR12','ASR13','ASR14','ASR15',
'ASR16','ASR17','ASR18','ASR19','ASR20','ASR21','ASR22','ASR23',
'ASR24','ASR25','ASR26','ASR27','ASR28','ASR29','ASR30','ASR31'
