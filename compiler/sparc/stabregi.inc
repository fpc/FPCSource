{*****************************************************************************}
{ File                   : stabregi.inc                                       }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\08\22                                         }
{ Last modification date : 2002\08\22                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
0{R_NO}
{General purpose global registers}
,1{R_G0}{This register is usually set to zero and used as a scratch register}
,2{R_G1},3{R_G2},4{R_G3},5{R_G4},6{R_G5},7{R_G6},8{R_G7}
{General purpose out registers}
,9{R_O0},10{R_O1},11{R_O2},12{R_O3},13{R_O4},14{R_O5},15{R_O6}
,16{R_O7}{This register is used to save the address of the last CALL instruction}
{General purpose local registers}
,16{R_L0}
,17{R_L1}{This register is used to save the Program Counter (PC) after a Trap}
,18{R_L2}{This register is used to save the Program Counter (nPC) after a Trap}
,19{R_L3},20{R_L4},21{R_L5},22{R_L6},23{R_L7}
{General purpose in registers}
,24{R_I0},25{R_I1},26{R_I2},27{R_I3},28{R_I4},29{R_I5},30{R_I6},31{R_I7}
{Floating point registers}
,32{R_F0},33{R_F1},34{R_F2},35{R_F3},36{R_F4},37{R_F5},38{R_F6},39{R_F7}
,40{R_F8},41{R_F9},42{R_F10},43{R_F11},44{R_F12},45{R_F13},46{R_F14},47{R_F15}
,48{R_F16},49{R_F17},50{R_F18},51{R_F19},52{R_F20},53{R_F21},54{R_F22},55{R_F23}
,56{R_F24},57{R_F25},58{R_F26},59{R_F27},60{R_F28},61{R_F29},62{R_F30},63{R_F31}
{Floating point status/"front of queue" registers}
,64{R_FSR},65{R_FQ}
{Coprocessor registers}
,66{R_C0},67{R_C1},68{R_C2},69{R_C3},70{R_C4},71{R_C5},72{R_C6},73{R_C7}
,74{R_C8},75{R_C9},76{R_C10},77{R_C11},78{R_C12},79{R_C13},80{R_C14},81{R_C15}
,82{R_C16},83{R_C17},84{R_C18},85{R_C19},86{R_C20},87{R_C21},88{R_C22},89{R_C23}
,90{R_C24},91{R_C25},92{R_C26},93{R_C27},94{R_C28},95{R_C29},96{R_C30},98{R_C31}
{Coprocessor status/queue registers}
,99{R_CSR}
,100{R_CQ}
{Integer Unit control & status registers}
,101{R_PSR}{Processor Status Register : informs upon the program status}
,102{R_TBR}{Trap Base Register : saves the Trap vactor base address}
,103{R_WIM}{Window Invalid Mask : }
,104{R_Y}{Multiply/Devide Register : }
{Ancillary State Registers : these are implementation dependent registers and
thus, are not specified by the SPARC Reference Manual. I did choose the SUN's
implementation according to the Assembler Refernce Manual.(MN)}
,105{R_ASR0},106{R_ASR1},107{R_ASR2},108{R_ASR3},109{R_ASR4},110{R_ASR5},111{R_ASR6},112{R_ASR7}
,113{R_ASR8},114{R_ASR9},115{R_ASR10},116{R_ASR11},117{R_ASR12},118{R_ASR13},119{R_ASR14},120{R_ASR15}
,121{R_ASR16},122{R_ASR17},123{R_ASR18},124{R_ASR19},125{R_ASR20},126{R_ASR21},128{R_ASR22},129{R_ASR23}
,130{R_ASR24},131{R_ASR25},132{R_ASR26},133{R_ASR27},134{R_ASR28},135{R_ASR29},136{R_ASR30},137{R_ASR31}
