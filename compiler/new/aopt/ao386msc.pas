Unit ao386msc;

Interface

uses i386base;

{ enable the following define if memory references can have both a base and  }
{ index register in 1 operand                                                }

{$define RefsHaveIndexReg}

{ enable the following define if memory references can have a scaled index   }

{$define RefsHaveScale}

{ enable the following define if memory references can have a segment        }
{ override                                                                   }

{$define RefsHaveSegment}

{ enable the following define if memory references can have a symbol         }
{ value                                                                      }

{$define RefsHaveSymbol}


Type

{possible actions on an operand: read, write or modify (= read & write)}
  TOpAction = (OpAct_Read, OpAct_Write, OpAct_Modify, OpAct_Unknown);

{ type of a normal instruction }
  TInstr = Tai386;
  PInstr = ^TInstr;

  TFlag = (DirFlag);

  TFlagContents = (F_Unknown, F_Clear, F_Set);

  TCondRegs = Object
                Flags: Array[TFlag] of TFlagContents;
                Constructor Init;
                Procedure InitFlag(f: TFlag);
                Procedure SetFlag(f: TFlag);
                Procedure ClearFlag(f: TFlag);
                Function GetFlag(f: TFlag): TFlagContents;
              End;

{What an instruction can change}
  TChange = (C_None,
             {Read from a register}
             C_REAX, C_RECX, C_REDX, C_REBX, C_RESP, C_REBP, C_RESI, C_REDI,
             {write from a register}
             C_WEAX, C_WECX, C_WEDX, C_WEBX, C_WESP, C_WEBP, C_WESI, C_WEDI,
             {read and write from/to a register}
             C_RWEAX, C_RWECX, C_RWEDX, C_RWEBX, C_RWESP, C_RWEBP, C_RWESI, C_RWEDI,
             {modify the contents of a register with the purpose of using
              this changed content afterwards (add/sub/..., but e.g. not rep
              or movsd)}
{$ifdef arithopt}
             C_MEAX, C_MECX, C_MEDX, C_MEBX, C_MESP, C_MEBP, C_MESI, C_MEDI,
{$endif arithopt}
             C_CDirFlag {clear direction flag}, C_SDirFlag {set dir flag},
             C_RFlags, C_WFlags, C_RWFlags, C_FPU,
             C_Rop1, C_Wop1, C_RWop1,
             C_Rop2, C_Wop2, C_RWop2,
             C_Rop3, C_WOp3, C_RWOp3,
{$ifdef arithopt}
             C_Mop1, C_Mop2, C_Mop3,
{$endif arithopt}
             C_WMemEDI,
             C_All);


Const

{$ifndef arithopt}
   C_MEAX = C_RWEAX;
   C_MECX = C_RWECX;
   C_MEDX = C_RWEDX;
   C_MEBX = C_RWEBX;
   C_MESP = C_RWESP;
   C_MEBP = C_RWEBP;
   C_MESI = C_RWESI;
   C_MEDI = C_RWEDI;
   C_Mop1 = C_RWOp1;
   C_Mop2 = C_RWOp2;
   C_Mop3 = C_RWOp3;
{$endif arithopt}

{ the maximum number of things (registers, memory, ...) a single instruction }
{ changes                                                                    }

  MaxCh = 3;

{ the maximum number of operands an instruction has                          }

  MaxOps = 3;

{Oper index of operand that contains the source (reference) with a load      }
{instruction                                                                 }

  LoadSrc = 0;

{Oper index of operand that contains the destination (register) with a load  }
{instruction                                                                 }

  LoadDst = 1;

{Oper index of operand that contains the source (register) with a store      }
{instruction                                                                 }

  StoreSrc = 0;

{Oper index of operand that contains the destination (reference) with a load }
{instruction                                                                 }

  StoreDst = 1;



{ low and high maximum width general purpose register                        }
  LoGPReg = R_EAX;
  HiGPReg = R_EDI;

{ low and high of every possible width general purpose register (same as     }
{ above on most architctures apart from the 80x86)                           }
  LoReg = R_EAX;
  HiReg = R_BL;

{ Stack pointer }
  StackPtr = R_ESP;

Const AsmInstr: Array[tasmop] Of TAsmInstrucProp = (
  {A_<NONE>} (Ch: (C_All, C_None, C_None)), { new }
  {A_LOCK} (Ch: (C_None, C_None, C_None)),
  {A_REP} (Ch: (C_RWECX, C_RFlags, C_None)),
  {A_REPE} (Ch: (C_RWECX, C_RFlags, C_None)),
  {A_REPNE} (Ch: (C_RWECX, C_RFlags, C_None)),
  {A_REPNZ} (Ch: (C_WECX, C_RWFLAGS, C_None)), { new }
  {A_REPZ} (Ch: (C_WECX, C_RWFLAGS, C_None)), { new }
  {A_SEGCS} (Ch: (C_None, C_None, C_None)), { new }
  {A_SEGES} (Ch: (C_None, C_None, C_None)), { new }
  {A_SEGDS} (Ch: (C_None, C_None, C_None)), { new }
  {A_SEGFS} (Ch: (C_None, C_None, C_None)), { new }
  {A_SEGGS} (Ch: (C_None, C_None, C_None)), { new }
  {A_SEGSS} (Ch: (C_None, C_None, C_None)), { new }
  {A_AAA} (Ch: (C_MEAX, C_WFlags, C_None)),
  {A_AAD} (Ch: (C_MEAX, C_WFlags, C_None)),
  {A_AAM} (Ch: (C_MEAX, C_WFlags, C_None)),
  {A_AAS} (Ch: (C_MEAX, C_WFlags, C_None)),
  {A_ADC} (Ch: (C_Mop2, C_Rop1, C_RWFlags)),
  {A_ADD} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_AND} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_ARPL} (Ch: (C_WFlags, C_None, C_None)),
  {A_BOUND} (Ch: (C_Rop1, C_None, C_None)),
  {A_BSF} (Ch: (C_Wop2, C_WFlags, C_Rop1)),
  {A_BSR} (Ch: (C_Wop2, C_WFlags, C_Rop1)),
  {A_BSWAP} (Ch: (C_MOp1, C_None, C_None)), { new }
  {A_BT} (Ch: (C_WFlags, C_Rop1, C_None)),
  {A_BTC} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_BTR} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_BTS} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_CALL} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {A_CBW} (Ch: (C_MEAX, C_None, C_None)),
  {A_CDQ} (Ch: (C_MEAX, C_WEDX, C_None)),
  {A_CLC} (Ch: (C_WFlags, C_None, C_None)),
  {A_CLD} (Ch: (C_CDirFlag, C_None, C_None)),
  {A_CLI} (Ch: (C_WFlags, C_None, C_None)),
  {A_CLTS} (Ch: (C_None, C_None, C_None)),
  {A_CMC} (Ch: (C_WFlags, C_None, C_None)),
  {A_CMP} (Ch: (C_WFlags, C_None, C_None)),
  {A_CMPSB} (Ch: (C_All, C_None, C_None)), { new }
  {A_CMPSD} (Ch: (C_All, C_None, C_None)), { new }
  {A_CMPSW} (Ch: (C_All, C_None, C_None)), { new }
  {A_CMPXCHG} (Ch: (C_All, C_None, C_None)), { new }
  {A_CMPXCHG486} (Ch: (C_All, C_None, C_None)), { new }
  {A_CMPXCHG8B} (Ch: (C_All, C_None, C_None)), { new }
  {A_CPUID} (Ch: (C_All, C_None, C_none)),
  {A_CWD} (Ch: (C_MEAX, C_WEDX, C_None)),
  {A_CWDE} (Ch: (C_MEAX, C_None, C_None)),
  {A_DAA} (Ch: (C_MEAX, C_None, C_None)),
  {A_DAS} (Ch: (C_MEAX, C_None, C_None)),
  {A_DEC} (Ch: (C_Mop1, C_WFlags, C_None)),
  {A_DIV} (Ch: (C_RWEAX, C_WEDX, C_WFlags)),
  {A_EMMS} (Ch: (C_FPU, C_None, C_None)), { new }
  {A_ENTER} (Ch: (C_RWESP, C_None, C_None)),
  {A_EQU} (Ch: (C_None, C_None, C_None)), { new }
  {A_F2XM1} (Ch: (C_FPU, C_None, C_None)),
  {A_FABS} (Ch: (C_FPU, C_None, C_None)),
  {A_FADD} (Ch: (C_FPU, C_None, C_None)),
  {A_FADDP} (Ch: (C_FPU, C_None, C_None)),
  {A_FBLD} (Ch: (C_Rop1, C_FPU, C_None)),
  {A_FBSTP} (Ch: (C_Wop1, C_FPU, C_None)),
  {A_FCHS} (Ch: (C_FPU, C_None, C_None)),
  {A_FCLEX} (Ch: (C_FPU, C_None, C_None)),
  {A_FCMOVB} (Ch: (C_FPU, C_RFLAGS, C_None)), { new }
  {A_FCMOVBE} (Ch: (C_FPU, C_RFLAGS, C_None)), { new }
  {A_FCMOVE} (Ch: (C_FPU, C_RFLAGS, C_None)), { new }
  {A_FCMOVNB} (Ch: (C_FPU, C_RFLAGS, C_None)), { new }
  {A_FCMOVNBE} (Ch: (C_FPU, C_RFLAGS, C_None)), { new }
  {A_FCMOVNE} (Ch: (C_FPU, C_RFLAGS, C_None)), { new }
  {A_FCMOVNU} (Ch: (C_FPU, C_RFLAGS, C_None)), { new }
  {A_FCMOVU} (Ch: (C_FPU, C_RFLAGS, C_None)), { new }
  {A_FCOM} (Ch: (C_FPU, C_None, C_None)),
  {A_FCOMI} (Ch: (C_WFLAGS, C_None, C_None)), { new }
  {A_FCOMIP} (Ch: (C_FPU, C_WFLAGS, C_None)), { new }
  {A_FCOMP} (Ch: (C_FPU, C_None, C_None)),
  {A_FCOMPP} (Ch: (C_FPU, C_None, C_None)),
  {A_FCOS} (Ch: (C_FPU, C_None, C_None)),
  {A_FDECSTP} (Ch: (C_FPU, C_None, C_None)),
  {A_FDISI} (Ch: (C_FPU, C_None, C_None)),
  {A_FDIV} (Ch: (C_FPU, C_None, C_None)),
  {A_FDIVP} (Ch: (C_FPU, C_None, C_None)),
  {A_FDIVR} (Ch: (C_FPU, C_None, C_None)),
  {A_FDIVRP} (Ch: (C_FPU, C_None, C_None)),
  {A_FEMMS} (Ch: (C_All, C_None, C_None)), { new }
  {A_FENI} (Ch: (C_FPU, C_None, C_None)),
  {A_FFREE} (Ch: (C_FPU, C_None, C_None)),
  {A_FIADD} (Ch: (C_FPU, C_None, C_None)),
  {A_FICOM} (Ch: (C_FPU, C_None, C_None)),
  {A_FICOMP} (Ch: (C_FPU, C_None, C_None)),
  {A_FIDIV} (Ch: (C_FPU, C_None, C_None)),
  {A_FIDIVR} (Ch: (C_FPU, C_None, C_None)),
  {A_FILD} (Ch: (C_FPU, C_None, C_None)),
  {A_FIMUL} (Ch: (C_FPU, C_None, C_None)),
  {A_FINCSTP} (Ch: (C_FPU, C_None, C_None)),
  {A_FINIT} (Ch: (C_FPU, C_None, C_None)),
  {A_FIST} (Ch: (C_Wop1, C_None, C_None)),
  {A_FISTP} (Ch: (C_Wop1, C_None, C_None)),
  {A_FISUB} (Ch: (C_FPU, C_None, C_None)),
  {A_FISUBR} (Ch: (C_FPU, C_None, C_None)), { new }
  {A_FLD} (Ch: (C_Rop1, C_FPU, C_None)),
  {A_FLD1} (Ch: (C_FPU, C_None, C_None)),
  {A_FLDCW} (Ch: (C_FPU, C_None, C_None)),
  {A_FLDENV} (Ch: (C_FPU, C_None, C_None)),
  {A_FLDL2E} (Ch: (C_FPU, C_None, C_None)),
  {A_FLDL2T} (Ch: (C_FPU, C_None, C_None)),
  {A_FLDLG2} (Ch: (C_FPU, C_None, C_None)),
  {A_FLDLN2} (Ch: (C_FPU, C_None, C_None)),
  {A_FLDPI} (Ch: (C_FPU, C_None, C_None)),
  {A_FLDZ} (Ch: (C_FPU, C_None, C_None)),
  {A_FMUL} (Ch: (C_FPU, C_None, C_None)),
  {A_FMULP} (Ch: (C_FPU, C_None, C_None)),
  {A_FNCLEX} (Ch: (C_FPU, C_None, C_None)),
  {A_FNDISI} (Ch: (C_FPU, C_None, C_None)),
  {A_FNENI} (Ch: (C_FPU, C_None, C_None)),
  {A_FNINIT} (Ch: (C_FPU, C_None, C_None)),
  {A_FNOP} (Ch: (C_FPU, C_None, C_None)),
  {A_FNSAVE} (Ch: (C_FPU, C_None, C_None)),
  {A_FNSTCW} (Ch: (C_Wop1, C_None, C_None)),
  {A_FNSTENV} (Ch: (C_Wop1, C_None, C_None)),
  {A_FNSTSW} (Ch: (C_Wop1, C_None, C_None)),
  {A_FPATAN} (Ch: (C_FPU, C_None, C_None)),
  {A_FPREM} (Ch: (C_FPU, C_None, C_None)),
  {A_FPREM1} (Ch: (C_FPU, C_None, C_None)),
  {A_FPTAN} (Ch: (C_FPU, C_None, C_None)),
  {A_FRNDINT} (Ch: (C_FPU, C_None, C_None)),
  {A_FRSTOR} (Ch: (C_FPU, C_None, C_None)),
  {A_FSAVE} (Ch: (C_Wop1, C_None, C_None)),
  {A_FSCALE} (Ch: (C_FPU, C_None, C_None)),
  {A_FSETPM} (Ch: (C_FPU, C_None, C_None)),
  {A_FSIN} (Ch: (C_FPU, C_None, C_None)),
  {A_FSINCOS} (Ch: (C_FPU, C_None, C_None)),
  {A_FSQRT} (Ch: (C_FPU, C_None, C_None)),
  {A_FST} (Ch: (C_Wop1, C_None, C_None)),
  {A_FSTCW} (Ch: (C_Wop1, C_None, C_None)),
  {A_FSTENV} (Ch: (C_Wop1, C_None, C_None)),
  {A_FSTP} (Ch: (C_Wop1, C_FPU, C_None)),
  {A_FSTSW} (Ch: (C_Wop1, C_None, C_None)),
  {A_FSUB} (Ch: (C_FPU, C_None, C_None)),
  {A_FSUBP} (Ch: (C_FPU, C_None, C_None)),
  {A_FSUBR} (Ch: (C_FPU, C_None, C_None)),
  {A_FSUBRP} (Ch: (C_FPU, C_None, C_None)),
  {A_FTST} (Ch: (C_FPU, C_None, C_None)),
  {A_FUCOM} (Ch: (C_None, C_None, C_None)), {changes fpu status word}
  {A_FUCOMI} (Ch: (C_WFLAGS, C_None, C_None)), { new }
  {A_FUCOMIP} (Ch: (C_FPU, C_WFLAGS, C_None)), { new }
  {A_FUCOMP} (Ch: (C_FPU, C_None, C_None)),
  {A_FUCOMPP} (Ch: (C_FPU, C_None, C_None)),
  {A_FWAIT} (Ch: (C_FPU, C_None, C_None)),
  {A_FXAM} (Ch: (C_FPU, C_None, C_None)),
  {A_FXCH} (Ch: (C_FPU, C_None, C_None)),
  {A_FXTRACT} (Ch: (C_FPU, C_None, C_None)),
  {A_FYL2X} (Ch: (C_FPU, C_None, C_None)),
  {A_FYL2XP1} (Ch: (C_FPU, C_None, C_None)),
  {A_HLT} (Ch: (C_None, C_None, C_None)),
  {A_IBTS} (Ch: (C_All, C_None, C_None)), { new }
  {A_ICEBP} (Ch: (C_All, C_None, C_None)), { new }
  {A_IDIV} (Ch: (C_RWEAX, C_WEDX, C_WFlags)), {handled separately, because modifies more than three things}
  {A_IMUL} (Ch: (C_RWEAX, C_WEDX, C_WFlags)), {handled separately, because several forms exist}
  {A_IN} (Ch: (C_Wop2, C_Rop1, C_None)),
  {A_INC} (Ch: (C_Mop1, C_WFlags, C_None)),
  {A_INSB} (Ch: (C_WMemEDI, C_RWEDI, C_None)), { new }
  {A_INSD} (Ch: (C_WMemEDI, C_RWEDI, C_None)), { new }
  {A_INSW} (Ch: (C_WMemEDI, C_RWEDI, C_None)), { new }
  {A_INT} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {A_INT01} (Ch: (C_All, C_None, C_None)), { new }
  {A_INT1} (Ch: (C_All, C_None, C_None)), { new }
  {A_INT3} (Ch: (C_None, C_None, C_None)),
  {A_INTO} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {A_INVD} (Ch: (C_All, C_None, C_None)), { new }
  {A_INVLPG} (Ch: (C_All, C_None, C_None)), { new }
  {A_IRET} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {A_IRETD} (Ch: (C_All, C_None, C_None)), { new }
  {A_IRETW} (Ch: (C_All, C_None, C_None)), { new }
  {A_JCXZ} (Ch: (C_RECX, C_None, C_None)),
  {A_JECXZ} (Ch: (C_RECX, C_None, C_None)),
  {A_JMP} (Ch: (C_None, C_None, C_None)),
  {A_LAHF} (Ch: (C_WEAX, C_RFlags, C_None)),
  {A_LAR} (Ch: (C_Wop2, C_None, C_None)),
  {A_LDS} (Ch: (C_Wop2, C_None, C_None)),
  {A_LEA} (Ch: (C_Wop2, C_Rop1, C_None)),
  {A_LEAVE} (Ch: (C_RWESP, C_None, C_None)),
  {A_LES} (Ch: (C_Wop2, C_None, C_None)),
  {A_LFS} (Ch: (C_Wop2, C_None, C_None)),
  {A_LGDT} (Ch: (C_None, C_None, C_None)),
  {A_LGS} (Ch: (C_Wop2, C_None, C_None)),
  {A_LIDT} (Ch: (C_None, C_None, C_None)),
  {A_LLDT} (Ch: (C_None, C_None, C_None)),
  {A_LMSW} (Ch: (C_None, C_None, C_None)),
  {A_LOADALL} (Ch: (C_All, C_None, C_None)), { new }
  {A_LOADALL286} (Ch: (C_All, C_None, C_None)), { new }
  {A_LODSB} (Ch: (C_WEAX, C_RWESI, C_None)), { new }
  {A_LODSD} (Ch: (C_WEAX, C_RWESI, C_None)), { new }
  {A_LODSW} (Ch: (C_WEAX, C_RWESI, C_None)), { new }
  {A_LOOP} (Ch: (C_RWECX, C_None, C_None)),
  {A_LOOPE} (Ch: (C_RWECX, C_RFlags, C_None)),
  {A_LOOPNE} (Ch: (C_RWECX, C_RFlags, C_None)),
  {A_LOOPNZ} (Ch: (C_RWECX, C_RFlags, C_None)),
  {A_LOOPZ} (Ch: (C_RWECX, C_RFlags, C_None)),
  {A_LSL} (Ch: (C_Wop2, C_WFlags, C_None)),
  {A_LSS} (Ch: (C_Wop2, C_None, C_None)),
  {A_LTR} (Ch: (C_None, C_None, C_None)),
  {A_MOV} (Ch: (C_Wop2, C_Rop1, C_None)),
  {A_MOVD} (Ch: (C_All, C_None, C_None)), { new }
  {A_MOVQ} (Ch: (C_All, C_None, C_None)), { new }
  {A_MOVSB} (Ch: (C_All, C_Rop1, C_None)),
  {A_MOVSD} (Ch: (C_All, C_None, C_None)), { new }
  {A_MOVSW} (Ch: (C_All, C_None, C_None)), { new }
  {A_MOVSX} (Ch: (C_Wop2, C_Rop1, C_None)),
  {A_MOVZX} (Ch: (C_Wop2, C_Rop1, C_None)),
  {A_MUL} (Ch: (C_RWEAX, C_WEDX, C_WFlags)), {handled separately, because modifies more than three things}
  {A_NEG} (Ch: (C_Mop1, C_None, C_None)),
  {A_NOP} (Ch: (C_None, C_None, C_None)),
  {A_NOT} (Ch: (C_Mop1, C_WFlags, C_None)),
  {A_OR} (Ch: (C_Mop2, C_WFlags, C_None)),
  {A_OUT} (Ch: (C_Rop1, C_Rop2, C_None)),
  {A_OUTSB} (Ch: (C_All, C_None, C_None)), { new }
  {A_OUTSD} (Ch: (C_All, C_None, C_None)), { new }
  {A_OUTSW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PACKSSDW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PACKSSWB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PACKUSWB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PADDB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PADDD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PADDSB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PADDSIW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PADDSW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PADDUSB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PADDUSW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PADDW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PAND} (Ch: (C_All, C_None, C_None)), { new }
  {A_PANDN} (Ch: (C_All, C_None, C_None)), { new }
  {A_PAVEB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PAVGUSB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PCMPEQB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PCMPEQD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PCMPEQW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PCMPGTB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PCMPGTD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PCMPGTW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PDISTIB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PF2ID} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFACC} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFADD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFCMPEQ} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFCMPGE} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFCMPGT} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFMAX} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFMIN} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFMUL} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFRCP} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFRCPIT1} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFRCPIT2} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFRSQIT1} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFRSQRT} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFSUB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PFSUBR} (Ch: (C_All, C_None, C_None)), { new }
  {A_PI2FD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMACHRIW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMADDWD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMAGW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMULHRIW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMULHRWA} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMULHRWC} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMULHW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMULLW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMVGEZB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMVLZB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMVNZB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PMVZB} (Ch: (C_All, C_None, C_None)), { new }
  {A_POP} (Ch: (C_Wop1, C_RWESP, C_None)),
  {A_POPA} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {A_POPAD} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {A_POPAW} (Ch: (C_All, C_None, C_None)), { new }
  {A_POPF} (Ch: (C_RWESP, C_WFlags, C_None)),
  {A_POPFD} (Ch: (C_RWESP, C_WFlags, C_None)),
  {A_POPFW} (Ch: (C_RWESP, C_WFLAGS, C_None)), { new }
  {A_POR} (Ch: (C_All, C_None, C_None)), { new }
  {A_PREFETCH} (Ch: (C_All, C_None, C_None)), { new }
  {A_PREFETCHW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSLLD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSLLQ} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSLLW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSRAD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSRAW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSRLD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSRLQ} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSRLW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSUBB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSUBD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSUBSB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSUBSIW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSUBSW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSUBUSB} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSUBUSW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PSUBW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PUNPCKHBW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PUNPCKHDQ} (Ch: (C_All, C_None, C_None)), { new }
  {A_PUNPCKHWD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PUNPCKLBW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PUNPCKLDQ} (Ch: (C_All, C_None, C_None)), { new }
  {A_PUNPCKLWD} (Ch: (C_All, C_None, C_None)), { new }
  {A_PUSH} (Ch: (C_Rop1, C_RWESP, C_None)),
  {A_PUSHA} (Ch: (C_All, C_None, C_None)),
  {A_PUSHAD} (Ch: (C_All, C_None, C_None)),
  {A_PUSHAW} (Ch: (C_All, C_None, C_None)), { new }
  {A_PUSHF} (Ch: (C_RWESP, C_RFlags, C_None)),
  {A_PUSHFD} (Ch: (C_RWESP, C_RFlags, C_None)),
  {A_PUSHFW} (Ch: (C_RWESP, C_RFLAGS, C_None)), { new }
  {A_PXOR} (Ch: (C_All, C_None, C_None)), { new }
  {A_RCL} (Ch: (C_Mop2, C_Rop1, C_RWFlags)),
  {A_RCR} (Ch: (C_Mop2, C_Rop1, C_RWFlags)),
  {A_RDMSR} (Ch: (C_WEAX, C_WEDX, C_None)), { new }
  {A_RDPMC} (Ch: (C_WEAX, C_WEDX, C_None)), { new }
  {A_RDTSC} (Ch: (C_WEAX, C_WEDX, C_None)), { new }
  {A_RESB} (Ch: (C_All, C_None, C_None)), { new }
  {A_RET} (Ch: (C_All, C_None, C_None)),
  {A_RETF} (Ch: (C_All, C_None, C_None)), { new }
  {A_RETN} (Ch: (C_All, C_None, C_None)), { new }
  {A_ROL} (Ch: (C_Mop2, C_Rop1, C_RWFlags)),
  {A_ROR} (Ch: (C_Mop2, C_Rop1, C_RWFlags)),
  {A_RSM} (Ch: (C_All, C_None, C_None)), { new }
  {A_SAHF} (Ch: (C_WFlags, C_REAX, C_None)),
  {A_SAL} (Ch: (C_Mop2, C_Rop1, C_RWFlags)),
  {A_SALC} (Ch: (C_WEAX, C_RFLAGS, C_None)), { new }
  {A_SAR} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_SBB} (Ch: (C_Mop2, C_Rop1, C_RWFlags)),
  {A_SCASB} (Ch: (C_All, C_None, C_None)), { new }
  {A_SCASD} (Ch: (C_All, C_None, C_None)), { new }
  {A_SCASW} (Ch: (C_All, C_None, C_None)), { new }
  {A_SGDT} (Ch: (C_Wop1, C_None, C_None)),
  {A_SHL} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_SHLD} (Ch: (C_MOp3, C_RWFlags, C_Rop2)),
  {A_SHR} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_SHRD} (Ch: (C_MOp3, C_RWFlags, C_Rop2)),
  {A_SIDT} (Ch: (C_Wop1, C_None, C_None)),
  {A_SLDT} (Ch: (C_Wop1, C_None, C_None)),
  {A_SMI} (Ch: (C_All, C_None, C_None)), { new }
  {A_SMSW} (Ch: (C_Wop1, C_None, C_None)),
  {A_STC} (Ch: (C_WFlags, C_None, C_None)),
  {A_STD} (Ch: (C_SDirFlag, C_None, C_None)),
  {A_STI} (Ch: (C_WFlags, C_None, C_None)),
  {A_STOSB} (Ch: (C_REAX, C_WMemEDI, C_RWEDI)), { new }
  {A_STOSD} (Ch: (C_REAX, C_WMemEDI, C_RWEDI)), { new }
  {A_STOSW} (Ch: (C_REAX, C_WMemEDI, C_RWEDI)), { new }
  {A_STR}  (Ch: (C_Wop1, C_None, C_None)),
  {A_SUB} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_TEST} (Ch: (C_WFlags, C_Rop1, C_Rop2)),
  {A_UMOV} (Ch: (C_All, C_None, C_None)), { new }
  {A_VERR} (Ch: (C_WFlags, C_None, C_None)),
  {A_VERW} (Ch: (C_WFlags, C_None, C_None)),
  {A_WAIT} (Ch: (C_None, C_None, C_None)),
  {A_WBINVD} (Ch: (C_None, C_None, C_None)), { new }
  {A_WRMSR} (Ch: (C_All, C_None, C_None)), { new }
  {A_XADD} (Ch: (C_All, C_None, C_None)), { new }
  {A_XBTS} (Ch: (C_All, C_None, C_None)), { new }
  {A_XCHG} (Ch: (C_RWop1, C_RWop2, C_None)), {(might be) handled seperately}
  {A_XLAT} (Ch: (C_WEAX, C_REBX, C_None)),
  {A_XLATB} (Ch: (C_WEAX, C_REBX, C_None)),
  {A_XOR} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_CMOV} (Ch: (C_ROp1, C_WOp2, C_RFLAGS)), { new }
  {A_J} (Ch: (C_None, C_None, C_None)), { new }
  {A_SET} (Ch: (C_WEAX, C_RFLAGS, C_None))  { new }
  );


Function RegMaxSize(Reg: TRegister): TRegister;
Function RegsSameSize(Reg1, Reg2: TRegister): Boolean;
Function IsLoadInstr(p: pai): Boolean;

Implementation

{************************ TCondRegs ************************}

Constructor TCondRegs.init;
Begin
  FillChar(Flags, SizeOf(Flags), Byte(F_Unknown))
End;

Procedure TCondRegs.InitFlag(f: TFlag);
Begin
  Flags[f] := F_Unknown
End;

Procedure TCondRegs.SetFlag(f: TFlag);
Begin
  Flags[f] := F_Set
End;

Procedure TCondRegs.ClearFlag(f: TFlag);
Begin
  Flags[f] : =F_Clear
End;

Function GetFlag(f: TFlag): TFlagContents;
Begin
  GetFlag := Flags[f]
End;

{*************************************************************}

Function RegMaxSize(Reg: TRegister): TRegister;
{Returns the 32 bit component of Reg if it exists, otherwise Reg is returned}
Begin
  RegMaxSize := Reg;
  If (Reg >= R_AX)
    Then
      If (Reg <= R_DI)
        Then RegMaxSize := Reg16ToReg32(Reg)
        Else
          If (Reg <= R_BL)
            Then RegMaxSize := Reg8toReg32(Reg);
End;

Function RegsSameSize(Reg1, Reg2: TRegister): Boolean;
{returns true if Reg1 and Reg2 are of the same size (so if they're both
 8bit, 16bit or 32bit)}
Begin
  If (Reg1 <= R_EDI)
    Then RegsSameSize := (Reg2 <= R_EDI)
    Else
      If (Reg1 <= R_DI)
        Then RegsSameSize := (Reg2 in [R_AX..R_DI])
        Else
          If (Reg1 <= R_BL)
            Then RegsSameSize := (Reg2 in [R_AL..R_BL])
            Else RegsSameSize := False
End;

Function IsLoadInstr(p: pai): Boolean;
{ returns true if p is a load instruction (loads value from memory in a     }
{ register)                                                                 }
Begin
  IsLoadInstr :=
    (p^.typ = ait_instruction) and
    ((PInstr(p)^.OpCode = A_MOV) or
     (PInstr(p)^.OpCode = A_MOVZX) or
     (PInstr(p)^.OpCode = A_MOVSX)) And
    (PInstr(p)^.oper[LoadSrc].typ = top_ref));
End;

Function IsStoreInstr(p: pai): Boolean;
{ returns true if p is a load instruction (loads value from memory in a     }
{ register)                                                                 }
Begin
  IsLoadInstr :=
    (p^.typ = ait_instruction) and
    ((PInstr(p)^.OpCode = A_MOV) or
     (PInstr(p)^.OpCode = A_MOVZX) or
     (PInstr(p)^.OpCode = A_MOVSX)) And
    (PInstr(p)^.oper[StoreDst].typ = top_ref));
End;


Function TCh2Reg(Ch: TChange): TRegister;
{converts a TChange variable to a TRegister}
Begin
  If (Ch <= C_REDI) Then
    TCh2Reg := TRegister(Byte(Ch))
  Else
    If (Ch <= C_WEDI) Then
      TCh2Reg := TRegister(Byte(Ch) - Byte(C_REDI))
    Else
      If (Ch <= C_RWEDI) Then
        TCh2Reg := TRegister(Byte(Ch) - Byte(C_WEDI))
      Else InternalError($db)
End;

Function RegReadByInstr(Reg: TRegister; p: Pai);
Begin
  RegReadByInstr := False;
  If (p^.typ = ait_instruction) Then
    Case p^.opcode of
      A_IMUL:
        With PInstr(p)^ Do
          RegReadByInstr :=
            RegInOp(Reg,op[0]) or
            RegInOp(Reg,op[1]) or
            ((op[1]^.typ = top_none) and
             (op[2]^.typ = top_none) and
             (reg in [R_EAX,R_EDX]))
      A_IDIV, A_MUL:
         RegReadByInstr :=
           RegInOp(Reg,op[0]) or
           (Reg = R_EAX) or
           ((Reg = R_EDX) and
            (p^.opcode = A_IDIV) and
            (p^.size = S_L));



End.