{
    $Id$
    Copyright (c) 1997-98 by Jonas Maebe

    This unit contains the data flow analyzer and several helper procedures
    and functions.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

{$ifDef TP}
  {$UnDef JumpAnal}
{$Endif TP}

Unit DAOpt386;

Interface

Uses
  GlobType,
  CObjects,Aasm,
  cpubase,cpuasm;

Type
  TRegArray = Array[R_EAX..R_BL] of TRegister;
  TRegSet = Set of R_EAX..R_BL;
  TRegInfo = Record
                NewRegsEncountered, OldRegsEncountered: TRegSet;
                RegsLoadedForRef: TRegSet;
                New2OldReg: TRegArray;
              End;

{possible actions on an operand: read, write or modify (= read & write)}
  TOpAction = (OpAct_Read, OpAct_Write, OpAct_Modify, OpAct_Unknown);

{*********************** Procedures and Functions ************************}

Procedure InsertLLItem(AsmL: PAasmOutput; prev, foll, new_one: PLinkedList_Item);

Function Reg32(Reg: TRegister): TRegister;
Function RefsEquivalent(Const R1, R2: TReference; Var RegInfo: TRegInfo; OpAct: TOpAction): Boolean;
Function RefsEqual(Const R1, R2: TReference): Boolean;
Function IsGP32Reg(Reg: TRegister): Boolean;
Function RegInRef(Reg: TRegister; Const Ref: TReference): Boolean;
Function RegInInstruction(Reg: TRegister; p1: Pai): Boolean;
Function RegModifiedByInstruction(Reg: TRegister; p1: Pai): Boolean;

Function GetNextInstruction(Current: Pai; Var Next: Pai): Boolean;
Function GetLastInstruction(Current: Pai; Var Last: Pai): Boolean;
Procedure SkipHead(var P: Pai);

Procedure UpdateUsedRegs(Var UsedRegs: TRegSet; p: Pai);
Function RegsEquivalent(OldReg, NewReg: TRegister; Var RegInfo: TRegInfo; OpAct: TopAction): Boolean;
Function InstructionsEquivalent(p1, p2: Pai; Var RegInfo: TRegInfo): Boolean;
Function OpsEqual(const o1,o2:toper): Boolean;

Function DFAPass1(AsmL: PAasmOutput; BlockStart: Pai): Pai;
Function DFAPass2(
{$ifdef statedebug}
                   AsmL: PAasmOutPut;
{$endif statedebug}
                                      BlockStart, BlockEnd: Pai): Boolean;
Procedure ShutDownDFA;

Function FindLabel(L: PasmLabel; Var hp: Pai): Boolean;

{******************************* Constants *******************************}

Const

{ait_* types which don't result in executable code or which don't influence
 the way the program runs/behaves}

  SkipInstr = [ait_comment, ait_align, ait_symbol
{$ifdef GDB}
  ,ait_stabs, ait_stabn, ait_stab_function_name
{$endif GDB}
  ,ait_regalloc, ait_tempalloc
  ];

{the maximum number of things (registers, memory, ...) a single instruction
 changes}

  MaxCh = 3;

{Possible register content types}
  con_Unknown = 0;
  con_ref = 1;
  con_const = 2;

{********************************* Types *********************************}

Type

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

{$ifndef arithopt}
Const
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

Type
{$endif arithopt}

{the possible states of a flag}
  TFlagContents = (F_Unknown, F_NotSet, F_Set);

{the properties of a cpu instruction}
  TAsmInstrucProp = Record
               {how many things it changes}
{                         NCh: Byte;}
               {and what it changes}
                         Ch: Array[1..MaxCh] of TChange;
                       End;

  TContent = Packed Record
      {start and end of block instructions that defines the
       content of this register. If Typ = con_const, then
       Longint(StartMod) = value of the constant)}
               StartMod: pai;
      {starts at 0, gets increased everytime the register is written to}
               WState: Byte;
      {starts at 0, gets increased everytime the register is read from}
               RState: Byte;
      {how many instructions starting with StarMod does the block consist of}
               NrOfMods: Byte;
      {the type of the content of the register: unknown, memory, constant}
               Typ: Byte;
             End;

{Contents of the integer registers}
  TRegContent = Array[R_EAX..R_EDI] Of TContent;

{contents of the FPU registers}
  TRegFPUContent = Array[R_ST..R_ST7] Of TContent;

{information record with the contents of every register. Every Pai object
 gets one of these assigned: a pointer to it is stored in the OptInfo field}
  TPaiProp = Record
               Regs: TRegContent;
{               FPURegs: TRegFPUContent;} {currently not yet used}
    {allocated Registers}
               UsedRegs: TRegSet;
    {status of the direction flag}
               DirFlag: TFlagContents;
    {can this instruction be removed?}
               CanBeRemoved: Boolean;
             End;

  PPaiProp = ^TPaiProp;

{$IfNDef TP}
  TPaiPropBlock = Array[1..250000] Of TPaiProp;
  PPaiPropBlock = ^TPaiPropBlock;
{$EndIf TP}

  TInstrSinceLastMod = Array[R_EAX..R_EDI] Of Byte;

  TLabelTableItem = Record
                      PaiObj: Pai;
{$IfDef JumpAnal}
                      InstrNr: Longint;
                      RefsFound: Word;
                      JmpsProcessed: Word
{$EndIf JumpAnal}
                    End;
{$IfDef tp}
  TLabelTable = Array[0..10000] Of TLabelTableItem;
{$Else tp}
  TLabelTable = Array[0..2500000] Of TLabelTableItem;
{$Endif tp}
  PLabelTable = ^TLabelTable;

{******************************* Variables *******************************}

Var
{the amount of PaiObjects in the current assembler list}
  NrOfPaiObjs: Longint;

{$IfNDef TP}
{Array which holds all TPaiProps}
  PaiPropBlock: PPaiPropBlock;
{$EndIf TP}

  LoLab, HiLab, LabDif: Longint;

  LTable: PLabelTable;

{*********************** End of Interface section ************************}


Implementation

Uses
  globals, systems, strings, verbose, hcodegen;

Const AsmInstr: Array[tasmop] Of TAsmInstrucProp = (
  {A_<NONE>} (Ch: (C_All, C_None, C_None)), { new }
  {A_LOCK} (Ch: (C_None, C_None, C_None)),
 { the repCC instructions don't write to the flags themselves, but since  }
 { they loop as long as CC is not fulfilled, it's possible that after the }
 { repCC instructions the flags have changed                              }
  {A_REP} (Ch: (C_RWECX, C_RWFlags, C_None)),
  {A_REPE} (Ch: (C_RWECX, C_RWFlags, C_None)),
  {A_REPNE} (Ch: (C_RWECX, C_RWFlags, C_None)),
  {A_REPNZ} (Ch: (C_RWECX, C_RWFLAGS, C_None)), { new }
  {A_REPZ} (Ch: (C_RWECX, C_RWFLAGS, C_None)), { new }
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
  {A_DIV} (Ch: (C_RWEAX, C_WEDX, C_WFlags)), {handled separately, because modifies more than three things}
  {A_EMMS} (Ch: (C_FPU, C_None, C_None)), { new }
  {A_ENTER} (Ch: (C_RWESP, C_None, C_None)),
  {A_EQU} (Ch: (C_ALL, C_None, C_None)), { new }
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
  {A_FMUL} (Ch: (C_ROp1, C_FPU, C_None)),
  {A_FMULP} (Ch: (C_ROp1, C_FPU, C_None)),
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
  {A_FSUB} (Ch: (C_ROp1, C_FPU, C_None)),
  {A_FSUBP} (Ch: (C_ROp1, C_FPU, C_None)),
  {A_FSUBR} (Ch: (C_ROp1, C_FPU, C_None)),
  {A_FSUBRP} (Ch: (C_ROp1, C_FPU, C_None)),
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
  {A_INSB} (Ch: (C_WMemEDI, C_RWEDI, C_REDX)), { new }
  {A_INSD} (Ch: (C_WMemEDI, C_RWEDI, C_REDX)), { new }
  {A_INSW} (Ch: (C_WMemEDI, C_RWEDI, C_REDX)), { new }
  {A_INT} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {A_INT01} (Ch: (C_All, C_None, C_None)), { new }
  {A_INT1} (Ch: (C_All, C_None, C_None)), { new }
{!!!} {A_INT03} (Ch: (C_None, C_None, C_None)),
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
{!!!}  {A_RDSHR} (Ch: (C_All, C_None, C_None)), { new }
  {A_RDMSR} (Ch: (C_WEAX, C_WEDX, C_None)), { new }
  {A_RDPMC} (Ch: (C_WEAX, C_WEDX, C_None)), { new }
  {A_RDTSC} (Ch: (C_WEAX, C_WEDX, C_None)), { new }
  {A_RESB} (Ch: (C_All, C_None, C_None)), { new }
  {A_RET} (Ch: (C_All, C_None, C_None)),
  {A_RETF} (Ch: (C_All, C_None, C_None)), { new }
  {A_RETN} (Ch: (C_All, C_None, C_None)), { new }
  {A_ROL} (Ch: (C_Mop2, C_Rop1, C_RWFlags)),
  {A_ROR} (Ch: (C_Mop2, C_Rop1, C_RWFlags)),
{!!!}  {A_RSDC} (Ch: (C_All, C_None, C_None)), { new }
{!!!}  {A_RSLDT} (Ch: (C_All, C_None, C_None)), { new }
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
{!!!}  {A_SMINT} (Ch: (C_All, C_None, C_None)), { new }
{!!!}  {A_SMINTOLD} (Ch: (C_All, C_None, C_None)), { new }
  {A_SMSW} (Ch: (C_Wop1, C_None, C_None)),
  {A_STC} (Ch: (C_WFlags, C_None, C_None)),
  {A_STD} (Ch: (C_SDirFlag, C_None, C_None)),
  {A_STI} (Ch: (C_WFlags, C_None, C_None)),
  {A_STOSB} (Ch: (C_REAX, C_WMemEDI, C_RWEDI)), { new }
  {A_STOSD} (Ch: (C_REAX, C_WMemEDI, C_RWEDI)), { new }
  {A_STOSW} (Ch: (C_REAX, C_WMemEDI, C_RWEDI)), { new }
  {A_STR}  (Ch: (C_Wop1, C_None, C_None)),
  {A_SUB} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
{!!!}  {A_SVDC} (Ch: (C_All, C_None, C_None)), { new }
{!!!}  {A_SVLDT} (Ch: (C_All, C_None, C_None)), { new }
{!!!}  {A_SVTS} (Ch: (C_All, C_None, C_None)), { new }
{!!!}  {A_SYSCALL} (Ch: (C_All, C_None, C_None)), { new }
{!!!}  {A_SYSENTER} (Ch: (C_All, C_None, C_None)), { new }
{!!!}  {A_SYSEXIT} (Ch: (C_All, C_None, C_None)), { new }
{!!!}  {A_SYSRET} (Ch: (C_All, C_None, C_None)), { new }
  {A_TEST} (Ch: (C_WFlags, C_Rop1, C_Rop2)),
{!!!}  {A_UD1} (Ch: (C_All, C_None, C_None)), { new }
{!!!}  {A_UD2} (Ch: (C_All, C_None, C_None)), { new }
  {A_UMOV} (Ch: (C_All, C_None, C_None)), { new }
  {A_VERR} (Ch: (C_WFlags, C_None, C_None)),
  {A_VERW} (Ch: (C_WFlags, C_None, C_None)),
  {A_WAIT} (Ch: (C_None, C_None, C_None)),
  {A_WBINVD} (Ch: (C_None, C_None, C_None)), { new }
{!!!}  {A_WRSHR} (Ch: (C_All, C_None, C_None)), { new }
  {A_WRMSR} (Ch: (C_All, C_None, C_None)), { new }
  {A_XADD} (Ch: (C_All, C_None, C_None)), { new }
  {A_XBTS} (Ch: (C_All, C_None, C_None)), { new }
  {A_XCHG} (Ch: (C_RWop1, C_RWop2, C_None)), {(might be) handled seperately}
  {A_XLAT} (Ch: (C_WEAX, C_REBX, C_None)),
  {A_XLATB} (Ch: (C_WEAX, C_REBX, C_None)),
  {A_XOR} (Ch: (C_Mop2, C_Rop1, C_WFlags)),
  {A_CMOV} (Ch: (C_ROp1, C_WOp2, C_RFLAGS)), { new }
  {A_J} (Ch: (C_None, C_None, C_None)), { new }
  {A_SET} (Ch: (C_WEAX, C_RFLAGS, C_None)),  { new }
{!!!! From here everything is new !!!!!!!!}
  {ADDPS} (Ch: (C_All, C_None, C_None)), { new }
  {ADDSS} (Ch: (C_All, C_None, C_None)), { new }
  {ANDNPS} (Ch: (C_All, C_None, C_None)), { new }
  {ANDPS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPEQPS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPEQSS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPLEPS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPLESS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPLTPS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPLTSS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPNEQPS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPNEQSS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPNLEPS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPNLESS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPNLTPS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPNLTSS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPORDPS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPORDSS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPUNORDPS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPUNORDSS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPPS} (Ch: (C_All, C_None, C_None)), { new }
  {CMPSS} (Ch: (C_All, C_None, C_None)), { new }
  {COMISS} (Ch: (C_All, C_None, C_None)), { new }
  {CVTPI2PS} (Ch: (C_All, C_None, C_None)), { new }
  {CVTPS2PI} (Ch: (C_All, C_None, C_None)), { new }
  {CVTSI2SS} (Ch: (C_All, C_None, C_None)), { new }
  {CVTSS2SI} (Ch: (C_All, C_None, C_None)), { new }
  {CVTTPS2PI} (Ch: (C_All, C_None, C_None)), { new }
  {CVTTSS2SI} (Ch: (C_All, C_None, C_None)), { new }
  {DIVPS} (Ch: (C_All, C_None, C_None)), { new }
  {DIVSS} (Ch: (C_All, C_None, C_None)), { new }
  {LDMXCSR} (Ch: (C_All, C_None, C_None)), { new }
  {MAXPS} (Ch: (C_All, C_None, C_None)), { new }
  {MAXSS} (Ch: (C_All, C_None, C_None)), { new }
  {MINPS} (Ch: (C_All, C_None, C_None)), { new }
  {MINSS} (Ch: (C_All, C_None, C_None)), { new }
  {MOVAPS} (Ch: (C_All, C_None, C_None)), { new }
  {MOVHPS} (Ch: (C_All, C_None, C_None)), { new }
  {MOVLHPS} (Ch: (C_All, C_None, C_None)), { new }
  {MOVLPS} (Ch: (C_All, C_None, C_None)), { new }
  {MOVHLPS} (Ch: (C_All, C_None, C_None)), { new }
  {MOVMSKPS} (Ch: (C_All, C_None, C_None)), { new }
  {MOVNTPS} (Ch: (C_All, C_None, C_None)), { new }
  {MOVSS} (Ch: (C_All, C_None, C_None)), { new }
  {MOVUPS} (Ch: (C_All, C_None, C_None)), { new }
  {MULPS} (Ch: (C_All, C_None, C_None)), { new }
  {MULSS} (Ch: (C_All, C_None, C_None)), { new }
  {ORPS} (Ch: (C_All, C_None, C_None)), { new }
  {RCPPS} (Ch: (C_All, C_None, C_None)), { new }
  {RCPSS} (Ch: (C_All, C_None, C_None)), { new }
  {RSQRTPS} (Ch: (C_All, C_None, C_None)), { new }
  {RSQRTSS} (Ch: (C_All, C_None, C_None)), { new }
  {SHUFPS} (Ch: (C_All, C_None, C_None)), { new }
  {SQRTPS} (Ch: (C_All, C_None, C_None)), { new }
  {SQRTSS} (Ch: (C_All, C_None, C_None)), { new }
  {STMXCSR} (Ch: (C_All, C_None, C_None)), { new }
  {SUBPS} (Ch: (C_All, C_None, C_None)), { new }
  {SUBSS} (Ch: (C_All, C_None, C_None)), { new }
  {UCOMISS} (Ch: (C_All, C_None, C_None)), { new }
  {UNPCKHPS} (Ch: (C_All, C_None, C_None)), { new }
  {UNPCKLPS} (Ch: (C_All, C_None, C_None)), { new }
  {XORPS} (Ch: (C_All, C_None, C_None)), { new }
  {FXRSTOR} (Ch: (C_All, C_None, C_None)), { new }
  {FXSAVE} (Ch: (C_All, C_None, C_None)), { new }
  {PREFETCHNTA} (Ch: (C_All, C_None, C_None)), { new }
  {PREFETCHT0} (Ch: (C_All, C_None, C_None)), { new }
  {PREFETCHT1} (Ch: (C_All, C_None, C_None)), { new }
  {PREFETCHT2} (Ch: (C_All, C_None, C_None)), { new }
  {SFENCE} (Ch: (C_All, C_None, C_None)), { new }
  {MASKMOVQ} (Ch: (C_All, C_None, C_None)), { new }
  {MOVNTQ} (Ch: (C_All, C_None, C_None)), { new }
  {PAVGB} (Ch: (C_All, C_None, C_None)), { new }
  {PAVGW} (Ch: (C_All, C_None, C_None)), { new }
  {PEXTRW} (Ch: (C_All, C_None, C_None)), { new }
  {PINSRW} (Ch: (C_All, C_None, C_None)), { new }
  {PMAXSW} (Ch: (C_All, C_None, C_None)), { new }
  {PMAXUB} (Ch: (C_All, C_None, C_None)), { new }
  {PMINSW} (Ch: (C_All, C_None, C_None)), { new }
  {PMINUB} (Ch: (C_All, C_None, C_None)), { new }
  {PMOVMSKB} (Ch: (C_All, C_None, C_None)), { new }
  {PMULHUW} (Ch: (C_All, C_None, C_None)), { new }
  {PSADBW} (Ch: (C_All, C_None, C_None)), { new }
  {PSHUFW} (Ch: (C_All, C_None, C_None)) { new }
  );

Var
 {How many instructions are between the current instruction and the last one
  that modified the register}
  NrOfInstrSinceLastMod: TInstrSinceLastMod;


{************************ Create the Label table ************************}

Function FindLoHiLabels(Var LowLabel, HighLabel, LabelDif: Longint; BlockStart: Pai): Pai;
{Walks through the paasmlist to find the lowest and highest label number}
Var LabelFound: Boolean;
    P: Pai;
Begin
  LabelFound := False;
  LowLabel := MaxLongint;
  HighLabel := 0;
  P := BlockStart;
  While Assigned(P) And
        ((P^.typ <> Ait_Marker) Or
         (Pai_Marker(P)^.Kind <> AsmBlockStart)) Do
    Begin
      If (Pai(p)^.typ = ait_label) Then
        If (Pai_Label(p)^.l^.is_used)
          Then
            Begin
              LabelFound := True;
              If (Pai_Label(p)^.l^.labelnr < LowLabel) Then
                LowLabel := Pai_Label(p)^.l^.labelnr;
              If (Pai_Label(p)^.l^.labelnr > HighLabel) Then
                HighLabel := Pai_Label(p)^.l^.labelnr;
            End;
      GetNextInstruction(p, p);
    End;
  FindLoHiLabels := p;
  If LabelFound
    Then LabelDif := HighLabel+1-LowLabel
    Else LabelDif := 0;
End;

Function FindRegAlloc(Reg: TRegister; StartPai: Pai): Boolean;
{Returns true if a ait_alloc object for Reg is found in the block of Pai's
 starting with StartPai and ending with the next "real" instruction}
Begin
  FindRegAlloc:=False;
  Repeat
    While Assigned(StartPai) And
          ((StartPai^.typ in (SkipInstr - [ait_regAlloc])) Or
           ((StartPai^.typ = ait_label) and
            Not(Pai_Label(StartPai)^.l^.Is_Used))) Do
      StartPai := Pai(StartPai^.Next);
    If Assigned(StartPai) And
       (StartPai^.typ = ait_regAlloc) and (PairegAlloc(StartPai)^.allocation) Then
      Begin
        if PairegAlloc(StartPai)^.Reg = Reg then
         begin
           FindRegAlloc:=true;
           exit;
         end;
        StartPai := Pai(StartPai^.Next);
      End
    else
      exit;
  Until false;
End;


Procedure BuildLabelTableAndFixRegAlloc(AsmL: PAasmOutput; Var LabelTable: PLabelTable; LowLabel: Longint;
            Var LabelDif: Longint; BlockStart, BlockEnd: Pai);
{Builds a table with the locations of the labels in the paasmoutput.
 Also fixes some RegDeallocs like "# %eax released; push (%eax)"}
Var p, hp1, hp2: Pai;
    UsedRegs: TRegSet;
Begin
  UsedRegs := [];
  If (LabelDif <> 0) Then
    Begin
{$IfDef TP}
      If (MaxAvail >= LabelDif*SizeOf(Pai))
        Then
          Begin
{$EndIf TP}
            GetMem(LabelTable, LabelDif*SizeOf(TLabelTableItem));
            FillChar(LabelTable^, LabelDif*SizeOf(TLabelTableItem), 0);
            p := BlockStart;
            While (P <> BlockEnd) Do
              Begin
                Case p^.typ Of
                  ait_Label:
                    If Pai_Label(p)^.l^.is_used Then
                      LabelTable^[Pai_Label(p)^.l^.labelnr-LowLabel].PaiObj := p;
                  ait_regAlloc:
                     begin
                       if PairegAlloc(p)^.Allocation then
                        Begin
                          If Not(PaiRegAlloc(p)^.Reg in UsedRegs) Then
                            UsedRegs := UsedRegs + [PaiRegAlloc(p)^.Reg]
                          Else
                            Begin
                              hp1 := p;
                              hp2 := nil;
                              While GetLastInstruction(hp1, hp1) And
                                    Not(RegInInstruction(PaiRegAlloc(p)^.Reg, hp1)) Do
                                hp2 := hp1;
                             If hp2 <> nil Then
                               Begin
                                 hp1 := New(PaiRegAlloc, DeAlloc(PaiRegAlloc(p)^.Reg));
                                 InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp1);
                               End;
                            End;
                        End
                       else
                        Begin
                          UsedRegs := UsedRegs - [PaiRegAlloc(p)^.Reg];
                          hp1 := p;
                          hp2 := nil;
                          While Not(FindRegAlloc(PaiRegAlloc(p)^.Reg, Pai(hp1^.Next))) And
                                GetNextInstruction(hp1, hp1) And
                                RegInInstruction(PaiRegAlloc(p)^.Reg, hp1) Do
                            hp2 := hp1;
                          If hp2 <> nil Then
                            Begin
                              hp1 := Pai(p^.previous);
                              AsmL^.Remove(p);
                              InsertLLItem(AsmL, hp2, Pai(hp2^.Next), p);
                              p := hp1;
                            End;
                        End;
                     end;
                End;
                P := Pai(p^.Next);
                While Assigned(p) And
                      (p^.typ in (SkipInstr - [ait_regalloc])) Do
                  P := Pai(P^.Next);
              End;
{$IfDef TP}
          End
        Else LabelDif := 0;
{$EndIf TP}
    End;
End;

{************************ Search the Label table ************************}

Function FindLabel(L: PasmLabel; Var hp: Pai): Boolean;

{searches for the specified label starting from hp as long as the
 encountered instructions are labels, to be able to optimize constructs like

 jne l2              jmp l2
 jmp l3     and      l1:
 l1:                 l2:
 l2:}

Var TempP: Pai;

Begin
  TempP := hp;
  While Assigned(TempP) and
       (TempP^.typ In SkipInstr + [ait_label]) Do
    If (TempP^.typ <> ait_Label) Or
       (pai_label(TempP)^.l <> L)
      Then GetNextInstruction(TempP, TempP)
      Else
        Begin
          hp := TempP;
          FindLabel := True;
          exit
        End;
  FindLabel := False;
End;

{************************ Some general functions ************************}

Function Reg32(Reg: TRegister): TRegister;
{Returns the 32 bit component of Reg if it exists, otherwise Reg is returned}
Begin
  Reg32 := Reg;
  If (Reg >= R_AX)
    Then
      If (Reg <= R_DI)
        Then Reg32 := Reg16ToReg32(Reg)
        Else
          If (Reg <= R_BL)
            Then Reg32 := Reg8toReg32(Reg);
End;

{ inserts new_one between prev and foll }
Procedure InsertLLItem(AsmL: PAasmOutput; prev, foll, new_one: PLinkedList_Item);
Begin
  If Assigned(prev) Then
    If Assigned(foll) Then
      Begin
        If Assigned(new_one) Then
          Begin
            new_one^.previous := prev;
            new_one^.next := foll;
            prev^.next := new_one;
            foll^.previous := new_one;
            Pai(new_one)^.fileinfo := Pai(foll)^.fileinfo;
          End;
      End
    Else AsmL^.Concat(new_one)
  Else If Assigned(Foll) Then AsmL^.Insert(new_one)
End;

{********************* Compare parts of Pai objects *********************}

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

Procedure AddReg2RegInfo(OldReg, NewReg: TRegister; Var RegInfo: TRegInfo);
{updates the ???RegsEncountered and ???2???Reg fields of RegInfo. Assumes that
 OldReg and NewReg have the same size (has to be chcked in advance with
 RegsSameSize) and that neither equals R_NO}
Begin
  With RegInfo Do
    Begin
      NewRegsEncountered := NewRegsEncountered + [NewReg];
      OldRegsEncountered := OldRegsEncountered + [OldReg];
      New2OldReg[NewReg] := OldReg;
      Case OldReg Of
        R_EAX..R_EDI:
          Begin
            NewRegsEncountered := NewRegsEncountered + [Reg32toReg16(NewReg)];
            OldRegsEncountered := OldRegsEncountered + [Reg32toReg16(OldReg)];
            New2OldReg[Reg32toReg16(NewReg)] := Reg32toReg16(OldReg);
            If (NewReg in [R_EAX..R_EBX]) And
               (OldReg in [R_EAX..R_EBX]) Then
              Begin
                NewRegsEncountered := NewRegsEncountered + [Reg32toReg8(NewReg)];
                OldRegsEncountered := OldRegsEncountered + [Reg32toReg8(OldReg)];
                New2OldReg[Reg32toReg8(NewReg)] := Reg32toReg8(OldReg);
              End;
          End;
        R_AX..R_DI:
          Begin
            NewRegsEncountered := NewRegsEncountered + [Reg16toReg32(NewReg)];
            OldRegsEncountered := OldRegsEncountered + [Reg16toReg32(OldReg)];
            New2OldReg[Reg16toReg32(NewReg)] := Reg16toReg32(OldReg);
            If (NewReg in [R_AX..R_BX]) And
               (OldReg in [R_AX..R_BX]) Then
              Begin
                NewRegsEncountered := NewRegsEncountered + [Reg16toReg8(NewReg)];
                OldRegsEncountered := OldRegsEncountered + [Reg16toReg8(OldReg)];
                New2OldReg[Reg16toReg8(NewReg)] := Reg16toReg8(OldReg);
              End;
          End;
        R_AL..R_BL:
          Begin
            NewRegsEncountered := NewRegsEncountered + [Reg8toReg32(NewReg)]
                               + [Reg8toReg16(NewReg)];
            OldRegsEncountered := OldRegsEncountered + [Reg8toReg32(OldReg)]
                               + [Reg8toReg16(OldReg)];
            New2OldReg[Reg8toReg32(NewReg)] := Reg8toReg32(OldReg);
          End;
      End;
    End;
End;

Procedure AddOp2RegInfo(const o:Toper; Var RegInfo: TRegInfo);
Begin
  Case o.typ Of
    Top_Reg:
      If (o.reg <> R_NO) Then
        AddReg2RegInfo(o.reg, o.reg, RegInfo);
    Top_Ref:
      Begin
        If o.ref^.base <> R_NO Then
          AddReg2RegInfo(o.ref^.base, o.ref^.base, RegInfo);
        If o.ref^.index <> R_NO Then
          AddReg2RegInfo(o.ref^.index, o.ref^.index, RegInfo);
      End;
  End;
End;


Function RegsEquivalent(OldReg, NewReg: TRegister; Var RegInfo: TRegInfo; OPAct: TOpAction): Boolean;
Begin
  If Not((OldReg = R_NO) Or (NewReg = R_NO)) Then
    If RegsSameSize(OldReg, NewReg) Then
      With RegInfo Do
{here we always check for the 32 bit component, because it is possible that
 the 8 bit component has not been set, event though NewReg already has been
 processed. This happens if it has been compared with a register that doesn't
 have an 8 bit component (such as EDI). In that case the 8 bit component is
 still set to R_NO and the comparison in the Else-part will fail}
        If (Reg32(OldReg) in OldRegsEncountered) Then
          If (Reg32(NewReg) in NewRegsEncountered) Then
            RegsEquivalent := (OldReg = New2OldReg[NewReg])

 { If we haven't encountered the new register yet, but we have encountered the
   old one already, the new one can only be correct if it's being written to
   (and consequently the old one is also being written to), otherwise

   movl -8(%ebp), %eax        and         movl -8(%ebp), %eax
   movl (%eax), %eax                      movl (%edx), %edx

   are considered equivalent}

          Else
            If (OpAct = OpAct_Write) Then
              Begin
                AddReg2RegInfo(OldReg, NewReg, RegInfo);
                RegsEquivalent := True
              End
            Else Regsequivalent := False
        Else
          If Not(Reg32(NewReg) in NewRegsEncountered) Then
            Begin
              AddReg2RegInfo(OldReg, NewReg, RegInfo);
              RegsEquivalent := True
            End
          Else RegsEquivalent := False
    Else RegsEquivalent := False
  Else RegsEquivalent := OldReg = NewReg
End;

Function RefsEquivalent(Const R1, R2: TReference; var RegInfo: TRegInfo; OpAct: TOpAction): Boolean;
Begin
  If R1.is_immediate Then
    RefsEquivalent := R2.is_immediate and (R1.Offset = R2.Offset)
  Else
    RefsEquivalent := (R1.Offset+R1.OffsetFixup = R2.Offset+R2.OffsetFixup) And
                      RegsEquivalent(R1.Base, R2.Base, RegInfo, OpAct) And
                      RegsEquivalent(R1.Index, R2.Index, RegInfo, OpAct) And
                      (R1.Segment = R2.Segment) And (R1.ScaleFactor = R2.ScaleFactor) And
                      (R1.Symbol = R2.Symbol);
End;


Function RefsEqual(Const R1, R2: TReference): Boolean;
Begin
  If R1.is_immediate Then
    RefsEqual := R2.is_immediate and (R1.Offset = R2.Offset)
  Else
    RefsEqual := (R1.Offset+R1.OffsetFixup = R2.Offset+R2.OffsetFixup) And
                 (R1.Segment = R2.Segment) And (R1.Base = R2.Base) And
                 (R1.Index = R2.Index) And (R1.ScaleFactor = R2.ScaleFactor) And
                 (R1.Symbol=R2.Symbol);
End;

Function IsGP32Reg(Reg: TRegister): Boolean;
{Checks if the register is a 32 bit general purpose register}
Begin
  If (Reg >= R_EAX) and (Reg <= R_EBX)
    Then IsGP32Reg := True
    Else IsGP32reg := False
End;

Function RegInRef(Reg: TRegister; Const Ref: TReference): Boolean;
Begin {checks whether Ref contains a reference to Reg}
  Reg := Reg32(Reg);
  RegInRef := (Ref.Base = Reg) Or (Ref.Index = Reg)
End;

Function RegInInstruction(Reg: TRegister; p1: Pai): Boolean;
{checks if Reg is used by the instruction p1}
Var Counter: Longint;
    TmpResult: Boolean;
Begin
  TmpResult := False;
  If (Pai(p1)^.typ = ait_instruction) Then
    Begin
      Reg := Reg32(Reg);
      Counter := 0;
      Repeat
        Case Pai386(p1)^.oper[Counter].typ Of
          Top_Reg: TmpResult := Reg = Reg32(Pai386(p1)^.oper[Counter].reg);
          Top_Ref: TmpResult := RegInRef(Reg, Pai386(p1)^.oper[Counter].ref^);
        End;
        Inc(Counter)
      Until (Counter = 3) or TmpResult;
    End;
  RegInInstruction := TmpResult
End;

{Function RegInOp(Reg: TRegister; const o:toper): Boolean;
Begin
  RegInOp := False;
  Case opt Of
    top_reg: RegInOp := Reg = o.reg;
    top_ref: RegInOp := (Reg = o.ref^.Base) Or
                        (Reg = o.ref^.Index);
  End;
End;}

Function RegModifiedByInstruction(Reg: TRegister; p1: Pai): Boolean;
{returns true if Reg is modified by the instruction p1. P1 is assumed to be
 of the type ait_instruction}
Var hp: Pai;
Begin
  If GetLastInstruction(p1, hp)
    Then
      RegModifiedByInstruction :=
        PPAiProp(p1^.OptInfo)^.Regs[Reg].WState <>
          PPAiProp(hp^.OptInfo)^.Regs[Reg].WState
    Else RegModifiedByInstruction := True;
End;

{********************* GetNext and GetLastInstruction *********************}
Function GetNextInstruction(Current: Pai; Var Next: Pai): Boolean;
{skips ait_regalloc, ait_regdealloc and ait_stab* objects and puts the
 next pai object in Next. Returns false if there isn't any}
Begin
  Repeat
    Current := Pai(Current^.Next);
    While Assigned(Current) And
          ((Current^.typ In SkipInstr) or
           ((Current^.typ = ait_label) And
            Not(Pai_Label(Current)^.l^.is_used))) Do
      Current := Pai(Current^.Next);
    If Assigned(Current) And
       (Current^.typ = ait_Marker) And
       (Pai_Marker(Current)^.Kind = NoPropInfoStart) Then
      Begin
        While Assigned(Current) And
              ((Current^.typ <> ait_Marker) Or
               (Pai_Marker(Current)^.Kind <> NoPropInfoEnd)) Do
          Current := Pai(Current^.Next);
      End;
  Until Not(Assigned(Current)) Or
        (Current^.typ <> ait_Marker) Or
        (Pai_Marker(Current)^.Kind <> NoPropInfoEnd);
  Next := Current;
  If Assigned(Current) And
     Not((Current^.typ In SkipInstr) or
         ((Current^.typ = ait_label) And
          Not(Pai_Label(Current)^.l^.is_used)))
    Then GetNextInstruction := True
    Else
      Begin
        Next := Nil;
        GetNextInstruction := False;
      End;
End;

Function GetLastInstruction(Current: Pai; Var Last: Pai): Boolean;
{skips the ait-types in SkipInstr puts the previous pai object in
 Last. Returns false if there isn't any}
Begin
  Repeat
    Current := Pai(Current^.previous);
    While Assigned(Current) And
          (((Current^.typ = ait_Marker) And
            Not(Pai_Marker(Current)^.Kind in [AsmBlockEnd,NoPropInfoEnd])) or
           (Current^.typ In SkipInstr) or
           ((Current^.typ = ait_label) And
             Not(Pai_Label(Current)^.l^.is_used))) Do
      Current := Pai(Current^.previous);
    If Assigned(Current) And
       (Current^.typ = ait_Marker) And
       (Pai_Marker(Current)^.Kind = NoPropInfoEnd) Then
      Begin
        While Assigned(Current) And
              ((Current^.typ <> ait_Marker) Or
               (Pai_Marker(Current)^.Kind <> NoPropInfoStart)) Do
          Current := Pai(Current^.previous);
      End;
  Until Not(Assigned(Current)) Or
        (Current^.typ <> ait_Marker) Or
        (Pai_Marker(Current)^.Kind <> NoPropInfoStart);
  If Not(Assigned(Current)) or
     (Current^.typ In SkipInstr) or
     ((Current^.typ = ait_label) And
      Not(Pai_Label(Current)^.l^.is_used)) or
     ((Current^.typ = ait_Marker) And
      (Pai_Marker(Current)^.Kind = AsmBlockEnd))
    Then
      Begin
        Last := Nil;
        GetLastInstruction := False
      End
    Else
      Begin
        Last := Current;
        GetLastInstruction := True;
      End;
End;

Procedure SkipHead(var P: Pai);
Var OldP: Pai;
Begin
  Repeat
    OldP := P;
    If (P^.typ in SkipInstr) Or
       ((P^.typ = ait_marker) And
        (Pai_Marker(P)^.Kind = AsmBlockEnd)) Then
      GetNextInstruction(P, P)
    Else If ((P^.Typ = Ait_Marker) And
        (Pai_Marker(P)^.Kind = NoPropInfoStart)) Then
   {a marker of the NoPropInfoStart can't be the first instruction of a
    paasmoutput list}
      GetNextInstruction(Pai(P^.Previous),P);
    If (P^.Typ = Ait_Marker) And
       (Pai_Marker(P)^.Kind = AsmBlockStart) Then
      Begin
        P := Pai(P^.Next);
        While (P^.typ <> Ait_Marker) Or
              (Pai_Marker(P)^.Kind <> AsmBlockEnd) Do
          P := Pai(P^.Next)
      End;
    Until P = OldP
End;
{******************* The Data Flow Analyzer functions ********************}

Procedure UpdateUsedRegs(Var UsedRegs: TRegSet; p: Pai);
{updates UsedRegs with the RegAlloc Information coming after P}
Begin
  Repeat
    While Assigned(p) And
          ((p^.typ in (SkipInstr - [ait_RegAlloc])) or
           ((p^.typ = ait_label) And
            Not(Pai_Label(p)^.l^.is_used))) Do
         p := Pai(p^.next);
    While Assigned(p) And
          (p^.typ=ait_RegAlloc) Do
      Begin
        if pairegalloc(p)^.allocation then
          UsedRegs := UsedRegs + [PaiRegAlloc(p)^.Reg]
        else
          UsedRegs := UsedRegs - [PaiRegAlloc(p)^.Reg];
        p := pai(p^.next);
      End;
  Until Not(Assigned(p)) Or
        (Not(p^.typ in SkipInstr) And
         Not((p^.typ = ait_label) And
            Not(Pai_Label(p)^.l^.is_used)));
End;


(*Function FindZeroreg(p: Pai; Var Result: TRegister): Boolean;
{Finds a register which contains the constant zero}
Var Counter: TRegister;
Begin
  Counter := R_EAX;
  FindZeroReg := True;
  While (Counter <= R_EDI) And
        ((PPaiProp(p^.OptInfo)^.Regs[Counter].Typ <> Con_Const) or
         (PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod <> Pointer(0))) Do
    Inc(Byte(Counter));
  If (PPaiProp(p^.OptInfo)^.Regs[Counter].Typ = Con_Const) And
     (PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod = Pointer(0))
    Then Result := Counter
    Else FindZeroReg := False;
End;*)

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
      Else
        If (Ch <= C_MEDI) Then
          TCh2Reg := TRegister(Byte(Ch) - Byte(C_RWEDI))
        Else InternalError($db)
End;

Procedure IncState(Var S: Byte);
{Increases S by 1, wraps around at $ffff to 0 (so we won't get overflow
 errors}
Begin
  If (s <> $ff)
    Then Inc(s)
    Else s := 0
End;

Function RegInSequence(Reg: TRegister; Const Content: TContent): Boolean;
{checks the whole sequence of Content (so StartMod and and the next NrOfMods
 Pai objects) to see whether Reg is used somewhere, without it being loaded
 with something else first}
Var p: Pai;
    Counter: Byte;
    TmpResult: Boolean;
    RegsChecked: TRegSet;
Begin
  RegsChecked := [];
  p := Content.StartMod;
  TmpResult := False;
  Counter := 1;
  While Not(TmpResult) And
        (Counter <= Content.NrOfMods) Do
    Begin
      If (p^.typ = ait_instruction) and
         ((Pai386(p)^.opcode = A_MOV) or
          (Pai386(p)^.opcode = A_MOVZX) or
          (Pai386(p)^.opcode = A_MOVSX))
        Then
          Begin
            If (Pai386(p)^.oper[0].typ = top_ref) Then
              With Pai386(p)^.oper[0].ref^ Do
                If (Base = ProcInfo.FramePointer) And
                   (Index = R_NO)
                  Then
                    Begin
                      RegsChecked := RegsChecked + [Reg32(Pai386(p)^.oper[1].reg)];
                      If Reg = Reg32(Pai386(p)^.oper[1].reg) Then
                        Break;
                    End
                  Else
                    Begin
                      If (Base = Reg) And
                         Not(Base In RegsChecked)
                        Then TmpResult := True;
                      If Not(TmpResult) And
                         (Index = Reg) And
                           Not(Index In RegsChecked)
                        Then TmpResult := True;
                    End
          End
        Else TmpResult := RegInInstruction(Reg, p);
      Inc(Counter);
      GetNextInstruction(p,p)
    End;
  RegInSequence := TmpResult
End;

Procedure DestroyReg(p1: PPaiProp; Reg: TRegister);
{Destroys the contents of the register Reg in the PPaiProp p1, as well as the
 contents of registers are loaded with a memory location based on Reg}
Var TmpWState, TmpRState: Byte;
    Counter: TRegister;
Begin
  Reg := Reg32(Reg);
  NrOfInstrSinceLastMod[Reg] := 0;
  If (Reg >= R_EAX) And (Reg <= R_EDI)
    Then
      Begin
        With p1^.Regs[Reg] Do
          Begin
            IncState(WState);
            TmpWState := WState;
            TmpRState := RState;
            FillChar(p1^.Regs[Reg], SizeOf(TContent), 0);
            WState := TmpWState;
            RState := TmpRState;
          End;
        For Counter := R_EAX to R_EDI Do
          With p1^.Regs[Counter] Do
            If (Typ = Con_Ref) And
               RegInSequence(Reg, p1^.Regs[Counter])
              Then
                Begin
                  IncState(WState);
                  TmpWState := WState;
                  TmpRState := RState;
                  FillChar(p1^.Regs[Counter], SizeOf(TContent), 0);
                  WState := TmpWState;
                  RState := TmpRState;
                End;
       End;
End;

{Procedure AddRegsToSet(p: Pai; Var RegSet: TRegSet);
Begin
  If (p^.typ = ait_instruction) Then
    Begin
      Case Pai386(p)^.oper[0].typ Of
        top_reg:
          If Not(Pai386(p)^.oper[0].reg in [R_NO,R_ESP,ProcInfo.FramePointer]) Then
            RegSet := RegSet + [Pai386(p)^.oper[0].reg];
        top_ref:
          With TReference(Pai386(p)^.oper[0]^) Do
            Begin
              If Not(Base in [ProcInfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Base];
              If Not(Index in [ProcInfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Index];
            End;
      End;
      Case Pai386(p)^.oper[1].typ Of
        top_reg:
          If Not(Pai386(p)^.oper[1].reg in [R_NO,R_ESP,ProcInfo.FramePointer]) Then
            If RegSet := RegSet + [TRegister(TwoWords(Pai386(p)^.oper[1]).Word1];
        top_ref:
          With TReference(Pai386(p)^.oper[1]^) Do
            Begin
              If Not(Base in [ProcInfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Base];
              If Not(Index in [ProcInfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Index];
            End;
      End;
    End;
End;}

Function OpsEquivalent(const o1, o2: toper; Var RegInfo: TRegInfo; OpAct: TopAction): Boolean;
Begin {checks whether the two ops are equivalent}
  OpsEquivalent := False;
  if o1.typ=o2.typ then
    Case o1.typ Of
      Top_Reg:
        OpsEquivalent :=RegsEquivalent(o1.reg,o2.reg, RegInfo, OpAct);
      Top_Ref:
        OpsEquivalent := RefsEquivalent(o1.ref^, o2.ref^, RegInfo, OpAct);
      Top_Const:
        OpsEquivalent := o1.val = o2.val;
      Top_None:
        OpsEquivalent := True
    End;
End;


Function OpsEqual(const o1,o2:toper): Boolean;
Begin {checks whether the two ops are equal}
  OpsEqual := False;
  if o1.typ=o2.typ then
    Case o1.typ Of
      Top_Reg :
        OpsEqual:=o1.reg=o2.reg;
      Top_Ref :
        OpsEqual := RefsEqual(o1.ref^, o2.ref^);
      Top_Const :
        OpsEqual:=o1.val=o2.val;
      Top_Symbol :
        OpsEqual:=(o1.sym=o2.sym) and (o1.symofs=o2.symofs);
      Top_None :
        OpsEqual := True
    End;
End;

Function InstructionsEquivalent(p1, p2: Pai; Var RegInfo: TRegInfo): Boolean;
{$ifdef csdebug}
var hp: pai;
{$endif csdebug}
Begin {checks whether two Pai386 instructions are equal}
  If Assigned(p1) And Assigned(p2) And
     (Pai(p1)^.typ = ait_instruction) And
     (Pai(p1)^.typ = ait_instruction) And
     (Pai386(p1)^.opcode = Pai386(p2)^.opcode) And
     (Pai386(p1)^.oper[0].typ = Pai386(p2)^.oper[0].typ) And
     (Pai386(p1)^.oper[1].typ = Pai386(p2)^.oper[1].typ) And
     (Pai386(p1)^.oper[2].typ = Pai386(p2)^.oper[2].typ)
    Then
 {both instructions have the same structure:
  "<operator> <operand of type1>, <operand of type 2>"}
      If ((Pai386(p1)^.opcode = A_MOV) or
          (Pai386(p1)^.opcode = A_MOVZX) or
          (Pai386(p1)^.opcode = A_MOVSX)) And
         (Pai386(p1)^.oper[0].typ = top_ref) {then .oper[1]t = top_reg} Then
        If Not(RegInRef(Pai386(p1)^.oper[1].reg, Pai386(p1)^.oper[0].ref^)) Then
 {the "old" instruction is a load of a register with a new value, not with
  a value based on the contents of this register (so no "mov (reg), reg")}
          If Not(RegInRef(Pai386(p2)^.oper[1].reg, Pai386(p2)^.oper[0].ref^)) And
             RefsEqual(Pai386(p1)^.oper[0].ref^, Pai386(p2)^.oper[0].ref^)
            Then
 {the "new" instruction is also a load of a register with a new value, and
  this value is fetched from the same memory location}
              Begin
                With Pai386(p2)^.oper[0].ref^ Do
                  Begin
                    If Not(Base in [ProcInfo.FramePointer, R_NO, R_ESP])
       {it won't do any harm if the register is already in RegsLoadedForRef}
                      Then RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Base];
                    If Not(Index in [ProcInfo.FramePointer, R_NO, R_ESP])
                      Then RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Index];
                  End;
 {add the registers from the reference (.oper[0]) to the RegInfo, all registers
  from the reference are the same in the old and in the new instruction
  sequence}
                AddOp2RegInfo(Pai386(p1)^.oper[0], RegInfo);
 {the registers from .oper[1] have to be equivalent, but not necessarily equal}
                InstructionsEquivalent :=
                  RegsEquivalent(Pai386(p1)^.oper[1].reg, Pai386(p2)^.oper[1].reg, RegInfo, OpAct_Write);
              End
 {the registers are loaded with values from different memory locations. If
  this was allowed, the instructions "mov -4(esi),eax" and "mov -4(ebp),eax"
  would be considered equivalent}
            Else InstructionsEquivalent := False
        Else
 {load register with a value based on the current value of this register}
          Begin
            With Pai386(p2)^.oper[0].ref^ Do
              Begin
                If Not(Base in [ProcInfo.FramePointer,
                                Reg32(Pai386(p2)^.oper[1].reg),R_NO,R_ESP])
 {it won't do any harm if the register is already in RegsLoadedForRef}
                  Then
                    Begin
                      RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Base];
{$ifdef csdebug}
                      Writeln(att_reg2str[base], ' added');
{$endif csdebug}
                    end;
                If Not(Index in [ProcInfo.FramePointer,
                                 Reg32(Pai386(p2)^.oper[1].reg),R_NO,R_ESP])
                  Then
                    Begin
                      RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Index];
{$ifdef csdebug}
                      Writeln(att_reg2str[index], ' added');
{$endif csdebug}
                    end;

              End;
            If Not(Reg32(Pai386(p2)^.oper[1].reg) In [ProcInfo.FramePointer,R_NO,R_ESP])
              Then
                Begin
                  RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef -
                                                 [Reg32(Pai386(p2)^.oper[1].reg)];
{$ifdef csdebug}
                  Writeln(att_reg2str[Reg32(Pai386(p2)^.oper[1].reg)], ' removed');
{$endif csdebug}
                end;
            InstructionsEquivalent :=
               OpsEquivalent(Pai386(p1)^.oper[0], Pai386(p2)^.oper[0], RegInfo, OpAct_Read) And
               OpsEquivalent(Pai386(p1)^.oper[1], Pai386(p2)^.oper[1], RegInfo, OpAct_Write)
          End
      Else
 {an instruction <> mov, movzx, movsx}
       begin
  {$ifdef csdebug}
         hp := new(pai_asm_comment,init(strpnew('checking if equivalent')));
         hp^.previous := p2;
         hp^.next := p2^.next;
         p2^.next^.previous := hp;
         p2^.next := hp;
  {$endif csdebug}
         InstructionsEquivalent :=
           OpsEquivalent(Pai386(p1)^.oper[0], Pai386(p2)^.oper[0], RegInfo, OpAct_Unknown) And
           OpsEquivalent(Pai386(p1)^.oper[1], Pai386(p2)^.oper[1], RegInfo, OpAct_Unknown) And
           OpsEquivalent(Pai386(p1)^.oper[2], Pai386(p2)^.oper[2], RegInfo, OpAct_Unknown)
       end
 {the instructions haven't even got the same structure, so they're certainly
  not equivalent}
    Else
      begin
  {$ifdef csdebug}
        hp := new(pai_asm_comment,init(strpnew('different opcodes/format')));
        hp^.previous := p2;
        hp^.next := p2^.next;
        p2^.next^.previous := hp;
        p2^.next := hp;
  {$endif csdebug}
        InstructionsEquivalent := False;
      end;
  {$ifdef csdebug}
    hp := new(pai_asm_comment,init(strpnew('instreq: '+tostr(byte(instructionsequivalent)))));
    hp^.previous := p2;
    hp^.next := p2^.next;
    p2^.next^.previous := hp;
    p2^.next := hp;
  {$endif csdebug}
End;

(*
Function InstructionsEqual(p1, p2: Pai): Boolean;
Begin {checks whether two Pai386 instructions are equal}
  InstructionsEqual :=
    Assigned(p1) And Assigned(p2) And
    ((Pai(p1)^.typ = ait_instruction) And
     (Pai(p1)^.typ = ait_instruction) And
     (Pai386(p1)^.opcode = Pai386(p2)^.opcode) And
     (Pai386(p1)^.oper[0].typ = Pai386(p2)^.oper[0].typ) And
     (Pai386(p1)^.oper[1].typ = Pai386(p2)^.oper[1].typ) And
     OpsEqual(Pai386(p1)^.oper[0].typ, Pai386(p1)^.oper[0], Pai386(p2)^.oper[0]) And
     OpsEqual(Pai386(p1)^.oper[1].typ, Pai386(p1)^.oper[1], Pai386(p2)^.oper[1]))
End;
*)

Function RefInInstruction(Const Ref: TReference; p: Pai): Boolean;
{checks whehter Ref is used in P}
Var TmpResult: Boolean;
Begin
  TmpResult := False;
  If (p^.typ = ait_instruction) Then
    Begin
      If (Pai386(p)^.oper[0].typ = Top_Ref) Then
        TmpResult := RefsEqual(Ref, Pai386(p)^.oper[0].ref^);
      If Not(TmpResult) And (Pai386(p)^.oper[1].typ = Top_Ref) Then
        TmpResult := RefsEqual(Ref, Pai386(p)^.oper[1].ref^);
    End;
  RefInInstruction := TmpResult;
End;

Function RefInSequence(Const Ref: TReference; Content: TContent): Boolean;
{checks the whole sequence of Content (so StartMod and and the next NrOfMods
 Pai objects) to see whether Ref is used somewhere}
Var p: Pai;
    Counter: Byte;
    TmpResult: Boolean;
Begin
  p := Content.StartMod;
  TmpResult := False;
  Counter := 1;
  While Not(TmpResult) And
        (Counter <= Content.NrOfMods) Do
    Begin
      If (p^.typ = ait_instruction) And
         RefInInstruction(Ref, p)
        Then TmpResult := True;
      Inc(Counter);
      GetNextInstruction(p,p)
    End;
  RefInSequence := TmpResult
End;

Procedure DestroyRefs(p: pai; Const Ref: TReference; WhichReg: TRegister);
{destroys all registers which possibly contain a reference to Ref, WhichReg
 is the register whose contents are being written to memory (if this proc
 is called because of a "mov?? %reg, (mem)" instruction)}
Var Counter: TRegister;
Begin
  WhichReg := Reg32(WhichReg);
  If ((Ref.base = ProcInfo.FramePointer) And
      (Ref.Index = R_NO)) Or
     Assigned(Ref.Symbol)
    Then
{write something to a parameter, a local or global variable, so
   * with uncertzain optimizations on:
      - destroy the contents of registers whose contents have somewhere a
        "mov?? (Ref), %reg". WhichReg (this is the register whose contents
        are being written to memory) is not destroyed if it's StartMod is
        of that form and NrOfMods = 1 (so if it holds ref, but is not a
        pointer based on Ref)
    * with uncertain optimizations off:
       - also destroy registers that contain any pointer}
      For Counter := R_EAX to R_EDI Do
        With PPaiProp(p^.OptInfo)^.Regs[Counter] Do
          Begin
            If (typ = Con_Ref) And
               ((Not(cs_UncertainOpts in aktglobalswitches) And
                 (NrOfMods <> 1)
                ) Or
                (RefInSequence(Ref,PPaiProp(p^.OptInfo)^.Regs[Counter]) And
                 ((Counter <> WhichReg) Or
                  ((NrOfMods <> 1) And
 {StarMod is always of the type ait_instruction}
                   (Pai386(StartMod)^.oper[0].typ = top_ref) And
                   RefsEqual(Pai386(StartMod)^.oper[0].ref^, Ref)
                  )
                 )
                )
               )
              Then
                DestroyReg(PPaiProp(p^.OptInfo), Counter)
          End
    Else
{write something to a pointer location, so
   * with uncertain optimzations on:
      - do not destroy registers which contain a local/global variable or a
        parameter, except if DestroyRefs is called because of a "movsl"
   * with uncertain optimzations off:
      - destroy every register which contains a memory location
      }
      For Counter := R_EAX to R_EDI Do
        With PPaiProp(p^.OptInfo)^.Regs[Counter] Do
        If (typ = Con_Ref) And
           (Not(cs_UncertainOpts in aktglobalswitches) Or
        {for movsl}
            (Ref.Base = R_EDI) Or
        {don't destroy if reg contains a parameter, local or global variable}
            Not((NrOfMods = 1) And
                (Pai386(StartMod)^.oper[0].typ = top_ref) And
                ((Pai386(StartMod)^.oper[0].ref^.base = ProcInfo.FramePointer) Or
                  Assigned(Pai386(StartMod)^.oper[0].ref^.Symbol)
                )
               )
           )
          Then DestroyReg(PPaiProp(p^.OptInfo), Counter)
End;

Procedure DestroyAllRegs(p: PPaiProp);
Var Counter: TRegister;
Begin {initializes/desrtoys all registers}
  For Counter := R_EAX To R_EDI Do
    DestroyReg(p, Counter);
  p^.DirFlag := F_Unknown;
End;

Procedure DestroyOp(PaiObj: Pai; const o:Toper);
Begin
  Case o.typ Of
    top_reg: DestroyReg(PPaiProp(PaiObj^.OptInfo), o.reg);
    top_ref: DestroyRefs(PaiObj, o.ref^, R_NO);
    top_symbol:;
  End;
End;

Procedure ReadReg(p: PPaiProp; Reg: TRegister);
Begin
  Reg := Reg32(Reg);
  If Reg in [R_EAX..R_EDI] Then
    IncState(p^.Regs[Reg32(Reg)].RState)
End;

Procedure ReadRef(p: PPaiProp; Ref: PReference);
Begin
  If Ref^.Base <> R_NO Then
    ReadReg(p, Ref^.Base);
  If Ref^.Index <> R_NO Then
    ReadReg(p, Ref^.Index);
End;

Procedure ReadOp(P: PPaiProp;const o:toper);
Begin
  Case o.typ Of
    top_reg: ReadReg(P, o.reg);
    top_ref: ReadRef(P, o.ref);
    top_symbol : ;
  End;
End;

Function DFAPass1(AsmL: PAasmOutput; BlockStart: Pai): Pai;
{gathers the RegAlloc data... still need to think about where to store it to
 avoid global vars}
Var BlockEnd: Pai;
Begin
  BlockEnd := FindLoHiLabels(LoLab, HiLab, LabDif, BlockStart);
  BuildLabelTableAndFixRegAlloc(AsmL, LTable, LoLab, LabDif, BlockStart, BlockEnd);
  DFAPass1 := BlockEnd;
End;

{$ifdef arithopt}
Procedure AddInstr2RegContents({$ifdef statedebug} asml: paasmoutput; {$endif}
p: pai386; reg: TRegister);
{$ifdef statedebug}
var hp: pai;
{$endif statedebug}
Begin
  Reg := Reg32(Reg);
  With PPaiProp(p^.optinfo)^.Regs[reg] Do
    If (Typ = Con_Ref)
      Then
        Begin
          IncState(WState);
 {also store how many instructions are part of the sequence in the first
  instructions PPaiProp, so it can be easily accessed from within
  CheckSequence}
          Inc(NrOfMods, NrOfInstrSinceLastMod[Reg]);
          PPaiProp(Pai(StartMod)^.OptInfo)^.Regs[Reg].NrOfMods := NrOfMods;
          NrOfInstrSinceLastMod[Reg] := 0;
{$ifdef StateDebug}
          hp := new(pai_asm_comment,init(strpnew(att_reg2str[reg]+': '+tostr(PPaiProp(p^.optinfo)^.Regs[reg].WState)
                + ' -- ' + tostr(PPaiProp(p^.optinfo)^.Regs[reg].nrofmods))));
          InsertLLItem(AsmL, p, p^.next, hp);
{$endif StateDebug}
        End
      Else
        Begin
          DestroyReg(PPaiProp(p^.optinfo), Reg);
{$ifdef StateDebug}
          hp := new(pai_asm_comment,init(strpnew(att_reg2str[reg]+': '+tostr(PPaiProp(p^.optinfo)^.Regs[reg].WState))));
          InsertLLItem(AsmL, p, p^.next, hp);
{$endif StateDebug}
        End
End;

Procedure AddInstr2OpContents({$ifdef statedebug} asml: paasmoutput; {$endif}
p: pai386; const oper: TOper);
Begin
  If oper.typ = top_reg Then
    AddInstr2RegContents({$ifdef statedebug} asml, {$endif}p, oper.reg)
  Else
    Begin
      ReadOp(PPaiProp(p^.optinfo), oper);
      DestroyOp(p, oper);
    End
End;
{$endif arithopt}

Procedure DoDFAPass2(
{$Ifdef StateDebug}
AsmL: PAasmOutput;
{$endif statedebug}
BlockStart, BlockEnd: Pai);
{Analyzes the Data Flow of an assembler list. Starts creating the reg
 contents for the instructions starting with p. Returns the last pai which has
 been processed}
Var
    CurProp: PPaiProp;
{$ifdef AnalyzeLoops}
    TmpState: Byte;
{$endif AnalyzeLoops}
    Cnt, InstrCnt : Longint;
    InstrProp: TAsmInstrucProp;
    UsedRegs: TRegSet;
    p, hp : Pai;
    TmpRef: TReference;
    TmpReg: TRegister;
Begin
  p := BlockStart;
  UsedRegs := [];
  UpdateUsedregs(UsedRegs, p);
  SkipHead(P);
  BlockStart := p;
  InstrCnt := 1;
  FillChar(NrOfInstrSinceLastMod, SizeOf(NrOfInstrSinceLastMod), 0);
  While (P <> BlockEnd) Do
    Begin
{$IfDef TP}
      New(CurProp);
{$Else TP}
      CurProp := @PaiPropBlock^[InstrCnt];
{$EndIf TP}
      If (p <> BlockStart)
        Then
          Begin
{$ifdef JumpAnal}
            If (p^.Typ <> ait_label) Then
{$endif JumpAnal}
              Begin
                GetLastInstruction(p, hp);
                CurProp^.Regs := PPaiProp(hp^.OptInfo)^.Regs;
                CurProp^.DirFlag := PPaiProp(hp^.OptInfo)^.DirFlag;
              End
          End
        Else
          Begin
            FillChar(CurProp^, SizeOf(CurProp^), 0);
{            For TmpReg := R_EAX to R_EDI Do
              CurProp^.Regs[TmpReg].WState := 1;}
          End;
      CurProp^.UsedRegs := UsedRegs;
      CurProp^.CanBeRemoved := False;
      UpdateUsedRegs(UsedRegs, Pai(p^.Next));
{$ifdef TP}
      PPaiProp(p^.OptInfo) := CurProp;
{$Endif TP}
      For TmpReg := R_EAX To R_EDI Do
        Inc(NrOfInstrSinceLastMod[TmpReg]);
      Case p^.typ Of
        ait_label:
{$Ifndef JumpAnal}
          If (Pai_label(p)^.l^.is_used) Then
            DestroyAllRegs(CurProp);
{$Else JumpAnal}
          Begin
           If (Pai_Label(p)^.is_used) Then
             With LTable^[Pai_Label(p)^.l^.labelnr-LoLab] Do
{$IfDef AnalyzeLoops}
              If (RefsFound = Pai_Label(p)^.l^.RefCount)
{$Else AnalyzeLoops}
              If (JmpsProcessed = Pai_Label(p)^.l^.RefCount)
{$EndIf AnalyzeLoops}
                Then
{all jumps to this label have been found}
{$IfDef AnalyzeLoops}
                  If (JmpsProcessed > 0)
                    Then
{$EndIf AnalyzeLoops}
 {we've processed at least one jump to this label}
                      Begin
                        If (GetLastInstruction(p, hp) And
                           Not(((hp^.typ = ait_instruction)) And
                                (pai386_labeled(hp)^.is_jmp))
                          Then
  {previous instruction not a JMP -> the contents of the registers after the
   previous intruction has been executed have to be taken into account as well}
                            For TmpReg := R_EAX to R_EDI Do
                              Begin
                                If (CurProp^.Regs[TmpReg].WState <>
                                    PPaiProp(hp^.OptInfo)^.Regs[TmpReg].WState)
                                  Then DestroyReg(CurProp, TmpReg)
                              End
                      End
{$IfDef AnalyzeLoops}
                    Else
 {a label from a backward jump (e.g. a loop), no jump to this label has
  already been processed}
                      If GetLastInstruction(p, hp) And
                         Not(hp^.typ = ait_instruction) And
                            (pai386_labeled(hp)^.opcode = A_JMP))
                        Then
  {previous instruction not a jmp, so keep all the registers' contents from the
   previous instruction}
                          Begin
                            CurProp^.Regs := PPaiProp(hp^.OptInfo)^.Regs;
                            CurProp^.DirFlag := PPaiProp(hp^.OptInfo)^.DirFlag;
                          End
                        Else
  {previous instruction a jmp and no jump to this label processed yet}
                          Begin
                            hp := p;
                            Cnt := InstrCnt;
     {continue until we find a jump to the label or a label which has already
      been processed}
                            While GetNextInstruction(hp, hp) And
                                  Not((hp^.typ = ait_instruction) And
                                      (pai386(hp)^.is_jmp) and
                                      (pasmlabel(pai386(hp)^.oper[0].sym)^.labelnr = Pai_Label(p)^.l^.labelnr)) And
                                  Not((hp^.typ = ait_label) And
                                      (LTable^[Pai_Label(hp)^.l^.labelnr-LoLab].RefsFound
                                       = Pai_Label(hp)^.l^.RefCount) And
                                      (LTable^[Pai_Label(hp)^.l^.labelnr-LoLab].JmpsProcessed > 0)) Do
                              Inc(Cnt);
                            If (hp^.typ = ait_label)
                              Then
   {there's a processed label after the current one}
                                Begin
                                  CurProp^.Regs := PaiPropBlock^[Cnt].Regs;
                                  CurProp^.DirFlag := PaiPropBlock^[Cnt].DirFlag;
                                End
                              Else
   {there's no label anymore after the current one, or they haven't been
    processed yet}
                                Begin
                                  GetLastInstruction(p, hp);
                                  CurProp^.Regs := PPaiProp(hp^.OptInfo)^.Regs;
                                  CurProp^.DirFlag := PPaiProp(hp^.OptInfo)^.DirFlag;
                                  DestroyAllRegs(PPaiProp(hp^.OptInfo))
                                End
                          End
{$EndIf AnalyzeLoops}
                Else
{not all references to this label have been found, so destroy all registers}
                  Begin
                    GetLastInstruction(p, hp);
                    CurProp^.Regs := PPaiProp(hp^.OptInfo)^.Regs;
                    CurProp^.DirFlag := PPaiProp(hp^.OptInfo)^.DirFlag;
                    DestroyAllRegs(CurProp)
                  End;
          End;
{$EndIf JumpAnal}

{$ifdef GDB}
        ait_stabs, ait_stabn, ait_stab_function_name:;
{$endif GDB}

        ait_instruction:
          Begin
            if pai386(p)^.is_jmp then
             begin
{$IfNDef JumpAnal}
  ;
{$Else JumpAnal}
          With LTable^[pasmlabel(pai386(p)^.oper[0].sym)^.labelnr-LoLab] Do
            If (RefsFound = pasmlabel(pai386(p)^.oper[0].sym)^.RefCount) Then
              Begin
                If (InstrCnt < InstrNr)
                  Then
                {forward jump}
                    If (JmpsProcessed = 0) Then
                {no jump to this label has been processed yet}
                      Begin
                        PaiPropBlock^[InstrNr].Regs := CurProp^.Regs;
                        PaiPropBlock^[InstrNr].DirFlag := CurProp^.DirFlag;
                        Inc(JmpsProcessed);
                      End
                    Else
                      Begin
                        For TmpReg := R_EAX to R_EDI Do
                          If (PaiPropBlock^[InstrNr].Regs[TmpReg].WState <>
                             CurProp^.Regs[TmpReg].WState) Then
                            DestroyReg(@PaiPropBlock^[InstrNr], TmpReg);
                        Inc(JmpsProcessed);
                      End
{$ifdef AnalyzeLoops}
                  Else
{                backward jump, a loop for example}
{                    If (JmpsProcessed > 0) Or
                       Not(GetLastInstruction(PaiObj, hp) And
                           (hp^.typ = ait_labeled_instruction) And
                           (pai386_labeled(hp)^.opcode = A_JMP))
                      Then}
{instruction prior to label is not a jmp, or at least one jump to the label
 has yet been processed}
                        Begin
                          Inc(JmpsProcessed);
                          For TmpReg := R_EAX to R_EDI Do
                            If (PaiPropBlock^[InstrNr].Regs[TmpReg].WState <>
                                CurProp^.Regs[TmpReg].WState)
                              Then
                                Begin
                                  TmpState := PaiPropBlock^[InstrNr].Regs[TmpReg].WState;
                                  Cnt := InstrNr;
                                  While (TmpState = PaiPropBlock^[Cnt].Regs[TmpReg].WState) Do
                                    Begin
                                      DestroyReg(@PaiPropBlock^[Cnt], TmpReg);
                                      Inc(Cnt);
                                    End;
                                  While (Cnt <= InstrCnt) Do
                                    Begin
                                      Inc(PaiPropBlock^[Cnt].Regs[TmpReg].WState);
                                      Inc(Cnt)
                                    End
                                End;
                        End
{                      Else }
{instruction prior to label is a jmp and no jumps to the label have yet been
 processed}
{                        Begin
                          Inc(JmpsProcessed);
                          For TmpReg := R_EAX to R_EDI Do
                            Begin
                              TmpState := PaiPropBlock^[InstrNr].Regs[TmpReg].WState;
                              Cnt := InstrNr;
                              While (TmpState = PaiPropBlock^[Cnt].Regs[TmpReg].WState) Do
                                Begin
                                  PaiPropBlock^[Cnt].Regs[TmpReg] := CurProp^.Regs[TmpReg];
                                  Inc(Cnt);
                                End;
                              TmpState := PaiPropBlock^[InstrNr].Regs[TmpReg].WState;
                              While (TmpState = PaiPropBlock^[Cnt].Regs[TmpReg].WState) Do
                                Begin
                                  DestroyReg(@PaiPropBlock^[Cnt], TmpReg);
                                  Inc(Cnt);
                                End;
                              While (Cnt <= InstrCnt) Do
                                Begin
                                  Inc(PaiPropBlock^[Cnt].Regs[TmpReg].WState);
                                  Inc(Cnt)
                                End
                            End
                        End}
{$endif AnalyzeLoops}
          End;
{$EndIf JumpAnal}
          end
          else
           begin
            InstrProp := AsmInstr[Pai386(p)^.opcode];
            Case Pai386(p)^.opcode Of
              A_MOV, A_MOVZX, A_MOVSX:
                Begin
                  Case Pai386(p)^.oper[0].typ Of
                    Top_Reg:
                      Case Pai386(p)^.oper[1].typ Of
                        Top_Reg:
                          Begin
                            DestroyReg(CurProp, Pai386(p)^.oper[1].reg);
                            ReadReg(CurProp, Pai386(p)^.oper[0].reg);
{                            CurProp^.Regs[Pai386(p)^.oper[1].reg] :=
                              CurProp^.Regs[Pai386(p)^.oper[0].reg];
                            If (CurProp^.Regs[Pai386(p)^.oper[1].reg].ModReg = R_NO) Then
                              CurProp^.Regs[Pai386(p)^.oper[1].reg].ModReg :=
                                Pai386(p)^.oper[0].reg;}
                          End;
                        Top_Ref:
                          Begin
                            ReadReg(CurProp, Pai386(p)^.oper[0].reg);
                            ReadRef(CurProp, Pai386(p)^.oper[1].ref);
                            DestroyRefs(p, Pai386(p)^.oper[1].ref^, Pai386(p)^.oper[0].reg);
                          End;
                      End;
                    Top_Ref:
                      Begin {destination is always a register in this case}
                        ReadRef(CurProp, Pai386(p)^.oper[0].ref);
                        ReadReg(CurProp, Pai386(p)^.oper[1].reg);
                        TmpReg := Reg32(Pai386(p)^.oper[1].reg);
                        If RegInRef(TmpReg, Pai386(p)^.oper[0].ref^) And
                           (CurProp^.Regs[TmpReg].Typ = Con_Ref)
                          Then
                            Begin
                              With CurProp^.Regs[TmpReg] Do
                                Begin
                                  IncState(WState);
 {also store how many instructions are part of the sequence in the first
  instructions PPaiProp, so it can be easily accessed from within
  CheckSequence}
                                  Inc(NrOfMods, NrOfInstrSinceLastMod[TmpReg]);
                                  PPaiProp(Pai(StartMod)^.OptInfo)^.Regs[TmpReg].NrOfMods := NrOfMods;
                                  NrOfInstrSinceLastMod[TmpReg] := 0;
                                End;
                            End
                          Else
                            Begin
                              DestroyReg(CurProp, TmpReg);
                              If Not(RegInRef(TmpReg, Pai386(p)^.oper[0].ref^)) Then
                                With CurProp^.Regs[TmpReg] Do
                                  Begin
                                    Typ := Con_Ref;
                                    StartMod := p;
                                    NrOfMods := 1;
                                  End
                            End;
{$ifdef StateDebug}
                  hp := new(pai_asm_comment,init(strpnew(att_reg2str[TmpReg]+': '+tostr(CurProp^.Regs[TmpReg].WState))));
                  InsertLLItem(AsmL, p, p^.next, hp);
{$endif StateDebug}

                      End;
                    Top_Const:
                      Begin
                        Case Pai386(p)^.oper[1].typ Of
                          Top_Reg:
                            Begin
                              TmpReg := Reg32(Pai386(p)^.oper[1].reg);
                              With CurProp^.Regs[TmpReg] Do
                                Begin
                                  DestroyReg(CurProp, TmpReg);
                                  typ := Con_Const;
                                  StartMod := p;
                                End
                            End;
                          Top_Ref:
                            Begin
                              ReadRef(CurProp, Pai386(p)^.oper[1].ref);
                              DestroyRefs(P, Pai386(p)^.oper[1].ref^, R_NO);
                            End;
                        End;
                      End;
                  End;
                End;
              A_DIV, A_IDIV, A_MUL:
                Begin
                  ReadOp(Curprop, Pai386(p)^.oper[0]);
                  ReadReg(CurProp,R_EAX);
                  If (Pai386(p)^.OpCode = A_IDIV) or
                     (Pai386(p)^.OpCode = A_DIV) Then
                    ReadReg(CurProp,R_EDX);
                  DestroyReg(CurProp, R_EAX)
                End;
              A_IMUL:
                Begin
                  ReadOp(CurProp,Pai386(p)^.oper[0]);
                  ReadOp(CurProp,Pai386(p)^.oper[1]);
                  If (Pai386(p)^.oper[2].typ = top_none) Then
                    If (Pai386(p)^.oper[1].typ = top_none) Then
                      Begin
                        ReadReg(CurProp,R_EAX);
                        DestroyReg(CurProp, R_EAX);
                        DestroyReg(CurProp, R_EDX)
                      End
                    Else
            {$ifdef arithopt}
                      AddOp2RegContents(Pai386(p), Pai386(p)^.oper[1])
            {$else arithopt}
                      DestroyOp(p, Pai386(p)^.oper[1])
            {$endif arithopt}
                  Else
            {$ifdef arithopt}
                    AddOp2RegContents(Pai386(p), Pai386(p)^.oper[2]);
            {$else arithopt}
                    DestroyOp(p, Pai386(p)^.oper[2]);
            {$endif arithopt}
                End;
              A_XOR:
                Begin
                  ReadOp(CurProp, Pai386(p)^.oper[0]);
                  ReadOp(CurProp, Pai386(p)^.oper[1]);
                  If (Pai386(p)^.oper[0].typ = top_reg) And
                     (Pai386(p)^.oper[1].typ = top_reg) And
                     (Pai386(p)^.oper[0].reg = Pai386(p)^.oper[1].reg)
                    Then
                      Begin
                        DestroyReg(CurProp, Pai386(p)^.oper[0].reg);
                        CurProp^.Regs[Reg32(Pai386(p)^.oper[0].reg)].typ := Con_Const;
                        CurProp^.Regs[Reg32(Pai386(p)^.oper[0].reg)].StartMod := Pointer(0)
                      End
                    Else
                      DestroyOp(p, Pai386(p)^.oper[1]);
                End
              Else
                Begin
                  Cnt := 1;
                  While (Cnt <= MaxCh) And
                        (InstrProp.Ch[Cnt] <> C_None) Do
                    Begin
                      Case InstrProp.Ch[Cnt] Of
                        C_REAX..C_REDI: ReadReg(CurProp,TCh2Reg(InstrProp.Ch[Cnt]));
                        C_WEAX..C_RWEDI:
                          Begin
                            If (InstrProp.Ch[Cnt] >= C_RWEAX) Then
                              ReadReg(CurProp, TCh2Reg(InstrProp.Ch[Cnt]));
                            DestroyReg(CurProp, TCh2Reg(InstrProp.Ch[Cnt]));
                          End;
{$ifdef arithopt}
                        C_MEAX..C_MEDI:
                          AddInstr2RegContents({$ifdef statedebug} asml, {$endif}
                                               Pai386(p),
                                               TCh2Reg(InstrProp.Ch[Cnt]));
{$endif arithopt}
                        C_CDirFlag: CurProp^.DirFlag := F_NotSet;
                        C_SDirFlag: CurProp^.DirFlag := F_Set;
                        C_Rop1: ReadOp(CurProp, Pai386(p)^.oper[0]);
                        C_Rop2: ReadOp(CurProp, Pai386(p)^.oper[1]);
                        C_ROp3: ReadOp(CurProp, Pai386(p)^.oper[2]);
                        C_Wop1..C_RWop1:
                          Begin
                            If (InstrProp.Ch[Cnt] in [C_RWop1]) Then
                              ReadOp(CurProp, Pai386(p)^.oper[0]);
                            DestroyOp(p, Pai386(p)^.oper[0]);
                          End;
{$ifdef arithopt}
                        C_Mop1:
                          AddInstr2OpContents({$ifdef statedebug} asml, {$endif}
                          Pai386(p), Pai386(p)^.oper[0]);
{$endif arithopt}
                        C_Wop2..C_RWop2:
                          Begin
                            If (InstrProp.Ch[Cnt] = C_RWop2) Then
                              ReadOp(CurProp, Pai386(p)^.oper[1]);
                            DestroyOp(p, Pai386(p)^.oper[1]);
                          End;
{$ifdef arithopt}
                        C_Mop2:
                          AddInstr2OpContents({$ifdef statedebug} asml, {$endif}
                          Pai386(p), Pai386(p)^.oper[1]);
{$endif arithopt}
                        C_WOp3..C_RWOp3:
                          Begin
                            If (InstrProp.Ch[Cnt] = C_RWOp3) Then
                              ReadOp(CurProp, Pai386(p)^.oper[2]);
                            DestroyOp(p, Pai386(p)^.oper[2]);
                          End;
{$ifdef arithopt}
                        C_Mop3:
                          AddInstr2OpContents({$ifdef statedebug} asml, {$endif}
                          Pai386(p), Pai386(p)^.oper[2]);
{$endif arithopt}
                        C_WMemEDI:
                          Begin
                            ReadReg(CurProp, R_EDI);
                            FillChar(TmpRef, SizeOf(TmpRef), 0);
                            TmpRef.Base := R_EDI;
                            DestroyRefs(p, TmpRef, R_NO)
                          End;
                        C_RFlags, C_WFlags, C_RWFlags, C_FPU:
                        Else
                          Begin
                            DestroyAllRegs(CurProp);
                          End;
                      End;
                      Inc(Cnt);
                    End
                End;
              end;
            End;
          End
        Else
          Begin
            DestroyAllRegs(CurProp);
          End;
      End;
      Inc(InstrCnt);
      GetNextInstruction(p, p);
    End;
End;

Function InitDFAPass2(BlockStart, BlockEnd: Pai): Boolean;
{reserves memory for the PPaiProps in one big memory block when not using
 TP, returns False if not enough memory is available for the optimizer in all
 cases}
Var p: Pai;
    Count: Longint;
{    TmpStr: String; }
Begin
  P := BlockStart;
  SkipHead(P);
  NrOfPaiObjs := 0;
  While (P <> BlockEnd) Do
    Begin
{$IfDef JumpAnal}
      Case P^.Typ Of
        ait_label:
          Begin
            If (Pai_Label(p)^.l^.is_used) Then
              LTable^[Pai_Label(P)^.l^.labelnr-LoLab].InstrNr := NrOfPaiObjs
          End;
        ait_instruction:
          begin
            if pai386(p)^.is_jmp then
             begin
               If (pasmlabel(pai386(P)^.oper[0].sym)^.labelnr >= LoLab) And
                  (pasmlabel(pai386(P)^.oper[0].sym)^.labelnr <= HiLab) Then
                 Inc(LTable^[pasmlabel(pai386(P)^.oper[0].sym)^.labelnr-LoLab].RefsFound);
             end;
          end;
{        ait_instruction:
          Begin
           If (Pai386(p)^.opcode = A_PUSH) And
              (Pai386(p)^.oper[0].typ = top_symbol) And
              (PCSymbol(Pai386(p)^.oper[0])^.offset = 0) Then
             Begin
               TmpStr := StrPas(PCSymbol(Pai386(p)^.oper[0])^.symbol);
               If}
      End;
{$EndIf JumpAnal}
      Inc(NrOfPaiObjs);
      GetNextInstruction(p, p);
    End;
{$IfDef TP}
  If (MemAvail < (SizeOf(TPaiProp)*NrOfPaiObjs))
     Or (NrOfPaiObjs = 0)
    {this doesn't have to be one contiguous block}
    Then InitDFAPass2 := False
    Else InitDFAPass2 := True;
{$Else}
{Uncomment the next line to see how much memory the reloading optimizer needs}
{  Writeln((NrOfPaiObjs*(((SizeOf(TPaiProp)+3)div 4)*4)));}
{no need to check mem/maxavail, we've got as much virtual memory as we want}
  If NrOfPaiObjs <> 0 Then
    Begin
      InitDFAPass2 := True;
      GetMem(PaiPropBlock, NrOfPaiObjs*(((SizeOf(TPaiProp)+3)div 4)*4));
      p := BlockStart;
      SkipHead(p);
      For Count := 1 To NrOfPaiObjs Do
        Begin
          PPaiProp(p^.OptInfo) := @PaiPropBlock^[Count];
          GetNextInstruction(p, p);
        End;
    End
  Else InitDFAPass2 := False;
 {$EndIf TP}
End;

Function DFAPass2(
{$ifdef statedebug}
                   AsmL: PAasmOutPut;
{$endif statedebug}
                                      BlockStart, BlockEnd: Pai): Boolean;
Begin
  If InitDFAPass2(BlockStart, BlockEnd) Then
    Begin
      DoDFAPass2(
{$ifdef statedebug}
         asml,
{$endif statedebug}
         BlockStart, BlockEnd);
      DFAPass2 := True
    End
  Else DFAPass2 := False;
End;

Procedure ShutDownDFA;
Begin
  If LabDif <> 0 Then
    FreeMem(LTable, LabDif*SizeOf(TLabelTableItem));
End;

End.

{
 $Log$
 Revision 1.56  1999-08-18 13:25:54  jonas
   * minor fixes regarding the reading of operands

 Revision 1.55  1999/08/12 14:36:03  peter
   + KNI instructions

 Revision 1.54  1999/08/05 15:01:52  jonas
   * fix in -darithopt code (sometimes crashed on 8/16bit regs)

 Revision 1.53  1999/08/04 00:22:59  florian
   * renamed i386asm and i386base to cpuasm and cpubase

 Revision 1.52  1999/08/02 14:35:21  jonas
   * bugfix in DestroyRefs

 Revision 1.51  1999/08/02 12:12:53  jonas
   * also add arithmetic operations to instruction sequences contained in registers
     (compile with -darithopt, very nice!)

 Revision 1.50  1999/07/30 18:18:51  jonas
   * small bugfix in instructionsequal
   * small bugfix in reginsequence
   * made regininstruction a bit more logical

 Revision 1.48  1999/07/01 18:21:21  jonas
   * removed unused AsmL parameter from FindLoHiLabels

 Revision 1.47  1999/05/27 19:44:24  peter
   * removed oldasm
   * plabel -> pasmlabel
   * -a switches to source writing automaticly
   * assembler readers OOPed
   * asmsymbol automaticly external
   * jumptables and other label fixes for asm readers

 Revision 1.46  1999/05/08 20:40:02  jonas
   * seperate OPTimizer INFO pointer field in tai object
   * fix to GetLastInstruction that sometimes caused a crash

 Revision 1.45  1999/05/01 13:48:37  peter
   * merged nasm compiler

 Revision 1.6  1999/04/18 17:57:21  jonas
   * fix for crash when the first instruction of a sequence that gets
     optimized is removed (this situation can't occur aymore now)

 Revision 1.5  1999/04/16 11:49:50  peter
   + tempalloc
   + -at to show temp alloc info in .s file

 Revision 1.4  1999/04/14 09:07:42  peter
   * asm reader improvements

 Revision 1.3  1999/03/31 13:55:29  peter
   * assembler inlining working for ag386bin

 Revision 1.2  1999/03/29 16:05:46  peter
   * optimizer working for ag386bin

 Revision 1.1  1999/03/26 00:01:10  peter
   * first things for optimizer (compiles but cycle crashes)

 Revision 1.39  1999/02/26 00:48:18  peter
   * assembler writers fixed for ag386bin

 Revision 1.38  1999/02/25 21:02:34  peter
   * ag386bin updates
   + coff writer

 Revision 1.37  1999/02/22 02:15:20  peter
   * updates for ag386bin

 Revision 1.36  1999/01/20 17:41:26  jonas
   * small bugfix (memory corruption could occur when certain fpu instructions
     were encountered)

 Revision 1.35  1999/01/08 12:39:22  florian
   Changes of Alexander Stohr integrated:
     + added KNI opcodes
     + added KNI registers
     + added 3DNow! opcodes
     + added 64 bit and 128 bit register flags
     * translated a few comments into english

 Revision 1.34  1998/12/29 18:48:19  jonas
   + optimize pascal code surrounding assembler blocks

 Revision 1.33  1998/12/17 16:37:38  jonas
   + extra checks in RegsEquivalent so some more optimizations can be done (which
     where disabled by the second fix from revision 1.22)

 Revision 1.32  1998/12/15 19:33:58  jonas
   * uncommented OpsEqual & added to interface because popt386 uses it now

 Revision 1.31  1998/12/11 00:03:13  peter
   + globtype,tokens,version unit splitted from globals

 Revision 1.30  1998/12/02 16:23:39  jonas
   * changed "if longintvar in set" to case or "if () or () .." statements
   * tree.pas: changed inlinenumber (and associated constructor/vars) to a byte

 Revision 1.29  1998/11/26 21:45:31  jonas
   - removed A_CLTD opcode (use A_CDQ instead)
   * changed cbw, cwde and cwd to cbtw, cwtl and cwtd in att_.oper[1]str array
   * in daopt386: adapted AsmInstr array to reflect changes + fixed line too long

 Revision 1.27  1998/11/24 19:47:22  jonas
   * fixed problems posible with 3 operand instructions

 Revision 1.26  1998/11/24 12:50:09  peter
   * fixed crash

 Revision 1.25  1998/11/18 17:58:22  jonas
   + gathering of register reading data, nowhere used yet (necessary for instruction scheduling)

 Revision 1.24  1998/11/13 10:13:44  peter
   + cpuid,emms support for asm readers

 Revision 1.23  1998/11/09 19:40:46  jonas
   * fixed comments from last commit (apparently there's still a 255 char limit :( )

 Revision 1.22  1998/11/09 19:33:40  jonas
   * changed specific bugfix (which was actually wrong implemented, but
     did the right thing in most cases nevertheless) to general bugfix
   * fixed bug that caused
     mov (ebp), edx                                    mov (ebp), edx
     mov (edx), edx                                    mov (edx), edx
     ...                   being changed to            ...
     mov (ebp), edx                                    mov edx, eax
     mov (eax), eax
     but this disabled another small correct optimization...
 Revision 1.21  1998/11/02 23:17:49  jonas
   * fixed bug shown in sortbug program from fpc-devel list

 Revision 1.20  1998/10/22 13:24:51  jonas
   * changed TRegSet to a small set

 Revision 1.19  1998/10/20 09:29:24  peter
   * bugfix so that code like
      movl  48(%esi),%esi                            movl  48(%esi),%esi
      pushl %esi            doesn't get changed to   pushl %esi
      movl 48(%esi),%edi                             movl  %esi,%edi

 Revision 1.18  1998/10/07 16:27:02  jonas
   * changed state to WState (WriteState), added RState for future use in
      instruction scheduling
   * RegAlloc data from the CG is now completely being patched and corrected (I
      think)

 Revision 1.17  1998/10/02 17:30:20  jonas
   * small patches to regdealloc data

 Revision 1.16  1998/10/01 20:21:47  jonas
   * inter-register CSE, still requires some tweaks (peepholeoptpass2, better  RegAlloc)

 Revision 1.15  1998/09/20 18:00:20  florian
   * small compiling problems fixed

 Revision 1.14  1998/09/20 17:12:36  jonas
   * small fix for uncertain optimizations & more cleaning up

 Revision 1.12  1998/09/16 18:00:01  jonas
   * optimizer now completely dependant on GetNext/GetLast instruction, works again with -dRegAlloc

 Revision 1.11  1998/09/15 14:05:27  jonas
   * fixed optimizer incompatibilities with freelabel code in psub

 Revision 1.10  1998/09/09 15:33:58  peter
   * removed warnings

 Revision 1.9  1998/09/03 16:24:51  florian
   * bug of type conversation from dword to real fixed
   * bug fix of Jonas applied

 Revision 1.8  1998/08/28 10:56:59  peter
   * removed warnings

 Revision 1.7  1998/08/19 16:07:44  jonas
   * changed optimizer switches + cleanup of DestroyRefs in daopt386.pas

 Revision 1.6  1998/08/10 14:49:57  peter
   + localswitches, moduleswitches, globalswitches splitting

 Revision 1.5  1998/08/09 13:56:24  jonas
   * small bugfix for uncertain optimizations in DestroyRefs

 Revision 1.4  1998/08/06 19:40:25  jonas
   * removed $ before and after Log in comment

 Revision 1.3  1998/08/05 16:00:14  florian
   * some fixes for ansi strings
   * log to Log changed

}
