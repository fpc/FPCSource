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
  CObjects,Aasm,i386;


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
Function OpsEqual(typ: Longint; op1, op2: Pointer): Boolean;

Function DFAPass1(AsmL: PAasmOutput; BlockStart: Pai): Pai;
Function DFAPass2(AsmL: PAasmOutput; BlockStart, BlockEnd: Pai): Boolean;
Procedure ShutDownDFA;

Function FindLabel(L: PLabel; Var hp: Pai): Boolean;
{Procedure FindLoHiLabels(AsmL: PAasmOutput; Var LoLab, HiLab, LabDif: Longint);}

{******************************* Constants *******************************}

Const

{ait_* types which don't result in executable code or which don't influence
 the way the program runs/behaves}

  SkipInstr = [ait_comment, ait_align, ait_symbol
{$ifdef GDB}
  ,ait_stabs, ait_stabn, ait_stab_function_name
{$endif GDB}
  ,ait_regalloc, ait_regdealloc
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
             C_CDirFlag {clear direction flag}, C_SDirFlag {set dir flag},
             C_RFlags, C_WFlags, C_RWFlags, C_FPU,
             C_ROp1, C_WOp1, C_RWOp1,
             C_ROp2, C_WOp2, C_RWOp2,
             C_ROp3, C_WOp3, C_RWOp3,
             C_WMemEDI,
             C_All);

{the possible states of a flag}
  TFlagContents = (F_Unknown, F_NotSet, F_Set);

{the properties of a cpu instruction}
  TAsmInstrucProp = Record
               {how many things it changes}
{                         NCh: Byte;}
               {and what it changes}
                         Ch: Array[1..MaxCh] of TChange;
                       End;

  TContent = Record
      {start and end of block instructions that defines the
       content of this register. If Typ = con_const, then
       Longint(StartMod) = value of the constant)}
               StartMod: Pointer;
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
 gets one of these assigned: a pointer to it is stored in the Line field and
 the original line number is stored in LineSave}
  TPaiProp = Record
               Regs: TRegContent;
{               FPURegs: TRegFPUContent;} {currently not yet used}
               LineSave: Longint;
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
  TwoWords = Record
               Word1, Word2: Word;
             End;

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

Uses globals, systems, strings, verbose, hcodegen,
   {$ifdef i386}
     pass_2;
   {$endif i386}

Const AsmInstr: Array[tasmop] Of TAsmInstrucProp = (
   {MOV} (Ch: (C_WOp2, C_ROp1, C_None)),
 {MOVZX} (Ch: (C_WOp2, C_ROp1, C_None)),
 {MOVSX} (Ch: (C_WOp2, C_ROp1, C_None)),
 {LABEL} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
   {ADD} (Ch: (C_RWOp2, C_ROp1, C_WFlags)),
  {CALL} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {IDIV} (Ch: (C_RWEAX, C_WEDX, C_WFlags)),
  {IMUL} (Ch: (C_RWEAX, C_WEDX, C_WFlags)), {handled separately, because several forms exist}
   {JMP} (Ch: (C_None, C_None, C_None)),
   {LEA} (Ch: (C_WOp2, C_ROp1, C_None)),
   {MUL} (Ch: (C_RWEAX, C_WEDX, C_WFlags)),
   {NEG} (Ch: (C_RWOp1, C_None, C_None)),
   {NOT} (Ch: (C_RWOp1, C_WFlags, C_None)),
   {POP} (Ch: (C_WOp1, C_RWESP, C_None)),
 {POPAD} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {PUSH} (Ch: (C_RWESP, C_None, C_None)),
{PUSHAD} (Ch: (C_RWESP, C_None, C_None)),
   {RET} (Ch: (C_ALL, C_None, C_None)),
   {SUB} (Ch: (C_RWOp2, C_ROp1, C_WFlags)),
  {XCHG} (Ch: (C_RWOp1, C_RWOp2, C_None)), {(might be) handled seperately}
   {XOR} (Ch: (C_RWOp2, C_ROp1, C_WFlags)),
  {FILD} (Ch: (C_FPU, C_None, C_None)),
   {CMP} (Ch: (C_WFlags, C_None, C_None)),
    {JZ} (Ch: (C_RFlags, C_None, C_None)),
   {INC} (Ch: (C_RWOp1, C_WFlags, C_None)),
   {DEC} (Ch: (C_RWOp1, C_WFlags, C_None)),
  {SETE} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETNE} (Ch: (C_WOp1, C_RFlags, C_None)),
  {SETL} (Ch: (C_WOp1, C_RFlags, C_None)),
  {SETG} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETLE} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETGE} (Ch: (C_WOp1, C_RFlags, C_None)),
    {JE} (Ch: (C_RFlags, C_None, C_None)),
   {JNE} (Ch: (C_RFlags, C_None, C_None)),
    {JL} (Ch: (C_RFlags, C_None, C_None)),
    {JG} (Ch: (C_RFlags, C_None, C_None)),
   {JLE} (Ch: (C_RFlags, C_None, C_None)),
   {JGE} (Ch: (C_RFlags, C_None, C_None)),
    {OR} (Ch: (C_RWOp2, C_WFlags, C_None)),
   {FLD} (Ch: (C_ROp1, C_FPU, C_None)),
  {FADD} (Ch: (C_FPU, C_None, C_None)),
  {FMUL} (Ch: (C_FPU, C_None, C_None)),
  {FSUB} (Ch: (C_FPU, C_None, C_None)),
  {FDIV} (Ch: (C_FPU, C_None, C_None)),
  {FCHS} (Ch: (C_FPU, C_None, C_None)),
  {FLD1} (Ch: (C_FPU, C_None, C_None)),
 {FIDIV} (Ch: (C_FPU, C_None, C_None)),
   {JNZ} (Ch: (C_RFlags, C_None, C_None)),
  {FSTP} (Ch: (C_WOp1, C_FPU, C_None)),
   {AND} (Ch: (C_RWOp2, C_ROp1, C_WFlags)),
   {JNO} (Ch: (C_RFlags, C_None, C_None)),
  {NOTH} (Ch: (C_None, C_None, C_None)), {***???***}
  {NONE} (Ch: (C_None, C_None, C_None)),
 {ENTER} (Ch: (C_RWESP, C_None, C_None)),
 {LEAVE} (Ch: (C_RWESP, C_None, C_None)),
   {CLD} (Ch: (C_CDirFlag, C_None, C_None)),
  {MOVS} (Ch: (C_RWESI, C_RWEDI, C_WMemEDI)),
   {REP} (Ch: (C_RWECX, C_RFlags, C_None)),
   {SHL} (Ch: (C_RWOp2, C_ROp1, C_WFlags)),
   {SHR} (Ch: (C_RWOp2, C_ROp1, C_WFlags)),
 {BOUND} (Ch: (C_ROp1, C_None, C_None)),
   {JNS} (Ch: (C_RFlags, C_None, C_None)),
    {JS} (Ch: (C_RFlags, C_None, C_None)),
    {JO} (Ch: (C_RFlags, C_None, C_None)),
   {SAR} (Ch: (C_RWOp2, C_ROp1, C_WFlags)),
  {TEST} (Ch: (C_WFlags, C_ROp1, C_ROp2)),
  {FCOM} (Ch: (C_FPU, C_None, C_None)),
 {FCOMP} (Ch: (C_FPU, C_None, C_None)),
{FCOMPP} (Ch: (C_FPU, C_None, C_None)),
  {FXCH} (Ch: (C_FPU, C_None, C_None)),
 {FADDP} (Ch: (C_FPU, C_None, C_None)),
 {FMULP} (Ch: (C_FPU, C_None, C_None)),
 {FSUBP} (Ch: (C_FPU, C_None, C_None)),
 {FDIVP} (Ch: (C_FPU, C_None, C_None)),
 {FNSTS} (Ch: (C_WOp1, C_None, C_None)),
  {SAHF} (Ch: (C_WFlags, C_REAX, C_None)),
{FDIVRP} (Ch: (C_FPU, C_None, C_None)),
{FSUBRP} (Ch: (C_FPU, C_None, C_None)),
  {SETC} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETNC} (Ch: (C_WOp1, C_RFlags, C_None)),
    {JC} (Ch: (C_None, C_RFlags, C_None)),
   {JNC} (Ch: (C_RFlags, C_None, C_None)),
    {JA} (Ch: (C_RFlags, C_None, C_None)),
   {JAE} (Ch: (C_RFlags, C_None, C_None)),
    {JB} (Ch: (C_RFlags, C_None, C_None)),
   {JBE} (Ch: (C_RFlags, C_None, C_None)),
  {SETA} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETAE} (Ch: (C_WOp1, C_RFlags, C_None)),
  {SETB} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETBE} (Ch: (C_WOp1, C_RFlags, C_None)),
   {AAA} (Ch: (C_RWEAX, C_WFlags, C_None)),
   {AAD} (Ch: (C_RWEAX, C_WFlags, C_None)),
   {AAM} (Ch: (C_RWEAX, C_WFlags, C_None)),
   {AAS} (Ch: (C_RWEAX, C_WFlags, C_None)),
   {CBW} (Ch: (C_RWEAX, C_None, C_None)),
   {CDQ} (Ch: (C_RWEAX, C_WEDX, C_None)),
   {CLC} (Ch: (C_WFlags, C_None, C_None)),
   {CLI} (Ch: (C_WFlags, C_None, C_None)),
  {CLTS} (Ch: (C_None, C_None, C_None)),
   {CMC} (Ch: (C_WFlags, C_None, C_None)),
   {CWD} (Ch: (C_RWEAX, C_WEDX, C_None)),
  {CWDE} (Ch: (C_RWEAX, C_None, C_None)),
   {DAA} (Ch: (C_RWEAX, C_None, C_None)),
   {DAS} (Ch: (C_RWEAX, C_None, C_None)),
   {HLT} (Ch: (C_None, C_None, C_None)),
  {IRET} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {LAHF} (Ch: (C_WEAX, C_RFlags, C_None)),
  {LODS} (Ch: (C_WEAX, C_RWESI, C_None)),
  {LOCK} (Ch: (C_None, C_None, C_None)),
   {NOP} (Ch: (C_None, C_None, C_None)),
 {PUSHA} (Ch: (C_ALL, C_None, C_None)), {not true, but a pushall is usually followed by an instruction that does, so
                                         it won hurt either}
 {PUSHF} (Ch: (C_RWESP, C_RFlags, C_None)),
{PUSHFD} (Ch: (C_RWESP, C_RFlags, C_None)),
   {STC} (Ch: (C_WFlags, C_None, C_None)),
   {STD} (Ch: (C_SDirFlag, C_None, C_None)),
   {STI} (Ch: (C_WFlags, C_None, C_None)),
  {STOS} (Ch: (C_WMemEDI, C_RWEDI, C_REAX)),
  {WAIT} (Ch: (C_None, C_None, C_None)),
  {XLAT} (Ch: (C_WEAX, C_REBX, C_None)),
 {XLATB} (Ch: (C_WEAX, C_REBX, C_None)),
 {MOVSB} (Ch: (C_WOp2, C_ROp1, C_None)),
{MOVSBL} (Ch: (C_WOp2, C_ROp1, C_None)),
{MOVSBW} (Ch: (C_WOp2, C_ROp1, C_None)),
{MOVSWL} (Ch: (C_WOp2, C_ROp1, C_None)),
 {MOVZB} (Ch: (C_WOp2, C_ROp1, C_None)),
{MOVZWL} (Ch: (C_WOp2, C_ROp1, C_None)),
  {POPA} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
    {IN} (Ch: (C_WOp2, C_ROp1, C_None)),
   {OUT} (Ch: (C_ROp1, C_ROp2, C_None)),
   {LDS} (Ch: (C_WOp2, C_None, C_None)),
   {LCS} (Ch: (C_WOp2, C_None, C_None)),
   {LES} (Ch: (C_WOp2, C_None, C_None)),
   {LFS} (Ch: (C_WOp2, C_None, C_None)),
   {LGS} (Ch: (C_WOp2, C_None, C_None)),
   {LSS} (Ch: (C_WOp2, C_None, C_None)),
  {POPF} (Ch: (C_RWESP, C_WFlags, C_None)),
   {SBB} (Ch: (C_RWOp2, C_ROp1, C_RWFlags)),
   {ADC} (Ch: (C_RWOp2, C_ROp1, C_RWFlags)),
   {DIV} (Ch: (C_RWEAX, C_WEDX, C_WFlags)),
   {ROR} (Ch: (C_RWOp2, C_ROp1, C_RWFlags)),
   {ROL} (Ch: (C_RWOp2, C_ROp1, C_RWFlags)),
   {RCL} (Ch: (C_RWOp2, C_ROp1, C_RWFlags)),
   {RCR} (Ch: (C_RWOp2, C_ROp1, C_RWFlags)),
   {SAL} (Ch: (C_RWOp2, C_ROp1, C_RWFlags)),
  {SHLD} (Ch: (C_RWOp3, C_RWFlags, C_ROp2)),
  {SHRD} (Ch: (C_RWOp3, C_RWFlags, C_ROp2)),
 {LCALL} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {LJMP} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {LRET} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {JNAE} (Ch: (C_RFlags, C_None, C_None)),
   {JNB} (Ch: (C_RFlags, C_None, C_None)),
   {JNA} (Ch: (C_RFlags, C_None, C_None)),
  {JNBE} (Ch: (C_RFlags, C_None, C_None)),
    {JP} (Ch: (C_RFlags, C_None, C_None)),
   {JNP} (Ch: (C_RFlags, C_None, C_None)),
   {JPE} (Ch: (C_RFlags, C_None, C_None)),
   {JPO} (Ch: (C_RFlags, C_None, C_None)),
  {JNGE} (Ch: (C_RFlags, C_None, C_None)),
   {JNG} (Ch: (C_RFlags, C_None, C_None)),
   {JNL} (Ch: (C_RFlags, C_None, C_None)),
  {JNLE} (Ch: (C_RFlags, C_None, C_None)),
  {JCXZ} (Ch: (C_RECX, C_None, C_None)),
 {JECXZ} (Ch: (C_RECX, C_None, C_None)),
  {LOOP} (Ch: (C_RWECX, C_None, C_None)),
  {CMPS} (Ch: (C_RWESI, C_RWEDI, C_WFlags)),
   {INS} (Ch: (C_RWEDI, C_WMemEDI, C_None)),
  {OUTS} (Ch: (C_RWESI, C_None, C_None)),
  {SCAS} (Ch: (C_RWEDI, C_WFlags, C_None)),
   {BSF} (Ch: (C_WOp2, C_WFlags, C_ROp1)),
   {BSR} (Ch: (C_WOp2, C_WFlags, C_ROp1)),
    {BT} (Ch: (C_WFlags, C_ROp1, C_None)),
   {BTC} (Ch: (C_RWOp2, C_ROp1, C_WFlags)),
   {BTR} (Ch: (C_RWOp2, C_ROp1, C_WFlags)),
   {BTS} (Ch: (C_RWOp2, C_ROp1, C_WFlags)),
   {INT} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {INT3} (Ch: (C_None, C_None, C_None)),
  {INTO} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
{BOUNDL} (Ch: (C_ROp1, C_None, C_None)),
{BOUNDW} (Ch: (C_ROp1, C_None, C_None)),
 {LOOPZ} (Ch: (C_RWECX, C_RFlags, C_None)),
 {LOOPE} (Ch: (C_RWECX, C_RFlags, C_None)),
{LOOPNZ} (Ch: (C_RWECX, C_RFlags, C_None)),
{LOOPNE} (Ch: (C_RWECX, C_RFlags, C_None)),
  {SETO} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETNO} (Ch: (C_WOp1, C_RFlags, C_None)),
{SETNAE} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETNB} (Ch: (C_WOp1, C_RFlags, C_None)),
  {SETZ} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETNZ} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETNA} (Ch: (C_WOp1, C_RFlags, C_None)),
{SETNBE} (Ch: (C_WOp1, C_RFlags, C_None)),
  {SETS} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETNS} (Ch: (C_WOp1, C_RFlags, C_None)),
  {SETP} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETPE} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETNP} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETPO} (Ch: (C_WOp1, C_RFlags, C_None)),
{SETNGE} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETNL} (Ch: (C_WOp1, C_RFlags, C_None)),
 {SETNG} (Ch: (C_WOp1, C_RFlags, C_None)),
{SETNLE} (Ch: (C_WOp1, C_RFlags, C_None)),
  {ARPL} (Ch: (C_WFlags, C_None, C_None)),
   {LAR} (Ch: (C_WOp2, C_None, C_None)),
  {LGDT} (Ch: (C_None, C_None, C_None)),
  {LIDT} (Ch: (C_None, C_None, C_None)),
  {LLDT} (Ch: (C_None, C_None, C_None)),
  {LMSW} (Ch: (C_None, C_None, C_None)),
   {LSL} (Ch: (C_WOp2, C_WFlags, C_None)),
   {LTR} (Ch: (C_None, C_None, C_None)),
  {SGDT} (Ch: (C_WOp1, C_None, C_None)),
  {SIDT} (Ch: (C_WOp1, C_None, C_None)),
  {SLDT} (Ch: (C_WOp1, C_None, C_None)),
  {SMSW} (Ch: (C_WOp1, C_None, C_None)),
  {STR}  (Ch: (C_WOp1, C_None, C_None)),
  {VERR} (Ch: (C_WFlags, C_None, C_None)),
  {VERW} (Ch: (C_WFlags, C_None, C_None)),
  {FABS} (Ch: (C_FPU, C_None, C_None)),
  {FBLD} (Ch: (C_ROp1, C_FPU, C_None)),
 {FBSTP} (Ch: (C_WOp1, C_FPU, C_None)),
 {FCLEX} (Ch: (C_FPU, C_None, C_None)),
{FNCLEX} (Ch: (C_FPU, C_None, C_None)),
  {FCOS} (Ch: (C_FPU, C_None, C_None)),
{FDECSTP}(Ch: (C_FPU, C_None, C_None)),
 {FDISI} (Ch: (C_FPU, C_None, C_None)),
{FNDISI} (Ch: (C_FPU, C_None, C_None)),
 {FDIVR} (Ch: (C_FPU, C_None, C_None)),
  {FENI} (Ch: (C_FPU, C_None, C_None)),
 {FNENI} (Ch: (C_FPU, C_None, C_None)),
 {FFREE} (Ch: (C_FPU, C_None, C_None)),
 {FIADD} (Ch: (C_FPU, C_None, C_None)),
 {FICOM} (Ch: (C_FPU, C_None, C_None)),
{FICOMP} (Ch: (C_FPU, C_None, C_None)),
{FIDIVR} (Ch: (C_FPU, C_None, C_None)),
 {FIMUL} (Ch: (C_FPU, C_None, C_None)),
{FINCSTP}(Ch: (C_FPU, C_None, C_None)),
 {FINIT} (Ch: (C_FPU, C_None, C_None)),
{FNINIT} (Ch: (C_FPU, C_None, C_None)),
  {FIST} (Ch: (C_WOp1, C_None, C_None)),
 {FISTP} (Ch: (C_WOp1, C_None, C_None)),
 {FISUB} (Ch: (C_FPU, C_None, C_None)),
 {FSUBR} (Ch: (C_FPU, C_None, C_None)),
 {FLDCW} (Ch: (C_FPU, C_None, C_None)),
{FLDENV} (Ch: (C_FPU, C_None, C_None)),
{FLDLG2} (Ch: (C_FPU, C_None, C_None)),
{FLDLN2} (Ch: (C_FPU, C_None, C_None)),
{FLDL2E} (Ch: (C_FPU, C_None, C_None)),
{FLDL2T} (Ch: (C_FPU, C_None, C_None)),
 {FLDPI} (Ch: (C_FPU, C_None, C_None)),
  {FLDS} (Ch: (C_FPU, C_None, C_None)),
  {FLDZ} (Ch: (C_FPU, C_None, C_None)),
  {FNOP} (Ch: (C_FPU, C_None, C_None)),
{FPATAN} (Ch: (C_FPU, C_None, C_None)),
 {FPREM} (Ch: (C_FPU, C_None, C_None)),
{FPREM1} (Ch: (C_FPU, C_None, C_None)),
 {FPTAN} (Ch: (C_FPU, C_None, C_None)),
{FRNDINT}(Ch: (C_FPU, C_None, C_None)),
{FRSTOR} (Ch: (C_FPU, C_None, C_None)),
 {FSAVE} (Ch: (C_WOp1, C_None, C_None)),
{FNSAVE} (Ch: (C_FPU, C_None, C_None)),
{FSCALE} (Ch: (C_FPU, C_None, C_None)),
{FSETPM} (Ch: (C_FPU, C_None, C_None)),
  {FSIN} (Ch: (C_FPU, C_None, C_None)),
{FSINCOS}(Ch: (C_FPU, C_None, C_None)),
 {FSQRT} (Ch: (C_FPU, C_None, C_None)),
   {FST} (Ch: (C_WOp1, C_None, C_None)),
 {FSTCW} (Ch: (C_WOp1, C_None, C_None)),
{FNSTCW} (Ch: (C_WOp1, C_None, C_None)),
{FSTENV} (Ch: (C_WOp1, C_None, C_None)),
{FNSTENV}(Ch: (C_WOp1, C_None, C_None)),
 {FSTSW} (Ch: (C_WOp1, C_None, C_None)),
{FNSTSW} (Ch: (C_WOp1, C_None, C_None)),
  {FTST} (Ch: (C_FPU, C_None, C_None)),
 {FUCOM} (Ch: (C_FPU, C_None, C_None)),
{FUCOMP} (Ch: (C_FPU, C_None, C_None)),
{FUCOMPP}(Ch: (C_FPU, C_None, C_None)),
 {FWAIT} (Ch: (C_FPU, C_None, C_None)),
  {FXAM} (Ch: (C_FPU, C_None, C_None)),
{FXTRACT}(Ch: (C_FPU, C_None, C_None)),
 {FYL2X} (Ch: (C_FPU, C_None, C_None)),
{FYL2XP1}(Ch: (C_FPU, C_None, C_None)),
 {F2XM1} (Ch: (C_FPU, C_None, C_None)),
 {FILDQ} (Ch: (C_FPU, C_None, C_None)),
 {FILDS} (Ch: (C_FPU, C_None, C_None)),
 {FILDL} (Ch: (C_FPU, C_None, C_None)),
  {FLDL} (Ch: (C_FPU, C_None, C_None)),
  {FLDT} (Ch: (C_FPU, C_None, C_None)),
 {FISTQ} (Ch: (C_WOp1, C_None, C_None)),
 {FISTS} (Ch: (C_WOp1, C_None, C_None)),
 {FISTL} (Ch: (C_WOp1, C_None, C_None)),
  {FSTL} (Ch: (C_WOp1, C_None, C_None)),
  {FSTS} (Ch: (C_WOp1, C_None, C_None)),
 {FSTPS} (Ch: (C_WOp1, C_FPU, C_None)),
{FISTPL} (Ch: (C_WOp1, C_None, C_None)),
 {FSTPL} (Ch: (C_WOp1, C_FPU, C_None)),
{FISTPS} (Ch: (C_WOp1, C_FPU, C_None)),
{FISTPQ} (Ch: (C_WOp1, C_FPU, C_None)),
 {FSTPT} (Ch: (C_WOp1, C_FPU, C_None)),
{FCOMPS} (Ch: (C_FPU, C_None, C_None)),
{FICOMPL}(Ch: (C_FPU, C_None, C_None)),
{FCOMPL} (Ch: (C_FPU, C_None, C_None)),
{FICOMPS}(Ch: (C_FPU, C_None, C_None)),
 {FCOMS} (Ch: (C_FPU, C_None, C_None)),
{FICOML} (Ch: (C_FPU, C_None, C_None)),
 {FCOML} (Ch: (C_FPU, C_None, C_None)),
{FICOMS} (Ch: (C_FPU, C_None, C_None)),
{FIADDL} (Ch: (C_FPU, C_None, C_None)),
 {FADDL} (Ch: (C_FPU, C_None, C_None)),
{FIADDS} (Ch: (C_FPU, C_None, C_None)),
{FISUBL} (Ch: (C_FPU, C_None, C_None)),
 {FSUBL} (Ch: (C_FPU, C_None, C_None)),
{FISUBS} (Ch: (C_FPU, C_None, C_None)),
 {FSUBS} (Ch: (C_FPU, C_None, C_None)),
 {FSUBR} (Ch: (C_FPU, C_None, C_None)),
{FSUBRS} (Ch: (C_FPU, C_None, C_None)),
{FISUBRL}(Ch: (C_FPU, C_None, C_None)),
{FSUBRL} (Ch: (C_FPU, C_None, C_None)),
{FISUBRS}(Ch: (C_FPU, C_None, C_None)),
 {FMULS} (Ch: (C_FPU, C_None, C_None)),
 {FIMUL} (Ch: (C_FPU, C_None, C_None)),
 {FMULL} (Ch: (C_FPU, C_None, C_None)),
{FIMULS} (Ch: (C_FPU, C_None, C_None)),
{FIDIVS} (Ch: (C_FPU, C_None, C_None)),
{FIDIVL} (Ch: (C_FPU, C_None, C_None)),
 {FDIVL} (Ch: (C_FPU, C_None, C_None)),
{FIDIVS} (Ch: (C_FPU, C_None, C_None)),
{FDIVRS} (Ch: (C_FPU, C_None, C_None)),
{FIDIVRL}(Ch: (C_FPU, C_None, C_None)),
{FDIVRL} (Ch: (C_FPU, C_None, C_None)),
{FIDIVRS}(Ch: (C_FPU, C_None, C_None)),
  {REPE} (Ch: (C_RWECX, C_RFlags, C_None)),
 {REPNE} (Ch: (C_RWECX, C_RFlags, C_None)),
 {CPUID} (Ch: (C_All, C_None, C_none)),
 {FADDS} (Ch: (C_FPU, C_None, C_None)),
 {POPFD} (Ch: (C_RWESP, C_WFlags, C_None)),
{below are the MMX instructions}
{A_EMMS} (Ch: (C_FPU, C_None, C_None)),
{A_MOVD} (Ch: (C_WOp2, C_None, C_None)),
{A_MOVQ} (Ch: (C_WOp2, C_None, C_None)),
{A_PACKSSDW} (Ch: (C_All, C_None, C_None)),
{A_PACKSSWB} (Ch: (C_All, C_None, C_None)),
{A_PACKUSWB} (Ch: (C_All, C_None, C_None)),
{A_PADDB} (Ch: (C_RWOp2, C_None, C_None)),
{A_PADDD} (Ch: (C_RWOp2, C_None, C_None)),
{A_PADDSB} (Ch: (C_RWOp2, C_None, C_None)),
{A_PADDSW} (Ch: (C_RWOp2, C_None, C_None)),
{A_PADDUSB} (Ch: (C_RWOp2, C_None, C_None)),
{A_PADDUSW} (Ch: (C_RWOp2, C_None, C_None)),
{A_PADDW} (Ch: (C_RWOp2, C_None, C_None)),
{A_PAND} (Ch: (C_RWOp2, C_None, C_None)),
{A_PANDN} (Ch: (C_RWOp2, C_None, C_None)),
{A_PCMPEQB} (Ch: (C_All, C_None, C_None)),
{A_PCMPEQD} (Ch: (C_All, C_None, C_None)),
{A_PCMPEQW} (Ch: (C_All, C_None, C_None)),
{A_PCMPGTB} (Ch: (C_All, C_None, C_None)),
{A_PCMPGTD} (Ch: (C_All, C_None, C_None)),
{A_PCMPGTW} (Ch: (C_All, C_None, C_None)),
{A_PMADDWD} (Ch: (C_RWOp2, C_None, C_None)),
{A_PMULHW} (Ch: (C_All, C_None, C_None)),
{A_PMULLW} (Ch: (C_All, C_None, C_None)),
{A_POR} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSLLD} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSLLQ} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSLLW} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSRAD} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSRAW} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSRLD} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSRLQ} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSRLW} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSUBB} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSUBD} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSUBSB} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSUBSW} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSUBUSB} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSUBUSW} (Ch: (C_RWOp2, C_None, C_None)),
{A_PSUBW} (Ch: (C_RWOp2, C_None, C_None)),
{A_PUNPCKHBW} (Ch: (C_All, C_None, C_None)),
{A_PUNPCKHDQ} (Ch: (C_All, C_None, C_None)),
{A_PUNPCKHWD} (Ch: (C_All, C_None, C_None)),
{A_PUNPCKLBW} (Ch: (C_All, C_None, C_None)),
{A_PUNPCKLDQ} (Ch: (C_All, C_None, C_None)),
{A_PUNPCKLWD} (Ch: (C_All, C_None, C_None)),
{A_PXOR} (Ch: (C_RWOp2, C_None, C_None)));

Var
 {How many instructions are between the current instruction and the last one
  that modified the register}
  NrOfInstrSinceLastMod: TInstrSinceLastMod;


{************************ Create the Label table ************************}

Function FindLoHiLabels(AsmL: PAasmOutput; Var LowLabel, HighLabel, LabelDif: Longint; BlockStart: Pai): Pai;
{Walks through the paasmlist to find the lowest and highest label number;
 Since 0.9.3: also removes unused labels}
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
              If (Pai_Label(p)^.l^.nb < LowLabel) Then
                LowLabel := Pai_Label(p)^.l^.nb;
              If (Pai_Label(p)^.l^.nb > HighLabel) Then
                HighLabel := Pai_Label(p)^.l^.nb;
            End
{          Else
            Begin
              hp1 := pai(p^.next);
              AsmL^.Remove(p);
              Dispose(p, Done);
              p := hp1;
              continue;
            End};
      GetNextInstruction(p, p);
    End;
  FindLoHiLabels := p;
  If LabelFound
    Then LabelDif := HighLabel+1-LowLabel
    Else LabelDif := 0;
End;

Function FindRegAlloc(Reg: TRegister; StartPai: Pai): Boolean;
{Returns true if a ait_regalloc object for Reg is found in the block of Pai's
 starting with StartPai and ending with the next "real" instruction}
Var TmpResult: Boolean;
Begin
  TmpResult := False;
  Repeat
    While Assigned(StartPai) And
          ((StartPai^.typ in (SkipInstr - [ait_RegAlloc])) Or
           ((StartPai^.typ = ait_label) and
            Not(Pai_Label(StartPai)^.l^.Is_Used))) Do
      StartPai := Pai(StartPai^.Next);
    If Assigned(StartPai) And
       (StartPai^.typ = ait_RegAlloc) Then
      Begin
        TmpResult := (PaiRegAlloc(StartPai)^.Reg = Reg);
        StartPai := Pai(StartPai^.Next);
      End;
  Until Not(Assigned(StartPai)) Or
        Not(StartPai^.typ in SkipInstr) or TmpResult;
  FindRegAlloc := TmpResult;
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
                      LabelTable^[Pai_Label(p)^.l^.nb-LowLabel].PaiObj := p;
                  ait_RegAlloc:
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
                             hp1 := New(PaiRegDeAlloc, Init(PaiRegAlloc(p)^.Reg));
                             InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp1);
                           End;
                        End;
                    End;
                  ait_RegDeAlloc:
                    Begin
                      UsedRegs := UsedRegs - [PaiRegDeAlloc(p)^.Reg];
                      hp1 := p;
                      hp2 := nil;
                      While Not(FindRegAlloc(PaiRegDeAlloc(p)^.Reg, Pai(hp1^.Next))) And
                            GetNextInstruction(hp1, hp1) And
                            RegInInstruction(PaiRegDeAlloc(p)^.Reg, hp1) Do
                        hp2 := hp1;
                      If hp2 <> nil Then
                        Begin
                          hp1 := Pai(p^.previous);
                          AsmL^.Remove(p);
                          InsertLLItem(AsmL, hp2, Pai(hp2^.Next), p);
                          p := hp1;
                        End;
                    End;
                End;
                P := Pai(p^.Next);
                While Assigned(p) And
                      (p^.typ in (SkipInstr - [ait_regdealloc,ait_regalloc])) Do
                  P := Pai(P^.Next);
              End;
{$IfDef TP}
          End
        Else LabelDif := 0;
{$EndIf TP}
    End;
End;

{************************ Search the Label table ************************}

Function FindLabel(L: PLabel; Var hp: Pai): Boolean;

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

Procedure AddOp2RegInfo(typ: Longint; Op: Pointer; Var RegInfo: TRegInfo);
Begin
  Case typ Of
    Top_Reg:
      If (TRegister(op) <> R_NO) Then
        AddReg2RegInfo(TRegister(op), TRegister(op), RegInfo);
    Top_Ref:
      Begin
        If TReference(op^).base <> R_NO Then
          AddReg2RegInfo(TReference(op^).base, TReference(op^).base, RegInfo);
        If TReference(op^).index <> R_NO Then
          AddReg2RegInfo(TReference(op^).index, TReference(op^).index, RegInfo);
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
  If R1.IsIntValue
     Then RefsEquivalent := R2.IsIntValue and (R1.Offset = R2.Offset)
     Else If (R1.Offset = R2.Offset) And
             RegsEquivalent(R1.Base, R2.Base, RegInfo, OpAct) And
             RegsEquivalent(R1.Index, R2.Index, RegInfo, OpAct) And
             (R1.Segment = R2.Segment) And (R1.ScaleFactor = R2.ScaleFactor)
            Then
              Begin
                If Assigned(R1.Symbol)
                  Then RefsEquivalent := Assigned(R2.Symbol) And (R1.Symbol^=R2.Symbol^)
                  Else RefsEquivalent := Not(Assigned(R2.Symbol));
              End
            Else RefsEquivalent := False;
End;


Function RefsEqual(Const R1, R2: TReference): Boolean;
Begin
  If R1.IsIntValue
     Then RefsEqual := R2.IsIntValue and (R1.Offset = R2.Offset)
     Else If (R1.Offset = R2.Offset) And (R1.Base = R2.Base) And
             (R1.Index = R2.Index) And (R1.Segment = R2.Segment) And
             (R1.ScaleFactor = R2.ScaleFactor)
            Then
              Begin
                If Assigned(R1.Symbol)
                  Then RefsEqual := Assigned(R2.Symbol) And (R1.Symbol^=R2.Symbol^)
                  Else RefsEqual := Not(Assigned(R2.Symbol));
              End
            Else RefsEqual := False;
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
Var TmpResult: Boolean;
Begin
  TmpResult := False;
  If (Pai(p1)^.typ = ait_instruction) Then
    Begin
      Case Pai386(p1)^.op1t Of
        Top_Reg: TmpResult := Reg = TRegister(Pai386(p1)^.op1);
        Top_Ref: TmpResult := RegInRef(Reg, TReference(Pai386(p1)^.op1^))
      End;
      If Not(TmpResult) Then
        Case Pai386(p1)^.op2t Of
          Top_Reg:
              if Pai386(p1)^.op3t<>Top_reg
                then TmpResult := Reg = TRegister(Pai386(p1)^.op2)
                else TmpResult := longint(Reg) = twowords(Pai386(p1)^.op2).word1;
          Top_Ref: TmpResult := RegInRef(Reg, TReference(Pai386(p1)^.op2^))
        End;
      If Not(TmpResult) Then
        Case Pai386(p1)^.op3t Of
          Top_Reg: TmpResult := longint(Reg) =twowords(Pai386(p1)^.op2).word2;
          Top_none:;
          else
             internalerror($Da);
       End
    End;
  RegInInstruction := TmpResult
End;

{Function RegInOp(Reg: TRegister; opt: Longint; op: Pointer): Boolean;
Begin
  RegInOp := False;
  Case opt Of
    top_reg: RegInOp := Reg = TRegister(Pointer);
    top_ref: RegInOp := (Reg = TReference(op^).Base) Or
                        (Reg = TReference(op^).Index);
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
        PPAiProp(p1^.fileinfo.line)^.Regs[Reg].WState <>
          PPAiProp(hp^.fileinfo.line)^.Regs[Reg].WState
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
          ((Pai(Current)^.typ In SkipInstr) or
           ((Pai(Current)^.typ = ait_label) And
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
 Last := Current;
  If Assigned(Current) And
     Not((Current^.typ In SkipInstr) or
         ((Current^.typ = ait_label) And
          Not(Pai_Label(Current)^.l^.is_used)))
    Then GetLastInstruction := True
    Else
      Begin
        Last := Nil;
        GetLastInstruction := False
      End;
End;

Procedure SkipHead(var P: Pai);
Var OldP: Pai;
Begin
  Repeat
    OldP := P;
    If (P^.typ in SkipInstr) Then
      GetNextInstruction(P, P)
    Else If ((P^.Typ = Ait_Marker) And
        (Pai_Marker(P)^.Kind = NoPropInfoStart)) Then
   {a marker of the NoPropInfoStart can4t be the first instruction of a
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
          ((p^.typ in (SkipInstr - [ait_RegAlloc, ait_RegDealloc])) or
           ((p^.typ = ait_label) And
            Not(Pai_Label(p)^.l^.is_used))) Do
         p := Pai(p^.next);
    While Assigned(p) And
          (p^.typ in [ait_RegAlloc, ait_RegDealloc]) Do
      Begin
        Case p^.typ Of
          ait_RegAlloc: UsedRegs := UsedRegs + [PaiRegAlloc(p)^.Reg];
          ait_regdealloc: UsedRegs := UsedRegs - [PaiRegDeAlloc(p)^.Reg];
        End;
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
        ((PPaiProp(p^.fileinfo.line)^.Regs[Counter].Typ <> Con_Const) or
         (PPaiProp(p^.fileinfo.line)^.Regs[Counter].StartMod <> Pointer(0))) Do
    Inc(Byte(Counter));
  If (PPaiProp(p^.fileinfo.line)^.Regs[Counter].Typ = Con_Const) And
     (PPaiProp(p^.fileinfo.line)^.Regs[Counter].StartMod = Pointer(0))
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
         ((Pai386(p)^._operator = A_MOV) or
          (Pai386(p)^._operator = A_MOVZX) or
          (Pai386(p)^._operator = A_MOVSX))
        Then
          If (Pai386(p)^.op1t = top_ref)
            Then
              With TReference(Pai386(p)^.op1^) Do
                If (Base = ProcInfo.FramePointer) And
                   (Index = R_NO)
                  Then RegsChecked := RegsChecked + [Reg32(TRegister(Pai386(p)^.op2))]
                  Else
                    Begin
                      If (Base = Reg) And
                         Not(Base In RegsChecked)
                        Then TmpResult := True;
                      If Not(TmpResult) And
                         (Index = Reg) And
                         Not(Index In RegsChecked)
                        Then TmpResult := True;
                    End;
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
      Case Pai386(p)^.op1t Of
        top_reg:
          If Not(TRegister(Pai386(p)^.op1) in [R_NO,R_ESP,ProcInfo.FramePointer]) Then
            RegSet := RegSet + [TRegister(Pai386(p)^.op1)];
        top_ref:
          With TReference(Pai386(p)^.op1^) Do
            Begin
              If Not(Base in [ProcInfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Base];
              If Not(Index in [ProcInfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Index];
            End;
      End;
      Case Pai386(p)^.op2t Of
        top_reg:
          If Not(TRegister(Pai386(p)^.op2) in [R_NO,R_ESP,ProcInfo.FramePointer]) Then
            If RegSet := RegSet + [TRegister(TwoWords(Pai386(p)^.op2).Word1];
        top_ref:
          With TReference(Pai386(p)^.op2^) Do
            Begin
              If Not(Base in [ProcInfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Base];
              If Not(Index in [ProcInfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Index];
            End;
      End;
    End;
End;}

Function OpsEquivalent(typ: Longint; OldOp, NewOp: Pointer; Var RegInfo: TRegInfo; OpAct: TopAction): Boolean;
Begin {checks whether the two ops are equivalent}
  Case typ Of
    Top_Reg: OpsEquivalent :=RegsEquivalent(TRegister(OldOp), TRegister(NewOp), RegInfo, OpAct);
    Top_Const: OpsEquivalent := OldOp = NewOp;
    Top_Ref: OpsEquivalent := RefsEquivalent(TReference(OldOp^), TReference(NewOp^), RegInfo, OpAct);
    Top_None: OpsEquivalent := True
    Else OpsEquivalent := False
  End;
End;


Function OpsEqual(typ: Longint; op1, op2: Pointer): Boolean;
Begin {checks whether the two ops are equal}
  Case typ Of
    Top_Reg, Top_Const: OpsEqual := op1 = op2;
    Top_Ref: OpsEqual := RefsEqual(TReference(op1^), TReference(op2^));
    Top_None: OpsEqual := True
    Else OpsEqual := False
  End;
End;

Function InstructionsEquivalent(p1, p2: Pai; Var RegInfo: TRegInfo): Boolean;
Begin {checks whether two Pai386 instructions are equal}
  If Assigned(p1) And Assigned(p2) And
     (Pai(p1)^.typ = ait_instruction) And
     (Pai(p1)^.typ = ait_instruction) And
     (Pai386(p1)^._operator = Pai386(p2)^._operator) And
     (Pai386(p1)^.op1t = Pai386(p2)^.op1t) And
     (Pai386(p1)^.op2t = Pai386(p2)^.op2t) And
     (Pai386(p1)^.op3t = Pai386(p2)^.op3t)
    Then
 {both instructions have the same structure:
  "<operator> <operand of type1>, <operand of type 2>"}
      If ((Pai386(p1)^._operator = A_MOV) or
          (Pai386(p1)^._operator = A_MOVZX) or
          (Pai386(p1)^._operator = A_MOVSX)) And
         (Pai386(p1)^.op1t = top_ref) {then op2t = top_reg} Then
        If Not(RegInRef(TRegister(Pai386(p1)^.op2), TReference(Pai386(p1)^.op1^))) Then
 {the "old" instruction is a load of a register with a new value, not with
  a value based on the contents of this register (so no "mov (reg), reg")}
          If Not(RegInRef(TRegister(Pai386(p2)^.op2), TReference(Pai386(p2)^.op1^))) And
             RefsEqual(TReference(Pai386(p1)^.op1^), TReference(Pai386(p2)^.op1^))
            Then
 {the "new" instruction is also a load of a register with a new value, and
  this value is fetched from the same memory location}
              Begin
                With TReference(Pai386(p2)^.op1^) Do
                  Begin
                    If Not(Base in [ProcInfo.FramePointer, R_NO, R_ESP])
       {it won't do any harm if the register is already in RegsLoadedForRef}
                      Then RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Base];
                    If Not(Index in [ProcInfo.FramePointer, R_NO, R_ESP])
                      Then RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Index];
                  End;
 {add the registers from the reference (op1) to the RegInfo, all registers
  from the reference are the same in the old and in the new instruction
  sequence}
                AddOp2RegInfo(Pai386(p1)^.op1t, Pai386(p1)^.op1, RegInfo);
 {the registers from op2 have to be equivalent, but not necessarily equal}
                InstructionsEquivalent :=
                  RegsEquivalent(TRegister(Pai386(p1)^.op2), TRegister(Pai386(p2)^.op2),
                                 RegInfo, OpAct_Write);
              End
 {the registers are loaded with values from different memory locations. If
  this was allowed, the instructions "mov -4(esi),eax" and "mov -4(ebp),eax"
  would be considered equivalent}
            Else InstructionsEquivalent := False
        Else
 {load register with a value based on the current value of this register}
          Begin
            With TReference(Pai386(p2)^.op1^) Do
              Begin
                If Not(Base in [ProcInfo.FramePointer,
                                Reg32(TRegister(Pai386(p2)^.op2)),R_NO,R_ESP])
 {it won't do any harm if the register is already in RegsLoadedForRef}
                  Then
                    Begin
                      RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Base];
{$ifdef csdebug}
                      Writeln(att_reg2str[base], ' added');
{$endif csdebug}
                    end;
                If Not(Index in [ProcInfo.FramePointer,
                                 Reg32(TRegister(Pai386(p2)^.op2)),R_NO,R_ESP])
                  Then
                    Begin
                      RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Index];
{$ifdef csdebug}
                      Writeln(att_reg2str[index], ' added');
{$endif csdebug}
                    end;

              End;
            If Not(Reg32(TRegister(Pai386(p2)^.op2)) In [ProcInfo.FramePointer,
                                                         R_NO,R_ESP])
              Then
                Begin
                  RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef -
                                                 [Reg32(TRegister(Pai386(p2)^.op2))];
{$ifdef csdebug}
                  Writeln(att_reg2str[Reg32(TRegister(Pai386(p2)^.op2))], ' removed');
{$endif csdebug}
                end;
            InstructionsEquivalent :=
               OpsEquivalent(Pai386(p1)^.op1t, Pai386(p1)^.op1, Pai386(p2)^.op1, RegInfo, OpAct_Read) And
               OpsEquivalent(Pai386(p1)^.op2t, Pai386(p1)^.op2, Pai386(p2)^.op2, RegInfo, OpAct_Write)
          End
      Else
 {an instruction <> mov, movzx, movsx}
        If (Pai386(p1)^.op3t = top_none) Then
          InstructionsEquivalent :=
            OpsEquivalent(Pai386(p1)^.op1t, Pai386(p1)^.op1, Pai386(p2)^.op1, RegInfo, OpAct_Unknown) And
            OpsEquivalent(Pai386(p1)^.op2t, Pai386(p1)^.op2, Pai386(p2)^.op2, RegInfo, OpAct_Unknown)
        Else
          InstructionsEquivalent :=
            OpsEquivalent(Pai386(p1)^.op1t, Pai386(p1)^.op1, Pai386(p2)^.op1, RegInfo, OpAct_Unknown) And
            OpsEquivalent(Pai386(p1)^.op2t, Pointer(Longint(TwoWords(Pai386(p1)^.op2).Word1)),
                          Pointer(Longint(TwoWords(Pai386(p2)^.op2).Word1)), RegInfo, OpAct_Unknown) And
            OpsEquivalent(Pai386(p1)^.op3t, Pointer(Longint(TwoWords(Pai386(p1)^.op2).Word2)),
                          Pointer(Longint(TwoWords(Pai386(p2)^.op2).Word2)), RegInfo, OpAct_Unknown)
 {the instructions haven't even got the same structure, so they're certainly
  not equivalent}
    Else InstructionsEquivalent := False;
End;

(*
Function InstructionsEqual(p1, p2: Pai): Boolean;
Begin {checks whether two Pai386 instructions are equal}
  InstructionsEqual :=
    Assigned(p1) And Assigned(p2) And
    ((Pai(p1)^.typ = ait_instruction) And
     (Pai(p1)^.typ = ait_instruction) And
     (Pai386(p1)^._operator = Pai386(p2)^._operator) And
     (Pai386(p1)^.op1t = Pai386(p2)^.op1t) And
     (Pai386(p1)^.op2t = Pai386(p2)^.op2t) And
     OpsEqual(Pai386(p1)^.op1t, Pai386(p1)^.op1, Pai386(p2)^.op1) And
     OpsEqual(Pai386(p1)^.op2t, Pai386(p1)^.op2, Pai386(p2)^.op2))
End;
*)

Function RefInInstruction(Const Ref: TReference; p: Pai): Boolean;
{checks whehter Ref is used in P}
Var TmpResult: Boolean;
Begin
  TmpResult := False;
  If (p^.typ = ait_instruction) Then
    Begin
      If (Pai386(p)^.op1t = Top_Ref)
        Then TmpResult := RefsEqual(Ref, TReference(Pai386(p)^.op1^));
      If Not(TmpResult) And
        (Pai386(p)^.op2t = Top_Ref)
        Then TmpResult := RefsEqual(Ref, TReference(Pai386(p)^.op2^));
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
        With PPaiProp(p^.fileinfo.line)^.Regs[Counter] Do
          Begin
            If (typ = Con_Ref) And
               (Not(cs_UncertainOpts in aktglobalswitches) And
                (NrOfMods <> 1)
               ) Or
               (RefInSequence(Ref,PPaiProp(p^.fileinfo.line)^.Regs[Counter]) And
                ((Counter <> WhichReg) Or
                 ((NrOfMods = 1) And
 {StarMod is always of the type ait_instruction}
                  (Pai386(StartMod)^.op1t = top_ref) And
                  RefsEqual(TReference(Pai386(StartMod)^.op1^), Ref)
                 )
                )
               )
              Then DestroyReg(PPaiProp(p^.fileinfo.line), Counter)
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
        With PPaiProp(p^.fileinfo.line)^.Regs[Counter] Do
        If (typ = Con_Ref) And
           (Not(cs_UncertainOpts in aktglobalswitches) Or
        {for movsl}
            (Ref.Base = R_EDI) Or
        {don't destroy if reg contains a parameter, local or global variable}
            Not((NrOfMods = 1) And
                (Pai386(StartMod)^.op1t = top_ref) And
                ((PReference(Pai386(StartMod)^.op1)^.base = ProcInfo.FramePointer) Or
                  Assigned(PReference(Pai386(StartMod)^.op1)^.Symbol)
                )
               )
           )
          Then DestroyReg(PPaiProp(p^.FileInfo.Line), Counter)
End;

Procedure DestroyAllRegs(p: PPaiProp);
Var Counter: TRegister;
Begin {initializes/desrtoys all registers}
  For Counter := R_EAX To R_EDI Do
    DestroyReg(p, Counter);
  p^.DirFlag := F_Unknown;
End;

Procedure Destroy(PaiObj: Pai; Opt: Longint; Op: Pointer);
Begin
  Case Opt Of
    top_reg: DestroyReg(PPaiProp(PaiObj^.fileinfo.line), TRegister(Op));
    top_ref: DestroyRefs(PaiObj, TReference(Op^), R_NO);
    top_symbol:;
  End;
End;

Procedure ReadReg(p: PPaiProp; Reg: TRegister);
Begin
    IncState(p^.Regs[Reg32(Reg)].RState)
End;

Procedure ReadRef(p: PPaiProp; Ref: PReference);
Begin
  If Ref^.Base <> R_NO Then
    ReadReg(p, Ref^.Base);
  If Ref^.Index <> R_NO Then
    ReadReg(p, Ref^.Index);
End;

Procedure ReadOp(P: PPaiProp; opt: Longint; Op: Pointer);
Begin
  Case Opt Of
    top_reg: ReadReg(P, TRegister(Op));
    top_ref: ReadRef(P, PReference(Op));
    top_symbol:
  End;
End;

Function DFAPass1(AsmL: PAasmOutput; BlockStart: Pai): Pai;
{gathers the RegAlloc data... still need to think about where to store it to
 avoid global vars}
Var BlockEnd: Pai;
Begin
  BlockEnd := FindLoHiLabels(AsmL, LoLab, HiLab, LabDif, BlockStart);
  BuildLabelTableAndFixRegAlloc(AsmL, LTable, LoLab, LabDif, BlockStart, BlockEnd);
  DFAPass1 := BlockEnd;
End;

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
  If (BlockStart^.typ in SkipInstr) Then
    GetNextInstruction(p, p);
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
                CurProp^.Regs := PPaiProp(hp^.fileinfo.line)^.Regs;
                CurProp^.DirFlag := PPaiProp(hp^.fileinfo.line)^.DirFlag;
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
      CurProp^.linesave := p^.fileinfo.line;
      PPaiProp(p^.fileinfo.line) := CurProp;
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
             With LTable^[Pai_Label(p)^.l^.nb-LoLab] Do
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
                           Not(((hp^.typ = ait_labeled_instruction) or
                                (hp^.typ = ait_instruction)) And
                                (Pai_Labeled(hp)^._operator = A_JMP))
                          Then
  {previous instruction not a JMP -> the contents of the registers after the
   previous intruction has been executed have to be taken into account as well}
                            For TmpReg := R_EAX to R_EDI Do
                              Begin
                                If (CurProp^.Regs[TmpReg].WState <>
                                    PPaiProp(hp^.FileInfo.Line)^.Regs[TmpReg].WState)
                                  Then DestroyReg(CurProp, TmpReg)
                              End
                      End
{$IfDef AnalyzeLoops}
                    Else
 {a label from a backward jump (e.g. a loop), no jump to this label has
  already been processed}
                      If GetLastInstruction(p, hp) And
                         Not(hp^.typ = ait_labeled_instruction) And
                            (Pai_Labeled(hp)^._operator = A_JMP))
                        Then
  {previous instruction not a jmp, so keep all the registers' contents from the
   previous instruction}
                          Begin
                            CurProp^.Regs := PPaiProp(hp^.FileInfo.Line)^.Regs;
                            CurProp^.DirFlag := PPaiProp(hp^.FileInfo.Line)^.DirFlag;
                          End
                        Else
  {previous instruction a jmp and no jump to this label processed yet}
                          Begin
                            hp := p;
                            Cnt := InstrCnt;
     {continue until we find a jump to the label or a label which has already
      been processed}
                            While GetNextInstruction(hp, hp) And
                                  Not((hp^.typ = ait_labeled_instruction) And
                                      (Pai_Labeled(hp)^.lab^.nb = Pai_Label(p)^.l^.nb)) And
                                  Not((hp^.typ = ait_label) And
                                      (LTable^[Pai_Label(hp)^.l^.nb-LoLab].RefsFound
                                       = Pai_Label(hp)^.l^.RefCount) And
                                      (LTable^[Pai_Label(hp)^.l^.nb-LoLab].JmpsProcessed > 0)) Do
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
                                  CurProp^.Regs := PPaiProp(hp^.FileInfo.Line)^.Regs;
                                  CurProp^.DirFlag := PPaiProp(hp^.FileInfo.Line)^.DirFlag;
                                  DestroyAllRegs(PPaiProp(hp^.FileInfo.Line))
                                End
                          End
{$EndIf AnalyzeLoops}
                Else
{not all references to this label have been found, so destroy all registers}
                  Begin
                    GetLastInstruction(p, hp);
                    CurProp^.Regs := PPaiProp(hp^.FileInfo.Line)^.Regs;
                    CurProp^.DirFlag := PPaiProp(hp^.FileInfo.Line)^.DirFlag;
                    DestroyAllRegs(CurProp)
                  End;
          End;
{$EndIf JumpAnal}
        ait_labeled_instruction:
{$IfNDef JumpAnal}
  ;
{$Else JumpAnal}
          With LTable^[Pai_Labeled(p)^.lab^.nb-LoLab] Do
            If (RefsFound = Pai_Labeled(p)^.lab^.RefCount) Then
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
                           (Pai_Labeled(hp)^._operator = A_JMP))
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
{$ifdef GDB}
        ait_stabs, ait_stabn, ait_stab_function_name:;
{$endif GDB}
        ait_instruction:
          Begin
            InstrProp := AsmInstr[Pai386(p)^._operator];
            Case Pai386(p)^._operator Of
              A_MOV, A_MOVZX, A_MOVSX:
                Begin
                  Case Pai386(p)^.op1t Of
                    Top_Reg:
                      Case Pai386(p)^.op2t Of
                        Top_Reg:
                          Begin
                            DestroyReg(CurProp, TRegister(Pai386(p)^.op2));
                            ReadReg(CurProp, TRegister(Pai386(p)^.op1));
{                            CurProp^.Regs[TRegister(Pai386(p)^.op2)] :=
                              CurProp^.Regs[TRegister(Pai386(p)^.op1)];
                            If (CurProp^.Regs[TRegister(Pai386(p)^.op2)].ModReg = R_NO) Then
                              CurProp^.Regs[TRegister(Pai386(p)^.op2)].ModReg :=
                                Tregister(Pai386(p)^.op1);}
                          End;
                        Top_Ref:
                          Begin
                            ReadReg(CurProp, TRegister(Pai386(p)^.op1));
                            ReadRef(CurProp, PReference(Pai386(p)^.op2));
                            DestroyRefs(p, TReference(Pai386(p)^.op2^), TRegister(Pai386(p)^.op1));
                          End;
                      End;
                    Top_Ref:
                      Begin {destination is always a register in this case}
                        ReadRef(CurProp, PReference(Pai386(p)^.op1));
                        ReadReg(CurProp, TRegister(Pai386(p)^.Op2));
                        TmpReg := Reg32(TRegister(Pai386(p)^.op2));
                        If RegInRef(TmpReg, TReference(Pai386(p)^.op1^)) And
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
                                  PPaiProp(Pai(StartMod)^.fileinfo.line)^.Regs[TmpReg].NrOfMods := NrOfMods;
                                  NrOfInstrSinceLastMod[TmpReg] := 0;
                                End;
                            End
                          Else
                            Begin
                              DestroyReg(CurProp, TmpReg);
                              If Not(RegInRef(TmpReg, TReference(Pai386(p)^.op1^))) Then
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
                        Case Pai386(p)^.op2t Of
                          Top_Reg:
                            Begin
                              TmpReg := Reg32(TRegister(Pai386(p)^.op2));
                              With CurProp^.Regs[TmpReg] Do
                                Begin
                                  DestroyReg(CurProp, TmpReg);
                                  typ := Con_Const;
                                  StartMod := Pai386(p)^.op1;
                                End
                            End;
                          Top_Ref:
                            Begin
                              ReadRef(CurProp, PReference(Pai386(p)^.op2));
                              DestroyRefs(P, TReference(Pai386(p)^.op2^), R_NO);
                            End;
                        End;
                      End;
                End;
              End;
              A_IMUL:
                Begin
                  ReadOp(CurProp, Pai386(p)^.Op1t, Pai386(p)^.Op1);
                  If (Pai386(p)^.Op2t = Top_Ref) Then
                    ReadOp(CurProp, Pai386(p)^.Op2t, Pai386(p)^.Op2)
                  Else ReadOp(CurProp, Pai386(p)^.Op2t, Pointer(Longint(TwoWords(Pai386(p)^.Op2).Word1)));
                  ReadOp(CurProp, Pai386(p)^.Op3t, Pointer(LongInt(TwoWords(Pai386(p)^.Op2).Word2)));
                  If (Pai386(p)^.Op3t = top_none)
                   Then
                     If (Pai386(p)^.Op2t = top_none)
                       Then
                         Begin
                           DestroyReg(CurProp, R_EAX);
                           DestroyReg(CurProp, R_EDX)
                         End
                       Else Destroy(p, Pai386(p)^.Op2t, Pai386(p)^.Op2)
                   Else DestroyReg(CurProp, TRegister(longint(twowords(Pai386(p)^.Op2).word2)));
                End;
              A_XOR:
                Begin
                  ReadOp(CurProp, Pai386(p)^.Op1t, Pai386(p)^.Op1);
                  ReadOp(CurProp, Pai386(p)^.Op2t, Pai386(p)^.Op2);
                  If (Pai386(p)^.op1t = top_reg) And
                     (Pai386(p)^.op2t = top_reg) And
                     (Pai386(p)^.op1 = Pai386(p)^.op2)
                    Then
                      Begin
                        DestroyReg(CurProp, Tregister(Pai386(p)^.op1));
                        CurProp^.Regs[Reg32(Tregister(Pai386(p)^.op1))].typ := Con_Const;
                        CurProp^.Regs[Reg32(Tregister(Pai386(p)^.op1))].StartMod := Pointer(0)
                      End
                    Else Destroy(p, Pai386(p)^.op2t, Pai386(p)^.op2);
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
                        C_CDirFlag: CurProp^.DirFlag := F_NotSet;
                        C_SDirFlag: CurProp^.DirFlag := F_Set;
                        C_ROp1: ReadOp(CurProp, Pai386(p)^.op1t, Pai386(p)^.op1);
                        C_ROp2: If (Pai386(p)^.Op3t = top_none) Then
                                  ReadOp(CurProp, Pai386(p)^.op2t, Pai386(p)^.op2)
                                Else ReadOp(CurProp, Pai386(p)^.op2t, Pointer(Longint(TwoWords(Pai386(p)^.op2).word1)));
                        C_ROp3: ReadOp(CurProp, Pai386(p)^.op3t, Pointer(Longint(TwoWords(Pai386(p)^.op2).word2)));
                        C_WOp1..C_RWOp1:
                          Begin
                            If (InstrProp.Ch[Cnt] = C_RWOp1) Then
                              ReadOp(CurProp, Pai386(p)^.op1t, Pai386(p)^.op1);
                            Destroy(p, Pai386(p)^.op1t, Pai386(p)^.op1);
                          End;
                        C_WOp2..C_RWOp2:
                          Begin
                            If (InstrProp.Ch[Cnt] = C_RWOp2) Then
                              If (Pai386(p)^.Op3t = top_none) Then
                                ReadOp(CurProp, Pai386(p)^.op2t, Pai386(p)^.op2)
                              Else ReadOp(CurProp, Pai386(p)^.op2t, Pointer(Longint(TwoWords(Pai386(p)^.op2).word1)));
                            If (Pai386(p)^.Op3t = top_none) Then
                              Destroy(p, Pai386(p)^.op2t, Pai386(p)^.op2)
                            Else Destroy(p, Pai386(p)^.op2t, Pointer(Longint(TwoWords(Pai386(p)^.op2).word1)));
                          End;
                        C_WOp3..C_RWOp3:
                          Begin
                            If (InstrProp.Ch[Cnt] = C_RWOp3) Then
                              ReadOp(CurProp, Pai386(p)^.op3t, Pointer(Longint(TwoWords(Pai386(p)^.op2).word2)));
                            Destroy(p, Pai386(p)^.op3t, Pointer(Longint(TwoWords(Pai386(p)^.op2).word2)));
                          End;
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

Function InitDFAPass2(AsmL: PAasmOutput; BlockStart, BlockEnd: Pai): Boolean;
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
        ait_labeled_instruction:
          begin
            If (Pai_Labeled(P)^.lab^.nb >= LoLab) And
               (Pai_Labeled(P)^.lab^.nb <= HiLab) Then
            Inc(LTable^[Pai_Labeled(P)^.lab^.nb-LoLab].RefsFound);
          end;
        ait_label:
          Begin
            If (Pai_Label(p)^.l^.is_used) Then
              LTable^[Pai_Label(P)^.l^.nb-LoLab].InstrNr := NrOfPaiObjs
          End;
{        ait_instruction:
          Begin
           If (Pai386(p)^._operator = A_PUSH) And
              (Pai386(p)^.op1t = top_symbol) And
              (PCSymbol(Pai386(p)^.op1)^.offset = 0) Then
             Begin
               TmpStr := StrPas(PCSymbol(Pai386(p)^.op1)^.symbol);
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
          PaiPropBlock^[Count].LineSave := p^.fileinfo.line;
          PPaiProp(p^.fileinfo.line) := @PaiPropBlock^[Count];
          GetNextInstruction(p, p);
        End;
    End
  Else InitDFAPass2 := False;
 {$EndIf TP}
End;

Function DFAPass2(AsmL: PAasmOutPut; BlockStart, BlockEnd: Pai): Boolean;
Begin
  If InitDFAPass2(AsmL, BlockStart, BlockEnd) Then
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
 Revision 1.34  1998-12-29 18:48:19  jonas
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
   * changed cbw, cwde and cwd to cbtw, cwtl and cwtd in att_op2str array
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
