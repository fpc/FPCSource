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

Uses AAsm, CObjects
  {$ifdef i386}
    ,i386
  {$endif}
  ;

{*********************** Procedures and Functions ************************}

Procedure InsertLLItem(AsmL: PAasmOutput; prev, foll, new_one: PLinkedList_Item);

Function Reg32(Reg: TRegister): TRegister;
Function RefsEqual(Const R1, R2: TReference): Boolean;
Function IsGP32Reg(Reg: TRegister): Boolean;
Function RegInRef(Reg: TRegister; Const Ref: TReference): Boolean;
Function RegInInstruction(Reg: TRegister; p1: Pai): Boolean;

Function GetNextInstruction(Current: Pai; Var Next: Pai): Boolean;
Function GetLastInstruction(Current: Pai; Var Last: Pai): Boolean;

Function RegsSameContent(p1, p2: Pai; Reg: TRegister): Boolean;
Function InstructionsEqual(p1, p2: Pai): Boolean;

Procedure DFAPass1(AsmL: PAasmOutput);
Function DFAPass2(AsmL: PAasmOutput): Pai;
Procedure ShutDownDFA;

Function FindLabel(L: PLabel; Var hp: Pai): Boolean;
{Procedure FindLoHiLabels(AsmL: PAasmOutput; Var LoLab, HiLab, LabDif: Longint);}

{******************************* Constants *******************************}

Const

{ait_* types which don't result in executable code or which don't influence
 the way the program runs/behaves}

  SkipInstr = [ait_comment
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

  TRegArray = Array[R_EAX..R_EDI] of TRegister;
  TRegSet = Set of TRegister;
  TRegInfo = Record
                RegsEncountered: TRegSet;
                SubstRegs: TRegArray;
              End;


{What an instruction can change}
  TChange = (C_None,
             C_REAX, C_RECX, C_REDX, C_REBX, C_RESP, C_REBP, C_RESI, C_REDI,
             C_WEAX, C_WECX, C_WEDX, C_WEBX, C_WESP, C_WEBP, C_WESI, C_WEDI,
             C_RWEAX, C_RWECX, C_RWEDX, C_RWEBX, C_RWESP, C_RWEBP, C_RWESI, C_RWEDI,
             C_CDirFlag {clear direction flag}, C_SDirFlag {set dir flag},
             C_Flags, C_FPU, C_Op1, C_Op2, C_Op3, C_MemEDI, C_All);

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
      {starts at 1, gets increased everytime the register is modified}
               State: Word;
      {how many instructions starting with StarMod does the block consist of}
               NrOfMods: Byte;
      {if one register gets a block assigned from an other register,
          this variable holds the name of that register (so it can be
          substituted when checking the block afterwards)}
{               ModReg: TRegister; }
      {the type of the content of the register: constant, ...}
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
    {status of the direction flag}
               DirFlag: TFlagContents;
    {can this instruction be removed?}
               CanBeRemoved: Boolean;
             End;

  PPaiProp = ^TPaiProp;
{$IfDef TP}
  TPaiPropBlock = Array[1..(65520 div (((SizeOf(TPaiProp)+1)div 2)*2))] Of TPaiProp;
{$else}
  TPaiPropBlock = Array[1..250000] Of TPaiProp;
{$EndIf TP}
  PPaiPropBlock = ^TPaiPropBlock;

  TInstrSinceLastMod = Array[R_EAX..R_EDI] Of Byte;

  TLabelTableItem = Record
                      PaiObj: Pai;
{$IfNDef TP}
                      InstrNr: Longint;
                      RefsFound: Word;
                      JmpsProcessed: Word
{$EndIf TP}
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
{Array which holds all (FPC) or as much as possible (TP) PPaiProps}
  PaiPropBlock: PPaiPropBlock;

  LoLab, HiLab, LabDif: Longint;

  LTable: PLabelTable;

{*********************** End of Interface section ************************}


Implementation

Uses globals, systems, strings, verbose, hcodegen,
   {$ifdef i386}
     cgi386;
   {$endif i386}

Const AsmInstr: Array[tasmop] Of TAsmInstrucProp = (
   {MOV} (Ch: (C_Op2, C_None, C_None)),
 {MOVZX} (Ch: (C_Op2, C_None, C_None)),
 {MOVSX} (Ch: (C_Op2, C_None, C_None)),
 {LABEL} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
   {ADD} (Ch: (C_Op2, C_Flags, C_None)),
  {CALL} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {IDIV} (Ch: (C_WEAX, C_WEDX, C_Flags)),
  {IMUL} (Ch: (C_WEAX, C_WEDX, C_Flags)), {handled separately, because several forms exist}
   {JMP} (Ch: (C_None, C_None, C_None)),
   {LEA} (Ch: (C_Op2, C_None, C_None)),
   {MUL} (Ch: (C_RWEAX, C_WEDX, C_Flags)),
   {NEG} (Ch: (C_Op1, C_None, C_None)),
   {NOT} (Ch: (C_Op1, C_Flags, C_None)),
   {POP} (Ch: (C_Op1, C_RWESP, C_None)),
 {POPAD} (Ch: (C_None, C_None, C_None)), {don't know value of any register}
  {PUSH} (Ch: (C_RWESP, C_None, C_None)),
{PUSHAD} (Ch: (C_RWESP, C_None, C_None)),
   {RET} (Ch: (C_None, C_None, C_None)), {don't know value of any register}
   {SUB} (Ch: (C_Op2, C_Flags, C_None)),
  {XCHG} (Ch: (C_Op1, C_Op2, C_None)), {(will be) handled seperately}
   {XOR} (Ch: (C_Op2, C_Flags, C_None)),
  {FILD} (Ch: (C_FPU, C_None, C_None)),
   {CMP} (Ch: (C_Flags, C_None, C_None)),
    {JZ} (Ch: (C_None, C_None, C_None)),
   {INC} (Ch: (C_Op1, C_Flags, C_None)),
   {DEC} (Ch: (C_Op1, C_Flags, C_None)),
  {SETE} (Ch: (C_Op1, C_None, C_None)),
 {SETNE} (Ch: (C_Op1, C_None, C_None)),
  {SETL} (Ch: (C_Op1, C_None, C_None)),
  {SETG} (Ch: (C_Op1, C_None, C_None)),
 {SETLE} (Ch: (C_Op1, C_None, C_None)),
 {SETGE} (Ch: (C_Op1, C_None, C_None)),
    {JE} (Ch: (C_None, C_None, C_None)),
   {JNE} (Ch: (C_None, C_None, C_None)),
    {JL} (Ch: (C_None, C_None, C_None)),
    {JG} (Ch: (C_None, C_None, C_None)),
   {JLE} (Ch: (C_None, C_None, C_None)),
   {JGE} (Ch: (C_None, C_None, C_None)),
    {OR} (Ch: (C_Op2, C_Flags, C_None)),
   {FLD} (Ch: (C_FPU, C_None, C_None)),
  {FADD} (Ch: (C_FPU, C_None, C_None)),
  {FMUL} (Ch: (C_FPU, C_None, C_None)),
  {FSUB} (Ch: (C_FPU, C_None, C_None)),
  {FDIV} (Ch: (C_FPU, C_None, C_None)),
  {FCHS} (Ch: (C_FPU, C_None, C_None)),
  {FLD1} (Ch: (C_FPU, C_None, C_None)),
 {FIDIV} (Ch: (C_FPU, C_None, C_None)),
  {CLTD} (Ch: (C_WEDX, C_None, C_None)),
   {JNZ} (Ch: (C_None, C_None, C_None)),
  {FSTP} (Ch: (C_Op1, C_None, C_None)),
   {AND} (Ch: (C_Op2, C_Flags, C_None)),
   {JNO} (Ch: (C_None, C_None, C_None)),
  {NOTH} (Ch: (C_None, C_None, C_None)), {***???***}
  {NONE} (Ch: (C_None, C_None, C_None)),
 {ENTER} (Ch: (C_RWESP, C_None, C_None)),
 {LEAVE} (Ch: (C_RWESP, C_None, C_None)),
   {CLD} (Ch: (C_CDirFlag, C_None, C_None)),
  {MOVS} (Ch: (C_RWESI, C_RWEDI, C_MemEDI)),
   {REP} (Ch: (C_RWECX, C_None, C_None)),
   {SHL} (Ch: (C_Op2, C_Flags, C_None)),
   {SHR} (Ch: (C_Op2, C_Flags, C_None)),
 {BOUND} (Ch: (C_None, C_None, C_None)),
   {JNS} (Ch: (C_None, C_None, C_None)),
    {JS} (Ch: (C_None, C_None, C_None)),
    {JO} (Ch: (C_None, C_None, C_None)),
   {SAR} (Ch: (C_Op2, C_Flags, C_None)),
  {TEST} (Ch: (C_Flags, C_None, C_None)),
  {FCOM} (Ch: (C_FPU, C_None, C_None)),
 {FCOMP} (Ch: (C_FPU, C_None, C_None)),
{FCOMPP} (Ch: (C_FPU, C_None, C_None)),
  {FXCH} (Ch: (C_FPU, C_None, C_None)),
 {FADDP} (Ch: (C_FPU, C_None, C_None)),
 {FMULP} (Ch: (C_FPU, C_None, C_None)),
 {FSUBP} (Ch: (C_FPU, C_None, C_None)),
 {FDIVP} (Ch: (C_FPU, C_None, C_None)),
 {FNSTS} (Ch: (C_Op1, C_None, C_None)),
  {SAHF} (Ch: (C_Flags, C_None, C_None)),
{FDIVRP} (Ch: (C_FPU, C_None, C_None)),
{FSUBRP} (Ch: (C_FPU, C_None, C_None)),
  {SETC} (Ch: (C_Op1, C_None, C_None)),
 {SETNC} (Ch: (C_Op1, C_None, C_None)),
    {JC} (Ch: (C_None, C_None, C_None)),
   {JNC} (Ch: (C_None, C_None, C_None)),
    {JA} (Ch: (C_None, C_None, C_None)),
   {JAE} (Ch: (C_None, C_None, C_None)),
    {JB} (Ch: (C_None, C_None, C_None)),
   {JBE} (Ch: (C_None, C_None, C_None)),
  {SETA} (Ch: (C_Op1, C_None, C_None)),
 {SETAE} (Ch: (C_Op1, C_None, C_None)),
  {SETB} (Ch: (C_Op1, C_None, C_None)),
 {SETBE} (Ch: (C_Op1, C_None, C_None)),
   {AAA} (Ch: (C_RWEAX, C_Flags, C_None)),
   {AAD} (Ch: (C_RWEAX, C_Flags, C_None)),
   {AAM} (Ch: (C_RWEAX, C_Flags, C_None)),
   {AAS} (Ch: (C_RWEAX, C_Flags, C_None)),
   {CBW} (Ch: (C_RWEAX, C_None, C_None)),
   {CDQ} (Ch: (C_RWEAX, C_WEDX, C_None)),
   {CLC} (Ch: (C_Flags, C_None, C_None)),
   {CLI} (Ch: (C_Flags, C_None, C_None)),
  {CLTS} (Ch: (C_None, C_None, C_None)),
   {CMC} (Ch: (C_Flags, C_None, C_None)),
   {CWD} (Ch: (C_RWEAX, C_WEDX, C_None)),
  {CWDE} (Ch: (C_RWEAX, C_None, C_None)),
   {DAA} (Ch: (C_RWEAX, C_None, C_None)),
   {DAS} (Ch: (C_RWEAX, C_None, C_None)),
   {HLT} (Ch: (C_None, C_None, C_None)),
  {IRET} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {LAHF} (Ch: (C_WEAX, C_None, C_None)),
  {LODS} (Ch: (C_WEAX, C_RWESI, C_None)),
  {LOCK} (Ch: (C_None, C_None, C_None)),
   {NOP} (Ch: (C_None, C_None, C_None)),
 {PUSHA} (Ch: (C_RWESP, C_None, C_None)),
 {PUSHF} (Ch: (C_RWESP, C_None, C_None)),
{PUSHFD} (Ch: (C_RWESP, C_None, C_None)),
   {STC} (Ch: (C_Flags, C_None, C_None)),
   {STD} (Ch: (C_SDirFlag, C_None, C_None)),
   {STI} (Ch: (C_Flags, C_None, C_None)),
  {STOS} (Ch: (C_MemEDI, C_RWEDI, C_None)),
  {WAIT} (Ch: (C_None, C_None, C_None)),
  {XLAT} (Ch: (C_WEAX, C_None, C_None)),
 {XLATB} (Ch: (C_WEAX, C_None, C_None)),
 {MOVSB} (Ch: (C_Op2, C_None, C_None)),
{MOVSBL} (Ch: (C_Op2, C_None, C_None)),
{MOVSBW} (Ch: (C_Op2, C_None, C_None)),
{MOVSWL} (Ch: (C_Op2, C_None, C_None)),
 {MOVZB} (Ch: (C_Op2, C_None, C_None)),
{MOVZWL} (Ch: (C_Op2, C_None, C_None)),
  {POPA} (Ch: (C_None, C_None, C_None)), {don't know value of any register}
    {IN} (Ch: (C_Op2, C_None, C_None)),
   {OUT} (Ch: (C_None, C_None, C_None)),
   {LDS} (Ch: (C_Op2, C_None, C_None)),
   {LCS} (Ch: (C_Op2, C_None, C_None)),
   {LES} (Ch: (C_Op2, C_None, C_None)),
   {LFS} (Ch: (C_Op2, C_None, C_None)),
   {LGS} (Ch: (C_Op2, C_None, C_None)),
   {LSS} (Ch: (C_Op2, C_None, C_None)),
  {POPF} (Ch: (C_RWESP, C_Flags, C_None)),
   {SBB} (Ch: (C_Op2, C_Flags, C_None)),
   {ADC} (Ch: (C_Op2, C_Flags, C_None)),
   {DIV} (Ch: (C_RWEAX, C_WEDX, C_Flags)),
   {ROR} (Ch: (C_Op2, C_Flags, C_None)),
   {ROL} (Ch: (C_Op2, C_Flags, C_None)),
   {RCL} (Ch: (C_Op2, C_Flags, C_None)),
   {RCR} (Ch: (C_Op2, C_Flags, C_None)),
   {SAL} (Ch: (C_Op2, C_Flags, C_None)),
  {SHLD} (Ch: (C_Op3, C_Flags, C_None)),
  {SHRD} (Ch: (C_Op3, C_Flags, C_None)),
 {LCALL} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {LJMP} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {LRET} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {JNAE} (Ch: (C_None, C_None, C_None)),
   {JNB} (Ch: (C_None, C_None, C_None)),
   {JNA} (Ch: (C_None, C_None, C_None)),
  {JNBE} (Ch: (C_None, C_None, C_None)),
    {JP} (Ch: (C_None, C_None, C_None)),
   {JNP} (Ch: (C_None, C_None, C_None)),
   {JPE} (Ch: (C_None, C_None, C_None)),
   {JPO} (Ch: (C_None, C_None, C_None)),
  {JNGE} (Ch: (C_None, C_None, C_None)),
   {JNG} (Ch: (C_None, C_None, C_None)),
   {JNL} (Ch: (C_None, C_None, C_None)),
  {JNLE} (Ch: (C_None, C_None, C_None)),
  {JCXZ} (Ch: (C_None, C_None, C_None)),
 {JECXZ} (Ch: (C_None, C_None, C_None)),
  {LOOP} (Ch: (C_RWECX, C_None, C_None)),
  {CMPS} (Ch: (C_RWESI, C_RWEDI, C_Flags)),
   {INS} (Ch: (C_RWEDI, C_MemEDI, C_None)),
  {OUTS} (Ch: (C_RWESI, C_None, C_None)),
  {SCAS} (Ch: (C_RWEDI, C_Flags, C_None)),
   {BSF} (Ch: (C_Op2, C_Flags, C_None)),
   {BSR} (Ch: (C_Op2, C_Flags, C_None)),
    {BT} (Ch: (C_Flags, C_None, C_None)),
   {BTC} (Ch: (C_Op2, C_Flags, C_None)),
   {BTR} (Ch: (C_Op2, C_Flags, C_None)),
   {BTS} (Ch: (C_Op2, C_Flags, C_None)),
   {INT} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
  {INT3} (Ch: (C_None, C_None, C_None)),
  {INTO} (Ch: (C_All, C_None, C_None)), {don't know value of any register}
{BOUNDL} (Ch: (C_None, C_None, C_None)),
{BOUNDW} (Ch: (C_None, C_None, C_None)),
 {LOOPZ} (Ch: (C_RWECX, C_None, C_None)),
 {LOOPE} (Ch: (C_RWECX, C_None, C_None)),
{LOOPNZ} (Ch: (C_RWECX, C_None, C_None)),
{LOOPNE} (Ch: (C_RWECX, C_None, C_None)),
  {SETO} (Ch: (C_Op1, C_None, C_None)),
 {SETNO} (Ch: (C_Op1, C_None, C_None)),
{SETNAE} (Ch: (C_Op1, C_None, C_None)),
 {SETNB} (Ch: (C_Op1, C_None, C_None)),
  {SETZ} (Ch: (C_Op1, C_None, C_None)),
 {SETNZ} (Ch: (C_Op1, C_None, C_None)),
 {SETNA} (Ch: (C_Op1, C_None, C_None)),
{SETNBE} (Ch: (C_Op1, C_None, C_None)),
  {SETS} (Ch: (C_Op1, C_None, C_None)),
 {SETNS} (Ch: (C_Op1, C_None, C_None)),
  {SETP} (Ch: (C_Op1, C_None, C_None)),
 {SETPE} (Ch: (C_Op1, C_None, C_None)),
 {SETNP} (Ch: (C_Op1, C_None, C_None)),
 {SETPO} (Ch: (C_Op1, C_None, C_None)),
{SETNGE} (Ch: (C_Op1, C_None, C_None)),
 {SETNL} (Ch: (C_Op1, C_None, C_None)),
 {SETNG} (Ch: (C_Op1, C_None, C_None)),
{SETNLE} (Ch: (C_Op1, C_None, C_None)),
  {ARPL} (Ch: (C_Flags, C_None, C_None)),
   {LAR} (Ch: (C_Op2, C_None, C_None)),
  {LGDT} (Ch: (C_None, C_None, C_None)),
  {LIDT} (Ch: (C_None, C_None, C_None)),
  {LLDT} (Ch: (C_None, C_None, C_None)),
  {LMSW} (Ch: (C_None, C_None, C_None)),
   {LSL} (Ch: (C_Op2, C_Flags, C_None)),
   {LTR} (Ch: (C_None, C_None, C_None)),
  {SGDT} (Ch: (C_Op1, C_None, C_None)),
  {SIDT} (Ch: (C_Op1, C_None, C_None)),
  {SLDT} (Ch: (C_Op1, C_None, C_None)),
  {SMSW} (Ch: (C_Op1, C_None, C_None)),
  {STR}  (Ch: (C_Op1, C_None, C_None)),
  {VERR} (Ch: (C_Flags, C_None, C_None)),
  {VERW} (Ch: (C_Flags, C_None, C_None)),
  {FABS} (Ch: (C_FPU, C_None, C_None)),
  {FBLD} (Ch: (C_FPU, C_None, C_None)),
 {FBSTP} (Ch: (C_Op1, C_None, C_None)),
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
  {FIST} (Ch: (C_Op1, C_None, C_None)),
 {FISTP} (Ch: (C_Op1, C_None, C_None)),
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
 {FSAVE} (Ch: (C_Op1, C_None, C_None)),
{FNSAVE} (Ch: (C_FPU, C_None, C_None)),
{FSCALE} (Ch: (C_FPU, C_None, C_None)),
{FSETPM} (Ch: (C_FPU, C_None, C_None)),
  {FSIN} (Ch: (C_FPU, C_None, C_None)),
{FSINCOS}(Ch: (C_FPU, C_None, C_None)),
 {FSQRT} (Ch: (C_FPU, C_None, C_None)),
   {FST} (Ch: (C_Op1, C_None, C_None)),
 {FSTCW} (Ch: (C_Op1, C_None, C_None)),
{FNSTCW} (Ch: (C_Op1, C_None, C_None)),
{FSTENV} (Ch: (C_Op1, C_None, C_None)),
{FNSTENV}(Ch: (C_Op1, C_None, C_None)),
 {FSTSW} (Ch: (C_Op1, C_None, C_None)),
{FNSTSW} (Ch: (C_Op1, C_None, C_None)),
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
 {FISTQ} (Ch: (C_Op1, C_None, C_None)),
 {FISTS} (Ch: (C_Op1, C_None, C_None)),
 {FISTL} (Ch: (C_Op1, C_None, C_None)),
  {FSTL} (Ch: (C_Op1, C_None, C_None)),
  {FSTS} (Ch: (C_Op1, C_None, C_None)),
 {FSTPS} (Ch: (C_Op1, C_None, C_None)),
{FISTPL} (Ch: (C_Op1, C_None, C_None)),
 {FSTPL} (Ch: (C_Op1, C_None, C_None)),
{FISTPS} (Ch: (C_Op1, C_None, C_None)),
{FISTPQ} (Ch: (C_Op1, C_None, C_None)),
 {FSTPT} (Ch: (C_Op1, C_None, C_None)),
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
  {REPE} (Ch: (C_RWECX, C_None, C_None)),
 {REPNE} (Ch: (C_RWECX, C_None, C_None)),
 {FADDS} (Ch: (C_FPU, C_None, C_None)),
 {POPFD} (Ch: (C_RWESP, C_Flags, C_None)),
{below are the MMX instructions}
{A_EMMS} (Ch: (C_FPU, C_None, C_None)),
{A_MOVD} (Ch: (C_Op2, C_None, C_None)),
{A_MOVQ} (Ch: (C_Op2, C_None, C_None)),
{A_PACKSSDW} (Ch: (C_All, C_None, C_None)),
{A_PACKSSWB} (Ch: (C_All, C_None, C_None)),
{A_PACKUSWB} (Ch: (C_All, C_None, C_None)),
{A_PADDB} (Ch: (C_Op2, C_None, C_None)),
{A_PADDD} (Ch: (C_Op2, C_None, C_None)),
{A_PADDSB} (Ch: (C_Op2, C_None, C_None)),
{A_PADDSW} (Ch: (C_Op2, C_None, C_None)),
{A_PADDUSB} (Ch: (C_Op2, C_None, C_None)),
{A_PADDUSW} (Ch: (C_Op2, C_None, C_None)),
{A_PADDW} (Ch: (C_Op2, C_None, C_None)),
{A_PAND} (Ch: (C_Op2, C_None, C_None)),
{A_PANDN} (Ch: (C_Op2, C_None, C_None)),
{A_PCMPEQB} (Ch: (C_All, C_None, C_None)),
{A_PCMPEQD} (Ch: (C_All, C_None, C_None)),
{A_PCMPEQW} (Ch: (C_All, C_None, C_None)),
{A_PCMPGTB} (Ch: (C_All, C_None, C_None)),
{A_PCMPGTD} (Ch: (C_All, C_None, C_None)),
{A_PCMPGTW} (Ch: (C_All, C_None, C_None)),
{A_PMADDWD} (Ch: (C_Op2, C_None, C_None)),
{A_PMULHW} (Ch: (C_All, C_None, C_None)),
{A_PMULLW} (Ch: (C_All, C_None, C_None)),
{A_POR} (Ch: (C_Op2, C_None, C_None)),
{A_PSLLD} (Ch: (C_Op2, C_None, C_None)),
{A_PSLLQ} (Ch: (C_Op2, C_None, C_None)),
{A_PSLLW} (Ch: (C_Op2, C_None, C_None)),
{A_PSRAD} (Ch: (C_Op2, C_None, C_None)),
{A_PSRAW} (Ch: (C_Op2, C_None, C_None)),
{A_PSRLD} (Ch: (C_Op2, C_None, C_None)),
{A_PSRLQ} (Ch: (C_Op2, C_None, C_None)),
{A_PSRLW} (Ch: (C_Op2, C_None, C_None)),
{A_PSUBB} (Ch: (C_Op2, C_None, C_None)),
{A_PSUBD} (Ch: (C_Op2, C_None, C_None)),
{A_PSUBSB} (Ch: (C_Op2, C_None, C_None)),
{A_PSUBSW} (Ch: (C_Op2, C_None, C_None)),
{A_PSUBUSB} (Ch: (C_Op2, C_None, C_None)),
{A_PSUBUSW} (Ch: (C_Op2, C_None, C_None)),
{A_PSUBW} (Ch: (C_Op2, C_None, C_None)),
{A_PUNPCKHBW} (Ch: (C_All, C_None, C_None)),
{A_PUNPCKHDQ} (Ch: (C_All, C_None, C_None)),
{A_PUNPCKHWD} (Ch: (C_All, C_None, C_None)),
{A_PUNPCKLBW} (Ch: (C_All, C_None, C_None)),
{A_PUNPCKLDQ} (Ch: (C_All, C_None, C_None)),
{A_PUNPCKLWD} (Ch: (C_All, C_None, C_None)),
{A_PXOR} (Ch: (C_Op2, C_None, C_None)));

Var
 {How many instructions are between the current instruction and the last one
  that modified the register}
  NrOfInstrSinceLastMod: TInstrSinceLastMod;


{************************ Create the Label table ************************}

Procedure FindLoHiLabels(AsmL: PAasmOutput; Var LowLabel, HighLabel, LabelDif: Longint);
{Walks through the paasmlist to find the lowest and highest label number;
 Since 0.9.3: also removes unused labels}
Var LabelFound: Boolean;
    P, hp1: Pai;
Begin
  LabelFound := False;
  LowLabel := MaxLongint;
  HighLabel := 0;
  P := Pai(AsmL^.first);
  While Assigned(p) Do
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
  If LabelFound
    Then LabelDif := HighLabel+1-LowLabel
    Else LabelDif := 0;
End;

Procedure BuildLabelTable(AsmL: PAasmOutput; Var LabelTable: PLabelTable; LowLabel: Longint; Var LabelDif: Longint);
{Builds a table with the locations of the labels in the paasmoutput}
Var p: Pai;
Begin
  If (LabelDif <> 0) Then
    Begin
{$IfDef TP}
      If (MaxAvail >= LabelDif*SizeOf(Pai))
        Then
          Begin
{$EndIf TP}
            GetMem(LabelTable, LabelDif*SizeOf(TLabelTableItem));
            FillChar(LabelTable^, LabelDif*SizeOf(TLabelTableItem), 0);
            p := pai(AsmL^.first);
            While Assigned(p) Do
              Begin
                If (Pai(p)^.typ = ait_label) And
                   (Pai_Label(p)^.l^.is_used) Then
                  LabelTable^[Pai_Label(p)^.l^.nb-LowLabel].PaiObj := p;
                GetNextInstruction(p, p);
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

Function RegsEquivalent(Reg1, Reg2: TRegister; Var RegInfo: TRegInfo): Boolean;
Begin
  With RegInfo Do
    If Not(Reg1 in RegsEncountered) Then
      Begin
        RegsEncountered := RegsEncountered + [Reg1];
        SubstRegs[Reg1] := Reg2;
        RegsEquivalent := True
      End
    Else RegsEquivalent := Reg1 = SubstRegs[Reg2];
End;

Function RefsEquivalent(Const R1, R2: TReference; var RegInfo: TRegInfo): Boolean;
Begin
  If R1.IsIntValue
     Then RefsEquivalent := R2.IsIntValue and (R1.Offset = R2.Offset)
     Else If (R1.Offset = R2.Offset) And
             RegsEquivalent(R1.Base, R2.Base, RegInfo) And
             RegsEquivalent(R1.Index, R2.Index, RegInfo) And
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

Function SubstRegInRef(Reg: TRegister; Const Ref: TReference; var RegInfo: TRegInfo): Boolean;
Begin
  Reg := Reg32(Reg);
  With RegInfo Do
    SubstRegInRef := RegsEquivalent(Reg, SubstRegs[Ref.Base], RegInfo) And
                     RegsEquivalent(Reg, SubstRegs[Ref.Base], RegInfo);
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

{********************* GetNext and GetLastInstruction *********************}

Function GetNextInstruction(Current: Pai; Var Next: Pai): Boolean;
{skips ait_regalloc, ait_regdealloc and ait_stab* objects and puts the
 next pai object in Next. Returns false if there isn't any}
Begin
  Current := Pai(Current^.Next);
  While Assigned(Current) And
        ((Current^.typ In SkipInstr) or
         ((Current^.typ = ait_label) And
          Not(Pai_Label(Current)^.l^.is_used))) Do
    Current := Pai(Current^.Next);
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
  Current := Pai(Current^.previous);
  While Assigned(Current) And
        ((Pai(Current)^.typ In SkipInstr) or
         ((Pai(Current)^.typ = ait_label) And
          Not(Pai_Label(Current)^.l^.is_used))) Do
    Current := Pai(Current^.previous);
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

{******************* The Data Flow Analyzer functions ********************}

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

Procedure IncState(Var S: Word);
{Increases the state by 1, wraps around at $ffff to 0 (so we won't get
 overflow errors}
Begin
  If (s <> $ffff)
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
         (Pai386(p)^._operator in [A_MOV, A_MOVZX, A_MOVSX])
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
Var TmpState: Longint;
    Counter: TRegister;
Begin
  Reg := Reg32(Reg);
  NrOfInstrSinceLastMod[Reg] := 0;
  If (Reg >= R_EAX) And (Reg <= R_EDI)
    Then
      Begin
        With p1^.Regs[Reg] Do
          Begin
            IncState(State);
            TmpState := State;
            FillChar(p1^.Regs[Reg], SizeOf(TContent), 0);
            State := TmpState;
          End;
        For Counter := R_EAX to R_EDI Do
          With p1^.Regs[Counter] Do
            If (Typ = Con_Ref) And
               RegInSequence(Reg, p1^.Regs[Counter])
              Then
                Begin
                  IncState(State);
                  TmpState := State;
                  FillChar(p1^.Regs[Counter], SizeOf(TContent), 0);
                  State := TmpState;
                End;
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

Function RegsSameContent(p1, p2: Pai; Reg: TRegister): Boolean;
{checks whether Reg has the same content in the PPaiProp of p1 and p2}
Begin
  Reg := Reg32(Reg);
  RegsSameContent :=
    PPaiProp(p1^.fileinfo.line)^.Regs[Reg].State =
    PPaiProp(p2^.fileinfo.line)^.Regs[Reg].State;
End;

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

Procedure Destroy(PaiObj: Pai; opt: Longint; Op: Pointer);
Begin
  Case opt Of
    top_reg: DestroyReg(PPaiProp(PaiObj^.fileinfo.line), TRegister(Op));
    top_ref: DestroyRefs(PaiObj, TReference(Op^), R_NO);
    top_symbol:;
  End;
End;

Procedure DFAPass1(AsmL: PAasmOutput);
{gathers the RegAlloc data... still need to think about where to store it}
Begin
  FindLoHiLabels(AsmL, LoLab, HiLab, LabDif);
  BuildLabelTable(AsmL, LTable, LoLab, LabDif);
End;

Function DoDFAPass2(
{$Ifdef StateDebug}
AsmL: PAasmOutput;
{$endif statedebug}
First: Pai): Pai;
{Analyzes the Data Flow of an assembler list. Starts creating the reg
 contents for the instructions starting with p. Returns the last pai which has
 been processed}
Var
    CurProp: PPaiProp;
{$ifdef AnalyzeLoops}
    TmpState,
{$endif AnalyzeLoops}
    Cnt, InstrCnt : Longint;
    InstrProp: TAsmInstrucProp;
    p, hp : Pai;
    TmpRef: TReference;
    TmpReg: TRegister;
Begin
  p := First;
  If (First^.typ in SkipInstr) Then
    GetNextInstruction(First, p);
  First := p;
  InstrCnt := 1;
  FillChar(NrOfInstrSinceLastMod, SizeOf(NrOfInstrSinceLastMod), 0);
  While Assigned(p) Do
    Begin
      DoDFAPass2 := p;
{$IfDef TP}
      New(CurProp);
{$Else TP}
      CurProp := @PaiPropBlock^[InstrCnt];
{$EndIf TP}
      If (p <> First)
        Then
{$ifdef JumpAnal}
          Begin
            If (p^.Typ <> ait_label) Then
{$endif JumpAnal}
              Begin
                GetLastInstruction(p, hp);
                CurProp^.Regs := PPaiProp(hp^.fileinfo.line)^.Regs;
                CurProp^.DirFlag := PPaiProp(hp^.fileinfo.line)^.DirFlag;
              End
{$ifdef JumpAnal}
          End
{$endif JumpAnal}
        Else
          Begin
            FillChar(CurProp^, SizeOf(CurProp^), 0);
{            For TmpReg := R_EAX to R_EDI Do
              CurProp^.Regs[TmpReg].State := 1;}
          End;
      CurProp^.CanBeRemoved := False;
{$ifdef TP}
      CurProp^.linesave := p^.fileinfo.line;
      PPaiProp(p^.fileinfo.line) := CurProp;
{$Endif TP}
{      If Not(p^.typ in SkipInstr) Then}
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
                           Not((hp^.typ = ait_labeled_instruction) And
                               (Pai_Labeled(hp)^._operator = A_JMP))
                          Then
  {previous instruction not a JMP -> the contents of the registers after the
   previous intruction has been executed have to be taken into account as well}
                            For TmpReg := R_EAX to R_EDI Do
                              Begin
                                If (CurProp^.Regs[TmpReg].State <>
                                    PPaiProp(hp^.FileInfo.Line)^.Regs[TmpReg].State)
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
                          If (PaiPropBlock^[InstrNr].Regs[TmpReg].State <>
                             CurProp^.Regs[TmpReg].State) Then
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
                            If (PaiPropBlock^[InstrNr].Regs[TmpReg].State <>
                                CurProp^.Regs[TmpReg].State)
                              Then
                                Begin
                                  TmpState := PaiPropBlock^[InstrNr].Regs[TmpReg].State;
                                  Cnt := InstrNr;
                                  While (TmpState = PaiPropBlock^[Cnt].Regs[TmpReg].State) Do
                                    Begin
                                      DestroyReg(@PaiPropBlock^[Cnt], TmpReg);
                                      Inc(Cnt);
                                    End;
                                  While (Cnt <= InstrCnt) Do
                                    Begin
                                      Inc(PaiPropBlock^[Cnt].Regs[TmpReg].State);
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
                              TmpState := PaiPropBlock^[InstrNr].Regs[TmpReg].State;
                              Cnt := InstrNr;
                              While (TmpState = PaiPropBlock^[Cnt].Regs[TmpReg].State) Do
                                Begin
                                  PaiPropBlock^[Cnt].Regs[TmpReg] := CurProp^.Regs[TmpReg];
                                  Inc(Cnt);
                                End;
                              TmpState := PaiPropBlock^[InstrNr].Regs[TmpReg].State;
                              While (TmpState = PaiPropBlock^[Cnt].Regs[TmpReg].State) Do
                                Begin
                                  DestroyReg(@PaiPropBlock^[Cnt], TmpReg);
                                  Inc(Cnt);
                                End;
                              While (Cnt <= InstrCnt) Do
                                Begin
                                  Inc(PaiPropBlock^[Cnt].Regs[TmpReg].State);
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
        ait_regalloc, ait_regdealloc:;
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
{                            CurProp^.Regs[TRegister(Pai386(p)^.op2)] :=
                              CurProp^.Regs[TRegister(Pai386(p)^.op1)];
                            If (CurProp^.Regs[TRegister(Pai386(p)^.op2)].ModReg = R_NO) Then
                              CurProp^.Regs[TRegister(Pai386(p)^.op2)].ModReg :=
                                Tregister(Pai386(p)^.op1);}
                          End;
                        Top_Ref: DestroyRefs(p, TReference(Pai386(p)^.op2^), TRegister(Pai386(p)^.op1));
                      End;
                    Top_Ref:
                      Begin {destination is always a register in this case}
                        TmpReg := Reg32(TRegister(Pai386(p)^.op2));
{$ifdef StateDebug}
                  hp := new(pai_asm_comment,init(strpnew(att_reg2str[TmpReg]+': '+tostr(CurProp^.Regs[TmpReg].State))));
                  InsertLLItem(AsmL, p, p^.next, hp);
{$endif SteDebug}
                        If RegInRef(TmpReg, TReference(Pai386(p)^.op1^)) And
                           (CurProp^.Regs[TmpReg].Typ = Con_Ref)
                          Then
                            Begin
                              With CurProp^.Regs[TmpReg] Do
                                Begin
                                  IncState(State);
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
                              With CurProp^.Regs[TmpReg] Do
                                Begin
                                  Typ := Con_Ref;
                                  StartMod := p;
                                  NrOfMods := 1;
                                End;
                            End;
                      End;
                    Top_Const:
                      Begin
                        Case Pai386(p)^.op2t Of
                          Top_Reg:
                            Begin
                              TmpReg := Reg32(TRegister(Pai386(p)^.op2));
                              With CurProp^.Regs[TmpReg] Do
                                Begin
                                {it doesn't matter that the state is changed,
                                 it isn't looked at when removing constant reloads}
                                  DestroyReg(CurProp, TmpReg);
                                  typ := Con_Const;
                                  StartMod := Pai386(p)^.op1;
                                End
                            End;
                          Top_Ref: DestroyRefs(P, TReference(Pai386(p)^.op2^), R_NO);
                        End;
                      End;
                End;
              End;
              A_IMUL:
                Begin
                  If (Pai386(p)^.Op3t = top_none)
                   Then
                     If (Pai386(p)^.Op2t = top_none)
                       Then
                         Begin
                           DestroyReg(CurProp, R_EAX);
                           DestroyReg(CurProp, R_EDX)
                         End
                       Else
                         Begin
                           If (Pai386(p)^.Op2t = top_reg) Then
                             DestroyReg(CurProp, TRegister(Pai386(p)^.Op2));
                         End
                   Else If (Pai386(p)^.Op3t = top_reg) Then
                          DestroyReg(CurProp, TRegister(longint(twowords(Pai386(p)^.Op2).word2)));
                End;
              A_XOR:
                Begin
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
                        C_WEAX..C_RWEDI: DestroyReg(CurProp, TCh2Reg(InstrProp.Ch[Cnt]));
                        C_CDirFlag: CurProp^.DirFlag := F_NotSet;
                        C_SDirFlag: CurProp^.DirFlag := F_Set;
                        C_Op1: Destroy(p, Pai386(p)^.op1t, Pai386(p)^.op1);
                        C_Op2: Destroy(p, Pai386(p)^.op2t, Pai386(p)^.op2);
                        C_Op3: Destroy(p, Pai386(p)^.op2t, Pointer(Longint(TwoWords(Pai386(p)^.op2).word2)));
                        C_MemEDI:
                          Begin
                            FillChar(TmpRef, SizeOf(TmpRef), 0);
                            TmpRef.Base := R_EDI;
                            DestroyRefs(p, TmpRef, R_NO)
                          End;
                        C_Flags, C_FPU:
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

Function InitDFAPass2(AsmL: PAasmOutput): Boolean;
{reserves memory for the PPaiProps in one big memory block when not using
 TP, returns False if not enough memory is available for the optimizer in all
 cases}
Var p: Pai;
    Count: Longint;
{    TmpStr: String; }
Begin
  P := Pai(AsmL^.First);
  If (p^.typ in SkipInstr) Then
    GetNextInstruction(p, p);
  NrOfPaiObjs := 0;
  While Assigned(P) Do
    Begin
{$IfNDef TP}
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
{$EndIf TP}
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
      p := Pai(AsmL^.First);
      If (p^.typ in SkipInstr) Then
        GetNextInstruction(p, p);
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

Function DFAPass2(AsmL: PAasmOutPut): Pai;
Begin
  If InitDFAPass2(AsmL)
    Then DFAPass2 := DoDFAPass2(
{$ifdef statedebug}
 asml,
{$endif statedbug}
    Pai(AsmL^.First))
    Else DFAPass2 := Nil;
End;

Procedure ShutDownDFA;
Begin
  If LabDif <> 0 Then
    FreeMem(LTable, LabDif*SizeOf(TLabelTableItem));
End;

End.

{
 $Log$
 Revision 1.14  1998-09-20 17:12:36  jonas
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
