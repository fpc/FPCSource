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
Function PowerOf2(L: Longint): Longint;

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
{$ifdef regalloc}
  ,ait_regalloc, ait_regdealloc
{$endif regalloc}
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
             C_EAX, C_ECX, C_EDX, C_EBX, C_ESP, C_EBP, C_ESI, C_EDI,
             C_CDirFlag {clear direction flag}, C_SDirFlag {set dir flag},
             C_Flags, C_FPU, C_Op1, C_Op2, C_Op3, C_MemEDI);

{the possible states of a flag}
  TFlagContents = (F_Unknown, F_NotSet, F_Set);

{the properties of a cpu instruction}
  TAsmInstrucProp = Record
               {how many things it changes}
                         NCh: Byte;
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
      {the tpye of the content of the register: constant, ...}
               Typ: Byte;
             End;

{Contents of the integer registers}
  TRegContent = Array[R_NO..R_EDI] Of TContent;

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
  NrOfPaiObjs,
{for TP only: the amount of PPaiProps that can be stored in the PaiPropBlock}
  NrOfPaiFast: Longint;
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
   {MOV} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
 {MOVZX} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
 {MOVSX} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
 {LABEL} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
   {ADD} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
  {CALL} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
  {IDIV} (NCh: 3; Ch: (C_EAX, C_EDX, C_Flags)),
  {IMUL} (NCh: 3; Ch: (C_EAX, C_EDX, C_Flags)), {handled separately, because several forms exist}
   {JMP} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
   {LEA} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
   {MUL} (NCh: 3; Ch: (C_EAX, C_EDX, C_Flags)),
   {NEG} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
   {NOT} (NCh: 2; Ch: (C_Op1, C_Flags, C_None)),
   {POP} (NCh: 2; Ch: (C_Op1, C_ESP, C_None)),
 {POPAD} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
  {PUSH} (NCh: 1; Ch: (C_ESP, C_None, C_None)),
{PUSHAD} (NCh: 1; Ch: (C_ESP, C_None, C_None)),
   {RET} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
   {SUB} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
  {XCHG} (NCh: 2; Ch: (C_Op1, C_Op2, C_None)), {(will be) handled seperately}
   {XOR} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
  {FILD} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
   {CMP} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
    {JZ} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {INC} (NCh: 2; Ch: (C_Op1, C_Flags, C_None)),
   {DEC} (NCh: 2; Ch: (C_Op1, C_Flags, C_None)),
  {SETE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETNE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {SETL} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {SETG} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETLE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETGE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
    {JE} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JNE} (NCh: 0; Ch: (C_None, C_None, C_None)),
    {JL} (NCh: 0; Ch: (C_None, C_None, C_None)),
    {JG} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JLE} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JGE} (NCh: 0; Ch: (C_None, C_None, C_None)),
    {OR} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {FLD} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FADD} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FMUL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FSUB} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FDIV} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FCHS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FLD1} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FIDIV} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {CLTD} (NCh: 1; Ch: (C_EDX, C_None, C_None)),
   {JNZ} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {FSTP} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
   {AND} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {JNO} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {NOTH} (NCh: 0; Ch: (C_None, C_None, C_None)), {***???***}
  {NONE} (NCh: 0; Ch: (C_None, C_None, C_None)),
 {ENTER} (NCh: 1; Ch: (C_ESP, C_None, C_None)),
 {LEAVE} (NCh: 1; Ch: (C_ESP, C_None, C_None)),
   {CLD} (NCh: 1; Ch: (C_CDirFlag, C_None, C_None)),
  {MOVS} (NCh: 3; Ch: (C_ESI, C_EDI, C_MemEDI)),
   {REP} (NCh: 1; Ch: (C_ECX, C_None, C_None)),
   {SHL} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {SHR} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
 {BOUND} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JNS} (NCh: 0; Ch: (C_None, C_None, C_None)),
    {JS} (NCh: 0; Ch: (C_None, C_None, C_None)),
    {JO} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {SAR} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
  {TEST} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
  {FCOM} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FCOMP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FCOMPP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FXCH} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FADDP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FMULP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FSUBP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FDIVP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FNSTS} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {SAHF} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
{FDIVRP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FSUBRP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {SETC} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETNC} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
    {JC} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JNC} (NCh: 0; Ch: (C_None, C_None, C_None)),
    {JA} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JAE} (NCh: 0; Ch: (C_None, C_None, C_None)),
    {JB} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JBE} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {SETA} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETAE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {SETB} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETBE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
   {AAA} (NCh: 2; Ch: (C_EAX, C_Flags, C_None)),
   {AAD} (NCh: 2; Ch: (C_EAX, C_Flags, C_None)),
   {AAM} (NCh: 2; Ch: (C_EAX, C_Flags, C_None)),
   {AAS} (NCh: 2; Ch: (C_EAX, C_Flags, C_None)),
   {CBW} (NCh: 1; Ch: (C_EAX, C_None, C_None)),
   {CDQ} (NCh: 2; Ch: (C_EAX, C_EDX, C_None)),
   {CLC} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
   {CLI} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
  {CLTS} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {CMC} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
   {CWD} (NCh: 2; Ch: (C_EAX, C_EDX, C_None)),
  {CWDE} (NCh: 1; Ch: (C_EAX, C_None, C_None)),
   {DAA} (NCh: 1; Ch: (C_EAX, C_None, C_None)),
   {DAS} (NCh: 1; Ch: (C_EAX, C_None, C_None)),
   {HLT} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {IRET} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
  {LAHF} (NCh: 1; Ch: (C_EAX, C_None, C_None)),
  {LODS} (NCh: 2; Ch: (C_EAX, C_ESI, C_None)),
  {LOCK} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {NOP} (NCh: 0; Ch: (C_None, C_None, C_None)),
 {PUSHA} (NCh: 1; Ch: (C_ESP, C_None, C_None)),
 {PUSHF} (NCh: 1; Ch: (C_ESP, C_None, C_None)),
{PUSHFD} (NCh: 1; Ch: (C_ESP, C_None, C_None)),
   {STC} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
   {STD} (NCh: 1; Ch: (C_SDirFlag, C_None, C_None)),
   {STI} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
  {STOS} (NCh: 2; Ch: (C_MemEDI, C_EDI, C_None)),
  {WAIT} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {XLAT} (NCh: 1; Ch: (C_EAX, C_None, C_None)),
 {XLATB} (NCh: 1; Ch: (C_EAX, C_None, C_None)),
 {MOVSB} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{MOVSBL} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{MOVSBW} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{MOVSWL} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
 {MOVZB} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{MOVZWL} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
  {POPA} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
    {IN} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
   {OUT} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {LDS} (NCh: 2; Ch: (C_Op2, C_None, C_None)),
   {LCS} (NCh: 2; Ch: (C_Op2, C_None, C_None)),
   {LES} (NCh: 2; Ch: (C_Op2, C_None, C_None)),
   {LFS} (NCh: 2; Ch: (C_Op2, C_None, C_None)),
   {LGS} (NCh: 2; Ch: (C_Op2, C_None, C_None)),
   {LSS} (NCh: 2; Ch: (C_Op2, C_None, C_None)),
  {POPF} (NCh: 2; Ch: (C_Flags, C_ESP, C_None)),
   {SBB} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {ADC} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {DIV} (NCh: 3; Ch: (C_EAX, C_EDX, C_Flags)),
   {ROR} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {ROL} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {RCL} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {RCR} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {SAL} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
  {SHLD} (NCh: 2; Ch: (C_Op3, C_Flags, C_None)),
  {SHRD} (NCh: 2; Ch: (C_Op3, C_Flags, C_None)),
 {LCALL} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
  {LJMP} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
  {LRET} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
  {JNAE} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JNB} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JNA} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {JNBE} (NCh: 0; Ch: (C_None, C_None, C_None)),
    {JP} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JNP} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JPE} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JPO} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {JNGE} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JNG} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {JNL} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {JNLE} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {JCXZ} (NCh: 0; Ch: (C_None, C_None, C_None)),
 {JECXZ} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {LOOP} (NCh: 1; Ch: (C_ECX, C_None, C_None)),
  {CMPS} (NCh: 3; Ch: (C_ESI, C_EDI, C_Flags)),
   {INS} (NCh: 1; Ch: (C_EDI, C_None, C_None)),
  {OUTS} (NCh: 1; Ch: (C_ESI, C_None, C_None)),
  {SCAS} (NCh: 2; Ch: (C_EDI, C_Flags, C_None)),
   {BSF} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {BSR} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
    {BT} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
   {BTC} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {BTR} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {BTS} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {INT} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
  {INT3} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {INTO} (NCh: 255; Ch: (C_None, C_None, C_None)), {don't know value of any register}
{BOUNDL} (NCh: 0; Ch: (C_None, C_None, C_None)),
{BOUNDW} (NCh: 0; Ch: (C_None, C_None, C_None)),
 {LOOPZ} (NCh: 1; Ch: (C_ECX, C_None, C_None)),
 {LOOPE} (NCh: 1; Ch: (C_ECX, C_None, C_None)),
{LOOPNZ} (NCh: 1; Ch: (C_ECX, C_None, C_None)),
{LOOPNE} (NCh: 1; Ch: (C_ECX, C_None, C_None)),
  {SETO} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETNO} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{SETNAE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETNB} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {SETZ} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETNZ} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETNA} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{SETNBE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {SETS} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETNS} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {SETP} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETPE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETNP} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETPO} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{SETNGE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETNL} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {SETNG} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{SETNLE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {ARPL} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
   {LAR} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
  {LGDT} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {LIDT} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {LLDT} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {LMSW} (NCh: 0; Ch: (C_None, C_None, C_None)),
   {LSL} (NCh: 2; Ch: (C_Op2, C_Flags, C_None)),
   {LTR} (NCh: 0; Ch: (C_None, C_None, C_None)),
  {SGDT} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {SIDT} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {SLDT} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {SMSW} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {STR}  (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {VERR} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
  {VERW} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
  {FABS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FBLD} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FBSTP} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {FCLEX} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FNCLEX} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FCOS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FDECSTP}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FDISI} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FNDISI} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FDIVR} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FENI} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FNENI} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FFREE} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FIADD} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FICOM} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FICOMP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FIDIVR} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FIMUL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FINCSTP}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FINIT} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FNINIT} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FIST} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {FISTP} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {FISUB} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FSUBR} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FLDCW} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FLDENV} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FLDLG2} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FLDLN2} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FLDL2E} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FLDL2T} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FLDPI} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FLDS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FLDZ} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FNOP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FPATAN} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FPREM} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FPREM1} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FPTAN} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FRNDINT}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FRSTOR} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FSAVE} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{FNSAVE} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FSCALE} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FSETPM} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FSIN} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FSINCOS}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FSQRT} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
   {FST} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {FSTCW} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{FNSTCW} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{FSTENV} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{FNSTENV}(NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {FSTSW} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{FNSTSW} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {FTST} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FUCOM} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FUCOMP} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FUCOMPP}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FWAIT} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FXAM} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FXTRACT}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FYL2X} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FYL2XP1}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {F2XM1} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FILDQ} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FILDS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FILDL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FLDL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {FLDT} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FISTQ} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {FISTS} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {FISTL} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {FSTL} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
  {FSTS} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {FSTPS} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{FISTPL} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {FSTPL} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{FISTPS} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{FISTPQ} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
 {FSTPT} (NCh: 1; Ch: (C_Op1, C_None, C_None)),
{FCOMPS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FICOMPL}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FCOMPL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FICOMPS}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FCOMS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FICOML} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FCOML} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FICOMS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FIADDL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FADDL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FIADDS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FISUBL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FSUBL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FISUBS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FSUBS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FSUBR} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FSUBRS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FISUBRL}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FSUBRL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FISUBRS}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FMULS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FIMUL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FMULL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FIMULS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FIDIVS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FIDIVL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {FDIVL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FIDIVS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FDIVRS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FIDIVRL}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FDIVRL} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{FIDIVRS}(NCh: 1; Ch: (C_FPU, C_None, C_None)),
  {REPE} (NCh: 0; Ch: (C_ECX, C_None, C_None)),
 {REPNE} (NCh: 0; Ch: (C_ECX, C_None, C_None)),
 {FADDS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
 {POPFD} (NCh: 2; Ch: (C_ESP, C_Flags, C_None)),
{below are the MMX instructions}
{A_EMMS} (NCh: 1; Ch: (C_FPU, C_None, C_None)),
{A_MOVD} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_MOVQ} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PACKSSDW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PACKSSWB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PACKUSWB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PADDB} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PADDD} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PADDSB} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PADDSW} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PADDUSB} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PADDUSW} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PADDW} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PAND} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PANDN} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PCMPEQB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPEQD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPEQW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPGTB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPGTD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPGTW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PMADDWD} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PMULHW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PMULLW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_POR} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSLLD} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSLLQ} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSLLW} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSRAD} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSRAW} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSRLD} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSRLQ} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSRLW} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSUBB} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSUBD} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSUBSB} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSUBSW} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSUBUSB} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSUBUSW} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PSUBW} (NCh: 1; Ch: (C_Op2, C_None, C_None)),
{A_PUNPCKHBW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKHDQ} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKHWD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKLBW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKLDQ} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKLWD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PXOR} (NCh: 1; Ch: (C_Op2, C_None, C_None)));

Var
 {How many instructions are betwen the current instruction and the last one
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
          Else
            Begin
              hp1 := pai(p^.next);
              AsmL^.Remove(p);
              Dispose(p, Done);
              p := hp1;
              continue;
            End;
      p := pai(p^.next);
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
                If (Pai(p)^.typ = ait_label) Then
                  LabelTable^[Pai_Label(p)^.l^.nb-LowLabel].PaiObj := p;
                p := pai(p^.next);
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
       (pai(TempP)^.typ In SkipInstr + [ait_label]) Do
    If (pai_label(TempP)^.l <> L)
      Then TempP := Pai(TempP^.next)
      Else
        Begin
          hp := TempP;
          FindLabel := True;
          exit
        End;
  FindLabel := False
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

Function PowerOf2(L: Longint): Longint;
Var Counter, TempVal: Longint;
Begin
  TempVal := 1;
  For Counter := 1 to L Do
    TempVal := TempVal * 2;
  PowerOf2 := TempVal;
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

{********************* GetNext and GetLastInstruction *********************}

Function GetNextInstruction(Current: Pai; Var Next: Pai): Boolean;
{skips ait_regalloc, ait_regdealloc and ait_stab* objects and puts the
 next pai object in Next. Returns false if there isn't any}
Begin
  GetNextInstruction := False;
  Current := Pai(Current^.Next);
  While Assigned(Current) And
        (Pai(Current)^.typ In SkipInstr) Do
    Current := Pai(Current^.Next);
  If Assigned(Current)
    Then
      Begin
        Next := Current;
        GetNextInstruction := True;
      End;
End;

Function GetLastInstruction(Current: Pai; Var Last: Pai): Boolean;
{skips the ait-types in SkipInstr puts the previous pai object in
 Last. Returns false if there isn't any}
Begin
  GetLastInstruction := False;
  Current := Pai(Current^.previous);
  While Assigned(Current) And
        (Pai(Current)^.typ In SkipInstr) Do
    Current := Pai(Current^.previous);
  If Assigned(Current)
    Then
      Begin
        Last := Current;
        GetLastInstruction := True;
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
  If (CH <= C_EDI)
    Then TCh2Reg := TRegister(Byte(Ch))
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

Procedure DestroyReg(p1: PPaiProp; Reg: TRegister);
{Destroys the contents of the register Reg in the PPaiProp of P}
Var TmpState: Longint;
Begin
  Reg := Reg32(Reg);
  NrOfInstrSinceLastMod[Reg] := 0;
  If (Reg >= R_EAX) And (Reg <= R_EDI)
    Then
      With p1^.Regs[Reg] Do
        Begin
          IncState(State);
          TmpState := State;
          FillChar(p1^.Regs[Reg], SizeOf(TContent), 0);
          State := TmpState;
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
{$ifdef regalloc}
    ((((Pai(p1)^.typ = ait_regalloc) And
       (Pai(p2)^.typ = ait_regalloc)) Or
      ((Pai(p1)^.typ = ait_regdealloc) And
       (Pai(p2)^.typ = ait_regdealloc))) And
     (PaiRegAlloc(p1)^.reg = PaiRegAlloc(p2)^.reg)) Or
{$endif regalloc}
    ((Pai(p1)^.typ = ait_instruction) And
     (Pai(p1)^.typ = ait_instruction) And
     (Pai386(p1)^._operator = Pai386(p2)^._operator) And
     (Pai386(p1)^.op1t = Pai386(p2)^.op1t) And
     (Pai386(p1)^.op2t = Pai386(p2)^.op2t) And
     OpsEqual(Pai386(p1)^.op1t, Pai386(p1)^.op1, Pai386(p2)^.op1) And
     OpsEqual(Pai386(p1)^.op2t, Pai386(p1)^.op2, Pai386(p2)^.op2))
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
      - destroy the contents of registers <> WhichReg whose StartMod is of
        the form "mov?? (Ref), %reg". WhichReg is destroyed if it's StartMod
        is of that form and NrOfMods > 1 (so if it is a pointer based on Ref)
    * with uncertzain optimizations off:
       - also destroy registers that contain any pointer}
      For Counter := R_EAX to R_EDI Do
        With PPaiProp(p^.fileinfo.line)^.Regs[Counter] Do
          Begin
            If (typ = Con_Ref) And
 {StarMod is always of the type ait_instruction}
               (Pai386(StartMod)^.op1t = top_ref) And
               ((RefsEqual(TReference(Pai386(StartMod)^.op1^), Ref) And
                ((Counter <> WhichReg) Or (NrOfMods <> 1))) Or
                (Not(cs_UncertainOpts in aktglobalswitches) And
                 (NrOfMods <> 1)))
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

Function DoDFAPass2(First: Pai): Pai;
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
    p, hp: Pai;
    TmpRef: TReference;
    TmpReg: TRegister;
Begin
  p := First;
  InstrCnt := 1;
  FillChar(NrOfInstrSinceLastMod, SizeOf(NrOfInstrSinceLastMod), 0);
  While Assigned(p) Do
    Begin
      DoDFAPass2 := p;
{$IfDef TP}
      If (InstrCnt <= NrOfPaiFast) Then
{$EndIf TP}
        CurProp := @PaiPropBlock^[InstrCnt]
{$IfDef TP}
        Else New(CurProp)
{$EndIf TP}
        ;
      If (p <> First)
        Then
{$ifdef JumpAnal}
          Begin
            If (p^.Typ <> ait_label) Then
{$endif JumpAnal}
              Begin
                CurProp^.Regs := PPaiProp(Pai(p^.previous)^.fileinfo.line)^.Regs;
                CurProp^.DirFlag := PPaiProp(Pai(p^.previous)^.fileinfo.line)^.DirFlag
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
      For TmpReg := R_EAX To R_EDI Do
        Inc(NrOfInstrSinceLastMod[TmpReg]);
      Case p^.typ Of
        ait_label:
{$Ifndef JumpAnal}
          DestroyAllRegs(CurProp);
{$Else JumpAnal}
          Begin
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
                        If Not(GetLastInstruction(p, hp) And
                               (hp^.typ = ait_labeled_instruction) And
                               (Pai_Labeled(hp)^._operator = A_JMP))
                          Then
  {previous instruction not a JMP -> the contents of the registers after the
   previous intruction has been executed have to be taken into account as well}
                            For TmpReg := R_EAX to R_EDI Do
                              Begin
                                If (CurProp^.Regs[TmpReg].State <>
                                    PPaiProp(Pai(p^.Previous)^.FileInfo.Line)^.Regs[TmpReg].State)
                                  Then DestroyReg(CurProp, TmpReg)
                              End
                      End
{$IfDef AnalyzeLoops}
                    Else
 {a label from a backward jump (e.g. a loop), no jump to this label has
  already been processed}
                      If Not(GetLastInstruction(p, hp) And
                          (hp^.typ = ait_labeled_instruction) And
                          (Pai_Labeled(hp)^._operator = A_JMP))
                        Then
  {previous instruction not a jmp, so keep all the registers' contents from the
   previous instruction}
                          Begin
                            CurProp^.Regs := PPaiProp(Pai(p^.Previous)^.FileInfo.Line)^.Regs;
                            CurProp^.DirFlag := PPaiProp(Pai(p^.Previous)^.FileInfo.Line)^.DirFlag;
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
                                  CurProp^.Regs := PPaiProp(Pai(p^.Previous)^.FileInfo.Line)^.Regs;
                                  CurProp^.DirFlag := PPaiProp(Pai(p^.Previous)^.FileInfo.Line)^.DirFlag;
                                  DestroyAllRegs(PPaiProp(Pai(p^.Previous)^.FileInfo.Line))
                                End
                          End
{$EndIf AnalyzeLoops}
                Else
{not all references to this label have been found, so destroy all registers}
                  Begin
                    CurProp^.Regs := PPaiProp(Pai(p^.Previous)^.FileInfo.Line)^.Regs;
                    CurProp^.DirFlag := PPaiProp(Pai(p^.Previous)^.FileInfo.Line)^.DirFlag;
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
{$ifdef regalloc}
        ait_regalloc, ait_regdealloc:;
{$endif regalloc}
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
                        If (RegInRef(TmpReg, TReference(Pai386(p)^.op1^)))
                          Then
                            Begin
                              With CurProp^.Regs[TmpReg] Do
                                Begin
                                  IncState(State);
                                  If (typ <> Con_Ref) Then
                                    Begin
                                      typ := Con_Ref;
                                      StartMod := p;
                                    End;
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
                  If InstrProp.NCh <> 255
                    Then
                      For Cnt := 1 To InstrProp.NCh Do
                        Case InstrProp.Ch[Cnt] Of
                          C_None:;
                          C_EAX..C_EDI: DestroyReg(CurProp, TCh2Reg(InstrProp.Ch[Cnt]));
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
                          C_Flags, C_FPU:;
                        End
                    Else
                      Begin
                        DestroyAllRegs(CurProp);
                      End;
                End;
            End;
          End
        Else
          Begin
            DestroyAllRegs(CurProp);
          End;
      End;
      Inc(InstrCnt);
      p := Pai(p^.next);
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
  NrOfPaiObjs := 1;
  While (P <> Pai(AsmL^.last)) Do
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
      P := Pai(P^.next)
    End;
{$IfDef TP}
  If (MemAvail < (SizeOf(TPaiProp)*NrOfPaiObjs))
    {this doesn't have to be one contiguous block}
    Then InitDFAPass2 := False
    Else
      Begin
        InitDFAPass2 := True;
        If (MaxAvail < 65520)
          Then NrOfPaiFast := MaxAvail Div (((SizeOf(TPaiProp)+1) div 2)*2)
          Else NrOfPaiFast := 65520 Div (((SizeOf(TPaiProp)+1) div 2)*2);
        If (NrOfPaiFast > 0) Then
           GetMem(PaiPropBlock, NrOfPaiFast*(((SizeOf(TPaiProp)+1) div 2)*2));
      End;
{$Else}
{Uncomment the next line to see how much memory the reloading optimizer needs}
{  Writeln((NrOfPaiObjs*(((SizeOf(TPaiProp)+3)div 4)*4)));}
{no need to check mem/maxavail, we've got as much virtual memory as we want}
  InitDFAPass2 := True;
  GetMem(PaiPropBlock, NrOfPaiObjs*(((SizeOf(TPaiProp)+3)div 4)*4));
  NrOfPaiFast := NrOfPaiObjs;
  p := Pai(AsmL^.First);
  For Count := 1 To NrOfPaiObjs Do
    Begin
      PaiPropBlock^[Count].LineSave := p^.fileinfo.line;
      PPaiProp(p^.fileinfo.line) := @PaiPropBlock^[Count];
      p := Pai(p^.next);
    End;
 {$EndIf TP}
End;

Function DFAPass2(AsmL: PAasmOutPut): Pai;
Begin
  If InitDFAPass2(AsmL)
    Then DFAPass2 := DoDFAPass2(Pai(AsmL^.First))
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
 Revision 1.9  1998-09-03 16:24:51  florian
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
