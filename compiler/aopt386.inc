{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl and Jonas Maebe

    This include file contains the reloading optimizer for i386+

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


{$Define OptimizeMovs}

Type    TwoWords = Record
            Word1, Word2: Word
        End;
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

Procedure ReloadOpt(AsmL: PaasmOutput);

Const MaxCh = 3;

      {content types}
      con_Unknown = 0;
      con_ref = 1;
      con_const = 2;
      con_symbol = 3;

Type TChange = (C_None,
                C_EAX, C_ECX, C_EDX, C_EBX, C_ESP, C_EBP, C_ESI, C_EDI,
{                C_AX,  C_CX,  C_DX,  C_BX,  C_SP,  C_BP,  C_SI,  C_DI,
                C_AL,  C_CL,  C_DL,  C_BL,
                C_AH,  C_CH,  C_BH,  C_DH,
                C_DEFAULT_SEG, C_CS, C_DS, C_ES, C_FS, C_GS, C_SS,
}                C_Flags, C_FPU,
                C_Op1, C_Op2, C_Op3,
                C_MemEDI);

     TAsmInstrucProp = Record
                         NCh: Byte;
                         Ch: Array[1..MaxCh] of TChange;
                       End;

     TContent = Record
                  StartMod: Pointer; {start and end of block instructions that defines the
                                          content of this register; If Typ = con_const, then
                                          Longint(StartMod) = value of the constant)}
                   State: Word; {starts at 0, gets increased everytime the register is modified}
                   NrOfMods: Byte;
{                  ModReg: TRegister; }{if one register gets a block assigned from an other register,
                                      this variable holds the name of that register (so it can be
                                      substituted when checking the block afterwards)}
                  Typ: Byte;        {con_*}
{                  CanBeDestroyed: Boolean;} {if it's a register modified by the optimizer}
                End;

     TRegContent = Array[R_NO..R_EDI] Of TContent;
     TRegFPUContent = Array[R_ST..R_ST7] Of TContent;

     TPaiProp = Record
                  Regs: TRegContent;
{                  FPURegs: TRegFPUContent;} {currently not yet used}
                  LineSave: Longint;
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
   {CLD} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
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
   {STD} (NCh: 1; Ch: (C_Flags, C_None, C_None)),
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
{A_MOVD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_MOVQ} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PACKSSDW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PACKSSWB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PACKUSWB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PADDB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PADDD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PADDSB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PADDSW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PADDUSB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PADDUSW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PADDW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PAND} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PANDN} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPEQB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPEQD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPEQW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPGTB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPGTD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PCMPGTW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PMADDWD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PMULHW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PMULLW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_POR} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSLLD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSLLQ} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSLLW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSRAD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSRAW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSRLD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSRLQ} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSRLW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSUBB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSUBD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSUBSB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSUBSW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSUBUSB} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSUBUSW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PSUBW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKHBW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKHDQ} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKHWD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKLBW} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKLDQ} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PUNPCKLWD} (NCh: 255; Ch: (C_FPU, C_None, C_None)),
{A_PXOR} (NCh: 255; Ch: (C_FPU, C_None, C_None)));

Var NrOfPaiObjs, NrOfPaiFast: Longint;
    PaiPropBlock: PPaiPropBlock;
    NrOfInstrSinceLastMod: Array[R_EAX..R_EDI] Of Byte;

Function TCh2Reg(Ch: TChange): TRegister;
{converts a TChange variable to a TRegister}
Begin
  If (CH <= C_EDI)
    Then TCh2Reg := TRegister(Byte(Ch))
    Else InternalError($db)
End;

Procedure DestroyReg(p1: pai; Reg: TRegister);
{Destroys the contents of the register Reg in the PPaiProp of P}
Var TmpState: Longint;
Begin
  Reg := Reg32(Reg);
  NrOfInstrSinceLastMod[Reg] := 0;
  If (Reg >= R_EAX) And (Reg <= R_EDI)
    Then
      Begin
        TmpState := PPaiProp(p1^.line)^.Regs[Reg].State+1;
        FillChar(PPaiProp(p1^.line)^.Regs[Reg], SizeOf(TContent), 0);
        PPaiProp(p1^.line)^.Regs[Reg].State := TmpState;
      End;
End;

(*Function FindZeroreg(p: Pai; Var Result: TRegister): Boolean;
{Finds a register which contains the constant zero}
Var Counter: TRegister;
Begin
  Counter := R_EAX;
  FindZeroReg := True;
  While (Counter <= R_EDI) And
        ((PPaiProp(p^.line)^.Regs[Counter].Typ <> Con_Const) or
         (PPaiProp(p^.line)^.Regs[Counter].StartMod <> Pointer(0))) Do
    Inc(Byte(Counter));
  If (PPaiProp(p^.line)^.Regs[Counter].Typ = Con_Const) And
     (PPaiProp(p^.line)^.Regs[Counter].StartMod = Pointer(0))
    Then Result := Counter
    Else FindZeroReg := False;
End;*)

Procedure DestroyRefs(p: pai; Const Ref: TReference; WhichRegNot: TRegister);
{destroys all registers which possibly contain a reference to Ref}
Var Counter: TRegister;
Begin
WhichRegNot := Reg32(WhichRegNot);
If (Ref.base <> R_NO) Or
   (Ref.index <> R_NO)
  Then
    Begin
      If (Ref.base = ProcInfo.FramePointer)
        Then
{write something to a parameter or a local variable}
          For Counter := R_EAX to R_EDI Do
            With PPaiProp(p^.line)^.Regs[Counter] Do
              Begin
                If (Counter <> WhichRegNot) And
                   (typ = Con_Ref) And
                   (Pai(StartMod)^.typ = ait_instruction) And
                   (Pai386(StartMod)^.op1t = top_ref) And
                   (RefsEqual(TReference(Pai386(StartMod)^.op1^), Ref) Or
                   (Not(cs_UncertainOpts in AktSwitches) And
                    (NrOfMods <> 1)))
                  Then DestroyReg(p, Counter)
              End
        Else
          {writing something to a pointer location}
          For Counter := R_EAX to R_EDI Do
            With PPaiProp(p^.line)^.Regs[Counter] Do
            If (Counter <> WhichRegNot) And
               (typ = Con_Ref) And
               (Not(cs_UncertainOpts in AktSwitches) Or
                (Ref.Base = R_EDI) Or
                (Not((NrOfMods = 1) And
                (Pai(StartMod)^.typ = ait_instruction) And
                (Pai386(StartMod)^.op1t = top_ref) And
                (PReference(Pai386(StartMod)^.op1)^.base = ProcInfo.FramePointer))))
              Then
                DestroyReg(p, Counter) {we don't know what memory location the reference points to,
                                      so we just destroy every register which contains a memory
                                      reference}
    End
  Else {the ref is a var name or we just have a reference an absolute offset}
    Begin
      For Counter := R_EAX to R_EDI Do
        If (Counter <> WhichRegNot) And
           (PPaiProp(p^.line)^.Regs[Counter].typ = Con_Ref) And
           (Not(cs_UncertainOpts in AktSwitches) Or
            RefsEqual(Ref,
                     TReference(Pai386(PPaiProp(p^.line)^.Regs[Counter].StartMod)^.op1^))) Then
          DestroyReg(p, Counter)
    End;
End;

{$IfDef OptimizeMovs}

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
    PPaiProp(p1^.line)^.Regs[Reg].State =
    PPaiProp(p2^.line)^.Regs[Reg].State;
End;

Function InstructionsEqual(p1, p2: Pai): Boolean;
Begin {checks whether two Pai386 instructions are equal}
  InstructionsEqual :=
    Assigned(p1) And Assigned(p2) And
    (Pai(p1)^.typ = ait_instruction) And
    (Pai(p1)^.typ = ait_instruction) And
    (Pai386(p1)^._operator = Pai386(p2)^._operator) And
    (Pai386(p1)^.op1t = Pai386(p2)^.op1t) And
    (Pai386(p1)^.op2t = Pai386(p2)^.op2t) And
    OpsEqual(Pai386(p1)^.op1t, Pai386(p1)^.op1, Pai386(p2)^.op1) And
    OpsEqual(Pai386(p1)^.op2t, Pai386(p1)^.op2, Pai386(p2)^.op2)
End;

Function CheckSequence(p: Pai; Reg: TRegister; Var Found: Longint): Boolean;
{checks whether the current instruction sequence (starting with p) and the
 one between StartMod and EndMod of Reg are the same. If so, the number of
 instructions that match is stored in Found and true is returned, otherwise
 Found holds the number of instructions between StartMod and EndMod and false
 is returned}
Var hp2, hp3, EndMod: Pai;
    TmpResult: Boolean;
    RegsNotYetChecked: Set Of TRegister;
    Counter: Byte;

Function NoChangedRegInRef(oldp, newp: Pai): Boolean;
Var TmpP: Pai;
{checks if the first operator of newp is a reference and in that case checks
 whether that reference includes regs that have been changed since oldp. This
 to avoid wrong optimizations like

 movl 8(%epb), %eax                          movl 8(%epb), %eax
 movl 12(%epb), %edx                         movl 12(%epb), %edx
 movl (%eax,%edx,1), %edi                    movl (%eax,%edx,1), %edi
 pushl %edi              being converted to  pushl %edi
 movl 8(%epb), %eax                          movl 16(%ebp), %edx
 movl 16(%epb), %edx                         pushl %edi
 movl (%eax,%edx,1), %edi
 pushl %edi

because first is checked whether %eax isn't changed (it isn't) and
consequently all instructions containg %eax are removed}
Begin
  TmpResult := True;
  If (Pai(oldp)^.typ = ait_instruction) Then {oldp and newp are the same instruction}
    Case Pai386(oldp)^.op1t Of
      Top_Reg:
        If (Reg32(TRegister(Pai386(oldp)^.op1)) in RegsNotYetChecked) Then
          Begin
            RegsNotYetChecked := RegsNotYetChecked - [Reg32(TRegister(Pai386(oldp)^.op1))];
            If Assigned(newp^.Last)
              Then
                Begin
                  TmpP := Pai(newp^.last);
                  While Assigned (TmpP^.Last) And
                        PPaiProp(TmpP^.Line)^.CanBeRemoved Do
                    TmpP := Pai(TmpP^.Last);
                  TmpResult := Assigned(TmpP) And
                               RegsSameContent(oldp, TmpP, Reg32(TRegister(Pai386(oldp)^.op1)))
                End
              Else TmpResult := False;
          End;
      Top_Ref:
        With TReference(Pai386(oldp)^.op1^) Do
          Begin
            If (Base in RegsNotYetChecked) And
               (Base <> R_NO) Then
              Begin
                RegsNotYetChecked := RegsNotYetChecked - [Base];
                If Assigned(newp^.Last)
                  Then
                    Begin
                      TmpP := Pai(newp^.last);
                      While Assigned (TmpP^.Last) And
                            PPaiProp(TmpP^.Line)^.CanBeRemoved Do
                        TmpP := Pai(TmpP^.Last);
                      TmpResult := Assigned(TmpP) And
                                   RegsSameContent(oldp, TmpP, Base)
                    End
                  Else TmpResult := False;
              End;
            If TmpResult And
               (Index <> R_NO) And
               (Index in RegsNotYetChecked) Then
              Begin
                RegsNotYetChecked := RegsNotYetChecked - [Index];
                If Assigned(newp^.Last)
                  Then
                    Begin
                      TmpP := Pai(newp^.last);
                      While Assigned (TmpP^.Last) And
                            PPaiProp(TmpP^.Line)^.CanBeRemoved Do
                        TmpP := Pai(TmpP^.Last);
                      TmpResult := Assigned(TmpP) And
                                   RegsSameContent(oldp, TmpP, Index)
                    End
                  Else TmpResult := False;
              End;
          End;
    End;
  NoChangedRegInRef := TmpResult;
End;

Begin {CheckSequence}
  Reg := Reg32(Reg);
  Found := 0;
  hp2 := p;
  hp3 := PPaiProp(Pai(p^.last)^.line)^.Regs[Reg].StartMod;
  EndMod := PPaiProp(Pai(p^.last)^.line)^.Regs[Reg].StartMod;
  RegsNotYetChecked := [R_EAX..R_EDI];
  For Counter := 2 to PPaiProp(Pai(p^.last)^.line)^.Regs[Reg].NrOfMods Do
    EndMod := Pai(EndMod^.Next);
  While (Found <> PPaiProp(Pai(p^.last)^.line)^.Regs[Reg].NrOfMods) And
         InstructionsEqual(hp2, hp3) And
         NoChangedRegInRef(EndMod, hp2) Do
    Begin
      hp2 := Pai(hp2^.next);
      hp3 := Pai(hp3^.next);
      Inc(Found)
    End;
  If (Found <> PPaiProp(Pai(p^.last)^.line)^.Regs[Reg].NrOfMods)
     Then
       Begin
         CheckSequence := False;
         If (found > 0) then
 {this is correct because we only need to turn off the CanBeRemoved flag
  when an instruction has already been processed by CheckSequence
  (otherwise CanBeRemoved can't be true, or can't have to be turned off).
  If it has already been processed by checkSequence and flagged to be
  removed, it means that it has been checked against a previous sequence
  and that it was equal (otherwise CheckSequence would have returned false
  and the instruction wouldn't have been removed). If this "If found > 0"
  check is left out, incorrect optimizations are performed.}
           Found := PPaiProp(Pai(p)^.line)^.Regs[Reg].NrOfMods
       End
     Else CheckSequence := True;
End; {CheckSequence}

{$Endif OptimizeMovs}

Procedure DestroyAllRegs(p: Pai);
Var Counter: TRegister;
Begin {initializes/desrtoys all registers}
  For Counter := R_EAX To R_EDI Do
    DestroyReg(p, Counter);
End;

Procedure Destroy(PaiObj: Pai; opt: Longint; Op: Pointer);
Begin
  Case opt Of
    top_reg: DestroyReg(PaiObj, TRegister(Op));
    top_ref: DestroyRefs(PaiObj, TReference(Op^), R_NO);
    top_symbol:;
  End;
End;

Function CreateRegs(First: Pai): Pai;
{Starts creating the reg contents for the instructions starting with p.
Returns the last pai which has been processed}
Var
    TmpProp: PPaiProp;
    Cnt, InstrCnt: Longint;
    InstrProp: TAsmInstrucProp;
    p: Pai;
    TmpRef: TReference;
    TmpReg: TRegister;
Begin
  p := First;
  InstrCnt := 1;
  FillChar(NrOfInstrSinceLastMod, SizeOf(NrOfInstrSinceLastMod), 0);
  While Assigned(p) Do
    Begin
      CreateRegs := p;
      If (InstrCnt <= NrOfPaiFast)
        Then TmpProp := @PaiPropBlock^[InstrCnt]
        Else New(TmpProp);
      If (p <> First)
        Then TmpProp^ := PPaiProp(Pai(p^.last)^.line)^
        Else FillChar(TmpProp^, SizeOf(TmpProp^), 0);
      TmpProp^.LineSave := p^.line;
      PPaiProp(p^.line) := TmpProp;
      For TmpReg := R_EAX To R_EDI Do
        Inc(NrOfInstrSinceLastMod[TmpReg]);
      Case p^.typ Of
        ait_label: DestroyAllRegs(p);
        ait_labeled_instruction, ait_stabs, ait_stabn,
        ait_stab_function_name:; {nothing changes}
        ait_instruction:
          Begin
            InstrProp := AsmInstr[Pai386(p)^._operator];
            Case Pai386(p)^._operator Of
{$IfDef OptimizeMovs}
              A_MOV, A_MOVZX, A_MOVSX:
                Begin
                  Case Pai386(p)^.op1t Of
                    Top_Reg:
                      Case Pai386(p)^.op2t Of
                        Top_Reg:
                          Begin
                            DestroyReg(p, TRegister(Pai386(p)^.op2));
{                            TmpProp^.Regs[TRegister(Pai386(p)^.op2)] :=
                              TmpProp^.Regs[TRegister(Pai386(p)^.op1)];
                            If (TmpProp^.Regs[TRegister(Pai386(p)^.op2)].ModReg = R_NO) Then
                              TmpProp^.Regs[TRegister(Pai386(p)^.op2)].ModReg :=
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
                              With PPaiProp(Pai(p)^.line)^.Regs[TmpReg] Do
                                Begin
                                  Inc(State);
 {also store how many instructions are part of the sequence in the first
  instructions PPaiProp, so it can be easily accessed from within
  CheckSequence}
                                  If (typ <> Con_Ref) Then
                                    Begin
                                      typ := Con_Ref;
                                      StartMod := p;
                                    End;
                                  Inc(NrOfMods, NrOfInstrSinceLastMod[TmpReg]);
                                  PPaiProp(Pai(StartMod)^.line)^.Regs[TmpReg].NrOfMods := NrOfMods;
                                  NrOfInstrSinceLastMod[TmpReg] := 0;
                                End;
                            End
                          Else
                            Begin
                              DestroyReg(p, TmpReg);
                              With PPaiProp(Pai(p)^.line)^.Regs[TmpReg] Do
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
                              With TmpProp^.Regs[TmpReg] Do
                                Begin
                                {it doesn't matter that the state is changed,
                                 it isn't looked at when removing constant reloads}
                                  DestroyReg(p, TmpReg);
                                  typ := Con_Const;
                                  StartMod := Pai386(p)^.op1;
                                End
                            End;
                          Top_Ref: DestroyRefs(P, TReference(Pai386(p)^.op2^), R_NO);
                        End;
                      End;
                End;
              End;
{$EndIf OptimizeMovs}
              A_IMUL:
                Begin
                  If (Pai386(p)^.Op3t = top_none)
                   Then
                     If (Pai386(p)^.Op2t = top_none)
                       Then
                         Begin
                           DestroyReg(p, R_EAX);
                           DestroyReg(p, R_EDX)
                         End
                       Else
                         Begin
                           If (Pai386(p)^.Op2t = top_reg) Then
                             DestroyReg(p, TRegister(Pai386(p)^.Op2));
                         End
                   Else If (Pai386(p)^.Op3t = top_reg) Then
                          DestroyReg(p, TRegister(longint(twowords(Pai386(p)^.Op2).word2)));
                End;
              A_XOR:
                Begin
                  If (Pai386(p)^.op1t = top_reg) And
                     (Pai386(p)^.op2t = top_reg) And
                     (Pai386(p)^.op1 = Pai386(p)^.op2)
                    Then
                      Begin
                        DestroyReg(p, Tregister(Pai386(p)^.op1));
                        TmpProp^.Regs[Reg32(Tregister(Pai386(p)^.op1))].typ := Con_Const;
                        TmpProp^.Regs[Reg32(Tregister(Pai386(p)^.op1))].StartMod := Pointer(0)
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
                          C_Op1: Destroy(p, Pai386(p)^.op1t, Pai386(p)^.op1);
                          C_Op2: Destroy(p, Pai386(p)^.op2t, Pai386(p)^.op2);
                          C_Op3: Destroy(p, Pai386(p)^.op2t, Pointer(Longint(TwoWords(Pai386(p)^.op2).word2)));
                          C_MemEDI:
                            Begin
                              FillChar(TmpRef, SizeOf(TmpRef), 0);
                              TmpRef.Base := R_EDI;
                              DestroyRefs(p, TmpRef, R_NO)
                            End;
                          C_EAX..C_EDI: DestroyReg(p, TCh2Reg(InstrProp.Ch[Cnt]));
                          C_Flags, C_FPU:;
                        End
                    Else
                      Begin
                        DestroyAllRegs(p);
                      End;
                End;
            End;
          End
        Else
          Begin
            DestroyAllRegs(p);
          End;
      End;
      Inc(InstrCnt);
      p := Pai(p^.next);
    End;
End;

Procedure OptimizeBlock(First, Last: Pai);
{marks the instructions that can be removed by RemoveInstructs. They're not
 removed immediately because sometimes an instruction needs to be checked in
 two different sequences}
Var Cnt, Cnt2: Longint;
    p, hp1, hp2: Pai;
Begin
  p := First;
  While (p <> Pai(Last^.Next)) Do
    Begin
      Case p^.typ Of
        ait_label, ait_labeled_instruction:;
        ait_instruction:
          Begin
            Case Pai386(p)^._operator Of
{$IfDef OptimizeMovs}
              A_MOV{, A_MOVZX, A_MOVSX}:
                Begin
                  Case Pai386(p)^.op1t Of
{                    Top_Reg:
                      Case Pai386(p)^.op2t Of
                        Top_Reg:;
                        Top_Ref:;
                      End;}
                    Top_Ref:
                      Begin {destination is always a register in this case}
                        With PPaiProp(p^.line)^.Regs[Reg32(Tregister(Pai386(p)^.op2))] Do
                          Begin
                            If Assigned(p^.last) And
                               (PPaiProp(Pai(p^.last)^.line)^.Regs[Reg32(TRegister(Pai386(p)^.op2))].typ = con_ref) Then
   {so we don't try to check a sequence when the register only contains a constant}
                               If CheckSequence(p, TRegister(Pai386(p)^.op2), Cnt) And
                                  (Cnt > 0)
                                 Then
                                   Begin
                                     hp1 := nil;
   {although it's perfectly ok to remove an instruction which doesn't contain
    the register that we've just checked (CheckSequence takes care of that),
    the sequence containing this other register should also be completely
    checked and removed, otherwise we may get situations like this:

      movl 12(%ebp), %edx                       movl 12(%ebp), %edx
      movl 16(%ebp), %eax                       movl 16(%ebp), %eax
      movl 8(%edx), %edx                        movl 8(%edx), %edx
      movl (%eax), eax                          movl (%eax), eax
      cmpl %eax, %edx                           cmpl %eax, %edx
      jnz  l123           getting converted to  jnz  l123
      movl 12(%ebp), %edx                       movl 4(%eax), eax
      movl 16(%ebp), %eax
      movl 8(%edx), %edx
      movl 4(%eax), eax}
                                     hp2 := p;
                                     For Cnt2 := 1 to Cnt Do
                                       Begin
                                         If Not(Pai(p)^.typ In [ait_stabs, ait_stabn, ait_stab_function_name]) Then
                                           Begin
                                             If (hp1 = nil) And
                                                Not(RegInInstruction(Tregister(Pai386(hp2)^.op2), p))
                                               Then hp1 := p;
                                             PPaiProp(p^.line)^.CanBeRemoved := True;
                                           End;
                                         p := Pai(p^.next);
                                       End;
                                     If hp1 <> nil Then p := hp1;
                                     Continue;
                                   End
                                 Else
                                   If (Cnt > 0) And
                                      (PPaiProp(p^.line)^.CanBeRemoved) Then
                                     Begin
                                       hp2 := p;
                                       For Cnt2 := 1 to Cnt Do
                                         Begin
                                           If RegInInstruction(Tregister(Pai386(hp2)^.op2), p)
                                             Then PPaiProp(p^.Line)^.CanBeRemoved := False;
                                           p := Pai(p^.Next)
                                         End;
                                       Continue;
                                     End;
                          End;
                      End;
                    Top_Const:
                      Begin
                        Case Pai386(p)^.op2t Of
                          Top_Reg:
                            Begin
                              If Assigned(p^.last) Then
                                With PPaiProp(Pai(p^.last)^.line)^.Regs[Reg32(TRegister(Pai386(p)^.op2))] Do
                                  If (Typ = Con_Const) And
                                     (StartMod = Pai386(p)^.op1) Then
                                    PPaiProp(p^.line)^.CanBeRemoved := True;
                            End;
                          Top_Ref:;
                        End;
                      End;
                  End;
                End;
{$EndIf OptimizeMovs}
              A_XOR:
                Begin
                  If (Pai386(p)^.op1t = top_reg) And
                     (Pai386(p)^.op2t = top_reg) And
                     (Pai386(p)^.op1 = Pai386(p)^.op2) And
                     Assigned(p^.last) And
                     (PPaiProp(Pai(p^.last)^.line)^.Regs[Reg32(Tregister(Pai386(p)^.op1))].typ = con_const) And
                     (PPaiProp(Pai(p^.last)^.line)^.Regs[Reg32(Tregister(Pai386(p)^.op1))].StartMod = Pointer(0))
                    Then PPaiProp(p^.line)^.CanBeRemoved := True
                End
            End
          End;
      End;
      p := Pai(p^.next);
    End;
End;

Procedure RemoveInstructs(First, Last: Pai);
{Removes the marked instructions and disposes the PPaiProps of the other
 instructions, restoring theirline number}
Var p, hp1: Pai;
    TmpLine, InstrCnt: Longint;
Begin
  p := First;
  InstrCnt := 1;
  While (p <> Pai(Last^.Next)) Do
    If PPaiProp(p^.line)^.CanBeRemoved
      Then
        Begin
          If (InstrCnt > NrOfPaiFast) Then
            Dispose(PPaiProp(p^.Line));
          hp1 := Pai(p^.Next);
          AsmL^.Remove(p);
          Dispose(p, Done);
          p := hp1;
          Inc(InstrCnt)
        End
      Else
        Begin
          If (InstrCnt > NrOfPaiFast)
            Then
              Begin
                TmpLine := PPaiProp(p^.Line)^.LineSave;
                Dispose(PPaiProp(p^.Line));
                p^.Line := TmpLine;
              End
            Else p^.Line := PPaiProp(p^.Line)^.LineSave;
          p := Pai(p^.Next);
          Inc(InstrCnt)
        End;
  If (NrOfPaiFast > 0) Then
{$IfDef TP}
    Freemem(PaiPropBlock, NrOfPaiFast*(((SizeOf(TPaiProp)+1)div 2)*2))
{$Else}
    FreeMem(PaiPropBlock, NrOfPaiFast*(((SizeOf(TPaiProp)+3)div 4)*4))
{$EndIf TP}
End;

Function InitReloadOpt(AsmL: PAasmOutput): Boolean;
{reserves memory for the PPaiProps in one big memory block when not using
 TP, returns False if not enough memory is available for the optimizer in all
 cases}
Var p: Pai;
Begin
  P := Pai(AsmL^.First);
  NrOfPaiObjs := 1;
  While (P <> Pai(AsmL^.Last)) Do
    Begin
      Inc(NrOfPaiObjs);
      P := Pai(P^.next)
    End;
{$IfDef TP}
  If (MemAvail < (SizeOf(TPaiProp)*NrOfPaiObjs))
    {this doesn't have to be one contiguous block}
    Then InitReloadOpt := False
    Else
      Begin
        InitReloadOpt := True;
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
  InitReloadOpt := True;
  GetMem(PaiPropBlock, NrOfPaiObjs*(((SizeOf(TPaiProp)+3)div 4)*4));
  InitReloadOpt := True;
  NrOfPaiFast := NrOfPaiObjs;
 {$EndIf TP}
End;

Var BlockEnd: Pai;

Begin {ReloadOpt}
  If InitReloadOpt(AsmL)
    Then
      Begin
        BlockEnd := CreateRegs(Pai(AsmL^.First));
        OptimizeBlock(Pai(AsmL^.First), BlockEnd);
        RemoveInstructs(Pai(AsmL^.First), BlockEnd)
      End;
End;

{
 $Log$
 Revision 1.1.1.1  1998-03-25 11:18:12  root
 * Restored version

 Revision 1.22  1998/03/24 21:48:29  florian
   * just a couple of fixes applied:
        - problem with fixed16 solved
        - internalerror 10005 problem fixed
        - patch for assembler reading
        - small optimizer fix
        - mem is now supported

 Revision 1.21  1998/03/11 15:45:35  florian
   * -Oa problem solved

 Revision 1.20  1998/03/10 01:17:13  peter
   * all files have the same header
   * messages are fully implemented, EXTDEBUG uses Comment()
   + AG... files for the Assembler generation

 Revision 1.19  1998/03/09 16:46:27  jonas
   * fixed bug with uncertain optimizations when moving data among variables using movsl

 Revision 1.18  1998/03/04 16:42:00  jonas
   * bugfix in destroyrefs and fixed a potential bug in createregs

 Revision 1.17  1998/03/03 20:33:29  jonas
   * TContent record now only occupies 8 bytes anymore

 Revision 1.15  1998/03/03 01:08:13  florian
   * bug0105 and bug0106 problem solved

 Revision 1.14  1998/03/02 21:35:16  jonas
   * added comments from last update

 Revision 1.13  1998/03/02 21:29:06  jonas
   * redesigned TContent record so it occupies only 13 bytes anymore (was about 18)
   * store TPaiProps of 16 and 8 bit registers in those of the 32 bit regs
   * fixed a small bug which prevented some optimizes from being performed
   * store TPaiProps in one big array instead of in seperate records
   * warning: TP version not tested because I only have TP, not BP (-> no proteced mode apps)

 Revision 1.12  1998/02/24 21:18:13  jonas
   * file name back to lower case

 Revision 1.4  1998/02/24 20:32:12  jonas
   * added comments from latest commit

 Revision 1.3  1998/02/24 20:27:51  jonas
   * if a register is being written to memory, it's contents aren't destroyed
    (wherever it's been written to, its contents are up-to-date)
   * changed the order in which some functions/procedure are defined, because some
    of them are now used by aopt386.pas

 Revision 1.11  1998/02/19 22:46:54  peter
   * Fixed linebreaks

 Revision 1.10  1998/02/13 10:34:31  daniel
 * Made Motorola version compilable.
 * Fixed optimizer

 Revision 1.9  1998/02/12 17:18:49  florian
   * fixed to get remake3 work, but needs additional fixes (output, I don't like
     also that aktswitches isn't a pointer)

 Revision 1.8  1998/02/12 11:49:37  daniel
 Yes! Finally! After three retries, my patch!

 Changes:

 Complete rewrite of psub.pas.
 Added support for DLL's.
 Compiler requires less memory.
 Platform units for each platform.

 Revision 1.7  1998/02/07 10:11:19  michael
   * RefsEqual made less harsh:

       * when something is written to x(%ebp), registers which contain
         a pointer that isn't "x(%ebp)"-based isn't destroyed

       * when something is written to a pointer location, registers
         which contain the contents of x(%ebp) aren't destroyed


 Revision 1.6  1998/01/12 17:45:20  jonas
   * merged DisposeProps and RemoveInstructs procedures (speed!)

 Revision 1.5  1998/01/11 22:51:30  jonas
   * back to unix linebreaks...(hate it! :)

 Revision 1.4 1998/01/11 22:50:10  jonas
   * all floating point store operations now change op1 instead of the fpu regs

 Revision 1.3  1998/01/11 14:40:04  jonas
   * bugfix in optimize procedure (too many instructions were removed in certain cases)

 Revision 1.1  1997/12/30 21:10:34  jonas
   * changed back to unix/linux line breaks


  Pre-CVS log:


   JM   Jonas Maebe

  + feature added
  - removed
  * bug fixed or changed

  History (started on 2nd December 1997):
       2nd December 1997:
         + initial version (JM)
         + removes redundant "xor %reg, %reg"'s (JM)
       3rd December 1997:
         + removes certain redundant movs (still bugged) (JM)
         * A_REP now destroys ECX
       4th December 1997:
         * fixed bugs in mov-removal (still bugged) (JM)
       5th December 1997:
         * fixed more bugs in mov-removal (a compiler compiled with these
           optimizations now can compile itself suyccessfully!) and enhanced
           it (introducing new bugs, which have to be fixed again...) (JM)
         * A_AND and A_OR now destroy op2 instead of op1 <g> (JM)
       6th December 1997:
         * A_PUSHAD now only destroys ESP instead of all registers (JM)
         * A_REPE and A_REPNE now also destroy ECX (JM)
         * Rewrote some procedures so it's a bit more modular and easier/
           cleaner/possible to do some optimizations, but it's slower (JM)
         * enabled mov-reloading optimization for A_MOVZX and A_MOVSX
           (actually it's already 7 December, 1:25 am in the mean time :) (JM)
       7th December 1997:
         * All instructions okayed by CheckSequence are now being removed (JM)


       To Do:
         * special case for A_XCHG
         * implementation of ModReg comparing
         * special case for lea
         * fpu optimizing
         * active optimizing (ie. change certain register allocations)
         * make DestroyRefs a little less harsh
         * bug fixes?
}
