{
    $Id $
    Copyright (c) 1999 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the processor specific implementation of the
    assembler optimizer data flow analyzer.

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
Unit aoptcpud;

Interface

uses Aasm, AoptCpub, AoptObj, AoptDA;

Type TAsmDFACpu = Object(TAsmDFA)
       { uses the same constructor as TAoptDFA = constructor from TAoptObj }

       { handles the processor dependent dataflow analizing               }
       Procedure CpuDFA(p: PInstr); Virtual;

     End;

Implementation

Procedure TAsmDFACpu(p: PInstr);
{ Analyzes the Data Flow of an assembler list. Analyses the reg contents     }
{ for the instructions between blockstart and blockend. Returns the last pai }
{ which has been processed                                                   }
Var CurProp: PPaiProp;
    InstrProp: TAsmInstrucProp;
    Cnt: Byte;
Begin
  CurProp := PPaiProp(p^.OptInfo);
  Case p^.Opcode Of
    A_DIV, A_IDIV, A_MUL:
      Begin
        CurProp^.ReadOp(p^.oper[0]);
        CurProp^.ReadReg(R_EAX);
        If (p^.OpCode = A_IDIV) or
           (p^.OpCode = A_DIV) Then
          CurProp^.ReadReg(R_EDX);
        DestroyReg(R_EAX)
      End;
    A_IMUL:
      Begin
        CurProp^.ReadOp(p^.oper[0]);
        CurProp^.ReadOp(p^.oper[1]);
        If (p^.oper[2].typ = top_none) Then
          If (p^.oper[1].typ = top_none) Then
            Begin
              CurProp^.ReadReg(R_EAX);
              CurProp^.DestroyReg(R_EAX, InstrSinceLastMod);
              CurProp^.DestroyReg(R_EDX, InstrSinceLastMod)
            End
          Else
  {$ifdef arithopt}
            CurProp^.ModifyOp(p^.oper[1], InstrSinceLastMod)
  {$else arithopt}
            CurProp^.DestroyOp(p^.oper[1], InstrSinceLastMod)
  {$endif arithopt}
        Else
  {$ifdef arithopt}
          CurProp^.ModifyOp(p^.oper[2], InstrSinceLastMod);
  {$else arithopt}
          CurProp^.DestroyOp(p^.oper[2], InstrsinceLastMod);
  {$endif arithopt}
      End;
    A_XOR:
      Begin
        CurProp^.ReadOp(p^.oper[0]);
        CurProp^.ReadOp(p^.oper[1]);
        If (p^.oper[0].typ = top_reg) And
           (p^.oper[1].typ = top_reg) And
           (p^.oper[0].reg = p^.oper[1].reg) Then
          Begin
            CurProp^.DestroyReg(p^.oper[0].reg, InstrSinceLastMod);
            CurProp^.Regs[RegMaxSize(p^.oper[0].reg)].typ := Con_Const;
            CurProp^.Regs[RegMaxSize(p^.oper[0].reg)].StartMod := Pointer(0)
          End
        Else
{$Ifdef ArithOpt}
          CurProp^.ModifyOp(p^.oper[1], InstrSinceLastMod);
{$Else ArithOpt}
          CurProp^.DestroyOp(p^.oper[1], InstrSinceLastMod);
{$EndIf ArithOpt}
        End
    Else
      Begin
        InstrProp := AsmInstr[p^.OpCode];
        Cnt := 1;
        While (Cnt <= MaxCh) And
              (InstrProp.Ch[Cnt] <> C_None) Do
          Begin
            Case InstrProp.Ch[Cnt] Of
              C_REAX..C_REDI:
                CurProp^.ReadReg(TCh2Reg(InstrProp.Ch[Cnt]));
              C_WEAX..C_RWEDI:
                Begin
                  If (InstrProp.Ch[Cnt] >= C_RWEAX) Then
                    CurProp^.ReadReg(TCh2Reg(InstrProp.Ch[Cnt]));
                  CurProp^.DestroyReg(TCh2Reg(InstrProp.Ch[Cnt]),InstrSinceLastMod);
                End;
          {$ifdef arithopt}
              C_MEAX..C_MEDI:
                CurProp^.ModifyReg(TCh2Reg(InstrProp.Ch[Cnt]), InstrSinceLastMod);
          {$endif arithopt}
              C_CDirFlag: CurProp^.CondRegs.ClearFlag(DirFlag);
              C_SDirFlag: CurProp^.CondRegs.SetFlag(DirFlag);
              C_Rop1: ReadOp(CurProp, p^.oper[0]);
              C_Rop2: ReadOp(CurProp, p^.oper[1]);
              C_ROp3: ReadOp(CurProp, p^.oper[2]);
              C_Wop1..C_RWop1:
                Begin
                  If (InstrProp.Ch[Cnt] = C_RWop1) Then
                    ReadOp(CurProp, p^.oper[0]);
                  DestroyOp(p, p^.oper[0], InstrSinceLastMod);
                End;
        {$ifdef arithopt}
              C_Mop1:
                CurProp^.ModifyOp(p^.oper[0], InstrSinceLastMod);
        {$endif arithopt}
              C_Wop2..C_RWop2:
                Begin
                  If (InstrProp.Ch[Cnt] = C_RWop2) Then
                    ReadOp(CurProp, p^.oper[1]);
                  DestroyOp(p, p^.oper[1], InstrSinceLastMod);
                End;
        {$ifdef arithopt}
              C_Mop2:
                CurProp^.ModifyOp(p^.oper[1], InstrSinceLastMod);
        {$endif arithopt}
              C_Wop3..C_RWop3:
                Begin
                  If (InstrProp.Ch[Cnt] = C_RWop3) Then
                    ReadOp(CurProp, p^.oper[2]);
                  DestroyOp(p, p^.oper[2], InstrSinceLastMod);
                End;
        {$ifdef arithopt}
              C_Mop3:
                CurProp^.ModifyOp(p^.oper[2], InstrSinceLastMod);
        {$endif arithopt}
              C_WMemEDI:
                Begin
                  CurProp^.ReadReg(R_EDI);
                  FillChar(TmpRef, SizeOf(TmpRef), 0);
                  TmpRef.Base := R_EDI;
                  DestroyRefs(p, TmpRef, R_NO, InstrSinceLastMod)
                End;
              C_RFlags, C_WFlags, C_RWFlags, C_FPU:;
              Else DestroyAllRegs(CurProp, InstrSinceLastMod)
            End;
            Inc(Cnt)
          End
      End
  End
End;


End.

{
  $Log$
  Revision 1.1  1999-08-11 14:22:56  jonas
    + first version, added TAsmDFACpu object and CpuDFA method

}
