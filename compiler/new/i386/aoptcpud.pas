{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
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

uses Aasm, cpubase, AoptCpub, AoptObj, AoptDA;

Type
  PAOptDFACpu = ^TAOptDFACpu;
  TAOptDFACpu = Object(TAOptDFA)
    { uses the same constructor as TAoptDFA = constructor from TAoptObj }

    { handles the processor dependent dataflow analizing               }
    Procedure CpuDFA(p: PInstr); Virtual;
    Function TCh2Reg(Ch: TInsChange): TRegister; Virtual;
    Function RegReadByInstr(Reg: TRegister; p: Pai): Boolean; Virtual;
  End;

Implementation

uses cpuinfo;

Procedure TAOptDFACpu.CpuDFA(p: PInstr);
{ Analyzes the Data Flow of an assembler list. Analyses the reg contents     }
{ for the instructions between blockstart and blockend. Returns the last pai }
{ which has been processed                                                   }
Var CurProp: PPaiProp;
    InstrProp: TInsProp;
    TmpRef: TReference;
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
        CurProp^.DestroyReg(R_EAX, InstrSinceLastMod)
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
            CurProp^.ModifyOp(p^.oper[1], InstrSinceLastMod)
        Else
          CurProp^.ModifyOp(p^.oper[2], InstrSinceLastMod);
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
          CurProp^.ModifyOp(p^.oper[1], InstrSinceLastMod);
        End
    Else
      Begin
        InstrProp := InsProp[p^.OpCode];
        Cnt := 1;
        While (Cnt <= MaxCh) And
              (InstrProp.Ch[Cnt] <> Ch_None) Do
          Begin
            Case InstrProp.Ch[Cnt] Of
              Ch_REAX..Ch_REDI:
                CurProp^.ReadReg(TCh2Reg(InstrProp.Ch[Cnt]));
              Ch_WEAX..Ch_RWEDI:
                Begin
                  If (InstrProp.Ch[Cnt] >= Ch_RWEAX) Then
                    CurProp^.ReadReg(TCh2Reg(InstrProp.Ch[Cnt]));
                  CurProp^.DestroyReg(TCh2Reg(InstrProp.Ch[Cnt]),InstrSinceLastMod);
                End;
              Ch_MEAX..Ch_MEDI:
                CurProp^.ModifyReg(TCh2Reg(InstrProp.Ch[Cnt]), InstrSinceLastMod);
              Ch_CDirFlag: CurProp^.CondRegs.ClearFlag(DirFlag);
              Ch_SDirFlag: CurProp^.CondRegs.SetFlag(DirFlag);
              Ch_Rop1: CurProp^.ReadOp(p^.oper[0]);
              Ch_Rop2: CurProp^.ReadOp(p^.oper[1]);
              Ch_Rop3: CurProp^.ReadOp(p^.oper[2]);
              Ch_Wop1..Ch_RWop1:
                Begin
                  If (InstrProp.Ch[Cnt] = Ch_RWop1) Then
                    CurProp^.ReadOp(p^.oper[0]);
                  CurProp^.DestroyOp(p^.oper[0], InstrSinceLastMod);
                End;
              Ch_Mop1:
                CurProp^.ModifyOp(p^.oper[0], InstrSinceLastMod);
              Ch_Wop2..Ch_RWop2:
                Begin
                  If (InstrProp.Ch[Cnt] = Ch_RWop2) Then
                    CurProp^.ReadOp(p^.oper[1]);
                  CurProp^.DestroyOp(p^.oper[1], InstrSinceLastMod);
                End;
              Ch_Mop2:
                CurProp^.ModifyOp(p^.oper[1], InstrSinceLastMod);
              Ch_Wop3..Ch_RWop3:
                Begin
                  If (InstrProp.Ch[Cnt] = Ch_RWop3) Then
                    CurProp^.ReadOp(p^.oper[2]);
                  CurProp^.DestroyOp(p^.oper[2], InstrSinceLastMod);
                End;
              Ch_Mop3:
                CurProp^.ModifyOp(p^.oper[2], InstrSinceLastMod);
              Ch_WMemEDI:
                Begin
                  CurProp^.ReadReg(R_EDI);
                  FillChar(TmpRef, SizeOf(TmpRef), 0);
                  TmpRef.Base := R_EDI;
                  CurProp^.DestroyRefs(TmpRef, R_NO, InstrSinceLastMod)
                End;
              Ch_RFlags, Ch_WFlags, Ch_RWFlags, Ch_FPU:;
              Else CurProp^.DestroyAllRegs(InstrSinceLastMod)
            End;
            Inc(Cnt)
          End
      End
  End
End;

Function TAOptDFACpu.RegReadByInstr(Reg: TRegister; p: Pai): Boolean;
Var Cnt: AWord;
    InstrProp: TInsProp;
    TmpResult: Boolean;
Begin
  TmpResult := False;
  If (p^.typ = ait_instruction) Then
    Case PInstr(p)^.opcode of
      A_IMUL:
        With PInstr(p)^ Do
          TmpResult :=
            RegInOp(Reg,oper[0]) or
            RegInOp(Reg,oper[1]) or
            ((ops = 1) and
             (reg = R_EAX));
      A_DIV, A_IDIV, A_MUL:
        With PInstr(p)^ Do
          TmpResult :=
            RegInOp(Reg,oper[0]) or
            (Reg = R_EAX) or
            ((Reg = R_EDX) and
             ((opcode = A_DIV) or
              (opcode = A_IDIV)) and
             (opsize = S_L))
      Else
        Begin
          Cnt := 1;
          InstrProp := InsProp[PInstr(p)^.OpCode];
          While (Cnt <= MaxCh) And
                (InstrProp.Ch[Cnt] <> Ch_None) And
                Not(TmpResult) Do
            Begin
              Case InstrProp.Ch[Cnt] Of
                Ch_REAX..Ch_REDI,Ch_RWEAX..Ch_RWEDI
                ,Ch_MEAX..Ch_MEDI
                  TmpResult := Reg = TCh2Reg(InstrProp.Ch[Cnt]);
                Ch_ROp1,Ch_RWOp1,Ch_Mop1:
                  TmpResult := RegInOp(Reg,PInstr(p)^.oper[0]);
                Ch_ROp2,Ch_RWOp2,Ch_Mop2:
                  TmpResult := RegInOp(Reg,PInstr(p)^.oper[1]);
                Ch_ROp3,Ch_RWOp3,Ch_Mop3:
                  TmpResult := RegInOp(Reg,PInstr(p)^.oper[2]);
                Ch_WOp1: TmpResult := (PInstr(p)^.oper[0].typ = top_ref) And
                                     RegInRef(Reg,PInstr(p)^.oper[0].ref^);
                Ch_WOp2: TmpResult := (PInstr(p)^.oper[1].typ = top_ref) And
                                     RegInRef(Reg,PInstr(p)^.oper[1].ref^);
                Ch_WOp3: TmpResult := (PInstr(p)^.oper[2].typ = top_ref) And
                                     RegInRef(Reg,PInstr(p)^.oper[2].ref^);
                Ch_WMemEDI: TmpResult := (Reg = R_EDI);
                Ch_FPU: TmpResult := Reg in [R_ST..R_ST7,R_MM0..R_MM7]
              End;
              Inc(Cnt)
            End
        End
    End;
  RegReadByInstr := TmpResult
End;

Function TAOptDFACpu.TCh2Reg(Ch: TInsChange): TRegister;
Begin
  If (Ch <= Ch_REDI) Then
    TCh2Reg := TRegister(Byte(Ch))
  Else
    If (Ch <= Ch_WEDI) Then
      TCh2Reg := TRegister(Byte(Ch) - Byte(Ch_REDI))
    Else
      If (Ch <= Ch_RWEDI) Then
        TCh2Reg := TRegister(Byte(Ch) - Byte(Ch_WEDI))
      Else
        If (Ch <= Ch_MEDI) Then
          TCh2Reg := TRegister(Byte(Ch) - Byte(Ch_RWEDI))
End;


End.

{
  $Log$
  Revision 1.3  2002-09-07 15:25:14  peter
    * old logs removed and tabs fixed

}
