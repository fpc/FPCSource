{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the data flow analyzer object of the assembler
    optimizer.

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
Unit aoptda;

{$i fpcdefs.inc}

  Interface

    uses
      cpubase,cgbase,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      aoptcpub, aoptbase;

    Type
      TAOptDFA = class
        { uses the same constructor as TAoptCpu = constructor from TAoptObj }

        { gathers the information regarding the contents of every register }
        { at the end of every instruction                                  }
        Procedure DoDFA;

        { handles the processor dependent dataflow analizing               }
        Procedure CpuDFA(p: PInstr); Virtual; Abstract;

        { How many instructions are between the current instruction and the }
        { last one that modified the register                               }
        InstrSinceLastMod: TInstrSinceLastMod;

        { convert a TInsChange value into the corresponding register }
        //!!!!!!!!!! Function TCh2Reg(Ch: TInsChange): TRegister; Virtual;
        { returns whether the instruction P reads from register Reg }
        Function RegReadByInstr(Reg: TRegister; p: tai): Boolean; Virtual; Abstract;
      End;

  Implementation

    uses
      globals, aoptobj;

    Procedure TAOptDFA.DoDFA;
    { Analyzes the Data Flow of an assembler list. Analyses the reg contents     }
    { for the instructions between blockstart and blockend. Returns the last pai }
    { which has been processed                                                   }
    {
    Var
        CurProp: TPaiProp;
        UsedRegs: TUsedRegs;
        p, hp, NewBlockStart : tai;
        TmpReg: TRegister;
    }
    Begin
    {!!!!!!!!!!
      p := BlockStart;
      UsedRegs.Create;
      UsedRegs.Update(p);
      NewBlockStart := SkipHead(p);
      { done implicitely by the constructor
      FillChar(InstrSinceLastMod, SizeOf(InstrSinceLastMod), 0); }
      While (P <> BlockEnd) Do
        Begin
          CurProp:=TPaiProp.Create;
          If (p <> NewBlockStart) Then
            Begin
              GetLastInstruction(p, hp);
              CurProp.Regs := TPaiProp(hp.OptInfo).Regs;
    { !!!!!!!!!!!! }
    {$ifdef x86}
              CurProp.CondRegs.Flags :=
                TPaiProp(hp.OptInfo).CondRegs.Flags;
    {$endif}
            End;
          CurProp.UsedRegs.InitWithValue(UsedRegs.GetUsedRegs);
          UsedRegs.Update(tai(p.Next));
          TPaiProp(p.OptInfo) := CurProp;
          For TmpReg := LoGPReg To HiGPReg Do
            Inc(InstrSinceLastMod[TmpReg]);
          Case p^.typ Of
            ait_label:
              If (Pai_label(p)^.l^.is_used) Then
                CurProp^.DestroyAllRegs(InstrSinceLastMod);
            ait_stab, ait_force_line, ait_function_name:;
            ait_instruction:
              if not(PInstr(p)^.is_jmp) then
                begin
                  If IsLoadMemReg(p) Then
                    Begin
                      CurProp^.ReadRef(PInstr(p)^.oper[LoadSrc].ref);
                      TmpReg := RegMaxSize(PInstr(p)^.oper[LoadDst].reg);
                      If RegInRef(TmpReg, PInstr(p)^.oper[LoadSrc].ref^) And
                         (CurProp^.GetRegContentType(TmpReg) = Con_Ref) Then
                        Begin
                          { a load based on the value this register already }
                          { contained                                       }
                          With CurProp^.Regs[TmpReg] Do
                            Begin
                              CurProp^.IncWState(TmpReg);
                               {also store how many instructions are part of the  }
                               { sequence in the first instruction's PPaiProp, so }
                               { it can be easily accessed from within            }
                               { CheckSequence                                    }
                              Inc(NrOfMods, InstrSinceLastMod[TmpReg]);
                              PPaiProp(Pai(StartMod)^.OptInfo)^.Regs[TmpReg].NrOfMods := NrOfMods;
                              InstrSinceLastMod[TmpReg] := 0
                            End
                        End
                      Else
                        Begin
                          { load of a register with a completely new value }
                          CurProp^.DestroyReg(TmpReg, InstrSinceLastMod);
                          If Not(RegInRef(TmpReg, PInstr(p)^.oper[LoadSrc].ref^)) Then
                            With CurProp^.Regs[TmpReg] Do
                              Begin
                                Typ := Con_Ref;
                                StartMod := p;
                                NrOfMods := 1;
                              End
                        End;
      {$ifdef StateDebug}
                        hp := new(pai_asm_comment,init(strpnew(std_reg2str[TmpReg]+': '+tostr(CurProp^.Regs[TmpReg].WState))));
                        InsertLLItem(AsmL, p, p^.next, hp);
      {$endif StateDebug}

                    End
                  Else if IsLoadConstReg(p) Then
                    Begin
                      TmpReg := RegMaxSize(PInstr(p)^.oper[LoadDst].reg);
                      With CurProp^.Regs[TmpReg] Do
                        Begin
                          CurProp^.DestroyReg(TmpReg, InstrSinceLastMod);
                          typ := Con_Const;
                          StartMod := Pointer(PInstr(p)^.oper[LoadSrc].val);
                        End
                    End
                  Else CpuDFA(Pinstr(p));
                End;
            Else CurProp^.DestroyAllRegs(InstrSinceLastMod);
          End;
    {      Inc(InstrCnt);}
          GetNextInstruction(p, p);
        End;
    }
    End;

End.
