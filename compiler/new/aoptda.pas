{
    $Id$
    Copyright (c) 1999 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the assembler optimizer data flow analyzer.

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

Interface

uses aasm, aoptcpub, aoptcpu;

Type TAsmDFA = Object(TAoptCpu)
       { uses the same constructor as TAoptCpu = constructor from TAoptObj }

       { gathers the information regarding the contents of every register }
       { at the end of every instruction                                  }
       Procedure TAsmOptimizer.DoDFA;

       { handles the processor dependent dataflow analizing               }
       Procedure CpuDFA(p: PInstr); Virtual;

      { How many instructions are between the current instruction and the }
      { last one that modified the register                               }
      NrOfInstrSinceLastMod: TInstrSinceLastMod;

     End;

Implementation

uses cpubase

Procedure TAsmOptimizer.DoDFAPass2;
{ Analyzes the Data Flow of an assembler list. Analyses the reg contents     }
{ for the instructions between blockstart and blockend. Returns the last pai }
{ which has been processed                                                   }
Var
    CurProp: PPaiProp;
    UsedRegs: TUsedRegs;
    p, hp, NewBlockStart : Pai;
    TmpReg: TRegister;
Begin
  p := BlockStart;
  UsedRegs.init;
  UsedRegs.Update(p);
  NewBlockStart := SkipHead(p);
{ done implicitely by the constructor
  FillChar(NrOfInstrSinceLastMod, SizeOf(NrOfInstrSinceLastMod), 0); }
  While (P <> BlockEnd) Do
    Begin
      CurProp := New(PPaiProp, init);
      If (p <> NewBlockStart) Then
        Begin
          GetLastInstruction(p, hp);
          CurProp^.Regs := PPaiProp(hp^.OptInfo)^.Regs;
          CurProp^.CondRegs.Flags :=
          PPaiProp(hp^.OptInfo)^.CondRegs.Flags;
        End;
      CurProp^.UsedRegs.InitWithValue(UsedRegs.GetUsedRegs);
      UsedRegs.Update(Pai(p^.Next)));
      PPaiProp(p^.OptInfo) := CurProp;
      For TmpReg := R_EAX To R_EDI Do
        Inc(NrOfInstrSinceLastMod[TmpReg]);
      Case p^.typ Of
        ait_label:
          If (Pai_label(p)^.l^.is_used) Then
            CurProp^.DestroyAllRegs(NrOfInstrSinceLastMod);
{$ifdef GDB}
        ait_stabs, ait_stabn, ait_stab_function_name:;
{$endif GDB}

        ait_instruction:
          if not(pai386(p)^.is_jmp) then
            begin
              If IsLoadMemReg(p) Then
                Begin
                  CurProp^.ReadRef(PInstr(p)^.oper[LoadSrc].ref);
                  TmpReg := RegMaxSize(PInstr(p)^.oper[LoadDst].reg);
                  If RegInRef(TmpReg, PInstr(p)^.oper[LoadSrc].ref^) And
                     (CurProp^.Regs[TmpReg].Typ = Con_Ref) Then
                    Begin
                      { a load based on the value this register already }
                      { contained                                       }
                      With CurProp^.Regs[TmpReg] Do
                        Begin
                          IncWState;
                           {also store how many instructions are part of the  }
                           { sequence in the first instruction's PPaiProp, so }
                           { it can be easily accessed from within            }
                           { CheckSequence                                    }
                          Inc(NrOfMods, NrOfInstrSinceLastMod[TmpReg]);
                          PPaiProp(Pai(StartMod)^.OptInfo)^.Regs[TmpReg].NrOfMods := NrOfMods;
                                    NrOfInstrSinceLastMod[TmpReg] := 0
                        End
                    End
                  Else
                    Begin
                      { load of a register with a completely new value }
                      CurProp^.DestroyReg(TmpReg, NrOfInstrSinceLastMod);
                      If Not(RegInRef(TmpReg, Pai386(p)^.oper[LoadSrc].ref^)) Then
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

                End
              Else if IsLoadConstReg(p) Then
                Begin
                  TmpReg := RegMaxSize(PInstr(p)^.oper[LoadDst].reg);
                  With CurProp^.Regs[TmpReg] Do
                    Begin
                      CurProp^.DestroyReg(TmpReg, NrOfInstrSinceLastMod);
                      typ := Con_Const;
                      StartMod := Pointer(PInstr(p)^.oper[LoadSrc].val);
                    End
                End
              Else CpuDFA(Pinstr(p));
            End;
        Else CurProp^.DestroyAllRegs(NrOfInstrSinceLastMod);
      End;
      Inc(InstrCnt);
      GetNextInstruction(p, p);
    End;
End;


End.