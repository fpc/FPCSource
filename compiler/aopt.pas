{
    Copyright (c) 1998-2004 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the interface routines between the code generator
    and the optimizer.

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
Unit aopt;

{$i fpcdefs.inc}

  Interface

    Uses
      aasmbase,aasmtai,aasmdata,aasmcpu,
      aoptobj;

    Type
      TAsmOptimizer = class(TAoptObj)

        { _AsmL is the PAasmOutpout list that has to be optimized }
        Constructor create(_AsmL: TAsmList); virtual; reintroduce;

        { call the necessary optimizer procedures }
        Procedure Optimize;
        Destructor destroy;override;

      private
        procedure FindLoHiLabels;
        Procedure BuildLabelTableAndFixRegAlloc;
        procedure clear;
        procedure pass_1;
      End;

    var
      casmoptimizer : class of tasmoptimizer;

    procedure Optimize(AsmL:TAsmList);

  Implementation

    uses
      globtype, globals,
      verbose,
      aoptda,aoptcpu,aoptcpud;

    Constructor TAsmOptimizer.create(_AsmL: TAsmList);
      Begin
        inherited create(_asml,nil,nil,nil);
      {setup labeltable, always necessary}
        New(LabelInfo);
      End;

    procedure TAsmOptimizer.FindLoHiLabels;
      { Walks through the paasmlist to find the lowest and highest label number.  }
      { Returns the last Pai object of the current block                          }
      Var LabelFound: Boolean;
          p: tai;
      Begin
        LabelInfo^.LowLabel := High(AWord);
        LabelInfo^.HighLabel := 0;
        LabelInfo^.LabelDif := 0;
        LabelInfo^.LabelTable:=nil;
        LabelFound := False;
        P := BlockStart;
        With LabelInfo^ Do
          Begin
            While Assigned(P) And
                  ((P.typ <> Ait_Marker) Or
                   (tai_Marker(P).Kind <> mark_AsmBlockStart)) Do
              Begin
                If (p.typ = ait_label) and
                   (tai_Label(p).labsym.labeltype=alt_jump) and
                   (tai_Label(p).labsym.is_used) Then
                  Begin
                    LabelFound := True;
                    If (tai_Label(p).labsym.labelnr < int64(LowLabel)) Then
                      LowLabel := tai_Label(p).labsym.labelnr;
                    If (tai_Label(p).labsym.labelnr > int64(HighLabel)) Then
                      HighLabel := tai_Label(p).labsym.labelnr
                  End;
                GetNextInstruction(p, p)
              End;
            blockend:=p;
            If LabelFound
              Then LabelDif := HighLabel-LowLabel+1
              Else LabelDif := 0
          End
      End;

    Procedure TAsmOptimizer.BuildLabelTableAndFixRegAlloc;
    { Builds a table with the locations of the labels in the TAsmList.       }
    { Also fixes some RegDeallocs like "# %eax released; push (%eax)"           }
    Var p{, hp1, hp2}: tai;
        {UsedRegs: TRegSet;}
        LabelIdx : longint;
    Begin
      {UsedRegs := [];}
      With LabelInfo^ Do
        If (LabelDif <> 0) Then
          Begin
            GetMem(LabelTable, LabelDif*SizeOf(TLabelTableItem));
            FillChar(LabelTable^, LabelDif*SizeOf(TLabelTableItem), 0);
            p := BlockStart;
            While (P <> BlockEnd) Do
              Begin
                Case p.typ Of
                  ait_Label:
                    begin
                      If tai_label(p).labsym.is_used and
                         (tai_Label(p).labsym.labeltype=alt_jump) then
                        begin
                          LabelIdx:=tai_label(p).labsym.labelnr-LowLabel;
                          if LabelIdx>int64(LabelDif) then
                            internalerror(200604202);
                          LabelTable^[LabelIdx].PaiObj := p;
                        end;
                    end;
                  ait_regAlloc:
                    begin
                    {!!!!!!!!!
                      if tai_regalloc(p).ratype=ra_alloc then
                        Begin
                          If Not(tai_regalloc(p).Reg in UsedRegs) Then
                            UsedRegs := UsedRegs + [tai_regalloc(p).Reg]
                          Else
                            Begin
                              hp1 := p;
                              hp2 := nil;
                              While GetLastInstruction(hp1, hp1) And
                                    Not(RegInInstruction(tai_regalloc(p).Reg, hp1)) Do
                                hp2:=hp1;
                              If hp2<>nil Then
                                Begin
                                  hp1:=tai_regalloc.DeAlloc(tai_regalloc(p).Reg,hp2);
                                  InsertLLItem(tai(hp2.previous), hp2, hp1);
                                End;
                            End;
                        End
                      else
                        Begin
                          UsedRegs := UsedRegs - [tai_regalloc(p).Reg];
                          hp1 := p;
                          hp2 := nil;
                          While Not(FindRegAlloc(tai_regalloc(p).Reg, tai(hp1.Next))) And
                                GetNextInstruction(hp1, hp1) And
                                RegInInstruction(tai_regalloc(p).Reg, hp1) Do
                            hp2 := hp1;
                          If hp2 <> nil Then
                            Begin
                              hp1 := tai(p.previous);
                              AsmL.Remove(p);
                              InsertLLItem(hp2, tai(hp2.Next), p);
                              p := hp1;
                            End
                        End
                    };
                    End
                End;
                P := tai(p.Next);
                While Assigned(p) and
                      (p <> blockend) and
                      (p.typ in (SkipInstr - [ait_regalloc])) Do
                  P := tai(P.Next)
              End;
          End
    End;

    procedure tasmoptimizer.clear;
      begin
        if assigned(LabelInfo^.labeltable) then
          begin
            freemem(LabelInfo^.labeltable);
            LabelInfo^.labeltable := nil;
          end;
        LabelInfo^.labeldif:=0;
        LabelInfo^.lowlabel:=high(AWord);
        LabelInfo^.highlabel:=0;
      end;

    procedure tasmoptimizer.pass_1;
      begin
        findlohilabels;
        BuildLabelTableAndFixRegAlloc;
      end;


    Procedure TAsmOptimizer.Optimize;
      Var
        HP: tai;
        pass: longint;
      Begin
        pass:=0;
        BlockStart := tai(AsmL.First);
        pass_1;
        While Assigned(BlockStart) Do
          Begin
             if pass = 0 then
               PrePeepHoleOpts;
            { Peephole optimizations }
             PeepHoleOptPass1;
            { Only perform them twice in the first pass }
             if pass = 0 then
               PeepHoleOptPass1;
            If (cs_opt_asmcse in current_settings.optimizerswitches) Then
              Begin
//                DFA:=TAOptDFACpu.Create(AsmL,BlockStart,BlockEnd,LabelInfo);
                { data flow analyzer }
//                DFA.DoDFA;
                { common subexpression elimination }
      {          CSE;}
              End;
            { more peephole optimizations }
            PeepHoleOptPass2;
            { if pass = last_pass then }
            PostPeepHoleOpts;
            { free memory }
            clear;
            { continue where we left off, BlockEnd is either the start of an }
            { assembler block or nil}
            BlockStart := BlockEnd;
            While Assigned(BlockStart) And
                  (BlockStart.typ = ait_Marker) And
                  (tai_Marker(BlockStart).Kind = mark_AsmBlockStart) Do
              Begin
               { we stopped at an assembler block, so skip it    }
               While GetNextInstruction(BlockStart, BlockStart) And
                     ((BlockStart.Typ <> Ait_Marker) Or
                      (tai_Marker(Blockstart).Kind <> mark_AsmBlockEnd)) Do;
               { blockstart now contains a tai_marker(mark_AsmBlockEnd) }
               If GetNextInstruction(BlockStart, HP) And
                  ((HP.typ <> ait_Marker) Or
                   (Tai_Marker(HP).Kind <> mark_AsmBlockStart)) Then
               { There is no assembler block anymore after the current one, so }
               { optimize the next block of "normal" instructions              }
                 pass_1
               { Otherwise, skip the next assembler block }
               else
                 blockStart := hp;
              End
          End;
      End;

    Destructor TAsmOptimizer.Destroy;
      Begin
        Dispose(LabelInfo)
      End;


    procedure Optimize(AsmL:TAsmList);
      var
        p : TAsmOptimizer;
      begin
        p:=casmoptimizer.Create(AsmL);
        p.Optimize;
        p.free
      end;


end.
