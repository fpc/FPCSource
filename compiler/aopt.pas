{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
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

Interface

Uses Aasm, cobjects, aoptobj, aoptcpud, aoptcpub {aoptcs, aoptpeep} ;

Type
  PAsmOptimizer = ^TAsmOptimizer;
  TAsmOptimizer = Object(TAoptObj)

    { _AsmL is the PAasmOutpout list that has to be optimized }
    Constructor Init(_AsmL: PAasmOutput);

    { call the necessary optimizer procedures }
    Procedure Optimize;
    Destructor Done;

    private

    Function FindLoHiLabels: Pai;
    Procedure BuildLabelTableAndFixRegAlloc;

  End;

procedure Optimize(AsmL:Paasmoutput);


Implementation

uses cpuinfo, globtype, globals, tainst;

Constructor TAsmOptimizer.Init(_AsmL: PAasmOutput);
Begin
  AsmL := _AsmL;
{setup labeltable, always necessary}
  New(LabelInfo);
  LabelInfo^.LowLabel := High(AWord);
  LabelInfo^.HighLabel := 0;
  LabelInfo^.LabelDif := 0;
End;

Function TAsmOptimizer.FindLoHiLabels: Pai;
{ Walks through the paasmlist to find the lowest and highest label number.  }
{ Returns the last Pai object of the current block                          }
Var LabelFound: Boolean;
    P: Pai;
Begin
  LabelFound := False;
  P := BlockStart;
  With LabelInfo^ Do
    Begin
      While Assigned(P) And
            ((P^.typ <> Ait_Marker) Or
             (Pai_Marker(P)^.Kind <> AsmBlockStart)) Do
        Begin
          If (Pai(p)^.typ = ait_label) Then
            If (Pai_Label(p)^.l^.is_used) Then
              Begin
                LabelFound := True;
                If (Pai_Label(p)^.l^.labelnr < LowLabel) Then
                  LowLabel := Pai_Label(p)^.l^.labelnr;
                If (Pai_Label(p)^.l^.labelnr > HighLabel) Then
                  HighLabel := Pai_Label(p)^.l^.labelnr
              End;
          GetNextInstruction(p, p)
        End;
      FindLoHiLabels := p;
      If LabelFound
        Then LabelDif := HighLabel-LowLabel+1
        Else LabelDif := 0
    End
End;

Procedure TAsmOptimizer.BuildLabelTableAndFixRegAlloc;
{ Builds a table with the locations of the labels in the paasmoutput.       }
{ Also fixes some RegDeallocs like "# %eax released; push (%eax)"           }
Var p, hp1, hp2: Pai;
    UsedRegs: TRegSet;
Begin
  UsedRegs := [];
  With LabelInfo^ Do
    If (LabelDif <> 0) Then
      Begin
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
                              InsertLLItem(Pai(hp2^.previous), hp2, hp1);
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
                          InsertLLItem(hp2, Pai(hp2^.Next), p);
                          p := hp1;
                        End
                    End
                End
            End
          End;
        P := Pai(p^.Next);
        While Assigned(p) And
              (p^.typ in (SkipInstr - [ait_regalloc])) Do
          P := Pai(P^.Next)
      End
End;



Procedure TAsmOptimizer.Optimize;
Var HP: Pai;
    DFA: PAOptDFACpu;
Begin
  BlockStart := Pai(AsmL^.First);
  While Assigned(BlockStart) Do
    Begin
      { Initialize BlockEnd and the LabelInfo (low and high label) }
      BlockEnd := FindLoHiLabels;
      { initialize the LabelInfo (labeltable) and fix the regalloc info }
      BuildLabelTableAndFixRegAlloc;
      { peephole optimizations, twice because you can't do them all in one }
      { pass                                                               }
{      PeepHoleOptPass1;
      PeepHoleOptPass1;}
      If (cs_slowoptimize in aktglobalswitches) Then
        Begin
          New(DFA,Init(AsmL,BlockStart,BlockEnd,LabelInfo));
          { data flow analyzer }
          DFA^.DoDFA;
          { common subexpression elimination }
{          CSE;}
        End;
      { more peephole optimizations }
{      PeepHoleOptPass2;}
      {dispose labeltabel}
      If Assigned(LabelInfo^.LabelTable) Then
        Begin
          Dispose(LabelInfo^.LabelTable);
          LabelInfo := Nil
        End;
      { continue where we left off, BlockEnd is either the start of an }
      { assembler block or nil}
      BlockStart := BlockEnd;
      While Assigned(BlockStart) And
            (BlockStart^.typ = ait_Marker) And
            (Pai_Marker(BlockStart)^.Kind = AsmBlockStart) Do
        Begin
         { we stopped at an assembler block, so skip it }
          While GetNextInstruction(BlockStart, BlockStart) And
                ((BlockStart^.Typ <> Ait_Marker) Or
                 (Pai_Marker(Blockstart)^.Kind <> AsmBlockEnd)) Do;
         { blockstart now contains a pai_marker(asmblockend) }
          If Not(GetNextInstruction(BlockStart, HP) And
                 ((HP^.typ <> ait_Marker) Or
                  (Pai_Marker(HP)^.Kind <> AsmBlockStart)
                 )
                ) Then
           {skip the next assembler block }
           BlockStart := HP;
         { otherwise there is no assembler block anymore after the current }
         { one, so optimize the next block of "normal" instructions        }
        End
    End;
End;

Destructor TAsmOptimizer.Done;
Begin
  Dispose(LabelInfo)
End;


procedure Optimize(AsmL:Paasmoutput);
var
  p : PAsmOptimizer;
begin
  new(p,Init(AsmL));
  p^.Optimize;
  dispose(p,Done);
end;


End.

{Virtual methods, most have to be overridden by processor dependent methods}

{
 $Log$
 Revision 1.3  2002-05-16 19:46:34  carl
 + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
 + try to fix temp allocation (still in ifdef)
 + generic constructor calls
 + start of tassembler / tmodulebase class cleanup

 Revision 1.1  2001/08/26 13:36:35  florian
   * some cg reorganisation
   * some PPC updates

 Revision 1.1  2000/07/13 06:30:07  michael
 + Initial import

 Revision 1.5  2000/01/07 01:14:51  peter
   * updated copyright to 2000

 Revision 1.4  1999/11/09 22:57:08  peter
   * compiles again both i386,alpha both with optimizer

 Revision 1.3  1999/08/18 14:32:20  jonas
   + compilable!
   + dataflow analyzer finished
   + start of CSE units
   + aoptbase which contains a base object for all optimizer objects
   * some constants and type definitions moved around to avoid circular
     dependencies
   * moved some methods from base objects to specialized objects because
     they're not used anywhere else

 Revision 1.2  1999/08/09 14:07:22  jonas
 commit.msg

 Revision 1.1  1999/08/08 13:24:50  jonas
   + added copyright header/GNU license info
   * made the assembler optimizer almost completely OOP
   * some code style clean up and extra comments
   * moved from the new/aopt to the /new and /new/i386 dirs

}