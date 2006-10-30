{
    Copyright (c) 1998-2002 by Jonas Maebe

    This unit calls the optimization procedures to optimize the assembler
    code for i386+

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
Unit aopt386;

{$i fpcdefs.inc}

Interface

Uses
  aasmbase,aasmtai,aasmdata,aasmcpu;

Procedure Optimize(AsmL: TAsmList);


Implementation

Uses
  globtype,
  globals,
  DAOpt386,POpt386,CSOpt386;


Procedure Optimize(AsmL: TAsmList);
Var
  BlockStart, BlockEnd, HP: Tai;
  pass: longint;
  slowopt, changed, lastLoop: boolean;
Begin
  slowopt := (cs_opt_level3 in current_settings.optimizerswitches);
  pass := 0;
  changed := false;
  dfa := TDFAObj.create(asml);
  repeat
     lastLoop :=
       not(slowopt) or
       (not changed and (pass > 2)) or
      { prevent endless loops }
       (pass = 4);
     changed := false;
   { Setup labeltable, always necessary }
     blockstart := tai(asml.first);
     blockend := dfa.pass_1(blockstart);
   { Blockend now either contains an ait_marker with Kind = mark_AsmBlockStart, }
   { or nil                                                                }
     While Assigned(BlockStart) Do
       Begin
         if pass = 0 then
           PrePeepHoleOpts(AsmL, BlockStart, BlockEnd);
        { Peephole optimizations }
         PeepHoleOptPass1(AsmL, BlockStart, BlockEnd);
        { Only perform them twice in the first pass }
         if pass = 0 then
           PeepHoleOptPass1(AsmL, BlockStart, BlockEnd);
        { Data flow analyzer }
         If (cs_opt_asmcse in current_settings.optimizerswitches) Then
           begin
             if dfa.pass_generate_code then
              { common subexpression elimination }
               changed := CSE(asmL, blockStart, blockEnd, pass) or changed;
           end;
        { More peephole optimizations }
         PeepHoleOptPass2(AsmL, BlockStart, BlockEnd);
         if lastLoop then
           PostPeepHoleOpts(AsmL, BlockStart, BlockEnd);

        { Free memory }
        dfa.clear;

        { Continue where we left off, BlockEnd is either the start of an }
        { assembler block or nil                                         }
         BlockStart := BlockEnd;
         While Assigned(BlockStart) And
               (BlockStart.typ = ait_Marker) And
               (Tai_Marker(BlockStart).Kind = mark_AsmBlockStart) Do
           Begin
           { We stopped at an assembler block, so skip it }
            Repeat
              BlockStart := Tai(BlockStart.Next);
            Until (BlockStart.Typ = Ait_Marker) And
                  (Tai_Marker(Blockstart).Kind = mark_AsmBlockEnd);
           { Blockstart now contains a Tai_marker(mark_AsmBlockEnd) }
             If GetNextInstruction(BlockStart, HP) And
                ((HP.typ <> ait_Marker) Or
                 (Tai_Marker(HP).Kind <> mark_AsmBlockStart)) Then
             { There is no assembler block anymore after the current one, so }
             { optimize the next block of "normal" instructions              }
               BlockEnd := dfa.pass_1(blockstart)
             { Otherwise, skip the next assembler block }
             else
               blockStart := hp;
           End;
       End;
     inc(pass);
  until lastLoop;
  dfa.free;

End;

End.
