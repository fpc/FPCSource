{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe

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

{$i defines.inc}

Interface

Uses
  aasm;

Procedure Optimize(AsmL: PAasmOutput);


Implementation

Uses
  globtype,
  globals,
  DAOpt386,POpt386,CSOpt386;


Procedure Optimize(AsmL: PAasmOutput);
Var
  count, max: longint;
  BlockStart, BlockEnd, HP: Pai;
Begin
  if (cs_slowoptimize in aktglobalswitches) then
   { Optimize twice }
    max := 2
  else max := 1;
  for count := 1 to max do
    begin
   { Setup labeltable, always necessary }
     BlockStart := Pai(AsmL^.First);
     BlockEnd := DFAPass1(AsmL, BlockStart);
   { Blockend now either contains an ait_marker with Kind = AsmBlockStart, }
   { or nil                                                                }
     While Assigned(BlockStart) Do
       Begin
        { Peephole optimizations }
         PeepHoleOptPass1(AsmL, BlockStart, BlockEnd);
        { Only perform them twice in the first pass }
         if count = 1 then
           PeepHoleOptPass1(AsmL, BlockStart, BlockEnd);
        { Data flow analyzer }
         If (cs_slowoptimize in aktglobalswitches) Then
           Begin
             If DFAPass2(
{$ifdef statedebug}
                         AsmL,
{$endif statedebug}
                               BlockStart, BlockEnd) Then
              { common subexpression elimination }
               CSE(AsmL, BlockStart, BlockEnd);
           End;
        { More peephole optimizations }
         PeepHoleOptPass2(AsmL, BlockStart, BlockEnd);
        { Dispose labeltabel }
         ShutDownDFA;
        { Continue where we left off, BlockEnd is either the start of an }
        { assembler block or nil                                         }
         BlockStart := BlockEnd;
         While Assigned(BlockStart) And
               (BlockStart^.typ = ait_Marker) And
               (Pai_Marker(BlockStart)^.Kind = AsmBlockStart) Do
           Begin
           { We stopped at an assembler block, so skip it }
            Repeat
              BlockStart := Pai(BlockStart^.Next);
            Until (BlockStart^.Typ = Ait_Marker) And
                  (Pai_Marker(Blockstart)^.Kind = AsmBlockEnd);
           { Blockstart now contains a pai_marker(asmblockend) }
             If GetNextInstruction(BlockStart, HP) And
                ((HP^.typ <> ait_Marker) Or
                 (Pai_Marker(HP)^.Kind <> AsmBlockStart)) Then
             { There is no assembler block anymore after the current one, so }
             { optimize the next block of "normal" instructions              }
               BlockEnd := DFAPass1(AsmL, BlockStart)
             { Otherwise, skip the next assembler block }
             Else BlockStart := HP;
           End
      End;
   end;
End;

End.
{
  $Log$
  Revision 1.1  2000-10-15 09:47:42  peter
    * moved to i386/

  Revision 1.5  2000/09/24 15:06:11  peter
    * use defines.inc

  Revision 1.4  2000/08/19 09:10:08  jonas
    * for all optimization levels > 1, all passes are done twice (the
      result improves the most if -Or is used as well)

  Revision 1.3  2000/07/14 05:11:48  michael
  + Patch to 1.1

  Revision 1.2  2000/07/13 11:32:31  michael
  + removed logs

}
