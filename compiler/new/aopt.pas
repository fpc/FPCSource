{
    $Id$
    Copyright (c) 1999 by Jonas Maebe, member of the Free Pascal
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

Uses Aasm, cobjects, aoptmsc, aoptda, aoptcs, aoptpeep
{$ifdef i386}
   , ao386msc;
{$endif i386}


Type

  TAsmOptimizer = Object
{ the PAasmOutput list this optimizer instance works on                      }
    AsmL: PAasmOutput;

{ The labeltable contains the addresses of the Pai objects that are labels   }
    LabelInfo: PLabelInfo;

{ Start and end of the block that is currently being optimized, initialized  }
{ by InitDFA                                                                 }

    BlockStart, BlockEnd: Pai;

{ How many instructions are between the current instruction and the last one }
{ that modified the register                                                 }
    NrOfInstrSinceLastMod: TInstrSinceLastMod;


{ _AsmL is the PAasmOutpout list that has to be optimized                    }
    Constructor Init(_AsmL: PAasmOutput);

  {  general, processor independent procedures  }

  {  general, processor dependent procedures  }

Implementation

Constructor TAsmOptimizer.Init(_AsmL: PAasmOutput);
Begin
  AsmL := _AsmL;
End;

Procedure Optimize;
Var HP: Pai;
    AsmDFA: TAsmDFA;
Begin
{setup labeltable, always necessary}
  BlockStart := Pai(AsmL^.First);
  AsmDFA.Init(AsmL, BlockStart, BlockEnd);
{Blockend now either contains an ait_marker with Kind = AsmBlockStart, or nil}
  While Assigned(BlockStart) Do
    Begin
      LabelInfo := AsmDFA.GetLabelInfo;
{ peephole optimizations, twice }
      PeepHoleOptPass1;
      PeepHoleOptPass1;
      If (cs_slowoptimize in aktglobalswitches) Then
        Begin
{ data flow analyzer }
          DFAPass2;
{ common subexpression elimination }
          CSE;
        End;
{ more peephole optimizations }
      PeepHoleOptPass2;
{dispose labeltabel}
      AsmDFA.Done;
{continue where we left off, BlockEnd is either the start of an assembler
 block or nil}
      BlockStart := BlockEnd;
      While Assigned(BlockStart) And
            (BlockStart^.typ = ait_Marker) And
            (Pai_Marker(BlockStart)^.Kind = AsmBlockStart) Do
        Begin
         {we stopped at an assembler block, so skip it}
          While GetNextInstruction(BlockStart, BlockStart) And
                ((BlockStart^.Typ <> Ait_Marker) Or
                 (Pai_Marker(Blockstart)^.Kind <> AsmBlockEnd)) Do;
         {blockstart now contains a pai_marker(asmblockend)}
          If GetNextInstruction(BlockStart, HP) And
             ((HP^.typ <> ait_Marker) Or
              (Pai_Marker(HP)^.Kind <> AsmBlockStart)) Then
           {there is no assembler block anymore after the current one, so
            optimize the next block of "normal" instructions}
           AsmDFA.Init(AsmL, BlockStart, BlockEnd);
           {otherwise, skip the next assembler block}
          Else BlockStart := HP;
        End
   End;
End;

Destructor TAsmOptimizer.Done;
Begin
End;

{Virtual methods, most have to be overridden by processor dependent methods}

{
 $Log$
 Revision 1.1  1999-08-08 13:24:50  jonas
   + added copyright header/GNU license info
   * made the assembler optimizer almost completely OOP
   * some code style clean up and extra comments
   * moved from the new/aopt to the /new and /new/i386 dirs

}