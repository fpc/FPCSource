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