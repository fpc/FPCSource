{
    Copyright (c) 1998-2006 by Carl Eric Codere and Peter Vreman

    Does the parsing for the i8086 intel styled inline assembler.

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
Unit ra8086int;

{$i fpcdefs.inc}

  interface

    uses
      rax86int;

    type
      ti8086intreader = class(tx86intreader)
        // procedure handleopcode;override;
      end;


  implementation

    uses
      rabase,systems,rax86,aasmcpu;

(*
    procedure ti386intreader.handleopcode;
      var
        instr : Tx86Instruction;
      begin
        instr:=Tx86Instruction.Create(Tx86Operand);
        instr.OpOrder:=op_att;
        BuildOpcode(instr);
        instr.AddReferenceSizes;
        instr.SetInstructionOpsize;
        {
        instr.CheckOperandSizes;
        }
        instr.ConcatInstruction(curlist);
        instr.Free;
      end;
*)

{*****************************************************************************
                               Initialize
*****************************************************************************}

const
  asmmode_i8086_intel_info : tasmmodeinfo =
          (
            id    : asmmode_i8086_intel;
            idtxt : 'INTEL';
            casmreader : ti8086intreader;
          );

  asmmode_i8086_standard_info : tasmmodeinfo =
          (
            id    : asmmode_standard;
            idtxt : 'STANDARD';
            casmreader : ti8086intreader;
          );

begin
  RegisterAsmMode(asmmode_i8086_intel_info);
  RegisterAsmMode(asmmode_i8086_standard_info);
end.
