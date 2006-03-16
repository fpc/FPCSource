{
    Copyright (c) 1998-2002 by Carl Eric Codere and Peter Vreman

    Does the parsing for the i386 GNU AS styled inline assembler.

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
Unit rax64att;

{$i fpcdefs.inc}

  interface

    uses
      rax86att;

    type
      tx8664attreader = class(tx86attreader)
        procedure handleopcode;override;
      end;


  implementation

    uses
      rabase,systems,rax86,aasmcpu;

    procedure tx8664attreader.handleopcode;
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


const
  asmmode_x86_64_gas_info : tasmmodeinfo =
          (
            id    : asmmode_x86_64_gas;
            idtxt : 'GAS';
            casmreader : tx8664attreader;
          );

  asmmode_x86_64_standard_info : tasmmodeinfo =
          (
            id    : asmmode_standard;
            idtxt : 'STANDARD';
            casmreader : tx8664attreader;
          );

initialization
  RegisterAsmMode(asmmode_x86_64_gas_info);
  RegisterAsmMode(asmmode_x86_64_standard_info);
end.
