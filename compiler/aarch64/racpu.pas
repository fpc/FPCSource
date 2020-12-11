{
    Copyright (c) 1998-2003 by Carl Eric Codere and Peter Vreman
    Copyright (c) 2014 by Jonas Maebe

    Handles the common AArch64 assembler reader routines

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
unit racpu;

{$i fpcdefs.inc}

  interface

    uses
      cgbase,
      cpubase,
      aasmtai,aasmdata,
      rautils;

    type
      TAArch64Operand=class(TOperand)
      end;

      TAArch64Instruction=class(TInstruction)
        oppostfix : toppostfix;
        function ConcatInstruction(p:TAsmList) : tai;override;
        function Is64bit: boolean;
        function cgsize: tcgsize;
      end;

  implementation

    uses
      verbose,
      aasmcpu;

    function TAArch64Instruction.ConcatInstruction(p:TAsmList) : tai;
      begin
        result:=inherited ConcatInstruction(p);
        taicpu(result).oppostfix:=oppostfix;
      end;


    function TAArch64Instruction.Is64bit: boolean;
      begin
        result:=
          (operands[1].opr.typ=OPR_REGISTER) and
          (getsubreg(operands[1].opr.reg)=R_SUBQ);
      end;

    function TAArch64Instruction.cgsize: tcgsize;
      begin
        if ops<1 then
          internalerror(2014122001);
        if (ops=1) and (operands[1].opr.typ=OPR_REFERENCE) then
          exit(OS_NO);
        if operands[1].opr.typ<>OPR_REGISTER then
          internalerror(2014122002);
        result:=reg_cgsize(operands[1].opr.reg);
        { a 32 bit integer register could actually be 16 or 8 bit }
        if result=OS_32 then
          case oppostfix of
            PF_B:
              result:=OS_8;
            PF_SB:
              result:=OS_S8;
            PF_H:
              result:=OS_16;
            PF_SH:
              result:=OS_S16;
          end;
      end;


end.
