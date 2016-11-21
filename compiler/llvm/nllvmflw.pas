{
    Copyright (c) 2016 by Jonas Maebe

    Generate assembler for nodes that influence the flow for llvm

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
unit nllvmflw;

{$i fpcdefs.inc}

interface

    uses
      aasmbase,
      nflw, ncgflw, ncgnstfl;

    type
      tllvmlabelnode = class(tcglabelnode)
        function getasmlabel: tasmlabel; override;
      end;


implementation

{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    uses
      aasmdata;


    function tllvmlabelnode.getasmlabel: tasmlabel;
      begin
        { don't allocate global labels even if the label is accessed from
          another routine: we always have to refer to such labels using the
          blockaddress() construct, which works with local labels too.
          Additionally, LLVM does not support defining global labels in the
          middle of a routine -> jumping to such a label from assembler code
          from another function will not work anyway (have to handle that by
          passing a blockaddress as argument to an assembler block, although
          "some targets may provide defined semantics when using the value as
          the operand to an inline assembly") }
        if not(assigned(asmlabel)) then
          current_asmdata.getjumplabel(asmlabel);
        result:=asmlabel
      end;

begin
  clabelnode:=tllvmlabelnode;
end.

