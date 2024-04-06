{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate MOS Technology 6502 assembler for in call nodes

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
unit nmos6502cal;

{$i fpcdefs.inc}

interface

    uses
      ncgcal;

    type
       TMOS6502CallNode = class(tcgcallnode)
       protected
          procedure pop_parasize(pop_size:longint);override;
       end;


implementation

    uses
      cpubase,
      aasmdata,aasmcpu,
      ncal,
      cgobj;


{*****************************************************************************
                             TMOS6502CallNode
*****************************************************************************}


    procedure TMOS6502CallNode.pop_parasize(pop_size:longint);
      begin
        //if pop_size>=2 then
        //  begin
        //    cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
        //    while pop_size>=2 do
        //      begin
        //        current_asmdata.CurrAsmList.Concat(taicpu.op_reg(A_POP,NR_AF));
        //        dec(pop_size,2);
        //      end;
        //    cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
        //  end;
        //if pop_size=1 then
        //  current_asmdata.CurrAsmList.Concat(taicpu.op_reg(A_INC,NR_SP));
      end;


begin
   ccallnode:=TMOS6502CallNode;
end.
