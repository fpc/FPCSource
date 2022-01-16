{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate Z80 assembler for in memory related nodes

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
unit nz80mem;

{$i fpcdefs.inc}

interface

    uses
      cgbase,cpubase,
      nmem,ncgmem;

    type

       { tz80loadparentfpnode }

       tz80loadparentfpnode = class(tcgloadparentfpnode)
         procedure pass_generate_code;override;
       end;

implementation

    uses
      aasmdata,aasmcpu,
      cgobj;


{*****************************************************************************
                            TZ80LOADPARENTFPNODE
*****************************************************************************}

      procedure tz80loadparentfpnode.pass_generate_code;
        begin
          inherited pass_generate_code;
          if (location.loc=LOC_REGISTER) and ((location.register=NR_IX) or (location.register=NR_IY)) then
            begin
              cg.getcpuregister(current_asmdata.CurrAsmList,NR_H);
              cg.getcpuregister(current_asmdata.CurrAsmList,NR_L);
              current_asmdata.CurrAsmList.Concat(taicpu.op_reg(A_PUSH,location.register));
              current_asmdata.CurrAsmList.Concat(taicpu.op_reg(A_POP,NR_HL));
              location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
              cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_8,OS_8,NR_L,location.register);
              cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_L);
              cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_8,OS_8,NR_H,cg.GetNextReg(location.register));
              cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_H);
            end;
        end;


begin
  cloadparentfpnode:=tz80loadparentfpnode;
end.
