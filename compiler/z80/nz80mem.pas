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
      node,nmem,ncgmem;

    type

       { tz80loadparentfpnode }

       tz80loadparentfpnode = class(tcgloadparentfpnode)
         procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
       end;

implementation

    uses
      aasmdata,aasmcpu,
      cgobj,
      pass_2_context,
      nodehelper;


{*****************************************************************************
                            TZ80LOADPARENTFPNODE
*****************************************************************************}

      procedure tz80loadparentfpnode.pass_generate_code(ctx:tpassgeneratecodecontext);
        begin
          inherited;
          if (location.loc=LOC_REGISTER) and ((location.register=NR_IX) or (location.register=NR_IY)) then
            begin
              ctx.cg.getcpuregister(ctx.CurrAsmList,NR_H);
              ctx.cg.getcpuregister(ctx.CurrAsmList,NR_L);
              ctx.CurrAsmList.Concat(taicpu.op_reg(A_PUSH,location.register));
              ctx.CurrAsmList.Concat(taicpu.op_reg(A_POP,NR_HL));
              location.register:=ctx.cg.getintregister(ctx.CurrAsmList,OS_16);
              ctx.cg.a_load_reg_reg(ctx.CurrAsmList,OS_8,OS_8,NR_L,location.register);
              ctx.cg.ungetcpuregister(ctx.CurrAsmList,NR_L);
              ctx.cg.a_load_reg_reg(ctx.CurrAsmList,OS_8,OS_8,NR_H,ctx.cg.GetNextReg(location.register));
              ctx.cg.ungetcpuregister(ctx.CurrAsmList,NR_H);
            end;
        end;


begin
  cloadparentfpnode:=tz80loadparentfpnode;
end.
