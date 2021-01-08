{
    Copyright (c) 1998-2020 by Florian Klaempfl

    Generate xtensa assembler for in memory related nodes

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
unit ncpumem;

{$i fpcdefs.inc}

interface
    uses
      globtype,
      cgbase,cpubase,
      symtype,
      nmem,ncgmem;

    type
      tcpuvecnode = class(tcgvecnode)
        procedure update_reference_reg_mul(maybe_const_reg: tregister; regsize: tdef; l: aint);override;
      end;

implementation

    uses
      cutils,verbose,
      aasmdata,aasmcpu,
      cgutils,cgobj,
      symconst,symcpu;

{*****************************************************************************
                             TCPUVECNODE
*****************************************************************************}

     procedure tcpuvecnode.update_reference_reg_mul(maybe_const_reg: tregister; regsize: tdef; l: aint);
       var
         hreg: tregister;
         op: TAsmOp;
       begin
         if (l in [2,4,8]) and ((location.reference.base<>NR_NO) or (location.reference.index<>NR_NO)) then
           begin
             case l of
               2 : op:=A_ADDX2;
               4 : op:=A_ADDX4;
               8 : op:=A_ADDX8;
               else
                 Internalerror(2020042201);
             end;
             hreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
             if location.reference.base<>NR_NO then
               begin
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,hreg,maybe_const_reg,location.reference.base));
                 location.reference.base:=hreg;
               end
             else if location.reference.index<>NR_NO then
               begin
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,hreg,maybe_const_reg,location.reference.index));
                 location.reference.index:=hreg;
               end
             else
               Internalerror(2020042214);
             { update alignment }
             if (location.reference.alignment=0) then
               internalerror(2020042217);
             location.reference.alignment:=newalignment(location.reference.alignment,l);
           end
         else
           inherited update_reference_reg_mul(maybe_const_reg,regsize,l);
       end;

begin
  cvecnode:=tcpuvecnode;
end.

