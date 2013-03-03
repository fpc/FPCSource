{
    Copyright (c) 2012 by Florian Klaempfl

    Generate arm assembler for in memory related nodes

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
unit narmmem;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      cgbase,cpubase,nmem,ncgmem;

    type
      tarmvecnode = class(tcgvecnode)
        procedure update_reference_reg_mul(maybe_const_reg: tregister; l: aint);override;
      end;

implementation

    uses
      cutils,verbose,globals,aasmdata,aasmcpu,cgobj,
      cpuinfo;

{*****************************************************************************
                             TARMVECNODE
*****************************************************************************}

     procedure tarmvecnode.update_reference_reg_mul(maybe_const_reg:tregister;l:aint);
       var
         hreg: tregister;
         hl : longint;
       begin
         if ((location.reference.base=NR_NO) and (location.reference.index=NR_NO)) or
            (current_settings.cputype in cpu_thumb) or
            { simple constant? }
            (l=1) or ispowerof2(l,hl) or ispowerof2(l+1,hl) or ispowerof2(l-1,hl) then
           inherited update_reference_reg_mul(maybe_const_reg,l)
         else if (location.reference.base<>NR_NO) then
           begin
             hreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
             cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_ADDR,l,hreg);
             current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg_reg(A_MLA,hreg,maybe_const_reg,hreg,location.reference.base));
             location.reference.base:=hreg;
             { update alignment }
             if (location.reference.alignment=0) then
               internalerror(2012052202);
             location.reference.alignment:=newalignment(location.reference.alignment,l);
           end
         else if (location.reference.index<>NR_NO) then
           begin
             hreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
             cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_ADDR,l,hreg);
             current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg_reg(A_MLA,hreg,maybe_const_reg,hreg,location.reference.index));
             location.reference.base:=hreg;
             location.reference.index:=NR_NO;
             { update alignment }
             if (location.reference.alignment=0) then
               internalerror(2012052203);
             location.reference.alignment:=newalignment(location.reference.alignment,l);
           end
         else
           internalerror(2012052201);
       end;


begin
  cvecnode:=tarmvecnode;
end.
