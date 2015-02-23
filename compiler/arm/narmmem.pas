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
      symtype,
      cgbase,cpubase,nmem,ncgmem;

    type
      tarmloadparentfpnode = class(tcgloadparentfpnode)
        procedure pass_generate_code; override;
      end;


      tarmvecnode = class(tcgvecnode)
        procedure update_reference_reg_mul(maybe_const_reg: tregister; regsize: tdef; l: aint);override;
      end;

implementation

    uses
      cutils,verbose,globals,aasmdata,aasmcpu,cgobj,cgcpu,
      cpuinfo,
      cgutils,
      procinfo;

{*****************************************************************************
                        TARMLOADPARENTFPNODE
*****************************************************************************}

    procedure tarmloadparentfpnode.pass_generate_code;
      begin
        { normally, we cannot use the stack pointer as normal register on arm thumb }
        if (GenerateThumbCode) and
          (getsupreg(current_procinfo.framepointer) in [RS_R8..RS_R15]) and
          (current_procinfo.procdef.parast.symtablelevel=parentpd.parast.symtablelevel) then
          begin
            location_reset(location,LOC_REGISTER,OS_ADDR);
            location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
            cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,current_procinfo.framepointer,location.register);
          end
        else
          inherited pass_generate_code;
      end;

{*****************************************************************************
                             TARMVECNODE
*****************************************************************************}

     procedure tarmvecnode.update_reference_reg_mul(maybe_const_reg: tregister; regsize: tdef; l: aint);
       var
         hreg: tregister;
         hl : longint;
       begin
         if ((location.reference.base=NR_NO) and (location.reference.index=NR_NO)) or
            (GenerateThumbCode) or
            { simple constant? }
            (l=1) or ispowerof2(l,hl) or ispowerof2(l+1,hl) or ispowerof2(l-1,hl) then
           inherited update_reference_reg_mul(maybe_const_reg,regsize,l)
         else if (location.reference.base<>NR_NO) then
           begin
             hreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
             cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_ADDR,l,hreg);
             tbasecgarm(cg).safe_mla(current_asmdata.CurrAsmList,hreg,maybe_const_reg,hreg,location.reference.base);
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
             tbasecgarm(cg).safe_mla(current_asmdata.CurrAsmList,hreg,maybe_const_reg,hreg,location.reference.index);
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
  cloadparentfpnode:=tarmloadparentfpnode;
end.
