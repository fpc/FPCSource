{
    Copyright (c) 2014 by the Free Pascal development team

    Generate m68k assembler for in memory related nodes

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
unit n68kmem;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      cgbase,cpuinfo,cpubase,
      node,nmem,ncgmem;

    type
       t68kvecnode = class(tcgvecnode)
          procedure update_reference_reg_mul(maybe_const_reg:tregister;l:aint);override;
          //procedure pass_generate_code;override;
       end;

implementation

    uses
      systems,globals,
      cutils,verbose,
      symdef,paramgr,
      aasmtai,aasmdata,
      nld,ncon,nadd,
      cgutils,cgobj;


{*****************************************************************************
                             T68KVECNODE
*****************************************************************************}

    { this routine must, like any other routine, not change the contents }
    { of base/index registers of references, as these may be regvars.    }
    { The register allocator can coalesce one LOC_REGISTER being moved   }
    { into another (as their live ranges won't overlap), but not a       }
    { LOC_CREGISTER moved into a LOC_(C)REGISTER most of the time (as    }
    { the live range of the LOC_CREGISTER will most likely overlap the   }
    { the live range of the target LOC_(C)REGISTER)                      }
    { The passed register may be a LOC_CREGISTER as well.                }
    procedure t68kvecnode.update_reference_reg_mul(maybe_const_reg:tregister;l:aint);
      var
        hreg: tregister;
        hreg2: tregister;
      begin
        if l<>1 then
          begin
            hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            { if we have a possibility, setup a scalefactor instead of the MUL }
            if (location.reference.base=NR_NO) or (location.reference.index<>NR_NO) or
               ((current_settings.cputype in cpu_coldfire) and not (l in [2,4])) or
               not (l in [2,4,8]) then
              cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_IMUL,OS_ADDR,l,maybe_const_reg,hreg)
            else
              begin
                cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,maybe_const_reg,hreg);
                location.reference.scalefactor:=l;
              end;
            { prefer an address reg, if we will be a base, otherwise for indexes
              a data register is better choice }
            if location.reference.base=NR_NO then
              begin
                hreg2:=cg.getaddressregister(current_asmdata.CurrAsmList);
                cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,hreg,hreg2);
                maybe_const_reg:=hreg2;
              end
            else
              maybe_const_reg:=hreg;
          end;
        if location.reference.base=NR_NO then
          location.reference.base:=maybe_const_reg
        else if location.reference.index=NR_NO then
          location.reference.index:=maybe_const_reg
        else
          begin
            hreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
            cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,location.reference,hreg);
            reference_reset_base(location.reference,hreg,0,location.reference.alignment);
            { insert new index register }
            location.reference.index:=maybe_const_reg;
          end;
          { update alignment }
          if (location.reference.alignment=0) then
            internalerror(2009020704);
          location.reference.alignment:=newalignment(location.reference.alignment,l);
      end;

    {procedure t68kvecnode.pass_generate_code;
      begin
        inherited pass_generate_code;
      end;}


begin
   cvecnode:=t68kvecnode;
end.
