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
        scaled: boolean;
      begin
        scaled:=false;
        //current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('updref: called')));
        if l<>1 then
          begin
            //current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('updref: l <> 1')));
            hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            { if we have a possibility, setup a scalefactor instead of the MUL }
            if (location.reference.index<>NR_NO) or
               (current_settings.cputype in [cpu_mc68000]) or
               ((current_settings.cputype in cpu_coldfire) and not (l in [2,4])) or
               not (l in [2,4,8]) then
              begin
                //current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('updref: mul')));
                cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_IMUL,OS_ADDR,l,maybe_const_reg,hreg);
              end
            else
              begin
                //current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('updref: scale')));
                cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,maybe_const_reg,hreg);
                scaled:=true;
              end;
            maybe_const_reg:=hreg;
          end;

        if (location.reference.base=NR_NO) and not (scaled) then
          begin
           { prefer an address reg, if we will be a base, for indexes any register works }
            if isintregister(maybe_const_reg) then
              begin
                //current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('updref: copytoa')));
                hreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
                cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,maybe_const_reg,hreg);
                maybe_const_reg:=hreg;
              end;
            location.reference.base:=maybe_const_reg;
          end
        else if location.reference.index=NR_NO then
          begin
            location.reference.index:=maybe_const_reg;
            if (scaled) then
              location.reference.scalefactor:=l;
          end
        else
          begin
            hreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
            cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,location.reference,hreg);
            reference_reset_base(location.reference,hreg,0,location.reference.alignment);
            { insert new index register }
            location.reference.index:=maybe_const_reg;
            if (scaled) then
              location.reference.scalefactor:=l;
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
