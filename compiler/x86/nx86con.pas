{
    Copyright (c) 1998-2012 by Florian Klaempfl and others

    Generate i386 assembler for constants

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
unit nx86con;

{$i fpcdefs.inc}

interface

    uses
       node,ncon,ncgcon;

    type
       tx86realconstnode = class(tcgrealconstnode)
          function pass_1 : tnode;override;
          procedure pass_generate_code;override;
       end;

implementation

    uses
      systems,globals,globtype,
      symdef,
      defutil,
      cpubase,
      aasmdata,
      cga,cgx86,cgobj,cgbase,cgutils;

{*****************************************************************************
                           TX86REALCONSTNODE
*****************************************************************************}

    function tx86realconstnode.pass_1 : tnode;
      begin
         result:=nil;
         if is_number_float(value_real) and not(use_vectorfpu(resultdef)) and ((value_real=1.0) or (value_real=-1.0) or ((value_real=0.0) and (get_real_sign(value_real)=1)) or
           ((value_real=2.0) and (cs_create_pic in current_settings.moduleswitches))) then
           expectloc:=LOC_FPUREGISTER
         else if (value_real=0.0) and (get_real_sign(value_real)=1) and use_vectorfpu(resultdef) then
           expectloc:=LOC_MMREGISTER
         else
           expectloc:=LOC_CREFERENCE;
      end;


    procedure tx86realconstnode.pass_generate_code;
      begin
         if is_number_float(value_real) then
           begin
             if (value_real=1.0) and not(use_vectorfpu(resultdef)) then
               begin
                  emit_none(A_FLD1,S_NO);
                  location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
                  location.register:=NR_ST;
                  tcgx86(cg).inc_fpu_stack;
               end
             else if (value_real=2.0) and (cs_create_pic in current_settings.moduleswitches) and not(use_vectorfpu(resultdef)) then
               begin
                  emit_none(A_FLD1,S_NO);
                  emit_reg_reg(A_FADD,S_NO,NR_ST,NR_ST);
                  location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
                  location.register:=NR_ST;
                  tcgx86(cg).inc_fpu_stack;
               end
             else if (value_real=-1.0) and not(use_vectorfpu(resultdef)) then
               begin
                  emit_none(A_FLD1,S_NO);
                  emit_none(A_FCHS,S_NO);
                  location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
                  location.register:=NR_ST;
                  tcgx86(cg).inc_fpu_stack;
               end
             else if (value_real=0.0) and (get_real_sign(value_real)=1) then
               begin
                 if use_vectorfpu(resultdef) then
                   begin
                     location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
                     location.register:=cg.getmmregister(current_asmdata.CurrAsmList,def_cgsize(resultdef));
                     if UseAVX then
                       begin
                         if is_single(resultdef) then
                           emit_reg_reg_reg(A_VXORPS,S_NO,location.register,location.register,location.register)
                         else
                           emit_reg_reg_reg(A_VXORPD,S_NO,location.register,location.register,location.register);
                       end
                     else
                       begin
                         if is_single(resultdef) then
                           emit_reg_reg(A_XORPS,S_NO,location.register,location.register)
                         else
                           emit_reg_reg(A_XORPD,S_NO,location.register,location.register);
                       end
                   end
                 else
                   begin
                      emit_none(A_FLDZ,S_NO);
                      if (get_real_sign(value_real) < 0) then
                        emit_none(A_FCHS,S_NO);
                      location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
                      location.register:=NR_ST;
                      tcgx86(cg).inc_fpu_stack;
                   end;
               end
            else
              inherited pass_generate_code;
           end
         else
           inherited pass_generate_code;
      end;


begin
   crealconstnode:=tx86realconstnode;
end.
