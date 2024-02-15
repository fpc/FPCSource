{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate LoongArch64 inline nodes

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
unit ncpuinl;

{$i fpcdefs.inc}

interface

    uses
       cpubase,
       node,ninl,ncginl;

    type

       tloongarch64inlinenode = class(tcginlinenode)
          { first pass override
            so that the code generator will actually generate
            these nodes.
          }
          function first_sqrt_real: tnode; override;
          function first_abs_real: tnode; override;
          function first_sqr_real: tnode; override;
          function first_round_real: tnode; override;
          function first_trunc_real: tnode; override;

          procedure second_sqrt_real; override;
          procedure second_abs_real; override;
          procedure second_sqr_real; override;
          procedure second_round_real; override;
          procedure second_trunc_real; override;
       protected
          procedure load_fpu_location;
       end;

implementation

    uses
      ncal,
      cutils,globals,verbose,globtype,
      aasmtai,aasmdata,aasmcpu,
      symconst,symdef,
      defutil,
      procinfo,
      cgbase,pass_2,
      cpuinfo,ncgutil,
      hlcgobj,cgutils,cgobj,rgobj,tgobj;


{*****************************************************************************
                              tloongarch64inlinenode
*****************************************************************************}

     function tloongarch64inlinenode.first_sqrt_real : tnode;
       begin
         expectloc:=LOC_FPUREGISTER;
         first_sqrt_real := nil;
         if needs_check_for_fpu_exceptions then
           Include(current_procinfo.flags,pi_do_call);
       end;


     function tloongarch64inlinenode.first_abs_real : tnode;
       begin
         expectloc:=LOC_FPUREGISTER;
         first_abs_real := nil;
       end;


     function tloongarch64inlinenode.first_sqr_real : tnode;
       begin
         expectloc:=LOC_FPUREGISTER;
         first_sqr_real := nil;
         if needs_check_for_fpu_exceptions then
           Include(current_procinfo.flags,pi_do_call);
       end;


     function tloongarch64inlinenode.first_round_real: tnode;
       begin
         expectloc:=LOC_FPUREGISTER;
         first_round_real := nil;
         if needs_check_for_fpu_exceptions then
           Include(current_procinfo.flags,pi_do_call);
       end;


     function tloongarch64inlinenode.first_trunc_real: tnode;
       begin
         expectloc:=LOC_FPUREGISTER;
         first_trunc_real := nil;
         if needs_check_for_fpu_exceptions then
           Include(current_procinfo.flags,pi_do_call);
       end;


     { load the FPU into the an fpu register }
     procedure tloongarch64inlinenode.load_fpu_location;
       begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
         secondpass(left);
         hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
         location.loc := LOC_FPUREGISTER;
         location.register := cg.getfpuregister(current_asmdata.CurrAsmList,def_cgsize(resultdef));
       end;


     procedure tloongarch64inlinenode.second_sqrt_real;
       var
         op: TAsmOp;
       begin
         location.loc:=LOC_FPUREGISTER;
         load_fpu_location;
         if (left.location.size = OS_F32) then
           op := A_FSQRT_S
         else
           op := A_FSQRT_D;
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,location.register,left.location.register));
         cg.maybe_check_for_fpu_exception(current_asmdata.CurrAsmList);
       end;


     procedure tloongarch64inlinenode.second_abs_real;
       var
         op: TAsmOp;
       begin
         location.loc:=LOC_FPUREGISTER;
         load_fpu_location;
         if (left.location.size = OS_F32) then
           op := A_FABS_S
         else
           op := A_FABS_D;
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,location.register,left.location.register));
       end;


     procedure tloongarch64inlinenode.second_sqr_real;
       var
         op: tasmop;
       begin
         location.loc:=LOC_FPUREGISTER;
         load_fpu_location;
         if (left.location.size = OS_F32) then
           op := A_FMUL_S
         else
           op := A_FMUL_D;
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,location.register,left.location.register,left.location.register));
         cg.maybe_check_for_fpu_exception(current_asmdata.CurrAsmList);
       end;


     procedure tloongarch64inlinenode.second_round_real;
       var
         op: TAsmOp;
         hreg: tregister;
       begin
         secondpass(left);
         hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
         location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
         location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
         hreg:= cg.getfpuregister(current_asmdata.CurrAsmList, OS_F64);
         if (left.location.size = OS_F32) then
           op := A_FTINT_L_S
         else
           op := A_FTINT_L_D;
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,hreg,left.location.register));
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_MOVFR2GR_D,location.register,hreg));
         cg.maybe_check_for_fpu_exception(current_asmdata.CurrAsmList);
       end;


     procedure tloongarch64inlinenode.second_trunc_real;
       var
         op,movop: TAsmOp;
         hreg: tregister;
       begin
         secondpass(left);
         hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
         location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
         location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
         hreg:= cg.getfpuregister(current_asmdata.CurrAsmList, OS_F64);
         if (left.location.size = OS_F32) then
           op := A_FTINTRZ_L_S
         else
           op := A_FTINTRZ_L_D;
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,hreg,left.location.register));
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_MOVFR2GR_D,location.register,hreg));
         cg.maybe_check_for_fpu_exception(current_asmdata.CurrAsmList);
       end;


begin
   cinlinenode:=tloongarch64inlinenode;
end.
