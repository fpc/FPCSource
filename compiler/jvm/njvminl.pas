{
    Copyright (c) 1998-2011 by Florian Klaempfl and Jonas Maebe

    Generate JVM inline nodes

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
unit njvminl;

{$i fpcdefs.inc}

interface

    uses
       cpubase,
       node,ninl,ncginl;

    type
       tjvminlinenode = class(tcginlinenode)
          { first pass override
            so that the code generator will actually generate
            these nodes.
          }
(*
          function first_sqrt_real: tnode; override;
          *)
          function first_sqr_real: tnode; override;
          function first_trunc_real: tnode; override;
(*
          function first_round_real: tnode; override;
          procedure second_sqrt_real; override;
          procedure second_abs_real; override;
*)
          procedure second_sqr_real; override;
          procedure second_trunc_real; override;
(*
          procedure second_round_real; override;
*)
       protected
          procedure load_fpu_location;
       end;

implementation

    uses
      cutils,globals,verbose,globtype,
      aasmtai,aasmdata,aasmcpu,
      symconst,symdef,
      defutil,
      cgbase,pass_2,
      cpuinfo,ncgutil,
      cgutils,hlcgobj,hlcgcpu;


{*****************************************************************************
                              tjvminlinenode
*****************************************************************************}
(*
    function tjvminlinenode.first_sqrt_real : tnode;
      begin
        if (current_settings.cputype >= cpu_PPC970) then
          begin
            expectloc:=LOC_FPUREGISTER;
            first_sqrt_real := nil;
          end
        else
          result:=inherited first_sqrt_real;
      end;
*)

     function tjvminlinenode.first_sqr_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_sqr_real:=nil;
      end;


     function tjvminlinenode.first_trunc_real : tnode;
      begin
        expectloc:=LOC_REGISTER;
        first_trunc_real:=nil;
      end;

(*
     function tjvminlinenode.first_round_real : tnode;
      begin
       if (current_settings.cputype >= cpu_PPC970) then
          begin
            expectloc:=LOC_REFERENCE;
            first_round_real := nil;
          end
        else
          result:=inherited first_round_real;
      end;
*)

     { load the FPU value on the evaluation stack }
     procedure tjvminlinenode.load_fpu_location;
       begin
         secondpass(left);
         thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
       end;

(*
    procedure tjvminlinenode.second_sqrt_real;
      begin
        if (current_settings.cputype < cpu_PPC970) then
          internalerror(2007020910);
        location.loc:=LOC_FPUREGISTER;
        load_fpu_location;
        case left.location.size of
          OS_F32:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRTS,location.register,
              left.location.register));
          OS_F64:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRT,location.register,
              left.location.register));
          else
            inherited;
        end;
      end;
*)

     procedure tjvminlinenode.second_sqr_real;
       begin
         load_fpu_location;
         location_reset(location,LOC_FPUREGISTER,location.size);
         location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);
         case left.location.size of
           OS_F32:
             begin
               current_asmdata.CurrAsmList.concat(taicpu.op_none(a_dup));
               thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,1);
               current_asmdata.CurrAsmList.concat(taicpu.op_none(a_fmul));
               thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
             end;
           OS_F64:
             begin
               current_asmdata.CurrAsmList.concat(taicpu.op_none(a_dup2));
               thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,2);
               current_asmdata.CurrAsmList.concat(taicpu.op_none(a_dmul));
               thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,2);
             end;
           else
             internalerror(2011010804);
         end;
         thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
       end;


    procedure tjvminlinenode.second_trunc_real;
      begin
         load_fpu_location;
         location_reset(location,LOC_REGISTER,left.location.size);
         location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
         case left.location.size of
           OS_F32:
             begin
               current_asmdata.CurrAsmList.concat(taicpu.op_none(a_f2l));
               { 32 bit float -> 64 bit int: +1 stack slot }
               thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,1);
             end;
           OS_F64:
             begin
               { 64 bit float -> 64 bit int: same number of stack slots }
               current_asmdata.CurrAsmList.concat(taicpu.op_none(a_d2l));
             end;
           else
             internalerror(2011010805);
         end;
         thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
      end;


begin
   cinlinenode:=tjvminlinenode;
end.
