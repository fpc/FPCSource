{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate PowerPC32/64 inline nodes

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
unit ngppcinl;

{$i fpcdefs.inc}

interface

    uses
       cpubase,
       node,ninl,ncginl;

    type
       tgppcinlinenode = class(tcginlinenode)
          { first pass override
            so that the code generator will actually generate
            these nodes.
          }
          function first_sqrt_real: tnode; override;
          function first_abs_real: tnode; override;
          function first_sqr_real: tnode; override;
          function first_trunc_real: tnode; override;
          function first_round_real: tnode; override;
          procedure second_sqrt_real; override;
          procedure second_abs_real; override;
          procedure second_sqr_real; override;
          procedure second_trunc_real; override;
          procedure second_round_real; override;
          procedure second_prefetch;override;
       protected
          procedure load_fpu_location;
          procedure second_trunc_round_real(op: tasmop);
       end;

implementation

    uses
      cutils,globals,verbose,globtype,
      aasmtai,aasmdata,aasmcpu,
      symconst,symdef,
      defutil,
      cgbase,pass_2,
      cpuinfo,ncgutil,
      hlcgobj,cgutils,cgobj,rgobj,tgobj;


{*****************************************************************************
                              tgppcinlinenode
*****************************************************************************}

    function tgppcinlinenode.first_sqrt_real : tnode;
      begin
        if (current_settings.cputype >= cpu_PPC970) then
          begin
            expectloc:=LOC_FPUREGISTER;
            first_sqrt_real := nil;
          end
        else
          result:=inherited first_sqrt_real;
      end;


    function tgppcinlinenode.first_abs_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_abs_real := nil;
      end;


     function tgppcinlinenode.first_sqr_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_sqr_real := nil;
      end;


     function tgppcinlinenode.first_trunc_real : tnode;
      begin
       if (current_settings.cputype >= cpu_PPC970) then
          begin
            expectloc:=LOC_REFERENCE;
            first_trunc_real := nil;
          end
        else
          result:=inherited first_trunc_real;
      end;


     function tgppcinlinenode.first_round_real : tnode;
      begin
       if (current_settings.cputype >= cpu_PPC970) then
          begin
            expectloc:=LOC_REFERENCE;
            first_round_real := nil;
          end
        else
          result:=inherited first_round_real;
      end;


     { load the FPU into the an fpu register }
     procedure tgppcinlinenode.load_fpu_location;
       begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
         secondpass(left);
         hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
         location.loc := LOC_FPUREGISTER;
         location.register := cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
       end;


    procedure tgppcinlinenode.second_sqrt_real;
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


     procedure tgppcinlinenode.second_abs_real;
       begin
         location.loc:=LOC_FPUREGISTER;
         load_fpu_location;
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FABS,location.register,
           left.location.register));
       end;


     procedure tgppcinlinenode.second_sqr_real;
       var
         op: tasmop;
       begin
         location.loc:=LOC_FPUREGISTER;
         load_fpu_location;
         if (left.location.size = OS_F32) then
           op := A_FMULS
         else
           op := A_FMUL;
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,location.register,
           left.location.register,left.location.register));
       end;


     procedure tgppcinlinenode.second_trunc_round_real(op: tasmop);
       var
         tmpreg: tregister;
       begin
         if (current_settings.cputype < cpu_PPC970) then
           internalerror(2007020910);
         secondpass(left);
         hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
         tmpreg:=cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,tmpreg,
           left.location.register));
         location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),0);
         tg.gethltemp(current_asmdata.CurrAsmList,resultdef,resultdef.size,
           tt_normal,location.reference);
         cg.a_loadfpu_reg_ref(current_asmdata.CurrAsmList,OS_F64,OS_F64,tmpreg,
           location.reference);
       end;


    procedure tgppcinlinenode.second_trunc_real;
      begin
        second_trunc_round_real(A_FCTIDZ);
      end;


    procedure tgppcinlinenode.second_round_real;
      begin
        second_trunc_round_real(A_FCTID);
      end;


     procedure tgppcinlinenode.second_prefetch;
       var
         r: tregister;
       begin
         secondpass(left);
         case left.location.loc of
           LOC_CREFERENCE,
           LOC_REFERENCE:
             begin
               r:=cg.getintregister(current_asmdata.CurrAsmList,OS_ADDR);
               if (left.location.reference.offset = 0) and
                  not assigned(left.location.reference.symbol) then
                 begin
                   if (left.location.reference.index = NR_NO) then
                     current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_DCBT,0,left.location.reference.base))
                   else
                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_DCBT,left.location.reference.base,left.location.reference.index));
                 end
               else
                 begin
                   cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.location.reference,r);
                   current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_DCBT,0,r));
                 end;
             end;
           else
             { nothing to prefetch };
         end;
       end;


begin
   cinlinenode:=tgppcinlinenode;
end.
