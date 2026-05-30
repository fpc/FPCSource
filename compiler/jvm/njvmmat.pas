{
    Copyright (c) 1998-2011 by Florian Klaempfl and Jonas Maebe

    Generate JVM code for math nodes

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
unit njvmmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat,ncghlmat,compilerbase;

    type
      tjvmmoddivnode = class(tmoddivnode)
        protected
          function use_moddiv64bitint_helper: boolean; override;
        public
         procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
      end;

      tjvmshlshrnode = class(tshlshrnode)
         procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
      end;

      tjvmnotnode = class(tcghlnotnode)
      end;

      tjvmunaryminusnode = class(tcgunaryminusnode)
        procedure second_float(ctx:tpassgeneratecodecontext);override;
      end;

implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,compinnr,
      symconst,symdef,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgbase,cgobj,pass_2,pass_2_context,procinfo,
      ncon,
      cpubase,
      nodehelper,hlcgcpu,cgutils,
      compiler;

{*****************************************************************************
                             tjvmmoddivnode
*****************************************************************************}

    function tjvmmoddivnode.use_moddiv64bitint_helper: boolean;
      begin
        result:=
          (left.resultdef.typ=orddef) and
          (right.resultdef.typ=orddef) and
          ((torddef(left.resultdef).ordtype=u64bit) or
           (torddef(right.resultdef).ordtype=u64bit));
      end;


    procedure tjvmmoddivnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
        tmpreg: tregister;
        lab: tasmlabel;
        op: topcg;
        isu32int: boolean;
      begin
         secondpass(left,ctx);
         secondpass(right,ctx);
         location_reset(location,LOC_REGISTER,left.location.size);
         location.register:=ctx.hlcg.getintregister(ctx.CurrAsmList,resultdef);


        if nodetype=divn then
          begin
            thlcgjvm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,left.resultdef,left.location);
            if is_signed(resultdef) then
              op:=OP_IDIV
            else
              op:=OP_DIV;
            thlcgjvm(ctx.hlcg).a_op_loc_stack(ctx.CurrAsmList,op,right.resultdef,right.location)
          end
        else
          begin
            { must be handled via a helper }
            if torddef(resultdef).ordtype=u64bit then
              internalerror(2011010416);
            if (torddef(resultdef).ordtype<>u32bit) then
              begin
                isu32int:=false;
                thlcgjvm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,left.resultdef,left.location);
                thlcgjvm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,right.resultdef,right.location);
              end
            else
              begin
                isu32int:=true;
                if left.location.loc=LOC_CONSTANT then
                  thlcgjvm(ctx.hlcg).a_load_const_stack(ctx.CurrAsmList,compiler.deftypes.s64inttype,left.location.value,R_INTREGISTER)
                else
                  begin
                    thlcgjvm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,left.resultdef,left.location);
                    thlcgjvm(ctx.hlcg).resize_stack_int_val(ctx.CurrAsmList,compiler.deftypes.u32inttype,compiler.deftypes.s64inttype,false);
                  end;
                if right.location.loc=LOC_CONSTANT then
                  thlcgjvm(ctx.hlcg).a_load_const_stack(ctx.CurrAsmList,compiler.deftypes.s64inttype,right.location.value,R_INTREGISTER)
                else
                  begin
                    thlcgjvm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,right.resultdef,right.location);
                    thlcgjvm(ctx.hlcg).resize_stack_int_val(ctx.CurrAsmList,compiler.deftypes.u32inttype,compiler.deftypes.s64inttype,false);
                  end;
              end;
            if isu32int or
               (torddef(resultdef).ordtype=s64bit) then
              begin
                ctx.CurrAsmList.concat(taicpu.op_none(a_lrem));
                thlcgjvm(ctx.hlcg).decstack(ctx.CurrAsmList,2);
              end
            else
              begin
                ctx.CurrAsmList.concat(taicpu.op_none(a_irem));
                thlcgjvm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
              end;
            if isu32int then
              thlcgjvm(ctx.hlcg).resize_stack_int_val(ctx.CurrAsmList,compiler.deftypes.s64inttype,compiler.deftypes.u32inttype,false);
          end;
         thlcgjvm(ctx.hlcg).a_load_stack_reg(ctx.CurrAsmList,resultdef,location.register);
         if (cs_check_overflow in compiler.globals.current_settings.localswitches) and
            is_signed(resultdef) then
           begin
             { the JVM raises an exception for integer div-iby-zero -> only
               overflow in case left is low(inttype) and right is -1 ->
               check by adding high(inttype) to left and and'ing with right
               -> result is -1 only in case above conditions are fulfilled)
             }
             tmpreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,resultdef);
             ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,resultdef,true);
             ctx.hlcg.a_op_const_reg_reg(ctx.CurrAsmList,OP_ADD,resultdef,torddef(resultdef).high,right.location.register,tmpreg);
             ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,resultdef,true);
             ctx.hlcg.a_op_reg_reg(ctx.CurrAsmList,OP_AND,resultdef,left.location.register,tmpreg);
             ctx.CurrAsmList.AsmData.getjumplabel(lab);
             ctx.hlcg.a_cmp_const_reg_label(ctx.CurrAsmList,resultdef,OC_NE,-1,tmpreg,lab);
             ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_overflow',[],nil);
             ctx.hlcg.a_label(ctx.CurrAsmList,lab);
           end;
      end;


{*****************************************************************************
                             tjvmshlshrnode
*****************************************************************************}

    procedure tjvmshlshrnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
        op : topcg;
      begin
        secondpass(left,ctx);
        secondpass(right,ctx);
        location_reset(location,LOC_REGISTER,left.location.size);
        location.register:=ctx.hlcg.getintregister(ctx.CurrAsmList,resultdef);

        thlcgjvm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,left.resultdef,left.location);
        if nodetype=shln then
          op:=OP_SHL
        else
          op:=OP_SHR;
        thlcgjvm(ctx.hlcg).a_op_loc_stack(ctx.CurrAsmList,op,resultdef,right.location);
        thlcgjvm(ctx.hlcg).a_load_stack_reg(ctx.CurrAsmList,resultdef,location.register);
      end;


{*****************************************************************************
                            tjvmunaryminustnode
*****************************************************************************}

    procedure tjvmunaryminusnode.second_float(ctx:tpassgeneratecodecontext);
      var
        opc: tasmop;
      begin
        secondpass(left,ctx);
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=ctx.hlcg.getfpuregister(ctx.CurrAsmList,resultdef);
        thlcgjvm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,left.resultdef,left.location);
        if (tfloatdef(left.resultdef).floattype=s32real) then
          opc:=a_fneg
        else
          opc:=a_dneg;
        ctx.CurrAsmList.concat(taicpu.op_none(opc));
        thlcgjvm(ctx.hlcg).a_load_stack_reg(ctx.CurrAsmList,resultdef,location.register);
      end;


begin
   cmoddivnode:=tjvmmoddivnode;
   cshlshrnode:=tjvmshlshrnode;
   cnotnode:=tjvmnotnode;
   cunaryminusnode:=tjvmunaryminusnode;
end.
