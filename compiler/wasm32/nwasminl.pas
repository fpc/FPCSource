{
    Copyright (c) 1998-2002, 2021 by Florian Klaempfl and Nikolay Nikolov

    Generate WebAssembly inline nodes

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
unit nwasminl;

{$i fpcdefs.inc}

interface

    uses
      node,ncginl,cpubase;

    type

      { twasminlinenode }

      twasminlinenode = class(tcginlinenode)
      private
        function first_abs_real:tnode;override;
        function first_int_real:tnode;override;
        function first_sqrt_real:tnode;override;
        function first_trunc_real:tnode;override;
        function first_round_real:tnode;override;
        function first_popcnt:tnode;override;
        procedure second_abs_real;override;
        procedure second_int_real;override;
        procedure second_sqrt_real;override;
        procedure second_trunc_real;override;
        procedure second_round_real;override;
        procedure second_high; override;
        procedure second_popcnt;override;
        procedure second_memory_size;
        procedure second_memory_grow;
        procedure second_memory_fill;
        procedure second_memory_copy;
        procedure second_unreachable;
        procedure second_throw_fpcexception;
        procedure second_atomic_fence;
        procedure second_atomic_load(op: TAsmOp);
        procedure second_atomic_store(op: TAsmOp);
        procedure second_atomic_rmw_x_y(op: TAsmOp);
        procedure second_atomic_rmw_x_y_z(op: TAsmOp);
        procedure second_tls_get(const SymStr: string);
      protected
        function first_sqr_real: tnode; override;
      public
        function pass_typecheck_cpu: tnode; override;
        function first_cpu: tnode; override;
        procedure pass_generate_code_cpu; override;
        procedure second_length;override;
        procedure second_sqr_real; override;
      end;

implementation

    uses
      ninl,ncal,compinnr,
      aasmbase,aasmdata,aasmcpu,
      cgbase,cgutils,
      hlcgobj,hlcgcpu,
      defutil,pass_2,verbose,
      symtype,symdef;

{*****************************************************************************
                               twasminlinenode
*****************************************************************************}

    function twasminlinenode.first_abs_real: tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        result:=nil;
      end;


    function twasminlinenode.first_int_real: tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        result:=nil;
      end;


    function twasminlinenode.first_sqrt_real: tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        result:=nil;
      end;


    function twasminlinenode.first_trunc_real: tnode;
      begin
        expectloc:=LOC_REGISTER;
        result:=nil;
      end;


    function twasminlinenode.first_round_real: tnode;
      begin
        expectloc:=LOC_REGISTER;
        result:=nil;
      end;


    function twasminlinenode.first_popcnt: tnode;
      begin
        expectloc:=LOC_REGISTER;
        result:=nil;
      end;


    procedure twasminlinenode.second_abs_real;
      begin
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);

        case left.location.size of
          OS_F32:
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_f32_abs));
          OS_F64:
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_f64_abs));
          else
            internalerror(2021092902);
        end;

        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    procedure twasminlinenode.second_int_real;
      begin
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);

        case left.location.size of
          OS_F32:
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_f32_trunc));
          OS_F64:
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_f64_trunc));
          else
            internalerror(2021092903);
        end;

        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    procedure twasminlinenode.second_sqrt_real;
      begin
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);

        case left.location.size of
          OS_F32:
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_f32_sqrt));
          OS_F64:
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_f64_sqrt));
          else
            internalerror(2021092901);
        end;

        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    procedure twasminlinenode.second_trunc_real;
      begin
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);

        case left.location.size of
          OS_F32:
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_i64_trunc_f32_s));
          OS_F64:
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_i64_trunc_f64_s));
          else
            internalerror(2021092904);
        end;

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    procedure twasminlinenode.second_round_real;
      begin
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);

        case left.location.size of
          OS_F32:
            begin
              current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_f32_nearest));
              current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_i64_trunc_f32_s));
            end;
          OS_F64:
            begin
              current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_f64_nearest));
              current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_i64_trunc_f64_s));
            end
          else
            internalerror(2021092905);
        end;

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    procedure twasminlinenode.second_high;
      var
        hightype: TWasmBasicType;
      begin
        secondpass(left);
        if not(is_dynamic_array(left.resultdef)) then
          Internalerror(2019122801);
        { determine the WasmBasicType of the result }
        if is_64bit(resultdef) then
          hightype:=wbt_i64
        else
          hightype:=wbt_i32;
        { length in dynamic arrays is at offset -sizeof(pint) }
        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        { 64-bit pointer values need a <>0 comparison to produce a 32-bit int on the stack (0 or 1) for the 'if' instruction.
          32-bit pointer values don't need it, because 'if' already expects and pops a 32-bit int and checks for <>0. }
        if is_64bit(left.resultdef) then
          begin
            thlcgwasm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,left.resultdef,0,R_INTREGISTER);
            thlcgwasm(hlcg).a_cmp_stack_stack(current_asmdata.CurrAsmList,left.resultdef,OC_NE);
          end;
        { if not nil }
        current_asmdata.CurrAsmList.Concat(taicpu.op_functype(a_if,TWasmFuncType.Create([],[hightype])));
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
        { volatility of the dyn. array refers to the volatility of the
          string pointer, not of the string data }
        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        { length in dynamic arrays is at offset -ossinttype.size }
        thlcgwasm(hlcg).a_op_const_stack(current_asmdata.CurrAsmList,OP_SUB,left.resultdef,ossinttype.size);
        { load length }
        if ossinttype.size=8 then
          current_asmdata.CurrAsmList.Concat(taicpu.op_const(a_i64_load,0))
        else
          current_asmdata.CurrAsmList.Concat(taicpu.op_const(a_i32_load,0));
        { else }
        current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_else));
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
        { high=-1 }
        thlcgwasm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,resultdef,-1,R_INTREGISTER);
        { endif }
        current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_end_if));

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
        if location.size in [OS_64,OS_S64] then
          begin
            location.register64.reglo := cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            location.register64.reghi := cg.getintregister(current_asmdata.CurrAsmList,OS_32);
          end
        else
{$endif}
          location.register := hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);

        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    procedure twasminlinenode.second_popcnt;
      begin
        secondpass(left);

        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,left.resultdef,left.location.register);

        if is_64bit(left.resultdef) then
          current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_i64_popcnt))
        else
          current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_i32_popcnt));

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    procedure twasminlinenode.second_memory_size;
      begin
        current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_memory_size));
        thlcgwasm(hlcg).incstack(current_asmdata.CurrAsmList,1);

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    procedure twasminlinenode.second_memory_grow;
      begin
        secondpass(left);

        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,left.resultdef,left.location.register);

        current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_memory_grow));

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    procedure twasminlinenode.second_memory_fill;
      begin
        location_reset(location,LOC_VOID,OS_NO);

        secondpass(tcallparanode(tcallparanode(tcallparanode(left).right).right).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.location,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.resultdef,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.resultdef,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.location.register);

        secondpass(tcallparanode(tcallparanode(left).right).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(left).right).left.location,
          tcallparanode(tcallparanode(left).right).left.resultdef,
          tcallparanode(tcallparanode(left).right).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(left).right).left.resultdef,
          tcallparanode(tcallparanode(left).right).left.location.register);

        secondpass(tcallparanode(left).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(left).left.location,
          tcallparanode(left).left.resultdef,
          tcallparanode(left).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(left).left.resultdef,
          tcallparanode(left).left.location.register);

        current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_memory_fill));
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,3);
      end;


    procedure twasminlinenode.second_memory_copy;
      begin
        location_reset(location,LOC_VOID,OS_NO);

        secondpass(tcallparanode(tcallparanode(tcallparanode(left).right).right).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.location,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.resultdef,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.resultdef,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.location.register);

        secondpass(tcallparanode(tcallparanode(left).right).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(left).right).left.location,
          tcallparanode(tcallparanode(left).right).left.resultdef,
          tcallparanode(tcallparanode(left).right).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(left).right).left.resultdef,
          tcallparanode(tcallparanode(left).right).left.location.register);

        secondpass(tcallparanode(left).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(left).left.location,
          tcallparanode(left).left.resultdef,
          tcallparanode(left).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(left).left.resultdef,
          tcallparanode(left).left.location.register);

        current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_memory_copy));
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,3);
      end;


    procedure twasminlinenode.second_unreachable;
      begin
        location_reset(location,LOC_VOID,OS_NO);
        current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_unreachable));
      end;


    procedure twasminlinenode.second_throw_fpcexception;
      begin
        location_reset(location,LOC_VOID,OS_NO);
        current_asmdata.CurrAsmList.Concat(taicpu.op_sym(a_throw,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));
      end;


    procedure twasminlinenode.second_atomic_fence;
      begin
        location_reset(location,LOC_VOID,OS_NO);
        current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_atomic_fence));
      end;


    procedure twasminlinenode.second_atomic_load(op: TAsmOp);
      begin
        secondpass(left);

        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,left.resultdef,left.location.register);

        current_asmdata.CurrAsmList.Concat(taicpu.op_const(op,0));

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;

    procedure twasminlinenode.second_atomic_store(op: TAsmOp);
      begin
        location_reset(location,LOC_VOID,OS_NO);

        secondpass(tcallparanode(tcallparanode(left).right).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(left).right).left.location,
          tcallparanode(tcallparanode(left).right).left.resultdef,
          tcallparanode(tcallparanode(left).right).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(left).right).left.resultdef,
          tcallparanode(tcallparanode(left).right).left.location.register);

        secondpass(tcallparanode(left).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(left).left.location,
          tcallparanode(left).left.resultdef,
          tcallparanode(left).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(left).left.resultdef,
          tcallparanode(left).left.location.register);

        current_asmdata.CurrAsmList.Concat(taicpu.op_const(op,0));
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,2);
      end;


    procedure twasminlinenode.second_atomic_rmw_x_y(op: TAsmOp);
      begin
        secondpass(tcallparanode(tcallparanode(left).right).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(left).right).left.location,
          tcallparanode(tcallparanode(left).right).left.resultdef,
          tcallparanode(tcallparanode(left).right).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(left).right).left.resultdef,
          tcallparanode(tcallparanode(left).right).left.location.register);

        secondpass(tcallparanode(left).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(left).left.location,
          tcallparanode(left).left.resultdef,
          tcallparanode(left).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(left).left.resultdef,
          tcallparanode(left).left.location.register);

        current_asmdata.CurrAsmList.Concat(taicpu.op_const(op,0));
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;

    procedure twasminlinenode.second_atomic_rmw_x_y_z(op: TAsmOp);
      begin
        secondpass(tcallparanode(tcallparanode(tcallparanode(left).right).right).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.location,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.resultdef,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.resultdef,
          tcallparanode(tcallparanode(tcallparanode(left).right).right).left.location.register);

        secondpass(tcallparanode(tcallparanode(left).right).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(left).right).left.location,
          tcallparanode(tcallparanode(left).right).left.resultdef,
          tcallparanode(tcallparanode(left).right).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(tcallparanode(left).right).left.resultdef,
          tcallparanode(tcallparanode(left).right).left.location.register);

        secondpass(tcallparanode(left).left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,
          tcallparanode(left).left.location,
          tcallparanode(left).left.resultdef,
          tcallparanode(left).left.resultdef,false);
        thlcgwasm(hlcg).a_load_reg_stack(current_asmdata.CurrAsmList,
          tcallparanode(left).left.resultdef,
          tcallparanode(left).left.location.register);

        current_asmdata.CurrAsmList.Concat(taicpu.op_const(op,0));
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,2);

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    procedure twasminlinenode.second_tls_get(const SymStr: string);
      var
        sym: TWasmGlobalAsmSymbol;
      begin
        sym:=TWasmGlobalAsmSymbol(current_asmdata.RefAsmSymbolByClass(TWasmGlobalAsmSymbol,SymStr,AT_WASM_GLOBAL));
        sym.WasmGlobalType:=wbt_i32;
        current_asmdata.CurrAsmList.Concat(taicpu.op_sym(a_global_get,sym));
        thlcgwasm(hlcg).incstack(current_asmdata.CurrAsmList,1);

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;


    function twasminlinenode.first_sqr_real: tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_sqr_real:=nil;
      end;


    function twasminlinenode.pass_typecheck_cpu: tnode;
      begin
        Result:=nil;
        case inlinenumber of
          in_wasm32_memory_size:
            begin
              CheckParameters(0);
              resultdef:=u32inttype;
            end;
          in_wasm32_memory_grow:
            begin
              CheckParameters(1);
              resultdef:=u32inttype;
            end;
          in_wasm32_unreachable:
            begin
              CheckParameters(0);
              resultdef:=voidtype;
            end;
          in_wasm32_throw_fpcexception:
            begin
              CheckParameters(0);
              resultdef:=voidtype;
            end;
          in_wasm32_memory_fill:
            begin
              CheckParameters(3);
              resultdef:=voidtype;
            end;
          in_wasm32_memory_copy:
            begin
              CheckParameters(3);
              resultdef:=voidtype;
            end;
          in_wasm32_atomic_fence:
            begin
              CheckParameters(0);
              resultdef:=voidtype;
            end;

          in_wasm32_i32_atomic_rmw8_add_u,
          in_wasm32_i32_atomic_rmw16_add_u,
          in_wasm32_i32_atomic_rmw_add,
          in_wasm32_i32_atomic_rmw8_sub_u,
          in_wasm32_i32_atomic_rmw16_sub_u,
          in_wasm32_i32_atomic_rmw_sub,
          in_wasm32_i32_atomic_rmw8_and_u,
          in_wasm32_i32_atomic_rmw16_and_u,
          in_wasm32_i32_atomic_rmw_and,
          in_wasm32_i32_atomic_rmw8_or_u,
          in_wasm32_i32_atomic_rmw16_or_u,
          in_wasm32_i32_atomic_rmw_or,
          in_wasm32_i32_atomic_rmw8_xor_u,
          in_wasm32_i32_atomic_rmw16_xor_u,
          in_wasm32_i32_atomic_rmw_xor,
          in_wasm32_i32_atomic_rmw8_xchg_u,
          in_wasm32_i32_atomic_rmw16_xchg_u,
          in_wasm32_i32_atomic_rmw_xchg:
            begin
              CheckParameters(2);
              resultdef:=u32inttype;
            end;
          in_wasm32_i64_atomic_rmw8_add_u,
          in_wasm32_i64_atomic_rmw16_add_u,
          in_wasm32_i64_atomic_rmw32_add_u,
          in_wasm32_i64_atomic_rmw_add,
          in_wasm32_i64_atomic_rmw8_sub_u,
          in_wasm32_i64_atomic_rmw16_sub_u,
          in_wasm32_i64_atomic_rmw32_sub_u,
          in_wasm32_i64_atomic_rmw_sub,
          in_wasm32_i64_atomic_rmw8_and_u,
          in_wasm32_i64_atomic_rmw16_and_u,
          in_wasm32_i64_atomic_rmw32_and_u,
          in_wasm32_i64_atomic_rmw_and,
          in_wasm32_i64_atomic_rmw8_or_u,
          in_wasm32_i64_atomic_rmw16_or_u,
          in_wasm32_i64_atomic_rmw32_or_u,
          in_wasm32_i64_atomic_rmw_or,
          in_wasm32_i64_atomic_rmw8_xor_u,
          in_wasm32_i64_atomic_rmw16_xor_u,
          in_wasm32_i64_atomic_rmw32_xor_u,
          in_wasm32_i64_atomic_rmw_xor,
          in_wasm32_i64_atomic_rmw8_xchg_u,
          in_wasm32_i64_atomic_rmw16_xchg_u,
          in_wasm32_i64_atomic_rmw32_xchg_u,
          in_wasm32_i64_atomic_rmw_xchg:
            begin
              CheckParameters(2);
              resultdef:=u64inttype;
            end;
          in_wasm32_i32_atomic_rmw8_cmpxchg_u,
          in_wasm32_i32_atomic_rmw16_cmpxchg_u,
          in_wasm32_i32_atomic_rmw_cmpxchg:
            begin
              CheckParameters(3);
              resultdef:=u32inttype;
            end;
          in_wasm32_i64_atomic_rmw8_cmpxchg_u,
          in_wasm32_i64_atomic_rmw16_cmpxchg_u,
          in_wasm32_i64_atomic_rmw32_cmpxchg_u,
          in_wasm32_i64_atomic_rmw_cmpxchg:
            begin
              CheckParameters(3);
              resultdef:=u64inttype;
            end;
          in_wasm32_memory_atomic_wait32,
          in_wasm32_memory_atomic_wait64:
            begin
              CheckParameters(3);
              resultdef:=s32inttype;
            end;
          in_wasm32_memory_atomic_notify:
            begin
              CheckParameters(2);
              resultdef:=u32inttype;
            end;
          in_i32_atomic_load8_u,
          in_i32_atomic_load16_u,
          in_i32_atomic_load:
            begin
              CheckParameters(1);
              resultdef:=u32inttype;
            end;
          in_i64_atomic_load8_u,
          in_i64_atomic_load16_u,
          in_i64_atomic_load32_u,
          in_i64_atomic_load:
            begin
              CheckParameters(1);
              resultdef:=u64inttype;
            end;
          in_i32_atomic_store8,
          in_i32_atomic_store16,
          in_i32_atomic_store,
          in_i64_atomic_store8,
          in_i64_atomic_store16,
          in_i64_atomic_store32,
          in_i64_atomic_store:
            begin
              CheckParameters(2);
              resultdef:=voidtype;
            end;
          in_wasm32_tls_size,
          in_wasm32_tls_align:
            begin
              CheckParameters(0);
              resultdef:=u32inttype;
            end;
          in_wasm32_tls_base:
            begin
              CheckParameters(0);
              resultdef:=voidpointertype;
            end;
          else
            Result:=inherited pass_typecheck_cpu;
        end;
      end;


    function twasminlinenode.first_cpu: tnode;
      begin
        Result:=nil;
        case inlinenumber of
          in_wasm32_memory_size,
          in_wasm32_memory_grow:
            expectloc:=LOC_REGISTER;
          in_wasm32_memory_fill,
          in_wasm32_memory_copy,
          in_wasm32_unreachable,
          in_wasm32_throw_fpcexception,
          in_wasm32_atomic_fence,
          in_i32_atomic_store8,
          in_i32_atomic_store16,
          in_i32_atomic_store,
          in_i64_atomic_store8,
          in_i64_atomic_store16,
          in_i64_atomic_store32,
          in_i64_atomic_store:
            expectloc:=LOC_VOID;
          in_wasm32_i32_atomic_rmw8_add_u,
          in_wasm32_i32_atomic_rmw16_add_u,
          in_wasm32_i32_atomic_rmw_add,
          in_wasm32_i64_atomic_rmw8_add_u,
          in_wasm32_i64_atomic_rmw16_add_u,
          in_wasm32_i64_atomic_rmw32_add_u,
          in_wasm32_i64_atomic_rmw_add,
          in_wasm32_i32_atomic_rmw8_sub_u,
          in_wasm32_i32_atomic_rmw16_sub_u,
          in_wasm32_i32_atomic_rmw_sub,
          in_wasm32_i64_atomic_rmw8_sub_u,
          in_wasm32_i64_atomic_rmw16_sub_u,
          in_wasm32_i64_atomic_rmw32_sub_u,
          in_wasm32_i64_atomic_rmw_sub,
          in_wasm32_i32_atomic_rmw8_and_u,
          in_wasm32_i32_atomic_rmw16_and_u,
          in_wasm32_i32_atomic_rmw_and,
          in_wasm32_i64_atomic_rmw8_and_u,
          in_wasm32_i64_atomic_rmw16_and_u,
          in_wasm32_i64_atomic_rmw32_and_u,
          in_wasm32_i64_atomic_rmw_and,
          in_wasm32_i32_atomic_rmw8_or_u,
          in_wasm32_i32_atomic_rmw16_or_u,
          in_wasm32_i32_atomic_rmw_or,
          in_wasm32_i64_atomic_rmw8_or_u,
          in_wasm32_i64_atomic_rmw16_or_u,
          in_wasm32_i64_atomic_rmw32_or_u,
          in_wasm32_i64_atomic_rmw_or,
          in_wasm32_i32_atomic_rmw8_xor_u,
          in_wasm32_i32_atomic_rmw16_xor_u,
          in_wasm32_i32_atomic_rmw_xor,
          in_wasm32_i64_atomic_rmw8_xor_u,
          in_wasm32_i64_atomic_rmw16_xor_u,
          in_wasm32_i64_atomic_rmw32_xor_u,
          in_wasm32_i64_atomic_rmw_xor,
          in_wasm32_i32_atomic_rmw8_xchg_u,
          in_wasm32_i32_atomic_rmw16_xchg_u,
          in_wasm32_i32_atomic_rmw_xchg,
          in_wasm32_i64_atomic_rmw8_xchg_u,
          in_wasm32_i64_atomic_rmw16_xchg_u,
          in_wasm32_i64_atomic_rmw32_xchg_u,
          in_wasm32_i64_atomic_rmw_xchg,
          in_wasm32_i32_atomic_rmw8_cmpxchg_u,
          in_wasm32_i32_atomic_rmw16_cmpxchg_u,
          in_wasm32_i32_atomic_rmw_cmpxchg,
          in_wasm32_i64_atomic_rmw8_cmpxchg_u,
          in_wasm32_i64_atomic_rmw16_cmpxchg_u,
          in_wasm32_i64_atomic_rmw32_cmpxchg_u,
          in_wasm32_i64_atomic_rmw_cmpxchg,
          in_wasm32_memory_atomic_wait32,
          in_wasm32_memory_atomic_wait64,
          in_wasm32_memory_atomic_notify,
          in_i32_atomic_load8_u,
          in_i32_atomic_load16_u,
          in_i32_atomic_load,
          in_i64_atomic_load8_u,
          in_i64_atomic_load16_u,
          in_i64_atomic_load32_u,
          in_i64_atomic_load,
          in_wasm32_tls_size,
          in_wasm32_tls_align,
          in_wasm32_tls_base:
            expectloc:=LOC_REGISTER;
          else
            Result:=inherited first_cpu;
        end;
      end;


    procedure twasminlinenode.pass_generate_code_cpu;
      begin
        case inlinenumber of
          in_wasm32_memory_size:
            second_memory_size;
          in_wasm32_memory_grow:
            second_memory_grow;
          in_wasm32_memory_fill:
            second_memory_fill;
          in_wasm32_memory_copy:
            second_memory_copy;
          in_wasm32_unreachable:
            second_unreachable;
          in_wasm32_throw_fpcexception:
            second_throw_fpcexception;
          in_wasm32_atomic_fence:
            second_atomic_fence;
          in_wasm32_i32_atomic_rmw8_add_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw8_add_u);
          in_wasm32_i32_atomic_rmw16_add_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw16_add_u);
          in_wasm32_i32_atomic_rmw_add:
            second_atomic_rmw_x_y(a_i32_atomic_rmw_add);
          in_wasm32_i64_atomic_rmw8_add_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw8_add_u);
          in_wasm32_i64_atomic_rmw16_add_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw16_add_u);
          in_wasm32_i64_atomic_rmw32_add_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw32_add_u);
          in_wasm32_i64_atomic_rmw_add:
            second_atomic_rmw_x_y(a_i64_atomic_rmw_add);
          in_wasm32_i32_atomic_rmw8_sub_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw8_sub_u);
          in_wasm32_i32_atomic_rmw16_sub_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw16_sub_u);
          in_wasm32_i32_atomic_rmw_sub:
            second_atomic_rmw_x_y(a_i32_atomic_rmw_sub);
          in_wasm32_i64_atomic_rmw8_sub_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw8_sub_u);
          in_wasm32_i64_atomic_rmw16_sub_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw16_sub_u);
          in_wasm32_i64_atomic_rmw32_sub_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw32_sub_u);
          in_wasm32_i64_atomic_rmw_sub:
            second_atomic_rmw_x_y(a_i64_atomic_rmw_sub);
          in_wasm32_i32_atomic_rmw8_and_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw8_and_u);
          in_wasm32_i32_atomic_rmw16_and_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw16_and_u);
          in_wasm32_i32_atomic_rmw_and:
            second_atomic_rmw_x_y(a_i32_atomic_rmw_and);
          in_wasm32_i64_atomic_rmw8_and_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw8_and_u);
          in_wasm32_i64_atomic_rmw16_and_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw16_and_u);
          in_wasm32_i64_atomic_rmw32_and_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw32_and_u);
          in_wasm32_i64_atomic_rmw_and:
            second_atomic_rmw_x_y(a_i64_atomic_rmw_and);
          in_wasm32_i32_atomic_rmw8_or_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw8_or_u);
          in_wasm32_i32_atomic_rmw16_or_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw16_or_u);
          in_wasm32_i32_atomic_rmw_or:
            second_atomic_rmw_x_y(a_i32_atomic_rmw_or);
          in_wasm32_i64_atomic_rmw8_or_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw8_or_u);
          in_wasm32_i64_atomic_rmw16_or_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw16_or_u);
          in_wasm32_i64_atomic_rmw32_or_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw32_or_u);
          in_wasm32_i64_atomic_rmw_or:
            second_atomic_rmw_x_y(a_i64_atomic_rmw_or);
          in_wasm32_i32_atomic_rmw8_xor_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw8_xor_u);
          in_wasm32_i32_atomic_rmw16_xor_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw16_xor_u);
          in_wasm32_i32_atomic_rmw_xor:
            second_atomic_rmw_x_y(a_i32_atomic_rmw_xor);
          in_wasm32_i64_atomic_rmw8_xor_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw8_xor_u);
          in_wasm32_i64_atomic_rmw16_xor_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw16_xor_u);
          in_wasm32_i64_atomic_rmw32_xor_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw32_xor_u);
          in_wasm32_i64_atomic_rmw_xor:
            second_atomic_rmw_x_y(a_i64_atomic_rmw_xor);
          in_wasm32_i32_atomic_rmw8_xchg_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw8_xchg_u);
          in_wasm32_i32_atomic_rmw16_xchg_u:
            second_atomic_rmw_x_y(a_i32_atomic_rmw16_xchg_u);
          in_wasm32_i32_atomic_rmw_xchg:
            second_atomic_rmw_x_y(a_i32_atomic_rmw_xchg);
          in_wasm32_i64_atomic_rmw8_xchg_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw8_xchg_u);
          in_wasm32_i64_atomic_rmw16_xchg_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw16_xchg_u);
          in_wasm32_i64_atomic_rmw32_xchg_u:
            second_atomic_rmw_x_y(a_i64_atomic_rmw32_xchg_u);
          in_wasm32_i64_atomic_rmw_xchg:
            second_atomic_rmw_x_y(a_i64_atomic_rmw_xchg);
          in_wasm32_i32_atomic_rmw8_cmpxchg_u:
            second_atomic_rmw_x_y_z(a_i32_atomic_rmw8_cmpxchg_u);
          in_wasm32_i32_atomic_rmw16_cmpxchg_u:
            second_atomic_rmw_x_y_z(a_i32_atomic_rmw16_cmpxchg_u);
          in_wasm32_i32_atomic_rmw_cmpxchg:
            second_atomic_rmw_x_y_z(a_i32_atomic_rmw_cmpxchg);
          in_wasm32_i64_atomic_rmw8_cmpxchg_u:
            second_atomic_rmw_x_y_z(a_i64_atomic_rmw8_cmpxchg_u);
          in_wasm32_i64_atomic_rmw16_cmpxchg_u:
            second_atomic_rmw_x_y_z(a_i64_atomic_rmw16_cmpxchg_u);
          in_wasm32_i64_atomic_rmw32_cmpxchg_u:
            second_atomic_rmw_x_y_z(a_i64_atomic_rmw32_cmpxchg_u);
          in_wasm32_i64_atomic_rmw_cmpxchg:
            second_atomic_rmw_x_y_z(a_i64_atomic_rmw_cmpxchg);
          in_wasm32_memory_atomic_wait32:
            second_atomic_rmw_x_y_z(a_memory_atomic_wait32);
          in_wasm32_memory_atomic_wait64:
            second_atomic_rmw_x_y_z(a_memory_atomic_wait64);
          in_wasm32_memory_atomic_notify:
            second_atomic_rmw_x_y(a_memory_atomic_notify);
          in_i32_atomic_load8_u:
            second_atomic_load(a_i32_atomic_load8_u);
          in_i32_atomic_load16_u:
            second_atomic_load(a_i32_atomic_load16_u);
          in_i32_atomic_load:
            second_atomic_load(a_i32_atomic_load);
          in_i64_atomic_load8_u:
            second_atomic_load(a_i64_atomic_load8_u);
          in_i64_atomic_load16_u:
            second_atomic_load(a_i64_atomic_load16_u);
          in_i64_atomic_load32_u:
            second_atomic_load(a_i64_atomic_load32_u);
          in_i64_atomic_load:
            second_atomic_load(a_i64_atomic_load);
          in_i32_atomic_store8:
            second_atomic_store(a_i32_atomic_store8);
          in_i32_atomic_store16:
            second_atomic_store(a_i32_atomic_store16);
          in_i32_atomic_store:
            second_atomic_store(a_i32_atomic_store);
          in_i64_atomic_store8:
            second_atomic_store(a_i64_atomic_store8);
          in_i64_atomic_store16:
            second_atomic_store(a_i64_atomic_store16);
          in_i64_atomic_store32:
            second_atomic_store(a_i64_atomic_store32);
          in_i64_atomic_store:
            second_atomic_store(a_i64_atomic_store);
          in_wasm32_tls_size:
            second_tls_get(TLS_SIZE_SYM);
          in_wasm32_tls_align:
            second_tls_get(TLS_ALIGN_SYM);
          in_wasm32_tls_base:
            second_tls_get(TLS_BASE_SYM);
          else
            inherited pass_generate_code_cpu;
        end;
      end;


    procedure twasminlinenode.second_length;
      var
        lendef : tdef;
        href : treference;
        extra_slots: LongInt;
      begin
        secondpass(left);
        if is_shortstring(left.resultdef) then
          begin
            location_copy(location,left.location);
            location.size:=OS_8;
          end
        else
          begin
            { length in ansi/wide strings and high in dynamic arrays is at offset -sizeof(pint) }
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

            thlcgwasm(hlcg).a_cmp_const_reg_stack(current_asmdata.CurrAsmList,left.resultdef,OC_EQ,0,left.location.register);

            current_asmdata.CurrAsmList.Concat(taicpu.op_functype(a_if,TWasmFuncType.Create([],[wbt_i32])));
            thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);

            current_asmdata.CurrAsmList.Concat(taicpu.op_const(a_i32_const,0));
            thlcgwasm(hlcg).incstack(current_asmdata.CurrAsmList,1);

            current_asmdata.CurrAsmList.Concat( taicpu.op_none(a_else) );
            thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);

            { the length of a widestring is a 32 bit unsigned int. Since every
              character occupies 2 bytes, on a 32 bit platform you can express
              the maximum length using 31 bits. On a 64 bit platform, it may be
              32 bits. This means that regardless of the platform, a location
              with size OS_SINT/ossinttype can hold the length without
              overflowing (this code returns an ossinttype value) }
            if is_widestring(left.resultdef) then
              lendef:=u32inttype
            else
              lendef:=ossinttype;
            { volatility of the ansistring/widestring refers to the volatility of the
              string pointer, not of the string data }
            hlcg.reference_reset_base(href,left.resultdef,left.location.register,-lendef.size,ctempposinvalid,lendef.alignment,[]);

            extra_slots:=thlcgwasm(hlcg).prepare_stack_for_ref(current_asmdata.CurrAsmList,href,false);
            thlcgwasm(hlcg).a_load_ref_stack(current_asmdata.CurrAsmList,lendef,href,extra_slots);
            if is_widestring(left.resultdef) then
              thlcgwasm(hlcg).a_op_const_stack(current_asmdata.CurrAsmList,OP_SHR,resultdef,1);

            { Dynamic arrays do not have their length attached but their maximum index }
            if is_dynamic_array(left.resultdef) then
              thlcgwasm(hlcg).a_op_const_stack(current_asmdata.CurrAsmList,OP_ADD,resultdef,1);

            current_asmdata.CurrAsmList.Concat( taicpu.op_none(a_end_if) );

            location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
            location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
            thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
          end;
      end;


    procedure twasminlinenode.second_sqr_real;
      begin
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);

        case left.location.size of
          OS_F32:
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_f32_mul));
          OS_F64:
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_f64_mul));
          else
            internalerror(2021060102);
        end;
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);

        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;

begin
  cinlinenode:=twasminlinenode;
end.
