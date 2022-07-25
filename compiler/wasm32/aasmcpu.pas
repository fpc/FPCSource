{
    Copyright (c) 2019 by Free Pascal and Lazarus foundation

    Contains the assembler object for the WebAssembly

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
unit aasmcpu;

{$i fpcdefs.inc}

interface

uses
  cclasses,
  globtype,globals,verbose,
  aasmbase,aasmtai,aasmdata,aasmsym,
  cgbase,cgutils,cpubase,cpuinfo,ogbase,
  symtype,
  widestr;

    { fake, there are no "mov reg,reg" instructions here }
    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 0;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;

    type

      { taicpu }

      taicpu = class(tai_cpu_abstract_sym)
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : aint);
         constructor op_ref(op : tasmop;const _op1 : treference);
         constructor op_sym(op : tasmop;_op1 : tasmsymbol);

         constructor op_sym_const(op : tasmop;_op1 : tasmsymbol;_op2 : aint);

         constructor op_single(op : tasmop;_op1 : single);
         constructor op_double(op : tasmop;_op1 : double);

         constructor op_functype(op : tasmop; _op1: TWasmFuncType);

         procedure loadfunctype(opidx:longint;ft:TWasmFuncType);
         procedure loadsingle(opidx:longint;f:single);
         procedure loaddouble(opidx:longint;d:double);

         { register allocation }
         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         { register spilling code }
         function spilling_get_operation_type(opnr: longint): topertype;override;

         function Pass1(objdata:TObjData):longint;override;
         procedure Pass2(objdata:TObjData);override;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

      TImpExpType= (
        ie_Func,   // functions
        ie_Table,  // tables (arrays of methods)
        ie_Memory, // memory reference
        ie_Global  // global variables
      );

      { tai_export_name }

      tai_export_name = class(tai)
        extname : ansistring; // external name
        intname : ansistring; // internal name
        symstype: TImpExpType;
        constructor create(const aextname, aintname: ansistring; asymtype: timpexptype);
      end;

      // local variable declaration

      { tai_local }

      tai_local = class(tai)
        bastyp: TWasmBasicType;
        name : string;
        first: boolean;
        last: boolean;
        constructor create(abasictype: TWasmBasicType; const aname: string = '');
      end;

      { tai_globaltype }

      tai_globaltype = class(tai)
        globalname: string;
        gtype: TWasmBasicType;
        immutable: boolean;
        is_external: boolean;
        is_global: boolean;
        sym       : tasmsymbol;
        constructor create(const aglobalname:string; atype: TWasmBasicType; aimmutable: boolean);
        constructor create_local(const aglobalname:string; atype: TWasmBasicType; aimmutable: boolean; def: tdef);
        constructor create_global(const aglobalname:string; atype: TWasmBasicType; aimmutable: boolean; def: tdef);
      end;

      { tai_functype }

      tai_functype = class(tai)
        funcname: string;
        functype: TWasmFuncType;
        constructor create(const afuncname: string; afunctype: TWasmFuncType);
        destructor destroy;override;
      end;

      { tai_tagtype }

      tai_tagtype = class(tai)
        tagname: string;
        params: TWasmResultType;
        constructor create(const atagname: string; aparams: TWasmResultType);
      end;

      { tai_import_module }

      tai_import_module = class(tai)
        symname: string;
        importmodule: string;
        constructor create(const asymname, aimportmodule: string);
      end;

      { tai_import_name }

      tai_import_name = class(tai)
        symname: string;
        importname: string;
        constructor create(const asymname, aimportname: string);
      end;

    procedure InitAsm;
    procedure DoneAsm;

    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

implementation

uses
  ogwasm;

    { tai_globaltype }

    constructor tai_globaltype.create(const aglobalname: string; atype: TWasmBasicType; aimmutable: boolean);
      begin
        inherited Create;
        sym:=current_asmdata.RefAsmSymbol(aglobalname,AT_WASM_GLOBAL);
        typ:=ait_globaltype;
        globalname:=aglobalname;
        gtype:=atype;
        immutable:=aimmutable;
        is_external:=true;
        is_global:=false;
      end;

    constructor tai_globaltype.create_local(const aglobalname: string; atype: TWasmBasicType; aimmutable: boolean; def: tdef);
      begin
        inherited Create;
        sym:=current_asmdata.DefineAsmSymbol(aglobalname,AB_LOCAL,AT_WASM_GLOBAL,def);
        typ:=ait_globaltype;
        globalname:=aglobalname;
        gtype:=atype;
        immutable:=aimmutable;
        is_external:=false;
        is_global:=false;
      end;

    constructor tai_globaltype.create_global(const aglobalname: string; atype: TWasmBasicType; aimmutable: boolean; def: tdef);
      begin
        inherited Create;
        sym:=current_asmdata.DefineAsmSymbol(aglobalname,AB_GLOBAL,AT_WASM_GLOBAL,def);
        typ:=ait_globaltype;
        globalname:=aglobalname;
        gtype:=atype;
        immutable:=aimmutable;
        is_external:=false;
        is_global:=true;
      end;

    { tai_import_name }

    constructor tai_import_name.create(const asymname, aimportname: string);
      begin
        inherited Create;
        typ:=ait_import_name;
        symname:=asymname;
        importname:=aimportname;
      end;

    { tai_import_module }

    constructor tai_import_module.create(const asymname, aimportmodule: string);
      begin
        inherited Create;
        typ:=ait_import_module;
        symname:=asymname;
        importmodule:=aimportmodule;
      end;

    { tai_functype }

    constructor tai_functype.create(const afuncname: string; afunctype: TWasmFuncType);
      begin
        inherited Create;
        typ:=ait_functype;
        funcname:=afuncname;
        functype:=afunctype;
      end;


    destructor tai_functype.destroy;
      begin
        functype.free;
        inherited;
      end;

    { tai_tagtype }

    constructor tai_tagtype.create(const atagname: string; aparams: TWasmResultType);
      begin
        typ:=ait_tagtype;
        tagname:=atagname;
        params:=aparams;
      end;

    { tai_local }

    constructor tai_local.create(abasictype: TWasmBasicType; const aname: string);
      begin
        inherited Create;
        bastyp := abasictype;
        typ := ait_local;
        name := aname;
      end;

    { timpexp_ai }

      constructor tai_export_name.create(const aextname, aintname: ansistring;
          asymtype: timpexptype);
        begin
          inherited create;
          typ := ait_export_name;
          extname := aextname;
          intname := aintname;
          symstype:= asymtype;
        end;

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    constructor taicpu.op_none(op : tasmop);
      begin
        inherited create(op);
      end;


    constructor taicpu.op_reg(op : tasmop;_op1 : tregister);
      begin
        inherited create(op);
        ops:=1;
{$ifdef EXTDEBUG}
        if getregtype(_op1)=R_INVALIDREGISTER then
          InternalError(2021011901);
{$endif EXTDEBUG}
        loadreg(0,_op1);
      end;


    constructor taicpu.op_ref(op : tasmop;const _op1 : treference);
      begin
        inherited create(op);
        ops:=1;
        loadref(0,_op1);
        if op in [a_local_get,a_local_set,a_local_tee] then
          begin
            if (_op1.base<>NR_LOCAL_STACK_POINTER_REG) or (_op1.index<>NR_NO) then
              internalerror(2021010201);
          end
        else
          begin
            if (_op1.base<>NR_NO) or (_op1.index<>NR_NO) then
              internalerror(2021010202);
          end;
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : aint);
      begin
        inherited create(op);
        ops:=1;
        loadconst(0,_op1);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
        inherited create(op);
        ops:=1;
        loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_const(op: tasmop; _op1: tasmsymbol; _op2: aint);
      begin
        inherited create(op);
        ops:=2;
        loadsymbol(0,_op1,0);
        loadconst(1,_op2);
      end;


    constructor taicpu.op_single(op: tasmop; _op1: single);
      begin
        inherited create(op);
        ops:=1;
        loadsingle(0,_op1);
      end;


    constructor taicpu.op_double(op: tasmop; _op1: double);
      begin
        inherited create(op);
        ops:=1;
        loaddouble(0,_op1);
      end;

    constructor taicpu.op_functype(op: tasmop; _op1: TWasmFuncType);
      begin
       inherited create(op);
       ops:=1;
       loadfunctype(0,_op1);
      end;

    procedure taicpu.loadfunctype(opidx: longint; ft: TWasmFuncType);
      begin
       allocate_oper(opidx+1);
       with oper[opidx]^ do
        begin
          if typ<>top_functype then
            clearop(opidx);
          functype:=ft;
          typ:=top_functype;
        end;
      end;


    procedure taicpu.loadsingle(opidx:longint;f:single);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_single then
             clearop(opidx);
           sval:=f;
           typ:=top_single;
         end;
      end;


    procedure taicpu.loaddouble(opidx: longint; d: double);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_double then
             clearop(opidx);
           dval:=d;
           typ:=top_double;
         end;
      end;


    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        result:=false;
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        if opcode in AsmOp_Store then
          result:=operand_write
        else
          result:=operand_read;
      end;


    function taicpu.Pass1(objdata: TObjData): longint;

        function SlebSize(v: tcgint): longint;
          begin
            result:=0;
            repeat
              v:=SarInt64(v,7);
              Inc(result);
            until ((v=0) and ((byte(v) and 64)=0)) or ((v=-1) and ((byte(v) and 64)<>0));
          end;

        function UlebSize(v: tcgint): longint;
          begin
            result:=0;
            repeat
              v:=v shr 7;
              Inc(result);
            until v=0;
          end;

      begin
        result:=0;
        case opcode of
          a_unreachable,
          a_nop,
          a_return,
          a_drop,
          a_i32_eqz,
          a_i32_eq,
          a_i32_ne,
          a_i32_lt_s,
          a_i32_lt_u,
          a_i32_gt_s,
          a_i32_gt_u,
          a_i32_le_s,
          a_i32_le_u,
          a_i32_ge_s,
          a_i32_ge_u,
          a_i64_eqz,
          a_i64_eq,
          a_i64_ne,
          a_i64_lt_s,
          a_i64_lt_u,
          a_i64_gt_s,
          a_i64_gt_u,
          a_i64_le_s,
          a_i64_le_u,
          a_i64_ge_s,
          a_i64_ge_u,
          a_f32_eq,
          a_f32_ne,
          a_f32_lt,
          a_f32_gt,
          a_f32_le,
          a_f32_ge,
          a_f64_eq,
          a_f64_ne,
          a_f64_lt,
          a_f64_gt,
          a_f64_le,
          a_f64_ge,
          a_i32_clz,
          a_i32_ctz,
          a_i32_popcnt,
          a_i32_add,
          a_i32_sub,
          a_i32_mul,
          a_i32_div_s,
          a_i32_div_u,
          a_i32_rem_s,
          a_i32_rem_u,
          a_i32_and,
          a_i32_or,
          a_i32_xor,
          a_i32_shl,
          a_i32_shr_s,
          a_i32_shr_u,
          a_i32_rotl,
          a_i32_rotr,
          a_i64_clz,
          a_i64_ctz,
          a_i64_popcnt,
          a_i64_add,
          a_i64_sub,
          a_i64_mul,
          a_i64_div_s,
          a_i64_div_u,
          a_i64_rem_s,
          a_i64_rem_u,
          a_i64_and,
          a_i64_or,
          a_i64_xor,
          a_i64_shl,
          a_i64_shr_s,
          a_i64_shr_u,
          a_i64_rotl,
          a_i64_rotr,
          a_f32_abs,
          a_f32_neg,
          a_f32_ceil,
          a_f32_floor,
          a_f32_trunc,
          a_f32_nearest,
          a_f32_sqrt,
          a_f32_add,
          a_f32_sub,
          a_f32_mul,
          a_f32_div,
          a_f32_min,
          a_f32_max,
          a_f32_copysign,
          a_f64_abs,
          a_f64_neg,
          a_f64_ceil,
          a_f64_floor,
          a_f64_trunc,
          a_f64_nearest,
          a_f64_sqrt,
          a_f64_add,
          a_f64_sub,
          a_f64_mul,
          a_f64_div,
          a_f64_min,
          a_f64_max,
          a_f64_copysign,
          a_i32_wrap_i64,
          a_i32_trunc_f32_s,
          a_i32_trunc_f32_u,
          a_i32_trunc_f64_s,
          a_i32_trunc_f64_u,
          a_i64_extend_i32_s,
          a_i64_extend_i32_u,
          a_i64_trunc_f32_s,
          a_i64_trunc_f32_u,
          a_i64_trunc_f64_s,
          a_i64_trunc_f64_u,
          a_f32_convert_i32_s,
          a_f32_convert_i32_u,
          a_f32_convert_i64_s,
          a_f32_convert_i64_u,
          a_f32_demote_f64,
          a_f64_convert_i32_s,
          a_f64_convert_i32_u,
          a_f64_convert_i64_s,
          a_f64_convert_i64_u,
          a_f64_promote_f32,
          a_i32_reinterpret_f32,
          a_i64_reinterpret_f64,
          a_f32_reinterpret_i32,
          a_f64_reinterpret_i64,
          a_i32_extend8_s,
          a_i32_extend16_s,
          a_i64_extend8_s,
          a_i64_extend16_s,
          a_i64_extend32_s,
          a_else,
          a_end_block,
          a_end_if,
          a_end_loop,
          a_end_try,
          a_catch_all:
            result:=1;
          a_i32_trunc_sat_f32_s,
          a_i32_trunc_sat_f32_u,
          a_i32_trunc_sat_f64_s,
          a_i32_trunc_sat_f64_u,
          a_i64_trunc_sat_f32_s,
          a_i64_trunc_sat_f32_u,
          a_i64_trunc_sat_f64_s,
          a_i64_trunc_sat_f64_u,
          a_memory_size,
          a_memory_grow:
            result:=2;
          a_memory_copy:
            result:=4;
          a_memory_fill,
          a_atomic_fence:
            result:=3;
          a_i32_const:
            begin
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        result:=6
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          result:=1+SlebSize(longint(ref^.offset));
                        end;
                    end;
                  top_const:
                    result:=1+SlebSize(longint(val));
                  else
                    internalerror(2021092615);
                end;
            end;
          a_i64_const:
            begin
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        result:=6
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          result:=1+SlebSize(int64(ref^.offset));
                        end;
                    end;
                  top_const:
                    result:=1+SlebSize(int64(val));
                  else
                    internalerror(2021092615);
                end;
            end;
          a_f32_const:
            result:=5;
          a_f64_const:
            result:=9;
          a_local_get,
          a_local_set,
          a_local_tee:
            begin
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        internalerror(2021092005);
                      if ref^.base<>NR_STACK_POINTER_REG then
                        internalerror(2021092006);
                      if ref^.index<>NR_NO then
                        internalerror(2021092007);
                      result:=1+UlebSize(ref^.offset);
                    end;
                  else
                    internalerror(2021092008);
                end;
            end;
          a_global_get,
          a_global_set:
            begin
              if ops<>1 then
                internalerror(2021092010);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) then
                        internalerror(2021092012);
                      if (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092013);
                      result:=6;
                    end;
                  else
                    internalerror(2021092011);
                end;
            end;
          a_end_function:
            result:=0;
          a_block,
          a_loop,
          a_if,
          a_try:
            begin
              if ops=0 then
                result:=2
              else
                begin
                  if ops<>1 then
                    internalerror(2021092015);
                  with oper[0]^ do
                    case typ of
                      top_functype:
                        begin
                          if (length(functype.params)=0) and (length(functype.results)<=1) then
                            result:=2
                          else
                            { more complex blocktypes are not yet implemented }
                            internalerror(2021092621);
                        end;
                      else
                        internalerror(2021092620);
                    end;
                end;
            end;
          a_i32_load,
          a_i64_load,
          a_f32_load,
          a_f64_load,
          a_i32_load8_s,
          a_i32_load8_u,
          a_i32_load16_s,
          a_i32_load16_u,
          a_i64_load8_s,
          a_i64_load8_u,
          a_i64_load16_s,
          a_i64_load16_u,
          a_i64_load32_s,
          a_i64_load32_u,
          a_i32_store,
          a_i64_store,
          a_f32_store,
          a_f64_store,
          a_i32_store8,
          a_i32_store16,
          a_i64_store8,
          a_i64_store16,
          a_i64_store32:
            begin
              if ops<>1 then
                internalerror(2021092016);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        begin
                          Result:=1+
                            UlebSize(natural_alignment_for_load_store(opcode))+
                            5;  { relocation, fixed size = 5 bytes }
                        end
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          Result:=1+
                            UlebSize(natural_alignment_for_load_store(opcode))+
                            UlebSize(ref^.offset);
                        end;
                    end;
                  top_const:
                    begin
                      Result:=1+
                        UlebSize(natural_alignment_for_load_store(opcode))+
                        UlebSize(val);
                    end;
                  else
                    internalerror(2021092017);
                end;
            end;
          a_memory_atomic_notify,
          a_memory_atomic_wait32,
          a_memory_atomic_wait64,
          a_i32_atomic_load,
          a_i64_atomic_load,
          a_i32_atomic_load8_u,
          a_i32_atomic_load16_u,
          a_i64_atomic_load8_u,
          a_i64_atomic_load16_u,
          a_i64_atomic_load32_u,
          a_i32_atomic_store,
          a_i64_atomic_store,
          a_i32_atomic_store8,
          a_i32_atomic_store16,
          a_i64_atomic_store8,
          a_i64_atomic_store16,
          a_i64_atomic_store32,
          a_i32_atomic_rmw_add,
          a_i64_atomic_rmw_add,
          a_i32_atomic_rmw8_add_u,
          a_i32_atomic_rmw16_add_u,
          a_i64_atomic_rmw8_add_u,
          a_i64_atomic_rmw16_add_u,
          a_i64_atomic_rmw32_add_u,
          a_i32_atomic_rmw_sub,
          a_i64_atomic_rmw_sub,
          a_i32_atomic_rmw8_sub_u,
          a_i32_atomic_rmw16_sub_u,
          a_i64_atomic_rmw8_sub_u,
          a_i64_atomic_rmw16_sub_u,
          a_i64_atomic_rmw32_sub_u,
          a_i32_atomic_rmw_and,
          a_i64_atomic_rmw_and,
          a_i32_atomic_rmw8_and_u,
          a_i32_atomic_rmw16_and_u,
          a_i64_atomic_rmw8_and_u,
          a_i64_atomic_rmw16_and_u,
          a_i64_atomic_rmw32_and_u,
          a_i32_atomic_rmw_or,
          a_i64_atomic_rmw_or,
          a_i32_atomic_rmw8_or_u,
          a_i32_atomic_rmw16_or_u,
          a_i64_atomic_rmw8_or_u,
          a_i64_atomic_rmw16_or_u,
          a_i64_atomic_rmw32_or_u,
          a_i32_atomic_rmw_xor,
          a_i64_atomic_rmw_xor,
          a_i32_atomic_rmw8_xor_u,
          a_i32_atomic_rmw16_xor_u,
          a_i64_atomic_rmw8_xor_u,
          a_i64_atomic_rmw16_xor_u,
          a_i64_atomic_rmw32_xor_u,
          a_i32_atomic_rmw_xchg,
          a_i64_atomic_rmw_xchg,
          a_i32_atomic_rmw8_xchg_u,
          a_i32_atomic_rmw16_xchg_u,
          a_i64_atomic_rmw8_xchg_u,
          a_i64_atomic_rmw16_xchg_u,
          a_i64_atomic_rmw32_xchg_u,
          a_i32_atomic_rmw_cmpxchg,
          a_i64_atomic_rmw_cmpxchg,
          a_i32_atomic_rmw8_cmpxchg_u,
          a_i32_atomic_rmw16_cmpxchg_u,
          a_i64_atomic_rmw8_cmpxchg_u,
          a_i64_atomic_rmw16_cmpxchg_u,
          a_i64_atomic_rmw32_cmpxchg_u:
            begin
              if ops<>1 then
                internalerror(2021092016);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        begin
                          Result:=2+
                            UlebSize(natural_alignment_for_load_store(opcode))+
                            5;  { relocation, fixed size = 5 bytes }
                        end
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          Result:=2+
                            UlebSize(natural_alignment_for_load_store(opcode))+
                            UlebSize(ref^.offset);
                        end;
                    end;
                  top_const:
                    begin
                      Result:=2+
                        UlebSize(natural_alignment_for_load_store(opcode))+
                        UlebSize(val);
                    end;
                  else
                    internalerror(2021092017);
                end;
            end;
          a_call:
            begin
              if ops<>1 then
                internalerror(2021092021);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092023);
                      result:=6;
                    end;
                  else
                    internalerror(2021092022);
                end;
            end;
          a_call_indirect:
            begin
              if ops<>1 then
                internalerror(2021092610);
              with oper[0]^ do
                case typ of
                  top_functype:
                    begin
                      TWasmObjData(objdata).AddFuncType(functype);
                      result:=6+
                        UlebSize(0);
                    end;
                  else
                    internalerror(2021092611);
                end;
            end;
          a_br,
          a_br_if,
          a_rethrow,
          a_delegate:
            begin
              if ops<>1 then
                internalerror(2021092610);
              with oper[0]^ do
                case typ of
                  top_const:
                    result:=1+
                      UlebSize(val);
                  else
                    internalerror(2021092625);
                end;
            end;
          a_catch,
          a_throw:
            begin
              if ops<>1 then
                internalerror(2021092709);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092711);
                      result:=6;
                    end;
                  else
                    internalerror(2021092710);
                end;
            end;
          a_memory_init:
            begin
              if ops<>1 then
                internalerror(2022052802);
              with oper[0]^ do
                case typ of
                  top_const:
                    result:=3+UlebSize(val);
                  else
                    internalerror(2022052803);
                end;
            end;
          a_data_drop:
            begin
              if ops<>1 then
                internalerror(2022052804);
              with oper[0]^ do
                case typ of
                  top_const:
                    result:=2+UlebSize(val);
                  else
                    internalerror(2022052805);
                end;
            end;
          else
            internalerror(2021092623);
        end;
      end;


    procedure taicpu.Pass2(objdata: TObjData);

        procedure WriteByte(b: byte);
          begin
            objdata.writebytes(b,1);
          end;

{$ifdef FPC_LITTLE_ENDIAN}
        procedure WriteSingle(s: single);
          begin
            objdata.writebytes(s,4);
          end;

        procedure WriteDouble(d: double);
          begin
            objdata.writebytes(d,8);
          end;
{$else FPC_LITTLE_ENDIAN}
        procedure WriteSingle(s: single);
          var
            l: longword;
          begin
            Move(s,l,4);
            l:=SwapEndian(l);
            objdata.writebytes(l,4);
          end;

        procedure WriteDouble(d: double);
          var
            q: qword;
          begin
            Move(d,q,8);
            q:=SwapEndian(q);
            objdata.writebytes(q,8);
          end;
{$endif FPC_LITTLE_ENDIAN}

        procedure WriteSleb(v: tcgint);
          var
            b: byte;
            Done: Boolean=false;
          begin
            repeat
              b:=byte(v) and 127;
              v:=SarInt64(v,7);
              if ((v=0) and ((b and 64)=0)) or ((v=-1) and ((b and 64)<>0)) then
                Done:=true
              else
                b:=b or 128;
              objdata.writebytes(b,1);
            until Done;
          end;

        procedure WriteUleb(v: tcgint);
          var
            b: byte;
          begin
            repeat
              b:=byte(v) and 127;
              v:=v shr 7;
              if v<>0 then
                b:=b or 128;
              objdata.writebytes(b,1);
            until v=0;
          end;

      begin
        case opcode of
          a_unreachable:
            WriteByte($00);
          a_nop:
            WriteByte($01);
          a_return:
            WriteByte($0F);
          a_drop:
            WriteByte($1A);
          a_memory_size:
            begin
              WriteByte($3F);
              WriteByte($00);
            end;
          a_memory_grow:
            begin
              WriteByte($40);
              WriteByte($00);
            end;
          a_memory_copy:
            begin
              WriteByte($FC);
              WriteUleb(10);
              WriteByte($00);
              WriteByte($00);
            end;
          a_memory_fill:
            begin
              WriteByte($FC);
              WriteUleb(11);
              WriteByte($00);
            end;
          a_atomic_fence:
            begin
              WriteByte($FE);
              WriteByte($03);
              WriteByte($00);
            end;
          a_i32_eqz:
            WriteByte($45);
          a_i32_eq:
            WriteByte($46);
          a_i32_ne:
            WriteByte($47);
          a_i32_lt_s:
            WriteByte($48);
          a_i32_lt_u:
            WriteByte($49);
          a_i32_gt_s:
            WriteByte($4A);
          a_i32_gt_u:
            WriteByte($4B);
          a_i32_le_s:
            WriteByte($4C);
          a_i32_le_u:
            WriteByte($4D);
          a_i32_ge_s:
            WriteByte($4E);
          a_i32_ge_u:
            WriteByte($4F);
          a_i64_eqz:
            WriteByte($50);
          a_i64_eq:
            WriteByte($51);
          a_i64_ne:
            WriteByte($52);
          a_i64_lt_s:
            WriteByte($53);
          a_i64_lt_u:
            WriteByte($54);
          a_i64_gt_s:
            WriteByte($55);
          a_i64_gt_u:
            WriteByte($56);
          a_i64_le_s:
            WriteByte($57);
          a_i64_le_u:
            WriteByte($58);
          a_i64_ge_s:
            WriteByte($59);
          a_i64_ge_u:
            WriteByte($5A);
          a_f32_eq:
            WriteByte($5B);
          a_f32_ne:
            WriteByte($5C);
          a_f32_lt:
            WriteByte($5D);
          a_f32_gt:
            WriteByte($5E);
          a_f32_le:
            WriteByte($5F);
          a_f32_ge:
            WriteByte($60);
          a_f64_eq:
            WriteByte($61);
          a_f64_ne:
            WriteByte($62);
          a_f64_lt:
            WriteByte($63);
          a_f64_gt:
            WriteByte($64);
          a_f64_le:
            WriteByte($65);
          a_f64_ge:
            WriteByte($66);
          a_i32_clz:
            WriteByte($67);
          a_i32_ctz:
            WriteByte($68);
          a_i32_popcnt:
            WriteByte($69);
          a_i32_add:
            WriteByte($6A);
          a_i32_sub:
            WriteByte($6B);
          a_i32_mul:
            WriteByte($6C);
          a_i32_div_s:
            WriteByte($6D);
          a_i32_div_u:
            WriteByte($6E);
          a_i32_rem_s:
            WriteByte($6F);
          a_i32_rem_u:
            WriteByte($70);
          a_i32_and:
            WriteByte($71);
          a_i32_or:
            WriteByte($72);
          a_i32_xor:
            WriteByte($73);
          a_i32_shl:
            WriteByte($74);
          a_i32_shr_s:
            WriteByte($75);
          a_i32_shr_u:
            WriteByte($76);
          a_i32_rotl:
            WriteByte($77);
          a_i32_rotr:
            WriteByte($78);
          a_i64_clz:
            WriteByte($79);
          a_i64_ctz:
            WriteByte($7A);
          a_i64_popcnt:
            WriteByte($7B);
          a_i64_add:
            WriteByte($7C);
          a_i64_sub:
            WriteByte($7D);
          a_i64_mul:
            WriteByte($7E);
          a_i64_div_s:
            WriteByte($7F);
          a_i64_div_u:
            WriteByte($80);
          a_i64_rem_s:
            WriteByte($81);
          a_i64_rem_u:
            WriteByte($82);
          a_i64_and:
            WriteByte($83);
          a_i64_or:
            WriteByte($84);
          a_i64_xor:
            WriteByte($85);
          a_i64_shl:
            WriteByte($86);
          a_i64_shr_s:
            WriteByte($87);
          a_i64_shr_u:
            WriteByte($88);
          a_i64_rotl:
            WriteByte($89);
          a_i64_rotr:
            WriteByte($8A);
          a_f32_abs:
            WriteByte($8B);
          a_f32_neg:
            WriteByte($8C);
          a_f32_ceil:
            WriteByte($8D);
          a_f32_floor:
            WriteByte($8E);
          a_f32_trunc:
            WriteByte($8F);
          a_f32_nearest:
            WriteByte($90);
          a_f32_sqrt:
            WriteByte($91);
          a_f32_add:
            WriteByte($92);
          a_f32_sub:
            WriteByte($93);
          a_f32_mul:
            WriteByte($94);
          a_f32_div:
            WriteByte($95);
          a_f32_min:
            WriteByte($96);
          a_f32_max:
            WriteByte($97);
          a_f32_copysign:
            WriteByte($98);
          a_f64_abs:
            WriteByte($99);
          a_f64_neg:
            WriteByte($9A);
          a_f64_ceil:
            WriteByte($9B);
          a_f64_floor:
            WriteByte($9C);
          a_f64_trunc:
            WriteByte($9D);
          a_f64_nearest:
            WriteByte($9E);
          a_f64_sqrt:
            WriteByte($9F);
          a_f64_add:
            WriteByte($A0);
          a_f64_sub:
            WriteByte($A1);
          a_f64_mul:
            WriteByte($A2);
          a_f64_div:
            WriteByte($A3);
          a_f64_min:
            WriteByte($A4);
          a_f64_max:
            WriteByte($A5);
          a_f64_copysign:
            WriteByte($A6);
          a_i32_wrap_i64:
            WriteByte($A7);
          a_i32_trunc_f32_s:
            WriteByte($A8);
          a_i32_trunc_f32_u:
            WriteByte($A9);
          a_i32_trunc_f64_s:
            WriteByte($AA);
          a_i32_trunc_f64_u:
            WriteByte($AB);
          a_i64_extend_i32_s:
            WriteByte($AC);
          a_i64_extend_i32_u:
            WriteByte($AD);
          a_i64_trunc_f32_s:
            WriteByte($AE);
          a_i64_trunc_f32_u:
            WriteByte($AF);
          a_i64_trunc_f64_s:
            WriteByte($B0);
          a_i64_trunc_f64_u:
            WriteByte($B1);
          a_f32_convert_i32_s:
            WriteByte($B2);
          a_f32_convert_i32_u:
            WriteByte($B3);
          a_f32_convert_i64_s:
            WriteByte($B4);
          a_f32_convert_i64_u:
            WriteByte($B5);
          a_f32_demote_f64:
            WriteByte($B6);
          a_f64_convert_i32_s:
            WriteByte($B7);
          a_f64_convert_i32_u:
            WriteByte($B8);
          a_f64_convert_i64_s:
            WriteByte($B9);
          a_f64_convert_i64_u:
            WriteByte($BA);
          a_f64_promote_f32:
            WriteByte($BB);
          a_i32_reinterpret_f32:
            WriteByte($BC);
          a_i64_reinterpret_f64:
            WriteByte($BD);
          a_f32_reinterpret_i32:
            WriteByte($BE);
          a_f64_reinterpret_i64:
            WriteByte($BF);
          a_i32_extend8_s:
            WriteByte($C0);
          a_i32_extend16_s:
            WriteByte($C1);
          a_i64_extend8_s:
            WriteByte($C2);
          a_i64_extend16_s:
            WriteByte($C3);
          a_i64_extend32_s:
            WriteByte($C4);
          a_end_block,
          a_end_if,
          a_end_loop,
          a_end_try:
            WriteByte($0B);
          a_catch_all:
            WriteByte($19);
          a_i32_const:
            begin
              WriteByte($41);
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        objdata.writeReloc(ref^.offset,5,ObjData.symbolref(ref^.symbol),RELOC_MEMORY_ADDR_OR_TABLE_INDEX_SLEB)
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          WriteSleb(longint(ref^.offset));
                        end;
                    end;
                  top_const:
                    WriteSleb(longint(val));
                  else
                    internalerror(2021092615);
                end;
            end;
          a_i64_const:
            begin
              WriteByte($42);
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        objdata.writeReloc(ref^.offset,5,ObjData.symbolref(ref^.symbol),RELOC_MEMORY_ADDR_OR_TABLE_INDEX_SLEB)
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          WriteSleb(int64(ref^.offset));
                        end;
                    end;
                  top_const:
                    WriteSleb(int64(val));
                  else
                    internalerror(2021092615);
                end;
            end;
          a_f32_const:
            begin
              if ops<>1 then
                internalerror(2021092619);
              WriteByte($43);
              with oper[0]^ do
                case typ of
                  top_single:
                    WriteSingle(sval);
                  else
                    internalerror(2021092618);
                end;
            end;
          a_f64_const:
            begin
              if ops<>1 then
                internalerror(2021092616);
              WriteByte($44);
              with oper[0]^ do
                case typ of
                  top_double:
                    WriteDouble(dval);
                  else
                    internalerror(2021092617);
                end;
            end;
          a_local_get,
          a_local_set,
          a_local_tee:
            begin
              case opcode of
                a_local_get:
                  WriteByte($20);
                a_local_set:
                  WriteByte($21);
                a_local_tee:
                  WriteByte($22);
                else
                  internalerror(2021092003);
              end;
              if ops<>1 then
                internalerror(2021092004);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        internalerror(2021092005);
                      if ref^.base<>NR_STACK_POINTER_REG then
                        internalerror(2021092006);
                      if ref^.index<>NR_NO then
                        internalerror(2021092007);
                      WriteUleb(ref^.offset);
                    end;
                  else
                    internalerror(2021092008);
                end;
            end;
          a_global_get,
          a_global_set:
            begin
              case opcode of
                a_global_get:
                  WriteByte($23);
                a_global_set:
                  WriteByte($24);
                else
                  internalerror(2021092009);
              end;
              if ops<>1 then
                internalerror(2021092010);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) then
                        internalerror(2021092012);
                      if (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092013);
                      objdata.writeReloc(0,5,TWasmObjData(ObjData).globalref(ref^.symbol),RELOC_GLOBAL_INDEX_LEB);
                    end;
                  else
                    internalerror(2021092011);
                end;
            end;
          a_end_function:
            ;
          a_block,
          a_loop,
          a_if,
          a_try:
            begin
              case opcode of
                a_block:
                  WriteByte($02);
                a_loop:
                  WriteByte($03);
                a_if:
                  WriteByte($04);
                a_try:
                  WriteByte($06);
                else
                  internalerror(2021092626);
              end;
              if ops=0 then
                WriteByte($40)
              else
                begin
                  if ops<>1 then
                    internalerror(2021092015);
                  with oper[0]^ do
                    case typ of
                      top_functype:
                        begin
                          if (length(functype.params)=0) and (length(functype.results)<=1) then
                            begin
                              if length(functype.results)=1 then
                                WriteByte(encode_wasm_basic_type(functype.results[0]))
                              else
                                WriteByte($40);
                            end
                          else
                            { more complex blocktypes are not yet implemented }
                            internalerror(2021092621);
                        end;
                      else
                        internalerror(2021092620);
                    end;
                end;
            end;
          a_else:
            WriteByte($05);
          a_i32_load,
          a_i64_load,
          a_f32_load,
          a_f64_load,
          a_i32_load8_s,
          a_i32_load8_u,
          a_i32_load16_s,
          a_i32_load16_u,
          a_i64_load8_s,
          a_i64_load8_u,
          a_i64_load16_s,
          a_i64_load16_u,
          a_i64_load32_s,
          a_i64_load32_u,
          a_i32_store,
          a_i64_store,
          a_f32_store,
          a_f64_store,
          a_i32_store8,
          a_i32_store16,
          a_i64_store8,
          a_i64_store16,
          a_i64_store32:
            begin
              case opcode of
                a_i32_load:
                  WriteByte($28);
                a_i64_load:
                  WriteByte($29);
                a_f32_load:
                  WriteByte($2A);
                a_f64_load:
                  WriteByte($2B);
                a_i32_load8_s:
                  WriteByte($2C);
                a_i32_load8_u:
                  WriteByte($2D);
                a_i32_load16_s:
                  WriteByte($2E);
                a_i32_load16_u:
                  WriteByte($2F);
                a_i64_load8_s:
                  WriteByte($30);
                a_i64_load8_u:
                  WriteByte($31);
                a_i64_load16_s:
                  WriteByte($32);
                a_i64_load16_u:
                  WriteByte($33);
                a_i64_load32_s:
                  WriteByte($34);
                a_i64_load32_u:
                  WriteByte($35);
                a_i32_store:
                  WriteByte($36);
                a_i64_store:
                  WriteByte($37);
                a_f32_store:
                  WriteByte($38);
                a_f64_store:
                  WriteByte($39);
                a_i32_store8:
                  WriteByte($3A);
                a_i32_store16:
                  WriteByte($3B);
                a_i64_store8:
                  WriteByte($3C);
                a_i64_store16:
                  WriteByte($3D);
                a_i64_store32:
                  WriteByte($3E);
                else
                  internalerror(2021092019);
              end;
              if ops<>1 then
                internalerror(2021092016);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        begin
                          WriteUleb(natural_alignment_for_load_store(opcode));
                          objdata.writeReloc(ref^.offset,5,ObjData.symbolref(ref^.symbol),RELOC_MEMORY_ADDR_LEB);
                        end
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          WriteUleb(natural_alignment_for_load_store(opcode));
                          WriteUleb(ref^.offset);
                        end;
                    end;
                  top_const:
                    begin
                      WriteUleb(natural_alignment_for_load_store(opcode));
                      WriteUleb(val);
                    end;
                  else
                    internalerror(2021092017);
                end;
            end;
          a_memory_atomic_notify,
          a_memory_atomic_wait32,
          a_memory_atomic_wait64,
          a_i32_atomic_load,
          a_i64_atomic_load,
          a_i32_atomic_load8_u,
          a_i32_atomic_load16_u,
          a_i64_atomic_load8_u,
          a_i64_atomic_load16_u,
          a_i64_atomic_load32_u,
          a_i32_atomic_store,
          a_i64_atomic_store,
          a_i32_atomic_store8,
          a_i32_atomic_store16,
          a_i64_atomic_store8,
          a_i64_atomic_store16,
          a_i64_atomic_store32,
          a_i32_atomic_rmw_add,
          a_i64_atomic_rmw_add,
          a_i32_atomic_rmw8_add_u,
          a_i32_atomic_rmw16_add_u,
          a_i64_atomic_rmw8_add_u,
          a_i64_atomic_rmw16_add_u,
          a_i64_atomic_rmw32_add_u,
          a_i32_atomic_rmw_sub,
          a_i64_atomic_rmw_sub,
          a_i32_atomic_rmw8_sub_u,
          a_i32_atomic_rmw16_sub_u,
          a_i64_atomic_rmw8_sub_u,
          a_i64_atomic_rmw16_sub_u,
          a_i64_atomic_rmw32_sub_u,
          a_i32_atomic_rmw_and,
          a_i64_atomic_rmw_and,
          a_i32_atomic_rmw8_and_u,
          a_i32_atomic_rmw16_and_u,
          a_i64_atomic_rmw8_and_u,
          a_i64_atomic_rmw16_and_u,
          a_i64_atomic_rmw32_and_u,
          a_i32_atomic_rmw_or,
          a_i64_atomic_rmw_or,
          a_i32_atomic_rmw8_or_u,
          a_i32_atomic_rmw16_or_u,
          a_i64_atomic_rmw8_or_u,
          a_i64_atomic_rmw16_or_u,
          a_i64_atomic_rmw32_or_u,
          a_i32_atomic_rmw_xor,
          a_i64_atomic_rmw_xor,
          a_i32_atomic_rmw8_xor_u,
          a_i32_atomic_rmw16_xor_u,
          a_i64_atomic_rmw8_xor_u,
          a_i64_atomic_rmw16_xor_u,
          a_i64_atomic_rmw32_xor_u,
          a_i32_atomic_rmw_xchg,
          a_i64_atomic_rmw_xchg,
          a_i32_atomic_rmw8_xchg_u,
          a_i32_atomic_rmw16_xchg_u,
          a_i64_atomic_rmw8_xchg_u,
          a_i64_atomic_rmw16_xchg_u,
          a_i64_atomic_rmw32_xchg_u,
          a_i32_atomic_rmw_cmpxchg,
          a_i64_atomic_rmw_cmpxchg,
          a_i32_atomic_rmw8_cmpxchg_u,
          a_i32_atomic_rmw16_cmpxchg_u,
          a_i64_atomic_rmw8_cmpxchg_u,
          a_i64_atomic_rmw16_cmpxchg_u,
          a_i64_atomic_rmw32_cmpxchg_u:
            begin
              WriteByte($FE);
              case opcode of
                a_memory_atomic_notify:
                  WriteByte($00);
                a_memory_atomic_wait32:
                  WriteByte($01);
                a_memory_atomic_wait64:
                  WriteByte($02);
                a_i32_atomic_load:
                  WriteByte($10);
                a_i64_atomic_load:
                  WriteByte($11);
                a_i32_atomic_load8_u:
                  WriteByte($12);
                a_i32_atomic_load16_u:
                  WriteByte($13);
                a_i64_atomic_load8_u:
                  WriteByte($14);
                a_i64_atomic_load16_u:
                  WriteByte($15);
                a_i64_atomic_load32_u:
                  WriteByte($16);
                a_i32_atomic_store:
                  WriteByte($17);
                a_i64_atomic_store:
                  WriteByte($18);
                a_i32_atomic_store8:
                  WriteByte($19);
                a_i32_atomic_store16:
                  WriteByte($1A);
                a_i64_atomic_store8:
                  WriteByte($1B);
                a_i64_atomic_store16:
                  WriteByte($1C);
                a_i64_atomic_store32:
                  WriteByte($1D);
                a_i32_atomic_rmw_add:
                  WriteByte($1E);
                a_i64_atomic_rmw_add:
                  WriteByte($1F);
                a_i32_atomic_rmw8_add_u:
                  WriteByte($20);
                a_i32_atomic_rmw16_add_u:
                  WriteByte($21);
                a_i64_atomic_rmw8_add_u:
                  WriteByte($22);
                a_i64_atomic_rmw16_add_u:
                  WriteByte($23);
                a_i64_atomic_rmw32_add_u:
                  WriteByte($24);
                a_i32_atomic_rmw_sub:
                  WriteByte($25);
                a_i64_atomic_rmw_sub:
                  WriteByte($26);
                a_i32_atomic_rmw8_sub_u:
                  WriteByte($27);
                a_i32_atomic_rmw16_sub_u:
                  WriteByte($28);
                a_i64_atomic_rmw8_sub_u:
                  WriteByte($29);
                a_i64_atomic_rmw16_sub_u:
                  WriteByte($2A);
                a_i64_atomic_rmw32_sub_u:
                  WriteByte($2B);
                a_i32_atomic_rmw_and:
                  WriteByte($2C);
                a_i64_atomic_rmw_and:
                  WriteByte($2D);
                a_i32_atomic_rmw8_and_u:
                  WriteByte($2E);
                a_i32_atomic_rmw16_and_u:
                  WriteByte($2F);
                a_i64_atomic_rmw8_and_u:
                  WriteByte($30);
                a_i64_atomic_rmw16_and_u:
                  WriteByte($31);
                a_i64_atomic_rmw32_and_u:
                  WriteByte($32);
                a_i32_atomic_rmw_or:
                  WriteByte($33);
                a_i64_atomic_rmw_or:
                  WriteByte($34);
                a_i32_atomic_rmw8_or_u:
                  WriteByte($35);
                a_i32_atomic_rmw16_or_u:
                  WriteByte($36);
                a_i64_atomic_rmw8_or_u:
                  WriteByte($37);
                a_i64_atomic_rmw16_or_u:
                  WriteByte($38);
                a_i64_atomic_rmw32_or_u:
                  WriteByte($39);
                a_i32_atomic_rmw_xor:
                  WriteByte($3A);
                a_i64_atomic_rmw_xor:
                  WriteByte($3B);
                a_i32_atomic_rmw8_xor_u:
                  WriteByte($3C);
                a_i32_atomic_rmw16_xor_u:
                  WriteByte($3D);
                a_i64_atomic_rmw8_xor_u:
                  WriteByte($3E);
                a_i64_atomic_rmw16_xor_u:
                  WriteByte($3F);
                a_i64_atomic_rmw32_xor_u:
                  WriteByte($40);
                a_i32_atomic_rmw_xchg:
                  WriteByte($41);
                a_i64_atomic_rmw_xchg:
                  WriteByte($42);
                a_i32_atomic_rmw8_xchg_u:
                  WriteByte($43);
                a_i32_atomic_rmw16_xchg_u:
                  WriteByte($44);
                a_i64_atomic_rmw8_xchg_u:
                  WriteByte($45);
                a_i64_atomic_rmw16_xchg_u:
                  WriteByte($46);
                a_i64_atomic_rmw32_xchg_u:
                  WriteByte($47);
                a_i32_atomic_rmw_cmpxchg:
                  WriteByte($48);
                a_i64_atomic_rmw_cmpxchg:
                  WriteByte($49);
                a_i32_atomic_rmw8_cmpxchg_u:
                  WriteByte($4A);
                a_i32_atomic_rmw16_cmpxchg_u:
                  WriteByte($4B);
                a_i64_atomic_rmw8_cmpxchg_u:
                  WriteByte($4C);
                a_i64_atomic_rmw16_cmpxchg_u:
                  WriteByte($4D);
                a_i64_atomic_rmw32_cmpxchg_u:
                  WriteByte($4E);
                else
                  internalerror(2022052101);
              end;
              if ops<>1 then
                internalerror(2021092016);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if assigned(ref^.symbol) then
                        begin
                          WriteUleb(natural_alignment_for_load_store(opcode));
                          objdata.writeReloc(ref^.offset,5,ObjData.symbolref(ref^.symbol),RELOC_MEMORY_ADDR_LEB);
                        end
                      else
                        begin
                          if assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) then
                            internalerror(2021092018);
                          WriteUleb(natural_alignment_for_load_store(opcode));
                          WriteUleb(ref^.offset);
                        end;
                    end;
                  top_const:
                    begin
                      WriteUleb(natural_alignment_for_load_store(opcode));
                      WriteUleb(val);
                    end;
                  else
                    internalerror(2021092017);
                end;
            end;
          a_call:
            begin
              if ops<>1 then
                internalerror(2021092021);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092023);
                      WriteByte($10);
                      objdata.writeReloc(0,5,ObjData.symbolref(ref^.symbol),RELOC_FUNCTION_INDEX_LEB);
                    end;
                  else
                    internalerror(2021092022);
                end;
            end;
          a_call_indirect:
            begin
              if ops<>1 then
                internalerror(2021092610);
              with oper[0]^ do
                case typ of
                  top_functype:
                    begin
                      WriteByte($11);
                      objdata.writeReloc(TWasmObjData(objdata).AddFuncType(functype),5,nil,RELOC_TYPE_INDEX_LEB);
                      WriteUleb(0);
                    end;
                  else
                    internalerror(2021092611);
                end;
            end;
          a_br,
          a_br_if,
          a_rethrow,
          a_delegate:
            begin
              case opcode of
                a_br:
                  WriteByte($0C);
                a_br_if:
                  WriteByte($0D);
                a_rethrow:
                  WriteByte($09);
                a_delegate:
                  WriteByte($18);
                else
                  internalerror(2021092622);
              end;
              if ops<>1 then
                internalerror(2021092610);
              with oper[0]^ do
                case typ of
                  top_const:
                    WriteUleb(val);
                  else
                    internalerror(2021092625);
                end;
            end;
          a_catch,
          a_throw:
            begin
              case opcode of
                a_catch:
                  WriteByte($07);
                a_throw:
                  WriteByte($08);
                else
                  internalerror(2021092708);
              end;
              if ops<>1 then
                internalerror(2021092709);
              with oper[0]^ do
                case typ of
                  top_ref:
                    begin
                      if not assigned(ref^.symbol) or (ref^.base<>NR_NO) or (ref^.index<>NR_NO) or (ref^.offset<>0) then
                        internalerror(2021092711);
                      objdata.writeReloc(0,5,TWasmObjData(ObjData).ExceptionTagRef(ref^.symbol),RELOC_TAG_INDEX_LEB);
                    end;
                  else
                    internalerror(2021092710);
                end;
            end;
          a_memory_init:
            begin
              WriteByte($FC);
              WriteByte($08);
              if ops<>1 then
                internalerror(2022052806);
              with oper[0]^ do
                case typ of
                  top_const:
                    WriteUleb(val);
                  else
                    internalerror(2022052807);
                end;
              WriteByte($00);
            end;
          a_data_drop:
            begin
              WriteByte($FC);
              WriteByte($09);
              if ops<>1 then
                internalerror(2022052808);
              with oper[0]^ do
                case typ of
                  top_const:
                    WriteUleb(val);
                  else
                    internalerror(2022052809);
                end;
            end;
          a_i32_trunc_sat_f32_s:
            begin
              WriteByte($FC);
              WriteByte($00);
            end;
          a_i32_trunc_sat_f32_u:
            begin
              WriteByte($FC);
              WriteByte($01);
            end;
          a_i32_trunc_sat_f64_s:
            begin
              WriteByte($FC);
              WriteByte($02);
            end;
          a_i32_trunc_sat_f64_u:
            begin
              WriteByte($FC);
              WriteByte($03);
            end;
          a_i64_trunc_sat_f32_s:
            begin
              WriteByte($FC);
              WriteByte($04);
            end;
          a_i64_trunc_sat_f32_u:
            begin
              WriteByte($FC);
              WriteByte($05);
            end;
          a_i64_trunc_sat_f64_s:
            begin
              WriteByte($FC);
              WriteByte($06);
            end;
          a_i64_trunc_sat_f64_u:
            begin
              WriteByte($FC);
              WriteByte($07);
            end;
          else
            internalerror(2021092624);
        end;
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      begin
       internalerror(2010122614);
       result:=nil;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      begin
       internalerror(2010122615);
       result:=nil;
      end;


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;

initialization
  cai_cpu:=taicpu;
  cai_align:=tai_align;
end.
