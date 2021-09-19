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

      // the actual use is defined by the assembly section used

      { timpexp_ai }

      tai_impexp = class(tai)
        extname : ansistring; // external name
        intname : ansistring; // internal name
        extmodule : ansistring; // external unit name
        symstype: TImpExpType;
        constructor create(const aextname, aintname: ansistring; asymtype: timpexptype); overload;
        constructor create(const aextmodule, aextname, aintname: ansistring; asymtype: timpexptype); overload;
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

    procedure InitAsm;
    procedure DoneAsm;

    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

implementation

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

      constructor tai_impexp.create(const aextname, aintname: ansistring;
          asymtype: timpexptype);
        begin
          create('', aextname, aintname, asymtype);;
        end;

      constructor tai_impexp.create(const aextmodule, aextname, aintname: ansistring; asymtype: timpexptype);
        begin
          inherited create;
          typ := ait_importexport;
          extmodule := aextmodule;
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
          a_end_block,
          a_end_if,
          a_end_loop,
          a_end_try,
          a_catch_all:
            result:=1;
          a_memory_size,
          a_memory_grow:
            result:=2;
          a_i32_const,
          a_i64_const:
            begin
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_const:
                    result:=1+SlebSize(val);
                  else
                    Writeln('Warning! Not implemented opcode, pass1: ', opcode, ' ', typ);
                end;
            end;
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
                      if ref^.symbol.Name<>'__stack_pointer' then
                        internalerror(2021092014);
                      result:=1+UlebSize(0);
                    end;
                  else
                    internalerror(2021092011);
                end;
            end;
          a_end_function:
            result:=0;
          a_block:
            begin
              if ops=0 then
                result:=2
              else
                begin
                  if ops<>1 then
                    internalerror(2021092015);
                  Writeln('Warning! Not implemented opcode, pass2: ', opcode, ' ', typ);
                end;
            end;
          else
            Writeln('Warning! Not implemented opcode, pass1: ', opcode);
        end;
      end;


    procedure taicpu.Pass2(objdata: TObjData);

        procedure WriteByte(b: byte);
          begin
            objdata.writebytes(b,1);
          end;

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
          a_i32_const,
          a_i64_const:
            begin
              case opcode of
                a_i32_const:
                  WriteByte($41);
                a_i64_const:
                  WriteByte($42);
                else
                  internalerror(2021092002);
              end;
              if ops<>1 then
                internalerror(2021092001);
              with oper[0]^ do
                case typ of
                  top_const:
                    WriteSleb(val);
                  else
                    Writeln('Warning! Not implemented opcode, pass2: ', opcode, ' ', typ);
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
                      if ref^.symbol.Name<>'__stack_pointer' then
                        internalerror(2021092014);
                      WriteUleb(0);
                    end;
                  else
                    internalerror(2021092011);
                end;
            end;
          a_end_function:
            ;
          a_block:
            begin
              WriteByte($02);
              if ops=0 then
                WriteByte($40)
              else
                begin
                  if ops<>1 then
                    internalerror(2021092015);
                  Writeln('Warning! Not implemented opcode, pass2: ', opcode, ' ', typ);
                end;
            end;
          else
            Writeln('Warning! Not implemented opcode, pass2: ', opcode);
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
  casmdata:=TAsmData;
end.
