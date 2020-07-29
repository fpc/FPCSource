{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit implements the WebAssembly high level code generator

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
unit hlcgcpu;

{$i fpcdefs.inc}

interface

uses
  globtype,
  aasmbase,aasmdata,
  symbase,symconst,symtype,symdef,symsym,
  node,
  cpubase, hlcgobj, cgbase, cgutils, parabase;

  type

    { thlcgwasm }

    thlcgwasm = class(thlcgobj)
     private
      fevalstackheight,
      fmaxevalstackheight: longint;
     public
      constructor create;

      procedure incstack(list : TAsmList;slots: longint);
      procedure decstack(list : TAsmList;slots: longint);

      class function def2regtyp(def: tdef): tregistertype; override;

      procedure a_load_const_cgpara(list : TAsmList;tosize : tdef;a : tcgint;const cgpara : TCGPara);override;

      function a_call_name(list : TAsmList;pd : tprocdef;const s : TSymStr; const paras: array of pcgpara; forceresdef: tdef; weak: boolean): tcgpara;override;
      function a_call_name_inherited(list : TAsmList;pd : tprocdef;const s : TSymStr; const paras: array of pcgpara): tcgpara;override;
      function a_call_reg(list: TAsmList; pd: tabstractprocdef; reg: tregister; const paras: array of pcgpara): tcgpara; override;

      { move instructions - a_load_FROM_TO }
      procedure a_load_const_reg(list : TAsmList;tosize : tdef;a : tcgint;register : tregister);override;
      procedure a_load_const_ref(list : TAsmList;tosize : tdef;a : tcgint;const ref : treference);override;
      procedure a_load_reg_ref(list : TAsmList;fromsize, tosize : tdef;register : tregister;const ref : treference);override;
      procedure a_load_reg_reg(list : TAsmList;fromsize, tosize : tdef;reg1,reg2 : tregister);override;
      procedure a_load_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;register : tregister);override;
      procedure a_load_ref_ref(list : TAsmList;fromsize, tosize : tdef;const sref : treference;const dref : treference);override;
      procedure a_loadaddr_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;r : tregister);override;

      { basic arithmetic operations }
      procedure a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; reg: TRegister); override;
      procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister); override;
      procedure a_op_const_ref(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; const ref: TReference); override;

      procedure a_op_ref_reg(list: TAsmList; Op: TOpCG; size: tdef; const ref: TReference; reg: TRegister); override;
      procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister); override;
      procedure a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister); override;
      procedure a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister;setflags : boolean;var ovloc : tlocation); override;
      procedure a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation); override;

      procedure a_cmp_const_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; const ref: treference; l: tasmlabel); override;
      procedure a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel); override;
      procedure a_cmp_ref_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; const ref: treference; reg: tregister; l: tasmlabel); override;
      procedure a_cmp_reg_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg: tregister; const ref: treference; l: tasmlabel); override;
      procedure a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel); override;

      procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;

      procedure g_concatcopy(list : TAsmList;size: tdef; const source,dest : treference);override;
      procedure g_copyshortstring(list : TAsmList;const source,dest : treference;strdef:tstringdef);override;

      procedure a_loadfpu_ref_ref(list: TAsmList; fromsize, tosize: tdef; const ref1, ref2: treference); override;
      procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister); override;
      procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference); override;
      procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister); override;

      procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean); override;
      procedure g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean); override;

      procedure gen_load_return_value(list:TAsmList);override;
      procedure record_generated_code_for_procdef(pd: tprocdef; code, data: TAsmList); override;

      procedure g_incrrefcount(list : TAsmList;t: tdef; const ref: treference);override;
      procedure g_array_rtti_helper(list: TAsmList; t: tdef; const ref: treference; const highloc: tlocation; const name: string); override;
      procedure g_initialize(list : TAsmList;t : tdef;const ref : treference);override;
      procedure g_finalize(list : TAsmList;t : tdef;const ref : treference);override;

      procedure g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef); override;
      procedure g_overflowCheck_loc(List:TAsmList;const Loc:TLocation;def:TDef;var ovloc : tlocation); override;

      procedure location_get_data_ref(list:TAsmList;def: tdef; const l:tlocation;var ref:treference;loadref:boolean; alignment: longint);override;
      procedure maybe_change_load_node_reg(list: TAsmList; var n: tnode; reload: boolean); override;
      procedure g_copyvaluepara_openarray(list: TAsmList; const ref: treference; const lenloc: tlocation; arrdef: tarraydef; destreg: tregister); override;
      procedure g_releasevaluepara_openarray(list: TAsmList; arrdef: tarraydef; const l: tlocation); override;

      procedure gen_initialize_code(list: TAsmList); override;

      procedure gen_entry_code(list: TAsmList); override;
      procedure gen_exit_code(list: TAsmList); override;

      { unimplemented/unnecessary routines }
      procedure a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; srcsize, dstsize: tdef; src, dst: tregister); override;
      procedure a_loadmm_loc_reg(list: TAsmList; fromsize, tosize: tdef; const loc: tlocation; const reg: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference; shuffle: pmmshuffle); override;
      procedure a_opmm_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; src, dst: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_intreg_reg(list: TAsmList; fromsize, tosize: tdef; intreg, mmreg: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_reg_intreg(list: TAsmList; fromsize, tosize: tdef; mmreg, intreg: tregister; shuffle: pmmshuffle); override;
      procedure g_stackpointer_alloc(list: TAsmList; size: longint); override;
      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint); override;
      procedure g_adjust_self_value(list: TAsmList; procdef: tprocdef; ioffset: aint); override;
      procedure g_local_unwind(list: TAsmList; l: TAsmLabel); override;

      { Wasm-specific routines }

      procedure a_load_stack_reg(list : TAsmList;size: tdef;reg: tregister);
      { extra_slots are the slots that are used by the reference, and that
        will be removed by the store operation }
      procedure a_load_stack_ref(list : TAsmList;size: tdef;const ref: treference;extra_slots: longint);
      procedure a_load_reg_stack(list : TAsmList;size: tdef;reg: tregister);
      { extra_slots are the slots that are used by the reference, and that
        will be removed by the load operation }
      procedure a_load_ref_stack(list : TAsmList;size: tdef;const ref: treference;extra_slots: longint);
      procedure a_load_const_stack(list : TAsmList;size: tdef;a :tcgint; typ: TRegisterType);

      procedure a_load_stack_loc(list : TAsmList;size: tdef;const loc: tlocation);
      procedure a_load_loc_stack(list : TAsmList;size: tdef;const loc: tlocation);

      procedure a_loadfpu_const_stack(list : TAsmList;size: tdef;a :double);

      procedure a_op_stack(list : TAsmList;op: topcg; size: tdef; trunc32: boolean);
      procedure a_op_const_stack(list : TAsmList;op: topcg; size: tdef;a : tcgint);
      procedure a_op_reg_stack(list : TAsmList;op: topcg; size: tdef;reg: tregister);
      procedure a_op_ref_stack(list : TAsmList;op: topcg; size: tdef;const ref: treference);
      procedure a_op_loc_stack(list : TAsmList;op: topcg; size: tdef;const loc: tlocation);

      procedure g_reference_loc(list: TAsmList; def: tdef; const fromloc: tlocation; out toloc: tlocation); override;

      { assumes that initdim dimensions have already been pushed on the
        evaluation stack, and creates a new array of type arrdef with these
        dimensions }
      procedure g_newarray(list : TAsmList; arrdef: tdef; initdim: longint);
      { gets the length of the array whose reference is stored in arrloc,
        and puts it on the evaluation stack }
      procedure g_getarraylen(list : TAsmList; const arrloc: tlocation);

      { this routine expects that all values are already massaged into the
        required form (sign bits xor'ed for gt/lt comparisons for OS_32/OS_64,
        see http://stackoverflow.com/questions/4068973/c-performing-signed-comparison-in-unsigned-variables-without-casting ) }
      procedure a_cmp_stack_label(list : TAsmlist; size: tdef; cmp_op: topcmp; lab: tasmlabel);
      { these 2 routines perform the massaging expected by the previous one }
      procedure maybe_adjust_cmp_stackval(list : TAsmlist; size: tdef; cmp_op: topcmp);
      function maybe_adjust_cmp_constval(size: tdef; cmp_op: topcmp; a: tcgint): tcgint;
      { truncate/sign extend after performing operations on values < 32 bit
        that may have overflowed outside the range }
      procedure maybe_adjust_op_result(list: TAsmList; op: TOpCg; size: tdef);

      { performs sign/zero extension as required }
      procedure resize_stack_int_val(list: TAsmList;fromsize,tosize: tdef; formemstore: boolean);

      { 8/16 bit unsigned parameters and return values must be sign-extended on
        the producer side, because the JVM does not support unsigned variants;
        then they have to be zero-extended again on the consumer side }
      procedure maybe_resize_stack_para_val(list: TAsmList; retdef: tdef; callside: boolean);

      { adjust the stack height after a call based on the specified number of
        slots used for parameters and the provided resultdef }
      procedure g_adjust_stack_after_call(list: TAsmList; pd: tabstractprocdef; paraheight: longint; forceresdef: tdef);

      property maxevalstackheight: longint read fmaxevalstackheight;

      procedure gen_initialize_fields_code(list:TAsmList);

      procedure gen_typecheck(list: TAsmList; checkop: tasmop; checkdef: tdef);
     protected
      procedure a_load_const_stack_intern(list : TAsmList;size : tdef;a : tcgint; typ: TRegisterType; legalize_const: boolean);

      function get_enum_init_val_ref(def: tdef; out ref: treference): boolean;

      procedure allocate_implicit_structs_for_st_with_base_ref(list: TAsmList; st: tsymtable; const ref: treference; allocvartyp: tsymtyp);
      procedure allocate_enum_with_base_ref(list: TAsmList; vs: tabstractvarsym; const initref: treference; destbaseref: treference);
      procedure allocate_implicit_struct_with_base_ref(list: TAsmList; vs: tabstractvarsym; ref: treference);
      procedure gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara); override;

      procedure g_copyvalueparas(p: TObject; arg: pointer); override;

      procedure inittempvariables(list:TAsmList);override;

      function g_call_system_proc_intern(list: TAsmList; pd: tprocdef; const paras: array of pcgpara; forceresdef: tdef): tcgpara; override;

      { in case of an array, the array base address and index have to be
        put on the evaluation stack before the stored value; similarly, for
        fields the self pointer has to be loaded first. Also checks whether
        the reference is valid. If dup is true, the necessary values are stored
        twice. Returns how many stack slots have been consumed, disregarding
        the "dup". }
      function prepare_stack_for_ref(list: TAsmList; const ref: treference; dup: boolean): longint;
      { return the load/store opcode to load/store from/to ref; if the result
        has to be and'ed after a load to get the final value, that constant
        is returned in finishandval (otherwise that value is set to -1) }
      function loadstoreopcref(def: tdef; isload: boolean; const ref: treference; out finishandval: tcgint): tasmop;
      { return the load/store opcode to load/store from/to reg; if the result
        has to be and'ed after a load to get the final value, that constant
        is returned in finishandval (otherwise that value is set to -1) }
      function loadstoreopc(def: tdef; isload, isarray: boolean; out finishandval: tcgint): tasmop;
      procedure resizestackfpuval(list: TAsmList; fromsize, tosize: tcgsize);
      { in case of an OS_32 OP_DIV, we have to use an OS_S64 OP_IDIV because the
        JVM does not support unsigned divisions }
      procedure maybepreparedivu32(list: TAsmList; var op: topcg; size: tdef; out isdivu32: boolean);
      { common implementation of a_call_* }
      function a_call_name_intern(list : TAsmList;pd : tprocdef;const s : TSymStr; forceresdef: tdef; inheritedcall: boolean): tcgpara;

      { concatcopy helpers }
      procedure concatcopy_normal_array(list: TAsmList; size: tdef; const source, dest: treference);
      procedure concatcopy_record(list: TAsmList; size: tdef; const source, dest: treference);
      procedure concatcopy_set(list: TAsmList; size: tdef; const source, dest: treference);
      procedure concatcopy_shortstring(list: TAsmList; size: tdef; const source, dest: treference);

    end;

implementation

  uses
    verbose,cutils,globals,fmodule,constexp,
    defutil,
    aasmtai,aasmcpu,
    symtable,symcpu, wasmdef,
    procinfo,cpuinfo,cgcpu,tgobj;

  const
    TOpCG2IAsmOp : array[topcg] of TAsmOp=(
      A_None,      {OP_NONE}
      A_None,      {OP_MOVE, replaced operation with direct load }
      a_i32_add,   {OP_ADD,  simple addition          }
      a_i32_and,   {OP_AND,  simple logical and       }
      a_i32_div_u, {OP_DIV,  simple unsigned division }
      a_i32_div_s, {OP_IDIV, simple signed division   }
      a_i32_mul,   {OP_IMUL, simple signed multiply   }
      a_i32_mul,   {OP_MUL,  simple unsigned multiply }
      A_None,      {OP_NEG,  simple negate            } // neg = xor + 1
      A_None,      {OP_NOT,  simple logical not       } // not = xor - 1
      a_i32_or,    {OP_OR,   simple logical or        }
      a_i32_shr_s, {OP_SAR,  arithmetic shift-right   }
      a_i32_shr_s, {OP_SHL,  logical shift left       }
      a_i32_shr_u, {OP_SHR,  logical shift right      }
      a_i32_sub,   {OP_SUB,  simple subtraction       }
      a_i32_xor,   {OP_XOR,  simple exclusive or      }
      a_i32_rotl,  {OP_ROL,  rotate left              }
      a_i32_rotr   {OP_ROR   rotate right             }
    );
    TOpCG2LAsmOp : array[topcg] of TAsmOp=(
      A_None,      {OP_NONE}
      a_i64_load,  {OP_MOVE, replaced operation with direct load }
      a_i64_add,   {OP_ADD,  simple addition          }
      a_i64_and,   {OP_AND,  simple logical and       }
      a_i64_div_u, {OP_DIV,  simple unsigned division }
      a_i64_div_s, {OP_IDIV, simple signed division   }
      a_i64_mul,   {OP_IMUL, simple signed multiply   }
      a_i64_mul,   {OP_MUL,  simple unsigned multiply }
      A_None,      {OP_NEG,  simple negate            } // neg = xor + 1
      A_None,      {OP_NOT,  simple logical not       } // not = xor - 1
      a_i64_or,    {OP_OR,   simple logical or        }
      a_i64_shr_s, {OP_SAR,  arithmetic shift-right   }
      a_i64_shr_s, {OP_SHL,  logical shift left       }
      a_i64_shr_u, {OP_SHR,  logical shift right      }
      a_i64_sub,   {OP_SUB,  simple subtraction       }
      a_i64_xor,   {OP_XOR,  simple exclusive or      }
      a_i64_rotl,  {OP_ROL,  rotate left              }
      a_i64_rotr   {OP_ROR   rotate right             }
    );

  constructor thlcgwasm.create;
    begin
      fevalstackheight:=0;
      fmaxevalstackheight:=0;
    end;

  procedure thlcgwasm.incstack(list: TasmList;slots: longint);
    begin
      if slots=0 then
        exit;
      inc(fevalstackheight,slots);
      if (fevalstackheight>fmaxevalstackheight) then
        fmaxevalstackheight:=fevalstackheight;
      if cs_asm_regalloc in current_settings.globalswitches then
        list.concat(tai_comment.Create(strpnew('    allocated '+tostr(slots)+', stack height = '+tostr(fevalstackheight))));
    end;

  procedure thlcgwasm.decstack(list: TAsmList;slots: longint);
    begin
      if slots=0 then
        exit;
      dec(fevalstackheight,slots);
      if (fevalstackheight<0) and
         not(cs_no_regalloc in current_settings.globalswitches) then
        internalerror(2010120501);
      if cs_asm_regalloc in current_settings.globalswitches then
        list.concat(tai_comment.Create(strpnew('    freed '+tostr(slots)+', stack height = '+tostr(fevalstackheight))));
    end;

  class function thlcgwasm.def2regtyp(def: tdef): tregistertype;
    begin
      case def.typ of
        { records (including files) and enums are implemented via classes }
        recorddef,
        filedef,
        enumdef,
        setdef:
          result:=R_ADDRESSREGISTER;
        { shortstrings are implemented via classes }
        else if is_shortstring(def) or
        { voiddef can only be typecasted into (implicit) pointers }
                is_void(def) then
          result:=R_ADDRESSREGISTER
        else
          result:=inherited;
      end;
    end;

  procedure thlcgwasm.a_load_const_cgpara(list: TAsmList; tosize: tdef; a: tcgint; const cgpara: TCGPara);
    begin
      tosize:=get_para_push_size(tosize);
      if tosize=s8inttype then
        a:=shortint(a)
      else if tosize=s16inttype then
        a:=smallint(a);
      inherited a_load_const_cgpara(list, tosize, a, cgpara);
    end;

  function thlcgwasm.a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; const paras: array of pcgpara; forceresdef: tdef; weak: boolean): tcgpara;
    begin
      result:=a_call_name_intern(list,pd,s,forceresdef,false);
    end;

  function thlcgwasm.a_call_name_inherited(list: TAsmList; pd: tprocdef; const s: TSymStr; const paras: array of pcgpara): tcgpara;
    begin
      result:=a_call_name_intern(list,pd,s,nil,true);
    end;


  function thlcgwasm.a_call_reg(list: TAsmList; pd: tabstractprocdef; reg: tregister; const paras: array of pcgpara): tcgpara;
    begin
      internalerror(2012042824);
      result.init;
    end;


  procedure thlcgwasm.a_load_const_stack_intern(list : TAsmList;size : tdef;a : tcgint; typ: TRegisterType; legalize_const: boolean);
    begin
      if legalize_const and
         (typ=R_INTREGISTER) and
         (size.typ=orddef) then
        begin
          { uses specific byte/short array store instructions, and the Dalvik
            VM does not like it if we store values outside the range }
          case torddef(size).ordtype of
            u8bit:
              a:=shortint(a);
            u16bit:
              a:=smallint(a);
            else
              ;
          end;
        end;
      a_load_const_stack(list,size,a,typ);
    end;


  procedure thlcgwasm.a_load_const_stack(list : TAsmList;size : tdef;a : tcgint; typ: TRegisterType);
    begin
      case typ of
        R_INTREGISTER:
          begin
            case def_cgsize(size) of
              OS_8,OS_16,OS_32,
              OS_S8,OS_S16,OS_S32:
                begin
                  { convert cardinals to longints }
                  list.concat(taicpu.op_const(a_i32_const, a));
                end;
              OS_64,OS_S64:
                begin
                  list.concat(taicpu.op_const(a_i64_const, a));
                  incstack(list,1); // why?
                end;
              else
                internalerror(2010110702);
            end;
          end;
        R_ADDRESSREGISTER:
          begin
            if a<>0 then
              internalerror(2010110701);
            list.concat(taicpu.op_none(a_none));
          end;
        else
          internalerror(2010110703);
      end;
      incstack(list,1);
    end;

  procedure thlcgwasm.a_load_stack_loc(list: TAsmList; size: tdef; const loc: tlocation);
    begin
      case loc.loc of
        LOC_REGISTER,LOC_CREGISTER,
        LOC_FPUREGISTER,LOC_CFPUREGISTER:
          a_load_stack_reg(list,size,loc.register);
        LOC_REFERENCE:
          a_load_stack_ref(list,size,loc.reference,prepare_stack_for_ref(list,loc.reference,false));
        else
          internalerror(2011020501);
      end;
    end;

  procedure thlcgwasm.a_load_loc_stack(list: TAsmList;size: tdef;const loc: tlocation);
    begin
      case loc.loc of
        LOC_REGISTER,LOC_CREGISTER,
        LOC_FPUREGISTER,LOC_CFPUREGISTER:
          a_load_reg_stack(list,size,loc.register);
        LOC_REFERENCE,LOC_CREFERENCE:
          a_load_ref_stack(list,size,loc.reference,prepare_stack_for_ref(list,loc.reference,false));
        LOC_CONSTANT:
          a_load_const_stack(list,size,loc.value,def2regtyp(size));
        else
          internalerror(2011010401);
      end;
    end;

  procedure thlcgwasm.a_loadfpu_const_stack(list: TAsmList; size: tdef; a: double);
    begin
      case tfloatdef(size).floattype of
        s32real:
          begin
            list.concat(taicpu.op_single(a_f32_const, a));
            incstack(list,1);
          end;
        s64real:
          begin
            list.concat(taicpu.op_double(a_f64_const,a));
            incstack(list,2);
          end
        else
          internalerror(2011010501);
      end;
    end;

  procedure thlcgwasm.a_op_stack(list: TAsmList; op: topcg; size: tdef; trunc32: boolean);
    var
      cgsize: tcgsize;
    begin
      if not trunc32 then
        cgsize:=def_cgsize(size)
      else
        begin
          resize_stack_int_val(list,u32inttype,s64inttype,false);
          cgsize:=OS_S64;
        end;
      case cgsize of
        OS_8,OS_S8,
        OS_16,OS_S16,
        OS_32,OS_S32:
          begin
            { not = xor 1 for boolean, xor -1 for the rest}
            if op=OP_NOT then
              begin
                if not is_pasbool(size) then
                  a_load_const_stack(list,s32inttype,high(cardinal),R_INTREGISTER)
                else
                  a_load_const_stack(list,size,1,R_INTREGISTER);
                op:=OP_XOR;
              end;
            if TOpCG2IAsmOp[op]=A_None then
              internalerror(2010120532);
            list.concat(taicpu.op_none(TOpCG2IAsmOp[op]));
            maybe_adjust_op_result(list,op,size);
            if op<>OP_NEG then
              decstack(list,1);
          end;
        OS_64,OS_S64:
          begin
            { unsigned 64 bit division must be done via a helper }
            if op=OP_DIV then
              internalerror(2010120530);
            { not = xor 1 for boolean, xor -1 for the rest}
            if op=OP_NOT then
              begin
                if not is_pasbool(size) then
                  a_load_const_stack(list,s64inttype,-1,R_INTREGISTER)
                else
                  a_load_const_stack(list,s64inttype,1,R_INTREGISTER);
                op:=OP_XOR;
              end;
            if TOpCG2LAsmOp[op]=A_None then
              internalerror(2010120533);
            list.concat(taicpu.op_none(TOpCG2LAsmOp[op]));
            case op of
              OP_NOT,
              OP_NEG:
                ;
              { the second argument here is an int rather than a long }
              OP_SHL,OP_SHR,OP_SAR:
                decstack(list,1);
              else
                decstack(list,2);
            end;
          end;
        else
          internalerror(2010120531);
      end;
      if trunc32 then
        begin
          list.concat(taicpu.op_none(a_i32_trunc_s_f32)); // todo: there are several truncs
          decstack(list,1);
        end;
    end;

  procedure thlcgwasm.a_op_const_stack(list: TAsmList;op: topcg;size: tdef;a: tcgint);
    var
      trunc32: boolean;
    begin
      maybepreparedivu32(list,op,size,trunc32);
      case op of
        OP_NEG,OP_NOT:
          internalerror(2011010801);
        OP_SHL,OP_SHR,OP_SAR:
          { the second argument here is an int rather than a long }
          a_load_const_stack(list,s32inttype,a,R_INTREGISTER);
        else
          a_load_const_stack(list,size,a,R_INTREGISTER);
      end;
      a_op_stack(list,op,size,trunc32);
    end;

  procedure thlcgwasm.a_op_reg_stack(list: TAsmList; op: topcg; size: tdef; reg: tregister);
    var
      trunc32: boolean;
    begin
      maybepreparedivu32(list,op,size,trunc32);
      case op of
        OP_SHL,OP_SHR,OP_SAR:
          if not is_64bitint(size) then
            a_load_reg_stack(list,size,reg)
          else
            begin
              { the second argument here is an int rather than a long }
              if getsubreg(reg)=R_SUBQ then
                internalerror(2011010802);
              a_load_reg_stack(list,s32inttype,reg)
            end
        else
          a_load_reg_stack(list,size,reg);
      end;
      a_op_stack(list,op,size,trunc32);
    end;

  procedure thlcgwasm.a_op_ref_stack(list: TAsmList; op: topcg; size: tdef; const ref: treference);
    var
      trunc32: boolean;
    begin
      { ref must not be the stack top, because that may indicate an error
        (it means that we will perform an operation of the stack top onto
         itself, so that means the two values have been loaded manually prior
         to calling this routine, instead of letting this routine load one of
         them; if something like that is needed, call a_op_stack() directly) }
      if ref.base=NR_EVAL_STACK_BASE then
        internalerror(2010121102);
      maybepreparedivu32(list,op,size,trunc32);
      case op of
        OP_SHL,OP_SHR,OP_SAR:
          begin
            if not is_64bitint(size) then
              a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false))
            else
              a_load_ref_stack(list,s32inttype,ref,prepare_stack_for_ref(list,ref,false));
          end;
        else
          a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false));
      end;
      a_op_stack(list,op,size,trunc32);
    end;

  procedure thlcgwasm.a_op_loc_stack(list: TAsmList; op: topcg; size: tdef; const loc: tlocation);
    begin
      case loc.loc of
        LOC_REGISTER,LOC_CREGISTER:
          a_op_reg_stack(list,op,size,loc.register);
        LOC_REFERENCE,LOC_CREFERENCE:
          a_op_ref_stack(list,op,size,loc.reference);
        LOC_CONSTANT:
          a_op_const_stack(list,op,size,loc.value);
        else
          internalerror(2011011415)
      end;
    end;

  procedure thlcgwasm.g_reference_loc(list: TAsmList; def: tdef; const fromloc: tlocation; out toloc: tlocation);
    begin
      case fromloc.loc of
        LOC_CREFERENCE,
        LOC_REFERENCE:
          begin
            toloc:=fromloc;
            if (fromloc.reference.base<>NR_NO) and
               (fromloc.reference.base<>current_procinfo.framepointer) and
               (fromloc.reference.base<>NR_STACK_POINTER_REG) then
              g_allocload_reg_reg(list,voidpointertype,fromloc.reference.base,toloc.reference.base,R_ADDRESSREGISTER);
          end;
        else
          inherited;
      end;
    end;

  procedure thlcgwasm.g_newarray(list: TAsmList; arrdef: tdef; initdim: longint);
    var
      recref,
      enuminitref: treference;
      elemdef: tdef;
      i: longint;
      mangledname: string;
      opc: tasmop;
      primitivetype: boolean;
    begin
      internalerror(2019083001); // arrays are note yet supported

      (*
      elemdef:=arrdef;
      if initdim>1 then
        begin
          { multianewarray typedesc ndim }
          {  todo: WASM
          list.concat(taicpu.op_sym_const(a_multianewarray,
            current_asmdata.RefAsmSymbol(jvmarrtype(elemdef,primitivetype),AT_METADATA),initdim));
            }
          { has to be a multi-dimensional array type }
          if primitivetype then
            internalerror(2011012207);
        end
      else
        begin
          { for primitive types:
              newarray typedesc
            for reference types:
              anewarray typedesc
          }
          { get the type of the elements of the array we are creating }
          elemdef:=tarraydef(arrdef).elementdef;

          { todo: WASM. Todo: array data structures needs to be stored in Memory
          mangledname:=jvmarrtype(elemdef,primitivetype);
          if primitivetype then
            opc:=a_newarray
          else
            opc:=a_anewarray;
          list.concat(taicpu.op_sym(opc,current_asmdata.RefAsmSymbol(mangledname,AT_METADATA)));
          }
        end;
      { all dimensions are removed from the stack, an array reference is
        added }
      decstack(list,initdim-1);
      { in case of an array of records, sets or shortstrings, initialise }
      elemdef:=tarraydef(arrdef).elementdef;
      for i:=1 to pred(initdim) do
        elemdef:=tarraydef(elemdef).elementdef;
      if (elemdef.typ in [recorddef,setdef]) or
         ((elemdef.typ=enumdef) and
          get_enum_init_val_ref(elemdef,enuminitref)) or
         is_shortstring(elemdef) or
         ((elemdef.typ=procvardef) and
          not tprocvardef(elemdef).is_addressonly) or
         is_ansistring(elemdef) or
         is_wide_or_unicode_string(elemdef) or
         is_dynamic_array(elemdef) then
        begin
          { duplicate array instance }
          list.concat(taicpu.op_none(a_dup));
          incstack(list,1);
          a_load_const_stack(list,s32inttype,initdim-1,R_INTREGISTER);
          case elemdef.typ of
            arraydef:
              g_call_system_proc(list,'fpc_initialize_array_dynarr',[],nil);
            recorddef,setdef,procvardef:
              begin
                tg.gethltemp(list,elemdef,elemdef.size,tt_persistent,recref);
                a_load_ref_stack(list,elemdef,recref,prepare_stack_for_ref(list,recref,false));
                case elemdef.typ of
                  recorddef:
                    g_call_system_proc(list,'fpc_initialize_array_record',[],nil);
                  setdef:
                    begin
                      if tsetdef(elemdef).elementdef.typ=enumdef then
                        g_call_system_proc(list,'fpc_initialize_array_enumset',[],nil)
                      else
                        g_call_system_proc(list,'fpc_initialize_array_bitset',[],nil)
                    end;
                  procvardef:
                    g_call_system_proc(list,'fpc_initialize_array_procvar',[],nil);
                  else
                    internalerror(2019051025);
                end;
                tg.ungettemp(list,recref);
              end;
            enumdef:
              begin
                a_load_ref_stack(list,java_jlobject,enuminitref,prepare_stack_for_ref(list,enuminitref,false));
                g_call_system_proc(list,'fpc_initialize_array_object',[],nil);
              end;
            stringdef:
              begin
                case tstringdef(elemdef).stringtype of
                  st_shortstring:
                    begin
                      a_load_const_stack_intern(list,u8inttype,tstringdef(elemdef).len,R_INTREGISTER,true);
                      g_call_system_proc(list,'fpc_initialize_array_shortstring',[],nil);
                    end;
                  st_ansistring:
                    g_call_system_proc(list,'fpc_initialize_array_ansistring',[],nil);
                  st_unicodestring,
                  st_widestring:
                    g_call_system_proc(list,'fpc_initialize_array_unicodestring',[],nil);
                  else
                    internalerror(2011081801);
                end;
              end;
            else
              internalerror(2011081801);
          end;
        end;
        *)
    end;

  procedure thlcgwasm.g_getarraylen(list: TAsmList; const arrloc: tlocation);
    var
      nillab,endlab: tasmlabel;
    begin
      internalerror(2019083001); // arrays are note yet supported
      (*
      { inline because we have to use the arraylength opcode, which
        cannot be represented directly in Pascal. Even though the JVM
        supports allocated arrays with length=0, we still also have to
        check for nil pointers because even if FPC always generates
        allocated empty arrays under all circumstances, external Java
        code could pass in nil pointers.

        Note that this means that assigned(arr) can be different from
        length(arr)<>0 for dynamic arrays when targeting the JVM.
      }
      current_asmdata.getjumplabel(nillab);
      current_asmdata.getjumplabel(endlab);

      { if assigned(arr) ... }
      a_load_loc_stack(list,java_jlobject,arrloc);
      list.concat(taicpu.op_none(a_dup));
      incstack(list,1);
      list.concat(taicpu.op_sym(a_ifnull,nillab));
      decstack(list,1);

      { ... then result:=arraylength(arr) ... }
      list.concat(taicpu.op_none(a_arraylength));
      a_jmp_always(list,endlab);

      { ... else result:=0 }
      a_label(list,nillab);
      list.concat(taicpu.op_none(a_pop));
      decstack(list,1);
      list.concat(taicpu.op_none(a_iconst_0));
      incstack(list,1);

      a_label(list,endlab);
      *)
    end;

    procedure thlcgwasm.a_cmp_stack_label(list: TAsmlist; size: tdef; cmp_op: topcmp; lab: tasmlabel);
      const
        opcmp32: array[topcmp] of tasmop = (
          A_None,     { OC_NONE, }
          a_i32_eq,   { OC_EQ,   equality comparison              }
          a_i32_gt_s, { OC_GT,   greater than (signed)            }
          a_i32_lt_s, { OC_LT,   less than (signed)               }
          a_i32_ge_s, { OC_GTE,  greater or equal than (signed)   }
          a_i32_le_s, { OC_LTE,  less or equal than (signed)      }
          a_i32_ne,   { OC_NE,   not equal                        }
          a_i32_le_u, { OC_BE,   less or equal than (unsigned)    }
          a_i32_lt_u, { OC_B,    less than (unsigned)             }
          a_i32_ge_u, { OC_AE,   greater or equal than (unsigned) }
          a_i32_gt_u  { OC_A     greater than (unsigned)          }
        );
      const
        opcmp64: array[TOpCmp] of TAsmOp = (A_None,
           a_i64_eq,               // OC_EQ
           a_i64_gt_s, a_i64_lt_s, // OC_GT, OC_LT
           a_i64_ge_s, a_i64_le_s, // OC_GTE, OC_LTE
           a_i64_ne,               // OC_NE
           a_i64_le_u, a_i64_lt_u, // OC_BE, OC_B
           a_i64_ge_u, a_i64_gt_u  // OC_AE, OC_A
        );

      var
        cgsize: tcgsize;
      begin
        // WASM doesn't have compare+jump (to label) operation
        // thus even though this is a_cmp_stack_label()
        // label operrand is ommited
        //
        // todo: it should NOT be ommitted when we're leaving a block
        // (i.e. Exit or break or continue operators)

        case def2regtyp(size) of
          R_INTREGISTER:
            begin
              cgsize:=def_cgsize(size);
              case cgsize of
                OS_S8,OS_8,
                OS_16,OS_S16,
                OS_S32,OS_32:
                  begin
                    //list.concat(taicpu.op_sym(opcmp32[cmp_op],lab));
                    list.concat(taicpu.op_none(opcmp32[cmp_op]));
                    decstack(list,2);
                  end;
                OS_64,OS_S64:
                  begin
                    //list.concat(taicpu.op_none(a_lcmp));
                    //decstack(list,3);
                    //list.concat(taicpu.op_sym(opcmp64[cmp_op],lab));
                    list.concat(taicpu.op_none(opcmp64[cmp_op]));
                    decstack(list,2);
                  end;
                else
                  internalerror(2010120538);
              end;
            end;
          R_ADDRESSREGISTER:
            begin
              case cmp_op of
                OC_EQ:
                  //list.concat(taicpu.op_sym(a_i64_eq,lab));
                  list.concat(taicpu.op_none(a_i32_eq));
                OC_NE:
                  //list.concat(taicpu.op_sym(a_i64_ne,lab));
                  list.concat(taicpu.op_none(a_i32_ne));
                else
                  internalerror(2010120537);
              end;
              decstack(list,2);
            end;
          else
            internalerror(2010120538);
        end;
      end;

    procedure thlcgwasm.maybe_adjust_cmp_stackval(list: TAsmlist; size: tdef; cmp_op: topcmp);
      begin
        { use cmp_op because eventually that's what indicates the
          signed/unsigned character of the operation, not the size... }
        if (cmp_op in [OC_EQ,OC_NE,OC_LT,OC_LTE,OC_GT,OC_GTE]) or
           (def2regtyp(size)<>R_INTREGISTER) then
          exit;
        { http://stackoverflow.com/questions/4068973/c-performing-signed-comparison-in-unsigned-variables-without-casting }
        case def_cgsize(size) of
          OS_32,OS_S32:
            a_op_const_stack(list,OP_XOR,size,cardinal($80000000));
          OS_64,OS_S64:
            a_op_const_stack(list,OP_XOR,size,tcgint($8000000000000000));
          else
            ;
        end;
      end;

    function thlcgwasm.maybe_adjust_cmp_constval(size: tdef; cmp_op: topcmp; a: tcgint): tcgint;
      begin
        result:=a;
        { use cmp_op because eventually that's what indicates the
          signed/unsigned character of the operation, not the size... }
        if (cmp_op in [OC_EQ,OC_NE,OC_LT,OC_LTE,OC_GT,OC_GTE]) or
           (def2regtyp(size)<>R_INTREGISTER) then
          exit;
        case def_cgsize(size) of
          OS_32,OS_S32:
            result:=a xor cardinal($80000000);
          OS_64,OS_S64:
{$push}{$r-}
            result:=a xor tcgint($8000000000000000);
{$pop}
          else
            ;
        end;
      end;

    procedure thlcgwasm.maybe_adjust_op_result(list: TAsmList; op: TOpCg; size: tdef);
      const
        overflowops = [OP_MUL,OP_SHL,OP_ADD,OP_SUB,OP_NOT,OP_NEG];
      begin
        if (op in overflowops) and
           (def_cgsize(size) in [OS_8,OS_S8,OS_16,OS_S16]) then
          resize_stack_int_val(list,s32inttype,size,false);
      end;

  procedure thlcgwasm.gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara);
    begin
      { constructors don't return anything in Java }
      if pd.proctypeoption=potype_constructor then
        exit;
      { must return a value of the correct type on the evaluation stack }
      case def2regtyp(resdef) of
        R_INTREGISTER,
        R_ADDRESSREGISTER:
          a_load_const_cgpara(list,resdef,0,resloc);
        R_FPUREGISTER:
          case tfloatdef(resdef).floattype of
            s32real:
              begin
                list.concat(taicpu.op_single(a_f32_const, 0));
                incstack(list,1);
              end;
            s64real:
              begin
                list.concat(taicpu.op_double(a_f64_const, 0));
                incstack(list,2);
              end;
            else
              internalerror(2011010302);
          end
        else
          internalerror(2011010301);
      end;
    end;


  procedure thlcgwasm.g_copyvalueparas(p: TObject; arg: pointer);
    var
      list: tasmlist;
      tmpref: treference;
    begin
      { zero-extend < 32 bit primitive types (FPC can zero-extend when calling,
        but that doesn't help when we're called from Java code or indirectly
        as a procvar -- exceptions: widechar (Java-specific type) and ordinal
        types whose upper bound does not set the sign bit }
      if (tsym(p).typ=paravarsym) and
         (tparavarsym(p).varspez in [vs_value,vs_const]) and
         (tparavarsym(p).vardef.typ=orddef) and
         not is_pasbool(tparavarsym(p).vardef) and
         not is_widechar(tparavarsym(p).vardef) and
         (tparavarsym(p).vardef.size<4) and
         not is_signed(tparavarsym(p).vardef) and
         (torddef(tparavarsym(p).vardef).high>=(1 shl (tparavarsym(p).vardef.size*8-1))) then
        begin
          list:=TAsmList(arg);
          { store value in new location to keep Android verifier happy }
          tg.gethltemp(list,tparavarsym(p).vardef,tparavarsym(p).vardef.size,tt_persistent,tmpref);
          a_load_loc_stack(list,tparavarsym(p).vardef,tparavarsym(p).initialloc);
          a_op_const_stack(list,OP_AND,tparavarsym(p).vardef,(1 shl (tparavarsym(p).vardef.size*8))-1);
          a_load_stack_ref(list,tparavarsym(p).vardef,tmpref,prepare_stack_for_ref(list,tmpref,false));
          location_reset_ref(tparavarsym(p).localloc,LOC_REFERENCE,def_cgsize(tparavarsym(p).vardef),4,tmpref.volatility);
          tparavarsym(p).localloc.reference:=tmpref;
        end;

      inherited g_copyvalueparas(p, arg);
    end;


  procedure thlcgwasm.inittempvariables(list: TAsmList);
    begin
      { these are automatically initialised when allocated if necessary }
    end;


  function thlcgwasm.g_call_system_proc_intern(list: TAsmList; pd: tprocdef; const paras: array of pcgpara; forceresdef: tdef): tcgpara;
    begin
      result:=inherited;
      pd.init_paraloc_info(callerside);
      g_adjust_stack_after_call(list,pd,pd.callerargareasize,forceresdef);
    end;


  function thlcgwasm.prepare_stack_for_ref(list: TAsmList; const ref: treference; dup: boolean): longint;
    var
      href: treference;
    begin
      result:=0;
      { fake location that indicates the value is already on the stack? }
      if (ref.base=NR_EVAL_STACK_BASE) or (ref.islocal) then
        exit;

      // setting up memory offset
      if assigned(ref.symbol) then
        list.Concat(taicpu.op_sym(a_get_global, ref.symbol))
      else if ref.index <> NR_NO then // array access
      begin
        // it's just faster to sum two of those together
        list.Concat(taicpu.op_reg(a_get_local, ref.base));
        list.Concat(taicpu.op_reg(a_get_local, ref.index));
        list.Concat(taicpu.op_none(a_i32_add));
        exit;
      end
      else
        list.Concat(taicpu.op_const(a_i32_const, 0)); //todo: this should not be 0, this should be reference to a global "memory"

        if (ref.base<>NR_NO) then
          begin
            if (ref.base<>NR_STACK_POINTER_REG) then
              begin
                { regular field -> load self on the stack }
                a_load_reg_stack(list,voidpointertype,ref.base);
                if dup then
                  begin
                    internalerror(2019083002);
                    //todo: add duplicate
                    //list.concat(taicpu.op_none(a_dup));
                    incstack(list,1);
                  end;
                { field name/type encoded in symbol, no index/offset }
                result:=1;
              end
            else
              begin
                { local variable -> offset encoded in opcode and nothing to
                  do here, except for checking that it's a valid reference }
                if assigned(ref.symbol) then
                  internalerror(2010120523);
              end;
          end
        else
          begin
            { static field -> nothing to do here, except for validity check }
            {if not assigned(ref.symbol) or
               (ref.offset<>0) then
            begin
              internalerror(2010120525);
            end;}
          end;
    end;

  procedure thlcgwasm.a_load_const_reg(list: TAsmList; tosize: tdef; a: tcgint; register: tregister);
    begin
      a_load_const_stack(list,tosize,a,def2regtyp(tosize));
      a_load_stack_reg(list,tosize,register);
    end;

  procedure thlcgwasm.a_load_const_ref(list: TAsmList; tosize: tdef; a: tcgint; const ref: treference);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_const_stack_intern(list,tosize,a,def2regtyp(tosize),assigned(ref.symbol));
      a_load_stack_ref(list,tosize,ref,extra_slots);
    end;

  procedure thlcgwasm.a_load_reg_ref(list: TAsmList; fromsize, tosize: tdef; register: tregister; const ref: treference);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_reg_stack(list,fromsize,register);
      if def2regtyp(fromsize)=R_INTREGISTER then
        resize_stack_int_val(list,fromsize,tosize,assigned(ref.symbol));
      a_load_stack_ref(list,tosize,ref,extra_slots);
    end;

  procedure thlcgwasm.a_load_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    begin
      a_load_reg_stack(list,fromsize,reg1);
      if def2regtyp(fromsize)=R_INTREGISTER then
        resize_stack_int_val(list,fromsize,tosize,false);
      a_load_stack_reg(list,tosize,reg2);
    end;

  procedure thlcgwasm.a_load_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; register: tregister);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_ref_stack(list,fromsize,ref,extra_slots);

      if def2regtyp(fromsize)=R_INTREGISTER then
        resize_stack_int_val(list,fromsize,tosize,false);
      a_load_stack_reg(list,tosize,register);
    end;

  procedure thlcgwasm.a_load_ref_ref(list: TAsmList; fromsize, tosize: tdef; const sref: treference; const dref: treference);
    var
      extra_sslots,
      extra_dslots: longint;
    begin
      { make sure the destination reference is on top, since in the end the
        order has to be "destref, value" -> first create "destref, sourceref" }
      extra_dslots:=prepare_stack_for_ref(list,dref,false);
      extra_sslots:=prepare_stack_for_ref(list,sref,false);
      a_load_ref_stack(list,fromsize,sref,extra_sslots);
      if def2regtyp(fromsize)=R_INTREGISTER then
        resize_stack_int_val(list,fromsize,tosize,assigned(dref.symbol));
      a_load_stack_ref(list,tosize,dref,extra_dslots);
    end;

  procedure thlcgwasm.a_loadaddr_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; r: tregister);
    begin
      { only allowed for types that are not implicit pointers in Pascal (in
        that case, ref contains a pointer to the actual data and we simply
        return that pointer) }
      if not wasmimplicitpointertype(fromsize) then
        internalerror(2010120534);
      a_load_ref_reg(list,ptruinttype,ptruinttype,ref,r);
    end;

  procedure thlcgwasm.a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; reg: TRegister);
    begin
      a_op_const_reg_reg(list,op,size,a,reg,reg);
    end;

  procedure thlcgwasm.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister);
    begin
      a_load_reg_stack(list,size,src);
      a_op_const_stack(list,op,size,a);
      a_load_stack_reg(list,size,dst);
    end;

  procedure thlcgwasm.a_op_const_ref(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; const ref: TReference);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,true);
      { TODO, here or in peepholeopt: use iinc when possible }
      a_load_ref_stack(list,size,ref,extra_slots);
      a_op_const_stack(list,op,size,a);
      { for android verifier }
      if (def2regtyp(size)=R_INTREGISTER) and
         (assigned(ref.symbol)) then
        resize_stack_int_val(list,size,size,true);
      a_load_stack_ref(list,size,ref,extra_slots);
    end;

  procedure thlcgwasm.a_op_ref_reg(list: TAsmList; Op: TOpCG; size: tdef; const ref: TReference; reg: TRegister);
    begin
      if not(op in [OP_NOT,OP_NEG]) then
        a_load_reg_stack(list,size,reg);
      a_op_ref_stack(list,op,size,ref);
      a_load_stack_reg(list,size,reg);
    end;

  procedure thlcgwasm.a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister);
    begin
      if not(op in [OP_NOT,OP_NEG]) then
        a_load_reg_stack(list,size,src2);
      a_op_reg_stack(list,op,size,src1);
      a_load_stack_reg(list,size,dst);
    end;

  procedure thlcgwasm.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister);
    begin
      a_op_reg_reg_reg(list,op,size,reg1,reg2,reg2);
    end;

  procedure thlcgwasm.a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister; setflags: boolean; var ovloc: tlocation);
    var
      tmpreg: tregister;
    begin
      if not setflags then
        begin
          inherited;
          exit;
        end;
      tmpreg:=getintregister(list,size);
      a_load_const_reg(list,size,a,tmpreg);
      a_op_reg_reg_reg_checkoverflow(list,op,size,tmpreg,src,dst,true,ovloc);
    end;

  procedure thlcgwasm.a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister; setflags: boolean; var ovloc: tlocation);
    var
      orgsrc1, orgsrc2: tregister;
      docheck: boolean;
      lab: tasmlabel;
    begin
      if not setflags then
        begin
          inherited;
          exit;
        end;
      { anything else cannot overflow }
      docheck:=size.size in [4,8];
      if docheck then
        begin
          orgsrc1:=src1;
          orgsrc2:=src2;
          if src1=dst then
            begin
              orgsrc1:=getintregister(list,size);
              a_load_reg_reg(list,size,size,src1,orgsrc1);
            end;
          if src2=dst then
            begin
              orgsrc2:=getintregister(list,size);
              a_load_reg_reg(list,size,size,src2,orgsrc2);
            end;
        end;
      a_op_reg_reg_reg(list,op,size,src1,src2,dst);
      if docheck then
        begin
          { * signed overflow for addition iff
             - src1 and src2 are negative and result is positive (excep in case of
               subtraction, then sign of src1 has to be inverted)
             - src1 and src2 are positive and result is negative
              -> Simplified boolean equivalent (in terms of sign bits):
                 not(src1 xor src2) and (src1 xor dst)

             for subtraction, multiplication: invert src1 sign bit
             for division: handle separately (div by zero, low(inttype) div -1),
               not supported by this code

            * unsigned overflow iff carry out, aka dst < src1 or dst < src2
          }
          location_reset(ovloc,LOC_REGISTER,OS_S32);
          { not pasbool8, because then we'd still have to convert the integer to
            a boolean via branches for Dalvik}
          ovloc.register:=getintregister(list,s32inttype);
          if not ((size.typ=pointerdef) or
                 ((size.typ=orddef) and
                  (torddef(size).ordtype in [u64bit,u16bit,u32bit,u8bit,uchar,
                                             pasbool1,pasbool8,pasbool16,pasbool32,pasbool64]))) then
            begin
              a_load_reg_stack(list,size,src1);
              if op in [OP_SUB,OP_IMUL] then
                a_op_stack(list,OP_NOT,size,false);
              a_op_reg_stack(list,OP_XOR,size,src2);
              a_op_stack(list,OP_NOT,size,false);
              a_load_reg_stack(list,size,src1);
              a_op_reg_stack(list,OP_XOR,size,dst);
              a_op_stack(list,OP_AND,size,false);
              a_op_const_stack(list,OP_SHR,size,(size.size*8)-1);
              if size.size=8 then
                begin
                  //todo: any operands needed?
                  list.concat(taicpu.op_none(a_i32_wrap_i64));
                  decstack(list,1);
                end;
            end
          else
            begin
              a_load_const_stack(list,s32inttype,0,R_INTREGISTER);
              current_asmdata.getjumplabel(lab);
              { can be optimized by removing duplicate xor'ing to convert dst from
                signed to unsigned quadrant }
              a_cmp_reg_reg_label(list,size,OC_B,dst,src1,lab);
              a_cmp_reg_reg_label(list,size,OC_B,dst,src2,lab);
              a_op_const_stack(list,OP_XOR,s32inttype,1);
              a_label(list,lab);
            end;
          a_load_stack_reg(list,s32inttype,ovloc.register);
        end
      else
        ovloc.loc:=LOC_VOID;
    end;

  procedure thlcgwasm.a_cmp_const_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; const ref: treference; l: tasmlabel);
    begin
      if ref.base<>NR_EVAL_STACK_BASE then
        a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false));
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_const_stack(list,size,maybe_adjust_cmp_constval(size,cmp_op,a),def2regtyp(size));
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgwasm.a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel);
    begin
      a_load_reg_stack(list,size,reg);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_const_stack(list,size,maybe_adjust_cmp_constval(size,cmp_op,a),def2regtyp(size));
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgwasm.a_cmp_ref_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; const ref: treference; reg: tregister; l: tasmlabel);
    begin
      a_load_reg_stack(list,size,reg);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      if ref.base<>NR_EVAL_STACK_BASE then
        a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false))
      else begin
        // todo: need a swap operation?
        //list.concat(taicpu.op_none(a_swap));
        Internalerror(2019083003);
      end;
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgwasm.a_cmp_reg_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg: tregister; const ref: treference; l: tasmlabel);
    begin
      if ref.base<>NR_EVAL_STACK_BASE then
        a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false));
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_reg_stack(list,size,reg);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgwasm.a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel);
    begin
      a_load_reg_stack(list,size,reg2);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_reg_stack(list,size,reg1);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgwasm.a_jmp_always(list: TAsmList; l: tasmlabel);
    begin
      //todo: for proper jumping it's necessary to check
      //      all active blocks (if, block, loops)
      //      and jump to the proper one.
      //list.concat(taicpu.op_const(a_i32_const, 0));
      if l = current_procinfo.CurrBreakLabel then begin
        // todo: this should be moved to node generator pass2
        list.concat(taicpu.op_const(a_i32_const, 0));
        list.concat(taicpu.op_const(a_br,1))
      end else if l = current_procinfo.CurrContinueLabel then begin
        list.concat(taicpu.op_const(a_i32_const, 0));
        list.concat(taicpu.op_const(a_br,0))
      end else
        //Internalerror(2019091806); // unexpected jump
    end;

  procedure thlcgwasm.concatcopy_normal_array(list: TAsmList; size: tdef; const source, dest: treference);
    var
      procname: string;
      eledef: tdef;
      ndim: longint;
      adddefaultlenparas: boolean;
    begin
      { load copy helper parameters on the stack }
      a_load_ref_stack(list,ptruinttype,source,prepare_stack_for_ref(list,source,false));
      a_load_ref_stack(list,ptruinttype,dest,prepare_stack_for_ref(list,dest,false));
      { call copy helper }
      eledef:=tarraydef(size).elementdef;
      ndim:=1;
      adddefaultlenparas:=true;
      case eledef.typ of
        orddef:
          begin
            case torddef(eledef).ordtype of
              pasbool1,pasbool8,s8bit,u8bit,bool8bit,uchar,
              s16bit,u16bit,bool16bit,pasbool16,
              uwidechar,
              s32bit,u32bit,bool32bit,pasbool32,
              s64bit,u64bit,bool64bit,pasbool64,scurrency:
                procname:='FPC_COPY_SHALLOW_ARRAY'
              else
                internalerror(2011020504);
            end;
          end;
        arraydef:
          begin
            { call fpc_setlength_dynarr_multidim with deepcopy=true, and extra
              parameters }
            while (eledef.typ=arraydef) and
                  not is_dynamic_array(eledef) do
              begin
                eledef:=tarraydef(eledef).elementdef;
                inc(ndim)
              end;
            if (ndim=1) then
              procname:='FPC_COPY_SHALLOW_ARRAY'
            else
              begin
                { deepcopy=true }
                a_load_const_stack(list,pasbool1type,1,R_INTREGISTER);
                { ndim }
                a_load_const_stack(list,s32inttype,ndim,R_INTREGISTER);
                { eletype }
                { todo: WASM
                a_load_const_stack(list,cwidechartype,ord(jvmarrtype_setlength(eledef)),R_INTREGISTER);
                }
                adddefaultlenparas:=false;
                procname:='FPC_SETLENGTH_DYNARR_MULTIDIM';
              end;
          end;
        recorddef:
          procname:='FPC_COPY_JRECORD_ARRAY';
        procvardef:
          if tprocvardef(eledef).is_addressonly then
            procname:='FPC_COPY_SHALLOW_ARRAY'
          else
            procname:='FPC_COPY_JPROCVAR_ARRAY';
        setdef:
          if tsetdef(eledef).elementdef.typ=enumdef then
            procname:='FPC_COPY_JENUMSET_ARRAY'
          else
            procname:='FPC_COPY_JBITSET_ARRAY';
        floatdef:
          procname:='FPC_COPY_SHALLOW_ARRAY';
        stringdef:
          if is_shortstring(eledef) then
            procname:='FPC_COPY_JSHORTSTRING_ARRAY'
          else
            procname:='FPC_COPY_SHALLOW_ARRAY';
        variantdef:
          begin
{$ifndef nounsupported}
            procname:='FPC_COPY_SHALLOW_ARRAY';
{$else}
            { todo: make a deep copy via clone... }
            internalerror(2011020505);
{$endif}
          end;
        else
          procname:='FPC_COPY_SHALLOW_ARRAY';
      end;
     if adddefaultlenparas then
       begin
         { -1, -1 means "copy entire array" }
         a_load_const_stack(list,s32inttype,-1,R_INTREGISTER);
         a_load_const_stack(list,s32inttype,-1,R_INTREGISTER);
       end;
     g_call_system_proc(list,procname,[],nil);
     if ndim<>1 then
       begin
         { pop return value, must be the same as dest }
         //list.concat(taicpu.op_none(a_pop));
         Internalerror(2019083001); // no support for arrays
         decstack(list,1);
       end;
    end;

    procedure thlcgwasm.concatcopy_record(list: TAsmList; size: tdef; const source, dest: treference);
      var
        srsym: tsym;
        pd: tprocdef;
      begin
        { self }
        a_load_ref_stack(list,size,source,prepare_stack_for_ref(list,source,false));
        { result }
        a_load_ref_stack(list,size,dest,prepare_stack_for_ref(list,dest,false));
        { call fpcDeepCopy helper }
        srsym:=search_struct_member(tabstractrecorddef(size),'FPCDEEPCOPY');
        if not assigned(srsym) or
           (srsym.typ<>procsym) then
          Message1(cg_f_unknown_compilerproc,size.typename+'.fpcDeepCopy');
        pd:=tprocdef(tprocsym(srsym).procdeflist[0]);
        a_call_name(list,pd,pd.mangledname,[],nil,false);
        { both parameters are removed, no function result }
        decstack(list,2);
      end;


    procedure thlcgwasm.concatcopy_set(list: TAsmList; size: tdef; const source, dest: treference);
      begin
        a_load_ref_stack(list,size,source,prepare_stack_for_ref(list,source,false));
        a_load_ref_stack(list,size,dest,prepare_stack_for_ref(list,dest,false));
        { call set copy helper }
        if tsetdef(size).elementdef.typ=enumdef then
          g_call_system_proc(list,'fpc_enumset_copy',[],nil)
        else
          g_call_system_proc(list,'fpc_bitset_copy',[],nil);
      end;


    procedure thlcgwasm.concatcopy_shortstring(list: TAsmList; size: tdef; const source, dest: treference);
      var
        srsym: tsym;
        pd: tprocdef;
      begin
        { self }
        a_load_ref_stack(list,size,source,prepare_stack_for_ref(list,source,false));
        { result }
        a_load_ref_stack(list,size,dest,prepare_stack_for_ref(list,dest,false));
        { call fpcDeepCopy helper }
        srsym:=search_struct_member(java_shortstring,'FPCDEEPCOPY');
        if not assigned(srsym) or
           (srsym.typ<>procsym) then
          Message1(cg_f_unknown_compilerproc,'ShortstringClass.FpcDeepCopy');
        pd:=tprocdef(tprocsym(srsym).procdeflist[0]);
        a_call_name(list,pd,pd.mangledname,[],nil,false);
        { both parameters are removed, no function result }
        decstack(list,2);
      end;


  procedure thlcgwasm.g_concatcopy(list: TAsmList; size: tdef; const source, dest: treference);
    var
      handled: boolean;
    begin
      handled:=false;
      case size.typ of
        arraydef:
          begin
            if not is_dynamic_array(size) then
              begin
                concatcopy_normal_array(list,size,source,dest);
                handled:=true;
              end;
          end;
        recorddef:
          begin
            concatcopy_record(list,size,source,dest);
            handled:=true;
          end;
        setdef:
          begin
            concatcopy_set(list,size,source,dest);
            handled:=true;
          end;
        stringdef:
          begin
            if is_shortstring(size) then
              begin
                concatcopy_shortstring(list,size,source,dest);
                handled:=true;
              end;
          end;
        procvardef:
          begin
            if not tprocvardef(size).is_addressonly then
              begin
                concatcopy_record(list,tcpuprocvardef(size).classdef,source,dest);
                handled:=true;
              end;
          end;
        else
          ;
      end;
      if not handled then
        inherited;
    end;

  procedure thlcgwasm.g_copyshortstring(list: TAsmList; const source, dest: treference; strdef: tstringdef);
    begin
      concatcopy_shortstring(list,strdef,source,dest);
    end;

  procedure thlcgwasm.a_loadfpu_ref_ref(list: TAsmList; fromsize, tosize: tdef; const ref1, ref2: treference);
    var
      dstack_slots: longint;
    begin
      dstack_slots:=prepare_stack_for_ref(list,ref2,false);
      a_load_ref_stack(list,fromsize,ref1,prepare_stack_for_ref(list,ref1,false));
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_ref(list,tosize,ref2,dstack_slots);
    end;

  procedure thlcgwasm.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister);
    begin
      a_load_ref_stack(list,fromsize,ref,prepare_stack_for_ref(list,ref,false));
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_reg(list,tosize,reg);
    end;

  procedure thlcgwasm.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference);
    var
      dstack_slots: longint;
    begin
      dstack_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_reg_stack(list,fromsize,reg);
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_ref(list,tosize,ref,dstack_slots);
    end;

  procedure thlcgwasm.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    begin
      a_load_reg_stack(list,fromsize,reg1);
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_reg(list,tosize,reg2);
    end;

  procedure thlcgwasm.g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean);
    begin
      { the localsize is based on tg.lasttemp -> already in terms of stack
        slots rather than bytes }
      list.concat(tai_directive.Create(asd_jlimit,'locals '+tostr(localsize)));
      { we insert the unit initialisation code afterwards in the proginit code,
        and it uses one stack slot }
      if (current_procinfo.procdef.proctypeoption=potype_proginit) then
        fmaxevalstackheight:=max(1,fmaxevalstackheight);
      list.concat(tai_directive.Create(asd_jlimit,'stack '+tostr(fmaxevalstackheight)));
    end;

  procedure thlcgwasm.g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean);
    begin
      list.concat(taicpu.op_none(a_return));
    end;

  procedure thlcgwasm.gen_load_return_value(list: TAsmList);
    begin
      { constructors don't return anything in the jvm }
      if current_procinfo.procdef.proctypeoption in [potype_constructor,potype_class_constructor] then
        exit;
      inherited gen_load_return_value(list);
    end;

  procedure thlcgwasm.record_generated_code_for_procdef(pd: tprocdef; code, data: TAsmList);
    begin
      { add something to the al_procedures list as well, because if all al_*
        lists are empty, the assembler writer isn't called }
      if not code.empty and
         current_asmdata.asmlists[al_procedures].empty then
        current_asmdata.asmlists[al_procedures].concat(tai_align.Create(4));
      tcpuprocdef(pd).exprasmlist:=TAsmList.create;
      tcpuprocdef(pd).exprasmlist.concatlist(code);
      if assigned(data) and
         not data.empty then
        internalerror(2010122801);
    end;

  procedure thlcgwasm.g_incrrefcount(list: TAsmList; t: tdef; const ref: treference);
    begin
      // do nothing
    end;

  procedure thlcgwasm.g_array_rtti_helper(list: TAsmList; t: tdef; const ref: treference; const highloc: tlocation; const name: string);
    var
      normaldim: longint;
      eleref: treference;
    begin
      { only in case of initialisation, we have to set all elements to "empty" }
      if name<>'fpc_initialize_array' then
        exit;
      { put array on the stack }
      a_load_ref_stack(list,ptruinttype,ref,prepare_stack_for_ref(list,ref,false));
      { in case it's an open array whose elements are regular arrays, put the
        dimension of the regular arrays on the stack (otherwise pass 0) }
      normaldim:=0;
      while (t.typ=arraydef) and
            not is_dynamic_array(t) do
        begin
          inc(normaldim);
          t:=tarraydef(t).elementdef;
        end;
      a_load_const_stack(list,s32inttype,normaldim,R_INTREGISTER);
      { highloc is invalid, the length is part of the array in Java }
      if is_wide_or_unicode_string(t) then
        g_call_system_proc(list,'fpc_initialize_array_unicodestring',[],nil)
      else if is_ansistring(t) then
        g_call_system_proc(list,'fpc_initialize_array_ansistring',[],nil)
      else if is_dynamic_array(t) then
        g_call_system_proc(list,'fpc_initialize_array_dynarr',[],nil)
      else if is_record(t) or
              (t.typ=setdef) then
        begin
          tg.gethltemp(list,t,t.size,tt_persistent,eleref);
          a_load_ref_stack(list,t,eleref,prepare_stack_for_ref(list,eleref,false));
          if is_record(t) then
            g_call_system_proc(list,'fpc_initialize_array_record',[],nil)
          else if tsetdef(t).elementdef.typ=enumdef then
            g_call_system_proc(list,'fpc_initialize_array_enumset',[],nil)
          else
            g_call_system_proc(list,'fpc_initialize_array_bitset',[],nil);
          tg.ungettemp(list,eleref);
        end
      else if (t.typ=enumdef) then
        begin
          if get_enum_init_val_ref(t,eleref) then
            begin
              a_load_ref_stack(list,ptruinttype,eleref,prepare_stack_for_ref(list,eleref,false));
              g_call_system_proc(list,'fpc_initialize_array_object',[],nil);
            end;
        end
      else
        internalerror(2011031901);
    end;

  procedure thlcgwasm.g_initialize(list: TAsmList; t: tdef; const ref: treference);
    var
      dummyloc: tlocation;
      sym: tsym;
      pd: tprocdef;
    begin
      if (t.typ=arraydef) and
         not is_dynamic_array(t) then
        begin
          dummyloc.loc:=LOC_INVALID;
          g_array_rtti_helper(list,tarraydef(t).elementdef,ref,dummyloc,'fpc_initialize_array')
        end
      else if is_record(t) then
        begin
          { call the fpcInitializeRec method }
          sym:=tsym(trecorddef(t).symtable.find('FPCINITIALIZEREC'));
          if assigned(sym) and
             (sym.typ=procsym) then
            begin
              if tprocsym(sym).procdeflist.Count<>1 then
                internalerror(2011071713);
              pd:=tprocdef(tprocsym(sym).procdeflist[0]);
            end
          else
            internalerror(2013113008);
          a_load_ref_stack(list,ptruinttype,ref,prepare_stack_for_ref(list,ref,false));
          a_call_name(list,pd,pd.mangledname,[],nil,false);
          { parameter removed, no result }
          decstack(list,1);
        end
      else
        a_load_const_ref(list,t,0,ref);
    end;

  procedure thlcgwasm.g_finalize(list: TAsmList; t: tdef; const ref: treference);
    begin
      // do nothing
    end;

  procedure thlcgwasm.g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef);
    begin
      { not possible, need the original operands }
      internalerror(2012102101);
    end;

  procedure thlcgwasm.g_overflowCheck_loc(List: TAsmList; const Loc: TLocation; def: TDef; var ovloc: tlocation);
    var
      hl : tasmlabel;
    begin
      if not(cs_check_overflow in current_settings.localswitches) then
        exit;
      current_asmdata.getjumplabel(hl);
      a_cmp_const_loc_label(list,s32inttype,OC_EQ,0,ovloc,hl);
      g_call_system_proc(list,'fpc_overflow',[],nil);
      a_label(list,hl);
    end;

  procedure thlcgwasm.location_get_data_ref(list: TAsmList; def: tdef; const l: tlocation; var ref: treference; loadref: boolean; alignment: longint);
    var
      tmploc: tlocation;
    begin
      { This routine is a combination of a generalised a_loadaddr_ref_reg()
        that also works for addresses in registers (in case loadref is false)
        and of a_load_ref_reg (in case loadref is true). It is used for
        a) getting the address of managed var/out parameters
        b) getting to the actual data of value types that are passed by
           reference by the compiler (and then get a local copy at the caller
           side). Normally, depending on whether this reference is passed in a
           register or reference, we either need a reference with that register
           as base or load the address in that reference and use that as a new
           base.

        Since the JVM cannot take the address of anything, all
        "pass-by-reference" value parameters (which are always aggregate types)
        are already simply the implicit pointer to the data (since arrays,
        records, etc are already internally implicit pointers). This means
        that if "loadref" is true, we must simply return this implicit pointer.
        If it is false, we are supposed the take the address of this implicit
        pointer, which is not possible.

        However, managed types are also implicit pointers in Pascal, so in that
        case "taking the address" again consists of simply returning the
        implicit pointer/current value (in case of a var/out parameter, this
        value is stored inside an array).
      }
      if not loadref then
        begin
          if not is_managed_type(def) then
            internalerror(2011020601);
          tmploc:=l;
        end
      else
        begin
          if not wasmimplicitpointertype(def) then
            begin
              { passed by reference in array of single element; l contains the
                base address of the array }
              location_reset_ref(tmploc,LOC_REFERENCE,OS_ADDR,4,ref.volatility);
              cgutils.reference_reset_base(tmploc.reference,getaddressregister(list,ptruinttype),0,tmploc.reference.temppos,4,ref.volatility);
              a_load_loc_reg(list,ptruinttype,ptruinttype,l,tmploc.reference.base);
            end
          else
            tmploc:=l;
        end;
      case tmploc.loc of
        LOC_REGISTER,
        LOC_CREGISTER :
          begin
            { the implicit pointer is in a register and has to be in a
              reference -> create a reference and put it there }
            location_force_mem(list,tmploc,ptruinttype);
            ref:=tmploc.reference;
          end;
        LOC_REFERENCE,
        LOC_CREFERENCE :
          begin
            ref:=tmploc.reference;
          end;
        else
          internalerror(2011020603);
      end;
    end;

  procedure thlcgwasm.maybe_change_load_node_reg(list: TAsmList; var n: tnode; reload: boolean);
    begin
      { don't do anything, all registers become stack locations anyway }
    end;

  procedure thlcgwasm.g_copyvaluepara_openarray(list: TAsmList; const ref: treference; const lenloc: tlocation; arrdef: tarraydef; destreg: tregister);
    var
      localref: treference;
      arrloc: tlocation;
      stackslots: longint;
    begin
      { temporary reference for passing to concatcopy }
      tg.gethltemp(list,ptruinttype,ptruinttype.size,tt_persistent,localref);
      stackslots:=prepare_stack_for_ref(list,localref,false);
      { create the local copy of the array (lenloc is invalid, get length
        directly from the array) }
      location_reset_ref(arrloc,LOC_REFERENCE,OS_ADDR,sizeof(pint),ref.volatility);
      arrloc.reference:=ref;
      g_getarraylen(list,arrloc);
      g_newarray(list,arrdef,1);
      a_load_stack_ref(list,ptruinttype,localref,stackslots);
      { copy the source array to the destination }
      g_concatcopy(list,arrdef,ref,localref);
      { and put the array pointer in the register as expected by the caller }
      a_load_ref_reg(list,ptruinttype,ptruinttype,localref,destreg);
    end;

  procedure thlcgwasm.g_releasevaluepara_openarray(list: TAsmList; arrdef: tarraydef; const l: tlocation);
    begin
      // do nothing, long live garbage collection!
    end;

  procedure thlcgwasm.gen_initialize_code(list: TAsmList);
    var
      ref: treference;
    begin
      { create globals with wrapped types such as arrays/records  }
      case current_procinfo.procdef.proctypeoption of
        potype_unitinit:
          begin
            cgutils.reference_reset_base(ref,NR_NO,0,ctempposinvalid,1,[]);
            if assigned(current_module.globalsymtable) then
              allocate_implicit_structs_for_st_with_base_ref(list,current_module.globalsymtable,ref,staticvarsym);
            allocate_implicit_structs_for_st_with_base_ref(list,current_module.localsymtable,ref,staticvarsym);
          end;
        potype_class_constructor:
          begin
            { also initialise local variables, if any }
            inherited;
            { initialise class fields }
            cgutils.reference_reset_base(ref,NR_NO,0,ctempposinvalid,1,[]);
            allocate_implicit_structs_for_st_with_base_ref(list,tabstractrecorddef(current_procinfo.procdef.owner.defowner).symtable,ref,staticvarsym);
          end
        else
          inherited
      end;
    end;

  procedure thlcgwasm.gen_entry_code(list: TAsmList);
    begin
      list.concat(Tai_force_line.Create);
    end;

  procedure thlcgwasm.gen_exit_code(list: TAsmList);
    begin
      { nothing }
    end;

  procedure thlcgwasm.a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; srcsize, dstsize: tdef; src, dst: tregister);
    begin
      internalerror(2012090201);
    end;

  procedure thlcgwasm.a_loadmm_loc_reg(list: TAsmList; fromsize, tosize: tdef; const loc: tlocation; const reg: tregister; shuffle: pmmshuffle);
    begin
      internalerror(2012090202);
    end;

  procedure thlcgwasm.a_loadmm_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister; shuffle: pmmshuffle);
    begin
      internalerror(2012060130);
    end;

  procedure thlcgwasm.a_loadmm_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister; shuffle: pmmshuffle);
    begin
      internalerror(2012060131);
    end;

  procedure thlcgwasm.a_loadmm_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference; shuffle: pmmshuffle);
    begin
      internalerror(2012060132);
    end;

  procedure thlcgwasm.a_opmm_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; src, dst: tregister; shuffle: pmmshuffle);
    begin
      internalerror(2012060133);
    end;

  procedure thlcgwasm.a_loadmm_intreg_reg(list: TAsmList; fromsize, tosize: tdef; intreg, mmreg: tregister; shuffle: pmmshuffle);
    begin
      internalerror(2012060134);
    end;

  procedure thlcgwasm.a_loadmm_reg_intreg(list: TAsmList; fromsize, tosize: tdef; mmreg, intreg: tregister; shuffle: pmmshuffle);
    begin
      internalerror(2012060135);
    end;

  procedure thlcgwasm.g_stackpointer_alloc(list: TAsmList; size: longint);
    begin
      internalerror(2012090203);
    end;

  procedure thlcgwasm.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    begin
      internalerror(2012090204);
    end;

  procedure thlcgwasm.g_adjust_self_value(list: TAsmList; procdef: tprocdef; ioffset: aint);
    begin
      internalerror(2012090205);
    end;

  procedure thlcgwasm.g_local_unwind(list: TAsmList; l: TAsmLabel);
    begin
      internalerror(2012090206);
    end;

  procedure thlcgwasm.a_load_stack_reg(list: TAsmList; size: tdef; reg: tregister);
    var
      opc: tasmop;
      finishandval: tcgint;
    begin
      opc:=loadstoreopc(size,false,false,finishandval);
      list.concat(taicpu.op_reg(opc,reg));
      { avoid problems with getting the size of an open array etc }
      if wasmimplicitpointertype(size) then
        size:=ptruinttype;
      decstack(list,1);
    end;

  procedure thlcgwasm.a_load_stack_ref(list: TAsmList; size: tdef; const ref: treference; extra_slots: longint);
    var
      opc: tasmop;
      finishandval: tcgint;
    begin
      { fake location that indicates the value has to remain on the stack }
      if ref.base=NR_EVAL_STACK_BASE then
        exit;
      opc:=loadstoreopcref(size,false,ref,finishandval);

      list.concat(taicpu.op_ref(opc,ref));
      { avoid problems with getting the size of an open array etc }
      if wasmimplicitpointertype(size) then
        size:=ptruinttype;
      decstack(list,1+extra_slots);
    end;

  procedure thlcgwasm.a_load_reg_stack(list: TAsmList; size: tdef; reg: tregister);
    var
      opc: tasmop;
      finishandval: tcgint;
    begin
      opc:=loadstoreopc(size,true,false,finishandval);
      list.concat(taicpu.op_reg(opc,reg));
      { avoid problems with getting the size of an open array etc }
      if wasmimplicitpointertype(size) then
        size:=ptruinttype;
      incstack(list,1);
      if finishandval<>-1 then
        a_op_const_stack(list,OP_AND,size,finishandval);
    end;

  procedure thlcgwasm.a_load_ref_stack(list: TAsmList; size: tdef; const ref: treference; extra_slots: longint);
    var
      opc: tasmop;
      finishandval: tcgint;
    begin
      { fake location that indicates the value is already on the stack? }
      if (ref.base=NR_EVAL_STACK_BASE) then
        exit;
      opc:=loadstoreopcref(size,true,ref,finishandval);

      list.concat(taicpu.op_ref(opc,ref));

      { avoid problems with getting the size of an open array etc }
      if wasmimplicitpointertype(size) then
        size:=ptruinttype;
      incstack(list,1-extra_slots);
      if finishandval<>-1 then
        a_op_const_stack(list,OP_AND,size,finishandval);

      // there's no cast check in Wasm
      //if ref.checkcast then
      //  gen_typecheck(list,a_checkcast,size);
    end;

  function thlcgwasm.loadstoreopcref(def: tdef; isload: boolean; const ref: treference; out finishandval: tcgint): tasmop;
    const
                          {iisload} {issigned}
      getputmem8   : array [boolean, boolean] of TAsmOp = ((a_i32_store8, a_i32_store8),   (a_i32_load8_u, a_i32_load8_s));
      getputmem16  : array [boolean, boolean] of TAsmOp = ((a_i32_store16, a_i32_store16), (a_i32_load16_u ,a_i32_load16_s));
      getputmem32  : array [boolean, boolean] of TAsmOp = ((a_i32_store, a_i32_store),     (a_i32_load, a_i32_load));
      getputmem64  : array [boolean, boolean] of TAsmOp = ((a_i64_store,  a_i64_store),    (a_i64_load, a_i64_load));
      getputmemf32 : array [boolean] of TAsmOp = (a_f32_store, a_f32_load);
      getputmemf64 : array [boolean] of TAsmOp = (a_f64_store, a_f64_load);
    begin
      if (not ref.islocal) or assigned(ref.symbol) then
        begin
          { -> either a global (static) field, or a regular field. If a regular
            field, then ref.base contains the self pointer, otherwise
            ref.base=NR_NO. In both cases, the symbol contains all other
            information (combined field name and type descriptor) }
          case def.size of
            1: result := getputmem8[isload, is_signed(def)];
            2: result := getputmem16[isload, is_signed(def)];
            4:
              if is_single(def) then
                result := getputmemf32[isload]
              else
                result := getputmem32[isload, is_signed(def)];
            8: if is_double(def) then
                 result := getputmemf64[isload]
               else
                result := getputmem64[isload, is_signed(def)];
          else
            Internalerror(201909150001);
          end;

          //result:=getputopc[isload,ref.base=NR_NO];
          finishandval:=-1;
          { erase sign extension for byte/smallint loads }
          if (def2regtyp(def)=R_INTREGISTER) and
             not is_signed(def) and
             (def.typ=orddef) and
             not is_widechar(def) then
            case def.size of
              1: if (torddef(def).high>127) then
                   finishandval:=255;
              2: if (torddef(def).high>32767) then
                   finishandval:=65535;
            end;
        end
      else
        result:=loadstoreopc(def,isload,false,finishandval);
    end;

  function thlcgwasm.loadstoreopc(def: tdef; isload, isarray: boolean; out finishandval: tcgint): tasmop;
    var
      size: longint;
    begin
      finishandval:=-1;
      if isload then result := a_get_local
      else result := a_set_local;
      {case def2regtyp(def) of
        R_INTREGISTER:
          begin
            size:=def.size;
            case size of
              1,2,3,4:
                if isload then
                  result:=a_i32_load
                else
                  result:=a_i32_store;
              8:
                if isload then
                  result:=a_i64_load
                else
                  result:=a_i64_store;
              else
                internalerror(2011032814);
            end;
          end;
        R_ADDRESSREGISTER:
          if isload then
            result:=a_i32_load
          else
            result:=a_i32_store;
        R_FPUREGISTER:
          begin
            case tfloatdef(def).floattype of
              s32real:
                if isload then
                  result:=a_f32_load
                else
                  result:=a_f32_store;
              s64real:
                if isload then
                  result:=a_f32_load
                else
                  result:=a_f32_store
              else
                internalerror(2010120504);
            end
          end
        else
          internalerror(2010120502);
      end;}
    end;

  procedure thlcgwasm.resize_stack_int_val(list: TAsmList; fromsize, tosize: tdef; formemstore: boolean);
    var
      fromcgsize, tocgsize: tcgsize;
    begin
      { When storing to an array, field or global variable, make sure the
        static type verification can determine that the stored value fits
        within the boundaries of the declared type (to appease the Dalvik VM).
        Local variables either get their type upgraded in the debug info,
        or have no type information at all }
      if formemstore and
         (tosize.typ=orddef) then
        if (torddef(tosize).ordtype in [u8bit,uchar]) then
          tosize:=s8inttype
        else if torddef(tosize).ordtype=u16bit then
          tosize:=s16inttype;

      fromcgsize:=def_cgsize(fromsize);
      tocgsize:=def_cgsize(tosize);
      if fromcgsize in [OS_S64,OS_64] then
        begin
          if not(tocgsize in [OS_S64,OS_64]) then
            begin
              { truncate }
              list.concat(taicpu.op_none(a_i32_wrap_i64));
              decstack(list,1);
            end;
        end
      else if tocgsize in [OS_S64,OS_64] then
        begin
          { extend }
          if tocgsize = OS_S64 then
            list.concat(taicpu.op_none(a_i64_extend_s_i32))
          else
            list.concat(taicpu.op_none(a_i64_extend_u_i32));
          incstack(list,1);
          { if it was an unsigned 32 bit value, remove sign extension }
          if fromcgsize=OS_32 then
            a_op_const_stack(list,OP_AND,s64inttype,cardinal($ffffffff));
        end;
      { Conversions between 32 and 64 bit types have been completely handled
        above. We still may have to truncate or sign extend in case the
        destination type is smaller that the source type, or has a different
        sign. In case the destination is a widechar and the source is not, we
        also have to insert a conversion to widechar.
       }
      if (not(fromcgsize in [OS_S64,OS_64,OS_32,OS_S32]) or
          not(tocgsize in [OS_S64,OS_64,OS_32,OS_S32])) and
         ((tcgsize2size[fromcgsize]>tcgsize2size[tocgsize]) or
          ((tcgsize2size[fromcgsize]=tcgsize2size[tocgsize]) and
           (fromcgsize<>tocgsize)) or
          { needs to mask out the sign in the top 16 bits }
          ((fromcgsize=OS_S8) and
           (tocgsize=OS_16)) or
          ((tosize=cwidechartype) and
           (fromsize<>cwidechartype))) then
        case tocgsize of
          OS_8:
            //todo: conversion
            //a_op_const_stack(list,OP_AND,s32inttype,255);
            ;
          OS_S8:
            //todo: conversion
            //list.concat(taicpu.op_none(a_i2b));
            ;
          OS_16:
            //todo: conversion
            //if (tosize.typ=orddef) and
            //   (torddef(tosize).ordtype=uwidechar) then
            //  list.concat(taicpu.op_none(a_i2c))
            //else
            //  a_op_const_stack(list,OP_AND,s32inttype,65535);
            ;
          OS_S16:
            //todo: conversion
            //list.concat(taicpu.op_none(a_i2s));
            ;
          else
            ;
        end;
    end;

    procedure thlcgwasm.maybe_resize_stack_para_val(list: TAsmList; retdef: tdef; callside: boolean);
      var
        convsize: tdef;
      begin
        if (retdef.typ=orddef) then
          begin
            if (torddef(retdef).ordtype in [u8bit,u16bit,uchar]) and
               (torddef(retdef).high>=(1 shl (retdef.size*8-1))) then
              begin
                convsize:=nil;
                if callside then
                  if torddef(retdef).ordtype in [u8bit,uchar] then
                    convsize:=s8inttype
                  else
                    convsize:=s16inttype
                else if torddef(retdef).ordtype in [u8bit,uchar] then
                    convsize:=u8inttype
                  else
                    convsize:=u16inttype;
                if assigned(convsize) then
                  resize_stack_int_val(list,s32inttype,convsize,false);
              end;
          end;
      end;


  procedure thlcgwasm.g_adjust_stack_after_call(list: TAsmList; pd: tabstractprocdef; paraheight: longint; forceresdef: tdef);
    var
      totalremovesize: longint;
      realresdef: tdef;
    begin
      if not assigned(forceresdef) then
        realresdef:=pd.returndef
      else
        realresdef:=forceresdef;
      { a constructor doesn't actually return a value in the jvm }
      if (tabstractprocdef(pd).proctypeoption=potype_constructor) then
        totalremovesize:=paraheight
      else
        { even a byte takes up a full stackslot -> align size to multiple of 4 }
        totalremovesize:=paraheight-(align(realresdef.size,4) shr 2);
      { remove parameters from internal evaluation stack counter (in case of
        e.g. no parameters and a result, it can also increase) }
      if totalremovesize>0 then
        decstack(list,totalremovesize)
      else if totalremovesize<0 then
        incstack(list,-totalremovesize);
    end;


  procedure thlcgwasm.allocate_implicit_struct_with_base_ref(list: TAsmList; vs: tabstractvarsym; ref: treference);
    var
      tmpref: treference;
    begin
      ref.symbol:=current_asmdata.RefAsmSymbol(vs.mangledname,AT_DATA);
      tg.gethltemp(list,vs.vardef,vs.vardef.size,tt_persistent,tmpref);
      { only copy the reference, not the actual data }
      a_load_ref_ref(list,ptruinttype,ptruinttype,tmpref,ref);
      { remains live since there's still a reference to the created
        entity }
      tg.ungettemp(list,tmpref);
    end;


  procedure thlcgwasm.allocate_enum_with_base_ref(list: TAsmList; vs: tabstractvarsym; const initref: treference; destbaseref: treference);
    begin
      destbaseref.symbol:=current_asmdata.RefAsmSymbol(vs.mangledname,AT_DATA);
      { only copy the reference, not the actual data }
      a_load_ref_ref(list,ptruinttype,ptruinttype,initref,destbaseref);
    end;


  function thlcgwasm.get_enum_init_val_ref(def: tdef; out ref: treference): boolean;
    var
      sym: tstaticvarsym;
    begin
      result:=false;
      sym:=tstaticvarsym(tcpuenumdef(tenumdef(def).getbasedef).classdef.symtable.Find('__FPC_ZERO_INITIALIZER'));
      { no enum with ordinal value 0 -> exit }
      if not assigned(sym) then
        exit;
      reference_reset_symbol(ref,current_asmdata.RefAsmSymbol(sym.mangledname,AT_DATA),0,4,[]);
      result:=true;
    end;


  procedure thlcgwasm.allocate_implicit_structs_for_st_with_base_ref(list: TAsmList; st: tsymtable; const ref: treference; allocvartyp: tsymtyp);
    var
      vs: tabstractvarsym;
      def: tdef;
      i: longint;
      initref: treference;
    begin
      for i:=0 to st.symlist.count-1 do
        begin
          if (tsym(st.symlist[i]).typ<>allocvartyp) then
            continue;
          vs:=tabstractvarsym(st.symlist[i]);
          if sp_static in vs.symoptions then
            continue;
          { vo_is_external and vo_has_local_copy means a staticvarsym that is
            alias for a constsym, whose sole purpose is for allocating and
            intialising the constant }
          if [vo_is_external,vo_has_local_copy]*vs.varoptions=[vo_is_external] then
             continue;
          { threadvar innitializations are handled at the node tree level }
          if vo_is_thread_var in vs.varoptions then
            begin
              { nothing }
            end
          else if wasmimplicitpointertype(vs.vardef) then
            allocate_implicit_struct_with_base_ref(list,vs,ref)
          { enums are class instances in Java, while they are ordinals in
            Pascal. When they are initialized with enum(0), such as in
            constructors or global variables, initialize them with the
            enum instance for 0 if it exists (if not, it remains nil since
            there is no valid enum value in it) }
          else if (vs.vardef.typ=enumdef) and
                  ((vs.typ<>fieldvarsym) or
                   (tdef(vs.owner.defowner).typ<>objectdef) or
                   (ts_jvm_enum_field_init in current_settings.targetswitches)) and
                  get_enum_init_val_ref(vs.vardef,initref) then
            allocate_enum_with_base_ref(list,vs,initref,ref);
        end;
      { process symtables of routines part of this symtable (for local typed
        constants) }
      if allocvartyp=staticvarsym then
        begin
          for i:=0 to st.deflist.count-1 do
            begin
              def:=tdef(st.deflist[i]);
              { the unit symtable also contains the methods of classes defined
                in that unit -> skip them when processing the unit itself.
                Localst is not assigned for the main program code.
                Localst can be the same as st in case of unit init code. }
              if (def.typ<>procdef) or
                 (def.owner<>st) or
                 not assigned(tprocdef(def).localst) or
                 (tprocdef(def).localst=st) then
                continue;
              allocate_implicit_structs_for_st_with_base_ref(list,tprocdef(def).localst,ref,allocvartyp);
            end;
        end;
    end;

  procedure thlcgwasm.gen_initialize_fields_code(list: TAsmList);
    var
      sym: tsym;
      selfpara: tparavarsym;
      selfreg: tregister;
      ref: treference;
      obj: tabstractrecorddef;
      i: longint;
      needinit: boolean;
    begin
      obj:=tabstractrecorddef(current_procinfo.procdef.owner.defowner);
      { check whether there are any fields that need initialisation }
      needinit:=false;
      for i:=0 to obj.symtable.symlist.count-1 do
        begin
          sym:=tsym(obj.symtable.symlist[i]);
          if (sym.typ=fieldvarsym) and
             not(sp_static in sym.symoptions) and
             (wasmimplicitpointertype(tfieldvarsym(sym).vardef) or
              ((tfieldvarsym(sym).vardef.typ=enumdef) and
               get_enum_init_val_ref(tfieldvarsym(sym).vardef,ref))) then
            begin
              needinit:=true;
              break;
            end;
        end;
      if not needinit then
        exit;
      selfpara:=tparavarsym(current_procinfo.procdef.parast.find('self'));
      if not assigned(selfpara) then
        internalerror(2011033001);
      selfreg:=getaddressregister(list,selfpara.vardef);
      a_load_loc_reg(list,obj,obj,selfpara.localloc,selfreg);
      cgutils.reference_reset_base(ref,selfreg,0,ctempposinvalid,1,[]);
      allocate_implicit_structs_for_st_with_base_ref(list,obj.symtable,ref,fieldvarsym);
    end;

  procedure thlcgwasm.gen_typecheck(list: TAsmList; checkop: tasmop; checkdef: tdef);
    begin
      { replace special types with their equivalent class type }
      if (checkdef.typ=pointerdef) and
         wasmimplicitpointertype(tpointerdef(checkdef).pointeddef) then
        checkdef:=tpointerdef(checkdef).pointeddef;
      if (checkdef=voidpointertype) or
         (checkdef.typ=formaldef) then
        checkdef:=ptruinttype
      else if checkdef.typ=enumdef then
        checkdef:=tcpuenumdef(checkdef).classdef
      else if checkdef.typ=setdef then
        begin
          if tsetdef(checkdef).elementdef.typ=enumdef then
            checkdef:=java_juenumset
          else
            checkdef:=java_jubitset;
        end
      else if checkdef.typ=procvardef then
        checkdef:=tcpuprocvardef(checkdef).classdef
      else if is_wide_or_unicode_string(checkdef) then
        checkdef:=java_jlstring
      else if is_ansistring(checkdef) then
        checkdef:=java_ansistring
      else if is_shortstring(checkdef) then
        checkdef:=java_shortstring;
      if checkdef.typ in [objectdef,recorddef] then
        list.concat(taicpu.op_sym(checkop,current_asmdata.RefAsmSymbol(tabstractrecorddef(checkdef).jvm_full_typename(true),AT_METADATA)))
      else if checkdef.typ=classrefdef then
        list.concat(taicpu.op_sym(checkop,current_asmdata.RefAsmSymbol('java/lang/Class',AT_METADATA)))
      { todo: WASM
      else
        list.concat(taicpu.op_sym(checkop,current_asmdata.RefAsmSymbol(jvmencodetype(checkdef,false),AT_METADATA)));
        }
    end;

  procedure thlcgwasm.resizestackfpuval(list: TAsmList; fromsize, tosize: tcgsize);
    begin
      if (fromsize=OS_F32) and
         (tosize=OS_F64) then
        begin
          list.concat(taicpu.op_none(a_f64_promote_f32));
          incstack(list,1);
        end
      else if (fromsize=OS_F64) and
              (tosize=OS_F32) then
        begin
          list.concat(taicpu.op_none(a_f32_demote_f64));
          decstack(list,1);
        end;
    end;

  procedure thlcgwasm.maybepreparedivu32(list: TAsmList; var op: topcg; size: tdef; out isdivu32: boolean);
    begin
      if (op=OP_DIV) and
         (def_cgsize(size)=OS_32) then
        begin
          { needs zero-extension to 64 bit, because the JVM only supports
            signed divisions }
          resize_stack_int_val(list,u32inttype,s64inttype,false);
          op:=OP_IDIV;
          isdivu32:=true;
        end
      else
        isdivu32:=false;
    end;

  function thlcgwasm.a_call_name_intern(list: TAsmList; pd: tprocdef; const s: TSymStr; forceresdef: tdef; inheritedcall: boolean): tcgpara;
    var
      opc: tasmop;
    begin
      {
        invoke types:
          * invokeinterface: call method from an interface (must also specify
              number of parameters in terms of stack slot count!)
          * invokespecial: invoke a constructor, method in a superclass,
              or private instance method
          * invokestatic: invoke a class method (private or not)
          * invokevirtual: invoke a regular method
      }
      case pd.owner.symtabletype of
        globalsymtable,
        staticsymtable,
        localsymtable:
          { regular and nested procedures are turned into static methods }
          opc:=a_call;
        objectsymtable:
          begin
            Internalerror(2019083004); // no support for symbol table calls
            opc:=a_call_indirect; // todo: need a reference to a (funccall) table

            {case tobjectdef(pd.owner.defowner).objecttype of

              odt_javaclass:
                begin
                  if (po_classmethod in pd.procoptions) or
                     (pd.proctypeoption=potype_operator) then
                    opc:=a_invokestatic
                  else if (pd.visibility=vis_strictprivate) or
                     (pd.proctypeoption=potype_constructor) or
                     inheritedcall then
                    opc:=a_invokespecial
                  else
                    opc:=a_invokevirtual;
                end;
              odt_interfacejava:
                { static interface methods are not allowed }
                opc:=a_invokeinterface;
              else
                internalerror(2010122601);
            end;
            }
          end;
        recordsymtable:
          begin
            if (po_staticmethod in pd.procoptions) or
               (pd.proctypeoption=potype_operator) then
              opc:=a_call
            else if (pd.visibility=vis_strictprivate) or
               (pd.proctypeoption=potype_constructor) or
               inheritedcall then
              opc:=a_call
            else
              opc:=a_call_indirect;
          end
        else
          internalerror(2010122602);
      end;
      if (opc<>a_call_indirect) then
        list.concat(taicpu.op_sym(opc,current_asmdata.RefAsmSymbol(s,AT_FUNCTION)))
      else
        begin
          pd.init_paraloc_info(calleeside);
          list.concat(taicpu.op_sym_const(opc,current_asmdata.RefAsmSymbol(s,AT_FUNCTION),pd.calleeargareasize));
        end;
      result:=get_call_result_cgpara(pd,forceresdef);
    end;

  procedure create_hlcodegen_cpu;
    begin
      hlcg:=thlcgwasm.create;
      create_codegen;
    end;

initialization
  chlcgobj:=thlcgwasm;
  create_hlcodegen:=@create_hlcodegen_cpu;
end.
