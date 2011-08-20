{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit implements the jvm high level code generator

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
  symtype,symdef,
  cpubase, hlcgobj, cgbase, cgutils, parabase;

  type

    { thlcgjvm }

    thlcgjvm = class(thlcgobj)
     private
      fevalstackheight,
      fmaxevalstackheight: longint;
     public
      constructor create;

      procedure incstack(list : TAsmList;slots: longint);
      procedure decstack(list : TAsmList;slots: longint);

      procedure a_call_name(list : TAsmList;pd : tprocdef;const s : string; weak: boolean);override;
      procedure a_call_name_inherited(list : TAsmList;pd : tprocdef;const s : string);override;

      procedure a_load_const_reg(list : TAsmList;tosize : tdef;a : aint;register : tregister);override;
      procedure a_load_const_ref(list : TAsmList;tosize : tdef;a : aint;const ref : treference);override;
      procedure a_load_reg_ref(list : TAsmList;fromsize, tosize : tdef;register : tregister;const ref : treference);override;
      procedure a_load_reg_reg(list : TAsmList;fromsize, tosize : tdef;reg1,reg2 : tregister);override;
      procedure a_load_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;register : tregister);override;
      procedure a_load_ref_ref(list : TAsmList;fromsize, tosize : tdef;const sref : treference;const dref : treference);override;
      procedure a_loadaddr_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;r : tregister);override;

      procedure a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: Aint; reg: TRegister); override;
      procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: aint; src, dst: tregister); override;
      procedure a_op_const_ref(list: TAsmList; Op: TOpCG; size: tdef; a: Aint; const ref: TReference); override;

      procedure a_op_ref_reg(list: TAsmList; Op: TOpCG; size: tdef; const ref: TReference; reg: TRegister); override;
      procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister); override;
      procedure a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister); override;

      procedure a_cmp_const_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: aint; const ref: treference; l: tasmlabel); override;
      procedure a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: aint; reg: tregister; l: tasmlabel); override;
      procedure a_cmp_ref_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; const ref: treference; reg: tregister; l: tasmlabel); override;
      procedure a_cmp_reg_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg: tregister; const ref: treference; l: tasmlabel); override;
      procedure a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel); override;

      procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;

      procedure a_loadfpu_ref_ref(list: TAsmList; fromsize, tosize: tdef; const ref1, ref2: treference); override;
      procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister); override;
      procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference); override;
      procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister); override;

      procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean); override;
      procedure g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean); override;

      procedure gen_load_return_value(list:TAsmList);override;
      procedure record_generated_code_for_procdef(pd: tprocdef; code, data: TAsmList); override;

      procedure g_incrrefcount(list : TAsmList;t: tdef; const ref: treference);override;
      procedure g_decrrefcount(list : TAsmList;t: tdef; const ref: treference);override;
      procedure g_initialize(list : TAsmList;t : tdef;const ref : treference);override;
      procedure g_finalize(list : TAsmList;t : tdef;const ref : treference);override;

      { JVM-specific routines }

      procedure a_load_stack_reg(list : TAsmList;size: tdef;reg: tregister);
      { extra_slots are the slots that are used by the reference, and that
        will be removed by the store operation }
      procedure a_load_stack_ref(list : TAsmList;size: tdef;const ref: treference;extra_slots: longint);
      procedure a_load_reg_stack(list : TAsmList;size: tdef;reg: tregister);
      { extra_slots are the slots that are used by the reference, and that
        will be removed by the load operation }
      procedure a_load_ref_stack(list : TAsmList;size: tdef;const ref: treference;extra_slots: longint);
      procedure a_load_const_stack(list : TAsmList;size: tdef;a :aint; typ: TRegisterType);

      procedure a_load_loc_stack(list : TAsmList;size: tdef;const loc: tlocation);

      procedure a_loadfpu_const_stack(list : TAsmList;size: tdef;a :double);

      procedure a_op_stack(list : TAsmList;op: topcg; size: tdef; trunc32: boolean);
      procedure a_op_const_stack(list : TAsmList;op: topcg; size: tdef;a : aint);
      procedure a_op_reg_stack(list : TAsmList;op: topcg; size: tdef;reg: tregister);
      procedure a_op_ref_stack(list : TAsmList;op: topcg; size: tdef;const ref: treference);
      procedure a_op_loc_stack(list : TAsmList;op: topcg; size: tdef;const loc: tlocation);

      { this routine expects that all values are already massaged into the
        required form (sign bits xor'ed for gt/lt comparisons for OS_32/OS_64,
        see http://stackoverflow.com/questions/4068973/c-performing-signed-comparison-in-unsigned-variables-without-casting ) }
      procedure a_cmp_stack_label(list : TAsmlist; size: tdef; cmp_op: topcmp; lab: tasmlabel);
      { these 2 routines perform the massaging expected by the previous one }
      procedure maybe_adjust_cmp_stackval(list : TAsmlist; size: tdef; cmp_op: topcmp);
      function maybe_adjust_cmp_constval(size: tdef; cmp_op: topcmp; a: aint): aint;
      { truncate/sign extend after performing operations on values < 32 bit
        that may have overflowed outside the range }
      procedure maybe_adjust_op_result(list: TAsmList; op: TOpCg; size: tdef);

      { performs sign/zero extension as required }
      procedure resize_stack_int_val(list: TAsmList;fromsize,tosize: tcgsize; forarraystore: boolean);

      property maxevalstackheight: longint read fmaxevalstackheight;

     protected
      procedure gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara); override;

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
      function loadstoreopcref(def: tdef; isload: boolean; const ref: treference; out finishandval: aint): tasmop;
      { return the load/store opcode to load/store from/to reg; if the result
        has to be and'ed after a load to get the final value, that constant
        is returned in finishandval (otherwise that value is set to -1) }
      function loadstoreopc(def: tdef; isload, isarray: boolean; out finishandval: aint): tasmop;
      procedure resizestackfpuval(list: TAsmList; fromsize, tosize: tcgsize);
      { in case of an OS_32 OP_DIV, we have to use an OS_S64 OP_IDIV because the
        JVM does not support unsigned divisions }
      procedure maybepreparedivu32(list: TAsmList; var op: topcg; size: tdef; out isdivu32: boolean);
      { common implementation of a_call_* }
      procedure a_call_name_intern(list : TAsmList;pd : tprocdef;const s : string; inheritedcall: boolean);
    end;

  procedure create_hlcodegen;


  const
    opcmp2if: array[topcmp] of tasmop = (A_None,
      a_ifeq,a_ifgt,a_iflt,a_ifge,a_ifle,
      a_ifne,a_ifle,a_iflt,a_ifge,a_ifgt);

implementation

  uses
    verbose,cutils,globals,
    defutil,
    aasmtai,aasmcpu,
    symconst,
    procinfo,cgcpu;

  const
    TOpCG2IAsmOp : array[topcg] of TAsmOp=(                       { not = xor -1 }
      A_None,A_None,a_iadd,a_iand,A_none,a_idiv,a_imul,a_imul,a_ineg,A_None,a_ior,a_ishr,a_ishl,a_iushr,a_isub,a_ixor,A_None,A_None
    );
    TOpCG2LAsmOp : array[topcg] of TAsmOp=(                       { not = xor -1 }
      A_None,A_None,a_ladd,a_land,A_none,a_ldiv,a_lmul,a_lmul,a_lneg,A_None,a_lor,a_lshr,a_lshl,a_lushr,a_lsub,a_lxor,A_None,A_None
    );

  constructor thlcgjvm.create;
    begin
      fevalstackheight:=0;
      fmaxevalstackheight:=0;
    end;

  procedure thlcgjvm.incstack(list: TasmList;slots: longint);
    begin
      if slots=0 then
        exit;
      inc(fevalstackheight,slots);
      if (fevalstackheight>fmaxevalstackheight) then
        fmaxevalstackheight:=fevalstackheight;
      if cs_asm_regalloc in current_settings.globalswitches then
        list.concat(tai_comment.Create(strpnew('allocated '+tostr(slots)+', stack height = '+tostr(fevalstackheight))));
    end;

  procedure thlcgjvm.decstack(list: TAsmList;slots: longint);
    begin
      if slots=0 then
        exit;
      dec(fevalstackheight,slots);
      if (fevalstackheight<0) then
        internalerror(2010120501);
      if cs_asm_regalloc in current_settings.globalswitches then
        list.concat(tai_comment.Create(strpnew('    freed '+tostr(slots)+', stack height = '+tostr(fevalstackheight))));
    end;

  procedure thlcgjvm.a_call_name(list: TAsmList; pd: tprocdef; const s: string; weak: boolean);
    begin
      a_call_name_intern(list,pd,s,false);
    end;

  procedure thlcgjvm.a_call_name_inherited(list: TAsmList; pd: tprocdef; const s: string);
    begin
      a_call_name_intern(list,pd,s,true);
    end;

  procedure thlcgjvm.a_load_const_stack(list : TAsmList;size : tdef;a : aint; typ: TRegisterType);
    const
      int2opc: array[-1..5] of tasmop = (a_iconst_m1,a_iconst_0,a_iconst_1,
        a_iconst_2,a_iconst_3,a_iconst_4,a_iconst_5);
    begin
      case typ of
        R_INTREGISTER:
          begin
            case def_cgsize(size) of
              OS_8,OS_16,OS_32,
              OS_S8,OS_S16,OS_S32:
                begin
                  { convert cardinals to longints }
                  a:=longint(a);
                  if (a>=-1) and
                     (a<=5) then
                    list.concat(taicpu.op_none(int2opc[a]))
                  else if (a>=low(shortint)) and
                          (a<=high(shortint)) then
                    list.concat(taicpu.op_const(a_bipush,a))
                  else if (a>=low(smallint)) and
                          (a<=high(smallint)) then
                    list.concat(taicpu.op_const(a_sipush,a))
                  else
                    list.concat(taicpu.op_const(a_ldc,a));
                end;
              OS_64,OS_S64:
                begin
                  case a of
                    0:
                      list.concat(taicpu.op_none(a_lconst_0));
                    1:
                      list.concat(taicpu.op_none(a_lconst_1));
                    else
                      list.concat(taicpu.op_const(a_ldc2_w,a));
                  end;
                  incstack(list,1);
                end;
              else
                internalerror(2010110702);
            end;
          end;
        R_ADDRESSREGISTER:
          begin
            if a<>0 then
              internalerror(2010110701);
            list.concat(taicpu.op_none(a_aconst_null));
          end;
        else
          internalerror(2010110703);
      end;
      incstack(list,1);
    end;

  procedure thlcgjvm.a_load_loc_stack(list: TAsmList;size: tdef;const loc: tlocation);
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

  procedure thlcgjvm.a_loadfpu_const_stack(list: TAsmList; size: tdef; a: double);
    begin
      case tfloatdef(size).floattype of
        s32real:
          begin
            if a=0.0 then
              list.concat(taicpu.op_none(a_fconst_0))
            else if a=1.0 then
              list.concat(taicpu.op_none(a_fconst_1))
            else if a=2.0 then
              list.concat(taicpu.op_none(a_fconst_2))
            else
              list.concat(taicpu.op_single(a_ldc,a));
            incstack(list,1);
          end;
        s64real:
          begin
            if a=0.0 then
              list.concat(taicpu.op_none(a_dconst_0))
            else if a=1.0 then
              list.concat(taicpu.op_none(a_dconst_1))
            else
              list.concat(taicpu.op_double(a_ldc2_w,a));
            incstack(list,2);
          end
        else
          internalerror(2011010501);
      end;
    end;

  procedure thlcgjvm.a_op_stack(list: TAsmList; op: topcg; size: tdef; trunc32: boolean);
    var
      cgsize: tcgsize;
    begin
      if not trunc32 then
        cgsize:=def_cgsize(size)
      else
        begin
          resize_stack_int_val(list,OS_32,OS_S64,false);
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
            { not = xor -1 }
            if op=OP_NOT then
              begin
                a_load_const_stack(list,s64inttype,-1,R_INTREGISTER);
                op:=OP_XOR;
              end;
            if TOpCG2LAsmOp[op]=A_None then
              internalerror(2010120533);
            list.concat(taicpu.op_none(TOpCG2LAsmOp[op]));
            case op of
              OP_NOT:
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
          list.concat(taicpu.op_none(a_l2i));
          decstack(list,1);
        end;
    end;

  procedure thlcgjvm.a_op_const_stack(list: TAsmList;op: topcg;size: tdef;a: aint);
    var
      trunc32: boolean;
    begin
      { use "integer to (wide)char" narrowing opcode for "and 65535" }
      if (op=OP_AND) and
         (def_cgsize(size) in [OS_16,OS_S16,OS_32,OS_S32]) and
         (a=65535) then
        list.concat(taicpu.op_none(a_i2c))
      else
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
    end;

  procedure thlcgjvm.a_op_reg_stack(list: TAsmList; op: topcg; size: tdef; reg: tregister);
    var
      trunc32: boolean;
    begin
      maybepreparedivu32(list,op,size,trunc32);
      case op of
        OP_NEG,OP_NOT:
          ;
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

  procedure thlcgjvm.a_op_ref_stack(list: TAsmList; op: topcg; size: tdef; const ref: treference);
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
      if not(op in [OP_NEG,OP_NOT]) then
        a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false));
      a_op_stack(list,op,size,trunc32);
    end;

  procedure thlcgjvm.a_op_loc_stack(list: TAsmList; op: topcg; size: tdef; const loc: tlocation);
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

    procedure thlcgjvm.a_cmp_stack_label(list: TAsmlist; size: tdef; cmp_op: topcmp; lab: tasmlabel);
      const
        opcmp2icmp: array[topcmp] of tasmop = (A_None,
          a_if_icmpeq,a_if_icmpgt,a_if_icmplt,a_if_icmpge,a_if_icmple,
          a_if_icmpne,a_if_icmple,a_if_icmplt,a_if_icmpge,a_if_icmpgt);
      var
        cgsize: tcgsize;
      begin
        case def2regtyp(size) of
          R_INTREGISTER:
            begin
              cgsize:=def_cgsize(size);
              case cgsize of
                OS_S8,OS_8,
                OS_16,OS_S16,
                OS_S32,OS_32:
                  begin
                    list.concat(taicpu.op_sym(opcmp2icmp[cmp_op],lab));
                    decstack(list,2);
                  end;
                OS_64,OS_S64:
                  begin
                    list.concat(taicpu.op_none(a_lcmp));
                    decstack(list,3);
                    list.concat(taicpu.op_sym(opcmp2if[cmp_op],lab));
                    decstack(list,1);
                  end;
                else
                  internalerror(2010120538);
              end;
            end;
          R_ADDRESSREGISTER:
            begin
              case cmp_op of
                OC_EQ:
                  list.concat(taicpu.op_sym(a_if_acmpeq,lab));
                OC_NE:
                  list.concat(taicpu.op_sym(a_if_acmpne,lab));
                else
                  internalerror(2010120537);
              end;
              decstack(list,2);
            end;
          else
            internalerror(2010120538);
        end;
      end;

    procedure thlcgjvm.maybe_adjust_cmp_stackval(list: TAsmlist; size: tdef; cmp_op: topcmp);
      begin
        if (cmp_op in [OC_EQ,OC_NE]) or
           (def2regtyp(size)<>R_INTREGISTER) then
          exit;
        { http://stackoverflow.com/questions/4068973/c-performing-signed-comparison-in-unsigned-variables-without-casting }
        case def_cgsize(size) of
          OS_32:
            a_op_const_stack(list,OP_XOR,size,cardinal($80000000));
          OS_64:
            a_op_const_stack(list,OP_XOR,size,aint($8000000000000000));
        end;
      end;

    function thlcgjvm.maybe_adjust_cmp_constval(size: tdef; cmp_op: topcmp; a: aint): aint;
      begin
        result:=a;
        if (cmp_op in [OC_EQ,OC_NE]) or
           (def2regtyp(size)<>R_INTREGISTER) then
          exit;
        case def_cgsize(size) of
          OS_32:
            result:=a xor cardinal($80000000);
          OS_64:
            result:=a xor aint($8000000000000000);
        end;
      end;

    procedure thlcgjvm.maybe_adjust_op_result(list: TAsmList; op: TOpCg; size: tdef);
      const
        overflowops = [OP_MUL,OP_SHL,OP_ADD,OP_SUB,OP_NOT,OP_NEG];
      begin
        if (op in overflowops) and
           (def_cgsize(size) in [OS_8,OS_S8,OS_16,OS_S16]) then
          resize_stack_int_val(list,OS_S32,def_cgsize(size),false);
      end;

  procedure thlcgjvm.gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara);
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
             list.concat(taicpu.op_none(a_fconst_0));
            s64real:
             list.concat(taicpu.op_none(a_dconst_0));
            else
              internalerror(2011010302);
          end
        else
          internalerror(2011010301);
      end;
    end;

  function thlcgjvm.prepare_stack_for_ref(list: TAsmList; const ref: treference; dup: boolean): longint;
    var
      href: treference;
    begin
      result:=0;
      { fake location that indicates the value is already on the stack? }
      if (ref.base=NR_EVAL_STACK_BASE) then
        exit;
      if ref.arrayreftype=art_none then
        begin
          { non-array accesses cannot have an index reg }
          if ref.index<>NR_NO then
            internalerror(2010120509);
          if (ref.base<>NR_NO) then
            begin
              if (ref.base<>NR_STACK_POINTER_REG) then
                begin
                  { regular field -> load self on the stack }
                  a_load_reg_stack(list,voidpointertype,ref.base);
                  if dup then
                    begin
                      list.concat(taicpu.op_none(a_dup));
                      incstack(list,1);
                    end;
                  { field name/type encoded in symbol, no index/offset }
                  if not assigned(ref.symbol) or
                     (ref.offset<>0) then
                    internalerror(2010120524);
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
              if not assigned(ref.symbol) or
                 (ref.offset<>0) then
                internalerror(2010120525);
            end;
        end
      else
        begin
          { arrays have implicit dereference -> pointer to array must have been
            loaded into base reg }
          if (ref.base=NR_NO) or
             (ref.base=NR_STACK_POINTER_REG) then
            internalerror(2010120511);
          if assigned(ref.symbol) then
            internalerror(2010120512);

          { stack: ... -> ..., arrayref, index }
          { load array base address }
          a_load_reg_stack(list,voidpointertype,ref.base);
          { index can either be in a register, or located in a simple memory
            location (since we have to load it anyway) }
          case ref.arrayreftype of
            art_indexreg:
              begin
                if ref.index=NR_NO then
                  internalerror(2010120513);
                { all array indices in Java are 32 bit ints }
                a_load_reg_stack(list,s32inttype,ref.index);
              end;
            art_indexref:
              begin
                reference_reset_base(href,ref.indexbase,ref.indexoffset,4);
                href.symbol:=ref.indexsymbol;
                a_load_ref_stack(list,s32inttype,href,prepare_stack_for_ref(list,href,false));
              end;
            art_indexconst:
              begin
                a_load_const_stack(list,s32inttype,ref.indexoffset,R_INTREGISTER);
              end;
            else
              internalerror(2011012001);
          end;
          { adjustment of the index }
          if ref.offset<>0 then
            a_op_const_stack(list,OP_ADD,s32inttype,ref.offset);
          if dup then
            begin
              list.concat(taicpu.op_none(a_dup2));
              incstack(list,2);
            end;
          result:=2;
        end;
    end;

  procedure thlcgjvm.a_load_const_reg(list: TAsmList; tosize: tdef; a: aint; register: tregister);
    begin
      a_load_const_stack(list,tosize,a,def2regtyp(tosize));
      a_load_stack_reg(list,tosize,register);
    end;

  procedure thlcgjvm.a_load_const_ref(list: TAsmList; tosize: tdef; a: aint; const ref: treference);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_const_stack(list,tosize,a,def2regtyp(tosize));
      a_load_stack_ref(list,tosize,ref,extra_slots);
    end;

  procedure thlcgjvm.a_load_reg_ref(list: TAsmList; fromsize, tosize: tdef; register: tregister; const ref: treference);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_reg_stack(list,fromsize,register);
      a_load_stack_ref(list,tosize,ref,extra_slots);
    end;

  procedure thlcgjvm.a_load_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    begin
      a_load_reg_stack(list,fromsize,reg1);
      if def2regtyp(fromsize)=R_INTREGISTER then
        resize_stack_int_val(list,def_cgsize(fromsize),def_cgsize(tosize),false);
      a_load_stack_reg(list,tosize,reg2);
    end;

  procedure thlcgjvm.a_load_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; register: tregister);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_ref_stack(list,fromsize,ref,extra_slots);

      if def2regtyp(fromsize)=R_INTREGISTER then
        resize_stack_int_val(list,def_cgsize(fromsize),def_cgsize(tosize),false);
      a_load_stack_reg(list,tosize,register);
    end;

  procedure thlcgjvm.a_load_ref_ref(list: TAsmList; fromsize, tosize: tdef; const sref: treference; const dref: treference);
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
        resize_stack_int_val(list,def_cgsize(fromsize),def_cgsize(tosize),dref.arrayreftype<>art_none);
      a_load_stack_ref(list,tosize,dref,extra_dslots);
    end;

  procedure thlcgjvm.a_loadaddr_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; r: tregister);
    begin
      internalerror(2010120534);
    end;

  procedure thlcgjvm.a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: Aint; reg: TRegister);
    begin
      a_op_const_reg_reg(list,op,size,a,reg,reg);
    end;

  procedure thlcgjvm.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: aint; src, dst: tregister);
    begin
      a_load_reg_stack(list,size,src);
      a_op_const_stack(list,op,size,a);
      a_load_stack_reg(list,size,dst);
    end;

  procedure thlcgjvm.a_op_const_ref(list: TAsmList; Op: TOpCG; size: tdef; a: Aint; const ref: TReference);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,true);
      { TODO, here or in peepholeopt: use iinc when possible }
      a_load_ref_stack(list,size,ref,extra_slots);
      a_op_const_stack(list,op,size,a);
      a_load_stack_ref(list,size,ref,extra_slots);
    end;

  procedure thlcgjvm.a_op_ref_reg(list: TAsmList; Op: TOpCG; size: tdef; const ref: TReference; reg: TRegister);
    begin
      a_load_reg_stack(list,size,reg);
      a_op_ref_stack(list,op,size,ref);
      a_load_stack_reg(list,size,reg);
    end;

  procedure thlcgjvm.a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister);
    begin
      a_load_reg_stack(list,size,src2);
      a_op_reg_stack(list,op,size,src1);
      a_load_stack_reg(list,size,dst);
    end;

  procedure thlcgjvm.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister);
    begin
      a_op_reg_reg_reg(list,op,size,reg1,reg2,reg2);
    end;

  procedure thlcgjvm.a_cmp_const_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: aint; const ref: treference; l: tasmlabel);
    begin
      if ref.base<>NR_EVAL_STACK_BASE then
        a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false));
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_const_stack(list,size,maybe_adjust_cmp_constval(size,cmp_op,a),def2regtyp(size));
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgjvm.a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: aint; reg: tregister; l: tasmlabel);
    begin
      a_load_reg_stack(list,size,reg);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_const_stack(list,size,maybe_adjust_cmp_constval(size,cmp_op,a),def2regtyp(size));
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgjvm.a_cmp_ref_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; const ref: treference; reg: tregister; l: tasmlabel);
    begin
      a_load_reg_stack(list,size,reg);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      if ref.base<>NR_EVAL_STACK_BASE then
        a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false))
      else
        list.concat(taicpu.op_none(a_swap));
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgjvm.a_cmp_reg_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg: tregister; const ref: treference; l: tasmlabel);
    begin
      if ref.base<>NR_EVAL_STACK_BASE then
        a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false));
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_reg_stack(list,size,reg);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgjvm.a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel);
    begin
      a_load_reg_stack(list,size,reg2);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_reg_stack(list,size,reg1);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgjvm.a_jmp_always(list: TAsmList; l: tasmlabel);
    begin
      list.concat(taicpu.op_sym(a_goto,current_asmdata.RefAsmSymbol(l.name)));
    end;

  procedure thlcgjvm.a_loadfpu_ref_ref(list: TAsmList; fromsize, tosize: tdef; const ref1, ref2: treference);
    var
      dstack_slots: longint;
    begin
      dstack_slots:=prepare_stack_for_ref(list,ref2,false);
      a_load_ref_stack(list,fromsize,ref1,prepare_stack_for_ref(list,ref1,false));
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_ref(list,tosize,ref2,dstack_slots);
    end;

  procedure thlcgjvm.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister);
    begin
      a_load_ref_stack(list,fromsize,ref,prepare_stack_for_ref(list,ref,false));
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_reg(list,tosize,reg);
    end;

  procedure thlcgjvm.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference);
    var
      dstack_slots: longint;
    begin
      dstack_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_reg_stack(list,fromsize,reg);
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_ref(list,tosize,ref,dstack_slots);
    end;

  procedure thlcgjvm.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    begin
      a_load_reg_stack(list,fromsize,reg1);
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_reg(list,tosize,reg2);
    end;

  procedure thlcgjvm.g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean);
    begin
      { the localsize is based on tg.lasttemp -> already in terms of stack
        slots rather than bytes }
      list.concat(tai_directive.Create(asd_jlimit,'locals '+tostr(localsize)));
      list.concat(tai_directive.Create(asd_jlimit,'stack '+tostr(fmaxevalstackheight)));
    end;

  procedure thlcgjvm.g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean);
    var
      retdef: tdef;
      opc: tasmop;
    begin
      if current_procinfo.procdef.proctypeoption=potype_constructor then
        retdef:=voidtype
      else
        retdef:=current_procinfo.procdef.returndef;
      case retdef.typ of
        orddef:
          case torddef(retdef).ordtype of
            uvoid:
              opc:=a_return;
            s64bit,
            u64bit,
            scurrency:
              opc:=a_lreturn;
            else
              opc:=a_ireturn;
          end;
        floatdef:
          case tfloatdef(retdef).floattype of
            s32real:
              opc:=a_freturn;
            s64real:
              opc:=a_dreturn;
            else
              internalerror(2011010213);
          end;
        else
          opc:=a_areturn;
      end;
      list.concat(taicpu.op_none(opc));
    end;

  procedure thlcgjvm.gen_load_return_value(list: TAsmList);
    begin
      { constructors don't return anything in the jvm }
      if current_procinfo.procdef.proctypeoption=potype_constructor then
        exit;
      inherited gen_load_return_value(list);
    end;

  procedure thlcgjvm.record_generated_code_for_procdef(pd: tprocdef; code, data: TAsmList);
    begin
      { add something to the al_procedures list as well, because if all al_*
        lists are empty, the assembler writer isn't called }
      if not code.empty and
         current_asmdata.asmlists[al_procedures].empty then
        current_asmdata.asmlists[al_procedures].concat(tai_align.Create(4));
      pd.exprasmlist:=TAsmList.create;
      pd.exprasmlist.concatlist(code);
      if assigned(data) and
         not data.empty then
        internalerror(2010122801);
    end;

  procedure thlcgjvm.g_incrrefcount(list: TAsmList; t: tdef; const ref: treference);
    begin
      // do nothing
    end;

  procedure thlcgjvm.g_decrrefcount(list: TAsmList; t: tdef; const ref: treference);
    begin
      // do nothing
    end;

  procedure thlcgjvm.g_initialize(list: TAsmList; t: tdef; const ref: treference);
    var
      extra_slots: longint;
    begin
      a_load_const_ref(list,t,0,ref);
    end;

  procedure thlcgjvm.g_finalize(list: TAsmList; t: tdef; const ref: treference);
    begin
      // do nothing
    end;

  procedure thlcgjvm.a_load_stack_reg(list: TAsmList; size: tdef; reg: tregister);
    var
      opc: tasmop;
      finishandval: aint;
    begin
      opc:=loadstoreopc(size,false,false,finishandval);
      list.concat(taicpu.op_reg(opc,reg));
      decstack(list,1+ord(size.size>4));
    end;

  procedure thlcgjvm.a_load_stack_ref(list: TAsmList; size: tdef; const ref: treference; extra_slots: longint);
    var
      opc: tasmop;
      finishandval: aint;
    begin
      { fake location that indicates the value has to remain on the stack }
      if ref.base=NR_EVAL_STACK_BASE then
        exit;
      opc:=loadstoreopcref(size,false,ref,finishandval);
      if ref.arrayreftype=art_none then
        list.concat(taicpu.op_ref(opc,ref))
      else
        list.concat(taicpu.op_none(opc));
      decstack(list,1+ord(size.size>4)+extra_slots);
    end;

  procedure thlcgjvm.a_load_reg_stack(list: TAsmList; size: tdef; reg: tregister);
    var
      opc: tasmop;
      finishandval: aint;
    begin
      opc:=loadstoreopc(size,true,false,finishandval);
      list.concat(taicpu.op_reg(opc,reg));
      if finishandval<>-1 then
        a_op_const_stack(list,OP_AND,size,finishandval);
      incstack(list,1+ord(size.size>4));
    end;

  procedure thlcgjvm.a_load_ref_stack(list: TAsmList; size: tdef; const ref: treference; extra_slots: longint);
    var
      opc: tasmop;
      finishandval: aint;
    begin
      { fake location that indicates the value is already on the stack? }
      if (ref.base=NR_EVAL_STACK_BASE) then
        exit;
      opc:=loadstoreopcref(size,true,ref,finishandval);
      if ref.arrayreftype=art_none then
        list.concat(taicpu.op_ref(opc,ref))
      else
        list.concat(taicpu.op_none(opc));
      if finishandval<>-1 then
        a_op_const_stack(list,OP_AND,size,finishandval);
      incstack(list,1+ord(size.size>4)-extra_slots);
    end;

  function thlcgjvm.loadstoreopcref(def: tdef; isload: boolean; const ref: treference; out finishandval: aint): tasmop;
    const
                     { isload  static }
      getputopc: array[boolean,boolean] of tasmop =
        ((a_putfield,a_putstatic),
         (a_getfield,a_getstatic));
    var
      size: aint;
    begin
      if assigned(ref.symbol) then
        begin
          finishandval:=-1;
          { -> either a global (static) field, or a regular field. If a regular
            field, then ref.base contains the self pointer, otherwise
            ref.base=NR_NO. In both cases, the symbol contains all other
            information (combined field name and type descriptor) }
          result:=getputopc[isload,ref.base=NR_NO];
        end
      else
        result:=loadstoreopc(def,isload,ref.arrayreftype<>art_none,finishandval);
    end;

  function thlcgjvm.loadstoreopc(def: tdef; isload, isarray: boolean; out finishandval: aint): tasmop;
    var
      size: longint;
    begin
      finishandval:=-1;
      case def2regtyp(def) of
        R_INTREGISTER:
          begin
            size:=def.size;
            if not isarray then
              begin
                case size of
                  1,2,3,4:
                    if isload then
                      result:=a_iload
                    else
                      result:=a_istore;
                  8:
                    if isload then
                      result:=a_lload
                    else
                      result:=a_lstore;
                end;
              end
            { array }
            else if isload then
              begin
                case size of
                  1:
                    begin
                      result:=a_baload;
                      if not is_signed(def) then
                        finishandval:=255;
                    end;
                  2:
                    begin
                      if is_widechar(def) then
                        result:=a_caload
                      else
                        begin
                          result:=a_saload;
                          { if we'd treat arrays of word as "array of widechar" we
                            could use a_caload, but that would make for even more
                            awkward interfacing with external Java code }
                          if not is_signed(def) then
                            finishandval:=65535;
                        end;
                    end;
                  4: result:=a_iaload;
                  8: result:=a_laload;
                  else
                    internalerror(2010120503);
                end
              end
            else
              begin
                case size of
                  1: result:=a_bastore;
                  2: if not is_widechar(def) then
                       result:=a_sastore
                     else
                       result:=a_castore;
                  4: result:=a_iastore;
                  8: result:=a_lastore;
                  else
                    internalerror(2010120508);
                end
              end
          end;
        R_ADDRESSREGISTER:
          if not isarray then
            if isload then
              result:=a_aload
            else
              result:=a_astore
          else if isload then
            result:=a_aaload
          else
            result:=a_aastore;
        R_FPUREGISTER:
          begin
            case tfloatdef(def).floattype of
              s32real:
                if not isarray then
                  if isload then
                    result:=a_fload
                  else
                    result:=a_fstore
                else if isload then
                  result:=a_faload
                else
                  result:=a_fastore;
              s64real:
                if not isarray then
                  if isload then
                    result:=a_dload
                  else
                    result:=a_dstore
                else if isload then
                  result:=a_daload
                else
                  result:=a_dastore;
              else
                internalerror(2010120504);
            end
          end
        else
          internalerror(2010120502);
      end;
    end;

  procedure thlcgjvm.resize_stack_int_val(list: TAsmList; fromsize, tosize: tcgsize; forarraystore: boolean);
    begin
      if fromsize in [OS_S64,OS_64] then
        begin
          if not(tosize in [OS_S64,OS_64]) then
            begin
              { truncate }
              list.concat(taicpu.op_none(a_l2i));
              decstack(list,1);
            end;
        end
      else if tosize in [OS_S64,OS_64] then
        begin
          { extend }
          list.concat(taicpu.op_none(a_i2l));
          incstack(list,1);
          { if it was an unsigned 32 bit value, remove sign extension }
          if fromsize=OS_32 then
            a_op_const_stack(list,OP_AND,s64inttype,cardinal($ffffffff));
        end;
      { if the value is immediately stored to an array afterwards, the store
        instruction will properly truncate the value; otherwise we may need
        additional truncation, except for 64/32 bit conversions, which are
        already handled above }
      if not forarraystore and
         (not(fromsize in [OS_S64,OS_64,OS_32,OS_S32]) or
          not(tosize in [OS_S64,OS_64,OS_32,OS_S32])) and
         (tcgsize2size[fromsize]>tcgsize2size[tosize]) or
         ((tcgsize2size[fromsize]=tcgsize2size[tosize]) and
          (fromsize<>tosize)) or
         { needs to mask out the sign in the top 16 bits }
         ((fromsize=OS_S8) and
          (tosize=OS_16)) then
        case tosize of
          OS_8:
            a_op_const_stack(list,OP_AND,s32inttype,255);
          OS_S8:
            list.concat(taicpu.op_none(a_i2b));
          OS_16:
            list.concat(taicpu.op_none(a_i2c));
          OS_S16:
            list.concat(taicpu.op_none(a_i2s));
        end;
    end;

  procedure thlcgjvm.resizestackfpuval(list: TAsmList; fromsize, tosize: tcgsize);
    begin
      if (fromsize=OS_F32) and
         (tosize=OS_F64) then
        begin
          list.concat(taicpu.op_none(a_f2d));
          incstack(list,1);
        end
      else if (fromsize=OS_F64) and
              (tosize=OS_F32) then
        begin
          list.concat(taicpu.op_none(a_d2f));
          decstack(list,1);
        end;
    end;

  procedure thlcgjvm.maybepreparedivu32(list: TAsmList; var op: topcg; size: tdef; out isdivu32: boolean);
    begin
      if (op=OP_DIV) and
         (def_cgsize(size)=OS_32) then
        begin
          { needs zero-extension to 64 bit, because the JVM only supports
            signed divisions }
          resize_stack_int_val(list,OS_32,OS_S64,false);
          op:=OP_IDIV;
          isdivu32:=true;
        end
      else
        isdivu32:=false;
    end;

  procedure thlcgjvm.a_call_name_intern(list: TAsmList; pd: tprocdef; const s: string; inheritedcall: boolean);
    var
      opc: tasmop;
    begin
      {
        invoke types:
          * invokeinterface: call method from an interface
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
          opc:=a_invokestatic;
        objectsymtable:
          begin
            case tobjectdef(pd.owner.defowner).objecttype of
              odt_javaclass:
                begin
                  if (po_staticmethod in pd.procoptions) then
                    opc:=a_invokestatic
                  else if (pd.visibility=vis_private) or
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
          end;
        else
          internalerror(2010122602);
      end;
      list.concat(taicpu.op_sym(opc,current_asmdata.RefAsmSymbol(s)));
    end;

  procedure create_hlcodegen;
    begin
      hlcg:=thlcgjvm.create;
      create_codegen;
    end;

end.
