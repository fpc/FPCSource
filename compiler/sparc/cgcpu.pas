{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the code generator for the SPARC

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
unit cgcpu;

{$i fpcdefs.inc}

interface

    uses
       cginfo,cgbase,cgobj,cg64f32,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,
       node,symconst;

    type
      TCgSparc=class(tcg)
      private
        function IsSimpleRef(const ref:treference):boolean;
      public
        procedure a_param_const(list:TAasmOutput;size:tcgsize;a:aword;const LocPara:TParaLocation);override;
        procedure a_param_ref(list:TAasmOutput;sz:tcgsize;const r:TReference;const LocPara:TParaLocation);override;
        procedure a_paramaddr_ref(list:TAasmOutput;const r:TReference;const LocPara:TParaLocation);override;
        procedure a_call_name(list:TAasmOutput;const s:string);override;
        procedure a_call_ref(list:TAasmOutput;const ref:TReference);override;
        procedure a_call_reg(list:TAasmOutput;Reg:TRegister);override;
        { General purpose instructions }
        procedure a_op_const_reg(list:TAasmOutput;Op:TOpCG;a:AWord;reg:TRegister);override;
        procedure a_op_reg_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;src, dst:TRegister);override;
        procedure a_op_ref_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;const ref:TReference;reg:TRegister);override;
        procedure a_op_const_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;a:aword;src, dst:tregister);override;
        procedure a_op_reg_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;src1, src2, dst:tregister);override;
        { move instructions }
        procedure a_load_const_reg(list:TAasmOutput;size:tcgsize;a:aword;reg:tregister);override;
        procedure a_load_const_ref(list:TAasmOutput;size:tcgsize;a:aword;const ref:TReference);override;
        procedure a_load_reg_ref(list:TAasmOutput;size:tcgsize;reg:tregister;const ref:TReference);override;
        procedure a_load_ref_reg(list:TAasmOutput;size:tcgsize;const ref:TReference;reg:tregister);override;
        procedure a_load_reg_reg(list:TAasmOutput;fromsize,tosize:tcgsize;reg1,reg2:tregister);override;
        procedure a_loadaddr_ref_reg(list:TAasmOutput;const ref:TReference;r:tregister);override;
        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list:TAasmOutput;reg1, reg2:tregister);override;
        procedure a_loadfpu_ref_reg(list:TAasmOutput;size:tcgsize;const ref:TReference;reg:tregister);override;
        procedure a_loadfpu_reg_ref(list:TAasmOutput;size:tcgsize;reg:tregister;const ref:TReference);override;
        { comparison operations }
        procedure a_cmp_const_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aword;reg:tregister;l:tasmlabel);override;
        procedure a_cmp_const_ref_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aword;const ref:TReference;l:tasmlabel);override;
        procedure a_cmp_reg_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;reg1,reg2:tregister;l:tasmlabel);override;
        procedure a_cmp_ref_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;const ref:TReference;reg:tregister;l:tasmlabel);override;
        procedure a_jmp_always(List:TAasmOutput;l:TAsmLabel);override;
        procedure a_jmp_cond(list:TAasmOutput;cond:TOpCmp;l:tasmlabel);{ override;}
        procedure a_jmp_flags(list:TAasmOutput;const f:TResFlags;l:tasmlabel);override;
        procedure g_flags2reg(list:TAasmOutput;Size:TCgSize;const f:tresflags;reg:TRegister);override;
        procedure g_overflowCheck(List:TAasmOutput;const p:TNode);override;
        procedure g_stackframe_entry(list:TAasmOutput;localsize:LongInt);override;
        procedure g_restore_all_registers(list:TAasmOutput;accused,acchiused:boolean);override;
        procedure g_restore_frame_pointer(list:TAasmOutput);override;
        procedure g_restore_standard_registers(list:taasmoutput;usedinproc:Tsupregset);override;
        procedure g_return_from_proc(list:TAasmOutput;parasize:aword);override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_save_standard_registers(list : taasmoutput; usedinproc : Tsupregset);override;
        procedure g_concatcopy(list:TAasmOutput;const source,dest:TReference;len:aword;delsource,loadref:boolean);override;
        class function reg_cgsize(const reg:tregister):tcgsize;override;
      end;

      TCg64Sparc=class(tcg64f32)
        procedure a_op64_ref_reg(list:TAasmOutput;op:TOpCG;const ref:TReference;reg:TRegister64);override;
        procedure a_op64_reg_reg(list:TAasmOutput;op:TOpCG;regsrc,regdst:TRegister64);override;
        procedure a_op64_const_reg(list:TAasmOutput;op:TOpCG;value:qWord;regdst:TRegister64);override;
        procedure a_op64_const_ref(list:TAasmOutput;op:TOpCG;value:qWord;const ref:TReference);override;
        procedure get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      end;

    const
      TOpCG2AsmOp : array[topcg] of TAsmOp=(
         A_NONE,A_ADD,A_AND,A_UDIV,A_SDIV,A_UMUL, A_SMUL, A_NEG,A_NOT,A_OR,A_not,A_not,A_not,A_SUB,A_XOR
      );
      TOpCmp2AsmCond : array[topcmp] of TAsmCond=(
        C_NONE,C_E,C_G,C_L,C_GE,C_LE,C_NE,C_BE,C_B,C_AE,C_A
      );


implementation

  uses
    globtype,globals,verbose,systems,cutils,
    symdef,symsym,defutil,paramgr,
    rgobj,tgobj,rgcpu,cpupi;


{****************************************************************************
                       This is private property, keep out! :)
****************************************************************************}

    function TCgSparc.IsSimpleRef(const ref:treference):boolean;
      begin
        if (ref.base.number=NR_NO) and (ref.index.number<>NR_NO) then
          InternalError(2002100804);
        result :=not(assigned(ref.symbol))and
                  (((ref.index.number = NR_NO) and
                   (ref.offset >= low(smallint)) and
                    (ref.offset <= high(smallint))) or
                  ((ref.index.number <> NR_NO) and
                  (ref.offset = 0)));
      end;


{****************************************************************************
                              Assembler code
****************************************************************************}

    function TCgSparc.reg_cgsize(const reg:tregister):tcgsize;
      begin
        result:=OS_32;
      end;


    procedure TCgSparc.a_param_const(list:TAasmOutput;size:tcgsize;a:aword;const LocPara:TParaLocation);
      var
        Ref:TReference;
      begin
        case locpara.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_load_const_reg(list,size,a,locpara.register);
          LOC_REFERENCE:
            begin
              reference_reset(ref);
              ref.base:=locpara.reference.index;
              ref.offset:=locpara.reference.offset;
              a_load_const_ref(list,size,a,ref);
            end;
          else
            InternalError(2002122200);
        end;
        if locpara.sp_fixup<>0 then
          InternalError(2002122201);
      end;


    procedure TCgSparc.a_param_ref(list:TAasmOutput;sz:TCgSize;const r:TReference;const LocPara:TParaLocation);
      var
        ref: treference;
        tmpreg:TRegister;
      begin
        with LocPara do
          case loc of
            LOC_REGISTER,LOC_CREGISTER:
              a_load_ref_reg(list,sz,r,Register);
            LOC_REFERENCE:
              begin
                { Code conventions need the parameters being allocated in %o6+92. See
                  comment on g_stack_frame }
                if locpara.sp_fixup<92 then
                  InternalError(2002081104);
                reference_reset(ref);
                ref.base:=locpara.reference.index;
                ref.offset:=locpara.reference.offset;
                tmpreg := get_scratch_reg_int(list,sz);
                a_load_ref_reg(list,sz,r,tmpreg);
                a_load_reg_ref(list,sz,tmpreg,ref);
                free_scratch_reg(list,tmpreg);
              end;
            LOC_FPUREGISTER,LOC_CFPUREGISTER:
              case sz of
                OS_32:
                  a_loadfpu_ref_reg(list,OS_F32,r,locpara.register);
                OS_64:
                  a_loadfpu_ref_reg(list,OS_F64,r,locpara.register);
              else
                internalerror(2002072801);
              end;
            else
              internalerror(2002081103);
          end;
      end;


    procedure TCgSparc.a_paramaddr_ref(list:TAasmOutput;const r:TReference;const LocPara:TParaLocation);
      var
        Ref:TReference;
        TmpReg:TRegister;
      begin
        case locpara.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_loadaddr_ref_reg(list,r,locpara.register);
          LOC_REFERENCE:
            begin
              reference_reset(ref);
              ref.base := locpara.reference.index;
              ref.offset := locpara.reference.offset;
              tmpreg := get_scratch_reg_address(list);
              a_loadaddr_ref_reg(list,r,tmpreg);
              a_load_reg_ref(list,OS_ADDR,tmpreg,ref);
              free_scratch_reg(list,tmpreg);
            end;
        else
          internalerror(2002080701);
        end;
      end;


    procedure TCgSparc.a_call_name(list:TAasmOutput;const s:string);
      begin
        list.concat(taicpu.op_sym(A_CALL,objectlibrary.newasmsymbol(s)));
        list.concat(taicpu.op_none(A_NOP));
        include(current_procinfo.flags,pi_do_call);
      end;


    procedure TCgSparc.a_call_ref(list:TAasmOutput;const ref:TReference);
      begin
        list.concat(taicpu.op_ref(A_CALL,ref));
        list.concat(taicpu.op_none(A_NOP));
        include(current_procinfo.flags,pi_do_call);
      end;


    procedure TCgSparc.a_call_reg(list:TAasmOutput;Reg:TRegister);
      var
        RetAddrReg:TRegister;
      begin
        retaddrreg.enum:=R_INTREGISTER;
        retaddrreg.Number:=NR_O7;
        list.concat(taicpu.op_reg_reg(A_JMPL,reg,RetAddrReg));
        { why only on Sparc/Linux? Doesn't use other implementations use the delay slot? (FK) }
        if target_info.system=system_sparc_linux then
          list.concat(taicpu.op_none(A_NOP));
        include(current_procinfo.flags,pi_do_call);
     end;


    {********************** load instructions ********************}

    procedure TCgSparc.a_load_const_reg(list : TAasmOutput;size : TCGSize;a : aword;reg : TRegister);
      var
        r:Tregister;
      begin
        r.enum:=R_INTREGISTER;
        r.number:=NR_G0;
        { we don't use the set instruction here because it could be evalutated to two
          instructions which would cause problems with the delay slot (FK) }
        { sethi allows to set the upper 22 bit, so we'll take full advantage of it }
        if (a and aword($3ff))=0 then
          list.concat(taicpu.op_const_reg(A_SETHI,(a and aword($fffffc00)) shr 10,reg))
        else if (a and aword($ffffe000))=0 then
          list.concat(taicpu.op_reg_const_reg(A_OR,r,a,reg))
        else
          begin
            list.concat(taicpu.op_const_reg(A_SETHI,(a and aword($ffffe000)) shr 13,reg));
            list.concat(taicpu.op_reg_const_reg(A_OR,r,a and $1fff,reg));
          end;
      end;


    procedure TCgSparc.a_load_const_ref(list : TAasmOutput;size : tcgsize;a : aword;const ref : TReference);
      var
        r:Tregister;
      begin
        inherited a_load_const_ref(list,size,a,ref);
        r.enum:=R_INTREGISTER;
        r.number:=NR_G0;
        if a=0 then
          a_load_reg_ref(list,size,r,ref)
        else
          inherited a_load_const_ref(list,size,a,ref);
      end;


    procedure TCgSparc.a_load_reg_ref(list:TAasmOutput;size:TCGSize;reg:tregister;const Ref:TReference);
      var
        op:tasmop;
      begin
        case size of
          { signed integer registers }
          OS_8,
          OS_S8:
            Op:=A_STB;
          OS_16,
          OS_S16:
            Op:=A_STH;
          OS_32,
          OS_S32:
            Op:=A_ST;
          else
            InternalError(2002122100);
        end;
        list.concat(taicpu.op_reg_ref(op,reg,ref));
      end;


    procedure TCgSparc.a_load_ref_reg(list:TAasmOutput;size:TCgSize;const ref:TReference;reg:tregister);
      var
        op:tasmop;
      begin
        case size of
          { signed integer registers }
          OS_S8:
            Op:=A_LDSB;{Load Signed Byte}
          OS_S16:
            Op:=A_LDSH;{Load Signed Halfword}
          OS_8:
            Op:=A_LDUB;{Load Unsigned Bye}
          OS_16:
            Op:=A_LDUH;{Load Unsigned Halfword}
          OS_S32,
          OS_32:
            Op:=A_LD;{Load Word}
          else
            InternalError(2002122100);
        end;
        list.concat(taicpu.op_ref_reg(op,ref,reg));
      end;


    procedure TCgSparc.a_load_reg_reg(list:TAasmOutput;fromsize,tosize:tcgsize;reg1,reg2:tregister);
      var
        zeroreg : Tregister;
      begin
        if(reg1.enum<>R_INTREGISTER)or(reg1.number=NR_NO) then
          InternalError(200303101);
        if(reg2.enum<>R_INTREGISTER)or(reg2.number=NR_NO) then
          InternalError(200303102);
        zeroreg.enum:=R_INTREGISTER;
        zeroreg.Number:=NR_G0;
        if (reg1.Number<>reg2.Number) or
           (tcgsize2size[tosize]<tcgsize2size[fromsize]) or
           (
            (tcgsize2size[tosize] = tcgsize2size[fromsize]) and
            (tosize <> fromsize) and
            not(fromsize in [OS_32,OS_S32])
           ) then
          begin
            case tosize of
              OS_8,OS_S8:
                list.Concat(taicpu.op_Reg_Const_Reg(A_AND,reg1,$FF,reg2));
              OS_16,OS_S16:
                begin
                  list.Concat(taicpu.op_Reg_Reg_Reg(A_AND,reg1,zeroreg,reg2));
                  { This will put 00...00111 in the highest 22 bits of the reg }
                  list.Concat(taicpu.op_Reg_Const_Reg(A_SETHI,reg2,$7,reg2));
                end;
              OS_32,OS_S32:
                begin
                  if reg1.number<>reg2.number then
                    list.Concat(taicpu.op_Reg_Reg_Reg(A_OR,zeroreg,reg1,reg2));
                end;
              else
                internalerror(2002090901);
            end;
          end;
      end;


    procedure TCgSparc.a_loadfpu_reg_reg(list:TAasmOutput;reg1, reg2:tregister);

           begin
    {         if NOT (reg1 IN [R_F0..R_F31]) then
               begin
                 list.concat(taicpu.op_reg(A_NONE,S_NO,
                   trgcpu(rg).correct_fpuregister(reg1,trgcpu(rg).fpuvaroffset)));
                 inc(trgcpu(rg).fpuvaroffset);
               end;
             if NOT (reg2 IN [R_F0..R_F31]) then
               begin
                 list.concat(taicpu.op_reg(A_JMPL,S_NO,
                     trgcpu(rg).correct_fpuregister(reg2,trgcpu(rg).fpuvaroffset)));
                 dec(trgcpu(rg).fpuvaroffset);
               end;}
           end;


        procedure TCgSparc.a_loadfpu_ref_reg(list:TAasmOutput;size:tcgsize;const ref:TReference;reg:tregister);

           begin
//             floatload(list,size,ref);
    {         if (reg <> R_ST) then
               a_loadfpu_reg_reg(list,R_ST,reg);}
           end;


        procedure TCgSparc.a_loadfpu_reg_ref(list:TAasmOutput;size:tcgsize;reg:tregister;const ref:TReference);
           const
             FpuStoreInstr: Array[OS_F32..OS_F64,boolean, boolean] of TAsmOp =
                                { indexed? updating?}
                        (((A_STF,A_STF),(A_STF,A_STF)),
                         ((A_STDF,A_STDF),(A_STDF,A_STDF)));
           var
             ref2: treference;
             freereg: boolean;
           begin
             if not(size in [OS_F32,OS_F64])
             then
               internalerror(200201122);
    {         ref2:=ref;
             freereg:=fixref(list,ref2);
             op:=fpustoreinstr[size,ref2.index.enum <> R_NO,false];
             a_load_store(list,op,reg,ref2);
             if freereg
             then
               cg.free_scratch_reg(list,ref2.base);}
           end;


    procedure TCgSparc.a_op_const_reg(list:TAasmOutput;Op:TOpCG;a:AWord;reg:TRegister);
      begin
        list.concat(taicpu.op_reg_const_reg(TOpCG2AsmOp[op],reg,a,reg));
      end;


    procedure TCgSparc.a_op_reg_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;src, dst:TRegister);
      begin
        list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp[op],dst,src,dst));
      end;


    procedure TCgSparc.a_op_ref_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;const ref:TReference;reg:TRegister);
      begin
        list.concat(taicpu.op_reg_ref_reg(TOpCG2AsmOp[op],reg,ref,reg));
      end;


    procedure TCgSparc.a_op_const_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;a:aword;src, dst:tregister);
      var
        tmpref:TReference;
        power:LongInt;
      begin
        if not (size in [OS_32,OS_S32]) then
          begin
            inherited a_op_const_reg_reg(list,op,size,a,src,dst);
            exit;
          end;
        { if we get here, we have to do a 32 bit calculation, guaranteed }
        Case Op of
          OP_DIV, OP_IDIV, OP_MUL, OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR,
          OP_SAR:
            { can't do anything special for these }
            inherited a_op_const_reg_reg(list,op,size,a,src,dst);
          OP_IMUL:
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(a,power) then
                { can be done with a shift }
                inherited a_op_const_reg_reg(list,op,size,a,src,dst);
              list.concat(taicpu.op_reg_const_reg(A_SMUL,src,a,dst));
            end;
          OP_ADD, OP_SUB:
            if (a = 0) then
              a_load_reg_reg(list,size,size,src,dst)
            else
              begin
                reference_reset(tmpref);
                tmpref.base := src;
                tmpref.offset := LongInt(a);
                if op = OP_SUB then
                  tmpref.offset := -tmpref.offset;
                list.concat(taicpu.op_ref_reg(A_NONE,tmpref,dst));
              end
          else
            internalerror(200112302);
        end;
      end;

    procedure TCgSparc.a_op_reg_reg_reg(list:TAasmOutput;op:TOpCg;
        size:tcgsize;src1, src2, dst:tregister);
      var
        tmpref:TReference;
      begin
        if not (size in [OS_32,OS_S32]) then
          begin
            inherited a_op_reg_reg_reg(list,op,size,src1,src2,dst);
            exit;
          end;
        { if we get here, we have to do a 32 bit calculation, guaranteed }
        Case Op of
          OP_DIV, OP_IDIV, OP_MUL, OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR,
          OP_SAR,OP_SUB,OP_NOT,OP_NEG:
            { can't do anything special for these }
            inherited a_op_reg_reg_reg(list,op,size,src1,src2,dst);
          OP_IMUL:
            list.concat(taicpu.op_reg_reg_reg(A_SMUL,src1,src2,dst));
          OP_ADD:
            begin
              reference_reset(tmpref);
              tmpref.base := src1;
              tmpref.index := src2;
              list.concat(taicpu.op_ref_reg(A_NONE,tmpref,dst));
            end
          else internalerror(200112303);
        end;
      end;


  {*************** compare instructructions ****************}

    procedure TCgSparc.a_cmp_const_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aword;reg:tregister;l:tasmlabel);
      begin
        if(a=0)
        then
          list.concat(taicpu.op_reg_reg(A_CMP,reg,reg))
        else
          list.concat(taicpu.op_reg_const(A_CMP,reg,a));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure TCgSparc.a_cmp_const_ref_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aword;const ref:TReference;l:tasmlabel);
      var
        cReg,rReg:TRegister;
      begin
        cReg:=get_scratch_reg_int(List,size);
        rReg:=get_scratch_reg_int(List,size);
        a_load_const_reg(List,OS_32,a,cReg);
        a_load_ref_reg(List,OS_32,ref,rReg);
        a_cmp_reg_reg_label(list,OS_32,cmp_op,rReg,cReg,l);
        free_scratch_reg(List,cReg);
        free_scratch_reg(List,rReg);
      end;


    procedure TCgSparc.a_cmp_reg_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;reg1,reg2:tregister;l:tasmlabel);
      begin
        List.Concat(taicpu.op_reg_reg(A_CMP,reg1,reg2));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure TCgSparc.a_cmp_ref_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;const ref:TReference;reg:tregister;l:tasmlabel);
      var
        zeroreg,TempReg:TRegister;
      begin
        TempReg:=cg.get_scratch_reg_int(List,size);
        a_load_ref_reg(list,OS_32,Ref,TempReg);
        zeroreg.enum:=R_INTREGISTER;
        zeroreg.number:=NR_G0;
        list.concat(taicpu.op_reg_reg_reg(A_SUBcc,TempReg,Reg,zeroreg));
        a_jmp_cond(list,cmp_op,l);
        cg.free_scratch_reg(exprasmlist,TempReg);
      end;


    procedure TCgSparc.a_jmp_always(List:TAasmOutput;l:TAsmLabel);
      begin
        List.Concat(TAiCpu.op_sym(A_BA,objectlibrary.newasmsymbol(l.name)));
      end;


    procedure TCgSparc.a_jmp_cond(list:TAasmOutput;cond:TOpCmp;l:TAsmLabel);
      var
        ai:TAiCpu;
      begin
        ai:=TAiCpu.Op_sym(A_BA,l);
        ai.SetCondition(TOpCmp2AsmCond[cond]);
        list.Concat(ai);
        list.Concat(TAiCpu.Op_none(A_NOP));
      end;


    procedure TCgSparc.a_jmp_flags(list:TAasmOutput;const f:TResFlags;l:tasmlabel);
      var
        ai:taicpu;
      begin
        ai := Taicpu.op_sym(A_BA,l);
        ai.SetCondition(flags_to_cond(f));
        list.Concat(ai);
        list.Concat(TAiCpu.Op_none(A_NOP));
      end;


    procedure TCgSparc.g_flags2reg(list:TAasmOutput;Size:TCgSize;const f:tresflags;reg:TRegister);
      var
        ai : taicpu;
        r : tregister;
      begin
        r.enum:=R_PSR;
        ai:=Taicpu.Op_reg_reg(A_RDPSR,r,reg);
        ai.SetCondition(flags_to_cond(f));
        list.Concat(ai);
        list.Concat(TAiCpu.Op_none(A_NOP));
      end;


    procedure TCgSparc.g_overflowCheck(List:TAasmOutput;const p:TNode);
      var
        hl:TAsmLabel;
      begin
        if not(cs_check_overflow in aktlocalswitches) then
          exit;
        objectlibrary.getlabel(hl);
        if not((p.resulttype.def.deftype=pointerdef) or
           ((p.resulttype.def.deftype=orddef) and
            (torddef(p.resulttype.def).typ in [u64bit,u16bit,u32bit,u8bit,uchar,
                                             bool8bit,bool16bit,bool32bit]))) then
          begin
{$warning TODO Overflow check}
            a_jmp_always(list,hl)
          end
        else
          a_jmp_cond(list,OC_NONE,hl);
        a_call_name(list,'FPC_OVERFLOW');
        a_label(list,hl);
      end;


  { *********** entry/exit code and address loading ************ }

  procedure TCgSparc.g_stackframe_entry(list:TAasmOutput;LocalSize:LongInt);
    var
      r : tregister;
    begin
      { Althogh the SPARC architecture require only word alignment, software
        convention and the operating system require every stack frame to be double word
        aligned }
      LocalSize:=align(LocalSize,8);
      { Execute the SAVE instruction to get a new register window and create a new
        stack frame. In the "SAVE %i6,size,%i6" the first %i6 is related to the state
        before execution of the SAVE instrucion so it is the caller %i6, when the %i6
        after execution of that instruction is the called function stack pointer}
      r.enum:=R_INTREGISTER;
      r.number:=NR_STACK_POINTER_REG;
      list.concat(Taicpu.Op_reg_const_reg(A_SAVE,r,aword(-LocalSize),r));
    end;


  procedure TCgSparc.g_restore_all_registers(list:TaasmOutput;accused,acchiused:boolean);
    begin
      { The sparc port uses the sparc standard calling convetions so this function has no used }
    end;


  procedure TCgSparc.g_restore_frame_pointer(list:TAasmOutput);
    begin
       { This function intontionally does nothing as frame pointer is restored in the
         delay slot of the return instrucion done in g_return_from_proc}
    end;


  procedure TCgSparc.g_restore_standard_registers(list:taasmoutput;usedinproc:Tsupregset);
    begin
      { The sparc port uses the sparc standard calling convetions so this function has no used }
    end;


  procedure TCgSparc.g_return_from_proc(list:TAasmOutput;parasize:aword);
    var
      r : tregister;
      href : treference;
    begin
      { According to the SPARC ABI, the stack is cleared using the RESTORE instruction
        which is genereted in the g_restore_frame_pointer. Notice that SPARC has no
        RETURN instruction and that JMPL is used instead. The JMPL instrucion have one
        delay slot, so an inversion is possible such as
        JMPL  %i7+8,%g0
        RESTORE  %g0,0,%g0
        If no inversion we can use just
        RESTORE  %g0,0,%g0
        JMPL  %i7+8,%g0
        NOP
      }
      { Return address is computed by adding 8 to the CALL address saved onto %i6}
      r.enum:=R_INTREGISTER;
      r.number:=NR_I7;
      reference_reset_base(href,r,8);

      r.enum:=R_INTREGISTER;
      r.number:=NR_G0;
      list.concat(Taicpu.op_ref_reg(A_JMPL,href,r));
      { We use trivial restore in the delay slot of the JMPL instruction, as we
        already set result onto %i0 }
      list.concat(Taicpu.Op_reg_const_reg(A_RESTORE,r,0,r));
    end;


  procedure TCgSparc.g_save_all_registers(list : taasmoutput);
    begin
      { The sparc port uses the sparc standard calling convetions so this function has no used }
    end;


  procedure TCgSparc.g_save_standard_registers(list : taasmoutput; usedinproc:Tsupregset);
    begin
      { The sparc port uses the sparc standard calling convetions so this function has no used }
    end;


  procedure TCgSparc.a_loadaddr_ref_reg(list : TAasmOutput;const ref : TReference;r : tregister);
    var
       href : treference;
       hreg : tregister;
    begin
      if (r.number=ref.index.number) or (r.number=ref.base.number) then
        begin
        {$ifdef newra}
          hreg:=rg.getaddressregister(list);
        {$else}
          hreg := get_scratch_reg_address(list);
        {$endif}
        end
      else
        hreg:=r;
      if assigned(ref.symbol) then
        begin
          reference_reset_symbol(href,ref.symbol,ref.offset);
          href.symaddr:=refs_hi;
          list.concat(taicpu.op_ref_reg(A_SETHI,href,hreg));
          href.symaddr:=refs_hi;
          list.concat(taicpu.op_reg_ref_reg(A_OR,hreg,href,hreg));
          // if (ref.index<>R_NO)
        end;

      if hreg.number<>r.number then
        begin
           a_load_reg_reg(list,OS_ADDR,OS_ADDR,hreg,r);
         {$ifdef newra}
           rg.ungetregisterint(list,hreg);
         {$else}
           free_scratch_reg(list,hreg);
         {$endif}
        end;
    end;

  { ************* 64bit operations ************ }
      procedure TCg64Sparc.get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
        begin
          case op of
            OP_ADD :
              begin
                op1:=A_ADD;
                op2:=A_ADD;
              end;
            OP_SUB :
              begin
                op1:=A_SUB;
                op2:=A_SUB;
              end;
            OP_XOR :
              begin
                op1:=A_XOR;
                op2:=A_XOR;
              end;
            OP_OR :
              begin
                op1:=A_OR;
                op2:=A_OR;
              end;
            OP_AND :
              begin
                op1:=A_AND;
                op2:=A_AND;
              end;
            else
              internalerror(200203241);
          end;
        end;


      procedure TCg64Sparc.a_op64_ref_reg(list:TAasmOutput;op:TOpCG;const ref:TReference;reg:TRegister64);
        var
          op1,op2:TAsmOp;
          tempref:TReference;
        begin
          get_64bit_ops(op,op1,op2);
          list.concat(taicpu.op_ref_reg(op1,ref,reg.reglo));
          tempref:=ref;
          inc(tempref.offset,4);
          list.concat(taicpu.op_ref_reg(op2,tempref,reg.reghi));
        end;


      procedure TCg64Sparc.a_op64_reg_reg(list:TAasmOutput;op:TOpCG;regsrc,regdst:TRegister64);
        var
          op1,op2:TAsmOp;
        begin
          get_64bit_ops(op,op1,op2);
          list.concat(taicpu.op_reg_reg(op1,regsrc.reglo,regdst.reglo));
          list.concat(taicpu.op_reg_reg(op2,regsrc.reghi,regdst.reghi));
        end;


      procedure TCg64Sparc.a_op64_const_reg(list:TAasmOutput;op:TOpCG;value:qWord;regdst:TRegister64);
        var
          op1,op2:TAsmOp;
        begin
          case op of
            OP_AND,OP_OR,OP_XOR:
              begin
                cg.a_op_const_reg(list,op,Lo(Value),regdst.reglo);
                cg.a_op_const_reg(list,op,Hi(Value),regdst.reghi);
              end;
            OP_ADD, OP_SUB:
              begin
                {can't use a_op_const_ref because this may use dec/inc}
                get_64bit_ops(op,op1,op2);
                list.concat(taicpu.op_const_reg(op1,Lo(Value),regdst.reglo));
                list.concat(taicpu.op_const_reg(op2,Hi(Value),regdst.reghi));
              end;
            else
              internalerror(200204021);
          end;
        end;


  procedure TCg64Sparc.a_op64_const_ref(list:TAasmOutput;op:TOpCG;value:qWord;const ref:TReference);
    var
      op1,op2:TAsmOp;
      tempref:TReference;
    begin
      case op of
        OP_AND,OP_OR,OP_XOR:
          begin
            cg.a_op_const_ref(list,op,OS_32,Lo(Value),ref);
            tempref:=ref;
            inc(tempref.offset,4);
            cg.a_op_const_ref(list,op,OS_32,Hi(Value),tempref);
          end;
        OP_ADD, OP_SUB:
              begin
                get_64bit_ops(op,op1,op2);
                { can't use a_op_const_ref because this may use dec/inc}
  {              list.concat(taicpu.op_const_ref(op1,Lo(Value),ref));
                tempref:=ref;
                inc(tempref.offset,4);
                list.concat(taicpu.op_const_ref(op2,S_SW,Hi(Value),tempref));}
                InternalError(2002102101);
              end;
            else
              internalerror(200204022);
          end;
        end;


    { ************* concatcopy ************ }

    procedure TCgSparc.g_concatcopy(list:taasmoutput;const source,dest:treference;len:aword;delsource,loadref:boolean);
      var
        countreg: TRegister;
        src, dst: TReference;
        lab: tasmlabel;
        count, count2: aword;
        orgsrc, orgdst: boolean;
        r:Tregister;
      begin
        if len > high(longint) then
          internalerror(2002072704);
        { make sure short loads are handled as optimally as possible }
        if not loadref then
         begin
           if (len <= 8) and (byte(len) in [1,2,4,8]) then
            begin
              if len < 8 then
                begin
                  a_load_ref_ref(list,int_cgsize(len),source,dest);
                  if delsource then
                    reference_release(list,source);
                end
              else
                begin
                  r.enum:=R_F0;
                  a_reg_alloc(list,r);
                  a_loadfpu_ref_reg(list,OS_F64,source,r);
                  if delsource then
                    reference_release(list,source);
                  a_loadfpu_reg_ref(list,OS_F64,r,dest);
                  a_reg_dealloc(list,r);
                end;
              exit;
            end;
          end;
        reference_reset(src);
        reference_reset(dst);
        { load the address of source into src.base }
        if loadref then
          begin
            src.base := get_scratch_reg_address(list);
            a_load_ref_reg(list,OS_32,source,src.base);
            orgsrc := false;
          end
        else
         if not issimpleref(source) or
            (
              (source.index.number<>NR_NO) and
              ((source.offset+longint(len))>high(smallint))
            ) then
           begin
             src.base := get_scratch_reg_address(list);
             a_loadaddr_ref_reg(list,source,src.base);
             orgsrc := false;
           end
        else
          begin
            src := source;
            orgsrc := true;
          end;
        if not orgsrc and delsource then
          reference_release(list,source);
          { load the address of dest into dst.base }
        if not issimpleref(dest) or
           (
            (dest.index.number<>NR_NO) and
            ((dest.offset + longint(len)) > high(smallint))
           ) then
          begin
            dst.base := get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list,dest,dst.base);
            orgdst := false;
          end
        else
          begin
            dst := dest;
            orgdst := true;
          end;
        { generate a loop }
        count:=len div 8;
        if count>4 then
          begin
            { the offsets are zero after the a_loadaddress_ref_reg and just }
            { have to be set to 8. I put an Inc there so debugging may be   }
            { easier (should offset be different from zero here, it will be }
            { easy to notice in the generated assembler                     }
            inc(dst.offset,8);
            inc(src.offset,8);
            list.concat(taicpu.op_reg_const_reg(A_SUB,src.base,8,src.base));
            list.concat(taicpu.op_reg_const_reg(A_SUB,dst.base,8,dst.base));
            countreg := get_scratch_reg_int(list,OS_32);
            a_load_const_reg(list,OS_32,count,countreg);
            { explicitely allocate R_O0 since it can be used safely here }
            { (for holding date that's being copied)                    }
            r.enum:=R_F0;
            a_reg_alloc(list,r);
            objectlibrary.getlabel(lab);
            a_label(list, lab);
            list.concat(taicpu.op_reg_const_reg(A_SUB,countreg,1,countreg));
            list.concat(taicpu.op_ref_reg(A_LDF,src,r));
            list.concat(taicpu.op_reg_ref(A_STD,r,dst));
            //a_jmp(list,A_BC,C_NE,0,lab);
            free_scratch_reg(list,countreg);
            a_reg_dealloc(list,r);
            len := len mod 8;
          end;
        { unrolled loop }
        count:=len and 7;
        if count>0 then
          begin
            r.enum:=R_F0;
            a_reg_alloc(list,r);
            for count2 := 1 to count do
              begin
                a_loadfpu_ref_reg(list,OS_F64,src,r);
                a_loadfpu_reg_ref(list,OS_F64,r,dst);
                inc(src.offset,8);
                inc(dst.offset,8);
              end;
            a_reg_dealloc(list,r);
            len := len mod 8;
          end;
        if (len and 4) <> 0 then
          begin
            r.enum:=R_INTREGISTER;
            r.number:=NR_O0;
            a_reg_alloc(list,r);
            a_load_ref_reg(list,OS_32,src,r);
            a_load_reg_ref(list,OS_32,r,dst);
            inc(src.offset,4);
            inc(dst.offset,4);
            a_reg_dealloc(list,r);
          end;
        { copy the leftovers }
        if (len and 2) <> 0 then
          begin
            r.enum:=R_INTREGISTER;
            r.number:=NR_O0;
            a_reg_alloc(list,r);
            a_load_ref_reg(list,OS_16,src,r);
            a_load_reg_ref(list,OS_16,r,dst);
            inc(src.offset,2);
            inc(dst.offset,2);
            a_reg_dealloc(list,r);
          end;
        if (len and 1) <> 0 then
          begin
            r.enum:=R_INTREGISTER;
            r.number:=NR_O0;
            a_reg_alloc(list,r);
            a_load_ref_reg(list,OS_8,src,r);
            a_load_reg_ref(list,OS_8,r,dst);
            a_reg_dealloc(list,r);
          end;
        if orgsrc then
          begin
            if delsource then
              reference_release(list,source);
          end
        else
          free_scratch_reg(list,src.base);
        if not orgdst then
          free_scratch_reg(list,dst.base);
      end;


begin
  cg:=TCgSparc.Create;
  cg64:=TCg64Sparc.Create;
end.
{
  $Log$
  Revision 1.54  2003-05-31 01:00:51  peter
    * register fixes

  Revision 1.53  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.52  2003/05/28 23:18:31  florian
    * started to fix and clean up the sparc port

  Revision 1.51  2003/05/26 22:04:57  mazen
  * added 64 bit value support to fix a problem in RTL

  Revision 1.50  2003/05/23 22:33:48  florian
    * fix some small flaws which prevent sparc linux system unit from compiling
    * some reformatting done

  Revision 1.49  2003/05/22 16:11:22  florian
    * fixed sparc compilation partially

  Revision 1.48  2003/05/07 15:04:30  mazen
  * invalid genrated code for CASE statement fixed

  Revision 1.47  2003/05/06 20:25:20  mazen
  * Invalid genrated code : A_JMPL changed to A_BA

  Revision 1.46  2003/05/06 15:02:40  mazen
  * fixed a bug in a_load_const_reg related to max 13bit value limit
    for immediat value ==> use of A_SETHI for greater values

  Revision 1.45  2003/04/29 11:58:21  mazen
  * fixed bug of output generated assembler for a_cmp_const_ref_label

  Revision 1.44  2003/04/28 09:44:42  mazen
  + NOP after conditional jump instruction to prevent delay slot execution

  Revision 1.43  2003/04/27 11:21:36  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.42  2003/03/16 20:45:45  mazen
  * fixing an LD operation without refernce in loading address parameters

  Revision 1.41  2003/03/10 21:59:54  mazen
  * fixing index overflow in handling new registers arrays.

  Revision 1.40  2003/02/25 21:41:44  mazen
  * code re-aligned 2 spaces

  Revision 1.39  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.38  2003/02/18 22:00:20  mazen
  * asm condition generation modified by TAiCpu.SetCondition

  Revision 1.37  2003/02/05 21:48:34  mazen
  * fixing run time errors related to unimplemented abstract methods in CG
  + giving empty emplementations for some RTL functions

  Revision 1.36  2003/01/22 22:30:03  mazen
  - internal errors rmoved from a_loar_reg_reg when reg sizes differs from 32

  Revision 1.35  2003/01/20 22:21:36  mazen
  * many stuff related to RTL fixed

  Revision 1.34  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.33  2003/01/07 22:03:40  mazen
  * adding unequaln node support to sparc compiler

  Revision 1.32  2003/01/06 22:51:47  mazen
  * fixing bugs related to load_reg_ref

  Revision 1.31  2003/01/05 21:32:35  mazen
  * fixing several bugs compiling the RTL

  Revision 1.30  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.29  2002/12/25 20:59:49  mazen
  - many emitXXX removed from cga.pas in order to remove that file.

  Revision 1.28  2002/12/22 19:26:31  mazen
  * many internal errors related to unimplemented nodes are fixed

  Revision 1.27  2002/12/21 23:21:47  mazen
  + added support for the shift nodes
  + added debug output on screen with -an command line option

  Revision 1.26  2002/11/25 19:21:49  mazen
  * fixed support of nSparcInline

  Revision 1.25  2002/11/25 17:43:28  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.24  2002/11/17 17:49:09  mazen
  + return_result_reg and FUNCTION_RESULT_REG are now used, in all plateforms, to pass functions result between called function and its caller. See the explanation of each one

  Revision 1.23  2002/11/10 19:07:46  mazen
  * SPARC calling mechanism almost OK (as in GCC./mppcsparc )

  Revision 1.22  2002/11/06 11:31:24  mazen
  * op_reg_reg_reg don't need any more a TOpSize parameter

  Revision 1.21  2002/11/05 16:15:00  mazen
  *** empty log message ***

  Revision 1.20  2002/11/03 20:22:40  mazen
  * parameter handling updated

  Revision 1.19  2002/10/28 20:59:17  mazen
  * TOpSize values changed S_L --> S_SW

  Revision 1.18  2002/10/22 13:43:01  mazen
  - cga.pas redueced to an empty unit

  Revision 1.17  2002/10/20 19:01:38  mazen
  + op_raddr_reg and op_caddr_reg added to fix functions prologue

  Revision 1.16  2002/10/13 21:46:07  mazen
  * assembler output format fixed

  Revision 1.15  2002/10/11 13:35:14  mazen
  *** empty log message ***

  Revision 1.14  2002/10/10 19:57:51  mazen
  * Just to update repsitory

  Revision 1.13  2002/10/10 15:10:39  mazen
  * Internal error fixed, but usually i386 parameter model used

  Revision 1.12  2002/10/08 17:17:03  mazen
  *** empty log message ***

  Revision 1.11  2002/10/07 20:33:04  mazen
  word alignement modified in g_stack_frame

  Revision 1.10  2002/10/04 21:57:42  mazen
  * register allocation for parameters now done in cpupara, but InternalError(200109223) in cgcpu.pas:1053 is still not fixed du to location_force problem in ncgutils.pas:419

  Revision 1.9  2002/10/02 22:20:28  mazen
  + out registers allocator for the first 6 scalar parameters which must be passed into %o0..%o5

  Revision 1.8  2002/10/01 21:35:58  mazen
  + procedures exiting prologue added and stack frame now restored in the delay slot of the return (JMPL) instruction

  Revision 1.7  2002/10/01 21:06:29  mazen
  attinst.inc --> strinst.inc

  Revision 1.6  2002/10/01 17:41:50  florian
    * fixed log and id
}
