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
       globtype,parabase,
       cgbase,cgobj,cg64f32,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,
       node,symconst,SymType,
       rgcpu;

    type
      TCgSparc=class(tcg)
      protected
        function IsSimpleRef(const ref:treference):boolean;
     public
        procedure init_register_allocators;override;
        procedure done_register_allocators;override;
        function  getfpuregister(list:Taasmoutput;size:Tcgsize):Tregister;override;
        { sparc special, needed by cg64 }
        procedure make_simple_ref(list:taasmoutput;var ref: treference);
        procedure handle_load_store(list:taasmoutput;isstore:boolean;op: tasmop;reg:tregister;ref: treference);
        procedure handle_reg_const_reg(list:taasmoutput;op:Tasmop;src:tregister;a:aint;dst:tregister);
        { parameter }
        procedure a_param_const(list:TAasmOutput;size:tcgsize;a:aint;const paraloc:TCGPara);override;
        procedure a_param_ref(list:TAasmOutput;sz:tcgsize;const r:TReference;const paraloc:TCGPara);override;
        procedure a_paramaddr_ref(list:TAasmOutput;const r:TReference;const paraloc:TCGPara);override;
        procedure a_paramfpu_reg(list : taasmoutput;size : tcgsize;const r : tregister;const paraloc : TCGPara);override;
        procedure a_paramfpu_ref(list : taasmoutput;size : tcgsize;const ref : treference;const paraloc : TCGPara);override;
        procedure a_call_name(list:TAasmOutput;const s:string);override;
        procedure a_call_reg(list:TAasmOutput;Reg:TRegister);override;
        { General purpose instructions }
        procedure a_op_const_reg(list:TAasmOutput;Op:TOpCG;size:tcgsize;a:aint;reg:TRegister);override;
        procedure a_op_reg_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;src, dst:TRegister);override;
        procedure a_op_const_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;a:aint;src, dst:tregister);override;
        procedure a_op_reg_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;src1, src2, dst:tregister);override;
        procedure a_op_const_reg_reg_checkoverflow(list: taasmoutput; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister;setflags : boolean;var ovloc : tlocation);override;
        procedure a_op_reg_reg_reg_checkoverflow(list: taasmoutput; op: TOpCg; size: tcgsize; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation);override;
        { move instructions }
        procedure a_load_const_reg(list:TAasmOutput;size:tcgsize;a:aint;reg:tregister);override;
        procedure a_load_const_ref(list:TAasmOutput;size:tcgsize;a:aint;const ref:TReference);override;
        procedure a_load_reg_ref(list:TAasmOutput;FromSize,ToSize:TCgSize;reg:TRegister;const ref:TReference);override;
        procedure a_load_ref_reg(list:TAasmOutput;FromSize,ToSize:TCgSize;const ref:TReference;reg:tregister);override;
        procedure a_load_reg_reg(list:TAasmOutput;FromSize,ToSize:TCgSize;reg1,reg2:tregister);override;
        procedure a_loadaddr_ref_reg(list:TAasmOutput;const ref:TReference;r:tregister);override;
        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list:TAasmOutput;size:tcgsize;reg1, reg2:tregister);override;
        procedure a_loadfpu_ref_reg(list:TAasmOutput;size:tcgsize;const ref:TReference;reg:tregister);override;
        procedure a_loadfpu_reg_ref(list:TAasmOutput;size:tcgsize;reg:tregister;const ref:TReference);override;
        { comparison operations }
        procedure a_cmp_const_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aint;reg:tregister;l:tasmlabel);override;
        procedure a_cmp_reg_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;reg1,reg2:tregister;l:tasmlabel);override;
        procedure a_jmp_always(List:TAasmOutput;l:TAsmLabel);override;
        procedure a_jmp_name(list : taasmoutput;const s : string);override;
        procedure a_jmp_cond(list:TAasmOutput;cond:TOpCmp;l:tasmlabel);{ override;}
        procedure a_jmp_flags(list:TAasmOutput;const f:TResFlags;l:tasmlabel);override;
        procedure g_flags2reg(list:TAasmOutput;Size:TCgSize;const f:tresflags;reg:TRegister);override;
        procedure g_overflowCheck(List:TAasmOutput;const Loc:TLocation;def:TDef);override;
        procedure g_overflowCheck_loc(List:TAasmOutput;const Loc:TLocation;def:TDef;ovloc : tlocation);override;
        procedure g_proc_entry(list : taasmoutput;localsize : longint;nostackframe:boolean);override;
        procedure g_proc_exit(list : taasmoutput;parasize:longint;nostackframe:boolean);override;
        procedure g_restore_all_registers(list:TAasmOutput;const funcretparaloc:TCGPara);override;
        procedure g_restore_standard_registers(list:taasmoutput);override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_save_standard_registers(list : taasmoutput);override;
        procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aint;loadref : boolean);override;
        procedure g_concatcopy_unaligned(list : taasmoutput;const source,dest : treference;len : aint;loadref : boolean);override;
      end;

      TCg64Sparc=class(tcg64f32)
      private
        procedure get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      public
        procedure a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);override;
        procedure a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64);override;
        procedure a_param64_ref(list : taasmoutput;const r : treference;const paraloc : tcgpara);override;
        procedure a_op64_reg_reg(list:TAasmOutput;op:TOpCG;regsrc,regdst:TRegister64);override;
        procedure a_op64_const_reg(list:TAasmOutput;op:TOpCG;value:int64;regdst:TRegister64);override;
        procedure a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;value : int64;regsrc,regdst : tregister64);override;
        procedure a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;regsrc1,regsrc2,regdst : tregister64);override;
      end;

    const
      TOpCG2AsmOp : array[topcg] of TAsmOp=(
        A_NONE,A_ADD,A_AND,A_UDIV,A_SDIV,A_SMUL,A_UMUL,A_NEG,A_NOT,A_OR,A_SRA,A_SLL,A_SRL,A_SUB,A_XOR
      );
      TOpCG2AsmOpWithFlags : array[topcg] of TAsmOp=(
        A_NONE,A_ADDcc,A_ANDcc,A_UDIVcc,A_SDIVcc,A_SMULcc,A_UMULcc,A_NEG,A_NOT,A_ORcc,A_SRA,A_SLL,A_SRL,A_SUBcc,A_XORcc
      );
      TOpCmp2AsmCond : array[topcmp] of TAsmCond=(C_NONE,
        C_E,C_G,C_L,C_GE,C_LE,C_NE,C_BE,C_B,C_AE,C_A
      );


implementation

  uses
    globals,verbose,systems,cutils,
    symdef,paramgr,
    tgobj,cpupi,cgutils;


{****************************************************************************
                       This is private property, keep out! :)
****************************************************************************}

    function TCgSparc.IsSimpleRef(const ref:treference):boolean;
      begin
        if (ref.base=NR_NO) and (ref.index<>NR_NO) then
          InternalError(2002100804);
        result :=not(assigned(ref.symbol))and
                  (((ref.index = NR_NO) and
                   (ref.offset >= simm13lo) and
                    (ref.offset <= simm13hi)) or
                  ((ref.index <> NR_NO) and
                  (ref.offset = 0)));
      end;


    procedure tcgsparc.make_simple_ref(list:taasmoutput;var ref: treference);
      var
        tmpreg : tregister;
        tmpref : treference;
      begin
        tmpreg:=NR_NO;
        { Be sure to have a base register }
        if (ref.base=NR_NO) then
          begin
            ref.base:=ref.index;
            ref.index:=NR_NO;
          end;
        { When need to use SETHI, do it first }
        if assigned(ref.symbol) or
           (ref.offset<simm13lo) or
           (ref.offset>simm13hi) then
          begin
            tmpreg:=GetIntRegister(list,OS_INT);
            reference_reset(tmpref);
            tmpref.symbol:=ref.symbol;
            tmpref.offset:=ref.offset;
            tmpref.refaddr:=addr_hi;
            list.concat(taicpu.op_ref_reg(A_SETHI,tmpref,tmpreg));
            { Load the low part is left }
{$warning TODO Maybe not needed to load symbol}
            tmpref.refaddr:=addr_lo;
            list.concat(taicpu.op_reg_ref_reg(A_OR,tmpreg,tmpref,tmpreg));
            { The offset and symbol are loaded, reset in reference }
            ref.offset:=0;
            ref.symbol:=nil;
            { Only an index register or offset is allowed }
            if tmpreg<>NR_NO then
              begin
                if (ref.index<>NR_NO) then
                  begin
                    list.concat(taicpu.op_reg_reg_reg(A_ADD,tmpreg,ref.index,tmpreg));
                    ref.index:=tmpreg;
                  end
                else
                  begin
                    if ref.base<>NR_NO then
                      ref.index:=tmpreg
                    else
                      ref.base:=tmpreg;
                  end;
              end;
          end;
        if (ref.base<>NR_NO) then
          begin
            if (ref.index<>NR_NO) and
               ((ref.offset<>0) or assigned(ref.symbol)) then
              begin
                if tmpreg=NR_NO then
                  tmpreg:=GetIntRegister(list,OS_INT);
                list.concat(taicpu.op_reg_reg_reg(A_ADD,ref.base,ref.index,tmpreg));
                ref.base:=tmpreg;
                ref.index:=NR_NO;
              end;
          end;
      end;


    procedure tcgsparc.handle_load_store(list:taasmoutput;isstore:boolean;op: tasmop;reg:tregister;ref: treference);
      begin
        make_simple_ref(list,ref);
        if isstore then
          list.concat(taicpu.op_reg_ref(op,reg,ref))
        else
          list.concat(taicpu.op_ref_reg(op,ref,reg));
      end;


    procedure tcgsparc.handle_reg_const_reg(list:taasmoutput;op:Tasmop;src:tregister;a:aint;dst:tregister);
      var
        tmpreg : tregister;
      begin
        if (a<simm13lo) or
           (a>simm13hi) then
          begin
            tmpreg:=GetIntRegister(list,OS_INT);
            a_load_const_reg(list,OS_INT,a,tmpreg);
            list.concat(taicpu.op_reg_reg_reg(op,src,tmpreg,dst));
          end
        else
          list.concat(taicpu.op_reg_const_reg(op,src,a,dst));
      end;


{****************************************************************************
                              Assembler code
****************************************************************************}

    procedure Tcgsparc.init_register_allocators;
      begin
        inherited init_register_allocators;
        rg[R_INTREGISTER]:=Trgcpu.create(R_INTREGISTER,R_SUBD,
            [RS_O0,RS_O1,RS_O2,RS_O3,RS_O4,RS_O5,
             RS_L0,RS_L1,RS_L2,RS_L3,RS_L4,RS_L5,RS_L6,RS_L7],
            first_int_imreg,[]);
        rg[R_FPUREGISTER]:=trgcpu.create(R_FPUREGISTER,R_SUBFS,
            [RS_F0,RS_F1,RS_F2,RS_F3,RS_F4,RS_F5,RS_F6,RS_F7,
             RS_F8,RS_F9,RS_F10,RS_F11,RS_F12,RS_F13,RS_F14,RS_F15,
             RS_F16,RS_F17,RS_F18,RS_F19,RS_F20,RS_F21,RS_F22,RS_F23,
             RS_F24,RS_F25,RS_F26,RS_F27,RS_F28,RS_F29,RS_F30,RS_F31],
            first_fpu_imreg,[]);
      end;


    procedure Tcgsparc.done_register_allocators;
      begin
        rg[R_INTREGISTER].free;
        rg[R_FPUREGISTER].free;
        inherited done_register_allocators;
      end;


    function tcgsparc.getfpuregister(list:Taasmoutput;size:Tcgsize):Tregister;
      begin
        if size=OS_F64 then
          result:=rg[R_FPUREGISTER].getregister(list,R_SUBFD)
        else
          result:=rg[R_FPUREGISTER].getregister(list,R_SUBFS);
      end;


    procedure TCgSparc.a_param_const(list:TAasmOutput;size:tcgsize;a:aint;const paraloc:TCGPara);
      var
        Ref:TReference;
      begin
        paraloc.check_simple_location;
        case paraloc.location^.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_load_const_reg(list,size,a,paraloc.location^.register);
          LOC_REFERENCE:
            begin
              { Code conventions need the parameters being allocated in %o6+92 }
              with paraloc.location^.Reference do
                begin
                  if (Index=NR_SP) and (Offset<Target_info.first_parm_offset) then
                    InternalError(2002081104);
                  reference_reset_base(ref,index,offset);
                end;
              a_load_const_ref(list,size,a,ref);
            end;
          else
            InternalError(2002122200);
        end;
      end;


    procedure TCgSparc.a_param_ref(list:TAasmOutput;sz:TCgSize;const r:TReference;const paraloc:TCGPara);
      var
        ref: treference;
        tmpreg:TRegister;
      begin
        paraloc.check_simple_location;
        with paraloc.location^ do
          begin
            case loc of
              LOC_REGISTER,LOC_CREGISTER :
                a_load_ref_reg(list,sz,sz,r,Register);
              LOC_REFERENCE:
                begin
                  { Code conventions need the parameters being allocated in %o6+92 }
                  with Reference do
                    begin
                      if (Index=NR_SP) and (Offset<Target_info.first_parm_offset) then
                        InternalError(2002081104);
                      reference_reset_base(ref,index,offset);
                    end;
                  tmpreg:=GetIntRegister(list,OS_INT);
                  a_load_ref_reg(list,sz,sz,r,tmpreg);
                  a_load_reg_ref(list,sz,sz,tmpreg,ref);
                end;
              else
                internalerror(2002081103);
            end;
          end;
      end;


    procedure TCgSparc.a_paramaddr_ref(list:TAasmOutput;const r:TReference;const paraloc:TCGPara);
      var
        Ref:TReference;
        TmpReg:TRegister;
      begin
        paraloc.check_simple_location;
        with paraloc.location^ do
          begin
            case loc of
              LOC_REGISTER,LOC_CREGISTER:
                a_loadaddr_ref_reg(list,r,register);
              LOC_REFERENCE:
                begin
                  reference_reset(ref);
                  ref.base := reference.index;
                  ref.offset := reference.offset;
                  tmpreg:=GetAddressRegister(list);
                  a_loadaddr_ref_reg(list,r,tmpreg);
                  a_load_reg_ref(list,OS_ADDR,OS_ADDR,tmpreg,ref);
                end;
              else
                internalerror(2002080701);
            end;
          end;
      end;


    procedure tcgsparc.a_paramfpu_ref(list : taasmoutput;size : tcgsize;const ref : treference;const paraloc : TCGPara);
      var
         href,href2 : treference;
         hloc : pcgparalocation;
      begin
        href:=ref;
        hloc:=paraloc.location;
        while assigned(hloc) do
          begin
            case hloc^.loc of
              LOC_REGISTER :
                a_load_ref_reg(list,hloc^.size,hloc^.size,href,hloc^.register);
              LOC_REFERENCE :
                begin
                  reference_reset_base(href2,hloc^.reference.index,hloc^.reference.offset);
                  a_load_ref_ref(list,hloc^.size,hloc^.size,href,href2);
                end;
              else
                internalerror(200408241);
           end;
           inc(href.offset,tcgsize2size[hloc^.size]);
           hloc:=hloc^.next;
         end;
      end;


    procedure tcgsparc.a_paramfpu_reg(list : taasmoutput;size : tcgsize;const r : tregister;const paraloc : TCGPara);
      var
        href : treference;
      begin
        tg.GetTemp(list,TCGSize2Size[size],tt_normal,href);
        a_loadfpu_reg_ref(list,size,r,href);
        a_paramfpu_ref(list,size,href,paraloc);
        tg.Ungettemp(list,href);
      end;


    procedure TCgSparc.a_call_name(list:TAasmOutput;const s:string);
      begin
        list.concat(taicpu.op_sym(A_CALL,objectlibrary.newasmsymbol(s,AB_EXTERNAL,AT_FUNCTION)));
        { Delay slot }
        list.concat(taicpu.op_none(A_NOP));
      end;


    procedure TCgSparc.a_call_reg(list:TAasmOutput;Reg:TRegister);
      begin
        list.concat(taicpu.op_reg(A_CALL,reg));
        { Delay slot }
        list.concat(taicpu.op_none(A_NOP));
     end;


    {********************** load instructions ********************}

    procedure TCgSparc.a_load_const_reg(list : TAasmOutput;size : TCGSize;a : aint;reg : TRegister);
      begin
        { we don't use the set instruction here because it could be evalutated to two
          instructions which would cause problems with the delay slot (FK) }
        if (a=0) then
          list.concat(taicpu.op_reg(A_CLR,reg))
        { sethi allows to set the upper 22 bit, so we'll take full advantage of it }
        else if (a and aint($1fff))=0 then
          list.concat(taicpu.op_const_reg(A_SETHI,a shr 10,reg))
        else if (a>=simm13lo) and (a<=simm13hi) then
          list.concat(taicpu.op_const_reg(A_MOV,a,reg))
        else
          begin
            list.concat(taicpu.op_const_reg(A_SETHI,a shr 10,reg));
            list.concat(taicpu.op_reg_const_reg(A_OR,reg,a and aint($3ff),reg));
          end;
      end;


    procedure TCgSparc.a_load_const_ref(list : TAasmOutput;size : tcgsize;a : aint;const ref : TReference);
      begin
        if a=0 then
          a_load_reg_ref(list,size,size,NR_G0,ref)
        else
          inherited a_load_const_ref(list,size,a,ref);
      end;


    procedure TCgSparc.a_load_reg_ref(list:TAasmOutput;FromSize,ToSize:TCGSize;reg:tregister;const Ref:TReference);
      var
        op : tasmop;
      begin
        if (TCGSize2Size[fromsize] >= TCGSize2Size[tosize]) then
          fromsize := tosize;
        case fromsize of
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
        handle_load_store(list,true,op,reg,ref);
      end;


    procedure TCgSparc.a_load_ref_reg(list:TAasmOutput;FromSize,ToSize:TCgSize;const ref:TReference;reg:tregister);
      var
        op : tasmop;
      begin
        if (TCGSize2Size[fromsize] >= TCGSize2Size[tosize]) then
          fromsize := tosize;
        case fromsize of
          OS_S8:
            Op:=A_LDSB;{Load Signed Byte}
          OS_8:
            Op:=A_LDUB;{Load Unsigned Byte}
          OS_S16:
            Op:=A_LDSH;{Load Signed Halfword}
          OS_16:
            Op:=A_LDUH;{Load Unsigned Halfword}
          OS_S32,
          OS_32:
            Op:=A_LD;{Load Word}
          OS_S64,
          OS_64:
            Op:=A_LDD;{Load a Long Word}
          else
            InternalError(2002122101);
        end;
        handle_load_store(list,false,op,reg,ref);
      end;


    procedure TCgSparc.a_load_reg_reg(list:TAasmOutput;fromsize,tosize:tcgsize;reg1,reg2:tregister);
      var
        instr : taicpu;
      begin
        if (tcgsize2size[tosize]<tcgsize2size[fromsize]) or
           (
            (tcgsize2size[tosize] = tcgsize2size[fromsize]) and
            (tosize <> fromsize) and
            not(fromsize in [OS_32,OS_S32])
           ) then
          begin
            case tosize of
              OS_8 :
                a_op_const_reg_reg(list,OP_AND,tosize,$ff,reg1,reg2);
              OS_16 :
                a_op_const_reg_reg(list,OP_AND,tosize,$ffff,reg1,reg2);
              OS_32,
              OS_S32 :
                begin
                  instr:=taicpu.op_reg_reg(A_MOV,reg1,reg2);
                  list.Concat(instr);
                  { Notify the register allocator that we have written a move instruction so
                   it can try to eliminate it. }
                  add_move_instruction(instr);
                end;
              OS_S8 :
                begin
                  list.concat(taicpu.op_reg_const_reg(A_SLL,reg1,24,reg2));
                  list.concat(taicpu.op_reg_const_reg(A_SRA,reg2,24,reg2));
                end;
              OS_S16 :
                begin
                  list.concat(taicpu.op_reg_const_reg(A_SLL,reg1,16,reg2));
                  list.concat(taicpu.op_reg_const_reg(A_SRA,reg2,16,reg2));
                end;
              else
                internalerror(2002090901);
            end;
          end
        else
          begin
            if reg1<>reg2 then
              begin
                { same size, only a register mov required }
                instr:=taicpu.op_reg_reg(A_MOV,reg1,reg2);
                list.Concat(instr);
                { Notify the register allocator that we have written a move instruction so
                  it can try to eliminate it. }
                add_move_instruction(instr);
              end;
          end;
      end;


    procedure TCgSparc.a_loadaddr_ref_reg(list : TAasmOutput;const ref : TReference;r : tregister);
      var
         tmpref : treference;
         hreg : tregister;
      begin
        if (ref.base=NR_NO) and (ref.index<>NR_NO) then
          internalerror(200306171);
        { At least big offset (need SETHI), maybe base and maybe index }
        if assigned(ref.symbol) or
           (ref.offset<simm13lo) or
           (ref.offset>simm13hi) then
          begin
            hreg:=GetAddressRegister(list);
            reference_reset(tmpref);
            tmpref.symbol := ref.symbol;
            tmpref.offset := ref.offset;
            tmpref.refaddr := addr_hi;
            list.concat(taicpu.op_ref_reg(A_SETHI,tmpref,hreg));
            { Only the low part is left }
            tmpref.refaddr:=addr_lo;
            list.concat(taicpu.op_reg_ref_reg(A_OR,hreg,tmpref,hreg));
            if ref.base<>NR_NO then
              begin
                if ref.index<>NR_NO then
                  begin
                    list.concat(taicpu.op_reg_reg_reg(A_ADD,hreg,ref.base,hreg));
                    list.concat(taicpu.op_reg_reg_reg(A_ADD,hreg,ref.index,r));
                  end
                else
                  list.concat(taicpu.op_reg_reg_reg(A_ADD,hreg,ref.base,r));
              end
            else
              begin
                if hreg<>r then
                  a_load_reg_reg(list,OS_ADDR,OS_ADDR,hreg,r);
              end;
          end
        else
        { At least small offset, maybe base and maybe index }
          if ref.offset<>0 then
            begin
              if ref.base<>NR_NO then
                begin
                  if ref.index<>NR_NO then
                    begin
                      hreg:=GetAddressRegister(list);
                      list.concat(taicpu.op_reg_const_reg(A_ADD,ref.base,ref.offset,hreg));
                      list.concat(taicpu.op_reg_reg_reg(A_ADD,hreg,ref.index,r));
                    end
                  else
                    list.concat(taicpu.op_reg_const_reg(A_ADD,ref.base,ref.offset,r));
                end
              else
                list.concat(taicpu.op_const_reg(A_MOV,ref.offset,r));
            end
        else
        { Both base and index }
          if ref.index<>NR_NO then
            list.concat(taicpu.op_reg_reg_reg(A_ADD,ref.base,ref.index,r))
        else
        { Only base }
          if ref.base<>NR_NO then
            a_load_reg_reg(list,OS_ADDR,OS_ADDR,ref.base,r)
        else
          { only offset, can be generated by absolute }
          a_load_const_reg(list,OS_ADDR,ref.offset,r);
      end;


    procedure TCgSparc.a_loadfpu_reg_reg(list:TAasmOutput;size:tcgsize;reg1, reg2:tregister);
      const
         FpuMovInstr : Array[OS_F32..OS_F64] of TAsmOp =
           (A_FMOVS,A_FMOVD);
      var
        instr : taicpu;
      begin
        if reg1<>reg2 then
          begin
            instr:=taicpu.op_reg_reg(fpumovinstr[size],reg1,reg2);
            list.Concat(instr);
            { Notify the register allocator that we have written a move instruction so
              it can try to eliminate it. }
            add_move_instruction(instr);
          end;
      end;


    procedure TCgSparc.a_loadfpu_ref_reg(list:TAasmOutput;size:tcgsize;const ref:TReference;reg:tregister);
       const
         FpuLoadInstr : Array[OS_F32..OS_F64] of TAsmOp =
           (A_LDF,A_LDDF);
       begin
         handle_load_store(list,false,fpuloadinstr[size],reg,ref);
       end;


     procedure TCgSparc.a_loadfpu_reg_ref(list:TAasmOutput;size:tcgsize;reg:tregister;const ref:TReference);
       const
         FpuLoadInstr : Array[OS_F32..OS_F64] of TAsmOp =
           (A_STF,A_STDF);
       begin
         handle_load_store(list,true,fpuloadinstr[size],reg,ref);
       end;


    procedure TCgSparc.a_op_const_reg(list:TAasmOutput;Op:TOpCG;size:tcgsize;a:aint;reg:TRegister);
      begin
        if Op in [OP_NEG,OP_NOT] then
          internalerror(200306011);
        if (a=0) then
          list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp[op],reg,NR_G0,reg))
        else
          handle_reg_const_reg(list,TOpCG2AsmOp[op],reg,a,reg);
      end;


    procedure TCgSparc.a_op_reg_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;src, dst:TRegister);
      var
        a : aint;
      begin
        Case Op of
          OP_NEG :
            list.concat(taicpu.op_reg_reg(TOpCG2AsmOp[op],src,dst));
          OP_NOT :
            begin
              case size of
                OS_8 :
                  a:=aint($ffffff00);
                OS_16 :
                  a:=aint($ffff0000);
                else
                  a:=0;
              end;
              handle_reg_const_reg(list,A_XNOR,src,a,dst);
            end;
          else
            list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp[op],dst,src,dst));
        end;
      end;


    procedure TCgSparc.a_op_const_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;a:aint;src, dst:tregister);
      var
        power : longInt;
      begin
        case op of
          OP_MUL,
          OP_IMUL:
            begin
              if ispowerof2(a,power) then
                begin
                  { can be done with a shift }
                  inherited a_op_const_reg_reg(list,op,size,a,src,dst);
                  exit;
                end;
            end;
          OP_SUB,
          OP_ADD :
            begin
              if (a=0) then
                begin
                  a_load_reg_reg(list,size,size,src,dst);
                  exit;
                end;
            end;
        end;
        handle_reg_const_reg(list,TOpCG2AsmOp[op],src,a,dst);
      end;


    procedure TCgSparc.a_op_reg_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;src1, src2, dst:tregister);
      begin
        list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp[op],src2,src1,dst));
      end;


    procedure tcgsparc.a_op_const_reg_reg_checkoverflow(list: taasmoutput; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister;setflags : boolean;var ovloc : tlocation);
      var
        power : longInt;
        tmpreg1,tmpreg2 : tregister;
      begin
        ovloc.loc:=LOC_VOID;
        case op of
          OP_SUB,
          OP_ADD :
            begin
              if (a=0) then
                begin
                  a_load_reg_reg(list,size,size,src,dst);
                  exit;
                end;
            end;
        end;
        if setflags then
          begin
            handle_reg_const_reg(list,TOpCG2AsmOpWithFlags[op],src,a,dst);
            case op of
              OP_MUL:
                begin
                  tmpreg1:=GetIntRegister(list,OS_INT);
                  list.concat(taicpu.op_reg_reg(A_MOV,NR_Y,tmpreg1));
                  list.concat(taicpu.op_reg_reg(A_CMP,NR_G0,tmpreg1));
                  ovloc.loc:=LOC_FLAGS;
                  ovloc.resflags:=F_NE;
                end;
              OP_IMUL:
                begin
                  tmpreg1:=GetIntRegister(list,OS_INT);
                  tmpreg2:=GetIntRegister(list,OS_INT);
                  list.concat(taicpu.op_reg_reg(A_MOV,NR_Y,tmpreg1));
                  list.concat(taicpu.op_reg_const_reg(A_SRL,dst,31,tmpreg2));
                  list.concat(taicpu.op_reg_reg(A_CMP,tmpreg1,tmpreg2));
                  ovloc.loc:=LOC_FLAGS;
                  ovloc.resflags:=F_NE;
                end;
            end;
          end
        else
          handle_reg_const_reg(list,TOpCG2AsmOp[op],src,a,dst)
      end;


    procedure tcgsparc.a_op_reg_reg_reg_checkoverflow(list: taasmoutput; op: TOpCg; size: tcgsize; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation);
      var
        tmpreg1,tmpreg2 : tregister;
      begin
        ovloc.loc:=LOC_VOID;
        if setflags then
          begin
            list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOpWithFlags[op],src2,src1,dst));
            case op of
              OP_MUL:
                begin
                  tmpreg1:=GetIntRegister(list,OS_INT);
                  list.concat(taicpu.op_reg_reg(A_MOV,NR_Y,tmpreg1));
                  list.concat(taicpu.op_reg_reg(A_CMP,NR_G0,tmpreg1));
                  ovloc.loc:=LOC_FLAGS;
                  ovloc.resflags:=F_NE;
                end;
              OP_IMUL:
                begin
                  tmpreg1:=GetIntRegister(list,OS_INT);
                  tmpreg2:=GetIntRegister(list,OS_INT);
                  list.concat(taicpu.op_reg_reg(A_MOV,NR_Y,tmpreg1));
                  list.concat(taicpu.op_reg_const_reg(A_SRL,dst,31,tmpreg2));
                  list.concat(taicpu.op_reg_reg(A_CMP,tmpreg1,tmpreg2));
                  ovloc.loc:=LOC_FLAGS;
                  ovloc.resflags:=F_NE;
                end;
            end;
          end
        else
          list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp[op],src2,src1,dst))
      end;



  {*************** compare instructructions ****************}

    procedure TCgSparc.a_cmp_const_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aint;reg:tregister;l:tasmlabel);
      begin
        if (a=0) then
          list.concat(taicpu.op_reg_reg_reg(A_SUBcc,reg,NR_G0,NR_G0))
        else
          handle_reg_const_reg(list,A_SUBcc,reg,a,NR_G0);
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure TCgSparc.a_cmp_reg_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;reg1,reg2:tregister;l:tasmlabel);
      begin
        list.concat(taicpu.op_reg_reg_reg(A_SUBcc,reg2,reg1,NR_G0));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure TCgSparc.a_jmp_always(List:TAasmOutput;l:TAsmLabel);
      begin
        List.Concat(TAiCpu.op_sym(A_BA,objectlibrary.newasmsymbol(l.name,AB_EXTERNAL,AT_FUNCTION)));
        { Delay slot }
        list.Concat(TAiCpu.Op_none(A_NOP));
      end;


    procedure tcgsparc.a_jmp_name(list : taasmoutput;const s : string);
      begin
        List.Concat(TAiCpu.op_sym(A_BA,objectlibrary.newasmsymbol(s,AB_EXTERNAL,AT_FUNCTION)));
        { Delay slot }
        list.Concat(TAiCpu.Op_none(A_NOP));
      end;


    procedure TCgSparc.a_jmp_cond(list:TAasmOutput;cond:TOpCmp;l:TAsmLabel);
      var
        ai:TAiCpu;
      begin
        ai:=TAiCpu.Op_sym(A_Bxx,l);
        ai.SetCondition(TOpCmp2AsmCond[cond]);
        list.Concat(ai);
        { Delay slot }
        list.Concat(TAiCpu.Op_none(A_NOP));
      end;


    procedure TCgSparc.a_jmp_flags(list:TAasmOutput;const f:TResFlags;l:tasmlabel);
      var
        ai : taicpu;
        op : tasmop;
      begin
        if f in [F_FE,F_FNE,F_FG,F_FL,F_FGE,F_FLE] then
          op:=A_FBxx
        else
          op:=A_Bxx;
        ai := Taicpu.op_sym(op,l);
        ai.SetCondition(flags_to_cond(f));
        list.Concat(ai);
        { Delay slot }
        list.Concat(TAiCpu.Op_none(A_NOP));
      end;


    procedure TCgSparc.g_flags2reg(list:TAasmOutput;Size:TCgSize;const f:tresflags;reg:TRegister);
      var
        hl : tasmlabel;
      begin
        objectlibrary.getlabel(hl);
        a_load_const_reg(list,size,1,reg);
        a_jmp_flags(list,f,hl);
        a_load_const_reg(list,size,0,reg);
        a_label(list,hl);
      end;


    procedure tcgsparc.g_overflowCheck(List:TAasmOutput;const Loc:TLocation;def:TDef);
      var
        l : tlocation;
      begin
        l.loc:=LOC_VOID;
        g_overflowCheck_loc(list,loc,def,l);
      end;


    procedure TCgSparc.g_overflowCheck_loc(List:TAasmOutput;const Loc:TLocation;def:TDef;ovloc : tlocation);
      var
        hl : tasmlabel;
        ai:TAiCpu;
        hflags : tresflags;
      begin
        if not(cs_check_overflow in aktlocalswitches) then
          exit;
        objectlibrary.getlabel(hl);
        case ovloc.loc of
          LOC_VOID:
            begin
              if not((def.deftype=pointerdef) or
                    ((def.deftype=orddef) and
                     (torddef(def).typ in [u64bit,u16bit,u32bit,u8bit,uchar,bool8bit,bool16bit,bool32bit]))) then
                begin
                  ai:=TAiCpu.Op_sym(A_Bxx,hl);
                  ai.SetCondition(C_NO);
                  list.Concat(ai);
                  { Delay slot }
                  list.Concat(TAiCpu.Op_none(A_NOP));
                end
              else
                a_jmp_cond(list,OC_AE,hl);
            end;
          LOC_FLAGS:
            begin
              hflags:=ovloc.resflags;
              inverse_flags(hflags);
              cg.a_jmp_flags(list,hflags,hl);
            end;
          else
            internalerror(200409281);
        end;

        a_call_name(list,'FPC_OVERFLOW');
        a_label(list,hl);
      end;

  { *********** entry/exit code and address loading ************ }

    procedure TCgSparc.g_proc_entry(list : taasmoutput;localsize : longint;nostackframe:boolean);
      begin
        if nostackframe then
          exit;
        { Althogh the SPARC architecture require only word alignment, software
          convention and the operating system require every stack frame to be double word
          aligned }
        LocalSize:=align(LocalSize,8);
        { Execute the SAVE instruction to get a new register window and create a new
          stack frame. In the "SAVE %i6,size,%i6" the first %i6 is related to the state
          before execution of the SAVE instrucion so it is the caller %i6, when the %i6
          after execution of that instruction is the called function stack pointer}
        { constant can be 13 bit signed, since it's negative, size can be max. 4096 }
        if LocalSize>4096 then
          begin
            a_load_const_reg(list,OS_ADDR,-LocalSize,NR_G1);
            list.concat(Taicpu.Op_reg_reg_reg(A_SAVE,NR_STACK_POINTER_REG,NR_G1,NR_STACK_POINTER_REG));
          end
        else
          list.concat(Taicpu.Op_reg_const_reg(A_SAVE,NR_STACK_POINTER_REG,-LocalSize,NR_STACK_POINTER_REG));
      end;


    procedure TCgSparc.g_restore_all_registers(list:TaasmOutput;const funcretparaloc:TCGPara);
      begin
        { The sparc port uses the sparc standard calling convetions so this function has no used }
      end;


    procedure TCgSparc.g_restore_standard_registers(list:taasmoutput);
      begin
        { The sparc port uses the sparc standard calling convetions so this function has no used }
      end;


    procedure TCgSparc.g_proc_exit(list : taasmoutput;parasize:longint;nostackframe:boolean);
      begin
        if nostackframe then
          begin
            { Here we need to use RETL instead of RET so it uses %o7 }
            list.concat(Taicpu.op_none(A_RETL));
            list.concat(Taicpu.op_none(A_NOP))
          end
        else
          begin
            { We use trivial restore in the delay slot of the JMPL instruction, as we
              already set result onto %i0 }
            list.concat(Taicpu.op_none(A_RET));
            list.concat(Taicpu.op_none(A_RESTORE));
          end;
      end;


    procedure TCgSparc.g_save_all_registers(list : taasmoutput);
      begin
        { The sparc port uses the sparc standard calling convetions so this function has no used }
      end;


    procedure TCgSparc.g_save_standard_registers(list : taasmoutput);
      begin
        { The sparc port uses the sparc standard calling convetions so this function has no used }
      end;


    { ************* concatcopy ************ }

    procedure TCgSparc.g_concatcopy(list:taasmoutput;const source,dest:treference;len:aint;loadref:boolean);
      var
        tmpreg1,
        hreg,
        countreg: TRegister;
        src, dst: TReference;
        lab: tasmlabel;
        count, count2: aint;
        orgsrc, orgdst: boolean;
      begin
        if len>high(longint) then
          internalerror(2002072704);
        reference_reset(src);
        reference_reset(dst);
        { load the address of source into src.base }
        if loadref then
          begin
            src.base:=GetAddressRegister(list);
            a_load_ref_reg(list,OS_32,OS_32,source,src.base);
            orgsrc := false;
          end
        else
          begin
            src.base:=GetAddressRegister(list);
            a_loadaddr_ref_reg(list,source,src.base);
            orgsrc := false;
          end;
          { load the address of dest into dst.base }
        dst.base:=GetAddressRegister(list);
        a_loadaddr_ref_reg(list,dest,dst.base);
        orgdst := false;
        { generate a loop }
        count:=len div 4;
        if count>4 then
          begin
            { the offsets are zero after the a_loadaddress_ref_reg and just }
            { have to be set to 8. I put an Inc there so debugging may be   }
            { easier (should offset be different from zero here, it will be }
            { easy to notice in the generated assembler                     }
            countreg:=GetIntRegister(list,OS_INT);
            tmpreg1:=GetIntRegister(list,OS_INT);
            a_load_const_reg(list,OS_INT,count,countreg);
            { explicitely allocate R_O0 since it can be used safely here }
            { (for holding date that's being copied)                    }
            objectlibrary.getlabel(lab);
            a_label(list, lab);
            list.concat(taicpu.op_ref_reg(A_LD,src,tmpreg1));
            list.concat(taicpu.op_reg_ref(A_ST,tmpreg1,dst));
            list.concat(taicpu.op_reg_const_reg(A_ADD,src.base,4,src.base));
            list.concat(taicpu.op_reg_const_reg(A_ADD,dst.base,4,dst.base));
            list.concat(taicpu.op_reg_const_reg(A_SUBcc,countreg,1,countreg));
            a_jmp_cond(list,OC_NE,lab);
            list.concat(taicpu.op_none(A_NOP));
            { keep the registers alive }
            list.concat(taicpu.op_reg_reg(A_MOV,countreg,countreg));
            list.concat(taicpu.op_reg_reg(A_MOV,src.base,src.base));
            list.concat(taicpu.op_reg_reg(A_MOV,dst.base,dst.base));
            len := len mod 4;
          end;
        { unrolled loop }
        count:=len div 4;
        if count>0 then
          begin
            tmpreg1:=GetIntRegister(list,OS_INT);
            for count2 := 1 to count do
              begin
                list.concat(taicpu.op_ref_reg(A_LD,src,tmpreg1));
                list.concat(taicpu.op_reg_ref(A_ST,tmpreg1,dst));
                inc(src.offset,4);
                inc(dst.offset,4);
              end;
            len := len mod 4;
          end;
        if (len and 4) <> 0 then
          begin
            hreg:=GetIntRegister(list,OS_INT);
            a_load_ref_reg(list,OS_32,OS_32,src,hreg);
            a_load_reg_ref(list,OS_32,OS_32,hreg,dst);
            inc(src.offset,4);
            inc(dst.offset,4);
          end;
        { copy the leftovers }
        if (len and 2) <> 0 then
          begin
            hreg:=GetIntRegister(list,OS_INT);
            a_load_ref_reg(list,OS_16,OS_16,src,hreg);
            a_load_reg_ref(list,OS_16,OS_16,hreg,dst);
            inc(src.offset,2);
            inc(dst.offset,2);
          end;
        if (len and 1) <> 0 then
          begin
            hreg:=GetIntRegister(list,OS_INT);
            a_load_ref_reg(list,OS_8,OS_8,src,hreg);
            a_load_reg_ref(list,OS_8,OS_8,hreg,dst);
          end;
      end;


    procedure tcgsparc.g_concatcopy_unaligned(list : taasmoutput;const source,dest : treference;len : aint;loadref : boolean);
      var
        paraloc1,paraloc2,paraloc3 : TCGPara;
      begin
        paraloc1.init;
        paraloc2.init;
        paraloc3.init;
        paramanager.getintparaloc(pocall_default,1,paraloc1);
        paramanager.getintparaloc(pocall_default,2,paraloc2);
        paramanager.getintparaloc(pocall_default,3,paraloc3);
        paramanager.allocparaloc(list,paraloc3);
        a_param_const(list,OS_INT,len,paraloc3);
        paramanager.allocparaloc(list,paraloc2);
        a_paramaddr_ref(list,dest,paraloc2);
        paramanager.allocparaloc(list,paraloc2);
        if loadref then
          a_param_ref(list,OS_ADDR,source,paraloc1)
        else
          a_paramaddr_ref(list,source,paraloc1);
        paramanager.freeparaloc(list,paraloc3);
        paramanager.freeparaloc(list,paraloc2);
        paramanager.freeparaloc(list,paraloc1);
        alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        alloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        a_call_name(list,'FPC_MOVE');
        dealloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        paraloc3.done;
        paraloc2.done;
        paraloc1.done;
      end;

{****************************************************************************
                               TCG64Sparc
****************************************************************************}


    procedure tcg64sparc.a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);
      var
        tmpref: treference;
      begin
        { Override this function to prevent loading the reference twice }
        tmpref:=ref;
        tcgsparc(cg).make_simple_ref(list,tmpref);
        cg.a_load_reg_ref(list,OS_32,OS_32,reg.reghi,tmpref);
        inc(tmpref.offset,4);
        cg.a_load_reg_ref(list,OS_32,OS_32,reg.reglo,tmpref);
      end;


    procedure tcg64sparc.a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64);
      var
        tmpref: treference;
      begin
        { Override this function to prevent loading the reference twice }
        tmpref:=ref;
        tcgsparc(cg).make_simple_ref(list,tmpref);
        cg.a_load_ref_reg(list,OS_32,OS_32,tmpref,reg.reghi);
        inc(tmpref.offset,4);
        cg.a_load_ref_reg(list,OS_32,OS_32,tmpref,reg.reglo);
      end;


    procedure tcg64sparc.a_param64_ref(list : taasmoutput;const r : treference;const paraloc : tcgpara);
      var
        hreg64 : tregister64;
      begin
        { Override this function to prevent loading the reference twice.
          Use here some extra registers, but those are optimized away by the RA }
        hreg64.reglo:=cg.GetIntRegister(list,OS_32);
        hreg64.reghi:=cg.GetIntRegister(list,OS_32);
        a_load64_ref_reg(list,r,hreg64);
        a_param64_reg(list,hreg64,paraloc);
      end;


    procedure TCg64Sparc.get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      begin
        case op of
          OP_ADD :
            begin
              op1:=A_ADDCC;
              op2:=A_ADDX;
            end;
          OP_SUB :
            begin
              op1:=A_SUBCC;
              op2:=A_SUBX;
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


    procedure TCg64Sparc.a_op64_reg_reg(list:TAasmOutput;op:TOpCG;regsrc,regdst:TRegister64);
      var
        op1,op2 : TAsmOp;
      begin
        case op of
          OP_NEG :
            begin
              { Use the simple code: y=0-z }
              list.concat(taicpu.op_reg_reg_reg(A_SUBcc,NR_G0,regsrc.reglo,regdst.reglo));
              list.concat(taicpu.op_reg_reg_reg(A_SUBX,NR_G0,regsrc.reghi,regdst.reghi));
              exit;
            end;
          OP_NOT :
            begin
              list.concat(taicpu.op_reg_reg_reg(A_XNOR,regsrc.reglo,NR_G0,regdst.reglo));
              list.concat(taicpu.op_reg_reg_reg(A_XNOR,regsrc.reghi,NR_G0,regdst.reghi));
              exit;
            end;
        end;
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_reg_reg_reg(op1,regdst.reglo,regsrc.reglo,regdst.reglo));
        list.concat(taicpu.op_reg_reg_reg(op2,regdst.reghi,regsrc.reghi,regdst.reghi));
      end;


    procedure TCg64Sparc.a_op64_const_reg(list:TAasmOutput;op:TOpCG;value:int64;regdst:TRegister64);
      var
        op1,op2:TAsmOp;
      begin
        case op of
          OP_NEG,
          OP_NOT :
            internalerror(200306017);
        end;
        get_64bit_ops(op,op1,op2);
        tcgsparc(cg).handle_reg_const_reg(list,op1,regdst.reglo,aint(lo(value)),regdst.reglo);
        tcgsparc(cg).handle_reg_const_reg(list,op2,regdst.reghi,aint(hi(value)),regdst.reghi);
      end;


    procedure tcg64sparc.a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;value : int64; regsrc,regdst : tregister64);
      var
        op1,op2:TAsmOp;
      begin
        case op of
          OP_NEG,
          OP_NOT :
            internalerror(200306017);
        end;
        get_64bit_ops(op,op1,op2);
        tcgsparc(cg).handle_reg_const_reg(list,op1,regsrc.reglo,aint(lo(value)),regdst.reglo);
        tcgsparc(cg).handle_reg_const_reg(list,op2,regsrc.reghi,aint(hi(value)),regdst.reghi);
      end;


    procedure tcg64sparc.a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;regsrc1,regsrc2,regdst : tregister64);
      var
        op1,op2:TAsmOp;
      begin
        case op of
          OP_NEG,
          OP_NOT :
            internalerror(200306017);
        end;
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_reg_reg_reg(op1,regsrc2.reglo,regsrc1.reglo,regdst.reglo));
        list.concat(taicpu.op_reg_reg_reg(op2,regsrc2.reghi,regsrc1.reghi,regdst.reghi));
      end;


begin
  cg:=TCgSparc.Create;
  cg64:=TCg64Sparc.Create;
end.
{
  $Log$
  Revision 1.95  2004-10-10 20:51:46  peter
    * fixed sparc compile
    * fixed float regvar loading

  Revision 1.94  2004/10/10 20:31:48  peter
    * concatcopy_unaligned maps by default to concatcopy, sparc will
      override it with call to fpc_move

  Revision 1.93  2004/09/29 18:55:40  florian
    * fixed more sparc overflow stuff
    * fixed some op64 stuff for sparc

  Revision 1.92  2004/09/27 21:24:17  peter
    * fixed passing of flaot parameters. The general size is still float,
      only the size of the locations is now OS_32

  Revision 1.91  2004/09/26 21:04:35  florian
    + partial overflow checking on sparc; multiplication still missing

  Revision 1.90  2004/09/26 17:36:12  florian
    + a_jmp_name for sparc added

  Revision 1.89  2004/09/25 14:23:55  peter
    * ungetregister is now only used for cpuregisters, renamed to
      ungetcpuregister
    * renamed (get|unget)explicitregister(s) to ..cpuregister
    * removed location-release/reference_release

  Revision 1.88  2004/09/21 20:33:00  peter
    * don't remove MOV reg1,reg1 it is needed for the RA

  Revision 1.87  2004/09/21 17:25:13  peter
    * paraloc branch merged

  Revision 1.86.4.5  2004/09/20 20:43:15  peter
    * implement reg_ref/ref_reg for 64bit to prevent loading the
      address symbol twice

  Revision 1.86.4.4  2004/09/17 17:19:26  peter
    * fixed 64 bit unaryminus for sparc
    * fixed 64 bit inlining
    * signness of not operation

  Revision 1.86.4.3  2004/09/12 21:31:03  peter
    * sign extension added

  Revision 1.86.4.2  2004/09/12 13:36:40  peter
    * fixed alignment issues

  Revision 1.86.4.1  2004/08/31 20:43:06  peter
    * paraloc patch

  Revision 1.86  2004/08/25 20:40:04  florian
    * fixed absolute on sparc

  Revision 1.85  2004/08/24 21:02:32  florian
    * fixed longbool(<int64>) on sparc

  Revision 1.84  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.83  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.82.2.9  2004/06/02 19:05:16  peter
    * use a_load_const_reg to load const

  Revision 1.82.2.8  2004/06/02 16:07:40  peter
    * implement op64_reg_reg_reg

  Revision 1.82.2.7  2004/05/31 22:07:54  peter
    * don't use float in concatcopy

  Revision 1.82.2.6  2004/05/30 17:54:14  florian
    + implemented cmp64bit
    * started to fix spilling
    * fixed int64 sub partially

}
