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
       node,symconst,SymType;

    type
      TCgSparc=class(tcg)
      protected
        function IsSimpleRef(const ref:treference):boolean;
     public
        procedure init_register_allocators;override;
        procedure done_register_allocators;override;
        { sparc special, needed by cg64 }
        procedure handle_load_store(list:taasmoutput;isstore:boolean;op: tasmop;reg:tregister;ref: treference);
        procedure handle_reg_const_reg(list:taasmoutput;op:Tasmop;src:tregister;a:aword;dst:tregister);
        { parameter }
        procedure a_param_const(list:TAasmOutput;size:tcgsize;a:aword;const LocPara:TParaLocation);override;
        procedure a_param_ref(list:TAasmOutput;sz:tcgsize;const r:TReference;const LocPara:TParaLocation);override;
        procedure a_paramaddr_ref(list:TAasmOutput;const r:TReference;const LocPara:TParaLocation);override;
        procedure a_paramfpu_reg(list : taasmoutput;size : tcgsize;const r : tregister;const locpara : tparalocation);override;
        procedure a_paramfpu_ref(list : taasmoutput;size : tcgsize;const ref : treference;const locpara : tparalocation);override;
        procedure a_load_param_ref(list : taasmoutput;const locpara : tparalocation;const ref:treference);override;
        procedure a_load_param_reg(list : taasmoutput;const locpara : tparalocation;const reg:tregister);override;
        procedure a_call_name(list:TAasmOutput;const s:string);override;
        procedure a_call_reg(list:TAasmOutput;Reg:TRegister);override;
        { General purpose instructions }
        procedure a_op_const_reg(list:TAasmOutput;Op:TOpCG;size:tcgsize;a:AWord;reg:TRegister);override;
        procedure a_op_reg_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;src, dst:TRegister);override;
        procedure a_op_const_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;a:aword;src, dst:tregister);override;
        procedure a_op_reg_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;src1, src2, dst:tregister);override;
        { move instructions }
        procedure a_load_const_reg(list:TAasmOutput;size:tcgsize;a:aword;reg:tregister);override;
        procedure a_load_const_ref(list:TAasmOutput;size:tcgsize;a:aword;const ref:TReference);override;
        procedure a_load_reg_ref(list:TAasmOutput;FromSize,ToSize:TCgSize;reg:TRegister;const ref:TReference);override;
        procedure a_load_ref_reg(list:TAasmOutput;FromSize,ToSize:TCgSize;const ref:TReference;reg:tregister);override;
        procedure a_load_reg_reg(list:TAasmOutput;FromSize,ToSize:TCgSize;reg1,reg2:tregister);override;
        procedure a_loadaddr_ref_reg(list:TAasmOutput;const ref:TReference;r:tregister);override;
        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list:TAasmOutput;size:tcgsize;reg1, reg2:tregister);override;
        procedure a_loadfpu_ref_reg(list:TAasmOutput;size:tcgsize;const ref:TReference;reg:tregister);override;
        procedure a_loadfpu_reg_ref(list:TAasmOutput;size:tcgsize;reg:tregister;const ref:TReference);override;
        { comparison operations }
        procedure a_cmp_const_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aword;reg:tregister;l:tasmlabel);override;
        procedure a_cmp_reg_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;reg1,reg2:tregister;l:tasmlabel);override;
        procedure a_jmp_always(List:TAasmOutput;l:TAsmLabel);override;
        procedure a_jmp_cond(list:TAasmOutput;cond:TOpCmp;l:tasmlabel);{ override;}
        procedure a_jmp_flags(list:TAasmOutput;const f:TResFlags;l:tasmlabel);override;
        procedure g_flags2reg(list:TAasmOutput;Size:TCgSize;const f:tresflags;reg:TRegister);override;
        procedure g_overflowCheck(List:TAasmOutput;const Loc:TLocation;def:TDef);override;
        procedure g_save_parent_framepointer_param(list:taasmoutput);override;
        procedure g_stackframe_entry(list:TAasmOutput;localsize:LongInt);override;
        procedure g_restore_all_registers(list:TAasmOutput;accused,acchiused:boolean);override;
        procedure g_restore_frame_pointer(list:TAasmOutput);override;
        procedure g_restore_standard_registers(list:taasmoutput;usedinproc:Tsuperregisterset);override;
        procedure g_return_from_proc(list:TAasmOutput;parasize:aword);override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_save_standard_registers(list : taasmoutput; usedinproc : Tsuperregisterset);override;
        procedure g_concatcopy(list:TAasmOutput;const source,dest:TReference;len:aword;delsource,loadref:boolean);override;
        class function reg_cgsize(const reg:tregister):tcgsize;override;
      end;

      TCg64Sparc=class(tcg64f32)
        procedure a_op64_reg_reg(list:TAasmOutput;op:TOpCG;regsrc,regdst:TRegister64);override;
        procedure a_op64_const_reg(list:TAasmOutput;op:TOpCG;value:qWord;regdst:TRegister64);override;
        procedure get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      end;

    const
      TOpCG2AsmOp : array[topcg] of TAsmOp=(
        A_NONE,A_ADD,A_AND,A_UDIV,A_SDIV,A_UMUL,A_SMUL,A_NEG,A_NOT,A_OR,A_SRA,A_SLL,A_SRL,A_SUB,A_XOR
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
        if (ref.base=NR_NO) and (ref.index<>NR_NO) then
          InternalError(2002100804);
        result :=not(assigned(ref.symbol))and
                  (((ref.index = NR_NO) and
                   (ref.offset >= simm13lo) and
                    (ref.offset <= simm13hi)) or
                  ((ref.index <> NR_NO) and
                  (ref.offset = 0)));
      end;


    procedure tcgsparc.handle_load_store(list:taasmoutput;isstore:boolean;op: tasmop;reg:tregister;ref: treference);
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
            tmpreg:=rg.getregisterint(list,OS_INT);
            reference_reset(tmpref);
            tmpref.symbol:=ref.symbol;
            tmpref.offset:=ref.offset;
            tmpref.symaddr:=refs_hi;
            list.concat(taicpu.op_ref_reg(A_SETHI,tmpref,tmpreg));
            { Load the low part is left }
{$warning TODO Maybe not needed to load symbol}
            tmpref.symaddr:=refs_lo;
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
                  tmpreg:=rg.getregisterint(list,OS_INT);
                if (ref.index<>NR_NO) then
                  begin
                    list.concat(taicpu.op_reg_reg_reg(A_ADD,ref.base,ref.index,tmpreg));
                    ref.index:=NR_NO;
                  end;
              end;
          end;
        if isstore then
          list.concat(taicpu.op_reg_ref(op,reg,ref))
        else
          list.concat(taicpu.op_ref_reg(op,ref,reg));
        if (tmpreg<>NR_NO) then
          rg.ungetregisterint(list,tmpreg);
      end;


    procedure tcgsparc.handle_reg_const_reg(list:taasmoutput;op:Tasmop;src:tregister;a:aword;dst:tregister);
      var
        tmpreg : tregister;
      begin
        if (longint(a)<simm13lo) or
           (longint(a)>simm13hi) then
          begin
            tmpreg:=rg.getregisterint(list,OS_INT);
            list.concat(taicpu.op_const_reg(A_SETHI,a shr 10,tmpreg));
            list.concat(taicpu.op_reg_const_reg(A_OR,tmpreg,a and aword($3ff),tmpreg));
            list.concat(taicpu.op_reg_reg_reg(op,src,tmpreg,dst));
            rg.ungetregisterint(list,tmpreg);
          end
        else
          list.concat(taicpu.op_reg_const_reg(op,src,a,dst));
      end;


{****************************************************************************
                              Assembler code
****************************************************************************}

    procedure Tcgsparc.init_register_allocators;
      begin
        rg:=Trgcpu.create(15,chr(RS_O0)+chr(RS_O1)+chr(RS_O2)+chr(RS_O3)+
                             chr(RS_O4)+chr(RS_O5)+chr(RS_O7)+
                             chr(RS_L0)+chr(RS_L1)+chr(RS_L2)+chr(RS_L3)+
                             chr(RS_L4)+chr(RS_L5)+chr(RS_L6)+chr(RS_L7));
      end;


    procedure Tcgsparc.done_register_allocators;
      begin
        rg.free;
      end;


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
              { Code conventions need the parameters being allocated in %o6+92 }
              if locpara.reference.offset<92 then
                InternalError(2002081104);
              reference_reset_base(ref,locpara.reference.index,locpara.reference.offset);
              a_load_const_ref(list,size,a,ref);
            end;
          else
            InternalError(2002122200);
        end;
      end;


    procedure TCgSparc.a_param_ref(list:TAasmOutput;sz:TCgSize;const r:TReference;const LocPara:TParaLocation);
      var
        ref: treference;
        tmpreg:TRegister;
      begin
        with LocPara do
          case loc of
            LOC_REGISTER,LOC_CREGISTER :
              a_load_ref_reg(list,sz,sz,r,Register);
            LOC_REFERENCE:
              begin
                { Code conventions need the parameters being allocated in %o6+92 }
                if locpara.reference.offset<92 then
                  InternalError(2002081104);
                reference_reset_base(ref,locpara.reference.index,locpara.reference.offset);
                tmpreg:=rg.getregisterint(list,OS_INT);
                a_load_ref_reg(list,sz,sz,r,tmpreg);
                a_load_reg_ref(list,sz,sz,tmpreg,ref);
                rg.ungetregisterint(list,tmpreg);
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
              tmpreg:=rg.getaddressregister(list);
              a_loadaddr_ref_reg(list,r,tmpreg);
              a_load_reg_ref(list,OS_ADDR,OS_ADDR,tmpreg,ref);
              rg.ungetregisterint(list,tmpreg);
            end;
          else
            internalerror(2002080701);
        end;
      end;


    procedure tcgsparc.a_paramfpu_reg(list : taasmoutput;size : tcgsize;const r : tregister;const locpara : tparalocation);
      var
        href : treference;
      begin
        tg.GetTemp(list,TCGSize2Size[size],tt_normal,href);
        a_loadfpu_reg_ref(list,size,r,href);
        a_paramfpu_ref(list,size,href,locpara);
        tg.Ungettemp(list,href);
      end;


    procedure tcgsparc.a_paramfpu_ref(list : taasmoutput;size : tcgsize;const ref : treference;const locpara : tparalocation);
      var
        templocpara : tparalocation;
      begin
        { floats are pushed in the int registers }
        templocpara:=locpara;
        case locpara.size of
          OS_F32 :
            begin
              templocpara.size:=OS_32;
              a_param_ref(list,OS_32,ref,templocpara);
            end;
          OS_F64 :
            begin
              templocpara.size:=OS_64;
              cg64.a_param64_ref(list,ref,templocpara);
            end;
          else
            internalerror(200307021);
        end;
      end;


    procedure tcgsparc.a_load_param_ref(list : taasmoutput;const locpara : tparalocation;const ref:treference);
      var
        href,
        tempref : treference;
        templocpara : tparalocation;
      begin
        { Load floats like ints }
        templocpara:=locpara;
        case locpara.size of
          OS_F32 :
            templocpara.size:=OS_32;
          OS_F64 :
            templocpara.size:=OS_64;
        end;
        { Word 0 is in register, word 1 is in reference }
        if (templocpara.loc=LOC_REFERENCE) and (templocpara.low_in_reg) then
          begin
            tempref:=ref;
            cg.a_load_reg_ref(list,OS_INT,OS_INT,templocpara.register,tempref);
            inc(tempref.offset,4);
            reference_reset_base(href,templocpara.reference.index,templocpara.reference.offset);
            cg.a_load_ref_ref(list,OS_INT,OS_INT,href,tempref);
          end
        else
          inherited a_load_param_ref(list,templocpara,ref);
      end;


    procedure tcgsparc.a_load_param_reg(list : taasmoutput;const locpara : tparalocation;const reg:tregister);
      var
        href : treference;
      begin
        { Word 0 is in register, word 1 is in reference, not
          possible to load it in 1 register }
        if (locpara.loc=LOC_REFERENCE) and (locpara.low_in_reg) then
          internalerror(200307011);
        { Float load use a temp reference }
        if locpara.size in [OS_F32,OS_F64] then
          begin
            tg.GetTemp(list,TCGSize2Size[locpara.size],tt_normal,href);
            a_load_param_ref(list,locpara,href);
            a_loadfpu_ref_reg(list,locpara.size,href,reg);
            tg.Ungettemp(list,href);
          end
        else
          inherited a_load_param_reg(list,locpara,reg);
      end;


    procedure TCgSparc.a_call_name(list:TAasmOutput;const s:string);
      begin
        list.concat(taicpu.op_sym(A_CALL,objectlibrary.newasmsymbol(s)));
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

    procedure TCgSparc.a_load_const_reg(list : TAasmOutput;size : TCGSize;a : aword;reg : TRegister);
      begin
        { we don't use the set instruction here because it could be evalutated to two
          instructions which would cause problems with the delay slot (FK) }
        { sethi allows to set the upper 22 bit, so we'll take full advantage of it }
        if (a and aword($1fff))=0 then
          list.concat(taicpu.op_const_reg(A_SETHI,a shr 10,reg))
        else if (longint(a)>=simm13lo) and (longint(a)<=simm13hi) then
          list.concat(taicpu.op_reg_const_reg(A_OR,NR_G0,a,reg))
        else
          begin
            list.concat(taicpu.op_const_reg(A_SETHI,a shr 10,reg));
            list.concat(taicpu.op_reg_const_reg(A_OR,reg,a and aword($3ff),reg));
          end;
      end;


    procedure TCgSparc.a_load_const_ref(list : TAasmOutput;size : tcgsize;a : aword;const ref : TReference);
      begin
        if a=0 then
          a_load_reg_ref(list,size,size,NR_G0,ref)
        else
          inherited a_load_const_ref(list,size,a,ref);
      end;


    procedure TCgSparc.a_load_reg_ref(list:TAasmOutput;FromSize,ToSize:TCGSize;reg:tregister;const Ref:TReference);
      var
        op:tasmop;
      begin
        case ToSize of
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
        op:tasmop;
      begin
        case Fromsize of
          { signed integer registers }
          OS_S8:
            Op:=A_LDSB;{Load Signed Byte}
          OS_8:
            Op:=A_LDUB;{Load Unsigned Bye}
          OS_S16:
            Op:=A_LDSH;{Load Signed Halfword}
          OS_16:
            Op:=A_LDUH;{Load Unsigned Halfword}
          OS_S32,
          OS_32:
            Op:=A_LD;{Load Word}
          else
            InternalError(2002122101);
        end;
        handle_load_store(list,false,op,reg,ref);
      end;


    procedure TCgSparc.a_load_reg_reg(list:TAasmOutput;fromsize,tosize:tcgsize;reg1,reg2:tregister);
      begin
        if (reg1<>reg2) or
           (tcgsize2size[tosize]<tcgsize2size[fromsize]) or
           (
            (tcgsize2size[tosize] = tcgsize2size[fromsize]) and
            (tosize <> fromsize) and
            not(fromsize in [OS_32,OS_S32])
           ) then
          begin
{$warning TODO Sign extension}
            case tosize of
              OS_8,OS_S8:
                a_op_const_reg_reg(list,OP_AND,tosize,$ff,reg1,reg2);
              OS_16,OS_S16:
                a_op_const_reg_reg(list,OP_AND,tosize,$ffff,reg1,reg2);
              OS_32,OS_S32:
                begin
                  if reg1<>reg2 then
                    list.Concat(taicpu.op_reg_reg(A_MOV,reg1,reg2));
                end;
              else
                internalerror(2002090901);
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
            if (ref.base<>r) and (ref.index<>r) then
              hreg:=r
            else
              hreg:=rg.getaddressregister(list);
            reference_reset(tmpref);
            tmpref.symbol := ref.symbol;
            tmpref.offset := ref.offset;
            tmpref.symaddr := refs_hi;
            list.concat(taicpu.op_ref_reg(A_SETHI,tmpref,hreg));
            { Only the low part is left }
            tmpref.symaddr:=refs_lo;
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
                  list.Concat(taicpu.op_reg_reg(A_MOV,hreg,r));
              end;
            if hreg<>r then
              rg.ungetaddressregister(list,hreg);
          end
        else
        { At least small offset, maybe base and maybe index }
          if ref.offset<>0 then
            begin
              if ref.base<>NR_NO then
                begin
                  if ref.index<>NR_NO then
                    begin
                      if (ref.base<>r) and (ref.index<>r) then
                        hreg:=r
                      else
                        hreg:=rg.getaddressregister(list);
                      list.concat(taicpu.op_reg_const_reg(A_ADD,ref.base,aword(ref.offset),hreg));
                      list.concat(taicpu.op_reg_reg_reg(A_ADD,hreg,ref.index,r));
                      if hreg<>r then
                        rg.ungetaddressregister(list,hreg);
                    end
                  else
                    list.concat(taicpu.op_reg_const_reg(A_ADD,ref.base,aword(ref.offset),r));
                end
              else
                list.concat(taicpu.op_reg_const_reg(A_ADD,NR_G0,aword(ref.offset),r));
            end
        else
        { Both base and index }
          if ref.index<>NR_NO then
            list.concat(taicpu.op_reg_reg_reg(A_ADD,ref.base,ref.index,r))
        else
        { Only base }
          if ref.base<>NR_NO then
            a_load_reg_reg(list,OS_INT,OS_INT,ref.base,r)
        else
          internalerror(200306172);
      end;


    procedure TCgSparc.a_loadfpu_reg_reg(list:TAasmOutput;size:tcgsize;reg1, reg2:tregister);
      begin
        if reg1<>reg2 then
          begin
            list.concat(taicpu.op_reg_reg(A_FMOVs,reg1,reg2));
            if size=OS_F64 then
              begin
                setsupreg(reg1,getsupreg(reg1)+1);
                setsupreg(reg2,getsupreg(reg2)+1);
                list.concat(taicpu.op_reg_reg(A_FMOVs,reg1,reg2));
              end;
          end;
      end;


    procedure TCgSparc.a_loadfpu_ref_reg(list:TAasmOutput;size:tcgsize;const ref:TReference;reg:tregister);
       const
         FpuLoadInstr : Array[OS_F32..OS_F64] of TAsmOp =
           (A_LDF,A_LDDF);
       begin
          { several functions call this procedure with OS_32 or OS_64 }
          { so this makes life easier (FK)                            }
          case size of
             OS_32,OS_F32:
               size:=OS_F32;
             OS_64,OS_F64,OS_C64:
               size:=OS_F64;
             else
               internalerror(200201121);
          end;
         handle_load_store(list,false,fpuloadinstr[size],reg,ref);
       end;


     procedure TCgSparc.a_loadfpu_reg_ref(list:TAasmOutput;size:tcgsize;reg:tregister;const ref:TReference);
       const
         FpuLoadInstr : Array[OS_F32..OS_F64] of TAsmOp =
           (A_STF,A_STDF);
       begin
          { several functions call this procedure with OS_32 or OS_64 }
          { so this makes life easier (FK)                            }
          case size of
             OS_32,OS_F32:
               size:=OS_F32;
             OS_64,OS_F64,OS_C64:
               size:=OS_F64;
             else
               internalerror(200201121);
          end;
         handle_load_store(list,true,fpuloadinstr[size],reg,ref);
       end;


    procedure TCgSparc.a_op_const_reg(list:TAasmOutput;Op:TOpCG;size:tcgsize;a:AWord;reg:TRegister);
      begin
        if Op in [OP_NEG,OP_NOT] then
          internalerror(200306011);
        if (a=0) then
          list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp[op],reg,NR_G0,reg))
        else
          handle_reg_const_reg(list,TOpCG2AsmOp[op],reg,a,reg);
      end;


    procedure TCgSparc.a_op_reg_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;src, dst:TRegister);
      begin
        Case Op of
          OP_NEG,
          OP_NOT:
            list.concat(taicpu.op_reg_reg(TOpCG2AsmOp[op],src,dst));
          else
            list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp[op],dst,src,dst));
        end;
      end;


    procedure TCgSparc.a_op_const_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;a:aword;src, dst:tregister);
      var
        power : longInt;
      begin
        case op of
          OP_IMUL :
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(a,power) then
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


  {*************** compare instructructions ****************}

    procedure TCgSparc.a_cmp_const_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aword;reg:tregister;l:tasmlabel);
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
        List.Concat(TAiCpu.op_sym(A_BA,objectlibrary.newasmsymbol(l.name)));
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
        ai:taicpu;
      begin
        ai := Taicpu.op_sym(A_Bxx,l);
        ai.SetCondition(flags_to_cond(f));
        list.Concat(ai);
        { Delay slot }
        list.Concat(TAiCpu.Op_none(A_NOP));
      end;


    procedure TCgSparc.g_flags2reg(list:TAasmOutput;Size:TCgSize;const f:tresflags;reg:TRegister);
      var
        ai : taicpu;
      begin
        ai:=Taicpu.Op_reg_reg(A_RDPSR,NR_PSR,reg);
{$warning Need to retrieve the correct flag setting in reg}
//        ai.SetCondition(flags_to_cond(f));
        list.Concat(ai);
        { Delay slot }
        list.Concat(TAiCpu.Op_none(A_NOP));
      end;


    procedure TCgSparc.g_overflowCheck(List:TAasmOutput;const Loc:TLocation;def:TDef);
      var
        hl : tasmlabel;
      begin
        if not(cs_check_overflow in aktlocalswitches) then
          exit;
        objectlibrary.getlabel(hl);
        if not((def.deftype=pointerdef)or
              ((def.deftype=orddef)and
               (torddef(def).typ in [u64bit,u16bit,u32bit,u8bit,uchar,bool8bit,bool16bit,bool32bit]))) then
          begin
            //r.enum:=R_CR7;
            //list.concat(taicpu.op_reg(A_MCRXR,r));
            //a_jmp_cond(list,A_Bxx,C_OV,hl)
            a_jmp_always(list,hl)
          end
        else
          a_jmp_cond(list,OC_AE,hl);
        a_call_name(list,'FPC_OVERFLOW');
        a_label(list,hl);
      end;

  { *********** entry/exit code and address loading ************ }

    procedure tcgsparc.g_save_parent_framepointer_param(list:taasmoutput);
      var
        href : treference;
      begin
        reference_reset_base(href,current_procinfo.framepointer,PARENT_FRAMEPOINTER_OFFSET);
        { Parent framepointer is always pushed the first parameter (%i0) }
        a_load_reg_ref(list,OS_ADDR,OS_ADDR,NR_I0,href);
      end;


    procedure TCgSparc.g_stackframe_entry(list:TAasmOutput;LocalSize:LongInt);
      begin
        { Althogh the SPARC architecture require only word alignment, software
          convention and the operating system require every stack frame to be double word
          aligned }
        LocalSize:=align(LocalSize,8);
        { Execute the SAVE instruction to get a new register window and create a new
          stack frame. In the "SAVE %i6,size,%i6" the first %i6 is related to the state
          before execution of the SAVE instrucion so it is the caller %i6, when the %i6
          after execution of that instruction is the called function stack pointer}
        list.concat(Taicpu.Op_reg_const_reg(A_SAVE,NR_STACK_POINTER_REG,aword(-LocalSize),NR_STACK_POINTER_REG));
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


    procedure TCgSparc.g_restore_standard_registers(list:taasmoutput;usedinproc:Tsuperregisterset);
      begin
        { The sparc port uses the sparc standard calling convetions so this function has no used }
      end;


    procedure TCgSparc.g_return_from_proc(list:TAasmOutput;parasize:aword);
      begin
        { According to the SPARC ABI, the stack is cleared using the RESTORE instruction
          which is genereted in the g_restore_frame_pointer. Notice that SPARC has no
          real RETURN instruction and that JMPL is used instead. The JMPL instrucion have one
          delay slot, so an inversion is possible such as
          RET      (=JMPL  %i7+8,%g0)
          RESTORE  (=RESTORE %g0,0,%g0)
          If no inversion we can use just
          RESTORE  (=RESTORE %g0,0,%g0)
          RET      (=JMPL  %i7+8,%g0)
          NOP
        }
        list.concat(Taicpu.op_none(A_RET));
        { We use trivial restore in the delay slot of the JMPL instruction, as we
          already set result onto %i0 }
        list.concat(Taicpu.op_none(A_RESTORE));
      end;


    procedure TCgSparc.g_save_all_registers(list : taasmoutput);
      begin
        { The sparc port uses the sparc standard calling convetions so this function has no used }
      end;


    procedure TCgSparc.g_save_standard_registers(list : taasmoutput; usedinproc:Tsuperregisterset);
      begin
        { The sparc port uses the sparc standard calling convetions so this function has no used }
      end;


    { ************* concatcopy ************ }

    procedure TCgSparc.g_concatcopy(list:taasmoutput;const source,dest:treference;len:aword;delsource,loadref:boolean);
      var
        hreg,
        countreg: TRegister;
        src, dst: TReference;
        lab: tasmlabel;
        count, count2: aword;
        orgsrc, orgdst: boolean;
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
                  a_load_ref_ref(list,int_cgsize(len),int_cgsize(len),source,dest);
                  if delsource then
                    reference_release(list,source);
                end
              else
                begin
                  a_reg_alloc(list,NR_F0);
                  a_loadfpu_ref_reg(list,OS_F64,source,NR_F0);
                  if delsource then
                    reference_release(list,source);
                  a_loadfpu_reg_ref(list,OS_F64,NR_F0,dest);
                  a_reg_dealloc(list,NR_F0);
                end;
              exit;
            end;
          end;
        reference_reset(src);
        reference_reset(dst);
        { load the address of source into src.base }
        if loadref then
          begin
            src.base:=rg.getaddressregister(list);
            a_load_ref_reg(list,OS_32,OS_32,source,src.base);
            orgsrc := false;
          end
        else
         if not issimpleref(source) or
            (
              (source.index<>NR_NO) and
              (((source.offset+longint(len))>simm13hi) or
               ((source.offset+longint(len))<simm13lo))
            ) then
           begin
             src.base:=rg.getaddressregister(list);
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
            (dest.index<>NR_NO) and
            (((dest.offset + longint(len)) > simm13hi) or
             ((dest.offset + longint(len)) < simm13lo))
           ) then
          begin
            dst.base:=rg.getaddressregister(list);
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
            countreg:=rg.getregisterint(list,OS_INT);
            a_load_const_reg(list,OS_INT,count,countreg);
            { explicitely allocate R_O0 since it can be used safely here }
            { (for holding date that's being copied)                    }
            a_reg_alloc(list,NR_F0);
            objectlibrary.getlabel(lab);
            a_label(list, lab);
            list.concat(taicpu.op_reg_const_reg(A_SUB,countreg,1,countreg));
            list.concat(taicpu.op_ref_reg(A_LDF,src,NR_F0));
            list.concat(taicpu.op_reg_ref(A_STD,NR_F0,dst));
            //a_jmp(list,A_BC,C_NE,0,lab);
            rg.ungetregisterint(list,countreg);
            a_reg_dealloc(list,NR_F0);
            len := len mod 8;
          end;
        { unrolled loop }
        count:=len and 7;
        if count>0 then
          begin
            a_reg_alloc(list,NR_F0);
            for count2 := 1 to count do
              begin
                a_loadfpu_ref_reg(list,OS_F64,src,NR_F0);
                a_loadfpu_reg_ref(list,OS_F64,NR_F0,dst);
                inc(src.offset,8);
                inc(dst.offset,8);
              end;
            a_reg_dealloc(list,NR_F0);
            len := len mod 8;
          end;
        if (len and 4) <> 0 then
          begin
            hreg:=rg.getregisterint(list,OS_INT);
            a_load_ref_reg(list,OS_32,OS_32,src,hreg);
            a_load_reg_ref(list,OS_32,OS_32,hreg,dst);
            inc(src.offset,4);
            inc(dst.offset,4);
            rg.ungetregisterint(list,hreg);
          end;
        { copy the leftovers }
        if (len and 2) <> 0 then
          begin
            hreg:=rg.getregisterint(list,OS_INT);
            a_load_ref_reg(list,OS_16,OS_16,src,hreg);
            a_load_reg_ref(list,OS_16,OS_16,hreg,dst);
            inc(src.offset,2);
            inc(dst.offset,2);
            rg.ungetregisterint(list,hreg);
          end;
        if (len and 1) <> 0 then
          begin
            hreg:=rg.getregisterint(list,OS_INT);
            a_load_ref_reg(list,OS_8,OS_8,src,hreg);
            a_load_reg_ref(list,OS_8,OS_8,hreg,dst);
            rg.ungetregisterint(list,hreg);
          end;
        if orgsrc then
          begin
            if delsource then
              reference_release(list,source);
          end
        else
          rg.ungetregisterint(list,src.base);
        if not orgdst then
          rg.ungetregisterint(list,dst.base);
      end;

{****************************************************************************
                               TCG64Sparc
****************************************************************************}

    procedure TCg64Sparc.get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      begin
        case op of
          OP_ADD :
            begin
              op1:=A_ADD;
              op2:=A_ADDX;
            end;
          OP_SUB :
            begin
              op1:=A_SUB;
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
          OP_NOT :
            begin
              op1:=A_NOT;
              op2:=A_NOT;
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
              list.concat(taicpu.op_reg_reg_reg(A_XNOR,NR_G0,regsrc.reghi,regdst.reghi));
              list.concat(taicpu.op_reg_reg_reg(A_SUBcc,NR_G0,regsrc.reglo,regdst.reglo));
              list.concat(taicpu.op_reg_const_reg(A_ADDX,regdst.reglo,aword(-1),regdst.reglo));
              exit;
            end;
        end;
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_reg_reg_reg(op1,regdst.reglo,regsrc.reglo,regdst.reglo));
        list.concat(taicpu.op_reg_reg_reg(op2,regdst.reghi,regsrc.reghi,regdst.reghi));
      end;


    procedure TCg64Sparc.a_op64_const_reg(list:TAasmOutput;op:TOpCG;value:qWord;regdst:TRegister64);
      var
        op1,op2:TAsmOp;
      begin
        case op of
          OP_NEG,
          OP_NOT :
            internalerror(200306017);
        end;
        get_64bit_ops(op,op1,op2);
        tcgsparc(cg).handle_reg_const_reg(list,op1,regdst.reglo,lo(value),regdst.reglo);
        tcgsparc(cg).handle_reg_const_reg(list,op1,regdst.reghi,hi(value),regdst.reghi);
      end;

begin
  cg:=TCgSparc.Create;
  cg64:=TCg64Sparc.Create;
end.
{
  $Log$
  Revision 1.67  2003-09-14 19:19:04  peter
    * updates for new ra

  Revision 1.66  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.65.2.1  2003/09/01 21:02:55  peter
    * sparc updates for new tregister

  Revision 1.65  2003/07/08 21:24:59  peter
    * sparc fixes

  Revision 1.64  2003/07/06 22:10:13  peter
    * operand order of cmp fixed

  Revision 1.63  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.62  2003/07/03 21:09:53  peter
    * delay slot NOPs and comments added
    * a_loadaddr_ref_reg fixed and optimized to reuse passed register
      if it is not used by the ref

  Revision 1.61  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.60  2003/06/17 16:35:56  peter
    * a_loadaddr_ref_reg fixed

  Revision 1.59  2003/06/13 21:19:32  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.58  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.57  2003/06/04 20:59:37  mazen
  + added size of destination in code gen methods
  + making g_overflowcheck declaration same as
    ancestor's method declaration

  Revision 1.56  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.55  2003/06/01 01:04:35  peter
    * reference fixes

  Revision 1.54  2003/05/31 01:00:51  peter
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
    * aktprocdef renamed to current_procinfo.procdef
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
  * register allocation for parameters now done in cpupara

  Revision 1.9  2002/10/02 22:20:28  mazen
  + out registers allocator for the first 6 scalar parameters which must be passed into %o0..%o5

  Revision 1.8  2002/10/01 21:35:58  mazen
  + procedures exiting prologue added and stack frame now restored in the delay slot of the return (JMPL) instruction

  Revision 1.7  2002/10/01 21:06:29  mazen
  attinst.inc --> strinst.inc

  Revision 1.6  2002/10/01 17:41:50  florian
    * fixed log and id
}
