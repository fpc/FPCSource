{

    Copyright (c) 2003 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generator for the avr32

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
       globtype,symtype,symdef,
       cgbase,cgutils,cgobj,
       aasmbase,aasmcpu,aasmtai,aasmdata,
       parabase,
       cpubase,cpuinfo,node,cg64f32,rgcpu,sysutils;


    type

      { tcgavr32 }

      tcgavr32 = class(tcg)
        procedure init_register_allocators;override;
        procedure done_register_allocators;override;

        {function getfpuregister(list: TAsmList; size: Tcgsize): Tregister; override;
        function getmmregister(list: TAsmList; size: Tcgsize): Tregister; override;}

        procedure a_load_const_cgpara(list : TAsmList;size : tcgsize;a : tcgint;const paraloc : TCGPara);override;
        procedure a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);override;
        procedure a_load_reg_cgpara(list: TAsmList; size: tcgsize; r: tregister; const cgpara: TCGPara); override;
        procedure a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);override;

        procedure a_call_name(list : TAsmList;const s : string; weak: boolean);override;
        procedure a_call_reg(list : TAsmList;reg: tregister);override;
        procedure a_call_ref(list : TAsmList;ref: treference);override;

        procedure a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister); override;
        procedure a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;

        procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg;size: tcgsize; a: tcgint; src, dst: tregister); override;
        procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg;size: tcgsize; src1, src2, dst: tregister); override;
        procedure a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister;setflags : boolean;var ovloc : tlocation);override;
        procedure a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation);override;

        procedure a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; size: tcgsize; src, dst: TRegister); override;

        { move instructions }
        procedure a_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);override;
        procedure a_load_reg_reg(list : TAsmList; fromsize, tosize : tcgsize;reg1,reg2 : tregister);override;
        procedure a_load_const_reg(list : TAsmList; size: tcgsize; a : tcgint;reg : tregister);override;
        procedure a_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister);override;
        function a_internal_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference):treference;
        function a_internal_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister):treference;

        {  comparison operations }
        procedure a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : tcgint;reg : tregister;l : tasmlabel);override;
        procedure a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;

        procedure a_jmp_name(list : TAsmList;const s : string); override;
        procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;
        procedure a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel); override;

        procedure g_flags2reg(list: TAsmList; size: TCgSize; const f: TResFlags; reg: TRegister); override;

        procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);override;
        procedure g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean); override;

        procedure a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);override;

        procedure g_concatcopy(list : TAsmList;const source,dest : treference;len : tcgint);override;
        procedure g_concatcopy_unaligned(list : TAsmList;const source,dest : treference;len : tcgint);override;
        procedure g_concatcopy_move(list : TAsmList;const source,dest : treference;len : tcgint);
        procedure g_concatcopy_internal(list : TAsmList;const source,dest : treference;len : tcgint;aligned : boolean);

        procedure g_overflowcheck(list: TAsmList; const l: tlocation; def: tdef); override;
        procedure g_overflowCheck_loc(List:TAsmList;const Loc:TLocation;def:TDef;ovloc : tlocation);override;

        procedure g_save_registers(list : TAsmList);override;
        procedure g_restore_registers(list : TAsmList);override;

        procedure a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
        procedure fixref(list : TAsmList;var ref : treference);
        function handle_load_store(list:TAsmList;op: tasmop;oppostfix : toppostfix;reg:tregister;ref: treference):treference; virtual;

        procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
        procedure g_adjust_self_value(list:TAsmList;procdef: tprocdef;ioffset: tcgint); override;
        procedure g_stackpointer_alloc(list : TAsmList;size : longint);override;
      private
        { clear out potential overflow bits from 8 or 16 bit operations  }
        { the upper 24/16 bits of a register after an operation          }
        procedure maybeadjustresult(list: TAsmList; op: TOpCg; size: tcgsize; dst: tregister);
      end;

      tcg64favr32 = class(tcg64f32)
        procedure a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);override;
        procedure a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);override;
        procedure a_op64_const_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64);override;
        procedure a_op64_reg_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64);override;
        procedure a_op64_const_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64;setflags : boolean;var ovloc : tlocation);override;
        procedure a_op64_reg_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64;setflags : boolean;var ovloc : tlocation);override;
      end;

    const
      OpCmp2AsmCond : Array[topcmp] of TAsmCond = (C_NONE,C_EQ,C_GT,
                           C_LT,C_GE,C_LE,C_NE,C_LS,C_CC,C_CS,C_HI);

      winstackpagesize = 4096;

    procedure create_codegen;

  implementation


    uses
       globals,verbose,systems,cutils,
       aopt,aoptcpu,
       fmodule,
       symconst,symsym,
       tgobj,
       procinfo,cpupi,
       paramgr;


    function size2postfix(size: TCgSize): TOpPostfix;
      begin
        case size of
          OS_S8:
            result:=PF_SB;
          OS_8:
            result:=PF_UB;
          OS_S16:
            result:=PF_SH;
          OS_16:
            result:=PF_UH;
          OS_S32,
          OS_32:
            result:=PF_W;
        else
          result:=PF_NONE;
        end;
      end;


    function unsignedsize2postfix(size: TCgSize): TOpPostfix;
      begin
        case size of
          OS_S8,
          OS_8:
            result:=PF_B;
          OS_S16,
          OS_16:
            result:=PF_H;
          OS_S32,
          OS_32:
            result:=PF_W;
        else
          result:=PF_NONE;
        end;
      end;


    function use_push(const cgpara:tcgpara):boolean;
      begin
        result:=(not paramanager.use_fixed_stack) and
                assigned(cgpara.location) and
                (cgpara.location^.loc=LOC_REFERENCE) and
                (cgpara.location^.reference.index=NR_STACK_POINTER_REG);
      end;


    procedure tcgavr32.init_register_allocators;
      begin
        inherited init_register_allocators;
        { Reverse order to make it allocate the higher registers first.
          This makes it more likely to contract a pushm statement into a compact pushm }
        rg[R_INTREGISTER]:=trgintcpu.create(R_INTREGISTER,R_SUBWHOLE,
            [RS_R12,RS_R11,RS_R10,RS_R9,RS_R8,RS_R6,RS_R5,RS_R4,
             RS_R3,RS_R2,RS_R1,RS_R0],first_int_imreg,[]); 
      end;


    procedure tcgavr32.done_register_allocators;
      begin
        rg[R_INTREGISTER].free;
        inherited done_register_allocators;
      end;

    {function tcgavr32.getfpuregister(list: TAsmList; size: Tcgsize): Tregister;
      begin
        case size of
          OS_F32:
            size:=OS_S32;
          OS_F64:
            size:=OS_S64;
          OS_F80,
          OS_F128:
            size:=OS_128;
        end;

        Result:=getintregister(list, size);
      end;

    function tcgavr32.getmmregister(list: TAsmList; size: Tcgsize): Tregister;
      begin
        case size of
          OS_F32:
            size:=OS_S32;
          OS_F64:
            size:=OS_S64;
          OS_F80,
          OS_F128:
            size:=OS_128;
        end;

        Result:=getintregister(list, size);
      end;}


     procedure tcgavr32.a_load_const_reg(list : TAsmList; size: tcgsize; a : tcgint;reg : tregister);
       var
          imm_shift : byte;
          l : tasmlabel;
          hr : treference;
          i : longint;
       begin
          if not(size in [OS_8,OS_S8,OS_16,OS_S16,OS_32,OS_S32]) then
            internalerror(2002090902);
          if in_signed_bits(a,21) then
            list.concat(taicpu.op_reg_const(A_MOV,reg,a))
          else if ispowerof2(a, i) then
            begin
              list.concat(taicpu.op_reg_reg(A_EOR, reg, reg));
              list.concat(taicpu.op_reg_const(A_SBR, reg, i));
            end
          else
            begin
               reference_reset(hr,4);

               current_asmdata.getjumplabel(l);
               cg.a_label(current_procinfo.aktlocaldata,l);
               hr.symboldata:=current_procinfo.aktlocaldata.last;
               current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(longint(a)));

               hr.symbol:=l;
               list.concat(taicpu.op_reg_ref(A_LDDPC,reg,hr));
            end;
       end;


     procedure tcgavr32.a_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister);
       var
         oppostfix:toppostfix;
         usedtmpref: treference;
         tmpreg,tmpreg2 : tregister;
         so : tshifterop;
         dir : integer;
       begin
         if (TCGSize2Size[FromSize] >= TCGSize2Size[ToSize]) then
           FromSize := ToSize;
         oppostfix:=size2postfix(fromsize);
         if (ref.alignment in [1,2]) and (ref.alignment<tcgsize2size[fromsize]) then
           begin
             if target_info.endian=endian_big then
               dir:=-1
             else
               dir:=1;
             case FromSize of
               OS_16,OS_S16:
                 begin
                   { only complicated references need an extra loadaddr }
                   if assigned(ref.symbol) or
                     (ref.index<>NR_NO) or
                     (ref.offset<-4095) or
                     (ref.offset>4094) or
                     { sometimes the compiler reused registers }
                     (reg=ref.index) or
                     (reg=ref.base) then
                     begin
                       tmpreg2:=getintregister(list,OS_INT);
                       a_loadaddr_ref_reg(list,ref,tmpreg2);
                       reference_reset_base(usedtmpref,tmpreg2,0,ref.alignment);
                     end
                   else
                     usedtmpref:=ref;

                   if target_info.endian=endian_big then
                     inc(usedtmpref.offset,1);
                   shifterop_reset(so);so.shiftmode:=SM_LSL;so.shiftimm:=8;
                   tmpreg:=getintregister(list,OS_INT);
                   a_internal_load_ref_reg(list,OS_8,OS_8,usedtmpref,reg);
                   inc(usedtmpref.offset,dir);
                   if FromSize=OS_16 then
                     a_internal_load_ref_reg(list,OS_8,OS_8,usedtmpref,tmpreg)
                   else
                     a_internal_load_ref_reg(list,OS_S8,OS_S8,usedtmpref,tmpreg);
                   list.concat(taicpu.op_reg_reg_reg_shifterop(A_OR,reg,reg,tmpreg,so));
                 end;
               OS_32,OS_S32:
                 begin
                   tmpreg:=getintregister(list,OS_INT);

                   { only complicated references need an extra loadaddr }
                   if assigned(ref.symbol) or
                     (ref.index<>NR_NO) or
                     (ref.offset<-4095) or
                     (ref.offset>4092) or
                     { sometimes the compiler reused registers }
                     (reg=ref.index) or
                     (reg=ref.base) then
                     begin
                       tmpreg2:=getintregister(list,OS_INT);
                       a_loadaddr_ref_reg(list,ref,tmpreg2);
                       reference_reset_base(usedtmpref,tmpreg2,0,ref.alignment);
                     end
                   else
                     usedtmpref:=ref;

                   shifterop_reset(so);so.shiftmode:=SM_LSL;
                   if ref.alignment=2 then
                     begin
                       if target_info.endian=endian_big then
                         inc(usedtmpref.offset,2);
                       a_internal_load_ref_reg(list,OS_16,OS_16,usedtmpref,reg);
                       inc(usedtmpref.offset,dir*2);
                       a_internal_load_ref_reg(list,OS_16,OS_16,usedtmpref,tmpreg);
                       so.shiftimm:=16;
                       list.concat(taicpu.op_reg_reg_reg_shifterop(A_OR,reg,reg,tmpreg,so));
                     end
                   else
                     begin
                       if target_info.endian=endian_big then
                         inc(usedtmpref.offset,3);
                       a_internal_load_ref_reg(list,OS_8,OS_8,usedtmpref,reg);
                       inc(usedtmpref.offset,dir);
                       a_internal_load_ref_reg(list,OS_8,OS_8,usedtmpref,tmpreg);
                       so.shiftimm:=8;
                       list.concat(taicpu.op_reg_reg_reg_shifterop(A_OR,reg,reg,tmpreg,so));
                       inc(usedtmpref.offset,dir);
                       a_internal_load_ref_reg(list,OS_8,OS_8,usedtmpref,tmpreg);
                       so.shiftimm:=16;
                       list.concat(taicpu.op_reg_reg_reg_shifterop(A_OR,reg,reg,tmpreg,so));
                       inc(usedtmpref.offset,dir);
                       a_internal_load_ref_reg(list,OS_8,OS_8,usedtmpref,tmpreg);
                       so.shiftimm:=24;
                       list.concat(taicpu.op_reg_reg_reg_shifterop(A_OR,reg,reg,tmpreg,so));
                     end;
                 end
               else
                 handle_load_store(list,A_LD,oppostfix,reg,ref);
             end;
           end
         else
           handle_load_store(list,A_LD,oppostfix,reg,ref);

         if (fromsize=OS_S8) and (tosize = OS_16) then
           a_load_reg_reg(list,OS_16,OS_32,reg,reg);
       end;


    procedure tcgavr32.a_load_const_cgpara(list : TAsmList;size : tcgsize;a : tcgint;const paraloc : TCGPara);
      var
        ref: treference;
        treg: TRegister;
      begin
        paraloc.check_simple_location;
        paramanager.allocparaloc(list,paraloc.location);
        case paraloc.location^.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_load_const_reg(list,size,a,paraloc.location^.register);
          LOC_REFERENCE:
            begin
              treg:=getintregister(list,size);
              a_load_const_reg(list,size,a,treg);
              a_load_reg_cgpara(list,size,treg,paraloc);
            end;
          else
            internalerror(2002081101);
        end;
      end;


    procedure tcgavr32.a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);
      var
        tmpref, ref: treference;
        location: pcgparalocation;
        sizeleft: tcgint;
      begin
        location := paraloc.location;
        tmpref := r;
        sizeleft := paraloc.intsize;
        while assigned(location) do
          begin
            paramanager.allocparaloc(list,location);
            case location^.loc of
              LOC_REGISTER,LOC_CREGISTER:
                a_load_ref_reg(list,location^.size,location^.size,tmpref,location^.register);
              LOC_REFERENCE:
                begin
                  reference_reset_base(ref,location^.reference.index,location^.reference.offset,paraloc.alignment);
                  { doubles in softemu mode have a strange order of registers and references }
                  if location^.size=OS_32 then
                    g_concatcopy(list,tmpref,ref,4)
                  else
                    begin
                      g_concatcopy(list,tmpref,ref,sizeleft);
                      if assigned(location^.next) then
                        internalerror(2005010710);
                    end;
                end;
              LOC_FPUREGISTER,LOC_CFPUREGISTER:
                case location^.size of
                   OS_F32, OS_F64:
                     a_loadfpu_ref_reg(list,location^.size,location^.size,tmpref,location^.register);
                   else
                     internalerror(2002072801);
                end;
              LOC_VOID:
                begin
                  // nothing to do
                end;
              else
                internalerror(2002081103);
            end;
            inc(tmpref.offset,tcgsize2size[location^.size]);
            dec(sizeleft,tcgsize2size[location^.size]);
            location := location^.next;
          end;
      end;


    procedure tcgavr32.a_load_reg_cgpara(list: TAsmList; size: tcgsize; r: tregister; const cgpara: TCGPara);
      var
        pushsize : tcgsize;
        href: treference;
      begin
        if use_push(cgpara) then
          begin
            cgpara.check_simple_location;
            if tcgsize2size[cgpara.location^.size]>cgpara.alignment then
              pushsize:=cgpara.location^.size
            else
              pushsize:=int_cgsize(cgpara.alignment);

            reference_reset_base(href,cgpara.Location^.reference.index,0,cgpara.Alignment);
            href.addressmode:=AM_PREINDEXED;
            list.concat(setoppostfix(taicpu.op_ref_reg(A_ST,href,makeregsize(list,r,pushsize)),size2postfix(pushsize)));
          end
        else
          inherited a_load_reg_cgpara(list, size, r, cgpara);
      end;


    procedure tcgavr32.a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);
      var
        ref: treference;
        tmpreg: tregister;
      begin
        paraloc.check_simple_location;
        paramanager.allocparaloc(list,paraloc.location);
        case paraloc.location^.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_loadaddr_ref_reg(list,r,paraloc.location^.register);
          LOC_REFERENCE:
            begin
              reference_reset(ref,paraloc.alignment);
              ref.base := paraloc.location^.reference.index;
              ref.offset := paraloc.location^.reference.offset;
              tmpreg := getintregister(list,OS_ADDR);
              a_loadaddr_ref_reg(list,r,tmpreg);
              a_load_reg_ref(list,OS_ADDR,OS_ADDR,tmpreg,ref);
            end;
          else
            internalerror(2002080701);
        end;
      end;


    procedure tcgavr32.a_call_name(list : TAsmList;const s : string; weak: boolean);
      begin
        if not weak then
          list.concat(taicpu.op_sym(A_RCALL,current_asmdata.RefAsmSymbol(s)))
        else
          list.concat(taicpu.op_sym(A_RCALL,current_asmdata.WeakRefAsmSymbol(s)));
{
        the compiler does not properly set this flag anymore in pass 1, and
        for now we only need it after pass 2 (I hope) (JM)
          if not(pi_do_call in current_procinfo.flags) then
            internalerror(2003060703);
}
        include(current_procinfo.flags,pi_do_call);
      end;


    procedure tcgavr32.a_call_reg(list : TAsmList;reg: tregister);
      begin
        { check not really correct: should only be used for non-Thumb cpus }
        list.concat(taicpu.op_reg(A_ICALL, reg));
{
        the compiler does not properly set this flag anymore in pass 1, and
        for now we only need it after pass 2 (I hope) (JM)
          if not(pi_do_call in current_procinfo.flags) then
            internalerror(2003060703);
}
        include(current_procinfo.flags,pi_do_call);
      end;


    procedure tcgavr32.a_call_ref(list : TAsmList;ref: treference);
      begin
        a_reg_alloc(list,NR_R12);
        a_load_ref_reg(list,OS_ADDR,OS_ADDR,ref,NR_R12);
        a_call_reg(list,NR_R12);
        a_reg_dealloc(list,NR_R12);
        include(current_procinfo.flags,pi_do_call);
      end;


     procedure tcgavr32.a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister);
       begin
          a_op_const_reg_reg(list,op,size,a,reg,reg);
       end;


     procedure tcgavr32.a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister);
       begin
         case op of
           OP_NEG:
             list.concat(taicpu.op_reg_reg_const(A_RSUB,dst,src,0));
           OP_NOT:
             begin
               a_load_reg_reg(list,size,size,src,dst);
               list.concat(taicpu.op_reg(A_COM,dst));
             end
           else
             a_op_reg_reg_reg(list,op,OS_32,src,dst,dst);
         end;
       end;


    const
      op_reg_reg_opcg2asmop: array[TOpCG] of tasmop =
        (A_NONE,A_MOV,A_ADD,A_AND,A_DIVU,A_DIVS,A_MUL,A_MUL,A_NONE,A_NONE,A_OR,
         A_NONE,A_NONE,A_NONE,A_SUB,A_EOR,A_NONE,A_NONE);


    procedure tcgavr32.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister);
      var
        ovloc : tlocation;
      begin
        a_op_const_reg_reg_checkoverflow(list,op,size,a,src,dst,false,ovloc);
      end;


    procedure tcgavr32.a_op_reg_reg_reg(list: TAsmList; op: TOpCg;
      size: tcgsize; src1, src2, dst: tregister);
      var
        ovloc : tlocation;
      begin
        a_op_reg_reg_reg_checkoverflow(list,op,size,src1,src2,dst,false,ovloc);
      end;


    procedure tcgavr32.a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister;setflags : boolean;var ovloc : tlocation);
      var
        shift : byte;
        tmpreg : tregister;
        so : tshifterop;
        i, l1 : longint;
      begin
        ovloc.loc:=LOC_VOID;

        if op = OP_ADD then
          begin
            op := OP_SUB;
            a := -a;
          end;

        if (op in [OP_SUB,OP_OR]) and (a = 0) then
          a_load_reg_reg(list,size,size,src,dst)
        else if in_signed_bits(a,16) and (op = OP_SUB) then
          list.concat(taicpu.op_reg_reg_const(A_SUB,dst,src,a))
        else if in_signed_bits(a,21) and (op = OP_SUB) then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            list.concat(taicpu.op_reg_const(A_SUB,dst,a));
          end
        else if in_signed_bits(a,8) and (op = OP_SUB) then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            list.concat(taicpu.op_reg_const(A_SUB,dst,a));
          end
        else if (op = OP_OR) and ispowerof2(a, i) then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            list.concat(taicpu.op_reg_const(A_SBR,dst,i));
          end
        else if (op = OP_AND) and ispowerof2(not a, i) then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            list.concat(taicpu.op_reg_const(A_CBR,dst,i));
          end
        else if (op = OP_AND) and ((a and $FFFF) = a) then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            if a = $FFFF then
              list.concat(setoppostfix(taicpu.op_reg(A_CASTU, dst), PF_H))
            else if a = $FF then
              list.concat(setoppostfix(taicpu.op_reg(A_CASTU, dst), PF_B))
            else if a = 0 then
              list.concat(taicpu.op_reg_reg(A_EOR, dst, dst))
            else
              list.concat(taicpu.op_reg_const_coh(A_ANDL, dst, a));
          end
        else if (op = OP_AND) and ((a and $FFFF0000) = a) then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            list.concat(taicpu.op_reg_const_coh(A_ANDH, dst, a shr 16));
          end
        else if (op = OP_OR) and ((a and $FFFF) = a) then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            list.concat(taicpu.op_reg_const(A_ORL, dst, a));
          end
        else if (op = OP_OR) and ((a and $FFFF0000) = a) then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            list.concat(taicpu.op_reg_const(A_ORH, dst, a shr 16));
          end
        else if (op = OP_XOR) and ((a and $FFFF) = a) then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            list.concat(taicpu.op_reg_const(A_EORL, dst, a));
          end
        else if (op = OP_XOR) and ((a and $FFFF0000) = a) then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            list.concat(taicpu.op_reg_const(A_EORH, dst, a shr 16));
          end
        { there could be added some more sophisticated optimizations }
        else if (op in [OP_MUL,OP_IMUL]) and (a=1) then
          a_load_reg_reg(list,size,size,src,dst)
        else if (op in [OP_MUL,OP_IMUL]) and (a=0) then
          a_load_const_reg(list,size,0,dst)
        else if (op in [OP_IMUL]) and (a=-1) then
          a_op_reg_reg(list,OP_NEG,size,src,dst)
        { we do this here instead in the peephole optimizer because
          it saves us a register }
        else if (op in [OP_MUL,OP_IMUL]) and ispowerof2(a,l1) then
          a_op_const_reg_reg(list,OP_SHL,size,l1,src,dst)
        else if op in [OP_SHL,OP_SHR,OP_SAR,OP_ROL,OP_ROR] then
          begin
            case op of
              OP_ROR,OP_ROL:
                internalerror(2011012001);
              OP_SHL:
                begin
                  if a>32 then
                    internalerror(200308294);
                  a_load_reg_reg(list,size,size,src,dst);
                  if a>0 then
                    list.concat(taicpu.op_reg_const(A_LSL,dst,a));
                end;
              OP_SHR:
                begin
                  if a>32 then
                    internalerror(200308294);
                  a_load_reg_reg(list,size,size,src,dst);
                  if a>0 then
                    list.concat(taicpu.op_reg_const(A_LSR,dst,a));
                end;
              OP_SAR:
                begin
                  if a>32 then
                    internalerror(200308294);
                  a_load_reg_reg(list,size,size,src,dst);
                  if a>0 then
                    list.concat(taicpu.op_reg_const(A_ASR,dst,a));
                end;
              else
                list.concat(taicpu.op_reg_reg_const(op_reg_reg_opcg2asmop[op],dst,src,a));
            end;
          end
        else
          begin
            tmpreg:=getintregister(list,size);
            a_load_const_reg(list,size,a,tmpreg);
            a_op_reg_reg_reg_checkoverflow(list,op,size,tmpreg,src,dst,setflags,ovloc);
          end;
        maybeadjustresult(list,op,size,dst);
      end;


    procedure tcgavr32.a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation);
      var
        so : tshifterop;
        tmpreg,overflowreg : tregister;
        asmop : tasmop;
      begin
        ovloc.loc:=LOC_VOID;
        case op of
          OP_NEG,OP_NOT,
          OP_ROL,OP_ROR:
            internalerror(200308281);
          OP_SHL:
            list.concat(taicpu.op_reg_reg_reg(A_LSL,dst,src2,src1));
          OP_SHR:
            list.concat(taicpu.op_reg_reg_reg(A_LSR,dst,src2,src1));
          OP_SAR:
            list.concat(taicpu.op_reg_reg_reg(A_ASR,dst,src2,src1));
          OP_IMUL,
          OP_MUL:
            begin
              if op=OP_IMUL then
                begin
                  alloccpuregisters(list,R_INTREGISTER,[RS_R10,RS_R11]);
                  list.concat(setoppostfix(taicpu.op_reg_reg_reg(A_MULS,NR_R10,src1,src2), PF_D));
                  dealloccpuregisters(list,R_INTREGISTER,[RS_R10,RS_R11]);
                  a_load_reg_reg(list,OS_INT,OS_INT,NR_R10,dst);
                end
              else
                list.concat(taicpu.op_reg_reg_reg(A_MUL,dst,src2,src1));
            end;
          else
            list.concat(taicpu.op_reg_reg_reg(op_reg_reg_opcg2asmop[op],dst,src2,src1));
        end;
        maybeadjustresult(list,op,size,dst);
      end;

    procedure tcgavr32.a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; size: tcgsize; src, dst: TRegister);
      begin
        if reverse then
          begin
            list.concat(taicpu.op_reg_reg(A_CLZ, dst, src));
            list.concat(taicpu.op_reg_reg_const(A_RSUB, dst, dst, 31));
            list.concat(setoppostfix(taicpu.op_reg(A_CASTU, dst), PF_B));
          end
        else
          begin
            a_load_reg_reg(list, size, size, src, dst);
            list.concat(taicpu.op_reg(A_BREV, dst));
            list.concat(taicpu.op_reg_reg(A_CLZ, dst, dst));
            list.concat(taicpu.op_reg_reg_const(A_RSUB, dst, dst, 31));
            list.concat(setoppostfix(taicpu.op_reg(A_CASTU, dst), PF_B));
          end
      end;


    function tcgavr32.handle_load_store(list:TAsmList;op: tasmop;oppostfix : toppostfix;reg:tregister;ref: treference):treference;
      var
        tmpreg : tregister;
        tmpref : treference;
        l : tasmlabel;
      begin
        tmpreg:=NR_NO;

        { Be sure to have a base register }
        if (ref.base=NR_NO) and
           not assigned(ref.symbol) then
          begin
            ref.base:=ref.index;
            ref.index:=NR_NO;
          end;

        { absolute symbols can't be handled directly, we've to store the symbol reference
          in the text segment and access it pc relative

          For now, we assume that references where base or index equals to PC are already
          relative, all other references are assumed to be absolute and thus they need
          to be handled extra.

          A proper solution would be to change refoptions to a set and store the information
          if the symbol is absolute or relative there.
        }

        if (assigned(ref.symbol) and
            not(is_pc(ref.base)) and
            not(is_pc(ref.index))
           ) or
           { [#xxx] isn't a valid address operand }
           ((ref.base=NR_NO) and (ref.index=NR_NO)) or
           (ref.offset<-65535) or
           (ref.offset>65535) then
          begin
            reference_reset(tmpref,4);

            { load symbol }
            tmpreg:=getintregister(list,OS_INT);
            if assigned(ref.symbol) then
              begin
                current_asmdata.getjumplabel(l);
                cg.a_label(current_procinfo.aktlocaldata,l);
                tmpref.symboldata:=current_procinfo.aktlocaldata.last;

                current_procinfo.aktlocaldata.concat(tai_const.create_sym_offset(ref.symbol,ref.offset));

                { load consts entry }
                tmpref.symbol:=l;
                tmpref.base:=NR_PC;
                list.concat(taicpu.op_reg_ref(A_LDDPC,tmpreg,tmpref));

                { in case of LDF/STF, we got rid of the NR_R15 }
                if is_pc(ref.base) then
                  ref.base:=NR_NO;
                if is_pc(ref.index) then
                  ref.index:=NR_NO;
              end
            else
              a_load_const_reg(list,OS_ADDR,ref.offset,tmpreg);

            if (ref.base<>NR_NO) then
              begin
                if ref.index<>NR_NO then
                  begin
                    list.concat(taicpu.op_reg_reg_reg(A_ADD,tmpreg,ref.base,tmpreg));
                    ref.base:=tmpreg;
                  end
                else
                  begin
                    ref.index:=tmpreg;
                  end;
              end
            else
              ref.base:=tmpreg;
            ref.offset:=0;
            ref.symbol:=nil;
          end;

        if (ref.base<>NR_NO) and (ref.index<>NR_NO) and (ref.offset<>0) then
          begin
            if tmpreg<>NR_NO then
              a_op_const_reg_reg(list,OP_ADD,OS_ADDR,ref.offset,tmpreg,tmpreg)
            else
              begin
                tmpreg:=getintregister(list,OS_ADDR);
                a_op_const_reg_reg(list,OP_ADD,OS_ADDR,ref.offset,ref.base,tmpreg);
                ref.base:=tmpreg;
              end;
            ref.offset:=0;
          end;

        if is_pc(ref.base) and
           (op = A_LD) then
          list.concat(taicpu.op_reg_ref(A_LDDPC,reg,ref))
        else if op in [A_LD,A_LDINS,A_LDSWP,A_LDDPC,A_LDDSP] then
          list.concat(setoppostfix(taicpu.op_reg_ref(op,reg,ref),oppostfix))
        else
          list.concat(setoppostfix(taicpu.op_ref_reg(op,ref,reg),oppostfix));
        Result := ref;
      end;


    procedure tcgavr32.a_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);
      var
        oppostfix:toppostfix;
        usedtmpref: treference;
        tmpreg : tregister;
        dir : integer;
      begin
        if (TCGSize2Size[FromSize] >= TCGSize2Size[ToSize]) then
          FromSize := ToSize;
        oppostfix:=unsignedsize2postfix(tosize);
        if (ref.alignment in [1,2]) and (ref.alignment<tcgsize2size[tosize]) then
          begin
            if target_info.endian=endian_big then
              dir:=-1
            else
              dir:=1;
            case FromSize of
              OS_16,OS_S16:
                begin
                  tmpreg:=getintregister(list,OS_INT);

                  usedtmpref:=ref;
                  if target_info.endian=endian_big then
                    inc(usedtmpref.offset,1);

                  usedtmpref:=a_internal_load_reg_ref(list,OS_8,OS_8,reg,usedtmpref);
                  inc(usedtmpref.offset,dir);
                  list.concat(taicpu.op_reg_reg_const(A_LSR,tmpreg,reg,8));
                  a_internal_load_reg_ref(list,OS_8,OS_8,tmpreg,usedtmpref);
                end;
              OS_32,OS_S32:
                begin
                  tmpreg:=getintregister(list,OS_INT);
                  usedtmpref:=ref;
                  if ref.alignment=2 then
                    begin
                      if target_info.endian=endian_big then
                        inc(usedtmpref.offset,2);
                      usedtmpref:=a_internal_load_reg_ref(list,OS_16,OS_16,reg,usedtmpref);
                      list.concat(taicpu.op_reg_reg_const(A_LSR,tmpreg,reg,16));
                      inc(usedtmpref.offset,dir*2);
                      a_internal_load_reg_ref(list,OS_16,OS_16,tmpreg,usedtmpref);
                    end
                  else
                    begin
                      if target_info.endian=endian_big then
                        inc(usedtmpref.offset,3);
                      usedtmpref:=a_internal_load_reg_ref(list,OS_8,OS_8,reg,usedtmpref);
                      list.concat(taicpu.op_reg_reg_const(A_LSR,tmpreg,reg,8));
                      {inc(usedtmpref.offset,dir);
                      a_internal_load_reg_ref(list,OS_8,OS_8,tmpreg,usedtmpref);
                      list.concat(taicpu.op_reg_reg_const(A_LSR,tmpreg,tmpreg,8));
                      inc(usedtmpref.offset,dir);
                      a_internal_load_reg_ref(list,OS_8,OS_8,tmpreg,usedtmpref);
                      list.concat(taicpu.op_reg_reg_const(A_LSR,tmpreg,tmpreg,8));}
                      inc(usedtmpref.offset,2*dir);
                      a_internal_load_reg_ref(list,OS_16,OS_16,tmpreg,usedtmpref);
                      list.concat(taicpu.op_reg_reg_const(A_LSR,tmpreg,tmpreg,16));

                      inc(usedtmpref.offset,dir);
                      a_internal_load_reg_ref(list,OS_8,OS_8,tmpreg,usedtmpref);
                    end;
                end
              else
                handle_load_store(list,A_ST,oppostfix,reg,ref);
            end;
          end
        else
          handle_load_store(list,A_ST,oppostfix,reg,ref);
      end;


    function tcgavr32.a_internal_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference):treference;
      var
        oppostfix:toppostfix;
      begin
        oppostfix:=unsignedsize2postfix(tosize);
        result:=handle_load_store(list,A_ST,oppostfix,reg,ref);
      end;


    function tcgavr32.a_internal_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister):treference;
      var
        oppostfix:toppostfix;
      begin
        oppostfix:=size2postfix(fromsize);
        result:=handle_load_store(list,A_LD,oppostfix,reg,ref);
      end;

    procedure tcgavr32.a_load_reg_reg(list : TAsmList; fromsize, tosize : tcgsize;reg1,reg2 : tregister);

      procedure do_shift(op: TAsmOp; shiftimm : byte; reg : tregister);
        begin
          list.concat(taicpu.op_reg_reg_const(op,reg2,reg,shiftimm));
        end;

      var
        instr: taicpu;
        conv_done: boolean;
      begin
        if (tcgsize2size[fromsize]>32) or (tcgsize2size[tosize]>32) or (fromsize=OS_NO) or (tosize=OS_NO) then
          internalerror(2002090901);

        conv_done:=false;
        {if tosize<>fromsize then
          begin
            conv_done:=true;
            if tcgsize2size[tosize]<=tcgsize2size[fromsize] then
              fromsize:=tosize;
            case fromsize of
              OS_8:
                begin
                  a_load_reg_reg(list,OS_INT,OS_INT,reg1,reg2);
                  list.concat(setoppostfix(taicpu.op_reg(A_CASTU, reg2), PF_B));
                end;
              OS_S8:
                begin
                  if tosize=OS_16 then
                    begin
                      do_shift(A_LSL,24,reg1);
                      do_shift(A_ASR,8,reg2);
                      do_shift(A_LSR,16,reg2);
                    end
                  else
                    list.concat(taicpu.op_reg_reg_const_const(A_BFEXTS,reg2,reg1,0,8));
                end;
              OS_16:
                list.concat(taicpu.op_reg_reg_const_const(A_BFEXTU,reg2,reg1,0,16));
              OS_S16:
                list.concat(taicpu.op_reg_reg_const_const(A_BFEXTS,reg2,reg1,0,16));
              else
                conv_done:=false;
            end;
          end;}
        if tcgsize2size[tosize]>tcgsize2size[fromsize] then
          begin
            conv_done := true;
            a_load_reg_reg(list, fromsize,fromsize, reg1, reg2);
            case fromsize of
              OS_8: list.concat(setoppostfix(taicpu.op_reg(A_CASTU, reg2), PF_B));
              OS_S8: list.concat(setoppostfix(taicpu.op_reg(A_CASTS, reg2), PF_B));
              OS_16: list.concat(setoppostfix(taicpu.op_reg(A_CASTU, reg2), PF_H));
              OS_S16: list.concat(setoppostfix(taicpu.op_reg(A_CASTS, reg2), PF_H));
            else
              conv_done := false;
            end;
          end;

        if not conv_done and (reg1<>reg2) then
          begin
            { same size, only a register mov required }
            instr:=taicpu.op_reg_reg(A_MOV,reg2,reg1);
            list.Concat(instr);
            { Notify the register allocator that we have written a move instruction so
              it can try to eliminate it. }
            add_move_instruction(instr);
          end;
      end;


    {  comparison operations }
    procedure tcgavr32.a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : tcgint;reg : tregister;
      l : tasmlabel);
      var
        tmpreg : tregister;
        b : byte;
      begin
        if in_signed_bits(a,21) and
           (size in [OS_32, OS_S32]) then
          list.concat(taicpu.op_reg_const(A_CP,reg,a))
        else
          begin
            tmpreg:=getintregister(list,size);
            a_load_const_reg(list,size,a,tmpreg);
            list.concat(setoppostfix(taicpu.op_reg_reg(A_CP,reg,tmpreg), unsignedsize2postfix(size)));
          end;
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgavr32.a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel);
      begin
        list.concat(setoppostfix(taicpu.op_reg_reg(A_CP,reg2,reg1), unsignedsize2postfix(size)));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgavr32.a_jmp_name(list : TAsmList;const s : string);
      var
        ai : taicpu;
      begin
        ai:=taicpu.op_sym(A_BR,current_asmdata.RefAsmSymbol(s));
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgavr32.a_jmp_always(list : TAsmList;l: tasmlabel);
      var
        ai : taicpu;
      begin
        ai:=taicpu.op_sym(A_RJMP,l);
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgavr32.a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel);
      var
        ai : taicpu;
      begin
        ai:=setcondition(taicpu.op_sym(A_BR,l),flags_to_cond(f));
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgavr32.g_flags2reg(list: TAsmList; size: TCgSize; const f: TResFlags; reg: TRegister);
      begin
        list.Concat(setcondition(taicpu.op_reg(A_SR,reg), flags_to_cond(f)));
      end;


    procedure tcgavr32.g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);
      var
         ref : treference;
         r : byte;
         regs : tcpuregisterset;
         stackcount : pint;
      begin
        localsize := align(localsize,4);
        if not(nostackframe) then
          begin
            //a_reg_alloc(list,NR_STACK_POINTER_REG);
            list.concat(tai_comment.Create(strpnew('Entry '+BoolToStr(current_procinfo.framepointer=NR_STACK_POINTER_REG,'t','f')+inttostr(localsize))));

            { save int registers }
            reference_reset(ref,4);
            ref.base:=NR_STACK_POINTER_REG;
            ref.addressmode:=AM_PREINDEXED;
            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);

            Exclude(regs,RS_STACK_POINTER_REG);

            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              regs:=regs+[RS_FRAME_POINTER_REG,RS_R14]
            else
              if (regs<>[]) or (pi_do_call in current_procinfo.flags) then
                include(regs,RS_R14);

            if regs<>[] then
              list.concat(taicpu.op_ref_regset(A_STM,ref,R_INTREGISTER,R_SUBWHOLE,regs));

            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              begin
                stackcount:=0;
                for r:=RS_R0 to RS_R15 do
                  if (r in regs) then
                    inc(stackcount,4);

                list.concat(taicpu.op_reg_reg_const(A_SUB,current_procinfo.framepointer,NR_STACK_POINTER_REG,-stackcount));

                if localsize > 0 then
                  list.concat(taicpu.op_reg_const(A_SUB,NR_STACK_POINTER_REG,localsize));
              end
            else if localsize <> 0 then
              list.concat(taicpu.op_reg_const(A_SUB,NR_STACK_POINTER_REG,localsize));
          end
        else
          begin
            reference_reset(ref,4);
            ref.base:=NR_STACK_POINTER_REG;
            ref.addressmode:=AM_PREINDEXED;
            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);

            if pi_do_call in current_procinfo.flags then
              Include(regs,RS_R14); // Link register

            if regs <> [] then
              list.concat(taicpu.op_ref_regset(A_STM,ref,R_INTREGISTER,R_SUBWHOLE,regs));

            if localsize<>0 then
              list.concat(taicpu.op_reg_const(A_SUB,NR_STACK_POINTER_REG,localsize));
          end;
      end;


    procedure tcgavr32.g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean);
      var
         ref : treference;
         LocalSize : longint;
         r,
         shift : byte;
         regs : tcpuregisterset;
         stackmisalignment: pint;
      begin
        localsize:=Align(current_procinfo.calc_stackframe_size,4);

        if not(nostackframe) then
          begin
            stackmisalignment:=0;

            list.concat(tai_comment.Create(strpnew('Exit '+inttostr(localsize))));

            if (current_procinfo.framepointer=NR_STACK_POINTER_REG) and
               (localsize<>0) then
              list.concat(taicpu.op_reg_const(A_SUB,NR_STACK_POINTER_REG,-localsize));

            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);
            if (pi_do_call in current_procinfo.flags) or (regs<>[]) then
              begin
                exclude(regs,RS_R14);
                include(regs,RS_R15);
              end;
            { restore saved stack pointer to SP (R13) and saved lr to PC (R15).
              The saved PC came after that but is discarded, since we restore
              the stack pointer }

            if (current_procinfo.framepointer<>NR_STACK_POINTER_REG) then
              regs:=regs+[RS_FRAME_POINTER_REG,RS_R15];

            if (current_procinfo.framepointer=NR_STACK_POINTER_REG) then
              begin
                if regs=[] then
                  list.concat(taicpu.op_reg(A_RET,NR_FUNCTION_RETURN_REG))
                else
                  begin
                    reference_reset(ref,4);
                    ref.base:=NR_STACK_POINTER_REG;
                    ref.addressmode:=AM_POSTINDEXED;
                    list.concat(taicpu.op_ref_regset(A_LDM,ref,R_INTREGISTER,R_SUBWHOLE,regs));
                  end;
              end
            else
              begin
                list.concat(taicpu.op_reg_reg(A_MOV,NR_STACK_POINTER_REG,NR_FRAME_POINTER_REG));

                { restore int registers and return }
                reference_reset(ref,4);
                ref.base:=NR_STACK_POINTER_REG;
                ref.addressmode:=AM_POSTINDEXED;
                list.concat(taicpu.op_ref_regset(A_LDM,ref,R_INTREGISTER,R_SUBWHOLE,regs));
              end;
          end
        else
          begin
            if localsize<>0 then
              list.concat(taicpu.op_reg_const(A_SUB,NR_STACK_POINTER_REG,-localsize));

            reference_reset(ref,4);
            ref.base:=NR_STACK_POINTER_REG;
            ref.addressmode:=AM_POSTINDEXED;
            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);

            if (pi_do_call in current_procinfo.flags) or
               (regs <> []) then
              Include(regs,RS_R15); // PC register

            if regs <> [] then
              list.concat(taicpu.op_ref_regset(A_LDM,ref,R_INTREGISTER,R_SUBWHOLE,regs))
            else
              list.concat(taicpu.op_reg(A_RET,NR_FUNCTION_RETURN_REG));
          end;
      end;


    procedure tcgavr32.a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);
      var
        b : byte;
        tmpref : treference;
        instr : taicpu;
      begin
        if ref.addressmode<>AM_OFFSET then
          internalerror(200309071);
        tmpref:=ref;
        { Be sure to have a base register }
        if (tmpref.base=NR_NO) then
          begin
            tmpref.base:=tmpref.index;
            tmpref.index:=NR_NO;
          end;

        if assigned(tmpref.symbol) or
           not in_signed_bits(tmpref.offset,16) then
          fixref(list,tmpref);

        { expect a base here if there is an index }
        if (tmpref.base=NR_NO) and (tmpref.index<>NR_NO) then
          internalerror(200312022);

        if tmpref.index<>NR_NO then
          begin
            a_op_reg_reg_reg(list,OP_ADD,OS_ADDR,tmpref.base,tmpref.index,r);
            if tmpref.offset<>0 then
              a_op_const_reg_reg(list,OP_ADD,OS_ADDR,tmpref.offset,r,r);
          end
        else
          begin
            if tmpref.base=NR_NO then
              a_load_const_reg(list,OS_ADDR,tmpref.offset,r)
            else
              if tmpref.offset<>0 then
                a_op_const_reg_reg(list,OP_ADD,OS_ADDR,tmpref.offset,tmpref.base,r)
              else
                begin
                  instr:=taicpu.op_reg_reg(A_MOV,r,tmpref.base);
                  list.concat(instr);
                  add_move_instruction(instr);
                end;
          end;
      end;


    procedure tcgavr32.fixref(list : TAsmList;var ref : treference);
      var
        tmpreg : tregister;
        tmpref : treference;
        l : tasmlabel;
      begin
        { absolute symbols can't be handled directly, we've to store the symbol reference
          in the text segment and access it pc relative

          For now, we assume that references where base or index equals to PC are already
          relative, all other references are assumed to be absolute and thus they need
          to be handled extra.

          A proper solution would be to change refoptions to a set and store the information
          if the symbol is absolute or relative there.
        }
        { create consts entry }
        reference_reset(tmpref,4);
        current_asmdata.getjumplabel(l);
        cg.a_label(current_procinfo.aktlocaldata,l);
        tmpref.symboldata:=current_procinfo.aktlocaldata.last;

        if assigned(ref.symbol) then
          current_procinfo.aktlocaldata.concat(tai_const.create_sym_offset(ref.symbol,ref.offset))
        else
          current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(ref.offset));

        { load consts entry }
        tmpreg:=getintregister(list,OS_INT);
        tmpref.symbol:=l;
        tmpref.base:=NR_PC;
        list.concat(taicpu.op_reg_ref(A_LDDPC,tmpreg,tmpref));

        if (ref.base<>NR_NO) then
          begin
            if ref.index<>NR_NO then
              begin
                list.concat(taicpu.op_reg_reg_reg(A_ADD,tmpreg,ref.base,tmpreg));
                ref.base:=tmpreg;
              end
            else
              if ref.base<>NR_PC then
                  ref.index:=tmpreg
                else
                  ref.base:=tmpreg;
          end
        else
          ref.base:=tmpreg;
        ref.offset:=0;
        ref.symbol:=nil;
      end;


    procedure tcgavr32.g_concatcopy_move(list : TAsmList;const source,dest : treference;len : tcgint);
      var
        paraloc1,paraloc2,paraloc3 : TCGPara;
      begin
        paraloc1.init;
        paraloc2.init;
        paraloc3.init;
        paramanager.getintparaloc(pocall_default,1,paraloc1);
        paramanager.getintparaloc(pocall_default,2,paraloc2);
        paramanager.getintparaloc(pocall_default,3,paraloc3);
        a_load_const_cgpara(list,OS_INT,len,paraloc3);
        a_loadaddr_ref_cgpara(list,dest,paraloc2);
        a_loadaddr_ref_cgpara(list,source,paraloc1);
        paramanager.freecgpara(list,paraloc3);
        paramanager.freecgpara(list,paraloc2);
        paramanager.freecgpara(list,paraloc1);
        alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        alloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        a_call_name(list,'FPC_MOVE',false);
        dealloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        paraloc3.done;
        paraloc2.done;
        paraloc1.done;
      end;


    procedure tcgavr32.g_concatcopy_internal(list : TAsmList;const source,dest : treference;len : tcgint;aligned : boolean);
      const
        maxtmpreg=10;{roozbeh: can be reduced to 8 or lower if might conflick with reserved ones,also +2 is used becouse of regs required for referencing}

      var
        srcref,dstref,usedtmpref,usedtmpref2:treference;
        srcreg,destreg,countreg,r,tmpreg:tregister;
        helpsize:tcgint;
        copysize:byte;
        cgsize:Tcgsize;
        tmpregisters:array[1..maxtmpreg] of tregister;
        tmpregi,tmpregi2:byte;

      { will never be called with count<=4 }
      procedure genloop(count : aword;size : byte);
        const
          size2opsize : array[1..4] of tcgsize = (OS_8,OS_16,OS_NO,OS_32);
        var
          l : tasmlabel;
        begin
          current_asmdata.getjumplabel(l);
          if count<size then size:=1;
          a_load_const_reg(list,OS_INT,count div size,countreg);
          cg.a_label(list,l);
          srcref.addressmode:=AM_POSTINDEXED;
          dstref.addressmode:=AM_POSTINDEXED;
          srcref.offset:=size;
          dstref.offset:=size;
          r:=getintregister(list,size2opsize[size]);
          a_load_ref_reg(list,size2opsize[size],size2opsize[size],srcref,r);
          list.concat(taicpu.op_reg_reg_const(A_SUB,countreg,countreg,1));
          a_load_reg_ref(list,size2opsize[size],size2opsize[size],r,dstref);
          a_jmp_flags(list,F_NE,l);
          srcref.offset:=1;
          dstref.offset:=1;
          case count mod size of
            1:
              begin
                a_load_ref_reg(list,OS_8,OS_8,srcref,r);
                a_load_reg_ref(list,OS_8,OS_8,r,dstref);
              end;
            2:
              if aligned then
                begin
                  a_load_ref_reg(list,OS_16,OS_16,srcref,r);
                  a_load_reg_ref(list,OS_16,OS_16,r,dstref);
                end
              else
                begin
                  a_load_ref_reg(list,OS_8,OS_8,srcref,r);
                  a_load_reg_ref(list,OS_8,OS_8,r,dstref);
                  a_load_ref_reg(list,OS_8,OS_8,srcref,r);
                  a_load_reg_ref(list,OS_8,OS_8,r,dstref);
                end;
            3:
              if aligned then
                begin
                  srcref.offset:=2;
                  dstref.offset:=2;
                  a_load_ref_reg(list,OS_16,OS_16,srcref,r);
                  a_load_reg_ref(list,OS_16,OS_16,r,dstref);
                  a_load_ref_reg(list,OS_8,OS_8,srcref,r);
                  a_load_reg_ref(list,OS_8,OS_8,r,dstref);
                end
              else
                begin
                  a_load_ref_reg(list,OS_8,OS_8,srcref,r);
                  a_load_reg_ref(list,OS_8,OS_8,r,dstref);
                  a_load_ref_reg(list,OS_8,OS_8,srcref,r);
                  a_load_reg_ref(list,OS_8,OS_8,r,dstref);
                  a_load_ref_reg(list,OS_8,OS_8,srcref,r);
                  a_load_reg_ref(list,OS_8,OS_8,r,dstref);
                end;
          end;
          { keep the registers alive }
          list.concat(taicpu.op_reg_reg(A_MOV,countreg,countreg));
          list.concat(taicpu.op_reg_reg(A_MOV,srcreg,srcreg));
          list.concat(taicpu.op_reg_reg(A_MOV,destreg,destreg));
        end;

      begin
        if len=0 then
          exit;
        helpsize:=12+maxtmpreg*4;//52 with maxtmpreg=10
        dstref:=dest;
        srcref:=source;
        if cs_opt_size in current_settings.optimizerswitches then
          helpsize:=8;
        if (len<=helpsize) and aligned then
          begin
            tmpregi:=0;
            srcreg:=getintregister(list,OS_ADDR);

            { explicit pc relative addressing, could be
              e.g. a floating point constant }
            if source.base=NR_PC then
              begin
                { ... then we don't need a loadaddr }
                srcref:=source;
              end
            else
              begin
                a_loadaddr_ref_reg(list,source,srcreg);
                reference_reset_base(srcref,srcreg,0,source.alignment);
              end;

            while (len div 4 <> 0) and (tmpregi<maxtmpreg) do
              begin
                inc(tmpregi);
                tmpregisters[tmpregi]:=getintregister(list,OS_32);
                a_load_ref_reg(list,OS_32,OS_32,srcref,tmpregisters[tmpregi]);
                inc(srcref.offset,4);
                dec(len,4);
              end;

            destreg:=getintregister(list,OS_ADDR);
            a_loadaddr_ref_reg(list,dest,destreg);
            reference_reset_base(dstref,destreg,0,dest.alignment);
            tmpregi2:=1;
            while (tmpregi2<=tmpregi) do
              begin
                a_load_reg_ref(list,OS_32,OS_32,tmpregisters[tmpregi2],dstref);
                inc(dstref.offset,4);
                inc(tmpregi2);
              end;

            copysize:=4;
            cgsize:=OS_32;
            while len<>0 do
              begin
                if len<2 then
                  begin
                    copysize:=1;
                    cgsize:=OS_8;
                  end
                else if len<4 then
                  begin
                    copysize:=2;
                    cgsize:=OS_16;
                  end;
                dec(len,copysize);
                r:=getintregister(list,cgsize);
                a_load_ref_reg(list,cgsize,cgsize,srcref,r);
                a_load_reg_ref(list,cgsize,cgsize,r,dstref);
                inc(srcref.offset,copysize);
                inc(dstref.offset,copysize);
              end;{end of while}
          end
        else
          begin
            cgsize:=OS_32;
            if (len<=4) then{len<=4 and not aligned}
              begin
                r:=getintregister(list,cgsize);
                usedtmpref:=a_internal_load_ref_reg(list,OS_8,OS_8,srcref,r);
                if Len=1 then
                  a_load_reg_ref(list,OS_8,OS_8,r,dstref)
                else
                  begin
                    tmpreg:=getintregister(list,cgsize);
                    usedtmpref2:=a_internal_load_reg_ref(list,OS_8,OS_8,r,dstref);
                    inc(usedtmpref.offset,1);
                    a_load_ref_reg(list,OS_8,OS_8,usedtmpref,tmpreg);
                    inc(usedtmpref2.offset,1);
                    a_load_reg_ref(list,OS_8,OS_8,tmpreg,usedtmpref2);
                    if len>2 then
                      begin
                        inc(usedtmpref.offset,1);
                        a_load_ref_reg(list,OS_8,OS_8,usedtmpref,tmpreg);
                        inc(usedtmpref2.offset,1);
                        a_load_reg_ref(list,OS_8,OS_8,tmpreg,usedtmpref2);
                        if len>3 then
                          begin
                            inc(usedtmpref.offset,1);
                            a_load_ref_reg(list,OS_8,OS_8,usedtmpref,tmpreg);
                            inc(usedtmpref2.offset,1);
                            a_load_reg_ref(list,OS_8,OS_8,tmpreg,usedtmpref2);
                          end;
                        end;
                      end;
            end{end of if len<=4}
            else
              begin{unaligned & 4<len<helpsize **or** aligned/unaligned & len>helpsize}
                destreg:=getintregister(list,OS_ADDR);
                a_loadaddr_ref_reg(list,dest,destreg);
                reference_reset_base(dstref,destreg,0,dest.alignment);

                srcreg:=getintregister(list,OS_ADDR);
                a_loadaddr_ref_reg(list,source,srcreg);
                reference_reset_base(srcref,srcreg,0,source.alignment);

                countreg:=getintregister(list,OS_32);

//            if cs_opt_size in current_settings.optimizerswitches  then
                { roozbeh : it seems loading 1 byte is faster becouse of caching/fetching(?) }
                {if aligned then
                genloop(len,4)
                else}
                genloop(len,1);
            end;
          end;
    end;

    procedure tcgavr32.g_concatcopy_unaligned(list : TAsmList;const source,dest : treference;len : tcgint);
      begin
        g_concatcopy_internal(list,source,dest,len,false);
      end;


    procedure tcgavr32.g_concatcopy(list : TAsmList;const source,dest : treference;len : tcgint);
      begin
        if (source.alignment in [1..3]) or
          (dest.alignment in [1..3]) then
          g_concatcopy_internal(list,source,dest,len,false)
        else
          g_concatcopy_internal(list,source,dest,len,true);
      end;


    procedure tcgavr32.g_overflowCheck(list : TAsmList;const l : tlocation;def : tdef);
      var
        ovloc : tlocation;
      begin
        ovloc.loc:=LOC_VOID;
        g_overflowCheck_loc(list,l,def,ovloc);
      end;


    procedure tcgavr32.g_overflowCheck_loc(List:TAsmList;const Loc:TLocation;def:TDef;ovloc : tlocation);
      var
        hl : tasmlabel;
        ai:TAiCpu;
        hflags : tresflags;
      begin
        if not(cs_check_overflow in current_settings.localswitches) then
          exit;
        current_asmdata.getjumplabel(hl);
        case ovloc.loc of
          LOC_VOID:
            begin
              ai:=taicpu.op_sym(A_BR,hl);
              ai.is_jmp:=true;

              if not((def.typ=pointerdef) or
                    ((def.typ=orddef) and
                     (torddef(def).ordtype in [u64bit,u16bit,u32bit,u8bit,uchar,pasbool]))) then
                 ai.SetCondition(C_VC)
              else
                if TAiCpu(List.Last).opcode in [A_RSUB,A_SBC,A_SCR,A_SUB] then
                  ai.SetCondition(C_CS)
                else
                  ai.SetCondition(C_CC);

              list.concat(ai);
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

        a_call_name(list,'FPC_OVERFLOW',false);
        a_label(list,hl);
      end;


    procedure tcgavr32.g_save_registers(list : TAsmList);
      begin
        { this work is done in g_proc_entry }
      end;


    procedure tcgavr32.g_restore_registers(list : TAsmList);
      begin
        { this work is done in g_proc_exit }
      end;


    procedure tcgavr32.a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
      var
        ai : taicpu;
      begin
        ai:=Taicpu.Op_sym(A_BR,l);
        ai.SetCondition(OpCmp2AsmCond[cond]);
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgavr32.g_adjust_self_value(list:TAsmList;procdef: tprocdef;ioffset: tcgint);
      var
        hsym : tsym;
        href : treference;
        paraloc : Pcgparalocation;
        shift : byte;
      begin
        { calculate the parameter info for the procdef }
        procdef.init_paraloc_info(callerside);
        hsym:=tsym(procdef.parast.Find('self'));
        if not(assigned(hsym) and
          (hsym.typ=paravarsym)) then
          internalerror(200305251);
        paraloc:=tparavarsym(hsym).paraloc[callerside].location;
        while paraloc<>nil do
          with paraloc^ do
            begin
              case loc of
                LOC_REGISTER:
                  begin
                    if in_signed_bits(ioffset, 21) then
                      a_op_const_reg(list,OP_SUB,size,ioffset,register)
                    else
                      begin
                        a_load_const_reg(list,OS_ADDR,ioffset,NR_R12);
                        a_op_reg_reg(list,OP_SUB,size,NR_R12,register);
                      end;
                  end;
                LOC_REFERENCE:
                  begin
                    { offset in the wrapper needs to be adjusted for the stored
                      return address }
                    reference_reset_base(href,reference.index,reference.offset+sizeof(aint),sizeof(pint));
                    if in_signed_bits(ioffset,21) then
                      a_op_const_ref(list,OP_SUB,size,ioffset,href)
                    else
                      begin
                        a_load_const_reg(list,OS_ADDR,ioffset,NR_R12);
                        a_op_reg_ref(list,OP_SUB,size,NR_R12,href);
                      end;
                  end
                else
                  internalerror(200309189);
              end;
              paraloc:=next;
            end;
      end;

    procedure tcgavr32.g_stackpointer_alloc(list: TAsmList; size: longint);
      begin
        internalerror(200807237);
      end;


    procedure tcgavr32.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);

      procedure loadvmttor12;
        var
          href : treference;
        begin
          reference_reset_base(href,NR_R0,0,sizeof(pint));
          cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R12);
        end;


      procedure op_onr12methodaddr;
        var
          href : treference;
        begin
          if (procdef.extnumber=$ffff) then
            Internalerror(200006139);
          { call/jmp  vmtoffs(%eax) ; method offs }
          reference_reset_base(href,NR_R12,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),sizeof(pint));
          cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R12);
          list.concat(taicpu.op_reg_reg(A_MOV,NR_PC,NR_R12));
        end;

      var
        make_global : boolean;
      begin
        if not(procdef.proctypeoption in [potype_function,potype_procedure]) then
          Internalerror(200006137);
        if not assigned(procdef.struct) or
           (procdef.procoptions*[po_classmethod, po_staticmethod,
             po_methodpointer, po_interrupt, po_iocheck]<>[]) then
          Internalerror(200006138);
        if procdef.owner.symtabletype<>ObjectSymtable then
          Internalerror(200109191);

        make_global:=false;
        if (not current_module.is_unit) or
           create_smartlink or
           (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
          make_global:=true;

        if make_global then
          list.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
        else
          list.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

        { the wrapper might need aktlocaldata for the additional data to
          load the constant }
        current_procinfo:=cprocinfo.create(nil);

        { set param1 interface to self  }
        g_adjust_self_value(list,procdef,ioffset);

        { case 4 }
        if po_virtualmethod in procdef.procoptions then
          begin
            loadvmttor12;
            op_onr12methodaddr;
          end
        { case 0 }
        else
          list.concat(taicpu.op_sym(A_BR,current_asmdata.RefAsmSymbol(procdef.mangledname)));
        list.concatlist(current_procinfo.aktlocaldata);

        current_procinfo.Free;
        current_procinfo:=nil;

        list.concat(Tai_symbol_end.Createname(labelname));
      end;


    procedure tcgavr32.maybeadjustresult(list: TAsmList; op: TOpCg; size: tcgsize; dst: tregister);
      const
        overflowops = [OP_MUL,OP_SHL,OP_ADD,OP_SUB,OP_NOT,OP_NEG];
      begin
        if (op in overflowops) and
           (size in [OS_8,OS_S8,OS_16,OS_S16]) then
          a_load_reg_reg(list,OS_32,size,dst,dst);
      end;


    procedure tcg64favr32.a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);
      begin
        case op of
          OP_NEG:
            begin
              list.concat(taicpu.op_reg_reg_const(A_RSUB,regdst.reglo,regsrc.reglo,0));
              list.concat(taicpu.op_reg(A_ACR, regdst.reghi));
              list.concat(taicpu.op_reg_reg_const(A_RSUB,regdst.reghi,regsrc.reghi,0));
            end;
          OP_NOT:
            begin
              cg.a_op_reg_reg(list,OP_NOT,OS_INT,regsrc.reglo,regdst.reglo);
              cg.a_op_reg_reg(list,OP_NOT,OS_INT,regsrc.reghi,regdst.reghi);
            end;
          else
            a_op64_reg_reg_reg(list,op,size,regsrc,regdst,regdst);
        end;
      end;


    procedure tcg64favr32.a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);
      begin
        a_op64_const_reg_reg(list,op,size,value,reg,reg);
      end;


    procedure tcg64favr32.a_op64_const_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64);
      var
        ovloc : tlocation;
      begin
        a_op64_const_reg_reg_checkoverflow(list,op,size,value,regsrc,regdst,false,ovloc);
      end;


    procedure tcg64favr32.a_op64_reg_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64);
      var
        ovloc : tlocation;
      begin
        a_op64_reg_reg_reg_checkoverflow(list,op,size,regsrc1,regsrc2,regdst,false,ovloc);
      end;


    procedure tcg64favr32.a_op64_const_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64;setflags : boolean;var ovloc : tlocation);
      var
        tmpreg : tregister;
        b : byte;
      begin
        ovloc.loc:=LOC_VOID;
        case op of
          OP_NEG,
          OP_NOT :
            internalerror(200306017);
        end;
          begin
            case op of
              OP_AND,OP_OR,OP_XOR:
                begin
                  cg.a_op_const_reg_reg(list,op,OS_32,aint(lo(value)),regsrc.reglo,regdst.reglo);
                  cg.a_op_const_reg_reg(list,op,OS_32,aint(hi(value)),regsrc.reghi,regdst.reghi);
                end;
              OP_ADD:
                begin
                    begin
                      cg.a_op_const_reg_reg(list,OP_ADD,OS_32,aint(lo(value)),regsrc.reglo,regdst.reglo);
                    end;

                    begin
                      tmpreg:=cg.getintregister(list,OS_32);
                      cg.a_load_const_reg(list,OS_32,aint(hi(value)),tmpreg);
                      list.concat(taicpu.op_reg_reg_reg(A_ADC,regdst.reghi,regsrc.reghi,tmpreg));
                    end;
                end;
              OP_SUB:
                begin
                    begin
                      cg.a_op_const_reg_reg(list,OP_SUB,OS_32,aint(lo(value)),regsrc.reglo,regdst.reglo);
                    end;

                    begin
                      tmpreg:=cg.getintregister(list,OS_32);
                      cg.a_load_const_reg(list,OS_32,hi(value),tmpreg);
                      list.concat(taicpu.op_reg_reg_reg(A_SBC,regdst.reghi,regsrc.reghi,tmpreg));
                    end;
                end;
            else
              internalerror(2003083101);
          end;
        end;
      end;


    procedure tcg64favr32.a_op64_reg_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64;setflags : boolean;var ovloc : tlocation);
      begin
        ovloc.loc:=LOC_VOID;
        case op of
          OP_NEG,
          OP_NOT :
            internalerror(200306017);
        end;
          begin
            case op of
              OP_AND,OP_OR,OP_XOR:
                begin
                  cg.a_op_reg_reg_reg(list,op,OS_32,regsrc1.reglo,regsrc2.reglo,regdst.reglo);
                  cg.a_op_reg_reg_reg(list,op,OS_32,regsrc1.reghi,regsrc2.reghi,regdst.reghi);
                end;
              OP_ADD:
                begin
                  list.concat(taicpu.op_reg_reg_reg(A_ADD,regdst.reglo,regsrc1.reglo,regsrc2.reglo));
                  list.concat(taicpu.op_reg_reg_reg(A_ADC,regdst.reghi,regsrc1.reghi,regsrc2.reghi));
                end;
              OP_SUB:
                begin
                  list.concat(taicpu.op_reg_reg_reg(A_SUB,regdst.reglo,regsrc2.reglo,regsrc1.reglo));
                  list.concat(taicpu.op_reg_reg_reg(A_SBC,regdst.reghi,regsrc2.reghi,regsrc1.reghi));
                end;
              else
                internalerror(2003083101);
            end;
          end;
      end;


    procedure create_codegen;
      begin
        cg:=tcgavr32.create;
        cg64:=tcg64favr32.create;

        casmoptimizer:=TCpuAsmOptimizer;
      end;

end.
