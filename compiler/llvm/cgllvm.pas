{

    Copyright (c) 2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit implements the code generator for LLVM

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
unit cgllvm;

{$i fpcdefs.inc}

  interface

    uses
      globtype,symtype,symdef,
      cgbase,cgutils,cgobj,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      parabase,
      cpubase,cpuinfo,node,cg64f32,rgcpu;


    type

     { tcgllvm }

     tcgllvm = class(tcg)
       procedure init_register_allocators; override;
       procedure done_register_allocators; override;

       procedure a_load_const_cgpara(list : TAsmList;size : tcgsize;a : aint;const paraloc : TCGPara);override;
       procedure a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);override;
       procedure a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);override;

       procedure a_call_name(list : TAsmList;const s : string; weak: boolean);override;
       procedure a_call_reg(list : TAsmList;reg: tregister);override;
       procedure a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: aint; reg: TRegister); override;
       procedure a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;

       procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister); override;
       procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister); override;
       procedure a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister;setflags : boolean;var ovloc : tlocation);override;
       procedure a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation);override;

       { move instructions }
       procedure a_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);override;
       procedure a_load_reg_reg(list : TAsmList; fromsize, tosize : tcgsize;reg1,reg2 : tregister);override;
       procedure a_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister);override;
       procedure a_load_const_reg(list : TAsmList; size: tcgsize; a : aint;reg : tregister);override;

       { fpu move instructions }
       procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister); override;
       procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister); override;
       procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference); override;

       procedure a_loadfpu_ref_cgpara(list : TAsmList;size : tcgsize;const ref : treference;const paraloc : TCGPara);override;
       {  comparison operations }
       procedure a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : aint;reg : tregister;
         l : tasmlabel);override;
       procedure a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;

       procedure a_jmp_name(list : TAsmList;const s : string); override;
       procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;
{$ifdef using_llvm_tresflags}
       procedure a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel); override;

       procedure g_flags2reg(list: TAsmList; size: TCgSize; const f: TResFlags; reg: TRegister); override;
{$endif}
       procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);override;
       procedure g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean); override;

       procedure a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);override;

       procedure g_concatcopy(list : TAsmList;const source,dest : treference;len : aint);override;

       procedure g_overflowcheck(list: TAsmList; const l: tlocation; def: tdef); override;
       procedure g_overflowCheck_loc(List:TAsmList;const Loc:TLocation;def:TDef;ovloc : tlocation);override;

       procedure g_save_registers(list : TAsmList);override;
       procedure g_restore_registers(list : TAsmList);override;

       procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
       procedure g_stackpointer_alloc(list : TAsmList;size : longint);override;

     private
       function make_simple_ref(list: TAsmList; const ref: treference): treference;
       function make_simple_ref_of_ptrsize(list: TAsmList; const ref: treference; tosize: tcgsize): treference;
     end;

    procedure create_codegen;

  implementation


    uses
      globals,verbose,systems,cutils,
      fmodule,
      symconst,symsym,
      tgobj,rgobj,
      procinfo,cpupi,
      paramgr,
      llvmbase,
      aasmllvm,defutil;


    procedure tcgllvm.init_register_allocators;
      begin
        inherited init_register_allocators;
        rg[R_INTREGISTER]:=trgobj.create(R_INTREGISTER,R_SUBWHOLE,[RS_INVALID],RS_INVALID,[]);
        rg[R_FPUREGISTER]:=trgobj.create(R_FPUREGISTER,R_SUBNONE,[RS_INVALID],RS_INVALID,[]);
        rg[R_MMREGISTER]:=trgobj.create(R_FPUREGISTER,R_SUBNONE,[RS_INVALID],RS_INVALID,[]);
        rg[R_ADDRESSREGISTER]:=trgobj.create(R_ADDRESSREGISTER,R_SUBNONE,[RS_INVALID],RS_INVALID,[]);
        rg[R_FLAGSREGISTER]:=trgobj.create(R_FLAGSREGISTER,R_SUBNONE,[RS_INVALID],RS_INVALID,[]);
        rg[R_AGGREGATEREGISTER]:=trgobj.create(R_AGGREGATEREGISTER,R_SUBNONE,[RS_INVALID],RS_INVALID,[]);
      end;


    procedure tcgllvm.done_register_allocators;
     begin
       rg[R_INTREGISTER].free;
       rg[R_FPUREGISTER].free;
       rg[R_MMREGISTER].free;
       rg[R_ADDRESSREGISTER].free;
       rg[R_FLAGSREGISTER].free;
       rg[R_AGGREGATEREGISTER].free;
       inherited done_register_allocators;
     end;


    procedure tcgllvm.a_load_const_cgpara(list : TAsmList;size : tcgsize;a : aint;const paraloc : TCGPara);
     //var
     //  ref: treference;
     begin
       { TODO }
       internalerror(2010081318);
       //paraloc.check_simple_location;
       //paramanager.allocparaloc(list,paraloc.location);
       //case paraloc.location^.loc of
       //  LOC_REGISTER,LOC_CREGISTER:
       //    a_load_const_reg(list,size,a,paraloc.location^.register);
       //  LOC_REFERENCE:
       //    begin
       //       reference_reset(ref,paraloc.alignment);
       //       ref.base:=paraloc.location^.reference.index;
       //       ref.offset:=paraloc.location^.reference.offset;
       //       a_load_const_ref(list,size,a,ref);
       //    end;
       //  else
       //    internalerror(2002081101);
       //end;
     end;


    procedure tcgllvm.a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);
     var
       tmpref, ref: treference;
       location: pcgparalocation;
       sizeleft: aint;
     begin
       { TODO }
       internalerror(2010081317);
       //location := paraloc.location;
       //tmpref := r;
       //sizeleft := paraloc.intsize;
       //while assigned(location) do
       //  begin
       //    paramanager.allocparaloc(list,location);
       //    case location^.loc of
       //      LOC_REGISTER,LOC_CREGISTER:
       //        a_load_ref_reg(list,location^.size,location^.size,tmpref,location^.register);
       //      LOC_REFERENCE:
       //        begin
       //          reference_reset_base(ref,location^.reference.index,location^.reference.offset,paraloc.alignment);
       //          { doubles in softemu mode have a strange order of registers and references }
       //          if location^.size=OS_32 then
       //            g_concatcopy(list,tmpref,ref,4)
       //          else
       //            begin
       //              g_concatcopy(list,tmpref,ref,sizeleft);
       //              if assigned(location^.next) then
       //                internalerror(2005010710);
       //            end;
       //        end;
       //      LOC_FPUREGISTER,LOC_CFPUREGISTER:
       //        case location^.size of
       //           OS_F32, OS_F64:
       //             a_loadfpu_ref_reg(list,location^.size,location^.size,tmpref,location^.register);
       //           else
       //             internalerror(2002072801);
       //        end;
       //      LOC_VOID:
       //        begin
       //          // nothing to do
       //        end;
       //      else
       //        internalerror(2002081103);
       //    end;
       //    inc(tmpref.offset,tcgsize2size[location^.size]);
       //    dec(sizeleft,tcgsize2size[location^.size]);
       //    location := location^.next;
       //  end;
     end;


    procedure tcgllvm.a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);
     var
       ref: treference;
       tmpreg: tregister;
     begin
       { TODO }
       internalerror(2010081316);
       //paraloc.check_simple_location;
       //paramanager.allocparaloc(list,paraloc.location);
       //case paraloc.location^.loc of
       //  LOC_REGISTER,LOC_CREGISTER:
       //    a_loadaddr_ref_reg(list,r,paraloc.location^.register);
       //  LOC_REFERENCE:
       //    begin
       //      reference_reset(ref,paraloc.alignment);
       //      ref.base := paraloc.location^.reference.index;
       //      ref.offset := paraloc.location^.reference.offset;
       //      tmpreg := getintregister(list,OS_ADDR);
       //      a_loadaddr_ref_reg(list,r,tmpreg);
       //      a_load_reg_ref(list,OS_ADDR,OS_ADDR,tmpreg,ref);
       //    end;
       //  else
       //    internalerror(2002080701);
       //end;
     end;


    procedure tcgllvm.a_call_name(list : TAsmList;const s : string; weak: boolean);
      begin
        { Not possible like this in LLVM, needs type info }
        internalerror(2010081315);
      end;


    procedure tcgllvm.a_call_reg(list : TAsmList;reg: tregister);
      begin
        { Not possible like this in LLVM, needs type info }
        internalerror(2010081313);
      end;


    procedure tcgllvm.a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: aint; reg: TRegister);
      begin
        { not SSA-safe! }
        internalerror(2010081312);
      end;


    procedure tcgllvm.a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister);
      begin
        case op of
          OP_NEG,
          OP_NOT:
            a_op_reg_reg_reg(list,op,size,src,src,dst)
          else
            { not SSA-safe! }
            internalerror(2010081311);
        end;
      end;


    const
      topcg2llvmop: array[topcg] of tllvmop =
       { OP_NONE  OP_MOVE     OP_ADD  OP_AND  OP_DIV   OP_IDIV  OP_IMUL OP_MUL }
        (la_none, la_bitcast, la_add, la_and, la_udiv, la_sdiv, la_mul, la_mul,
       { OP_NEG   OP_NOT   OP_OR  OP_SAR   OP_SHL  OP_SHR   OP_SUB  OP_XOR }
         la_none, la_none, la_or, la_ashr, la_shl, la_lshr, la_sub, la_xor,
       { OP_ROL   OP_ROR }
         la_none, la_none);

    procedure tcgllvm.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister);
      var
        tmpreg: tregister;
      begin
        { default tcg implementation is not SSA-safe }
        tmpreg:=getintregister(list,size);
        a_load_const_reg(list,size,a,tmpreg);
        a_op_reg_reg_reg(list,op,size,tmpreg,src,dst);
      end;


    procedure tcgllvm.a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister);
     var
       orgdst,
       tmpreg1,
       tmpreg2,
       tmpreg3: tregister;
       llsize: tllvmopsize;
       tmpref: treference;
     begin
       llsize:=cgsize2llvmopsize[size];
       orgdst:=dst;
       if isaddressregister(src1) or
          isaddressregister(src2) or
          isaddressregister(dst) then
         begin
           { use getelementptr for address registers if possible, it helps with
             LLVM optimisations }
           if (op=OP_SUB) and
              (isaddressregister(src1)<>isaddressregister(src2)) then
             begin
               { since getelementptr can only add, convert the sub into an add }
               tmpreg1:=getintregister(list,OS_ADDR);
               if isaddressregister(src1) then
                 begin
                   a_op_reg_reg_reg(list,OP_NEG,size,src2,NR_NO,tmpreg1);
                   src2:=tmpreg2;
                 end
               else
                 begin
                   a_op_reg_reg_reg(list,OP_NEG,size,src1,NR_NO,tmpreg1);
                   src1:=tmpreg2;
                 end;
               op:=OP_ADD;
             end;
           if (op=OP_ADD) and
              (isaddressregister(src1)<>isaddressregister(src2)) then
             begin
               reference_reset_base(tmpref,NR_NO,0,1);
               if isaddressregister(src1) then
                 begin
                   tmpref.base:=src1;
                   tmpref.index:=src2;
                 end
               else
                 begin
                   tmpref.base:=src2;
                   tmpref.index:=src1;
                 end;
               if isaddressregister(dst) then
                 tmpreg1:=dst
               else
                 tmpreg1:=getaddressregister(list);
               a_loadaddr_ref_reg(list,tmpref,tmpreg1);
               if tmpreg1<>dst then
                 a_load_reg_reg(list,OS_ADDR,OS_ADDR,tmpreg1,dst);
               exit;
             end
           else
             begin
               { move everything to integer registers }
               if isaddressregister(src1) then
                 begin
                   tmpreg1:=getintregister(list,OS_ADDR);
                   a_load_reg_reg(list,OS_ADDR,OS_ADDR,src1,tmpreg1);
                   src1:=tmpreg1;
                 end;
               if isaddressregister(src2) then
                 begin
                   tmpreg1:=getintregister(list,OS_ADDR);
                   a_load_reg_reg(list,OS_ADDR,OS_ADDR,src2,tmpreg1);
                   src2:=tmpreg1;
                 end;
               if isaddressregister(dst) then
                 begin
                   tmpreg1:=getintregister(list,OS_ADDR);
                   dst:=tmpreg1;
                 end
             end;
         end;
       if topcg2llvmop[op]<>la_none then
         list.concat(taillvm.op_reg_size_reg_reg(topcg2llvmop[op],dst,llsize,src1,src2))
       else
         begin
           case op of
             OP_NEG:
               { %dst = sub size 0, %src1 }
               list.concat(taillvm.op_reg_size_const_reg(la_sub,dst,llsize,0,src1));
             OP_NOT:
               { %dst = xor size -1, %src1 }
               list.concat(taillvm.op_reg_size_const_reg(la_xor,dst,llsize,-1,src1));
             OP_ROL:
               begin
                 tmpreg1:=getintregister(list,size);
                 tmpreg2:=getintregister(list,size);
                 tmpreg3:=getintregister(list,size);
                 { tmpreg1 := tcgsize2size[size] - src1 }
                 list.concat(taillvm.op_reg_size_const_reg(la_sub,tmpreg1,llsize,tcgsize2size[size],src1));
                 { tmpreg2 := src2 shr tmpreg1 }
                 a_op_reg_reg_reg(list,OP_SHR,size,tmpreg1,src2,tmpreg2);
                 { tmpreg3 := src2 shl src1 }
                 a_op_reg_reg_reg(list,OP_SHL,size,src1,src2,tmpreg3);
                 { dst := tmpreg2 or tmpreg3 }
                 a_op_reg_reg_reg(list,OP_OR,size,tmpreg2,tmpreg3,dst);
               end;
             OP_ROR:
               begin
                 tmpreg1:=getintregister(list,size);
                 tmpreg2:=getintregister(list,size);
                 tmpreg3:=getintregister(list,size);
                 { tmpreg1 := tcgsize2size[size] - src1 }
                 list.concat(taillvm.op_reg_size_const_reg(la_sub,tmpreg1,llsize,tcgsize2size[size],src1));
                 { tmpreg2 := src2 shl tmpreg1 }
                 a_op_reg_reg_reg(list,OP_SHL,size,tmpreg1,src2,tmpreg2);
                 { tmpreg3 := src2 shr src1 }
                 a_op_reg_reg_reg(list,OP_SHR,size,src1,src2,tmpreg3);
                 { dst := tmpreg2 or tmpreg3 }
                 a_op_reg_reg_reg(list,OP_OR,size,tmpreg2,tmpreg3,dst);
               end;
             else
               internalerror(2010081310);
           end;
         end;
       if dst<>orgdst then
         a_load_reg_reg(list,OS_ADDR,OS_ADDR,dst,orgdst);
     end;


    procedure tcgllvm.a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister;setflags : boolean;var ovloc : tlocation);
      begin
        { TODO: call intrinsics }
        internalerror(2010081314)
      end;


    procedure tcgllvm.a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation);
     begin
        { TODO: call intrinsics }
        internalerror(2010081315)
     end;


    { returns a reference that either only has the base set, or a symbol }
    function tcgllvm.make_simple_ref(list: TAsmList; const ref: treference): treference;
      var
        tmpref: treference;
        hreg: tregister;
      begin
        { base (if it's present) has to be an address register, index (if it's
          present) an integer }
        if (ref.index<>NR_NO) and
           isaddressregister(ref.index) then
          internalerror(2010081301);
        { base address: either a symbol, a base register, or a direct
          offset }
        if assigned(ref.symbol) and
           (ref.base<>NR_NO) then
          internalerror(2010081302);
        hreg:=NR_NO;
        { the index consists of the index reg (if any) and the offset }
        if (ref.index<>NR_NO) then
          begin
            { if we have both an index register and an offset, add them
              together first }
            if (ref.offset<>0) then
              begin
                hreg:=getintregister(list,OS_ADDR);
                a_op_const_reg_reg(list,OP_ADD,OS_ADDR,ref.offset,ref.index,hreg);
              end
            else
              { assume the index register has always OS_ADDR as size}
              hreg:=ref.index;
          end
        else if (ref.offset<>0) or
           (not assigned(ref.symbol) and
           (ref.base=NR_NO)) then
          begin
            { if we have as symbol or base register, they are the base address;
              if not, use the offset as "base" address (e.g. a null pointer) }
            if assigned(ref.symbol) or
               (ref.base<>NR_NO) then
              hreg:=getintregister(list,OS_ADDR)
            else
              hreg:=getaddressregister(list);
            a_load_const_reg(list,OS_ADDR,ref.offset,hreg);
          end;
        if hreg<>NR_NO then
          begin
            reference_reset_base(result,getaddressregister(list),0,ref.alignment);
            if assigned(ref.symbol) or
               (ref.base<>NR_NO) then
              begin
                { only one of the above conditions is true, checked at the
                  start }
                tmpref:=ref;
                tmpref.index:=NR_NO;
                tmpref.offset:=0;
                list.concat(taillvm.getelementptr_reg_size_ref_size_reg(result.base,SL_I8P,tmpref,cgsize2llvmopsize[OS_ADDR],hreg))
              end
            else
              a_load_reg_reg(list,OS_ADDR,OS_ADDR,hreg,result.base);
          end
        else
          result:=ref;
      end;


    function tcgllvm.make_simple_ref_of_ptrsize(list: TAsmList; const ref: treference; tosize: tcgsize): treference;
      var
        newbase: tregister;
      begin
        result:=make_simple_ref(list,ref);
        { convert to the desired size if <> SL_I8P }
        if not(tosize in [OS_8,OS_S8]) then
          begin
            newbase:=cg.getaddressregister(list);
            list.concat(taillvm.op_reg_size_ref_size(la_bitcast,newbase,SL_I8P,result,cgsize2llvmptropsize[tosize]));
            result.symbol:=nil;
            result.base:=newbase;
          end;
      end;


    procedure tcgllvm.a_load_const_reg(list : TAsmList; size: tcgsize; a : aint;reg : tregister);
      var
        op: tllvmop;
        fromsize,
        tosize: tllvmopsize;
      begin
        fromsize:=cgsize2llvmopsize[size];
        { bitcast only works amongst integers and vectors }
        if not isaddressregister(reg) then
          begin
             op:=la_bitcast;
             tosize:=fromsize;
          end
        else
          begin
            op:=la_inttoptr;
            tosize:=cgsize2llvmptropsize[size];
          end;
        { reg = la_bitcast fromsize a to tosize }
        list.concat(taillvm.op_reg_size_const_size(op,reg,fromsize,a,tosize));
      end;


    procedure tcgllvm.a_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);
     var
        tmpreg: tregister;
        href: treference;
      begin
        href:=make_simple_ref_of_ptrsize(list,ref,tosize);
        if tcgsize2size[fromsize]<>tcgsize2size[tosize] then
          begin
            tmpreg:=getintregister(list,tosize);
            a_load_reg_reg(list,fromsize,tosize,reg,tmpreg);
          end
        else
          tmpreg:=reg;
        { store tosize tmpreg, tosize* href }
        list.concat(taillvm.op_size_reg_size_ref(la_store,cgsize2llvmopsize[tosize],tmpreg,cgsize2llvmptropsize[tosize],href));
      end;


    procedure tcgllvm.a_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;const ref : treference;reg : tregister);
     var
        tmpreg: tregister;
        href: treference;
      begin
        href:=make_simple_ref_of_ptrsize(list,ref,fromsize);
        if tcgsize2size[fromsize]<>tcgsize2size[tosize] then
          tmpreg:=getintregister(list,fromsize)
        else
          tmpreg:=reg;
        { %tmpreg = load size* %ref }
        list.concat(taillvm.op_reg_size_ref(la_load,tmpreg,cgsize2llvmptropsize[fromsize],href));
        if tmpreg<>reg then
          a_load_reg_reg(list,fromsize,tosize,tmpreg,reg);
      end;


    procedure tcgllvm.a_load_reg_reg(list : TAsmList; fromsize, tosize : tcgsize;reg1, reg2 : tregister);
      var
        op: tllvmop;
        fromllsize,
        tollsize: tllvmopsize;
      begin
        if (fromsize=OS_NO) or (tosize=OS_NO) then
          internalerror(2010081001);

        { get llvm fromsize/tosize }
        if not isaddressregister(reg1) then
          fromllsize:=cgsize2llvmopsize[fromsize]
        else
          fromllsize:=cgsize2llvmptropsize[fromsize];
        if not isaddressregister(reg2) then
          tollsize:=cgsize2llvmopsize[tosize]
        else
          tollsize:=cgsize2llvmptropsize[tosize];
        { int to pointer or vice versa }
        if isaddressregister(reg1) and
           not isaddressregister(reg2) then
          op:=la_ptrtoint
        else if not isaddressregister(reg1) and
           isaddressregister(reg2) then
          op:=la_inttoptr
        { int to int or ptr to ptr: need zero/sign extension, or plain bitcast? }
        else if tcgsize2size[tosize]<>tcgsize2size[fromsize] then
          begin
            if tcgsize2size[tosize]<tcgsize2size[fromsize] then
              op:=la_trunc
            else if tcgsize2unsigned[fromsize]<>fromsize then
              { fromsize is signed -> sign extension }
              op:=la_sext
            else
              op:=la_zext;
          end
        else
          op:=la_bitcast;
        { reg2 = bitcast fromllsize reg1 to tollsize }
        list.concat(taillvm.op_reg_size_reg_size(op,reg2,fromllsize,reg1,tollsize));
      end;


    procedure tcgllvm.a_loadfpu_ref_cgpara(list : TAsmList;size : tcgsize;const ref : treference;const paraloc : TCGPara);
     //var
     //   href,href2 : treference;
     //   hloc : pcgparalocation;
     begin
       { TODO }
       internalerror(2010081323);
       //href:=ref;
       //hloc:=paraloc.location;
       //while assigned(hloc) do
       //  begin
       //    case hloc^.loc of
       //      LOC_FPUREGISTER,LOC_CFPUREGISTER:
       //        begin
       //          paramanager.allocparaloc(list,paraloc.location);
       //          a_loadfpu_ref_reg(list,size,size,ref,hloc^.register);
       //        end;
       //      LOC_REGISTER :
       //        case hloc^.size of
       //          OS_32,
       //          OS_F32:
       //            begin
       //              paramanager.allocparaloc(list,paraloc.location);
       //              a_load_ref_reg(list,OS_32,OS_32,href,hloc^.register);
       //            end;
       //          OS_64,
       //          OS_F64:
       //            cg64.a_load64_ref_cgpara(list,href,paraloc);
       //          else
       //            a_load_ref_reg(list,hloc^.size,hloc^.size,href,hloc^.register);
       //        end;
       //      LOC_REFERENCE :
       //        begin
       //          reference_reset_base(href2,hloc^.reference.index,hloc^.reference.offset,paraloc.alignment);
       //          { concatcopy should choose the best way to copy the data }
       //          g_concatcopy(list,href,href2,tcgsize2size[hloc^.size]);
       //        end;
       //      else
       //        internalerror(200408241);
       //   end;
       //   inc(href.offset,tcgsize2size[hloc^.size]);
       //   hloc:=hloc^.next;
       // end;
     end;


    procedure tcgllvm.a_loadfpu_reg_reg(list: TAsmList; fromsize,tosize: tcgsize; reg1, reg2: tregister);
      var
        op: tllvmop;
        fromllsize,
        tollsize: tllvmopsize;
      begin
        if (fromsize=OS_NO) or (tosize=OS_NO) then
          internalerror(2010081309);

        { get llvm fromsize/tosize }
        fromllsize:=cgsize2llvmopsize[fromsize];
        tollsize:=cgsize2llvmopsize[tosize];
        if fromllsize<tollsize then
          op:=la_fptrunc
         else if fromllsize>tollsize then
          op:=la_fpext
        else
          op:=la_bitcast;
        { reg2 = bitcast fromllsize reg1 to tollsize }
        list.concat(taillvm.op_reg_size_reg_size(op,reg2,fromllsize,reg1,tollsize));
      end;


    procedure tcgllvm.a_loadfpu_ref_reg(list: TAsmList; fromsize,tosize: tcgsize; const ref: treference; reg: tregister);
     var
        tmpreg: tregister;
        href: treference;
      begin
        href:=make_simple_ref_of_ptrsize(list,ref,fromsize);
        if fromsize<>tosize then
          tmpreg:=getfpuregister(list,fromsize)
        else
          tmpreg:=reg;
        { %tmpreg = load size* %ref }
        list.concat(taillvm.op_reg_size_ref(la_load,tmpreg,cgsize2llvmptropsize[fromsize],href));
        if tmpreg<>reg then
          a_loadfpu_reg_reg(list,fromsize,tosize,tmpreg,reg);
      end;


    procedure tcgllvm.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference);
     var
        tmpreg: tregister;
        href: treference;
      begin
        href:=make_simple_ref_of_ptrsize(list,ref,tosize);
        if tcgsize2size[fromsize]<>tcgsize2size[tosize] then
          begin
            tmpreg:=getfpuregister(list,tosize);
            a_loadfpu_reg_reg(list,fromsize,tosize,reg,tmpreg);
          end
        else
          tmpreg:=reg;
        { store tosize tmpreg, tosize* href }
        list.concat(taillvm.op_size_reg_size_ref(la_store,cgsize2llvmopsize[tosize],tmpreg,cgsize2llvmptropsize[tosize],href));
      end;


    {  comparison operations }
    procedure tcgllvm.a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : aint;reg : tregister;l : tasmlabel);
     var
       tmpreg : tregister;
     begin
       if not isaddressregister(reg) then
         tmpreg:=getintregister(list,size)
       else
         tmpreg:=getaddressregister(list);
       a_load_const_reg(list,size,a,tmpreg);
       a_cmp_reg_reg_label(list,size,cmp_op,tmpreg,reg,l);
     end;


    procedure tcgllvm.a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel);
      var
        resreg,
        tmpreg: tregister;
        cmpsize: tllvmopsize;
      begin
        if isaddressregister(reg1)<>isaddressregister(reg2) then
          begin
            tmpreg:=getaddressregister(list);
            if not isaddressregister(reg1) then
              begin
                a_load_reg_reg(list,size,size,reg1,tmpreg);
                reg1:=tmpreg;
              end
            else
              begin
                a_load_reg_reg(list,size,size,reg2,tmpreg);
                reg2:=tmpreg;
              end;
          end;
        if isaddressregister(reg1) then
          begin
            if size<>OS_ADDR then
              internalerror(2010081308);
            cmpsize:=SL_I8P;
          end
        else
          cmpsize:=cgsize2llvmopsize[size];
        resreg:=getflagsregister(list);
        list.concat(taillvm.op_reg_cond_size_reg_reg(la_icmp,resreg,cmp_op,cmpsize,reg1,reg2));
{$ifdef using_llvm_tresflags}
        a_jmp_flags(list,resreg,l);
{$endif}
      end;


    procedure tcgllvm.a_jmp_name(list : TAsmList;const s : string);
     begin
       { it's not possible to jump between different functions in llvm }
       internalerror(2010081307);
     end;


    procedure tcgllvm.a_jmp_always(list : TAsmList;l: tasmlabel);
     var
       ai : taillvm;
     begin
       ai:=taillvm.op_lab(la_br,l);
       ai.is_jmp:=true;
       list.concat(ai);
     end;

{$ifdef using_llvm_tresflags}
    procedure tcgllvm.a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel);
     var
       ai : taicpu;
       lab: tasmlabel;
     begin
       current_asmdata.getjumplabel(lab);
       ai:=taillvm.op_size_reg_lab_lab(la_br,SL_I1,f,l,lab);
       ai.is_jmp:=true;
       list.concat(ai);
       a_label(list,lab);
     end;


    procedure tcgllvm.g_flags2reg(list: TAsmList; size: TCgSize; const f: TResFlags; reg: TRegister);
      var
        op: tllvmop;
      begin
        if not isaddressregister(reg) then
          op:=la_zext
        else
          op:=la_inttoptr;
        list.concat(taillvm.op_reg_size_reg_size(op,reg,SL_I1,f,cgsize2llvmopsize[size]));
      end;
{$endif using_llvm_tresflags}

    procedure tcgllvm.g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);
      begin
        { nothing to do }
      end;


    procedure tcgllvm.g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean);
      begin
        if is_void(current_procinfo.procdef.returndef) or
           (
            (po_assembler in current_procinfo.procdef.procoptions) and
            (not(assigned(current_procinfo.procdef.funcretsym)) or
             (tabstractvarsym(current_procinfo.procdef.funcretsym).refs=0))
           ) or
           paramanager.ret_in_param(current_procinfo.procdef.returndef,current_procinfo.procdef.proccalloption) then
          list.concat(taillvm.op_none(la_ret))
        else
          begin
            { TODO: in case of a simple result, return location^.register,
                otherwise define an undef and use insertvalue to insert the
                fields
            list.concat(taillvm.op_def_reg(la_ret,current_procinfo.procdef.returndef,current_procinfo.procdef.funcretloc[calleeside].location^.register)); }
            internalerror(2010081401);
          end;
      end;


    procedure tcgllvm.a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);
      var
        tmpref: treference;
        hreg: tregister;
      begin
        if not isaddressregister(r) then
          internalerror(2010081503);
        tmpref:=make_simple_ref(list,ref);
        list.concat(taillvm.op_reg_size_ref_size(la_bitcast,r,SL_I8P,tmpref,SL_I8P));
      end;


    procedure tcgllvm.g_concatcopy(list : TAsmList;const source,dest : treference;len : aint);
      var
        srcref,dstref: treference;
        tmpreg : tregister;
        copyalignment: longint;
        copysize: tcgsize;

      procedure create_loop(iterations: aint; copyalignment: longint);
        var
          tmpbasereg,
          newbasereg: tregister;
          lab: tasmlabel;
          loopcntref,tmpref: treference;
        begin
          if iterations=0 then
            exit;

          if iterations<>1 then
            begin
              { init loop counter }
              tg.gettemp(list,sizeof(pint),sizeof(pint),tt_normal,loopcntref);
              a_load_const_ref(list,OS_ADDR,iterations,loopcntref);

              { loop label }
              current_asmdata.getjumplabel(lab);
              a_label(list,lab);
            end;

          { load source value: first load source address pointer  }
          tmpbasereg:=getaddressregister(list);
          a_load_ref_reg(list,OS_ADDR,OS_ADDR,srcref,tmpbasereg);
          reference_reset_base(tmpref,tmpbasereg,0,copyalignment);
          { and then the value }
          tmpreg:=getintregister(list,copysize);
          a_load_ref_reg(list,copysize,copysize,tmpref,tmpreg);
          if iterations<>1 then
            begin
              { update the source reference }
              newbasereg:=getaddressregister(list);
              a_op_const_reg_reg(list,OP_ADD,OS_ADDR,copyalignment,tmpbasereg,newbasereg);
              a_load_reg_ref(list,OS_ADDR,OS_ADDR,newbasereg,srcref);
            end;
          { store the loaded value: first load the dest address pointer }
          tmpbasereg:=getaddressregister(list);
          a_load_ref_reg(list,OS_ADDR,OS_ADDR,dstref,tmpbasereg);
          reference_reset_base(tmpref,tmpbasereg,0,copyalignment);
          { and store the value }
          a_load_reg_ref(list,copysize,copysize,tmpreg,tmpref);

          if iterations<>1 then
            begin
              { update the dest reference }
              newbasereg:=getaddressregister(list);
              a_op_const_reg_reg(list,OP_ADD,OS_ADDR,copyalignment,tmpbasereg,newbasereg);
              a_load_reg_ref(list,OS_ADDR,OS_ADDR,newbasereg,dstref);

              { decrease the loop counter }
              a_op_const_ref(list,OP_SUB,OS_ADDR,1,loopcntref);

              { loop }
              a_cmp_const_ref_label(list,OS_ADDR,OC_NE,0,loopcntref,lab);
            end;
        end;

      begin { g_concatcopy }
        if len=0 then
          exit;
        { determine maximum common copyalignment }
        copyalignment:=min(source.alignment,dest.alignment);
        { limit to 128 bits, since we don't support > 128 bit loads/stores }
        if copyalignment>16 then
          copyalignment:=16;
        { don't load more per iteration than the total length }
        while copyalignment>len do
          copyalignment:=copyalignment div 2;
        case copyalignment of
          1  : copysize:=OS_8;
          2  : copysize:=OS_16;
          4  : copysize:=OS_32;
          8  : copysize:=OS_64;
          16 : copysize:=OS_128;
        else
          internalerror(2010081304);
        end;
        { keep reference addresses in memory so we don't have to insert phi
          nodes; llvm will lower everything to registers, or replace it with
          memcpy }
        tmpreg:=getaddressregister(list);
        a_loadaddr_ref_reg(list,source,tmpreg);
        tg.gettemp(list,sizeof(pint),sizeof(pint),tt_normal,srcref);
        a_load_reg_ref(list,OS_ADDR,OS_ADDR,tmpreg,srcref);

        tmpreg:=getaddressregister(list);
        a_loadaddr_ref_reg(list,dest,tmpreg);
        tg.gettemp(list,sizeof(pint),sizeof(pint),tt_normal,dstref);
        a_load_reg_ref(list,OS_ADDR,OS_ADDR,tmpreg,dstref);

        { main loop (separately because alignment is propagated to LLVM) }
        create_loop(len div copyalignment,copyalignment);

        { leftovers }
        create_loop(len mod copyalignment,1);
      end;


    procedure tcgllvm.g_overflowCheck(list : TAsmList;const l : tlocation;def : tdef);
      begin
        { overflow checking cannot be performed like that with LLVM }
        internalerror(2010081322);
      end;


    procedure tcgllvm.g_overflowCheck_loc(List:TAsmList;const Loc:TLocation;def:TDef;ovloc : tlocation);
     var
       hl : tasmlabel;
       ai:TAiCpu;
       hflags : tresflags;
     begin
       if not(cs_check_overflow in current_settings.localswitches) then
         exit;
       current_asmdata.getjumplabel(hl);
       case ovloc.loc of
         LOC_FLAGS:
           cg.a_jmp_flags(list,ovloc.resflags,hl);
         else
           internalerror(2010081321);
       end;

       a_call_name(list,'FPC_OVERFLOW',false);
       a_label(list,hl);
     end;


    procedure tcgllvm.g_save_registers(list : TAsmList);
      begin
        { LLVM does that for us }
      end;


    procedure tcgllvm.g_restore_registers(list : TAsmList);
      begin
        { LLVM does that for us }
      end;


    procedure tcgllvm.g_stackpointer_alloc(list: TAsmList; size: longint);
      begin
       internalerror(2010081319);
      end;


    procedure tcgllvm.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
      begin
       { TODO }
       internalerror(2010081320);
       //if not(procdef.proctypeoption in [potype_function,potype_procedure]) then
       //  Internalerror(200006137);
       //if not assigned(procdef._class) or
       //   (procdef.procoptions*[po_classmethod, po_staticmethod,
       //     po_methodpointer, po_interrupt, po_iocheck]<>[]) then
       //  Internalerror(200006138);
       //if procdef.owner.symtabletype<>ObjectSymtable then
       //  Internalerror(200109191);
       //
       //make_global:=false;
       //if (not current_module.is_unit) or
       //   create_smartlink or
       //   (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
       //  make_global:=true;
       //
       //if make_global then
       //  list.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
       //else
       //  list.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));
       //
       //{ the wrapper might need aktlocaldata for the additional data to
       //  load the constant }
       //current_procinfo:=cprocinfo.create(nil);
       //
       //{ set param1 interface to self  }
       //g_adjust_self_value(list,procdef,ioffset);
       //
       //{ case 4 }
       //if po_virtualmethod in procdef.procoptions then
       //  begin
       //    loadvmttor12;
       //    op_onr12methodaddr;
       //  end
       //{ case 0 }
       //else
       //  list.concat(taicpu.op_sym(A_B,current_asmdata.RefAsmSymbol(procdef.mangledname)));
       //list.concatlist(current_procinfo.aktlocaldata);
       //
       //current_procinfo.Free;
       //current_procinfo:=nil;
       //
       //list.concat(Tai_symbol_end.Createname(labelname));
     end;


    procedure create_codegen;
      begin
        cg:=tcgllvm.create;
     end;

end.
