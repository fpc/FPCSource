{

    Copyright (c) 2008 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generator for the AVR

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
       cpubase,cpuinfo,node,cg64f32,rgcpu;


    type

      { tcgavr }

      tcgavr = class(tcg)
        { true, if the next arithmetic operation should modify the flags }
        cgsetflags : boolean;
        procedure init_register_allocators;override;
        procedure done_register_allocators;override;

        function getintregister(list:TAsmList;size:Tcgsize):Tregister;override;

        procedure a_load_const_cgpara(list : TAsmList;size : tcgsize;a : aint;const paraloc : TCGPara);override;
        procedure a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);override;
        procedure a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);override;

        procedure a_call_name(list : TAsmList;const s : string; weak: boolean);override;
        procedure a_call_reg(list : TAsmList;reg: tregister);override;
        procedure a_call_ref(list : TAsmList;ref: treference);override;

        procedure a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: aint; reg: TRegister); override;
        procedure a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;

        { move instructions }
        procedure a_load_const_reg(list : TAsmList; size: tcgsize; a : aint;reg : tregister);override;
        procedure a_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);override;
        procedure a_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister);override;
        procedure a_load_reg_reg(list : TAsmList; fromsize, tosize : tcgsize;reg1,reg2 : tregister);override;

        {  comparison operations }
        procedure a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : aint;reg : tregister;
          l : tasmlabel);override;
        procedure a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;

        procedure a_jmp_name(list : TAsmList;const s : string); override;
        procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;
        procedure a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel); override;

        procedure g_flags2reg(list: TAsmList; size: TCgSize; const f: TResFlags; reg: TRegister); override;

        procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);override;
        procedure g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean); override;

        procedure a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);override;

        procedure g_concatcopy(list : TAsmList;const source,dest : treference;len : aint);override;
        procedure g_concatcopy_unaligned(list : TAsmList;const source,dest : treference;len : aint);override;
        procedure g_concatcopy_move(list : TAsmList;const source,dest : treference;len : aint);
        procedure g_concatcopy_internal(list : TAsmList;const source,dest : treference;len : aint;aligned : boolean);

        procedure g_overflowcheck(list: TAsmList; const l: tlocation; def: tdef); override;

//        procedure g_save_registers(list : TAsmList);override;
//        procedure g_restore_registers(list : TAsmList);override;

        procedure a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
        procedure fixref(list : TAsmList;var ref : treference);
        function normalize_ref(list:TAsmList;ref: treference):treference;

        procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
        procedure emit_mov(list: TAsmList;reg2: tregister; reg1: tregister);

        procedure a_adjust_sp(list: TAsmList; value: longint);
      end;

      tcg64favr = class(tcg64f32)
        procedure a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);override;
        procedure a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);override;
        procedure a_op64_const_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64);override;
        procedure a_op64_reg_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64);override;
        procedure a_op64_const_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64;setflags : boolean;var ovloc : tlocation);override;
        procedure a_op64_reg_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64;setflags : boolean;var ovloc : tlocation);override;
      end;

    procedure create_codegen;

    const
      TOpCG2AsmOp: Array[topcg] of TAsmOp = (A_NONE,A_MOV,A_ADD,A_AND,A_NONE,
                            A_NONE,A_MUL,A_MULS,A_NEG,A_COM,A_OR,
                            A_ASR,A_LSL,A_LSR,A_SUB,A_EOR,A_ROL,A_ROR);
  implementation

    uses
       globals,verbose,systems,cutils,
       fmodule,
       symconst,symsym,
       tgobj,
       procinfo,cpupi,
       paramgr;


    procedure tcgavr.init_register_allocators;
      begin
        inherited init_register_allocators;
        rg[R_INTREGISTER]:=trgintcpu.create(R_INTREGISTER,R_SUBWHOLE,
            [RS_R0,RS_R2,RS_R3,RS_R4,RS_R5,RS_R6,RS_R7,RS_R8,RS_R9,
             RS_R10,RS_R11,RS_R12,RS_R13,RS_R14,RS_R15,RS_R16,RS_R17,RS_R18,RS_R19,
             RS_R20,RS_R21,RS_R22,RS_R23,RS_R24,RS_R25],first_int_imreg,[]);
        rg[R_ADDRESSREGISTER]:=trgintcpu.create(R_ADDRESSREGISTER,R_SUBWHOLE,
            [RS_R26,RS_R30],first_int_imreg,[]);
      end;


    procedure tcgavr.done_register_allocators;
      begin
        rg[R_INTREGISTER].free;
        rg[R_ADDRESSREGISTER].free;
        inherited done_register_allocators;
      end;


    function tcgavr.getintregister(list: TAsmList; size: Tcgsize): Tregister;
      var
        tmp1,tmp2,tmp3 : TRegister;
      begin
        case size of
          OS_8,OS_S8:
            Result:=inherited getintregister(list, size);
          OS_16,OS_S16:
            begin
              Result:=inherited getintregister(list, OS_8);
              { ensure that the high register can be retrieved by
                GetNextReg
              }
              if inherited getintregister(list, OS_8)<>GetNextReg(Result) then
                internalerror(2011021331);
            end;
          OS_32,OS_S32:
            begin
              Result:=inherited getintregister(list, OS_8);
              tmp1:=inherited getintregister(list, OS_8);
              { ensure that the high register can be retrieved by
                GetNextReg
              }
              if tmp1<>GetNextReg(Result) then
                internalerror(2011021332);
              tmp2:=inherited getintregister(list, OS_8);
              { ensure that the upper register can be retrieved by
                GetNextReg
              }
              if tmp2<>GetNextReg(tmp1) then
                internalerror(2011021333);
              tmp3:=inherited getintregister(list, OS_8);
              { ensure that the upper register can be retrieved by
                GetNextReg
              }
              if tmp3<>GetNextReg(tmp2) then
                internalerror(2011021334);
            end;
          else
            internalerror(2011021330);
        end;
      end;


    procedure tcgavr.a_load_const_cgpara(list : TAsmList;size : tcgsize;a : aint;const paraloc : TCGPara);
      var
        ref: treference;
      begin
        paraloc.check_simple_location;
        paramanager.allocparaloc(list,paraloc.location);
        case paraloc.location^.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_load_const_reg(list,size,a,paraloc.location^.register);
          LOC_REFERENCE:
            begin
               reference_reset(ref,paraloc.alignment);
               ref.base:=paraloc.location^.reference.index;
               ref.offset:=paraloc.location^.reference.offset;
               a_load_const_ref(list,size,a,ref);
            end;
          else
            internalerror(2002081101);
        end;
      end;


    procedure tcgavr.a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);
      var
        tmpref, ref: treference;
        location: pcgparalocation;
        sizeleft: aint;
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


    procedure tcgavr.a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);
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


    procedure tcgavr.a_call_name(list : TAsmList;const s : string; weak: boolean);
      begin
        list.concat(taicpu.op_sym(A_RCALL,current_asmdata.RefAsmSymbol(s)));
{
        the compiler does not properly set this flag anymore in pass 1, and
        for now we only need it after pass 2 (I hope) (JM)
          if not(pi_do_call in current_procinfo.flags) then
            internalerror(2003060703);
}
        include(current_procinfo.flags,pi_do_call);
      end;


    procedure tcgavr.a_call_reg(list : TAsmList;reg: tregister);
      begin
        a_reg_alloc(list,NR_ZLO);
        a_reg_alloc(list,NR_ZHI);
        list.concat(taicpu.op_reg_reg(A_MOV,NR_ZLO,reg));
        list.concat(taicpu.op_reg_reg(A_MOV,NR_ZHI,GetHigh(reg)));
        list.concat(taicpu.op_none(A_ICALL));
        a_reg_dealloc(list,NR_ZLO);
        a_reg_dealloc(list,NR_ZHI);

        include(current_procinfo.flags,pi_do_call);
      end;


    procedure tcgavr.a_call_ref(list : TAsmList;ref: treference);
      begin
        a_reg_alloc(list,NR_ZLO);
        a_reg_alloc(list,NR_ZHI);
        a_load_ref_reg(list,OS_ADDR,OS_ADDR,ref,NR_ZLO);
        list.concat(taicpu.op_none(A_ICALL));
        a_reg_dealloc(list,NR_ZLO);
        a_reg_dealloc(list,NR_ZHI);

        include(current_procinfo.flags,pi_do_call);
      end;


     procedure tcgavr.a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: aint; reg: TRegister);
       var
         mask : qword;
         shift : byte;
         i : byte;
         tmpreg : tregister;
       begin
         mask:=$ff;
         shift:=0;
         case op of
           OP_OR:
             begin
               for i:=1 to tcgsize2size[size] do
                 begin
                   list.concat(taicpu.op_reg_const(A_ORI,reg,(a and mask) shr shift));
                   reg:=GetNextReg(reg);
                   mask:=mask shl 8;
                   inc(shift,8);
                 end;
             end;
           OP_AND:
             begin
               for i:=1 to tcgsize2size[size] do
                 begin
                   list.concat(taicpu.op_reg_const(A_ANDI,reg,(a and mask) shr shift));
                   reg:=GetNextReg(reg);
                   mask:=mask shl 8;
                   inc(shift,8);
                 end;
             end;
           OP_SUB:
             begin
               list.concat(taicpu.op_reg_const(A_SUBI,reg,a));
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       reg:=GetNextReg(reg);
                       mask:=mask shl 8;
                       inc(shift,8);
                       list.concat(taicpu.op_reg_const(A_SBCI,reg,(a and mask) shr shift));
                     end;
                 end;
             end;
         else
           begin
             tmpreg:=getintregister(list,size);
             a_load_const_reg(list,size,a,tmpreg);
             a_op_reg_reg(list,op,size,tmpreg,reg);
           end;
         end;
       end;


     procedure tcgavr.a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister);
       var
         tmpreg: tregister;
         i : integer;
         instr : taicpu;
      begin
         case op of
           OP_ADD:
             begin
               if src<>dst then
                 a_load_reg_reg(list,size,size,src,dst);

               list.concat(taicpu.op_reg_reg(A_ADD,dst,src));
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       dst:=GetNextReg(dst);
                       src:=GetNextReg(src);
                       list.concat(taicpu.op_reg_reg(A_ADC,dst,src));
                   end;
                 end
               else
             end;

           OP_SUB:
             begin
               if src<>dst then
                 a_load_reg_reg(list,size,size,src,dst);

               list.concat(taicpu.op_reg_reg(A_SUB,dst,src));
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       dst:=GetNextReg(dst);
                       src:=GetNextReg(src);
                       list.concat(taicpu.op_reg_reg(A_SBC,dst,src));
                     end;
                 end;
             end;

           OP_NEG:
             begin
               if src<>dst then
                 a_load_reg_reg(list,size,size,src,dst);

               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   tmpreg:=GetNextReg(dst);
                   for i:=2 to tcgsize2size[size] do
                     begin
                       list.concat(taicpu.op_reg(A_COM,tmpreg));
                       tmpreg:=GetNextReg(tmpreg);
                     end;
                   list.concat(taicpu.op_reg(A_NEG,dst));
                   tmpreg:=GetNextReg(dst);
                   for i:=2 to tcgsize2size[size] do
                     begin
                       list.concat(taicpu.op_const_reg(A_SBCI,-1,dst));
                       tmpreg:=GetNextReg(tmpreg);
                   end;
                 end
               else
                 list.concat(taicpu.op_reg(A_NEG,dst));
             end;

           OP_NOT:
             begin
               for i:=1 to tcgsize2size[size] do
                 begin
                   if src<>dst then
                     a_load_reg_reg(list,OS_8,OS_8,src,dst);
                   list.concat(taicpu.op_reg(A_COM,dst));
                   src:=GetNextReg(src);
                   dst:=GetNextReg(dst);
                 end;
             end;

           OP_MUL,OP_IMUL:
             begin
               if size in [OS_8,OS_S8] then
                 list.concat(taicpu.op_reg_reg(topcg2asmop[op],dst,src))
               else
                 internalerror(2011022002);
             end;

           OP_DIV,OP_IDIV:
             { special stuff, needs separate handling inside code }
             { generator                                          }
             internalerror(2011022001);

           OP_SHR,OP_SHL,OP_SAR,OP_ROL,OP_ROR:
             begin
               {!!!!!!!}
             end;

           OP_AND,OP_OR,OP_XOR:
             begin
               if src<>dst then
                 a_load_reg_reg(list,size,size,src,dst);

                for i:=1 to tcgsize2size[size] do
                  begin
                    list.concat(taicpu.op_reg_reg(topcg2asmop[op],dst,src));
                    dst:=GetNextReg(dst);
                    src:=GetNextReg(src);
                  end;
             end;
          else
             internalerror(2011022004);
         end;
       end;


     procedure tcgavr.a_load_const_reg(list : TAsmList; size: tcgsize; a : aint;reg : tregister);
       var
         mask : qword;
         shift : byte;
         i : byte;
       begin
         mask:=$ff;
         shift:=0;
         for i:=1 to tcgsize2size[size] do
           begin
             list.concat(taicpu.op_reg_const(A_LDI,reg,(qword(a) and mask) shr shift));
             mask:=mask shl 8;
             inc(shift,8);
             reg:=GetNextReg(reg);
           end;
       end;


    function tcgavr.normalize_ref(list:TAsmList;ref: treference):treference;
      var
        tmpreg : tregister;
        tmpref : treference;
        l : tasmlabel;
      begin
        tmpreg:=NR_NO;

        Result:=ref;

         if ref.addressmode<>AM_UNCHANGED then
           internalerror(2011021701);

        { Be sure to have a base register }
        if (ref.base=NR_NO) then
          begin
            { only symbol+offset? }
            if ref.index=NR_NO then
              exit;
            ref.base:=ref.index;
            ref.index:=NR_NO;
          end;
        if assigned(ref.symbol) or (ref.offset<>0) then
          begin
            tmpreg:=getaddressregister(list);
            reference_reset(tmpref,0);
            tmpref.symbol:=ref.symbol;
            tmpref.offset:=lo(word(ref.offset));
            tmpref.refaddr:=addr_lo8;
            list.concat(taicpu.op_reg_ref(A_LDI,tmpreg,tmpref));
            tmpref.offset:=hi(word(ref.offset));
            tmpref.refaddr:=addr_hi8;
            list.concat(taicpu.op_reg_ref(A_LDI,GetNextReg(tmpreg),tmpref));
            if (ref.base<>NR_NO) then
              begin
                list.concat(taicpu.op_reg_reg(A_ADD,tmpreg,ref.base));
                list.concat(taicpu.op_reg_reg(A_ADC,GetNextReg(tmpreg),GetNextReg(ref.base)));
              end;
            if (ref.index<>NR_NO) then
              begin
                list.concat(taicpu.op_reg_reg(A_ADD,tmpreg,ref.base));
                list.concat(taicpu.op_reg_reg(A_ADC,GetNextReg(tmpreg),GetNextReg(ref.base)));
              end;
            ref.base:=tmpreg;
            ref.index:=NR_NO;
          end
        else if (ref.base<>NR_NO) and (ref.index<>NR_NO) then
          begin
            tmpreg:=getaddressregister(list);
            list.concat(taicpu.op_reg_reg(A_MOVW,tmpreg,ref.index));
            list.concat(taicpu.op_reg_reg(A_ADD,tmpreg,ref.base));
            list.concat(taicpu.op_reg_reg(A_ADC,GetNextReg(tmpreg),GetNextReg(ref.base)));
            ref.base:=tmpreg;
            ref.index:=NR_NO;
          end;
        Result:=ref;
      end;


     procedure tcgavr.a_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);
       var
         href : treference;
         conv_done: boolean;
         tmpreg : tregister;
         i : integer;
       begin
         href:=normalize_ref(list,Ref);
         if (tcgsize2size[fromsize]>32) or (tcgsize2size[tosize]>32) or (fromsize=OS_NO) or (tosize=OS_NO) then
           internalerror(2011021307);

         conv_done:=false;
         if tosize<>fromsize then
           begin
             conv_done:=true;
             if tcgsize2size[tosize]<=tcgsize2size[fromsize] then
               fromsize:=tosize;
             case fromsize of
               OS_8:
                 begin
                   if (href.base<>NR_NO) and (tcgsize2size[tosize]>1) then
                     href.addressmode:=AM_POSTINCREMENT;

                   list.concat(taicpu.op_ref_reg(A_ST,href,reg));
                   for i:=2 to tcgsize2size[tosize] do
                     begin
                       if (href.offset<>0) or assigned(href.symbol) then
                         inc(href.offset);

                       if (href.base<>NR_NO) and (i<tcgsize2size[fromsize]) then
                         href.addressmode:=AM_POSTINCREMENT
                       else
                         href.addressmode:=AM_UNCHANGED;

                       list.concat(taicpu.op_ref_reg(A_ST,href,NR_R1));
                     end;
                 end;
               OS_S8:
                 begin
                   if (href.base<>NR_NO) and (tcgsize2size[tosize]>1) then
                     href.addressmode:=AM_POSTINCREMENT;
                   list.concat(taicpu.op_ref_reg(A_ST,href,reg));

                   if tcgsize2size[tosize]>1 then
                     begin
                       tmpreg:=getintregister(list,OS_8);
                       list.concat(taicpu.op_reg(A_CLR,tmpreg));
                       list.concat(taicpu.op_reg_const(A_SBIC,reg,7));
                       list.concat(taicpu.op_reg(A_COM,tmpreg));
                       for i:=2 to tcgsize2size[tosize] do
                         begin
                           if (href.offset<>0) or assigned(href.symbol) then
                             inc(href.offset);

                           if (href.base<>NR_NO) and (i<tcgsize2size[fromsize]) then
                             href.addressmode:=AM_POSTINCREMENT
                           else
                             href.addressmode:=AM_UNCHANGED;
                           list.concat(taicpu.op_ref_reg(A_ST,href,tmpreg));
                         end;
                     end;
                 end;
               OS_16:
                 begin
                   if (href.base<>NR_NO) and (tcgsize2size[tosize]>1) then
                     href.addressmode:=AM_POSTINCREMENT;

                   list.concat(taicpu.op_ref_reg(A_ST,href,reg));
                   if (href.offset<>0) or assigned(href.symbol) then
                     inc(href.offset)
                   else if (href.base<>NR_NO) and (tcgsize2size[fromsize]>2) then
                     href.addressmode:=AM_POSTINCREMENT
                   else
                     href.addressmode:=AM_UNCHANGED;

                   reg:=GetNextReg(reg);
                   list.concat(taicpu.op_ref_reg(A_ST,href,reg));

                   for i:=3 to tcgsize2size[tosize] do
                     begin
                       if (href.offset<>0) or assigned(href.symbol) then
                         inc(href.offset);

                       if (href.base<>NR_NO) and (i<tcgsize2size[fromsize]) then
                         href.addressmode:=AM_POSTINCREMENT
                       else
                         href.addressmode:=AM_UNCHANGED;

                       list.concat(taicpu.op_ref_reg(A_ST,href,NR_R1));
                     end;
                 end;
               OS_S16:
                 begin
                   if (href.base<>NR_NO) and (tcgsize2size[tosize]>1) then
                     href.addressmode:=AM_POSTINCREMENT;

                   list.concat(taicpu.op_ref_reg(A_ST,href,reg));
                   if (href.offset<>0) or assigned(href.symbol) then
                     inc(href.offset)
                   else if (href.base<>NR_NO) and (tcgsize2size[fromsize]>2) then
                     href.addressmode:=AM_POSTINCREMENT
                   else
                     href.addressmode:=AM_UNCHANGED;

                   reg:=GetNextReg(reg);
                   list.concat(taicpu.op_ref_reg(A_ST,href,reg));

                   if tcgsize2size[tosize]>2 then
                     begin
                       tmpreg:=getintregister(list,OS_8);
                       list.concat(taicpu.op_reg(A_CLR,tmpreg));
                       list.concat(taicpu.op_reg_const(A_SBIC,reg,7));
                       list.concat(taicpu.op_reg(A_COM,tmpreg));
                       for i:=3 to tcgsize2size[tosize] do
                         begin
                           if (href.offset<>0) or assigned(href.symbol) then
                             inc(href.offset);

                           if (href.base<>NR_NO) and (i<tcgsize2size[fromsize]) then
                             href.addressmode:=AM_POSTINCREMENT
                           else
                             href.addressmode:=AM_UNCHANGED;
                           list.concat(taicpu.op_ref_reg(A_ST,href,tmpreg));
                         end;
                     end;
                 end;
               else
                 conv_done:=false;
             end;
           end;
         if not conv_done then
           begin
             for i:=1 to tcgsize2size[fromsize] do
               begin
                   if (href.base<>NR_NO) and (i<tcgsize2size[fromsize]) then
                     href.addressmode:=AM_POSTINCREMENT
                   else
                     href.addressmode:=AM_UNCHANGED;

                 list.concat(taicpu.op_ref_reg(A_ST,href,reg));

                 if (href.offset<>0) or assigned(href.symbol) then
                   inc(href.offset);

                 reg:=GetNextReg(reg);
               end;
           end;
       end;


     procedure tcgavr.a_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;
       const Ref : treference;reg : tregister);
       var
         href : treference;
         conv_done: boolean;
         tmpreg : tregister;
         i : integer;
       begin
         href:=normalize_ref(list,Ref);
         if (tcgsize2size[fromsize]>32) or (tcgsize2size[tosize]>32) or (fromsize=OS_NO) or (tosize=OS_NO) then
           internalerror(2011021307);

         conv_done:=false;
         if tosize<>fromsize then
           begin
             conv_done:=true;
             if tcgsize2size[tosize]<=tcgsize2size[fromsize] then
               fromsize:=tosize;
             case fromsize of
               OS_8:
                 begin
                   list.concat(taicpu.op_reg_ref(A_LD,reg,href));
                   for i:=2 to tcgsize2size[tosize] do
                     begin
                       reg:=GetNextReg(reg);
                       list.concat(taicpu.op_reg(A_CLR,reg));
                     end;
                 end;
               OS_S8:
                 begin
                   list.concat(taicpu.op_reg_ref(A_LD,reg,href));
                   tmpreg:=reg;

                   if tcgsize2size[tosize]>1 then
                     begin
                       reg:=GetNextReg(reg);
                       list.concat(taicpu.op_reg(A_CLR,reg));
                       list.concat(taicpu.op_reg_const(A_SBIC,tmpreg,7));
                       list.concat(taicpu.op_reg(A_COM,reg));
                       tmpreg:=reg;
                       for i:=3 to tcgsize2size[tosize] do
                         begin
                           reg:=GetNextReg(reg);
                           emit_mov(list,reg,tmpreg);
                         end;
                     end;
                 end;
               OS_16:
                 begin
                   if href.base<>NR_NO then
                     href.addressmode:=AM_POSTINCREMENT;
                   list.concat(taicpu.op_reg_ref(A_LD,reg,href));

                   if (href.offset<>0) or assigned(href.symbol) then
                     inc(href.offset);
                   href.addressmode:=AM_UNCHANGED;

                   reg:=GetNextReg(reg);
                   list.concat(taicpu.op_reg_ref(A_LD,reg,href));

                   for i:=3 to tcgsize2size[tosize] do
                     begin
                       reg:=GetNextReg(reg);
                       list.concat(taicpu.op_reg(A_CLR,reg));
                     end;
                 end;
               OS_S16:
                 begin
                   if href.base<>NR_NO then
                     href.addressmode:=AM_POSTINCREMENT;
                   list.concat(taicpu.op_reg_ref(A_LD,reg,href));
                   if (href.offset<>0) or assigned(href.symbol) then
                     inc(href.offset);
                   href.addressmode:=AM_UNCHANGED;

                   reg:=GetNextReg(reg);
                   list.concat(taicpu.op_reg_ref(A_LD,reg,href));
                   tmpreg:=reg;

                   reg:=GetNextReg(reg);
                   list.concat(taicpu.op_reg(A_CLR,reg));
                   list.concat(taicpu.op_reg_const(A_SBIC,tmpreg,7));
                   list.concat(taicpu.op_reg(A_COM,reg));
                   tmpreg:=reg;
                   for i:=4 to tcgsize2size[tosize] do
                     begin
                       reg:=GetNextReg(reg);
                       emit_mov(list,reg,tmpreg);
                     end;
                 end;
               else
                 conv_done:=false;
             end;
           end;
         if not conv_done then
           begin
             for i:=1 to tcgsize2size[fromsize] do
               begin
                   if (href.base<>NR_NO) and (i<tcgsize2size[fromsize]) then
                     href.addressmode:=AM_POSTINCREMENT
                   else
                     href.addressmode:=AM_UNCHANGED;

                 list.concat(taicpu.op_reg_ref(A_LD,reg,href));

                 if (href.offset<>0) or assigned(href.symbol) then
                   inc(href.offset);

                 reg:=GetNextReg(reg);
               end;
           end;
       end;


     procedure tcgavr.a_load_reg_reg(list : TAsmList; fromsize, tosize : tcgsize;reg1,reg2 : tregister);
       var
         conv_done: boolean;
         tmpreg : tregister;
         i : integer;
       begin
         if (tcgsize2size[fromsize]>32) or (tcgsize2size[tosize]>32) or (fromsize=OS_NO) or (tosize=OS_NO) then
           internalerror(2011021310);

         conv_done:=false;
         if tosize<>fromsize then
           begin
             conv_done:=true;
             if tcgsize2size[tosize]<=tcgsize2size[fromsize] then
               fromsize:=tosize;
             case fromsize of
               OS_8:
                 begin
                   emit_mov(list,reg2,reg1);
                   for i:=2 to tcgsize2size[tosize] do
                     begin
                       reg2:=GetNextReg(reg2);
                       list.concat(taicpu.op_reg(A_CLR,reg2));
                     end;
                 end;
               OS_S8:
                 begin
                   { dest is always at least 16 bit at this point }
                   emit_mov(list,reg2,reg1);

                   reg2:=GetNextReg(reg2);
                   list.concat(taicpu.op_reg(A_CLR,reg2));
                   list.concat(taicpu.op_reg_const(A_SBIC,reg1,7));
                   list.concat(taicpu.op_reg(A_COM,reg2));
                   tmpreg:=reg2;
                   for i:=3 to tcgsize2size[tosize] do
                     begin
                       reg2:=GetNextReg(reg2);
                       emit_mov(list,reg2,tmpreg);
                     end;
                 end;
               OS_16:
                 begin
                   emit_mov(list,reg2,reg1);

                   reg1:=GetNextReg(reg1);
                   reg2:=GetNextReg(reg2);
                   emit_mov(list,reg2,reg1);

                   for i:=3 to tcgsize2size[tosize] do
                     begin
                       reg2:=GetNextReg(reg2);
                       list.concat(taicpu.op_reg(A_CLR,reg2));
                     end;
                 end;
               OS_S16:
                 begin
                   { dest is always at least 32 bit at this point }
                   emit_mov(list,reg2,reg1);

                   reg1:=GetNextReg(reg1);
                   reg2:=GetNextReg(reg2);
                   emit_mov(list,reg2,reg1);

                   reg2:=GetNextReg(reg2);
                   list.concat(taicpu.op_reg(A_CLR,reg2));
                   list.concat(taicpu.op_reg_const(A_SBIC,reg1,7));
                   list.concat(taicpu.op_reg(A_COM,reg2));
                   tmpreg:=reg2;
                   for i:=4 to tcgsize2size[tosize] do
                     begin
                       reg2:=GetNextReg(reg2);
                       emit_mov(list,reg2,tmpreg);
                     end;
                 end;
               else
                 conv_done:=false;
             end;
           end;
         if not conv_done and (reg1<>reg2) then
           begin
             for i:=1 to tcgsize2size[fromsize] do
               begin
                 emit_mov(list,reg2,reg1);
                 reg1:=GetNextReg(reg1);
                 reg2:=GetNextReg(reg2);
               end;
           end;
       end;


    {  comparison operations }
    procedure tcgavr.a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : aint;reg : tregister;
      l : tasmlabel);
      begin
        internalerror(2011021311);
      end;


    procedure tcgavr.a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel);
      begin
        internalerror(2011021312);
      end;


    procedure tcgavr.a_jmp_name(list : TAsmList;const s : string);
      begin
        internalerror(2011021313);
      end;


    procedure tcgavr.a_jmp_always(list : TAsmList;l: tasmlabel);
      var
        ai : taicpu;
      begin
        ai:=taicpu.op_sym(A_JMP,l);
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgavr.a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel);
      var
        ai : taicpu;
      begin
        ai:=setcondition(taicpu.op_sym(A_BRxx,l),flags_to_cond(f));
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgavr.g_flags2reg(list: TAsmList; size: TCgSize; const f: TResFlags; reg: TRegister);
      begin
        internalerror(2011021316);
      end;

    procedure tcgavr.a_adjust_sp(list : TAsmList; value : longint);
      var
        i : integer;
      begin
        case value of
          0:
            ;
          -14..-1:
            begin
              if ((-value) mod 2)<>0 then
                list.concat(taicpu.op_reg(A_PUSH,NR_R0));
              for i:=1 to (-value) div 2 do
                list.concat(taicpu.op_const(A_RCALL,0));
            end;
          1..7:
            begin
              for i:=1 to value do
                list.concat(taicpu.op_reg(A_POP,NR_R0));
            end;
          else
            begin
              list.concat(taicpu.op_reg_const(A_SUBI,NR_R28,lo(word(-value))));
              list.concat(taicpu.op_reg_const(A_SBCI,NR_R29,hi(word(-value))));
              // get SREG
              list.concat(taicpu.op_reg_const(A_IN,NR_R0,NIO_SREG));

              // block interrupts
              list.concat(taicpu.op_none(A_CLI));

              // write high SP
              list.concat(taicpu.op_const_reg(A_OUT,NIO_SP_HI,NR_R29));

              // release interrupts
              list.concat(taicpu.op_const_reg(A_OUT,NIO_SREG,NR_R0));

              // write low SP
              list.concat(taicpu.op_const_reg(A_OUT,NIO_SP_LO,NR_R28));
            end;
        end;
      end;

    procedure tcgavr.g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);
      var
         regs : tcpuregisterset;
         reg : tsuperregister;
      begin
        if not(nostackframe) then
          begin
            { save int registers }
            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);
            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              regs:=regs+[RS_R28,RS_R29];

            for reg:=RS_R31 downto RS_R0 do
              if reg in regs then
                list.concat(taicpu.op_reg(A_PUSH,newreg(R_INTREGISTER,reg,R_SUBWHOLE)));

            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              begin
                list.concat(taicpu.op_reg_const(A_IN,NR_R28,NIO_SP_LO));
                list.concat(taicpu.op_reg_const(A_IN,NR_R29,NIO_SP_HI));
              end
            else
              { the framepointer cannot be omitted on avr because sp
                is not a register but part of the i/o map
              }
              internalerror(2011021901);

            a_adjust_sp(list,-localsize);
          end;
      end;


    procedure tcgavr.g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean);
      var
        regs : tcpuregisterset;
        reg : TSuperRegister;
        LocalSize : longint;
      begin
        if not(nostackframe) then
          begin
            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              begin
                LocalSize:=current_procinfo.calc_stackframe_size;
                a_adjust_sp(list,LocalSize);

                for reg:=RS_R0 to RS_R31 do
                  if reg in regs then
                    list.concat(taicpu.op_reg(A_POP,newreg(R_INTREGISTER,reg,R_SUBWHOLE)));

                regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);
              end
            else
              { the framepointer cannot be omitted on avr because sp
                is not a register but part of the i/o map
              }
              internalerror(2011021902);
          end;
        list.concat(taicpu.op_none(A_RET));
      end;


    procedure tcgavr.a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);
      begin
        //!!!!
      end;


    procedure tcgavr.fixref(list : TAsmList;var ref : treference);
      begin
        internalerror(2011021320);
      end;


    procedure tcgavr.g_concatcopy_move(list : TAsmList;const source,dest : treference;len : aint);
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
        a_call_name_static(list,'FPC_MOVE');
        dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        paraloc3.done;
        paraloc2.done;
        paraloc1.done;
      end;


    procedure tcgavr.g_concatcopy_internal(list : TAsmList;const source,dest : treference;len : aint;aligned : boolean);
      begin
        internalerror(2011021321);
      end;

    procedure tcgavr.g_concatcopy_unaligned(list : TAsmList;const source,dest : treference;len : aint);
      begin
        g_concatcopy_internal(list,source,dest,len,false);
      end;


    procedure tcgavr.g_concatcopy(list : TAsmList;const source,dest : treference;len : aint);
      var
        countreg,tmpreg : tregister;
        srcref,dstref : treference;
        copysize,countregsize : tcgsize;
        l : TAsmLabel;
        i : longint;
      begin
        current_asmdata.getjumplabel(l);
        if len>16 then
          begin
            {!!!!!!! load refs!}
            copysize:=OS_8;
            if len<256 then
              countregsize:=OS_8
            else if len<65536 then
              countregsize:=OS_16
            else
              internalerror(2011022007);
            countreg:=getintregister(list,countregsize);
            a_load_const_reg(list,countregsize,len,countreg);
            cg.a_label(list,l);
            tmpreg:=getintregister(list,copysize);
            list.concat(taicpu.op_reg_ref(A_LD,tmpreg,srcref));
            list.concat(taicpu.op_ref_reg(A_ST,dstref,tmpreg));
            a_op_const_reg(list,OP_SUB,countregsize,1,countreg);
            a_jmp_flags(list,F_NE,l);
          end
        else
          begin
            for i:=1 to len do
              begin
                srcref:=normalize_ref(list,source);
                dstref:=normalize_ref(list,source);
                copysize:=OS_8;
                tmpreg:=getintregister(list,copysize);
                if (srcref.base<>NR_NO) and (i<len) then
                  srcref.addressmode:=AM_POSTINCREMENT
                else
                  srcref.addressmode:=AM_UNCHANGED;
                if (dstref.base<>NR_NO) and (i<len) then
                  dstref.addressmode:=AM_POSTINCREMENT
                else
                  dstref.addressmode:=AM_UNCHANGED;

              list.concat(taicpu.op_reg_ref(A_LD,tmpreg,srcref));
              list.concat(taicpu.op_ref_reg(A_ST,dstref,tmpreg));

              if (dstref.offset<>0) or assigned(dstref.symbol) then
                inc(dstref.offset);
              if (srcref.offset<>0) or assigned(srcref.symbol) then
                inc(srcref.offset);

              end;
          end;
      end;


    procedure tcgavr.g_overflowCheck(list : TAsmList;const l : tlocation;def : tdef);
      var
        hl : tasmlabel;
        ai : taicpu;
        cond : TAsmCond;
      begin
        if not(cs_check_overflow in current_settings.localswitches) then
         exit;
        current_asmdata.getjumplabel(hl);
        if not ((def.typ=pointerdef) or
               ((def.typ=orddef) and
                (torddef(def).ordtype in [u64bit,u16bit,u32bit,u8bit,uchar,pasbool]))) then
          cond:=C_VC
        else
          cond:=C_CC;
        ai:=Taicpu.Op_Sym(A_BRxx,hl);
        ai.SetCondition(cond);
        ai.is_jmp:=true;
        list.concat(ai);

        a_call_name(list,'FPC_OVERFLOW',false);
        a_label(list,hl);
      end;


    procedure tcgavr.a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
      var
        ai : taicpu;
      begin
      {!!!!!
        ai:=Taicpu.Op_sym(A_BRxx,l);
        case cond of
          OC_EQ:
            ai.SetCondition(C_EQ);
          OC_GT
          OC_LT
          OC_GTE
          OC_LTE
          OC_NE
          OC_BE
          OC_B
          OC_AE
          OC_A:

        ai.is_jmp:=true;
        list.concat(ai);
        }
      end;


    procedure tcgavr.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
      begin
        internalerror(2011021324);
      end;


    procedure tcgavr.emit_mov(list: TAsmList;reg2: tregister; reg1: tregister);
      var
         instr: taicpu;
      begin
       instr:=taicpu.op_reg_reg(A_MOV,reg2,reg1);
       list.Concat(instr);
       { Notify the register allocator that we have written a move instruction so
         it can try to eliminate it. }
       add_move_instruction(instr);
      end;


    procedure tcg64favr.a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);
      begin
        internalerror(2011021325);
      end;


    procedure tcg64favr.a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);
      begin
        a_op64_const_reg_reg(list,op,size,value,reg,reg);
      end;


    procedure tcg64favr.a_op64_const_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64);
      var
        ovloc : tlocation;
      begin
        a_op64_const_reg_reg_checkoverflow(list,op,size,value,regsrc,regdst,false,ovloc);
      end;


    procedure tcg64favr.a_op64_reg_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64);
      var
        ovloc : tlocation;
      begin
        a_op64_reg_reg_reg_checkoverflow(list,op,size,regsrc1,regsrc2,regdst,false,ovloc);
      end;


    procedure tcg64favr.a_op64_const_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64;setflags : boolean;var ovloc : tlocation);
      begin
        internalerror(2011021326);
      end;


    procedure tcg64favr.a_op64_reg_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64;setflags : boolean;var ovloc : tlocation);
      begin
        internalerror(2011021327);
      end;


    procedure create_codegen;
      begin
        cg:=tcgavr.create;
        cg64:=tcg64favr.create;
      end;

end.
