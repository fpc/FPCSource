{

    Copyright (c) 2008 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generator for the SPC32

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

      { tcgspc32 }

      tcgspc32 = class(tcg)
        { true, if the next arithmetic operation should modify the flags }
        cgsetflags : boolean;
        procedure init_register_allocators;override;
        procedure done_register_allocators;override;

        function getaddressregister(list:TAsmList):TRegister;override;
        function getfpuregister(list:TAsmList;size:Tcgsize):TRegister;override;

        procedure a_load_const_cgpara(list : TAsmList;size : tcgsize;a : tcgint;const paraloc : TCGPara);override;
        procedure a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);override;
        procedure a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);override;
        procedure a_load_reg_cgpara(list : TAsmList; size : tcgsize;r : tregister; const cgpara : tcgpara);override;

        procedure a_call_name(list : TAsmList;const s : string; weak: boolean);override;
        procedure a_call_reg(list : TAsmList;reg: tregister);override;

        procedure a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister); override;
        procedure a_op_reg_reg(list: TAsmList; Op: TOpCG; size: TCGSize; src, dst : TRegister); override;

        procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister); override;
        procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister); override;

        { move instructions }
        procedure a_load_const_reg(list : TAsmList; size: tcgsize; a : tcgint;reg : tregister);override;
        procedure a_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);override;
        procedure a_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister);override;
        procedure a_load_reg_reg(list : TAsmList; fromsize, tosize : tcgsize;reg1,reg2 : tregister);override;

        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister); override;
        procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister); override;
        procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference); override;

        {  comparison operations }
        procedure a_cmp_const_reg_label(list: TAsmList; size: tcgsize; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel); override;
        procedure a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;

        procedure a_jmp_name(list : TAsmList;const s : string); override;
        procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;
        procedure a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel); override;

        procedure g_flags2reg(list: TAsmList; size: TCgSize; const f: TResFlags; reg: TRegister); override;

        procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);override;
        procedure g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean); override;

        procedure a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);override;

        procedure g_concatcopy(list : TAsmList;const source,dest : treference;len : tcgint);override;
        procedure g_concatcopy_move(list : TAsmList;const source,dest : treference;len : tcgint);

        procedure g_overflowcheck(list: TAsmList; const l: tlocation; def: tdef); override;

        procedure g_save_registers(list : TAsmList);override;
        procedure g_restore_registers(list : TAsmList);override;

        procedure a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
        procedure fixref(list : TAsmList;var ref : treference);
        function normalize_ref(list : TAsmList;ref : treference) : treference;

        procedure g_stackpointer_alloc(list : TAsmList;size : longint);override;

        procedure a_adjust_sp(list: TAsmList; value: longint);

        procedure call_rtl_mul_const_reg(list:tasmlist;size:tcgsize;a:tcgint;reg:tregister;const name:string);
        procedure call_rtl_mul_reg_reg(list:tasmlist;reg1,reg2:tregister;const name:string);
      end;

      tcg64fspc32 = class(tcg64f32)
        procedure a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);override;
        procedure a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);override;
      end;

    procedure create_codegen;

    const
      TOpCG2AsmOp: Array[topcg] of TAsmOp = (A_None,A_None,A_ADD,A_AND,A_None,
                                             A_None,A_MUL,A_None,A_None,A_None,A_ORR,
                                             A_ASR,A_LSL,A_LSR,A_SUB,A_XOR,A_None,A_None);
  implementation

    uses
       globals,verbose,systems,cutils,
       fmodule,
       symconst,symsym,symtable,
       tgobj,rgobj,
       procinfo,cpupi,
       paramgr;


    procedure tcgspc32.init_register_allocators;
      begin
        inherited init_register_allocators;
        rg[R_INTREGISTER]:=trgintcpu.create(R_INTREGISTER,R_SUBWHOLE,
            [RS_R0,RS_R1,RS_R2,RS_R3,RS_R4,RS_R5],first_int_imreg,[]);
        { rg[R_ADDRESSREGISTER]:=trgintcpu.create(R_ADDRESSREGISTER,R_SUBWHOLE,
            [RS_R26,RS_R30],first_int_imreg,[]); }
      end;


    procedure tcgspc32.done_register_allocators;
      begin
        rg[R_INTREGISTER].free;
        // rg[R_ADDRESSREGISTER].free;
        inherited done_register_allocators;
      end;


    function tcgspc32.getaddressregister(list: TAsmList): TRegister;
      begin
        Result:=getintregister(list,OS_ADDR);
      end;


    function tcgspc32.getfpuregister(list:TAsmList;size:Tcgsize): TRegister;
      begin
        case size of
          OS_F32:
            Result:=getintregister(list,OS_32);
          OS_F64:
            Result:=getintregister(list,OS_64);
          else
            internalerror(2014041704);
        end;
      end;


    procedure tcgspc32.a_load_reg_cgpara(list : TAsmList;size : tcgsize;r : tregister;const cgpara : tcgpara);

      procedure load_para_loc(r : TRegister;paraloc : PCGParaLocation);
        var
          ref : treference;
        begin
          paramanager.allocparaloc(list,paraloc);
          case paraloc^.loc of
             LOC_REGISTER,LOC_CREGISTER:
               a_load_reg_reg(list,paraloc^.size,paraloc^.size,r,paraloc^.register);
             LOC_REFERENCE,LOC_CREFERENCE:
               begin
                  reference_reset_base(ref,paraloc^.reference.index,paraloc^.reference.offset,4);
                  a_load_reg_ref(list,paraloc^.size,paraloc^.size,r,ref);
               end;
             else
               internalerror(2002071004);
          end;
        end;

      var
        i, i2 : longint;
        hp : PCGParaLocation;

      begin
        {if use_push(cgpara) then
          begin
            if tcgsize2size[cgpara.Size] > 2 then
              begin
                if tcgsize2size[cgpara.Size] <> 4 then
                  internalerror(2013031101);
                if cgpara.location^.Next = nil then
                  begin
                    if tcgsize2size[cgpara.location^.size] <> 4 then
                      internalerror(2013031101);
                  end
                else
                  begin
                    if tcgsize2size[cgpara.location^.size] <> 2 then
                      internalerror(2013031101);
                    if tcgsize2size[cgpara.location^.Next^.size] <> 2 then
                      internalerror(2013031101);
                    if cgpara.location^.Next^.Next <> nil then
                      internalerror(2013031101);
                  end;

                if tcgsize2size[cgpara.size]>cgpara.alignment then
                  pushsize:=cgpara.size
                else
                  pushsize:=int_cgsize(cgpara.alignment);
                pushsize2 := int_cgsize(tcgsize2size[pushsize] - 2);
                list.concat(taicpu.op_reg(A_PUSH,TCgsize2opsize[pushsize2],makeregsize(list,GetNextReg(r),pushsize2)));
                list.concat(taicpu.op_reg(A_PUSH,S_W,makeregsize(list,r,OS_16)));
              end
            else
              begin
                cgpara.check_simple_location;
                if tcgsize2size[cgpara.location^.size]>cgpara.alignment then
                  pushsize:=cgpara.location^.size
                else
                  pushsize:=int_cgsize(cgpara.alignment);
                list.concat(taicpu.op_reg(A_PUSH,TCgsize2opsize[pushsize],makeregsize(list,r,pushsize)));
              end;

          end
        else}
          begin
            if not(tcgsize2size[cgpara.Size] in [1..4]) then
              internalerror(2014011101);

            hp:=cgpara.location;

            i:=0;
            while i<tcgsize2size[cgpara.Size] do
              begin
                if not(assigned(hp)) then
                  internalerror(2014011102);

                inc(i, tcgsize2size[hp^.Size]);

                if hp^.Loc=LOC_REGISTER then
                  begin
                    load_para_loc(r,hp);
                    hp:=hp^.Next;
                    r:=GetNextReg(r);
                  end
                else
                  begin
                    load_para_loc(r,hp);

                    for i2:=1 to tcgsize2size[hp^.Size] do
                      r:=GetNextReg(r);

                    hp:=hp^.Next;
                  end;
              end;
            if assigned(hp) then
              internalerror(2014011103);
          end;
      end;


    procedure tcgspc32.a_load_const_cgpara(list : TAsmList;size : tcgsize;a : tcgint;const paraloc : TCGPara);
      var
        i : longint;
        hp : PCGParaLocation;
        val : tcgint;
      begin
        paraloc.check_simple_location;
        if not(tcgsize2size[paraloc.Size] in [1..4]) then
          internalerror(2014011101);

        hp:=paraloc.location;

        i:=0;
        while i < tcgsize2size[paraloc.Size] do
          begin
            val:=a shr (i*8);

            case tcgsize2size[hp^.size] of
              1: val:=val and $FF;
              2: val:=val and $FFFF;
              4: val:=val and $FFFFFFFF;
            end;

            case hp^.loc of
              LOC_REGISTER,LOC_CREGISTER:
                a_load_const_reg(list,hp^.size,val,hp^.register);
              LOC_REFERENCE,LOC_CREFERENCE:
                begin
                  list.concat(taicpu.op_reg(A_LD,NR_STACK_POINTER_REG));

                  if ((val and $7FFF)=val) or
                     (((-val) and $7FFF)=-val) then
                    begin
                      list.concat(taicpu.op_const(A_SUB,4));
                      list.concat(taicpu.op_const(A_STW,val));
                    end
                  else
                    begin
                      list.concat(taicpu.op_const(A_SUB,2));
                      list.concat(taicpu.op_const(A_STH,val and $FFFF));
                      list.concat(taicpu.op_const(A_SUB,2));
                      list.concat(taicpu.op_const(A_STH,val shr 16));
                    end;

                  list.concat(taicpu.op_reg(A_ST,NR_STACK_POINTER_REG));
                end
                //list.concat(taicpu.op_const(A_PUSH,a));
              else
                internalerror(2002071004);
            end;
            inc(i, tcgsize2size[hp^.size]);
            hp:=hp^.Next;
          end;
        if assigned(hp) then
          internalerror(2014011104);
      end;


    procedure tcgspc32.a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);
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


    procedure tcgspc32.a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);
      var
        tmpreg: tregister;
      begin
        tmpreg:=getaddressregister(list);
        a_loadaddr_ref_reg(list,r,tmpreg);
        a_load_reg_cgpara(list,OS_ADDR,tmpreg,paraloc);
      end;


    procedure tcgspc32.a_call_name(list : TAsmList;const s : string; weak: boolean);
      var
        href: treference;
        l: tasmlabel;
      begin
        reference_reset_symbol(href, current_asmdata.RefAsmSymbol(s), 0,1);

        current_asmdata.getjumplabel(l);

        href.relsymbol:=l;

        a_label(list,l);
        list.concat(taicpu.op_ref(A_PCALL,href));
{
        the compiler does not properly set this flag anymore in pass 1, and
        for now we only need it after pass 2 (I hope) (JM)
          if not(pi_do_call in current_procinfo.flags) then
            internalerror(2003060703);
}
        include(current_procinfo.flags,pi_do_call);
      end;


    procedure tcgspc32.a_call_reg(list : TAsmList;reg: tregister);
      begin
        {a_reg_alloc(list,NR_ZLO);
        a_reg_alloc(list,NR_ZHI);
        list.concat(taicpu.op_reg_reg(A_MOV,NR_ZLO,reg));
        list.concat(taicpu.op_reg_reg(A_MOV,NR_ZHI,GetHigh(reg)));
        list.concat(taicpu.op_none(A_ICALL));
        a_reg_dealloc(list,NR_ZLO);
        a_reg_dealloc(list,NR_ZHI);}

        list.concat(taicpu.op_reg(A_CALL,reg));

        include(current_procinfo.flags,pi_do_call);
      end;


     procedure tcgspc32.a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister);
       var
         tmpreg: TRegister;
       begin
         if not(size in [OS_S8,OS_8,OS_S16,OS_16,OS_S32,OS_32]) then
           internalerror(2012102403);

         a_op_const_reg_reg(list,op,size,a,reg,reg);
       end;


     procedure tcgspc32.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: TCGSize; src, dst : TRegister);
       begin
         case op of
           OP_NEG:
             begin
               list.concat(taicpu.op_none(A_NUL));
               list.concat(taicpu.op_reg(A_SUB,src));
               list.concat(taicpu.op_reg(A_ST,dst));
             end;
           OP_NOT:
             a_op_const_reg_reg(list,OP_XOR,size,-1,src,dst);
           else
             a_op_reg_reg_reg(list,op,size,src,dst,dst);
         end;
       end;


    procedure tcgspc32.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister);
      var
        tmpreg: TRegister;
        power: longint;
      begin
        if (op in [OP_MUL,OP_IMUL]) and (not ispowerof2(a, power)) then
          begin
            a_load_reg_reg(list,size,size,src,dst);

            if op = OP_MUL then
              call_rtl_mul_const_reg(list, size, a, dst,'fpc_mul_dword')
            else
              call_rtl_mul_const_reg(list, size, a, dst,'fpc_mul_longint');

            exit;
          end
        else if (op in [OP_MUL,OP_IMUL]) and ispowerof2(a, power) then
          begin
            op:=OP_SHL;
            a:=power;
          end;

        if ((a and $7FFF) = a) or
           (((-a) and $7FFF) = -a) then
          begin
            list.concat(taicpu.op_reg(A_LD,src));
            list.concat(taicpu.op_const(TOpCG2AsmOp[Op],a));
            list.concat(taicpu.op_reg(A_ST,dst));
          end
        else
          begin
            tmpreg:=getintregister(list,OS_INT);
            a_load_const_reg(list,OS_INT,a,tmpreg);
            a_op_reg_reg_reg(list,OP,size,tmpreg,src,dst);
          end;
      end;


    procedure tcgspc32.a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister);
      begin
        if op in [OP_MUL,OP_IMUL] then
          begin
            a_load_reg_reg(list,size,size,src2,dst);

            if op = OP_MUL then
              call_rtl_mul_reg_reg(list, src1, dst,'fpc_mul_dword')
            else
              call_rtl_mul_reg_reg(list, src1, dst,'fpc_mul_longint');
          end
        else
          begin
            list.concat(taicpu.op_reg(A_LD,src2));
            list.concat(taicpu.op_reg(TOpCG2AsmOp[Op],src1));
            list.concat(taicpu.op_reg(A_ST,dst));
          end;
      end;


    procedure tcgspc32.a_load_const_reg(list : TAsmList; size: tcgsize; a : tcgint;reg : tregister);
      var
        mask : qword;
        shift : byte;
        i : byte;
        lab,
        dst: tasmlabel;
      begin
        if a = 0 then
          list.concat(taicpu.op_none(A_NUL))
        else if (a and $7FFF) = a then
          list.concat(taicpu.op_const(A_LD, a))
        else if ((-a) and $7FFF) = -a then
          list.concat(taicpu.op_const(A_LD, a))
        else
          begin
            list.concat(taicpu.op_const(A_LD,a and $FFFF));
            list.concat(taicpu.op_const(A_LDU,(a shr 16) and $FFFF));
          end;
        list.concat(taicpu.op_reg(A_ST,reg));
      end;


    function tcgspc32.normalize_ref(list:TAsmList;ref: treference) : treference;
      var
        tmpref : treference;
        tmpreg , b: tregister;
        l : tasmlabel;
      begin
        Result:=ref;

        if (result.base=NR_NO) and
           (result.index<>NR_NO) then
          begin
            result.base:=result.index;
            result.index:=NR_NO;
          end;

        if (result.base<>NR_NO) and
           (result.index<>NR_NO) then
          begin
            tmpreg:=getaddressregister(list);
            a_op_reg_reg_reg(list,OP_ADD,OS_ADDR,result.base,result.index,tmpreg);
            result.base:=tmpreg;
            result.index:=NR_NO;
          end;

        if assigned(result.symbol) then
          begin
            tmpreg:=getaddressregister(list);

            b:=result.base;
            result.base:=NR_NO;

            result.refaddr:=addr_lo16;
            list.concat(taicpu.op_ref(A_LD,Result));
            result.refaddr:=addr_hi16;
            list.concat(taicpu.op_ref(A_LDU,Result));

            if b<>NR_NO then
              list.concat(taicpu.op_reg(A_ADD,b));

            list.concat(taicpu.op_reg(A_ST,tmpreg));

            reference_reset_base(result,tmpreg,0,ref.alignment);
          end;

        if (result.offset<-32767) or (Result.offset>32767) then
          begin
            tmpreg:=getaddressregister(list);

            if result.base<>NR_NO then
              a_op_const_reg_reg(list,OP_ADD,OS_ADDR,result.offset,result.base,tmpreg)
            else
              a_load_const_reg(list,OS_ADDR,result.offset,tmpreg);

            result.base:=tmpreg;
            result.offset:=0;
          end;
      end;


     procedure tcgspc32.a_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);
       var
         href : treference;
         conv_done: boolean;
         tmpreg : tregister;
         i : integer;
         QuickRef : Boolean;
         SizeCode: TAsmOp;
       begin
         case tosize of
           OS_S8,OS_8: SizeCode:=A_STB;
           OS_S16,OS_16: SizeCode:=A_STH;
           OS_S32,OS_32: SizeCode:=A_STW;
         end;

         href:=normalize_ref(list,ref);

         if (href.offset<>0) and
            (href.base<>NR_NO) then
           begin
             tmpreg:=getaddressregister(list);
             a_op_const_reg_reg(list,OP_ADD,OS_ADDR,href.offset,href.base,tmpreg);

             href.base:=tmpreg;
             href.offset:=0
           end;

         if assigned(href.symbol) then
           begin
             tmpreg:=href.base;
             href.base:=NR_NO;

             href.refaddr:=addr_lo16;
             list.concat(taicpu.op_ref(A_LD,href));
             href.refaddr:=addr_hi16;
             list.concat(taicpu.op_ref(A_LDU,href));

             if tmpreg<>NR_NO then
               list.concat(taicpu.op_reg(A_ADD,tmpreg));
           end
         else if href.base<>NR_NO then
           list.concat(taicpu.op_reg(A_LD,href.base))
         else
           internalerror(2014032001);

         list.concat(taicpu.op_reg(SizeCode,reg));
       end;


     procedure tcgspc32.a_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;
       const Ref : treference;reg : tregister);
       var
         href : treference;
         conv_done: boolean;
         tmpreg : tregister;
         i : integer;
         QuickRef : boolean;
         SizeCode: TAsmOp;
       begin
         case fromsize of
           OS_S8,OS_8: SizeCode:=A_LDB;
           OS_S16,OS_16: SizeCode:=A_LDH;
           OS_S32,OS_32: SizeCode:=A_LDW;
         end;

         href:=normalize_ref(list,ref);

         if (tcgsize2size[fromsize]>32) or (tcgsize2size[tosize]>32) or (fromsize=OS_NO) or (tosize=OS_NO) then
           internalerror(2011021307);

         if assigned(href.symbol) then
           begin
             tmpreg:=href.base;
             href.base:=NR_NO;

             href.refaddr:=addr_lo16;
             list.concat(taicpu.op_ref(A_LD,href));
             href.refaddr:=addr_hi16;
             list.concat(taicpu.op_ref(A_LDU,href));

             if tmpreg<>NR_NO then
               list.concat(taicpu.op_reg(SizeCode,tmpreg))
             else
               list.concat(taicpu.op_const(SizeCode,0));
           end
         else if href.offset<>0 then
           begin
             list.concat(taicpu.op_reg(A_LD,href.base));
             list.concat(taicpu.op_const(SizeCode,href.offset));
           end
         else
           begin
             list.concat(taicpu.op_none(A_NUL));
             list.concat(taicpu.op_reg(SizeCode,href.base));
           end;

         list.concat(taicpu.op_reg(A_ST,reg));
       end;


     procedure tcgspc32.a_load_reg_reg(list : TAsmList; fromsize, tosize : tcgsize;reg1,reg2 : tregister);
       var
         ai: taicpu;
       begin
         {ai:=taicpu.op_reg(A_LD,reg1);
         //rg[R_INTREGISTER].add_move_instruction(ai);
         list.concat(ai);
         list.concat(taicpu.op_reg(A_ST,reg2));}

         ai:=taicpu.op_reg_reg(A_MOV,reg2,reg1);
         rg[R_INTREGISTER].add_move_instruction(ai);
         list.concat(ai);
       end;


     procedure tcgspc32.a_loadfpu_reg_reg(list: TAsmList; fromsize,tosize: tcgsize; reg1, reg2: tregister);
       begin
         case fromsize of
           OS_F32: fromsize:=OS_32;
           OS_F64: fromsize:=OS_64;
         end;

         case tosize of
           OS_F32: tosize:=OS_32;
           OS_F64: tosize:=OS_64;
         end;

         a_load_reg_reg(list,fromsize,tosize,reg1,reg2);
       end;


     procedure tcgspc32.a_loadfpu_ref_reg(list: TAsmList; fromsize,tosize: tcgsize; const ref: treference; reg: tregister);
       begin
         case fromsize of
           OS_F32: fromsize:=OS_32;
           OS_F64: fromsize:=OS_64;
         end;

         case tosize of
           OS_F32: tosize:=OS_32;
           OS_F64: tosize:=OS_64;
         end;

         a_load_ref_reg(list,fromsize,tosize,ref,reg);
       end;


     procedure tcgspc32.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference);
       begin
         case fromsize of
           OS_F32: fromsize:=OS_32;
           OS_F64: fromsize:=OS_64;
         end;

         case tosize of
           OS_F32: tosize:=OS_32;
           OS_F64: tosize:=OS_64;
         end;

         a_load_reg_ref(list,fromsize,tosize,reg,ref);
       end;


    {  comparison operations }

    procedure tcgspc32.a_cmp_const_reg_label(list: TAsmList; size: tcgsize;
      cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel);
      var
        swapped : boolean;
        tmpreg : tregister;
        i : byte;
      begin
        if ((a and $7FFF) <> a) and
           ((-a and $7FFF) <> -a) then
          begin
            inherited a_cmp_const_reg_label(list,size,cmp_op,a,reg,l);
          end
        else
          begin
            swapped:=false;
            { swap parameters? }
            case cmp_op of
              OC_GT:
                begin
                  swapped:=true;
                  cmp_op:=OC_LT;
                end;
              OC_GTE:
                begin
                  swapped:=true;
                  cmp_op:=OC_LTE;
                end;
              OC_AE:
                begin
                  swapped:=true;
                  cmp_op:=OC_B;
                end;
              OC_A:
                begin
                  swapped:=true;
                  cmp_op:=OC_BE;
                end;
            end;
            if swapped then
              begin
                list.concat(taicpu.op_const(A_LD,a));
                a_reg_alloc(list,NR_DEFAULTFLAGS);
                list.concat(taicpu.op_reg(A_SUB,reg));
              end
            else
              begin
                list.concat(taicpu.op_reg(A_LD,reg));
                a_reg_alloc(list,NR_DEFAULTFLAGS);
                list.concat(taicpu.op_const(A_SUB,a));
              end;

            a_jmp_cond(list,cmp_op,l);
            a_reg_dealloc(list,NR_DEFAULTFLAGS);
          end;
      end;


    procedure tcgspc32.a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;
      cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel);
      var
        swapped : boolean;
        tmpreg : tregister;
        i : byte;
      begin
        swapped:=false;
        { swap parameters? }
        case cmp_op of
          OC_GT:
            begin
              swapped:=true;
              cmp_op:=OC_LT;
            end;
          OC_GTE:
            begin
              swapped:=true;
              cmp_op:=OC_LTE;
            end;
          OC_AE:
            begin
              swapped:=true;
              cmp_op:=OC_B;
            end;
          OC_A:
            begin
              swapped:=true;
              cmp_op:=OC_BE;
            end;
        end;
        if swapped then
          begin
            tmpreg:=reg1;
            reg1:=reg2;
            reg2:=tmpreg;
          end;
        list.concat(taicpu.op_reg(A_LD,reg2));
        a_reg_alloc(list,NR_DEFAULTFLAGS);
        list.concat(taicpu.op_reg(A_SUB,reg1));
        a_jmp_cond(list,cmp_op,l);
        a_reg_dealloc(list,NR_DEFAULTFLAGS);
      end;


    procedure tcgspc32.a_jmp_name(list : TAsmList;const s : string);
      var
        ai : taicpu;
        href: treference;
        rl: TAsmLabel;
      begin
        current_asmdata.getjumplabel(rl);

        reference_reset_symbol(href, current_asmdata.RefAsmSymbol(s), 0, 1);
        href.relsymbol:=rl;

        a_label(list, rl);
        ai:=taicpu.op_ref(A_PJMP,href);
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgspc32.a_jmp_always(list : TAsmList;l: tasmlabel);
      var
        ai : taicpu;
        rl: TAsmLabel;
        href: treference;
      begin
        current_asmdata.getjumplabel(rl);

        reference_reset_symbol(href, l, 0, 1);
        href.relsymbol:=rl;

        a_label(list, rl);

        ai:=taicpu.op_ref(A_PJMP,href);
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgspc32.a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel);
      var
        ai : taicpu;
        rl: TAsmLabel;
        href: treference;
      begin
        current_asmdata.getjumplabel(rl);

        reference_reset_symbol(href, l, 0, 1);
        href.relsymbol:=rl;

        a_label(list, rl);

        ai:=setcondition(taicpu.op_ref(A_PJxx,href),flags_to_cond(f));
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgspc32.g_flags2reg(list: TAsmList; size: TCgSize; const f: TResFlags; reg: TRegister);
      var
        l : TAsmLabel;
        tmpflags : TResFlags;
      begin
        current_asmdata.getjumplabel(l);
        {
        if flags_to_cond(f) then
          begin
            tmpflags:=f;
            inverse_flags(tmpflags);
            list.concat(taicpu.op_reg(A_CLR,reg));
            a_jmp_flags(list,tmpflags,l);
            list.concat(taicpu.op_reg_const(A_LDI,reg,1));
          end
        else
        }
          begin
            list.concat(taicpu.op_const(A_LD,1));
            list.concat(taicpu.op_reg(A_ST,reg));
            a_jmp_flags(list,f,l);
            list.concat(taicpu.op_none(A_NUL));
            list.concat(taicpu.op_reg(A_ST,reg));
          end;
        cg.a_label(list,l);
      end;


    procedure tcgspc32.a_adjust_sp(list : TAsmList; value : longint);
      var
        i : integer;
      begin
      end;


    procedure tcgspc32.call_rtl_mul_const_reg(list:tasmlist;size:tcgsize;a:tcgint;reg:tregister;const name:string);
      var
        paraloc1,paraloc2,paraloc3 : tcgpara;
        pd : tprocdef;
      begin
        pd:=search_system_proc(name);
        paraloc1.init;
        paraloc2.init;
        paraloc3.init;
        paramanager.getintparaloc(list,pd,1,paraloc1);
        paramanager.getintparaloc(list,pd,2,paraloc2);
        paramanager.getintparaloc(list,pd,3,paraloc3);
        a_load_const_cgpara(list,OS_8,0,paraloc3);
        a_load_const_cgpara(list,size,a,paraloc2);
        a_load_reg_cgpara(list,OS_32,reg,paraloc1);
        paramanager.freecgpara(list,paraloc3);
        paramanager.freecgpara(list,paraloc2);
        paramanager.freecgpara(list,paraloc1);
        alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        a_call_name(list,name,false);
        dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        cg.a_reg_alloc(list,NR_FUNCTION_RESULT_REG);
        cg.a_load_reg_reg(list,OS_32,OS_32,NR_FUNCTION_RESULT_REG,reg);
        paraloc3.done;
        paraloc2.done;
        paraloc1.done;
      end;


    procedure tcgspc32.call_rtl_mul_reg_reg(list:tasmlist;reg1,reg2:tregister;const name:string);
      var
        paraloc1,paraloc2,paraloc3 : tcgpara;
        pd : tprocdef;
      begin
       pd:=search_system_proc(name);
        paraloc1.init;
        paraloc2.init;
        paraloc3.init;
        paramanager.getintparaloc(list,pd,1,paraloc1);
        paramanager.getintparaloc(list,pd,2,paraloc2);
        paramanager.getintparaloc(list,pd,3,paraloc3);
        a_load_const_cgpara(list,OS_8,0,paraloc3);
        a_load_reg_cgpara(list,OS_32,reg1,paraloc2);
        a_load_reg_cgpara(list,OS_32,reg2,paraloc1);
        paramanager.freecgpara(list,paraloc3);
        paramanager.freecgpara(list,paraloc2);
        paramanager.freecgpara(list,paraloc1);
        alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        a_call_name(list,name,false);
        dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        cg.a_reg_alloc(list,NR_FUNCTION_RESULT_REG);
        cg.a_load_reg_reg(list,OS_32,OS_32,NR_FUNCTION_RESULT_REG,reg2);
        paraloc3.done;
        paraloc2.done;
        paraloc1.done;
      end;


    procedure tcgspc32.g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);
      var
         regs : tcpuregisterset;
         reg : tsuperregister;
         cnt : longint;
      begin
        if not(nostackframe) then
          begin
            {
              push #regs
              ld r6
              add #123
              st r5
              sub #123+4
              st r6
              ...
              pop #regs
              gs lr
              jmp #0
            }
            LocalSize:=current_procinfo.calc_stackframe_size;

            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);
            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              include(regs, RS_FRAME_POINTER_REG);

            if pi_do_call in current_procinfo.flags then
              include(regs,RS_LR);

            if regs<>[] then
              list.concat(taicpu.op_const(A_PUSH,longword(regs)));

            cnt:=0;
            {for reg:=RS_R0 to RS_LR do
              if reg in regs then
                inc(cnt);}

            if (current_procinfo.framepointer<>NR_STACK_POINTER_REG) or (LocalSize>0) then
              begin
                list.concat(taicpu.op_reg(A_LD,NR_STACK_POINTER_REG));

                if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
                  begin
                    list.concat(taicpu.op_const(A_ADD,cnt*4));
                    list.concat(taicpu.op_reg(A_ST,NR_FRAME_POINTER_REG));
                  end;

                if LocalSize>0 then
                  begin
                    list.concat(taicpu.op_const(A_SUB,cnt*4+localsize));
                    list.concat(taicpu.op_reg(A_ST,NR_STACK_POINTER_REG));
                  end;
              end;

            { save int registers }
            {regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);
            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              regs:=regs+[RS_FRAME_POINTER_REG];

            cnt:=0;

            if pi_do_call in current_procinfo.flags then
              include(regs,RS_R4);

            if regs<>[] then
              begin
                list.Concat(taicpu.op_reg(A_LD,NR_STACK_POINTER_REG));

                for reg:=RS_R6 downto RS_R0 do
                  if reg in regs then
                    begin
                      list.Concat(taicpu.op_const(A_SUB,4));
                      list.concat(taicpu.op_reg(A_STW,newreg(R_INTREGISTER,reg,R_SUBWHOLE)));
                      inc(cnt,4);
                    end;

                list.Concat(taicpu.op_reg(A_ST,NR_STACK_POINTER_REG));

                if pi_do_call in current_procinfo.flags then
                  begin
                    list.concat(taicpu.op_const(A_GS,SS_LR));
                    list.concat(taicpu.op_reg(A_ST,NR_R4));

                    list.Concat(taicpu.op_reg(A_LD,NR_STACK_POINTER_REG));
                    list.Concat(taicpu.op_const(A_SUB,4));
                    list.concat(taicpu.op_reg(A_STW,NR_R4));
                    inc(cnt,4);

                    list.Concat(taicpu.op_reg(A_ST,NR_STACK_POINTER_REG));
                  end;
              end;

            if localsize>0 then
              begin
                list.Concat(taicpu.op_reg(A_LD,NR_STACK_POINTER_REG));
                list.Concat(taicpu.op_const(A_SUB,localsize));
                list.Concat(taicpu.op_reg(A_ST,NR_STACK_POINTER_REG));
              end;

            if (regs<>[]) and (current_procinfo.framepointer<>NR_STACK_POINTER_REG) then
              begin
                list.concat(taicpu.op_const(A_ADD,cnt+localsize));
                list.Concat(taicpu.op_reg(A_ST,current_procinfo.framepointer));
              end;}
          end;
      end;


    procedure tcgspc32.g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean);
      var
        regs : tcpuregisterset;
        reg : TSuperRegister;
        LocalSize : longint;
        href: treference;
         cnt : longint;
      begin
        if not(nostackframe) then
          begin
            LocalSize:=current_procinfo.calc_stackframe_size;

            if localsize>0 then
              begin
                list.Concat(taicpu.op_reg(A_LD,NR_STACK_POINTER_REG));
                list.Concat(taicpu.op_const(A_ADD,localsize));
                list.Concat(taicpu.op_reg(A_ST,NR_STACK_POINTER_REG));
              end;

            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);
            if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              include(regs, RS_FRAME_POINTER_REG);

            if pi_do_call in current_procinfo.flags then
              include(regs,RS_LR);

            if regs<>[] then
              list.concat(taicpu.op_const(A_POP,longword(regs)));

            //if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
              {begin
                LocalSize:=current_procinfo.calc_stackframe_size;

                if localsize>0 then
                  begin
                    list.Concat(taicpu.op_reg(A_LD,NR_STACK_POINTER_REG));
                    list.Concat(taicpu.op_const(A_ADD,localsize));
                    list.Concat(taicpu.op_reg(A_ST,NR_STACK_POINTER_REG));
                  end;

                regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);
                if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
                  regs:=regs+[RS_FRAME_POINTER_REG];

                if pi_do_call in current_procinfo.flags then
                  include(regs,RS_R4);

                if regs<>[] then
                  begin
                    cnt:=0;

                    if pi_do_call in current_procinfo.flags then
                      begin
                        list.concat(taicpu.op_none(A_NUL));
                        list.concat(taicpu.op_reg(A_LDW,NR_STACK_POINTER_REG));
                        list.concat(taicpu.op_const(A_SS,SS_LR));
                        inc(cnt,4);
                      end;

                    for reg:=RS_R0 to RS_R6 do
                      if reg in regs then
                        begin
                          if cnt=0 then
                            list.concat(taicpu.op_none(A_NUL))
                          else
                            list.concat(taicpu.op_const(A_LD,cnt));
                          list.concat(taicpu.op_reg(A_LDW,NR_STACK_POINTER_REG));
                          list.concat(taicpu.op_reg(A_ST,newreg(R_INTREGISTER,reg,R_SUBWHOLE)));
                          inc(cnt,4);
                        end;

                    list.concat(taicpu.op_reg(A_LD,NR_STACK_POINTER_REG));
                    list.concat(taicpu.op_const(A_ADD,cnt));
                    list.concat(taicpu.op_reg(A_ST,NR_STACK_POINTER_REG));
                  end;
              end}
            {else
              begin
                LocalSize:=current_procinfo.calc_stackframe_size;

                if localsize>0 then
                  begin
                    list.Concat(taicpu.op_reg(A_LD,NR_STACK_POINTER_REG));
                    list.Concat(taicpu.op_const(A_ADD,localsize));
                    list.Concat(taicpu.op_reg(A_ST,NR_STACK_POINTER_REG));
                  end;
              end;}
          end;
        list.concat(taicpu.op_const(A_GS,SS_LR));
        list.concat(taicpu.op_const(A_JMP,0));
      end;


    procedure tcgspc32.a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);
      var
        href : treference;
        conv_done: boolean;
        tmpreg : tregister;
        i : integer;
        QuickRef : boolean;
      begin
        href:=normalize_ref(list,ref);

        if assigned(href.symbol) then
          begin
            href.refaddr:=addr_lo16;
            list.concat(taicpu.op_ref(A_LD,href));
            href.refaddr:=addr_hi16;
            list.concat(taicpu.op_ref(A_LDU,href));
          end
        else
          begin
            list.concat(taicpu.op_reg(A_LD,href.base));

            if href.offset<>0 then
              list.concat(taicpu.op_const(A_ADD,href.offset));
          end;

        list.concat(taicpu.op_reg(A_ST,r));
      end;


    procedure tcgspc32.fixref(list : TAsmList;var ref : treference);
      begin
        internalerror(2011021320);
      end;


    procedure tcgspc32.g_concatcopy_move(list : TAsmList;const source,dest : treference;len : tcgint);
      var
        paraloc1,paraloc2,paraloc3 : TCGPara;
        pd : tprocdef;
      begin
        pd:=search_system_proc('MOVE');
        paraloc1.init;
        paraloc2.init;
        paraloc3.init;
        paramanager.getintparaloc(list,pd,1,paraloc1);
        paramanager.getintparaloc(list,pd,2,paraloc2);
        paramanager.getintparaloc(list,pd,3,paraloc3);
        a_load_const_cgpara(list,OS_SINT,len,paraloc3);
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


    procedure tcgspc32.g_concatcopy(list : TAsmList;const source,dest : treference;len : tcgint);
      var
        srcreg, dstreg, cntreg, tmpreg: TRegister;
        l: tasmlabel;
      begin
        if len <= 0 then
          exit;

        if (len > 8) or
           ((len > 4) and
            (not (source.alignment in [4,8]))) or
           ((len = 2) and
            (not (source.alignment in [2,4,8]))) or
           ((len = 4) and
            (not (source.alignment in [4,8]))) then
          begin
            g_concatcopy_move(list,source,dest,len);
            exit;
          end;

        srcreg:=getaddressregister(list);
        dstreg:=getaddressregister(list);
        tmpreg:=getintregister(list,OS_INT);

        a_loadaddr_ref_reg(list,source,srcreg);

        list.concat(taicpu.op_reg(A_LD,srcreg));
        case len of
          1: list.Concat(taicpu.op_const(A_LDB,0));
          2: list.Concat(taicpu.op_const(A_LDH,0));
          4: list.Concat(taicpu.op_const(A_LDW,0));
        end;
        list.concat(taicpu.op_reg(A_ST,tmpreg));

        a_loadaddr_ref_reg(list,dest,dstreg);

        list.concat(taicpu.op_reg(A_LD,dstreg));
        case len of
          1: list.Concat(taicpu.op_reg(A_STB,tmpreg));
          2: list.Concat(taicpu.op_reg(A_STH,tmpreg));
          4: list.Concat(taicpu.op_reg(A_STW,tmpreg));
        end;

        if len > 4 then
          begin
            dec(len,4);

            list.concat(taicpu.op_reg(A_LD,srcreg));
            case len of
              1: list.Concat(taicpu.op_const(A_LDB,4));
              2: list.Concat(taicpu.op_const(A_LDH,4));
              4: list.Concat(taicpu.op_const(A_LDW,4));
            end;
            list.concat(taicpu.op_reg(A_ST,tmpreg));

            list.concat(taicpu.op_reg(A_LD,dstreg));
            list.concat(taicpu.op_const(A_ADD,4));
            case len of
              1: list.Concat(taicpu.op_reg(A_STB,tmpreg));
              2: list.Concat(taicpu.op_reg(A_STH,tmpreg));
              4: list.Concat(taicpu.op_reg(A_STW,tmpreg));
            end;
          end;
      end;


        procedure tcgspc32.g_overflowcheck(list: TAsmList; const l: tlocation; def: tdef);
      var
        hl : tasmlabel;
        ai : taicpu;
        cond : TAsmCond;
      begin
        if not(cs_check_overflow in current_settings.localswitches) then
         exit;
        {current_asmdata.getjumplabel(hl);
        if not ((def.typ=pointerdef) or
               ((def.typ=orddef) and
                (torddef(def).ordtype in [u64bit,u16bit,u32bit,u8bit,uchar,
                                          pasbool8,pasbool16,pasbool32,pasbool64]))) then
          cond:=C_VC
        else
          cond:=C_CC;
        ai:=Taicpu.Op_Sym(A_Jxx,hl);
        ai.SetCondition(cond);
        ai.is_jmp:=true;
        list.concat(ai);

        a_call_name(list,'FPC_OVERFLOW',false);
        a_label(list,hl);}
      end;


    procedure tcgspc32.g_save_registers(list: TAsmList);
      begin
        { this is done by the entry code }
      end;


    procedure tcgspc32.g_restore_registers(list: TAsmList);
      begin
        { this is done by the exit code }
      end;


    procedure tcgspc32.a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
      var
        ai1,ai2 : taicpu;
        hl , rl: TAsmLabel;
        href: treference;
      begin
        current_asmdata.getjumplabel(rl);

        reference_reset_symbol(href, l, 0,1);
        href.relsymbol:=rl;

        ai1:=Taicpu.op_ref(A_PJxx,href);
        ai1.is_jmp:=true;
        hl:=nil;
        case cond of
          OC_EQ:
            ai1.SetCondition(C_EQ);
          OC_LT:
            ai1.SetCondition(C_LT);
          OC_LTE:
            ai1.SetCondition(C_LE);
          OC_NE:
            ai1.SetCondition(C_NE);
          OC_BE:
            ai1.SetCondition(C_BE);
          OC_B:
            ai1.SetCondition(C_B);
          else
            internalerror(2011082501);
        end;
        a_label(list,rl);
        list.concat(ai1);
        if assigned(hl) then
          a_label(list,hl);
      end;


    procedure tcgspc32.g_stackpointer_alloc(list: TAsmList; size: longint);
      begin
        internalerror(201201071);
      end;


    procedure tcg64fspc32.a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);
      begin
        if not(size in [OS_S64,OS_64]) then
          internalerror(2012102402);

        case op of
          OP_ADD:
            begin
              list.concat(taicpu.op_reg(A_LD,regdst.reglo));
              list.concat(taicpu.op_reg(A_ADD,regsrc.reglo));
              list.concat(taicpu.op_reg(A_ST,regdst.reglo));
              list.concat(taicpu.op_reg(A_LD,regdst.reghi));
              list.concat(taicpu.op_reg(A_ADC,regsrc.reghi));
              list.concat(taicpu.op_reg(A_ST,regdst.reghi));
            end;
          OP_SUB:
            begin
              list.concat(taicpu.op_reg(A_LD,regdst.reglo));
              list.concat(taicpu.op_reg(A_SUB,regsrc.reglo));
              list.concat(taicpu.op_reg(A_ST,regdst.reglo));
              list.concat(taicpu.op_reg(A_LD,regdst.reghi));
              list.concat(taicpu.op_reg(A_SBC,regsrc.reghi));
              list.concat(taicpu.op_reg(A_ST,regdst.reghi));
            end;
          OP_NEG:
            begin
              list.concat(taicpu.op_none(A_NUL));
              list.concat(taicpu.op_reg(A_SUB,regsrc.reglo));
              list.concat(taicpu.op_reg(A_ST,regdst.reglo));
              list.concat(taicpu.op_none(A_NUL));
              list.concat(taicpu.op_reg(A_SBC,regsrc.reghi));
              list.concat(taicpu.op_reg(A_ST,regdst.reghi));
            end;
          OP_NOT:
            begin
              list.concat(taicpu.op_const(A_LD,-1));
              list.concat(taicpu.op_reg(A_XOR,regsrc.reglo));
              list.concat(taicpu.op_reg(A_ST,regdst.reglo));
              list.concat(taicpu.op_const(A_LD,-1));
              list.concat(taicpu.op_reg(A_XOR,regsrc.reghi));
              list.concat(taicpu.op_reg(A_ST,regdst.reghi));
            end;
          OP_AND,
          OP_OR,
          OP_XOR:
            begin
              list.concat(taicpu.op_reg(A_LD,regdst.reglo));
              list.concat(taicpu.op_reg(TOpCG2AsmOp[op],regsrc.reglo));
              list.concat(taicpu.op_reg(A_ST,regdst.reglo));

              list.concat(taicpu.op_reg(A_LD,regdst.reghi));
              list.concat(taicpu.op_reg(TOpCG2AsmOp[op],regsrc.reghi));
              list.concat(taicpu.op_reg(A_ST,regdst.reghi));
            end;
          else
            begin
              writeln(op);
              Internalerror(2014041601);
            end;
        end;
      end;


    procedure tcg64fspc32.a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);
      var
        tmpreg : tregister64;
      begin
        if not(size in [OS_S64,OS_64]) then
          internalerror(2012102402);

        if ((value and $FFFFFFFF) = SarLongint((value shl 16) and $FFFFFFFF, 16)) and
           (((value shr 32) and $FFFFFFFF) = SarLongint((value shr 16) and $FFFF0000, 16)) then
          begin
            case op of
              OP_ADD:
                begin
                  list.concat(taicpu.op_reg(A_LD,reg.reglo));
                  list.concat(taicpu.op_const(A_ADD,value and $FFFFFFFF));
                  list.concat(taicpu.op_reg(A_ST,reg.reglo));

                  list.concat(taicpu.op_reg(A_LD,reg.reghi));
                  list.concat(taicpu.op_const(A_ADC,(value shr 32) and $FFFFFFFF));
                  list.concat(taicpu.op_reg(A_ST,reg.reghi));
                end;
              OP_SUB:
                begin
                  list.concat(taicpu.op_reg(A_LD,reg.reglo));
                  list.concat(taicpu.op_const(A_SUB,value and $FFFFFFFF));
                  list.concat(taicpu.op_reg(A_ST,reg.reglo));

                  list.concat(taicpu.op_reg(A_LD,reg.reghi));
                  list.concat(taicpu.op_const(A_SBC,(value shr 32) and $FFFFFFFF));
                  list.concat(taicpu.op_reg(A_ST,reg.reghi));
                end;
              OP_AND:
                begin
                  list.concat(taicpu.op_reg(A_LD,reg.reglo));
                  list.concat(taicpu.op_const(A_AND,value and $FFFFFFFF));
                  list.concat(taicpu.op_reg(A_ST,reg.reglo));

                  if ((value shr 32) and $FFFFFFFF) = 0 then
                    begin
                      list.concat(taicpu.op_none(A_NUL));
                      list.concat(taicpu.op_reg(A_ST,reg.reghi));
                    end
                  else
                    begin
                      list.concat(taicpu.op_reg(A_LD,reg.reghi));
                      list.concat(taicpu.op_const(A_AND,(value shr 32) and $FFFFFFFF));
                      list.concat(taicpu.op_reg(A_ST,reg.reghi));
                    end;
                end;
              OP_OR:
                begin
                  list.concat(taicpu.op_reg(A_LD,reg.reglo));
                  if (value and $FFFFFFFF) <> 0 then
                    list.concat(taicpu.op_const(A_ORR,value and $FFFFFFFF));
                  list.concat(taicpu.op_reg(A_ST,reg.reglo));

                  list.concat(taicpu.op_reg(A_LD,reg.reghi));
                  if ((value shr 32) and $FFFFFFFF) <> 0 then
                    list.concat(taicpu.op_const(A_ORR,(value shr 32) and $FFFFFFFF));
                  list.concat(taicpu.op_reg(A_ST,reg.reghi));
                end;
              OP_XOR:
                begin
                  list.concat(taicpu.op_reg(A_LD,reg.reglo));
                  if (value and $FFFFFFFF) <> 0 then
                    list.concat(taicpu.op_const(A_XOR,value and $FFFFFFFF));
                  list.concat(taicpu.op_reg(A_ST,reg.reglo));

                  list.concat(taicpu.op_reg(A_LD,reg.reghi));
                  if ((value shr 32) and $FFFFFFFF) <> 0 then
                    list.concat(taicpu.op_const(A_XOR,(value shr 32) and $FFFFFFFF));
                  list.concat(taicpu.op_reg(A_ST,reg.reghi));
                end;
              else
                begin
                  writeln(op);
                  Internalerror(2014041602);
                end;
            end;
          end
        else
          begin
            tmpreg.reglo:=cg.getintregister(list,OS_INT);
            tmpreg.reghi:=cg.getintregister(list,OS_INT);

            a_load64_const_reg(list,value,tmpreg);

            a_op64_reg_reg(list,op,size,tmpreg,reg);
          end;
      end;


    procedure create_codegen;
      begin
        cg:=tcgspc32.create;
        cg64:=tcg64fspc32.create;
      end;

end.
