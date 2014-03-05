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
        function getaddressregister(list:TAsmList):TRegister;override;

        procedure a_load_const_cgpara(list : TAsmList;size : tcgsize;a : tcgint;const paraloc : TCGPara);override;
        procedure a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);override;
        procedure a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);override;
        procedure a_load_reg_cgpara(list : TAsmList; size : tcgsize;r : tregister; const cgpara : tcgpara);override;

        procedure a_call_name(list : TAsmList;const s : string; weak: boolean);override;
        procedure a_call_reg(list : TAsmList;reg: tregister);override;

        procedure a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister); override;
        procedure a_op_reg_reg(list: TAsmList; Op: TOpCG; size: TCGSize; src, dst : TRegister); override;

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
        procedure a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : tcgint;reg : tregister;
          l : tasmlabel);override;
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
        function normalize_ref(list : TAsmList;ref : treference;
          tmpreg : tregister) : treference;

        procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
        procedure g_stackpointer_alloc(list : TAsmList;size : longint);override;
        procedure emit_mov(list: TAsmList;reg2: tregister; reg1: tregister);

        procedure a_adjust_sp(list: TAsmList; value: longint);
        function GetLoad(const ref : treference) : tasmop;
        function GetStore(const ref: treference): tasmop;

        procedure a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; size: TCGSize; src, dst: TRegister); override;
      protected
        procedure a_op_reg_reg_internal(list: TAsmList; Op: TOpCG; size: TCGSize; src, srchi, dst, dsthi: TRegister);
        procedure a_op_const_reg_internal(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg, reghi: TRegister);
      end;

      tcg64favr = class(tcg64f32)
        procedure a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);override;
        procedure a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);override;
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
       symconst,symsym,symtable,
       tgobj,rgobj,
       procinfo,cpupi,
       paramgr;


    procedure tcgavr.init_register_allocators;
      begin
        inherited init_register_allocators;
        rg[R_INTREGISTER]:=trgintcpu.create(R_INTREGISTER,R_SUBWHOLE,
            [RS_R8,RS_R9,
             RS_R10,RS_R11,RS_R12,RS_R13,RS_R14,RS_R15,RS_R16,RS_R17,RS_R18,RS_R19,
             RS_R20,RS_R21,RS_R22,RS_R23,RS_R24,RS_R25,
             RS_R2,RS_R3,RS_R4,RS_R5,RS_R6,RS_R7],first_int_imreg,[]);
        { rg[R_ADDRESSREGISTER]:=trgintcpu.create(R_ADDRESSREGISTER,R_SUBWHOLE,
            [RS_R26,RS_R30],first_int_imreg,[]); }
      end;


    procedure tcgavr.done_register_allocators;
      begin
        rg[R_INTREGISTER].free;
        // rg[R_ADDRESSREGISTER].free;
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


    function tcgavr.getaddressregister(list: TAsmList): TRegister;
      begin
       Result:=getintregister(list,OS_ADDR);
      end;


    procedure tcgavr.a_load_reg_cgpara(list : TAsmList;size : tcgsize;r : tregister;const cgpara : tcgpara);

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
                  reference_reset_base(ref,paraloc^.reference.index,paraloc^.reference.offset,2);
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
{        if use_push(cgpara) then
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
        else }
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


    procedure tcgavr.a_load_const_cgpara(list : TAsmList;size : tcgsize;a : tcgint;const paraloc : TCGPara);
      var
        i : longint;
        hp : PCGParaLocation;
      begin
        if not(tcgsize2size[paraloc.Size] in [1..4]) then
          internalerror(2014011101);

        hp:=paraloc.location;

        for i:=1 to tcgsize2size[paraloc.Size] do
          begin
            if not(assigned(hp)) or
              (tcgsize2size[hp^.size]<>1) or
              (hp^.shiftval<>0) then
              internalerror(2014011105);
             case hp^.loc of
               LOC_REGISTER,LOC_CREGISTER:
                 a_load_const_reg(list,hp^.size,(a shr (i-1)) and $ff,hp^.register);
               LOC_REFERENCE,LOC_CREFERENCE:
                 begin
                   list.concat(taicpu.op_const(A_PUSH,(a shr (i-1)) and $ff));
                 end;
               else
                 internalerror(2002071004);
            end;
            hp:=hp^.Next;
          end;
        if assigned(hp) then
          internalerror(2014011104);
      end;


    procedure tcgavr.a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);
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


    procedure tcgavr.a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);
      var
        tmpreg: tregister;
      begin
        tmpreg:=getaddressregister(list);
        a_loadaddr_ref_reg(list,r,tmpreg);
        a_load_reg_cgpara(list,OS_ADDR,tmpreg,paraloc);
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


     procedure tcgavr.a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister);
       begin
         if not(size in [OS_S8,OS_8,OS_S16,OS_16,OS_S32,OS_32]) then
           internalerror(2012102403);
         a_op_const_reg_internal(list,Op,size,a,reg,NR_NO);
       end;


     procedure tcgavr.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: TCGSize; src, dst : TRegister);
       begin
         if not(size in [OS_S8,OS_8,OS_S16,OS_16,OS_S32,OS_32]) then
           internalerror(2012102401);
         a_op_reg_reg_internal(list,Op,size,src,NR_NO,dst,NR_NO);
       end;


     procedure tcgavr.a_op_reg_reg_internal(list : TAsmList; Op: TOpCG; size: TCGSize; src, srchi, dst, dsthi: TRegister);
       var
         countreg,
         tmpreg: tregister;
         i : integer;
         instr : taicpu;
         paraloc1,paraloc2,paraloc3 : TCGPara;
         l1,l2 : tasmlabel;
         pd : tprocdef;

       procedure NextSrcDst;
         begin
           if i=5 then
             begin
               dst:=dsthi;
               src:=srchi;
             end
           else
             begin
               dst:=GetNextReg(dst);
               src:=GetNextReg(src);
             end;
         end;

       { iterates TmpReg through all registers of dst }
       procedure NextTmp;
         begin
           if i=5 then
             tmpreg:=dsthi
           else
             tmpreg:=GetNextReg(tmpreg);
         end;

      begin
         case op of
           OP_ADD:
             begin
               list.concat(taicpu.op_reg_reg(A_ADD,dst,src));
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       NextSrcDst;
                       list.concat(taicpu.op_reg_reg(A_ADC,dst,src));
                     end;
                 end;
             end;

           OP_SUB:
             begin
               list.concat(taicpu.op_reg_reg(A_SUB,dst,src));
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       NextSrcDst;
                       list.concat(taicpu.op_reg_reg(A_SBC,dst,src));
                     end;
                 end;
             end;

           OP_NEG:
             begin
               if src<>dst then
                 begin
                   if size in [OS_S64,OS_64] then
                     begin
                       a_load_reg_reg(list,OS_32,OS_32,src,dst);
                       a_load_reg_reg(list,OS_32,OS_32,srchi,dsthi);
                     end
                   else
                     a_load_reg_reg(list,size,size,src,dst);
                 end;

               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   tmpreg:=GetNextReg(dst);
                   for i:=2 to tcgsize2size[size] do
                     begin
                       list.concat(taicpu.op_reg(A_COM,tmpreg));
                       NextTmp;
                     end;
                   list.concat(taicpu.op_reg(A_NEG,dst));
                   tmpreg:=GetNextReg(dst);
                   for i:=2 to tcgsize2size[size] do
                     begin
                       list.concat(taicpu.op_reg_const(A_SBCI,tmpreg,-1));
                       NextTmp;
                   end;
                 end;
             end;

           OP_NOT:
             begin
               for i:=1 to tcgsize2size[size] do
                 begin
                   if src<>dst then
                     a_load_reg_reg(list,OS_8,OS_8,src,dst);
                   list.concat(taicpu.op_reg(A_COM,dst));
                   NextSrcDst;
                 end;
             end;

           OP_MUL,OP_IMUL:
             begin
               if size in [OS_8,OS_S8] then
                 list.concat(taicpu.op_reg_reg(topcg2asmop[op],dst,src))
               else if size=OS_16 then
                 begin
                   pd:=search_system_proc('fpc_mul_word');
                   paraloc1.init;
                   paraloc2.init;
                   paraloc3.init;
                   paramanager.getintparaloc(pd,1,paraloc1);
                   paramanager.getintparaloc(pd,2,paraloc2);
                   paramanager.getintparaloc(pd,3,paraloc3);
                   a_load_const_cgpara(list,OS_8,0,paraloc3);
                   a_load_reg_cgpara(list,OS_16,src,paraloc2);
                   a_load_reg_cgpara(list,OS_16,dst,paraloc1);
                   paramanager.freecgpara(list,paraloc3);
                   paramanager.freecgpara(list,paraloc2);
                   paramanager.freecgpara(list,paraloc1);
                   alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
                   a_call_name(list,'FPC_MUL_WORD',false);
                   dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
                   cg.a_reg_alloc(list,NR_FUNCTION_RESULT_REG);
                   cg.a_load_reg_reg(list,OS_16,OS_16,NR_FUNCTION_RESULT_REG,dst);
                   paraloc3.done;
                   paraloc2.done;
                   paraloc1.done;
                 end
               else
                 internalerror(2011022002);
             end;

           OP_DIV,OP_IDIV:
             { special stuff, needs separate handling inside code }
             { generator                                          }
             internalerror(2011022001);

           OP_SHR,OP_SHL,OP_SAR,OP_ROL,OP_ROR:
             begin
               current_asmdata.getjumplabel(l1);
               current_asmdata.getjumplabel(l2);
               countreg:=getintregister(list,OS_8);
               a_load_reg_reg(list,size,OS_8,src,countreg);
               list.concat(taicpu.op_reg_const(A_CPI,countreg,0));
               a_jmp_flags(list,F_EQ,l2);
               cg.a_label(list,l1);
               case op of
                 OP_SHR:
                   list.concat(taicpu.op_reg(A_LSR,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-1)));
                 OP_SHL:
                   list.concat(taicpu.op_reg(A_LSL,dst));
                 OP_SAR:
                   list.concat(taicpu.op_reg(A_ASR,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-1)));
                 OP_ROR:
                   begin
                     { load carry? }
                     if not(size in [OS_8,OS_S8]) then
                       begin
                         list.concat(taicpu.op_none(A_CLC));
                         list.concat(taicpu.op_reg_const(A_SBRC,src,0));
                         list.concat(taicpu.op_none(A_SEC));
                       end;
                     list.concat(taicpu.op_reg(A_ROR,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-1)));
                   end;
                 OP_ROL:
                   begin
                     { load carry? }
                     if not(size in [OS_8,OS_S8]) then
                       begin
                         list.concat(taicpu.op_none(A_CLC));
                         list.concat(taicpu.op_reg_const(A_SBRC,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-1),7));
                         list.concat(taicpu.op_none(A_SEC));
                       end;
                     list.concat(taicpu.op_reg(A_ROL,dst))
                   end;
                 else
                   internalerror(2011030901);
               end;
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       case op of
                         OP_ROR,
                         OP_SHR:
                           list.concat(taicpu.op_reg(A_ROR,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-i)));
                         OP_ROL,
                         OP_SHL:
                           list.concat(taicpu.op_reg(A_ROL,GetOffsetReg64(dst,dsthi,i-1)));
                         OP_SAR:
                           list.concat(taicpu.op_reg(A_ROR,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-i)));
                         else
                           internalerror(2011030902);
                       end;
                   end;
                 end;

               a_op_const_reg(list,OP_SUB,OS_8,1,countreg);
               a_jmp_flags(list,F_NE,l1);
               // keep registers alive
               list.concat(taicpu.op_reg_reg(A_MOV,countreg,countreg));
               cg.a_label(list,l2);
             end;

           OP_AND,OP_OR,OP_XOR:
             begin
                for i:=1 to tcgsize2size[size] do
                  begin
                    list.concat(taicpu.op_reg_reg(topcg2asmop[op],dst,src));
                    NextSrcDst;
                  end;
             end;
           else
             internalerror(2011022004);
         end;
       end;

     procedure tcgavr.a_op_const_reg_internal(list: TAsmList; Op: TOpCG;
      size: TCGSize; a: tcgint; reg, reghi: TRegister);

       var
         mask : qword;
         shift : byte;
         i : byte;
         tmpreg : tregister;
         tmpreg64 : tregister64;

      procedure NextReg;
        begin
          if i=5 then
            reg:=reghi
          else
            reg:=GetNextReg(reg);
        end;

       begin
         mask:=$ff;
         shift:=0;
         case op of
           OP_OR:
             begin
               for i:=1 to tcgsize2size[size] do
                 begin
                   list.concat(taicpu.op_reg_const(A_ORI,reg,(a and mask) shr shift));
                   NextReg;
                   mask:=mask shl 8;
                   inc(shift,8);
                 end;
             end;
           OP_AND:
             begin
               for i:=1 to tcgsize2size[size] do
                 begin
                   list.concat(taicpu.op_reg_const(A_ANDI,reg,(a and mask) shr shift));
                   NextReg;
                   mask:=mask shl 8;
                   inc(shift,8);
                 end;
             end;
           OP_SUB:
             begin
               list.concat(taicpu.op_reg_const(A_SUBI,reg,a and mask));
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       NextReg;
                       mask:=mask shl 8;
                       inc(shift,8);
                       list.concat(taicpu.op_reg_const(A_SBCI,reg,(a and mask) shr shift));
                     end;
                 end;
             end;
           {OP_ADD:
             begin
               list.concat(taicpu.op_reg_const(A_SUBI,reg,(-a) and mask));
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       NextReg;
                       mask:=mask shl 8;
                       inc(shift,8);
                       list.concat(taicpu.op_reg_const(A_ADC,reg,(a and mask) shr shift));
                     end;
                 end;
             end; }
         else
           begin
             if size in [OS_64,OS_S64] then
               begin
                 tmpreg64.reglo:=getintregister(list,OS_32);
                 tmpreg64.reghi:=getintregister(list,OS_32);
                 cg64.a_load64_const_reg(list,a,tmpreg64);
                 cg64.a_op64_reg_reg(list,op,size,tmpreg64,joinreg64(reg,reghi));
               end
             else
               begin
                 tmpreg:=getintregister(list,size);
                 a_load_const_reg(list,size,a,tmpreg);
                 a_op_reg_reg(list,op,size,tmpreg,reg);
               end;
           end;
       end;
     end;


     procedure tcgavr.a_load_const_reg(list : TAsmList; size: tcgsize; a : tcgint;reg : tregister);
       var
         mask : qword;
         shift : byte;
         i : byte;
       begin
         mask:=$ff;
         shift:=0;
         for i:=1 to tcgsize2size[size] do
           begin
             if ((qword(a) and mask) shr shift)=0 then
               emit_mov(list,reg,NR_R1)
             else
               list.concat(taicpu.op_reg_const(A_LDI,reg,(qword(a) and mask) shr shift));

             mask:=mask shl 8;
             inc(shift,8);
             reg:=GetNextReg(reg);
           end;
       end;


    function tcgavr.normalize_ref(list:TAsmList;ref: treference;tmpreg : tregister) : treference;

      procedure maybegetcpuregister(list:tasmlist;reg : tregister);
        begin
          { allocate the register only, if a cpu register is passed }
          if getsupreg(reg)<first_int_imreg then
            getcpuregister(list,reg);
        end;

      var
        tmpref : treference;
        l : tasmlabel;
      begin
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
            reference_reset(tmpref,0);
            tmpref.symbol:=ref.symbol;
            tmpref.offset:=ref.offset;
            tmpref.refaddr:=addr_lo8;
            maybegetcpuregister(list,tmpreg);
            list.concat(taicpu.op_reg_ref(A_LDI,tmpreg,tmpref));
            tmpref.refaddr:=addr_hi8;
            maybegetcpuregister(list,GetNextReg(tmpreg));
            list.concat(taicpu.op_reg_ref(A_LDI,GetNextReg(tmpreg),tmpref));
            if (ref.base<>NR_NO) then
              begin
                list.concat(taicpu.op_reg_reg(A_ADD,tmpreg,ref.base));
                list.concat(taicpu.op_reg_reg(A_ADC,GetNextReg(tmpreg),GetNextReg(ref.base)));
              end;
            if (ref.index<>NR_NO) then
              begin
                list.concat(taicpu.op_reg_reg(A_ADD,tmpreg,ref.index));
                list.concat(taicpu.op_reg_reg(A_ADC,GetNextReg(tmpreg),GetNextReg(ref.index)));
              end;
            ref.symbol:=nil;
            ref.offset:=0;
            ref.base:=tmpreg;
            ref.index:=NR_NO;
          end
        else if (ref.base<>NR_NO) and (ref.index<>NR_NO) then
          begin
            maybegetcpuregister(list,tmpreg);
            emit_mov(list,tmpreg,ref.base);
            maybegetcpuregister(list,GetNextReg(tmpreg));
            emit_mov(list,GetNextReg(tmpreg),GetNextReg(ref.base));
            list.concat(taicpu.op_reg_reg(A_ADD,tmpreg,ref.index));
            list.concat(taicpu.op_reg_reg(A_ADC,GetNextReg(tmpreg),GetNextReg(ref.index)));
            ref.base:=tmpreg;
            ref.index:=NR_NO;
          end
        else if (ref.base<>NR_NO) then
          begin
            maybegetcpuregister(list,tmpreg);
            emit_mov(list,tmpreg,ref.base);
            maybegetcpuregister(list,GetNextReg(tmpreg));
            emit_mov(list,GetNextReg(tmpreg),GetNextReg(ref.base));
            ref.base:=tmpreg;
            ref.index:=NR_NO;
          end
        else if (ref.index<>NR_NO) then
          begin
            maybegetcpuregister(list,tmpreg);
            emit_mov(list,tmpreg,ref.index);
            maybegetcpuregister(list,GetNextReg(tmpreg));
            emit_mov(list,GetNextReg(tmpreg),GetNextReg(ref.index));
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
         QuickRef : Boolean;
       begin
         QuickRef:=false;
         if not((Ref.addressmode=AM_UNCHANGED) and
                (Ref.symbol=nil) and
                ((Ref.base=NR_R28) or
                 (Ref.base=NR_R29)) and
                 (Ref.Index=NR_No) and
                 (Ref.Offset in [0..64-tcgsize2size[tosize]])) and
           not((Ref.Base=NR_NO) and (Ref.Index=NR_NO)) then
           href:=normalize_ref(list,Ref,NR_R30)
         else
           begin
             QuickRef:=true;
             href:=Ref;
           end;

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
                   if not(QuickRef) and (tcgsize2size[tosize]>1) then
                     href.addressmode:=AM_POSTINCREMENT;

                   list.concat(taicpu.op_ref_reg(GetStore(href),href,reg));
                   for i:=2 to tcgsize2size[tosize] do
                     begin
                       if QuickRef then
                         inc(href.offset);

                       if not(QuickRef) and (i<tcgsize2size[fromsize]) then
                         href.addressmode:=AM_POSTINCREMENT
                       else
                         href.addressmode:=AM_UNCHANGED;

                       list.concat(taicpu.op_ref_reg(GetStore(href),href,NR_R1));
                     end;
                 end;
               OS_S8:
                 begin
                   if not(QuickRef) and (tcgsize2size[tosize]>1) then
                     href.addressmode:=AM_POSTINCREMENT;
                   list.concat(taicpu.op_ref_reg(GetStore(href),href,reg));

                   if tcgsize2size[tosize]>1 then
                     begin
                       tmpreg:=getintregister(list,OS_8);
                       list.concat(taicpu.op_reg(A_CLR,tmpreg));
                       list.concat(taicpu.op_reg_const(A_SBRC,reg,7));
                       list.concat(taicpu.op_reg(A_COM,tmpreg));
                       for i:=2 to tcgsize2size[tosize] do
                         begin
                           if QuickRef then
                             inc(href.offset);

                           if not(QuickRef) and (i<tcgsize2size[fromsize]) then
                             href.addressmode:=AM_POSTINCREMENT
                           else
                             href.addressmode:=AM_UNCHANGED;
                           list.concat(taicpu.op_ref_reg(GetStore(href),href,tmpreg));
                         end;
                     end;
                 end;
               OS_16:
                 begin
                   if not(QuickRef) and (tcgsize2size[tosize]>1) then
                     href.addressmode:=AM_POSTINCREMENT;

                   list.concat(taicpu.op_ref_reg(GetStore(href),href,reg));
                   if QuickRef then
                     inc(href.offset)
                   else if not(QuickRef) and (tcgsize2size[fromsize]>2) then
                     href.addressmode:=AM_POSTINCREMENT
                   else
                     href.addressmode:=AM_UNCHANGED;

                   reg:=GetNextReg(reg);
                   list.concat(taicpu.op_ref_reg(GetStore(href),href,reg));

                   for i:=3 to tcgsize2size[tosize] do
                     begin
                       if QuickRef then
                         inc(href.offset);

                       if not(QuickRef) and (i<tcgsize2size[fromsize]) then
                         href.addressmode:=AM_POSTINCREMENT
                       else
                         href.addressmode:=AM_UNCHANGED;

                       list.concat(taicpu.op_ref_reg(GetStore(href),href,NR_R1));
                     end;
                 end;
               OS_S16:
                 begin
                   if not(QuickRef) and (tcgsize2size[tosize]>1) then
                     href.addressmode:=AM_POSTINCREMENT;

                   list.concat(taicpu.op_ref_reg(GetStore(href),href,reg));
                   if QuickRef then
                     inc(href.offset)
                   else if not(QuickRef) and (tcgsize2size[fromsize]>2) then
                     href.addressmode:=AM_POSTINCREMENT
                   else
                     href.addressmode:=AM_UNCHANGED;

                   reg:=GetNextReg(reg);
                   list.concat(taicpu.op_ref_reg(GetStore(href),href,reg));

                   if tcgsize2size[tosize]>2 then
                     begin
                       tmpreg:=getintregister(list,OS_8);
                       list.concat(taicpu.op_reg(A_CLR,tmpreg));
                       list.concat(taicpu.op_reg_const(A_SBRC,reg,7));
                       list.concat(taicpu.op_reg(A_COM,tmpreg));
                       for i:=3 to tcgsize2size[tosize] do
                         begin
                           if QuickRef then
                             inc(href.offset);

                           if not(QuickRef) and (i<tcgsize2size[fromsize]) then
                             href.addressmode:=AM_POSTINCREMENT
                           else
                             href.addressmode:=AM_UNCHANGED;
                           list.concat(taicpu.op_ref_reg(GetStore(href),href,tmpreg));
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
                   if not(QuickRef) and (i<tcgsize2size[fromsize]) then
                     href.addressmode:=AM_POSTINCREMENT
                   else
                     href.addressmode:=AM_UNCHANGED;

                 list.concat(taicpu.op_ref_reg(GetStore(href),href,reg));

                 if QuickRef then
                   inc(href.offset);

                 reg:=GetNextReg(reg);
               end;
           end;

         if not(QuickRef) then
           begin
             ungetcpuregister(list,href.base);
             ungetcpuregister(list,GetNextReg(href.base));
           end;
       end;


     procedure tcgavr.a_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;
       const Ref : treference;reg : tregister);
       var
         href : treference;
         conv_done: boolean;
         tmpreg : tregister;
         i : integer;
         QuickRef : boolean;
       begin
         QuickRef:=false;
         if not((Ref.addressmode=AM_UNCHANGED) and
                (Ref.symbol=nil) and
                ((Ref.base=NR_R28) or
                 (Ref.base=NR_R29)) and
                 (Ref.Index=NR_No) and
                 (Ref.Offset in [0..64-tcgsize2size[fromsize]])) and
           not((Ref.Base=NR_NO) and (Ref.Index=NR_NO)) then
           href:=normalize_ref(list,Ref,NR_R30)
         else
           begin
             QuickRef:=true;
             href:=Ref;
           end;

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
                   list.concat(taicpu.op_reg_ref(GetLoad(href),reg,href));
                   for i:=2 to tcgsize2size[tosize] do
                     begin
                       reg:=GetNextReg(reg);
                       list.concat(taicpu.op_reg(A_CLR,reg));
                     end;
                 end;
               OS_S8:
                 begin
                   list.concat(taicpu.op_reg_ref(GetLoad(href),reg,href));
                   tmpreg:=reg;

                   if tcgsize2size[tosize]>1 then
                     begin
                       reg:=GetNextReg(reg);
                       list.concat(taicpu.op_reg(A_CLR,reg));
                       list.concat(taicpu.op_reg_const(A_SBRC,tmpreg,7));
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
                   if not(QuickRef) then
                     href.addressmode:=AM_POSTINCREMENT;
                   list.concat(taicpu.op_reg_ref(GetLoad(href),reg,href));

                   if QuickRef then
                     inc(href.offset);
                   href.addressmode:=AM_UNCHANGED;

                   reg:=GetNextReg(reg);
                   list.concat(taicpu.op_reg_ref(GetLoad(href),reg,href));

                   for i:=3 to tcgsize2size[tosize] do
                     begin
                       reg:=GetNextReg(reg);
                       list.concat(taicpu.op_reg(A_CLR,reg));
                     end;
                 end;
               OS_S16:
                 begin
                   if not(QuickRef) then
                     href.addressmode:=AM_POSTINCREMENT;
                   list.concat(taicpu.op_reg_ref(GetLoad(href),reg,href));
                   if QuickRef then
                     inc(href.offset);
                   href.addressmode:=AM_UNCHANGED;

                   reg:=GetNextReg(reg);
                   list.concat(taicpu.op_reg_ref(GetLoad(href),reg,href));
                   tmpreg:=reg;

                   reg:=GetNextReg(reg);
                   list.concat(taicpu.op_reg(A_CLR,reg));
                   list.concat(taicpu.op_reg_const(A_SBRC,tmpreg,7));
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
                 if not(QuickRef) and (i<tcgsize2size[fromsize]) then
                   href.addressmode:=AM_POSTINCREMENT
                 else
                   href.addressmode:=AM_UNCHANGED;

                 list.concat(taicpu.op_reg_ref(GetLoad(href),reg,href));

                 if QuickRef then
                   inc(href.offset);

                 reg:=GetNextReg(reg);
               end;
           end;

         if not(QuickRef) then
           begin
             ungetcpuregister(list,href.base);
             ungetcpuregister(list,GetNextReg(href.base));
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
                   emit_mov(list,reg2,reg1);

                   if tcgsize2size[tosize]>1 then
                     begin
                       reg2:=GetNextReg(reg2);
                       list.concat(taicpu.op_reg(A_CLR,reg2));
                       list.concat(taicpu.op_reg_const(A_SBRC,reg1,7));
                       list.concat(taicpu.op_reg(A_COM,reg2));
                       tmpreg:=reg2;
                       for i:=3 to tcgsize2size[tosize] do
                         begin
                           reg2:=GetNextReg(reg2);
                           emit_mov(list,reg2,tmpreg);
                         end;
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
                   emit_mov(list,reg2,reg1);

                   reg1:=GetNextReg(reg1);
                   reg2:=GetNextReg(reg2);
                   emit_mov(list,reg2,reg1);

                   if tcgsize2size[tosize]>2 then
                     begin
                       reg2:=GetNextReg(reg2);
                       list.concat(taicpu.op_reg(A_CLR,reg2));
                       list.concat(taicpu.op_reg_const(A_SBRC,reg1,7));
                       list.concat(taicpu.op_reg(A_COM,reg2));
                       tmpreg:=reg2;
                       for i:=4 to tcgsize2size[tosize] do
                         begin
                           reg2:=GetNextReg(reg2);
                           emit_mov(list,reg2,tmpreg);
                         end;
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


     procedure tcgavr.a_loadfpu_reg_reg(list: TAsmList; fromsize,tosize: tcgsize; reg1, reg2: tregister);
       begin
         internalerror(2012010702);
       end;


     procedure tcgavr.a_loadfpu_ref_reg(list: TAsmList; fromsize,tosize: tcgsize; const ref: treference; reg: tregister);
       begin
         internalerror(2012010703);
       end;


     procedure tcgavr.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference);
       begin
         internalerror(2012010704);
       end;


    {  comparison operations }
    procedure tcgavr.a_cmp_const_reg_label(list : TAsmList;size : tcgsize;
      cmp_op : topcmp;a : tcgint;reg : tregister;l : tasmlabel);
      var
        swapped : boolean;
        tmpreg : tregister;
        i : byte;
      begin
        if a=0 then
          begin
            swapped:=false;
            { swap parameters? }
            case cmp_op of
              OC_GT:
                begin
                  swapped:=true;
                  cmp_op:=OC_LT;
                end;
              OC_LTE:
                begin
                  swapped:=true;
                  cmp_op:=OC_GTE;
                end;
              OC_BE:
                begin
                  swapped:=true;
                  cmp_op:=OC_AE;
                end;
              OC_A:
                begin
                  swapped:=true;
                  cmp_op:=OC_B;
                end;
            end;

            if swapped then
              list.concat(taicpu.op_reg_reg(A_CP,reg,NR_R1))
            else
              list.concat(taicpu.op_reg_reg(A_CP,NR_R1,reg));

            for i:=2 to tcgsize2size[size] do
              begin
                reg:=GetNextReg(reg);
                if swapped then
                  list.concat(taicpu.op_reg_reg(A_CPC,reg,NR_R1))
                else
                  list.concat(taicpu.op_reg_reg(A_CPC,NR_R1,reg));
              end;

            a_jmp_cond(list,cmp_op,l);
          end
        else
          inherited a_cmp_const_reg_label(list,size,cmp_op,a,reg,l);
      end;


    procedure tcgavr.a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;
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
          OC_LTE:
            begin
              swapped:=true;
              cmp_op:=OC_GTE;
            end;
          OC_BE:
            begin
              swapped:=true;
              cmp_op:=OC_AE;
            end;
          OC_A:
            begin
              swapped:=true;
              cmp_op:=OC_B;
            end;
        end;
        if swapped then
          begin
            tmpreg:=reg1;
            reg1:=reg2;
            reg2:=tmpreg;
          end;
        list.concat(taicpu.op_reg_reg(A_CP,reg2,reg1));

        for i:=2 to tcgsize2size[size] do
          begin
            reg1:=GetNextReg(reg1);
            reg2:=GetNextReg(reg2);
            list.concat(taicpu.op_reg_reg(A_CPC,reg2,reg1));
          end;

        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgavr.a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; size: TCGSize; src, dst: TRegister);
      begin
        Comment(V_Error,'tcgarm.a_bit_scan_reg_reg method not implemented');
      end;


    procedure tcgavr.a_jmp_name(list : TAsmList;const s : string);
      var
        ai : taicpu;
      begin
        ai:=taicpu.op_sym(A_JMP,current_asmdata.RefAsmSymbol(s));
        ai.is_jmp:=true;
        list.concat(ai);
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
            list.concat(taicpu.op_reg_const(A_LDI,reg,1));
            a_jmp_flags(list,f,l);
            list.concat(taicpu.op_reg(A_CLR,reg));
          end;
        cg.a_label(list,l);
      end;


    procedure tcgavr.a_adjust_sp(list : TAsmList; value : longint);
      var
        i : integer;
      begin
        case value of
          0:
            ;
          {-14..-1:
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
            end;}
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


    function tcgavr.GetLoad(const ref: treference) : tasmop;
      begin
        if (ref.base=NR_NO) and (ref.index=NR_NO) then
          result:=A_LDS
        else if (ref.base<>NR_NO) and (ref.offset<>0) then
          result:=A_LDD
        else
          result:=A_LD;
      end;


    function tcgavr.GetStore(const ref: treference) : tasmop;
      begin
        if (ref.base=NR_NO) and (ref.index=NR_NO) then
          result:=A_STS
        else if (ref.base<>NR_NO) and (ref.offset<>0) then
          result:=A_STD
        else
          result:=A_ST;
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
                regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);
                if current_procinfo.framepointer<>NR_STACK_POINTER_REG then
                  regs:=regs+[RS_R28,RS_R29];

                for reg:=RS_R0 to RS_R31 do
                  if reg in regs then
                    list.concat(taicpu.op_reg(A_POP,newreg(R_INTREGISTER,reg,R_SUBWHOLE)));

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
      var
        tmpref : treference;
      begin
         if ref.addressmode<>AM_UNCHANGED then
           internalerror(2011021701);

        if assigned(ref.symbol) or (ref.offset<>0) then
          begin
            reference_reset(tmpref,0);
            tmpref.symbol:=ref.symbol;
            tmpref.offset:=ref.offset;
            tmpref.refaddr:=addr_lo8;
            list.concat(taicpu.op_reg_ref(A_LDI,r,tmpref));
            tmpref.refaddr:=addr_hi8;
            list.concat(taicpu.op_reg_ref(A_LDI,GetNextReg(r),tmpref));
            if (ref.base<>NR_NO) then
              begin
                list.concat(taicpu.op_reg_reg(A_ADD,r,ref.base));
                list.concat(taicpu.op_reg_reg(A_ADC,GetNextReg(r),GetNextReg(ref.base)));
              end;
            if (ref.index<>NR_NO) then
              begin
                list.concat(taicpu.op_reg_reg(A_ADD,r,ref.index));
                list.concat(taicpu.op_reg_reg(A_ADC,GetNextReg(r),GetNextReg(ref.index)));
              end;
          end
        else if (ref.base<>NR_NO)then
          begin
            emit_mov(list,r,ref.base);
            emit_mov(list,GetNextReg(r),GetNextReg(ref.base));
            if (ref.index<>NR_NO) then
              begin
                list.concat(taicpu.op_reg_reg(A_ADD,r,ref.index));
                list.concat(taicpu.op_reg_reg(A_ADC,GetNextReg(r),GetNextReg(ref.index)));
              end;
          end
        else if (ref.index<>NR_NO) then
          begin
            emit_mov(list,r,ref.index);
            emit_mov(list,GetNextReg(r),GetNextReg(ref.index));
          end;
      end;


    procedure tcgavr.fixref(list : TAsmList;var ref : treference);
      begin
        internalerror(2011021320);
      end;


    procedure tcgavr.g_concatcopy_move(list : TAsmList;const source,dest : treference;len : tcgint);
      var
        paraloc1,paraloc2,paraloc3 : TCGPara;
        pd : tprocdef;
      begin
        pd:=search_system_proc('MOVE');
        paraloc1.init;
        paraloc2.init;
        paraloc3.init;
        paramanager.getintparaloc(pd,1,paraloc1);
        paramanager.getintparaloc(pd,2,paraloc2);
        paramanager.getintparaloc(pd,3,paraloc3);
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


    procedure tcgavr.g_concatcopy(list : TAsmList;const source,dest : treference;len : tcgint);
      var
        countreg,tmpreg : tregister;
        srcref,dstref : treference;
        copysize,countregsize : tcgsize;
        l : TAsmLabel;
        i : longint;
        SrcQuickRef, DestQuickRef : Boolean;
      begin
        if len>16 then
          begin
            current_asmdata.getjumplabel(l);

            reference_reset(srcref,0);
            reference_reset(dstref,0);
            srcref.base:=NR_R30;
            srcref.addressmode:=AM_POSTINCREMENT;
            dstref.base:=NR_R26;
            dstref.addressmode:=AM_POSTINCREMENT;

            copysize:=OS_8;
            if len<256 then
              countregsize:=OS_8
            else if len<65536 then
              countregsize:=OS_16
            else
              internalerror(2011022007);
            countreg:=getintregister(list,countregsize);
            a_load_const_reg(list,countregsize,len,countreg);
            a_loadaddr_ref_reg(list,source,NR_R30);
            tmpreg:=getaddressregister(list);
            a_loadaddr_ref_reg(list,dest,tmpreg);

            { X is used for spilling code so we can load it
              only by a push/pop sequence, this can be
              optimized later on by the peephole optimizer
            }
            list.concat(taicpu.op_reg(A_PUSH,tmpreg));
            list.concat(taicpu.op_reg(A_PUSH,GetNextReg(tmpreg)));
            list.concat(taicpu.op_reg(A_POP,NR_R27));
            list.concat(taicpu.op_reg(A_POP,NR_R26));
            cg.a_label(list,l);
            list.concat(taicpu.op_reg_ref(GetLoad(srcref),NR_R0,srcref));
            list.concat(taicpu.op_ref_reg(GetStore(dstref),dstref,NR_R0));
            a_op_const_reg(list,OP_SUB,countregsize,1,countreg);
            a_jmp_flags(list,F_NE,l);
            // keep registers alive
            list.concat(taicpu.op_reg_reg(A_MOV,countreg,countreg));
          end
        else
          begin
            SrcQuickRef:=false;
            DestQuickRef:=false;
            if not((source.addressmode=AM_UNCHANGED) and
                   (source.symbol=nil) and
                   ((source.base=NR_R28) or
                    (source.base=NR_R29)) and
                    (source.Index=NR_NO) and
                    (source.Offset in [0..64-len])) and
              not((source.Base=NR_NO) and (source.Index=NR_NO)) then
              srcref:=normalize_ref(list,source,NR_R30)
            else
              begin
                SrcQuickRef:=true;
                srcref:=source;
              end;

            if not((dest.addressmode=AM_UNCHANGED) and
                   (dest.symbol=nil) and
                   ((dest.base=NR_R28) or
                    (dest.base=NR_R29)) and
                    (dest.Index=NR_No) and
                    (dest.Offset in [0..64-len])) and
              not((dest.Base=NR_NO) and (dest.Index=NR_NO)) then
              begin
                if not(SrcQuickRef) then
                  begin
                    tmpreg:=getaddressregister(list);
                    dstref:=normalize_ref(list,dest,tmpreg);

                    { X is used for spilling code so we can load it
                      only by a push/pop sequence, this can be
                      optimized later on by the peephole optimizer
                    }
                    list.concat(taicpu.op_reg(A_PUSH,tmpreg));
                    list.concat(taicpu.op_reg(A_PUSH,GetNextReg(tmpreg)));
                    list.concat(taicpu.op_reg(A_POP,NR_R27));
                    list.concat(taicpu.op_reg(A_POP,NR_R26));
                    dstref.base:=NR_R26;
                  end
                else
                  dstref:=normalize_ref(list,dest,NR_R30);
              end
            else
              begin
                DestQuickRef:=true;
                dstref:=dest;
              end;

            for i:=1 to len do
              begin
                if not(SrcQuickRef) and (i<len) then
                  srcref.addressmode:=AM_POSTINCREMENT
                else
                  srcref.addressmode:=AM_UNCHANGED;

                if not(DestQuickRef) and (i<len) then
                  dstref.addressmode:=AM_POSTINCREMENT
                else
                  dstref.addressmode:=AM_UNCHANGED;

                list.concat(taicpu.op_reg_ref(GetLoad(srcref),NR_R0,srcref));
                list.concat(taicpu.op_ref_reg(GetStore(dstref),dstref,NR_R0));

                if SrcQuickRef then
                  inc(srcref.offset);
                if DestQuickRef then
                  inc(dstref.offset);
              end;
            if not(SrcQuickRef) then
              begin
                ungetcpuregister(list,srcref.base);
                ungetcpuregister(list,GetNextReg(srcref.base));
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
                (torddef(def).ordtype in [u64bit,u16bit,u32bit,u8bit,uchar,
                                          pasbool8,pasbool16,pasbool32,pasbool64]))) then
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


    procedure tcgavr.g_save_registers(list: TAsmList);
      begin
        { this is done by the entry code }
      end;


    procedure tcgavr.g_restore_registers(list: TAsmList);
      begin
        { this is done by the exit code }
      end;


    procedure tcgavr.a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
      var
        ai1,ai2 : taicpu;
        hl : TAsmLabel;
      begin
        ai1:=Taicpu.Op_sym(A_BRxx,l);
        ai1.is_jmp:=true;
        hl:=nil;
        case cond of
          OC_EQ:
            ai1.SetCondition(C_EQ);
          OC_GT:
            begin
              { emulate GT }
              current_asmdata.getjumplabel(hl);
              ai2:=Taicpu.Op_Sym(A_BRxx,hl);
              ai2.SetCondition(C_EQ);
              ai2.is_jmp:=true;
              list.concat(ai2);

              ai1.SetCondition(C_GE);
            end;
          OC_LT:
            ai1.SetCondition(C_LT);
          OC_GTE:
            ai1.SetCondition(C_GE);
          OC_LTE:
            begin
              { emulate LTE }
              ai2:=Taicpu.Op_Sym(A_BRxx,l);
              ai2.SetCondition(C_EQ);
              ai2.is_jmp:=true;
              list.concat(ai2);

              ai1.SetCondition(C_LT);
            end;
          OC_NE:
            ai1.SetCondition(C_NE);
          OC_BE:
            begin
              { emulate BE }
              ai2:=Taicpu.Op_Sym(A_BRxx,l);
              ai2.SetCondition(C_EQ);
              ai2.is_jmp:=true;
              list.concat(ai2);

              ai1.SetCondition(C_LO);
            end;
          OC_B:
            ai1.SetCondition(C_LO);
          OC_AE:
            ai1.SetCondition(C_SH);
          OC_A:
            begin
              { emulate A (unsigned GT) }
              current_asmdata.getjumplabel(hl);
              ai2:=Taicpu.Op_Sym(A_BRxx,hl);
              ai2.SetCondition(C_EQ);
              ai2.is_jmp:=true;
              list.concat(ai2);

              ai1.SetCondition(C_SH);
            end;
          else
            internalerror(2011082501);
        end;
        list.concat(ai1);
        if assigned(hl) then
          a_label(list,hl);
      end;


    procedure tcgavr.g_stackpointer_alloc(list: TAsmList; size: longint);
      begin
        internalerror(201201071);
      end;


    procedure tcgavr.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
      begin
        //internalerror(2011021324);
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
         if not(size in [OS_S64,OS_64]) then
           internalerror(2012102402);
         tcgavr(cg).a_op_reg_reg_internal(list,Op,size,regsrc.reglo,regsrc.reghi,regdst.reglo,regdst.reghi);
      end;


    procedure tcg64favr.a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);
      begin
        tcgavr(cg).a_op_const_reg_internal(list,Op,size,value,reg.reglo,reg.reghi);
      end;


    procedure create_codegen;
      begin
        cg:=tcgavr.create;
        cg64:=tcg64favr.create;
      end;

end.
