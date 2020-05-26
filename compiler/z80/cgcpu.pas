{

    Copyright (c) 2008 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generator for the Z80

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
      tregisterlist = array of tregister;

      { tcgz80 }

      tcgz80 = class(tcg)
        { true, if the next arithmetic operation should modify the flags }
        cgsetflags : boolean;
        procedure init_register_allocators;override;
        procedure done_register_allocators;override;

        procedure getcpuregisters(list:TAsmList;regs:tregisterlist);
        procedure ungetcpuregisters(list:TAsmList;regs:tregisterlist);

        function getaddressregister(list:TAsmList):TRegister;override;

        function GetOffsetReg(const r: TRegister;ofs : shortint): TRegister;override;
        function GetOffsetReg64(const r,rhi: TRegister;ofs : shortint): TRegister;override;

        procedure a_load_const_cgpara(list : TAsmList;size : tcgsize;a : tcgint;const paraloc : TCGPara);override;
        procedure a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const cgpara : TCGPara);override;
        procedure a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);override;
        procedure a_load_reg_cgpara(list : TAsmList; size : tcgsize;r : tregister; const cgpara : tcgpara);override;

        procedure a_loadfpu_ref_cgpara(list : TAsmList;size : tcgsize;const ref : treference;const cgpara : TCGPara);override;

        procedure a_call_name(list : TAsmList;const s : string; weak: boolean);override;
        procedure a_call_reg(list : TAsmList;reg: tregister);override;

        procedure a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister); override;
        procedure a_op_reg_reg(list: TAsmList; Op: TOpCG; size: TCGSize; src, dst : TRegister); override;

        { move instructions }
        procedure a_load_const_reg(list : TAsmList; size: tcgsize; a : tcgint;reg : tregister);override;
        procedure a_load_const_ref(list : TAsmList;size : tcgsize;a : tcgint;const ref : treference);override;
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

        { Z80-specific unsigned comparison code generation jmp helper }
        procedure a_jmp_unsigned_cmp_3way(list : TAsmList;onbelow,onequal,onabove: tasmlabel);
        { Z80-specific signed comparison code generation jmp helper. Should follow a SUB instruction,
          and the A register must still contain the result. }
        procedure a_jmp_signed_cmp_3way(list : TAsmList;onless,onequal,ongreater: tasmlabel);

        procedure g_flags2reg(list: TAsmList; size: TCgSize; const f: TResFlags; reg: TRegister); override;

        procedure g_stackpointer_alloc(list : TAsmList;localsize : longint);override;
        procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);override;
        procedure g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean); override;

        procedure a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);override;

        procedure g_concatcopy(list : TAsmList;const source,dest : treference;len : tcgint);override;

        procedure g_overflowcheck(list: TAsmList; const l: tlocation; def: tdef); override;

        procedure g_save_registers(list : TAsmList);override;
        procedure g_restore_registers(list : TAsmList);override;

        procedure a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
        function normalize_ref(list : TAsmList;ref : treference; const refopertypes:trefoperandtypes; out allocatedregs:tregisterlist) : treference;
        procedure adjust_normalized_ref(list: TAsmList;var ref: treference; value: longint);

        procedure emit_mov(list: TAsmList;reg2: tregister; reg1: tregister);

        procedure a_adjust_sp(list: TAsmList; value: longint);

      protected
        procedure a_op_reg_reg_internal(list: TAsmList; Op: TOpCG; size: TCGSize; src, srchi, dst, dsthi: TRegister);
        procedure a_op_const_reg_internal(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg, reghi: TRegister);
        procedure gen_multiply(list: TAsmList; op: topcg; size: TCgSize; src2, src1, dst: tregister; check_overflow: boolean);
      end;

      tcg64fz80 = class(tcg64f32)
        procedure a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);override;
        procedure a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);override;
      end;

    procedure create_codegen;

    const
      TOpCG2AsmOp: Array[topcg] of TAsmOp = (A_NONE,A_LD,A_ADD,A_AND,A_NONE,
                            A_NONE,A_NONE,A_NONE,A_NEG,A_CPL,A_OR,
                            A_SRA,A_SLA,A_SRL,A_SUB,A_XOR,A_RLCA,A_RRCA);
  implementation

    uses
       globals,verbose,systems,cutils,
       fmodule,
       symconst,symsym,symtable,
       tgobj,rgobj,
       procinfo,cpupi,
       paramgr;


    function use_push(const cgpara:tcgpara):boolean;
      begin
        result:=(not paramanager.use_fixed_stack) and
                assigned(cgpara.location) and
                (cgpara.location^.loc=LOC_REFERENCE) and
                (cgpara.location^.reference.index=NR_STACK_POINTER_REG);
      end;


    procedure tcgz80.init_register_allocators;
      begin
        inherited init_register_allocators;
        rg[R_INTREGISTER]:=trgintcpu.create(R_INTREGISTER,R_SUBWHOLE,
            [RS_A,RS_B,RS_C,RS_D,RS_E,RS_H,RS_L],first_int_imreg,[]);
      end;


    procedure tcgz80.done_register_allocators;
      begin
        rg[R_INTREGISTER].free;
        inherited done_register_allocators;
      end;


    procedure tcgz80.getcpuregisters(list: TAsmList; regs: tregisterlist);
      var
        r: tregister;
      begin
        for r in regs do
          getcpuregister(list,r);
      end;

    procedure tcgz80.ungetcpuregisters(list: TAsmList; regs: tregisterlist);
      var
        r: tregister;
      begin
        for r in regs do
          ungetcpuregister(list,r);
      end;


    function tcgz80.getaddressregister(list: TAsmList): TRegister;
      begin
       Result:=getintregister(list,OS_ADDR);
      end;


    function tcgz80.GetOffsetReg(const r: TRegister; ofs: shortint): TRegister;
      var
        i: Integer;
      begin
        result:=r;
        for i:=1 to ofs do
          result:=GetNextReg(result);
      end;


    function tcgz80.GetOffsetReg64(const r, rhi: TRegister; ofs: shortint): TRegister;
      var
        i: Integer;
      begin
        if ofs>=4 then
          begin
            result:=rhi;
            dec(ofs,4);
          end
        else
          result:=r;
        for i:=1 to ofs do
          result:=GetNextReg(result);
      end;


    procedure tcgz80.a_load_reg_cgpara(list : TAsmList;size : tcgsize;r : tregister;const cgpara : tcgpara);

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
                  reference_reset_base(ref,paraloc^.reference.index,paraloc^.reference.offset,ctempposinvalid,2,[]);
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
        if use_push(cgpara) then
          begin
            case tcgsize2size[cgpara.Size] of
              1:
                begin
                  cgpara.check_simple_location;
                  getcpuregister(list,NR_A);
                  a_load_reg_reg(list,OS_8,OS_8,r,NR_A);
                  list.concat(taicpu.op_reg(A_PUSH,NR_AF));
                  list.concat(taicpu.op_reg(A_INC,NR_SP));
                  ungetcpuregister(list,NR_A);
                end;
              2:
                begin
                  cgpara.check_simple_location;
                  getcpuregister(list,NR_L);
                  a_load_reg_reg(list,OS_8,OS_8,r,NR_L);
                  getcpuregister(list,NR_H);
                  a_load_reg_reg(list,OS_8,OS_8,GetNextReg(r),NR_H);
                  list.concat(taicpu.op_reg(A_PUSH,NR_HL));
                  getcpuregister(list,NR_H);
                  getcpuregister(list,NR_L);
                end;
              4:
                begin
                  cgpara.check_simple_location;

                  getcpuregister(list,NR_L);
                  a_load_reg_reg(list,OS_8,OS_8,GetNextReg(GetNextReg(r)),NR_L);
                  getcpuregister(list,NR_H);
                  a_load_reg_reg(list,OS_8,OS_8,GetNextReg(GetNextReg(GetNextReg(r))),NR_H);
                  list.concat(taicpu.op_reg(A_PUSH,NR_HL));
                  getcpuregister(list,NR_H);
                  getcpuregister(list,NR_L);

                  getcpuregister(list,NR_L);
                  a_load_reg_reg(list,OS_8,OS_8,r,NR_L);
                  getcpuregister(list,NR_H);
                  a_load_reg_reg(list,OS_8,OS_8,GetNextReg(r),NR_H);
                  list.concat(taicpu.op_reg(A_PUSH,NR_HL));
                  getcpuregister(list,NR_H);
                  getcpuregister(list,NR_L);
                end;
              else
                internalerror(2020040801);
            end;
          end
        else
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


    procedure tcgz80.a_loadfpu_ref_cgpara(list: TAsmList; size: tcgsize; const ref: treference; const cgpara: TCGPara);
      var
        href: treference;
        curloc: PCGParaLocation;
        i: Integer;
      begin
        case cgpara.location^.loc of
          LOC_REGISTER,LOC_CREGISTER:
            begin
              case size of
                OS_F32:
                  begin
                    curloc:=cgpara.location;
                    href:=ref;
                    for i:=1 to 4 do
                      begin
                        if not assigned(curloc) then
                          internalerror(2020042303);
                        if not (curloc^.Loc in [LOC_REGISTER,LOC_CREGISTER]) then
                          internalerror(2020042304);
                        a_load_ref_reg(list,OS_8,OS_8,href,curloc^.register);
                        curloc:=curloc^.Next;
                      end;
                    if assigned(curloc) then
                      internalerror(2020042305);
                  end;
                else
                  internalerror(2020042302);
              end;
            end;
          else
            inherited;
        end;
      end;


    procedure tcgz80.a_load_const_cgpara(list : TAsmList;size : tcgsize;a : tcgint;const paraloc : TCGPara);
      var
        i : longint;
        hp : PCGParaLocation;
        ref: treference;
      begin
        if not(tcgsize2size[paraloc.Size] in [1..4]) then
          internalerror(2014011101);

        if use_push(paraloc) then
          begin
            case tcgsize2size[paraloc.Size] of
               1:
                 begin
                   getcpuregister(list,NR_A);
                   a_load_const_reg(list,OS_8,a,NR_A);
                   list.Concat(taicpu.op_reg(A_PUSH,NR_AF));
                   list.Concat(taicpu.op_reg(A_INC,NR_SP));
                   ungetcpuregister(list,NR_A);
                 end;
               2:
                 begin
                   getcpuregister(list,NR_IY);
                   list.Concat(taicpu.op_reg_const(A_LD,NR_IY,a));
                   list.Concat(taicpu.op_reg(A_PUSH,NR_IY));
                   ungetcpuregister(list,NR_IY);
                 end;
               4:
                 begin
                   getcpuregister(list,NR_IY);
                   list.Concat(taicpu.op_reg_const(A_LD,NR_IY,Word(a shr 16)));
                   list.Concat(taicpu.op_reg(A_PUSH,NR_IY));
                   if Word(a)<>Word(a shr 16) then
                     list.Concat(taicpu.op_reg_const(A_LD,NR_IY,Word(a)));
                   list.Concat(taicpu.op_reg(A_PUSH,NR_IY));
                   ungetcpuregister(list,NR_IY);
                 end;
               else
                 internalerror(2020040701);
            end;
          end
        else
          begin
            hp:=paraloc.location;

            i:=1;
            while i<=tcgsize2size[paraloc.Size] do
              begin
                if not(assigned(hp)) then
                  internalerror(2014011105);
                 //paramanager.allocparaloc(list,hp);
                 case hp^.loc of
                   LOC_REGISTER,LOC_CREGISTER:
                     begin
                       if (tcgsize2size[hp^.size]<>1) or
                         (hp^.shiftval<>0) then
                         internalerror(2015041101);
                       a_load_const_reg(list,hp^.size,(a shr (8*(i-1))) and $ff,hp^.register);

                       inc(i,tcgsize2size[hp^.size]);
                       hp:=hp^.Next;
                     end;
                   LOC_REFERENCE,LOC_CREFERENCE:
                     begin
                       reference_reset(ref,paraloc.alignment,[]);
                       ref.base:=hp^.reference.index;
                       ref.offset:=hp^.reference.offset;
                       a_load_const_ref(list,hp^.size,a shr (8*(i-1)),ref);

                       inc(i,tcgsize2size[hp^.size]);
                       hp:=hp^.Next;
                     end;
                   else
                     internalerror(2002071004);
                end;
              end;
          end;
      end;


    procedure tcgz80.a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const cgpara : TCGPara);

        procedure pushdata(paraloc:pcgparalocation;ofs:tcgint);
        var
          pushsize : tcgsize;
          opsize : topsize;
          tmpreg   : tregister;
          href,tmpref: treference;
        begin
          if not assigned(paraloc) then
            exit;
          if (paraloc^.loc<>LOC_REFERENCE) or
             (paraloc^.reference.index<>NR_STACK_POINTER_REG) or
             (tcgsize2size[paraloc^.size]>4) then
            internalerror(200501162);
          { Pushes are needed in reverse order, add the size of the
            current location to the offset where to load from. This
            prevents wrong calculations for the last location when
            the size is not a power of 2 }
          if assigned(paraloc^.next) then
            pushdata(paraloc^.next,ofs+tcgsize2size[paraloc^.size]);
          { Push the data starting at ofs }
          href:=r;
          inc(href.offset,ofs);
          {if tcgsize2size[paraloc^.size]>cgpara.alignment then}
            pushsize:=paraloc^.size
          {else
            pushsize:=int_cgsize(cgpara.alignment)};
          {Writeln(pushsize);}
          case tcgsize2size[pushsize] of
            1:
              begin
                tmpreg:=getintregister(list,OS_8);
                a_load_ref_reg(list,paraloc^.size,pushsize,href,tmpreg);
                getcpuregister(list,NR_A);
                a_load_reg_reg(list,OS_8,OS_8,tmpreg,NR_A);
                list.concat(taicpu.op_reg(A_PUSH,NR_AF));
                list.concat(taicpu.op_reg(A_INC,NR_SP));
                ungetcpuregister(list,NR_A);
              end;
            2:
              begin
                tmpreg:=getintregister(list,OS_16);
                a_load_ref_reg(list,paraloc^.size,pushsize,href,tmpreg);
                getcpuregister(list,NR_L);
                a_load_reg_reg(list,OS_8,OS_8,tmpreg,NR_L);
                getcpuregister(list,NR_H);
                a_load_reg_reg(list,OS_8,OS_8,GetNextReg(tmpreg),NR_H);
                list.concat(taicpu.op_reg(A_PUSH,NR_HL));
                ungetcpuregister(list,NR_H);
                ungetcpuregister(list,NR_L);
              end;
            4:
              begin
                tmpreg:=getintregister(list,OS_16);
                inc(href.offset,2);
                a_load_ref_reg(list,OS_16,OS_16,href,tmpreg);
                getcpuregister(list,NR_L);
                a_load_reg_reg(list,OS_8,OS_8,tmpreg,NR_L);
                getcpuregister(list,NR_H);
                a_load_reg_reg(list,OS_8,OS_8,GetNextReg(tmpreg),NR_H);
                list.concat(taicpu.op_reg(A_PUSH,NR_HL));
                ungetcpuregister(list,NR_H);
                ungetcpuregister(list,NR_L);
                dec(href.offset,2);
                a_load_ref_reg(list,OS_16,OS_16,href,tmpreg);
                getcpuregister(list,NR_L);
                a_load_reg_reg(list,OS_8,OS_8,tmpreg,NR_L);
                getcpuregister(list,NR_H);
                a_load_reg_reg(list,OS_8,OS_8,GetNextReg(tmpreg),NR_H);
                list.concat(taicpu.op_reg(A_PUSH,NR_HL));
                ungetcpuregister(list,NR_H);
                ungetcpuregister(list,NR_L);
              end;
            else
              internalerror(2020040803);
          end;
        end;

      var
        tmpref, ref, href: treference;
        location: pcgparalocation;
        sizeleft: tcgint;
        len: tcgint;
      begin
        { cgpara.size=OS_NO requires a copy on the stack }
        if use_push(cgpara) then
          begin
            { Record copy? }
            if (cgpara.size in [OS_NO,OS_F64]) or (size=OS_NO) then
              begin
                cgpara.check_simple_location;
                len:=align(cgpara.intsize,cgpara.alignment);
                g_stackpointer_alloc(list,len);
                reference_reset_base(href,NR_STACK_POINTER_REG,0,ctempposinvalid,4,[]);
                g_concatcopy(list,r,href,len);
              end
            else
              begin
                if tcgsize2size[cgpara.size]<>tcgsize2size[size] then
                  internalerror(200501161);
                { We need to push the data in reverse order,
                  therefor we use a recursive algorithm }
                pushdata(cgpara.location,0);
              end
          end
        else
          begin
            location := cgpara.location;
            tmpref := r;
            sizeleft := cgpara.intsize;
            while assigned(location) do
              begin
                paramanager.allocparaloc(list,location);
                case location^.loc of
                  LOC_REGISTER,LOC_CREGISTER:
                    a_load_ref_reg(list,location^.size,location^.size,tmpref,location^.register);
                  LOC_REFERENCE:
                    begin
                      reference_reset_base(ref,location^.reference.index,location^.reference.offset,ctempposinvalid,cgpara.alignment,[]);
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
      end;


    procedure tcgz80.a_loadaddr_ref_cgpara(list : TAsmList;const r : treference;const paraloc : TCGPara);
      var
        tmpreg: tregister;
      begin
        tmpreg:=getaddressregister(list);
        a_loadaddr_ref_reg(list,r,tmpreg);
        a_load_reg_cgpara(list,OS_ADDR,tmpreg,paraloc);
      end;


    procedure tcgz80.a_call_name(list : TAsmList;const s : string; weak: boolean);
      var
        sym: TAsmSymbol;
      begin
        if weak then
          sym:=current_asmdata.WeakRefAsmSymbol(s,AT_FUNCTION)
        else
          sym:=current_asmdata.RefAsmSymbol(s,AT_FUNCTION);

        list.concat(taicpu.op_sym(A_CALL,sym));

        include(current_procinfo.flags,pi_do_call);
      end;


    procedure tcgz80.a_call_reg(list : TAsmList;reg: tregister);
      var
        l : TAsmLabel;
        ref : treference;
      begin
        { HACK: at this point all registers are allocated, due to the way the
          calling convention works, but we need to free some registers, in order
          for the following code to work, so we do it here }
        dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));

        getcpuregister(list,NR_L);
        a_load_reg_reg(list,OS_8,OS_8,reg,NR_L);
        getcpuregister(list,NR_H);
        a_load_reg_reg(list,OS_8,OS_8,GetNextReg(reg),NR_H);
        current_asmdata.getjumplabel(l);
        reference_reset(ref,0,[]);
        ref.symbol:=l;
        list.concat(taicpu.op_ref_reg(A_LD,ref,NR_HL));
        ungetcpuregister(list,NR_H);
        ungetcpuregister(list,NR_L);

        { allocate them again, right before the actual call instruction }
        alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));

        list.concat(tai_const.Create_8bit($CD));  { $CD is the opcode of the call instruction }
        list.concat(tai_label.Create(l));
        list.concat(tai_const.Create_16bit(0));
        include(current_procinfo.flags,pi_do_call);
      end;


     procedure tcgz80.a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister);
       begin
         if not(size in [OS_S8,OS_8,OS_S16,OS_16,OS_S32,OS_32]) then
           internalerror(2012102403);
         a_op_const_reg_internal(list,Op,size,a,reg,NR_NO);
       end;


     procedure tcgz80.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: TCGSize; src, dst : TRegister);
       begin
         if not(size in [OS_S8,OS_8,OS_S16,OS_16,OS_S32,OS_32]) then
           internalerror(2012102401);
         a_op_reg_reg_internal(list,Op,size,src,NR_NO,dst,NR_NO);
       end;


     procedure tcgz80.a_op_reg_reg_internal(list : TAsmList; Op: TOpCG; size: TCGSize; src, srchi, dst, dsthi: TRegister);
       var
         i : integer;

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

       var
         tmpreg,tmpreg2: tregister;
         instr : taicpu;
         l1,l2 : tasmlabel;

      begin
         case op of
           OP_ADD:
             begin
               getcpuregister(list,NR_A);
               a_load_reg_reg(list,OS_8,OS_8,dst,NR_A);
               list.concat(taicpu.op_reg_reg(A_ADD,NR_A,src));
               a_load_reg_reg(list,OS_8,OS_8,NR_A,dst);
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       NextSrcDst;
                       a_load_reg_reg(list,OS_8,OS_8,dst,NR_A);
                       list.concat(taicpu.op_reg_reg(A_ADC,NR_A,src));
                       a_load_reg_reg(list,OS_8,OS_8,NR_A,dst);
                     end;
                 end;
               ungetcpuregister(list,NR_A);
             end;

           OP_SUB:
             begin
               getcpuregister(list,NR_A);
               a_load_reg_reg(list,OS_8,OS_8,dst,NR_A);
               list.concat(taicpu.op_reg_reg(A_SUB,NR_A,src));
               a_load_reg_reg(list,OS_8,OS_8,NR_A,dst);
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       NextSrcDst;
                       a_load_reg_reg(list,OS_8,OS_8,dst,NR_A);
                       list.concat(taicpu.op_reg_reg(A_SBC,NR_A,src));
                       a_load_reg_reg(list,OS_8,OS_8,NR_A,dst);
                     end;
                 end;
               ungetcpuregister(list,NR_A);
             end;

           OP_NEG:
             begin
               getcpuregister(list,NR_A);
               if tcgsize2size[size]>=2 then
                 begin
                   tmpreg:=GetNextReg(src);
                   tmpreg2:=GetNextReg(dst);
                   for i:=2 to tcgsize2size[size] do
                     begin
                       a_load_reg_reg(list,OS_8,OS_8,tmpreg,NR_A);
                       list.concat(taicpu.op_none(A_CPL));
                       a_load_reg_reg(list,OS_8,OS_8,NR_A,tmpreg2);
                       if i<>tcgsize2size[size] then
                         begin
                           if i=4 then
                             begin
                               tmpreg:=srchi;
                               tmpreg2:=dsthi;
                             end
                           else
                             begin
                               tmpreg:=GetNextReg(tmpreg);
                               tmpreg2:=GetNextReg(tmpreg2);
                             end;
                         end;
                     end;
                 end;
               a_load_reg_reg(list,OS_8,OS_8,src,NR_A);
               list.concat(taicpu.op_none(A_NEG));
               a_load_reg_reg(list,OS_8,OS_8,NR_A,dst);
               if tcgsize2size[size]>=2 then
                 begin
                   tmpreg2:=GetNextReg(dst);
                   for i:=2 to tcgsize2size[size] do
                     begin
                       a_load_reg_reg(list,OS_8,OS_8,tmpreg2,NR_A);
                       list.concat(taicpu.op_reg_const(A_SBC,NR_A,-1));
                       a_load_reg_reg(list,OS_8,OS_8,NR_A,tmpreg2);
                       if i<>tcgsize2size[size] then
                         begin
                           if i=4 then
                             begin
                               tmpreg2:=dsthi;
                             end
                           else
                             begin
                               tmpreg2:=GetNextReg(tmpreg2);
                             end;
                         end;
                     end;
                 end;
               ungetcpuregister(list,NR_A);
             end;

           OP_NOT:
             begin
               getcpuregister(list,NR_A);
               for i:=1 to tcgsize2size[size] do
                 begin
                   if i<>1 then
                     NextSrcDst;
                   a_load_reg_reg(list,OS_8,OS_8,src,NR_A);
                   list.concat(taicpu.op_none(A_CPL));
                   a_load_reg_reg(list,OS_8,OS_8,NR_A,dst);
                 end;
               ungetcpuregister(list,NR_A);
             end;

           OP_MUL,OP_IMUL:
             begin
               tmpreg:=dst;
               if size in [OS_16,OS_S16] then
                 begin
                   tmpreg:=getintregister(list,size);
                   a_load_reg_reg(list,size,size,dst,tmpreg);
                 end;
               gen_multiply(list,op,size,src,tmpreg,dst,false);
             end;

           OP_DIV,OP_IDIV:
             { special stuff, needs separate handling inside code
               generator                                          }
             internalerror(2017032604);

           OP_SHR,OP_SHL,OP_SAR,OP_ROL,OP_ROR:
             begin
               current_asmdata.getjumplabel(l1);
               current_asmdata.getjumplabel(l2);
               getcpuregister(list,NR_B);
               emit_mov(list,NR_B,src);
               list.concat(taicpu.op_reg(A_INC,NR_B));
               list.concat(taicpu.op_reg(A_DEC,NR_B));
               a_jmp_flags(list,F_E,l2);
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 case op of
                   OP_ROL:
                     begin
                       list.concat(taicpu.op_reg(A_RRC,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-1)));
                       list.concat(taicpu.op_reg(A_RLC,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-1)));
                     end;
                   OP_ROR:
                     begin
                       list.concat(taicpu.op_reg(A_RLC,dst));
                       list.concat(taicpu.op_reg(A_RRC,dst));
                     end;
                   else
                     ;
                 end;
               cg.a_label(list,l1);
               case op of
                 OP_SHL:
                   list.concat(taicpu.op_reg(A_SLA,dst));
                 OP_SHR:
                   list.concat(taicpu.op_reg(A_SRL,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-1)));
                 OP_SAR:
                   list.concat(taicpu.op_reg(A_SRA,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-1)));
                 OP_ROL:
                   if size in [OS_8,OS_S8] then
                     list.concat(taicpu.op_reg(A_RLC,dst))
                   else
                     list.concat(taicpu.op_reg(A_RL,dst));
                 OP_ROR:
                   if size in [OS_8,OS_S8] then
                     list.concat(taicpu.op_reg(A_RRC,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-1)))
                   else
                     list.concat(taicpu.op_reg(A_RR,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-1)));
                 else
                   internalerror(2020040903);
               end;
               if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                 begin
                   for i:=2 to tcgsize2size[size] do
                     begin
                       case op of
                         OP_ROR,
                         OP_SHR,
                         OP_SAR:
                           list.concat(taicpu.op_reg(A_RR,GetOffsetReg64(dst,dsthi,tcgsize2size[size]-i)));
                         OP_ROL,
                         OP_SHL:
                           list.concat(taicpu.op_reg(A_RL,GetOffsetReg64(dst,dsthi,i-1)));
                         else
                           internalerror(2020040904);
                       end;
                   end;
                 end;
               instr:=taicpu.op_sym(A_DJNZ,l1);
               instr.is_jmp:=true;
               list.concat(instr);
               ungetcpuregister(list,NR_B);
               cg.a_label(list,l2);
             end;

           OP_AND,OP_OR,OP_XOR:
             begin
               getcpuregister(list,NR_A);
               for i:=1 to tcgsize2size[size] do
                 begin
                   if i<>1 then
                     NextSrcDst;
                   a_load_reg_reg(list,OS_8,OS_8,dst,NR_A);
                   list.concat(taicpu.op_reg_reg(topcg2asmop[op],NR_A,src));
                   a_load_reg_reg(list,OS_8,OS_8,NR_A,dst);
                 end;
               ungetcpuregister(list,NR_A);
             end;
           else
             internalerror(2011022004);
         end;
       end;

     procedure tcgz80.a_op_const_reg_internal(list: TAsmList; Op: TOpCG;
      size: TCGSize; a: tcgint; reg, reghi: TRegister);

       var
         i : byte;

      procedure NextReg;
        begin
          if i=4 then
            reg:=reghi
          else
            reg:=GetNextReg(reg);
        end;

      var
        mask : qword;
        shift : byte;
        curvalue : byte;
        tmpop: TAsmOp;
        l1: TAsmLabel;
        instr: taicpu;
        tmpreg : tregister;
        tmpreg64 : tregister64;

       begin
         optimize_op_const(size,op,a);
         mask:=$ff;
         shift:=0;
         case op of
           OP_NONE:
             begin
               { Opcode is optimized away }
             end;
           OP_MOVE:
             begin
               { Optimized, replaced with a simple load }
               a_load_const_reg(list,size,a,reg);
             end;
           OP_AND:
             begin
               curvalue:=a and mask;
               for i:=1 to tcgsize2size[size] do
                 begin
                   case curvalue of
                     0:
                       list.concat(taicpu.op_reg_const(A_LD,reg,0));
                     $ff:
                       {nothing};
                     else
                       begin
                         getcpuregister(list,NR_A);
                         emit_mov(list,NR_A,reg);
                         list.concat(taicpu.op_reg_const(A_AND,NR_A,curvalue));
                         emit_mov(list,reg,NR_A);
                         ungetcpuregister(list,NR_A);
                       end;
                   end;
                   if i<>tcgsize2size[size] then
                     begin
                       NextReg;
                       mask:=mask shl 8;
                       inc(shift,8);
                       curvalue:=(qword(a) and mask) shr shift;
                     end;
                 end;
             end;
           OP_OR:
             begin
               curvalue:=a and mask;
               for i:=1 to tcgsize2size[size] do
                 begin
                   case curvalue of
                     0:
                       {nothing};
                     $ff:
                       list.concat(taicpu.op_reg_const(A_LD,reg,$ff));
                     else
                       begin
                         getcpuregister(list,NR_A);
                         emit_mov(list,NR_A,reg);
                         list.concat(taicpu.op_reg_const(A_OR,NR_A,curvalue));
                         emit_mov(list,reg,NR_A);
                         ungetcpuregister(list,NR_A);
                       end;
                   end;
                   if i<>tcgsize2size[size] then
                     begin
                       NextReg;
                       mask:=mask shl 8;
                       inc(shift,8);
                       curvalue:=(qword(a) and mask) shr shift;
                     end;
                 end;
             end;
           OP_XOR:
             begin
               curvalue:=a and mask;
               for i:=1 to tcgsize2size[size] do
                 begin
                   case curvalue of
                     0:
                       {nothing};
                     $ff:
                       begin
                         getcpuregister(list,NR_A);
                         emit_mov(list,NR_A,reg);
                         list.concat(taicpu.op_none(A_CPL));
                         emit_mov(list,reg,NR_A);
                         ungetcpuregister(list,NR_A);
                       end;
                     else
                       begin
                         getcpuregister(list,NR_A);
                         emit_mov(list,NR_A,reg);
                         list.concat(taicpu.op_reg_const(A_XOR,NR_A,curvalue));
                         emit_mov(list,reg,NR_A);
                         ungetcpuregister(list,NR_A);
                       end;
                   end;
                   if i<>tcgsize2size[size] then
                     begin
                       NextReg;
                       mask:=mask shl 8;
                       inc(shift,8);
                       curvalue:=(qword(a) and mask) shr shift;
                     end;
                 end;
             end;
           OP_SHR,OP_SHL,OP_SAR,OP_ROL,OP_ROR:
             begin
               if size in [OS_64,OS_S64] then
                 a:=a and 63
               else
                 a:=a and 31;
               if a<>0 then
                 begin
                   if a>1 then
                     begin
                       current_asmdata.getjumplabel(l1);
                       getcpuregister(list,NR_B);
                       list.concat(taicpu.op_reg_const(A_LD,NR_B,a));
                     end;
                   if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                     case op of
                       OP_ROL:
                         begin
                           list.concat(taicpu.op_reg(A_RRC,GetOffsetReg64(reg,reghi,tcgsize2size[size]-1)));
                           list.concat(taicpu.op_reg(A_RLC,GetOffsetReg64(reg,reghi,tcgsize2size[size]-1)));
                         end;
                       OP_ROR:
                         begin
                           list.concat(taicpu.op_reg(A_RLC,reg));
                           list.concat(taicpu.op_reg(A_RRC,reg));
                         end;
                       else
                         ;
                     end;
                   if a>1 then
                     cg.a_label(list,l1);
                   case op of
                     OP_SHL:
                       list.concat(taicpu.op_reg(A_SLA,reg));
                     OP_SHR:
                       list.concat(taicpu.op_reg(A_SRL,GetOffsetReg64(reg,reghi,tcgsize2size[size]-1)));
                     OP_SAR:
                       list.concat(taicpu.op_reg(A_SRA,GetOffsetReg64(reg,reghi,tcgsize2size[size]-1)));
                     OP_ROL:
                       if size in [OS_8,OS_S8] then
                         list.concat(taicpu.op_reg(A_RLC,reg))
                       else
                         list.concat(taicpu.op_reg(A_RL,reg));
                     OP_ROR:
                       if size in [OS_8,OS_S8] then
                         list.concat(taicpu.op_reg(A_RRC,GetOffsetReg64(reg,reghi,tcgsize2size[size]-1)))
                       else
                         list.concat(taicpu.op_reg(A_RR,GetOffsetReg64(reg,reghi,tcgsize2size[size]-1)));
                     else
                       internalerror(2020040903);
                   end;
                   if size in [OS_S16,OS_16,OS_S32,OS_32,OS_S64,OS_64] then
                     begin
                       for i:=2 to tcgsize2size[size] do
                         begin
                           case op of
                             OP_ROR,
                             OP_SHR,
                             OP_SAR:
                               list.concat(taicpu.op_reg(A_RR,GetOffsetReg64(reg,reghi,tcgsize2size[size]-i)));
                             OP_ROL,
                             OP_SHL:
                               list.concat(taicpu.op_reg(A_RL,GetOffsetReg64(reg,reghi,i-1)));
                             else
                               internalerror(2020040904);
                           end;
                       end;
                     end;
                   if a>1 then
                     begin
                       instr:=taicpu.op_sym(A_DJNZ,l1);
                       instr.is_jmp:=true;
                       list.concat(instr);
                       ungetcpuregister(list,NR_B);
                     end;
                 end;
             end;
           OP_ADD:
             begin
               curvalue:=a and mask;
               tmpop:=A_NONE;
               for i:=1 to tcgsize2size[size] do
                 begin
                   if (tmpop=A_NONE) and (curvalue=1) and (i=tcgsize2size[size]) then
                     tmpop:=A_INC
                   else if (tmpop=A_NONE) and (curvalue=255) and (i=tcgsize2size[size]) then
                     tmpop:=A_DEC
                   else if (tmpop=A_NONE) and (curvalue<>0) then
                     tmpop:=A_ADD
                   else if tmpop=A_ADD then
                     tmpop:=A_ADC;
                   case tmpop of
                     A_NONE:
                       {nothing};
                     A_INC,A_DEC:
                       list.concat(taicpu.op_reg(tmpop,reg));
                     A_ADD,A_ADC:
                       begin
                         getcpuregister(list,NR_A);
                         emit_mov(list,NR_A,reg);
                         list.concat(taicpu.op_reg_const(tmpop,NR_A,curvalue));
                         emit_mov(list,reg,NR_A);
                         ungetcpuregister(list,NR_A);
                       end;
                     else
                       internalerror(2020040901);
                   end;
                   if i<>tcgsize2size[size] then
                     begin
                       NextReg;
                       mask:=mask shl 8;
                       inc(shift,8);
                       curvalue:=(qword(a) and mask) shr shift;
                     end;
                 end;
             end;
           OP_SUB:
             begin
               curvalue:=a and mask;
               tmpop:=A_NONE;
               for i:=1 to tcgsize2size[size] do
                 begin
                   if (tmpop=A_NONE) and (curvalue=1) and (i=tcgsize2size[size]) then
                     tmpop:=A_DEC
                   else if (tmpop=A_NONE) and (curvalue=255) and (i=tcgsize2size[size]) then
                     tmpop:=A_INC
                   else if (tmpop=A_NONE) and (curvalue<>0) then
                     tmpop:=A_SUB
                   else if tmpop=A_SUB then
                     tmpop:=A_SBC;
                   case tmpop of
                     A_NONE:
                       {nothing};
                     A_DEC,A_INC:
                       list.concat(taicpu.op_reg(tmpop,reg));
                     A_SUB,A_SBC:
                       begin
                         getcpuregister(list,NR_A);
                         emit_mov(list,NR_A,reg);
                         list.concat(taicpu.op_reg_const(tmpop,NR_A,curvalue));
                         emit_mov(list,reg,NR_A);
                         ungetcpuregister(list,NR_A);
                       end;
                     else
                       internalerror(2020040902);
                   end;
                   if i<>tcgsize2size[size] then
                     begin
                       NextReg;
                       mask:=mask shl 8;
                       inc(shift,8);
                       curvalue:=(qword(a) and mask) shr shift;
                     end;
                 end;
             end;
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
{$if 0}
                 { code not working yet }
                 if (op=OP_SAR) and (a=31) and (size in [OS_32,OS_S32]) then
                   begin
                     tmpreg:=reg;
                     for i:=1 to 4 do
                       begin
                         list.concat(taicpu.op_reg_reg(A_MOV,tmpreg,NR_R1));
                         tmpreg:=GetNextReg(tmpreg);
                       end;
                   end
                 else
{$endif}
                   begin
                     tmpreg:=getintregister(list,size);
                     a_load_const_reg(list,size,a,tmpreg);
                     a_op_reg_reg(list,op,size,tmpreg,reg);
                   end;
               end;
           end;
       end;
     end;


     procedure tcgz80.gen_multiply(list: TAsmList; op: topcg; size: TCgSize; src2, src1, dst: tregister; check_overflow: boolean);
       var
         pd: tprocdef;
         paraloc1, paraloc2: tcgpara;
         ai: taicpu;
         hl, no_overflow: TAsmLabel;
         name: String;
       begin
         if size in [OS_8,OS_S8] then
           begin
             if size=OS_8 then
               name:='fpc_mul_byte'
             else
               name:='fpc_mul_shortint';

             if check_overflow then
               name:=name+'_checkoverflow';

             pd:=search_system_proc(name);
             paraloc1.init;
             paraloc2.init;
             paramanager.getcgtempparaloc(list,pd,1,paraloc1);
             paramanager.getcgtempparaloc(list,pd,2,paraloc2);
             a_load_reg_cgpara(list,OS_8,src1,paraloc2);
             a_load_reg_cgpara(list,OS_8,src2,paraloc1);
             paramanager.freecgpara(list,paraloc2);
             paramanager.freecgpara(list,paraloc1);
             alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
             a_call_name(list,upper(name),false);
             dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
             cg.a_reg_alloc(list,NR_L);
             cg.a_load_reg_reg(list,OS_8,OS_8,NR_L,dst);
             cg.a_reg_dealloc(list,NR_L);
             paraloc2.done;
             paraloc1.done;
           end
         else if size in [OS_16,OS_S16] then
           begin
             if size=OS_16 then
               name:='fpc_mul_word'
             else
               name:='fpc_mul_integer';

             if check_overflow then
               name:=name+'_checkoverflow';

             pd:=search_system_proc(name);
             paraloc1.init;
             paraloc2.init;
             paramanager.getcgtempparaloc(list,pd,1,paraloc1);
             paramanager.getcgtempparaloc(list,pd,2,paraloc2);
             a_load_reg_cgpara(list,OS_16,src1,paraloc2);
             a_load_reg_cgpara(list,OS_16,src2,paraloc1);
             paramanager.freecgpara(list,paraloc2);
             paramanager.freecgpara(list,paraloc1);
             alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
             a_call_name(list,upper(name),false);
             dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
             cg.a_reg_alloc(list,NR_L);
             cg.a_reg_alloc(list,NR_H);
             cg.a_load_reg_reg(list,OS_8,OS_8,NR_L,dst);
             cg.a_reg_dealloc(list,NR_L);
             cg.a_load_reg_reg(list,OS_8,OS_8,NR_H,GetNextReg(dst));
             cg.a_reg_dealloc(list,NR_H);
             paraloc2.done;
             paraloc1.done;
           end
         else
           internalerror(2011022002);
       end;


     procedure tcgz80.a_load_const_reg(list : TAsmList; size: tcgsize; a : tcgint;reg : tregister);
       var
         mask : qword;
         shift : byte;
         i : byte;
       begin
         mask:=$ff;
         shift:=0;
         for i:=tcgsize2size[size] downto 1 do
           begin
             list.Concat(taicpu.op_reg_const(A_LD,reg,(qword(a) and mask) shr shift));
             if i<>1 then
               begin
                 mask:=mask shl 8;
                 inc(shift,8);
                 reg:=GetNextReg(reg);
               end;
           end;
       end;


     procedure tcgz80.a_load_const_ref(list: TAsmList; size: tcgsize; a: tcgint; const ref: treference);
       var
         mask : qword;
         shift : byte;
         href: treference;
         i: Integer;
       begin
         mask:=$ff;
         shift:=0;
         href:=ref;
         if (href.base=NR_NO) and (href.index<>NR_NO) then
           begin
             href.base:=href.index;
             href.index:=NR_NO;
           end;
         if is_ref_in_opertypes(href,[OT_REF_IX_d,OT_REF_IY_d]) or
            (is_ref_hl(href) and (size in [OS_8,OS_S8])) then
           begin
             for i:=tcgsize2size[size] downto 1 do
               begin
                 list.Concat(taicpu.op_ref_const(A_LD,href,(qword(a) and mask) shr shift));
                 if i<>1 then
                   begin
                     mask:=mask shl 8;
                     inc(shift,8);
                     inc(href.offset);
                   end;
               end;
           end
         else
           inherited;
       end;


    function tcgz80.normalize_ref(list: TAsmList; ref: treference;
        const refopertypes: trefoperandtypes; out allocatedregs: tregisterlist): treference;
      var
        tmpref : treference;
        l : tasmlabel;
      begin
        SetLength(allocatedregs,0);

        if (ref.base=NR_NO) and (ref.index<>NR_NO) and (ref.scalefactor<=1) then
          begin
            ref.base:=ref.index;
            ref.index:=NR_NO;
          end;

        if is_ref_in_opertypes(ref,refopertypes) then
          begin
            Result:=ref;
            exit;
          end;

        { can we use the HL register? }
        if OT_REF_HL in refopertypes then
          begin
            SetLength(allocatedregs,2);
            allocatedregs[0]:=NR_H;
            allocatedregs[1]:=NR_L;
            getcpuregisters(list,allocatedregs);
            if assigned(ref.symbol) or (ref.offset<>0) then
              begin
                if assigned(ref.symbol) then
                  begin
                    reference_reset(tmpref,0,[]);
                    tmpref.symbol:=ref.symbol;
                    tmpref.offset:=ref.offset;

                    tmpref.refaddr:=addr_full;
                    list.concat(taicpu.op_reg_ref(A_LD,NR_HL,tmpref));
                  end
                else
                  list.concat(taicpu.op_reg_const(A_LD,NR_HL,ref.offset));
                if (ref.base=NR_IX) or (ref.base=NR_IY) then
                  begin
                    getcpuregister(list,NR_D);
                    getcpuregister(list,NR_E);
                    list.concat(taicpu.op_reg(A_PUSH,ref.base));
                    list.concat(taicpu.op_reg(A_POP,NR_DE));
                    list.concat(taicpu.op_reg_reg(A_ADD,NR_HL,NR_DE));
                    ungetcpuregister(list,NR_E);
                    ungetcpuregister(list,NR_D);
                  end
                else if ref.base<>NR_NO then
                  begin
                    getcpuregister(list,NR_A);
                    emit_mov(list,NR_A,NR_L);
                    list.concat(taicpu.op_reg_reg(A_ADD,NR_A,ref.base));
                    emit_mov(list,NR_L,NR_A);
                    emit_mov(list,NR_A,NR_H);
                    list.concat(taicpu.op_reg_reg(A_ADC,NR_A,GetNextReg(ref.base)));
                    emit_mov(list,NR_H,NR_A);
                    ungetcpuregister(list,NR_A);
                  end;
                if ref.index<>NR_NO then
                  begin
                    if ref.scalefactor>1 then
                      internalerror(2020042002);
                    getcpuregister(list,NR_A);
                    emit_mov(list,NR_A,NR_L);
                    list.concat(taicpu.op_reg_reg(A_ADD,NR_A,ref.index));
                    emit_mov(list,NR_L,NR_A);
                    emit_mov(list,NR_A,NR_H);
                    list.concat(taicpu.op_reg_reg(A_ADC,NR_A,GetNextReg(ref.index)));
                    emit_mov(list,NR_H,NR_A);
                    ungetcpuregister(list,NR_A);
                  end;
              end
            else
              begin
                { not assigned(ref.symbol) and (ref.offset=0) }
                if (ref.base=NR_IX) or (ref.base=NR_IY) then
                  begin
                    list.concat(taicpu.op_reg(A_PUSH,ref.base));
                    list.concat(taicpu.op_reg(A_POP,NR_HL));
                  end
                else if ref.base<>NR_NO then
                  begin
                    emit_mov(list,NR_L,ref.base);
                    emit_mov(list,NR_H,GetNextReg(ref.base));
                  end;
                if ref.index<>NR_NO then
                  begin
                    if ref.scalefactor>1 then
                      internalerror(2020042002);
                    getcpuregister(list,NR_A);
                    emit_mov(list,NR_A,NR_L);
                    list.concat(taicpu.op_reg_reg(A_ADD,NR_A,ref.index));
                    emit_mov(list,NR_L,NR_A);
                    emit_mov(list,NR_A,NR_H);
                    list.concat(taicpu.op_reg_reg(A_ADC,NR_A,GetNextReg(ref.index)));
                    emit_mov(list,NR_H,NR_A);
                    ungetcpuregister(list,NR_A);
                  end;
              end;
            reference_reset_base(result,NR_HL,0,ctempposinvalid,0,[]);
          end
        else
          internalerror(2020042001);
      end;


    procedure tcgz80.adjust_normalized_ref(list: TAsmList; var ref: treference; value: longint);
      var
        i: Integer;
      begin
        if is_ref_addr16(ref) then
          Inc(ref.offset,value)
        else if is_ref_hl(ref) then
          begin
            if value>0 then
              for i:=1 to value do
                list.concat(taicpu.op_reg(A_INC,NR_HL))
            else
              for i:=-1 downto value do
                list.concat(taicpu.op_reg(A_DEC,NR_HL));
          end
        else if is_ref_ix_d(ref) then
          begin
            if ((ref.offset+value)<=127) and ((ref.offset+value)>=-128) then
              inc(ref.offset,value)
            else
              begin
                { todo: IX is the frame pointer, we cannot change it, so we }
                {       think of another mechanism to deal with this situation }
                internalerror(2020042101);
                //if value>0 then
                //  for i:=1 to value do
                //    list.concat(taicpu.op_reg(A_INC,NR_IX))
                //else
                //  for i:=-1 downto value do
                //    list.concat(taicpu.op_reg(A_DEC,NR_IX));
              end;
          end
        else if is_ref_iy_d(ref) then
          begin
            if ((ref.offset+value)<=127) and ((ref.offset+value)>=-128) then
              inc(ref.offset,value)
            else
              if value>0 then
                for i:=1 to value do
                  list.concat(taicpu.op_reg(A_INC,NR_IY))
              else
                for i:=-1 downto value do
                  list.concat(taicpu.op_reg(A_DEC,NR_IY));
          end;
      end;


     procedure tcgz80.a_load_reg_ref(list : TAsmList; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);
       var
         href : treference;
         i : integer;
         regsused: tregisterlist;
       begin
         if (tcgsize2size[fromsize]>32) or (tcgsize2size[tosize]>32) or (fromsize=OS_NO) or (tosize=OS_NO) then
           internalerror(2011021307);
         if tcgsize2size[fromsize]>tcgsize2size[tosize] then
           internalerror(2020040802);

         href:=normalize_ref(list,Ref,[OT_REF_ADDR16,OT_REF_HL,OT_REF_IX_d,OT_REF_IY_d],regsused);
         if (tcgsize2size[fromsize]=tcgsize2size[tosize]) or (fromsize in [OS_8,OS_16,OS_32]) then
           begin
             getcpuregister(list,NR_A);
             for i:=1 to tcgsize2size[fromsize] do
               begin
                 a_load_reg_reg(list,OS_8,OS_8,reg,NR_A);
                 list.concat(taicpu.op_ref_reg(A_LD,href,NR_A));
                 if i<>tcgsize2size[fromsize] then
                   reg:=GetNextReg(reg);
                 if i<>tcgsize2size[tosize] then
                   adjust_normalized_ref(list,href,1);
               end;
             for i:=tcgsize2size[fromsize]+1 to tcgsize2size[tosize] do
               begin
                 if i=(tcgsize2size[fromsize]+1) then
                   list.concat(taicpu.op_reg_const(A_LD,NR_A,0));
                 list.concat(taicpu.op_ref_reg(A_LD,href,NR_A));
                 if i<>tcgsize2size[tosize] then
                   begin
                     adjust_normalized_ref(list,href,1);
                     reg:=GetNextReg(reg);
                   end;
               end;
             ungetcpuregister(list,NR_A);
           end
         else
           begin
             getcpuregister(list,NR_A);
             for i:=1 to tcgsize2size[fromsize] do
               begin
                 a_load_reg_reg(list,OS_8,OS_8,reg,NR_A);
                 list.concat(taicpu.op_ref_reg(A_LD,href,NR_A));
                 if i<>tcgsize2size[fromsize] then
                   reg:=GetNextReg(reg);
                 if i<>tcgsize2size[tosize] then
                   adjust_normalized_ref(list,href,1);
               end;
             list.concat(taicpu.op_none(A_RLA));
             list.concat(taicpu.op_reg_reg(A_SBC,NR_A,NR_A));
             for i:=tcgsize2size[fromsize]+1 to tcgsize2size[tosize] do
               begin
                 list.concat(taicpu.op_ref_reg(A_LD,href,NR_A));
                 if i<>tcgsize2size[tosize] then
                   begin
                     adjust_normalized_ref(list,href,1);
                     reg:=GetNextReg(reg);
                   end;
               end;
             ungetcpuregister(list,NR_A);
           end;
         ungetcpuregisters(list,regsused);
       end;


     procedure tcgz80.a_load_ref_reg(list : TAsmList; fromsize, tosize : tcgsize;
       const Ref : treference;reg : tregister);
       var
         href : treference;
         i : integer;
         regsused: tregisterlist;
       begin
         if (tcgsize2size[fromsize]>32) or (tcgsize2size[tosize]>32) or (fromsize=OS_NO) or (tosize=OS_NO) then
           internalerror(2011021307);
         if tcgsize2size[fromsize]>=tcgsize2size[tosize] then
           fromsize:=tosize;

         href:=normalize_ref(list,Ref,[OT_REF_ADDR16,OT_REF_HL,OT_REF_IX_d,OT_REF_IY_d],regsused);
         if (tcgsize2size[tosize]=tcgsize2size[fromsize]) or (fromsize in [OS_8,OS_16,OS_32]) then
           begin
             getcpuregister(list,NR_A);
             for i:=1 to tcgsize2size[fromsize] do
               begin
                 list.concat(taicpu.op_reg_ref(A_LD,NR_A,href));
                 a_load_reg_reg(list,OS_8,OS_8,NR_A,reg);

                 if i<>tcgsize2size[fromsize] then
                   adjust_normalized_ref(list,href,1);
                 if i<>tcgsize2size[tosize] then
                   reg:=GetNextReg(reg);
               end;
             ungetcpuregisters(list,regsused);
             ungetcpuregister(list,NR_A);
             for i:=tcgsize2size[fromsize]+1 to tcgsize2size[tosize] do
               begin
                 list.concat(taicpu.op_reg_const(A_LD,reg,0));
                 if i<>tcgsize2size[tosize] then
                   reg:=GetNextReg(reg);
               end;
           end
         else
           begin
             getcpuregister(list,NR_A);
             for i:=1 to tcgsize2size[fromsize] do
               begin
                 list.concat(taicpu.op_reg_ref(A_LD,NR_A,href));
                 a_load_reg_reg(list,OS_8,OS_8,NR_A,reg);

                 if i<>tcgsize2size[fromsize] then
                   adjust_normalized_ref(list,href,1);
                 if i<>tcgsize2size[tosize] then
                   reg:=GetNextReg(reg);
               end;
             ungetcpuregisters(list,regsused);
             list.concat(taicpu.op_none(A_RLA));
             list.concat(taicpu.op_reg_reg(A_SBC,NR_A,NR_A));
             for i:=tcgsize2size[fromsize]+1 to tcgsize2size[tosize] do
               begin
                 emit_mov(list,reg,NR_A);
                 if i<>tcgsize2size[tosize] then
                   reg:=GetNextReg(reg);
               end;
             ungetcpuregister(list,NR_A);
           end;
       end;


     procedure tcgz80.a_load_reg_reg(list : TAsmList; fromsize, tosize : tcgsize;reg1,reg2 : tregister);
       var
         conv_done: boolean;
         tmpreg : tregister;
         i : integer;
       begin
         if (tcgsize2size[fromsize]>32) or (tcgsize2size[tosize]>32) or (fromsize=OS_NO) or (tosize=OS_NO) then
           internalerror(2011021310);
         if tcgsize2size[fromsize]>tcgsize2size[tosize] then
           fromsize:=tosize;

         if (tcgsize2size[tosize]=tcgsize2size[fromsize]) or (fromsize in [OS_8,OS_16,OS_32]) then
           begin
             if reg1<>reg2 then
               for i:=1 to tcgsize2size[fromsize] do
                 begin
                   emit_mov(list,reg2,reg1);
                   if i<>tcgsize2size[fromsize] then
                     reg1:=GetNextReg(reg1);
                   if i<>tcgsize2size[tosize] then
                     reg2:=GetNextReg(reg2);
                 end
             else
               for i:=1 to tcgsize2size[fromsize] do
                 if i<>tcgsize2size[tosize] then
                   reg2:=GetNextReg(reg2);
             for i:=tcgsize2size[fromsize]+1 to tcgsize2size[tosize] do
               begin
                 list.Concat(taicpu.op_reg_const(A_LD,reg2,0));
                 if i<>tcgsize2size[tosize] then
                   reg2:=GetNextReg(reg2);
               end
           end
         else
           begin
             if reg1<>reg2 then
               for i:=1 to tcgsize2size[fromsize]-1 do
                 begin
                   emit_mov(list,reg2,reg1);
                   reg1:=GetNextReg(reg1);
                   reg2:=GetNextReg(reg2);
                 end
             else
               for i:=1 to tcgsize2size[fromsize]-1 do
                 reg2:=GetNextReg(reg2);
             emit_mov(list,reg2,reg1);
             getcpuregister(list,NR_A);
             emit_mov(list,NR_A,reg2);
             reg2:=GetNextReg(reg2);
             list.concat(taicpu.op_none(A_RLA));
             list.concat(taicpu.op_reg_reg(A_SBC,NR_A,NR_A));
             for i:=tcgsize2size[fromsize]+1 to tcgsize2size[tosize] do
               begin
                 emit_mov(list,reg2,NR_A);
                 if i<>tcgsize2size[tosize] then
                   reg2:=GetNextReg(reg2);
               end;
             ungetcpuregister(list,NR_A);
           end;
       end;


     procedure tcgz80.a_loadfpu_reg_reg(list: TAsmList; fromsize,tosize: tcgsize; reg1, reg2: tregister);
       begin
         internalerror(2012010702);
       end;


     procedure tcgz80.a_loadfpu_ref_reg(list: TAsmList; fromsize,tosize: tcgsize; const ref: treference; reg: tregister);
       begin
         internalerror(2012010703);
       end;


     procedure tcgz80.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference);
       begin
         internalerror(2012010704);
       end;


    {  comparison operations }
    procedure tcgz80.a_cmp_const_reg_label(list : TAsmList;size : tcgsize;
      cmp_op : topcmp;a : tcgint;reg : tregister;l : tasmlabel);
      var
        swapped : boolean;
        tmpreg : tregister;
        i : byte;
        tmpl: TAsmLabel;
      begin
        if size in [OS_8,OS_S8]then
          begin
            if cmp_op in [OC_EQ,OC_NE,OC_B,OC_AE] then
              begin
                getcpuregister(list,NR_A);
                a_load_reg_reg(list,OS_8,OS_8,reg,NR_A);
                list.concat(taicpu.op_reg_const(A_CP,NR_A,a));
                case cmp_op of
                  OC_EQ:
                    a_jmp_flags(list,F_E,l);
                  OC_NE:
                    a_jmp_flags(list,F_NE,l);
                  OC_B:
                    a_jmp_flags(list,F_C,l);
                  OC_AE:
                    a_jmp_flags(list,F_NC,l);
                  else
                    internalerror(2020042206);
                end;
                ungetcpuregister(list,NR_A);
              end
            else if cmp_op in [OC_A,OC_BE] then
              begin
                getcpuregister(list,NR_A);
                a_load_const_reg(list,OS_8,a,NR_A);
                list.concat(taicpu.op_reg_reg(A_CP,NR_A,reg));
                case cmp_op of
                  OC_A:
                    a_jmp_flags(list,F_C,l);
                  OC_BE:
                    a_jmp_flags(list,F_NC,l);
                  else
                    internalerror(2020042206);
                end;
                ungetcpuregister(list,NR_A);
              end
            else if cmp_op in [OC_LT,OC_GTE] then
              begin
                getcpuregister(list,NR_A);
                a_load_reg_reg(list,OS_8,OS_8,reg,NR_A);
                list.concat(taicpu.op_reg_const(A_SUB,NR_A,a));
                current_asmdata.getjumplabel(tmpl);
                a_jmp_flags(list,F_PO,tmpl);
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
                cg.a_label(current_asmdata.CurrAsmList,tmpl);
                case cmp_op of
                  OC_LT:
                    a_jmp_flags(list,F_M,l);
                  OC_GTE:
                    a_jmp_flags(list,F_P,l);
                  else
                    internalerror(2020042206);
                end;
                ungetcpuregister(list,NR_A);
              end
            else if cmp_op in [OC_GT,OC_LTE] then
              begin
                getcpuregister(list,NR_A);
                a_load_const_reg(list,OS_8,a,NR_A);
                list.concat(taicpu.op_reg_reg(A_SUB,NR_A,reg));
                current_asmdata.getjumplabel(tmpl);
                a_jmp_flags(list,F_PO,tmpl);
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
                cg.a_label(current_asmdata.CurrAsmList,tmpl);
                case cmp_op of
                  OC_GT:
                    a_jmp_flags(list,F_M,l);
                  OC_LTE:
                    a_jmp_flags(list,F_P,l);
                  else
                    internalerror(2020042206);
                end;
                ungetcpuregister(list,NR_A);
              end;
          end
        else if cmp_op in [OC_EQ,OC_NE] then
          begin
            if cmp_op=OC_EQ then
              current_asmdata.getjumplabel(tmpl);
            for i:=0 to tcgsize2size[size]-1 do
              begin
                a_load_reg_reg(list,OS_8,OS_8,GetOffsetReg(reg,i),NR_A);
                list.concat(taicpu.op_reg_const(A_CP,NR_A,Byte(a shr (8*i))));
                case cmp_op of
                  OC_EQ:
                    if i<>(tcgsize2size[size]-1) then
                      a_jmp_flags(list,F_NE,tmpl)
                    else
                      a_jmp_flags(list,F_E,l);
                  OC_NE:
                    a_jmp_flags(list,F_NE,l);
                  else
                    internalerror(2020042206);
                end;
              end;
            if cmp_op=OC_EQ then
              cg.a_label(current_asmdata.CurrAsmList,tmpl);
          end
        else if cmp_op in [OC_GT,OC_LT,OC_GTE,OC_LTE,OC_BE,OC_B,OC_AE,OC_A] then
          begin
            getcpuregister(list,NR_A);
            current_asmdata.getjumplabel(tmpl);
            for i:=tcgsize2size[size]-1 downto 0 do
              begin
                a_load_reg_reg(list,OS_8,OS_8,GetOffsetReg(reg,i),NR_A);
                list.concat(taicpu.op_reg_const(A_CP,NR_A,Byte(a shr (8*i))));
                if (i=(tcgsize2size[size]-1)) and (cmp_op in [OC_GT,OC_LT,OC_GTE,OC_LTE]) then
                  case cmp_op of
                    OC_GTE,
                    OC_GT:
                      a_jmp_signed_cmp_3way(list,tmpl,nil,l);
                    OC_LT,
                    OC_LTE:
                      a_jmp_signed_cmp_3way(list,l,nil,tmpl);
                    else
                      internalerror(2020042206);
                  end
                else if i<>0 then
                  case cmp_op of
                    OC_AE,
                    OC_A,
                    OC_GTE,
                    OC_GT:
                      a_jmp_unsigned_cmp_3way(list,tmpl,nil,l);
                    OC_BE,
                    OC_B,
                    OC_LT,
                    OC_LTE:
                      a_jmp_unsigned_cmp_3way(list,l,nil,tmpl);
                    else
                      internalerror(2020042206);
                  end
                else
                case cmp_op of
                  OC_A,
                  OC_GT:
                    a_jmp_unsigned_cmp_3way(list,nil,nil,l);
                  OC_B,
                  OC_LT:
                    a_jmp_unsigned_cmp_3way(list,l,nil,nil);
                  OC_AE,
                  OC_GTE:
                    a_jmp_unsigned_cmp_3way(list,nil,l,l);
                  OC_BE,
                  OC_LTE:
                    a_jmp_unsigned_cmp_3way(list,l,l,nil);
                  else
                    internalerror(2020042206);
                end;
              end;
            cg.a_label(current_asmdata.CurrAsmList,tmpl);
            ungetcpuregister(list,NR_A);
          end
        else
          internalerror(2020042205);
      end;


    procedure tcgz80.a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel);
      begin
        internalerror(2020042301);
      end;


    procedure tcgz80.a_jmp_name(list : TAsmList;const s : string);
      var
        ai : taicpu;
      begin
        ai:=taicpu.op_sym(A_JRJP,current_asmdata.RefAsmSymbol(s,AT_FUNCTION));
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgz80.a_jmp_always(list : TAsmList;l: tasmlabel);
      var
        ai : taicpu;
      begin
        ai:=taicpu.op_sym(A_JRJP,l);
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgz80.a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel);
      var
        ai : taicpu;
      begin
        ai:=taicpu.op_cond_sym(A_JRJP,flags_to_cond(f),l);
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgz80.a_jmp_unsigned_cmp_3way(list: TAsmList; onbelow, onequal, onabove: tasmlabel);
      var
        skiplabel: TAsmLabel;
      begin
        if      (onbelow= nil) and (onequal= nil) and (onabove= nil) then
          {nothing}
        else if (onbelow= nil) and (onequal= nil) and (onabove<>nil) then
          begin
            current_asmdata.getjumplabel(skiplabel);
            a_jmp_flags(list,F_E,skiplabel);
            a_jmp_flags(list,F_NC,onabove);
            cg.a_label(list,skiplabel);
          end
        else if (onbelow= nil) and (onequal<>nil) and (onabove= nil) then
          a_jmp_flags(list,F_E,onequal)
        else if (onbelow= nil) and (onequal<>nil) and (onabove<>nil) then
          begin
            if onequal<>onabove then
              a_jmp_flags(list,F_E,onequal);
            a_jmp_flags(list,F_NC,onabove);
          end
        else if (onbelow<>nil) and (onequal= nil) and (onabove= nil) then
          a_jmp_flags(list,F_C,onbelow)
        else if (onbelow<>nil) and (onequal= nil) and (onabove<>nil) then
          begin
            if onbelow<>onabove then
              a_jmp_flags(list,F_C,onbelow);
            a_jmp_flags(list,F_NE,onabove);
          end
        else if (onbelow<>nil) and (onequal<>nil) and (onabove= nil) then
          begin
            a_jmp_flags(list,F_C,onbelow);
            a_jmp_flags(list,F_E,onequal);
          end
        else if (onbelow<>nil) and (onequal<>nil) and (onabove<>nil) then
          begin
            if (onbelow=onequal) and (onequal=onabove) then
              a_jmp_always(list,onbelow)
            else if onequal=onabove then
              begin
                a_jmp_flags(list,F_C,onbelow);
                a_jmp_always(list,onabove);
              end
            else if onbelow=onequal then
              begin
                a_jmp_flags(list,F_C,onbelow);
                a_jmp_flags(list,F_E,onequal);
                a_jmp_always(list,onabove);
              end
            else if onbelow=onabove then
              begin
                a_jmp_flags(list,F_E,onequal);
                a_jmp_always(list,onabove);
              end
            else
              begin
                { the generic case - all 3 are different labels }
                a_jmp_flags(list,F_C,onbelow);
                a_jmp_flags(list,F_E,onequal);
                a_jmp_always(list,onabove);
              end;
          end
        else
          begin
            { Shouldn't happen! All possible combinations are handled by the above code. }
            internalerror(2020042201);
          end;
      end;

    procedure tcgz80.a_jmp_signed_cmp_3way(list: TAsmList; onless, onequal, ongreater: tasmlabel);
      var
        l, skiplabel: TAsmLabel;
      begin
        if      (onless= nil) and (onequal= nil) and (ongreater= nil) then
          {nothing}
        else if (onless= nil) and (onequal= nil) and (ongreater<>nil) then
          begin
            current_asmdata.getjumplabel(skiplabel);
            a_jmp_flags(list,F_E,skiplabel);
            current_asmdata.getjumplabel(l);
            a_jmp_flags(list,F_PO,l);
            current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
            cg.a_label(current_asmdata.CurrAsmList,l);
            a_jmp_flags(list,F_P,ongreater);
            cg.a_label(list,skiplabel);
          end
        else if (onless= nil) and (onequal<>nil) and (ongreater= nil) then
          a_jmp_flags(list,F_E,onequal)
        else if (onless= nil) and (onequal<>nil) and (ongreater<>nil) then
          begin
            if onequal<>ongreater then
              a_jmp_flags(list,F_E,onequal);
            current_asmdata.getjumplabel(l);
            a_jmp_flags(list,F_PO,l);
            current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
            cg.a_label(current_asmdata.CurrAsmList,l);
            a_jmp_flags(list,F_P,ongreater);
          end
        else if (onless<>nil) and (onequal= nil) and (ongreater= nil) then
          begin
            current_asmdata.getjumplabel(l);
            a_jmp_flags(list,F_PO,l);
            current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
            cg.a_label(current_asmdata.CurrAsmList,l);
            a_jmp_flags(list,F_M,onless);
          end
        else if (onless<>nil) and (onequal= nil) and (ongreater<>nil) then
          begin
            if onless=ongreater then
              a_jmp_flags(list,F_NE,onless)
            else
              begin
                current_asmdata.getjumplabel(skiplabel);
                a_jmp_flags(list,F_E,skiplabel);
                current_asmdata.getjumplabel(l);
                a_jmp_flags(list,F_PO,l);
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
                cg.a_label(current_asmdata.CurrAsmList,l);
                a_jmp_flags(list,F_M,onless);
                a_jmp_always(list,ongreater);
                cg.a_label(list,skiplabel);
              end;
          end
        else if (onless<>nil) and (onequal<>nil) and (ongreater= nil) then
          begin
            a_jmp_flags(list,F_E,onequal);
            current_asmdata.getjumplabel(l);
            a_jmp_flags(list,F_PO,l);
            current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
            cg.a_label(current_asmdata.CurrAsmList,l);
            a_jmp_flags(list,F_M,onless);
          end
        else if (onless<>nil) and (onequal<>nil) and (ongreater<>nil) then
          begin
            if (onless=onequal) and (onequal=ongreater) then
              a_jmp_always(list,onless)
            else if onequal=ongreater then
              begin
                current_asmdata.getjumplabel(l);
                a_jmp_flags(list,F_PO,l);
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
                cg.a_label(current_asmdata.CurrAsmList,l);
                a_jmp_flags(list,F_M,onless);
                a_jmp_always(list,ongreater);
              end
            else if onless=onequal then
              begin
                a_jmp_flags(list,F_E,onequal);
                current_asmdata.getjumplabel(l);
                a_jmp_flags(list,F_PO,l);
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
                cg.a_label(current_asmdata.CurrAsmList,l);
                a_jmp_flags(list,F_M,onless);
                a_jmp_always(list,ongreater);
              end
            else if onless=ongreater then
              begin
                a_jmp_flags(list,F_E,onequal);
                a_jmp_always(list,ongreater);
              end
            else
              begin
                { the generic case - all 3 are different labels }
                a_jmp_flags(list,F_E,onequal);
                current_asmdata.getjumplabel(l);
                a_jmp_flags(list,F_PO,l);
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
                cg.a_label(current_asmdata.CurrAsmList,l);
                a_jmp_flags(list,F_M,onless);
                a_jmp_always(list,ongreater);
              end;
          end
        else
          begin
            { Shouldn't happen! All possible combinations are handled by the above code. }
            internalerror(2020042204);
          end;
      end;


    procedure tcgz80.g_flags2reg(list: TAsmList; size: TCgSize; const f: TResFlags; reg: TRegister);
      var
        l : TAsmLabel;
        tmpflags : TResFlags;
      begin
        if f in [F_C,F_NC] then
          begin
            a_load_const_reg(list,size,0,reg);
            if f=F_NC then
              list.concat(taicpu.op_none(A_CCF));
            list.concat(taicpu.op_reg(A_RL,reg));
          end
        else
          begin
            current_asmdata.getjumplabel(l);
            a_load_const_reg(list,size,0,reg);
            tmpflags:=f;
            inverse_flags(tmpflags);
            a_jmp_flags(list,tmpflags,l);
            list.concat(taicpu.op_reg(A_INC,reg));
            cg.a_label(list,l);
          end;
      end;


    procedure tcgz80.g_stackpointer_alloc(list: TAsmList; localsize: longint);
      begin
        if localsize>0 then
          begin
            list.Concat(taicpu.op_reg_const(A_LD,NR_HL,-localsize));
            list.Concat(taicpu.op_reg_reg(A_ADD,NR_HL,NR_SP));
            list.Concat(taicpu.op_reg_reg(A_LD,NR_SP,NR_HL));
          end;
      end;


    procedure tcgz80.a_adjust_sp(list : TAsmList; value : longint);
      var
        i : integer;
        sym: TAsmSymbol;
        ref: treference;
      begin
        case value of
          0:
            ;
          -7..-1:
            begin
              for i:=value to -1 do
                list.concat(taicpu.op_reg(A_DEC,NR_SP));
            end;
          1..7:
            begin
              for i:=1 to value do
                list.concat(taicpu.op_reg(A_INC,NR_SP));
            end;
          else
            begin
              sym:=current_asmdata.RefAsmSymbol('FPC_Z80_SAVE_HL',AT_DATA);
              reference_reset_symbol(ref,sym,0,1,[]);

              // block interrupts
              list.concat(taicpu.op_none(A_DI));

              // save HL
              list.concat(taicpu.op_ref_reg(A_LD,ref,NR_HL));

              // adjust SP
              list.concat(taicpu.op_reg_const(A_LD,NR_HL,value));
              list.concat(taicpu.op_reg_reg(A_ADD,NR_HL,NR_SP));
              list.concat(taicpu.op_reg_reg(A_LD,NR_SP,NR_HL));

              // restore HL
              list.concat(taicpu.op_reg_ref(A_LD,NR_HL,ref));

              // release interrupts
              list.concat(taicpu.op_none(A_EI));
            end;
        end;
      end;


    procedure tcgz80.g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);
      var
        regsize,stackmisalignment: longint;
      begin
        regsize:=0;
        stackmisalignment:=0;
        { save old framepointer }
        if not nostackframe then
          begin
            { return address }
            inc(stackmisalignment,2);
            list.concat(tai_regalloc.alloc(current_procinfo.framepointer,nil));
            if current_procinfo.framepointer=NR_FRAME_POINTER_REG then
              begin
                { push <frame_pointer> }
                inc(stackmisalignment,2);
                include(rg[R_INTREGISTER].preserved_by_proc,RS_FRAME_POINTER_REG);
                list.concat(Taicpu.op_reg(A_PUSH,NR_FRAME_POINTER_REG));
                { Return address and FP are both on stack }
                current_asmdata.asmcfi.cfa_def_cfa_offset(list,2*2);
                current_asmdata.asmcfi.cfa_offset(list,NR_FRAME_POINTER_REG,-(2*2));
                if current_procinfo.procdef.proctypeoption<>potype_exceptfilter then
                  begin
                    list.concat(Taicpu.op_reg_const(A_LD,NR_FRAME_POINTER_REG,0));
                    list.concat(Taicpu.op_reg_reg(A_ADD,NR_FRAME_POINTER_REG,NR_STACK_POINTER_REG))
                  end
                else
                  begin
                    internalerror(2020040301);
                    (*push_regs;
                    gen_load_frame_for_exceptfilter(list);
                    { Need only as much stack space as necessary to do the calls.
                      Exception filters don't have own local vars, and temps are 'mapped'
                      to the parent procedure.
                      maxpushedparasize is already aligned at least on x86_64. }
                    localsize:=current_procinfo.maxpushedparasize;*)
                  end;
                current_asmdata.asmcfi.cfa_def_cfa_register(list,NR_FRAME_POINTER_REG);
              end
            else
              begin
                CGmessage(cg_d_stackframe_omited);
              end;

            { allocate stackframe space }
            if (localsize<>0) or
               ((target_info.stackalign>sizeof(pint)) and
                (stackmisalignment <> 0) and
                ((pi_do_call in current_procinfo.flags) or
                 (po_assembler in current_procinfo.procdef.procoptions))) then
              begin
                if target_info.stackalign>sizeof(pint) then
                  localsize := align(localsize+stackmisalignment,target_info.stackalign)-stackmisalignment;
                g_stackpointer_alloc(list,localsize);
                if current_procinfo.framepointer=NR_STACK_POINTER_REG then
                  current_asmdata.asmcfi.cfa_def_cfa_offset(list,regsize+localsize+sizeof(pint));
                current_procinfo.final_localsize:=localsize;
              end
          end;
      end;


    procedure tcgz80.g_proc_exit(list : TAsmList;parasize : longint;nostackframe:boolean);
      var
        regs : tcpuregisterset;
        reg : TSuperRegister;
        LocalSize : longint;
        stacksize : longint;
      begin
        { every byte counts for Z80, so if a subroutine is marked as non-returning, we do
          not generate any exit code, so we really trust the noreturn directive
        }
        if po_noreturn in current_procinfo.procdef.procoptions then
          exit;

        { remove stackframe }
        if not nostackframe then
          begin
            stacksize:=current_procinfo.calc_stackframe_size;
            if (target_info.stackalign>4) and
               ((stacksize <> 0) or
                (pi_do_call in current_procinfo.flags) or
                { can't detect if a call in this case -> use nostackframe }
                { if you (think you) know what you are doing              }
                (po_assembler in current_procinfo.procdef.procoptions)) then
              stacksize := align(stacksize+sizeof(aint),target_info.stackalign) - sizeof(aint);
            if (current_procinfo.framepointer=NR_STACK_POINTER_REG) then
              begin
                if stacksize<>0 then
                  a_adjust_sp(list,stacksize);
              end
            else
              begin
                list.Concat(taicpu.op_reg_reg(A_LD,NR_STACK_POINTER_REG,NR_FRAME_POINTER_REG));
                list.Concat(taicpu.op_reg(A_POP,NR_FRAME_POINTER_REG));
              end;
            list.concat(tai_regalloc.dealloc(current_procinfo.framepointer,nil));
          end;

        list.concat(taicpu.op_none(A_RET));
      end;


    procedure tcgz80.a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);
      var
        tmpref : treference;
      begin
        if assigned(ref.symbol) then
          begin
            reference_reset(tmpref,0,[]);
            tmpref.symbol:=ref.symbol;
            tmpref.offset:=ref.offset;

            tmpref.refaddr:=addr_lo8;
            list.concat(taicpu.op_reg_ref(A_LD,r,tmpref));

            tmpref.refaddr:=addr_hi8;
            list.concat(taicpu.op_reg_ref(A_LD,GetNextReg(r),tmpref));

            if (ref.base<>NR_NO) then
              a_op_reg_reg(list,OP_ADD,OS_16,ref.base,r);
            if (ref.index<>NR_NO) then
              a_op_reg_reg(list,OP_ADD,OS_16,ref.index,r);
          end
        else if ref.base=NR_IX then
          begin
            list.concat(taicpu.op_reg(A_PUSH,NR_IX));
            getcpuregister(list,NR_H);
            getcpuregister(list,NR_L);
            list.concat(taicpu.op_reg(A_POP,NR_HL));
            emit_mov(list,r,NR_L);
            ungetcpuregister(list,NR_L);
            emit_mov(list,GetNextReg(r),NR_H);
            ungetcpuregister(list,NR_H);
            if (ref.index<>NR_NO) then
              a_op_reg_reg(list,OP_ADD,OS_16,ref.index,r);
            if ref.offset<>0 then
              a_op_const_reg(list,OP_ADD,OS_16,ref.offset,r);
          end
        else if (ref.base=NR_SP) or (ref.base=NR_BC) or (ref.base=NR_DE) then
          begin
            getcpuregister(list,NR_H);
            getcpuregister(list,NR_L);
            list.Concat(taicpu.op_reg_const(A_LD,NR_HL,ref.offset));
            list.Concat(taicpu.op_reg_reg(A_ADD,NR_HL,ref.base));
            emit_mov(list,r,NR_L);
            ungetcpuregister(list,NR_L);
            emit_mov(list,GetNextReg(r),NR_H);
            ungetcpuregister(list,NR_H);
            if (ref.index<>NR_NO) then
              a_op_reg_reg(list,OP_ADD,OS_16,ref.index,r);
          end
        else if ref.base<>NR_NO then
          begin
            a_op_const_reg_reg(list,OP_ADD,OS_16,ref.offset,ref.base,r);
            if (ref.index<>NR_NO) then
              a_op_reg_reg(list,OP_ADD,OS_16,ref.index,r);
          end
        else if ref.index<>NR_NO then
          a_op_const_reg_reg(list,OP_ADD,OS_16,ref.offset,ref.index,r)
        else
          a_load_const_reg(list,OS_16,ref.offset,r);
      end;


    procedure tcgz80.g_concatcopy(list : TAsmList;const source,dest : treference;len : tcgint);
      var
        tmpreg,srcreg,dstreg: tregister;
        srcref,dstref : treference;
        i: Integer;
      begin
        if (len<=2) and
           is_ref_in_opertypes(source,[OT_REF_IX_d,OT_REF_IY_d,OT_REF_HL]) and
           is_ref_in_opertypes(dest,[OT_REF_IX_d,OT_REF_IY_d,OT_REF_HL]) then
          begin
            srcref:=source;
            dstref:=dest;
            tmpreg:=getintregister(list,OS_8);
            for i:=1 to len do
              begin
                list.concat(taicpu.op_reg_ref(A_LD,tmpreg,srcref));
                list.concat(taicpu.op_ref_reg(A_LD,dstref,tmpreg));
                if i<>len then
                  begin
                    adjust_normalized_ref(list,srcref,1);
                    adjust_normalized_ref(list,dstref,1);
                  end;
              end;
          end
        else
          begin
            srcreg:=getintregister(list,OS_16);
            a_loadaddr_ref_reg(list,source,srcreg);
            dstreg:=getintregister(list,OS_16);
            a_loadaddr_ref_reg(list,dest,dstreg);
            getcpuregister(list,NR_L);
            a_load_reg_reg(list,OS_8,OS_8,srcreg,NR_L);
            getcpuregister(list,NR_H);
            a_load_reg_reg(list,OS_8,OS_8,GetNextReg(srcreg),NR_H);
            getcpuregister(list,NR_E);
            a_load_reg_reg(list,OS_8,OS_8,dstreg,NR_E);
            getcpuregister(list,NR_D);
            a_load_reg_reg(list,OS_8,OS_8,GetNextReg(dstreg),NR_D);
            getcpuregister(list,NR_B);
            getcpuregister(list,NR_C);
            list.concat(taicpu.op_reg_const(A_LD,NR_BC,len));
            list.concat(taicpu.op_none(A_LDIR));
            ungetcpuregister(list,NR_B);
            ungetcpuregister(list,NR_C);
            ungetcpuregister(list,NR_D);
            ungetcpuregister(list,NR_E);
            ungetcpuregister(list,NR_H);
            ungetcpuregister(list,NR_L);
          end;
      end;


    procedure tcgz80.g_overflowcheck(list: TAsmList; const l: tlocation; def: tdef);
      var
        hl : tasmlabel;
        ai : taicpu;
        cond : TAsmCond;
      begin
        list.Concat(tai_comment.Create(strpnew('WARNING! not implemented: g_overflowCheck')));
        //if not(cs_check_overflow in current_settings.localswitches) then
        // exit;
        //current_asmdata.getjumplabel(hl);
        //if not ((def.typ=pointerdef) or
        //       ((def.typ=orddef) and
        //        (torddef(def).ordtype in [u64bit,u16bit,u32bit,u8bit,uchar,
        //                                  pasbool8,pasbool16,pasbool32,pasbool64]))) then
        //  cond:=C_VC
        //else
        //  cond:=C_CC;
        //ai:=Taicpu.Op_Sym(A_BRxx,hl);
        //ai.SetCondition(cond);
        //ai.is_jmp:=true;
        //list.concat(ai);
        //
        //a_call_name(list,'FPC_OVERFLOW',false);
        //a_label(list,hl);
      end;


    procedure tcgz80.g_save_registers(list: TAsmList);
      begin
        { this is done by the entry code }
      end;


    procedure tcgz80.g_restore_registers(list: TAsmList);
      begin
        { this is done by the exit code }
      end;


    procedure tcgz80.a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
      begin
        case cond of
          OC_EQ:
            a_jmp_unsigned_cmp_3way(list,nil,l,nil);
          OC_NE:
            a_jmp_unsigned_cmp_3way(list,l,nil,l);
          OC_A:
            a_jmp_unsigned_cmp_3way(list,nil,nil,l);
          OC_B:
            a_jmp_unsigned_cmp_3way(list,l,nil,nil);
          OC_AE:
            a_jmp_unsigned_cmp_3way(list,nil,l,l);
          OC_BE:
            a_jmp_unsigned_cmp_3way(list,l,l,nil);
          OC_GT:
            a_jmp_signed_cmp_3way(list,nil,nil,l);
          OC_LT:
            a_jmp_signed_cmp_3way(list,l,nil,nil);
          OC_GTE:
            a_jmp_signed_cmp_3way(list,nil,l,l);
          OC_LTE:
            a_jmp_signed_cmp_3way(list,l,l,nil);
          else
            internalerror(2011082501);
        end;
      end;


    procedure tcgz80.emit_mov(list: TAsmList;reg2: tregister; reg1: tregister);
      var
         instr: taicpu;
      begin
       instr:=taicpu.op_reg_reg(A_LD,reg2,reg1);
       list.Concat(instr);
       { Notify the register allocator that we have written a move instruction so
         it can try to eliminate it. }
       add_move_instruction(instr);
      end;


    procedure tcg64fz80.a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);
      begin
         if not(size in [OS_S64,OS_64]) then
           internalerror(2012102402);
         tcgz80(cg).a_op_reg_reg_internal(list,Op,size,regsrc.reglo,regsrc.reghi,regdst.reglo,regdst.reghi);
      end;


    procedure tcg64fz80.a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);
      begin
        tcgz80(cg).a_op_const_reg_internal(list,Op,size,value,reg.reglo,reg.reghi);
      end;


    procedure create_codegen;
      begin
        cg:=tcgz80.create;
        cg64:=tcg64fz80.create;
      end;

end.
