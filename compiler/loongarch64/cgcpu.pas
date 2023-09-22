{
    Copyright (C) 2022 Loongson Technology Corporation Limited.

    This unit implements the code generator for the LoongArch64

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

{$I fpcdefs.inc}

  interface

    uses
      globtype, symtype, symdef, symsym,
      cgbase, cgobj,
      aasmbase, aasmcpu, aasmtai,aasmdata,
      cpubase, cpuinfo, cgutils, rgcpu,
      parabase;

    type
      tfixref = (
        fr_normal, { Normal type, reg + 12i }
        fr_big, { Reg + 14i }
        fr_reg { Reg + reg }
      );
      tcgloongarch64 = class(tcg)
      public
        procedure init_register_allocators; override;
        procedure done_register_allocators; override;

        procedure a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const cgpara : TCGPara);override;
        { call instructions }
        procedure a_call_name(list : TAsmList;const s : string; weak: boolean); override;
        procedure a_call_reg(list : TAsmList;reg: tregister); override;

        { move instructions }
        procedure a_load_const_reg(list: TAsmList; size: tcgsize; a: tcgint; register: tregister); override;
        procedure a_load_const_ref(list: TAsmList; size: tcgsize; a: tcgint; const ref: treference); override;
        procedure a_load_reg_ref(list: TAsmList; fromsize, tosize: TCGSize; reg: tregister; const ref: treference); override;
        procedure a_load_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister); override;
        procedure a_load_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister); override;
        procedure a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);override;

        { bit scan instructions }
        procedure a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; srcsize, dstsize: tcgsize; src, dst: TRegister); override;

        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister); override;
        procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister); override;
        procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference); override;

        { basic arithmetic operations }
        procedure a_op_const_reg(list: TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister); override;
        procedure a_op_reg_reg(list: TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;
        procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister); override;
        procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister); override;

        { comparison operations }
        procedure a_cmp_const_reg_label(list: TAsmList; size: tcgsize; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel); override;
        procedure a_cmp_reg_reg_label(list: TAsmList; size: tcgsize; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel); override;

        { jump instructions }
        procedure a_jmp_name(list: TAsmList;const s : string); override;
        procedure a_jmp_always(list: TAsmList;l: tasmlabel); override;

        procedure a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister; setflags: boolean; var ovloc: tlocation); override;
        procedure a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister; setflags: boolean; var ovloc: tlocation); override;


        { mem operations }
        procedure g_concatcopy(list: TAsmList; const source, dest: treference; len: aint); override;

        { overflow check }
        procedure g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef); override;

        { proc entry and exit }
        procedure g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean); override;
        procedure g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean); override;

      protected
        function fixref(list: TAsmList; var ref: treference; mode : tfixref;out tmpreg : tregister): boolean;
        procedure ungetregister(r : tregister;list :TAsmList);
        procedure maybeadjustresult(list: TAsmList; op: topcg; size: tcgsize; dst: tregister);
      end;

    procedure create_codegen;

  { OP_NONE,OP_MOVE,OP_ADD,OP_AND,OP_DIV,OP_IDIV,OP_IMUL,OP_MUL,OP_NEG,
    OP_NOT,OP_OR,OP_SAR,OP_SHL,OP_SHR,OP_SUB,OP_XOR,OP_ROL,OP_ROR }
  const
    TOpCG2AsmConstOp32: Array[topcg] of TAsmOp = (A_NONE,
          A_NONE,A_ADDI_W,A_ANDI,A_NONE,A_NONE,A_NONE,A_NONE,
          A_NONE,A_NONE,A_ORI,A_SRAI_W,A_SLLI_W,A_SRLI_W,
          A_NONE,A_XORI,A_NONE,A_ROTRI_W);
    TOpCG2AsmOp32: Array[topcg] of TAsmOp = (A_NONE,
          A_NONE,A_ADD_W,A_AND,A_DIV_WU,A_DIV_W,A_MUL_W,
          A_MULW_D_WU,A_NONE,A_NONE,A_OR,A_SRA_W,A_SLL_W,A_SRL_W,
          A_SUB_W,A_XOR,A_NONE,A_ROTR_W);
    TOpCG2AsmConstOp: Array[topcg] of TAsmOp = (A_NONE,
          A_NONE,A_ADDI_D,A_ANDI,A_NONE,A_NONE,A_NONE,A_NONE,
          A_NONE,A_NONE,A_ORI,A_SRAI_D,A_SLLI_D,A_SRLI_D,
          A_NONE,A_XORI,A_NONE,A_ROTRI_D);
    TOpCG2AsmOp: Array[topcg] of TAsmOp = (A_NONE,
          A_NONE,A_ADD_D,A_AND,A_DIV_DU,A_DIV_D,A_MUL_D,
          A_MUL_D,A_NONE,A_NONE,A_OR,A_SRA_D,A_SLL_D,A_SRL_D,
          A_SUB_D,A_XOR,A_NONE,A_ROTR_D);


implementation

    uses
      sysutils, cclasses,
      globals, verbose, systems, cutils,
      symconst, fmodule, symtable,
      rgobj, tgobj, cpupi, procinfo, paramgr, cpupara;


    procedure tcgloongarch64.init_register_allocators;
      begin
        inherited init_register_allocators;
        { From GCC REG_ALLOC_ORDER }
        rg[R_INTREGISTER]:=trgintcpu.create(R_INTREGISTER,R_SUBWHOLE,
          [RS_R12,RS_R13,RS_R14,RS_R16,RS_R16,RS_R17,RS_R18,RS_R19,RS_R20,
           RS_R4,RS_R5,RS_R6,RS_R7,RS_R8,RS_R9,RS_R10,RS_R11,
           RS_R1,
           RS_R23,RS_R24,RS_R25,RS_R26,RS_R27,RS_R28,RS_R29,RS_R30,RS_R31],first_int_imreg,[]);
        rg[R_FPUREGISTER]:=trgcpu.create(R_FPUREGISTER,R_SUBNONE,
          [RS_F0,RS_F1,RS_F2,RS_F3,RS_F4,RS_F5,RS_F6,RS_F7,RS_F8,
           RS_F9,RS_F10,RS_F11,RS_F12,RS_F13,RS_F14,RS_F15,RS_F16,
           RS_F17,RS_F18,RS_F19,RS_F20,RS_F21,RS_F22,RS_F23,RS_F24,
           RS_F25,RS_F26,RS_F27,RS_F28,RS_F29,RS_F30,RS_F31],first_fpu_imreg,[]);
      end;


    procedure tcgloongarch64.done_register_allocators;
      begin
        rg[R_INTREGISTER].free;
        rg[R_FPUREGISTER].free;
        inherited done_register_allocators;
      end;


    procedure tcgloongarch64.a_call_reg(list : TAsmList;reg: tregister);
      begin
        list.concat(taicpu.op_reg_reg_const(A_JIRL,NR_RETURN_ADDRESS_REG,reg,0));
        include(current_procinfo.flags,pi_do_call);
      end;


    procedure tcgloongarch64.a_call_name(list : TAsmList;const s : string; weak: boolean);
      var
        href: treference;
      begin
        if not(weak) then
          reference_reset_symbol(href,current_asmdata.RefAsmSymbol(s,AT_FUNCTION),0,0,[])
        else
          reference_reset_symbol(href,current_asmdata.WeakRefAsmSymbol(s,AT_FUNCTION),0,0,[]);
        if cs_create_pic in current_settings.moduleswitches then
          { PIC need using global symbol through GOT.  }
          begin
            href.refaddr:=addr_plt;
            list.concat(taicpu.op_ref(A_BL,href));
          end
        else
          begin
            if not(weak) then
              href.refaddr:=addr_pcrel
            else
              href.refaddr:=addr_plt;
            list.concat(taicpu.op_ref(A_BL,href));
          end;
        { not assigned while generating external wrappers }
        if assigned(current_procinfo) then
          include(current_procinfo.flags,pi_do_call);
      end;


    procedure tcgloongarch64.a_load_ref_cgpara(list : TAsmList;size : tcgsize;const r : treference;const cgpara : TCGPara);
      var
        tmpref, ref: treference;
        tmpreg: tregister;
        location: pcgparalocation;
        orgsizeleft,
        sizeleft: tcgint;
        usesize: tcgsize;
        reghasvalue: boolean;
      begin
        location:=cgpara.location;
        tmpref:=r;
        sizeleft:=cgpara.intsize;
        repeat
          paramanager.allocparaloc(list,location);
          case location^.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 if (size<>OS_NO) and
                    (tcgsize2size[size]<=sizeof(aint)) then
                   begin
                     a_load_ref_reg(list,size,location^.size,tmpref,location^.register);
                     if location^.shiftval<0 then
                       a_op_const_reg(list,OP_SHL,location^.size,-location^.shiftval,location^.register);
                   end
                 { there's a lot more data left, and the current paraloc's
                   register is entirely filled with part of that data }
                 else if (sizeleft>sizeof(aint)) then
                   begin
                     a_load_ref_reg(list,location^.size,location^.size,tmpref,location^.register);
                   end
                 { we're at the end of the data, and it can be loaded into
                   the current location's register with a single regular
                   load }
                 else if sizeleft in [1,2,4,8] then
                   begin
                     a_load_ref_reg(list,int_cgsize(sizeleft),location^.size,tmpref,location^.register);
                     if location^.shiftval<0 then
                       a_op_const_reg(list,OP_SHL,location^.size,-location^.shiftval,location^.register);
                   end
                 { we're at the end of the data, and we need multiple loads
                   to get it in the register because it's an irregular size }
                 else
                   begin
                     { should be the last part }
                     if assigned(location^.next) then
                       internalerror(2022111934);
                     { load the value piecewise to get it into the register }
                     orgsizeleft:=sizeleft;
                     reghasvalue:=false;
{$ifdef cpu64bitalu}
                     if sizeleft>=4 then
                       begin
                         a_load_ref_reg(list,OS_32,location^.size,tmpref,location^.register);
                         dec(sizeleft,4);
                         inc(tmpref.offset,4);
                         reghasvalue:=true;
                       end;
{$endif cpu64bitalu}
                     if sizeleft>=2 then
                       begin
                         tmpreg:=getintregister(list,location^.size);
                         a_load_ref_reg(list,OS_16,location^.size,tmpref,tmpreg);
                         dec(sizeleft,2);
                         if reghasvalue then
                           begin
                             a_op_const_reg(list,OP_SHL,location^.size,(orgsizeleft-(sizeleft+2))*8,tmpreg);
                             a_op_reg_reg(list,OP_OR,location^.size,tmpreg,location^.register);
                           end
                         else
                           begin
                             a_load_reg_reg(list,location^.size,location^.size,tmpreg,location^.register);
                           end;
                         inc(tmpref.offset,2);
                         reghasvalue:=true;
                       end;
                     if sizeleft=1 then
                       begin
                         tmpreg:=getintregister(list,location^.size);
                         a_load_ref_reg(list,OS_8,location^.size,tmpref,tmpreg);
                         dec(sizeleft,1);
                         if reghasvalue then
                           begin
                             a_op_const_reg(list,OP_SHL,location^.size,(orgsizeleft-(sizeleft+1))*8,tmpreg);
                             a_op_reg_reg(list,OP_OR,location^.size,tmpreg,location^.register)
                           end
                         else
                           a_load_reg_reg(list,location^.size,location^.size,tmpreg,location^.register);
                         inc(tmpref.offset);
                       end;
                     if location^.shiftval<0 then
                       a_op_const_reg(list,OP_SHL,location^.size,-location^.shiftval,location^.register);
                     { the loop will already adjust the offset and sizeleft }
                     dec(tmpref.offset,orgsizeleft);
                     sizeleft:=orgsizeleft;
                   end;
              end;
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                reference_reset_base(ref,location^.reference.index,location^.reference.offset,ctempposinvalid,newalignment(cgpara.alignment,cgpara.intsize-sizeleft),[]);
                a_load_ref_cgparalocref(list,size,sizeleft,tmpref,ref,cgpara,location);
              end;
            LOC_FPUREGISTER,LOC_CFPUREGISTER:
              begin
                { can be not a float size in case of a record passed in fpu registers }
                { the size comparison is to catch F128 passed in two 64 bit floating point registers }
                if is_float_cgsize(size) and
                   (tcgsize2size[location^.size]>=tcgsize2size[size]) then
                  usesize:=size
                else
                  usesize:=location^.size;
                a_loadfpu_ref_reg(list,usesize,location^.size,tmpref,location^.register);
              end
            else
              internalerror(2022111935);
          end;
          inc(tmpref.offset,tcgsize2size[location^.size]);
          dec(sizeleft,tcgsize2size[location^.size]);
          location:=location^.next;
        until not assigned(location);
      end;

    procedure tcgloongarch64.a_load_const_ref(list: TAsmList; size: tcgsize; a: tcgint; const ref: treference);
      begin
        if a=0 then
          a_load_reg_ref(list,size,size,NR_R0,ref)
        else
          inherited a_load_const_ref(list,size,a,ref);
      end;


    procedure tcgloongarch64.a_load_reg_ref(list: TAsmList; fromsize, tosize: TCGSize;
        reg: tregister; const ref: treference);
      var
        href: treference;
        op: TAsmOp;
        hlist: TAsmList;
        tmpreg : tregister;
      const
        st_ops: array[boolean,OS_8..OS_INT] of TAsmOp = (
          (A_ST_B,A_ST_H,A_ST_W,A_ST_D),
          (A_STX_B,A_STX_H,A_STX_W,A_STX_D)
        );
        stptr_ops: array[OS_8..OS_INT] of TAsmOp = (A_NONE,A_NONE,A_STPTR_W,A_STPTR_D);
      begin
        if not (fromsize in [OS_8..OS_INT,OS_S8..OS_SINT]) then
          internalerror(2022111936);
        if not (tosize in [OS_8..OS_INT,OS_S8..OS_SINT]) then
          internalerror(2022111937);

        hlist:=TAsmList.create;
        tosize:=tcgsize2unsigned[tosize];
        if stptr_ops[tosize]<>A_NONE then
          begin
            href:=ref;
            if fixref(hlist,href,fr_big,tmpreg) then
              begin
                list.concatList(hlist);
                hlist.free;
                list.concat(taicpu.op_reg_ref(stptr_ops[tosize],reg,href));
                exit;
              end
            else
              if (tmpreg<>NR_NO) then
                ungetregister(tmpreg,hlist);
          end;
        hlist.Clear;
        hlist.free;
        href:=ref;
        op:=st_ops[fixref(list,href,fr_reg,tmpreg),tosize];
        list.concat(taicpu.op_reg_ref(op,reg,href));
      end;


    procedure tcgloongarch64.a_load_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister);
      var
        href: treference;
        op: TAsmOp;
        usizef,usizet: tcgsize;
        have_done: boolean;
        hlist: TAsmList;
        samesign: boolean;
        tmpreg : tregister;
      const
        ld_ops: array[boolean,boolean,OS_8..OS_INT] of TAsmOp = (
          ((A_LD_B,A_LD_H,A_LD_W,A_LD_D),
           (A_LD_BU,A_LD_HU,A_LD_WU,A_LD_D)),
          ((A_LDX_B,A_LDX_H,A_LDX_W,A_LDX_D),
           (A_LDX_BU,A_LDX_HU,A_LDX_WU,A_LDX_D))
        );
      begin
        tmpreg:=NR_NO;
        if not (fromsize in [OS_8..OS_INT,OS_S8..OS_SINT]) then
          internalerror(2022111938);
        if not (tosize in [OS_8..OS_INT,OS_S8..OS_SINT]) then
          internalerror(2022111939);

        hlist:=TAsmList.create;
        have_done:=false;
        usizef:=tcgsize2unsigned[fromsize];
        usizet:=tcgsize2unsigned[tosize];
        samesign:=((fromsize=usizef) and (tosize=usizet)) or ((fromsize<>usizef) and (tosize<>usizet));
        if (fromsize=OS_S32) then
          begin
            href:=ref;
            if fixref(hlist,href,fr_big,tmpreg) then
              begin
                hlist.concat(taicpu.op_reg_ref(A_LDPTR_W,reg,href));
                have_done:=true;
              end;
          end
        else if (fromsize=OS_S64) or (fromsize=OS_64) then
          begin
            href:=ref;
            if fixref(hlist,href,fr_big,tmpreg) then
              begin
                hlist.concat(taicpu.op_reg_ref(A_LDPTR_D,reg,href));
                have_done:=true;
              end;
          end;

        if not(have_done) then
          begin
            if (tmpreg<>NR_NO) then
              ungetregister(tmpreg,hlist);
            hlist.Clear;
            href:=ref;
            op:=ld_ops[fixref(list,href,fr_reg,tmpreg),fromsize=usizef,usizef];
            list.concat(taicpu.op_reg_ref(op,reg,href));
          end
        else
          list.concatList(hlist);

        hlist.free;
        { First, when load a reg to OS_(S)INT, we use signed load operations.
          Then, when fromsize is less or equal than tosize, we can not do
          a_load_reg_reg because of signed load operations. }
        if (fromsize<>tosize) and (not (tosize in [OS_SINT,OS_INT])) and (not((usizef<=usizet) and samesign)) then
          a_load_reg_reg(list,fromsize,tosize,reg,reg);
      end;


    procedure tcgloongarch64.a_load_const_reg(list: TAsmList; size: tcgsize; a: tcgint; register: tregister);
      begin
        case size of
          OS_S8: a:= tcgint(shortint(a));
          OS_S16: a:= tcgint(smallint(a));
          OS_S32: a:= tcgint(longint(a));
        else
          ;
        end;
        list.concat(taicpu.op_reg_const(A_LI_D,register,a));
      end;


    procedure tcgloongarch64.a_load_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister);
      const
        zeroextbits: array[OS_8..OS_INT] of longint=(7,15,31,63);
        signextop: array[OS_8..OS_INT] of TAsmOp=(A_EXT_W_B,A_EXT_W_H,A_ADDI_W,A_MOVE);
      var
        ai: taicpu;
        ufromsize,utosize: tcgsize;
        ufrom,uto : boolean;
      begin
        if not (fromsize in [OS_8..OS_INT,OS_S8..OS_SINT]) then
          internalerror(2022111940);
        if not (tosize in [OS_8..OS_INT,OS_S8..OS_SINT]) then
          internalerror(2022111941);
        ufromsize:=tcgsize2unsigned[fromsize];
        utosize:=tcgsize2unsigned[tosize];
        ufrom:=ufromsize=fromsize;
        uto:=utosize=tosize;
        if (fromsize=tosize) or ((ufromsize=OS_INT) and (utosize=OS_INT)) then
          begin
            ai:=taicpu.op_reg_reg(A_MOVE,reg2,reg1);
            list.concat(ai);
            rg[R_INTREGISTER].add_move_instruction(ai);
          end
        else if ufromsize>=utosize then
          begin
            if uto then
              list.concat(taicpu.op_reg_reg_const_const(A_BSTRPICK_D,reg2,reg1,zeroextbits[utosize],0))
            else if utosize=OS_32 then
              list.concat(taicpu.op_reg_reg_const(signextop[utosize],reg2,reg1,0))
            else
              list.concat(taicpu.op_reg_reg(signextop[utosize],reg2,reg1));
          end
        else { ufromsize<utosize }
          begin
            if ufrom then
              list.concat(taicpu.op_reg_reg_const_const(A_BSTRPICK_D,reg2,reg1,zeroextbits[ufromsize],0))
            else
              begin
                if ufromsize=OS_32 then
                  list.concat(taicpu.op_reg_reg_const(signextop[ufromsize],reg2,reg1,0))
                else
                  list.concat(taicpu.op_reg_reg(signextop[ufromsize],reg2,reg1));
                if uto then
                  list.concat(taicpu.op_reg_reg_const_const(A_BSTRPICK_D,reg2,reg2,zeroextbits[utosize],0));
              end;
          end;
      end;


    procedure tcgloongarch64.a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);
      var
        href: treference;
        l: TAsmLabel;
        tmpreg : tregister;
      begin
        href:=ref;
        fixref(list,href,fr_normal,tmpreg);
        { Fixref, so simplely work here. }
        if href.offset=0 then
          a_load_reg_reg(list,OS_ADDR,OS_ADDR,href.base,r)
        else if is_simm12(href.offset) and (href.base<>NR_NO) then
          list.concat(taicpu.op_reg_reg_const(A_ADDI_D,r,href.base,href.offset))
        else
          internalerror(2022111942);
      end;


    procedure tcgloongarch64.a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; srcsize, dstsize: tcgsize; src, dst: TRegister);
      begin
        internalerror(2022111943);
      end;


    { dstreg = dstreg op imm }
    procedure tcgloongarch64.a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister);
      begin
        a_op_const_reg_reg(list,op,size,a,reg,reg);
      end;


    { dstreg = dstreg op srcreg }
    procedure tcgloongarch64.a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister);
      begin
        a_op_reg_reg_reg(list,op,size,src,dst,dst);
      end;


    procedure tcgloongarch64.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister);
      var
        tmpreg: TRegister;
        usetmp: boolean;
      begin
        optimize_op_const(size,op,a);

        if op=OP_NONE then
          begin
            a_load_reg_reg(list,size,size,src,dst);
            exit;
          end;

        { Sub const to add -const }
        if op=OP_SUB then
          begin
            op:=OP_ADD;
            a:=-a;
          end;

        { Make sure shift operation const not overflow. }
        if op in [OP_SAR,OP_SHL,OP_SHR] then
          if (size in [OS_32,OS_S32]) and (a>31) then
            a:=31
          else if (size in [OS_16,OS_S16]) and (a>15) then
            a:=15
          else if (size in [OS_8,OS_S8]) and (a>7) then
            a:=7
          else if (size in [OS_64,OS_S64]) and (a>63) then
            a:=63;

        { Rotate imm bits left to rotate (size - imm) bits right. }
        if op=OP_ROL then
          begin
            op:=OP_ROR;
            if size in [OS_32,OS_S32] then
              a:=32-a
            else if size in [OS_16,OS_S16] then
              a:=16-a
            else if size in [OS_8,OS_S8] then
              a:=8-a
            else
              a:=64-a;
          end;

        { Only effective imm can be used by insn operation. }
        if ((is_simm12(a) and (op=OP_ADD)) or
            (is_uimm12(a) and (op<>OP_ADD))) and
            (TOpCG2AsmConstOp32[op]<>A_NONE)
            { or (TOpCG2AsmConstOp[op]<>A_NONE)} then
          begin
            usetmp:=false;
            tmpreg:=getintregister(list,OS_INT);
            { Size = 16bits or 8bits do special if rotate. }
            if (size in [OS_16,OS_S16]) and (op=OP_ROR) then
              begin
                list.concat(taicpu.op_reg_reg_const_const(A_BSTRINS_W,tmpreg,src,31,16));
                usetmp:=true;
              end
            else if (size in [OS_8,OS_S8]) and (op=OP_ROR) then
              begin
                list.concat(taicpu.op_reg_reg_const_const(A_BSTRINS_W,tmpreg,src,15,8));
                usetmp:=true;
              end
            { Signext to 32bits if sra 16bits or 8bits}
            else if (size in [OS_S16,OS_S8]) and (op=OP_SAR) then
              begin
                a_load_reg_reg(list,size,OS_S32,tmpreg,src);
                usetmp:=true;
              end;

            if size in [OS_64,OS_S64] then
              begin
                { Use tmp cannot be true here. }
                list.concat(taicpu.op_reg_reg_const(TOpCG2AsmConstOp[op],dst,src,a));
                maybeadjustresult(list,op,size,dst);
              end
            else { OS_32/S32, OS_16/S16, OS_8/S8 }
              begin
                if usetmp then
                  list.concat(taicpu.op_reg_reg_const(TOpCG2AsmConstOp32[op],dst,tmpreg,a))
                else
                  list.concat(taicpu.op_reg_reg_const(TOpCG2AsmConstOp32[op],dst,src,a));
                maybeadjustresult(list,op,size,dst);
              end;
          end
        else
          begin
            tmpreg:=getintregister(list,OS_INT);
            a_load_const_reg(list,size,a,tmpreg);
            a_op_reg_reg_reg(list,op,size,tmpreg,src,dst);
          end;
      end;


    procedure tcgloongarch64.a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister);
      var
        tmpreg1, tmpreg2: TRegister;
        usetmp1, usetmp2: boolean;
      begin
        usetmp1:=false;
        usetmp2:=false;
        tmpreg1:=getintregister(list,OS_INT);
        tmpreg2:=getintregister(list,OS_INT);

        if op=OP_NOT then
          begin
            list.concat(taicpu.op_reg_reg_reg(A_NOR,dst,NR_R0,src1));
            maybeadjustresult(list,op,size,dst);
            exit;
          end
        else if op=OP_NEG then
          begin
            list.concat(taicpu.op_reg_reg_reg(A_SUB_D,dst,NR_R0,src1));
            maybeadjustresult(list,op,size,dst);
            exit;
          end
        else if op=OP_MOVE then
          begin
            a_load_reg_reg(list,size,size,src1,dst);
            exit;
          end
        else if op=OP_ROL then
          begin
            list.concat(taicpu.op_reg_reg_reg(A_SUB_D,tmpreg1,NR_R0,src1));
            usetmp1:=true;
            op:=OP_ROR;
          end;

        if (TOpCG2AsmOp32[op]<>A_NONE) {or (TOpCG2AsmConstOp[op]<>A_NONE)} then
          begin
            { Size = 16bits or 8bits do special if rotate. }
            if (size in [OS_16,OS_S16]) and (op=OP_ROR) then
              begin
                list.concat(taicpu.op_reg_reg_const_const(A_BSTRINS_W,tmpreg2,src2,31,16));
                usetmp2:=true;
              end
            else if (size in [OS_8,OS_S8]) and (op=OP_ROR) then
              begin
                list.concat(taicpu.op_reg_reg_const_const(A_BSTRINS_W,tmpreg2,src2,15,8));
                list.concat(taicpu.op_reg_reg_const_const(A_BSTRINS_W,tmpreg2,tmpreg2,31,16));
                usetmp2:=true;
              end
            { Signext to 32bits if sra 16bits or 8bits}
            else if (size in [OS_S16,OS_S8]) and (op=OP_SAR) then
              begin
                a_load_reg_reg(list,size,OS_S32,src2,tmpreg2);
                usetmp2:=true;
              end;

            if size in [OS_64,OS_S64] then
              begin
                { usetmp2 cannot be true here. }
                if usetmp1 then
                  list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp[op],dst,src2,tmpreg1))
                else
                  list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp[op],dst,src2,src1));
                maybeadjustresult(list,op,size,dst);
              end
            else
              begin
                if usetmp1 and usetmp2 then
                  list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp32[op],dst,tmpreg2,tmpreg1))
                else if usetmp1 then
                  list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp32[op],dst,src2,tmpreg1))
                else if usetmp2 then
                  list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp32[op],dst,tmpreg2,src1))
                else
                  list.concat(taicpu.op_reg_reg_reg(TOpCG2AsmOp32[op],dst,src2,src1));
                maybeadjustresult(list,op,size,dst);
              end;
          end;
      end;


    procedure tcgloongarch64.a_cmp_const_reg_label(list: tasmlist; size: tcgsize; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel);
      var
        tmpreg: tregister;
      begin
        if a = 0 then
          a_cmp_reg_reg_label(list,size,cmp_op,NR_R0,reg,l)
        else
          begin
            tmpreg := GetIntRegister(list,OS_INT);
            a_load_const_reg(list,OS_INT,a,tmpreg);
            a_cmp_reg_reg_label(list, size, cmp_op, tmpreg, reg, l);
          end;
      end;


    procedure tcgloongarch64.a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp; reg1,reg2 : tregister;l : tasmlabel);
      const
        { OC_NONE,OC_EQ,OC_GT,OC_LT,OC_GTE,OC_LTE,OC_NE,OC_BE,OC_B,OC_AE,OC_A }
        TOpCmp2AsmCond: Array[TOpCmp] of TAsmCond = (C_NONE,
          C_EQ,C_GT,C_LT,C_GE,C_LE,C_NE,C_LEU,C_LTU,C_GEU,C_GTU);
        TOpCmp2AsmCondZ: Array[TOpCmp] of TAsmCond = (C_NONE,
          C_EQZ,C_GTZ,C_LTZ,C_GEZ,C_LEZ,C_NEZ,C_NONE,C_NONE,C_NONE,C_NONE);
      var
        href: treference;
        ai: taicpu;
      begin
        reference_reset_symbol(href,l,0,0,[]);
        { It is better to use pcrel instead addr_b16 or addr_b21,
          because we hardly know the real op after optimizing. }
        href.refaddr:=addr_pcrel;
        if (reg1=NR_R0) or (reg2=NR_R0) then
          begin
            if (reg1=NR_R0) and (reg2=NR_R0) then
              begin
                a_jmp_always(list,l);
                exit;
              end
            else if reg2=NR_R0 then
              begin
                reg2:=reg1;
                reg1:=NR_R0;
                cmp_op:=swap_opcmp(cmp_op);
              end;
            if TOpCmp2AsmCondZ[cmp_op]<>C_NONE then
              begin
                ai:=taicpu.op_reg_ref(A_BXX,reg2,href);
                ai.is_jmp:=true;
                ai.condition:=TOpCmp2AsmCondZ[cmp_op];
                list.concat(ai);
                exit;
              end;
          end;
        ai:=taicpu.op_reg_reg_ref(A_BXX,reg2,reg1,href);
        ai.is_jmp:=true;
        ai.condition:=TOpCmp2AsmCond[cmp_op];
        list.concat(ai);
      end;


    procedure tcgloongarch64.a_jmp_name(list : TAsmList;const s : string);
      var
        ai: taicpu;
        href: treference;
      begin
        reference_reset_symbol(href,current_asmdata.RefAsmSymbol(s,AT_FUNCTION),0,0,[]);
        href.refaddr:=addr_b26;
        ai:=taicpu.op_ref(A_B,href);
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcgloongarch64.a_jmp_always(list : TAsmList;l: tasmlabel);
      var
        ai: taicpu;
        href: treference;
      begin
        reference_reset_symbol(href,l,0,0,[]);
        href.refaddr:=addr_b26;
        ai:=taicpu.op_ref(A_B,href);
        ai.is_jmp:=true;
        list.concat(ai);
       end;


    procedure tcgloongarch64.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister);
      var
        op: TAsmOp;
        ai: taicpu;
      const
        FpuConvOp: array[OS_F32..OS_F64,OS_F32..OS_F64] of TAsmOp =
                   ((A_FMOV_S,A_FCVT_D_S),(A_FCVT_S_D,A_FMOV_D));
      begin
        if (reg1<>reg2) or (fromsize<>tosize) then
          begin
            ai:= taicpu.op_reg_reg(fpuconvop[fromsize,tosize],reg2,reg1);
            list.concat(ai);
            if (fromsize=tosize) then
              rg[R_FPUREGISTER].add_move_instruction(ai);
          end;
      end;


    procedure tcgloongarch64.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister);
      var
        op: TAsmOp;
        href: treference;
        tmpreg : tregister;
      const
        fld_ops: array[boolean,boolean] of TAsmOp = (
          (A_FLD_D, A_FLD_S),
          (A_FLDX_D, A_FLDX_S)
        );
      begin
        href:=ref;
        op:=fld_ops[fixref(list,href,fr_reg,tmpreg),fromsize=OS_F32];
        list.concat(taicpu.op_reg_ref(op,reg,href));
        if fromsize<>tosize then
          a_loadfpu_reg_reg(list,fromsize,tosize,reg,reg);
      end;

    procedure tcgloongarch64.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference);
      var
        op: TAsmOp;
        tmpfreg: TRegister;
        href: treference;
        tmpreg : tregister;
        fst_ops: array[boolean,boolean] of TAsmOp = (
          (A_FST_D, A_FST_S),
          (A_FSTX_D, A_FSTX_S)
        );
      begin
        if fromsize<>tosize then
          begin
            tmpfreg:=getfpuregister(list,tosize);
            a_loadfpu_reg_reg(list,fromsize,tosize,reg,tmpfreg);
            reg:=tmpfreg;
          end;

        href:=ref;
        op:=fst_ops[fixref(list,href,fr_reg,tmpreg),tosize=OS_F32];
        list.concat(taicpu.op_reg_ref(op,reg,href));
      end;


    procedure tcgloongarch64.g_concatcopy(list: TAsmList; const source, dest: treference; len: aint);
      var
        tmpreg,countreg: TRegister;
        src,dst: TReference;
        lab: tasmlabel;
        len_8,tmplen: longint;
        len_1,len_2,len_4: boolean;
      begin
        { Size is zero }
        if len=0 then
          exit;
        { It's too large or illegal. }
        if len>high(longint) then
          internalerror(2022111944);
        { len = d*8(+4?)(+2?)(+1?). }
        len_8:=len div 8;
        tmplen:=len-len_8*8;
        len_1:=(tmplen and 1)<>0;
        len_2:=(tmplen and 2)<>0;
        len_4:=(tmplen and 4)<>0;

        { Set the reference. }
        reference_reset(src,sizeof(aint),[]);
        reference_reset(dst,sizeof(aint),[]);
        src.base:=GetAddressRegister(list);
        dst.base:=GetAddressRegister(list);
        src.refaddr:=addr_reg_12i;
        dst.refaddr:=addr_reg_12i;
        a_loadaddr_ref_reg(list,source,src.base);
        a_loadaddr_ref_reg(list,dest,dst.base);
        tmpreg:= GetIntRegister(list, OS_INT);
        { TODO Some optimization. }
        if len_8>0 then
          begin
            current_asmdata.getjumplabel(lab);
            countreg := GetIntRegister(list,OS_INT);
            if len_8>1 then
              begin
                a_load_const_reg(list,OS_INT,len_8,countreg);
                a_label(list, lab);
              end;
            list.concat(taicpu.op_reg_ref(A_LD_D,tmpreg,src));
            list.concat(taicpu.op_reg_ref(A_ST_D,tmpreg,dst));
            if len_1 or len_2 or len_4 or (len_8>1) then
              begin
                list.concat(taicpu.op_reg_reg_const(A_ADDI_D,src.base,src.base,8));
                list.concat(taicpu.op_reg_reg_const(A_ADDI_D,dst.base,dst.base,8));
              end;
            if len_8>1 then
              begin
                list.concat(taicpu.op_reg_reg_const(A_ADDI_D,countreg,countreg,-1));
                a_cmp_reg_reg_label(list,OS_INT,OC_GT,NR_R0,countreg,lab);
              end;
          end; { len_8>0 }
        if len_4 then
          begin
            list.concat(taicpu.op_reg_ref(A_LD_W,tmpreg,src));
            list.concat(taicpu.op_reg_ref(A_ST_W,tmpreg,dst));
            inc(src.offset,4);
            inc(dst.offset,4);
          end;
        if len_2 then
          begin
            list.concat(taicpu.op_reg_ref(A_LD_H,tmpreg,src));
            list.concat(taicpu.op_reg_ref(A_ST_H,tmpreg,dst));
            inc(src.offset,2);
            inc(dst.offset,2);
          end;
        if len_1 then
          begin
            list.concat(taicpu.op_reg_ref(A_LD_B,tmpreg,src));
            list.concat(taicpu.op_reg_ref(A_ST_B,tmpreg,dst));
          end;
      end;


    procedure tcgloongarch64.g_overflowcheck(list: TAsmList; const loc: tlocation; def: tdef);
      begin
        { TODO }
        { internalerror(2022111945); }
      end;


    procedure tcgloongarch64.g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean);
      var
        regs, fregs: tcpuregisterset;
        r: TSuperRegister;
        href: treference;
        stackcount, stackAdjust: longint;
      begin
        if not(nostackframe) then
          begin
            a_reg_alloc(list,NR_STACK_POINTER_REG);
            { Always use $fp. }
            a_reg_alloc(list,NR_FRAME_POINTER_REG);
            { Int registers }
            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);
            regs:=regs+[RS_FRAME_POINTER_REG];
            { Does it call another function? }
            if (pi_do_call in current_procinfo.flags) or
               (RS_R1 in rg[R_INTREGISTER].used_in_proc) then
              regs:=regs+[RS_RETURN_ADDRESS_REG];
            { Float registers }
            fregs:=rg[R_FPUREGISTER].used_in_proc-paramanager.get_volatile_registers_fpu(pocall_stdcall);
            { Calculate the stackcount of all regesters. }
            stackcount:=16;
            for r:=RS_R1 to RS_R31 do
              if (r in regs) and (r<>RS_FRAME_POINTER_REG) and (r<>RS_RETURN_ADDRESS_REG) then
                inc(stackcount,8);
            for r:=RS_F0 to RS_F31 do
              if r in fregs then
                inc(stackcount,8);
            { Calculate the frame total size. }
            inc(localsize,stackcount);
            if localsize=0 then
              exit;
            { ADDI instructions only has 12bits-sign-imm range, [-2048,2047]. Once we
              use -2048 in the prologue, we cannot get back in epilogue use one ADDI.
              Due to LoongArch psABI, we should save the $ra and then use it as free.
              We should make $fp as $fp on entry, so ADDI cannot do 2048 size. It
              seems use -2032 may a good choice without care of many of cases. }
            if (-localsize<-2032) and (not (RS_RETURN_ADDRESS_REG in regs)) then
              begin
                regs:=regs+[RS_RETURN_ADDRESS_REG];
                inc(localsize,8);
                inc(stackcount,8);
              end;
            if (localsize mod 16)<>0 then
              inc(localsize,16-(localsize mod 16));
            { Do first decrease the stack, and record the stackadjust. }
            stackadjust:=0;
            if (-localsize<-2032) then
              begin
                list.concat(taicpu.op_reg_reg_const(A_ADDI_D,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,-2032));
                reference_reset_base(href,NR_STACK_POINTER_REG,2032-8,ctempposinvalid,0,[]);
                href.refaddr:=addr_reg_12i;
                stackadjust:=2032;
                current_asmdata.asmcfi.cfa_def_cfa_offset(list,2032);
              end
            else
              begin
                list.concat(taicpu.op_reg_reg_const(A_ADDI_D,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,-localsize));
                reference_reset_base(href,NR_STACK_POINTER_REG,localsize-8,ctempposinvalid,0,[]);
                href.refaddr:=addr_reg_12i;
                stackadjust:=localsize;
                current_asmdata.asmcfi.cfa_def_cfa_offset(list,localsize);
              end;
            { $ra in cfa -8. }
            if RS_RETURN_ADDRESS_REG in regs then
              begin
                list.concat(taicpu.op_reg_ref(A_ST_D,newreg(R_INTREGISTER,RS_RETURN_ADDRESS_REG,R_SUBWHOLE),href));
                current_asmdata.asmcfi.cfa_offset(list,NR_RETURN_ADDRESS_REG,-8);
              end;
            { $fp in cfa -16. }
            dec(href.offset,8);
            list.concat(taicpu.op_reg_ref(A_ST_D,newreg(R_INTREGISTER,RS_FRAME_POINTER_REG,R_SUBWHOLE),href));
            current_asmdata.asmcfi.cfa_offset(list,NR_FRAME_POINTER_REG,-16);
            { $fp = cfa. }
            list.concat(taicpu.op_reg_reg_const(A_ADDI_D,NR_FRAME_POINTER_REG,NR_STACK_POINTER_REG,stackadjust));
            current_asmdata.asmcfi.cfa_def_cfa(list,NR_FRAME_POINTER_REG,0);
            { Int registers }
            for r:=RS_R1 to RS_R31 do
              if (r in regs) and (r<>RS_FRAME_POINTER_REG) and (r<>RS_RETURN_ADDRESS_REG) then
                begin
                  dec(href.offset,8);
                  list.concat(taicpu.op_reg_ref(A_ST_D,newreg(R_INTREGISTER,r,R_SUBWHOLE),href));
                  current_asmdata.asmcfi.cfa_offset(list,newreg(R_INTREGISTER,r,R_SUBWHOLE),href.offset-stackadjust);
                end;
            { Float registers }
            for r:=RS_F0 to RS_F31 do
              if r in fregs then
                begin
                  dec(href.offset,8);
                  list.concat(taicpu.op_reg_ref(A_FST_D,newreg(R_FPUREGISTER,r,R_SUBWHOLE),href));
                  current_asmdata.asmcfi.cfa_offset(list,newreg(R_FPUREGISTER,r,R_SUBWHOLE),href.offset);
                end;
            { Decrese the remaining stack size. }
            if (localsize-stackadjust)>2048 then
              begin
                a_load_const_reg(list,OS_INT,localsize-stackadjust,NR_RETURN_ADDRESS_REG);
                list.concat(taicpu.op_reg_reg_reg(A_SUB_D,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,NR_RETURN_ADDRESS_REG));
              end
            else if (localsize-stackadjust)>0 then
              begin
                { TODO It seems to no need $ra. }
                list.concat(taicpu.op_reg_reg_const(A_ADDI_D,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,stackadjust-localsize));
              end;
          end;
      end; { g_proc_entry }

    procedure tcgloongarch64.g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean);
      var
        r: tsuperregister;
        regs, fregs: tcpuregisterset;
        stackcount, localsize, stackleft: longint;
        href: treference;
      begin
        if not(nostackframe) then
          begin
            localsize:=current_procinfo.calc_stackframe_size;
            regs:=rg[R_INTREGISTER].used_in_proc-paramanager.get_volatile_registers_int(pocall_stdcall);
            fregs:=rg[R_FPUREGISTER].used_in_proc-paramanager.get_volatile_registers_fpu(pocall_stdcall);
            regs:=regs+[RS_FRAME_POINTER_REG];
            if (pi_do_call in current_procinfo.flags) or
               (RS_R1 in rg[R_INTREGISTER].used_in_proc) then
              regs:=regs+[RS_RETURN_ADDRESS_REG];
            stackcount:=16;
            for r:=RS_R1 to RS_R31 do
              if (r in regs) and (r<>RS_FRAME_POINTER_REG) and (r<>RS_RETURN_ADDRESS_REG) then
                inc(stackcount,8);
            for r:=RS_F0 to RS_F31 do
              if r in fregs then
                inc(stackcount,8);
            inc(localsize,stackcount);
            if (-localsize<-2032) and (not (RS_RETURN_ADDRESS_REG in regs)) then
              begin
                regs:=regs+[RS_RETURN_ADDRESS_REG];
                inc(localsize,8);
                inc(stackcount,8);
              end;
            if (localsize mod 16)<>0 then
              inc(localsize,16-(localsize mod 16));

            stackleft:=0;
            if (-localsize<-2032) then
              stackleft:=2032
            else
              stackleft:=localsize;
            list.concat(taicpu.op_reg_reg_const(A_ADDI_D,NR_STACK_POINTER_REG,NR_FRAME_POINTER_REG,-stackleft));
            reference_reset_base(href,NR_STACK_POINTER_REG,stackleft-8,ctempposinvalid,0,[]);
            href.refaddr:=addr_reg_12i;
            { Restore registers. }
            if RS_RETURN_ADDRESS_REG in regs then
              begin
                list.concat(taicpu.op_reg_ref(A_LD_D,newreg(R_INTREGISTER,RS_RETURN_ADDRESS_REG,R_SUBWHOLE),href));
                current_asmdata.asmcfi.cfa_restore(list,NR_RETURN_ADDRESS_REG);
              end;
            dec(href.offset,8);
            list.concat(taicpu.op_reg_ref(A_LD_D,newreg(R_INTREGISTER,RS_FRAME_POINTER_REG,R_SUBWHOLE),href));
            current_asmdata.asmcfi.cfa_restore(list,NR_FRAME_POINTER_REG);
            current_asmdata.asmcfi.cfa_def_cfa(list,NR_STACK_POINTER_REG,stackleft);
            for r:=RS_R1 to RS_R31 do
              if (r in regs) and (r<>RS_FRAME_POINTER_REG) and (r<>RS_RETURN_ADDRESS_REG) then
                begin
                  dec(href.offset,8);
                  list.concat(taicpu.op_reg_ref(A_LD_D,newreg(R_INTREGISTER,r,R_SUBWHOLE),href));
                  current_asmdata.asmcfi.cfa_restore(list,newreg(R_INTREGISTER,r,R_SUBWHOLE));
                end;
            for r:=RS_F0 to RS_F31 do
              if r in fregs then
                begin
                  dec(href.offset,8);
                  list.concat(taicpu.op_reg_ref(A_FLD_D,newreg(R_FPUREGISTER,r,R_SUBWHOLE),href));
                  current_asmdata.asmcfi.cfa_restore(list,newreg(R_FPUREGISTER,r,R_SUBWHOLE));
                end;
            { Restore $sp. }
            list.concat(taicpu.op_reg_reg_const(A_ADDI_D,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,stackleft));
          end;
        { jr $ra. }
        reference_reset_base(href,NR_RETURN_ADDRESS_REG,0,ctempposinvalid,0,[]);
        href.refaddr:=addr_reg;
        list.concat(taicpu.op_ref(A_JR,href));
      end; { g_proc_exit }


    procedure tcgloongarch64.a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; a: tcgint; src, dst: tregister; setflags: boolean; var ovloc: tlocation);
    var
      signed: Boolean;
      l: TAsmLabel;
      tmpreg: tregister;
      ai: taicpu;
      href: treference;
      tmpreg0,tmpreg1: tregister;
    begin
      if setflags then
        begin
          if is_simm12(a) and (op=OP_ADD) then
            begin
              current_asmdata.getjumplabel(l);
              reference_reset_symbol(href,l,0,0,[]);
              href.refaddr:=addr_pcrel;
              tmpreg1:=getintregister(list,OS_INT);
              if size in [OS_64,OS_S64,OS_32,OS_S32] then
                begin
                  { Backup src so we can compare with it. }
                  a_load_reg_reg(list,OS_INT,OS_INT,src,tmpreg1);
                end;
              if size in [OS_64,OS_S64] then
                list.concat(taicpu.op_reg_reg_const(A_ADDI_D,dst,src,a))
              else
                list.concat(taicpu.op_reg_reg_const(A_ADDI_W,dst,src,a));
              case size of
                OS_S64,OS_S32:
                  begin
                    ai:=taicpu.op_reg_reg_ref(A_BXX,dst,tmpreg1,href);
                    if a<0 then
                      ai.condition:=C_LT
                    else
                      ai.condition:=C_GE;
                  end;
                OS_S16,OS_S8:
                  begin
                    tmpreg0:=getintregister(list,OS_INT);
                    a_load_reg_reg(list,OS_INT,size,dst,tmpreg0);
                    ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg0,dst,href);
                    ai.condition:=C_EQ;
                  end;
                OS_64,OS_32:
                  begin
                    ai:=taicpu.op_reg_reg_ref(A_BXX,dst,tmpreg1,href);
                    ai.condition:=C_GEU;
                  end;
                OS_16,OS_8:
                  begin
                    tmpreg0:=getintregister(list,OS_INT);
                    if size=OS_16 then
                      a_load_const_reg(list,OS_INT,1 shl 16,tmpreg0)
                    else
                      a_load_const_reg(list,OS_INT,1 shl 8,tmpreg0);
                    ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg0,dst,href);
                    ai.condition:=C_GTU;
                  end;
                else
                  internalerror(2022082303);
                end;
                ai.is_jmp:=true;
                list.concat(ai);
              a_call_name(list,'FPC_OVERFLOW',false);
              a_label(list,l);
            end
          else if op in [OP_ADD,OP_SUB,OP_MUL,OP_IMUL,OP_IDIV] then
            begin
              tmpreg:=getintregister(list,size);
              a_load_const_reg(list,size,a,tmpreg);
              a_op_reg_reg_reg_checkoverflow(list,op,size,tmpreg,src,dst,setflags,ovloc);
            end
          else
            internalerror(2022082302);
        end
      else
        a_op_const_reg_reg(list,op,size,a,src,dst);
    end;


    procedure tcgloongarch64.a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tcgsize; src1, src2, dst: tregister; setflags: boolean; var ovloc: tlocation);
      var
      signed: Boolean;
      l: TAsmLabel;
      tmpreg0,tmpreg1,tmpreg2,tmpreg3: tregister;
      ai: taicpu;
      href: treference;
    begin
      signed:=tcgsize2unsigned[size]<>size;

      if setflags then
        case op of
          OP_ADD:
            begin
              current_asmdata.getjumplabel(l);
              reference_reset_symbol(href,l,0,0,[]);
              href.refaddr:=addr_pcrel;
              tmpreg2:=getintregister(list,OS_INT);
              tmpreg3:=getintregister(list,OS_INT);
              if size in [OS_64,OS_S64,OS_32,OS_S32] then
                begin
                  { Backup src so we can compare with it. }
                  a_load_reg_reg(list,OS_INT,OS_INT,src1,tmpreg2);
                  a_load_reg_reg(list,OS_INT,OS_INT,src2,tmpreg3);
                end;
              if size in [OS_S64,OS_64] then
                list.Concat(taicpu.op_reg_reg_reg(A_ADD_D,dst,src2,src1))
              else
                list.Concat(taicpu.op_reg_reg_reg(A_ADD_W,dst,src2,src1));
              case size of
                OS_S64,OS_S32:
                  begin
                    { if (src1<0)<>(dst<src2) overflow; }
                    tmpreg0:=getintregister(list,OS_INT);
                    tmpreg1:=getintregister(list,OS_INT);
                    list.Concat(taicpu.op_reg_reg_const(A_SLTI,tmpreg0,tmpreg2,0));
                    list.Concat(taicpu.op_reg_reg_reg(A_SLT,tmpreg1,dst,tmpreg3));
                    ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg0,tmpreg1,href);
                    ai.condition:=C_EQ;
                  end;
                OS_S16,OS_S8:
                  begin
                    tmpreg0:=getintregister(list,OS_INT);
                    a_load_reg_reg(list,OS_INT,size,dst,tmpreg0);
                    ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg0,dst,href);
                    ai.condition:=C_EQ;
                  end;
                OS_64,OS_32:
                  begin
                    ai:=taicpu.op_reg_reg_ref(A_BXX,dst,tmpreg3,href);
                    ai.condition:=C_GEU;
                  end;
                OS_16,OS_8:
                  begin
                    tmpreg0:=getintregister(list,OS_INT);
                    if size=OS_16 then
                      a_load_const_reg(list,OS_INT,1 shl 16,tmpreg0)
                    else
                      a_load_const_reg(list,OS_INT,1 shl 8,tmpreg0);
                    ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg0,dst,href);
                    ai.condition:=C_GTU;
                  end;
              else
                internalerror(2022082304);
              end;
              ai.is_jmp:=true;
              list.concat(ai);
              a_call_name(list,'FPC_OVERFLOW',false);
              a_label(list,l);
            end;
          OP_SUB:
            begin
              current_asmdata.getjumplabel(l);
              reference_reset_symbol(href,l,0,0,[]);
              href.refaddr:=addr_pcrel;
              tmpreg2:=getintregister(list,OS_INT);
              tmpreg3:=getintregister(list,OS_INT);
              if size in [OS_64,OS_S64,OS_32,OS_S32] then
                begin
                  { Backup src so we can compare with it. }
                  a_load_reg_reg(list,OS_INT,OS_INT,src1,tmpreg2);
                  a_load_reg_reg(list,OS_INT,OS_INT,src2,tmpreg3);
                end;
              if size in [OS_S64,OS_64] then
                list.Concat(taicpu.op_reg_reg_reg(A_SUB_D,dst,src2,src1))
              else
                list.Concat(taicpu.op_reg_reg_reg(A_SUB_W,dst,src2,src1));
              case size of
                OS_S64,OS_S32:
                  begin
                    { if (src1<0)<>(dst<src2) overflow; }
                    tmpreg0:=getintregister(list,OS_INT);
                    tmpreg1:=getintregister(list,OS_INT);
                    list.Concat(taicpu.op_reg_reg_reg(A_SLT,tmpreg0,NR_R0,tmpreg2));
                    list.Concat(taicpu.op_reg_reg_reg(A_SLT,tmpreg1,dst,tmpreg3));
                    ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg0,tmpreg1,href);
                    ai.condition:=C_EQ;
                  end;
                OS_S16,OS_S8:
                  begin
                    tmpreg0:=getintregister(list,OS_INT);
                    a_load_reg_reg(list,OS_INT,size,dst,tmpreg0);
                    ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg0,dst,href);
                    ai.condition:=C_EQ;
                  end;
                OS_64,OS_32:
                  begin
                    ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg3,dst,href);
                    ai.condition:=C_GEU;
                  end;
                OS_16,OS_8:
                  begin
                    tmpreg0:=getintregister(list,OS_INT);
                    if size=OS_16 then
                      a_load_const_reg(list,OS_INT,1 shl 16,tmpreg0)
                    else
                      a_load_const_reg(list,OS_INT,1 shl 8,tmpreg0);
                    ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg0,dst,href);
                    ai.condition:=C_GTU;
                  end;
              else
                internalerror(2022082305);
              end;
              ai.is_jmp:=true;
              list.concat(ai);
              a_call_name(list,'FPC_OVERFLOW',false);
              a_label(list,l);
            end;
          OP_IMUL:
            begin
              { No overflow if upper result is same as sign of result }
              current_asmdata.getjumplabel(l);
              reference_reset_symbol(href,l,0,0,[]);
              href.refaddr:=addr_pcrel;
              tmpreg0:=getintregister(list,OS_INT);
              tmpreg1:=getintregister(list,OS_INT);
              if size in [OS_S64,OS_64] then
                begin
                  a_load_reg_reg(list,OS_INT,OS_INT,src1,tmpreg0);
                  a_load_reg_reg(list,OS_INT,OS_INT,src2,tmpreg1);
                  list.Concat(taicpu.op_reg_reg_reg(A_MUL_D,dst,src1,src2));
                  list.Concat(taicpu.op_reg_reg_reg(A_MULH_D,tmpreg0,tmpreg0,tmpreg1));
                  list.concat(taicpu.op_reg_reg_const(A_SRAI_D,tmpreg1,dst,63));
                  ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg0,tmpreg1,href);
                  ai.condition:=C_EQ;
                end
              else
                begin
                  list.Concat(taicpu.op_reg_reg_reg(A_MULW_D_W,dst,src1,src2));
                  case size of
                    OS_S32,OS_32:
                      list.concat(taicpu.op_reg_reg_const(A_SRAI_D,tmpreg0,dst,31));
                    OS_S16,OS_16:
                      list.concat(taicpu.op_reg_reg_const(A_SRAI_D,tmpreg0,dst,15));
                    OS_S8,OS_8:
                      list.concat(taicpu.op_reg_reg_const(A_SRAI_D,tmpreg0,dst,7));
                  else
                    internalerror(2022082306);
                  end;
                  list.concat(taicpu.op_reg_reg_const(A_ADDI_D,tmpreg0,tmpreg0,1));
                  list.Concat(taicpu.op_reg_reg_const(A_SLTI,tmpreg1,tmpreg0,2));
                  ai:=taicpu.op_reg_ref(A_BXX,tmpreg1,href);
                  ai.condition:=C_NEZ;
                end;
              ai.is_jmp:=true;
              list.concat(ai);
              a_call_name(list,'FPC_OVERFLOW',false);
              a_label(list,l);
            end;
          OP_MUL:
            begin
              { No overflow if upper result is 0 }
              current_asmdata.getjumplabel(l);
              reference_reset_symbol(href,l,0,0,[]);
              href.refaddr:=addr_pcrel;
              tmpreg0:=getintregister(list,OS_INT);
              if size in [OS_S64,OS_64] then
                begin
                  tmpreg1:=getintregister(list,OS_INT);
                  a_load_reg_reg(list,OS_INT,OS_INT,src1,tmpreg0);
                  a_load_reg_reg(list,OS_INT,OS_INT,src2,tmpreg1);
                  list.Concat(taicpu.op_reg_reg_reg(A_MUL_D,dst,src1,src2));
                  list.Concat(taicpu.op_reg_reg_reg(A_MULH_DU,tmpreg0,tmpreg0,tmpreg1));
                  ai:=taicpu.op_reg_ref(A_BXX,tmpreg0,href);
                  ai.condition:=C_EQZ;
                end
              else
                begin
                  list.Concat(taicpu.op_reg_reg_reg(A_MULW_D_WU,dst,src1,src2));
                  case size of
                    OS_S32,OS_32:
                      list.concat(taicpu.op_reg_reg_const(A_SRAI_D,tmpreg0,dst,31));
                    OS_S16,OS_16:
                      list.concat(taicpu.op_reg_reg_const(A_SRAI_D,tmpreg0,dst,15));
                    OS_S8,OS_8:
                      list.concat(taicpu.op_reg_reg_const(A_SRAI_D,tmpreg0,dst,7));
                  else
                    internalerror(2022082307);
                  end;
                  ai:=taicpu.op_reg_ref(A_BXX,tmpreg0,href);
                  ai.condition:=C_EQZ;
                end;
              ai.is_jmp:=true;
              list.concat(ai);
              a_call_name(list,'FPC_OVERFLOW',false);
              a_label(list,l);
            end;
          OP_IDIV:
            begin
              { Only overflow if -2^(N-1)/(-1)  }
              current_asmdata.getjumplabel(l);
              reference_reset_symbol(href,l,0,0,[]);
              href.refaddr:=addr_pcrel;
              tmpreg2:=getintregister(list,OS_INT);
              tmpreg3:=getintregister(list,OS_INT);
              a_load_reg_reg(list,OS_INT,OS_INT,src1,tmpreg2);
              a_load_reg_reg(list,OS_INT,OS_INT,src2,tmpreg3);
              if size in [OS_S64,OS_64] then
                list.Concat(taicpu.op_reg_reg_reg(A_DIV_D,dst,src1,src2))
              else
                list.Concat(taicpu.op_reg_reg_reg(A_DIV_W,dst,src1,src2));
              if size in [OS_S64,OS_64,OS_S32,OS_32] then
                begin
                  ai:=taicpu.op_reg_reg_ref(A_BXX,dst,tmpreg3,href);
                  ai.condition:=C_NE;
                  ai.is_jmp:=true;
                  list.concat(ai);
                  ai:=taicpu.op_reg_ref(A_BXX,tmpreg2,href);
                  ai.condition:=C_GEZ;
                  ai.is_jmp:=true;
                  list.concat(ai);
                end
              else
                begin
                  ai:=taicpu.op_reg_ref(A_BXX,tmpreg2,href);
                  ai.condition:=C_GEZ;
                  ai.is_jmp:=true;
                  list.concat(ai);
                  tmpreg0:=getintregister(list,OS_INT);
                  a_load_reg_reg(list,OS_INT,size,dst,tmpreg0);
                  ai:=taicpu.op_reg_reg_ref(A_BXX,tmpreg0,tmpreg3,href);
                  ai.condition:=C_NE;
                  ai.is_jmp:=true;
                  list.concat(ai);
                end;

              a_call_name(list,'FPC_OVERFLOW',false);
              a_label(list,l);
            end;
        else
          internalerror(2022082301);
        end
      else
        a_op_reg_reg_reg(list,op,size,src1,src2,dst);
    end;


    procedure tcgloongarch64.ungetregister(r : tregister;list : TAsmList);
      var
        supreg : tsuperregister;
        rt : tregistertype;
        live : tai;
      function is_in_list(t : tai;hlist : TAsmList) : boolean;
        var
          current : tai;
        begin
          result:=false;
          if not assigned(t) then
            exit;
          current:=tai(hlist.first);
          while assigned(current) do
            begin
              if (current=t) then
                begin
                  result:=true;
                  exit;
                end
              else
                current:=tai(current.next);
            end;
        end;

      begin
        if not assigned(list) then
          exit;
        supreg:=getsupreg(r);
        rt:=getregtype(r);
        if assigned(rg[rt]) then
          begin
            if is_in_list(rg[rt].live_start[supreg],list) then
              rg[rt].live_start[supreg]:=nil;
            if is_in_list(rg[rt].live_end[supreg],list) then
              rg[rt].live_end[supreg]:=nil;
          end;
      end;

    function tcgloongarch64.fixref(list: TAsmList; var ref: treference; mode : tfixref; out tmpreg : tregister): boolean;
      var
        href: treference;
      begin
        tmpreg:=NR_NO;
        if ref.refaddr=addr_reg_12i then
          begin
            result:=mode=fr_normal;
            exit;
          end
        else if ref.refaddr=addr_reg_reg then
          begin
            result:=mode=fr_reg;
            exit;
          end
        else if ref.refaddr=addr_reg_reg then
          begin
            result:=mode=fr_big;
            exit;
          end;

        { Reference with symbol, load symbol address first. }
        if assigned(ref.symbol) then
          begin
            tmpreg:=getintregister(list,OS_INT);
            if ((cs_create_pic in current_settings.moduleswitches) and
                (ref.symbol.bind in [AB_LOCAL,AB_TEMP])) or
               ((not(cs_create_pic in current_settings.moduleswitches)) and
                (ref.symbol.bind in [AB_LOCAL,AB_GLOBAL,AB_TEMP])) then
              begin
                { Load symbol address as local. }
                reference_reset_symbol(href,ref.symbol,ref.offset,ref.alignment,ref.volatility);
                ref.symbol:=nil;
                ref.offset:=0;
                href.refaddr:=addr_pc_hi20;
                list.concat(taicpu.op_reg_ref(A_PCALAU12I,tmpreg,href));
                href.refaddr:=addr_pc_lo12;
                list.concat(taicpu.op_reg_reg_ref(A_ADDI_D,tmpreg,tmpreg,href));
              end
            else
              begin
                { Load symbol address as global. }
                reference_reset_symbol(href,ref.symbol,0,0,[]);
                ref.symbol:=nil;
                href.refaddr:=addr_got_pc_hi20;
                list.concat(taicpu.op_reg_ref(A_PCALAU12I,tmpreg,href));
                href.refaddr:=addr_got_pc_lo12;
                list.concat(taicpu.op_reg_reg_ref(A_LD_D,tmpreg,tmpreg,href));
              end;
            { Make address format become basereg (+indexreg). }
            if (ref.index<>NR_NO) and (ref.base<>NR_NO) then
              begin
                a_op_reg_reg(list,OP_ADD,OS_INT,ref.base,tmpreg);
                ref.base:=tmpreg;
              end
            else if (ref.base=NR_NO) then
              ref.base:=tmpreg
            else { ref.index=NR_NO }
              ref.index:=tmpreg;
          end
        { Refernce only offset, make offset become a reg. }
        else if (ref.index=NR_NO) and (ref.base=NR_NO) then
          begin
            tmpreg:=getintregister(list,OS_INT);
            a_load_const_reg(list,OS_ADDR,ref.offset,tmpreg);
            reference_reset_base(ref,tmpreg,0,ctempposinvalid,ref.alignment,ref.volatility);
            ref.index:=NR_R0;
          end;

        if (ref.index<>NR_NO) and (ref.base=NR_NO) then
          begin
            ref.base:=ref.index;
            ref.index:=NR_R0;
          end;
        if (ref.index=NR_NO) then
          ref.index:=NR_R0;

        { The normal type is widely applicable. When we find it is better to
          use the normal type, we prevent other types. Or add strong types and
          adjust it in the future. }
        if is_simm12(ref.offset) then
          begin
            if ref.index<>NR_R0 then
              begin
                tmpreg:=getintregister(list,OS_INT);
                a_op_reg_reg_reg(list,OP_ADD,OS_INT,ref.base,ref.index,tmpreg);
                ref.base:=tmpreg;
              end;
            ref.index:=NR_NO;
            ref.refaddr:=addr_reg_12i;
            result:=mode=fr_normal;
            exit;
          end;

        if (mode=fr_big) and (is_simm16_and_quadruple(ref.offset)) then
          begin
            if ref.index<>NR_NO then
              begin
                tmpreg:=getintregister(list,OS_INT);
                a_op_reg_reg_reg(list,OP_ADD,OS_INT,ref.base,ref.index,tmpreg);
                ref.base:=tmpreg;
              end;
            ref.index:=NR_NO;
            ref.refaddr:=addr_reg_14i;
            result:=true;
            exit;
          end;

        if mode=fr_reg then
          begin
            if ref.offset<>0 then
              begin
                tmpreg:=getintregister(list,OS_INT);
                a_load_const_reg(list,OS_INT,ref.offset,tmpreg);
                if ref.index<>NR_R0 then
                  begin
                    a_op_reg_reg(list,OP_ADD,OS_INT,ref.index,tmpreg);
                    ref.index:=tmpreg;
                  end
                else
                  ref.index:=tmpreg;
              end;
            ref.refaddr:=addr_reg_reg;
            ref.offset:=0;
            result:=true;
            exit;
          end;

        tmpreg:=getintregister(list,OS_INT);
        a_load_const_reg(list,OS_INT,ref.offset,tmpreg);
        if ref.index<>NR_R0 then
          a_op_reg_reg(list,OP_ADD,OS_INT,ref.index,tmpreg);
        a_op_reg_reg(list,OP_ADD,OS_INT,ref.base,tmpreg);
        ref.base:=tmpreg;
        ref.index:=NR_NO;
        ref.offset:=0;
        result:=mode=fr_normal;
      end;


    procedure tcgloongarch64.maybeadjustresult(list: TAsmList; op: topcg; size: tcgsize; dst: tregister);
      const
        overflowops = [OP_MUL,OP_IMUL,OP_SHL,OP_ADD,OP_SUB,OP_NOT,OP_NEG];
      begin
        if (op in overflowops) and
           (size in [OS_8,OS_S8,OS_16,OS_S16,OS_32,OS_S32]) then
          a_load_reg_reg(list,OS_INT,size,dst,dst)
        else if (op in [OP_ROL,OP_ROR]) and (size in [OS_8,OS_S8,OS_16,OS_S16]) then
          a_load_reg_reg(list,OS_INT,size,dst,dst);
      end;


    procedure create_codegen;
      begin
        cg := tcgloongarch64.create;
        cg128:=tcg128.create;
      end;

end.
