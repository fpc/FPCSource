{
    $Id$

    Copyright (c) 2003 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generator for the ARM

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
       symtype,
       cgbase,cgobj,
       aasmbase,aasmcpu,aasmtai,
       cpubase,cpuinfo,node,cg64f32,cginfo;


    type
      tcgarm = class(tcg)
        procedure a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);override;
        procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);override;
        procedure a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);override;

        procedure a_call_name(list : taasmoutput;const s : string);override;
        procedure a_call_reg(list : taasmoutput;reg: tregister); override;
        procedure a_call_ref(list : taasmoutput;const ref : treference);override;

        procedure a_op_const_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; reg: TRegister); override;
        procedure a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;

        procedure a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
          size: tcgsize; a: aword; src, dst: tregister); override;
        procedure a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
          size: tcgsize; src1, src2, dst: tregister); override;

        { move instructions }
        procedure a_load_const_reg(list : taasmoutput; size: tcgsize; a : aword;reg : tregister);override;
        procedure a_load_reg_ref(list : taasmoutput; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);override;
        procedure a_load_ref_reg(list : taasmoutput; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister);override;
        procedure a_load_reg_reg(list : taasmoutput; fromsize, tosize : tcgsize;reg1,reg2 : tregister);override;

        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list: taasmoutput; size: tcgsize; reg1, reg2: tregister); override;
        procedure a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister); override;
        procedure a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference); override;

        {  comparison operations }
        procedure a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
          l : tasmlabel);override;
        procedure a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;

        procedure a_jmp_always(list : taasmoutput;l: tasmlabel); override;
        procedure a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel); override;

        procedure g_flags2reg(list: taasmoutput; size: TCgSize; const f: TResFlags; reg: TRegister); override;

        procedure g_copyvaluepara_openarray(list : taasmoutput;const ref, lenref:treference;elesize:integer);override;
        procedure g_stackframe_entry(list : taasmoutput;localsize : longint);override;
        procedure g_return_from_proc(list : taasmoutput;parasize : aword); override;
        procedure g_restore_frame_pointer(list : taasmoutput);override;

        procedure a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);override;

        procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);override;

        procedure g_overflowcheck(list: taasmoutput; const l: tlocation; def: tdef); override;

        procedure g_save_standard_registers(list : taasmoutput; usedinproc : tsuperregisterset);override;
        procedure g_restore_standard_registers(list : taasmoutput; usedinproc : tsuperregisterset);override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_restore_all_registers(list : taasmoutput;accused,acchiused:boolean);override;

        procedure a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);
        procedure fixref(list : taasmoutput;var ref : treference);
        procedure handle_load_store(list:taasmoutput;op: tasmop;oppostfix : toppostfix;reg:tregister;ref: treference);
      end;

      tcg64farm = class(tcg64f32)
        procedure a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);override;
        procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;reg : tregister64);override;
        procedure a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;value : qword;regsrc,regdst : tregister64);override;
        procedure a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;regsrc1,regsrc2,regdst : tregister64);override;
      end;

    const
      OpCmp2AsmCond : Array[topcmp] of TAsmCond = (C_NONE,C_EQ,C_GT,
                           C_LT,C_GE,C_LE,C_NE,C_LE,C_LT,C_GE,C_GT);

    function is_shifter_const(d : dword;var imm_shift : byte) : boolean;

  implementation


    uses
       globtype,globals,verbose,systems,cutils,symconst,symdef,symsym,rgobj,tgobj,cpupi;


    procedure tcgarm.a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);
      var
        ref: treference;
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
            internalerror(2002081101);
        end;
        if locpara.sp_fixup<>0 then
          internalerror(2002081102);
      end;


    procedure tcgarm.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);
      var
        ref: treference;
        tmpreg: tregister;
      begin
        case locpara.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_load_ref_reg(list,size,size,r,locpara.register);
          LOC_REFERENCE:
            begin
               reference_reset(ref);
               ref.base:=locpara.reference.index;
               ref.offset:=locpara.reference.offset;
               tmpreg := rg.getregisterint(list,size);
               a_load_ref_reg(list,size,size,r,tmpreg);
               a_load_reg_ref(list,size,size,tmpreg,ref);
               rg.ungetregisterint(list,tmpreg);
            end;
          LOC_FPUREGISTER,LOC_CFPUREGISTER:
            case size of
               OS_F32, OS_F64:
                 a_loadfpu_ref_reg(list,size,r,locpara.register);
               else
                 internalerror(2002072801);
            end;
          else
            internalerror(2002081103);
        end;
        if locpara.sp_fixup<>0 then
          internalerror(2002081104);
      end;


    procedure tcgarm.a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);
      var
        ref: treference;
        tmpreg: tregister;
      begin
         case locpara.loc of
            LOC_REGISTER,LOC_CREGISTER:
              a_loadaddr_ref_reg(list,r,locpara.register);
            LOC_REFERENCE:
              begin
                reference_reset(ref);
                ref.base := locpara.reference.index;
                ref.offset := locpara.reference.offset;
                tmpreg := rg.getregisterint(list,OS_ADDR);
                a_loadaddr_ref_reg(list,r,tmpreg);
                a_load_reg_ref(list,OS_ADDR,OS_ADDR,tmpreg,ref);
                rg.ungetregisterint(list,tmpreg);
              end;
            else
              internalerror(2002080701);
         end;
      end;


    procedure tcgarm.a_call_name(list : taasmoutput;const s : string);
      begin
         list.concat(taicpu.op_sym(A_BL,objectlibrary.newasmsymbol(s)));
         if not(pi_do_call in current_procinfo.flags) then
           internalerror(2003060703);
      end;


    procedure tcgarm.a_call_reg(list : taasmoutput;reg: tregister);
      var
         r : tregister;
      begin
        list.concat(taicpu.op_reg_reg(A_MOV,NR_PC,reg));
        if not(pi_do_call in current_procinfo.flags) then
          internalerror(2003060704);
      end;


    procedure tcgarm.a_call_ref(list : taasmoutput;const ref : treference);
      var
         r : tregister;
      begin
        a_load_ref_reg(list,OS_ADDR,OS_ADDR,ref,NR_PC);
        if not(pi_do_call in current_procinfo.flags) then
          internalerror(2003060705);
      end;


     procedure tcgarm.a_op_const_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; reg: TRegister);
       begin
          a_op_const_reg_reg(list,op,size,a,reg,reg);
       end;


     procedure tcgarm.a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister);
       begin
         case op of
           OP_NEG:
             list.concat(taicpu.op_reg_reg_const(A_RSB,dst,src,0));
           OP_NOT:
             list.concat(taicpu.op_reg_reg(A_MVN,dst,src));
           else
             a_op_reg_reg_reg(list,op,OS_32,src,dst,dst);
         end;
       end;


     const
       op_reg_reg_opcg2asmop: array[TOpCG] of tasmop =
         (A_NONE,A_ADD,A_AND,A_NONE,A_NONE,A_MUL,A_MUL,A_NONE,A_NONE,A_ORR,
          A_NONE,A_NONE,A_NONE,A_SUB,A_EOR);


     procedure tcgarm.a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
       size: tcgsize; a: aword; src, dst: tregister);
       var
         shift : byte;
         tmpreg : tregister;
         so : tshifterop;
       begin
          if is_shifter_const(a,shift) and (not(op in [OP_IMUL,OP_MUL])) then
            case op of
              OP_NEG,OP_NOT,
              OP_DIV,OP_IDIV:
                internalerror(200308281);
              OP_SHL:
                begin
                  if a>32 then
                    internalerror(200308291);
                  shifterop_reset(so);
                  so.shiftmode:=SM_LSL;
                  so.shiftimm:=a;
                  list.concat(taicpu.op_reg_reg_shifterop(A_MOV,dst,src,so));
                end;
              OP_SHR:
                begin
                  if a>32 then
                    internalerror(200308292);
                  shifterop_reset(so);
                  so.shiftmode:=SM_LSR;
                  so.shiftimm:=a;
                  list.concat(taicpu.op_reg_reg_shifterop(A_MOV,dst,src,so));
                end;
              OP_SAR:
                begin
                  if a>32 then
                    internalerror(200308291);
                  shifterop_reset(so);
                  so.shiftmode:=SM_LSL;
                  so.shiftimm:=a;
                  list.concat(taicpu.op_reg_reg_shifterop(A_MOV,dst,src,so));
                end;
              else
                list.concat(taicpu.op_reg_reg_const(op_reg_reg_opcg2asmop[op],dst,src,a));
            end
          else
            begin
              { there could be added some more sophisticated optimizations }
              if (op in [OP_MUL,OP_IMUL]) and (a=1) then
                a_load_reg_reg(list,size,size,src,dst)
              else if (op in [OP_MUL,OP_IMUL]) and (a=0) then
                a_load_const_reg(list,size,0,dst)
              else if (op in [OP_IMUL]) and (a=-1) then
                a_op_reg_reg(list,OP_NEG,size,src,dst)
              else
                begin
                  tmpreg := rg.getregisterint(list,size);
                  a_load_const_reg(list,size,a,tmpreg);
                  a_op_reg_reg_reg(list,op,size,tmpreg,src,dst);
                  rg.ungetregisterint(list,tmpreg);
                end;
            end;
       end;


     procedure tcgarm.a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
       size: tcgsize; src1, src2, dst: tregister);
       var
         so : tshifterop;
         tmpreg : tregister;
       begin
         case op of
           OP_NEG,OP_NOT,
           OP_DIV,OP_IDIV:
             internalerror(200308281);
           OP_SHL:
             begin
               shifterop_reset(so);
               so.rs:=src1;
               so.shiftmode:=SM_LSL;
               list.concat(taicpu.op_reg_reg_shifterop(A_MOV,dst,src2,so));
             end;
           OP_SHR:
             begin
               shifterop_reset(so);
               so.rs:=src1;
               so.shiftmode:=SM_LSR;
               list.concat(taicpu.op_reg_reg_shifterop(A_MOV,dst,src2,so));
             end;
           OP_SAR:
             begin
               shifterop_reset(so);
               so.rs:=src1;
               so.shiftmode:=SM_ASR;
               list.concat(taicpu.op_reg_reg_shifterop(A_MOV,dst,src2,so));
             end;
           OP_IMUL,
           OP_MUL:
             begin
               { the arm doesn't allow that rd and rm are the same }
               if dst=src2 then
                 begin
                   if src1<>src2 then
                     list.concat(taicpu.op_reg_reg_reg(A_MUL,dst,src1,src2))
                   else
                     begin
                       writeln('Warning: Fix MUL');
                       list.concat(taicpu.op_reg_reg_reg(A_MUL,dst,src2,src1));
                     end;
                 end
               else
                 list.concat(taicpu.op_reg_reg_reg(A_MUL,dst,src2,src1));
             end;
           else
             list.concat(taicpu.op_reg_reg_reg(op_reg_reg_opcg2asmop[op],dst,src2,src1));
         end;
       end;


     function rotl(d : dword;b : byte) : dword;
       begin
          result:=(d shr (32-b)) or (d shl b);
       end;


     function is_shifter_const(d : dword;var imm_shift : byte) : boolean;
       var
          i : longint;
       begin
          for i:=0 to 15 do
            begin
               if (d and not(rotl($ff,i*2)))=0 then
                 begin
                    imm_shift:=i*2;
                    result:=true;
                    exit;
                 end;
            end;
          result:=false;
       end;


     procedure tcgarm.a_load_const_reg(list : taasmoutput; size: tcgsize; a : aword;reg : tregister);
       var
          imm_shift : byte;
          l : tasmlabel;
          hr : treference;
       begin
          if not(size in [OS_8,OS_S8,OS_16,OS_S16,OS_32,OS_S32]) then
            internalerror(2002090902);
          if is_shifter_const(a,imm_shift) then
            list.concat(taicpu.op_reg_const(A_MOV,reg,a))
          else if is_shifter_const(not(a),imm_shift) then
            list.concat(taicpu.op_reg_const(A_MVN,reg,not(a)))
          else
            begin
               objectlibrary.getdatalabel(l);
               current_procinfo.aktlocaldata.concat(tai_symbol.Create(l,0));
               current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(a));
               reference_reset(hr);
               hr.symbol:=l;
               list.concat(taicpu.op_reg_ref(A_LDR,reg,hr));
            end;
       end;


    procedure tcgarm.handle_load_store(list:taasmoutput;op: tasmop;oppostfix : toppostfix;reg:tregister;ref: treference);
      var
        tmpreg : tregister;
        tmpref : treference;
        l : tasmlabel;
      begin
        tmpreg:=NR_NO;

        { Be sure to have a base register }
        if (ref.base=NR_NO) then
          begin
            if ref.shiftmode<>SM_None then
              internalerror(200308294);
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
           (ref.offset<-4095) or
           (ref.offset>4095) then
          begin
            { check consts distance }

            { create consts entry }
            objectlibrary.getdatalabel(l);
            current_procinfo.aktlocaldata.concat(Tai_symbol.Create(l,0));
            if assigned(ref.symbol) then
              current_procinfo.aktlocaldata.concat(tai_const_symbol.Create_offset(ref.symbol,ref.offset))
            else
              current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(ref.offset));

            { load consts entry }
            tmpreg:=rg.getregisterint(list,OS_INT);
            reference_reset(tmpref);
            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            list.concat(taicpu.op_reg_ref(A_LDR,tmpreg,tmpref));

            if (ref.base<>NR_NO) then
              begin
                if ref.index<>NR_NO then
                  begin
                    list.concat(taicpu.op_reg_reg_reg(A_ADD,tmpreg,ref.base,tmpreg));
                    rg.ungetregister(list,ref.base);
                    ref.base:=tmpreg;
                  end
                else
                  begin
                    ref.index:=tmpreg;
                    ref.shiftimm:=0;
                    ref.signindex:=1;
                    ref.shiftmode:=SM_None;
                  end;
              end
            else
              ref.base:=tmpreg;
            ref.offset:=0;
            ref.symbol:=nil;
          end;
        list.concat(setoppostfix(taicpu.op_reg_ref(op,reg,ref),oppostfix));
        if (tmpreg<>NR_NO) then
          rg.ungetregisterint(list,tmpreg);
      end;


     procedure tcgarm.a_load_reg_ref(list : taasmoutput; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);
       var
         oppostfix:toppostfix;
       begin
         case ToSize of
           { signed integer registers }
           OS_8,
           OS_S8:
             oppostfix:=PF_B;
           OS_16,
           OS_S16:
             oppostfix:=PF_H;
           OS_32,
           OS_S32:
             oppostfix:=PF_None;
           else
             InternalError(200308295);
         end;
         handle_load_store(list,A_STR,oppostfix,reg,ref);
       end;


     procedure tcgarm.a_load_ref_reg(list : taasmoutput; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister);
       var
         oppostfix:toppostfix;
       begin
         case FromSize of
           { signed integer registers }
           OS_8:
             oppostfix:=PF_B;
           OS_S8:
             oppostfix:=PF_SB;
           OS_16:
             oppostfix:=PF_H;
           OS_S16:
             oppostfix:=PF_SH;
           OS_32,
           OS_S32:
             oppostfix:=PF_None;
           else
             InternalError(200308291);
         end;
         handle_load_store(list,A_LDR,oppostfix,reg,ref);
       end;


     procedure tcgarm.a_load_reg_reg(list : taasmoutput; fromsize, tosize : tcgsize;reg1,reg2 : tregister);
       var
         instr: taicpu;
         so : tshifterop;
       begin
         shifterop_reset(so);
         if (reg1<>reg2) or
            (tcgsize2size[tosize] < tcgsize2size[fromsize]) or
            ((tcgsize2size[tosize] = tcgsize2size[fromsize]) and
             (tosize <> fromsize) and
             not(fromsize in [OS_32,OS_S32])) then
           begin
             case tosize of
               OS_8:
                 instr := taicpu.op_reg_reg_const(A_AND,
                   reg2,reg1,$ff);
               OS_S8:
                 begin
                   so.shiftmode:=SM_LSL;
                   so.shiftimm:=24;
                   list.concat(taicpu.op_reg_reg_shifterop(A_MOV,reg2,reg1,so));
                   so.shiftmode:=SM_ASR;
                   so.shiftimm:=24;
                   list.concat(taicpu.op_reg_reg_shifterop(A_MOV,reg2,reg2,so));
                 end;
               OS_16:
                 begin
                   so.shiftmode:=SM_LSL;
                   so.shiftimm:=16;
                   list.concat(taicpu.op_reg_reg_shifterop(A_MOV,reg2,reg1,so));
                   so.shiftmode:=SM_LSR;
                   so.shiftimm:=16;
                   list.concat(taicpu.op_reg_reg_shifterop(A_MOV,reg2,reg2,so));
                 end;
               OS_S16:
                 begin
                   so.shiftmode:=SM_LSL;
                   so.shiftimm:=16;
                   list.concat(taicpu.op_reg_reg_shifterop(A_MOV,reg2,reg1,so));
                   so.shiftmode:=SM_ASR;
                   so.shiftimm:=16;
                   list.concat(taicpu.op_reg_reg_shifterop(A_MOV,reg2,reg2,so));
                 end;
               OS_32,OS_S32:
                 begin
                   instr:=taicpu.op_reg_reg(A_MOV,reg2,reg1);
                   rg.add_move_instruction(instr);
                   list.concat(instr);
                 end;
               else internalerror(2002090901);
             end;
           end;
       end;


     procedure tcgarm.a_loadfpu_reg_reg(list: taasmoutput; size: tcgsize; reg1, reg2: tregister);
       begin
         list.concat(setoppostfix(taicpu.op_reg_reg(A_MVF,reg2,reg1),cgsize2fpuoppostfix[size]));
       end;


     procedure tcgarm.a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister);
       var
         oppostfix:toppostfix;
       begin
         case size of
           OS_F32:
             oppostfix:=PF_S;
           OS_F64:
             oppostfix:=PF_D;
           OS_F80:
             oppostfix:=PF_E;
           else
             InternalError(200309021);
         end;
         handle_load_store(list,A_LDF,oppostfix,reg,ref);
       end;


     procedure tcgarm.a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference);
       var
         oppostfix:toppostfix;
       begin
         case size of
           OS_F32:
             oppostfix:=PF_S;
           OS_F64:
             oppostfix:=PF_D;
           OS_F80:
             oppostfix:=PF_E;
           else
             InternalError(200309021);
         end;
         handle_load_store(list,A_STF,oppostfix,reg,ref);
       end;


     {  comparison operations }
    procedure tcgarm.a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
      l : tasmlabel);
      var
        tmpreg : tregister;
        b : byte;
      begin
        if is_shifter_const(a,b) then
          list.concat(taicpu.op_reg_const(A_CMN,reg,a))
        { CMN reg,0 and CMN reg,$80000000 are different from CMP reg,$ffffffff
          and CMP reg,$7fffffff regarding the flags according to the ARM manual }
        else if is_shifter_const(not(a),b) and (a<>$7fffffff) and (a<>$ffffffff) then
          list.concat(taicpu.op_reg_const(A_CMN,reg,not(a)))
        else
          begin
            tmpreg:=rg.getregisterint(list,size);
            a_load_const_reg(list,size,a,tmpreg);
            list.concat(taicpu.op_reg_reg(A_CMP,reg,tmpreg));
            rg.ungetregisterint(list,tmpreg);
          end;
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgarm.a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel);
      begin
        list.concat(taicpu.op_reg_reg(A_CMP,reg2,reg1));
        a_jmp_cond(list,cmp_op,l);
      end;


     procedure tcgarm.a_jmp_always(list : taasmoutput;l: tasmlabel);
       begin
         list.concat(taicpu.op_sym(A_B,objectlibrary.newasmsymbol(l.name)));
       end;


     procedure tcgarm.a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel);
       var
         ai : taicpu;
       begin
         ai:=setcondition(taicpu.op_sym(A_B,l),flags_to_cond(f));
         ai.is_jmp:=true;
         list.concat(ai);
       end;


    procedure tcgarm.g_flags2reg(list: taasmoutput; size: TCgSize; const f: TResFlags; reg: TRegister);
      var
        ai : taicpu;
      begin
        list.concat(setcondition(taicpu.op_reg_const(A_MOV,reg,1),flags_to_cond(f)));
        list.concat(setcondition(taicpu.op_reg_const(A_MOV,reg,0),inverse_cond[flags_to_cond(f)]));
      end;


    procedure tcgarm.g_copyvaluepara_openarray(list : taasmoutput;const ref, lenref:treference;elesize:integer);
      begin
      end;


    procedure tcgarm.g_stackframe_entry(list : taasmoutput;localsize : longint);
      var
        rip,rsp,rfp : tregister;
      begin
        LocalSize:=align(LocalSize,4);

        a_reg_alloc(list,NR_STACK_POINTER_REG);
        a_reg_alloc(list,NR_FRAME_POINTER_REG);
        a_reg_alloc(list,NR_R12);

        list.concat(taicpu.op_reg_reg(A_MOV,NR_R12,NR_STACK_POINTER_REG));
        { restore int registers and return }
        list.concat(setoppostfix(taicpu.op_reg_regset(A_STM,rsp,rg.used_in_proc_int-[RS_R0..RS_R3]+[RS_R11,RS_R12,RS_R15]),PF_FD));

        list.concat(taicpu.op_reg_reg_const(A_SUB,NR_FRAME_POINTER_REG,NR_R12,4));
        a_reg_alloc(list,NR_R12);

        { allocate necessary stack size }
        list.concat(taicpu.op_reg_reg_const(A_SUB,NR_STACK_POINTER_REG,NR_STACK_POINTER_REG,LocalSize));
      end;


    procedure tcgarm.g_return_from_proc(list : taasmoutput;parasize : aword);
      begin
        if (current_procinfo.framepointer=NR_STACK_POINTER_REG) then
          list.concat(taicpu.op_reg_reg(A_MOV,NR_R15,NR_R14))
        else
          { restore int registers and return }
          list.concat(setoppostfix(taicpu.op_reg_regset(A_LDM,NR_R11,rg.used_in_proc_int-[RS_R0..RS_R3]+[RS_R11,RS_R13,RS_R15]),PF_EA));
      end;


    procedure tcgarm.g_restore_frame_pointer(list : taasmoutput);
      begin
         { the frame pointer on the ARM is restored while the ret is executed }
      end;


    procedure tcgarm.a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);
      var
        b : byte;
        tmpref : treference;
      begin
        tmpref:=ref;
        { Be sure to have a base register }
        if (tmpref.base=NR_NO) then
          begin
            if tmpref.shiftmode<>SM_None then
              internalerror(200308294);
            tmpref.base:=tmpref.index;
            tmpref.index:=NR_NO;
          end;

        if assigned(tmpref.symbol) or
           not(is_shifter_const(tmpref.offset,b)) or
           ((tmpref.base<>NR_NO) and (tmpref.index<>NR_NO)) then
          fixref(list,tmpref);

        if ref.index<>NR_NO then
          begin
          end
{        else
          list.concat(taicpu.op_reg_reg(A_MOV,r,));
          ref.signindex<0 then }
      end;


    procedure tcgarm.fixref(list : taasmoutput;var ref : treference);
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

        { check consts distance }
        {!!!!!}

        { create consts entry }
        objectlibrary.getdatalabel(l);
        current_procinfo.aktlocaldata.concat(Tai_symbol.Create(l,0));
        if assigned(ref.symbol) then
          current_procinfo.aktlocaldata.concat(tai_const_symbol.Create_offset(ref.symbol,ref.offset))
        else
          current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(ref.offset));

        { load consts entry }
        tmpreg:=rg.getregisterint(list,OS_INT);
        reference_reset(tmpref);
        tmpref.symbol:=l;
        tmpref.base:=NR_PC;
        list.concat(taicpu.op_reg_ref(A_LDR,tmpreg,tmpref));

        if (ref.base<>NR_NO) then
          begin
            if ref.index<>NR_NO then
              begin
                list.concat(taicpu.op_reg_reg_reg(A_ADD,tmpreg,ref.base,tmpreg));
                rg.ungetregister(list,ref.base);
                ref.base:=tmpreg;
              end
            else
              begin
                ref.index:=tmpreg;
                ref.shiftimm:=0;
                ref.signindex:=1;
                ref.shiftmode:=SM_None;
              end;
          end
        else
          ref.base:=tmpreg;
        ref.offset:=0;
        ref.symbol:=nil;
      end;



    procedure tcgarm.g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);
      var
        srcref,dstref:treference;
        srcreg,destreg,countreg,r:tregister;
        helpsize:aword;
        copysize:byte;
        cgsize:Tcgsize;

      procedure genloop(count : aword;size : byte);
        const
          size2opsize : array[1..4] of tcgsize = (OS_8,OS_16,OS_NO,OS_32);
        var
          l : tasmlabel;
        begin
          objectlibrary.getdatalabel(l);
          a_load_const_reg(list,OS_INT,count,countreg);
          list.concat(Tai_symbol.Create(l,0));
          srcref.addressmode:=AM_POSTINDEXED;
          dstref.addressmode:=AM_POSTINDEXED;
          srcref.offset:=size;
          dstref.offset:=size;
          r:=rg.getregisterint(list,size2opsize[size]);
          a_load_ref_reg(list,size2opsize[size],size2opsize[size],srcref,r);
          a_load_reg_ref(list,size2opsize[size],size2opsize[size],r,dstref);
          rg.ungetregisterint(list,r);
          list.concat(setoppostfix(taicpu.op_reg_reg_const(A_SUB,countreg,countreg,1),PF_S));
          list.concat(setcondition(taicpu.op_sym(A_B,l),C_NE));
        end;

      begin
        helpsize:=12;
        dstref:=dest;
        srcref:=source;
        if cs_littlesize in aktglobalswitches then
          helpsize:=8;
        if not loadref and (len<=helpsize) then
          begin
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
                r:=rg.getregisterint(list,cgsize);
                a_load_ref_reg(list,cgsize,cgsize,srcref,r);
                if (len=0) and delsource then
                  reference_release(list,source);
                a_load_reg_ref(list,cgsize,cgsize,r,dstref);
                inc(srcref.offset,copysize);
                inc(dstref.offset,copysize);
                rg.ungetregisterint(list,r);
              end;
          end
        else
          begin
            destreg:=rg.getregisterint(list,OS_ADDR);
            a_loadaddr_ref_reg(list,dest,destreg);
            srcreg:=rg.getregisterint(list,OS_ADDR);
            if loadref then
              a_load_ref_reg(list,OS_ADDR,OS_ADDR,source,srcreg)
            else
              begin
                a_loadaddr_ref_reg(list,source,srcreg);
                if delsource then
                  begin
                    srcref:=source;
                    reference_release(list,srcref);
                  end;
              end;

            countreg:=rg.getregisterint(list,OS_32);

//            if cs_littlesize in aktglobalswitches  then
              genloop(len,1);
{
            else
              begin
                helpsize:=len shr 2;
                len:=len and 3;
                if helpsize>1 then
                  begin
                    a_load_const_reg(list,OS_INT,helpsize,countreg);
                    list.concat(Taicpu.op_none(A_REP,S_NO));
                  end;
                if helpsize>0 then
                  list.concat(Taicpu.op_none(A_MOVSD,S_NO));
                if len>1 then
                  begin
                    dec(len,2);
                    list.concat(Taicpu.op_none(A_MOVSW,S_NO));
                  end;
                if len=1 then
                  list.concat(Taicpu.op_none(A_MOVSB,S_NO));
                end;
}
            rg.ungetregisterint(list,countreg);
            rg.ungetregisterint(list,srcreg);
            rg.ungetregisterint(list,destreg);
          end;
        if delsource then
          tg.ungetiftemp(list,source);
      end;


    procedure tcgarm.g_overflowcheck(list: taasmoutput; const l: tlocation; def: tdef);
      begin
      end;


    procedure tcgarm.g_save_standard_registers(list : taasmoutput; usedinproc : tsuperregisterset);
      begin
        { we support only ARM standard calling conventions so this procedure has no use on the ARM }
      end;


    procedure tcgarm.g_restore_standard_registers(list : taasmoutput; usedinproc : tsuperregisterset);
      begin
        { we support only ARM standard calling conventions so this procedure has no use on the ARM }
      end;


    procedure tcgarm.g_save_all_registers(list : taasmoutput);
      begin
        { we support only ARM standard calling conventions so this procedure has no use on the ARM }
      end;


    procedure tcgarm.g_restore_all_registers(list : taasmoutput;accused,acchiused:boolean);
      begin
        { we support only ARM standard calling conventions so this procedure has no use on the ARM }
      end;


    procedure tcgarm.a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);
      var
        ai : taicpu;
      begin
        ai:=Taicpu.Op_sym(A_B,l);
        ai.SetCondition(OpCmp2AsmCond[cond]);
        ai.is_jmp:=true;
        list.concat(ai);
      end;


    procedure tcg64farm.a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);
      var
        tmpreg : tregister;
      begin
        case op of
          OP_NEG:
            begin
              list.concat(setoppostfix(taicpu.op_reg_reg_const(A_RSB,regdst.reglo,regsrc.reglo,0),PF_S));
              list.concat(taicpu.op_reg_reg_const(A_RSC,regdst.reghi,regsrc.reghi,0));
            end;
          else
            a_op64_reg_reg_reg(list,op,regsrc,regdst,regdst);
        end;
      end;


    procedure tcg64farm.a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;reg : tregister64);
      begin
        a_op64_const_reg_reg(list,op,value,reg,reg);
      end;


    procedure tcg64farm.a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;value : qword;regsrc,regdst : tregister64);
      var
        tmpreg : tregister;
        b : byte;
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_const_reg_reg(list,op,OS_32,lo(value),regsrc.reglo,regdst.reglo);
              cg.a_op_const_reg_reg(list,op,OS_32,hi(value),regsrc.reghi,regdst.reghi);
            end;
          OP_ADD:
            begin
              if is_shifter_const(lo(value),b) then
                list.concat(setoppostfix(taicpu.op_reg_reg_const(A_ADD,regdst.reglo,regsrc.reglo,lo(value)),PF_S))
              else
                begin
                  tmpreg:=rg.getregisterint(list,OS_32);
                  cg.a_load_const_reg(list,OS_32,lo(value),tmpreg);
                  list.concat(setoppostfix(taicpu.op_reg_reg_reg(A_ADD,regdst.reglo,regsrc.reglo,tmpreg),PF_S));
                  rg.ungetregisterint(list,tmpreg);
                end;

              if is_shifter_const(hi(value),b) then
                list.concat(taicpu.op_reg_reg_const(A_ADC,regdst.reghi,regsrc.reghi,hi(value)))
              else
                begin
                  tmpreg:=rg.getregisterint(list,OS_32);
                  cg.a_load_const_reg(list,OS_32,hi(value),tmpreg);
                  list.concat(taicpu.op_reg_reg_reg(A_ADC,regdst.reghi,regsrc.reghi,tmpreg));
                  rg.ungetregisterint(list,tmpreg);
                end;
            end;
          OP_SUB:
            begin
              if is_shifter_const(lo(value),b) then
                list.concat(setoppostfix(taicpu.op_reg_reg_const(A_SUB,regdst.reglo,regsrc.reglo,lo(value)),PF_S))
              else
                begin
                  tmpreg:=rg.getregisterint(list,OS_32);
                  cg.a_load_const_reg(list,OS_32,lo(value),tmpreg);
                  list.concat(setoppostfix(taicpu.op_reg_reg_reg(A_SUB,regdst.reglo,regsrc.reglo,tmpreg),PF_S));
                  rg.ungetregisterint(list,tmpreg);
                end;

              if is_shifter_const(hi(value),b) then
                list.concat(taicpu.op_reg_reg_const(A_SBC,regdst.reghi,regsrc.reghi,hi(value)))
              else
                begin
                  tmpreg:=rg.getregisterint(list,OS_32);
                  cg.a_load_const_reg(list,OS_32,hi(value),tmpreg);
                  list.concat(taicpu.op_reg_reg_reg(A_SBC,regdst.reghi,regsrc.reghi,tmpreg));
                  rg.ungetregisterint(list,tmpreg);
                end;
            end;
          else
            internalerror(2003083101);
        end;
      end;


    procedure tcg64farm.a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;regsrc1,regsrc2,regdst : tregister64);
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_reg_reg_reg(list,op,OS_32,regsrc1.reglo,regsrc2.reglo,regdst.reglo);
              cg.a_op_reg_reg_reg(list,op,OS_32,regsrc1.reghi,regsrc2.reghi,regdst.reghi);
            end;
          OP_ADD:
            begin
              list.concat(setoppostfix(taicpu.op_reg_reg_reg(A_ADD,regdst.reglo,regsrc1.reglo,regsrc2.reglo),PF_S));
              list.concat(taicpu.op_reg_reg_reg(A_ADC,regdst.reghi,regsrc1.reghi,regsrc2.reghi));
            end;
          OP_SUB:
            begin
              list.concat(setoppostfix(taicpu.op_reg_reg_reg(A_SUB,regdst.reglo,regsrc2.reglo,regsrc1.reglo),PF_S));
              list.concat(taicpu.op_reg_reg_reg(A_SBC,regdst.reghi,regsrc2.reghi,regsrc1.reghi));
            end;
          else
            internalerror(2003083101);
        end;
      end;


begin
  cg:=tcgarm.create;
  cg64:=tcg64farm.create;
end.
{
  $Log$
  Revision 1.14  2003-09-04 21:07:03  florian
    * ARM compiler compiles again

  Revision 1.13  2003/09/04 00:15:29  florian
    * first bunch of adaptions of arm compiler for new register type

  Revision 1.12  2003/09/03 19:10:30  florian
    * initial revision of new register naming

  Revision 1.11  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.10  2003/09/01 15:11:16  florian
    * fixed reference handling
    * fixed operand postfix for floating point instructions
    * fixed wrong shifter constant handling

  Revision 1.9  2003/09/01 09:54:57  florian
    *  results of work on arm port last weekend

  Revision 1.8  2003/08/29 21:36:28  florian
    * fixed procedure entry/exit code
    * started to fix reference handling

  Revision 1.7  2003/08/28 13:26:10  florian
    * another couple of arm fixes

  Revision 1.6  2003/08/28 00:05:29  florian
    * today's arm patches

  Revision 1.5  2003/08/25 23:20:38  florian
    + started to implement FPU support for the ARM
    * fixed a lot of other things

  Revision 1.4  2003/08/24 12:27:26  florian
    * continued to work on the arm port

  Revision 1.3  2003/08/21 03:14:00  florian
    * arm compiler can be compiled; far from being working

  Revision 1.2  2003/08/20 15:50:12  florian
    * more arm stuff

  Revision 1.1  2003/07/21 16:35:30  florian
    * very basic stuff for the arm
}
