{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the code generator for the i386

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
       globtype,
       cgbase,cgobj,cg64f32,cgx86,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,parabase,
       node,symconst
       ;

    type
      tcg386 = class(tcgx86)
        procedure init_register_allocators;override;
        { passing parameter using push instead of mov }
        procedure a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const cgpara : tcgpara);override;
        procedure a_param_const(list : taasmoutput;size : tcgsize;a : aint;const cgpara : tcgpara);override;
        procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const cgpara : tcgpara);override;
        procedure a_paramaddr_ref(list : taasmoutput;const r : treference;const cgpara : tcgpara);override;

        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_restore_all_registers(list : taasmoutput;const funcretparaloc:tcgpara);override;
        procedure g_proc_exit(list : taasmoutput;parasize:longint;nostackframe:boolean);override;
        procedure g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;const lenloc:tlocation;elesize:aint;destreg:tregister);override;

        procedure g_exception_reason_save(list : taasmoutput; const href : treference);override;
        procedure g_exception_reason_save_const(list : taasmoutput; const href : treference; a: aint);override;
        procedure g_exception_reason_load(list : taasmoutput; const href : treference);override;
     end;

      tcg64f386 = class(tcg64f32)
        procedure a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);override;
        procedure a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);override;
        procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;value : int64;reg : tregister64);override;
        procedure a_op64_const_ref(list : taasmoutput;op:TOpCG;value : int64;const ref : treference);override;
      private
        procedure get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      end;

  implementation

    uses
       globals,verbose,systems,cutils,
       paramgr,procinfo,
       rgcpu,rgx86,tgobj,
       cgutils;

    procedure Tcg386.init_register_allocators;
      begin
        inherited init_register_allocators;
        if cs_create_pic in aktmoduleswitches then
          rg[R_INTREGISTER]:=trgcpu.create(R_INTREGISTER,R_SUBWHOLE,[RS_EAX,RS_EDX,RS_ECX,RS_ESI,RS_EDI],first_int_imreg,[RS_EBP,RS_EBX])
        else
          rg[R_INTREGISTER]:=trgcpu.create(R_INTREGISTER,R_SUBWHOLE,[RS_EAX,RS_EDX,RS_ECX,RS_EBX,RS_ESI,RS_EDI],first_int_imreg,[RS_EBP]);
        rg[R_MMXREGISTER]:=trgcpu.create(R_MMXREGISTER,R_SUBNONE,[RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7],first_mm_imreg,[]);
        rg[R_MMREGISTER]:=trgcpu.create(R_MMREGISTER,R_SUBNONE,[RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7],first_mm_imreg,[]);
        rgfpu:=Trgx86fpu.create;
      end;


    procedure tcg386.a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const cgpara : tcgpara);
      var
        pushsize : tcgsize;
      begin
        check_register_size(size,r);
        with cgpara do
          if assigned(location) and
             (location^.loc=LOC_REFERENCE) and
             (location^.reference.index=NR_STACK_POINTER_REG) then
            begin
              pushsize:=int_cgsize(alignment);
              list.concat(taicpu.op_reg(A_PUSH,tcgsize2opsize[pushsize],makeregsize(list,r,pushsize)));
            end
          else
            inherited a_param_reg(list,size,r,cgpara);
      end;


    procedure tcg386.a_param_const(list : taasmoutput;size : tcgsize;a : aint;const cgpara : tcgpara);
      var
        pushsize : tcgsize;
      begin
        with cgpara do
          if assigned(location) and
             (location^.loc=LOC_REFERENCE) and
             (location^.reference.index=NR_STACK_POINTER_REG) then
            begin
              pushsize:=int_cgsize(alignment);
              list.concat(taicpu.op_const(A_PUSH,tcgsize2opsize[pushsize],a));
            end
          else
            inherited a_param_const(list,size,a,cgpara);
      end;


    procedure tcg386.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const cgpara : tcgpara);
      var
        pushsize : tcgsize;
        tmpreg : tregister;
      begin
        with cgpara do
          if assigned(location) and
             (location^.loc=LOC_REFERENCE) and
             (location^.reference.index=NR_STACK_POINTER_REG) then
            begin
              pushsize:=int_cgsize(alignment);
              if tcgsize2size[size]<alignment then
                begin
                  tmpreg:=getintregister(list,pushsize);
                  a_load_ref_reg(list,size,pushsize,r,tmpreg);
                  list.concat(taicpu.op_reg(A_PUSH,TCgsize2opsize[pushsize],tmpreg));
                end
              else
                list.concat(taicpu.op_ref(A_PUSH,TCgsize2opsize[pushsize],r));
            end
          else
            inherited a_param_ref(list,size,r,cgpara);
      end;


    procedure tcg386.a_paramaddr_ref(list : taasmoutput;const r : treference;const cgpara : tcgpara);
      var
        tmpreg : tregister;
        opsize : topsize;
      begin
        with r do
          begin
            if (segment<>NR_NO) then
              cgmessage(cg_e_cant_use_far_pointer_there);
            with cgpara do
              if assigned(location) and
                 (location^.loc=LOC_REFERENCE) and
                 (location^.reference.index=NR_STACK_POINTER_REG) then
                begin
                  opsize:=tcgsize2opsize[OS_ADDR];
                  if (base=NR_NO) and (index=NR_NO) then
                    begin
                      if assigned(symbol) then
                        list.concat(Taicpu.Op_sym_ofs(A_PUSH,opsize,symbol,offset))
                      else
                        list.concat(Taicpu.Op_const(A_PUSH,opsize,offset));
                    end
                  else if (base=NR_NO) and (index<>NR_NO) and
                          (offset=0) and (scalefactor=0) and (symbol=nil) then
                    list.concat(Taicpu.Op_reg(A_PUSH,opsize,index))
                  else if (base<>NR_NO) and (index=NR_NO) and
                          (offset=0) and (symbol=nil) then
                    list.concat(Taicpu.Op_reg(A_PUSH,opsize,base))
                  else
                    begin
                      tmpreg:=getaddressregister(list);
                      a_loadaddr_ref_reg(list,r,tmpreg);
                      list.concat(taicpu.op_reg(A_PUSH,opsize,tmpreg));
                    end;
                end
              else
                inherited a_paramaddr_ref(list,r,cgpara);
        end;
      end;


    procedure tcg386.g_save_all_registers(list : taasmoutput);
      begin
        list.concat(Taicpu.Op_none(A_PUSHA,S_L));
        tg.GetTemp(list,sizeof(aint),tt_noreuse,current_procinfo.save_regs_ref);
        a_load_reg_ref(list,OS_ADDR,OS_ADDR,NR_STACK_POINTER_REG,current_procinfo.save_regs_ref);
      end;


    procedure tcg386.g_restore_all_registers(list : taasmoutput;const funcretparaloc:tcgpara);
      var
        href : treference;
      begin
        a_load_ref_reg(list,OS_ADDR,OS_ADDR,current_procinfo.save_regs_ref,NR_STACK_POINTER_REG);
        tg.UnGetTemp(list,current_procinfo.save_regs_ref);
        if assigned(funcretparaloc.location) and
           (funcretparaloc.location^.loc=LOC_REGISTER) then
          begin
            if funcretparaloc.size in [OS_64,OS_S64] then
              begin
                reference_reset_base(href,NR_STACK_POINTER_REG,20);
                a_load_reg_ref(list,OS_32,OS_32,NR_FUNCTION_RETURN64_HIGH_REG,href);
                reference_reset_base(href,NR_STACK_POINTER_REG,28);
                a_load_reg_ref(list,OS_32,OS_32,NR_FUNCTION_RETURN64_LOW_REG,href);
              end
            else
              begin
                reference_reset_base(href,NR_STACK_POINTER_REG,28);
                a_load_reg_ref(list,OS_32,OS_32,NR_FUNCTION_RETURN_REG,href);
              end;
          end;
        list.concat(Taicpu.Op_none(A_POPA,S_L));
        { We add a NOP because of the 386DX CPU bugs with POPAD }
        list.concat(taicpu.op_none(A_NOP,S_L));
      end;


    procedure tcg386.g_proc_exit(list : taasmoutput;parasize:longint;nostackframe:boolean);
      var
        stacksize : longint;
      begin
        { Release PIC register }
        if cs_create_pic in aktmoduleswitches then
          list.concat(tai_regalloc.dealloc(NR_PIC_OFFSET_REG,nil));

        { MMX needs to call EMMS }
        if assigned(rg[R_MMXREGISTER]) and
           (rg[R_MMXREGISTER].uses_registers) then
          list.concat(Taicpu.op_none(A_EMMS,S_NO));

        { remove stackframe }
        if not nostackframe then
          begin
            if (current_procinfo.framepointer=NR_STACK_POINTER_REG) then
              begin
                stacksize:=current_procinfo.calc_stackframe_size;
                if (stacksize<>0) then
                  cg.a_op_const_reg(list,OP_ADD,OS_ADDR,stacksize,current_procinfo.framepointer);
              end
            else
              list.concat(Taicpu.op_none(A_LEAVE,S_NO));
            list.concat(tai_regalloc.dealloc(NR_FRAME_POINTER_REG,nil));
          end;

        { return from proc }
        if (po_interrupt in current_procinfo.procdef.procoptions) then
          begin
            if assigned(current_procinfo.procdef.funcret_paraloc[calleeside].location) and
               (current_procinfo.procdef.funcret_paraloc[calleeside].location^.loc=LOC_REGISTER) then
              list.concat(Taicpu.Op_const_reg(A_ADD,S_L,4,NR_ESP))
            else
              list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EAX));
            list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EBX));
            list.concat(Taicpu.Op_reg(A_POP,S_L,NR_ECX));
            if assigned(current_procinfo.procdef.funcret_paraloc[calleeside].location) and
               assigned(current_procinfo.procdef.funcret_paraloc[calleeside].location^.next) and
               (current_procinfo.procdef.funcret_paraloc[calleeside].location^.next^.loc=LOC_REGISTER) then
              list.concat(Taicpu.Op_const_reg(A_ADD,S_L,4,NR_ESP))
            else
              list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EDX));
            list.concat(Taicpu.Op_reg(A_POP,S_L,NR_ESI));
            list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EDI));
            { .... also the segment registers }
            list.concat(Taicpu.Op_reg(A_POP,S_W,NR_DS));
            list.concat(Taicpu.Op_reg(A_POP,S_W,NR_ES));
            list.concat(Taicpu.Op_reg(A_POP,S_W,NR_FS));
            list.concat(Taicpu.Op_reg(A_POP,S_W,NR_GS));
            { this restores the flags }
            list.concat(Taicpu.Op_none(A_IRET,S_NO));
          end
        { Routines with the poclearstack flag set use only a ret }
        else if current_procinfo.procdef.proccalloption in clearstack_pocalls then
         begin
           { complex return values are removed from stack in C code PM }
           if paramanager.ret_in_param(current_procinfo.procdef.rettype.def,
                                       current_procinfo.procdef.proccalloption) then
             list.concat(Taicpu.Op_const(A_RET,S_NO,sizeof(aint)))
           else
             list.concat(Taicpu.Op_none(A_RET,S_NO));
         end
        { ... also routines with parasize=0 }
        else if (parasize=0) then
         list.concat(Taicpu.Op_none(A_RET,S_NO))
        else
         begin
           { parameters are limited to 65535 bytes because ret allows only imm16 }
           if (parasize>65535) then
             CGMessage(cg_e_parasize_too_big);
           list.concat(Taicpu.Op_const(A_RET,S_NO,parasize));
         end;
      end;


    procedure tcg386.g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;const lenloc:tlocation;elesize:aint;destreg:tregister);
      var
        power,len  : longint;
        opsize : topsize;
{$ifndef __NOWINPECOFF__}
        again,ok : tasmlabel;
{$endif}
      begin
        { get stack space }
        getcpuregister(list,NR_EDI);
        a_load_loc_reg(list,OS_INT,lenloc,NR_EDI);
        list.concat(Taicpu.op_reg(A_INC,S_L,NR_EDI));
        if (elesize<>1) then
         begin
           if ispowerof2(elesize, power) then
             list.concat(Taicpu.op_const_reg(A_SHL,S_L,power,NR_EDI))
           else
             list.concat(Taicpu.op_const_reg(A_IMUL,S_L,elesize,NR_EDI));
         end;
{$ifndef __NOWINPECOFF__}
        { windows guards only a few pages for stack growing, }
        { so we have to access every page first              }
        if target_info.system=system_i386_win32 then
          begin
             objectlibrary.getlabel(again);
             objectlibrary.getlabel(ok);
             a_label(list,again);
             list.concat(Taicpu.op_const_reg(A_CMP,S_L,winstackpagesize,NR_EDI));
             a_jmp_cond(list,OC_B,ok);
             list.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize-4,NR_ESP));
             list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_EDI));
             list.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize,NR_EDI));
             a_jmp_always(list,again);

             a_label(list,ok);
             list.concat(Taicpu.op_reg_reg(A_SUB,S_L,NR_EDI,NR_ESP));
             ungetcpuregister(list,NR_EDI);
             { now reload EDI }
             getcpuregister(list,NR_EDI);
             a_load_loc_reg(list,OS_INT,lenloc,NR_EDI);
             list.concat(Taicpu.op_reg(A_INC,S_L,NR_EDI));

             if (elesize<>1) then
              begin
                if ispowerof2(elesize, power) then
                  list.concat(Taicpu.op_const_reg(A_SHL,S_L,power,NR_EDI))
                else
                  list.concat(Taicpu.op_const_reg(A_IMUL,S_L,elesize,NR_EDI));
              end;
          end
        else
{$endif __NOWINPECOFF__}
          list.concat(Taicpu.op_reg_reg(A_SUB,S_L,NR_EDI,NR_ESP));
        { align stack on 4 bytes }
        list.concat(Taicpu.op_const_reg(A_AND,S_L,aint($fffffff4),NR_ESP));
        { load destination, don't use a_load_reg_reg, that will add a move instruction
          that can confuse the reg allocator }
        list.concat(Taicpu.Op_reg_reg(A_MOV,S_L,NR_ESP,NR_EDI));

        { Allocate other registers }
        getcpuregister(list,NR_ECX);
        getcpuregister(list,NR_ESI);

        { load count }
        a_load_loc_reg(list,OS_INT,lenloc,NR_ECX);

        { load source }
        a_loadaddr_ref_reg(list,ref,NR_ESI);

        { scheduled .... }
        list.concat(Taicpu.op_reg(A_INC,S_L,NR_ECX));

        { calculate size }
        len:=elesize;
        opsize:=S_B;
        if (len and 3)=0 then
         begin
           opsize:=S_L;
           len:=len shr 2;
         end
        else
         if (len and 1)=0 then
          begin
            opsize:=S_W;
            len:=len shr 1;
          end;

        if len<>0 then
          begin
            if ispowerof2(len, power) then
              list.concat(Taicpu.op_const_reg(A_SHL,S_L,power,NR_ECX))
            else
              list.concat(Taicpu.op_const_reg(A_IMUL,S_L,len,NR_ECX));
          end;
        list.concat(Taicpu.op_none(A_REP,S_NO));
        case opsize of
          S_B : list.concat(Taicpu.Op_none(A_MOVSB,S_NO));
          S_W : list.concat(Taicpu.Op_none(A_MOVSW,S_NO));
          S_L : list.concat(Taicpu.Op_none(A_MOVSD,S_NO));
        end;
        ungetcpuregister(list,NR_EDI);
        ungetcpuregister(list,NR_ECX);
        ungetcpuregister(list,NR_ESI);

        { patch the new address, but don't use a_load_reg_reg, that will add a move instruction
          that can confuse the reg allocator }
        list.concat(Taicpu.Op_reg_reg(A_MOV,S_L,NR_ESP,destreg));
      end;


    procedure tcg386.g_exception_reason_save(list : taasmoutput; const href : treference);
      begin
        list.concat(Taicpu.op_reg(A_PUSH,tcgsize2opsize[OS_INT],NR_FUNCTION_RESULT_REG));
      end;


    procedure tcg386.g_exception_reason_save_const(list : taasmoutput;const href : treference; a: aint);
      begin
        list.concat(Taicpu.op_const(A_PUSH,tcgsize2opsize[OS_INT],a));
      end;


    procedure tcg386.g_exception_reason_load(list : taasmoutput; const href : treference);
      begin
        list.concat(Taicpu.op_reg(A_POP,tcgsize2opsize[OS_INT],NR_FUNCTION_RESULT_REG));
      end;


{ ************* 64bit operations ************ }

    procedure tcg64f386.get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      begin
        case op of
          OP_ADD :
            begin
              op1:=A_ADD;
              op2:=A_ADC;
            end;
          OP_SUB :
            begin
              op1:=A_SUB;
              op2:=A_SBB;
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


    procedure tcg64f386.a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);
      var
        op1,op2 : TAsmOp;
        tempref : treference;
      begin
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_ref_reg(op1,S_L,ref,reg.reglo));
        tempref:=ref;
        inc(tempref.offset,4);
        list.concat(taicpu.op_ref_reg(op2,S_L,tempref,reg.reghi));
      end;


    procedure tcg64f386.a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);
      var
        op1,op2 : TAsmOp;
      begin
        case op of
          OP_NEG :
            begin
              if (regsrc.reglo<>regdst.reglo) then
                a_load64_reg_reg(list,regsrc,regdst);
              list.concat(taicpu.op_reg(A_NOT,S_L,regdst.reghi));
              list.concat(taicpu.op_reg(A_NEG,S_L,regdst.reglo));
              list.concat(taicpu.op_const_reg(A_SBB,S_L,-1,regdst.reghi));
              exit;
            end;
          OP_NOT :
            begin
              if (regsrc.reglo<>regdst.reglo) then
                a_load64_reg_reg(list,regsrc,regdst);
              list.concat(taicpu.op_reg(A_NOT,S_L,regdst.reghi));
              list.concat(taicpu.op_reg(A_NOT,S_L,regdst.reglo));
              exit;
            end;
        end;
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_reg_reg(op1,S_L,regsrc.reglo,regdst.reglo));
        list.concat(taicpu.op_reg_reg(op2,S_L,regsrc.reghi,regdst.reghi));
      end;


    procedure tcg64f386.a_op64_const_reg(list : taasmoutput;op:TOpCG;value : int64;reg : tregister64);
      var
        op1,op2 : TAsmOp;
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_const_reg(list,op,OS_32,aint(lo(value)),reg.reglo);
              cg.a_op_const_reg(list,op,OS_32,aint(hi(value)),reg.reghi);
            end;
          OP_ADD, OP_SUB:
            begin
              // can't use a_op_const_ref because this may use dec/inc
              get_64bit_ops(op,op1,op2);
              list.concat(taicpu.op_const_reg(op1,S_L,aint(lo(value)),reg.reglo));
              list.concat(taicpu.op_const_reg(op2,S_L,aint(hi(value)),reg.reghi));
            end;
          else
            internalerror(200204021);
        end;
      end;


    procedure tcg64f386.a_op64_const_ref(list : taasmoutput;op:TOpCG;value : int64;const ref : treference);
      var
        op1,op2 : TAsmOp;
        tempref : treference;
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_const_ref(list,op,OS_32,lo(value),ref);
              tempref:=ref;
              inc(tempref.offset,4);
              cg.a_op_const_ref(list,op,OS_32,hi(value),tempref);
            end;
          OP_ADD, OP_SUB:
            begin
              get_64bit_ops(op,op1,op2);
              // can't use a_op_const_ref because this may use dec/inc
              list.concat(taicpu.op_const_ref(op1,S_L,lo(value),ref));
              tempref:=ref;
              inc(tempref.offset,4);
              list.concat(taicpu.op_const_ref(op2,S_L,hi(value),tempref));
            end;
          else
            internalerror(200204022);
        end;
      end;

begin
  cg := tcg386.create;
  cg64 := tcg64f386.create;
end.
{
  $Log$
  Revision 1.58  2004-10-24 11:44:28  peter
    * small regvar fixes
    * loadref parameter removed from concatcopy,incrrefcount,etc

  Revision 1.57  2004/10/15 09:16:21  mazen
  - remove $IFDEF DELPHI and related code
  - remove $IFDEF FPCPROCVAR and related code

  Revision 1.56  2004/10/13 21:12:51  peter
    * -Or fixes for open array

  Revision 1.55  2004/10/11 15:46:45  peter
    * length parameter for copyvaluearray changed to tlocation

  Revision 1.54  2004/10/05 20:41:01  peter
    * more spilling rewrites

  Revision 1.53  2004/09/25 14:23:54  peter
    * ungetregister is now only used for cpuregisters, renamed to
      ungetcpuregister
    * renamed (get|unget)explicitregister(s) to ..cpuregister
    * removed location-release/reference_release

  Revision 1.52  2004/09/21 17:25:12  peter
    * paraloc branch merged

  Revision 1.51.4.1  2004/08/31 20:43:06  peter
    * paraloc patch

  Revision 1.51  2004/07/09 23:30:13  jonas
    *  changed first_sse_imreg to first_mm_imreg

  Revision 1.50  2004/06/20 08:55:31  florian
    * logs truncated

  Revision 1.49  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.48  2004/04/09 14:36:05  peter
    * A_MOVSL renamed to A_MOVSD

  Revision 1.47.2.9  2004/05/30 10:45:50  peter
    * merged fixes from main branch

  Revision 1.47.2.8  2004/05/02 21:34:01  florian
    * i386 compilation fixed

  Revision 1.47.2.7  2004/05/02 12:45:32  peter
    * enabled cpuhasfixedstack for x86-64 again
    * fixed size of temp allocation for parameters

}
