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
{ This unit implements the code generator for the i386.
}
unit cgcpu;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       cgbase,cgobj,cg64f32,cgx86,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,
       node,symconst
{$ifdef delphi}
       ,dmisc
{$endif}
       ;

    type
      tcg386 = class(tcgx86)
        procedure init_register_allocators;override;
        { passing parameter using push instead of mov }
        procedure a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const locpara : tparalocation);override;
        procedure a_param_const(list : taasmoutput;size : tcgsize;a : aint;const locpara : tparalocation);override;
        procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);override;
        procedure a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);override;

        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_restore_all_registers(list : taasmoutput;const funcretparaloc:tparalocation);override;
        procedure g_proc_exit(list : taasmoutput;parasize:longint;nostackframe:boolean);override;
        procedure g_copyvaluepara_openarray(list : taasmoutput;const ref, lenref:treference;elesize:aint);override;

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
       symdef,symsym,defutil,paramgr,procinfo,
       rgcpu,rgx86,tgobj,
       cgutils;

    procedure Tcg386.init_register_allocators;
      begin
        inherited init_register_allocators;
        if cs_create_pic in aktmoduleswitches then
          rg[R_INTREGISTER]:=trgcpu.create(R_INTREGISTER,R_SUBWHOLE,[RS_EAX,RS_EDX,RS_ECX,RS_ESI,RS_EDI],first_int_imreg,[RS_EBP,RS_EBX])
        else
          rg[R_INTREGISTER]:=trgcpu.create(R_INTREGISTER,R_SUBWHOLE,[RS_EAX,RS_EDX,RS_ECX,RS_EBX,RS_ESI,RS_EDI],first_int_imreg,[RS_EBP]);
        rg[R_MMXREGISTER]:=trgcpu.create(R_MMXREGISTER,R_SUBNONE,[RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7],first_sse_imreg,[]);
        rg[R_MMREGISTER]:=trgcpu.create(R_MMREGISTER,R_SUBNONE,[RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7],first_sse_imreg,[]);
        rgfpu:=Trgx86fpu.create;
      end;


    procedure tcg386.a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const locpara : tparalocation);
      var
        pushsize : tcgsize;
      begin
        check_register_size(size,r);
        with locpara do
          if (loc=LOC_REFERENCE) and
             (reference.index=NR_STACK_POINTER_REG) then
            begin
              pushsize:=int_cgsize(alignment);
              list.concat(taicpu.op_reg(A_PUSH,tcgsize2opsize[pushsize],makeregsize(list,r,pushsize)));
            end
          else
            inherited a_param_reg(list,size,r,locpara);
      end;


    procedure tcg386.a_param_const(list : taasmoutput;size : tcgsize;a : aint;const locpara : tparalocation);
      var
        pushsize : tcgsize;
      begin
        with locpara do
          if (loc=LOC_REFERENCE) and
             (reference.index=NR_STACK_POINTER_REG) then
            begin
              pushsize:=int_cgsize(alignment);
              list.concat(taicpu.op_const(A_PUSH,tcgsize2opsize[pushsize],a));
            end
          else
            inherited a_param_const(list,size,a,locpara);
      end;


    procedure tcg386.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);
      var
        pushsize : tcgsize;
        tmpreg : tregister;
      begin
        with locpara do
          if (loc=LOC_REFERENCE) and
             (reference.index=NR_STACK_POINTER_REG) then
            begin
              pushsize:=int_cgsize(alignment);
              if tcgsize2size[size]<alignment then
                begin
                  tmpreg:=getintregister(list,pushsize);
                  a_load_ref_reg(list,size,pushsize,r,tmpreg);
                  list.concat(taicpu.op_reg(A_PUSH,TCgsize2opsize[pushsize],tmpreg));
                  ungetregister(list,tmpreg);
                end
              else
                list.concat(taicpu.op_ref(A_PUSH,TCgsize2opsize[pushsize],r));
            end
          else
            inherited a_param_ref(list,size,r,locpara);
      end;


    procedure tcg386.a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);
      var
        tmpreg : tregister;
        opsize : topsize;
      begin
        with r do
          begin
            if (segment<>NR_NO) then
              cgmessage(cg_e_cant_use_far_pointer_there);
            with locpara do
              if (locpara.loc=LOC_REFERENCE) and
                 (locpara.reference.index=NR_STACK_POINTER_REG) then
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
                      ungetregister(list,tmpreg);
                      list.concat(taicpu.op_reg(A_PUSH,opsize,tmpreg));
                    end;
                end
              else
                inherited a_paramaddr_ref(list,r,locpara);
        end;
      end;


    procedure tcg386.g_save_all_registers(list : taasmoutput);
      begin
        list.concat(Taicpu.Op_none(A_PUSHA,S_L));
        tg.GetTemp(list,sizeof(aint),tt_noreuse,current_procinfo.save_regs_ref);
        a_load_reg_ref(list,OS_ADDR,OS_ADDR,NR_STACK_POINTER_REG,current_procinfo.save_regs_ref);
      end;


    procedure tcg386.g_restore_all_registers(list : taasmoutput;const funcretparaloc:tparalocation);
      var
        href : treference;
      begin
        a_load_ref_reg(list,OS_ADDR,OS_ADDR,current_procinfo.save_regs_ref,NR_STACK_POINTER_REG);
        tg.UnGetTemp(list,current_procinfo.save_regs_ref);
        if funcretparaloc.loc=LOC_REGISTER then
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
          list.concat(tai_regalloc.dealloc(NR_PIC_OFFSET_REG));

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
            list.concat(tai_regalloc.dealloc(NR_FRAME_POINTER_REG));
          end;

        { return from proc }
        if (po_interrupt in current_procinfo.procdef.procoptions) then
          begin
            if current_procinfo.procdef.funcret_paraloc[calleeside].loc=LOC_REGISTER then
              list.concat(Taicpu.Op_const_reg(A_ADD,S_L,4,NR_ESP))
            else
              list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EAX));
            list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EBX));
            list.concat(Taicpu.Op_reg(A_POP,S_L,NR_ECX));
            if current_procinfo.procdef.funcret_paraloc[calleeside].lochigh=LOC_REGISTER then
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


    procedure tcg386.g_copyvaluepara_openarray(list : taasmoutput;const ref, lenref:treference;elesize:aint);
      var
        power,len  : longint;
        opsize : topsize;
{$ifndef __NOWINPECOFF__}
        again,ok : tasmlabel;
{$endif}
      begin
        { get stack space }
        getexplicitregister(list,NR_EDI);
        list.concat(Taicpu.op_ref_reg(A_MOV,S_L,lenref,NR_EDI));
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
             ungetregister(list,NR_EDI);
             { now reload EDI }
             getexplicitregister(list,NR_EDI);
             list.concat(Taicpu.op_ref_reg(A_MOV,S_L,lenref,NR_EDI));
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
        { load destination }
        a_load_reg_reg(list,OS_INT,OS_INT,NR_ESP,NR_EDI);

        { Allocate other registers }
        getexplicitregister(list,NR_ECX);
        getexplicitregister(list,NR_ESI);

        { load count }
        a_load_ref_reg(list,OS_INT,OS_INT,lenref,NR_ECX);

        { load source }
        a_load_ref_reg(list,OS_INT,OS_INT,ref,NR_ESI);

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

        if ispowerof2(len, power) then
          list.concat(Taicpu.op_const_reg(A_SHL,S_L,power,NR_ECX))
        else
          list.concat(Taicpu.op_const_reg(A_IMUL,S_L,len,NR_ECX));
        list.concat(Taicpu.op_none(A_REP,S_NO));
        case opsize of
          S_B : list.concat(Taicpu.Op_none(A_MOVSB,S_NO));
          S_W : list.concat(Taicpu.Op_none(A_MOVSW,S_NO));
          S_L : list.concat(Taicpu.Op_none(A_MOVSD,S_NO));
        end;
        ungetregister(list,NR_EDI);
        ungetregister(list,NR_ECX);
        ungetregister(list,NR_ESI);

        { patch the new address }
        a_load_reg_ref(list,OS_INT,OS_INT,NR_ESP,ref);
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
  Revision 1.49  2004-06-16 20:07:10  florian
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

  Revision 1.47.2.6  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.47.2.5  2004/04/29 23:30:28  peter
    * fix i386 compiler

  Revision 1.47.2.4  2004/04/29 19:07:22  peter
    * compile fixes

  Revision 1.47.2.3  2004/04/24 20:13:24  florian
    * fixed x86-64 exception handling

  Revision 1.47.2.2  2004/04/24 18:30:11  florian
    * extended parameters shouldn't be poped by the callee on x86-64 either

  Revision 1.47.2.1  2004/04/08 18:33:22  peter
    * rewrite of TAsmSection

  Revision 1.47  2004/02/27 10:21:05  florian
    * top_symbol killed
    + refaddr to treference added
    + refsymbol to treference added
    * top_local stuff moved to an extra record to save memory
    + aint introduced
    * tppufile.get/putint64/aint implemented

  Revision 1.46  2004/02/22 16:48:09  florian
    * x86_64 uses generic concatcopy_valueopenarray for now

  Revision 1.45  2004/02/04 22:01:13  peter
    * first try to get cpupara working for x86_64

  Revision 1.44  2004/01/14 23:39:05  florian
    * another bunch of x86-64 fixes mainly calling convention and
      assembler reader related

  Revision 1.43  2004/01/12 16:39:40  peter
    * sparc updates, mostly float related

  Revision 1.42  2003/12/24 00:10:02  florian
    - delete parameter in cg64 methods removed

  Revision 1.41  2003/12/19 22:08:44  daniel
    * Some work to restore the MMX capabilities

  Revision 1.40  2003/10/10 17:48:14  peter
    * old trgobj moved to x86/rgcpu and renamed to trgx86fpu
    * tregisteralloctor renamed to trgobj
    * removed rgobj from a lot of units
    * moved location_* and reference_* to cgobj
    * first things for mmx register allocation

  Revision 1.39  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.38  2003/09/25 13:13:32  florian
    * more x86-64 fixes

  Revision 1.37  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.36.2.1  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.36  2003/06/12 18:31:18  peter
    * fix newra cycle for i386

  Revision 1.35  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.34  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.33  2003/05/22 21:32:28  peter
    * removed some unit dependencies

  Revision 1.32  2002/11/25 17:43:26  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.31  2002/10/05 12:43:29  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.30  2002/09/07 15:25:10  peter
    * old logs removed and tabs fixed

  Revision 1.29  2002/07/20 19:28:47  florian
    * splitting of i386\cgcpu.pas into x86\cgx86.pas and i386\cgcpu.pas
      cgx86.pas will contain the common code for i386 and x86_64

  Revision 1.28  2002/07/20 11:58:00  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.27  2002/07/11 14:41:32  florian
    * start of the new generic parameter handling

  Revision 1.26  2002/07/07 09:52:33  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.25  2002/07/01 18:46:30  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.24  2002/07/01 16:23:55  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.23  2002/06/16 08:16:59  carl
  * bugfix of missing popecx for shift operations

  Revision 1.22  2002/05/22 19:02:16  carl
  + generic FPC_HELP_FAIL
  + generic FPC_HELP_DESTRUCTOR instated (original from Pierre)
  + generic FPC_DISPOSE_CLASS
  + TEST_GENERIC define

  Revision 1.21  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.20  2002/05/18 13:34:22  peter
    * readded missing revisions

  Revision 1.19  2002/05/16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.17  2002/05/13 19:54:37  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.16  2002/05/12 19:59:05  carl
  * some small portability fixes

  Revision 1.15  2002/05/12 16:53:16  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.14  2002/04/25 20:16:40  peter
    * moved more routines from cga/n386util

  Revision 1.13  2002/04/21 15:31:05  carl
  * changeregsize -> rg.makeregsize
  + a_jmp_always added

  Revision 1.12  2002/04/15 19:44:20  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.11  2002/04/04 19:06:10  peter
    * removed unused units
    * use tlocation.size in a_*loc*() routines

  Revision 1.10  2002/04/02 20:29:02  jonas
    * optimized the code generated by the a_op_const_* and a_op64_const
      methods

  Revision 1.9  2002/04/02 17:11:33  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

}
