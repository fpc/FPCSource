{
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
       aasmbase,aasmtai,aasmdata,aasmcpu,
       cpubase,parabase,cgutils,
       symconst,symdef
       ;

    type
      tcg386 = class(tcgx86)
        procedure init_register_allocators;override;
        procedure do_register_allocation(list:TAsmList;headertai:tai);override;

        { passing parameter using push instead of mov }
        procedure a_param_reg(list : TAsmList;size : tcgsize;r : tregister;const cgpara : tcgpara);override;
        procedure a_param_const(list : TAsmList;size : tcgsize;a : aint;const cgpara : tcgpara);override;
        procedure a_param_ref(list : TAsmList;size : tcgsize;const r : treference;const cgpara : tcgpara);override;
        procedure a_paramaddr_ref(list : TAsmList;const r : treference;const cgpara : tcgpara);override;

        procedure g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean);override;
        procedure g_copyvaluepara_openarray(list : TAsmList;const ref:treference;const lenloc:tlocation;elesize:aint;destreg:tregister);override;

        procedure g_exception_reason_save(list : TAsmList; const href : treference);override;
        procedure g_exception_reason_save_const(list : TAsmList; const href : treference; a: aint);override;
        procedure g_exception_reason_load(list : TAsmList; const href : treference);override;
        procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
     end;

      tcg64f386 = class(tcg64f32)
        procedure a_op64_ref_reg(list : TAsmList;op:TOpCG;size : tcgsize;const ref : treference;reg : tregister64);override;
        procedure a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);override;
        procedure a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);override;
        procedure a_op64_const_ref(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;const ref : treference);override;
      private
        procedure get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      end;

  implementation

    uses
       globals,verbose,systems,cutils,
       paramgr,procinfo,fmodule,
       rgcpu,rgx86;

    function use_push(const cgpara:tcgpara):boolean;
      begin
        result:=(not use_fixed_stack) and
                assigned(cgpara.location) and
                (cgpara.location^.loc=LOC_REFERENCE) and
                (cgpara.location^.reference.index=NR_STACK_POINTER_REG);
      end;


    procedure tcg386.init_register_allocators;
      begin
        inherited init_register_allocators;
        if cs_create_pic in current_settings.moduleswitches then
          rg[R_INTREGISTER]:=trgcpu.create(R_INTREGISTER,R_SUBWHOLE,[RS_EAX,RS_EDX,RS_ECX,RS_ESI,RS_EDI],first_int_imreg,[RS_EBP])
        else
          rg[R_INTREGISTER]:=trgcpu.create(R_INTREGISTER,R_SUBWHOLE,[RS_EAX,RS_EDX,RS_ECX,RS_EBX,RS_ESI,RS_EDI],first_int_imreg,[RS_EBP]);
        rg[R_MMXREGISTER]:=trgcpu.create(R_MMXREGISTER,R_SUBNONE,[RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7],first_mm_imreg,[]);
        rg[R_MMREGISTER]:=trgcpu.create(R_MMREGISTER,R_SUBWHOLE,[RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7],first_mm_imreg,[]);
        rgfpu:=Trgx86fpu.create;
      end;

    procedure tcg386.do_register_allocation(list:TAsmList;headertai:tai);
      begin
        if pi_needs_got in current_procinfo.flags then
          include(rg[R_INTREGISTER].used_in_proc,getsupreg(current_procinfo.got));
        inherited do_register_allocation(list,headertai);
      end;


    procedure tcg386.a_param_reg(list : TAsmList;size : tcgsize;r : tregister;const cgpara : tcgpara);
      var
        pushsize : tcgsize;
      begin
        check_register_size(size,r);
        if use_push(cgpara) then
          begin
            cgpara.check_simple_location;
            if tcgsize2size[cgpara.location^.size]>cgpara.alignment then
              pushsize:=cgpara.location^.size
            else
              pushsize:=int_cgsize(cgpara.alignment);
            list.concat(taicpu.op_reg(A_PUSH,tcgsize2opsize[pushsize],makeregsize(list,r,pushsize)));
          end
        else
          inherited a_param_reg(list,size,r,cgpara);
      end;


    procedure tcg386.a_param_const(list : TAsmList;size : tcgsize;a : aint;const cgpara : tcgpara);
      var
        pushsize : tcgsize;
      begin
        if use_push(cgpara) then
          begin
            cgpara.check_simple_location;
            if tcgsize2size[cgpara.location^.size]>cgpara.alignment then
              pushsize:=cgpara.location^.size
            else
              pushsize:=int_cgsize(cgpara.alignment);
            list.concat(taicpu.op_const(A_PUSH,tcgsize2opsize[pushsize],a));
          end
        else
          inherited a_param_const(list,size,a,cgpara);
      end;


    procedure tcg386.a_param_ref(list : TAsmList;size : tcgsize;const r : treference;const cgpara : tcgpara);

        procedure pushdata(paraloc:pcgparalocation;ofs:aint);
        var
          pushsize : tcgsize;
          tmpreg   : tregister;
          href     : treference;
        begin
          if not assigned(paraloc) then
            exit;
          if (paraloc^.loc<>LOC_REFERENCE) or
             (paraloc^.reference.index<>NR_STACK_POINTER_REG) or
             (tcgsize2size[paraloc^.size]>sizeof(aint)) then
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
          if tcgsize2size[paraloc^.size]>cgpara.alignment then
            pushsize:=paraloc^.size
          else
            pushsize:=int_cgsize(cgpara.alignment);
          if tcgsize2size[paraloc^.size]<cgpara.alignment then
            begin
              tmpreg:=getintregister(list,pushsize);
              a_load_ref_reg(list,paraloc^.size,pushsize,href,tmpreg);
              list.concat(taicpu.op_reg(A_PUSH,TCgsize2opsize[pushsize],tmpreg));
            end
          else
            list.concat(taicpu.op_ref(A_PUSH,TCgsize2opsize[pushsize],href));
        end;

      var
        len : aint;
        href : treference;
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
                reference_reset_base(href,NR_STACK_POINTER_REG,0);
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
          inherited a_param_ref(list,size,r,cgpara);
      end;


    procedure tcg386.a_paramaddr_ref(list : TAsmList;const r : treference;const cgpara : tcgpara);
      var
        tmpreg : tregister;
        opsize : topsize;
      begin
        with r do
          begin
            if (segment<>NR_NO) then
              cgmessage(cg_e_cant_use_far_pointer_there);
            if use_push(cgpara) then
              begin
                cgpara.check_simple_location;
                opsize:=tcgsize2opsize[OS_ADDR];
                if (segment=NR_NO) and (base=NR_NO) and (index=NR_NO) then
                  begin
                    if assigned(symbol) then
                      list.concat(Taicpu.Op_sym_ofs(A_PUSH,opsize,symbol,offset))
                    else
                      list.concat(Taicpu.Op_const(A_PUSH,opsize,offset));
                  end
                else if (segment=NR_NO) and (base=NR_NO) and (index<>NR_NO) and
                        (offset=0) and (scalefactor=0) and (symbol=nil) then
                  list.concat(Taicpu.Op_reg(A_PUSH,opsize,index))
                else if (segment=NR_NO) and (base<>NR_NO) and (index=NR_NO) and
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


    procedure tcg386.g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean);
      var
        stacksize : longint;
      begin
        { Release PIC register }
        if cs_create_pic in current_settings.moduleswitches then
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
                if (target_info.system = system_i386_darwin) and
                   ((stacksize <> 0) or
                    (pi_do_call in current_procinfo.flags) or
                    { can't detect if a call in this case -> use nostackframe }
                    { if you (think you) know what you are doing              }
                    (po_assembler in current_procinfo.procdef.procoptions)) then
                  stacksize := align(stacksize+sizeof(aint),16) - sizeof(aint);
                if (stacksize<>0) then
                  cg.a_op_const_reg(list,OP_ADD,OS_ADDR,stacksize,current_procinfo.framepointer);
              end
            else
              list.concat(Taicpu.op_none(A_LEAVE,S_NO));
            list.concat(tai_regalloc.dealloc(current_procinfo.framepointer,nil));
          end;

        { return from proc }
        if (po_interrupt in current_procinfo.procdef.procoptions) and
           { this messes up stack alignment }
           (target_info.system <> system_i386_darwin) then
          begin
            if (current_procinfo.procdef.funcretloc[calleeside].loc<>LOC_VOID) and
               (current_procinfo.procdef.funcretloc[calleeside].loc=LOC_REGISTER) then
              list.concat(Taicpu.Op_const_reg(A_ADD,S_L,4,NR_ESP))
            else
              list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EAX));
            list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EBX));
            list.concat(Taicpu.Op_reg(A_POP,S_L,NR_ECX));

            if (current_procinfo.procdef.funcretloc[calleeside].loc=LOC_REGISTER) and
               (current_procinfo.procdef.funcretloc[calleeside].size in [OS_64,OS_S64]) then
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
        else if (current_procinfo.procdef.proccalloption in clearstack_pocalls) and
                (not use_fixed_stack)  then
         begin
           { complex return values are removed from stack in C code PM }
           if paramanager.ret_in_param(current_procinfo.procdef.returndef,
                                       current_procinfo.procdef.proccalloption) then
             list.concat(Taicpu.Op_const(A_RET,S_W,sizeof(aint)))
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
           list.concat(Taicpu.Op_const(A_RET,S_W,parasize));
         end;
      end;


    procedure tcg386.g_copyvaluepara_openarray(list : TAsmList;const ref:treference;const lenloc:tlocation;elesize:aint;destreg:tregister);
      var
        power,len  : longint;
        opsize : topsize;
{$ifndef __NOWINPECOFF__}
        again,ok : tasmlabel;
{$endif}
      begin
        if use_fixed_stack then
          begin
            inherited g_copyvaluepara_openarray(list,ref,lenloc,elesize,destreg);
            exit;
          end;

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
             current_asmdata.getjumplabel(again);
             current_asmdata.getjumplabel(ok);
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


    procedure tcg386.g_exception_reason_save(list : TAsmList; const href : treference);
      begin
        if not use_fixed_stack then
          list.concat(Taicpu.op_reg(A_PUSH,tcgsize2opsize[OS_INT],NR_FUNCTION_RESULT_REG))
        else
         inherited g_exception_reason_save(list,href);
      end;


    procedure tcg386.g_exception_reason_save_const(list : TAsmList;const href : treference; a: aint);
      begin
        if not use_fixed_stack then
          list.concat(Taicpu.op_const(A_PUSH,tcgsize2opsize[OS_INT],a))
        else
          inherited g_exception_reason_save_const(list,href,a);
      end;


    procedure tcg386.g_exception_reason_load(list : TAsmList; const href : treference);
      begin
        if not use_fixed_stack then
          list.concat(Taicpu.op_reg(A_POP,tcgsize2opsize[OS_INT],NR_FUNCTION_RESULT_REG))
        else
          inherited g_exception_reason_load(list,href);
      end;



    procedure tcg386.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
      {
      possible calling conventions:
                    default stdcall cdecl pascal register
      default(0):      OK     OK    OK(1)  OK       OK
      virtual(2):      OK     OK    OK(3)  OK       OK

      (0):
          set self parameter to correct value
          jmp mangledname

      (1): The code is the following
           set self parameter to correct value
           call mangledname
           set self parameter to interface value

      (2): The wrapper code use %eax to reach the virtual method address
           set self to correct value
           move self,%eax
           mov  0(%eax),%eax ; load vmt
           jmp  vmtoffs(%eax) ; method offs

      (3): The wrapper code use %eax to reach the virtual method address
           set self to correct value
           move self,%eax
           mov  0(%eax),%eax ; load vmt
           jmp  vmtoffs(%eax) ; method offs
           set self parameter to interface value


      (4): Virtual use values pushed on stack to reach the method address
           so the following code be generated:
           set self to correct value
           push %ebx ; allocate space for function address
           push %eax
           mov  self,%eax
           mov  0(%eax),%eax ; load vmt
           mov  vmtoffs(%eax),eax ; method offs
           mov  %eax,4(%esp)
           pop  %eax
           ret  0; jmp the address

      }

        procedure getselftoeax(offs: longint);
        var
          href : treference;
          selfoffsetfromsp : longint;
        begin
          { mov offset(%esp),%eax }
          if (procdef.proccalloption<>pocall_register) then
            begin
              { framepointer is pushed for nested procs }
              if procdef.parast.symtablelevel>normal_function_level then
                selfoffsetfromsp:=2*sizeof(aint)
              else
                selfoffsetfromsp:=sizeof(aint);
              reference_reset_base(href,NR_ESP,selfoffsetfromsp+offs);
              cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_EAX);
            end;
        end;

        procedure loadvmttoeax;
        var
          href : treference;
        begin
          { mov  0(%eax),%eax ; load vmt}
          reference_reset_base(href,NR_EAX,0);
          cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_EAX);
        end;

        procedure op_oneaxmethodaddr(op: TAsmOp);
        var
          href : treference;
        begin
          if (procdef.extnumber=$ffff) then
            Internalerror(200006139);
          { call/jmp  vmtoffs(%eax) ; method offs }
          reference_reset_base(href,NR_EAX,procdef._class.vmtmethodoffset(procdef.extnumber));
          list.concat(taicpu.op_ref(op,S_L,href));
        end;

        procedure loadmethodoffstoeax;
        var
          href : treference;
        begin
          if (procdef.extnumber=$ffff) then
            Internalerror(200006139);
          { mov vmtoffs(%eax),%eax ; method offs }
          reference_reset_base(href,NR_EAX,procdef._class.vmtmethodoffset(procdef.extnumber));
          cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_EAX);
        end;

      var
        lab : tasmsymbol;
        make_global : boolean;
        href : treference;
      begin
        if not(procdef.proctypeoption in [potype_function,potype_procedure]) then
          Internalerror(200006137);
        if not assigned(procdef._class) or
           (procdef.procoptions*[po_classmethod, po_staticmethod,
             po_methodpointer, po_interrupt, po_iocheck]<>[]) then
          Internalerror(200006138);
        if procdef.owner.symtabletype<>ObjectSymtable then
          Internalerror(200109191);

        make_global:=false;
        if (not current_module.is_unit) or
           (cs_create_smart in current_settings.moduleswitches) or
           (af_smartlink_sections in target_asm.flags) or
           (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
          make_global:=true;

        if make_global then
         List.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
        else
        List.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

        { set param1 interface to self  }
        g_adjust_self_value(list,procdef,ioffset);

        { case 1 or 2 }
        if (procdef.proccalloption in clearstack_pocalls) then
          begin
            if po_virtualmethod in procdef.procoptions then
              begin
                { case 2 }
                getselftoeax(0);
                loadvmttoeax;
                op_oneaxmethodaddr(A_CALL);
              end
            else
              begin
                { case 1 }
                cg.a_call_name(list,procdef.mangledname);
              end;
            { restore param1 value self to interface }
            g_adjust_self_value(list,procdef,-ioffset);
          end
        else if po_virtualmethod in procdef.procoptions then
          begin
            if (procdef.proccalloption=pocall_register) then
              begin
                { case 4 }
                list.concat(taicpu.op_reg(A_PUSH,S_L,NR_EBX)); { allocate space for address}
                list.concat(taicpu.op_reg(A_PUSH,S_L,NR_EAX));
                getselftoeax(8);
                loadvmttoeax;
                loadmethodoffstoeax;
                { mov %eax,4(%esp) }
                reference_reset_base(href,NR_ESP,4);
                list.concat(taicpu.op_reg_ref(A_MOV,S_L,NR_EAX,href));
                { pop  %eax }
                list.concat(taicpu.op_reg(A_POP,S_L,NR_EAX));
                { ret  ; jump to the address }
                list.concat(taicpu.op_none(A_RET,S_L));
              end
            else
              begin
                { case 3 }
                getselftoeax(0);
                loadvmttoeax;
                op_oneaxmethodaddr(A_JMP);
              end;
          end
        { case 0 }
        else
          begin
            if (target_info.system <> system_i386_darwin) then
              begin
                lab:=current_asmdata.RefAsmSymbol(procdef.mangledname);
                list.concat(taicpu.op_sym(A_JMP,S_NO,lab))
              end
            else
              list.concat(taicpu.op_sym(A_JMP,S_NO,get_darwin_call_stub(procdef.mangledname)))
          end;

        List.concat(Tai_symbol_end.Createname(labelname));
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


    procedure tcg64f386.a_op64_ref_reg(list : TAsmList;op:TOpCG;size : tcgsize;const ref : treference;reg : tregister64);
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


    procedure tcg64f386.a_op64_reg_reg(list : TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);
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


    procedure tcg64f386.a_op64_const_reg(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;reg : tregister64);
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


    procedure tcg64f386.a_op64_const_ref(list : TAsmList;op:TOpCG;size : tcgsize;value : int64;const ref : treference);
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
