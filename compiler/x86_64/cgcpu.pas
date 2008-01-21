{
    Copyright (c) 2002 by Florian Klaempfl

    This unit implements the code generator for the x86-64.

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
       cgbase,cgutils,cgobj,cgx86,
       aasmbase,aasmtai,aasmdata,aasmcpu,
       cpubase,cpuinfo,cpupara,parabase,
       symdef,
       node,symconst,rgx86,procinfo;

    type
      tcgx86_64 = class(tcgx86)
        procedure init_register_allocators;override;
        procedure done_register_allocators;override;

        procedure g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean);override;
        procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;

        procedure a_param_ref(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);override;
      end;


  implementation

    uses
       globtype,globals,verbose,systems,cutils,
       symsym,defutil,paramgr,fmodule,
       rgobj,tgobj,rgcpu;


    procedure Tcgx86_64.init_register_allocators;
      const
        win64_saved_std_regs : array[0..6] of tsuperregister = (RS_RBX,RS_RDI,RS_RSI,RS_R12,RS_R13,RS_R14,RS_R15);
        others_saved_std_regs : array[0..4] of tsuperregister = (RS_RBX,RS_R12,RS_R13,RS_R14,RS_R15);

        win64_saved_xmm_regs : array[0..9] of tsuperregister = (RS_XMM6,RS_XMM7,
          RS_XMM8,RS_XMM9,RS_XMM10,RS_XMM11,RS_XMM12,RS_XMM13,RS_XMM14,RS_XMM15);
      var
        i : longint;
        framepointer : tsuperregister;
      begin
        inherited init_register_allocators;
        if target_info.system=system_x86_64_win64 then
          begin
            SetLength(saved_standard_registers,Length(win64_saved_std_regs));
            SetLength(saved_mm_registers,Length(win64_saved_xmm_regs));

            for i:=low(win64_saved_std_regs) to high(win64_saved_std_regs) do
              saved_standard_registers[i]:=win64_saved_std_regs[i];

            for i:=low(win64_saved_xmm_regs) to high(win64_saved_xmm_regs) do
              saved_mm_registers[i]:=win64_saved_xmm_regs[i];
          end
        else
          begin
            SetLength(saved_standard_registers,Length(others_saved_std_regs));
            SetLength(saved_mm_registers,0);

            for i:=low(others_saved_std_regs) to high(others_saved_std_regs) do
              saved_standard_registers[i]:=others_saved_std_regs[i];
          end;
        if assigned(current_procinfo) then
          framepointer:=getsupreg(current_procinfo.framepointer)
        else
          { in intf. wrapper code generation }
          framepointer:=RS_FRAME_POINTER_REG;
        rg[R_INTREGISTER]:=trgcpu.create(R_INTREGISTER,R_SUBWHOLE,[RS_RAX,RS_RDX,RS_RCX,RS_RBX,RS_RSI,RS_RDI,
          RS_R8,RS_R9,RS_R10,RS_R11,RS_R12,RS_R13,RS_R14,RS_R15],first_int_imreg,[framepointer]);
        rg[R_MMREGISTER]:=trgcpu.create(R_MMREGISTER,R_SUBWHOLE,[RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7,
          RS_XMM8,RS_XMM9,RS_XMM10,RS_XMM11,RS_XMM12,RS_XMM13,RS_XMM14,RS_XMM15],first_mm_imreg,[]);
        rgfpu:=Trgx86fpu.create;
      end;


    procedure Tcgx86_64.done_register_allocators;
      begin
        inherited done_register_allocators;
        setlength(saved_standard_registers,0);
        setlength(saved_mm_registers,0);
      end;


    procedure tcgx86_64.a_param_ref(list : TAsmList;size : tcgsize;const r : treference;const paraloc : TCGPara);
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
            case location^.loc of
              LOC_REGISTER,LOC_CREGISTER:
                a_load_ref_reg(list,location^.size,location^.size,tmpref,location^.register);
              LOC_REFERENCE:
                begin
                  reference_reset_base(ref,location^.reference.index,location^.reference.offset);
                  g_concatcopy(list,tmpref,ref,sizeleft);
                end;
              else
                internalerror(2002081103);
            end;
            inc(tmpref.offset,tcgsize2size[location^.size]);
            dec(sizeleft,tcgsize2size[location^.size]);
            location := location^.next;
          end;
      end;


    procedure tcgx86_64.g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean);
      var
        stacksize : longint;
      begin
        { Release PIC register }
        if cs_create_pic in current_settings.moduleswitches then
          list.concat(tai_regalloc.dealloc(NR_PIC_OFFSET_REG,nil));

        { remove stackframe }
        if not nostackframe then
          begin
            if (current_procinfo.framepointer=NR_STACK_POINTER_REG) then
              begin
                stacksize:=current_procinfo.calc_stackframe_size;
                if (target_info.system in [system_x86_64_win64,system_x86_64_linux,system_x86_64_freebsd]) and
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
            list.concat(tai_regalloc.dealloc(NR_FRAME_POINTER_REG,nil));
          end;

        list.concat(Taicpu.Op_none(A_RET,S_NO));
      end;


    procedure tcgx86_64.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
      var
        make_global : boolean;
        href : treference;
        sym : tasmsymbol;
        r : treference;
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
           (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
          make_global:=true;

        if make_global then
          List.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
        else
          List.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

        { set param1 interface to self  }
        g_adjust_self_value(list,procdef,ioffset);

        if po_virtualmethod in procdef.procoptions then
          begin
            if (procdef.extnumber=$ffff) then
              Internalerror(200006139);
            { load vmt from first paramter }
            { win64 uses a different abi }
            if target_info.system=system_x86_64_win64 then
              reference_reset_base(href,NR_RCX,0)
            else
              reference_reset_base(href,NR_RDI,0);
            cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_RAX);
            { jmp *vmtoffs(%eax) ; method offs }
            reference_reset_base(href,NR_RAX,procdef._class.vmtmethodoffset(procdef.extnumber));
            list.concat(taicpu.op_ref_reg(A_MOV,S_Q,href,NR_RAX));
            list.concat(taicpu.op_reg(A_JMP,S_Q,NR_RAX));
          end
        else
          begin
            sym:=current_asmdata.RefAsmSymbol(procdef.mangledname);
            reference_reset_symbol(r,sym,0);
            if cs_create_pic in current_settings.moduleswitches then
              r.refaddr:=addr_pic
            else
              r.refaddr:=addr_full;

            list.concat(taicpu.op_ref(A_JMP,S_NO,r));
          end;

        List.concat(Tai_symbol_end.Createname(labelname));
      end;


begin
  cg:=tcgx86_64.create;
{$ifndef cpu64bit}
  cg64:=tcg64f64.create;
{$endif cpu64bit}
end.
