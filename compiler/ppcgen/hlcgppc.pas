{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines high-level code generator support shared by
    ppc32 and ppc64

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
unit hlcgppc;

{$i fpcdefs.inc}

interface

uses
  globtype,globals,
  aasmdata,
  symtype,symdef,
  cgbase,cgutils,hlcgobj,hlcg2ll;

type
  thlcgppcgen = class(thlcg2ll)
   protected
    procedure a_load_subsetref_regs_noindex(list: TAsmList; subsetsize: tdef; loadbitsize: byte; const sref: tsubsetreference; valuereg, extra_value_reg: tregister); override;
   public
    procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
    procedure a_jmp_external_name(list: TAsmList; const externalname: TSymStr); override;
    procedure gen_load_para_value(list: TAsmList); override;
  end;

implementation

  uses
    verbose,
    systems,fmodule,
    symconst,
    aasmbase,aasmtai,aasmcpu,
    cpubase,
    procinfo,cpupi,cgobj,cgppc,
    defutil;

{ thlcgppc }

  procedure thlcgppcgen.a_load_subsetref_regs_noindex(list: TAsmList; subsetsize: tdef; loadbitsize: byte; const sref: tsubsetreference; valuereg, extra_value_reg: tregister);
    var
      fromsreg, tosreg: tsubsetregister;
      restbits: byte;
    begin
      { the code below is only valid for big endian }
      if target_info.endian=endian_little then
        begin
         inherited;
         exit
        end;
      restbits:=(sref.bitlen-(loadbitsize-sref.startbit));
      if is_signed(subsetsize) then
        begin
         { sign extend }
         a_op_const_reg(list,OP_SHL,osuinttype,AIntBits-loadbitsize+sref.startbit,valuereg);
         a_op_const_reg(list,OP_SAR,osuinttype,AIntBits-sref.bitlen,valuereg);
        end
      else
        begin
          a_op_const_reg(list,OP_SHL,osuinttype,restbits,valuereg);
          { mask other bits }
          if (sref.bitlen<>AIntBits) then
            a_op_const_reg(list,OP_AND,osuinttype,(aword(1) shl sref.bitlen)-1,valuereg);
        end;
      { use subsetreg routine, it may have been overridden with an optimized version }
      fromsreg.subsetreg:=extra_value_reg;
      fromsreg.subsetregsize:=OS_INT;
      { subsetregs always count bits from right to left }
      fromsreg.startbit:=loadbitsize-restbits;
      fromsreg.bitlen:=restbits;

      tosreg.subsetreg:=valuereg;
      tosreg.subsetregsize:=OS_INT;
      tosreg.startbit:=0;
      tosreg.bitlen:=restbits;

      a_load_subsetreg_subsetreg(list,subsetsize,subsetsize,fromsreg,tosreg);
    end;


  procedure thlcgppcgen.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);

      procedure loadvmttor11;
      var
        href : treference;
      begin
        reference_reset_base(href,voidpointertype,NR_R3,0,ctempposinvalid,sizeof(pint),[]);
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R11);
      end;


      procedure op_onr11methodaddr;
      var
        href : treference;
      begin
        if (procdef.extnumber=$ffff) then
          Internalerror(200006139);
        { call/jmp  vmtoffs(%eax) ; method offs }
        reference_reset_base(href,voidpointertype,NR_R11,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),ctempposinvalid,sizeof(pint),[]);
        if tcgppcgen(cg).hasLargeOffset(href) then
          begin
{$ifdef cpu64}
            if (longint(href.offset) <> href.offset) then
              { add support for offsets > 32 bit }
              internalerror(200510201);
{$endif cpu64}
            list.concat(taicpu.op_reg_reg_const(A_ADDIS,NR_R11,NR_R11,
              smallint((href.offset shr 16)+ord(smallint(href.offset and $ffff) < 0))));
            href.offset := smallint(href.offset and $ffff);
          end;
        { use R12 for dispatch because most ABIs don't care and ELFv2
          requires it }
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R12);
        if (target_info.system in systems_aix) or
           ((target_info.system = system_powerpc64_linux) and
            (target_info.abi=abi_powerpc_sysv)) then
          begin
            reference_reset_base(href, voidpointertype, NR_R12, 0, ctempposinvalid, sizeof(pint),[]);
            cg.a_load_ref_reg(list, OS_ADDR, OS_ADDR, href, NR_R12);
          end;
        list.concat(taicpu.op_reg(A_MTCTR,NR_R12));
        list.concat(taicpu.op_none(A_BCTR));
        if (target_info.system in ([system_powerpc64_linux]+systems_aix)) then
          list.concat(taicpu.op_none(A_NOP));
      end;


    var
      make_global : boolean;
    begin
      if not(procdef.proctypeoption in [potype_function,potype_procedure]) then
        Internalerror(200006137);
      if not assigned(procdef.struct) or
         (procdef.procoptions*[po_classmethod, po_staticmethod,
           po_methodpointer, po_interrupt, po_iocheck]<>[]) then
        Internalerror(200006138);
      if procdef.owner.symtabletype<>ObjectSymtable then
        Internalerror(200109191);

      make_global:=false;
      if (not current_module.is_unit) or
          create_smartlink or
         (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
        make_global:=true;

      if make_global then
        List.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0,procdef))
      else
        List.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0,procdef));

      { set param1 interface to self  }
      g_adjust_self_value(list,procdef,ioffset);

      { case 4 }
      if (po_virtualmethod in procdef.procoptions) and
          not is_objectpascal_helper(procdef.struct) then
        begin
          loadvmttor11;
          op_onr11methodaddr;
        end
      { case 0 }
      else
        case target_info.system of
          system_powerpc_darwin,
          system_powerpc64_darwin:
            list.concat(taicpu.op_sym(A_B,tcgppcgen(cg).get_darwin_call_stub(procdef.mangledname,false)));
          else if use_dotted_functions then
            {$note ts:todo add GOT change?? - think not needed :) }
            list.concat(taicpu.op_sym(A_B,current_asmdata.RefAsmSymbol('.' + procdef.mangledname,AT_FUNCTION)))
          else
            list.concat(taicpu.op_sym(A_B,current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION)))
        end;
      List.concat(Tai_symbol_end.Createname(labelname));
    end;


  procedure thlcgppcgen.a_jmp_external_name(list: TAsmList; const externalname: TSymStr);
    var
      href : treference;
    begin
      if not(target_info.system in ([system_powerpc64_linux]+systems_aix)) then begin
        inherited;
        exit;
      end;

      { for ppc64/linux and aix emit correct code which sets up a stack frame
        and then calls the external method normally to ensure that the GOT/TOC
        will be loaded correctly if required.

      The resulting code sequence looks as follows:

      mflr r0
      stw/d r0, 16(r1)
      stw/du r1, -112(r1)
      bl <external_method>
      nop
      addi r1, r1, 112
      lwz/d r0, 16(r1)
      mtlr r0
      blr

      }
      list.concat(taicpu.op_reg(A_MFLR, NR_R0));
      if target_info.abi=abi_powerpc_sysv then
        reference_reset_base(href, voidstackpointertype, NR_STACK_POINTER_REG, LA_LR_SYSV, ctempposinvalid, 8, [])
      else
        reference_reset_base(href, voidstackpointertype, NR_STACK_POINTER_REG, LA_LR_AIX, ctempposinvalid, 8, []);
      cg.a_load_reg_ref(list,OS_ADDR,OS_ADDR,NR_R0,href);
      reference_reset_base(href, voidstackpointertype, NR_STACK_POINTER_REG, -MINIMUM_STACKFRAME_SIZE, ctempposinvalid, 8, []);
      list.concat(taicpu.op_reg_ref({$ifdef cpu64bitaddr}A_STDU{$else}A_STWU{$endif}, NR_STACK_POINTER_REG, href));

      cg.a_call_name(list,externalname,false);

      list.concat(taicpu.op_reg_reg_const(A_ADDI, NR_STACK_POINTER_REG, NR_STACK_POINTER_REG, MINIMUM_STACKFRAME_SIZE));


      if target_info.abi=abi_powerpc_sysv then
        reference_reset_base(href, voidstackpointertype, NR_STACK_POINTER_REG, LA_LR_SYSV, ctempposinvalid, 8, [])
      else
        reference_reset_base(href, voidstackpointertype, NR_STACK_POINTER_REG, LA_LR_AIX, ctempposinvalid, 8, []);
      cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R0);
      list.concat(taicpu.op_reg(A_MTLR, NR_R0));
      list.concat(taicpu.op_none(A_BLR));
    end;


  procedure thlcgppcgen.gen_load_para_value(list: TAsmList);
    begin
      { get the register that contains the stack pointer before the procedure
        entry, which is used to access the parameters in their original
        callee-side location }
      if (tcpuprocinfo(current_procinfo).needs_frame_pointer) then
        getcpuregister(list,NR_OLD_STACK_POINTER_REG);
      inherited;
      { free it again }
      if (tcpuprocinfo(current_procinfo).needs_frame_pointer) then
        ungetcpuregister(list,NR_OLD_STACK_POINTER_REG);
    end;

end.

