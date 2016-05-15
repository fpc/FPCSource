{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines to create a pass-through high-level code
    generator. This is used by most regular code generators.

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

unit hlcgcpu;

{$i fpcdefs.inc}

interface

  uses
    symtype,
    aasmdata,
    symdef,
    cgbase,cgutils,
    hlcgobj, hlcg2ll;

  type
    thlcgspc32 = class(thlcg2ll)
      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint); override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    verbose,globtype,fmodule,
    aasmbase,aasmtai,
    symconst,symsym,defutil,
    cpubase,aasmcpu,parabase,
    cgobj,cgcpu;

  procedure thlcgspc32.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    var
      make_global: boolean;
      href: treference;
      hsym: tsym;
      paraloc: pcgparalocation;
      op: tasmop;
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
      if (not current_module.is_unit) or create_smartlink_library or
         (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
        make_global:=true;

      if make_global then
        list.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
      else
        list.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

      { set param1 interface to self  }
      procdef.init_paraloc_info(callerside);
      hsym:=tsym(procdef.parast.Find('self'));
      if not(assigned(hsym) and
        (hsym.typ=paravarsym)) then
        internalerror(2010103101);
      paraloc:=tparavarsym(hsym).paraloc[callerside].location;
      if assigned(paraloc^.next) then
        InternalError(2013020101);

      case paraloc^.loc of
        LOC_REGISTER:
          cg.a_op_const_reg(list,OP_SUB,paraloc^.Size,ioffset,paraloc^.register)
          //tcgspc32(cg).handle_reg_imm12_reg(list,A_SUB,paraloc^.size,paraloc^.register,ioffset,paraloc^.register,NR_IP0,false,true);
        else
          internalerror(2010103102);
      end;

      if (po_virtualmethod in procdef.procoptions) and
          not is_objectpascal_helper(procdef.struct) then
        begin
          if (procdef.extnumber=$ffff) then
            Internalerror(200006139);
          { mov  0(%rdi),%rax ; load vmt}
          reference_reset_base(href,voidpointertype,paraloc^.register,0,sizeof(pint));
          getcpuregister(list,NR_R0);
          a_load_ref_reg(list,voidpointertype,voidpointertype,href,NR_R0);
          { jmp *vmtoffs(%eax) ; method offs }
          reference_reset_base(href,voidpointertype,NR_R0,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),sizeof(pint));
          op:=A_LDW;
          href:=tcgspc32(cg).normalize_ref(list,href);
          list.concat(taicpu.op_reg_ref(op,NR_R0,href));
          ungetcpuregister(list,NR_R0);
          list.concat(taicpu.op_none(A_NUL));
          list.concat(taicpu.op_reg(A_JMP,NR_R0));
        end
      else
        cg.a_jmp_name(list,procdef.mangledname);
      list.concat(Tai_symbol_end.Createname(labelname));
    end;

  procedure create_hlcodegen;
    begin
      hlcg:=thlcgspc32.create;
      create_codegen;
    end;

end.
