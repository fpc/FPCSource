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
    aasmdata,
    symdef,
    hlcg2ll;

  type
    thlcgcpu = class(thlcg2ll)
     procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
     procedure g_external_wrapper(list : TAsmList; procdef: tprocdef; const externalname: string);override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    verbose,globtype,fmodule,
    aasmbase,aasmtai,aasmcpu,
    parabase,
    symconst,symtype,symsym,
    cgbase,cgutils,cgobj,hlcgobj,cpubase,cgcpu;


  procedure thlcgcpu.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    var
      make_global : boolean;
      href : treference;
      hsym : tsym;
      paraloc : pcgparalocation;
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
      if (not current_module.is_unit) or create_smartlink or
         (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
        make_global:=true;

      if make_global then
        List.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
      else
        List.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

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
          begin
            if ((ioffset>=simm13lo) and (ioffset<=simm13hi)) then
              cg.a_op_const_reg(list,OP_SUB,paraloc^.size,ioffset,paraloc^.register)
            else
              begin
                cg.a_load_const_reg(list,paraloc^.size,ioffset,NR_G1);
                cg.a_op_reg_reg(list,OP_SUB,paraloc^.size,NR_G1,paraloc^.register);
              end;
          end;
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
          cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_G1);
          { jmp *vmtoffs(%eax) ; method offs }
          reference_reset_base(href,voidpointertype,NR_G1,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),sizeof(pint));
          list.concat(taicpu.op_ref_reg(A_LD,href,NR_G1));
          list.concat(taicpu.op_reg(A_JMP,NR_G1));
          { Delay slot }
          list.Concat(TAiCpu.Op_none(A_NOP));
        end
      else
        g_external_wrapper(list,procdef,procdef.mangledname);
      List.concat(Tai_symbol_end.Createname(labelname));
    end;


  procedure thlcgcpu.g_external_wrapper(list : TAsmList; procdef: tprocdef; const externalname: string);
    begin
      { CALL overwrites %o7 with its own address, we use delay slot to restore it. }
      list.concat(taicpu.op_reg_reg(A_MOV,NR_O7,NR_G1));
      list.concat(taicpu.op_sym(A_CALL,current_asmdata.RefAsmSymbol(externalname)));
      list.concat(taicpu.op_reg_reg(A_MOV,NR_G1,NR_O7));
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;

begin
  chlcgobj:=thlcgcpu;
end.
