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
    end;

implementation

  uses
    verbose,
    cpubase,
    symconst,
    aasmbase,aasmtai,aasmcpu,
    procinfo,
    fmodule,
    hlcgobj,
    cgcpu;

  procedure thlcgcpu.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    var
      make_global: boolean;
    begin
      if not(procdef.proctypeoption in [potype_function,potype_procedure]) then
        Internalerror(2024091401);
      if not assigned(procdef.struct) or
         (procdef.procoptions*[po_classmethod, po_staticmethod,
           po_methodpointer, po_interrupt, po_iocheck]<>[]) then
        Internalerror(2024091402);
      if procdef.owner.symtabletype<>ObjectSymtable then
        Internalerror(2024091403);

      make_global:=false;
      if (not current_module.is_unit) or
         create_smartlink or
         (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
        make_global:=true;

      if make_global then
        list.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0,procdef))
      else
        list.concat(Tai_symbol.Createname_hidden(labelname,AT_FUNCTION,0,procdef));

      { the wrapper might need aktlocaldata for the additional data to
        load the constant }
      current_procinfo:=cprocinfo.create(nil);

      //{ set param1 interface to self  }
      //g_adjust_self_value(list,procdef,ioffset);
      //
      //{ case 4 }
      //if (po_virtualmethod in procdef.procoptions) and
      //    not is_objectpascal_helper(procdef.struct) then
      //  begin
      //    loadvmttor12;
      //    op_onr12methodaddr;
      //  end
      //{ case 0 }
      //else if GenerateThumbCode then
      //  begin
      //    { bl cannot be used here because it destroys lr }
      //
      //    list.concat(taicpu.op_regset(A_PUSH,R_INTREGISTER,R_SUBWHOLE,[RS_R0]));
      //
      //    { create consts entry }
      //    reference_reset(tmpref,4,[]);
      //    current_asmdata.getjumplabel(l);
      //    current_procinfo.aktlocaldata.Concat(tai_align.Create(4));
      //    cg.a_label(current_procinfo.aktlocaldata,l);
      //    tmpref.symboldata:=current_procinfo.aktlocaldata.last;
      //    current_procinfo.aktlocaldata.concat(tai_const.Create_sym(current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION)));
      //
      //    tmpref.symbol:=l;
      //    tmpref.base:=NR_PC;
      //    cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,tmpref,NR_R0);
      //    list.concat(taicpu.op_reg_reg(A_MOV,NR_R12,NR_R0));
      //    list.concat(taicpu.op_regset(A_POP,R_INTREGISTER,R_SUBWHOLE,[RS_R0]));
      //    list.concat(taicpu.op_reg(A_BX,NR_R12));
      //  end
      //else
      //  list.concat(taicpu.op_sym(A_B,current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION)));
      list.concatlist(current_procinfo.aktlocaldata);

      { dummy so far }
      list.concat(taicpu.op_none(A_RET));

      current_procinfo.Free;
      current_procinfo:=nil;

      list.concat(Tai_symbol_end.Createname(labelname));
    end;


  procedure create_hlcodegen_cpu;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;

begin
  chlcgobj:=thlcgcpu;
  create_hlcodegen:=@create_hlcodegen_cpu;
end.
