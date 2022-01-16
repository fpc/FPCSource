{
    Copyright (c) 2018 by Jonas Maebe

    This unit provides helpers for creating procdefs

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
{$i fpcdefs.inc}
unit procdefutil;

interface

uses
  symconst,symtype,symdef;

{ create a nested procdef that will be used to outline code from a procedure;
  astruct should usually be nil, except in special cases like the Windows SEH
  exception handling funclets }
function create_outline_procdef(const basesymname: string; astruct: tabstractrecorddef; potype: tproctypeoption; resultdef: tdef): tprocdef;

implementation

  uses
    cutils,
    symbase,symsym,symtable,pparautl,globtype;


  function create_outline_procdef(const basesymname: string; astruct: tabstractrecorddef; potype: tproctypeoption; resultdef: tdef): tprocdef;
    var
      st:TSymTable;
      checkstack: psymtablestackitem;
      oldsymtablestack: tsymtablestack;
      sym:tprocsym;
    begin
      { get actual procedure symtable (skip withsymtables, etc.) }
      st:=nil;
      checkstack:=symtablestack.stack;
      while assigned(checkstack) do
        begin
          st:=checkstack^.symtable;
          if st.symtabletype in [staticsymtable,globalsymtable,localsymtable] then
            break;
          checkstack:=checkstack^.next;
        end;
      { Create a nested procedure, even from main_program_level.
        Furthermore, force procdef and procsym into the same symtable
        (by default, defs are registered with symtablestack.top which may be
        something temporary like exceptsymtable - in that case, procdef can be
        destroyed before procsym, leaving invalid pointers). }
      oldsymtablestack:=symtablestack;
      symtablestack:=nil;
      result:=cprocdef.create(max(normal_function_level,st.symtablelevel)+1,true);
      result.returndef:=resultdef;
      { if the parent is a generic or a specialization, the new function is one
        as well }
      if st.symtabletype=localsymtable then
        result.defoptions:=result.defoptions+(tstoreddef(st.defowner).defoptions*[df_generic,df_specialization]);
      symtablestack:=oldsymtablestack;
      st.insertdef(result);
      result.struct:=astruct;
      { tabstractprocdef constructor sets po_delphi_nested_cc whenever
        nested procvars modeswitch is active. We must be independent of this switch. }
      exclude(result.procoptions,po_delphi_nested_cc);
      result.proctypeoption:=potype;
      { always use the default calling convention }
      result.proccalloption:=pocall_default;
      include(result.procoptions,po_hascallingconvention);
      handle_calling_convention(result,hcc_default_actions_impl);
      sym:=cprocsym.create(basesymname+result.unique_id_str);
      st.insert(sym);

      result.procsym:=sym;
      proc_add_definition(result);
      { the code will be assigned directly to the "code" field later }
      result.forwarddef:=false;
      result.aliasnames.insert(result.mangledname);
    end;


end.

