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
  symconst,symtype,symdef,globtype;

{ create a nested procdef that will be used to outline code from a procedure;
  astruct should usually be nil, except in special cases like the Windows SEH
  exception handling funclets }
function create_outline_procdef(const basesymname: string; astruct: tabstractrecorddef; potype: tproctypeoption; resultdef: tdef): tprocdef;

procedure convert_to_funcref_intf(const n:tidstring;var def:tdef);
function adjust_funcref(var def:tdef;sym,dummysym:tsym):boolean;

implementation

  uses
    cutils,cclasses,verbose,globals,
    nobj,
    symbase,symsym,symtable,defutil,pparautl;


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
      st.insertsym(sym);

      result.procsym:=sym;
      proc_add_definition(result);
      { the code will be assigned directly to the "code" field later }
      result.forwarddef:=false;
      result.aliasnames.insert(result.mangledname);
    end;


  function fileinfo_to_suffix(const fileinfo:tfileposinfo):tsymstr;inline;
    begin
      result:=tostr(fileinfo.moduleindex)+'_'+
              tostr(fileinfo.fileindex)+'_'+
              tostr(fileinfo.line)+'_'+
              tostr(fileinfo.column);
    end;


  const
    anon_funcref_prefix='$FuncRef_';


  procedure convert_to_funcref_intf(const n:tidstring;var def:tdef);
    var
      oldsymtablestack : tsymtablestack;
      pvdef : tprocvardef absolute def;
      intfdef : tobjectdef;
      invokedef : tprocdef;
      psym : tprocsym;
      sym : tsym;
      st : tsymtable;
      i : longint;
      name : tidstring;
    begin
      if def.typ<>procvardef then
        internalerror(2021040201);
      if not (po_is_function_ref in tprocvardef(pvdef).procoptions) then
        internalerror(2021022101);
      if n='' then
        name:=anon_funcref_prefix+fileinfo_to_suffix(current_filepos)
      else
        name:=n;
      intfdef:=cobjectdef.create(odt_interfacecom,name,interface_iunknown,true);
      include(intfdef.objectoptions,oo_is_funcref);
      include(intfdef.objectoptions,oo_is_invokable);
      include(intfdef.objectoptions,oo_has_virtual);
      intfdef.typesym:=pvdef.typesym;
      pvdef.typesym:=nil;

      if cs_generate_rtti in current_settings.localswitches then
        include(intfdef.objectoptions,oo_can_have_published);

      oldsymtablestack:=symtablestack;
      symtablestack:=nil;

      invokedef:=tprocdef(pvdef.getcopyas(procdef,pc_normal_no_paras,'',false));
      invokedef.struct:=intfdef;
      invokedef.forwarddef:=false;

      include(invokedef.procoptions,po_overload);
      include(invokedef.procoptions,po_virtualmethod);

      invokedef.procsym:=cprocsym.create(method_name_funcref_invoke_decl);
      if cs_generate_rtti in current_settings.localswitches then
        invokedef.visibility:=vis_published
      else
        invokedef.visibility:=vis_public;

      intfdef.symtable.insertsym(invokedef.procsym);
      intfdef.symtable.insertdef(invokedef);

      if pvdef.is_generic or pvdef.is_specialization then
        begin
          if assigned(pvdef.genericdef) and (pvdef.genericdef.typ<>objectdef) then
            internalerror(2021040501);
          intfdef.genericdef:=pvdef.genericdef;
          intfdef.defoptions:=intfdef.defoptions+(pvdef.defoptions*[df_generic,df_specialization]);
          { in case of a generic we move all involved syms/defs to the interface }
          intfdef.genericparas:=pvdef.genericparas;
          pvdef.genericparas:=nil;
          for i:=0 to intfdef.genericparas.count-1 do
            begin
              sym:=tsym(intfdef.genericparas[i]);
              if sym.owner<>pvdef.parast then
                continue;
              sym.changeowner(intfdef.symtable);
              if (sym.typ=typesym) and (ttypesym(sym).typedef.owner=pvdef.parast) then
                ttypesym(sym).typedef.changeowner(intfdef.symtable);
            end;
        end;

      { now move the symtable over }
      invokedef.parast.free;
      invokedef.parast:=pvdef.parast;
      invokedef.parast.defowner:=invokedef;
      pvdef.parast:=nil;

      for i:=0 to invokedef.parast.symlist.count-1 do
        begin
          sym:=tsym(invokedef.parast.symlist[i]);
          if sym.typ<>paravarsym then
            continue;
          if tparavarsym(sym).vardef=pvdef then
            tparavarsym(sym).vardef:=intfdef;
        end;

      symtablestack:=oldsymtablestack;

      if invokedef.returndef=pvdef then
        invokedef.returndef:=intfdef;

      handle_calling_convention(invokedef,hcc_default_actions_intf_struct);
      proc_add_definition(invokedef);
      invokedef.calcparas;
      { def is not owned, so it can be simply freed }
      def.free;
      def:=intfdef;
    end;


  function adjust_funcref(var def:tdef;sym,dummysym:tsym):boolean;
    var
      sympos : tfileposinfo;
      name : string;
    begin
      result:=false;
      if (def.typ<>procvardef) and not is_funcref(def) then
        internalerror(2022020401);
      if assigned(sym) and not (sym.typ=typesym) then
        internalerror(2022020402);
      { these always support everything, no "of object" or
        "is_nested" is allowed }
      if is_nested_pd(tprocvardef(def)) or
         is_methodpointer(def) then
        cgmessage(type_e_function_reference_kind);
      if not (po_is_block in tprocvardef(def).procoptions) then
        begin
          if assigned(dummysym) then
            ttypesym(dummysym).typedef:=nil;
          if assigned(sym) then
            begin
              ttypesym(sym).typedef:=nil;
              name:=sym.name;
            end
          else
            name:='';
          convert_to_funcref_intf(name,def);
          if assigned(sym) then
            ttypesym(sym).typedef:=def;
          if assigned(dummysym) then
            ttypesym(dummysym).typedef:=def;
          build_vmt(tobjectdef(def));
          result:=true;
        end
      else
        begin
          if assigned(sym) and (sym.refs>0) then
            begin
              { find where the symbol was used and trigger
                a "symbol not completely defined" error }
              if not fileinfo_of_typesym_in_def(def,sym,sympos) then
                sympos:=sym.fileinfo;
              messagepos1(sympos,type_e_type_is_not_completly_defined,sym.realname);
            end;
        end;
    end;


end.

