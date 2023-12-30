{
    Copyright (c) 2018 by Jonas Maebe
    Copyright (c) 2011-2021 by Blaise.ru

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
  globtype,procinfo,
  symconst,symtype,symdef,
  node,nbas;

{ create a nested procdef that will be used to outline code from a procedure;
  astruct should usually be nil, except in special cases like the Windows SEH
  exception handling funclets }
function create_outline_procdef(const basesymname: string; astruct: tabstractrecorddef; potype: tproctypeoption; resultdef: tdef): tprocdef;

procedure convert_to_funcref_intf(const n:tidstring;var def:tdef);
function adjust_funcref(var def:tdef;sym,dummysym:tsym):boolean;

{ functionality related to capturing local variables for anonymous functions }

function get_or_create_capturer(pd:tprocdef):tsym;
function capturer_add_anonymous_proc(owner:tprocinfo;pd:tprocdef;out capturer:tsym):tobjectdef;
function capturer_add_procvar_or_proc(owner:tprocinfo;n:tnode;out capturer:tsym;out capturen:tnode):tobjectdef;
procedure initialize_capturer(ctx:tprocinfo;var stmt:tstatementnode);
procedure postprocess_capturer(ctx:tprocinfo);
procedure convert_captured_syms(pd:tprocdef;tree:tnode);

implementation

  uses
    cutils,cclasses,verbose,globals,
    fmodule,
    pass_1,
    nobj,ncal,nmem,nld,nutils,
    ngenutil,
    symbase,symsym,symtable,defutil,defcmp,
    htypechk,
    pparautl,psub;


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
    capturer_class_name='$CapturerClass';
    { the leading $ is only added when registering the var symbol }
    capturer_var_name='Capturer';
    keepalive_suffix='_keepalive';
    outer_self_field_name='OuterSelf';


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

      intfdef.defoptions:=intfdef.defoptions+pvdef.defoptions*[df_generic,df_specialization];
      { also inherit the general flags from the surrounding structured type or
        function }
      if assigned(current_structdef) then
        begin
          intfdef.defoptions:=intfdef.defoptions+current_structdef.defoptions*[df_generic,df_specialization];
        end
      else if assigned(current_procinfo) then
        begin
          intfdef.defoptions:=intfdef.defoptions+current_procinfo.procdef.defoptions*[df_generic,df_specialization];
        end;

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

      { we need to do this even if the def isn't a generic/specialization itself,
        but *belongs* to one }
      if intfdef.defoptions*[df_generic,df_specialization]<>[] then
        begin
          if assigned(pvdef.genericdef) and (pvdef.genericdef.typ<>objectdef) then
            internalerror(2021040501);
          intfdef.genericdef:=pvdef.genericdef;
          { in case of a generic we move all involved syms/defs to the interface }
          intfdef.genericparas:=pvdef.genericparas;
          pvdef.genericparas:=nil;
          if assigned(intfdef.genericparas) then
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


  function funcref_intf_for_proc(pd:tabstractprocdef;const suffix:string):tobjectdef;
    var
      name : tsymstr;
      sym : tsym;
      symowner : tsymtable;
      oldsymtablestack: TSymtablestack;
      invokedef: tprocdef;
    begin
      if pd.is_generic then
        internalerror(2022010710);

      name:='funcrefintf_'+suffix;
      if pd.owner.symtabletype=globalsymtable then
        symowner:=current_module.localsymtable
      else
        symowner:=pd.owner;
      sym:=tsym(symowner.find(name));
      if assigned(sym) then
        begin
          if sym.typ<>typesym then
            internalerror(2022010708);
          if not is_funcref(ttypesym(sym).typedef) then
            internalerror(2022010709);
          result:=tobjectdef(ttypesym(sym).typedef);
          exit;
        end;

      name:='$'+name;

      result:=cobjectdef.create(odt_interfacecom,name,interface_iunknown,false);
      include(result.objectoptions,oo_is_funcref);
      include(result.objectoptions,oo_is_invokable);

      sym:=ctypesym.create(name,result);

      oldsymtablestack:=symtablestack;
      symtablestack:=nil;

      invokedef:=tprocdef(pd.getcopyas(procdef,pc_normal_no_hidden,'',false));
      invokedef.struct:=result;
      invokedef.visibility:=vis_public;
      invokedef.procsym:=cprocsym.create(method_name_funcref_invoke_decl);
      invokedef.parast.symtablelevel:=normal_function_level;
      invokedef.localst.symtablelevel:=normal_function_level;
      include(invokedef.procoptions,po_virtualmethod);
      exclude(invokedef.procoptions,po_staticmethod);
      exclude(invokedef.procoptions,po_classmethod);
      invokedef.forwarddef:=false;

      symtablestack:=oldsymtablestack;

      result.symtable.insertsym(invokedef.procsym);
      result.symtable.insertdef(invokedef);

      proc_add_definition(invokedef);
      invokedef.calcparas;
      include(result.objectoptions,oo_has_virtual);

      symowner.insertsym(sym);
      symowner.insertdef(result);
      addsymref(sym);

      build_vmt(result);
    end;


  {.$define DEBUG_CAPTURER}


  function get_capturer(pd:tprocdef):tabstractvarsym;

    function getsym(st:tsymtable;typ:tsymtyp):tabstractvarsym;
      begin
        result:=tabstractvarsym(st.find(capturer_var_name));
        if not assigned(result) then
          internalerror(2022010703);
        if result.typ<>typ then
          internalerror(2022010704);
        if not is_class(result.vardef) then
          internalerror(2022010705);
      end;

    begin
      case pd.proctypeoption of
        potype_unitfinalize,
        potype_unitinit,
        potype_proginit:
          begin
            if not assigned(pd.owner) then
              internalerror(2022052401);
            if pd.owner.symtabletype<>staticsymtable then
              internalerror(2022052402);
            result:=getsym(pd.owner,staticvarsym);
          end;
        else
          begin
            if not assigned(pd.localst) then
              internalerror(2022020502);
            result:=getsym(pd.localst,localvarsym);
          end;
      end;
    end;


  function get_capturer_alive(pd:tprocdef):tabstractvarsym;

    function getsym(st:tsymtable;typ:tsymtyp):tabstractvarsym;
      begin
        result:=tabstractvarsym(st.find(capturer_var_name+keepalive_suffix));
        if not assigned(result) then
          internalerror(2022051703);
        if result.typ<>typ then
          internalerror(2022051704);
        if not is_interfacecom(result.vardef) then
          internalerror(2022051705);
      end;

    begin
      case pd.proctypeoption of
        potype_unitfinalize,
        potype_unitinit,
        potype_proginit:
          begin
            if not assigned(pd.owner) then
              internalerror(2022052403);
            if pd.owner.symtabletype<>staticsymtable then
              internalerror(2022052404);
            result:=getsym(pd.owner,staticvarsym);
          end;
        else
          begin
            if not assigned(pd.localst) then
              internalerror(2022051702);
            result:=getsym(pd.localst,localvarsym);
          end;
      end;
    end;


  function get_or_create_capturer(pd:tprocdef):tsym;
    var
      name : tsymstr;
      parent,
      def : tobjectdef;
      typesym : tsym;
      keepalive : tabstractvarsym;
      intfimpl : TImplementedInterface;
      st : tsymtable;
    begin
      if pd.has_capturer then
        begin
          result:=get_capturer(pd);
        end
      else
        begin
          parent:=tobjectdef(search_system_type('TINTERFACEDOBJECT').typedef);
          if not is_class(parent) then
            internalerror(2022010706);

          name:=capturer_class_name+'_'+fileinfo_to_suffix(pd.fileinfo);

          case pd.proctypeoption of
            potype_unitfinalize,
            potype_unitinit,
            potype_proginit:
              st:=pd.owner;
            else
              st:=pd.localst;
          end;

          def:=cobjectdef.create(odt_class,name,parent,false);
          include(def.objectoptions,oo_is_capturer);
          typesym:=ctypesym.create(name,def);
          typesym.fileinfo:=pd.fileinfo;
          st.insertdef(def);
          st.insertsym(typesym);
          addsymref(typesym);

          if df_generic in pd.defoptions then
            include(def.defoptions,df_generic);
          { don't set df_specialization as in that case genericdef needs to be
            set, but the local symtables are freed once a unit is finished }
          {if df_specialization in pd.defoptions then
            begin
              if not assigned(pd.genericdef) or (pd.genericdef.typ<>procdef) then
                internalerror(2022020501);
              def.genericdef:=tstoreddef(get_capturer(tprocdef(pd.genericdef)).vardef);
              include(def.defoptions,df_specialization);
            end;}

          if st.symtabletype=localsymtable then
            result:=clocalvarsym.create('$'+capturer_var_name,vs_value,def,[vo_is_internal])
          else
            result:=cstaticvarsym.create('$'+capturer_var_name,vs_value,def,[vo_is_internal]);
          result.fileinfo:=pd.fileinfo;
          st.insertsym(result);
          addsymref(result);

          if st.symtabletype=localsymtable then
            keepalive:=clocalvarsym.create('$'+capturer_var_name+keepalive_suffix,vs_value,interface_iunknown,[vo_is_internal])
          else
            keepalive:=cstaticvarsym.create('$'+capturer_var_name+keepalive_suffix,vs_value,interface_iunknown,[vo_is_internal]);
          keepalive.fileinfo:=pd.fileinfo;
          st.insertsym(keepalive);
          addsymref(keepalive);

          if st.symtabletype<>localsymtable then
            begin
              cnodeutils.insertbssdata(tstaticvarsym(result));
              cnodeutils.insertbssdata(tstaticvarsym(keepalive));
            end;

          { avoid warnings as these symbols are initialized using initialize_capturer
            after parsing the body }
          tabstractvarsym(result).varstate:=vs_readwritten;
          keepalive.varstate:=vs_readwritten;

          pd.has_capturer:=true;
        end;
    end;


  function can_be_captured(sym:tsym;curpd:tprocdef):boolean;
    begin
      result:=false;
      if (sym.typ=procsym) and assigned(curpd) and (curpd.procsym=sym) then
        exit(true);
      if not (sym.typ in [localvarsym,paravarsym]) then
        exit;
      if tabstractnormalvarsym(sym).varoptions*[vo_is_result,vo_is_funcret]<>[] then
        exit;
      if sym.typ=paravarsym then
        begin
          if (tparavarsym(sym).varspez in [vs_out,vs_var]) and
              not (vo_is_self in tparavarsym(sym).varoptions) then
            exit;
          if is_open_array(tparavarsym(sym).vardef) then
            exit;
        end;
      result:=true;
    end;


  type
    tsym_mapping = record
      oldsym:tsym;
      newsym:tsym;
    end;
    psym_mapping = ^tsym_mapping;


  function replace_self_sym(var n:tnode;arg:pointer):foreachnoderesult;
    var
      mapping : psym_mapping absolute arg;
      ld : tloadnode;
    begin
      if n.nodetype=loadn then
        begin
          ld:=tloadnode(n);
          if ld.symtableentry=mapping^.oldsym then
            begin
              ld.symtableentry:=mapping^.newsym;
              { make sure that the node is processed again }
              ld.resultdef:=nil;
              if assigned(ld.left) then
                begin
                  { no longer loaded through the frame pointer }
                  ld.left.free;
                  ld.left:=nil;
                end;
              typecheckpass(n);
            end;
        end;
      result:=fen_true;
    end;


  procedure capture_captured_syms(pd:tprocdef;owner:tprocinfo;capturedef:tobjectdef;oldpd:tprocdef);
    var
      curpd : tprocdef;
      subcapturer : tobjectdef;
      symstodo : TFPList;
      i : longint;
      sym : tsym;
      fieldsym : tfieldvarsym;
      fieldname : tsymstr;
      fielddef : tdef;
    begin
      if not pd.was_anonymous or not assigned(pd.capturedsyms) or (pd.capturedsyms.count=0) then
        exit;
      { capture all variables that the original procdef captured }
      curpd:=owner.procdef;
      subcapturer:=capturedef;
      symstodo:=tfplist.create;
      for i:=0 to pd.capturedsyms.count-1 do
        if can_be_captured(pcapturedsyminfo(pd.capturedsyms[i])^.sym,oldpd) and
            (pcapturedsyminfo(pd.capturedsyms[i])^.sym.typ<>procsym) then
          symstodo.add(pcapturedsyminfo(pd.capturedsyms[i])^.sym);
      while symstodo.count>0 do
        begin
          { we know we have symbols left to capture thus we either have a
            symbol that's located in the capturer of the current procdef or
            we need to put in the OuterSelf reference }
          if curpd=owner.procdef then
            subcapturer:=capturedef
          else
            subcapturer:=tobjectdef(tabstractvarsym(get_or_create_capturer(curpd)).vardef);
          i:=0;
          while i<symstodo.count do
            begin
              sym:=tsym(symstodo[i]);
              if (sym.owner=curpd.localst) or
                  (sym.owner=curpd.parast) then
                begin
                  {$ifdef DEBUG_CAPTURER}writeln('Symbol ',sym.name,' captured from ',curpd.procsym.name);{$endif}
                  { the symbol belongs to the current procdef, so add a field to
                    the capturer if it doesn't already exist }
                  if vo_is_self in tabstractnormalvarsym(sym).varoptions then
                    fieldname:=outer_self_field_name
                  else
                    fieldname:=sym.name;
                  fieldsym:=tfieldvarsym(subcapturer.symtable.find(fieldname));
                  if not assigned(fieldsym) then
                    begin
                      {$ifdef DEBUG_CAPTURER}writeln('Adding field ',fieldname,' to ',subcapturer.typesym.name);{$endif}
                      fielddef:=tabstractvarsym(sym).vardef;
                      if vo_is_self in tabstractnormalvarsym(sym).varoptions then
                        begin
                          fieldname:='$'+fieldname;
                          if not is_implicit_pointer_object_type(fielddef) then
                            fielddef:=cpointerdef.getreusable(fielddef);
                        end;
                      fieldsym:=cfieldvarsym.create(fieldname,vs_value,fielddef,[]);
                      fieldsym.fileinfo:=sym.fileinfo;
                      subcapturer.symtable.insertsym(fieldsym);
                      tabstractrecordsymtable(subcapturer.symtable).addfield(fieldsym,vis_public);
                    end;
                  if not assigned(tabstractnormalvarsym(sym).capture_sym) then
                    tabstractnormalvarsym(sym).capture_sym:=fieldsym
                  else if tabstractnormalvarsym(sym).capture_sym<>fieldsym then
                    internalerror(2022011602);
                  symstodo.delete(i);
                end
              else if sym=pd.procsym then
                { no explicit capturing needed here }
                symstodo.delete(i)
              else
                inc(i);
            end;
          if symstodo.count>0 then
            begin
              if curpd.owner.symtabletype<>localsymtable then
                internalerror(2022011001);
              { there are still symbols left, so before we move to the parent
                procdef we add the OuterSelf field to set up the chain of
                capturers }
              {$ifdef DEBUG_CAPTURER}writeln('Initialize capturer for ',curpd.procsym.name);{$endif}
              { we no longer need the curpd, but we need the parent, so change
                curpd here }
                curpd:=tprocdef(curpd.owner.defowner);
                if curpd.typ<>procdef then
                  internalerror(2022011002);
              if not assigned(subcapturer.symtable.find(outer_self_field_name)) then
                begin
                  {$ifdef DEBUG_CAPTURER}writeln('Adding field OuterSelf to ',subcapturer.typesym.name);{$endif}
                  if subcapturer.owner.symtablelevel>normal_function_level then
                    { the outer self is the capturer of the outer procdef }
                    sym:=get_or_create_capturer(curpd)
                  else
                    begin
                      { the outer self is the self of the method }
                      if not (curpd.owner.symtabletype in [objectsymtable,recordsymtable]) then
                        internalerror(2022011603);
                      sym:=tsym(curpd.parast.find('self'));
                      if not assigned(sym) then
                        internalerror(2022011604);
                    end;
                  { add the keep alive IUnknown symbol }
                  fieldsym:=cfieldvarsym.create('$'+outer_self_field_name+keepalive_suffix,vs_value,interface_iunknown,[]);
                  fieldsym.fileinfo:=sym.fileinfo;
                  subcapturer.symtable.insertsym(fieldsym);
                  tabstractrecordsymtable(subcapturer.symtable).addfield(fieldsym,vis_public);
                  { add the capturer symbol }
                  fieldsym:=cfieldvarsym.create('$'+outer_self_field_name,vs_value,tabstractvarsym(sym).vardef,[]);
                  fieldsym.fileinfo:=sym.fileinfo;
                  subcapturer.symtable.insertsym(fieldsym);
                  tabstractrecordsymtable(subcapturer.symtable).addfield(fieldsym,vis_public);
                  if (sym.typ=paravarsym) and (vo_is_self in tparavarsym(sym).varoptions) then
                    begin
                      if assigned(tparavarsym(sym).capture_sym) then
                        internalerror(2022011705);
                      tparavarsym(sym).capture_sym:=fieldsym;
                    end;
                end;
            end;
        end;
      symstodo.free;
    end;


  function retrieve_sym_for_filepos(var n:tnode;arg:pointer):foreachnoderesult;
    var
      sym : ^tsym absolute arg;
    begin
      if assigned(sym^) then
        exit(fen_norecurse_true);
      result:=fen_false;
      if not (n.resultdef.typ in [procdef,procvardef]) then
        exit;
      if n.nodetype=loadn then
        begin
          sym^:=tloadnode(n).symtableentry;
          result:=fen_norecurse_true;
        end
      else if n.nodetype=subscriptn then
        begin
          sym^:=tsubscriptnode(n).vs;
          result:=fen_norecurse_true;
        end;
    end;


  function collect_syms_to_capture(var n:tnode;arg:pointer):foreachnoderesult;
    var
      pd : tprocdef absolute arg;
      sym : tsym;
    begin
      result:=fen_false;
      if n.nodetype<>loadn then
        exit;
      sym:=tsym(tloadnode(n).symtableentry);
      if not (sym.owner.symtabletype in [parasymtable,localsymtable]) then
        exit;
      if sym.owner.symtablelevel>normal_function_level then begin
        pd.add_captured_sym(sym,tloadnode(n).resultdef,n.fileinfo);
        result:=fen_true;
      end;
    end;


  type
    tselfinfo=record
      selfsym:tsym;
      ignore:tsym;
    end;
    pselfinfo=^tselfinfo;


  function find_self_sym(var n:tnode;arg:pointer):foreachnoderesult;
    var
      info : pselfinfo absolute arg;
    begin
      result:=fen_false;
      if assigned(info^.selfsym) then
        exit(fen_norecurse_true);
      if n.nodetype<>loadn then
        exit;
      if tloadnode(n).symtableentry.typ<>paravarsym then
        exit;
      if tloadnode(n).symtableentry=info^.ignore then
        exit;
      if vo_is_self in tparavarsym(tloadnode(n).symtableentry).varoptions then
        begin
          info^.selfsym:=tparavarsym(tloadnode(n).symtableentry);
          result:=fen_norecurse_true;
        end;
    end;


  function find_outermost_loaded_sym(var n:tnode;arg:pointer):foreachnoderesult;
    var
      sym : ^tsym absolute arg;
    begin
      if assigned(sym^) then
        exit(fen_norecurse_true);
      result:=fen_false;
      if n.nodetype<>loadn then
        exit;
      if not (n.resultdef.typ in [procdef,procvardef]) then
        exit;
      sym^:=tloadnode(n).symtableentry;
      result:=fen_norecurse_true;
    end;


  function find_procdef(var n:tnode;arg:pointer):foreachnoderesult;
    var
      pd : ^tprocdef absolute arg;
    begin
      if assigned(pd^) then
        exit(fen_norecurse_true);
      result:=fen_false;
      if n.resultdef.typ<>procdef then
        exit;
      pd^:=tprocdef(n.resultdef);
      result:=fen_norecurse_true;
    end;


  function capturer_add_procvar_or_proc(owner:tprocinfo;n:tnode;out capturer:tsym;out capturen:tnode):tobjectdef;

    function create_paras(pd:tprocdef):tcallparanode;
      var
        para : tparavarsym;
        i : longint;
      begin
        result:=nil;
        for i:=0 to pd.paras.count-1 do
          begin
            para:=tparavarsym(pd.paras[i]);
            if vo_is_hidden_para in para.varoptions then
              continue;
            result:=ccallparanode.create(cloadnode.create(para,pd.parast),result);
          end;
      end;

      function find_nested_procinfo(pd:tprocdef):tcgprocinfo;
        var
          tmp,
          res : tprocinfo;
        begin
          tmp:=owner;
          while assigned(tmp) and (tmp.procdef.parast.symtablelevel>=normal_function_level) do
            begin
              res:=tmp.find_nestedproc_by_pd(pd);
              if assigned(res) then
                exit(tcgprocinfo(res));
              tmp:=tmp.parent;
            end;
          result:=nil;
        end;

      procedure swap_symtable(var st1,st2:tsymtable);
        var
          st : tsymtable;
          owner : tdefentry;
          level : byte;
        begin
          { first swap the symtables themselves }
          st:=st1;
          st1:=st2;
          st2:=st;
          { then swap the symtables' owners }
          owner:=st1.defowner;
          st1.defowner:=st2.defowner;
          st2.defowner:=owner;
          { and finally the symtable level }
          level:=st1.symtablelevel;
          st1.symtablelevel:=st2.symtablelevel;
          st2.symtablelevel:=level;
        end;

      procedure print_procinfo(pi:tcgprocinfo);
        begin
          { Print the node to tree.log }
          if paraprintnodetree <> 0 then
            pi.printproc('after parsing');

{$ifdef DEBUG_NODE_XML}
          { Methods of generic classes don't get any code generated, so output
            the node tree here }
          if (df_generic in pi.procdef.defoptions) then
            pi.XMLPrintProc(True);
{$endif DEBUG_NODE_XML}
        end;

    var
      ps : tprocsym;
      oldpd,
      pd : tprocdef;
      pinested,
      pi : tcgprocinfo;
      sym,
      fpsym,
      selfsym : tsym;
      invokename : tsymstr;
      capturedef : tobjectdef;
      capturesyms : tfplist;
      captured : pcapturedsyminfo;
      implintf : TImplementedInterface;
      i : longint;
      stmt : tstatementnode;
      n1 : tnode;
      fieldsym : tfieldvarsym;
      selfinfo : tselfinfo;
    begin
      if not (n.resultdef.typ in [procdef,procvardef]) then
        internalerror(2022022101);

      capturer:=nil;
      capturen:=nil;
      pinested:=nil;
      oldpd:=nil;

      { determine a unique name for the variable, field for function of the
        node we're trying to load }

      sym:=nil;
      if not foreachnodestatic(pm_preprocess,n,@find_outermost_loaded_sym,@sym) then
        internalerror(2022022102);

      result:=funcref_intf_for_proc(tabstractprocdef(n.resultdef),fileinfo_to_suffix(sym.fileinfo));

      if (sym.typ=procsym) and (sym.owner.symtabletype=localsymtable) then
        begin
          { this is assigning a nested function, so retrieve the correct procdef
            so that we can then retrieve the procinfo for it }
          if n.resultdef.typ=procdef then
            pd:=tprocdef(n.resultdef)
          else
            begin
              pd:=nil;
              if not foreachnodestatic(pm_preprocess,n,@find_procdef,@pd) then
                internalerror(2022041801);
              if not assigned(pd) then
                internalerror(2022041802);
            end;
          { check whether all captured symbols can indeed be captured }
          capturesyms:=pd.capturedsyms;
          if assigned(capturesyms) then
            for i:=0 to capturesyms.count-1 do
              begin
                captured:=pcapturedsyminfo(capturesyms[i]);
                if not can_be_captured(captured^.sym,pd) then
                  MessagePos1(captured^.fileinfo,sym_e_symbol_no_capture,captured^.sym.realname);
              end;
          if not (df_generic in owner.procdef.defoptions) then
            begin
              pinested:=find_nested_procinfo(pd);
              if not assigned(pinested) then
                internalerror(2022041803);
              oldpd:=pd;
              if pinested.parent<>owner then
                begin
                  { we need to capture this into the owner of the nested function
                    instead }
                  owner:=pinested;
                  capturer:=get_or_create_capturer(pinested.procdef);
                  if not assigned(capturer) then
                    internalerror(2022041804);
                end;
            end;
        end
      else if (n.resultdef.typ=procvardef) and
          (po_delphi_nested_cc in tprocvardef(n.resultdef).procoptions) then
        begin
          MessagePos(n.fileinfo,type_e_nested_procvar_to_funcref);
          exit;
        end
      else
        pinested:=nil;

      if df_generic in owner.procdef.defoptions then
        exit;

      if not assigned(capturer) then
        capturer:=get_or_create_capturer(owner.procdef);

      if not (capturer.typ in [localvarsym,staticvarsym]) then
        internalerror(2022022103);
      capturedef:=tobjectdef(tabstractvarsym(capturer).vardef);
      if not is_class(capturedef) then
        internalerror(2022022104);
      implintf:=find_implemented_interface(capturedef,result);
      if assigned(implintf) then
        begin
          { this is already captured into a method of the capturer, so nothing
            further to do }
          exit;
        end;
      implintf:=capturedef.register_implemented_interface(result,true);

      invokename:=method_name_funcref_invoke_decl+'__FPCINTERNAL__'+fileinfo_to_suffix(sym.fileinfo);

      ps:=cprocsym.create(invokename);
      pd:=tprocdef(tabstractprocdef(n.resultdef).getcopyas(procdef,pc_normal_no_hidden,'',false));
      pd.aliasnames.clear;

      pd.procsym:=ps;
      pd.struct:=capturedef;
      pd.changeowner(capturedef.symtable);
      pd.parast.symtablelevel:=normal_function_level;
      pd.localst.symtablelevel:=normal_function_level;
      { reset procoptions }
      pd.procoptions:=[];
      ps.ProcdefList.Add(pd);
      pd.forwarddef:=false;
      { set procinfo and current_procinfo.procdef }
      pi:=tcgprocinfo(cprocinfo.create(nil));
      pi.procdef:=pd;
      if not assigned(pinested) then
        begin
          insert_funcret_para(pd);
          insert_funcret_local(pd);
          { we always do a call, namely to the provided function }
          include(pi.flags,pi_do_call);
        end
      else
        begin
          { the original nested function now calls the method }
          include(pinested.flags,pi_do_call);
          { swap the para and local symtables of the nested and new routine }
          swap_symtable(pinested.procdef.parast,pd.parast);
          swap_symtable(pinested.procdef.localst,pd.localst);
          { fix function return symbol }
          pd.funcretsym:=pinested.procdef.funcretsym;
          pinested.procdef.funcretsym:=nil;
          insert_funcret_para(pinested.procdef);
          insert_funcret_local(pinested.procdef);
          { the nested function needs access to the parent's framepointer to
            access the capturer }
          insert_parentfp_para(pinested.procdef);
          pd.copied_from:=pinested.procdef;
        end;
      { to simplify some checks, but only after insert_funcret_para }
      pd.was_anonymous:=true;
      capturedef.symtable.insertsym(ps);
      owner.addnestedproc(pi);

      { remove self and parentfp parameter if any as that will be replaced by
        the capturer }
      selfsym:=nil;
      fpsym:=nil;
      for i:=0 to pd.parast.symlist.count-1 do
        begin
          sym:=tsym(pd.parast.symlist[i]);
          if sym.typ<>paravarsym then
            continue;
          if vo_is_self in tparavarsym(sym).varoptions then
            selfsym:=sym
          else if vo_is_parentfp in tparavarsym(sym).varoptions then
            fpsym:=sym;
          if assigned(selfsym) and assigned(fpsym) then
            break;
        end;
      if assigned(selfsym) then
        pd.parast.deletesym(selfsym);
      if assigned(fpsym) then
        pd.parast.deletesym(fpsym);
      pd.calcparas;
      if assigned(pinested) then
        pinested.procdef.calcparas;

      insert_self_and_vmt_para(pd);

      if assigned(pinested) then
        begin
          { when we're assigning a nested function to a function reference we
            move the code of the nested function to the newly created capturer
            method (including the captured symbols) and have the original nested
            function simply call that function-turned-method }
          pi.code:=pinested.code;
          pinested.code:=internalstatements(stmt);
        end
      else
        pi.code:=internalstatements(stmt);

      selfinfo.selfsym:=nil;
      selfinfo.ignore:=nil;

      fieldsym:=nil;
      if assigned(pinested) then
        begin
          n1:=ccallnode.create(create_paras(pinested.procdef),ps,capturedef.symtable,cloadnode.create(capturer,capturer.owner),[],nil);
          { captured variables cannot be in registers }
          make_not_regable(tcallnode(n1).methodpointer,[ra_addr_regable,ra_addr_taken]);
        end
      else if n.resultdef.typ=procvardef then
        begin
          { store the procvar in a field so that it won't be changed if the
            procvar itself is changed }
          fieldsym:=cfieldvarsym.create('$'+fileinfo_to_suffix(n.fileinfo),vs_value,n.resultdef,[]);
          fieldsym.fileinfo:=n.fileinfo;
          capturedef.symtable.insertsym(fieldsym);
          tabstractrecordsymtable(capturedef.symtable).addfield(fieldsym,vis_public);

          capturen:=csubscriptnode.create(fieldsym,cloadnode.create(capturer,capturer.owner));

          selfsym:=tsym(pd.parast.find('self'));
          if not assigned(selfsym) then
            internalerror(2022052301);
          selfinfo.ignore:=selfsym;
          n1:=ccallnode.create_procvar(create_paras(pd),csubscriptnode.create(fieldsym,cloadnode.create(selfsym,selfsym.owner)));
        end
      else
        begin
          if n.nodetype<>loadn then
            internalerror(2022032401);
          if tloadnode(n).symtableentry.typ<>procsym then
            internalerror(2022032402);
          n1:=ccallnode.create(create_paras(pd),tprocsym(tloadnode(n).symtableentry),tloadnode(n).symtable,tloadnode(n).left,[],nil);
          tloadnode(n).left:=nil;
        end;
      if assigned(pd.returndef) and not is_void(pd.returndef) then
        begin
          if assigned(pinested) then
            sym:=pinested.procdef.funcretsym
          else
            sym:=pd.funcretsym;
          n1:=cassignmentnode.create(
                      cloadnode.create(sym,sym.owner),
                      n1
                    );
          { captured variables cannot be in registers }
          make_not_regable(tassignmentnode(n1).left,[ra_addr_regable,ra_addr_taken]);
        end;
      addstatement(stmt,n1);
      pd.aliasnames.insert(pd.mangledname);

      if assigned(pinested) then
        begin
          { transfer all captured syms }
          capturesyms:=pinested.procdef.capturedsyms;
          if assigned(capturesyms) then
            begin
              for i:=0 to capturesyms.count-1 do
                begin
                  captured:=pcapturedsyminfo(capturesyms[i]);
                  pi.add_captured_sym(captured^.sym,captured^.def,captured^.fileinfo);
                  dispose(captured);
                end;
              capturesyms.clear;
            end;
          { the original nested function now needs to capture only the capturer }
          pinested.procdef.add_captured_sym(capturer,capturedef,n.fileinfo);
        end
      { does this need to capture Self? }
      else if not foreachnodestatic(pm_postprocess,n,@find_self_sym,@selfinfo) then
        begin
          { is this a method of the current class? }
          if (n.resultdef.typ=procdef) and
              assigned(tprocdef(n.resultdef).struct) and
              not (po_staticmethod in tprocdef(n.resultdef).procoptions) and
              assigned(current_procinfo.procdef.struct) and
              def_is_related(current_procinfo.procdef.struct,tprocdef(n.resultdef).struct) then
            begin
              selfinfo.selfsym:=tsym(current_procinfo.procdef.parast.find('self'));
              if not assigned(selfinfo.selfsym) then
                internalerror(2022110601);
            end
          else
            { does this need some other local variable or parameter? }
            foreachnodestatic(pm_postprocess,n,@collect_syms_to_capture,@pd)
        end;

      if assigned(selfinfo.selfsym) and not assigned(fieldsym) then
        { this isn't a procdef that was captured into a field, so capture the
          self }
        pd.add_captured_sym(selfinfo.selfsym,tabstractvarsym(selfinfo.selfsym).vardef,n.fileinfo);

      print_procinfo(pi);
      if assigned(pinested) then
        print_procinfo(pinested);

      implintf.AddMapping(upcase(result.objrealname^+'.')+method_name_funcref_invoke_find,upcase(invokename));

      capture_captured_syms(pd,owner,capturedef,oldpd);
    end;


  function capturer_add_anonymous_proc(owner:tprocinfo;pd:tprocdef;out capturer:tsym):tobjectdef;
    var
      capturedef : tobjectdef;
      implintf : TImplementedInterface;
      invokename : tsymstr;
      i : longint;
      outerself,
      fpsym,
      selfsym,
      sym : tsym;
      info : pcapturedsyminfo;
      pi : tprocinfo;
      mapping : tsym_mapping;
      invokedef,
      parentdef,
      curpd : tprocdef;
    begin
      capturer:=nil;
      result:=funcref_intf_for_proc(pd,fileinfo_to_suffix(pd.fileinfo));

      if df_generic in pd.defoptions then
        begin
          if (po_anonymous in pd.procoptions) and
              assigned(pd.capturedsyms) and
              (pd.capturedsyms.count>0) then
            begin
              { only check whether the symbols can be captured, but don't
                convert anything to avoid problems }
              for i:=0 to pd.capturedsyms.count-1 do
                begin
                  info:=pcapturedsyminfo(pd.capturedsyms[i]);
                  if not can_be_captured(info^.sym,pd) then
                    MessagePos1(info^.fileinfo,sym_e_symbol_no_capture,info^.sym.realname)
                end;
            end;
          exit;
        end;

      capturer:=get_or_create_capturer(owner.procdef);

      if not (capturer.typ in [localvarsym,staticvarsym]) then
        internalerror(2022010711);
      capturedef:=tobjectdef(tabstractvarsym(capturer).vardef);
      if not is_class(capturedef) then
        internalerror(2022010712);
      implintf:=find_implemented_interface(capturedef,result);
      if assigned(implintf) then
        begin
          { this can only already be an implemented interface if a named procdef
            was assigned to a function ref at an earlier point, an anonymous
            function can be used only once }
          if po_anonymous in pd.procoptions then
            internalerror(2022010713);
          exit;
        end;
      implintf:=capturedef.register_implemented_interface(result,true);

      invokename:=method_name_funcref_invoke_decl+'__FPCINTERNAL__'+fileinfo_to_suffix(pd.fileinfo);
      if po_anonymous in pd.procoptions then
        begin
          { turn the anonymous function into a method of the capturer }
          pd.changeowner(capturedef.symtable);
          pd.struct:=capturedef;
          exclude(pd.procoptions,po_anonymous);
          exclude(pd.procoptions,po_delphi_nested_cc);
          exclude(pd.procoptions,po_staticmethod);
          exclude(pd.procoptions,po_classmethod);
          pd.was_anonymous:=true;
          pd.procsym.ChangeOwnerAndName(capturedef.symtable,upcase(invokename));
          pd.procsym.realname:=invokename;
          pd.parast.symtablelevel:=normal_function_level;
          pd.localst.symtablelevel:=normal_function_level;
          { retrieve framepointer and self parameters if any }
          fpsym:=nil;
          selfsym:=nil;
          for i:=0 to pd.parast.symlist.count-1 do
            begin
              sym:=tsym(pd.parast.symlist[i]);
              if sym.typ<>paravarsym then
                continue;
              if vo_is_parentfp in tparavarsym(sym).varoptions then
                fpsym:=sym
              else if vo_is_self in tparavarsym(sym).varoptions then
                selfsym:=sym;
              if assigned(fpsym) and assigned(selfsym) then
                break;
            end;
          { get rid of the framepointer parameter }
          if assigned(fpsym) then
            pd.parast.deletesym(fpsym);
          outerself:=nil;
          { complain about all symbols that can't be captured and add the symbols
            to this procdefs capturedsyms if it isn't a top level function }
          if assigned(pd.capturedsyms) and (pd.capturedsyms.count>0) then
            begin
              for i:=0 to pd.capturedsyms.count-1 do
                begin
                  info:=pcapturedsyminfo(pd.capturedsyms[i]);
                  if not can_be_captured(info^.sym,pd) then
                    MessagePos1(info^.fileinfo,sym_e_symbol_no_capture,info^.sym.realname)
                  else if info^.sym=selfsym then
                    begin
                      { we need to replace the captured "dummy" self parameter
                        with the real self parameter symbol from the surrounding
                        method }
                      if not assigned(outerself) then
                        outerself:=tsym(owner.get_normal_proc.procdef.parast.find('self'));
                      if not assigned(outerself) then
                        internalerror(2022010905);

                      { the anonymous function can only be a direct child of the
                        owner }
                      pi:=owner.get_first_nestedproc;
                      while assigned(pi) do
                        begin
                          if pi.procdef=pd then
                            break;
                          pi:=tprocinfo(pi.next);
                        end;

                      if not assigned(pi) then
                        internalerror(2022010906);

                      mapping.oldsym:=selfsym;
                      mapping.newsym:=outerself;

                      { replace all uses of the captured Self by the new Self
                        parameter }
                      foreachnodestatic(pm_preprocess,tcgprocinfo(pi).code,@replace_self_sym,@mapping);

                      { update the captured symbol }
                      info^.sym:=outerself;
                      info^.def:=tabstractvarsym(outerself).vardef;
                    end
                  else if info^.sym.owner.defowner<>owner.procdef then
                    owner.procdef.add_captured_sym(info^.sym,info^.def,info^.fileinfo);
                end;
            end;
          { delete the original self parameter }
          if assigned(selfsym) then
            pd.parast.deletesym(selfsym);
          { note: don't call insert_self_and_vmt_para here, as that is later on
                  done when building the VMT }
        end
      else
        internalerror(2022022201);
      implintf.AddMapping(upcase(result.objrealname^+'.')+method_name_funcref_invoke_find,upcase(invokename));

      capture_captured_syms(pd,owner,capturedef,nil);
    end;


  function load_capturer(capturer:tabstractvarsym):tnode;inline;
    begin
      result:=cloadnode.create(capturer,capturer.owner);
    end;


  function instantiate_capturer(capturer_sym:tabstractvarsym):tnode;
    var
      capturer_def : tobjectdef;
      ctor : tprocsym;
    begin
      capturer_def:=tobjectdef(capturer_sym.vardef);

      { Neither TInterfacedObject, nor TCapturer have a custom constructor }
      ctor:=tprocsym(class_tobject.symtable.Find('CREATE'));
      if not assigned(ctor) then
        internalerror(2022010801);

      { Insert "Capturer := TCapturer.Create()" as the first statement of the routine }
      result:=cloadvmtaddrnode.create(ctypenode.create(capturer_def));
      result:=ccallnode.create(nil,ctor,capturer_def.symtable,result,[],nil);
      result:=cassignmentnode.create(load_capturer(capturer_sym),result);
    end;


  procedure initialize_captured_paras(pd:tprocdef;capturer:tabstractvarsym;var stmt:tstatementnode);
    var
      i : longint;
      psym: tparavarsym;
      n : tnode;
    begin
      for i:=0 to pd.paras.count-1 do
        begin
          psym:=tparavarsym(pd.paras[i]);
          if not psym.is_captured then
            continue;
          {$ifdef DEBUG_CAPTURER}writeln(#9'initialize captured parameter ',psym.RealName);{$endif}
          n:=cloadnode.create(psym,psym.owner);
          if psym.capture_sym.owner.defowner<>capturer.vardef then
            internalerror(2022010903);
          if (vo_is_self in psym.varoptions) and not is_implicit_pointer_object_type(psym.vardef) then
            n:=caddrnode.create(n);
          n:=cassignmentnode.create(
               csubscriptnode.create(psym.capture_sym,cloadnode.create(capturer,capturer.owner)),
               n
               );
          addstatement(stmt,n);
        end;
    end;


  procedure attach_outer_capturer(ctx:tprocinfo;capturer:tabstractvarsym;var stmt:tstatementnode);
    var
      alivefield,
      selffield : tfieldvarsym;
      outeralive,
      outercapturer : tabstractvarsym;
      alivenode,
      selfnode : tnode;
    begin
      if not ctx.procdef.was_anonymous and
          not (ctx.procdef.owner.symtabletype=localsymtable) then
        exit;
      selffield:=tfieldvarsym(tobjectdef(capturer.vardef).symtable.find(outer_self_field_name));
      if not assigned(selffield) then
        { we'll simply assume that we don't need the outer capturer }
        exit;
      alivefield:=tfieldvarsym(tobjectdef(capturer.vardef).symtable.find(outer_self_field_name+keepalive_suffix));
      if not assigned(alivefield) then
        internalerror(2022051701);
      if ctx.procdef.was_anonymous then
        begin
          selfnode:=load_self_node;
          alivenode:=selfnode.getcopy;
        end
      else
        begin
          outercapturer:=get_capturer(tprocdef(ctx.procdef.owner.defowner));
          if not assigned(outercapturer) then
            internalerror(2022011605);
          selfnode:=cloadnode.create(outercapturer,outercapturer.owner);
          outeralive:=get_capturer_alive(tprocdef(ctx.procdef.owner.defowner));
          if not assigned(outeralive) then
            internalerror(2022051706);
          alivenode:=cloadnode.create(outeralive,outeralive.owner);
        end;
      addstatement(stmt,cassignmentnode.create(
                          csubscriptnode.create(
                            selffield,
                            cloadnode.create(
                              capturer,
                              capturer.owner
                              )
                            ),
                            selfnode));
      addstatement(stmt,cassignmentnode.create(
                          csubscriptnode.create(
                            alivefield,
                            cloadnode.create(
                              capturer,
                              capturer.owner
                              )
                            ),
                            alivenode));
    end;


  procedure initialize_capturer(ctx:tprocinfo;var stmt:tstatementnode);
    var
      capturer_sym,
      keepalive_sym : tabstractvarsym;
    begin
      if ctx.procdef.has_capturer then
        begin
          capturer_sym:=get_capturer(ctx.procdef);
          {$ifdef DEBUG_CAPTURER}writeln('initialize_capturer @ ',ctx.procdef.procsym.RealName);{$endif}

          addstatement(stmt,instantiate_capturer(capturer_sym));
          attach_outer_capturer(ctx,capturer_sym,stmt);
          initialize_captured_paras(ctx.procdef,capturer_sym,stmt);

          keepalive_sym:=get_capturer_alive(ctx.procdef);
          if not assigned(keepalive_sym) then
            internalerror(2022010701);
          addstatement(stmt,cassignmentnode.create(cloadnode.create(keepalive_sym,keepalive_sym.owner),load_capturer(capturer_sym)));
        end;
    end;


  procedure postprocess_capturer(ctx: tprocinfo);
    var
      def: tobjectdef;
    begin
      if not ctx.procdef.has_capturer then
        exit;

      def:=tobjectdef(get_capturer(ctx.procdef).vardef);
      {$ifdef DEBUG_CAPTURER}writeln('process capturer ',def.typesym.Name);{$endif}
      { These two are delayed until this point because
        ... we have been adding fields on-the-fly }
      tabstractrecordsymtable(def.symtable).addalignmentpadding;
      { ... we have been adding interfaces on-the-fly }
      build_vmt(def);
    end;


  type
    tconvert_arg=record
      mappings:tfplist;
    end;
    pconvert_arg=^tconvert_arg;

    tconvert_mapping=record
      oldsym:tsym;
      newsym:tsym;
      olddef:tdef;
      selfnode:tnode;
    end;
    pconvert_mapping=^tconvert_mapping;


  function convert_captured_sym(var n:tnode;arg:pointer):foreachnoderesult;
    var
      convertarg : pconvert_arg absolute arg;
      mapping : pconvert_mapping;
      i : longint;
      old_filepos : tfileposinfo;
      loadprocvar : boolean;
      paras,
      mp : tnode;
      cnf : tcallnodeflags;
      paraold,
      paranew : tcallparanode;
    begin
      result:=fen_true;
      if not (n.nodetype in [loadn,calln]) then
        exit;
      for i:=0 to convertarg^.mappings.count-1 do
        begin
          mapping:=convertarg^.mappings[i];
          case n.nodetype of
            loadn:
              begin
                if tloadnode(n).symtableentry<>mapping^.oldsym then
                  continue;
                old_filepos:=current_filepos;
                current_filepos:=n.fileinfo;
                loadprocvar:=nf_load_procvar in n.flags;
                n.free;
                n:=csubscriptnode.create(mapping^.newsym,mapping^.selfnode.getcopy);
                if loadprocvar then
                  include(n.flags,nf_load_procvar);
                if (mapping^.oldsym.typ=paravarsym) and
                    (vo_is_self in tparavarsym(mapping^.oldsym).varoptions) and
                    not is_implicit_pointer_object_type(tparavarsym(mapping^.oldsym).vardef) then
                  n:=cderefnode.create(n);
                typecheckpass(n);
                current_filepos:=old_filepos;
                break;
              end;
            calln:
              begin
                if mapping^.oldsym.typ<>procsym then
                  continue;
                if tcallnode(n).symtableprocentry<>tprocsym(mapping^.oldsym) then
                  continue;
                if tcallnode(n).procdefinition<>tprocdef(mapping^.olddef) then
                  continue;
                old_filepos:=current_filepos;
                current_filepos:=n.fileinfo;
                loadprocvar:=nf_load_procvar in n.flags;
                paras:=tcallnode(n).left;
                paraold:=tcallparanode(paras);
                paranew:=nil;
                while assigned(paraold) do
                  begin
                    if not (vo_is_hidden_para in paraold.parasym.varoptions) then
                      begin
                        paranew:=ccallparanode.create(paraold.left,paranew);
                        paraold.left:=nil;
                      end;
                    paraold:=tcallparanode(paraold.right);
                  end;
                reverseparameters(paranew);
                if assigned(tcallnode(n).methodpointer) then
                  internalerror(2023120802);
                cnf:=tcallnode(n).callnodeflags;
                n.free;
                n:=ccallnode.create(paranew,tprocsym(mapping^.newsym),mapping^.newsym.owner,mapping^.selfnode.getcopy,cnf,nil);
                if loadprocvar then
                  include(n.flags,nf_load_procvar);
                typecheckpass(n);
                current_filepos:=old_filepos;
                break;
              end;
            else
              internalerror(2023120801);
          end;
        end;
    end;


  procedure convert_captured_syms(pd:tprocdef;tree:tnode);

    function self_tree_for_sym(selfsym:tsym;fieldsym:tsym):tnode;
      var
        fieldowner : tdef;
        newsym : tsym;
      begin
        result:=cloadnode.create(selfsym,selfsym.owner);
        fieldowner:=tdef(fieldsym.owner.defowner);
        newsym:=selfsym;
        while (tabstractvarsym(newsym).vardef<>fieldowner) do
          begin
            newsym:=tsym(tobjectdef(tabstractvarsym(newsym).vardef).symtable.find(outer_self_field_name));
            if not assigned(newsym) then
              internalerror(2022011101);
            result:=csubscriptnode.create(newsym,result);
          end;
      end;

    var
      i,j : longint;
      capturer : tobjectdef;
      tocapture,
      capturedsyms : tfplist;
      convertarg : tconvert_arg;
      mapping : pconvert_mapping;
      invokepd : tprocdef;
      selfsym,
      sym : tsym;
      info: pcapturedsyminfo;
    begin
      {$ifdef DEBUG_CAPTURER}writeln('Converting captured symbols of ',pd.procsym.name);{$endif}

      convertarg.mappings:=tfplist.create;

      capturedsyms:=tfplist.create;

      if pd.was_anonymous and
          assigned(pd.capturedsyms) and
          (pd.capturedsyms.count>0) then
        begin
          {$ifdef DEBUG_CAPTURER}writeln('Converting symbols of converted anonymous function ',pd.procsym.name);{$endif}

          { this is a converted anonymous function, so rework all symbols that
            now belong to the new Self }

          selfsym:=tsym(pd.parast.find('self'));
          if not assigned(selfsym) then
            internalerror(2022010809);

          for i:=0 to pd.capturedsyms.count-1 do
            begin
              sym:=tsym(pcapturedsyminfo(pd.capturedsyms[i])^.sym);
              if not can_be_captured(sym,pd) and
                  not (
                    (sym.typ=procsym) and
                    assigned(pd.copied_from) and
                    (pd.copied_from.procsym=sym)
                  ) then
                continue;
              {$ifdef DEBUG_CAPTURER}writeln('Replacing symbol ',sym.Name);{$endif}
              new(mapping);
              mapping^.oldsym:=sym;
              if sym.typ=procsym then
                begin
                  if not assigned(pd.copied_from) or
                      (pd.copied_from.procsym<>sym) then
                    internalerror(2023123001);
                  mapping^.newsym:=pd.procsym;
                end
              else
                mapping^.newsym:=tabstractnormalvarsym(sym).capture_sym;
              mapping^.olddef:=pcapturedsyminfo(pd.capturedsyms[i])^.def;
              if not assigned(mapping^.newsym) then
                internalerror(2022010810);
              mapping^.selfnode:=self_tree_for_sym(selfsym,mapping^.newsym);
              convertarg.mappings.add(mapping);
              capturedsyms.add(sym);
            end;
        end;

      if (pd.parast.symtablelevel>normal_function_level) and
          assigned(pd.capturedsyms) and
          (pd.capturedsyms.count>0) then
        begin
          if pd.was_anonymous then
            internalerror(2022081201);

          {$ifdef DEBUG_CAPTURER}writeln('Converting symbols of nested function ',pd.procsym.name);{$endif}

          { this is a nested function, so rework all symbols that are used from
            a parent function, but that might have been captured }

          for i:=0 to pd.capturedsyms.count-1 do
            begin
              sym:=tsym(pcapturedsyminfo(pd.capturedsyms[i])^.sym);
              if not can_be_captured(sym,pd) or
                  (sym.typ=procsym) or
                  not assigned(tabstractnormalvarsym(sym).capture_sym) then
                continue;
              {$ifdef DEBUG_CAPTURER}writeln('Replacing symbol ',sym.Name);{$endif}
              new(mapping);
              mapping^.oldsym:=sym;
              mapping^.newsym:=tabstractnormalvarsym(sym).capture_sym;
              mapping^.olddef:=pcapturedsyminfo(pd.capturedsyms[i])^.def;
              capturer:=tobjectdef(mapping^.newsym.owner.defowner);
              if not is_class(capturer) then
                internalerror(2022012701);
              if not (capturer.typesym.owner.symtabletype in [localsymtable,staticsymtable]) then
                internalerror(2022012702);
              selfsym:=tsym(capturer.typesym.owner.find(capturer_var_name));
              if not assigned(selfsym) then
                internalerror(2022012703);
              mapping^.selfnode:=self_tree_for_sym(selfsym,mapping^.newsym);
              convertarg.mappings.add(mapping);
              capturedsyms.add(sym);
            end;
        end;

      if pd.has_capturer then
        begin
          {$ifdef DEBUG_CAPTURER}writeln('Converting symbols of function ',pd.procsym.name,' with capturer');{$endif}
          { this procedure has a capturer, so rework all symbols that are
            captured in that capturer }

          selfsym:=get_capturer(pd);

          { only capture those symbols that weren't captured already by one of
            the above if-clauses and thus are now listed in capturedsyms }
          tocapture:=tfplist.create;

          for i:=0 to pd.localst.symlist.count-1 do
            begin
              sym:=tsym(pd.localst.symlist[i]);
              if sym.typ<>localvarsym then
                continue;
              if assigned(tabstractnormalvarsym(sym).capture_sym) then
                if capturedsyms.indexof(sym)<0 then
                  tocapture.add(sym);
            end;

          for i:=0 to pd.parast.symlist.count-1 do
            begin
              sym:=tsym(pd.parast.symlist[i]);
              if sym.typ<>paravarsym then
                continue;
              if assigned(tabstractnormalvarsym(sym).capture_sym) and
                  { no need to adjust accesses to the outermost Self inside the
                    outermost method }
                  not (vo_is_self in tabstractvarsym(sym).varoptions) then
                if capturedsyms.indexof(sym)<0 then
                  tocapture.add(sym);
            end;

          for i:=0 to tocapture.count-1 do
            begin
              new(mapping);
              mapping^.oldsym:=tsym(tocapture[i]);
              {$ifdef DEBUG_CAPTURER}writeln('Replacing symbol ',mapping^.oldsym.Name);{$endif}
              mapping^.newsym:=tabstractnormalvarsym(mapping^.oldsym).capture_sym;
              if not assigned(mapping^.newsym) then
                internalerror(2022010805);
              mapping^.selfnode:=self_tree_for_sym(selfsym,mapping^.newsym);
              convertarg.mappings.add(mapping);
            end;

          tocapture.free;
        end;

      { not required anymore }
      capturedsyms.free;

      if convertarg.mappings.count>0 then
        foreachnodestatic(pm_postprocess,tree,@convert_captured_sym,@convertarg);

      for i:=0 to convertarg.mappings.count-1 do
        begin
          mapping:=pconvert_mapping(convertarg.mappings[i]);
          mapping^.selfnode.free;
          dispose(mapping);
        end;

      convertarg.mappings.free;
    end;


end.

