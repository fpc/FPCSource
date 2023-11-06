{
    Copyright (c) 2011

    Contains different functions that are used in the context of
    parsing generics.

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
unit pgenutil;

{$i fpcdefs.inc}

interface

uses
  { common }
  cclasses,
  { global }
  globtype,
  { parser }
  pgentype,
  { node }
  node,
  { symtable }
  symtype,symdef,symbase;

    procedure generate_specialization(var tt:tdef;enforce_unit:boolean;parse_class_parent:boolean;const _prettyname:string;parsedtype:tdef;const symname:string;parsedpos:tfileposinfo);inline;
    procedure generate_specialization(var tt:tdef;enforce_unit:boolean;parse_class_parent:boolean;const _prettyname:string);inline;
    function generate_specialization_phase1(out context:tspecializationcontext;genericdef:tdef;enforce_unit:boolean):tdef;inline;
    function generate_specialization_phase1(out context:tspecializationcontext;genericdef:tdef;enforce_unit:boolean;const symname:string;symtable:tsymtable):tdef;inline;
    function generate_specialization_phase1(out context:tspecializationcontext;genericdef:tdef;enforce_unit:boolean;parsedtype:tdef;const symname:string;symtable:tsymtable;parsedpos:tfileposinfo):tdef;
    function generate_specialization_phase2(context:tspecializationcontext;genericdef:tstoreddef;parse_class_parent:boolean;const _prettyname:ansistring):tdef;
    function check_generic_constraints(genericdef:tstoreddef;paramlist:tfpobjectlist;poslist:tfplist):boolean;
    function parse_generic_parameters(allowconstraints:boolean):tfphashobjectlist;
    function parse_generic_specialization_types(paramlist:tfpobjectlist;poslist:tfplist;out prettyname,specializename:ansistring):boolean;
    procedure insert_generic_parameter_types(def:tstoreddef;genericdef:tstoreddef;genericlist:tfphashobjectlist;isfwd:boolean);
    procedure maybe_insert_generic_rename_symbol(const name:tidstring;genericlist:tfphashobjectlist);
    function generate_generic_name(const name:tidstring;const specializename:ansistring;const owner_hierarchy:ansistring):tidstring;
    procedure split_generic_name(const name:tidstring;out nongeneric:string;out count:longint);
    procedure add_generic_dummysym(sym:tsym);
    function resolve_generic_dummysym(const name:tidstring):tsym;
    function could_be_generic(const name:tidstring):boolean;inline;
    function try_implicit_specialization(sym:tsym;para:tnode;pdoverloadlist:tfpobjectlist;var unnamed_syms:tfplist;var first_procsym:tsym;var hasoverload:boolean):boolean;
    function finalize_specialization(var pd:tprocdef;spezcontext:tspecializationcontext):boolean;

    procedure generate_specialization_procs;
    procedure generate_specializations_for_forwarddef(def:tdef);
    procedure maybe_add_pending_specialization(def:tdef;unnamed_syms:tfplist);
    function determine_generic_def(const name:tidstring):tstoreddef;

    procedure specialization_init(genericdef:tdef;var state:tspecializationstate);
    procedure specialization_done(var state:tspecializationstate);

implementation

uses
  { common }
  cutils,fpchash,
  { global }
  globals,tokens,verbose,finput,constexp,
  { symtable }
  symconst,symsym,symtable,defcmp,defutil,procinfo,
  { modules }
  fmodule,
  { node }
  nobj,ncon,ncal,
  { parser }
  scanner,
  pbase,pexpr,pdecsub,ptype,psub,pparautl,pdecl,procdefutil;

  type
    tdeftypeset = set of tdeftyp;
  const
    tgeneric_param_const_types : tdeftypeset = [orddef,stringdef,floatdef,setdef,pointerdef,enumdef];
    tgeneric_param_nodes : tnodetypeset = [typen,ordconstn,stringconstn,realconstn,setconstn,niln];

    procedure make_prettystring(paramtype:tdef;first:boolean;constprettyname:ansistring;var prettyname,specializename:ansistring);
      var
        namepart : string;
        prettynamepart : ansistring;
        module : tmodule;
      begin
        if assigned(paramtype.owner) then
          module:=find_module_from_symtable(paramtype.owner)
        else
          module:=current_module;
        if not assigned(module) then
          internalerror(2016112802);
        namepart:='_$'+hexstr(module.moduleid,8)+'$$'+paramtype.unique_id_str;
        if constprettyname<>'' then
          namepart:=namepart+'$$'+constprettyname;
        { we use the full name of the type to uniquely identify it }
        if (symtablestack.top.symtabletype=parasymtable) and
            (symtablestack.top.defowner.typ=procdef) and
            (paramtype.owner=symtablestack.top) then
          begin
            { special handling for specializations inside generic function declarations }
            prettynamepart:=tdef(symtablestack.top.defowner).fullownerhierarchyname(true)+tprocdef(symtablestack.top.defowner).procsym.prettyname;
          end
        else
          begin
            prettynamepart:=paramtype.fullownerhierarchyname(true);
          end;
        specializename:=specializename+namepart;
        if not first then
          prettyname:=prettyname+',';
        if constprettyname<>'' then
          prettyname:=prettyname+constprettyname
        else
          prettyname:=prettyname+prettynamepart+paramtype.typesym.prettyname;
      end;

    function get_generic_param_def(sym:tsym):tdef;
      begin
        if sym.typ=constsym then
          result:=tconstsym(sym).constdef
        else
          result:=ttypesym(sym).typedef;
      end;

    function compare_orddef_by_range(param1,param2:torddef;value:tconstvalue):boolean;
      begin
        if (value.valueord<param2.low) or (value.valueord>param2.high) then
          result:=false
        else
          result:=true;
      end;

    function compare_generic_params(param1,param2:tdef;constparamsym:tconstsym):boolean;
      begin
        if (param1.typ=orddef) and (param2.typ=orddef) then
          begin
            if is_boolean(param2) then
              result:=is_boolean(param1)
            else if is_char(param2) then
              result:=is_char(param1)
            else if compare_orddef_by_range(torddef(param1),torddef(param2),constparamsym.value) then
              result:=true
            else
              result:=false;
          end
        { arraydef is string constant so it's compatible with stringdef }
        else if (param1.typ=arraydef) and (param2.typ=stringdef) then
          result:=true
        { integer ords are compatible with float }
        else if (param1.typ=orddef) and is_integer(param1) and (param2.typ=floatdef) then
          result:=true
        { chars are compatible with stringdef }
        else if (param1.typ=orddef) and is_char(param1) and (param2.typ=stringdef) then
          result:=true
        { undefined def is compatible with all types }
        else if param2.typ=undefineddef then
          result:=true
        { sets require stricter checks }
        else if is_set(param2) then
          result:=equal_defs(param1,param2)
        else
          result:=param1.typ=param2.typ;
      end;

    function create_generic_constsym(fromdef:tdef;node:tnode;out prettyname:string):tconstsym;
      const
        undefinedname = 'undefined';
      var
        sym : tconstsym;
        setdef : tsetdef;
        enumsym : tsym;
        enumname : string;
        sp : pchar;
        ps : ^tconstset;
        pd : ^bestreal;
        i : integer;
      begin
        if node=nil then
          internalerror(2020011401);
        case node.nodetype of
          ordconstn:
            begin
              sym:=cconstsym.create_ord(undefinedname,constord,tordconstnode(node).value,fromdef);
              prettyname:=tostr(tordconstnode(node).value.svalue);
            end;
          stringconstn:
            begin
              getmem(sp,tstringconstnode(node).len+1);
              move(tstringconstnode(node).value_str^,sp^,tstringconstnode(node).len+1);
              sym:=cconstsym.create_string(undefinedname,conststring,sp,tstringconstnode(node).len,fromdef);
              prettyname:=''''+tstringconstnode(node).value_str+'''';
            end;
          realconstn:
            begin
              new(pd);
              pd^:=trealconstnode(node).value_real;
              sym:=cconstsym.create_ptr(undefinedname,constreal,pd,fromdef);
              prettyname:=realtostr(trealconstnode(node).value_real);
            end;
          setconstn:
            begin
              new(ps);
              ps^:=tsetconstnode(node).value_set^;
              sym:=cconstsym.create_ptr(undefinedname,constset,ps,fromdef);
              setdef:=tsetdef(tsetconstnode(node).resultdef);
              prettyname:='[';
              for i := setdef.setbase to setdef.setmax do
                if i in tsetconstnode(node).value_set^ then
                  begin
                    if setdef.elementdef.typ=enumdef then
                      enumsym:=tenumdef(setdef.elementdef).int2enumsym(i)
                    else
                      enumsym:=nil;
                    if assigned(enumsym) then
                      enumname:=enumsym.realname
                    else if setdef.elementdef.typ=orddef then
                      begin
                        if torddef(setdef.elementdef).ordtype=uchar then
                          enumname:=chr(i)
                        else
                          enumname:=tostr(i);
                      end
                    else
                      enumname:=tostr(i);
                    if length(prettyname) > 1 then
                      prettyname:=prettyname+','+enumname
                    else
                      prettyname:=prettyname+enumname;
                  end;
              prettyname:=prettyname+']';
            end;
          niln:
            begin
              { only "nil" is available for pointer constants }
              sym:=cconstsym.create_ord(undefinedname,constnil,0,fromdef);
              prettyname:='nil';
            end;
          else
            internalerror(2019021601);
        end;
        { the sym needs an owner for later checks so use the typeparam owner }
        sym.owner:=fromdef.owner;
        include(sym.symoptions,sp_generic_const);
        result:=sym;
      end;

    procedure maybe_add_waiting_unit(tt:tdef);
      var
        hmodule : tmodule;
      begin
        if not assigned(tt) or
            not (df_generic in tt.defoptions) then
          exit;

        hmodule:=find_module_from_symtable(tt.owner);
        if not assigned(hmodule) then
          internalerror(2012092401);

        if hmodule=current_module then
          exit;

        if hmodule.state<>ms_compiled then
          begin
{$ifdef DEBUG_UNITWAITING}
            Writeln('Unit ', current_module.modulename^,
              ' waiting for ', hmodule.modulename^);
{$endif DEBUG_UNITWAITING}
            if current_module.waitingforunit.indexof(hmodule)<0 then
              current_module.waitingforunit.add(hmodule);
            if hmodule.waitingunits.indexof(current_module)<0 then
              hmodule.waitingunits.add(current_module);
          end;
      end;


    procedure add_forward_generic_def(def:tdef;context:tspecializationcontext);
      var
        list : tfpobjectlist;
        fwdcontext : tspecializationcontext;
      begin
        if not is_implicit_pointer_object_type(def) then
          internalerror(2020070301);
        if not (oo_is_forward in tobjectdef(def).objectoptions) then
          internalerror(2020070302);
        if not assigned(tobjectdef(def).genericdef) then
          internalerror(2020070303);
        list:=tfpobjectlist(current_module.forwardgenericdefs.find(tobjectdef(def).genericdef.fulltypename));
        if not assigned(list) then
          begin
            list:=tfpobjectlist.create(true);
            current_module.forwardgenericdefs.add(tobjectdef(def).genericdef.fulltypename,list);
          end;
        fwdcontext:=context.getcopy;
        fwdcontext.forwarddef:=def;
        list.add(fwdcontext);
      end;


    function check_generic_constraints(genericdef:tstoreddef;paramlist:tfpobjectlist;poslist:tfplist):boolean;
      var
        i,j,
        intfcount : longint;
        formaldef,
        paradef : tstoreddef;
        genparadef : tdef;
        objdef,
        paraobjdef,
        formalobjdef : tobjectdef;
        intffound : boolean;
        filepos : tfileposinfo;
        is_const : boolean;
      begin
        { check whether the given specialization parameters fit to the eventual
          constraints of the generic }
        if not assigned(genericdef.genericparas) or (genericdef.genericparas.count=0) then
          internalerror(2012101001);
        if genericdef.genericparas.count<>paramlist.count then
          internalerror(2012101002);
        if paramlist.count<>poslist.count then
          internalerror(2012120801);
        result:=true;
        for i:=0 to genericdef.genericparas.count-1 do
          begin
            filepos:=pfileposinfo(poslist[i])^;
            paradef:=tstoreddef(get_generic_param_def(tsym(paramlist[i])));
            is_const:=tsym(paramlist[i]).typ=constsym;
            genparadef:=genericdef.get_generic_param_def(i);
            { validate const params }
            if not genericdef.is_generic_param_const(i) and is_const then
              begin
                MessagePos(filepos,type_e_mismatch);
                exit(false);
              end
            else if genericdef.is_generic_param_const(i) then
              begin
                { param type mismatch (type <> const) }
                 if genericdef.is_generic_param_const(i)<>is_const then
                   begin
                    MessagePos(filepos,type_e_mismatch);
                    exit(false);
                  end;
                { type constrained param doesn't match type }
                if not compare_generic_params(paradef,genericdef.get_generic_param_def(i),tconstsym(paramlist[i])) then
                  begin
                    MessagePos2(filepos,type_e_incompatible_types,FullTypeName(paradef,genparadef),FullTypeName(genparadef,paradef));
                    exit(false);
                  end;
              end;
            { test constraints for non-const params }
            if not genericdef.is_generic_param_const(i) then
              begin
                formaldef:=tstoreddef(ttypesym(genericdef.genericparas[i]).typedef);
                if formaldef.typ=undefineddef then
                  { the parameter is of unspecified type, so no need to check }
                  continue;
                if not (df_genconstraint in formaldef.defoptions) or
                    not assigned(formaldef.genconstraintdata) then
                  internalerror(2013021602);
                { undefineddef is compatible with anything }
                if formaldef.typ=undefineddef then
                  continue;
                if paradef.typ<>formaldef.typ then
                  begin
                    case formaldef.typ of
                      recorddef:
                        { delphi has own fantasy about record constraint
                          (almost non-nullable/non-nilable value type) }
                        if m_delphi in current_settings.modeswitches then
                          case paradef.typ of
                            floatdef,enumdef,orddef:
                              continue;
                            objectdef:
                              if tobjectdef(paradef).objecttype=odt_object then
                                continue
                              else
                                MessagePos(filepos,type_e_record_type_expected);
                            else
                              MessagePos(filepos,type_e_record_type_expected);
                          end
                        else
                          MessagePos(filepos,type_e_record_type_expected);
                      objectdef:
                        case tobjectdef(formaldef).objecttype of
                          odt_class,
                          odt_javaclass:
                            MessagePos1(filepos,type_e_class_type_expected,paradef.typename);
                          odt_interfacecom,
                          odt_interfacecorba,
                          odt_dispinterface,
                          odt_interfacejava:
                            MessagePos1(filepos,type_e_interface_type_expected,paradef.typename);
                          else
                            internalerror(2012101003);
                        end;
                      errordef:
                        { ignore }
                        ;
                      else
                        internalerror(2012101004);
                    end;
                    result:=false;
                  end
                else
                  begin
                    { the paradef types are the same, so do special checks for the
                      cases in which they are needed }
                    if formaldef.typ=objectdef then
                      begin
                        paraobjdef:=tobjectdef(paradef);
                        formalobjdef:=tobjectdef(formaldef);
                        if not (formalobjdef.objecttype in [odt_class,odt_javaclass,odt_interfacecom,odt_interfacecorba,odt_interfacejava,odt_dispinterface]) then
                          internalerror(2012101102);
                        if formalobjdef.objecttype in [odt_interfacecom,odt_interfacecorba,odt_interfacejava,odt_dispinterface] then
                          begin
                            { this is either a concerete interface or class type (the
                              latter without specific implemented interfaces) }
                            case paraobjdef.objecttype of
                              odt_interfacecom,
                              odt_interfacecorba,
                              odt_interfacejava,
                              odt_dispinterface:
                                begin
                                  if (oo_is_forward in paraobjdef.objectoptions) and
                                      (paraobjdef.objecttype=formalobjdef.objecttype) and
                                      (df_genconstraint in formalobjdef.defoptions) and
                                      (
                                        (formalobjdef.objecttype=odt_interfacecom) and
                                        (formalobjdef.childof=interface_iunknown)
                                      )
                                      or
                                      (
                                        (formalobjdef.objecttype=odt_interfacecorba) and
                                        (formalobjdef.childof=nil)
                                      ) then
                                    continue;
                                  if not def_is_related(paraobjdef,formalobjdef.childof) then
                                    begin
                                      MessagePos2(filepos,type_e_incompatible_types,paraobjdef.typename,formalobjdef.childof.typename);
                                      result:=false;
                                    end;
                                end;
                              odt_class,
                              odt_javaclass:
                                begin
                                  objdef:=paraobjdef;
                                  intffound:=false;
                                  while assigned(objdef) do
                                    begin
                                      for j:=0 to objdef.implementedinterfaces.count-1 do
                                        if timplementedinterface(objdef.implementedinterfaces[j]).intfdef=formalobjdef.childof then
                                          begin
                                            intffound:=true;
                                            break;
                                          end;
                                      if intffound then
                                        break;
                                      objdef:=objdef.childof;
                                    end;
                                  result:=intffound;
                                  if not result then
                                    MessagePos2(filepos,parser_e_class_doesnt_implement_interface,paraobjdef.typename,formalobjdef.childof.typename);
                                end;
                              else
                                begin
                                  MessagePos1(filepos,type_e_class_or_interface_type_expected,paraobjdef.typename);
                                  result:=false;
                                end;
                            end;
                          end
                        else
                          begin
                            { this is either a "class" or a concrete instance with
                              or without implemented interfaces }
                            if not (paraobjdef.objecttype in [odt_class,odt_javaclass]) then
                              begin
                                MessagePos1(filepos,type_e_class_type_expected,paraobjdef.typename);
                                result:=false;
                                continue;
                              end;
                            { for forward declared classes we allow pure TObject/class declarations }
                            if (oo_is_forward in paraobjdef.objectoptions) and
                                (df_genconstraint in formaldef.defoptions) then
                              begin
                                if (formalobjdef.childof=class_tobject) and
                                    not formalobjdef.implements_any_interfaces then
                                  continue;
                              end;
                            if assigned(formalobjdef.childof) and
                                not def_is_related(paradef,formalobjdef.childof) then
                              begin
                                MessagePos2(filepos,type_e_incompatible_types,paraobjdef.typename,formalobjdef.childof.typename);
                                result:=false;
                              end;
                            intfcount:=0;
                            for j:=0 to formalobjdef.implementedinterfaces.count-1 do
                              begin
                                objdef:=paraobjdef;
                                intffound:=false;
                                while assigned(objdef) do
                                  begin
                                    intffound:=assigned(
                                                 find_implemented_interface(objdef,
                                                   timplementedinterface(formalobjdef.implementedinterfaces[j]).intfdef
                                                 )
                                               );
                                    if intffound then
                                      break;
                                    objdef:=objdef.childof;
                                  end;
                                if intffound then
                                  inc(intfcount)
                                else
                                  MessagePos2(filepos,parser_e_class_doesnt_implement_interface,paraobjdef.typename,timplementedinterface(formalobjdef.implementedinterfaces[j]).intfdef.typename);
                              end;
                            if intfcount<>formalobjdef.implementedinterfaces.count then
                              result:=false;
                          end;
                      end;
                  end;
              end;
          end;
      end;

    function parse_generic_specialization_types_internal(paramlist:tfpobjectlist;poslist:tfplist;out prettyname,specializename:ansistring;parsedtype:tdef;parsedpos:tfileposinfo):boolean;
      var
        old_block_type : tblock_type;
        first : boolean;
        typeparam : tnode;
        parampos : pfileposinfo;
        tmpparampos : tfileposinfo;
        namepart : string;
        module : tmodule;
        constprettyname : string;
        validparam : boolean;
      begin
        result:=true;
        prettyname:='';
        constprettyname:='';
        if paramlist=nil then
          internalerror(2012061401);
        { set the block type to type, so that the parsed type are returned as
          ttypenode (e.g. classes are in non type-compatible blocks returned as
          tloadvmtaddrnode) }
        old_block_type:=block_type;
        { if parsedtype is set, then the first type identifer was already parsed
          (happens in inline specializations) and thus we only need to parse
          the remaining types and do as if the first one was already given }
        first:=not assigned(parsedtype);
        if assigned(parsedtype) then
          begin
            paramlist.Add(parsedtype.typesym);
            module:=find_module_from_symtable(parsedtype.owner);
            if not assigned(module) then
              internalerror(2016112801);
            namepart:='_$'+hexstr(module.moduleid,8)+'$$'+parsedtype.unique_id_str;
            specializename:='$'+namepart;
            prettyname:=parsedtype.fullownerhierarchyname(true)+parsedtype.typesym.prettyname;
            if assigned(poslist) then
              begin
                New(parampos);
                parampos^:=parsedpos;
                poslist.add(parampos);
              end;
          end
        else
          specializename:='$';
        while not (token in [_GT,_RSHARPBRACKET]) do
          begin
            { "first" is set to false at the end of the loop! }
            if not first then
              consume(_COMMA);
            block_type:=bt_type;
            tmpparampos:=current_filepos;
            typeparam:=factor(false,[ef_accept_equal]);
            { determine if the typeparam node is a valid type or const }
            validparam:=typeparam.nodetype in tgeneric_param_nodes;
            if validparam then
              begin
                if tstoreddef(typeparam.resultdef).is_generic and
                    (
                      not parse_generic or
                      not defs_belong_to_same_generic(typeparam.resultdef,current_genericdef)
                    ) then
                  Message(parser_e_no_generics_as_params);
                if assigned(poslist) then
                  begin
                    New(parampos);
                    parampos^:=tmpparampos;
                    poslist.add(parampos);
                  end;
                if typeparam.resultdef.typ<>errordef then
                  begin
                    if (typeparam.nodetype=typen) and not assigned(typeparam.resultdef.typesym) then
                      message(type_e_generics_cannot_reference_itself)
                    else if (typeparam.resultdef.typ<>errordef) then
                      begin
                        { all non-type nodes are considered const }
                        if typeparam.nodetype<>typen then
                          paramlist.Add(create_generic_constsym(typeparam.resultdef,typeparam,constprettyname))
                        else
                          begin
                            constprettyname:='';
                            paramlist.Add(typeparam.resultdef.typesym);
                          end;
                        make_prettystring(typeparam.resultdef,first,constprettyname,prettyname,specializename);
                      end;
                  end
                else
                  begin
                    result:=false;
                  end;
              end
            else
              begin
                Message(type_e_type_id_expected);
                result:=false;
              end;
            typeparam.free;
            first:=false;
          end;
        block_type:=old_block_type;
      end;


    function parse_generic_specialization_types(paramlist:tfpobjectlist;poslist:tfplist;out prettyname,specializename:ansistring):boolean;
      var
        dummypos : tfileposinfo;
      begin
        FillChar(dummypos, SizeOf(tfileposinfo), 0);
        result:=parse_generic_specialization_types_internal(paramlist,poslist,prettyname,specializename,nil,dummypos);
      end;


    procedure generate_specialization(var tt:tdef;enforce_unit:boolean;parse_class_parent:boolean;const _prettyname:string);
      var
        dummypos : tfileposinfo;
      begin
        FillChar(dummypos, SizeOf(tfileposinfo), 0);
        generate_specialization(tt,enforce_unit,parse_class_parent,_prettyname,nil,'',dummypos);
      end;


    function finalize_specialization(var pd:tprocdef;spezcontext:tspecializationcontext):boolean;
      var
        def : tdef;
      begin
        result:=false;
        if assigned(spezcontext) then
          begin
            if not (df_generic in pd.defoptions) then
              internalerror(2015060301);
            { check whether the given parameters are compatible
              to the def's constraints }
            if not check_generic_constraints(pd,spezcontext.paramlist,spezcontext.poslist) then
              exit;
            def:=generate_specialization_phase2(spezcontext,pd,false,'');
            case def.typ of
              errordef:
                { do nothing }
                ;
              procdef:
                pd:=tprocdef(def);
              else
                internalerror(2015070303);
            end;
          end;
        result:=true;
      end;


    procedure transfer_unnamed_symbols(owner:tsymtable;unnamed_syms:tfplist);
      var
        i : integer;
        sym : tsym;
      begin
        for i:=0 to unnamed_syms.count-1 do
          begin
            sym:=tsym(unnamed_syms[i]);
            sym.ChangeOwnerAndName(owner,sym.realname);
          end;
        unnamed_syms.clear;
      end;


    function try_implicit_specialization(sym:tsym;para:tnode;pdoverloadlist:tfpobjectlist;var unnamed_syms:tfplist;var first_procsym:tsym;var hasoverload:boolean):boolean;

      { hash key for generic parameter lookups }
      function generic_param_hash(def:tdef):string;inline;
        begin
          result:=def.typename;
        end;

      { returns true if the def a literal array such as [1,2,3] and not a shortstring }
      function is_array_literal(def:tdef):boolean;
        begin
          result:=(def.typ=arraydef) and not is_conststring_array(def);
        end;

      { makes the specialization context from the generic proc def and generic params }
      procedure generate_implicit_specialization(out context:tspecializationcontext;genericdef:tprocdef;genericparams:tfphashlist);
        var
          parsedpos:tfileposinfo;
          poslist:tfplist;
          i: longint;
          paramtype: ttypesym;
          parampos : pfileposinfo;
          tmpparampos : tfileposinfo;
          paramname: string;
        begin
          context:=tspecializationcontext.create;
          fillchar(parsedpos,sizeof(parsedpos),0);
          poslist:=context.poslist;
          tmpparampos:=current_filepos;
          if genericparams.count<>genericdef.genericparas.count then
            internalerror(2021020901);
          for i:=0 to genericparams.count-1 do
            begin
              paramname:=generic_param_hash(ttypesym(genericdef.genericparas[i]).typedef);
              paramtype:=ttypesym(genericparams.find(paramname));
              if not assigned(paramtype) then
                internalerror(2021020902);
              new(parampos);
              parampos^:=tmpparampos;
              poslist.add(parampos);
              context.paramlist.Add(paramtype);
              make_prettystring(paramtype.typedef,i=0,'',context.prettyname,context.specializename);
            end;
          context.genname:=genericdef.procsym.realname;
        end;

      { specialization context parameter lists require a typesym so we need
        to generate a placeholder for unnamed constant types like
        short strings, open arrays, function pointers etc... }
      function create_unnamed_typesym(def:tdef):tsym;
        var
          newtype: tsym;
        begin
          newtype:=nil;
          if is_conststring_array(def) then
            begin
              { for constant strings we need to respect various modeswitches }
              if (cs_refcountedstrings in current_settings.localswitches) then
                begin
                  if m_default_unicodestring in current_settings.modeswitches then
                    newtype:=cunicodestringtype.typesym
                  else
                    newtype:=cansistringtype.typesym;
                end
              else
                newtype:=cshortstringtype.typesym;
            end
          else if def.typ=stringdef then
            newtype:=tstringdef(def).get_default_string_type.typesym
          else
            begin
              if is_array_constructor(def) then
                begin
                  { array constructor is not a valid parameter type; getreusable
                    avoids creating multiple implementations for calls with the
                    same number of array elements of a particular type }
                  def:=carraydef.getreusable(tarraydef(def).elementdef,tarraydef(def).highrange-tarraydef(def).lowrange+1);
                end;
              newtype:=ctypesym.create(def.fullownerhierarchyname(false)+typName[def.typ]+'$'+def.unique_id_str,def);
              include(newtype.symoptions,sp_generic_unnamed_type);
              newtype.owner:=def.owner;
              { ensure that there's no warning }
              newtype.refs:=1;
            end;
          if not assigned(newtype) then
            internalerror(2021020904);
          result:=newtype;
        end;

      { searches for the generic param in specializations }
      function find_param_in_specialization(owner:tprocdef;genericparam:ttypesym;def:tstoreddef):boolean;
        var
          parasym: ttypesym;
          k, i: integer;
        begin
          result:=false;
          for i:=0 to def.genericparas.count-1 do
            begin
              parasym:=ttypesym(def.genericparas[i]);
              { the generic param must have a named typesym }
              if not assigned(parasym.typedef.typesym) then
                internalerror(2021020907);
              { recurse into inline specialization }
              if tstoreddef(parasym.typedef).is_specialization then
                begin
                  result:=find_param_in_specialization(owner,genericparam,tstoreddef(parasym.typedef));
                  if result then
                    exit;
                end
              else if (genericparam=parasym.typedef.typesym) and owner.is_generic_param(parasym.typedef) then
                exit(true);
            end;
        end;

      { searches for the generic param in arrays }
      function find_param_in_array(owner:tprocdef;genericparam:ttypesym;def:tarraydef):boolean;
        var
          elementdef:tstoreddef;
        begin
          elementdef:=tstoreddef(def.elementdef);
          { recurse into multi-dimensional array }
          if elementdef.typ=arraydef then
            result:=find_param_in_array(owner,genericparam,tarraydef(elementdef))
          { something went wrong during parsing and the element is invalid }
          else if elementdef.typ=errordef then
            result:=false
          else
            begin
              { the element must have a named typesym }
              if not assigned(elementdef.typesym) then
                internalerror(2021020906);
              result:=(genericparam=elementdef.typesym) and owner.is_generic_param(elementdef);
            end;
        end;

      { tests if the generic param is used in the parameter list }
      function is_generic_param_used(owner:tprocdef;genericparam:ttypesym;paras:tfplist):boolean;
        var
          paravar:tparavarsym;
          i: integer;
        begin
          result:=false;
          for i:=0 to paras.count-1 do
            begin
              paravar:=tparavarsym(paras[i]);

              { handle array types by using element types (for example: array of T) }
              if paravar.vardef.typ=arraydef then
                result:=find_param_in_array(owner,genericparam,tarraydef(paravar.vardef))
              { for specializations check search in generic params }
              else if tstoreddef(paravar.vardef).is_specialization then
                result:=find_param_in_specialization(owner,genericparam,tstoreddef(paravar.vardef))
              { something went wrong during parsing and the parameter is invalid }
              else if paravar.vardef.typ=errordef then
                exit(false)
              else
                begin
                  if not assigned(paravar.vardef.typesym) then
                    internalerror(2021020905);
                  result:=(genericparam=paravar.vardef.typesym) and owner.is_generic_param(paravar.vardef)
                end;

              { exit if we find a used parameter }
              if result then
                exit;
            end;
        end;

      { handle generic specializations by using generic params from caller
        to specialize the target. for example "TRec<Integer>" can use "Integer"
        to specialize "TRec<T>" with "Integer" for "T". }
      procedure handle_specializations(genericparams:tfphashlist;target_def,caller_def:tstoreddef);
        var
          i,
          index : integer;
          key : string;
          target_param,
          caller_param : ttypesym;
        begin
          { the target and the caller must the same generic def 
            with the same set of generic parameters }
          if target_def.genericdef<>caller_def.genericdef then
            internalerror(2021020909);

          for i:=0 to target_def.genericparas.count-1 do
            begin
              target_param:=ttypesym(target_def.genericparas[i]);
              caller_param:=ttypesym(caller_def.genericparas[i]);

              { reject generics with constants }
              if (target_param.typ=constsym) or (caller_param.typ=constsym) then
                exit;

              key:=generic_param_hash(target_param.typedef);

              { the generic param is already used }
              index:=genericparams.findindexof(key);
              if index>=0 then
                continue;

              { add the type to the generic params }
              genericparams.add(key,caller_param);
            end;
        end;

      { specialize arrays by using element types but arrays may be multi-dimensional 
        so we need to examine the caller/target pairs recursively in order to
        verify the dimensionality is equal }
      function handle_arrays(owner:tprocdef;target_def,caller_def:tarraydef;out target_element,caller_element:tdef):boolean;
        begin
          { the target and the caller are both arrays and the target is a 
            specialization so we can recurse into the targets element def }
          if is_array_literal(target_def.elementdef) and 
            is_array_literal(caller_def.elementdef) and 
            target_def.is_specialization then
            result:=handle_arrays(owner,tarraydef(target_def.elementdef),tarraydef(caller_def.elementdef),target_element,caller_element)
          else
            begin
              { the caller is an array which means the dimensionality is unbalanced
                and thus the arrays are compatible }
              if is_array_literal(caller_def.elementdef) then
                exit(false);
              { if the element is a generic param then return this type
                along with the caller element type at the same level }
              result:=owner.is_generic_param(target_def.elementdef);
              if result then
                begin
                  target_element:=target_def.elementdef;
                  caller_element:=caller_def.elementdef;
                end;
            end;
        end;

      { handle procvars by using the parameters from the caller to specialize
        the parameters of the target generic procedure specialization. for example:

          type generic TProc<S> = procedure(value: S);
          generic procedure Run<T>(proc: specialize TProc<T>);
          procedure DoCallback(value: integer);
          Run(@DoCallback);

        will specialize as Run<integer> because the signature
        of DoCallback() matches TProc<S> so we can specialize "S"
        with "integer", as they are both parameter #1
      }

      function handle_procvars(genericparams:tfphashlist;callerparams:tfplist;target_def:tdef;caller_def:tdef):boolean;
        var
          newparams : tfphashlist;

        procedure handle_generic_param(targetparadef,callerparadef:tdef);
          var
            key : string;
            index : integer;
          begin
            if not assigned(callerparadef.typesym) then
              internalerror(2021020908);

            key:=generic_param_hash(targetparadef);

            { the generic param must not already be used }
            index:=genericparams.findindexof(key);
            if index<0 then
              begin
                { add the type to the list }
                index:=newparams.findindexof(key);
                if index<0 then
                  newparams.add(key,callerparadef.typesym);
              end;
          end;

        var
          i,j : integer;
          paravar : tparavarsym;
          target_proc,
          caller_proc : tprocvardef;
          target_proc_para,
          caller_proc_para : tparavarsym;
          valid_params : integer;
        begin
          result := false;

          target_proc:=tprocvardef(target_def);
          caller_proc:=tprocvardef(caller_def);

          { parameter count must match exactly
            currently default values are not considered }
          if target_proc.paras.count<>caller_proc.paras.count then
            exit;

          { a mixture of functions and procedures is not allowed }
          if (not assigned(target_proc.returndef) or is_void(target_proc.returndef)) xor
              (not assigned(caller_proc.returndef) or is_void(caller_proc.returndef)) then
            exit;

          { reject generics with constants }
          for i:=0 to target_proc.genericdef.genericparas.count-1 do
            if tsym(target_proc.genericdef.genericparas[i]).typ=constsym then
              exit;

          newparams:=tfphashlist.create;
          valid_params:=0;

          for i:=0 to target_proc.paras.count-1 do
            begin
              target_proc_para:=tparavarsym(target_proc.paras[i]);
              caller_proc_para:=tparavarsym(caller_proc.paras[i]);

              { the parameters are not compatible }
              if compare_defs(caller_proc_para.vardef,target_proc_para.vardef,nothingn)=te_incompatible then
                begin
                  newparams.free;
                  exit(false);
                end;

              if sp_generic_para in target_proc_para.vardef.typesym.symoptions then
                begin
                  paravar:=tparavarsym(tprocvardef(target_proc.genericdef).paras[i]);

                  { find the generic param name in the generic def parameters }
                  j:=target_proc.genericdef.genericparas.findindexof(paravar.vardef.typesym.name);

                  handle_generic_param(ttypesym(target_proc.genericparas[j]).typedef,caller_proc_para.vardef);
                end;

              inc(valid_params);
            end;

          if assigned(target_proc.returndef) and not is_void(target_proc.returndef) then
            begin
              { or check for exact? }
              if compare_defs(caller_proc.returndef,target_proc.returndef,nothingn)<te_equal then
                begin
                  newparams.free;
                  exit(false);
                end;

              if sp_generic_para in target_proc.returndef.typesym.symoptions then
                begin
                  handle_generic_param(target_proc.returndef,caller_proc.returndef);
                end;
            end;

          { if the count of valid params matches the target then
            transfer the temporary params to the actual params }
          result:=valid_params=target_proc.paras.count;
          if result then
            for i := 0 to newparams.count-1 do
              genericparams.add(newparams.nameofindex(i),newparams[i]);

          newparams.free;
        end;

      function maybe_inherited_specialization(givendef,desireddef:tstoreddef;out basedef:tstoreddef):boolean;
        begin
          result:=false;
          basedef:=nil;
          if givendef.typ<>objectdef then
            begin
              result:=givendef.is_specialization and (givendef.genericdef=desireddef.genericdef);
              if result then
                basedef:=givendef;
            end
          else
            begin
              while assigned(givendef) do
                begin
                  if givendef.is_specialization and (givendef.genericdef=desireddef.genericdef) then
                    begin
                      basedef:=givendef;
                      result:=true;
                      break;
                    end;

                  givendef:=tobjectdef(givendef).childof;
                end;
            end;
        end;

      { compare generic parameters <T> with call node parameters. }
      function is_possible_specialization(callerparams:tfplist;genericdef:tprocdef;out unnamed_syms:tfplist;out genericparams:tfphashlist):boolean;
        var
          i,j,
          count : integer;
          paravar : tparavarsym;
          base_def : tstoreddef;
          target_def,
          caller_def : tdef;
          target_key : string;
          index : integer;
          paras : tfplist;
          target_element,
          caller_element : tdef;
          required_param_count : integer;
          adef : tarraydef;
        begin
          result:=false;
          paras:=nil;
          genericparams:=nil;
          required_param_count:=0;
          unnamed_syms:=nil;

          { first perform a check to reject generics with constants }
          for i:=0 to genericdef.genericparas.count-1 do
            if tsym(genericdef.genericparas[i]).typ=constsym then
              exit;

          { build list of visible target function parameters }
          paras:=tfplist.create;
          for i:=0 to genericdef.paras.count-1 do
            begin
              paravar:=tparavarsym(genericdef.paras[i]);
              { ignore hidden parameters }
              if vo_is_hidden_para in paravar.varoptions then
                continue;
              paras.add(paravar);

              { const non-default parameters are required }
              if not assigned(paravar.defaultconstsym) then
                inc(required_param_count);
            end;

          { not enough parameters were supplied }
          if callerparams.count<required_param_count then
            begin
              paras.free;
              exit;
            end;

          { check to make sure the generic parameters are all used 
            at least once in the  caller parameters. }
          count:=0;
          for i:=0 to genericdef.genericparas.count-1 do
            if is_generic_param_used(genericdef,ttypesym(genericdef.genericparas[i]),paras) then
              inc(count);

          if count<genericdef.genericparas.count then
            begin
              paras.free;
              exit;
            end;

          genericparams:=tfphashlist.create;
          for i:=0 to callerparams.count-1 do
            begin
              caller_def:=ttypesym(callerparams[i]).typedef;

              { caller parameter exceeded the possible parameters }
              if i=paras.count then
                begin
                  genericparams.free;
                  paras.free;
                  exit;
                end;

              target_def:=tparavarsym(paras[i]).vardef;
              target_key:='';

              { strings are compatible with "array of T" so we 
                need to use the element type for specialization }
              if is_stringlike(caller_def) and
                is_array_literal(target_def) and
                genericdef.is_generic_param(tarraydef(target_def).elementdef) then
                begin
                  target_def:=tarraydef(target_def).elementdef;
                  target_key:=generic_param_hash(target_def);
                  caller_def:=chartype_for_stringlike(caller_def);
                end
              { non-uniform array constructors (i.e. array of const) are not compatible 
                with normal arrays like "array of T" so we reject them }
              else if is_array_literal(target_def) and
                (caller_def.typ=arraydef) and 
                (ado_IsConstructor in tarraydef(caller_def).arrayoptions) and
                (ado_IsArrayOfConst in tarraydef(caller_def).arrayoptions) then
                begin
                  continue;
                end
              { handle generic arrays }
              else if is_array_literal(caller_def) and
                is_array_literal(target_def) and
                handle_arrays(genericdef,tarraydef(target_def),tarraydef(caller_def),target_element,caller_element) then
                begin
                  target_def:=target_element;
                  caller_def:=caller_element;
                  target_key:=generic_param_hash(target_def);
                end
              { handle generic procvars }
              else if (caller_def.typ=procvardef) and 
                (target_def.typ=procvardef) and 
                tprocvardef(target_def).is_specialization and
                handle_procvars(genericparams,callerparams,target_def,caller_def) then
                begin
                  continue;
                end
              { handle specialized objects by taking the base class as the type to specialize }    
              else if is_class_or_object(caller_def) and 
                is_class_or_object(target_def) and
                genericdef.is_generic_param(target_def) then
                begin
                  target_key:=generic_param_hash(target_def);
                  target_def:=tobjectdef(target_def).childof;
                end
              { handle generic specializations }
              else if tstoreddef(target_def).is_specialization and
                maybe_inherited_specialization(tstoreddef(caller_def),tstoreddef(target_def),base_def) then
                begin
                  handle_specializations(genericparams,tstoreddef(target_def),base_def);
                  continue;
                end
              { handle all other generic params }
              else if target_def.typ=undefineddef then
                target_key:=generic_param_hash(target_def);

              { the param doesn't have a generic key which means we don't need to consider it }
              if target_key='' then
                continue;

              { the generic param is already used }
              index:=genericparams.findindexof(target_key);
              if index>=0 then
                continue;

              { the caller type may not have a typesym so we need to create an unnamed one }
              if not assigned(caller_def.typesym) then
                begin
                  sym:=create_unnamed_typesym(caller_def);
                  { add the unnamed sym to the list but only it was allocated manually }
                  if sym.owner=caller_def.owner then
                    begin
                      if not assigned(unnamed_syms) then
                        unnamed_syms:=tfplist.create;
                      unnamed_syms.add(sym);
                    end;
                  genericparams.add(target_key,sym);
                end
              else
                genericparams.add(target_key,caller_def.typesym);
            end;

          { if the parameter counts match then the specialization is possible }
          result:=genericparams.count=genericdef.genericparas.count;

          { cleanup }
          paras.free;
          if not result then
            genericparams.free;
        end;

      { make an ordered list of parameters from the caller }
      function make_param_list(dummysym:tsym;para:tnode;var unnamed_syms:tfplist):tfplist;
        var
          pt : tcallparanode;
          paradef : tdef;
          sym : tsym;
          i : integer;
        begin
          result:=tfplist.create;
          pt:=tcallparanode(para);
          while assigned(pt) do
            begin
              paradef:=pt.paravalue.resultdef;
              { unnamed parameter types can not be specialized }
              if not assigned(paradef.typesym) then
                begin
                  sym:=create_unnamed_typesym(paradef);
                  result.insert(0,sym);
                  { add the unnamed sym to the list but only if it was allocated manually }
                  if sym.owner=paradef.owner then
                    begin
                      if not assigned(unnamed_syms) then
                        unnamed_syms:=tfplist.create;
                      unnamed_syms.add(sym);
                    end;
                end
              else
                result.insert(0,paradef.typesym);
              pt:=tcallparanode(pt.nextpara);
            end;
        end;

      var
        i,j,k : integer;
        srsym : tprocsym;
        callerparams : tfplist;
        pd : tprocdef;
        dummysym : tprocsym;
        genericparams : tfphashlist;
        spezcontext : tspecializationcontext;
        pd_unnamed_syms : tfplist;
      begin
        result:=false;
        spezcontext:=nil;
        genericparams:=nil;
        dummysym:=tprocsym(sym);
        callerparams:=make_param_list(dummysym,para,unnamed_syms);

        { failed to build the parameter list }
        if not assigned(callerparams) then
          exit;

        for i:=0 to dummysym.genprocsymovlds.count-1 do
          begin
            srsym:=tprocsym(dummysym.genprocsymovlds[i]);
            for j:=0 to srsym.ProcdefList.Count-1 do
              begin
                pd:=tprocdef(srsym.ProcdefList[j]);
                if is_possible_specialization(callerparams,pd,pd_unnamed_syms,genericparams) then
                  begin
                    generate_implicit_specialization(spezcontext,pd,genericparams);
                    genericparams.free;
                    { finalize the specialization so it can be added to the list of overloads }
                    if not finalize_specialization(pd,spezcontext) then
                      begin
                        spezcontext.free;
                        continue;
                      end;
                    { handle unnamed syms used by the specialization }
                    if pd_unnamed_syms<>nil then
                      begin
                        transfer_unnamed_symbols(pd.owner,pd_unnamed_syms);
                        pd_unnamed_syms.free;
                      end;
                    pdoverloadlist.add(pd);
                    spezcontext.free;
                    if po_overload in pd.procoptions then
                      hasoverload:=true;
                    { store first procsym found }
                    if not assigned(first_procsym) then
                      first_procsym:=srsym;
                    result:=true;
                  end
                else
                  begin
                    { the specialization was not chosen so clean up any unnamed syms }
                    if pd_unnamed_syms<>nil then
                      begin
                        for k:=0 to pd_unnamed_syms.count-1 do
                          tsym(pd_unnamed_syms[k]).free;
                        pd_unnamed_syms.free;
                      end;
                  end;
              end;
          end;
        callerparams.free;
      end;

    function generate_specialization_phase1(out context:tspecializationcontext;genericdef:tdef;enforce_unit:boolean):tdef;
      var
        dummypos : tfileposinfo;
{$push}
{$warn 5036 off}
      begin
        result:=generate_specialization_phase1(context,genericdef,enforce_unit,nil,'',nil,dummypos);
      end;
{$pop}


    function generate_specialization_phase1(out context:tspecializationcontext;genericdef:tdef;enforce_unit:boolean;const symname:string;symtable:tsymtable):tdef;
      var
        dummypos : tfileposinfo;
{$push}
{$warn 5036 off}
      begin
        result:=generate_specialization_phase1(context,genericdef,enforce_unit,nil,symname,symtable,dummypos);
      end;
{$pop}


    function generate_specialization_phase1(out context:tspecializationcontext;genericdef:tdef;enforce_unit:boolean;parsedtype:tdef;const symname:string;symtable:tsymtable;parsedpos:tfileposinfo):tdef;
      var
        found,
        err : boolean;
        i,
        gencount : longint;
        countstr,genname,ugenname,prettygenname: string;
        tmpstack : tfpobjectlist;
        symowner : tsymtable;
        hmodule : tmodule;
      begin
        context:=nil;
        result:=nil;

        { either symname must be given or genericdef needs to be valid }
        if (symname='') and
            (not assigned(genericdef) or
              (
                (genericdef.typ<>procdef) and
                (
                  not assigned(genericdef.typesym) or
                  (genericdef.typesym.typ<>typesym)
                ) and
                (
                  (genericdef.typ<>objectdef) or
                  not (oo_is_forward in tobjectdef(genericdef).objectoptions)
                )
              ) or
              (
                (genericdef.typ=procdef) and
                (
                  not assigned(tprocdef(genericdef).procsym) or
                  (tprocdef(genericdef).procsym.typ<>procsym)
                )
              )
            ) then
          begin
            internalerror(2019112401);
          end;

        if not assigned(parsedtype) and not try_to_consume(_LT) then
          begin
            consume(_LSHARPBRACKET);
            { handle "<>" }
            if (token=_GT) or (token=_RSHARPBRACKET) then
              begin
                Message(type_e_type_id_expected);
                if not try_to_consume(_GT) then
                  try_to_consume(_RSHARPBRACKET);
                result:=generrordef;
                exit;
              end;
          end;

        context:=tspecializationcontext.create;

        { Parse type parameters }
        err:=not parse_generic_specialization_types_internal(context.paramlist,context.poslist,context.prettyname,context.specializename,parsedtype,parsedpos);
        if err then
          begin
            if not try_to_consume(_GT) then
              try_to_consume(_RSHARPBRACKET);
            context.free;
            context:=nil;
            result:=generrordef;
            exit;
          end;

        { use the name of the symbol as procvars return a user friendly version
          of the name }
        if symname='' then
          begin
            if genericdef.typ=procdef then
              genname:=tprocdef(genericdef).procsym.realname
            else if assigned(genericdef.typesym) then
              genname:=ttypesym(genericdef.typesym).realname
            else if (genericdef.typ=objectdef) and (oo_is_forward in tobjectdef(genericdef).objectoptions) then
              genname:=tobjectdef(genericdef).objrealname^
            else
              internalerror(2020071201);
          end
        else
          genname:=symname;

        { in case of non-Delphi mode the type name could already be a generic
          def (but maybe the wrong one) }
        if assigned(genericdef) and
            ([df_generic,df_specialization]*genericdef.defoptions<>[]) then
          begin
            { remove the type count suffix from the generic's name }
            for i:=Length(genname) downto 1 do
              if genname[i]='$' then
                begin
                  genname:=copy(genname,1,i-1);
                  break;
                end;
            { in case of a specialization we've only reached the specialization
              checksum yet }
            if df_specialization in genericdef.defoptions then
              for i:=length(genname) downto 1 do
                if genname[i]='$' then
                  begin
                    genname:=copy(genname,1,i-1);
                    break;
                  end;
          end
        else
          begin
            split_generic_name(genname,ugenname,gencount);
            if genname<>ugenname then
              genname:=ugenname;
          end;

        { search a generic with the given count of params }
        countstr:='';
        str(context.paramlist.Count,countstr);

        prettygenname:=genname;
        genname:=genname+'$'+countstr;
        ugenname:=upper(genname);

        context.genname:=genname;

        if assigned(genericdef) then
          symowner:=genericdef.owner
        else
          symowner:=symtable;

        if assigned(symowner) and (symowner.symtabletype in [objectsymtable,recordsymtable]) then
          begin
            if symowner.symtabletype = objectsymtable then
              found:=searchsym_in_class(tobjectdef(symowner.defowner),tobjectdef(symowner.defowner),ugenname,context.sym,context.symtable,[])
            else
              found:=searchsym_in_record(tabstractrecorddef(symowner.defowner),ugenname,context.sym,context.symtable);
            if not found then
              found:=searchsym(ugenname,context.sym,context.symtable);
          end
        else if enforce_unit then
          begin
            if not assigned(symowner) then
              internalerror(2022102101);
            if not (symowner.symtabletype in [globalsymtable,recordsymtable]) then
              internalerror(2022102102);
            hmodule:=find_module_from_symtable(symowner);
            if not assigned(hmodule) then
              internalerror(2022102103);
            found:=searchsym_in_module(hmodule,ugenname,context.sym,context.symtable);
          end
        else
          found:=searchsym(ugenname,context.sym,context.symtable);

        if found and (context.sym.typ=absolutevarsym) and
            (vo_is_funcret in tabstractvarsym(context.sym).varoptions) then
          begin
            { we found the function result alias of a generic function; go up the
              symbol stack *before* this alias was inserted, so that we can
              (hopefully) find the correct generic symbol }
            tmpstack:=tfpobjectlist.create(false);
            while assigned(symtablestack.top) do
              begin
                tmpstack.Add(symtablestack.top);
                symtablestack.pop(symtablestack.top);
                if tmpstack.Last=context.symtable then
                  break;
              end;
            if not assigned(symtablestack.top) then
              internalerror(2019123001);
            found:=searchsym(ugenname,context.sym,context.symtable);
            for i:=tmpstack.count-1 downto 0 do
              symtablestack.push(tsymtable(tmpstack[i]));
            tmpstack.free;
          end;

        if not found or not (context.sym.typ in [typesym,procsym]) then
          begin
            identifier_not_found(prettygenname);
            if not try_to_consume(_GT) then
              try_to_consume(_RSHARPBRACKET);
            context.free;
            context:=nil;
            result:=generrordef;
            exit;
          end;

        { we've found the correct def }
        if context.sym.typ=typesym then
          result:=tstoreddef(ttypesym(context.sym).typedef)
        else
          begin
            if tprocsym(context.sym).procdeflist.count=0 then
              internalerror(2015061203);
            result:=tstoreddef(tprocsym(context.sym).procdefList[0]);
          end;

        if not try_to_consume(_GT) then
          consume(_RSHARPBRACKET);
      end;

    function generate_specialization_phase2(context:tspecializationcontext;genericdef:tstoreddef;parse_class_parent:boolean;const _prettyname:ansistring):tdef;

        procedure unset_forwarddef(def: tdef);
          var
            st : TSymtable;
            i : longint;
          begin
            case def.typ of
              procdef:
                tprocdef(def).forwarddef:=false;
              objectdef,
              recorddef:
                begin
                  st:=def.getsymtable(gs_record);
                  for i:=0 to st.deflist.count-1 do
                    unset_forwarddef(tdef(st.deflist[i]));
                end;
              else
                ;
            end;
          end;

        procedure retrieve_genericdef_or_procsym(sym:tsym;out gendef:tdef;out psym:tsym);
          var
            i : longint;
          begin
            gendef:=nil;
            psym:=nil;
            case sym.typ of
              typesym:
                begin
                  gendef:=ttypesym(sym).typedef
                end;
              procsym:
                begin
                  for i:=0 to tprocsym(sym).procdeflist.count-1 do
                    if tstoreddef(tprocsym(sym).procdeflist[i]).genericdef=genericdef then
                      begin
                        gendef:=tdef(tprocsym(sym).procdeflist[i]);
                        break;
                      end;
                  psym:=sym;
                end
              else
                internalerror(200710171);
            end;
          end;

        function find_in_hierarchy(def:tdef;generictypelist:tfphashobjectlist):tdef;
          var
            paramdef1,
            paramdef2 : tdef;
            allequal : boolean;
            i : longint;
          begin
            result:=nil;
            while assigned(def) do
              begin
                if (df_generic in def.defoptions) and (def=genericdef) then
                  begin
                    result:=def;
                    break;
                  end;
                { the following happens when a routine with its parent struct
                  as parameter is specialized as a parameter or result of a
                  generic function }
                if (df_specialization in def.defoptions) and (tstoreddef(def).genericdef=genericdef) then
                  begin
                    if tstoreddef(def).genericparas.count=generictypelist.count then
                      begin
                        allequal:=true;
                        for i:=0 to generictypelist.count-1 do
                          begin
                            if tsym(generictypelist[i]).typ<>tsym(tstoreddef(def).genericparas[i]).typ then
                              begin
                                allequal:=false;
                                break;
                              end;
                            if tsym(generictypelist[i]).typ=constsym then
                              paramdef1:=tconstsym(generictypelist[i]).constdef
                            else
                              paramdef1:=ttypesym(generictypelist[i]).typedef;
                            if tsym(tstoreddef(def).genericparas[i]).typ=constsym then
                              paramdef2:=tconstsym(tstoreddef(def).genericparas[i]).constdef
                            else
                              paramdef2:=ttypesym(tstoreddef(def).genericparas[i]).typedef;
                            if not equal_defs(paramdef1,paramdef2) then
                              begin
                                allequal:=false;
                                break;
                              end;
                            if (tsym(generictypelist[i]).typ=constsym) and
                                (
                                  (tconstsym(generictypelist[i]).consttyp<>tconstsym(tstoreddef(def).genericparas[i]).consttyp) or
                                  not same_constvalue(tconstsym(generictypelist[i]).consttyp,tconstsym(generictypelist[i]).value,tconstsym(tstoreddef(def).genericparas[i]).value)
                                ) then
                                begin
                                  allequal:=false;
                                  break;
                                end;
                          end;
                        if allequal then
                          begin
                            result:=def;
                            break;
                          end;
                      end;
                  end;
                if assigned(def.owner) then
                  def:=tstoreddef(def.owner.defowner)
                else
                  def:=nil;
              end;
          end;

      var
        finalspecializename,
        ufinalspecializename : tidstring;
        hierarchy,
        prettyname : ansistring;
        generictypelist : tfphashobjectlist;
        srsymtable,
        specializest : tsymtable;
        hashedid : thashedidstring;
        tempst : tglobalsymtable;
        tsrsym : ttypesym;
        psym,
        srsym : tsym;
        flags : thccflags;
        paramdef1,
        paramdef2,
        def : tdef;
        old_block_type : tblock_type;
        state : tspecializationstate;
        old_current_structdef : tabstractrecorddef;
        old_current_specializedef,
        old_current_genericdef : tstoreddef;
        old_current_procinfo : tprocinfo;
        old_module_procinfo : tobject;
        hmodule : tmodule;
        oldcurrent_filepos : tfileposinfo;
        recordbuf : tdynamicarray;
        hadtypetoken : boolean;
        i,
        replaydepth : longint;
        item : tobject;
        allequal,
        hintsprocessed : boolean;
        pd : tprocdef;
        pdflags : tpdflags;
        ppflags : tparse_proc_flags;
      begin
        if not assigned(context) then
          internalerror(2015052203);

        result:=nil;

        pd:=nil;
        hmodule:=nil;

        if not check_generic_constraints(genericdef,context.paramlist,context.poslist) then
          begin
            { the parameters didn't fit the constraints, so don't continue with the
              specialization }
            result:=generrordef;
            exit;
          end;

        { build the new type's name }
        hierarchy:=genericdef.ownerhierarchyname;
        if assigned(genericdef.owner) then
          begin
            hmodule:=find_module_from_symtable(genericdef.owner);
            if not assigned(hmodule) then
              internalerror(2022102801);
            if hierarchy<>'' then
              hierarchy:='.'+hierarchy;
            hierarchy:=hmodule.modulename^+hierarchy;
          end;
        finalspecializename:=generate_generic_name(context.genname,context.specializename,hierarchy);
        ufinalspecializename:=upper(finalspecializename);
        if genericdef.typ=procdef then
          prettyname:=tprocdef(genericdef).procsym.prettyname
        else
          prettyname:=genericdef.typesym.prettyname;
        prettyname:=prettyname+'<'+context.prettyname+'>';
        generictypelist:=tfphashobjectlist.create(false);

        { build the list containing the types for the generic params }
        if not assigned(genericdef.genericparas) then
          internalerror(2013092601);
        if context.paramlist.count<>genericdef.genericparas.count then
          internalerror(2013092603);
        for i:=0 to genericdef.genericparas.Count-1 do
          begin
            srsym:=tsym(genericdef.genericparas[i]);
            if not (sp_generic_para in srsym.symoptions) then
              internalerror(2013092602);
            generictypelist.add(srsym.realname,context.paramlist[i]);
          end;

        { Special case if we are referencing the current defined object }
        if assigned(current_structdef) and
           (current_structdef.objname^=ufinalspecializename) then
          result:=current_structdef;

        { Can we reuse an already specialized type? }

        { for this first check whether we are currently specializing a nested
          type of the current (main) specialization (this is necessary, because
          during that time the symbol of the main specialization will still
          contain a reference to an errordef) }
        if not assigned(result) and assigned(current_specializedef) then
          begin
            def:=current_specializedef;
            repeat
              if def.typ in [objectdef,recorddef] then
                if tabstractrecorddef(def).objname^=ufinalspecializename then begin
                  result:=def;
                  break;
                end;
              if assigned(def.owner) then
                def:=tstoreddef(def.owner.defowner)
              else
                { this can happen when specializing a generic function }
                def:=nil;
            until not assigned(def) or not (df_specialization in def.defoptions);
          end;

        { if the genericdef is the def we are currently parsing (or one of its parents) then we can
          not use it for specializing as the tokenbuffer is not yet set (and we aren't done with
          parsing anyway), so for now we treat those still as generic defs without doing a partial
          specialization }
        if not assigned(result) then
          begin
            def:=current_genericdef;
            if def=genericdef then
              result:=def
            else if assigned(current_genericdef) then
              result:=find_in_hierarchy(current_genericdef,generictypelist);
            if not assigned(result) and assigned(current_specializedef) then
              result:=find_in_hierarchy(current_specializedef,generictypelist);
          end;

        { decide in which symtable to put the specialization }
        if assigned(context.forwarddef) then
          begin
            specializest:=context.forwarddef.owner;
          end
        else if parse_generic and not assigned(result) then
          begin
            srsymtable:=symtablestack.top;
            if (srsymtable.symtabletype in [localsymtable,parasymtable]) and tstoreddef(srsymtable.defowner).is_specialization then
              { if we are currently specializing a routine we need to specialize into
                the routine's local- or parasymtable so that they are correctly
                registered should the specialization be finalized }
              specializest:=srsymtable
            else if assigned(current_procinfo) and (df_generic in current_procinfo.procdef.defoptions) then
              { if we are parsing the definition of a method we specialize into
                the local symtable of it }
              specializest:=current_procinfo.procdef.getsymtable(gs_local)
            else
              begin
                if not assigned(current_genericdef) then
                  internalerror(2014050901);
                { we specialize the partial specialization into the symtable of the currently parsed
                  generic }
                case current_genericdef.typ of
                  procvardef:
                    specializest:=current_genericdef.getsymtable(gs_para);
                  procdef:
                    specializest:=current_genericdef.getsymtable(gs_local);
                  objectdef,
                  recorddef:
                    specializest:=current_genericdef.getsymtable(gs_record);
                  arraydef:
                    specializest:=tarraydef(current_genericdef).symtable;
                  else
                    internalerror(2014050902);
                end;
              end;
          end
        else
          if current_module.is_unit and current_module.in_interface then
            specializest:=current_module.globalsymtable
          else
            specializest:=current_module.localsymtable;
        if not assigned(specializest) then
          internalerror(2014050910);

        { now check whether there is a specialization somewhere else }
        psym:=nil;
        if not assigned(result) then
          begin
            hashedid.id:=ufinalspecializename;

            if (specializest.symtabletype=objectsymtable) and not assigned(context.forwarddef) then
              begin
                { search also in parent classes }
                if not assigned(current_genericdef) or (current_genericdef.typ<>objectdef) then
                  internalerror(2016112901);
                if not searchsym_in_class(tobjectdef(current_genericdef),tobjectdef(current_genericdef),ufinalspecializename,srsym,srsymtable,[]) then
                  srsym:=nil;
              end
            else
              srsym:=tsym(specializest.findwithhash(hashedid));

            if assigned(context.forwarddef) then
              begin
                { just do a few sanity checks }
                if not assigned(srsym) or not (srsym.typ=typesym) then
                  internalerror(2020070306);
                if ttypesym(srsym).typedef<>context.forwarddef then
                  internalerror(2020070307);
              end
            else if assigned(srsym) then
              begin
                retrieve_genericdef_or_procsym(srsym,result,psym);
              end
            else
              { the generic could have been specialized in the globalsymtable
                already, so search there as well }
              if (specializest<>current_module.globalsymtable) and assigned(current_module.globalsymtable) then
                begin
                  srsym:=tsym(current_module.globalsymtable.findwithhash(hashedid));
                  if assigned(srsym) then
                    begin
                      retrieve_genericdef_or_procsym(srsym,result,psym);
                    end;
                end;
          end;

        if not assigned(result) then
          begin
            specialization_init(genericdef,state);

            { push a temporary global symtable so that the specialization is
              added to the correct symtable; this symtable does not contain
              any other symbols, so that the type resolution can not be
              influenced by symbols in the current unit }
            tempst:=tspecializesymtable.create(current_module.modulename^,current_module.moduleid);
            symtablestack.push(tempst);

            { Reparse the original type definition }
              begin
                old_current_specializedef:=nil;
                old_current_genericdef:=nil;
                old_current_structdef:=nil;
                old_current_procinfo:=current_procinfo;
                old_module_procinfo:=current_module.procinfo;

                current_procinfo:=nil;
                current_module.procinfo:=nil;

                if parse_class_parent then
                  begin
                    old_current_structdef:=current_structdef;
                    old_current_genericdef:=current_genericdef;
                    old_current_specializedef:=current_specializedef;

                    if genericdef.owner.symtabletype in [recordsymtable,objectsymtable] then
                      current_structdef:=tabstractrecorddef(genericdef.owner.defowner)
                    else
                      current_structdef:=nil;
                    current_genericdef:=nil;
                    current_specializedef:=nil;
                  end;

                maybe_add_waiting_unit(genericdef);

                { First a new sym so we can reuse this specialization and
                  references to this specialization can be handled }
                if genericdef.typ=procdef then
                  if assigned(psym) then
                    srsym:=psym
                  else
                    srsym:=cprocsym.create(finalspecializename)
                else
                  srsym:=ctypesym.create(finalspecializename,generrordef);
                { insert the symbol only if we don't know already that we have
                  a procsym to add it to and we aren't dealing with a forwarddef }
                if not assigned(psym) and not assigned(context.forwarddef) then
                  specializest.insertsym(srsym);

                { specializations are declarations as such it is the wisest to
                  declare set the blocktype to "type"; otherwise we'll
                  experience unexpected side effects like the addition of
                  classrefdefs if we have a generic that's derived from another
                  generic }
                old_block_type:=block_type;
                block_type:=bt_type;

                if (
                     (genericdef.typ=procdef) and
                     not assigned(tprocdef(genericdef).genericdecltokenbuf)
                   ) or (
                     (genericdef.typ<>procdef) and
                     not assigned(genericdef.generictokenbuf)
                   ) then
                  internalerror(200511171);
                if hmodule=nil then
                  internalerror(2012051202);
                oldcurrent_filepos:=current_filepos;
                { use the index the module got from the current compilation process }
                current_filepos.moduleindex:=hmodule.unit_index;
                current_tokenpos:=current_filepos;
                if parse_generic then
                  begin
                    recordbuf:=current_scanner.recordtokenbuf;
                    current_scanner.recordtokenbuf:=nil;
                  end
                else
                  recordbuf:=nil;
                replaydepth:=current_scanner.replay_stack_depth;
                if genericdef.typ=procdef then
                  begin
                    current_scanner.startreplaytokens(tprocdef(genericdef).genericdecltokenbuf,hmodule.change_endian);
                    parse_proc_head(tprocdef(genericdef).struct,tprocdef(genericdef).proctypeoption,[],genericdef,generictypelist,pd);
                    if assigned(pd) then
                      begin
                        if assigned(psym) then
                          pd.procsym:=psym
                        else
                          pd.procsym:=srsym;
                        ppflags:=[];
                        if po_classmethod in tprocdef(genericdef).procoptions then
                          include(ppflags,ppf_classmethod);
                        parse_proc_dec_finish(pd,ppflags,tprocdef(genericdef).struct);
                      end;
                    result:=pd;
                  end
                else
                  begin
                    current_scanner.startreplaytokens(genericdef.generictokenbuf,hmodule.change_endian);
                    if assigned(context.forwarddef) then
                      begin
                        tsrsym:=nil;
                        result:=parse_forward_declaration(context.forwarddef.typesym,ufinalspecializename,finalspecializename,genericdef,generictypelist,tsrsym);
                        srsym:=tsrsym;
                      end
                    else
                      begin
                        hadtypetoken:=false;
                        read_named_type(result,srsym,genericdef,generictypelist,false,hadtypetoken);
                        ttypesym(srsym).typedef:=result;
                        result.typesym:=srsym;
                      end;


                    if _prettyname<>'' then
                      ttypesym(result.typesym).fprettyname:=_prettyname
                    else
                      ttypesym(result.typesym).fprettyname:=prettyname;
                  end;
                current_filepos:=oldcurrent_filepos;

                { Note regarding hint directives:
                  There is no need to remove the flags for them from the
                  specialized generic symbol, because hint directives that
                  follow the specialization are handled by the code in
                  pdecl.types_dec and added to the type symbol.
                  E.g.: TFoo = TBar<Blubb> deprecated;
                  Here the symbol TBar$1$Blubb will contain the
                  "sp_hint_deprecated" flag while the TFoo symbol won't.}

                case result.typ of
                  { Build VMT indexes for classes and read hint directives }
                  objectdef:
                    begin
                      if replaydepth<current_scanner.replay_stack_depth then
                        begin
                          try_consume_hintdirective(srsym.symoptions,srsym.deprecatedmsg);
                          if replaydepth<current_scanner.replay_stack_depth then
                            consume(_SEMICOLON);
                        end;

                      if oo_is_forward in tobjectdef(result).objectoptions then
                        add_forward_generic_def(result,context)
                      else
                        build_vmt(tobjectdef(result));
                    end;
                  { handle params, calling convention, etc }
                  procvardef:
                    begin
                      hintsprocessed:=false;
                      if replaydepth<current_scanner.replay_stack_depth then
                        begin
                          if not check_proc_directive(true) then
                            begin
                              hintsprocessed:=try_consume_hintdirective(ttypesym(srsym).symoptions,ttypesym(srsym).deprecatedmsg);
                              if replaydepth<current_scanner.replay_stack_depth then
                                consume(_SEMICOLON);
                            end
                          else
                            hintsprocessed:=true;
                        end;
                      if replaydepth<current_scanner.replay_stack_depth then
                        parse_proctype_directives(tprocvardef(result));
                      if po_is_function_ref in tprocvardef(result).procoptions then
                        adjust_funcref(result,srsym,nil);
                      if result.typ=procvardef then
                        flags:=hcc_default_actions_intf
                      else
                        flags:=hcc_default_actions_intf_struct;
                      handle_calling_convention(result,flags);
                      if not hintsprocessed and (replaydepth<current_scanner.replay_stack_depth) then
                        begin
                          try_consume_hintdirective(ttypesym(srsym).symoptions,ttypesym(srsym).deprecatedmsg);
                          if replaydepth<current_scanner.replay_stack_depth then
                            consume(_SEMICOLON);
                        end;
                    end;
                  procdef:
                    begin
                      pdflags:=[];
                      if genericdef.owner.symtabletype=objectsymtable then
                        include(pdflags,pd_object)
                      else if genericdef.owner.symtabletype=recordsymtable then
                        include(pdflags,pd_record);
                      parse_proc_directives(pd,pdflags);
                      while try_consume_hintdirective(pd.symoptions,pd.deprecatedmsg) do
                        consume(_SEMICOLON);
                      if parse_generic then
                        handle_calling_convention(tprocdef(result),hcc_default_actions_intf)
                      else
                        handle_calling_convention(tprocdef(result),hcc_default_actions_impl);
                      proc_add_definition(tprocdef(result));
                      { for partial specializations we implicitely declare the routine as
                        having its implementation although we'll not specialize it in reality }
                      if parse_generic then
                        unset_forwarddef(result);
                    end;
                  else
                    { parse hint directives for records and arrays }
                    if replaydepth<current_scanner.replay_stack_depth then begin
                      try_consume_hintdirective(srsym.symoptions,srsym.deprecatedmsg);
                      if replaydepth<current_scanner.replay_stack_depth then
                        consume(_SEMICOLON);
                    end;
                end;
                { Consume the remainder of the buffer }
                while current_scanner.replay_stack_depth>replaydepth do
                  consume(token);

                if assigned(recordbuf) then
                  begin
                    if assigned(current_scanner.recordtokenbuf) then
                      internalerror(2014050909);
                    current_scanner.recordtokenbuf:=recordbuf;
                  end;

                block_type:=old_block_type;
                current_procinfo:=old_current_procinfo;
                current_module.procinfo:=old_module_procinfo;
                if parse_class_parent then
                  begin
                    current_structdef:=old_current_structdef;
                    current_genericdef:=old_current_genericdef;
                    current_specializedef:=old_current_specializedef;
                  end;
              end;

            { extract all created symbols and defs from the temporary symtable
              and add them to the specializest }
            for i:=tempst.SymList.Count-1 downto 0 do
              begin
                item:=tempst.SymList.Items[i];
                { using changeowner the symbol is automatically added to the
                  new symtable }
                tsym(item).ChangeOwner(specializest);
              end;

            for i:=tempst.DefList.Count-1 downto 0 do
              begin
                item:=tempst.DefList.Items[i];
                { using changeowner the def is automatically added to the new
                  symtable }
                tdef(item).ChangeOwner(specializest);
                { for partial specializations we implicitely declare any methods as having their
                  implementations although we'll not specialize them in reality }
                if parse_generic then
                  unset_forwarddef(tdef(item));
              end;

            { if a generic was declared during the specialization we need to
              flag the specialize symtable accordingly }
            if sto_has_generic in tempst.tableoptions then
              specializest.includeoption(sto_has_generic);

            tempst.free;

            specialization_done(state);

            { procdefs are only added once we know which overload we use }
            if not parse_generic and (result.typ<>procdef) then
              current_module.pendingspecializations.add(result.typename,result);
          end;

        generictypelist.free;
        if assigned(genericdef) then
          begin
            { check the hints of the found generic symbol }
            if genericdef.typ=procdef then
              srsym:=tprocdef(genericdef).procsym
            else
              srsym:=genericdef.typesym;
            check_hints(srsym,srsym.symoptions,srsym.deprecatedmsg);
          end;
      end;


    procedure generate_specialization(var tt:tdef;enforce_unit:boolean;parse_class_parent:boolean;const _prettyname:string;parsedtype:tdef;const symname:string;parsedpos:tfileposinfo);
      var
        context : tspecializationcontext;
        genericdef : tstoreddef;
      begin
        genericdef:=tstoreddef(generate_specialization_phase1(context,tt,enforce_unit,parsedtype,symname,nil,parsedpos));
        if genericdef<>generrordef then
          genericdef:=tstoreddef(generate_specialization_phase2(context,genericdef,parse_class_parent,_prettyname));
        tt:=genericdef;
        if assigned(context) then
          context.free;
      end;


    function parse_generic_parameters(allowconstraints:boolean):tfphashobjectlist;
      var
        generictype : tstoredsym;
        i,firstidx,const_list_index : longint;
        srsymtable : tsymtable;
        basedef,def : tdef;
        defname : tidstring;
        allowconst,
        allowconstructor,
        is_const,
        doconsume : boolean;
        constraintdata : tgenericconstraintdata;
        old_block_type : tblock_type;
        fileinfo : tfileposinfo;
      begin
        result:=tfphashobjectlist.create(false);
        firstidx:=0;
        const_list_index:=0;
        old_block_type:=block_type;
        block_type:=bt_type;
        allowconst:=true;
        is_const:=false;
        repeat
          if allowconst and try_to_consume(_CONST) then
            begin
              allowconst:=false;
              is_const:=true;
              const_list_index:=result.count;
            end;
          if token=_ID then
            begin
              if is_const then
                generictype:=cconstsym.create_undefined(orgpattern,cundefinedtype)
              else
                generictype:=ctypesym.create(orgpattern,cundefinedtype);
              { type parameters need to be added as strict private }
              generictype.visibility:=vis_strictprivate;
              include(generictype.symoptions,sp_generic_para);
              result.add(orgpattern,generictype);
            end;
          consume(_ID);
          fileinfo:=current_tokenpos;
          { const restriction }
          if is_const and try_to_consume(_COLON) then
            begin
              def:=nil;
              { parse the type and assign the const type to generictype  }
              single_type(def,[]);
              for i:=const_list_index to result.count-1 do
                begin
                  { finalize constant information once type is known }
                  if assigned(def) and (def.typ in tgeneric_param_const_types) then
                    begin
                      case def.typ of
                        orddef,
                        enumdef:
                          tconstsym(result[i]).consttyp:=constord;
                        stringdef:
                          tconstsym(result[i]).consttyp:=conststring;
                        floatdef:
                          tconstsym(result[i]).consttyp:=constreal;
                        setdef:
                          tconstsym(result[i]).consttyp:=constset;
                        { pointer always refers to nil with constants }
                        pointerdef:
                          tconstsym(result[i]).consttyp:=constnil;
                        else
                          internalerror(2020011402);
                      end;
                      tconstsym(result[i]).constdef:=def;
                    end
                  else
                    Message1(type_e_generic_const_type_not_allowed,def.fulltypename);
                end;
              { after type restriction const list terminates }
              is_const:=false;
            end
          { type restriction }
          else if try_to_consume(_COLON) then
            begin
              if not allowconstraints then
                Message(parser_e_generic_constraints_not_allowed_here);
              { construct a name which can be used for a type specification }
              constraintdata:=tgenericconstraintdata.create;
              constraintdata.fileinfo:=fileinfo;
              defname:='';
              str(current_module.deflist.count,defname);
              defname:='$gendef'+defname;

              allowconstructor:=m_delphi in current_settings.modeswitches;

              basedef:=generrordef;
              repeat
                doconsume:=true;

                case token of
                  _CONSTRUCTOR:
                    begin
                      if not allowconstructor or (gcf_constructor in constraintdata.flags) then
                        Message(parser_e_illegal_expression);
                      include(constraintdata.flags,gcf_constructor);
                      allowconstructor:=false;
                    end;
                  _CLASS:
                    begin
                      if gcf_class in constraintdata.flags then
                        Message(parser_e_illegal_expression);
                      if basedef=generrordef then
                        include(constraintdata.flags,gcf_class)
                      else
                        Message(parser_e_illegal_expression);
                    end;
                  _RECORD:
                    begin
                      if ([gcf_constructor,gcf_class]*constraintdata.flags<>[])
                          or (constraintdata.interfaces.count>0) then
                        Message(parser_e_illegal_expression)
                      else
                        begin
                          srsymtable:=trecordsymtable.create(defname,0,1);
                          basedef:=crecorddef.create(defname,srsymtable);
                          include(constraintdata.flags,gcf_record);
                          allowconstructor:=false;
                        end;
                    end;
                  else
                    begin
                      { after single_type "token" is the trailing ",", ";" or
                        ">"! }
                      doconsume:=false;
                      { def is already set to a class or record }
                      if gcf_record in constraintdata.flags then
                        Message(parser_e_illegal_expression);
                      single_type(def, [stoAllowSpecialization]);
                      { only types that are inheritable are allowed }
                      if (def.typ<>objectdef) or
                          not (tobjectdef(def).objecttype in [odt_class,odt_interfacecom,odt_interfacecorba,odt_interfacejava,odt_javaclass]) then
                        Message1(type_e_class_or_interface_type_expected,def.typename)
                      else
                        case tobjectdef(def).objecttype of
                          odt_class,
                          odt_javaclass:
                            begin
                              if gcf_class in constraintdata.flags then
                                { "class" + concrete class is not allowed }
                                Message(parser_e_illegal_expression)
                              else
                                { do we already have a concrete class? }
                                if basedef<>generrordef then
                                  Message(parser_e_illegal_expression)
                                else
                                  basedef:=def;
                            end;
                          odt_interfacecom,
                          odt_interfacecorba,
                          odt_interfacejava,
                          odt_dispinterface:
                            constraintdata.interfaces.add(def);
                          else
                            ;
                        end;
                    end;
                end;
                if doconsume then
                  consume(token);
              until not try_to_consume(_COMMA);

              if ([gcf_class,gcf_constructor]*constraintdata.flags<>[]) or
                  (constraintdata.interfaces.count>1) or
                  (
                    (basedef.typ=objectdef) and
                    (tobjectdef(basedef).objecttype in [odt_javaclass,odt_class])
                  ) then
                begin
                  if basedef.typ=errordef then
                    { don't pass an errordef as a parent to a tobjectdef }
                    basedef:=class_tobject
                  else
                    if (basedef.typ<>objectdef) or
                        not (tobjectdef(basedef).objecttype in [odt_javaclass,odt_class]) then
                      internalerror(2012101101);
                  basedef:=cobjectdef.create(tobjectdef(basedef).objecttype,defname,tobjectdef(basedef),false);
                  for i:=0 to constraintdata.interfaces.count-1 do
                    tobjectdef(basedef).register_implemented_interface(tobjectdef(constraintdata.interfaces[i]),false);
                end
              else
                if constraintdata.interfaces.count=1 then
                  begin
                    if basedef.typ<>errordef then
                      internalerror(2013021601);
                    def:=tdef(constraintdata.interfaces[0]);
                    basedef:=cobjectdef.create(tobjectdef(def).objecttype,defname,tobjectdef(def),false);
                    constraintdata.interfaces.delete(0);
                  end;

              if basedef.typ<>errordef then
                with tstoreddef(basedef) do
                  begin
                    genconstraintdata:=tgenericconstraintdata.create;
                    genconstraintdata.flags:=constraintdata.flags;
                    genconstraintdata.interfaces.assign(constraintdata.interfaces);
                    genconstraintdata.fileinfo:=constraintdata.fileinfo;
                    include(defoptions,df_genconstraint);
                  end;

              for i:=firstidx to result.count-1 do
                ttypesym(result[i]).typedef:=basedef;
              { we need a typesym in case we do a Delphi-mode inline
                specialization with this parameter; so just use the first sym }
              if not assigned(basedef.typesym) then
                basedef.typesym:=ttypesym(result[firstidx]);
              firstidx:=result.count;

              constraintdata.free;
            end
          else
            begin
              if token=_SEMICOLON then
                begin
                  { two different typeless parameters are considered as incompatible }
                  for i:=firstidx to result.count-1 do
                    if tsym(result[i]).typ<>constsym then
                      begin
                        ttypesym(result[i]).typedef:=cundefineddef.create(false);
                        ttypesym(result[i]).typedef.typesym:=ttypesym(result[i]);
                      end;
                  { a semicolon terminates a type parameter group }
                  firstidx:=result.count;
                end;
            end;
          if token=_SEMICOLON then
            begin
              is_const:=false;
              allowconst:=true;
            end;
        until not (try_to_consume(_COMMA) or try_to_consume(_SEMICOLON));
        { if the constant parameter is not terminated then the type restriction was
          not specified and we need to give an error }
        if is_const then
          consume(_COLON);
        { two different typeless parameters are considered as incompatible }
        for i:=firstidx to result.count-1 do
          if tsym(result[i]).typ<>constsym then
            begin
              ttypesym(result[i]).typedef:=cundefineddef.create(false);
              ttypesym(result[i]).typedef.typesym:=ttypesym(result[i]);
            end;
        block_type:=old_block_type;
      end;


    procedure insert_generic_parameter_types(def:tstoreddef;genericdef:tstoreddef;genericlist:tfphashobjectlist;isfwd:boolean);
      var
        i : longint;
        generictype,
        fwdparam : tstoredsym;
        generictypedef : tdef;
        sym : tsym;
        st : tsymtable;
        fwdok : boolean;
        conv : tconverttype;
        op : tprocdef;
      begin
        def.genericdef:=genericdef;
        if not assigned(genericlist) then
          exit;

        if assigned(genericdef) then
          include(def.defoptions,df_specialization)
        else
          if genericlist.count>0 then
            include(def.defoptions,df_generic);

        case def.typ of
          recorddef,objectdef: st:=tabstractrecorddef(def).symtable;
          arraydef: st:=tarraydef(def).symtable;
          procvardef,procdef: st:=tabstractprocdef(def).parast;
          else
            internalerror(201101020);
        end;

        { if we have a forwarddef we check whether the generic parameters are
          equal and otherwise ignore the list }
        if isfwd then
          begin
            fwdok:=true;
            if (genericlist.count>0) and
                (
                  not assigned(def.genericparas)
                  or (def.genericparas.count<>genericlist.count)
                ) then
              fwdok:=false
            else
              begin
                for i:=0 to genericlist.count-1 do
                  begin
                    if def.genericparas.nameofindex(i)<>genericlist.nameofindex(i) then
                      begin
                        fwdok:=false;
                        break;
                      end;
                    generictype:=tstoredsym(genericlist[i]);
                    fwdparam:=tstoredsym(def.genericparas[i]);
                    op:=nil;
                    conv:=tc_equal;
                    if generictype.typ<>fwdparam.typ then
                      fwdok:=false
                    else if (generictype.typ=typesym) then
                      begin
                        if compare_defs_ext(ttypesym(generictype).typedef,ttypesym(fwdparam).typedef,nothingn,conv,op,[cdo_strict_genconstraint_check])<te_exact then
                          fwdok:=false;
                      end
                    else if (generictype.typ=constsym) then
                      begin
                        if (tconstsym(generictype).consttyp<>tconstsym(fwdparam).consttyp) or
                            (compare_defs_ext(tconstsym(generictype).constdef,tconstsym(fwdparam).constdef,nothingn,conv,op,[cdo_strict_genconstraint_check])<te_exact) then
                          fwdok:=false;
                      end
                    else
                      internalerror(2020070101);

                    if not fwdok then
                      break;
                  end;
              end;

            if not fwdok then
              Message(parser_e_forward_mismatch);

            exit;
          end;

        if (genericlist.count>0) and not assigned(def.genericparas) then
          def.genericparas:=tfphashobjectlist.create(false);
        for i:=0 to genericlist.count-1 do
          begin
            generictype:=tstoredsym(genericlist[i]);
            if assigned(generictype.owner) then
              begin
                if generictype.typ=typesym then
                  sym:=ctypesym.create(genericlist.nameofindex(i),ttypesym(generictype).typedef)
                else if generictype.typ=constsym then
                  { generictype is a constsym that was created in create_generic_constsym
                    during phase 1 so we pass this directly without copying }
                  begin
                    sym:=generictype;
                    { the sym name is still undefined so we set it to match
                      the generic param name so it's accessible }
                    sym.realname:=genericlist.nameofindex(i);
                    include(sym.symoptions,sp_generic_const);
                  end
                else
                  internalerror(2019021602);
                { type parameters need to be added as strict private }
                sym.visibility:=vis_strictprivate;
                st.insertsym(sym);
                include(sym.symoptions,sp_generic_para);
              end
            else
              begin
                if generictype.typ=typesym then
                  begin
                    generictypedef:=ttypesym(generictype).typedef;
                    if (generictypedef.typ=undefineddef) and (generictypedef<>cundefinedtype) then
                      begin
                        { the generic parameters were parsed before the genericdef existed thus the
                          undefineddefs were added as part of the parent symtable }
                        if assigned(generictypedef.owner) then
                          generictypedef.owner.DefList.Extract(generictypedef);
                        generictypedef.changeowner(st);
                      end;
                  end;
                st.insertsym(generictype);
                include(generictype.symoptions,sp_generic_para);
              end;
            def.genericparas.add(genericlist.nameofindex(i),generictype);
          end;
       end;

    procedure maybe_insert_generic_rename_symbol(const name:tidstring;genericlist:tfphashobjectlist);
      var
        gensym : ttypesym;
      begin
        { for generics in non-Delphi modes we insert a private type symbol
          that has the same base name as the currently parsed generic and
          that references this defs }
        if not (m_delphi in current_settings.modeswitches) and
            (
              (
                parse_generic and
                assigned(genericlist) and
                (genericlist.count>0)
              ) or
              (
                assigned(current_specializedef) and
                assigned(current_structdef.genericdef) and
                (current_structdef.genericdef.typ in [objectdef,recorddef]) and
                (pos('$',name)>0)
              )
            ) then
          begin
            { we need to pass nil as def here, because the constructor wants
              to set the typesym of the def which is not what we want }
            gensym:=ctypesym.create(copy(name,1,pos('$',name)-1),nil);
            gensym.typedef:=current_structdef;
            include(gensym.symoptions,sp_internal);
            { the symbol should be only visible to the generic class
              itself }
            gensym.visibility:=vis_strictprivate;
            symtablestack.top.insertsym(gensym);
          end;
      end;

    function generate_generic_name(const name:tidstring;const specializename:ansistring;const owner_hierarchy:ansistring):tidstring;
    var
      crc : cardinal;
    begin
      if specializename='' then
        internalerror(2012061901);
      { build the new type's name }
      crc:=UpdateCrc32(0,specializename[1],length(specializename));
      result:=name+'$crc'+hexstr(crc,8);
      if owner_hierarchy<>'' then
        begin
          crc:=UpdateCrc32(0,owner_hierarchy[1],length(owner_hierarchy));
          result:=result+'_crc'+hexstr(crc,8);
        end;
    end;

    procedure split_generic_name(const name:tidstring;out nongeneric:string;out count:longint);
      var
        i,code : longint;
        countstr : string;
      begin
        for i:=length(name) downto 1 do
          if name[i]='$' then
            begin
              nongeneric:=copy(name,1,i-1);
              countstr:=copy(name,i+1,length(name)-i);
              val(countstr,count,code);
              if code<>0 then
                break;
              exit;
            end;
        nongeneric:=name;
        count:=0;
      end;


    procedure add_generic_dummysym(sym:tsym);
      var
        list: TFPObjectList;
        srsym : tsym;
        srsymtable : tsymtable;
        entry : tgenericdummyentry;
      begin
        if sp_generic_dummy in sym.symoptions then
          begin
            { did we already search for a generic with that name? }
            list:=tfpobjectlist(current_module.genericdummysyms.find(sym.name));
            if not assigned(list) then
              begin
                list:=tfpobjectlist.create(true);
                current_module.genericdummysyms.add(sym.name,list);
              end;
            { is the dummy sym still "dummy"? }
            if (sym.typ=typesym) and
                (
                  { dummy sym defined in mode Delphi }
                  (ttypesym(sym).typedef.typ=undefineddef) or
                  { dummy sym defined in non-Delphi mode }
                  (tstoreddef(ttypesym(sym).typedef).is_generic)
                ) then
              begin
                { do we have a non-generic type of the same name
                  available? }
                if not searchsym_with_flags(sym.name,srsym,srsymtable,[ssf_no_addsymref]) then
                  srsym:=nil;
              end
            else if sym.typ=procsym then
              srsym:=sym
            else
              { dummy symbol is already not so dummy anymore }
              srsym:=nil;
            if assigned(srsym) then
              begin
                entry:=tgenericdummyentry.create;
                entry.resolvedsym:=srsym;
                entry.dummysym:=sym;
                list.add(entry);
              end;
          end;
      end;


    function resolve_generic_dummysym(const name:tidstring):tsym;
      var
        list : tfpobjectlist;
      begin
        list:=tfpobjectlist(current_module.genericdummysyms.find(name));
        if assigned(list) and (list.count>0) then
          result:=tgenericdummyentry(list.last).resolvedsym
        else
          result:=nil;
      end;


    function could_be_generic(const name:tidstring):boolean;
      begin
        result:=(name<>'') and
                  (current_module.genericdummysyms.findindexof(name)>=0);
      end;

    procedure specialization_init(genericdef:tdef;var state: tspecializationstate);
    var
      pu : tused_unit;
      hmodule : tmodule;
      unitsyms : TFPHashObjectList;
      sym : tsym;
      i : Integer;
      n : string;

    begin
      if not assigned(genericdef) then
        internalerror(200705151);
      { Setup symtablestack at definition time
        to get types right, however this is not perfect, we should probably record
        the resolved symbols }
      state.oldsymtablestack:=symtablestack;
      state.oldextendeddefs:=current_module.extendeddefs;
      state.oldgenericdummysyms:=current_module.genericdummysyms;
      current_module.extendeddefs:=TFPHashObjectList.create(true);
      current_module.genericdummysyms:=tfphashobjectlist.create(true);
      symtablestack:=tdefawaresymtablestack.create;
      if not assigned(genericdef.owner) then
        hmodule:=current_module
      else
        hmodule:=find_module_from_symtable(genericdef.owner);
      if hmodule=nil then
        internalerror(200705152);
      { collect all unit syms in the generic's unit as we need to establish
        their unitsym.module link again so that unit identifiers can be used }
      unitsyms:=tfphashobjectlist.create(false);
      if (hmodule<>current_module) and assigned(hmodule.globalsymtable) then
        for i:=0 to hmodule.globalsymtable.symlist.count-1 do
          begin
            sym:=tsym(hmodule.globalsymtable.symlist[i]);
            if sym.typ=unitsym then
              begin
              n:=sym.realname;
              if (Copy(n,1,7)='$hidden') then
                Delete(n,1,7);
              unitsyms.add(upper(n),sym);
              end;
          end;
      { add all units if we are specializing inside the current unit (as the
        generic could have been declared in the implementation part), but load
        only interface units, if we are in a different unit as then the generic
        needs to be in the interface section }
      pu:=tused_unit(hmodule.used_units.first);
      while assigned(pu) do
        begin
          if not assigned(pu.u.globalsymtable) then
            { in certain circular, but valid unit constellations it can happen
              that we specialize a generic in a different unit that was used
              in the implementation section of the generic's unit and were the
              interface is still being parsed and thus the localsymtable is in
              reality the global symtable }
            if pu.u.in_interface then
              begin
                {
                  MVC: The case where localsymtable is also nil can appear in complex cases and still produce valid code.
                  In order to allow people in this case to continue, SKIP_INTERNAL20231102 can be defined.
                  Default behaviour is to raise an internal error.
                  See also
                  https://gitlab.com/freepascal.org/fpc/source/-/issues/40502
                }
                {$IFDEF SKIP_INTERNAL20231102}
                if (pu.u.localsymtable<>Nil) then
                {$ELSE}
                if (pu.u.localsymtable=Nil) then
                  internalerror(20231102);
                {$ENDIF}
                  symtablestack.push(pu.u.localsymtable);
              end
            else
              internalerror(200705153)
          else
            symtablestack.push(pu.u.globalsymtable);
          sym:=tsym(unitsyms.find(pu.u.modulename^));
          if assigned(sym) and not assigned(tunitsym(sym).module) then
            tunitsym(sym).module:=pu.u;
          pu:=tused_unit(pu.next);
        end;
      unitsyms.free;
      if assigned(hmodule.globalsymtable) then
        symtablestack.push(hmodule.globalsymtable);
      { push the localsymtable if needed }
      if ((hmodule<>current_module) or not current_module.in_interface)
          and assigned(hmodule.localsymtable) then
        symtablestack.push(hmodule.localsymtable);
    end;

    procedure specialization_done(var state: tspecializationstate);
    begin
      { Restore symtablestack }
      current_module.extendeddefs.free;
      current_module.extendeddefs:=state.oldextendeddefs;
      current_module.genericdummysyms.free;
      current_module.genericdummysyms:=state.oldgenericdummysyms;
      symtablestack.free;
      symtablestack:=state.oldsymtablestack;
      { clear the state record to be on the safe side }
      fillchar(state, sizeof(state), 0);
    end;


{****************************************************************************
                      SPECIALIZATION BODY GENERATION
****************************************************************************}


    procedure process_procdef(def:tprocdef;hmodule:tmodule);
      var
        oldcurrent_filepos : tfileposinfo;
      begin
        if assigned(def.genericdef) and
            (def.genericdef.typ=procdef) and
            assigned(tprocdef(def.genericdef).generictokenbuf) then
          begin
            if not assigned(tprocdef(def.genericdef).generictokenbuf) then
              internalerror(2015061902);
            oldcurrent_filepos:=current_filepos;
            current_filepos:=tprocdef(def.genericdef).fileinfo;
            { use the index the module got from the current compilation process }
            current_filepos.moduleindex:=hmodule.unit_index;
            current_tokenpos:=current_filepos;
            current_scanner.startreplaytokens(tprocdef(def.genericdef).generictokenbuf,hmodule.change_endian);
            read_proc_body(def);
            current_filepos:=oldcurrent_filepos;
          end
        { synthetic routines will be implemented afterwards }
        else if def.synthetickind=tsk_none then
          MessagePos1(def.fileinfo,sym_e_forward_not_resolved,def.fullprocname(false));
      end;


    function process_abstractrecorddef(def:tabstractrecorddef):boolean;
      var
        i  : longint;
        hp : tdef;
        hmodule : tmodule;
      begin
        result:=true;
        hmodule:=nil;
        if assigned(def.genericdef) then
          hmodule:=find_module_from_symtable(def.genericdef.owner)
        else if not (df_internal in def.defoptions) then
          internalerror(201202041);
        for i:=0 to def.symtable.DefList.Count-1 do
          begin
            hp:=tdef(def.symtable.DefList[i]);
            if hp.typ=procdef then
             begin
               { only generate the code if we need a body }
               if assigned(tprocdef(hp).struct) and not tprocdef(hp).forwarddef then
                 continue;
               { and the body is available already (which is implicitely the
                 case if the generic routine is part of another unit) }
               if (
                    not assigned(hmodule) or
                    (hmodule=current_module) or
                    (hmodule.state=ms_compile)
                  ) and
                  { may not be assigned in case it's a synthetic procdef that
                    still needs to be generated }
                  assigned(tprocdef(hp).genericdef) and
                  tprocdef(tprocdef(hp).genericdef).forwarddef then
                 begin
                   result:=false;
                   continue;
                 end;
               process_procdef(tprocdef(hp),hmodule);
             end
           else
             if hp.typ in [objectdef,recorddef] then
               { generate code for subtypes as well }
               result:=process_abstractrecorddef(tabstractrecorddef(hp)) and result;
         end;
      end;


    procedure generate_specialization_procs;
      var
        i : longint;
        list,
        readdlist : tfpobjectlist;
        def : tstoreddef;
        state : tspecializationstate;
        hmodule : tmodule;
      begin
        { first copy all entries and then work with that list to ensure that
          we don't get an infinite recursion }
        list:=tfpobjectlist.create(false);
        readdlist:=tfpobjectlist.create(false);

        for i:=0 to current_module.pendingspecializations.Count-1 do
          list.add(current_module.pendingspecializations.Items[i]);

        current_module.pendingspecializations.clear;

        for i:=0 to list.count-1 do
          begin
            def:=tstoreddef(list[i]);
            if not tstoreddef(def).is_specialization then
              continue;
            case def.typ of
              procdef:
                begin
                  { the use of forwarddef should not backfire as the
                    specialization always belongs to the current module }
                  if not tprocdef(def).forwarddef then
                    continue;
                  if not assigned(def.genericdef) then
                    internalerror(2015061903);
                  hmodule:=find_module_from_symtable(def.genericdef.owner);
                  if hmodule=nil then
                    internalerror(2015061904);
                  { we need to check for a forward declaration only if the
                    generic was declared in the same unit (otherwise there
                    should be one) }
                  if ((hmodule=current_module) or (hmodule.state=ms_compile)) and tprocdef(def.genericdef).forwarddef then
                    begin
                      readdlist.add(def);
                      continue;
                    end;

                  specialization_init(tstoreddef(def).genericdef,state);

                  process_procdef(tprocdef(def),hmodule);

                  specialization_done(state);
                end;
              recorddef,
              objectdef:
                begin
                  specialization_init(tstoreddef(def).genericdef,state);

                  if not process_abstractrecorddef(tabstractrecorddef(def)) then
                    readdlist.add(def);

                  specialization_done(state);
                end;
              else
                ;
            end;
          end;

        { add those defs back to the pending list for which we don't yet have
          all method bodies }
        for i:=0 to readdlist.count-1 do
          current_module.pendingspecializations.add(tstoreddef(readdlist[i]).typename,readdlist[i]);

        readdlist.free;
        list.free;
      end;


    procedure generate_specializations_for_forwarddef(def:tdef);
      var
        list : tfpobjectlist;
        idx,
        i : longint;
        context : tspecializationcontext;
      begin
        if not tstoreddef(def).is_generic then
          internalerror(2020070304);
        idx:=current_module.forwardgenericdefs.findindexof(def.fulltypename);
        if idx<0 then
          exit;
        list:=tfpobjectlist(current_module.forwardgenericdefs.items[idx]);
        if not assigned(list) then
          internalerror(2020070305);
        for i:=0 to list.count-1 do begin
          context:=tspecializationcontext(list[i]);
          generate_specialization_phase2(context,tstoreddef(def),false,'');
        end;
        current_module.forwardgenericdefs.delete(idx);
      end;


    procedure maybe_add_pending_specialization(def:tdef;unnamed_syms: tfplist);
      var
        hmodule : tmodule;
        st : tsymtable;
        i : integer;
      begin
        if parse_generic then
          exit;
        { transfer ownership of any unnamed syms to be the specialization }
        if unnamed_syms<>nil then
          transfer_unnamed_symbols(tprocdef(def).parast,unnamed_syms);
        st:=def.owner;
        while st.symtabletype in [localsymtable] do
          st:=st.defowner.owner;
        hmodule:=find_module_from_symtable(st);
        if tstoreddef(def).is_specialization and (hmodule=current_module) then
          current_module.pendingspecializations.add(def.typename,def);
      end;


    function determine_generic_def(const name:tidstring):tstoreddef;
      var
        hashedid : THashedIDString;
        pd : tprocdef;
        sym : tsym;
      begin
        result:=nil;
        { check whether this is a declaration of a type inside a
          specialization }
        if assigned(current_structdef) and
            (df_specialization in current_structdef.defoptions) then
          begin
            if not assigned(current_structdef.genericdef) or
                not (current_structdef.genericdef.typ in [recorddef,objectdef]) then
              internalerror(2011052301);
            hashedid.id:=name;
            { we could be inside a method of the specialization
              instead of its declaration, so check that first (as
              local nested types aren't allowed we don't need to
              walk the symtablestack to find the localsymtable) }
            if symtablestack.top.symtabletype=localsymtable then
              begin
                { we are in a method }
                if not assigned(symtablestack.top.defowner) or
                    (symtablestack.top.defowner.typ<>procdef) then
                  internalerror(2011120701);
                pd:=tprocdef(symtablestack.top.defowner);
                if not assigned(pd.genericdef) or (pd.genericdef.typ<>procdef) then
                  internalerror(2011120702);
                sym:=tsym(tprocdef(pd.genericdef).localst.findwithhash(hashedid));
              end
            else
              sym:=nil;
            if not assigned(sym) or not (sym.typ=typesym) then
              begin
                { now search in the declaration of the generic }
                sym:=tsym(tabstractrecorddef(current_structdef.genericdef).symtable.findwithhash(hashedid));
                if not assigned(sym) or not (sym.typ=typesym) then
                  internalerror(2011052302);
              end;
            { use the corresponding type in the generic's symtable as
              genericdef for the specialized type }
            result:=tstoreddef(ttypesym(sym).typedef);
          end;
      end;


end.
