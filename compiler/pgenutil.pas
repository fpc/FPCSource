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
  { symtable }
  symtype,symdef,symbase;

    procedure generate_specialization(var tt:tdef;parse_class_parent:boolean;_prettyname:string;parsedtype:tdef;symname:string;parsedpos:tfileposinfo);
    procedure generate_specialization(var tt:tdef;parse_class_parent:boolean;_prettyname:string);
    function parse_generic_parameters(allowconstraints:boolean):tfphashobjectlist;
    function parse_generic_specialization_types(genericdeflist:tfpobjectlist;poslist:tfplist;out prettyname,specializename:ansistring):boolean;
    procedure insert_generic_parameter_types(def:tstoreddef;genericdef:tstoreddef;genericlist:tfphashobjectlist);
    procedure maybe_insert_generic_rename_symbol(const name:tidstring;genericlist:tfphashobjectlist);
    function generate_generic_name(const name:tidstring;specializename:ansistring):tidstring;
    procedure split_generic_name(const name:tidstring;out nongeneric:string;out count:longint);
    function resolve_generic_dummysym(const name:tidstring):tsym;
    function could_be_generic(const name:tidstring):boolean;inline;

    type
      tspecializationstate = record
        oldsymtablestack   : tsymtablestack;
        oldextendeddefs    : TFPHashObjectList;
        oldgenericdummysyms: tfphashobjectlist;
      end;

    procedure specialization_init(genericdef:tdef;var state:tspecializationstate);
    procedure specialization_done(var state:tspecializationstate);

implementation

uses
  { common }
  cutils,fpccrc,
  { global }
  globals,tokens,verbose,finput,
  { symtable }
  symconst,symsym,symtable,defcmp,procinfo,
  { modules }
  fmodule,
  { pass 1 }
  htypechk,
  node,nobj,nmem,
  { parser }
  scanner,
  pbase,pexpr,pdecsub,ptype;


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

    function check_generic_constraints(genericdef:tstoreddef;paradeflist:tfpobjectlist;poslist:tfplist):boolean;
      var
        i,j,
        intfcount : longint;
        formaldef,
        paradef : tstoreddef;
        objdef,
        paraobjdef,
        formalobjdef : tobjectdef;
        intffound : boolean;
        filepos : tfileposinfo;
      begin
        { check whether the given specialization parameters fit to the eventual
          constraints of the generic }
        if not assigned(genericdef.genericparas) or (genericdef.genericparas.count=0) then
          internalerror(2012101001);
        if genericdef.genericparas.count<>paradeflist.count then
          internalerror(2012101002);
        if paradeflist.count<>poslist.count then
          internalerror(2012120801);
        result:=true;
        for i:=0 to genericdef.genericparas.count-1 do
          begin
            filepos:=pfileposinfo(poslist[i])^;
            formaldef:=tstoreddef(ttypesym(genericdef.genericparas[i]).typedef);
            if formaldef.typ=undefineddef then
              { the parameter is of unspecified type, so no need to check }
              continue;
            if not (df_genconstraint in formaldef.defoptions) or
                not assigned(formaldef.genconstraintdata) then
              internalerror(2013021602);
            paradef:=tstoreddef(paradeflist[i]);
            { undefineddef is compatible with anything }
            if formaldef.typ=undefineddef then
              continue;
            if paradef.typ<>formaldef.typ then
              begin
                case formaldef.typ of
                  recorddef:
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


    function parse_generic_specialization_types_internal(genericdeflist:tfpobjectlist;poslist:tfplist;out prettyname,specializename:ansistring;parsedtype:tdef;parsedpos:tfileposinfo):boolean;
      var
        old_block_type : tblock_type;
        first : boolean;
        typeparam : tnode;
        parampos : pfileposinfo;
        tmpparampos : tfileposinfo;
      begin
        result:=true;
        if genericdeflist=nil then
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
            genericdeflist.Add(parsedtype);
            specializename:='$'+parsedtype.fulltypename;
            prettyname:=parsedtype.typesym.prettyname;
            if assigned(poslist) then
              begin
                New(parampos);
                parampos^:=parsedpos;
                poslist.add(parampos);
              end;
          end
        else
          begin
            specializename:='';
            prettyname:='';
          end;
        while not (token in [_GT,_RSHARPBRACKET]) do
          begin
            { "first" is set to false at the end of the loop! }
            if not first then
              consume(_COMMA);
            block_type:=bt_type;
            tmpparampos:=current_filepos;
            typeparam:=factor(false,true);
            if typeparam.nodetype=typen then
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
                genericdeflist.Add(typeparam.resultdef);
                if not assigned(typeparam.resultdef.typesym) then
                  message(type_e_generics_cannot_reference_itself)
                else
                  begin
                    { we use the full name of the type to uniquely identify it }
                    specializename:=specializename+'$'+typeparam.resultdef.fulltypename;
                    if not first then
                      prettyname:=prettyname+',';
                    prettyname:=prettyname+typeparam.resultdef.fullownerhierarchyname+typeparam.resultdef.typesym.prettyname;
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


    function parse_generic_specialization_types(genericdeflist:tfpobjectlist;poslist:tfplist;out prettyname,specializename:ansistring):boolean;
      var
        dummypos : tfileposinfo;
      begin
        FillChar(dummypos, SizeOf(tfileposinfo), 0);
        result:=parse_generic_specialization_types_internal(genericdeflist,poslist,prettyname,specializename,nil,dummypos);
      end;


    procedure generate_specialization(var tt:tdef;parse_class_parent:boolean;_prettyname:string);
      var
        dummypos : tfileposinfo;
      begin
        FillChar(dummypos, SizeOf(tfileposinfo), 0);
        generate_specialization(tt,parse_class_parent,_prettyname,nil,'',dummypos);
      end;


    procedure generate_specialization(var tt:tdef;parse_class_parent:boolean;_prettyname:string;parsedtype:tdef;symname:string;parsedpos:tfileposinfo);

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
            end;
          end;

      var
        st  : TSymtable;
        srsym : tsym;
        pt2 : tnode;
        hadtypetoken,
        errorrecovery,
        found,
        first,
        err : boolean;
        errval,
        i,
        gencount : longint;
        genericdef,def : tstoreddef;
        generictype : ttypesym;
        genericdeflist : TFPObjectList;
        generictypelist : tfphashobjectlist;
        prettyname,specializename : ansistring;
        ufinalspecializename,
        countstr,genname,ugenname,finalspecializename : string;
        vmtbuilder : TVMTBuilder;
        specializest : tsymtable;
        item : tobject;
        old_current_structdef : tabstractrecorddef;
        old_current_genericdef,old_current_specializedef : tstoreddef;
        tempst : tglobalsymtable;
        old_block_type: tblock_type;
        hashedid: thashedidstring;
        state : tspecializationstate;
        hmodule : tmodule;
        oldcurrent_filepos : tfileposinfo;
        poslist : tfplist;
        recordbuf: tdynamicarray;
      begin
        { retrieve generic def that we are going to replace }
        genericdef:=tstoreddef(tt);
        tt:=nil;

        { either symname must be given or genericdef needs to be valid }
        errorrecovery:=false;
        if (symname='') and
            (not assigned(genericdef) or
            not assigned(genericdef.typesym) or
            (genericdef.typesym.typ<>typesym)) then
          begin
            errorrecovery:=true;
            tt:=generrordef;
          end;

        { Only parse the parameters for recovery or
          for recording in genericbuf }
        if errorrecovery then
          begin
            first:=assigned(parsedtype);
            if not first and not try_to_consume(_LT) then
              consume(_LSHARPBRACKET);
            gencount:=0;
            { handle "<>" }
            if not first and ((token=_RSHARPBRACKET) or (token=_GT)) then
              Message(type_e_type_id_expected)
            else
              repeat
                if not first then
                  begin
                    pt2:=factor(false,true);
                    pt2.free;
                  end;
                first:=false;
                inc(gencount);
              until not try_to_consume(_COMMA);
            if not try_to_consume(_GT) then
              consume(_RSHARPBRACKET);
            { we need to return a def that can later pass some checks like
              whether it's an interface or not }
            if not errorrecovery and
                (not assigned(tt) or (tt.typ=undefineddef)) then
              begin
                if (symname='') and genericdef.is_generic then
                  { this happens in non-Delphi modes }
                  tt:=genericdef
                else
                  begin
                    { find the corresponding generic symbol so that any checks
                      done on the returned def will be handled correctly }
                    str(gencount,countstr);
                    if symname='' then
                      genname:=ttypesym(genericdef.typesym).realname
                    else
                      genname:=symname;
                    genname:=genname+'$'+countstr;
                    ugenname:=upper(genname);
                    { first check whether the found name is the same as that of
                      the current def or one of its (generic) surrounding defs;
                      this is necessary as the symbol of the generic can not yet
                      be used for lookup as it still contains a reference to an
                      errordef) }
                    def:=current_genericdef;
                    repeat
                      if def.typ in [objectdef,recorddef] then
                        if tabstractrecorddef(def).objname^=ugenname then
                          begin
                            tt:=def;
                            break;
                          end;
                      def:=tstoreddef(def.owner.defowner);
                    until not assigned(def) or not (df_generic in def.defoptions);
                    { it's not part of the current object hierarchy, so search
                      for the symbol }
                    if not assigned(tt) then
                      begin
                      srsym:=nil;
                      if not searchsym(ugenname,srsym,st) or
                          (srsym.typ<>typesym) then
                        begin
                          identifier_not_found(genname);
                          tt:=generrordef;
                          exit;
                        end;
                      tt:=ttypesym(srsym).typedef;
                      { this happens in non-Delphi modes if we encounter a
                        specialization of the generic class or record we're
                        currently parsing }
                      if (tt.typ=errordef) and assigned(current_structdef) and
                          (current_structdef.objname^=ugenname) then
                        tt:=current_structdef;
                    end;
                  end;
              end;
            exit;
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
                tt:=generrordef;
                exit;
              end;
          end;

        genericdeflist:=TFPObjectList.Create(false);
        poslist:=tfplist.create;

        { Parse type parameters }
        err:=not parse_generic_specialization_types_internal(genericdeflist,poslist,prettyname,specializename,parsedtype,parsedpos);
        if err then
          begin
            if not try_to_consume(_GT) then
              try_to_consume(_RSHARPBRACKET);
            genericdeflist.free;
            for i:=0 to poslist.count-1 do
              dispose(pfileposinfo(poslist[i]));
            poslist.free;
            tt:=generrordef;
            exit;
          end;

        { use the name of the symbol as procvars return a user friendly version
          of the name }
        if symname='' then
          genname:=ttypesym(genericdef.typesym).realname
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
        str(genericdeflist.Count,countstr);

        genname:=genname+'$'+countstr;
        ugenname:=upper(genname);

        if assigned(genericdef) and (genericdef.owner.symtabletype in [objectsymtable,recordsymtable]) then
          begin
            if genericdef.owner.symtabletype = objectsymtable then
              found:=searchsym_in_class(tobjectdef(genericdef.owner.defowner),tobjectdef(genericdef.owner.defowner),ugenname,srsym,st,[])
            else
              found:=searchsym_in_record(tabstractrecorddef(genericdef.owner.defowner),ugenname,srsym,st);
            if not found then
              found:=searchsym(ugenname,srsym,st);
          end
        else
          found:=searchsym(ugenname,srsym,st);

        if not found or (srsym.typ<>typesym) then
          begin
            identifier_not_found(genname);
            if not try_to_consume(_GT) then
              try_to_consume(_RSHARPBRACKET);
            for i:=0 to poslist.count-1 do
              dispose(pfileposinfo(poslist[i]));
            poslist.free;
            genericdeflist.Free;
            tt:=generrordef;
            exit;
          end;

        { we've found the correct def }
        genericdef:=tstoreddef(ttypesym(srsym).typedef);

        if not check_generic_constraints(genericdef,genericdeflist,poslist) then
          begin
            { the parameters didn't fit the constraints, so don't continue with the
              specialization }
            genericdeflist.free;
            for i:=0 to poslist.count-1 do
              dispose(pfileposinfo(poslist[i]));
            poslist.free;
            tt:=generrordef;
            if not try_to_consume(_GT) then
              try_to_consume(_RSHARPBRACKET);
            exit;
          end;

        { build the new type's name }
        finalspecializename:=generate_generic_name(genname,specializename);
        ufinalspecializename:=upper(finalspecializename);
        prettyname:=genericdef.typesym.prettyname+'<'+prettyname+'>';

        { select the symtable containing the params }
        case genericdef.typ of
          procdef:
            st:=genericdef.GetSymtable(gs_para);
          objectdef,
          recorddef:
            st:=genericdef.GetSymtable(gs_record);
          arraydef:
            st:=tarraydef(genericdef).symtable;
          procvardef:
            st:=genericdef.GetSymtable(gs_para);
          else
            internalerror(200511182);
        end;

        generictypelist:=tfphashobjectlist.create(false);

        { build the list containing the types for the generic params }
        if not assigned(genericdef.genericparas) then
          internalerror(2013092601);
        if genericdeflist.count<>genericdef.genericparas.count then
          internalerror(2013092603);
        for i:=0 to genericdef.genericparas.Count-1 do
          begin
            srsym:=tsym(genericdef.genericparas[i]);
            if not (sp_generic_para in srsym.symoptions) then
              internalerror(2013092602);
            generictypelist.add(srsym.realname,tdef(genericdeflist[i]).typesym);
          end;

        { Special case if we are referencing the current defined object }
        if assigned(current_structdef) and
           (current_structdef.objname^=ufinalspecializename) then
          tt:=current_structdef;

        { Can we reuse an already specialized type? }

        { for this first check whether we are currently specializing a nested
          type of the current (main) specialization (this is necessary, because
          during that time the symbol of the main specialization will still
          contain a reference to an errordef) }
        if not assigned(tt) and assigned(current_specializedef) then
          begin
            def:=current_specializedef;
            repeat
              if def.typ in [objectdef,recorddef] then
                if tabstractrecorddef(def).objname^=ufinalspecializename then begin
                  tt:=def;
                  break;
                end;
              def:=tstoreddef(def.owner.defowner);
            until not assigned(def) or not (df_specialization in def.defoptions);
          end;

        { if the genericdef is the def we are currently parsing (or one of its parents) then we can
          not use it for specializing as the tokenbuffer is not yet set (and we aren't done with
          parsing anyway), so for now we treat those still as generic defs without doing a partial
          specialization }
        if not assigned(tt) then
          begin
            def:=current_genericdef;
            while assigned(def) and (def.typ in [recorddef,objectdef]) do
              begin
                if def=genericdef then
                  begin
                    tt:=def;
                    break;
                  end;
                def:=tstoreddef(def.owner.defowner);
              end;
          end;

        { decide in which symtable to put the specialization }
        if parse_generic and not assigned(tt) then
          begin
            if not assigned(current_genericdef) then
              internalerror(2014050901);
            if assigned(current_procinfo) and (df_generic in current_procinfo.procdef.defoptions) then
              { if we are parsing the definition of a method we specialize into
                the local symtable of it }
              specializest:=current_procinfo.procdef.getsymtable(gs_local)
            else
              { we specialize the partial specialization into the symtable of the currently parsed
                generic }
              case current_genericdef.typ of
                procvardef,
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
          end
        else
          if current_module.is_unit and current_module.in_interface then
            specializest:=current_module.globalsymtable
          else
            specializest:=current_module.localsymtable;
        if not assigned(specializest) then
          internalerror(2014050910);

        { now check whether there is a specialization somewhere else }
        if not assigned(tt) then
          begin
            hashedid.id:=ufinalspecializename;

            srsym:=tsym(specializest.findwithhash(hashedid));
            if assigned(srsym) then
              begin
                if srsym.typ<>typesym then
                  internalerror(200710171);
                tt:=ttypesym(srsym).typedef;
              end
            else
              { the generic could have been specialized in the globalsymtable
                already, so search there as well }
              if (specializest<>current_module.globalsymtable) and assigned(current_module.globalsymtable) then
                begin
                  srsym:=tsym(current_module.globalsymtable.findwithhash(hashedid));
                  if assigned(srsym) then
                    begin
                      if srsym.typ<>typesym then
                        internalerror(2011121101);
                      tt:=ttypesym(srsym).typedef;
                    end;
                end;
          end;

        if not assigned(tt) then
          begin
            specialization_init(genericdef,state);

            { push a temporary global symtable so that the specialization is
              added to the correct symtable; this symtable does not contain
              any other symbols, so that the type resolution can not be
              influenced by symbols in the current unit }
            tempst:=tspecializesymtable.create(current_module.modulename^,current_module.moduleid);
            symtablestack.push(tempst);

            { Reparse the original type definition }
            if not err then
              begin
                old_current_specializedef:=nil;
                old_current_genericdef:=nil;
                old_current_structdef:=nil;

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

                { First a new typesym so we can reuse this specialization and
                  references to this specialization can be handled }
                srsym:=ctypesym.create(finalspecializename,generrordef);
                specializest.insert(srsym);

                { specializations are declarations as such it is the wisest to
                  declare set the blocktype to "type"; otherwise we'll
                  experience unexpected side effects like the addition of
                  classrefdefs if we have a generic that's derived from another
                  generic }
                old_block_type:=block_type;
                block_type:=bt_type;

                if not assigned(genericdef.generictokenbuf) then
                  internalerror(200511171);
                hmodule:=find_module_from_symtable(genericdef.owner);
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
                current_scanner.startreplaytokens(genericdef.generictokenbuf);
                hadtypetoken:=false;
                read_named_type(tt,srsym,genericdef,generictypelist,false,hadtypetoken);
                current_filepos:=oldcurrent_filepos;
                ttypesym(srsym).typedef:=tt;
                tt.typesym:=srsym;

                if _prettyname<>'' then
                  ttypesym(tt.typesym).fprettyname:=_prettyname
                else
                  ttypesym(tt.typesym).fprettyname:=prettyname;

                { Note regarding hint directives:
                  There is no need to remove the flags for them from the
                  specialized generic symbol, because hint directives that
                  follow the specialization are handled by the code in
                  pdecl.types_dec and added to the type symbol.
                  E.g.: TFoo = TBar<Blubb> deprecated;
                  Here the symbol TBar$1$Blubb will contain the
                  "sp_hint_deprecated" flag while the TFoo symbol won't.}

                case tt.typ of
                  { Build VMT indexes for classes and read hint directives }
                  objectdef:
                    begin
                      try_consume_hintdirective(srsym.symoptions,srsym.deprecatedmsg);
                      consume(_SEMICOLON);

                      vmtbuilder:=TVMTBuilder.Create(tobjectdef(tt));
                      vmtbuilder.generate_vmt;
                      vmtbuilder.free;
                    end;
                  { handle params, calling convention, etc }
                  procvardef:
                    begin
                      if not check_proc_directive(true) then
                        begin
                          try_consume_hintdirective(ttypesym(srsym).symoptions,ttypesym(srsym).deprecatedmsg);
                          consume(_SEMICOLON);
                        end;
                      parse_var_proc_directives(ttypesym(srsym));
                      handle_calling_convention(tprocvardef(tt));
                      if try_consume_hintdirective(ttypesym(srsym).symoptions,ttypesym(srsym).deprecatedmsg) then
                        consume(_SEMICOLON);
                    end;
                  else
                    { parse hint directives for records and arrays }
                    begin
                      try_consume_hintdirective(srsym.symoptions,srsym.deprecatedmsg);
                      consume(_SEMICOLON);
                    end;
                end;
                { Consume the semicolon if it is also recorded }
                try_to_consume(_SEMICOLON);

                if assigned(recordbuf) then
                  begin
                    if assigned(current_scanner.recordtokenbuf) then
                      internalerror(2014050909);
                    current_scanner.recordtokenbuf:=recordbuf;
                  end;

                block_type:=old_block_type;
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
          end;

        if not (token in [_GT, _RSHARPBRACKET]) then
          begin
            consume(_RSHARPBRACKET);
            exit;
          end
        else
          consume(token);

        genericdeflist.free;
        generictypelist.free;
        if assigned(genericdef) then
          begin
            { check the hints of the found generic symbol }
            srsym:=genericdef.typesym;
            check_hints(srsym,srsym.symoptions,srsym.deprecatedmsg);
          end;
      end;


    function parse_generic_parameters(allowconstraints:boolean):tfphashobjectlist;
      var
        generictype : ttypesym;
        i,firstidx : longint;
        srsymtable : tsymtable;
        basedef,def : tdef;
        defname : tidstring;
        allowconstructor,
        doconsume : boolean;
        constraintdata : tgenericconstraintdata;
        old_block_type : tblock_type;
      begin
        result:=tfphashobjectlist.create(false);
        firstidx:=0;
        old_block_type:=block_type;
        block_type:=bt_type;
        repeat
          if token=_ID then
            begin
              generictype:=ctypesym.create(orgpattern,cundefinedtype);
              include(generictype.symoptions,sp_generic_para);
              result.add(orgpattern,generictype);
            end;
          consume(_ID);
          if try_to_consume(_COLON) then
            begin
              if not allowconstraints then
                { TODO }
                Message(parser_e_illegal_expression{ parser_e_generic_constraints_not_allowed_here});
              { construct a name which can be used for a type specification }
              constraintdata:=tgenericconstraintdata.create;
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
                          srsymtable:=trecordsymtable.create(defname,0);
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
                  basedef:=cobjectdef.create(tobjectdef(basedef).objecttype,defname,tobjectdef(basedef));
                  for i:=0 to constraintdata.interfaces.count-1 do
                    tobjectdef(basedef).implementedinterfaces.add(
                      timplementedinterface.create(tobjectdef(constraintdata.interfaces[i])));
                end
              else
                if constraintdata.interfaces.count=1 then
                  begin
                    if basedef.typ<>errordef then
                      internalerror(2013021601);
                    def:=tdef(constraintdata.interfaces[0]);
                    basedef:=cobjectdef.create(tobjectdef(def).objecttype,defname,tobjectdef(def));
                    constraintdata.interfaces.delete(0);
                  end;
              if basedef.typ<>errordef then
                with tstoreddef(basedef) do
                  begin
                    genconstraintdata:=tgenericconstraintdata.create;
                    genconstraintdata.flags:=constraintdata.flags;
                    genconstraintdata.interfaces.assign(constraintdata.interfaces);
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
            end;
        until not (try_to_consume(_COMMA) or try_to_consume(_SEMICOLON));
        block_type:=old_block_type;
      end;


    procedure insert_generic_parameter_types(def:tstoreddef;genericdef:tstoreddef;genericlist:tfphashobjectlist);
      var
        i : longint;
        generictype,sym : ttypesym;
        st : tsymtable;
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

        if (genericlist.count>0) and not assigned(def.genericparas) then
          def.genericparas:=tfphashobjectlist.create(false);
        for i:=0 to genericlist.count-1 do
          begin
            generictype:=ttypesym(genericlist[i]);
            if assigned(generictype.owner) then
              begin
                sym:=ctypesym.create(genericlist.nameofindex(i),generictype.typedef);
                st.insert(sym);
                include(sym.symoptions,sp_generic_para);
              end
            else
              begin
                st.insert(generictype);
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
            symtablestack.top.insert(gensym);
          end;
      end;

    function generate_generic_name(const name:tidstring;specializename:ansistring):tidstring;
    var
      crc : cardinal;
    begin
      if specializename='' then
        internalerror(2012061901);
      { build the new type's name }
      crc:=UpdateCrc32(0,specializename[1],length(specializename));
      result:=name+'$crc'+hexstr(crc,8);
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
              unitsyms.add(upper(sym.realname),sym);
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
              symtablestack.push(pu.u.localsymtable)
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
      if (hmodule<>current_module) or not current_module.in_interface then
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

end.
