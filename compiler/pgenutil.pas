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

    procedure generate_specialization(var tt:tdef;parse_class_parent:boolean;_prettyname:string;parsedtype:tdef;symname:string);
    function parse_generic_parameters:TFPObjectList;
    function parse_generic_specialization_types(genericdeflist:tfpobjectlist;out prettyname,specializename:ansistring;parsedtype:tdef):boolean;
    procedure insert_generic_parameter_types(def:tstoreddef;genericdef:tstoreddef;genericlist:TFPObjectList);
    procedure maybe_insert_generic_rename_symbol(const name:tidstring;genericlist:tfpobjectlist);
    function generate_generic_name(const name:tidstring;specializename:ansistring):tidstring;

    type
      tspecializationstate = record
        oldsymtablestack   : tsymtablestack;
        oldextendeddefs    : TFPHashObjectList;
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
  symconst,symsym,symtable,
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

    procedure generate_specialization(var tt:tdef;parse_class_parent:boolean;_prettyname:string;parsedtype:tdef;symname:string);
      var
        st  : TSymtable;
        srsym : tsym;
        pt2 : tnode;
        found,
        first,
        err : boolean;
        errval,
        i,
        gencount : longint;
        crc : cardinal;
        genericdef,def : tstoreddef;
        generictype : ttypesym;
        genericdeflist : TFPObjectList;
        generictypelist : TFPObjectList;
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
      begin
        { retrieve generic def that we are going to replace }
        genericdef:=tstoreddef(tt);
        tt:=nil;

        { either symname must be given or genericdef needs to be valid }
        if (symname='') and
            (not assigned(genericdef) or
            not assigned(genericdef.typesym) or
            (genericdef.typesym.typ<>typesym)) then
           internalerror(2011042701);

        { Only parse the parameters for recovery or
          for recording in genericbuf }
        if parse_generic then
          begin
            first:=assigned(parsedtype);
            if not first and not try_to_consume(_LT) then
              consume(_LSHARPBRACKET);
            gencount:=0;
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
            if not assigned(tt) or (tt.typ=undefineddef) then
              begin
                if (symname='') and (df_generic in genericdef.defoptions) then
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
          consume(_LSHARPBRACKET);

        generictypelist:=TFPObjectList.create(false);
        genericdeflist:=TFPObjectList.Create(false);

        { Parse type parameters }
        err:=not parse_generic_specialization_types(genericdeflist,prettyname,specializename,parsedtype);
        if err then
          begin
            try_to_consume(_RSHARPBRACKET);
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
          { search for a potential suffix }
          for i:=length(genname) downto 1 do
            if genname[i]='$' then
              begin
                { if the part right of the $ is a number we assume that the left
                  part is the name of the generic, otherwise we assume that the
                  complete name is the name of the generic }
                countstr:=copy(genname,i+1,length(genname)-i);
                gencount:=0;
                val(countstr,gencount,errval);
                if errval=0 then
                  genname:=copy(genname,1,i-1);
                break;
              end;

        { search a generic with the given count of params }
        countstr:='';
        str(genericdeflist.Count,countstr);

        genname:=genname+'$'+countstr;
        ugenname:=upper(genname);

        if assigned(genericdef) and (genericdef.owner.symtabletype in [objectsymtable,recordsymtable]) then
          begin
            if genericdef.owner.symtabletype = objectsymtable then
              found:=searchsym_in_class(tobjectdef(genericdef.owner.defowner),tobjectdef(genericdef.owner.defowner),ugenname,srsym,st,false)
            else
              found:=searchsym_in_record(tabstractrecorddef(genericdef.owner.defowner),ugenname,srsym,st);
          end
        else
          found:=searchsym(ugenname,srsym,st);

        if not found or (srsym.typ<>typesym) then
          begin
            identifier_not_found(genname);
            genericdeflist.Free;
            generictypelist.Free;
            tt:=generrordef;
            exit;
          end;

        { we've found the correct def }
        genericdef:=tstoreddef(ttypesym(srsym).typedef);

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

        { build the list containing the types for the generic params }
        gencount:=0;
        for i:=0 to st.SymList.Count-1 do
          begin
            srsym:=tsym(st.SymList[i]);
            if sp_generic_para in srsym.symoptions then
              begin
                if gencount=genericdeflist.Count then
                  internalerror(2011042702);
                generictype:=ttypesym.create(srsym.realname,tdef(genericdeflist[gencount]));
                generictypelist.add(generictype);
                inc(gencount);
              end;
          end;

        { Special case if we are referencing the current defined object }
        if assigned(current_structdef) and
           (current_structdef.objname^=ufinalspecializename) then
          tt:=current_structdef;

        { decide in which symtable to put the specialization }
        if current_module.is_unit and current_module.in_interface then
          specializest:=current_module.globalsymtable
        else
          specializest:=current_module.localsymtable;

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
                srsym:=ttypesym.create(finalspecializename,generrordef);
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
                current_scanner.startreplaytokens(genericdef.generictokenbuf);
                read_named_type(tt,srsym,genericdef,generictypelist,false);
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


    function parse_generic_parameters:TFPObjectList;
      var
        generictype : ttypesym;
      begin
        result:=TFPObjectList.Create(false);
        repeat
          if token=_ID then
            begin
              generictype:=ttypesym.create(orgpattern,cundefinedtype);
              include(generictype.symoptions,sp_generic_para);
              result.add(generictype);
            end;
          consume(_ID);
        until not try_to_consume(_COMMA) ;
      end;

    function parse_generic_specialization_types(genericdeflist:tfpobjectlist;out prettyname,specializename:ansistring;parsedtype:tdef):boolean;
      var
        old_block_type : tblock_type;
        first : boolean;
        typeparam : tnode;
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
            specializename:='$'+parsedtype.typename;
            prettyname:=parsedtype.typesym.prettyname;
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
            typeparam:=factor(false,true);
            if typeparam.nodetype=typen then
              begin
                if df_generic in typeparam.resultdef.defoptions then
                  Message(parser_e_no_generics_as_params);
                genericdeflist.Add(typeparam.resultdef);
                if not assigned(typeparam.resultdef.typesym) then
                  message(type_e_generics_cannot_reference_itself)
                else
                  begin
                    specializename:=specializename+'$'+typeparam.resultdef.typename;
                    if first then
                      prettyname:=prettyname+typeparam.resultdef.typesym.prettyname
                    else
                      prettyname:=prettyname+','+typeparam.resultdef.typesym.prettyname;
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

    procedure insert_generic_parameter_types(def:tstoreddef;genericdef:tstoreddef;genericlist:TFPObjectList);
      var
        i: longint;
        generictype: ttypesym;
        st: tsymtable;
      begin
        def.genericdef:=genericdef;
        if not assigned(genericlist) then
          exit;

        case def.typ of
          recorddef,objectdef: st:=tabstractrecorddef(def).symtable;
          arraydef: st:=tarraydef(def).symtable;
          procvardef,procdef: st:=tabstractprocdef(def).parast;
          else
            internalerror(201101020);
        end;

        for i:=0 to genericlist.count-1 do
          begin
            generictype:=ttypesym(genericlist[i]);
            if generictype.typedef.typ=undefineddef then
              include(def.defoptions,df_generic)
            else
              include(def.defoptions,df_specialization);
            st.insert(generictype);
            include(generictype.symoptions,sp_generic_para);
            def.genericparas.add(generictype.name,generictype);
          end;
       end;

    procedure maybe_insert_generic_rename_symbol(const name:tidstring;genericlist:tfpobjectlist);
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
            gensym:=ttypesym.create(copy(name,1,pos('$',name)-1),nil);
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
      current_module.extendeddefs:=TFPHashObjectList.create(true);
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
      symtablestack.free;
      symtablestack:=state.oldsymtablestack;
      { clear the state record to be on the safe side }
      fillchar(state, sizeof(state), 0);
    end;

end.
