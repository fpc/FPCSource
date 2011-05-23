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
  { symtable }
  symtype,symdef;

    procedure generate_specialization(var tt:tdef;parse_class_parent:boolean;parsedtype:tdef);
    function parse_generic_parameters:TFPObjectList;
    procedure insert_generic_parameter_types(def:tstoreddef;genericdef:tstoreddef;genericlist:TFPObjectList);

implementation

uses
  { common }
  cutils,
  { global }
  globals,tokens,verbose,
  { symtable }
  symconst,symbase,symsym,symtable,
  { modules }
  fmodule,
  { pass 1 }
  node,nobj,
  { parser }
  scanner,
  pbase,pexpr,pdecsub,ptype;


    procedure generate_specialization(var tt:tdef;parse_class_parent:boolean;parsedtype:tdef);
      var
        st  : TSymtable;
        srsym : tsym;
        pt2 : tnode;
        first,
        err : boolean;
        i,
        gencount : longint;
        genericdef : tstoreddef;
        genericsym,
        generictype : ttypesym;
        genericdeflist : TFPObjectList;
        generictypelist : TFPObjectList;
        oldsymtablestack   : tsymtablestack;
        oldextendeddefs    : TFPHashObjectList;
        hmodule : tmodule;
        pu : tused_unit;
        uspecializename,
        countstr,genname,ugenname,specializename : string;
        vmtbuilder : TVMTBuilder;
        onlyparsepara : boolean;
        specializest : tsymtable;
        item: psymtablestackitem;
      begin
        { retrieve generic def that we are going to replace }
        genericdef:=tstoreddef(tt);
        tt:=nil;
        onlyparsepara:=false;

        if not assigned(genericdef.typesym) or
            (genericdef.typesym.typ<>typesym) then
           internalerror(2011042701);

        genericsym:=ttypesym(genericdef.typesym);

        { only need to record the tokens, then we don't know the type yet  ... }
        if parse_generic then
          begin
            { ... but we have to insert a def into the symtable else the deflist
              of generic and specialization might not be equally sized which
              is later assumed }
            tt:=tundefineddef.create;
            if parse_class_parent then
              tt:=genericdef;
            onlyparsepara:=true;
          end;

        { Only parse the parameters for recovery or
          for recording in genericbuf }
        if onlyparsepara then
          begin
            consume(_LSHARPBRACKET);
            repeat
              pt2:=factor(false,true);
              pt2.free;
            until not try_to_consume(_COMMA);
            consume(_RSHARPBRACKET);
            exit;
          end;

        if not assigned(parsedtype) and not try_to_consume(_LT) then
          consume(_LSHARPBRACKET);

        generictypelist:=TFPObjectList.create(false);
        genericdeflist:=TFPObjectList.Create(false);

        { Parse type parameters }
        if not assigned(genericdef.typesym) then
          internalerror(200710173);
        err:=false;
        { if parsedtype is set, then the first type identifer was already parsed
          (happens in inline specializations) and thus we only need to parse
          the remaining types and do as if the first one was already given }
        first:=not assigned(parsedtype);
        if assigned(parsedtype) then
          begin
            genericdeflist.Add(parsedtype);
            specializename:='$'+parsedtype.typesym.realname;
          end
        else
          specializename:='';
        while not (token in [_GT,_RSHARPBRACKET]) do
          begin
            if not first then
              consume(_COMMA)
            else
              first:=false;
            pt2:=factor(false,true);
            if pt2.nodetype=typen then
              begin
                if df_generic in pt2.resultdef.defoptions then
                  Message(parser_e_no_generics_as_params);
                genericdeflist.Add(pt2.resultdef);
                if not assigned(pt2.resultdef.typesym) then
                  message(type_e_generics_cannot_reference_itself)
                else
                  specializename:=specializename+'$'+pt2.resultdef.typesym.realname;
              end
            else
              begin
                Message(type_e_type_id_expected);
                err:=true;
              end;
            pt2.free;
          end;

        if err then
          begin
            try_to_consume(_RSHARPBRACKET);
            exit;
          end;

        { search a generic with the given count of params }
        countstr:='';
        str(genericdeflist.Count,countstr);
        { use the name of the symbol as procvars return a user friendly version
          of the name }
        genname:=ttypesym(genericdef.typesym).realname;
        { in case of non-Delphi mode the type name could already be a generic
          def (but maybe the wrong one) }
        if df_generic in genericdef.defoptions then
          begin
            { remove the type count suffix from the generic's name }
            for i:=Length(genname) downto 1 do
              if genname[i]='$' then
                begin
                  genname:=copy(genname,1,i-1);
                  break;
                end;
          end;
        genname:=genname+'$'+countstr;
        ugenname:=upper(genname);

        if not searchsym(ugenname,srsym,st)
            or (srsym.typ<>typesym) then
          begin
            identifier_not_found(genname);
            genericdeflist.Free;
            generictypelist.Free;
            exit;
          end;

        { we've found the correct def }
        genericdef:=tstoreddef(ttypesym(srsym).typedef);

        { build the new type's name }
        specializename:=genname+specializename;
        uspecializename:=upper(specializename);

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
           (current_structdef.objname^=uspecializename) then
          tt:=current_structdef;

        { for units specializations can already be needed in the interface, therefor we
          will use the global symtable. Programs don't have a globalsymtable and there we
          use the localsymtable }
        if current_module.is_unit then
          specializest:=current_module.globalsymtable
        else
          specializest:=current_module.localsymtable;

        { Can we reuse an already specialized type? }
        if not assigned(tt) then
          begin
            srsym:=tsym(specializest.find(uspecializename));
            if assigned(srsym) then
              begin
                if srsym.typ<>typesym then
                  internalerror(200710171);
                tt:=ttypesym(srsym).typedef;
              end;
          end;

        if not assigned(tt) then
          begin
            { Setup symtablestack at definition time
              to get types right, however this is not perfect, we should probably record
              the resolved symbols }
            oldsymtablestack:=symtablestack;
            oldextendeddefs:=current_module.extendeddefs;
            current_module.extendeddefs:=TFPHashObjectList.create(true);
            symtablestack:=tdefawaresymtablestack.create;
            if not assigned(genericdef) then
              internalerror(200705151);
            hmodule:=find_module_from_symtable(genericdef.owner);
            if hmodule=nil then
              internalerror(200705152);
            pu:=tused_unit(hmodule.used_units.first);
            while assigned(pu) do
              begin
                if not assigned(pu.u.globalsymtable) then
                  internalerror(200705153);
                symtablestack.push(pu.u.globalsymtable);
                pu:=tused_unit(pu.next);
              end;

            if assigned(hmodule.globalsymtable) then
              symtablestack.push(hmodule.globalsymtable);

            { hacky, but necessary to insert the newly generated class properly }
            item:=oldsymtablestack.stack;
            while assigned(item) and (item^.symtable.symtablelevel>main_program_level) do
              item:=item^.next;
            if assigned(item) and (item^.symtable<>symtablestack.top) then
              symtablestack.push(item^.symtable);

            { Reparse the original type definition }
            if not err then
              begin
                { First a new typesym so we can reuse this specialization and
                  references to this specialization can be handled }
                srsym:=ttypesym.create(specializename,generrordef);
                specializest.insert(srsym);

                if not assigned(genericdef.generictokenbuf) then
                  internalerror(200511171);
                current_scanner.startreplaytokens(genericdef.generictokenbuf);
                read_named_type(tt,specializename,genericdef,generictypelist,false);
                ttypesym(srsym).typedef:=tt;
                tt.typesym:=srsym;

                case tt.typ of
                  { Build VMT indexes for classes }
                  objectdef:
                    begin
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
                end;
                { Consume the semicolon if it is also recorded }
                try_to_consume(_SEMICOLON);
              end;

            { Restore symtablestack }
            current_module.extendeddefs.free;
            current_module.extendeddefs:=oldextendeddefs;
            symtablestack.free;
            symtablestack:=oldsymtablestack;
          end
        else
          begin
            { There is comment few lines before ie 200512115
              saying "We are parsing the same objectdef, the def index numbers
              are the same". This is wrong (index numbers are not same)
              in case there is specialization (S2 in this case) inside
              specialized generic (G2 in this case) which is equal to
              some previous specialization (S1 in this case). In that case,
              new symbol is not added to currently specialized type
              (S in this case) for that specializations (S2 in this case),
              and this results in that specialization and generic definition
              don't have same number of elements in their object symbol tables.
              This patch adds undefined def to ensure that those
              two symbol tables will have same number of elements.
            }
            tundefineddef.create;
          end;

        genericdeflist.free;
        generictypelist.free;
        if not try_to_consume(_GT) then
          consume(_RSHARPBRACKET);
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
          end;
       end;


end.
