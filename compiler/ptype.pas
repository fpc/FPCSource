{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Does parsing types for Free Pascal

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
unit ptype;

{$i fpcdefs.inc}

interface

    uses
       globtype,cclasses,compilerbase,
       symtype,symdef,symbase;

    type
      TSingleTypeOption=(
        stoIsForwardDef,          { foward declaration         }
        stoAllowTypeDef,          { allow type definitions     }
        stoAllowSpecialization,   { allow type specialization  }
        stoParseClassParent       { parse of parent class type }
      );
      TSingleTypeOptions=set of TSingleTypeOption;

  TTypesParser = class
  private
    FCompiler: TCompilerBase;

    { we use TObject instead of TParser to avoid cyclic unit reference }
    FParser: TObject;

    procedure id_type(var def : tdef;isforwarddef,checkcurrentrecdef,allowgenericsyms,allowunitsym:boolean;out srsym:tsym;out srsymtable:tsymtable;out is_specialize,is_unit_specific:boolean);
    function try_parse_structdef_nested_type(out def: tdef; basedef: tabstractrecorddef; isfowarddef: boolean): boolean;
    procedure parse_record_members(recsym:tsym);
    function record_dec(const n:tidstring;recsym:tsym;genericdef:tstoreddef;genericlist:tfphashobjectlist):tdef;
    property Compiler: TCompilerBase read FCompiler;
  public
    constructor Create(AParser: TObject; ACompiler: TCompilerBase);

    procedure resolve_forward_types;

    { reads a string, file type or a type identifier }
    procedure single_type(out def:tdef;options:TSingleTypeOptions);
    { ... but rejects types that cannot be returned from functions }
    function result_type(options:TSingleTypeOptions):tdef;

    { reads any type declaration, where the resulting type will get name as type identifier }
    procedure read_named_type(var def:tdef;const newsym:tsym;genericdef:tstoreddef;genericlist:tfphashobjectlist;parseprocvardir:boolean;var hadtypetoken:boolean);

    { reads any type declaration }
    procedure read_anon_type(var def : tdef;parseprocvardir:boolean;genericdef:tstoreddef);

    { parse nested type declaration of the def (typedef) }
    procedure parse_nested_types(var def: tdef; isforwarddef,allowspecialization: boolean; currentstructstack: tfpobjectlist);


    { add a definition for a method to a record/objectdef that will contain
      all code for initialising typed constants (only for targets in
      systems.systems_typed_constants_node_init) }
    procedure add_typedconst_init_routine(def: tabstractrecorddef);

    { parse hint directives (platform, deprecated, ...) for a procdef }
    procedure maybe_parse_hint_directives(pd:tprocdef);
  end;

implementation

    uses
       { common }
       cutils,
       { global }
       globals,tokens,verbose,constexp,
       systems,compiler,
       { symtable }
       symconst,symsym,symtable,symcreat,
       defutil,defcmp,
{$ifdef jvm}
       jvmdef,
{$endif}
       { modules }
       fmodule,
       { pass 1 }
       node,
       nset,ncnv,ncon,nld,
       { parser }
       scanner,parser,
       pbase,pexpr,pdecsub,pdecvar,pdecobj,pdecl,pgenutil,pparautl,procdefutil
{$ifdef jvm}
       ,pjvm
{$endif}
       ;

    type

      { TTypesParserHelper }

      TTypesParserHelper = class helper for TTypesParser
        function Parser: TParser; inline;
      end;

    { TTypesParserHelper }

    function TTypesParserHelper.Parser: TParser;
      begin
        Result:=TParser(FParser);
      end;


    constructor TTypesParser.Create(AParser: TObject; ACompiler: TCompilerBase);
      begin
        FParser:=AParser;
        FCompiler:=ACompiler;
      end;


    procedure TTypesParser.maybe_parse_hint_directives(pd:tprocdef);
      var
        dummysymoptions : tsymoptions;
        deprecatedmsg : pshortstring;
      begin
        if assigned(pd) then
          begin
            dummysymoptions:=pd.symoptions;
            deprecatedmsg:=pd.deprecatedmsg;
          end
        else
          begin
            dummysymoptions:=[];
            deprecatedmsg:=nil;
          end;
        while parser.pbase.try_consume_hintdirective(dummysymoptions,deprecatedmsg) do
          parser.pbase.consume(_SEMICOLON);
        if assigned(pd) then
          begin
            pd.symoptions:=pd.symoptions+dummysymoptions;
            if sp_has_deprecated_msg in dummysymoptions then
              pd.deprecatedmsg:=deprecatedmsg;
          end
        else
          stringdispose(deprecatedmsg);
      end;


    procedure TTypesParser.resolve_forward_types;
      var
        i: longint;
        tmp,
        hpd,
        def : tdef;
        srsym  : tsym;
        srsymtable : TSymtable;
        hs : string;
        fileinfo : tfileposinfo;
      begin
        for i:=0 to current_module.checkforwarddefs.Count-1 do
          begin
            def:=tdef(current_module.checkforwarddefs[i]);
            case def.typ of
              pointerdef,
              classrefdef :
                begin
                  { classrefdef inherits from pointerdef }
                  hpd:=tabstractpointerdef(def).pointeddef;
                  { still a forward def ? }
                  if hpd.typ=forwarddef then
                   begin
                     { try to resolve the forward }
                     if not assigned(tforwarddef(hpd).tosymname) then
                       internalerror(200211201);
                     hs:=tforwarddef(hpd).tosymname^;
                     compiler.symtablestack.searchsym(upper(hs),srsym,srsymtable);
                     { we don't need the forwarddef anymore, dispose it }
                     hpd.free;
                     hpd := nil;
                     tabstractpointerdef(def).pointeddef:=nil; { if error occurs }
                     { was a type sym found ? }
                     if assigned(srsym) and
                        (srsym.typ=typesym) then
                      begin
                        if (sp_generic_dummy in srsym.symoptions) and
                            not (ttypesym(srsym).typedef.typ=undefineddef) and
                            assigned(def.owner.defowner) then
                          begin
                            { is the forward def part of a specialization? }
                            tmp:=tdef(def.owner.defowner);
                            while not tstoreddef(tmp).is_specialization and assigned(tmp.owner.defowner) do
                              tmp:=tdef(tmp.owner.defowner);
                            { if the genericdef of the specialization is the same as the
                              def the dummy points to, then update the found symbol }
                            if tstoreddef(tmp).is_specialization and
                                (tstoreddef(tmp).genericdef=ttypesym(srsym).typedef) then
                              srsym:=tstoreddef(tmp).typesym;
                          end;
                        tabstractpointerdef(def).pointeddef:=ttypesym(srsym).typedef;
                        { correctly set the generic/specialization flags and the genericdef }
                        if df_generic in tstoreddef(tabstractpointerdef(def).pointeddef).defoptions then
                          include(tstoreddef(def).defoptions,df_generic);
                        if df_specialization in tstoreddef(tabstractpointerdef(def).pointeddef).defoptions then
                          begin
                            include(tstoreddef(def).defoptions,df_specialization);
                            case def.typ of
                              pointerdef:
                                tstoreddef(def).genericdef:=cpointerdef.getreusable(tstoreddef(tabstractpointerdef(def).pointeddef).genericdef,compiler);
                              classrefdef:
                                tstoreddef(def).genericdef:=cclassrefdef.create(tstoreddef(tabstractpointerdef(def).pointeddef).genericdef,compiler);
                              else
                                internalerror(2016120901);
                            end;
                          end;
                        { avoid wrong unused warnings web bug 801 PM }
                        inc(ttypesym(srsym).refs);
                        { we need a class type for classrefdef }
                        if (def.typ=classrefdef) and
                           not(is_class(ttypesym(srsym).typedef)) and
                           not(is_objcclass(ttypesym(srsym).typedef)) and
                           not(is_javaclass(ttypesym(srsym).typedef)) then
                          compiler.verbose.MessagePos1(def.typesym.fileinfo,type_e_class_type_expected,ttypesym(srsym).typedef.typename);
                        { this could also be a generic dummy that was not
                          overridden with a specific type }
                        if (sp_generic_dummy in srsym.symoptions) and
                            (
                              (ttypesym(srsym).typedef.typ=undefineddef) or
                              (
                                { or an unspecialized generic symbol, which is
                                  the case for generics defined in non-Delphi
                                  modes }
                                tstoreddef(ttypesym(srsym).typedef).is_generic and
                                not defs_belong_to_same_generic(def,ttypesym(srsym).typedef)
                              )
                            ) then
                          begin
                            if assigned(def.typesym) then
                              fileinfo:=def.typesym.fileinfo
                            else
                              { this is the case for inline pointer declarations }
                              fileinfo:=srsym.fileinfo;
                            compiler.verbose.MessagePos(fileinfo,parser_e_no_generics_as_types);
                          end;
                      end
                     else
                      begin
                        compiler.verbose.Message1(sym_e_forward_type_not_resolved,hs);
                        { try to recover }
                        tabstractpointerdef(def).pointeddef:=generrordef;
                      end;
                   end;
                end;
              objectdef :
                begin
                  { give an error as the implementation may follow in an
                    other type block which is allowed by FPC modes }
                  if not(m_fpc in current_settings.modeswitches) and
                     (oo_is_forward in tobjectdef(def).objectoptions) then
                    compiler.verbose.MessagePos1(def.typesym.fileinfo,type_e_type_is_not_completly_defined,def.typename);
                  { generate specializations for generic forwarddefs }
                  if not (oo_is_forward in tobjectdef(def).objectoptions) and
                      tstoreddef(def).is_generic then
                    parser.pgenutil.generate_specializations_for_forwarddef(def);
                 end;
              else
                internalerror(200811071);
            end;
          end;
        current_module.checkforwarddefs.clear;
      end;


    { def is the outermost type in which other types have to be searched

      isforward indicates whether the current definition can be a forward definition

      if assigned, currentstructstack is a list of tabstractrecorddefs that, from
      last to first, are child types of def that are not yet visible via the
      normal symtable searching routines because they are types that are currently
      being parsed (so using id_type on them after pushing def on the
      symtablestack would result in errors because they'd come back as errordef)
    }
    procedure TTypesParser.parse_nested_types(var def: tdef; isforwarddef,allowspecialization: boolean; currentstructstack: tfpobjectlist);
      var
        t2: tdef;
        structstackindex: longint;
        srsym: tsym;
        srsymtable: tsymtable;
        oldsymtablestack: TSymtablestack;
        isspecialize,
        isunitspecific : boolean;
      begin
        if assigned(currentstructstack) then
          structstackindex:=currentstructstack.count-1
        else
          structstackindex:=-1;
        { handle types inside classes, e.g. TNode.TLongint }
        while (current_scanner.token=_POINT) do
          begin
             if is_class_or_object(def) or is_record(def) or is_java_class_or_interface(def) then
               begin
                 if (def.typ=objectdef) then
                   def:=compiler.symtablestack.find_real_class_definition(tobjectdef(def),false);
                 parser.pbase.consume(_POINT);
                 if (structstackindex>=0) and
                    (tabstractrecorddef(currentstructstack[structstackindex]).objname^=current_scanner.pattern) then
                   begin
                     def:=tdef(currentstructstack[structstackindex]);
                     dec(structstackindex);
                     parser.pbase.consume(_ID);
                   end
                 else
                   begin
                     structstackindex:=-1;
                     oldsymtablestack:=compiler.symtablestack;
                     tcompiler(compiler).symtablestack:=TSymtablestack.create(compiler);
                     compiler.symtablestack.push(tabstractrecorddef(def).symtable);
                     t2:=generrordef;
                     id_type(t2,isforwarddef,false,false,false,srsym,srsymtable,isspecialize,isunitspecific);
                     compiler.symtablestack.pop(tabstractrecorddef(def).symtable);
                     compiler.symtablestack.free;
                     tcompiler(compiler).symtablestack:=oldsymtablestack;
                     if isspecialize or
                         (
                           (m_delphi in current_settings.modeswitches) and
                           (current_scanner.token=_LSHARPBRACKET)
                         ) then
                       begin
                         if not allowspecialization then
                           compiler.verbose.Message(parser_e_no_local_para_def);
                         parser.pgenutil.generate_specialization(t2,isunitspecific,false,'',srsym.name,srsymtable);
                       end;
                     def:=t2;
                   end;
               end
             else
               break;
          end;
      end;


    function TTypesParser.try_parse_structdef_nested_type(out def: tdef; basedef: tabstractrecorddef; isfowarddef: boolean): boolean;
      var
        structdef : tdef;
        structdefstack : tfpobjectlist;
      begin
         def:=nil;
         { use of current parsed object:
           classes, objects, records can be used also in themself }
         structdef:=basedef;
         structdefstack:=nil;
         while assigned(structdef) and (structdef.typ in [objectdef,recorddef]) do
           begin
             if (tabstractrecorddef(structdef).objname^=current_scanner.pattern) then
               begin
                 parser.pbase.consume(_ID);
                 def:=structdef;
                 { we found the top-most match, now check how far down we can
                   follow }
                 structdefstack:=tfpobjectlist.create(false);
                 structdef:=basedef;
                 while (structdef<>def) do
                   begin
                     structdefstack.add(structdef);
                     structdef:=tabstractrecorddef(structdef.owner.defowner);
                   end;
                 parse_nested_types(def,isfowarddef,true,structdefstack);
                 structdefstack.free;
                 structdefstack := nil;
                 result:=true;
                 exit;
               end;
             structdef:=tdef(tabstractrecorddef(structdef).owner.defowner);
           end;
         result:=false;
      end;

    procedure TTypesParser.id_type(var def : tdef;isforwarddef,checkcurrentrecdef,allowgenericsyms,allowunitsym:boolean;out srsym:tsym;out srsymtable:tsymtable;out is_specialize,is_unit_specific:boolean);
    { reads a type definition }
    { to a appropriating tdef, s gets the name of   }
    { the type to allow name mangling          }
      var
        not_a_type : boolean;
        pos : tfileposinfo;
        s,sorg : TIDString;
        t : ttoken;
      begin
         srsym:=nil;
         srsymtable:=nil;
         is_specialize:=false;
         is_unit_specific:=false;
         s:=current_scanner.pattern;
         sorg:=current_scanner.orgpattern;
         pos:=compiler.globals.current_tokenpos;
         { use of current parsed object:
           classes, objects, records can be used also in themself }
         if checkcurrentrecdef and
            try_parse_structdef_nested_type(def,current_structdef,isforwarddef) then
           exit;
         if not allowunitsym and not (m_delphi in current_settings.modeswitches) and (current_scanner.idtoken=_SPECIALIZE) then
           begin
             parser.pbase.consume(_ID);
             is_specialize:=true;
             s:=current_scanner.pattern;
             sorg:=current_scanner.orgpattern;
             pos:=compiler.globals.current_tokenpos;
           end;
         { Use the special searchsym_type that search only types }
         if not compiler.symtablestack.searchsym_type(s,srsym,srsymtable) then
           { for a good error message we need to know whether the symbol really did not exist or
             whether we found a non-type one }
           not_a_type:=compiler.symtablestack.searchsym(s,srsym,srsymtable)
         else
           not_a_type:=false;
         { handle unit specification like System.Writeln }
         if allowunitsym then
           is_unit_specific:=parser.pbase.try_consume_unitsym(srsym,srsymtable,t,[cuf_consume_id,cuf_allow_specialize],is_specialize,s)
         else
           begin
             t:=_ID;
             is_unit_specific:=false;
           end;
         parser.pbase.consume(t);
         if not_a_type then
           begin
             { reset the symbol and symtable to not leak any unexpected values }
             srsym:=nil;
             srsymtable:=nil;
           end;
         { Types are first defined with an error def before assigning
           the real type so check if it's an errordef. if so then
           give an error. Only check for typesyms in the current symbol
           table as forwarddef are not resolved directly }
         if assigned(srsym) and
            (srsym.typ=typesym) and
            ((ttypesym(srsym).typedef.typ=errordef) or
            (not allowgenericsyms and
            (ttypesym(srsym).typedef.typ=undefineddef) and
            not (sp_generic_para in srsym.symoptions) and
            not (sp_explicitrename in srsym.symoptions) and
            not assigned(srsym.owner.defowner) and
            { use df_generic instead of is_generic to allow aliases in nested types as well }
            not (df_generic in tstoreddef(srsym.owner.defowner).defoptions))) then
          begin
            compiler.verbose.Message1(type_e_type_is_not_completly_defined,ttypesym(srsym).realname);
            def:=generrordef;
            exit;
          end;
         { are we parsing a possible forward def ? }
         if isforwarddef and
            not(is_unit_specific) then
           begin
             def:=cforwarddef.create(sorg,pos,compiler);
             exit;
           end;
         { unknown sym ? }
         if not assigned(srsym) and not not_a_type then
          begin
            compiler.verbose.Message1(sym_e_id_not_found,sorg);
            def:=generrordef;
            exit;
          end;
         { type sym ? }
         if not_a_type or (srsym.typ<>typesym) then
          begin
            compiler.verbose.Message(type_e_type_id_expected);
            def:=generrordef;
            exit;
          end;
         { Give an error when referring to an errordef }
         if (ttypesym(srsym).typedef.typ=errordef) then
          begin
            compiler.verbose.Message(sym_e_error_in_type_def);
            def:=generrordef;
            exit;
          end;
         { In non-Delphi modes the class/record name of a generic might be used
           in the declaration of sub types without type parameters; in that case
           we need to check by name as the link from the dummy symbol to the
           current type is not yet established }
         if (sp_generic_dummy in srsym.symoptions) and
             assigned(current_structdef) and
             (df_generic in current_structdef.defoptions) and
             (ttypesym(srsym).typedef.typ=undefineddef) and
             not (m_delphi in current_settings.modeswitches) then
           begin
             def:=get_generic_in_hierarchy_by_name(srsym,current_structdef);
             if assigned(def) then
               exit;
           end;
        def:=ttypesym(srsym).typedef;
      end;


    procedure TTypesParser.single_type(out def:tdef;options:TSingleTypeOptions);

       function handle_dummysym(sym:tsym):tdef;
         begin
           sym:=parser.pgenutil.resolve_generic_dummysym(sym.name);
           if assigned(sym) and
               not (sp_generic_dummy in sym.symoptions) and
               (sym.typ=typesym) then
             result:=ttypesym(sym).typedef
           else
             begin
               compiler.verbose.Message(parser_e_no_generics_as_types);
               result:=generrordef;
             end;
         end;

       var
         t2 : tdef;
         isunitspecific,
         isspecialize,
         dospecialize,
         again : boolean;
         srsym : tsym;
         srsymtable : tsymtable;
         symname : tsymstr;
       begin
         dospecialize:=false;
         isunitspecific:=false;
         srsymtable:=nil;
         srsym:=nil;
         repeat
           again:=false;
             case current_scanner.token of
               _STRING:
                 parser.pexpr.string_dec(def,stoAllowTypeDef in options);
               _FILE:
                 begin
                    parser.pbase.consume(_FILE);
                    if (current_scanner.token=_OF) then
                      begin
                         if not(stoAllowTypeDef in options) then
                           compiler.verbose.Message(parser_e_no_local_para_def);
                         parser.pbase.consume(_OF);
                         single_type(t2,[stoAllowTypeDef]);
                         if is_managed_type(t2) then
                           compiler.verbose.Message(parser_e_no_refcounted_typed_file);
                         def:=cfiledef.createtyped(t2,compiler);
                      end
                    else
                      def:=cfiletype;
                 end;

               _ID:
                 begin
                   if not (m_delphi in current_settings.modeswitches) and parser.pbase.try_to_consume(_SPECIALIZE) then
                     begin
                       if ([stoAllowSpecialization,stoAllowTypeDef] * options = []) then
                         begin
                           compiler.verbose.Message(parser_e_no_local_para_def);

                           { try to recover }
                           while current_scanner.token<>_SEMICOLON do
                             parser.pbase.consume(current_scanner.token);
                           def:=generrordef;
                         end
                       else
                         begin
                           dospecialize:=true;
                           again:=true;
                         end;
                     end
                   else
                     begin
                       id_type(def,stoIsForwardDef in options,true,true,not dospecialize or ([stoAllowSpecialization,stoAllowTypeDef]*options=[]),srsym,srsymtable,isspecialize,isunitspecific);
                       if isspecialize and dospecialize then
                         internalerror(2015021301);
                       if isspecialize then
                         dospecialize:=true;
                       parse_nested_types(def,stoIsForwardDef in options,[stoAllowSpecialization,stoAllowTypeDef]*options<>[],nil);
                     end;
                 end;

               else
                 begin
                   compiler.verbose.Message(type_e_type_id_expected);
                   def:=generrordef;
                 end;
            end;
        until not again;
        if ([stoAllowSpecialization,stoAllowTypeDef] * options <> []) and
           (m_delphi in current_settings.modeswitches) then
          dospecialize:=current_scanner.token in [_LSHARPBRACKET,_LT];
        if dospecialize and
            (def.typ=forwarddef) then
          begin
            if not assigned(srsym) or not (srsym.typ=typesym) then
              begin
                compiler.verbose.Message1(type_e_type_is_not_completly_defined,def.typename);
                def:=generrordef;
                dospecialize:=false;
              end;
          end;
        { recover from error? }
        if def.typ=errordef then
          begin
            while (current_scanner.token<>_SEMICOLON) and (current_scanner.token<>_RKLAMMER) do
              parser.pbase.consume(current_scanner.token);
          end
        else if dospecialize then
          begin
            if def.typ=forwarddef then
              def:=ttypesym(srsym).typedef;
            if assigned(srsym) then
              symname:=srsym.name
            else if assigned(def.typesym) then
              begin
                symname:=def.typesym.name;
                srsymtable:=def.typesym.owner;
              end
            else if (def.typ=objectdef) then
              begin
                symname:=tobjectdef(def).objname^;
                if assigned(def.owner) then
                  srsymtable:=def.owner
                else
                  srsymtable:=compiler.symtablestack.top;
              end
            else
              symname:='';
            parser.pgenutil.generate_specialization(def,isunitspecific,stoParseClassParent in options,'',symname,srsymtable);
            parse_nested_types(def,stoIsForwardDef in options,[stoAllowSpecialization,stoAllowTypeDef]*options<>[],nil);
          end
        else
          begin
            if assigned(current_specializedef) and (def=current_specializedef.genericdef) then
              begin
                def:=current_specializedef
              end
            else if (def=current_genericdef) then
              begin
                def:=current_genericdef
              end
            { when parsing a nested specialization in non-Delphi mode it might
              use the name of the topmost generic without type paramaters, thus
              def will contain the generic definition, but we need a reference
              to the specialization of that generic }
            { TODO : only in non-Delphi modes? }
            else if assigned(current_structdef) and
                (df_specialization in current_structdef.defoptions) and
                return_specialization_of_generic(current_structdef,def,t2) then
              begin
                def:=t2
              end
            else if tstoreddef(def).is_generic and
                not
                  (
                    parser.pbase.parse_generic and
                    (
                      { if this is a generic parameter than it has already been checked that this is
                        a valid usage of a generic }
                      (sp_generic_para in srsym.symoptions) or
                      (
                        (current_genericdef.typ in [recorddef,objectdef]) and
                        (
                          { if both defs belong to the same generic (e.g. both are
                            subtypes) then we must allow the usage }
                          defs_belong_to_same_generic(def,current_genericdef) or
                          { this is needed to correctly resolve "type Foo=SomeGeneric<T>"
                            declarations inside a generic }
                          sym_is_owned_by(srsym,tabstractrecorddef(current_genericdef).symtable)
                        )
                      )
                    )
                  )
                then
              begin
                def:=handle_dummysym(srsym);
              end
            else if (def.typ=undefineddef) and
                (sp_generic_dummy in srsym.symoptions) then
              begin
                if parser.pbase.parse_generic and
                    (current_genericdef.typ in [recorddef,objectdef]) and
                    (Pos(upper(srsym.realname),tabstractrecorddef(current_genericdef).objname^)=1) then
                  begin
                    if m_delphi in current_settings.modeswitches then
                      begin
                        def:=handle_dummysym(srsym);
                      end
                    else
                      def:=current_genericdef;
                  end
                else
                  begin
                    def:=handle_dummysym(srsym);
                  end;
              end
            else if is_classhelper(def) and
                not (stoParseClassParent in options) then
              begin
                compiler.verbose.Message(parser_e_no_category_as_types);
                def:=generrordef
              end
          end;
      end;


    function TTypesParser.result_type(options:TSingleTypeOptions):tdef;
      begin
        single_type(result,options);
        { file types cannot be function results }
        if result.typ=filedef then
          compiler.verbose.Message(parser_e_illegal_function_result);
      end;

    procedure TTypesParser.parse_record_members(recsym:tsym);

      function IsAnonOrLocal: Boolean;
        begin
          result:=(current_structdef.objname^='') or
                  not(compiler.symtablestack.stack^.next^.symtable.symtabletype in [globalsymtable,staticsymtable,objectsymtable,recordsymtable]);
        end;

      var
        pd : tprocdef;
        oldparse_only: boolean;
        member_blocktype : tblock_type;
        hadgeneric,
        fields_allowed, is_classdef, classfields, threadvarfields: boolean;
        vdoptions: tvar_dec_options;
        rtti_attrs_def: trtti_attribute_list;
        fldCount : Integer;
        attr_element_count : Integer;

      procedure check_unbound_attributes;
        begin
          if assigned(rtti_attrs_def) and (rtti_attrs_def.get_attribute_count>0) then
            compiler.verbose.Message1(parser_e_unbound_attribute,trtti_attribute(rtti_attrs_def.rtti_attributes[0]).typesym.prettyname);
          rtti_attrs_def.free;
          rtti_attrs_def:=nil;
        end;

      begin
        { empty record declaration ? }
        if (current_scanner.token=_SEMICOLON) then
          Exit;

        current_structdef.symtable.currentvisibility:=vis_public;
        fields_allowed:=true;
        is_classdef:=false;
        hadgeneric:=false;
        classfields:=false;
        threadvarfields:=false;
        member_blocktype:=bt_general;
        rtti_attrs_def := nil;
        repeat
          case current_scanner.token of
            _TYPE :
              begin
                check_unbound_attributes;
                parser.pbase.consume(_TYPE);
                member_blocktype:=bt_type;

                { local and anonymous records can not have inner types. skip top record symtable }
                if IsAnonOrLocal then
                  compiler.verbose.Message(parser_e_no_types_in_local_anonymous_records);
              end;
            _VAR :
              begin
                check_unbound_attributes;
                parser.pbase.consume(_VAR);
                fields_allowed:=true;
                member_blocktype:=bt_general;
                classfields:=is_classdef;
                threadvarfields:=false;
                is_classdef:=false;
              end;
            _THREADVAR :
              begin
                check_unbound_attributes;
                if not is_classdef then
                  begin
                    compiler.verbose.Message(parser_e_threadvar_must_be_class);
                    { for error recovery we enforce class fields }
                    is_classdef:=true;
                  end;
                parser.pbase.consume(_THREADVAR);
                fields_allowed:=true;
                member_blocktype:=bt_general;
                classfields:=is_classdef;
                threadvarfields:=true;
                is_classdef:=false;
              end;
            _CONST:
              begin
                check_unbound_attributes;
                parser.pbase.consume(_CONST);
                member_blocktype:=bt_const;

                { local and anonymous records can not have constants. skip top record symtable }
                if IsAnonOrLocal then
                  compiler.verbose.Message(parser_e_no_consts_in_local_anonymous_records);
              end;
            _ID, _CASE, _OPERATOR :
              begin
                case current_scanner.idtoken of
                  _PRIVATE :
                    begin
                      check_unbound_attributes;
                       parser.pbase.consume(_PRIVATE);
                       current_structdef.symtable.currentvisibility:=vis_private;
                       include(current_structdef.objectoptions,oo_has_private);
                       fields_allowed:=true;
                       is_classdef:=false;
                       classfields:=false;
                       threadvarfields:=false;
                       member_blocktype:=bt_general;
                     end;
                   _PROTECTED :
                     begin
                       check_unbound_attributes;
                       compiler.verbose.Message1(parser_e_not_allowed_in_record,tokeninfo^[_PROTECTED].str);
                       parser.pbase.consume(_PROTECTED);
                       current_structdef.symtable.currentvisibility:=vis_protected;
                       include(current_structdef.objectoptions,oo_has_protected);
                       fields_allowed:=true;
                       is_classdef:=false;
                       classfields:=false;
                       threadvarfields:=false;
                       member_blocktype:=bt_general;
                     end;
                   _PUBLIC :
                     begin
                       check_unbound_attributes;
                       parser.pbase.consume(_PUBLIC);
                       current_structdef.symtable.currentvisibility:=vis_public;
                       fields_allowed:=true;
                       is_classdef:=false;
                       classfields:=false;
                       threadvarfields:=false;
                       member_blocktype:=bt_general;
                     end;
                   _PUBLISHED :
                     begin
                       check_unbound_attributes;
                       compiler.verbose.Message(parser_e_no_record_published);
                       parser.pbase.consume(_PUBLISHED);
                       current_structdef.symtable.currentvisibility:=vis_published;
                       fields_allowed:=true;
                       is_classdef:=false;
                       classfields:=false;
                       threadvarfields:=false;
                       member_blocktype:=bt_general;
                     end;
                   _STRICT :
                     begin
                        parser.pbase.consume(_STRICT);
                        if current_scanner.token=_ID then
                          begin
                            case current_scanner.idtoken of
                              _PRIVATE:
                                begin
                                  parser.pbase.consume(_PRIVATE);
                                  current_structdef.symtable.currentvisibility:=vis_strictprivate;
                                  include(current_structdef.objectoptions,oo_has_strictprivate);
                                end;
                              _PROTECTED:
                                begin
                                  { "strict protected" is not allowed for records }
                                  compiler.verbose.Message1(parser_e_not_allowed_in_record,tokeninfo^[_STRICT].str+' '+tokeninfo^[_PROTECTED].str);
                                  parser.pbase.consume(_PROTECTED);
                                  current_structdef.symtable.currentvisibility:=vis_strictprotected;
                                  include(current_structdef.objectoptions,oo_has_strictprotected);
                                end;
                              else
                                compiler.verbose.Message(parser_e_protected_or_private_expected);
                            end;
                          end
                        else
                          compiler.verbose.Message(parser_e_protected_or_private_expected);
                        fields_allowed:=true;
                        is_classdef:=false;
                        classfields:=false;
                        threadvarfields:=false;
                        member_blocktype:=bt_general;
                     end
                    else
                    if is_classdef and (current_scanner.idtoken=_OPERATOR) then
                      begin
                        check_unbound_attributes;
                        pd:=parser.pdecsub.parse_record_method_dec(current_structdef,is_classdef,false);
                        fields_allowed:=false;
                        is_classdef:=false;
                      end
                    else
                      begin
                        if member_blocktype=bt_general then
                          begin
                            if (current_scanner.idtoken=_GENERIC) and
                                not (m_delphi in current_settings.modeswitches) and
                                not fields_allowed then
                              begin
                                if hadgeneric then
                                  compiler.verbose.Message(parser_e_procedure_or_function_expected);
                                parser.pbase.consume(_ID);
                                hadgeneric:=true;
                                if not (current_scanner.token in [_PROCEDURE,_FUNCTION,_CLASS]) then
                                  compiler.verbose.Message(parser_e_procedure_or_function_expected);
                              end
                            else
                              begin
                                if (not fields_allowed)and(current_scanner.idtoken<>_CASE) then
                                  compiler.verbose.Message(parser_e_field_not_allowed_here);
                                vdoptions:=[vd_record];
                                if classfields then
                                  include(vdoptions,vd_class);
                                if not (m_delphi in current_settings.modeswitches) then
                                  include(vdoptions,vd_check_generic);
                                if threadvarfields then
                                  include(vdoptions,vd_threadvar);
                                fldCount:=current_structdef.symtable.SymList.Count;
                                parser.pdecvar.read_record_fields(vdoptions,nil,nil,hadgeneric,attr_element_count);
                                {
                                  attr_element_count returns the number of fields to which the attribute must be applied.
                                  For
                                  [someattr]
                                  a : integer;
                                  b : integer;
                                  attr_element_count returns 1. For
                                  [someattr]
                                  a, b : integer;
                                  it returns 2.
                                  Basically the number of variables before the first colon.
                                }
                                if assigned(rtti_attrs_def) then
                                  begin
                                  While (attr_element_count>1) do
                                    begin
                                    trtti_attribute_list.copyandbind(rtti_attrs_def,(current_structdef.symtable.SymList[fldCount] as tfieldvarsym).rtti_attribute_list);
                                    inc(fldcount);
                                    dec(attr_element_count);
                                    end;
                                  if fldCount<current_structdef.symtable.SymList.Count then
                                    trtti_attribute_list.bind(rtti_attrs_def,(current_structdef.symtable.SymList[fldCount] as tfieldvarsym).rtti_attribute_list);
                                  end;
                              end;
                          end
                        else if member_blocktype=bt_type then
                          parser.pdecl.types_dec(true,hadgeneric, rtti_attrs_def)
                        else if member_blocktype=bt_const then
                          parser.pdecl.consts_dec(true,true,hadgeneric)
                        else
                          internalerror(201001110);
                      end;
                end;
              end;
            _PROPERTY :
              begin
                if IsAnonOrLocal then
                  compiler.verbose.Message(parser_e_no_properties_in_local_anonymous_records);
                parser.pdecobj.struct_property_dec(is_classdef, rtti_attrs_def);
                fields_allowed:=false;
                is_classdef:=false;
              end;
            _CLASS:
              begin
                check_unbound_attributes;
                is_classdef:=false;
                { read class method/field/property }
                parser.pbase.consume(_CLASS);
                { class modifier is only allowed for procedures, functions, }
                { constructors, destructors, fields and properties          }
                if (hadgeneric and not (current_scanner.token in [_FUNCTION,_PROCEDURE])) or
                    (not hadgeneric and (not ((current_scanner.token in [_FUNCTION,_PROCEDURE,_PROPERTY,_VAR,_DESTRUCTOR,_OPERATOR,_THREADVAR]) or (current_scanner.token=_CONSTRUCTOR)) and
                   not((current_scanner.token=_ID) and (current_scanner.idtoken=_OPERATOR)))) then
                  compiler.verbose.Message(parser_e_procedure_or_function_expected);

                if IsAnonOrLocal then
                  compiler.verbose.Message(parser_e_no_class_in_local_anonymous_records);

                is_classdef:=true;
              end;
            _PROCEDURE,
            _FUNCTION:
              begin
                if IsAnonOrLocal then
                  compiler.verbose.Message(parser_e_no_methods_in_local_anonymous_records);
                pd:=parser.pdecsub.parse_record_method_dec(current_structdef,is_classdef,hadgeneric);
                if assigned(rtti_attrs_def) then
                  begin
                  trtti_attribute_list.bind(rtti_attrs_def,pd.rtti_attribute_list);
                  rtti_attrs_def:=Nil;
                  end;
                hadgeneric:=false;
                fields_allowed:=false;
                is_classdef:=false;
              end;
            _CONSTRUCTOR :
              begin
                check_unbound_attributes;
                if IsAnonOrLocal then
                  compiler.verbose.Message(parser_e_no_methods_in_local_anonymous_records);
                if not is_classdef and (current_structdef.symtable.currentvisibility <> vis_public) then
                  compiler.verbose.Message(parser_w_constructor_should_be_public);

                { only 1 class constructor is allowed }
                if is_classdef and (oo_has_class_constructor in current_structdef.objectoptions) then
                  compiler.verbose.Message1(parser_e_only_one_class_constructor_allowed, current_structdef.objrealname^);

                oldparse_only:=parser.pbase.parse_only;
                parser.pbase.parse_only:=true;
                if is_classdef then
                  pd:=parser.pdecobj.class_constructor_head(current_structdef)
                else
                  begin
                    pd:=parser.pdecobj.constructor_head;
                    if pd.minparacount = 0 then
                      compiler.verbose.MessagePos(pd.procsym.fileinfo,parser_e_no_parameterless_constructor_in_records);
                  end;

                parser.pbase.parse_only:=oldparse_only;
                fields_allowed:=false;
                is_classdef:=false;
              end;
            _DESTRUCTOR :
              begin
                check_unbound_attributes;
                if IsAnonOrLocal then
                  compiler.verbose.Message(parser_e_no_methods_in_local_anonymous_records);
                if not is_classdef then
                  compiler.verbose.Message(parser_e_no_destructor_in_records);

                { only 1 class destructor is allowed }
                if is_classdef and (oo_has_class_destructor in current_structdef.objectoptions) then
                  compiler.verbose.Message1(parser_e_only_one_class_destructor_allowed, current_structdef.objrealname^);

                oldparse_only:=parser.pbase.parse_only;
                parser.pbase.parse_only:=true;
                if is_classdef then
                  pd:=parser.pdecobj.class_destructor_head(current_structdef)
                else
                  pd:=parser.pdecobj.destructor_head;

                parser.pbase.parse_only:=oldparse_only;
                fields_allowed:=false;
                is_classdef:=false;
              end;
            _LECKKLAMMER:
              begin
                if m_prefixed_attributes in current_settings.modeswitches then
                  parser.pdecl.parse_rttiattributes(rtti_attrs_def)
                else
                  parser.pbase.consume(_ID);
              end;
            _END :
              begin
{$ifdef jvm}
                add_java_default_record_methods_intf(trecorddef(current_structdef));
{$endif}
                if compiler.target.info.system in systems_typed_constants_node_init then
                  add_typedconst_init_routine(current_structdef);
                parser.pbase.consume(_END);
                break;
              end;
            else
              parser.pbase.consume(_ID); { Give a ident expected message, like tp7 }
          end;
        until false;
      end;

    { reads a record declaration }
    function TTypesParser.record_dec(const n:tidstring;recsym:tsym;genericdef:tstoreddef;genericlist:tfphashobjectlist):tdef;
      var
         olddef : tdef;

      procedure set_typesym;
        begin
          if not assigned(recsym) then
            exit;
          if ttypesym(recsym).typedef=current_structdef then
            exit;
          ttypesym(recsym).typedef:=current_structdef;
          current_structdef.typesym:=recsym;
        end;

      procedure reset_typesym;
        begin
          if not assigned(recsym) then
            exit;
          if ttypesym(recsym).typedef<>current_structdef then
            exit;
          ttypesym(recsym).typedef:=olddef;
          current_structdef.typesym:=nil;
        end;

      var
         old_current_structdef: tabstractrecorddef;
         old_current_genericdef,
         old_current_specializedef: tstoreddef;
         old_parse_generic: boolean;
         recst: trecordsymtable;
         hadgendummy : boolean;
         alignment: Integer;
         dummyattrelcount : Integer;
      begin
         old_current_structdef:=current_structdef;
         old_current_genericdef:=current_genericdef;
         old_current_specializedef:=current_specializedef;
         old_parse_generic:=parser.pbase.parse_generic;

         current_genericdef:=nil;
         current_specializedef:=nil;
         { create recdef }
         if (n<>'') or
            not(compiler.target.info.system in systems_jvm) then
           begin
             recst:=trecordsymtable.create(n,current_settings.packrecords,current_settings.alignment.recordalignmin,compiler);
             { can't use recst.realname^ instead of n, because recst.realname is
               nil in case of an empty name }
             current_structdef:=crecorddef.create(n,recst,compiler);
           end
         else
           begin
             { for the JVM target records always need a name, because they are
               represented by a class }
             recst:=trecordsymtable.create(current_module.realmodulename^+'__fpc_intern_recname_'+tostr(current_module.deflist.count),
               current_settings.packrecords,current_settings.alignment.recordalignmin,compiler);
             current_structdef:=crecorddef.create(recst.name^,recst,compiler);
           end;
         result:=current_structdef;
         { insert in symtablestack }
         compiler.symtablestack.push(recst);

         { usage of specialized type inside its generic template }
         if assigned(genericdef) then
           current_specializedef:=current_structdef
         { reject declaration of generic class inside generic class }
         else if assigned(genericlist) then
           current_genericdef:=current_structdef;

         { nested types of specializations are specializations as well }
         if assigned(old_current_structdef) and
             (df_specialization in old_current_structdef.defoptions) then
           include(current_structdef.defoptions,df_specialization);
         if assigned(old_current_structdef) and
             (df_generic in old_current_structdef.defoptions) then
           include(current_structdef.defoptions,df_generic);

         parser.pgenutil.insert_generic_parameter_types(current_structdef,genericdef,genericlist,false);
         { when we are parsing a generic already then this is a generic as
           well }
         if old_parse_generic then
           include(current_structdef.defoptions, df_generic);
         parser.pbase.parse_generic:=(df_generic in current_structdef.defoptions);
         if parser.pbase.parse_generic and not assigned(current_genericdef) then
           current_genericdef:=current_structdef;
         { in non-Delphi modes we need a strict private symbol without type
           count and type parameters in the name to simply resolving }
         parser.pgenutil.maybe_insert_generic_rename_symbol(n,genericlist);
         { apply $RTTI directive to current object }
         current_structdef.apply_rtti_directive(current_module.rtti_directive);

         { the correct typesym<->def relationship is needed for example when
           parsing parameters that are specializations of the record, when
           using nested constants and such or when specializing the type
           itself as a pointer type }
         if assigned(recsym) then
           olddef:=ttypesym(recsym).typedef
         else
           olddef:=nil;
         set_typesym;

         if m_advanced_records in current_settings.modeswitches then
           begin
             parse_record_members(recsym);
           end
         else
           begin
             parser.pdecvar.read_record_fields([vd_record],nil,nil,hadgendummy,dummyattrelcount);
{$ifdef jvm}
             { we need a constructor to create temps, a deep copy helper, ... }
             add_java_default_record_methods_intf(trecorddef(current_structdef));
{$endif}
             if compiler.target.info.system in systems_typed_constants_node_init then
               add_typedconst_init_routine(current_structdef);
             parser.pbase.consume(_END);
            end;

         reset_typesym;

         if (current_scanner.token=_ID) and (current_scanner.pattern='ALIGN') then
           begin
             parser.pbase.consume(_ID);
             alignment:=parser.pexpr.get_intconst.svalue;
             { "(alignment and not $7F) = 0" means it's between 0 and 127, and
               PopCnt = 1 for powers of 2 }
             if ((alignment and not $7F) <> 0) or (PopCnt(Byte(alignment))<>1) then
               compiler.verbose.Message(scanner_e_illegal_alignment_directive)
             else
               begin
                 recst.recordalignment:=shortint(alignment);
                 recst.explicitrecordalignment:=shortint(alignment);
               end;
           end;
         { make the record size aligned (has to be done before inserting the
           parameters, because that may depend on the record's size) }
         recst.addalignmentpadding;
         { don't keep track of procdefs in a separate list, because the
           compiler may add additional procdefs (e.g. property wrappers for
           the jvm backend) }
         compiler.parser.pparautl.insert_struct_hidden_paras(trecorddef(current_structdef));
         { restore symtable stack }
         compiler.symtablestack.pop(recst);
         if trecorddef(current_structdef).is_packed and is_managed_type(current_structdef) then
           compiler.verbose.Message(type_e_no_packed_inittable);
         { restore old state }
         parser.pbase.parse_generic:=old_parse_generic;
         current_structdef:=old_current_structdef;
         current_genericdef:=old_current_genericdef;
         current_specializedef:=old_current_specializedef;
      end;


    { reads a type definition and returns a pointer to it }
    procedure TTypesParser.read_named_type(var def:tdef;const newsym:tsym;genericdef:tstoreddef;genericlist:tfphashobjectlist;parseprocvardir:boolean;var hadtypetoken:boolean);
      const
        SingleTypeOptionsInTypeBlock:array[Boolean] of TSingleTypeOptions = ([],[stoIsForwardDef]);
      var
        pt : tnode;
        tt2 : tdef;
        aktenumdef : tenumdef;
        s : TIDString;
        l,v : TConstExprInt;
        oldpackrecords : longint;
        defpos,storepos : tfileposinfo;
        name: TIDString;

        procedure expr_type;
        var
           pt1,pt2 : tnode;
           lv,hv   : TConstExprInt;
           old_block_type : tblock_type;
           dospecialize : boolean;
           newdef  : tdef;
           sym     : tsym;
           genstr  : string;
           gencount : longint;
        begin
           old_block_type:=compiler.globals.block_type;
           dospecialize:=false;
           { use of current parsed object:
             classes, objects, records can be used also in themself }
           if (current_scanner.token=_ID) then
             if try_parse_structdef_nested_type(def,current_structdef,false) then
               exit;
           { we can't accept a equal in type }
           pt1:=parser.pexpr.comp_expr([ef_type_only]);
           if parser.pbase.try_to_consume(_POINTPOINT) then
             begin
               { get high value of range }
               pt2:=parser.pexpr.comp_expr([]);
               { make both the same type or give an error. This is not
                 done when both are integer values, because typecasting
                 between -3200..3200 will result in a signed-unsigned
                 conflict and give a range check error (PFV) }
               if not(is_integer(pt1.resultdef) and is_integer(pt2.resultdef)) then
                 inserttypeconv(pt1,pt2.resultdef,compiler);
               { both must be evaluated to constants now }
               if (pt1.nodetype=ordconstn) and
                  (pt2.nodetype=ordconstn) then
                 begin
                   lv:=tordconstnode(pt1).value;
                   hv:=tordconstnode(pt2).value;
                   { Check bounds }
                   if hv<lv then
                     compiler.verbose.Message(parser_e_upper_lower_than_lower)
                   else if (lv.signed and (lv.svalue<0)) and (not hv.signed and (hv.uvalue>qword(high(int64)))) then
                     compiler.verbose.Message(type_e_cant_eval_constant_expr)
                   else
                     begin
                       { All checks passed, create the new def }
                       case pt1.resultdef.typ of
                         enumdef :
                           def:=cenumdef.create_subrange(tenumdef(pt1.resultdef),lv.svalue,hv.svalue,compiler);
                         orddef :
                           begin
                             if is_char(pt1.resultdef) then
                               def:=corddef.create(uchar,lv,hv,true,compiler)
                             else
                               if is_boolean(pt1.resultdef) then
                                 def:=corddef.create(pasbool1,lv,hv,true,compiler)
                               else if is_signed(pt1.resultdef) then
                                 def:=corddef.create(range_to_basetype(lv,hv),lv,hv,true,compiler)
                               else
                                 def:=corddef.create(range_to_basetype(lv,hv),lv,hv,true,compiler);
                           end;
                         else
                           internalerror(2019050527);
                       end;
                     end;
                 end
               else
                 compiler.verbose.Message(sym_e_error_in_type_def);
               pt2.free;
               pt2 := nil;
             end
           else
             begin
               { a simple type renaming or generic specialization }
               if (pt1.nodetype=typen) then
                 begin
                   def:=ttypenode(pt1).resultdef;
                   { Delphi mode specialization? }
                   if (m_delphi in current_settings.modeswitches) then
                     dospecialize:=current_scanner.token=_LSHARPBRACKET
                   else
                     begin
                       dospecialize:=false;
                       { in non-Delphi modes we might get a inline specialization
                         without "specialize" or "<T>" of the same type we're
                         currently parsing, so we need to handle that special }
                       newdef:=nil;
                     end;
                   if not dospecialize and
                       assigned(ttypenode(pt1).typesym) and
                       (ttypenode(pt1).typesym.typ=typesym) and
                       (sp_generic_dummy in ttypenode(pt1).typesym.symoptions) and
                       assigned(current_structdef) and
                       (
                         (
                           not (m_delphi in current_settings.modeswitches) and
                           (ttypesym(ttypenode(pt1).typesym).typedef.typ=undefineddef) and
                           (df_generic in current_structdef.defoptions) and
                           (ttypesym(ttypenode(pt1).typesym).typedef.owner=current_structdef.owner) and
                           (upper(ttypenode(pt1).typesym.realname)=copy(current_structdef.objname^,1,pos('$',current_structdef.objname^)-1))
                         ) or (
                           { this could be a nested specialization which uses
                             the type name of a surrounding generic to
                             reference the specialization of said surrounding
                             class }
                           (df_specialization in current_structdef.defoptions) and
                           return_specialization_of_generic(current_structdef,ttypesym(ttypenode(pt1).typesym).typedef,newdef)
                         )
                       )
                       then
                     begin
                       if assigned(newdef) then
                         def:=newdef
                       else
                         def:=current_structdef;
                       if assigned(def) then
                         { handle nested types }
                         parser.pexpr.post_comp_expr_gendef(def)
                       else
                         def:=generrordef;
                     end;
                   if dospecialize then
                     begin
                       if not assigned(ttypenode(pt1).typesym) then
                         internalerror(2025103102);
                       parser.pgenutil.generate_specialization(def,false,false,name,ttypenode(pt1).typesym.name,ttypenode(pt1).typesym.owner);
                       { handle nested types }
                       if assigned(def) then
                         parser.pexpr.post_comp_expr_gendef(def);
                     end
                   else
                     begin
                       if assigned(current_specializedef) and (def=current_specializedef.genericdef) then
                         begin
                           def:=current_specializedef
                         end
                       else if (def=current_genericdef) then
                         begin
                           def:=current_genericdef
                         end
                       else if tstoreddef(def).is_generic and
                           { TODO : check once nested generics are allowed }
                           not
                             (
                               parser.pbase.parse_generic and
                               (current_genericdef.typ in [recorddef,objectdef]) and
                               (def.typ in [recorddef,objectdef]) and
                               (
                                 { if both defs belong to the same generic (e.g. both are
                                   subtypes) then we must allow the usage }
                                 defs_belong_to_same_generic(def,current_genericdef) or
                                 { this is needed to correctly resolve "type Foo=SomeGeneric<T>"
                                   declarations inside a generic }
                                 (
                                   (ttypenode(pt1).typesym<>nil) and
                                   sym_is_owned_by(ttypenode(pt1).typesym,tabstractrecorddef(current_genericdef).symtable)
                                 )
                               )
                             )
                           then
                         begin
                           if assigned(def.typesym) then
                             begin
                               if ttypesym(def.typesym).typedef.typ<>undefineddef then
                                 { non-Delphi modes... }
                                 parser.pgenutil.split_generic_name(def.typesym.name,genstr,gencount)
                               else
                                 genstr:=def.typesym.name;
                               sym:=parser.pgenutil.resolve_generic_dummysym(genstr);
                             end
                           else
                             sym:=nil;
                           if assigned(sym) and
                               not (sp_generic_dummy in sym.symoptions) and
                               (sym.typ=typesym) then
                             def:=ttypesym(sym).typedef
                           else
                             begin
                               compiler.verbose.Message(parser_e_no_generics_as_types);
                               def:=generrordef;
                             end;
                         end
                       else if is_classhelper(def) then
                         begin
                           compiler.verbose.Message(parser_e_no_category_as_types);
                           def:=generrordef
                         end
                     end;
                 end
               else
                 compiler.verbose.Message(sym_e_error_in_type_def);
             end;
           pt1.free;
           pt1 := nil;
           compiler.globals.block_type:=old_block_type;
        end;


      procedure set_dec;
        begin
          parser.pbase.consume(_SET);
          parser.pbase.consume(_OF);
          read_anon_type(tt2,true,nil);
          if assigned(tt2) then
           begin
             case tt2.typ of
               { don't forget that min can be negativ  PM }
               enumdef :
                 if (tenumdef(tt2).min>=0) and
                    (tenumdef(tt2).max<=255) then
                  // !! def:=csetdef.create(tt2,tenumdef(tt2.def).min,tenumdef(tt2.def).max),true)
                  def:=csetdef.create(tt2,tenumdef(tt2).min,tenumdef(tt2).max,true,compiler)
                 else
                  compiler.verbose.Message(sym_e_ill_type_decl_set);
               orddef :
                 begin
                   if (torddef(tt2).ordtype=uwidechar) then
                     begin
                     if (m_default_unicodestring in current_settings.modeswitches) then
                       begin
                         compiler.verbose.Message(parser_w_widechar_set_reduced);
                         def:=csetdef.create(cansichartype,torddef(cansichartype).low.svalue,torddef(cansichartype).high.svalue,true,compiler);
                       end
                     else
                       compiler.verbose.Message(sym_e_ill_type_decl_set);
                     end
                   else if (torddef(tt2).ordtype<>uvoid) and
                      (torddef(tt2).low>=0) then
                     // !! def:=csetdef.create(tt2,torddef(tt2.def).low,torddef(tt2.def).high),true)
                     if Torddef(tt2).high>int64(high(byte)) then
                       compiler.verbose.Message(sym_e_ill_type_decl_set)
                     else
                       def:=csetdef.create(tt2,torddef(tt2).low.svalue,torddef(tt2).high.svalue,true,compiler)
                   else
                     compiler.verbose.Message(sym_e_ill_type_decl_set);
                 end;
               { generic parameter? }
               undefineddef:
                ;
               else
                 compiler.verbose.Message(sym_e_ill_type_decl_set);
             end;
           end
          else
           def:=generrordef;
        end;


      procedure pointer_dec;
        var
          sym: tsym;
        begin
          parser.pbase.consume(_CARET);
          single_type(tt2,
              SingleTypeOptionsInTypeBlock[compiler.globals.block_type=bt_type]+[stoAllowSpecialization]
            );
          { in case of e.g. var or const sections we need to especially
            check that we don't use a generic dummy symbol }
          if (compiler.globals.block_type<>bt_type) and
              (tt2.typ=undefineddef) and
              assigned(tt2.typesym) and
              (sp_generic_dummy in tt2.typesym.symoptions) then
            begin
              sym:=parser.pgenutil.resolve_generic_dummysym(tt2.typesym.name);
              if assigned(sym) and
                  not (sp_generic_dummy in sym.symoptions) and
                  (sym.typ=typesym) then
                tt2:=ttypesym(sym).typedef
              else
                compiler.verbose.Message(parser_e_no_generics_as_types);
            end;
          { don't use cpointerdef.getreusable() here, since this is a type
            declaration (-> must create new typedef) }
          def:=cpointerdef.create(tt2,compiler);
          if tt2.typ=forwarddef then
            current_module.checkforwarddefs.add(def);
        end;


      procedure array_dec(is_packed:boolean;genericdef:tstoreddef;genericlist:tfphashobjectlist);
        var
          isgeneric : boolean;
          lowval,
          highval   : TConstExprInt;
          indexdef  : tdef;
          hdef      : tdef;
          arrdef    : tarraydef;

        procedure setdefdecl(def:tdef);
          begin
            case def.typ of
              enumdef :
                begin
                  lowval:=tenumdef(def).min;
                  highval:=tenumdef(def).max;
                  if (m_fpc in current_settings.modeswitches) and
                     (tenumdef(def).has_jumps) then
                   compiler.verbose.Message(type_e_array_index_enums_with_assign_not_possible);
                  indexdef:=def;
                end;
              orddef :
                begin
                  if torddef(def).ordtype in [uchar,
                    u8bit,
                    s8bit,s16bit,
{$if defined(cpu32bitaddr) or defined(cpu64bitaddr)}
                    u16bit,s32bit,
{$endif defined(cpu32bitaddr) or defined(cpu64bitaddr)}
{$ifdef cpu64bitaddr}
                    u32bit,s64bit,
{$endif cpu64bitaddr}
                    pasbool1,pasbool8,pasbool16,pasbool32,pasbool64,
                    bool8bit,bool16bit,bool32bit,bool64bit,
                    uwidechar] then
                    begin
                       lowval:=torddef(def).low;
                       highval:=torddef(def).high;
                       indexdef:=def;
                    end
                  else
                    compiler.verbose.Message1(parser_e_type_cant_be_used_in_array_index,def.typename);
                end;
              { generic parameter? }
              undefineddef:
                begin
                  lowval:=0;
                  highval:=1;
                  indexdef:=def;
                  isgeneric:=true;
                end;
              else
                compiler.verbose.Message(sym_e_error_in_type_def);
            end;
          end;

        var
          old_current_genericdef,
          old_current_specializedef: tstoreddef;
          first,
          old_parse_generic: boolean;
        begin
           old_current_genericdef:=current_genericdef;
           old_current_specializedef:=current_specializedef;
           old_parse_generic:=parser.pbase.parse_generic;

           current_genericdef:=nil;
           current_specializedef:=nil;
           first:=true;
           arrdef:=carraydef.create(0,0,s32inttype,compiler);
           parser.pbase.consume(_ARRAY);

           { usage of specialized type inside its generic template }
           if assigned(genericdef) then
             current_specializedef:=arrdef
           { reject declaration of generic class inside generic class }
           else if assigned(genericlist) then
             current_genericdef:=arrdef;
           compiler.symtablestack.push(arrdef.symtable);
           parser.pgenutil.insert_generic_parameter_types(arrdef,genericdef,genericlist,false);
           { there are two possibilities for the following to be true:
             * the array declaration itself is generic
             * the array is declared inside a generic
             in both cases we need "parser.pbase.parse_generic" and "current_genericdef"
             so that e.g. specializations of another generic inside the
             current generic can be used (either inline ones or "type" ones) }
           if old_parse_generic then
             include(arrdef.defoptions,df_generic);
           parser.pbase.parse_generic:=(df_generic in arrdef.defoptions);
           if parser.pbase.parse_generic and not assigned(current_genericdef) then
             current_genericdef:=old_current_genericdef;

           { open array? }
           if parser.pbase.try_to_consume(_LECKKLAMMER) then
             begin
                { defaults }
                indexdef:=generrordef;
                isgeneric:=false;
                { use defaults which don't overflow the compiler }
                lowval:=0;
                highval:=0;
                repeat
                  { read the expression and check it, check apart if the
                    declaration is an enum declaration because that needs to
                    be parsed by readtype (PFV) }
                  if current_scanner.token=_LKLAMMER then
                   begin
                     read_anon_type(hdef,true,nil);
                     setdefdecl(hdef);
                   end
                  else
                   begin
                     pt:=parser.pexpr.expr(true);
                     isgeneric:=false;
                     if pt.nodetype=typen then
                       setdefdecl(pt.resultdef)
                     else
                       begin
                         if pt.nodetype=rangen then
                           begin
                             if nf_generic_para in pt.flags then
                               isgeneric:=true;
                             { pure ordconstn expressions can be checked for
                               generics as well, but don't give an error in case
                               of parsing a generic if that isn't yet the case }
                             if (trangenode(pt).left.nodetype=ordconstn) and
                                (trangenode(pt).right.nodetype=ordconstn) then
                               begin
                                 { make both the same type or give an error. This is not
                                   done when both are integer values, because typecasting
                                   between -3200..3200 will result in a signed-unsigned
                                   conflict and give a range check error (PFV) }
                                 if not(is_integer(trangenode(pt).left.resultdef) and is_integer(trangenode(pt).left.resultdef)) then
                                   inserttypeconv(trangenode(pt).left,trangenode(pt).right.resultdef,compiler);
                                 lowval:=tordconstnode(trangenode(pt).left).value;
                                 highval:=tordconstnode(trangenode(pt).right).value;
                                 if highval<lowval then
                                  begin
                                    { ignore error if node is generic param }
                                    if not (nf_generic_para in pt.flags) then
                                      compiler.verbose.Message(parser_e_array_lower_less_than_upper_bound);
                                    highval:=lowval;
                                  end
                                 else if (lowval<int64(low(asizeint))) or
                                         (highval>high(asizeint)) then
                                   begin
                                     compiler.verbose.Message(parser_e_array_range_out_of_bounds);
                                     lowval :=0;
                                     highval:=0;
                                   end;
                                 if is_integer(trangenode(pt).left.resultdef) then
                                   range_to_type(lowval,highval,indexdef)
                                 else
                                   indexdef:=trangenode(pt).left.resultdef;
                               end
                             else
                               if not parser.pbase.parse_generic then
                                 compiler.verbose.Message(type_e_cant_eval_constant_expr)
                               else
                                 { we need a valid range for debug information }
                                 range_to_type(lowval,highval,indexdef);
                           end
                         else
                           compiler.verbose.Message(sym_e_error_in_type_def)
                       end;
                     pt.free;
                     pt := nil;
                   end;

                  { if we are not at the first dimension, add the new array
                    as element of the existing array, otherwise modify the existing array }
                  if not(first) then
                    begin
                      arrdef.elementdef:=carraydef.create(lowval.svalue,highval.svalue,indexdef,compiler);
                      { push new symtable }
                      compiler.symtablestack.pop(arrdef.symtable);
                      arrdef:=tarraydef(arrdef.elementdef);
                      compiler.symtablestack.push(arrdef.symtable);
                      { correctly update the generic information of the new array def }
                      parser.pgenutil.insert_generic_parameter_types(arrdef,genericdef,genericlist,false);
                      if old_parse_generic then
                        include(arrdef.defoptions,df_generic);
                    end
                  else
                    begin
                      arrdef.lowrange:=lowval.svalue;
                      arrdef.highrange:=highval.svalue;
                      arrdef.rangedef:=indexdef;
                      def:=arrdef;
                      first:=false;
                    end;
                  if is_packed then
                    include(arrdef.arrayoptions,ado_IsBitPacked);
                  if isgeneric then
                    include(arrdef.arrayoptions,ado_IsGeneric);

                  if current_scanner.token=_COMMA then
                    parser.pbase.consume(_COMMA)
                  else
                    break;
                until false;
                parser.pbase.consume(_RECKKLAMMER);
             end
           else
             begin
                if is_packed then
                  compiler.verbose.Message(parser_e_packed_dynamic_open_array);
                arrdef.lowrange:=0;
                arrdef.highrange:=-1;
                arrdef.rangedef:=sizesinttype;
                include(arrdef.arrayoptions,ado_IsDynamicArray);
                def:=arrdef;
             end;
           parser.pbase.consume(_OF);
           read_anon_type(tt2,true,nil);
           { set element type of the last array definition }
           if assigned(arrdef) then
             begin
               compiler.symtablestack.pop(arrdef.symtable);
               arrdef.elementdef:=tt2;
               if is_packed and
                  is_managed_type(tt2) then
                 compiler.verbose.Message(type_e_no_packed_inittable);
             end;
           { restore old state }
           parser.pbase.parse_generic:=old_parse_generic;
           current_genericdef:=old_current_genericdef;
           current_specializedef:=old_current_specializedef;
        end;


        function procvar_dec(genericdef:tstoreddef;genericlist:tfphashobjectlist;sym:tsym;doregister:boolean):tdef;
          var
            is_func:boolean;
            pd:tprocvardef;
            old_current_genericdef,
            old_current_specializedef: tstoreddef;
            old_parse_generic: boolean;
            olddef : tdef;
          begin
            old_current_genericdef:=current_genericdef;
            old_current_specializedef:=current_specializedef;
            old_parse_generic:=parser.pbase.parse_generic;

            current_genericdef:=nil;
            current_specializedef:=nil;
            olddef:=nil;

            is_func:=(current_scanner.token=_FUNCTION);
            if current_scanner.token in [_FUNCTION,_PROCEDURE] then
              parser.pbase.consume(current_scanner.token)
            else
              parser.pbase.consume(_FUNCTION);
            pd:=cprocvardef.create(normal_function_level,doregister,compiler);

            if assigned(sym) then
              begin
                pd.typesym:=sym;
                olddef:=ttypesym(sym).typedef;
                ttypesym(sym).typedef:=pd;
              end;

            { usage of specialized type inside its generic template }
            if assigned(genericdef) then
              current_specializedef:=pd
            { reject declaration of generic class inside generic class }
            else if assigned(genericlist) then
              current_genericdef:=pd;
            compiler.symtablestack.push(pd.parast);
            parser.pgenutil.insert_generic_parameter_types(pd,genericdef,genericlist,false);
            { there are two possibilities for the following to be true:
              * the procvar declaration itself is generic
              * the procvar is declared inside a generic
              in both cases we need "parser.pbase.parse_generic" and "current_genericdef"
              so that e.g. specializations of another generic inside the
              current generic can be used (either inline ones or "type" ones) }
            if old_parse_generic then
              include(pd.defoptions,df_generic);
            parser.pbase.parse_generic:=(df_generic in pd.defoptions);
            if parser.pbase.parse_generic and not assigned(current_genericdef) then
              current_genericdef:=old_current_genericdef;

            if current_scanner.token=_LKLAMMER then
              parser.pdecsub.parse_parameter_dec(pd);
            if is_func then
              begin
                parser.pbase.consume(_COLON);
                pd.proctypeoption:=potype_function;
                pd.returndef:=result_type([stoAllowSpecialization]);
              end
            else
              pd.proctypeoption:=potype_procedure;
            if parser.pbase.try_to_consume(_OF) then
              begin
                parser.pbase.consume(_OBJECT);
                include(pd.procoptions,po_methodpointer);
              end
            else if (m_nested_procvars in current_settings.modeswitches) and
                    parser.pbase.try_to_consume(_IS) then
              begin
                parser.pbase.consume(_NESTED);
                pd.parast.symtablelevel:=normal_function_level+1;
                pd.check_mark_as_nested;
              end;
            compiler.symtablestack.pop(pd.parast);
            { possible proc directives }
            if parseprocvardir then
              begin
                if parser.pdecsub.check_proc_directive(true) then
                  parser.pdecsub.parse_proctype_directives(pd);
                { Add implicit hidden parameters and function result }
                compiler.parser.pparautl.handle_calling_convention(pd,hcc_default_actions_intf);
              end;
            { restore old state }
            parser.pbase.parse_generic:=old_parse_generic;
            current_genericdef:=old_current_genericdef;
            current_specializedef:=old_current_specializedef;

            if assigned(sym) then
              begin
                pd.typesym:=nil;
                ttypesym(sym).typedef:=olddef;
              end;

            result:=pd;
          end;

      var
        p  : tnode;
        hdef : tdef;
        enumdupmsg, first, is_specialize : boolean;
        oldlocalswitches : tlocalswitches;
        bitpacking: boolean;
        stitem: psymtablestackitem;
        sym: tsym;
        st: tsymtable;
      begin
         def:=nil;
         v:=0;
         l:=0;
         if assigned(newsym) then
           name:=newsym.RealName
         else
           name:='';
         { type a = type ..,; syntax is allowed only with type syms and apparently helpers, see below }
         if hadtypetoken and
             (
               (current_scanner.token<>_ID) or
               (
                 (m_function_references in current_settings.modeswitches) and
                 (current_scanner.idtoken=_REFERENCE)
               )
             ) and
             (current_scanner.token<>_STRING) and (current_scanner.token<>_FILE) then
           parser.pbase.consume(_ID);
         case current_scanner.token of
            _STRING,_FILE:
              begin
                if hadtypetoken then
                  single_type(def,[])
                else
                  single_type(def,[stoAllowTypeDef]);
              end;
           _LKLAMMER:
              begin
                parser.pbase.consume(_LKLAMMER);
                first:=true;
                { allow negativ value_str }
                l:=int64(-1);
                enumdupmsg:=false;
                { check that we are not adding an enum from specialization
                  we can't just use current_specializedef because of inner types
                  like specialize array of record }
                is_specialize:=false;
                stitem:=compiler.symtablestack.stack;
                while assigned(stitem) do
                  begin
                    { check records, classes and arrays because they can be specialized }
                    if stitem^.symtable.symtabletype in [recordsymtable,ObjectSymtable,arraysymtable] then
                      begin
                        is_specialize:=is_specialize or (df_specialization in tstoreddef(stitem^.symtable.defowner).defoptions);
                        stitem:=stitem^.next;
                      end
                    else
                      break;
                  end;
                if not is_specialize then
                  aktenumdef:=cenumdef.create(compiler)
                else
                  aktenumdef:=nil;
                repeat
                  { if it is a specialization then search the first enum member
                    and get the member owner instead of just created enumdef }
                  if not assigned(aktenumdef) then
                    begin
                      if not compiler.symtablestack.searchsym(current_scanner.pattern,sym,st) then
                        internalerror(202504121)
                      else if sym.typ=enumsym then
                        aktenumdef:=tenumsym(sym).definition
                      else
                        internalerror(201101021);
                    end;
                  s:=current_scanner.orgpattern;
                  defpos:=compiler.globals.current_tokenpos;
                  parser.pbase.consume(_ID);
                  { only allow assigning of specific numbers under fpc mode }
                  if not(m_tp7 in current_settings.modeswitches) and
                     (
                      { in fpc mode also allow := to be compatible
                        with previous 1.0.x versions }
                      ((m_fpc in current_settings.modeswitches) and
                       parser.pbase.try_to_consume(_ASSIGNMENT)) or
                      parser.pbase.try_to_consume(_EQ)
                     ) then
                    begin
                       oldlocalswitches:=current_settings.localswitches;
                       include(current_settings.localswitches,cs_allow_enum_calc);
                       p:=parser.pexpr.comp_expr([ef_accept_equal]);
                       current_settings.localswitches:=oldlocalswitches;
                       if (p.nodetype=ordconstn) then
                        begin
                          { we expect an integer or an enum of the
                            same type }
                          if is_integer(p.resultdef) or
                             is_char(p.resultdef) or
                             equal_defs(compiler.symtablestack,p.resultdef,aktenumdef) then
                           v:=tordconstnode(p).value
                          else
                           IncompatibleTypes(p.resultdef,s32inttype);
                        end
                       else
                        compiler.verbose.Message(parser_e_illegal_expression);
                       p.free;
                       p := nil;
                       { please leave that a note, allows type save }
                       { declarations in the win32 units ! }
                       if (not first) and (v<=l) and (not enumdupmsg) then
                        begin
                          compiler.verbose.Message(parser_n_duplicate_enum);
                          enumdupmsg:=true;
                        end;
                       l:=v;
                    end
                  else
                    inc(l.svalue);
                  first:=false;
                  { don't generate enum members if this is a specialization because aktenumdef is copied from the generic type }
                  if not is_specialize then
                    begin
                      storepos:=compiler.globals.current_tokenpos;
                      compiler.globals.current_tokenpos:=defpos;
                      if (l.svalue<low(longint)) or (l.svalue>high(longint)) then
                        if m_delphi in current_settings.modeswitches then
                          compiler.verbose.Message(parser_w_enumeration_out_of_range)
                        else
                          compiler.verbose.Message(parser_e_enumeration_out_of_range);
                      tenumsymtable(aktenumdef.symtable).insertsym(cenumsym.create(s,aktenumdef,longint(l.svalue)));
                      if not (cs_scopedenums in current_settings.localswitches) or
                          { also provide the global symbol for anonymous enums }
                          not assigned(newsym) then
                        tstoredsymtable(aktenumdef.owner).insertsym(cenumsym.create(s,aktenumdef,longint(l.svalue)));
                      compiler.globals.current_tokenpos:=storepos;
                    end;
                until not parser.pbase.try_to_consume(_COMMA);
                def:=aktenumdef;
                parser.pbase.consume(_RKLAMMER);
{$ifdef jvm}
                jvm_maybe_create_enum_class(name,def);
{$endif}
              end;
            _ARRAY:
              array_dec(false,genericdef,genericlist);
            _SET:
              set_dec;
            _CARET:
              pointer_dec;
            _RECORD:
              begin
                parser.pbase.consume(current_scanner.token);
                if (current_scanner.idtoken=_HELPER) and (m_advanced_records in current_settings.modeswitches) then
                  begin
                    parser.pbase.consume(_HELPER);
                    def:=parser.pdecobj.object_dec(odt_helper,name,newsym,genericdef,genericlist,nil,ht_record);
                  end
                else
                  def:=record_dec(name,newsym,genericdef,genericlist);
              end;
            _PACKED,
            _BITPACKED:
              begin
                bitpacking :=
                  (cs_bitpacking in current_settings.localswitches) or
                  (current_scanner.token = _BITPACKED);
                parser.pbase.consume(current_scanner.token);
                if current_scanner.token=_ARRAY then
                  array_dec(bitpacking,genericdef,genericlist)
                else if current_scanner.token=_SET then
                  set_dec
                else if current_scanner.token=_FILE then
                  single_type(def,[stoAllowTypeDef])
                else
                  begin
                    oldpackrecords:=current_settings.packrecords;
                    if (not bitpacking) or
                       (current_scanner.token in [_CLASS,_OBJECT]) then
                      current_settings.packrecords:=1
                    else
                      current_settings.packrecords:=bit_alignment;
                    case current_scanner.token of
                      _CLASS :
                        begin
                          parser.pbase.consume(_CLASS);
                          def:=parser.pdecobj.object_dec(odt_class,name,newsym,genericdef,genericlist,nil,ht_none);
                        end;
                      _OBJECT :
                        begin
                          parser.pbase.consume(_OBJECT);
                          def:=parser.pdecobj.object_dec(odt_object,name,newsym,genericdef,genericlist,nil,ht_none);
                        end;
                      else begin
                        parser.pbase.consume(_RECORD);
                        def:=record_dec(name,newsym,genericdef,genericlist);
                      end;
                    end;
                    current_settings.packrecords:=oldpackrecords;
                  end;
              end;
            _DISPINTERFACE :
              begin
                { need extra check here since interface is a keyword
                  in all pascal modes }
                if not(m_class in current_settings.modeswitches) then
                  compiler.verbose.Message(parser_f_need_objfpc_or_delphi_mode);
                parser.pbase.consume(current_scanner.token);
                def:=parser.pdecobj.object_dec(odt_dispinterface,name,newsym,genericdef,genericlist,nil,ht_none);
              end;
            _CLASS :
              begin
                parser.pbase.consume(current_scanner.token);
                { Delphi only allows class of in type blocks }
                if (current_scanner.token=_OF) and
                   (
                    not(m_delphi in current_settings.modeswitches) or
                    (compiler.globals.block_type=bt_type)
                   ) then
                  begin
                    parser.pbase.consume(_OF);
                    single_type(hdef,SingleTypeOptionsInTypeBlock[compiler.globals.block_type=bt_type]);
                    if is_class(hdef) or
                       is_objcclass(hdef) or
                       is_javaclass(hdef) then
                      def:=cclassrefdef.create(hdef,compiler)
                    else
                      if hdef.typ=forwarddef then
                        begin
                          def:=cclassrefdef.create(hdef,compiler);
                          current_module.checkforwarddefs.add(def);
                        end
                    else
                      compiler.verbose.Message1(type_e_class_or_objcclass_type_expected,hdef.typename);
                  end
                else
                if (current_scanner.idtoken=_HELPER) then
                  begin
                    parser.pbase.consume(_HELPER);
                    def:=parser.pdecobj.object_dec(odt_helper,name,newsym,genericdef,genericlist,nil,ht_class);
                  end
                else
                  def:=parser.pdecobj.object_dec(default_class_type,name,newsym,genericdef,genericlist,nil,ht_none);
              end;
            _CPPCLASS :
              begin
                parser.pbase.consume(current_scanner.token);
                def:=parser.pdecobj.object_dec(odt_cppclass,name,newsym,genericdef,genericlist,nil,ht_none);
              end;
            _OBJCCLASS :
              begin
                if not(m_objectivec1 in current_settings.modeswitches) then
                  compiler.verbose.Message(parser_f_need_objc);

                parser.pbase.consume(current_scanner.token);
                def:=parser.pdecobj.object_dec(odt_objcclass,name,newsym,genericdef,genericlist,nil,ht_none);
              end;
            _INTERFACE :
              begin
                { need extra check here since interface is a keyword
                  in all pascal modes }
                if not(m_class in current_settings.modeswitches) then
                  compiler.verbose.Message(parser_f_need_objfpc_or_delphi_mode);
                parser.pbase.consume(current_scanner.token);
                case current_settings.interfacetype of
                  it_interfacecom:
                    def:=parser.pdecobj.object_dec(odt_interfacecom,name,newsym,genericdef,genericlist,nil,ht_none);
                  it_interfacecorba:
                    def:=parser.pdecobj.object_dec(odt_interfacecorba,name,newsym,genericdef,genericlist,nil,ht_none);
                  it_interfacejava:
                    def:=parser.pdecobj.object_dec(odt_interfacejava,name,newsym,genericdef,genericlist,nil,ht_none);
                end;
              end;
            _OBJCPROTOCOL :
               begin
                if not(m_objectivec1 in current_settings.modeswitches) then
                  compiler.verbose.Message(parser_f_need_objc);

                parser.pbase.consume(current_scanner.token);
                def:=parser.pdecobj.object_dec(odt_objcprotocol,name,newsym,genericdef,genericlist,nil,ht_none);
               end;
            _OBJCCATEGORY :
               begin
                if not(m_objectivec1 in current_settings.modeswitches) then
                  compiler.verbose.Message(parser_f_need_objc);

                parser.pbase.consume(current_scanner.token);
                def:=parser.pdecobj.object_dec(odt_objccategory,name,newsym,genericdef,genericlist,nil,ht_none);
               end;
            _OBJECT :
              begin
                parser.pbase.consume(current_scanner.token);
                def:=parser.pdecobj.object_dec(odt_object,name,newsym,genericdef,genericlist,nil,ht_none);
              end;
            _PROCEDURE,
            _FUNCTION:
              begin
                def:=procvar_dec(genericdef,genericlist,nil,true);
{$ifdef jvm}
                jvm_create_procvar_class(name,def);
{$endif}
              end;
            _ID:
              begin
                case current_scanner.idtoken of
                  _HELPER:
                    begin
                      if hadtypetoken and
                         (m_type_helpers in current_settings.modeswitches) then
                        begin
                          { reset hadtypetoken, so that calling code knows that it should not be handled
                            as a "unique" type }
                          hadtypetoken:=false;
                          parser.pbase.consume(_HELPER);
                          def:=parser.pdecobj.object_dec(odt_helper,name,newsym,genericdef,genericlist,nil,ht_type);
                        end
                      else
                        expr_type
                    end;
                  _REFERENCE:
                    begin
                      if current_settings.modeswitches*[m_blocks,m_function_references]<>[] then
                        begin
                          parser.pbase.consume(_REFERENCE);
                          parser.pbase.consume(_TO);
                          { don't register the def as a non-cblock function
                            reference will be converted to an interface }
                          def:=procvar_dec(genericdef,genericlist,newsym,false);
                          { could be errordef in case of a syntax error }
                          if assigned(def) and
                             (def.typ=procvardef) then
                            begin
                              include(tprocvardef(def).procoptions,po_is_function_ref);
                            end;
                        end
                      else
                        expr_type;
                    end;
                  else
                    expr_type;
                end;
              end
            else
              if (current_scanner.token=_KLAMMERAFFE) and (([m_iso,m_extpas]*current_settings.modeswitches)<>[]) then
                begin
                  parser.pbase.consume(_KLAMMERAFFE);
                  single_type(tt2,SingleTypeOptionsInTypeBlock[compiler.globals.block_type=bt_type]);
                  def:=cpointerdef.create(tt2,compiler);
                  if tt2.typ=forwarddef then
                    current_module.checkforwarddefs.add(def);
                end
              else
                expr_type;
         end;

         if def=nil then
          def:=generrordef;
      end;


    procedure TTypesParser.read_anon_type(var def : tdef;parseprocvardir:boolean;genericdef:tstoreddef);
      var
        hadtypetoken : boolean;
      begin
        hadtypetoken:=false;
        read_named_type(def,nil,genericdef,nil,parseprocvardir,hadtypetoken);
      end;




    procedure TTypesParser.add_typedconst_init_routine(def: tabstractrecorddef);
      var
        sstate: tscannerstate;
        pd: tprocdef;
      begin
        replace_scanner('tcinit_routine',sstate);
        { the typed constant initialization code is called from the class
          constructor by tnodeutils.wrap_proc_body; at this point, we don't
          know yet whether that will be necessary, because there may be
          typed constants inside method bodies -> always force the addition
          of a class constructor.

          We cannot directly add the typed constant initializations to the
          class constructor, because when it's parsed not all method bodies
          are necessarily already parsed }
        pd:=def.find_procdef_bytype(potype_class_constructor);
        { the class constructor }
        if not assigned(pd) then
          begin
            if str_parse_method_dec('constructor fpc_init_typed_consts_class_constructor;',potype_class_constructor,true,def,pd) then
              pd.synthetickind:=tsk_empty
            else
              internalerror(2011040206);
          end;
        { the initialisation helper }
        if str_parse_method_dec('procedure fpc_init_typed_consts_helper; static;',potype_procedure,true,def,pd) then
          pd.synthetickind:=tsk_tcinit
        else
          internalerror(2011040207);
        restore_scanner(sstate);
      end;


end.
