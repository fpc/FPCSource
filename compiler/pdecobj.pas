{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Does object types for Free Pascal

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
unit pdecobj;

{$i fpcdefs.inc}

interface

    uses
      cclasses,
      globtype,symtype,symdef;

    { parses a object declaration }
    function object_dec(const n : TIDString;genericdef:tstoreddef;genericlist:TFPObjectList;fd : tobjectdef) : tdef;

implementation

    uses
      cutils,
      globals,verbose,systems,tokens,
      symconst,symbase,symsym,symtable,
      node,nld,nmem,ncon,ncnv,ncal,
      scanner,
      pbase,pexpr,pdecsub,pdecvar,ptype,pdecl
      ;

    const
      { Please leave this here, this module should NOT use
        these variables.
        Declaring it as string here results in an error when compiling (PFV) }
      current_procinfo = 'error';


    function object_dec(const n : TIDString;genericdef:tstoreddef;genericlist:TFPObjectList;fd : tobjectdef) : tdef;
    { this function parses an object or class declaration }
      var
         there_is_a_destructor : boolean;
         classtype : tobjecttyp;
         pcrd      : tclassrefdef;
         hdef      : tdef;
         old_object_option : tsymoptions;
         oldparse_only : boolean;
         storetypecanbeforward : boolean;


      function constructor_head:tprocdef;
        var
          pd : tprocdef;
        begin
           consume(_CONSTRUCTOR);
           { must be at same level as in implementation }
           parse_proc_head(aktobjectdef,potype_constructor,pd);
           if not assigned(pd) then
             begin
               consume(_SEMICOLON);
               exit;
             end;
           if (cs_constructor_name in current_settings.globalswitches) and
              (pd.procsym.name<>'INIT') then
             Message(parser_e_constructorname_must_be_init);
           consume(_SEMICOLON);
           include(aktobjectdef.objectoptions,oo_has_constructor);
           { Set return type, class constructors return the
             created instance, object constructors return boolean }
           if is_class(pd._class) then
             pd.returndef:=pd._class
           else
{$ifdef CPU64bitaddr}
             pd.returndef:=bool64type;
{$else CPU64bitaddr}
             pd.returndef:=bool32type;
{$endif CPU64bitaddr}
           constructor_head:=pd;
        end;


      procedure property_dec;
        var
          p : tpropertysym;
        begin
           { check for a class }
           if not((is_class_or_interface_or_dispinterface(aktobjectdef)) or
              (not(m_tp7 in current_settings.modeswitches) and (is_object(aktobjectdef)))) then
             Message(parser_e_syntax_error);
           consume(_PROPERTY);
           p:=read_property_dec(aktobjectdef);
           consume(_SEMICOLON);
           if try_to_consume(_DEFAULT) then
             begin
               if oo_has_default_property in aktobjectdef.objectoptions then
                 message(parser_e_only_one_default_property);
               include(aktobjectdef.objectoptions,oo_has_default_property);
               include(p.propoptions,ppo_defaultproperty);
               if not(ppo_hasparameters in p.propoptions) then
                 message(parser_e_property_need_paras);
               consume(_SEMICOLON);
             end;
           { hint directives, these can be separated by semicolons here,
             that needs to be handled here with a loop (PFV) }
           while try_consume_hintdirective(p.symoptions) do
             Consume(_SEMICOLON);
        end;


      function destructor_head:tprocdef;
        var
          pd : tprocdef;
        begin
           consume(_DESTRUCTOR);
           parse_proc_head(aktobjectdef,potype_destructor,pd);
           if not assigned(pd) then
             begin
               consume(_SEMICOLON);
               exit;
             end;
           if (cs_constructor_name in current_settings.globalswitches) and
              (pd.procsym.name<>'DONE') then
             Message(parser_e_destructorname_must_be_done);
           if not(pd.maxparacount=0) and
              (m_fpc in current_settings.modeswitches) then
             Message(parser_e_no_paras_for_destructor);
           consume(_SEMICOLON);
           include(aktobjectdef.objectoptions,oo_has_destructor);
           { no return value }
           pd.returndef:=voidtype;
           destructor_head:=pd;
        end;

      procedure setclassattributes;

        begin
           { publishable }
           if classtype in [odt_interfacecom,odt_class] then
             begin
                aktobjectdef.objecttype:=classtype;
                { set published flag in $M+ mode or it is inherited }
                if (cs_generate_rtti in current_settings.localswitches) or
                    (assigned(aktobjectdef.childof) and
                     (oo_can_have_published in aktobjectdef.childof.objectoptions)) then
                  include(aktobjectdef.objectoptions,oo_can_have_published);
                { in "publishable" classes the default access type is published, this is
                  done separate from above if-statement because the option can be
                  inherited from the forward class definition }
                if (oo_can_have_published in aktobjectdef.objectoptions) then
                  current_object_option:=[sp_published];
             end;
        end;


      procedure setinterfacemethodoptions;

        var
          i   : longint;
          def : tdef;
        begin
          include(aktobjectdef.objectoptions,oo_has_virtual);
          for i:=0 to aktobjectdef.symtable.DefList.count-1 do
            begin
              def:=tdef(aktobjectdef.symtable.DefList[i]);
              if assigned(def) and
                 (def.typ=procdef) then
                begin
                  include(tprocdef(def).procoptions,po_virtualmethod);
                  tprocdef(def).forwarddef:=false;
                end;
            end;
        end;

      function readobjecttype : boolean;

        begin
           readobjecttype:=true;
           { distinguish classes and objects }
           case token of
              _OBJECT:
                begin
                   classtype:=odt_object;
                   consume(_OBJECT)
                end;
              _CPPCLASS:
                begin
                   classtype:=odt_cppclass;
                   consume(_CPPCLASS);
                end;
              _DISPINTERFACE:
                begin
                   { need extra check here since interface is a keyword
                     in all pascal modes }
                   if not(m_class in current_settings.modeswitches) then
                     Message(parser_f_need_objfpc_or_delphi_mode);
                   classtype:=odt_dispinterface;
                   consume(_DISPINTERFACE);
                   { no forward declaration }
                   if not(assigned(fd)) and (token=_SEMICOLON) then
                     begin
                       { also anonym objects aren't allow (o : object a : longint; end;) }
                       if n='' then
                         Message(parser_f_no_anonym_objects);
                       aktobjectdef:=tobjectdef.create(classtype,n,nil);
                       include(aktobjectdef.objectoptions,oo_is_forward);
                       object_dec:=aktobjectdef;
                       typecanbeforward:=storetypecanbeforward;
                       readobjecttype:=false;
                       exit;
                     end;
                end;
              _INTERFACE:
                begin
                   { need extra check here since interface is a keyword
                     in all pascal modes }
                   if not(m_class in current_settings.modeswitches) then
                     Message(parser_f_need_objfpc_or_delphi_mode);
                   if current_settings.interfacetype=it_interfacecom then
                     classtype:=odt_interfacecom
                   else {it_interfacecorba}
                     classtype:=odt_interfacecorba;
                   consume(_INTERFACE);
                   { forward declaration }
                   if not(assigned(fd)) and (token=_SEMICOLON) then
                     begin
                       { also anonym objects aren't allow (o : object a : longint; end;) }
                       if n='' then
                         Message(parser_f_no_anonym_objects);
                       aktobjectdef:=tobjectdef.create(classtype,n,nil);
                       if (cs_compilesystem in current_settings.moduleswitches) and
                          (classtype=odt_interfacecom) and (upper(n)='IUNKNOWN') then
                         interface_iunknown:=aktobjectdef;
                       include(aktobjectdef.objectoptions,oo_is_forward);
                       if (cs_generate_rtti in current_settings.localswitches) and
                          (classtype=odt_interfacecom) then
                         include(aktobjectdef.objectoptions,oo_can_have_published);
                       object_dec:=aktobjectdef;
                       typecanbeforward:=storetypecanbeforward;
                       readobjecttype:=false;
                       exit;
                     end;
                end;
              _CLASS:
                begin
                   classtype:=odt_class;
                   consume(_CLASS);
                   if not(assigned(fd)) and
                      (token=_OF) and
                      { Delphi only allows class of in type blocks.
                        Note that when parsing the type of a variable declaration
                        the blocktype is bt_type so the check for typecanbeforward
                        is also necessary (PFV) }
                      (((block_type=bt_type) and typecanbeforward) or
                       not(m_delphi in current_settings.modeswitches)) then
                     begin
                        { a hack, but it's easy to handle
                          class reference type }
                        consume(_OF);
                        single_type(hdef,typecanbeforward);

                        { accept hp1, if is a forward def or a class }
                        if (hdef.typ=forwarddef) or
                           is_class(hdef) then
                          begin
                             pcrd:=tclassrefdef.create(hdef);
                             object_dec:=pcrd;
                          end
                        else
                          begin
                             object_dec:=generrordef;
                             Message1(type_e_class_type_expected,generrordef.typename);
                          end;
                        typecanbeforward:=storetypecanbeforward;
                        readobjecttype:=false;
                        exit;
                     end
                   { forward class }
                   else if not(assigned(fd)) and (token=_SEMICOLON) then
                     begin
                        { also anonym objects aren't allow (o : object a : longint; end;) }
                        if n='' then
                          Message(parser_f_no_anonym_objects);
                        aktobjectdef:=tobjectdef.create(odt_class,n,nil);
                        if (cs_compilesystem in current_settings.moduleswitches) and (upper(n)='TOBJECT') then
                          class_tobject:=aktobjectdef;
                        aktobjectdef.objecttype:=odt_class;
                        include(aktobjectdef.objectoptions,oo_is_forward);
                        if (cs_generate_rtti in current_settings.localswitches) then
                          include(aktobjectdef.objectoptions,oo_can_have_published);
                        { all classes must have a vmt !!  at offset zero }
                        if not(oo_has_vmt in aktobjectdef.objectoptions) then
                          aktobjectdef.insertvmt;
                        object_dec:=aktobjectdef;
                        typecanbeforward:=storetypecanbeforward;
                        readobjecttype:=false;
                        exit;
                     end;
                end;
              else
                begin
                   classtype:=odt_class; { this is error but try to recover }
                   consume(_OBJECT);
                end;
           end;
        end;

      procedure handleImplementedInterface(intfdef : tobjectdef);

        begin
            if not is_interface(intfdef) then
              begin
                 Message1(type_e_interface_type_expected,intfdef.typename);
                 exit;
              end;
            if aktobjectdef.find_implemented_interface(intfdef)<>nil then
              Message1(sym_e_duplicate_id,intfdef.objname^)
            else
              begin
                { allocate and prepare the GUID only if the class
                  implements some interfaces. }
                if aktobjectdef.ImplementedInterfaces.count = 0 then
                  aktobjectdef.prepareguid;
                aktobjectdef.ImplementedInterfaces.Add(TImplementedInterface.Create(intfdef));
              end;
        end;

      procedure readImplementedInterfaces;
        var
          hdef : tdef;
        begin
          while try_to_consume(_COMMA) do
            begin
               id_type(hdef,false);
               if (hdef.typ<>objectdef) then
                 begin
                    Message1(type_e_interface_type_expected,hdef.typename);
                    continue;
                 end;
               handleImplementedInterface(tobjectdef(hdef));
            end;
        end;

      procedure readinterfaceiid;
        var
          p : tnode;
          valid : boolean;
        begin
          p:=comp_expr(true);
          if p.nodetype=stringconstn then
            begin
              stringdispose(aktobjectdef.iidstr);
              aktobjectdef.iidstr:=stringdup(strpas(tstringconstnode(p).value_str)); { or upper? }
              p.free;
              valid:=string2guid(aktobjectdef.iidstr^,aktobjectdef.iidguid^);
              if (classtype in [odt_interfacecom,odt_dispinterface]) and not assigned(aktobjectdef.iidguid) and not valid then
                Message(parser_e_improper_guid_syntax);
              include(aktobjectdef.objectoptions,oo_has_valid_guid);
            end
          else
            begin
              p.free;
              Message(parser_e_illegal_expression);
            end;
        end;


      procedure readparentclasses;
        var
           intfchildof,
           childof : tobjectdef;
           hdef : tdef;
           hasparentdefined : boolean;
        begin
          childof:=nil;
          intfchildof:=nil;
          hasparentdefined:=false;

          { reads the parent class }
          if try_to_consume(_LKLAMMER) then
            begin
              { use single_type instead of id_type for specialize support }
              single_type(hdef,false);
              if (not assigned(hdef)) or
                 (hdef.typ<>objectdef) then
                begin
                  if assigned(hdef) then
                    Message1(type_e_class_type_expected,hdef.typename);
                end
              else
                begin
                  childof:=tobjectdef(hdef);
                  { a mix of class, interfaces, objects and cppclasses
                    isn't allowed }
                  case classtype of
                     odt_class:
                       if not(is_class(childof)) then
                         begin
                            if is_interface(childof) then
                              begin
                                 { we insert the interface after the child
                                   is set, see below
                                 }
                                 intfchildof:=childof;
                                 childof:=class_tobject;
                              end
                            else
                              Message(parser_e_mix_of_classes_and_objects);
                         end;
                     odt_interfacecorba,
                     odt_interfacecom:
                       begin
                         if not(is_interface(childof)) then
                           Message(parser_e_mix_of_classes_and_objects);
                         classtype:=childof.objecttype;
                         aktobjectdef.objecttype:=classtype;
                       end;
                     odt_cppclass:
                       if not(is_cppclass(childof)) then
                         Message(parser_e_mix_of_classes_and_objects);
                     odt_object:
                       if not(is_object(childof)) then
                         Message(parser_e_mix_of_classes_and_objects);
                     odt_dispinterface:
                       Message(parser_e_dispinterface_cant_have_parent);
                  end;
                end;
              hasparentdefined:=true;
            end;

          { no generic as parents }
          if assigned(childof) and
             (df_generic in childof.defoptions) then
            begin
              Message(parser_e_no_generics_as_types);
              childof:=nil;
            end;

          { if no parent class, then a class get tobject as parent }
          if not assigned(childof) then
            begin
              case classtype of
                odt_class:
                  if aktobjectdef<>class_tobject then
                    childof:=class_tobject;
                odt_interfacecom:
                  if aktobjectdef<>interface_iunknown then
                    childof:=interface_iunknown;
              end;
            end;

          if assigned(childof) then
            begin
              { Forbid not completly defined objects to be used as parents. This will
                also prevent circular loops of classes, because we set the forward flag
                at the start of the new definition and will reset it below after the
                parent has been set }
              if not(oo_is_forward in childof.objectoptions) then
                aktobjectdef.set_parent(childof)
              else
                Message1(parser_e_forward_declaration_must_be_resolved,childof.objrealname^);
            end;

          { remove forward flag, is resolved }
          exclude(aktobjectdef.objectoptions,oo_is_forward);

          if hasparentdefined then
            begin
              if aktobjectdef.objecttype=odt_class then
                begin
                  if assigned(intfchildof) then
                    handleImplementedInterface(intfchildof);
                  readImplementedInterfaces;
                end;
              consume(_RKLAMMER);
            end;

          { read GUID }
          if (classtype in [odt_interfacecom,odt_interfacecorba,odt_dispinterface]) and
             try_to_consume(_LECKKLAMMER) then
            begin
              readinterfaceiid;
              consume(_RECKKLAMMER);
            end
          else if (classtype=odt_dispinterface) then
            message(parser_e_dispinterface_needs_a_guid);
        end;

        procedure chkcpp(pd:tprocdef);
        begin
           if is_cppclass(pd._class) then
            begin
              pd.proccalloption:=pocall_cppdecl;
              pd.setmangledname(target_info.Cprefix+pd.cplusplusmangledname);
            end;
        end;

      var
        pd : tprocdef;
        dummysymoptions : tsymoptions;
        i : longint;
        generictype : ttypesym;
        current_blocktype : tblock_type;
        oldaktobjectdef : tobjectdef;
        old_parse_generic : boolean;
      begin
         old_object_option:=current_object_option;
         oldaktobjectdef:=aktobjectdef;
         old_parse_generic:=parse_generic;

         { objects and class types can't be declared local }
         if not(symtablestack.top.symtabletype in [globalsymtable,staticsymtable]) and
            not assigned(genericlist) then
           Message(parser_e_no_local_objects);

         storetypecanbeforward:=typecanbeforward;
         { for tp7 don't allow forward types }
         if (m_tp7 in current_settings.modeswitches) then
           typecanbeforward:=false;

         if not(readobjecttype) then
           exit;

         if assigned(fd) then
           aktobjectdef:=fd
         else
           begin
             { anonym objects aren't allow (o : object a : longint; end;) }
             if n='' then
               Message(parser_f_no_anonym_objects);
             aktobjectdef:=tobjectdef.create(classtype,n,nil);
             { include forward flag, it'll be removed after the parent class have been
               added. This is to prevent circular childof loops }
             include(aktobjectdef.objectoptions,oo_is_forward);
           end;

         { read list of parent classes }
         readparentclasses;

         { default access is public }
         there_is_a_destructor:=false;
         current_object_option:=[sp_public];

         { set class flags and inherits published }
         setclassattributes;

         symtablestack.push(aktobjectdef.symtable);
         testcurobject:=1;

         { add generic type parameters }
         aktobjectdef.genericdef:=genericdef;
         if assigned(genericlist) then
           begin
             for i:=0 to genericlist.count-1 do
               begin
                 generictype:=ttypesym(genericlist[i]);
                 if generictype.typedef.typ=undefineddef then
                   begin
                     include(aktobjectdef.defoptions,df_generic);
                     parse_generic:=true;
                   end
                 else
                   include(aktobjectdef.defoptions,df_specialization);
                 symtablestack.top.insert(generictype);
               end;
           end;

         { short class declaration ? }
         if (classtype<>odt_class) or (token<>_SEMICOLON) then
          begin
            { Parse componenten }
            current_blocktype:=bt_general;
            repeat
              case token of
                _TYPE :
                  begin
                    if ([df_generic,df_specialization]*aktobjectdef.defoptions)=[] then
                      Message(parser_e_type_and_var_only_in_generics);
                     consume(_TYPE);
                     current_blocktype:=bt_type;
                  end;
                _VAR :
                  begin
                    if ([df_generic,df_specialization]*aktobjectdef.defoptions)=[] then
                      Message(parser_e_type_and_var_only_in_generics);
                    consume(_VAR);
                    current_blocktype:=bt_general;
                  end;
                _ID :
                  begin
                    case idtoken of
                      _PRIVATE :
                        begin
                          if is_interface(aktobjectdef) then
                             Message(parser_e_no_access_specifier_in_interfaces);
                           consume(_PRIVATE);
                           current_object_option:=[sp_private];
                           include(aktobjectdef.objectoptions,oo_has_private);
                         end;
                       _PROTECTED :
                         begin
                           if is_interface(aktobjectdef) then
                             Message(parser_e_no_access_specifier_in_interfaces);
                           consume(_PROTECTED);
                           current_object_option:=[sp_protected];
                           include(aktobjectdef.objectoptions,oo_has_protected);
                         end;
                       _PUBLIC :
                         begin
                           if is_interface(aktobjectdef) then
                             Message(parser_e_no_access_specifier_in_interfaces);
                           consume(_PUBLIC);
                           current_object_option:=[sp_public];
                         end;
                       _PUBLISHED :
                         begin
                           { we've to check for a pushlished section in non-  }
                           { publishable classes later, if a real declaration }
                           { this is the way, delphi does it                  }
                           if is_interface(aktobjectdef) then
                             Message(parser_e_no_access_specifier_in_interfaces);
                           consume(_PUBLISHED);
                           current_object_option:=[sp_published];
                         end;
                       _STRICT :
                         begin
                           if is_interface(aktobjectdef) then
                              Message(parser_e_no_access_specifier_in_interfaces);
                            consume(_STRICT);
                            if token=_ID then
                              begin
                                case idtoken of
                                  _PRIVATE:
                                    begin
                                      consume(_PRIVATE);
                                      current_object_option:=[sp_strictprivate];
                                      include(aktobjectdef.objectoptions,oo_has_strictprivate);
                                    end;
                                  _PROTECTED:
                                    begin
                                      consume(_PROTECTED);
                                      current_object_option:=[sp_strictprotected];
                                      include(aktobjectdef.objectoptions,oo_has_strictprotected);
                                    end;
                                  else
                                    message(parser_e_protected_or_private_expected);
                                end;
                              end
                            else
                              message(parser_e_protected_or_private_expected);
                          end;
                        else
                          begin
                            if current_blocktype=bt_general then
                              begin
                                if is_interface(aktobjectdef) then
                                  Message(parser_e_no_vars_in_interfaces);

                                if (sp_published in current_object_option) and
                                  not(oo_can_have_published in aktobjectdef.objectoptions) then
                                  Message(parser_e_cant_have_published);

                                read_record_fields([vd_object])
                              end
                            else
                              types_dec;
                          end;
                    end;
                  end;
                _PROPERTY :
                  begin
                    property_dec;
                  end;
                _PROCEDURE,
                _FUNCTION,
                _CLASS :
                  begin
                    if (sp_published in current_object_option) and
                       not(oo_can_have_published in aktobjectdef.objectoptions) then
                      Message(parser_e_cant_have_published);

                    oldparse_only:=parse_only;
                    parse_only:=true;
                    pd:=parse_proc_dec(aktobjectdef);

                    { this is for error recovery as well as forward }
                    { interface mappings, i.e. mapping to a method  }
                    { which isn't declared yet                      }
                    if assigned(pd) then
                     begin
                       parse_object_proc_directives(pd);

                       { all Macintosh Object Pascal methods are virtual.  }
                       { this can't be a class method, because macpas mode }
                       { has no m_class                                    }
                       if (m_mac in current_settings.modeswitches) then
                         include(pd.procoptions,po_virtualmethod);

                       handle_calling_convention(pd);

                       { add definition to procsym }
                       proc_add_definition(pd);

                       { add procdef options to objectdef options }
                       if (po_msgint in pd.procoptions) then
                        include(aktobjectdef.objectoptions,oo_has_msgint);
                       if (po_msgstr in pd.procoptions) then
                         include(aktobjectdef.objectoptions,oo_has_msgstr);
                       if (po_virtualmethod in pd.procoptions) then
                         include(aktobjectdef.objectoptions,oo_has_virtual);

                       chkcpp(pd);
                     end;

                    { Support hint directives }
                    dummysymoptions:=[];
                    while try_consume_hintdirective(dummysymoptions) do
                      Consume(_SEMICOLON);
                    if assigned(pd) then
                      pd.symoptions:=pd.symoptions+dummysymoptions;

                    parse_only:=oldparse_only;
                  end;
                _CONSTRUCTOR :
                  begin
                    if (sp_published in current_object_option) and
                      not(oo_can_have_published in aktobjectdef.objectoptions) then
                      Message(parser_e_cant_have_published);

                    if not(sp_public in current_object_option) and
                       not(sp_published in current_object_option) then
                      Message(parser_w_constructor_should_be_public);

                    if is_interface(aktobjectdef) then
                      Message(parser_e_no_con_des_in_interfaces);

                    oldparse_only:=parse_only;
                    parse_only:=true;
                    pd:=constructor_head;
                    parse_object_proc_directives(pd);
                    handle_calling_convention(pd);

                    { add definition to procsym }
                    proc_add_definition(pd);

                    { add procdef options to objectdef options }
                    if (po_virtualmethod in pd.procoptions) then
                      include(aktobjectdef.objectoptions,oo_has_virtual);
                    chkcpp(pd);

                    { Support hint directives }
                    dummysymoptions:=[];
                    while try_consume_hintdirective(dummysymoptions) do
                      Consume(_SEMICOLON);
                    if assigned(pd) then
                      pd.symoptions:=pd.symoptions+dummysymoptions;

                    parse_only:=oldparse_only;
                  end;
                _DESTRUCTOR :
                  begin
                    if (sp_published in current_object_option) and
                      not(oo_can_have_published in aktobjectdef.objectoptions) then
                      Message(parser_e_cant_have_published);

                    if there_is_a_destructor then
                      Message(parser_n_only_one_destructor);

                    if is_interface(aktobjectdef) then
                      Message(parser_e_no_con_des_in_interfaces);

                    if not(sp_public in current_object_option) then
                      Message(parser_w_destructor_should_be_public);

                    there_is_a_destructor:=true;
                    oldparse_only:=parse_only;
                    parse_only:=true;
                    pd:=destructor_head;
                    parse_object_proc_directives(pd);
                    handle_calling_convention(pd);

                    { add definition to procsym }
                    proc_add_definition(pd);

                    { add procdef options to objectdef options }
                    if (po_virtualmethod in pd.procoptions) then
                      include(aktobjectdef.objectoptions,oo_has_virtual);

                    chkcpp(pd);

                    { Support hint directives }
                    dummysymoptions:=[];
                    while try_consume_hintdirective(dummysymoptions) do
                      Consume(_SEMICOLON);
                    if assigned(pd) then
                      pd.symoptions:=pd.symoptions+dummysymoptions;

                    parse_only:=oldparse_only;
                  end;
                _END :
                  begin
                    consume(_END);
                    break;
                  end;
                else
                  consume(_ID); { Give a ident expected message, like tp7 }
              end;
            until false;
          end;

         { generate vmt space if needed }
         if not(oo_has_vmt in aktobjectdef.objectoptions) and
            (([oo_has_virtual,oo_has_constructor,oo_has_destructor]*aktobjectdef.objectoptions<>[]) or
             (classtype in [odt_class])
            ) then
           aktobjectdef.insertvmt;

         if is_interface(aktobjectdef) then
           setinterfacemethodoptions;

         { remove symtable from stack }
         symtablestack.pop(aktobjectdef.symtable);

         { return defined objectdef }
         result:=aktobjectdef;

         { restore old state }
         aktobjectdef:=oldaktobjectdef;
         testcurobject:=0;
         typecanbeforward:=storetypecanbeforward;
         parse_generic:=old_parse_generic;
         current_object_option:=old_object_option;
      end;

end.
