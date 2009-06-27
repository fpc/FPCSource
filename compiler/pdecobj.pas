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
      globtype,symconst,symtype,symdef;

    { parses a object declaration }
    function object_dec(objecttype:tobjecttyp;const n:tidstring;genericdef:tstoreddef;genericlist:TFPObjectList;fd : tobjectdef) : tobjectdef;

implementation

    uses
      cutils,
      globals,verbose,systems,tokens,
      symbase,symsym,symtable,
      node,nld,nmem,ncon,ncnv,ncal,
      fmodule,scanner,
      pbase,pexpr,pdecsub,pdecvar,ptype,pdecl
      ;

    const
      { Please leave this here, this module should NOT use
        these variables.
        Declaring it as string here results in an error when compiling (PFV) }
      current_procinfo = 'error';


    function constructor_head:tprocdef;
      var
        pd : tprocdef;
      begin
        result:=nil;
        consume(_CONSTRUCTOR);
        { must be at same level as in implementation }
        parse_proc_head(current_objectdef,potype_constructor,pd);
        if not assigned(pd) then
          begin
            consume(_SEMICOLON);
            exit;
          end;
        if (cs_constructor_name in current_settings.globalswitches) and
           (pd.procsym.name<>'INIT') then
          Message(parser_e_constructorname_must_be_init);
        consume(_SEMICOLON);
        include(current_objectdef.objectoptions,oo_has_constructor);
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
        result:=pd;
      end;


    procedure property_dec;
      var
        p : tpropertysym;
      begin
        { check for a class }
        if not((is_class_or_interface_or_dispinterface(current_objectdef)) or
           (not(m_tp7 in current_settings.modeswitches) and (is_object(current_objectdef)))) then
          Message(parser_e_syntax_error);
        consume(_PROPERTY);
        p:=read_property_dec(current_objectdef);
        consume(_SEMICOLON);
        if try_to_consume(_DEFAULT) then
          begin
            if oo_has_default_property in current_objectdef.objectoptions then
              message(parser_e_only_one_default_property);
            include(current_objectdef.objectoptions,oo_has_default_property);
            include(p.propoptions,ppo_defaultproperty);
            if not(ppo_hasparameters in p.propoptions) then
              message(parser_e_property_need_paras);
            if (token=_COLON) then
              begin
                Message(parser_e_field_not_allowed_here);
                consume_all_until(_SEMICOLON);
              end;
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
        result:=nil;
        consume(_DESTRUCTOR);
        parse_proc_head(current_objectdef,potype_destructor,pd);
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
        include(current_objectdef.objectoptions,oo_has_destructor);
        { no return value }
        pd.returndef:=voidtype;
        result:=pd;
      end;


    procedure setinterfacemethodoptions;
      var
        i   : longint;
        def : tdef;
      begin
        include(current_objectdef.objectoptions,oo_has_virtual);
        for i:=0 to current_objectdef.symtable.DefList.count-1 do
          begin
            def:=tdef(current_objectdef.symtable.DefList[i]);
            if assigned(def) and
               (def.typ=procdef) then
              begin
                include(tprocdef(def).procoptions,po_virtualmethod);
                tprocdef(def).forwarddef:=false;
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
        if current_objectdef.find_implemented_interface(intfdef)<>nil then
          Message1(sym_e_duplicate_id,intfdef.objname^)
        else
          begin
            { allocate and prepare the GUID only if the class
              implements some interfaces. }
            if current_objectdef.ImplementedInterfaces.count = 0 then
              current_objectdef.prepareguid;
            current_objectdef.ImplementedInterfaces.Add(TImplementedInterface.Create(intfdef));
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
            stringdispose(current_objectdef.iidstr);
            current_objectdef.iidstr:=stringdup(strpas(tstringconstnode(p).value_str));
            valid:=string2guid(current_objectdef.iidstr^,current_objectdef.iidguid^);
            if (current_objectdef.objecttype in [odt_interfacecom,odt_dispinterface]) and
               not valid then
              Message(parser_e_improper_guid_syntax);
            include(current_objectdef.objectoptions,oo_has_valid_guid);
          end
        else
          Message(parser_e_illegal_expression);
        p.free;
      end;


    procedure parse_parent_classes;
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
                case current_objectdef.objecttype of
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
                       current_objectdef.objecttype:=childof.objecttype;
                       current_objectdef.objecttype:=current_objectdef.objecttype;
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
            case current_objectdef.objecttype of
              odt_class:
                if current_objectdef<>class_tobject then
                  childof:=class_tobject;
              odt_interfacecom:
                if current_objectdef<>interface_iunknown then
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
              current_objectdef.set_parent(childof)
            else
              Message1(parser_e_forward_declaration_must_be_resolved,childof.objrealname^);
          end;

        { remove forward flag, is resolved }
        exclude(current_objectdef.objectoptions,oo_is_forward);

        if hasparentdefined then
          begin
            if current_objectdef.objecttype=odt_class then
              begin
                if assigned(intfchildof) then
                  handleImplementedInterface(intfchildof);
                readImplementedInterfaces;
              end;
            consume(_RKLAMMER);
          end;
      end;


    procedure parse_guid;
      begin
        { read GUID }
        if (current_objectdef.objecttype in [odt_interfacecom,odt_interfacecorba,odt_dispinterface]) and
           try_to_consume(_LECKKLAMMER) then
          begin
            readinterfaceiid;
            consume(_RECKKLAMMER);
          end
        else if (current_objectdef.objecttype=odt_dispinterface) then
          message(parser_e_dispinterface_needs_a_guid);
      end;


    procedure insert_generic_parameter_types(genericdef:tstoreddef;genericlist:TFPObjectList);
      var
        i : longint;
        generictype : ttypesym;
      begin
        current_objectdef.genericdef:=genericdef;
        if not assigned(genericlist) then
          exit;
        for i:=0 to genericlist.count-1 do
          begin
            generictype:=ttypesym(genericlist[i]);
            if generictype.typedef.typ=undefineddef then
              include(current_objectdef.defoptions,df_generic)
            else
              include(current_objectdef.defoptions,df_specialization);
            symtablestack.top.insert(generictype);
          end;
       end;


    procedure parse_object_members;

        procedure chkcpp(pd:tprocdef);
        begin
           if is_cppclass(pd._class) then
            begin
              pd.proccalloption:=pocall_cppdecl;
              pd.setmangledname(target_info.Cprefix+pd.cplusplusmangledname);
            end;
        end;

        procedure maybe_parse_hint_directives(pd:tprocdef);
        var
          dummysymoptions : tsymoptions;
        begin
          dummysymoptions:=[];
          while try_consume_hintdirective(dummysymoptions) do
            Consume(_SEMICOLON);
          if assigned(pd) then
            pd.symoptions:=pd.symoptions+dummysymoptions;
        end;

      var
        pd : tprocdef;
        has_destructor,
        oldparse_only,
        old_parse_generic : boolean;
        object_member_blocktype : tblock_type;
        fields_allowed: boolean;
      begin
        { empty class declaration ? }
        if (current_objectdef.objecttype=odt_class) and
           (token=_SEMICOLON) then
          exit;

        old_parse_generic:=parse_generic;

        parse_generic:=(df_generic in current_objectdef.defoptions);
        { in "publishable" classes the default access type is published }
        if (oo_can_have_published in current_objectdef.objectoptions) then
          current_objectdef.symtable.currentvisibility:=vis_published
        else
          current_objectdef.symtable.currentvisibility:=vis_public;
        testcurobject:=1;
        has_destructor:=false;
        fields_allowed:=true;
        object_member_blocktype:=bt_general;
        repeat
          case token of
            _TYPE :
              begin
                if ([df_generic,df_specialization]*current_objectdef.defoptions)=[] then
                  Message(parser_e_type_and_var_only_in_generics);
                 consume(_TYPE);
                 object_member_blocktype:=bt_type;
              end;
            _VAR :
              begin
                if ([df_generic,df_specialization]*current_objectdef.defoptions)=[] then
                  Message(parser_e_type_and_var_only_in_generics);
                consume(_VAR);
                object_member_blocktype:=bt_general;
              end;
            _ID :
              begin
                case idtoken of
                  _PRIVATE :
                    begin
                      if is_interface(current_objectdef) then
                         Message(parser_e_no_access_specifier_in_interfaces);
                       consume(_PRIVATE);
                       current_objectdef.symtable.currentvisibility:=vis_private;
                       include(current_objectdef.objectoptions,oo_has_private);
                       fields_allowed:=true;
                     end;
                   _PROTECTED :
                     begin
                       if is_interface(current_objectdef) then
                         Message(parser_e_no_access_specifier_in_interfaces);
                       consume(_PROTECTED);
                       current_objectdef.symtable.currentvisibility:=vis_protected;
                       include(current_objectdef.objectoptions,oo_has_protected);
                       fields_allowed:=true;
                     end;
                   _PUBLIC :
                     begin
                       if is_interface(current_objectdef) then
                         Message(parser_e_no_access_specifier_in_interfaces);
                       consume(_PUBLIC);
                       current_objectdef.symtable.currentvisibility:=vis_public;
                       fields_allowed:=true;
                     end;
                   _PUBLISHED :
                     begin
                       { we've to check for a pushlished section in non-  }
                       { publishable classes later, if a real declaration }
                       { this is the way, delphi does it                  }
                       if is_interface(current_objectdef) then
                         Message(parser_e_no_access_specifier_in_interfaces);
                       consume(_PUBLISHED);
                       current_objectdef.symtable.currentvisibility:=vis_published;
                       fields_allowed:=true;
                     end;
                   _STRICT :
                     begin
                       if is_interface(current_objectdef) then
                          Message(parser_e_no_access_specifier_in_interfaces);
                        consume(_STRICT);
                        if token=_ID then
                          begin
                            case idtoken of
                              _PRIVATE:
                                begin
                                  consume(_PRIVATE);
                                  current_objectdef.symtable.currentvisibility:=vis_strictprivate;
                                  include(current_objectdef.objectoptions,oo_has_strictprivate);
                                end;
                              _PROTECTED:
                                begin
                                  consume(_PROTECTED);
                                  current_objectdef.symtable.currentvisibility:=vis_strictprotected;
                                  include(current_objectdef.objectoptions,oo_has_strictprotected);
                                end;
                              else
                                message(parser_e_protected_or_private_expected);
                            end;
                          end
                        else
                          message(parser_e_protected_or_private_expected);
                        fields_allowed:=true;
                      end;
                    else
                      begin
                        if object_member_blocktype=bt_general then
                          begin
                            if is_interface(current_objectdef) then
                              Message(parser_e_no_vars_in_interfaces);

                            if (current_objectdef.symtable.currentvisibility=vis_published) and
                               not(oo_can_have_published in current_objectdef.objectoptions) then
                              Message(parser_e_cant_have_published);
                            if (not fields_allowed) then
                              Message(parser_e_field_not_allowed_here);

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
                fields_allowed:=false;
              end;
            _PROCEDURE,
            _FUNCTION,
            _CLASS :
              begin
                if (current_objectdef.symtable.currentvisibility=vis_published) and
                   not(oo_can_have_published in current_objectdef.objectoptions) then
                  Message(parser_e_cant_have_published);

                oldparse_only:=parse_only;
                parse_only:=true;
                pd:=parse_proc_dec(current_objectdef);

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
                      include(current_objectdef.objectoptions,oo_has_msgint);
                    if (po_msgstr in pd.procoptions) then
                      include(current_objectdef.objectoptions,oo_has_msgstr);
                    if (po_virtualmethod in pd.procoptions) then
                      include(current_objectdef.objectoptions,oo_has_virtual);

                    chkcpp(pd);
                  end;

                maybe_parse_hint_directives(pd);

                parse_only:=oldparse_only;
                fields_allowed:=false;
              end;
            _CONSTRUCTOR :
              begin
                if (current_objectdef.symtable.currentvisibility=vis_published) and
                  not(oo_can_have_published in current_objectdef.objectoptions) then
                  Message(parser_e_cant_have_published);

                if not(current_objectdef.symtable.currentvisibility in [vis_public,vis_published]) then
                  Message(parser_w_constructor_should_be_public);

                if is_interface(current_objectdef) then
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
                  include(current_objectdef.objectoptions,oo_has_virtual);
                chkcpp(pd);
                maybe_parse_hint_directives(pd);

                parse_only:=oldparse_only;
                fields_allowed:=false;
              end;
            _DESTRUCTOR :
              begin
                if (current_objectdef.symtable.currentvisibility=vis_published) and
                   not(oo_can_have_published in current_objectdef.objectoptions) then
                  Message(parser_e_cant_have_published);

                if has_destructor then
                  Message(parser_n_only_one_destructor);
                has_destructor:=true;

                if is_interface(current_objectdef) then
                  Message(parser_e_no_con_des_in_interfaces);

                if (current_objectdef.symtable.currentvisibility<>vis_public) then
                  Message(parser_w_destructor_should_be_public);

                oldparse_only:=parse_only;
                parse_only:=true;
                pd:=destructor_head;
                parse_object_proc_directives(pd);
                handle_calling_convention(pd);

                { add definition to procsym }
                proc_add_definition(pd);

                { add procdef options to objectdef options }
                if (po_virtualmethod in pd.procoptions) then
                  include(current_objectdef.objectoptions,oo_has_virtual);

                chkcpp(pd);
                maybe_parse_hint_directives(pd);

                parse_only:=oldparse_only;
                fields_allowed:=false;
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

        { restore }
        testcurobject:=0;
        parse_generic:=old_parse_generic;
      end;


    function object_dec(objecttype:tobjecttyp;const n:tidstring;genericdef:tstoreddef;genericlist:TFPObjectList;fd : tobjectdef) : tobjectdef;
      var
        old_current_objectdef : tobjectdef;
      begin
        old_current_objectdef:=current_objectdef;

        current_objectdef:=nil;

        { objects and class types can't be declared local }
        if not(symtablestack.top.symtabletype in [globalsymtable,staticsymtable]) and
           not assigned(genericlist) then
          Message(parser_e_no_local_objects);

        { reuse forward objectdef? }
        if assigned(fd) then
          begin
            if fd.objecttype<>objecttype then
              begin
                Message(parser_e_forward_mismatch);
                { recover }
                current_objectdef:=tobjectdef.create(current_objectdef.objecttype,n,nil);
                include(current_objectdef.objectoptions,oo_is_forward);
              end
            else
              current_objectdef:=fd
          end
        else
          begin
            { anonym objects aren't allow (o : object a : longint; end;) }
            if n='' then
              Message(parser_f_no_anonym_objects);

            { create new class }
            current_objectdef:=tobjectdef.create(objecttype,n,nil);

            { include always the forward flag, it'll be removed after the parent class have been
              added. This is to prevent circular childof loops }
            include(current_objectdef.objectoptions,oo_is_forward);

            if (cs_compilesystem in current_settings.moduleswitches) then
              begin
                case current_objectdef.objecttype of
                  odt_interfacecom :
                    if (current_objectdef.objname^='IUNKNOWN') then
                      interface_iunknown:=current_objectdef;
                  odt_class :
                    if (current_objectdef.objname^='TOBJECT') then
                      class_tobject:=current_objectdef;
                end;
              end;
          end;

        { set published flag in $M+ mode, it can also be inherited and will
          be added when the parent class set with tobjectdef.set_parent (PFV) }
        if (cs_generate_rtti in current_settings.localswitches) and
           (current_objectdef.objecttype in [odt_interfacecom,odt_class]) then
          include(current_objectdef.objectoptions,oo_can_have_published);

        { forward def? }
        if not assigned(fd) and
           (token=_SEMICOLON) then
          begin
            { add to the list of definitions to check that the forward
              is resolved. this is required for delphi mode }
            current_module.checkforwarddefs.add(current_objectdef);
          end
        else
          begin
            { parse list of parent classes }
            parse_parent_classes;

            { parse optional GUID for interfaces }
            parse_guid;

            { parse and insert object members }
            symtablestack.push(current_objectdef.symtable);
            insert_generic_parameter_types(genericdef,genericlist);
            parse_object_members;
            symtablestack.pop(current_objectdef.symtable);
          end;

        { generate vmt space if needed }
        if not(oo_has_vmt in current_objectdef.objectoptions) and
           (
            ([oo_has_virtual,oo_has_constructor,oo_has_destructor]*current_objectdef.objectoptions<>[]) or
            (current_objectdef.objecttype in [odt_class])
           ) then
          current_objectdef.insertvmt;

        { for implemented classes with a vmt check if there is a constructor }
        if (oo_has_vmt in current_objectdef.objectoptions) and
           not(oo_is_forward in current_objectdef.objectoptions) and
           not(oo_has_constructor in current_objectdef.objectoptions) then
          Message1(parser_w_virtual_without_constructor,current_objectdef.objrealname^);

        if is_interface(current_objectdef) then
          setinterfacemethodoptions;

        { return defined objectdef }
        result:=current_objectdef;

        { restore old state }
        current_objectdef:=old_current_objectdef;
      end;

end.
