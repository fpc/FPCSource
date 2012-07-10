{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Routines for the code generation of data structures
    like VMT, Messages, VTables, Interfaces descs

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
unit nobj;

{$i fpcdefs.inc}

interface

    uses
       cutils,cclasses,
       globtype,
       symdef,symsym,
       aasmbase,aasmtai,aasmdata
       ;

    type
      TVMTBuilder=class
      private
        _Class : tobjectdef;
        handledprotocols: tfpobjectlist;
        function  is_new_vmt_entry(pd:tprocdef; out overridesclasshelper: boolean):boolean;
        procedure add_new_vmt_entry(pd:tprocdef; allowoverridingmethod: boolean);
        function  check_msg_str(vmtpd, pd: tprocdef):boolean;
        function  intf_search_procdef_by_name(proc: tprocdef;const name: string): tprocdef;
        procedure intf_get_procdefs(ImplIntf:TImplementedInterface;IntfDef:TObjectDef);
        procedure intf_get_procdefs_recursive(ImplIntf:TImplementedInterface;IntfDef:TObjectDef);
        procedure prot_get_procdefs_recursive(ImplProt:TImplementedInterface;ProtDef:TObjectDef);
        procedure intf_optimize_vtbls;
        procedure intf_allocate_vtbls;
      public
        constructor create(c:tobjectdef);
        procedure  generate_vmt;
        procedure  build_interface_mappings;
      end;

    type
      pprocdeftree = ^tprocdeftree;
      tprocdeftree = record
         data : tprocdef;
         nl   : tasmlabel;
         l,r  : pprocdeftree;
      end;

      TVMTWriter=class
      private
        _Class : tobjectdef;
        { message tables }
        root : pprocdeftree;
        procedure disposeprocdeftree(p : pprocdeftree);
        procedure insertmsgint(p:TObject;arg:pointer);
        procedure insertmsgstr(p:TObject;arg:pointer);
        procedure insertint(p : pprocdeftree;var at : pprocdeftree;var count:longint);
        procedure insertstr(p : pprocdeftree;var at : pprocdeftree;var count:longint);
        procedure writenames(list : TAsmList;p : pprocdeftree);
        procedure writeintentry(list : TAsmList;p : pprocdeftree);
        procedure writestrentry(list : TAsmList;p : pprocdeftree);
{$ifdef WITHDMT}
        { dmt }
        procedure insertdmtentry(p:TObject;arg:pointer);
        procedure writedmtindexentry(p : pprocdeftree);
        procedure writedmtaddressentry(p : pprocdeftree);
{$endif}
        { published methods }
        procedure do_count_published_methods(p:TObject;arg:pointer);
        procedure do_gen_published_methods(p:TObject;arg:pointer);
        { virtual methods }
        procedure writevirtualmethods(List:TAsmList);
        { interface tables }
        function  intf_get_vtbl_name(AImplIntf:TImplementedInterface): string;
        procedure intf_create_vtbl(rawdata: TAsmList;AImplIntf:TImplementedInterface);
        procedure intf_gen_intf_ref(rawdata: TAsmList;AImplIntf:TImplementedInterface);
        function  intf_write_table(list : TAsmList):TAsmLabel;
        { generates the message tables for a class }
        function  genstrmsgtab(list : TAsmList) : tasmlabel;
        function  genintmsgtab(list : TAsmList) : tasmlabel;
        function  genpublishedmethodstable(list : TAsmList) : tasmlabel;
        function  generate_field_table(list : TAsmList) : tasmlabel;
{$ifdef WITHDMT}
        { generates a DMT for _class }
        function  gendmt : tasmlabel;
{$endif WITHDMT}
      public
        constructor create(c:tobjectdef);
        { write the VMT to al_globals }
        procedure writevmt;
        procedure writeinterfaceids;
      end;

implementation

    uses
       SysUtils,
       globals,verbose,systems,
       node,
       symbase,symtable,symconst,symtype,defcmp,
       dbgbase,
       ncgrtti,
       wpobase
       ;


{*****************************************************************************
                              TVMTBuilder
*****************************************************************************}

    constructor TVMTBuilder.create(c:tobjectdef);
      begin
        inherited Create;
        _Class:=c;
      end;


    procedure TVMTBuilder.add_new_vmt_entry(pd:tprocdef; allowoverridingmethod: boolean);
      var
        i : longint;
        vmtentry : pvmtentry;
        vmtpd : tprocdef;
      begin
        { new entry is needed, override was not possible }
        { Allowed when overriding a category method for a parent class in a
          descendent Objective-C class }
        if not allowoverridingmethod and
           (po_overridingmethod in pd.procoptions) then
          MessagePos1(pd.fileinfo,parser_e_nothing_to_be_overridden,pd.fullprocname(false));

        { check that all methods have overload directive }
        if not(m_fpc in current_settings.modeswitches) then
          begin
            for i:=0 to _class.vmtentries.count-1 do
              begin
                vmtentry:=pvmtentry(_class.vmtentries[i]);
                vmtpd:=tprocdef(vmtentry^.procdef);
                if (vmtpd.procsym=pd.procsym) and
                   (not(po_overload in pd.procoptions) or
                    not(po_overload in vmtpd.procoptions)) then
                  begin
                    MessagePos1(pd.fileinfo,parser_e_no_overload_for_all_procs,pd.procsym.realname);
                    { recover }
                    include(vmtpd.procoptions,po_overload);
                    include(pd.procoptions,po_overload);
                  end;
              end;
          end;

        { Register virtual method and give it a number }
        if (po_virtualmethod in pd.procoptions) then
          begin
             { store vmt entry number in procdef }
             if (pd.extnumber<>$ffff) and
                (pd.extnumber<>_class.VMTEntries.Count) then
               internalerror(200810283);
             pd.extnumber:=_class.VMTEntries.Count;
             new(vmtentry);
             vmtentry^.procdef:=pd;
             vmtentry^.procdefderef.reset;
             vmtentry^.visibility:=pd.visibility;
             _class.VMTEntries.Add(vmtentry);
          end;
      end;


      function TVMTBuilder.check_msg_str(vmtpd, pd: tprocdef): boolean;
        begin
          result:=true;
          if not(is_objc_class_or_protocol(_class)) then
            begin
              { the only requirement for normal methods is that both either
                have a message string or not (the value is irrelevant) }
              if ((pd.procoptions * [po_msgstr]) <> (vmtpd.procoptions * [po_msgstr])) then
                begin
                  MessagePos1(pd.fileinfo,parser_e_header_dont_match_forward,pd.fullprocname(false));
                  tprocsym(vmtpd.procsym).write_parameter_lists(pd);
                  result:=false;
                end
            end
          else
            begin
              { the compiler should have ensured that the protocol or parent
                class method has a message name specified }
              if not(po_msgstr in vmtpd.procoptions) then
                internalerror(2009070601);
              if not(po_msgstr in pd.procoptions) then
                begin
                  { copy the protocol's/parent class' message name to the one in
                    the class if none has been specified there }
                  include(pd.procoptions,po_msgstr);
                  pd.messageinf.str:=stringdup(vmtpd.messageinf.str^);
                end
              else
                begin
                  { if both have a message name, make sure they are equal }
                  if (vmtpd.messageinf.str^<>pd.messageinf.str^) then
                    begin
                      MessagePos2(pd.fileinfo,parser_e_objc_message_name_changed,vmtpd.messageinf.str^,pd.messageinf.str^);
                      result:=false;
                    end;
                end;
            end;
        end;


    function TVMTBuilder.is_new_vmt_entry(pd:tprocdef; out overridesclasshelper: boolean):boolean;
      const
        po_comp = [po_classmethod,po_virtualmethod,po_staticmethod,po_interrupt,po_iocheck,po_msgint,
                   po_exports,po_varargs,po_explicitparaloc,po_nostackframe];
      var
        i : longint;
        hasequalpara,
        hasoverloads,
        pdoverload : boolean;
        srsym : tsym;
        st : tsymtable;

      // returns true if we can stop checking, false if we have to continue
      function found_entry(var vmtpd: tprocdef; var vmtentryvis: tvisibility; updatevalues: boolean): boolean;
{$ifdef jvm}
        var
          javanewtreeok: boolean;
{$endif jvm}
        begin
          result:=false;

          { ignore hidden entries (e.g. virtual overridden by a static) that are not visible anymore }
          if vmtentryvis=vis_hidden then
            exit;

          { ignore different names }
          if vmtpd.procsym.name<>pd.procsym.name then
            exit;

          { hide private methods that are not visible anymore. For this check we
            must override the visibility with the highest value in the override chain.
            This is required for case (see tw3292) with protected-private-protected where the
            same vmtentry is used (PFV) }
          if not is_visible_for_object(vmtpd.owner,vmtentryvis,_class) then
            exit;

          { inherit overload }
          if (po_overload in vmtpd.procoptions) then
            begin
              include(pd.procoptions,po_overload);
              pdoverload:=true;
            end;

          { compare parameter types only, no specifiers yet }
          hasequalpara:=(compare_paras(vmtpd.paras,pd.paras,cp_none,[cpo_ignoreuniv,cpo_ignorehidden])>=te_equal);

          { check that we are not trying to override a final method }
          { in Java, new virtual inheritance trees can never be started ->
            treat all methods as "overriding" in the context of this check
            (Java does check whether the mangled names are identical, so if they
             are not we can stil get away with it) }
          if (po_finalmethod in vmtpd.procoptions) and
             hasequalpara and
             ((po_overridingmethod in pd.procoptions) or
              (is_javaclass(_class) and
               (pd.mangledname=vmtpd.mangledname))) and
             (is_class(_class) or is_objectpascal_helper(_class) or is_javaclass(_class)) then
            MessagePos1(pd.fileinfo,parser_e_final_can_no_be_overridden,pd.fullprocname(false))
          else
          { old definition has virtual
            new definition has no virtual or override }
          if (po_virtualmethod in vmtpd.procoptions) and
             (
              not(po_virtualmethod in pd.procoptions) or
              (
               { new one does not have reintroduce in case of an objccategory }
               (is_objccategory(_class) and
                 not(po_reintroduce in pd.procoptions)) or
               { new one does not have override in case of objpas/objc/java class/intf/proto }
               ((is_class_or_interface_or_objc_or_java(_class) or is_objectpascal_helper(_class)) and
                not is_objccategory(_class) and
                not(po_overridingmethod in pd.procoptions)
               )
              )
             ) then
            begin
              if (
                  not(pdoverload or hasoverloads) or
                  hasequalpara
                 ) then
                begin
{$ifdef jvm}
                  { if the mangled names are different, the inheritance trees
                    are different too in Java; exception: when the parent method
                    is a virtual class method or virtual constructor, because
                    those are looked up dynamicall by name }
                  javanewtreeok:=
                    is_java_class_or_interface(_class) and
                    (pd.jvmmangledbasename(false)<>vmtpd.jvmmangledbasename(false)) and
                    ((vmtpd.proctypeoption<>potype_constructor) and
                     not(po_staticmethod in vmtpd.procoptions));
{$endif}
                  if not(po_reintroduce in pd.procoptions) and
                     not(po_java_nonvirtual in vmtpd.procoptions) then
                    if not(is_objc_class_or_protocol(_class))
{$ifdef jvm}
                       and (not is_java_class_or_interface(_class) or
                        javanewtreeok)
{$endif jvm}
                       then
                      MessagePos1(pd.fileinfo,parser_w_should_use_override,pd.fullprocname(false))
                    else
                      begin
                        { In Objective-C, you cannot create a new VMT entry to
                          start a new inheritance tree. We therefore give an
                          error when the class is implemented in Pascal, to
                          avoid confusion due to things working differently
                          with Object Pascal classes.

                          In case of external classes, we only give a hint,
                          because requiring override everywhere may make
                          automated header translation tools too complex.

                          The same goes for Java. }
                        if not(oo_is_external in _class.objectoptions) then
                          if not is_objccategory(_class) then
                            MessagePos1(pd.fileinfo,parser_e_must_use_override,FullTypeName(tdef(vmtpd.owner.defowner),nil))
                          else
                            MessagePos1(pd.fileinfo,parser_e_must_use_reintroduce_objc,FullTypeName(tdef(vmtpd.owner.defowner),nil))
                        { there may be a lot of these in auto-translated
                          headers, so only calculate the fulltypename if
                          the hint will be shown  }
                        else if CheckVerbosity(V_Hint) then
                          if not is_objccategory(_class) then
                            MessagePos1(pd.fileinfo,parser_h_should_use_override,FullTypeName(tdef(vmtpd.owner.defowner),nil))
                          else
                            MessagePos1(pd.fileinfo,parser_h_should_use_reintroduce_objc,FullTypeName(tdef(vmtpd.owner.defowner),nil));
                        { no new entry, but copy the message name if any from
                          the procdef in the parent class }
                        check_msg_str(vmtpd,pd);
                        if updatevalues then
                          begin
                            { in case of Java, copy the real name from the parent,
                              since overriding "Destroy" with "destroy" is not
                              going to work very well }
                            if is_java_class_or_interface(_class) and
                               (pd.procsym.realname<>vmtpd.procsym.realname) then
                              pd.procsym.realname:=vmtpd.procsym.realname;
                            { in case we are overriding an abstract method,
                              decrease the number of abstract methods in this class }
                            if (po_abstractmethod in vmtpd.procoptions) then
                              dec(tobjectdef(pd.owner.defowner).abstractcnt);
                            if (vmtpd.extnumber<>i) then
                              internalerror(2011083101);
                            pd.extnumber:=vmtpd.extnumber;
                            vmtpd:=pd;
                          end;
                        result:=true;
                        exit;
{$ifdef jvm}
                      end
                  else
                    if not javanewtreeok and
                       is_java_class_or_interface(_class) then
                      begin
                        { mangled names are the same -> can only override }
                        MessagePos1(pd.fileinfo,parser_e_must_use_override,FullTypeName(tdef(vmtpd.owner.defowner),nil))
{$endif jvm}
                      end;
                  { disable/hide old VMT entry }
                  if updatevalues then
                    vmtentryvis:=vis_hidden;
                end;
            end
          { both are virtual? }
          else if (po_virtualmethod in pd.procoptions) and
                  (po_virtualmethod in vmtpd.procoptions) then
            begin
              { same parameter and return types (parameter specifiers will be checked below) }
              if hasequalpara and
                 compatible_childmethod_resultdef(vmtpd.returndef,pd.returndef) then
                begin
                  { inherite calling convention when it was explicit and the
                    current definition has none explicit set }
                  if (po_hascallingconvention in vmtpd.procoptions) and
                     not(po_hascallingconvention in pd.procoptions) then
                    begin
                      pd.proccalloption:=vmtpd.proccalloption;
                      include(pd.procoptions,po_hascallingconvention);
                    end;

                  { All parameter specifiers and some procedure the flags have to match
                    except abstract and override }
                  if (compare_paras(vmtpd.paras,pd.paras,cp_all,[cpo_ignoreuniv,cpo_ignorehidden])<te_equal) or
                     (vmtpd.proccalloption<>pd.proccalloption) or
                     (vmtpd.proctypeoption<>pd.proctypeoption) or
                     ((vmtpd.procoptions*po_comp)<>(pd.procoptions*po_comp)) then
                     begin
                       MessagePos1(pd.fileinfo,parser_e_header_dont_match_forward,pd.fullprocname(false));
                       tprocsym(vmtpd.procsym).write_parameter_lists(pd);
                     end;

                  check_msg_str(vmtpd,pd);

                  { Give a note if the new visibility is lower. For a higher
                    visibility update the vmt info }
                  if vmtentryvis>pd.visibility then
{$ifdef jvm}
                    MessagePos4(pd.fileinfo,parser_e_method_lower_visibility,
{$else jvm}
                    MessagePos4(pd.fileinfo,parser_n_ignore_lower_visibility,
{$endif jvm}
                         pd.fullprocname(false),
                         visibilityname[pd.visibility],tobjectdef(vmtpd.owner.defowner).objrealname^,visibilityname[vmtentryvis])
                  else if pd.visibility>vmtentryvis then
                    begin
                      if updatevalues then
                        vmtentryvis:=pd.visibility;
                    end;

                  { override old virtual method in VMT }
                  if updatevalues then
                    begin
                      { in case we are overriding an abstract method,
                        decrease the number of abstract methods in this class }
                      if (po_overridingmethod in pd.procoptions) and
                         (po_abstractmethod in vmtpd.procoptions) then
                        dec(tobjectdef(pd.owner.defowner).abstractcnt);

                      if (vmtpd.extnumber<>i) then
                        internalerror(200611084);
                      pd.extnumber:=vmtpd.extnumber;
                      { in case of Java, copy the real name from the parent,
                        since overriding "Destroy" with "destroy" is not
                        going to work very well }
                      if is_java_class_or_interface(_class) and
                         (pd.procsym.realname<>vmtpd.procsym.realname) then
                        pd.procsym.realname:=vmtpd.procsym.realname;
                      vmtpd:=pd;
                    end;
                  result:=true;
                  exit;
                end
              { different parameters }
              else
               begin
                 { when we got an override directive then can search futher for
                   the procedure to override.
                   If we are starting a new virtual tree then hide the old tree }
                 if not(po_overridingmethod in pd.procoptions) and
                    not(pdoverload or hasoverloads) then
                   begin
                     if not(po_reintroduce in pd.procoptions) then
                       begin
                         if not is_object(_class) and
                            not is_objc_class_or_protocol(_class) and
                            not is_java_class_or_interface(_class) then
                           MessagePos1(pd.fileinfo,parser_w_should_use_override,pd.fullprocname(false))
                         else
                           { objects don't allow starting a new virtual tree
                             and neither do Objective-C or Java }
                           MessagePos1(pd.fileinfo,parser_e_header_dont_match_forward,vmtpd.fullprocname(false));
                       end;
                     { disable/hide old VMT entry }
                     if updatevalues then
                       vmtentryvis:=vis_hidden;
                   end;
               end;
            end;
        end;

        function found_category_method(st: tsymtable): boolean;
          var
            entrycount: longint;
            cat: tobjectdef;
            vmtpd: tprocdef;
            vmtvis: tvisibility;
          begin
            result:=false;
            if is_objccategory(tdef(st.defowner)) then
              begin
                cat:=tobjectdef(st.defowner);
                { go through all of the category's methods to find the
                  vmtentry corresponding to the procdef we are handling }
                for entrycount:=0 to cat.vmtentries.Count-1 do
                  begin
                    vmtpd:=pvmtentry(cat.vmtentries[entrycount])^.procdef;
                    vmtvis:=pvmtentry(cat.vmtentries[entrycount])^.visibility;
                    { don't change the vmtentry of the category }
                    if found_entry(vmtpd,vmtvis,false) then
                      begin
                        result:=true;
                        exit;
                      end;
                  end;
              end;
          end;

      begin
        result:=false;
        overridesclasshelper:=false;
        { Load other values for easier readability }
        hasoverloads:=(tprocsym(pd.procsym).ProcdefList.Count>1);
        pdoverload:=(po_overload in pd.procoptions);

        { compare with all stored definitions }
        for i:=0 to _class.vmtentries.Count-1 do
          begin
            if found_entry(pvmtentry(_class.vmtentries[i])^.procdef, pvmtentry(_class.vmtentries[i])^.visibility,true) then
              exit;
          end;

        { in case of Objective-C, also check the categories that apply to this
          class' *parent* for methods to override (don't allow class X to
          "override" a method added by a category to class X itself, since in
          that case the category method will in fact replace class X'
          "overriding" method }
        if is_objcclass(_class) and
           assigned(_class.childof) and
           search_objc_helper(_class.childof,pd.procsym.name,srsym,st) then
          begin
            overridesclasshelper:=found_category_method(st);
          end;

        { No entry found, we need to create a new entry }
        result:=true;
      end;


    function TVMTBuilder.intf_search_procdef_by_name(proc: tprocdef;const name: string): tprocdef;
      const
        po_comp = [po_classmethod,po_staticmethod,po_interrupt,po_iocheck,po_msgint,
                   po_exports,po_varargs,po_explicitparaloc,po_nostackframe];
      var
        implprocdef : Tprocdef;
        i: cardinal;
        hclass : tobjectdef;
        hashedid : THashedIDString;
        srsym      : tsym;
      begin
        result:=nil;
        hashedid.id:=name;
        hclass:=_class;
        while assigned(hclass) do
          begin
            srsym:=tsym(hclass.symtable.FindWithHash(hashedid));
            if assigned(srsym) and
               (srsym.typ=procsym) then
              begin
                for i:=0 to Tprocsym(srsym).ProcdefList.Count-1 do
                  begin
                    implprocdef:=tprocdef(tprocsym(srsym).ProcdefList[i]);
                    if (implprocdef.procsym=tprocsym(srsym)) and
                       (compare_paras(proc.paras,implprocdef.paras,cp_all,[cpo_ignorehidden,cpo_ignoreuniv])>=te_equal) and
                       (compare_defs(proc.returndef,implprocdef.returndef,nothingn)>=te_equal) and
                       (proc.proccalloption=implprocdef.proccalloption) and
                       (proc.proctypeoption=implprocdef.proctypeoption) and
                       ((proc.procoptions*po_comp)=((implprocdef.procoptions+[po_virtualmethod])*po_comp)) and
                       check_msg_str(proc,implprocdef) then
                      begin
                        { does the interface increase the visibility of the
                          implementing method? }
                        if implprocdef.visibility<proc.visibility then
{$ifdef jvm}
                          MessagePos2(implprocdef.fileinfo,type_e_interface_lower_visibility,proc.fullprocname(false),implprocdef.fullprocname(false));
{$else}
                          MessagePos2(implprocdef.fileinfo,type_w_interface_lower_visibility,proc.fullprocname(false),implprocdef.fullprocname(false));
{$endif}
                        result:=implprocdef;
                        exit;
                      end;
                  end;
              end;
            hclass:=hclass.childof;
          end;
      end;


    procedure TVMTBuilder.intf_get_procdefs(ImplIntf:TImplementedInterface;IntfDef:TObjectDef);
      var
        i   : longint;
        def : tdef;
        hs,
        prefix,
        mappedname: string;
        implprocdef: tprocdef;
      begin
        prefix:=ImplIntf.IntfDef.symtable.name^+'.';
        for i:=0 to IntfDef.symtable.DefList.Count-1 do
          begin
            def:=tdef(IntfDef.symtable.DefList[i]);
            if assigned(def) and
               (def.typ=procdef) then
              begin
                { Find implementing procdef
                   1. Check for mapped name
                   2. Use symbol name, but only if there's no mapping,
                      or we're processing ancestor of interface.
                  When modifying this code, ensure that webtbs/tw11862, webtbs/tw4950
                  and webtbf/tw19591 stay correct. }
                implprocdef:=nil;
                hs:=prefix+tprocdef(def).procsym.name;
                mappedname:=ImplIntf.GetMapping(hs);
                if mappedname<>'' then
                  implprocdef:=intf_search_procdef_by_name(tprocdef(def),mappedname);
                if not assigned(implprocdef) then
                  if (mappedname='') or (ImplIntf.IntfDef<>IntfDef) then
                    implprocdef:=intf_search_procdef_by_name(tprocdef(def),tprocdef(def).procsym.name);

                { Add procdef to the implemented interface }
                if assigned(implprocdef) then
                  begin
                    if (tobjectdef(implprocdef.struct).objecttype<>odt_objcclass) then
                      begin
                        { in case of Java, copy the real name from the parent,
                          since overriding "Destroy" with "destroy" is not
                          going to work very well }
                        if is_javaclass(implprocdef.struct) and
                           (implprocdef.procsym.realname<>tprocdef(def).procsym.realname) then
                          implprocdef.procsym.realname:=tprocdef(def).procsym.realname;
                        ImplIntf.AddImplProc(implprocdef);
                      end
                    else
                      begin
                        { If no message name has been specified for the method
                          in the objcclass, copy it from the protocol
                          definition.  }
                        if not(po_msgstr in tprocdef(def).procoptions) then
                          begin
                            include(tprocdef(def).procoptions,po_msgstr);
                            implprocdef.messageinf.str:=stringdup(tprocdef(def).messageinf.str^);
                          end
                        else
                          begin
                            { If a message name has been specified in the
                              objcclass, it has to match the message name in the
                              protocol definition.  }
                            if (implprocdef.messageinf.str^<>tprocdef(def).messageinf.str^) then
                              MessagePos2(implprocdef.fileinfo,parser_e_objc_message_name_changed,tprocdef(def).messageinf.str^,implprocdef.messageinf.str^);
                          end;
                      end;
                  end
                else
                  if (ImplIntf.IType=etStandard) and
                     not(po_optional in tprocdef(def).procoptions) then
                    MessagePos1(_Class.typesym.fileinfo,sym_e_no_matching_implementation_found,tprocdef(def).fullprocname(false));
              end;
          end;
      end;


    procedure TVMTBuilder.intf_get_procdefs_recursive(ImplIntf:TImplementedInterface;IntfDef:TObjectDef);
      begin
        if assigned(IntfDef.childof) then
          intf_get_procdefs_recursive(ImplIntf,IntfDef.childof);
        intf_get_procdefs(ImplIntf,IntfDef);
      end;


    procedure TVMTBuilder.prot_get_procdefs_recursive(ImplProt:TImplementedInterface;ProtDef:TObjectDef);
      var
        i: longint;
      begin
        { don't check the same protocol twice }
        if handledprotocols.IndexOf(ProtDef)<>-1 then
          exit;
        handledprotocols.add(ProtDef);
        for i:=0 to ProtDef.ImplementedInterfaces.count-1 do
          prot_get_procdefs_recursive(ImplProt,TImplementedInterface(ProtDef.ImplementedInterfaces[i]).intfdef);
        intf_get_procdefs(ImplProt,ProtDef);
      end;


    procedure TVMTBuilder.intf_optimize_vtbls;
      type
        tcompintfentry = record
          weight: longint;
          compintf: longint;
        end;
        { Max 1000 interface in the class header interfaces it's enough imho }
        tcompintfs = array[0..1000] of tcompintfentry;
        pcompintfs = ^tcompintfs;
        tequals    = array[0..1000] of longint;
        pequals    = ^tequals;
        timpls    = array[0..1000] of longint;
        pimpls    = ^timpls;
      var
        aequals: pequals;
        compats: pcompintfs;
        impls: pimpls;
        ImplIntfCount,
        w,i,j,k: longint;
        ImplIntfI,
        ImplIntfJ  : TImplementedInterface;
        cij: boolean;
        cji: boolean;
      begin
        ImplIntfCount:=_class.ImplementedInterfaces.count;
        if ImplIntfCount>=High(tequals) then
          Internalerror(200006135);
        getmem(compats,sizeof(tcompintfentry)*ImplIntfCount);
        getmem(aequals,sizeof(longint)*ImplIntfCount);
        getmem(impls,sizeof(longint)*ImplIntfCount);
        filldword(compats^,(sizeof(tcompintfentry) div sizeof(dword))*ImplIntfCount,dword(-1));
        filldword(aequals^,ImplIntfCount,dword(-1));
        filldword(impls^,ImplIntfCount,dword(-1));
        { ismergepossible is a containing relation
          meaning of ismergepossible(a,b,w) =
          if implementorfunction map of a is contained implementorfunction map of b
          imp(a,b) and imp(b,c) => imp(a,c) ; imp(a,b) and imp(b,a) => a == b
        }
        { the order is very important for correct allocation }
        for i:=0 to ImplIntfCount-1 do
          begin
            for j:=i+1 to ImplIntfCount-1 do
              begin
                ImplIntfI:=TImplementedInterface(_class.ImplementedInterfaces[i]);
                ImplIntfJ:=TImplementedInterface(_class.ImplementedInterfaces[j]);
                cij:=ImplIntfI.IsImplMergePossible(ImplIntfJ,w);
                cji:=ImplIntfJ.IsImplMergePossible(ImplIntfI,w);
                if cij and cji then { i equal j }
                  begin
                    { get minimum index of equal }
                    if aequals^[j]=-1 then
                      aequals^[j]:=i;
                  end
                else if cij then
                  begin
                    { get minimum index of maximum weight  }
                    if compats^[i].weight<w then
                      begin
                        compats^[i].weight:=w;
                        compats^[i].compintf:=j;
                      end;
                  end
                else if cji then
                  begin
                    { get minimum index of maximum weight  }
                    if (compats^[j].weight<w) then
                      begin
                        compats^[j].weight:=w;
                        compats^[j].compintf:=i;
                      end;
                  end;
              end;
          end;
        { Reset, no replacements by default }
        for i:=0 to ImplIntfCount-1 do
          impls^[i]:=i;
        { Replace vtbls when equal or compat, repeat
          until there are no replacements possible anymore. This is
          needed for the cases like:
            First loop: 2->3, 3->1
            Second loop: 2->1 (because 3 was replaced with 1)
        }
        repeat
          k:=0;
          for i:=0 to ImplIntfCount-1 do
            begin
              if compats^[impls^[i]].compintf<>-1 then
                impls^[i]:=compats^[impls^[i]].compintf
              else if aequals^[impls^[i]]<>-1 then
                impls^[i]:=aequals^[impls^[i]]
              else
                inc(k);
            end;
        until k=ImplIntfCount;
        { Update the VtblImplIntf }
        for i:=0 to ImplIntfCount-1 do
          begin
            ImplIntfI:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            ImplIntfI.VtblImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[impls^[i]]);
          end;
        freemem(compats);
        freemem(aequals);
        freemem(impls);
      end;


    procedure TVMTBuilder.intf_allocate_vtbls;
      var
        i : longint;
        ImplIntf : TImplementedInterface;
      begin
        { Allocation vtbl space }
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            { if it implements itself and if it's not implemented by delegation }
            if (ImplIntf.VtblImplIntf=ImplIntf) and (ImplIntf.IType=etStandard) then
              begin
                { allocate a pointer in the object memory }
                with tObjectSymtable(_class.symtable) do
                  begin
                    datasize:=align(datasize,sizeof(pint));
                    ImplIntf.Ioffset:=datasize;
                    datasize:=datasize+sizeof(pint);
                  end;
              end;
          end;
        { Update ioffset of current interface with the ioffset from
          the interface that is reused to implements this interface }
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            if ImplIntf.VtblImplIntf<>ImplIntf then
              ImplIntf.IOffset:=ImplIntf.VtblImplIntf.IOffset;
          end;
      end;


    procedure TVMTBuilder.generate_vmt;
      var
        i : longint;
        def : tdef;
        old_current_structdef : tabstractrecorddef;
        overridesclasshelper : boolean;
      begin
        old_current_structdef:=current_structdef;
        current_structdef:=_class;

        _class.resetvmtentries;

        { inherit (copy) VMT from parent object }
        if assigned(_class.childof) then
          begin
            if not assigned(_class.childof.vmtentries) then
              internalerror(200810281);
            _class.copyvmtentries(_class.childof);
          end;

        { process all procdefs, we must process the defs to
          keep the same order as that is written in the source
          to be compatible with the indexes in the interface vtable (PFV) }
        for i:=0 to _class.symtable.DefList.Count-1 do
          begin
            def:=tdef(_class.symtable.DefList[i]);
            if def.typ=procdef then
              begin
                { VMT entry }
                if is_new_vmt_entry(tprocdef(def),overridesclasshelper) then
                  add_new_vmt_entry(tprocdef(def),overridesclasshelper);
              end;
          end;
        build_interface_mappings;
        if assigned(_class.ImplementedInterfaces) and
           not(is_objc_class_or_protocol(_class)) and
           not(is_java_class_or_interface(_class)) then
          begin
            { Optimize interface tables to reuse wrappers }
            intf_optimize_vtbls;
            { Allocate interface tables }
            intf_allocate_vtbls;
          end;

        current_structdef:=old_current_structdef;
      end;


    procedure TVMTBuilder.build_interface_mappings;
      var
        ImplIntf : TImplementedInterface;
        i: longint;
      begin
        { Find Procdefs implementing the interfaces (both Objective-C protocols
          and Java interfaces can have multiple parent interfaces, but in that
          case obviously no implementations are required) }
        if assigned(_class.ImplementedInterfaces) and
           not(_class.objecttype in [odt_objcprotocol,odt_interfacejava]) and
           // abstract java classes do not have to implement all interface
           // methods. todo: check that non-abstract descendents do!
           not((_class.objecttype=odt_javaclass) and (oo_is_abstract in _class.objectoptions)) then
          begin
            { Collect implementor functions into the tImplementedInterface.procdefs }
            case _class.objecttype of
              odt_class:
                begin
                  for i:=0 to _class.ImplementedInterfaces.count-1 do
                    begin
                      ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
                      intf_get_procdefs_recursive(ImplIntf,ImplIntf.IntfDef)
                    end;
                end;
              odt_objcclass,
              odt_javaclass:
                begin
                  { Object Pascal interfaces are afterwards optimized via the
                    intf_optimize_vtbls() method, but we can't do this for
                    protocols/Java interfaces -> check for duplicates here
                    already. }
                  handledprotocols:=tfpobjectlist.create(false);
                  for i:=0 to _class.ImplementedInterfaces.count-1 do
                    begin
                      ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
                      prot_get_procdefs_recursive(ImplIntf,ImplIntf.IntfDef);
                    end;
                  handledprotocols.free;
                end
              else
                internalerror(2009091801);
            end
          end;
      end;


{*****************************************************************************
                                TVMTWriter
*****************************************************************************}

    constructor TVMTWriter.create(c:tobjectdef);
      begin
        inherited Create;
        _Class:=c;
      end;


{**************************************
           Message Tables
**************************************}

    procedure TVMTWriter.disposeprocdeftree(p : pprocdeftree);
      begin
         if assigned(p^.l) then
           disposeprocdeftree(p^.l);
         if assigned(p^.r) then
           disposeprocdeftree(p^.r);
         dispose(p);
      end;


    procedure TVMTWriter.insertint(p : pprocdeftree;var at : pprocdeftree;var count:longint);
      begin
         if at=nil then
           begin
              at:=p;
              inc(count);
           end
         else
           begin
              if p^.data.messageinf.i<at^.data.messageinf.i then
                insertint(p,at^.l,count)
              else if p^.data.messageinf.i>at^.data.messageinf.i then
                insertint(p,at^.r,count)
              else
                Message1(parser_e_duplicate_message_label,tostr(p^.data.messageinf.i));
           end;
      end;


    procedure TVMTWriter.insertstr(p : pprocdeftree;var at : pprocdeftree;var count:longint);
      var
         i : integer;
      begin
         if at=nil then
           begin
              at:=p;
              inc(count);
           end
         else
           begin
              i:=CompareStr(p^.data.messageinf.str^,at^.data.messageinf.str^);
              if i<0 then
                insertstr(p,at^.l,count)
              else if i>0 then
                insertstr(p,at^.r,count)
              else
                Message1(parser_e_duplicate_message_label,p^.data.messageinf.str^);
           end;
      end;


    procedure TVMTWriter.insertmsgint(p:TObject;arg:pointer);
      var
        i  : longint;
        pd : Tprocdef;
        pt : pprocdeftree;
      begin
        if tsym(p).typ<>procsym then
          exit;
        for i:=0 to Tprocsym(p).ProcdefList.Count-1 do
          begin
            pd:=tprocdef(Tprocsym(p).ProcdefList[i]);
            if po_msgint in pd.procoptions then
              begin
                new(pt);
                pt^.data:=pd;
                pt^.l:=nil;
                pt^.r:=nil;
                insertint(pt,root,plongint(arg)^);
              end;
          end;
      end;


    procedure TVMTWriter.insertmsgstr(p:TObject;arg:pointer);
      var
        i  : longint;
        pd : Tprocdef;
        pt : pprocdeftree;
      begin
        if tsym(p).typ<>procsym then
          exit;
        for i:=0 to Tprocsym(p).ProcdefList.Count-1 do
          begin
            pd:=tprocdef(Tprocsym(p).ProcdefList[i]);
            if po_msgstr in pd.procoptions then
              begin
                new(pt);
                pt^.data:=pd;
                pt^.l:=nil;
                pt^.r:=nil;
                insertstr(pt,root,plongint(arg)^);
              end;
          end;
      end;


    procedure TVMTWriter.writenames(list : TAsmList;p : pprocdeftree);
      var
        ca : pchar;
        len : byte;
      begin
         current_asmdata.getdatalabel(p^.nl);
         if assigned(p^.l) then
           writenames(list,p^.l);
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_label.Create(p^.nl));
         len:=length(p^.data.messageinf.str^);
         list.concat(tai_const.create_8bit(len));
         getmem(ca,len+1);
         move(p^.data.messageinf.str^[1],ca^,len);
         ca[len]:=#0;
         list.concat(Tai_string.Create_pchar(ca,len));
         if assigned(p^.r) then
           writenames(list,p^.r);
      end;

    procedure TVMTWriter.writestrentry(list : TAsmList;p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writestrentry(list,p^.l);

         { write name label }
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_const.Create_sym(p^.nl));
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_const.Createname(p^.data.mangledname,0));

         if assigned(p^.r) then
           writestrentry(list,p^.r);
     end;


    function TVMTWriter.genstrmsgtab(list : TAsmList) : tasmlabel;
      var
         count : longint;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.SymList.ForEachCall(@insertmsgstr,@count);

         { write all names }
         if assigned(root) then
           writenames(list,root);

         { now start writing of the message string table }
         current_asmdata.getlabel(result,alt_data);
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_label.Create(result));
         list.concat(cai_align.create(const_align(sizeof(longint))));
         list.concat(Tai_const.Create_32bit(count));
         list.concat(cai_align.create(const_align(sizeof(pint))));
         if assigned(root) then
           begin
              writestrentry(list,root);
              disposeprocdeftree(root);
           end;
      end;


    procedure TVMTWriter.writeintentry(list : TAsmList;p : pprocdeftree);
      begin
         if assigned(p^.l) then
           writeintentry(list,p^.l);

         { write name label }
         list.concat(cai_align.create(const_align(sizeof(longint))));
         list.concat(Tai_const.Create_32bit(p^.data.messageinf.i));
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_const.Createname(p^.data.mangledname,0));

         if assigned(p^.r) then
           writeintentry(list,p^.r);
      end;


    function TVMTWriter.genintmsgtab(list : TAsmList) : tasmlabel;
      var
         r : tasmlabel;
         count : longint;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.SymList.ForEachCall(@insertmsgint,@count);

         { now start writing of the message string table }
         current_asmdata.getlabel(r,alt_data);
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_label.Create(r));
         genintmsgtab:=r;
         list.concat(cai_align.create(const_align(sizeof(longint))));
         list.concat(Tai_const.Create_32bit(count));
         list.concat(cai_align.create(const_align(sizeof(pint))));
         if assigned(root) then
           begin
              writeintentry(list,root);
              disposeprocdeftree(root);
           end;
      end;

{$ifdef WITHDMT}

{**************************************
              DMT
**************************************}

    procedure TVMTWriter.insertdmtentry(p:TObject;arg:pointer);

      var
         hp : tprocdef;
         pt : pprocdeftree;

      begin
         if tsym(p).typ=procsym then
           begin
              hp:=tprocsym(p).definition;
              while assigned(hp) do
                begin
                   if (po_msgint in hp.procoptions) then
                     begin
                        new(pt);
                        pt^.p:=hp;
                        pt^.l:=nil;
                        pt^.r:=nil;
                        insertint(pt,root);
                     end;
                   hp:=hp.nextoverloaded;
                end;
           end;
      end;

    procedure TVMTWriter.writedmtindexentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writedmtindexentry(p^.l);
         al_globals.concat(Tai_const.Create_32bit(p^.data.messageinf.i));
         if assigned(p^.r) then
           writedmtindexentry(p^.r);
      end;

    procedure TVMTWriter.writedmtaddressentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writedmtaddressentry(p^.l);
         al_globals.concat(Tai_const_symbol.Createname(p^.data.mangledname,0));
         if assigned(p^.r) then
           writedmtaddressentry(p^.r);
      end;

    function TVMTWriter.gendmt : tasmlabel;

      var
         r : tasmlabel;

      begin
         root:=nil;
         count:=0;
         gendmt:=nil;
         { insert all message handlers into a tree, sorted by number }
         _class.symtable.SymList.ForEachCall(insertdmtentry);

         if count>0 then
           begin
              current_asmdata.getdatalabel(r);
              gendmt:=r;
              al_globals.concat(cai_align.create(const_align(sizeof(pint))));
              al_globals.concat(Tai_label.Create(r));
              { entries for caching }
              al_globals.concat(Tai_const.Create_ptr(0));
              al_globals.concat(Tai_const.Create_ptr(0));

              al_globals.concat(Tai_const.Create_32bit(count));
              if assigned(root) then
                begin
                   writedmtindexentry(root);
                   writedmtaddressentry(root);
                   disposeprocdeftree(root);
                end;
           end;
      end;

{$endif WITHDMT}

{**************************************
        Published Methods
**************************************}

    procedure TVMTWriter.do_count_published_methods(p:TObject;arg:pointer);
      var
        i  : longint;
        pd : tprocdef;
      begin
        if (tsym(p).typ<>procsym) then
          exit;
        for i:=0 to Tprocsym(p).ProcdefList.Count-1 do
          begin
            pd:=tprocdef(Tprocsym(p).ProcdefList[i]);
            if (pd.procsym=tsym(p)) and
               (pd.visibility=vis_published) then
              inc(plongint(arg)^);
          end;
      end;


    procedure TVMTWriter.do_gen_published_methods(p:TObject;arg:pointer);
      var
        i  : longint;
        l  : tasmlabel;
        pd : tprocdef;
        lists: ^TAsmList absolute arg;
      begin
        if (tsym(p).typ<>procsym) then
          exit;
        for i:=0 to Tprocsym(p).ProcdefList.Count-1 do
          begin
            pd:=tprocdef(Tprocsym(p).ProcdefList[i]);
            if (pd.procsym=tsym(p)) and
               (pd.visibility=vis_published) then
              begin
                current_asmdata.getlabel(l,alt_data);
                lists[1].concat(cai_align.Create(const_align(sizeof(pint))));
                lists[1].concat(Tai_label.Create(l));
                lists[1].concat(Tai_const.Create_8bit(length(tsym(p).realname)));
                lists[1].concat(Tai_string.Create(tsym(p).realname));

                lists[0].concat(Tai_const.Create_sym(l));
                if po_abstractmethod in pd.procoptions then
                  lists[0].concat(Tai_const.Create_sym(nil))
                else
                  lists[0].concat(Tai_const.Createname(pd.mangledname,0));
              end;
           end;
      end;


    function TVMTWriter.genpublishedmethodstable(list : TAsmList) : tasmlabel;

      var
         l : tasmlabel;
         count : longint;
         lists : array[0..1] of TAsmList;
      begin
         count:=0;
         _class.symtable.SymList.ForEachCall(@do_count_published_methods,@count);
         if count>0 then
           begin
              lists[0]:=list;
              lists[1]:=TAsmList.Create;
              current_asmdata.getlabel(l,alt_data);
              list.concat(cai_align.create(const_align(sizeof(pint))));
              list.concat(Tai_label.Create(l));
              list.concat(Tai_const.Create_32bit(count));
              _class.symtable.SymList.ForEachCall(@do_gen_published_methods,@lists);
              list.concatlist(lists[1]);
              lists[1].Free;
              genpublishedmethodstable:=l;
           end
         else
           genpublishedmethodstable:=nil;
      end;


    function TVMTWriter.generate_field_table(list : TAsmList) : tasmlabel;
      var
        i   : longint;
        sym : tsym;
        fieldtable,
        classtable : tasmlabel;
        classindex,
        fieldcount : longint;
        classtablelist : TFPList;
      begin
        classtablelist:=TFPList.Create;
        { retrieve field info fields }
        fieldcount:=0;
        for i:=0 to _class.symtable.SymList.Count-1 do
          begin
            sym:=tsym(_class.symtable.SymList[i]);
            if (sym.typ=fieldvarsym) and
               (sym.visibility=vis_published) then
             begin
                if tfieldvarsym(sym).vardef.typ<>objectdef then
                  internalerror(200611032);
                classindex:=classtablelist.IndexOf(tfieldvarsym(sym).vardef);
                if classindex=-1 then
                  classtablelist.Add(tfieldvarsym(sym).vardef);
                inc(fieldcount);
             end;
          end;

        if fieldcount>0 then
          begin
            current_asmdata.getlabel(fieldtable,alt_data);
            current_asmdata.getlabel(classtable,alt_data);

            list.concat(cai_align.create(const_align(sizeof(pint))));
            { write fields }
            list.concat(Tai_label.Create(fieldtable));
            list.concat(Tai_const.Create_16bit(fieldcount));
            if (tf_requires_proper_alignment in target_info.flags) then
              list.concat(cai_align.Create(sizeof(TConstPtrUInt)));
            list.concat(Tai_const.Create_sym(classtable));
            for i:=0 to _class.symtable.SymList.Count-1 do
              begin
                sym:=tsym(_class.symtable.SymList[i]);
                if (sym.typ=fieldvarsym) and
                  (sym.visibility=vis_published) then
                  begin
                    if (tf_requires_proper_alignment in target_info.flags) then
                      list.concat(cai_align.Create(sizeof(pint)));
                    list.concat(Tai_const.Create_pint(tfieldvarsym(sym).fieldoffset));
                    classindex:=classtablelist.IndexOf(tfieldvarsym(sym).vardef);
                    if classindex=-1 then
                      internalerror(200611033);
                    list.concat(Tai_const.Create_16bit(classindex+1));
                    list.concat(Tai_const.Create_8bit(length(tfieldvarsym(sym).realname)));
                    list.concat(Tai_string.Create(tfieldvarsym(sym).realname));
                  end;
              end;

            { generate the class table }
            list.concat(cai_align.create(const_align(sizeof(pint))));
            list.concat(Tai_label.Create(classtable));
            list.concat(Tai_const.Create_16bit(classtablelist.count));
            if (tf_requires_proper_alignment in target_info.flags) then
              list.concat(cai_align.Create(sizeof(TConstPtrUInt)));
            for i:=0 to classtablelist.Count-1 do
              list.concat(Tai_const.Createname(tobjectdef(classtablelist[i]).vmt_mangledname,0));
            result:=fieldtable;
          end
        else
          result:=nil;

        classtablelist.free;
      end;


{**************************************
           Interface tables
**************************************}

    function  TVMTWriter.intf_get_vtbl_name(AImplIntf:TImplementedInterface): string;
      begin
        result:=make_mangledname('VTBL',_class.owner,_class.objname^+'_$_'+AImplIntf.IntfDef.objname^);
      end;


    procedure TVMTWriter.intf_create_vtbl(rawdata: TAsmList;AImplIntf:TImplementedInterface);
      var
        pd : tprocdef;
        vtblstr,
        hs : string;
        i  : longint;
      begin
        vtblstr:=intf_get_vtbl_name(AImplIntf);
        rawdata.concat(cai_align.create(const_align(sizeof(pint))));
        rawdata.concat(tai_symbol.createname(vtblstr,AT_DATA,0));
        if assigned(AImplIntf.procdefs) then
          begin
            for i:=0 to AImplIntf.procdefs.count-1 do
              begin
                pd:=tprocdef(AImplIntf.procdefs[i]);
                hs:=make_mangledname('WRPR',_class.owner,_class.objname^+'_$_'+AImplIntf.IntfDef.objname^+'_$_'+
                                     tostr(i)+'_$_'+pd.mangledname);
                { create reference }
                rawdata.concat(Tai_const.Createname(hs,0));
              end;
           end;
        rawdata.concat(tai_symbol_end.createname(vtblstr));
      end;


    procedure TVMTWriter.intf_gen_intf_ref(rawdata: TAsmList;AImplIntf:TImplementedInterface);
      var
        pd: tprocdef;
      begin
        { GUID (or nil for Corba interfaces) }
        if AImplIntf.IntfDef.objecttype in [odt_interfacecom] then
          rawdata.concat(Tai_const.CreateName(
            make_mangledname('IID',AImplIntf.IntfDef.owner,AImplIntf.IntfDef.objname^),0))
        else
          rawdata.concat(Tai_const.Create_sym(nil));

        { VTable }
        rawdata.concat(Tai_const.Createname(intf_get_vtbl_name(AImplIntf.VtblImplIntf),0));
        { IOffset field }
        case AImplIntf.VtblImplIntf.IType of
          etFieldValue, etFieldValueClass,
          etStandard:
            rawdata.concat(Tai_const.Create_pint(AImplIntf.VtblImplIntf.IOffset));
          etStaticMethodResult, etStaticMethodClass:
            rawdata.concat(Tai_const.Createname(
              tprocdef(tpropertysym(AImplIntf.ImplementsGetter).propaccesslist[palt_read].procdef).mangledname,
              0
            ));
          etVirtualMethodResult, etVirtualMethodClass:
            begin
              pd := tprocdef(tpropertysym(AImplIntf.ImplementsGetter).propaccesslist[palt_read].procdef);
              rawdata.concat(Tai_const.Create_pint(tobjectdef(pd.struct).vmtmethodoffset(pd.extnumber)));
            end;
          else
            internalerror(200802162);
        end;

        { IIDStr }
        rawdata.concat(Tai_const.CreateName(
          make_mangledname('IIDSTR',AImplIntf.IntfDef.owner,AImplIntf.IntfDef.objname^),0));
        { IType }
        rawdata.concat(Tai_const.Create_pint(aint(AImplIntf.VtblImplIntf.IType)));
      end;


    function TVMTWriter.intf_write_table(list : TAsmList):TAsmLabel;
      var
        i        : longint;
        ImplIntf : TImplementedInterface;
      begin
        current_asmdata.getlabel(result,alt_data);
        list.concat(cai_align.create(const_align(sizeof(pint))));
        list.concat(Tai_label.Create(result));
        list.concat(Tai_const.Create_pint(_class.ImplementedInterfaces.count));
        { Write vtbl references }
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            intf_gen_intf_ref(list,ImplIntf);
          end;

        { Write vtbls }
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            if ImplIntf.VtblImplIntf=ImplIntf then
              intf_create_vtbl(list,ImplIntf);
          end;
      end;


  { Write interface identifiers to the data section }
  procedure TVMTWriter.writeinterfaceids;
    var
      i : longint;
      s : string;
    begin
      if assigned(_class.iidguid) then
        begin
          s:=make_mangledname('IID',_class.owner,_class.objname^);
          maybe_new_object_file(current_asmdata.asmlists[al_globals]);
          new_section(current_asmdata.asmlists[al_globals],sec_rodata,s,const_align(sizeof(pint)));
          current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(s,AT_DATA,0));
          current_asmdata.asmlists[al_globals].concat(Tai_const.Create_32bit(longint(_class.iidguid^.D1)));
          current_asmdata.asmlists[al_globals].concat(Tai_const.Create_16bit(_class.iidguid^.D2));
          current_asmdata.asmlists[al_globals].concat(Tai_const.Create_16bit(_class.iidguid^.D3));
          for i:=Low(_class.iidguid^.D4) to High(_class.iidguid^.D4) do
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_8bit(_class.iidguid^.D4[i]));
        end;
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      s:=make_mangledname('IIDSTR',_class.owner,_class.objname^);
      new_section(current_asmdata.asmlists[al_globals],sec_rodata,s,sizeof(pint));
      current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(s,AT_DATA,0));
      current_asmdata.asmlists[al_globals].concat(Tai_const.Create_8bit(length(_class.iidstr^)));
      current_asmdata.asmlists[al_globals].concat(Tai_string.Create(_class.iidstr^));
    end;


    procedure TVMTWriter.writevirtualmethods(List:TAsmList);
      var
         vmtpd : tprocdef;
         vmtentry : pvmtentry;
         i  : longint;
         procname : string;
{$ifdef vtentry}
         hs : string;
{$endif vtentry}
      begin
        if not assigned(_class.VMTEntries) then
          exit;
        for i:=0 to _class.VMTEntries.Count-1 do
         begin
           vmtentry:=pvmtentry(_class.vmtentries[i]);
           vmtpd:=vmtentry^.procdef;
           { safety checks }
           if not(po_virtualmethod in vmtpd.procoptions) then
             internalerror(200611082);
           if vmtpd.extnumber<>i then
             internalerror(200611083);
           if (po_abstractmethod in vmtpd.procoptions) then
             procname:='FPC_ABSTRACTERROR'
           else if not wpoinfomanager.optimized_name_for_vmt(_class,vmtpd,procname) then
             procname:=vmtpd.mangledname;
           List.concat(Tai_const.createname(procname,0));
{$ifdef vtentry}
           hs:='VTENTRY'+'_'+_class.vmt_mangledname+'$$'+tostr(_class.vmtmethodoffset(i) div sizeof(pint));
           current_asmdata.asmlists[al_globals].concat(tai_symbol.CreateName(hs,AT_DATA,0));
{$endif vtentry}
         end;
      end;


    procedure TVMTWriter.writevmt;
      var
         methodnametable,intmessagetable,
         strmessagetable,classnamelabel,
         fieldtablelabel : tasmlabel;
         hs: string;
{$ifdef WITHDMT}
         dmtlabel : tasmlabel;
{$endif WITHDMT}
         interfacetable : tasmlabel;
         templist : TAsmList;
      begin
{$ifdef WITHDMT}
         dmtlabel:=gendmt;
{$endif WITHDMT}
         templist:=TAsmList.Create;

         { write tables for classes, this must be done before the actual
           class is written, because we need the labels defined }
         if is_class(_class) then
          begin
            { write class name }
            current_asmdata.getlabel(classnamelabel,alt_data);
            templist.concat(cai_align.create(const_align(sizeof(pint))));
            templist.concat(Tai_label.Create(classnamelabel));
            hs:=_class.RttiName;
            templist.concat(Tai_const.Create_8bit(length(hs)));
            templist.concat(Tai_string.Create(hs));

            { interface table }
            if _class.ImplementedInterfaces.count>0 then
              interfacetable:=intf_write_table(templist);

            methodnametable:=genpublishedmethodstable(templist);
            fieldtablelabel:=generate_field_table(templist);

            { generate message and dynamic tables }
            if (oo_has_msgstr in _class.objectoptions) then
              strmessagetable:=genstrmsgtab(templist);
            if (oo_has_msgint in _class.objectoptions) then
              intmessagetable:=genintmsgtab(templist);
          end;

        { write debug info }
        maybe_new_object_file(current_asmdata.asmlists[al_globals]);
        new_section(current_asmdata.asmlists[al_globals],sec_rodata,_class.vmt_mangledname,const_align(sizeof(pint)));
        current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(_class.vmt_mangledname,AT_DATA,0));

         { determine the size with symtable.datasize, because }
         { size gives back 4 for classes                    }
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create(aitconst_ptr,tObjectSymtable(_class.symtable).datasize));
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create(aitconst_ptr,-int64(tObjectSymtable(_class.symtable).datasize)));
{$ifdef WITHDMT}
         if _class.classtype=ct_object then
           begin
              if assigned(dmtlabel) then
                current_asmdata.asmlists[al_globals].concat(Tai_const_symbol.Create(dmtlabel)))
              else
                current_asmdata.asmlists[al_globals].concat(Tai_const.Create_ptr(0));
           end;
{$endif WITHDMT}
         { write pointer to parent VMT, this isn't implemented in TP }
         { but this is not used in FPC ? (PM) }
         { it's not used yet, but the delphi-operators as and is need it (FK) }
         { it is not written for parents that don't have any vmt !! }
         if assigned(_class.childof) and
            (oo_has_vmt in _class.childof.objectoptions) then
           current_asmdata.asmlists[al_globals].concat(Tai_const.Createname(_class.childof.vmt_mangledname,0))
         else
           current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));

         { write extended info for classes, for the order see rtl/inc/objpash.inc }
         if is_class(_class) then
          begin
            { pointer to class name string }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(classnamelabel));
            { pointer to dynamic table or nil }
            if (oo_has_msgint in _class.objectoptions) then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(intmessagetable))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));
            { pointer to method table or nil }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(methodnametable));
            { pointer to field table }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(fieldtablelabel));
            { pointer to type info of published section }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(RTTIWriter.get_rtti_label(_class,fullrtti)));
            { inittable for con-/destruction }
            if _class.members_need_inittable then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(RTTIWriter.get_rtti_label(_class,initrtti)))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));
            { auto table }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));
            { interface table }
            if _class.ImplementedInterfaces.count>0 then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(interfacetable))
            else if _class.implements_any_interfaces then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(current_asmdata.RefAsmSymbol('FPC_EMPTYINTF')));
            { table for string messages }
            if (oo_has_msgstr in _class.objectoptions) then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(strmessagetable))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));
          end;
         { write virtual methods }
         writevirtualmethods(current_asmdata.asmlists[al_globals]);
         current_asmdata.asmlists[al_globals].concat(Tai_const.create(aitconst_ptr,0));
         { write the size of the VMT }
         current_asmdata.asmlists[al_globals].concat(Tai_symbol_end.Createname(_class.vmt_mangledname));
{$ifdef vtentry}
         { write vtinherit symbol to notify the linker of the class inheritance tree }
         hs:='VTINHERIT'+'_'+_class.vmt_mangledname+'$$';
         if assigned(_class.childof) then
           hs:=hs+_class.childof.vmt_mangledname
         else
           hs:=hs+_class.vmt_mangledname;
         current_asmdata.asmlists[al_globals].concat(tai_symbol.CreateName(hs,AT_DATA,0));
{$endif vtentry}
         if is_class(_class) then
           current_asmdata.asmlists[al_globals].concatlist(templist);
        templist.Free;
      end;


end.
