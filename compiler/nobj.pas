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
      pprocdeftree = ^tprocdeftree;
      tprocdeftree = record
         data : tprocdef;
         nl   : tasmlabel;
         l,r  : pprocdeftree;
      end;

      pprocdefcoll = ^tprocdefcoll;
      tprocdefcoll = record
         data    : tprocdef;
         hidden  : boolean;
         visible : boolean;
         next    : pprocdefcoll;
      end;

      pvmtentry = ^tvmtentry;
      tvmtentry = record
         speedvalue   : cardinal;
         name         : pshortstring;
         firstprocdef : pprocdefcoll;
         next         : pvmtentry;
      end;

      tclassheader=class
      private
        _Class : tobjectdef;
      private
        { message tables }
        root : pprocdeftree;
        procedure disposeprocdeftree(p : pprocdeftree);
        procedure insertmsgint(p : tnamedindexitem;arg:pointer);
        procedure insertmsgstr(p : tnamedindexitem;arg:pointer);
        procedure insertint(p : pprocdeftree;var at : pprocdeftree;var count:longint);
        procedure insertstr(p : pprocdeftree;var at : pprocdeftree;var count:longint);
        procedure writenames(p : pprocdeftree);
        procedure writeintentry(p : pprocdeftree);
        procedure writestrentry(p : pprocdeftree);
{$ifdef WITHDMT}
      private
        { dmt }
        procedure insertdmtentry(p : tnamedindexitem;arg:pointer);
        procedure writedmtindexentry(p : pprocdeftree);
        procedure writedmtaddressentry(p : pprocdeftree);
{$endif}
      private
        { published methods }
        procedure do_count_published_methods(p : tnamedindexitem;arg:pointer);
        procedure do_gen_published_methods(p : tnamedindexitem;arg:pointer);
      private
        { vmt }
        firstvmtentry      : pvmtentry;
        nextvirtnumber     : integer;
        has_constructor,
        has_virtual_method : boolean;
        procedure newdefentry(vmtentry:pvmtentry;pd:tprocdef;is_visible:boolean);
        function  newvmtentry(sym:tprocsym):pvmtentry;
        procedure eachsym(sym : tnamedindexitem;arg:pointer);
        procedure disposevmttree;
        procedure writevirtualmethods(List:TAsmList);
      private
        { interface tables }
        function  intf_get_vtbl_name(AImplIntf:TImplementedInterface): string;
        procedure intf_create_vtbl(rawdata: TAsmList;AImplIntf:TImplementedInterface);
        procedure intf_gen_intf_ref(rawdata: TAsmList;AImplIntf:TImplementedInterface);
        procedure intf_optimize_vtbls;
        procedure intf_write_data;
        function  intf_search_procdef_by_name(proc: tprocdef;const name: string): tprocdef;
        procedure intf_get_procdefs(ImplIntf:TImplementedInterface;IntfDef:TObjectDef);
        procedure intf_get_procdefs_recursive(ImplIntf:TImplementedInterface;IntfDef:TObjectDef);
      public
        constructor create(c:tobjectdef);
        destructor destroy;override;
        { generates the message tables for a class }
        function  genstrmsgtab : tasmlabel;
        function  genintmsgtab : tasmlabel;
        function  genpublishedmethodstable : tasmlabel;
        { generates a VMT entries }
        procedure genvmt;
{$ifdef WITHDMT}
        { generates a DMT for _class }
        function  gendmt : tasmlabel;
{$endif WITHDMT}
        { interfaces }
        function  genintftable: tasmlabel;
        { write the VMT to al_globals }
        procedure writevmt;
        procedure writeinterfaceids;
      end;


implementation

    uses
       SysUtils,
       globals,verbose,systems,
       symtable,symconst,symtype,defcmp,
       dbgbase
       ;


{*****************************************************************************
                                TClassHeader
*****************************************************************************}

    constructor tclassheader.create(c:tobjectdef);
      begin
        inherited Create;
        _Class:=c;
      end;


    destructor tclassheader.destroy;
      begin
        disposevmttree;
      end;


{**************************************
           Message Tables
**************************************}

    procedure tclassheader.disposeprocdeftree(p : pprocdeftree);
      begin
         if assigned(p^.l) then
           disposeprocdeftree(p^.l);
         if assigned(p^.r) then
           disposeprocdeftree(p^.r);
         dispose(p);
      end;


    procedure tclassheader.insertint(p : pprocdeftree;var at : pprocdeftree;var count:longint);

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

    procedure tclassheader.insertstr(p : pprocdeftree;var at : pprocdeftree;var count:longint);

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

    procedure tclassheader.insertmsgint(p : tnamedindexitem;arg:pointer);

      var
         i  : cardinal;
         def: Tprocdef;
         pt : pprocdeftree;

      begin
         if tsym(p).typ=procsym then
            for i:=1 to Tprocsym(p).procdef_count do
              begin
                def:=Tprocsym(p).procdef[i];
                if po_msgint in def.procoptions then
                  begin
                    new(pt);
                    pt^.data:=def;
                    pt^.l:=nil;
                    pt^.r:=nil;
                    insertint(pt,root,plongint(arg)^);
                  end;
              end;
      end;

    procedure tclassheader.insertmsgstr(p : tnamedindexitem;arg:pointer);

      var
         i  : cardinal;
         def: Tprocdef;
         pt : pprocdeftree;

      begin
         if tsym(p).typ=procsym then
            for i:=1 to Tprocsym(p).procdef_count do
              begin
                def:=Tprocsym(p).procdef[i];
                if po_msgstr in def.procoptions then
                  begin
                    new(pt);
                    pt^.data:=def;
                    pt^.l:=nil;
                    pt^.r:=nil;
                    insertstr(pt,root,plongint(arg)^);
                  end;
              end;
      end;

    procedure tclassheader.writenames(p : pprocdeftree);
      var
        ca : pchar;
        len : byte;
      begin
         current_asmdata.getdatalabel(p^.nl);
         if assigned(p^.l) then
           writenames(p^.l);
         current_asmdata.asmlists[al_globals].concat(cai_align.create(const_align(sizeof(aint))));
         current_asmdata.asmlists[al_globals].concat(Tai_label.Create(p^.nl));
         len:=length(p^.data.messageinf.str^);
         current_asmdata.asmlists[al_globals].concat(tai_const.create_8bit(len));
         getmem(ca,len+1);
         move(p^.data.messageinf.str[1],ca^,len);
         ca[len]:=#0;
         current_asmdata.asmlists[al_globals].concat(Tai_string.Create_pchar(ca,len));
         if assigned(p^.r) then
           writenames(p^.r);
      end;

    procedure tclassheader.writestrentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writestrentry(p^.l);

         { write name label }
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(p^.nl));
         current_asmdata.asmlists[al_globals].concat(Tai_const.Createname(p^.data.mangledname,0));

         if assigned(p^.r) then
           writestrentry(p^.r);
     end;


    function tclassheader.genstrmsgtab : tasmlabel;
      var
         count : aint;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.foreach(@insertmsgstr,@count);

         { write all names }
         if assigned(root) then
           writenames(root);

         { now start writing of the message string table }
         current_asmdata.getdatalabel(result);
         current_asmdata.asmlists[al_globals].concat(cai_align.create(const_align(sizeof(aint))));
         current_asmdata.asmlists[al_globals].concat(Tai_label.Create(result));
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create_aint(count));
         if assigned(root) then
           begin
              writestrentry(root);
              disposeprocdeftree(root);
           end;
      end;


    procedure tclassheader.writeintentry(p : pprocdeftree);
      begin
         if assigned(p^.l) then
           writeintentry(p^.l);

         { write name label }
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create_32bit(p^.data.messageinf.i));
         current_asmdata.asmlists[al_globals].concat(Tai_const.Createname(p^.data.mangledname,0));

         if assigned(p^.r) then
           writeintentry(p^.r);
      end;


    function tclassheader.genintmsgtab : tasmlabel;
      var
         r : tasmlabel;
         count : longint;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.foreach(@insertmsgint,@count);

         { now start writing of the message string table }
         current_asmdata.getdatalabel(r);
         current_asmdata.asmlists[al_globals].concat(cai_align.create(const_align(sizeof(aint))));
         current_asmdata.asmlists[al_globals].concat(Tai_label.Create(r));
         genintmsgtab:=r;
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create_32bit(count));
         if assigned(root) then
           begin
              writeintentry(root);
              disposeprocdeftree(root);
           end;
      end;

{$ifdef WITHDMT}

{**************************************
              DMT
**************************************}

    procedure tclassheader.insertdmtentry(p : tnamedindexitem;arg:pointer);

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

    procedure tclassheader.writedmtindexentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writedmtindexentry(p^.l);
         al_globals.concat(Tai_const.Create_32bit(p^.data.messageinf.i));
         if assigned(p^.r) then
           writedmtindexentry(p^.r);
      end;

    procedure tclassheader.writedmtaddressentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writedmtaddressentry(p^.l);
         al_globals.concat(Tai_const_symbol.Createname(p^.data.mangledname,0));
         if assigned(p^.r) then
           writedmtaddressentry(p^.r);
      end;

    function tclassheader.gendmt : tasmlabel;

      var
         r : tasmlabel;

      begin
         root:=nil;
         count:=0;
         gendmt:=nil;
         { insert all message handlers into a tree, sorted by number }
         _class.symtable.foreach(insertdmtentry);

         if count>0 then
           begin
              current_asmdata.getdatalabel(r);
              gendmt:=r;
              al_globals.concat(cai_align.create(const_align(sizeof(aint))));
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

    procedure tclassheader.do_count_published_methods(p : tnamedindexitem;arg:pointer);
      var
        i : longint;
        pd : tprocdef;
      begin
         if (tsym(p).typ=procsym) then
           begin
             for i:=1 to tprocsym(p).procdef_count do
               begin
                 pd:=tprocsym(p).procdef[i];
                 if (pd.procsym=tsym(p)) and
                    (sp_published in pd.symoptions) then
                   inc(plongint(arg)^);
                end;
           end;
      end;


    procedure tclassheader.do_gen_published_methods(p : tnamedindexitem;arg:pointer);
      var
        i  : longint;
        l  : tasmlabel;
        pd : tprocdef;
      begin
         if (tsym(p).typ=procsym) then
           begin
             for i:=1 to tprocsym(p).procdef_count do
               begin
                 pd:=tprocsym(p).procdef[i];
                 if (pd.procsym=tsym(p)) and
                    (sp_published in pd.symoptions) then
                   begin
                     current_asmdata.getdatalabel(l);

                     current_asmdata.asmlists[al_typedconsts].concat(cai_align.create(const_align(sizeof(aint))));
                     current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(l));
                     current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(length(tsym(p).realname)));
                     current_asmdata.asmlists[al_typedconsts].concat(Tai_string.Create(tsym(p).realname));

                     current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(l));
                     if po_abstractmethod in pd.procoptions then
                       current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil))
                     else
                       current_asmdata.asmlists[al_globals].concat(Tai_const.Createname(pd.mangledname,0));
                   end;
                end;
           end;
      end;


    function tclassheader.genpublishedmethodstable : tasmlabel;

      var
         l : tasmlabel;
         count : longint;

      begin
         count:=0;
         _class.symtable.foreach(@do_count_published_methods,@count);
         if count>0 then
           begin
              current_asmdata.getdatalabel(l);
              current_asmdata.asmlists[al_globals].concat(cai_align.create(const_align(sizeof(aint))));
              current_asmdata.asmlists[al_globals].concat(Tai_label.Create(l));
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_32bit(count));
              _class.symtable.foreach(@do_gen_published_methods,nil);
              genpublishedmethodstable:=l;
           end
         else
           genpublishedmethodstable:=nil;
      end;


{**************************************
               VMT
**************************************}


    procedure tclassheader.newdefentry(vmtentry:pvmtentry;pd:tprocdef;is_visible:boolean);
      var
        procdefcoll : pprocdefcoll;
      begin
        if (_class=pd._class) then
          begin
            { new entry is needed, override was not possible }
            if (po_overridingmethod in pd.procoptions) then
              MessagePos1(pd.fileinfo,parser_e_nothing_to_be_overridden,pd.fullprocname(false));

            { check that all methods have overload directive }
            if not(m_fpc in current_settings.modeswitches) then
              begin
                procdefcoll:=vmtentry^.firstprocdef;
                while assigned(procdefcoll) do
                  begin
                    if (procdefcoll^.data._class=pd._class) and
                       ((po_overload in pd.procoptions)<>(po_overload in procdefcoll^.data.procoptions)) then
                      begin
                        MessagePos1(pd.fileinfo,parser_e_no_overload_for_all_procs,pd.procsym.realname);
                        { recover }
                        include(procdefcoll^.data.procoptions,po_overload);
                        include(pd.procoptions,po_overload);
                      end;
                    procdefcoll:=procdefcoll^.next;
                  end;
              end;
          end;

        { generate new entry }
        new(procdefcoll);
        procdefcoll^.data:=pd;
        procdefcoll^.hidden:=false;
        procdefcoll^.visible:=is_visible;
        procdefcoll^.next:=vmtentry^.firstprocdef;
        vmtentry^.firstprocdef:=procdefcoll;

        { give virtual method a number }
        if (po_virtualmethod in pd.procoptions) then
          begin
             pd.extnumber:=nextvirtnumber;
             inc(nextvirtnumber);
             has_virtual_method:=true;
          end;

        if (pd.proctypeoption=potype_constructor) then
          has_constructor:=true;
      end;


    function tclassheader.newvmtentry(sym:tprocsym):pvmtentry;
      begin
        { generate new vmtentry }
        new(result);
        result^.speedvalue:=sym.speedvalue;
        result^.name:=stringdup(sym.name);
        result^.next:=firstvmtentry;
        result^.firstprocdef:=nil;
        firstvmtentry:=result;
      end;


    procedure tclassheader.eachsym(sym : tnamedindexitem;arg:pointer);
      const
        po_comp = [po_classmethod,po_virtualmethod,po_staticmethod,po_interrupt,po_iocheck,po_msgstr,po_msgint,
                   po_exports,po_varargs,po_explicitparaloc,po_nostackframe];
      label
         handlenextdef;
      var
         pd : tprocdef;
         i : cardinal;
         is_visible,
         hasoverloads,
         pdoverload : boolean;
         procdefcoll : pprocdefcoll;
         vmtentry : pvmtentry;
         _name : string;
         _speed : cardinal;
      begin
        if (tsym(sym).typ<>procsym) then
          exit;

        { check the current list of symbols }
        _name:=sym.name;
        _speed:=sym.speedvalue;
        vmtentry:=firstvmtentry;
        while assigned(vmtentry) do
         begin
           { does the symbol already exist in the list? First
             compare speedvalue before doing the string compare to
             speed it up a little }
           if (_speed=vmtentry^.speedvalue) and
              (_name=vmtentry^.name^) then
            begin
              hasoverloads:=(Tprocsym(sym).procdef_count>1);
              { walk through all defs of the symbol }
              for i:=1 to Tprocsym(sym).procdef_count do
                begin
                 pd:=Tprocsym(sym).procdef[i];

                 { is this procdef visible from the class that we are
                   generating. This will be used to hide the other procdefs.
                   When the symbol is not visible we don't hide the other
                   procdefs, because they can be reused in the next class.
                   The check to skip the invisible methods that are in the
                   list is futher down in the code }
                 is_visible:=pd.is_visible_for_object(_class,nil);

                 if pd.procsym=sym then
                  begin
                    pdoverload:=(po_overload in pd.procoptions);

                    { compare with all stored definitions }
                    procdefcoll:=vmtentry^.firstprocdef;
                    while assigned(procdefcoll) do
                      begin
                         { compare only if the definition is not hidden }
                         if not procdefcoll^.hidden then
                          begin
                            { check if one of the two methods has virtual }
                            if (po_virtualmethod in procdefcoll^.data.procoptions) or
                               (po_virtualmethod in pd.procoptions) then
                             begin
                               { if the current definition has no virtual then hide the
                                 old virtual if the new definition has the same arguments or
                                 when it has no overload directive and no overloads }
                               if not(po_virtualmethod in pd.procoptions) then
                                begin
                                  if procdefcoll^.visible and
                                     (not(pdoverload or hasoverloads) or
                                      (compare_paras(procdefcoll^.data.paras,pd.paras,cp_all,[])>=te_equal)) then
                                   begin
                                     if is_visible then
                                       procdefcoll^.hidden:=true;
                                     if (_class=pd._class) and not(po_reintroduce in pd.procoptions) then
                                       MessagePos1(pd.fileinfo,parser_w_should_use_override,pd.fullprocname(false));
                                   end;
                                end
                               { if both are virtual we check the header }
                               else if (po_virtualmethod in pd.procoptions) and
                                       (po_virtualmethod in procdefcoll^.data.procoptions) then
                                begin
                                  { new one has not override }
                                  if is_class(_class) and
                                     not(po_overridingmethod in pd.procoptions) then
                                   begin
                                     { we start a new virtual tree, hide the old }
                                     if (not(pdoverload or hasoverloads) or
                                         (compare_paras(procdefcoll^.data.paras,pd.paras,cp_all,[])>=te_equal)) and
                                        (procdefcoll^.visible) then
                                      begin
                                        if is_visible then
                                          procdefcoll^.hidden:=true;
                                        if (_class=pd._class) and not(po_reintroduce in pd.procoptions) then
                                          MessagePos1(pd.fileinfo,parser_w_should_use_override,pd.fullprocname(false));
                                      end;
                                   end
                                  { same parameters }
                                  else if (compare_paras(procdefcoll^.data.paras,pd.paras,cp_all,[])>=te_equal) then
                                   begin
                                     { overload is inherited }
                                     if (po_overload in procdefcoll^.data.procoptions) then
                                      include(pd.procoptions,po_overload);

                                     { inherite calling convention when it was force and the
                                       current definition has none force }
                                     if (po_hascallingconvention in procdefcoll^.data.procoptions) and
                                        not(po_hascallingconvention in pd.procoptions) then
                                       begin
                                         pd.proccalloption:=procdefcoll^.data.proccalloption;
                                         include(pd.procoptions,po_hascallingconvention);
                                       end;

                                     { the flags have to match except abstract and override }
                                     { only if both are virtual !!  }
                                     if (procdefcoll^.data.proccalloption<>pd.proccalloption) or
                                        (procdefcoll^.data.proctypeoption<>pd.proctypeoption) or
                                        ((procdefcoll^.data.procoptions*po_comp)<>(pd.procoptions*po_comp)) then
                                        begin
                                          MessagePos1(pd.fileinfo,parser_e_header_dont_match_forward,pd.fullprocname(false));
                                          tprocsym(procdefcoll^.data.procsym).write_parameter_lists(pd);
                                        end;

                                     { error, if the return types aren't equal }
                                     if not(equal_defs(procdefcoll^.data.returndef,pd.returndef)) and
                                        not((procdefcoll^.data.returndef.deftype=objectdef) and
                                         (pd.returndef.deftype=objectdef) and
                                         is_class_or_interface(procdefcoll^.data.returndef) and
                                         is_class_or_interface(pd.returndef) and
                                         (tobjectdef(pd.returndef).is_related(
                                             tobjectdef(procdefcoll^.data.returndef)))) then
                                       begin
                                         if not((m_delphi in current_settings.modeswitches) and
                                                is_interface(_class)) then
                                           Message2(parser_e_overridden_methods_not_same_ret,pd.fullprocname(false),
                                                    procdefcoll^.data.fullprocname(false))
                                         else
                                          { Delphi allows changing the result type }
                                          { of interface methods from anything to  }
                                          { anything (JM)                          }
                                           Message2(parser_w_overridden_methods_not_same_ret,pd.fullprocname(false),
                                                    procdefcoll^.data.fullprocname(false));
                                       end;
                                     { check if the method to override is visible, check is only needed
                                       for the current parsed class. Parent classes are already validated and
                                       need to include all virtual methods including the ones not visible in the
                                       current class }
                                     if (_class=pd._class) and
                                        (po_overridingmethod in pd.procoptions) and
                                        (not procdefcoll^.visible) then
                                       MessagePos1(pd.fileinfo,parser_e_nothing_to_be_overridden,pd.fullprocname(false));

                                     { override old virtual method in VMT }
                                     pd.extnumber:=procdefcoll^.data.extnumber;
                                     procdefcoll^.data:=pd;
                                     if is_visible then
                                       procdefcoll^.visible:=true;

                                     goto handlenextdef;
                                   end
                                  { different parameters }
                                  else
                                   begin
                                     { when we got an override directive then can search futher for
                                       the procedure to override.
                                       If we are starting a new virtual tree then hide the old tree }
                                     if not(po_overridingmethod in pd.procoptions) and
                                        not pdoverload then
                                      begin
                                        if is_visible then
                                          procdefcoll^.hidden:=true;
                                        if (_class=pd._class) and not(po_reintroduce in pd.procoptions) then
                                          MessagePos1(pd.fileinfo,parser_w_should_use_override,pd.fullprocname(false));
                                      end;
                                   end;
                                end
                               else
                                begin
                                  { the new definition is virtual and the old static, we hide the old one
                                    if the new defintion has not the overload directive }
                                  if is_visible and
                                     ((not(pdoverload or hasoverloads)) or
                                      (compare_paras(procdefcoll^.data.paras,pd.paras,cp_all,[])>=te_equal)) then
                                    procdefcoll^.hidden:=true;
                                end;
                             end
                            else
                             begin
                               { both are static, we hide the old one if the new defintion
                                 has not the overload directive }
                               if is_visible and
                                  ((not pdoverload) or
                                   (compare_paras(procdefcoll^.data.paras,pd.paras,cp_all,[])>=te_equal)) then
                                 procdefcoll^.hidden:=true;
                             end;
                          end; { not hidden }
                         procdefcoll:=procdefcoll^.next;
                      end;

                    { if it isn't saved in the list we create a new entry }
                    newdefentry(vmtentry,pd,is_visible);
                  end;
                  handlenextdef:
               end;
              exit;
            end;
           vmtentry:=vmtentry^.next;
         end;

        { Generate new procsym entry in vmt }
        vmtentry:=newvmtentry(tprocsym(sym));

        { Add procdefs }
        for i:=1 to Tprocsym(sym).procdef_count do
          begin
            pd:=Tprocsym(sym).procdef[i];
            newdefentry(vmtentry,pd,pd.is_visible_for_object(_class,nil));
          end;
      end;


    procedure tclassheader.disposevmttree;
      var
        vmtentry : pvmtentry;
        procdefcoll : pprocdefcoll;
      begin
        { disposes the above generated tree }
        vmtentry:=firstvmtentry;
        while assigned(vmtentry) do
          begin
            firstvmtentry:=vmtentry^.next;
            stringdispose(vmtentry^.name);
            procdefcoll:=vmtentry^.firstprocdef;
            while assigned(procdefcoll) do
              begin
                vmtentry^.firstprocdef:=procdefcoll^.next;
                dispose(procdefcoll);
                procdefcoll:=vmtentry^.firstprocdef;
              end;
            dispose(vmtentry);
            vmtentry:=firstvmtentry;
          end;
      end;


    procedure tclassheader.genvmt;

      procedure do_genvmt(p : tobjectdef);

        begin
           { start with the base class }
           if assigned(p.childof) then
             do_genvmt(p.childof);

           { walk through all public syms }
           p.symtable.foreach(@eachsym,nil);
        end;

      begin
         firstvmtentry:=nil;
         nextvirtnumber:=0;

         has_constructor:=false;
         has_virtual_method:=false;

         { generates a tree of all used methods }
         do_genvmt(_class);

         if not(is_interface(_class)) and
            has_virtual_method and
            not(has_constructor) then
           Message1(parser_w_virtual_without_constructor,_class.objrealname^);
      end;


{**************************************
           Interface tables
**************************************}

    function  tclassheader.intf_get_vtbl_name(AImplIntf:TImplementedInterface): string;
      begin
        result:=make_mangledname('VTBL',_class.owner,_class.objname^+'_$_'+AImplIntf.IntfDef.objname^);
      end;


    procedure tclassheader.intf_create_vtbl(rawdata: TAsmList;AImplIntf:TImplementedInterface);
      var
        pd : tprocdef;
        vtblstr,
        hs : string;
        i  : longint;
      begin
        vtblstr:=intf_get_vtbl_name(AImplIntf);
        section_symbol_start(rawdata,vtblstr,AT_DATA,true,sec_data,const_align(sizeof(aint)));
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
        section_symbol_end(rawdata,vtblstr);
      end;


    procedure tclassheader.intf_gen_intf_ref(rawdata: TAsmList;AImplIntf:TImplementedInterface);
      var
        iidlabel,
        guidlabel : tasmlabel;
        i: longint;
      begin
        { GUID }
        if AImplIntf.IntfDef.objecttype in [odt_interfacecom] then
          begin
            { label for GUID }
            current_asmdata.getdatalabel(guidlabel);
            rawdata.concat(cai_align.create(const_align(sizeof(aint))));
            rawdata.concat(Tai_label.Create(guidlabel));
            with AImplIntf.IntfDef.iidguid^ do
              begin
                rawdata.concat(Tai_const.Create_32bit(longint(D1)));
                rawdata.concat(Tai_const.Create_16bit(D2));
                rawdata.concat(Tai_const.Create_16bit(D3));
                for i:=Low(D4) to High(D4) do
                  rawdata.concat(Tai_const.Create_8bit(D4[i]));
              end;
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(guidlabel));
          end
        else
          begin
            { nil for Corba interfaces }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));
          end;
        { VTable }
        current_asmdata.asmlists[al_globals].concat(Tai_const.Createname(intf_get_vtbl_name(AImplIntf.VtblImplIntf),0));
        { IOffset field }
        current_asmdata.asmlists[al_globals].concat(Tai_const.Create_aint(AImplIntf.VtblImplIntf.ioffset));
        { IIDStr }
        current_asmdata.getdatalabel(iidlabel);
        rawdata.concat(cai_align.create(const_align(sizeof(aint))));
        rawdata.concat(Tai_label.Create(iidlabel));
        rawdata.concat(Tai_const.Create_8bit(length(AImplIntf.IntfDef.iidstr^)));
        if AImplIntf.IntfDef.objecttype=odt_interfacecom then
          rawdata.concat(Tai_string.Create(upper(AImplIntf.IntfDef.iidstr^)))
        else
          rawdata.concat(Tai_string.Create(AImplIntf.IntfDef.iidstr^));
        current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(iidlabel));
        { EntryType }
        current_asmdata.asmlists[al_globals].concat(Tai_const.Create_aint(aint(AImplIntf.IntfDef.iitype)));
        { EntryOffset }
        current_asmdata.asmlists[al_globals].concat(Tai_const.Create_aint(aint(AImplIntf.IntfDef.iioffset)));
      end;


    procedure tclassheader.intf_optimize_vtbls;
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
        equals: pequals;
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
        getmem(equals,sizeof(longint)*ImplIntfCount);
        getmem(impls,sizeof(longint)*ImplIntfCount);
        filldword(compats^,(sizeof(tcompintfentry) div sizeof(dword))*ImplIntfCount,dword(-1));
        filldword(equals^,ImplIntfCount,dword(-1));
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
                    if equals^[j]=-1 then
                      equals^[j]:=i;
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
              else if equals^[impls^[i]]<>-1 then
                impls^[i]:=equals^[impls^[i]]
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
        freemem(equals);
        freemem(impls);
      end;


    procedure tclassheader.intf_write_data;
      var
        rawdata  : TAsmList;
        i        : longint;
        ImplIntf : TImplementedInterface;
      begin
        rawdata:=TAsmList.Create;
        { Two pass, one for allocation and vtbl creation }
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            { if it implements itself }
            if ImplIntf.VtblImplIntf=ImplIntf then
              begin
                { allocate a pointer in the object memory }
                with tobjectsymtable(_class.symtable) do
                  begin
                    datasize:=align(datasize,sizeof(aint));
                    ImplIntf.Ioffset:=datasize;
                    inc(datasize,sizeof(aint));
                  end;
                { write vtbl }
                intf_create_vtbl(rawdata,ImplIntf);
              end;
          end;
        { second pass: for fill interfacetable and remained ioffsets }
        current_asmdata.asmlists[al_globals].concat(Tai_const.Create_aint(_class.ImplementedInterfaces.count));
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            { Update ioffset of current interface with the ioffset from
              the interface that is reused to implements this interface }
            if ImplIntf.VtblImplIntf<>ImplIntf then
              ImplIntf.Ioffset:=ImplIntf.VtblImplIntf.Ioffset;
            intf_gen_intf_ref(rawdata,ImplIntf);
          end;
        current_asmdata.asmlists[al_globals].concatlist(rawdata);
        rawdata.free;
      end;


    function tclassheader.intf_search_procdef_by_name(proc: tprocdef;const name: string): tprocdef;
      const
        po_comp = [po_classmethod,po_staticmethod,po_interrupt,po_iocheck,po_msgstr,po_msgint,
                   po_exports,po_varargs,po_explicitparaloc,po_nostackframe];
      var
        sym: tsym;
        implprocdef : Tprocdef;
        i: cardinal;
      begin
        result:=nil;

        sym:=tsym(search_class_member(_class,name));
        if assigned(sym) and
           (sym.typ=procsym) then
          begin
            { when the definition has overload directive set, we search for
              overloaded definitions in the class, this only needs to be done once
              for class entries as the tree keeps always the same }
            if (not tprocsym(sym).overloadchecked) and
               (po_overload in tprocsym(sym).first_procdef.procoptions) and
               (tprocsym(sym).owner.symtabletype=objectsymtable) then
             search_class_overloads(tprocsym(sym));

            for i:=1 to tprocsym(sym).procdef_count do
              begin
                implprocdef:=tprocsym(sym).procdef[i];
                if (compare_paras(proc.paras,implprocdef.paras,cp_none,[])>=te_equal) and
                   (proc.proccalloption=implprocdef.proccalloption) and
                   (proc.proctypeoption=implprocdef.proctypeoption) and
                   ((proc.procoptions*po_comp)=((implprocdef.procoptions+[po_virtualmethod])*po_comp)) then
                  begin
                    result:=implprocdef;
                    exit;
                  end;
              end;
          end;
      end;


    procedure tclassheader.intf_get_procdefs(ImplIntf:TImplementedInterface;IntfDef:TObjectDef);
      var
        def: tdef;
        hs,
        prefix,
        mappedname: string;
        implprocdef: tprocdef;
      begin
        prefix:=ImplIntf.IntfDef.symtable.name^+'.';
        def:=tdef(IntfDef.symtable.defindex.first);
        while assigned(def) do
          begin
            if def.deftype=procdef then
              begin
                { Find implementing procdef
                   1. Check for mapped name
                   2. Use symbol name }
                implprocdef:=nil;
                hs:=prefix+tprocdef(def).procsym.name;
                mappedname:=ImplIntf.GetMapping(hs);
                if mappedname<>'' then
                  implprocdef:=intf_search_procdef_by_name(tprocdef(def),mappedname);
                if not assigned(implprocdef) then
                  implprocdef:=intf_search_procdef_by_name(tprocdef(def),tprocdef(def).procsym.name);
                { Add procdef to the implemented interface }
                if assigned(implprocdef) then
                  ImplIntf.AddImplProc(implprocdef)
                else
                  if ImplIntf.IntfDef.iitype = etStandard then
                    Message1(sym_e_no_matching_implementation_found,tprocdef(def).fullprocname(false));
              end;
            def:=tdef(def.indexnext);
          end;
      end;


    procedure tclassheader.intf_get_procdefs_recursive(ImplIntf:TImplementedInterface;IntfDef:TObjectDef);
      begin
        if assigned(IntfDef.childof) then
          intf_get_procdefs_recursive(ImplIntf,IntfDef.childof);
        intf_get_procdefs(ImplIntf,IntfDef);
      end;


    function tclassheader.genintftable: tasmlabel;
      var
        ImplIntf  : TImplementedInterface;
        intftable : tasmlabel;
        i : longint;
      begin
        { 1. step collect implementor functions into the tImplementedInterface.procdefs }
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            intf_get_procdefs_recursive(ImplIntf,ImplIntf.IntfDef);
          end;
        { 2. Optimize interface tables to reuse wrappers }
        intf_optimize_vtbls;
        { 3. Calculate offsets in object map and Write interface tables }
        current_asmdata.getdatalabel(intftable);
        current_asmdata.asmlists[al_globals].concat(cai_align.create(const_align(sizeof(aint))));
        current_asmdata.asmlists[al_globals].concat(Tai_label.Create(intftable));
        intf_write_data;
        genintftable:=intftable;
      end;


  { Write interface identifiers to the data section }
  procedure tclassheader.writeinterfaceids;
    var
      i : longint;
      s : string;
    begin
      if assigned(_class.iidguid) then
        begin
          s:=make_mangledname('IID',_class.owner,_class.objname^);
          maybe_new_object_file(current_asmdata.asmlists[al_globals]);
          new_section(current_asmdata.asmlists[al_globals],sec_rodata,s,const_align(sizeof(aint)));
          current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(s,AT_DATA,0));
          current_asmdata.asmlists[al_globals].concat(Tai_const.Create_32bit(longint(_class.iidguid^.D1)));
          current_asmdata.asmlists[al_globals].concat(Tai_const.Create_16bit(_class.iidguid^.D2));
          current_asmdata.asmlists[al_globals].concat(Tai_const.Create_16bit(_class.iidguid^.D3));
          for i:=Low(_class.iidguid^.D4) to High(_class.iidguid^.D4) do
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_8bit(_class.iidguid^.D4[i]));
        end;
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      s:=make_mangledname('IIDSTR',_class.owner,_class.objname^);
      new_section(current_asmdata.asmlists[al_globals],sec_rodata,s,0);
      current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(s,AT_DATA,0));
      current_asmdata.asmlists[al_globals].concat(Tai_const.Create_8bit(length(_class.iidstr^)));
      current_asmdata.asmlists[al_globals].concat(Tai_string.Create(_class.iidstr^));
    end;


    procedure tclassheader.writevirtualmethods(List:TAsmList);
      var
         vmtentry : pvmtentry;
         procdefcoll : pprocdefcoll;
         i : longint;
         procname : string;
{$ifdef vtentry}
         hs : string;
{$endif vtentry}
      begin
         { walk trough all numbers for virtual methods and search }
         { the method                                             }
         for i:=0 to nextvirtnumber-1 do
           begin
              { walk trough all symbols }
              vmtentry:=firstvmtentry;
              while assigned(vmtentry) do
                begin
                   { walk trough all methods }
                   procdefcoll:=vmtentry^.firstprocdef;
                   while assigned(procdefcoll) do
                     begin
                        { writes the addresses to the VMT }
                        { but only this which are declared as virtual }
                        if (procdefcoll^.data.extnumber=i) and
                           (po_virtualmethod in procdefcoll^.data.procoptions) then
                          begin
                            if (po_abstractmethod in procdefcoll^.data.procoptions) then
                              procname:='FPC_ABSTRACTERROR'
                            else
                              procname:=procdefcoll^.data.mangledname;
                            List.concat(Tai_const.createname(procname,0));
{$ifdef vtentry}
                            hs:='VTENTRY'+'_'+_class.vmt_mangledname+'$$'+tostr(_class.vmtmethodoffset(i) div sizeof(aint));
                            current_asmdata.asmlists[al_globals].concat(tai_symbol.CreateName(hs,AT_DATA,0));
{$endif vtentry}
                            break;
                          end;
                        procdefcoll:=procdefcoll^.next;
                     end;
                   vmtentry:=vmtentry^.next;
                end;
           end;
      end;

    { generates the vmt for classes as well as for objects }
    procedure tclassheader.writevmt;

      var
         methodnametable,intmessagetable,
         strmessagetable,classnamelabel,
         fieldtablelabel : tasmlabel;
{$ifdef WITHDMT}
         dmtlabel : tasmlabel;
{$endif WITHDMT}
         interfacetable : tasmlabel;
{$ifdef vtentry}
         hs: string;
{$endif vtentry}
      begin
{$ifdef WITHDMT}
         dmtlabel:=gendmt;
{$endif WITHDMT}

         { write tables for classes, this must be done before the actual
           class is written, because we need the labels defined }
         if is_class(_class) then
          begin
            current_asmdata.getdatalabel(classnamelabel);
            maybe_new_object_file(current_asmdata.asmlists[al_globals]);
            new_section(current_asmdata.asmlists[al_globals],sec_rodata,classnamelabel.name,const_align(sizeof(aint)));

            { interface table }
            if _class.ImplementedInterfaces.count>0 then
              interfacetable:=genintftable;

            methodnametable:=genpublishedmethodstable;
            fieldtablelabel:=_class.generate_field_table;
            { write class name }
            current_asmdata.asmlists[al_globals].concat(Tai_label.Create(classnamelabel));
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_8bit(length(_class.objrealname^)));
            current_asmdata.asmlists[al_globals].concat(Tai_string.Create(_class.objrealname^));

            { generate message and dynamic tables }
            if (oo_has_msgstr in _class.objectoptions) then
              strmessagetable:=genstrmsgtab;
            if (oo_has_msgint in _class.objectoptions) then
              intmessagetable:=genintmsgtab;
          end;

        { write debug info }
        maybe_new_object_file(current_asmdata.asmlists[al_globals]);
        new_section(current_asmdata.asmlists[al_globals],sec_rodata,_class.vmt_mangledname,const_align(sizeof(aint)));
        current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(_class.vmt_mangledname,AT_DATA,0));

         { determine the size with symtable.datasize, because }
         { size gives back 4 for classes                    }
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create(aitconst_ptr,tobjectsymtable(_class.symtable).datasize));
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create(aitconst_ptr,-int64(tobjectsymtable(_class.symtable).datasize)));
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
            if (oo_can_have_published in _class.objectoptions) then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(_class.get_rtti_label(fullrtti)))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));
            { inittable for con-/destruction }
            if _class.members_need_inittable then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(_class.get_rtti_label(initrtti)))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));
            { auto table }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));
            { interface table }
            if _class.ImplementedInterfaces.count>0 then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(interfacetable))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(nil));
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
      end;


end.
