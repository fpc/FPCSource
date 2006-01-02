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
       aasmbase,aasmtai
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
         name         : pstring;
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
        procedure writevirtualmethods(List:TAAsmoutput);
      private
        { interface tables }
        function  gintfgetvtbllabelname(intfindex: integer): string;
        procedure gintfcreatevtbl(intfindex: integer; rawdata: TAAsmoutput);
        procedure gintfgenentry(intfindex, contintfindex: integer; rawdata: TAAsmoutput);
        procedure gintfoptimizevtbls;
        procedure gintfwritedata;
        function  gintfgetcprocdef(proc: tprocdef;const name: string): tprocdef;
        procedure gintfdoonintf(intf: tobjectdef; intfindex: longint);
        procedure gintfwalkdowninterface(intf: tobjectdef; intfindex: longint);
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
        { write the VMT to datasegment }
        procedure writevmt;
        procedure writeinterfaceids;
      end;


implementation

    uses
       strings,
       globals,verbose,systems,
       symtable,symconst,symtype,defcmp,defutil
{$ifdef GDB}
       ,gdb
{$endif GDB}
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
              i:=strcomp(p^.data.messageinf.str,at^.data.messageinf.str);
              if i<0 then
                insertstr(p,at^.l,count)
              else if i>0 then
                insertstr(p,at^.r,count)
              else
                Message1(parser_e_duplicate_message_label,strpas(p^.data.messageinf.str));
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
        len : longint;
      begin
         objectlibrary.getdatalabel(p^.nl);
         if assigned(p^.l) then
           writenames(p^.l);
         datasegment.concat(cai_align.create(const_align(sizeof(aint))));
         dataSegment.concat(Tai_label.Create(p^.nl));
         len:=strlen(p^.data.messageinf.str);
         datasegment.concat(tai_const.create_8bit(len));
         getmem(ca,len+1);
         move(p^.data.messageinf.str^,ca^,len+1);
         dataSegment.concat(Tai_string.Create_pchar(ca));
         if assigned(p^.r) then
           writenames(p^.r);
      end;

    procedure tclassheader.writestrentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writestrentry(p^.l);

         { write name label }
         dataSegment.concat(Tai_const.Create_sym(p^.nl));
         dataSegment.concat(Tai_const.Createname(p^.data.mangledname,AT_FUNCTION,0));

         if assigned(p^.r) then
           writestrentry(p^.r);
     end;


    function tclassheader.genstrmsgtab : tasmlabel;
      var
         r : tasmlabel;
         count : longint;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.foreach(@insertmsgstr,@count);

         { write all names }
         if assigned(root) then
           writenames(root);

         { now start writing of the message string table }
         objectlibrary.getdatalabel(r);
         datasegment.concat(cai_align.create(const_align(sizeof(aint))));
         dataSegment.concat(Tai_label.Create(r));
         genstrmsgtab:=r;
         dataSegment.concat(Tai_const.Create_32bit(count));
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
         dataSegment.concat(Tai_const.Create_32bit(p^.data.messageinf.i));
         dataSegment.concat(Tai_const.Createname(p^.data.mangledname,AT_FUNCTION,0));

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
         objectlibrary.getdatalabel(r);
         datasegment.concat(cai_align.create(const_align(sizeof(aint))));
         dataSegment.concat(Tai_label.Create(r));
         genintmsgtab:=r;
         dataSegment.concat(Tai_const.Create_32bit(count));
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
         dataSegment.concat(Tai_const.Create_32bit(p^.data.messageinf.i));
         if assigned(p^.r) then
           writedmtindexentry(p^.r);
      end;

    procedure tclassheader.writedmtaddressentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writedmtaddressentry(p^.l);
         dataSegment.concat(Tai_const_symbol.Createname(p^.data.mangledname,AT_FUNCTION,0));
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
              objectlibrary.getdatalabel(r);
              gendmt:=r;
              datasegment.concat(cai_align.create(const_align(sizeof(aint))));
              dataSegment.concat(Tai_label.Create(r));
              { entries for caching }
              dataSegment.concat(Tai_const.Create_ptr(0));
              dataSegment.concat(Tai_const.Create_ptr(0));

              dataSegment.concat(Tai_const.Create_32bit(count));
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
                     objectlibrary.getdatalabel(l);

                     consts.concat(cai_align.create(const_align(sizeof(aint))));
                     Consts.concat(Tai_label.Create(l));
                     Consts.concat(Tai_const.Create_8bit(length(tsym(p).realname)));
                     Consts.concat(Tai_string.Create(tsym(p).realname));

                     dataSegment.concat(Tai_const.Create_sym(l));
                     if po_abstractmethod in pd.procoptions then
                       dataSegment.concat(Tai_const.Create_sym(nil))
                     else
                       dataSegment.concat(Tai_const.Createname(pd.mangledname,AT_FUNCTION,0));
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
              objectlibrary.getdatalabel(l);
              datasegment.concat(cai_align.create(const_align(sizeof(aint))));
              dataSegment.concat(Tai_label.Create(l));
              dataSegment.concat(Tai_const.Create_32bit(count));
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
            if not(m_fpc in aktmodeswitches) then
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
                 is_visible:=pd.is_visible_for_object(_class);

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
                                     if not(equal_defs(procdefcoll^.data.rettype.def,pd.rettype.def)) and
                                        not((procdefcoll^.data.rettype.def.deftype=objectdef) and
                                         (pd.rettype.def.deftype=objectdef) and
                                         is_class_or_interface(procdefcoll^.data.rettype.def) and
                                         is_class_or_interface(pd.rettype.def) and
                                         (tobjectdef(pd.rettype.def).is_related(
                                             tobjectdef(procdefcoll^.data.rettype.def)))) then
                                       Message2(parser_e_overridden_methods_not_same_ret,pd.fullprocname(false),
                                                procdefcoll^.data.fullprocname(false));

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
            newdefentry(vmtentry,pd,pd.is_visible_for_object(_class));
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

    function  tclassheader.gintfgetvtbllabelname(intfindex: integer): string;
      begin
        gintfgetvtbllabelname:=make_mangledname('VTBL',_class.owner,_class.objname^+
                               '_$_'+_class.implementedinterfaces.interfaces(intfindex).objname^);
      end;


    procedure tclassheader.gintfcreatevtbl(intfindex: integer; rawdata: TAAsmoutput);
      var
        implintf: timplementedinterfaces;
        curintf: tobjectdef;
        proccount: integer;
        tmps: string;
        i: longint;
      begin
        implintf:=_class.implementedinterfaces;
        curintf:=implintf.interfaces(intfindex);
        rawdata.concat(cai_align.create(const_align(sizeof(aint))));
        if maybe_smartlink_symbol then
         rawdata.concat(Tai_symbol.Createname_global(gintfgetvtbllabelname(intfindex),AT_DATA ,0))
        else
         rawdata.concat(Tai_symbol.Createname(gintfgetvtbllabelname(intfindex),AT_DATA,0));
        proccount:=implintf.implproccount(intfindex);
        for i:=1 to proccount do
          begin
            tmps:=make_mangledname('WRPR',_class.owner,_class.objname^+'_$_'+curintf.objname^+'_$_'+
              tostr(i)+'_$_'+
              implintf.implprocs(intfindex,i).mangledname);
            { create reference }
            rawdata.concat(Tai_const.Createname(tmps,AT_FUNCTION,0));
          end;
      end;


    procedure tclassheader.gintfgenentry(intfindex, contintfindex: integer; rawdata: TAAsmoutput);
      var
        implintf: timplementedinterfaces;
        curintf: tobjectdef;
        tmplabel: tasmlabel;
        i: longint;
      begin
        implintf:=_class.implementedinterfaces;
        curintf:=implintf.interfaces(intfindex);
        { GUID }
        if curintf.objecttype in [odt_interfacecom] then
          begin
            { label for GUID }
            objectlibrary.getdatalabel(tmplabel);
            rawdata.concat(cai_align.create(const_align(sizeof(aint))));
            rawdata.concat(Tai_label.Create(tmplabel));
            rawdata.concat(Tai_const.Create_32bit(longint(curintf.iidguid^.D1)));
            rawdata.concat(Tai_const.Create_16bit(curintf.iidguid^.D2));
            rawdata.concat(Tai_const.Create_16bit(curintf.iidguid^.D3));
            for i:=Low(curintf.iidguid^.D4) to High(curintf.iidguid^.D4) do
              rawdata.concat(Tai_const.Create_8bit(curintf.iidguid^.D4[i]));
            dataSegment.concat(Tai_const.Create_sym(tmplabel));
          end
        else
          begin
            { nil for Corba interfaces }
            dataSegment.concat(Tai_const.Create_sym(nil));
          end;
        { VTable }
        dataSegment.concat(Tai_const.Createname(gintfgetvtbllabelname(contintfindex),AT_DATA,0));
        { IOffset field }
        dataSegment.concat(Tai_const.Create_32bit(implintf.ioffsets(contintfindex)));
        { IIDStr }
        objectlibrary.getdatalabel(tmplabel);
        rawdata.concat(cai_align.create(const_align(sizeof(aint))));
        rawdata.concat(Tai_label.Create(tmplabel));
        rawdata.concat(Tai_const.Create_8bit(length(curintf.iidstr^)));
        if curintf.objecttype=odt_interfacecom then
          rawdata.concat(Tai_string.Create(upper(curintf.iidstr^)))
        else
          rawdata.concat(Tai_string.Create(curintf.iidstr^));
        dataSegment.concat(Tai_const.Create_sym(tmplabel));
      end;


    procedure tclassheader.gintfoptimizevtbls;
      type
        tcompintfentry = record
          weight: longint;
          compintf: longint;
        end;
        { Max 1000 interface in the class header interfaces it's enough imho }
        tcompintfs = array[1..1000] of tcompintfentry;
        pcompintfs = ^tcompintfs;
        tequals    = array[1..1000] of longint;
        pequals    = ^tequals;
        timpls    = array[1..1000] of longint;
        pimpls    = ^timpls;
      var
        max: longint;
        equals: pequals;
        compats: pcompintfs;
        impls: pimpls;
        w,i,j,k: longint;
        cij: boolean;
        cji: boolean;
      begin
        max:=_class.implementedinterfaces.count;
        if max>High(tequals) then
          Internalerror(200006135);
        getmem(compats,sizeof(tcompintfentry)*max);
        getmem(equals,sizeof(longint)*max);
        getmem(impls,sizeof(longint)*max);
        fillchar(compats^,sizeof(tcompintfentry)*max,0);
        fillchar(equals^,sizeof(longint)*max,0);
        fillchar(impls^,sizeof(longint)*max,0);
        { ismergepossible is a containing relation
          meaning of ismergepossible(a,b,w) =
          if implementorfunction map of a is contained implementorfunction map of b
          imp(a,b) and imp(b,c) => imp(a,c) ; imp(a,b) and imp(b,a) => a == b
        }
        { the order is very important for correct allocation }
        for i:=1 to max do
          begin
            for j:=i+1 to max do
              begin
                cij:=_class.implementedinterfaces.isimplmergepossible(i,j,w);
                cji:=_class.implementedinterfaces.isimplmergepossible(j,i,w);
                if cij and cji then { i equal j }
                  begin
                    { get minimum index of equal }
                    if equals^[j]=0 then
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
        for i:=1 to max do
          impls^[i]:=i;
        { Replace vtbls when equal or compat, repeat
          until there are no replacements possible anymore. This is
          needed for the cases like:
            First loop: 2->3, 3->1
            Second loop: 2->1 (because 3 was replaced with 1)
        }
        repeat
          k:=0;
          for i:=1 to max do
            begin
              if compats^[impls^[i]].compintf<>0 then
                impls^[i]:=compats^[impls^[i]].compintf
              else if equals^[impls^[i]]<>0 then
                impls^[i]:=equals^[impls^[i]]
              else
                inc(k);
            end;
        until k=max;
        { Update the implindex }
        for i:=1 to max do
          _class.implementedinterfaces.setimplindex(i,impls^[i]);
        freemem(compats);
        freemem(equals);
        freemem(impls);
      end;


    procedure tclassheader.gintfwritedata;
      var
        rawdata: taasmoutput;
        max,i,j : smallint;
      begin
        max:=_class.implementedinterfaces.count;

        rawdata:=TAAsmOutput.Create;
        dataSegment.concat(Tai_const.Create_16bit(max));
        { Two pass, one for allocation and vtbl creation }
        for i:=1 to max do
          begin
            if _class.implementedinterfaces.implindex(i)=i then { if implement itself }
              begin
                { allocate a pointer in the object memory }
                with tobjectsymtable(_class.symtable) do
                  begin
                    datasize:=align(datasize,min(sizeof(aint),fieldalignment));
                    _class.implementedinterfaces.setioffsets(i,datasize);
                    inc(datasize,sizeof(aint));
                  end;
                { write vtbl }
                gintfcreatevtbl(i,rawdata);
              end;
          end;
        { second pass: for fill interfacetable and remained ioffsets }
        for i:=1 to max do
          begin
            j:=_class.implementedinterfaces.implindex(i);
            if j<>i then
              _class.implementedinterfaces.setioffsets(i,_class.implementedinterfaces.ioffsets(j));
            gintfgenentry(i,j,rawdata);
          end;
        dataSegment.concatlist(rawdata);
        rawdata.free;
      end;


    function tclassheader.gintfgetcprocdef(proc: tprocdef;const name: string): tprocdef;
      const
        po_comp = [po_classmethod,po_staticmethod,po_interrupt,po_iocheck,po_msgstr,po_msgint,
                   po_exports,po_varargs,po_explicitparaloc,po_nostackframe];
      var
        sym: tsym;
        implprocdef : Tprocdef;
        i: cardinal;
      begin
        gintfgetcprocdef:=nil;

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
                    gintfgetcprocdef:=implprocdef;
                    exit;
                  end;
              end;
          end;
      end;


    procedure tclassheader.gintfdoonintf(intf: tobjectdef; intfindex: longint);
      var
        def: tdef;
        hs,
        mappedname: string;
        nextexist: pointer;
        implprocdef: tprocdef;
      begin
        def:=tdef(intf.symtable.defindex.first);
        while assigned(def) do
          begin
            if def.deftype=procdef then
              begin
                implprocdef:=nil;
                nextexist:=nil;
                repeat
                  hs:=intf.symtable.name^+'.'+tprocdef(def).procsym.name;
                  mappedname:=_class.implementedinterfaces.getmappings(intfindex,hs,nextexist);
                  if mappedname<>'' then
                    implprocdef:=gintfgetcprocdef(tprocdef(def),mappedname);
                until assigned(implprocdef) or not assigned(nextexist);
                if not assigned(implprocdef) then
                  implprocdef:=gintfgetcprocdef(tprocdef(def),tprocdef(def).procsym.name);
                if assigned(implprocdef) then
                  _class.implementedinterfaces.addimplproc(intfindex,implprocdef)
                else
                  Message1(sym_e_no_matching_implementation_found,tprocdef(def).fullprocname(false));
              end;
            def:=tdef(def.indexnext);
          end;
      end;


    procedure tclassheader.gintfwalkdowninterface(intf: tobjectdef; intfindex: longint);
      begin
        if assigned(intf.childof) then
          gintfwalkdowninterface(intf.childof,intfindex);
        gintfdoonintf(intf,intfindex);
      end;


    function tclassheader.genintftable: tasmlabel;
      var
        intfindex: longint;
        curintf: tobjectdef;
        intftable: tasmlabel;
      begin
        { 1. step collect implementor functions into the implementedinterfaces.implprocs }
        for intfindex:=1 to _class.implementedinterfaces.count do
          begin
            curintf:=_class.implementedinterfaces.interfaces(intfindex);
            gintfwalkdowninterface(curintf,intfindex);
          end;
        { 2. step calc required fieldcount and their offsets in the object memory map
             and write data }
        objectlibrary.getdatalabel(intftable);
        dataSegment.concat(cai_align.create(const_align(sizeof(aint))));
        dataSegment.concat(Tai_label.Create(intftable));
        { Optimize interface tables to reuse wrappers }
        gintfoptimizevtbls;
        { Write interface tables }
        gintfwritedata;
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
          maybe_new_object_file(dataSegment);
          new_section(dataSegment,sec_rodata,s,const_align(sizeof(aint)));
          dataSegment.concat(Tai_symbol.Createname_global(s,AT_DATA,0));
          dataSegment.concat(Tai_const.Create_32bit(longint(_class.iidguid^.D1)));
          dataSegment.concat(Tai_const.Create_16bit(_class.iidguid^.D2));
          dataSegment.concat(Tai_const.Create_16bit(_class.iidguid^.D3));
          for i:=Low(_class.iidguid^.D4) to High(_class.iidguid^.D4) do
            dataSegment.concat(Tai_const.Create_8bit(_class.iidguid^.D4[i]));
        end;
      maybe_new_object_file(dataSegment);
      s:=make_mangledname('IIDSTR',_class.owner,_class.objname^);
      new_section(dataSegment,sec_rodata,s,0);
      dataSegment.concat(Tai_symbol.Createname_global(s,AT_DATA,0));
      dataSegment.concat(Tai_const.Create_8bit(length(_class.iidstr^)));
      dataSegment.concat(Tai_string.Create(_class.iidstr^));
    end;


    procedure tclassheader.writevirtualmethods(List:TAAsmoutput);
      var
         vmtentry : pvmtentry;
         procdefcoll : pprocdefcoll;
         i : longint;
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
                        if procdefcoll^.data.extnumber=i then
                          begin
                             if (po_virtualmethod in procdefcoll^.data.procoptions) then
                               begin
                                  { if a method is abstract, then is also the }
                                  { class abstract and it's not allow to      }
                                  { generates an instance                     }
                                  if (po_abstractmethod in procdefcoll^.data.procoptions) then
                                    List.concat(Tai_const.Createname('FPC_ABSTRACTERROR',AT_FUNCTION,0))
                                  else
                                    List.concat(Tai_const.createname(procdefcoll^.data.mangledname,AT_FUNCTION,0));
                               end;
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
      begin
{$ifdef WITHDMT}
         dmtlabel:=gendmt;
{$endif WITHDMT}

         { write tables for classes, this must be done before the actual
           class is written, because we need the labels defined }
         if is_class(_class) then
          begin
            objectlibrary.getdatalabel(classnamelabel);
            maybe_new_object_file(dataSegment);
            new_section(dataSegment,sec_rodata,classnamelabel.name,const_align(sizeof(aint)));

            { interface table }
            if _class.implementedinterfaces.count>0 then
              interfacetable:=genintftable;

            methodnametable:=genpublishedmethodstable;
            fieldtablelabel:=_class.generate_field_table;
            { write class name }
            dataSegment.concat(Tai_label.Create(classnamelabel));
            dataSegment.concat(Tai_const.Create_8bit(length(_class.objrealname^)));
            dataSegment.concat(Tai_string.Create(_class.objrealname^));

            { generate message and dynamic tables }
            if (oo_has_msgstr in _class.objectoptions) then
              strmessagetable:=genstrmsgtab;
            if (oo_has_msgint in _class.objectoptions) then
              intmessagetable:=genintmsgtab;
          end;

        { write debug info }
        maybe_new_object_file(dataSegment);
        new_section(dataSegment,sec_rodata,_class.vmt_mangledname,const_align(sizeof(aint)));
{$ifdef GDB}
        if (cs_debuginfo in aktmoduleswitches) then
         begin
           do_count_dbx:=true;
           if assigned(_class.owner) and assigned(_class.owner.name) then
             dataSegment.concat(Tai_stabs.Create(strpnew('"vmt_'+_class.owner.name^+_class.name+':S'+
               tstoreddef(vmttype.def).numberstring+'",'+tostr(N_STSYM)+',0,0,'+_class.vmt_mangledname)));
         end;
{$endif GDB}
         dataSegment.concat(Tai_symbol.Createname_global(_class.vmt_mangledname,AT_DATA,0));

         { determine the size with symtable.datasize, because }
         { size gives back 4 for classes                    }
         dataSegment.concat(Tai_const.Create(ait_const_ptr,tobjectsymtable(_class.symtable).datasize));
         dataSegment.concat(Tai_const.Create(ait_const_ptr,-int64(tobjectsymtable(_class.symtable).datasize)));
{$ifdef WITHDMT}
         if _class.classtype=ct_object then
           begin
              if assigned(dmtlabel) then
                dataSegment.concat(Tai_const_symbol.Create(dmtlabel)))
              else
                dataSegment.concat(Tai_const.Create_ptr(0));
           end;
{$endif WITHDMT}
         { write pointer to parent VMT, this isn't implemented in TP }
         { but this is not used in FPC ? (PM) }
         { it's not used yet, but the delphi-operators as and is need it (FK) }
         { it is not written for parents that don't have any vmt !! }
         if assigned(_class.childof) and
            (oo_has_vmt in _class.childof.objectoptions) then
           dataSegment.concat(Tai_const.Createname(_class.childof.vmt_mangledname,AT_DATA,0))
         else
           dataSegment.concat(Tai_const.Create_sym(nil));

         { write extended info for classes, for the order see rtl/inc/objpash.inc }
         if is_class(_class) then
          begin
            { pointer to class name string }
            dataSegment.concat(Tai_const.Create_sym(classnamelabel));
            { pointer to dynamic table or nil }
            if (oo_has_msgint in _class.objectoptions) then
              dataSegment.concat(Tai_const.Create_sym(intmessagetable))
            else
              dataSegment.concat(Tai_const.Create_sym(nil));
            { pointer to method table or nil }
            dataSegment.concat(Tai_const.Create_sym(methodnametable));
            { pointer to field table }
            dataSegment.concat(Tai_const.Create_sym(fieldtablelabel));
            { pointer to type info of published section }
            if (oo_can_have_published in _class.objectoptions) then
              dataSegment.concat(Tai_const.Create_sym(_class.get_rtti_label(fullrtti)))
            else
              dataSegment.concat(Tai_const.Create_sym(nil));
            { inittable for con-/destruction }
            if _class.members_need_inittable then
              dataSegment.concat(Tai_const.Create_sym(_class.get_rtti_label(initrtti)))
            else
              dataSegment.concat(Tai_const.Create_sym(nil));
            { auto table }
            dataSegment.concat(Tai_const.Create_sym(nil));
            { interface table }
            if _class.implementedinterfaces.count>0 then
              dataSegment.concat(Tai_const.Create_sym(interfacetable))
            else
              dataSegment.concat(Tai_const.Create_sym(nil));
            { table for string messages }
            if (oo_has_msgstr in _class.objectoptions) then
              dataSegment.concat(Tai_const.Create_sym(strmessagetable))
            else
              dataSegment.concat(Tai_const.Create_sym(nil));
          end;
         { write virtual methods }
         writevirtualmethods(dataSegment);
         datasegment.concat(Tai_const.create(ait_const_ptr,0));
         { write the size of the VMT }
         dataSegment.concat(Tai_symbol_end.Createname(_class.vmt_mangledname));
      end;


end.
