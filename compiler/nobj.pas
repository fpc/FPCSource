{
    $Id$
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
       symdef,aasmbase,aasmtai,aasmcpu;

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
         next    : pprocdefcoll;
      end;

      psymcoll = ^tsymcoll;
      tsymcoll = record
         name : pstring;
         data : pprocdefcoll;
         next : psymcoll;
      end;

      tclassheader=class
      private
        _Class : tobjectdef;
        count  : integer;
      private
        { message tables }
        root : pprocdeftree;
        procedure disposeprocdeftree(p : pprocdeftree);
        procedure insertmsgint(p : tnamedindexitem;arg:pointer);
        procedure insertmsgstr(p : tnamedindexitem;arg:pointer);
        procedure insertint(p : pprocdeftree;var at : pprocdeftree);
        procedure insertstr(p : pprocdeftree;var at : pprocdeftree);
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
        procedure do_count(p : tnamedindexitem;arg:pointer);
        procedure genpubmethodtableentry(p : tnamedindexitem;arg:pointer);
      private
        { vmt }
        wurzel : psymcoll;
        nextvirtnumber : integer;
        has_constructor,
        has_virtual_method : boolean;
        procedure eachsym(sym : tnamedindexitem;arg:pointer);
        procedure disposevmttree;
        procedure writevirtualmethods(List:TAAsmoutput);
      private
        { interface tables }
        function  gintfgetvtbllabelname(intfindex: integer): string;
        procedure gintfcreatevtbl(intfindex: integer; rawdata,rawcode: TAAsmoutput);
        procedure gintfgenentry(intfindex, contintfindex: integer; rawdata: TAAsmoutput);
        procedure gintfoptimizevtbls(implvtbl : plongint);
        procedure gintfwritedata;
        function  gintfgetcprocdef(proc: tprocdef;const name: string): tprocdef;
        procedure gintfdoonintf(intf: tobjectdef; intfindex: longint);
        procedure gintfwalkdowninterface(intf: tobjectdef; intfindex: longint);
      protected
        procedure cgintfwrapper(asmlist: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);virtual;abstract;
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

      tclassheaderclass=class of tclassheader;

    var
      cclassheader : tclassheaderclass;


implementation

    uses
{$ifdef delphi}
       sysutils,
{$else}
       strings,
{$endif}
       globtype,globals,verbose,
       symtable,symconst,symtype,symsym,defbase,
{$ifdef GDB}
       gdb,
{$endif GDB}
       cpuinfo
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


    procedure tclassheader.insertint(p : pprocdeftree;var at : pprocdeftree);

      begin
         if at=nil then
           begin
              at:=p;
              inc(count);
           end
         else
           begin
              if p^.data.messageinf.i<at^.data.messageinf.i then
                insertint(p,at^.l)
              else if p^.data.messageinf.i>at^.data.messageinf.i then
                insertint(p,at^.r)
              else
                Message1(parser_e_duplicate_message_label,tostr(p^.data.messageinf.i));
           end;
      end;

    procedure tclassheader.insertstr(p : pprocdeftree;var at : pprocdeftree);

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
                insertstr(p,at^.l)
              else if i>0 then
                insertstr(p,at^.r)
              else
                Message1(parser_e_duplicate_message_label,strpas(p^.data.messageinf.str));
           end;
      end;

    procedure tclassheader.insertmsgint(p : tnamedindexitem;arg:pointer);

      var
         hp : pprocdeflist;
         pt : pprocdeftree;
      begin
         if tsym(p).typ=procsym then
           begin
              hp:=tprocsym(p).defs;
              while assigned(hp) do
                begin
                   if (po_msgint in hp^.def.procoptions) then
                     begin
                        new(pt);
                        pt^.data:=hp^.def;
                        pt^.l:=nil;
                        pt^.r:=nil;
                        insertint(pt,root);
                     end;
                   hp:=hp^.next;
                end;
           end;
      end;

    procedure tclassheader.insertmsgstr(p : tnamedindexitem;arg:pointer);

      var
         hp : pprocdeflist;
         pt : pprocdeftree;

      begin
         if tsym(p).typ=procsym then
           begin
              hp:=tprocsym(p).defs;
              while assigned(hp) do
                begin
                   if (po_msgstr in hp^.def.procoptions) then
                     begin
                        new(pt);
                        pt^.data:=hp^.def;
                        pt^.l:=nil;
                        pt^.r:=nil;
                        insertstr(pt,root);
                     end;
                   hp:=hp^.next;
                end;
           end;
      end;

    procedure tclassheader.writenames(p : pprocdeftree);

      begin
         getdatalabel(p^.nl);
         if assigned(p^.l) then
           writenames(p^.l);
         dataSegment.concat(Tai_label.Create(p^.nl));
         dataSegment.concat(Tai_const.Create_8bit(strlen(p^.data.messageinf.str)));
         dataSegment.concat(Tai_string.Create_pchar(p^.data.messageinf.str));
         if assigned(p^.r) then
           writenames(p^.r);
      end;

    procedure tclassheader.writestrentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writestrentry(p^.l);

         { write name label }
         dataSegment.concat(Tai_const_symbol.Create(p^.nl));
         dataSegment.concat(Tai_const_symbol.Createname(p^.data.mangledname));

         if assigned(p^.r) then
           writestrentry(p^.r);
     end;


    function tclassheader.genstrmsgtab : tasmlabel;
      var
         r : tasmlabel;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}insertmsgstr,nil);

         { write all names }
         if assigned(root) then
           writenames(root);

         { now start writing of the message string table }
         getdatalabel(r);
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
         dataSegment.concat(Tai_const_symbol.Createname(p^.data.mangledname));

         if assigned(p^.r) then
           writeintentry(p^.r);
      end;


    function tclassheader.genintmsgtab : tasmlabel;
      var
         r : tasmlabel;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}insertmsgint,nil);

         { now start writing of the message string table }
         getdatalabel(r);
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
         dataSegment.concat(Tai_const_symbol.Createname(p^.data.mangledname));
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
         _class.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}insertdmtentry);

         if count>0 then
           begin
              getdatalabel(r);
              gendmt:=r;
              dataSegment.concat(Tai_label.Create(r));
              { entries for caching }
              dataSegment.concat(Tai_const.Create_32bit(0));
              dataSegment.concat(Tai_const.Create_32bit(0));

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

    procedure tclassheader.do_count(p : tnamedindexitem;arg:pointer);

      begin
         if (tsym(p).typ=procsym) and (sp_published in tsym(p).symoptions) then
           inc(count);
      end;

    procedure tclassheader.genpubmethodtableentry(p : tnamedindexitem;arg:pointer);

      var
         hp : tprocdef;
         l : tasmlabel;

      begin
         if (tsym(p).typ=procsym) and (sp_published in tsym(p).symoptions) then
           begin
              if assigned(tprocsym(p).defs^.next) then
                internalerror(1209992);
              hp:=tprocsym(p).defs^.def;
              getdatalabel(l);

              Consts.concat(Tai_label.Create(l));
              Consts.concat(Tai_const.Create_8bit(length(p.name)));
              Consts.concat(Tai_string.Create(p.name));

              dataSegment.concat(Tai_const_symbol.Create(l));
              dataSegment.concat(Tai_const_symbol.Createname(hp.mangledname));
           end;
      end;

    function tclassheader.genpublishedmethodstable : tasmlabel;

      var
         l : tasmlabel;

      begin
         count:=0;
         _class.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}do_count,nil);
         if count>0 then
           begin
              getdatalabel(l);
              dataSegment.concat(Tai_label.Create(l));
              dataSegment.concat(Tai_const.Create_32bit(count));
              _class.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}genpubmethodtableentry,nil);
              genpublishedmethodstable:=l;
           end
         else
           genpublishedmethodstable:=nil;
      end;


{**************************************
               VMT
**************************************}

    procedure tclassheader.eachsym(sym : tnamedindexitem;arg:pointer);

      var
         procdefcoll : pprocdefcoll;
         hp : pprocdeflist;
         symcoll : psymcoll;
         _name : string;

      procedure newdefentry(pd:tprocdef);
        begin
           new(procdefcoll);
           procdefcoll^.data:=pd;
           procdefcoll^.hidden:=false;
           procdefcoll^.next:=symcoll^.data;
           symcoll^.data:=procdefcoll;

           { if it's a virtual method }
           if (po_virtualmethod in pd.procoptions) then
             begin
                { then it gets a number ... }
                pd.extnumber:=nextvirtnumber;
                { and we inc the number }
                inc(nextvirtnumber);
                has_virtual_method:=true;
             end;

           if (pd.proctypeoption=potype_constructor) then
             has_constructor:=true;

           { check, if a method should be overridden }
           if (pd._class=_class) and
              (po_overridingmethod in pd.procoptions) then
             MessagePos1(pd.fileinfo,parser_e_nothing_to_be_overridden,pd.fullprocname);
        end;

      { creates a new entry in the procsym list }
      procedure newentry;
        begin
           { if not, generate a new symbol item }
           new(symcoll);
           symcoll^.name:=stringdup(sym.name);
           symcoll^.next:=wurzel;
           symcoll^.data:=nil;
           wurzel:=symcoll;

           { inserts all definitions }
           hp:=tprocsym(sym).defs;
           while assigned(hp) do
             begin
                newdefentry(hp^.def);
                hp:=hp^.next;
             end;
        end;

      label
         handlenextdef;
      var
         pd : tprocdef;
         pdoverload : boolean;
      begin
         { put only sub routines into the VMT }
         if tsym(sym).typ=procsym then
           begin
              { check the current list of symbols }
              _name:=sym.name;
              symcoll:=wurzel;
              while assigned(symcoll) do
               begin
                 { does the symbol already exist in the list ? }
                 if _name=symcoll^.name^ then
                  begin
                    { walk through all defs of the symbol }
                    hp:=tprocsym(sym).defs;
                    while assigned(hp) do
                     begin
                       pd:=hp^.def;
                       if pd.procsym=sym then
                        begin
                          pdoverload:=(po_overload in pd.procoptions);

                          { compare with all stored definitions }
                          procdefcoll:=symcoll^.data;
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
                                       has no overload directive }
                                     if not(po_virtualmethod in pd.procoptions) then
                                      begin
                                        if (not pdoverload or
                                            equal_paras(procdefcoll^.data.para,pd.para,cp_value_equal_const)) and
                                           (tstoredsym(procdefcoll^.data.procsym).is_visible_for_object(pd._class)) then
                                         begin
                                           procdefcoll^.hidden:=true;
                                           if _class=pd._class then
                                             MessagePos1(pd.fileinfo,parser_w_should_use_override,pd.fullprocname);
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
                                           if (not pdoverload or
                                               equal_paras(procdefcoll^.data.para,pd.para,cp_value_equal_const)) and
                                              (tstoredsym(procdefcoll^.data.procsym).is_visible_for_object(pd._class)) then
                                            begin
                                              procdefcoll^.hidden:=true;
                                              if _class=pd._class then
                                                MessagePos1(pd.fileinfo,parser_w_should_use_override,pd.fullprocname);
                                            end;
                                         end
                                        { check if the method to override is visible }
                                        else if (po_overridingmethod in pd.procoptions) and
                                                (not tstoredsym(procdefcoll^.data.procsym).is_visible_for_object(pd._class)) then
                                         begin
                                           { do nothing, the error will follow when adding the entry }
                                         end
                                        { same parameters }
                                        else if (equal_paras(procdefcoll^.data.para,pd.para,cp_value_equal_const)) then
                                         begin
                                           { overload is inherited }
                                           if (po_overload in procdefcoll^.data.procoptions) then
                                            include(pd.procoptions,po_overload);

                                           { the flags have to match except abstract and override }
                                           { only if both are virtual !!  }
                                           if (procdefcoll^.data.proccalloption<>pd.proccalloption) or
                                               (procdefcoll^.data.proctypeoption<>pd.proctypeoption) or
                                               ((procdefcoll^.data.procoptions-
                                                   [po_abstractmethod,po_overridingmethod,po_assembler,po_overload])<>
                                                (pd.procoptions-[po_abstractmethod,po_overridingmethod,po_assembler,po_overload])) then
                                              MessagePos1(pd.fileinfo,parser_e_header_dont_match_forward,pd.fullprocname);

                                           { error, if the return types aren't equal }
                                           if not(is_equal(procdefcoll^.data.rettype.def,pd.rettype.def)) and
                                              not((procdefcoll^.data.rettype.def.deftype=objectdef) and
                                               (pd.rettype.def.deftype=objectdef) and
                                               is_class(procdefcoll^.data.rettype.def) and
                                               is_class(pd.rettype.def) and
                                               (tobjectdef(pd.rettype.def).is_related(
                                                   tobjectdef(procdefcoll^.data.rettype.def)))) then
                                             Message2(parser_e_overridden_methods_not_same_ret,pd.fullprocnamewithret,
                                                      procdefcoll^.data.fullprocnamewithret);

                                           { now set the number }
                                           pd.extnumber:=procdefcoll^.data.extnumber;
                                           { and exchange }
                                           procdefcoll^.data:=pd;
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
                                              procdefcoll^.hidden:=true;
                                              if _class=pd._class then
                                                MessagePos1(pd.fileinfo,parser_w_should_use_override,pd.fullprocname);
                                            end;
                                         end;
                                      end
                                     else
                                      begin
                                        { the new definition is virtual and the old static, we hide the old one
                                          if the new defintion has not the overload directive }
                                        if not pdoverload or
                                           equal_paras(procdefcoll^.data.para,pd.para,cp_value_equal_const) then
                                         procdefcoll^.hidden:=true;
                                      end;
                                   end
                                  else
                                   begin
                                     { both are static, we hide the old one if the new defintion
                                       has not the overload directive }
                                     if equal_paras(procdefcoll^.data.para,pd.para,cp_value_equal_const) or
                                        not pdoverload then
                                      procdefcoll^.hidden:=true;
                                   end;
                                end; { not hidden }
                               procdefcoll:=procdefcoll^.next;
                            end;

                          { if it isn't saved in the list we create a new entry }
                          newdefentry(pd);
                        end;
                     handlenextdef:
                       hp:=hp^.next;
                     end;
                    exit;
                  end;
                 symcoll:=symcoll^.next;
               end;
             newentry;
           end;
      end;

     procedure tclassheader.disposevmttree;

       var
          symcoll : psymcoll;
          procdefcoll : pprocdefcoll;

       begin
          { disposes the above generated tree }
          symcoll:=wurzel;
          while assigned(symcoll) do
            begin
               wurzel:=symcoll^.next;
               stringdispose(symcoll^.name);
               procdefcoll:=symcoll^.data;
               while assigned(procdefcoll) do
                 begin
                    symcoll^.data:=procdefcoll^.next;
                    dispose(procdefcoll);
                    procdefcoll:=symcoll^.data;
                 end;
               dispose(symcoll);
               symcoll:=wurzel;
            end;
       end;


    procedure tclassheader.genvmt;

      procedure do_genvmt(p : tobjectdef);

        begin
           { start with the base class }
           if assigned(p.childof) then
             do_genvmt(p.childof);

           { walk through all public syms }
           p.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}eachsym,nil);
        end;

      begin
         wurzel:=nil;
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
        gintfgetvtbllabelname:=mangledname_prefix('VTBL',_class.owner)+_class.objname^+
                               '_$_'+_class.implementedinterfaces.interfaces(intfindex).objname^;
      end;


    procedure tclassheader.gintfcreatevtbl(intfindex: integer; rawdata,rawcode: TAAsmoutput);
      var
        implintf: timplementedinterfaces;
        curintf: tobjectdef;
        proccount: integer;
        tmps: string;
        i: longint;
      begin
        implintf:=_class.implementedinterfaces;
        curintf:=implintf.interfaces(intfindex);
        if (cs_create_smart in aktmoduleswitches) then
         rawdata.concat(Tai_symbol.Createname_global(gintfgetvtbllabelname(intfindex),0))
        else
         rawdata.concat(Tai_symbol.Createname(gintfgetvtbllabelname(intfindex),0));
        proccount:=implintf.implproccount(intfindex);
        for i:=1 to proccount do
          begin
            tmps:=implintf.implprocs(intfindex,i).mangledname+'_$_'+curintf.objname^;
            { create wrapper code }
            cgintfwrapper(rawcode,implintf.implprocs(intfindex,i),tmps,implintf.ioffsets(intfindex)^);
            { create reference }
            rawdata.concat(Tai_const_symbol.Createname(tmps));
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
            getdatalabel(tmplabel);
            rawdata.concat(Tai_label.Create(tmplabel));
            rawdata.concat(Tai_const.Create_32bit(curintf.iidguid.D1));
            rawdata.concat(Tai_const.Create_16bit(curintf.iidguid.D2));
            rawdata.concat(Tai_const.Create_16bit(curintf.iidguid.D3));
            for i:=Low(curintf.iidguid.D4) to High(curintf.iidguid.D4) do
              rawdata.concat(Tai_const.Create_8bit(curintf.iidguid.D4[i]));
            dataSegment.concat(Tai_const_symbol.Create(tmplabel));
          end
        else
          begin
            { nil for Corba interfaces }
            dataSegment.concat(Tai_const.Create_32bit(0)); { nil }
          end;
        { VTable }
        dataSegment.concat(Tai_const_symbol.Createname(gintfgetvtbllabelname(contintfindex)));
        { IOffset field }
        dataSegment.concat(Tai_const.Create_32bit(implintf.ioffsets(contintfindex)^));
        { IIDStr }
        getdatalabel(tmplabel);
        rawdata.concat(Tai_label.Create(tmplabel));
        rawdata.concat(Tai_const.Create_8bit(length(curintf.iidstr^)));
        if curintf.objecttype=odt_interfacecom then
          rawdata.concat(Tai_string.Create(upper(curintf.iidstr^)))
        else
          rawdata.concat(Tai_string.Create(curintf.iidstr^));
        dataSegment.concat(Tai_const_symbol.Create(tmplabel));
      end;


    procedure tclassheader.gintfoptimizevtbls(implvtbl : plongint);
      type
        tcompintfentry = record
          weight: longint;
          compintf: longint;
        end;
        { Max 1000 interface in the class header interfaces it's enough imho }
        tcompintfs = packed array[1..1000] of tcompintfentry;
        pcompintfs = ^tcompintfs;
        tequals    = packed array[1..1000] of longint;
        pequals    = ^tequals;
      var
        max: longint;
        equals: pequals;
        compats: pcompintfs;
        i: longint;
        j: longint;
        w: longint;
        cij: boolean;
        cji: boolean;
      begin
        max:=_class.implementedinterfaces.count;
        if max>High(tequals) then
          Internalerror(200006135);
        getmem(compats,sizeof(tcompintfentry)*max);
        getmem(equals,sizeof(longint)*max);
        fillchar(compats^,sizeof(tcompintfentry)*max,0);
        fillchar(equals^,sizeof(longint)*max,0);
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
        for i:=1 to max do
          begin
            if compats^[i].compintf<>0 then
              implvtbl[i]:=compats^[i].compintf
            else if equals^[i]<>0 then
              implvtbl[i]:=equals^[i]
            else
              implvtbl[i]:=i;
          end;
        freemem(compats,sizeof(tcompintfentry)*max);
        freemem(equals,sizeof(longint)*max);
      end;


    procedure tclassheader.gintfwritedata;
      var
        rawdata,rawcode: taasmoutput;
        impintfindexes: plongint;
        max: longint;
        i: longint;
      begin
        max:=_class.implementedinterfaces.count;
        getmem(impintfindexes,(max+1)*sizeof(longint));

        gintfoptimizevtbls(impintfindexes);

        rawdata:=TAAsmOutput.Create;
        rawcode:=TAAsmOutput.Create;
        dataSegment.concat(Tai_const.Create_16bit(max));
        { Two pass, one for allocation and vtbl creation }
        for i:=1 to max do
          begin
            if impintfindexes[i]=i then { if implement itself }
              begin
                { allocate a pointer in the object memory }
                with tstoredsymtable(_class.symtable) do
                  begin
                    if (dataalignment>=pointer_size) then
                      datasize:=align(datasize,dataalignment)
                    else
                      datasize:=align(datasize,pointer_size);
                    _class.implementedinterfaces.ioffsets(i)^:=datasize;
                    datasize:=datasize+pointer_size;
                  end;
                { write vtbl }
                gintfcreatevtbl(i,rawdata,rawcode);
              end;
          end;
        { second pass: for fill interfacetable and remained ioffsets }
        for i:=1 to max do
          begin
            if i<>impintfindexes[i] then { why execute x:=x ? }
              with _class.implementedinterfaces do
                ioffsets(i)^:=ioffsets(impintfindexes[i])^;
            gintfgenentry(i,impintfindexes[i],rawdata);
          end;
        dataSegment.concatlist(rawdata);
        rawdata.free;
        codeSegment.concatlist(rawcode);
        rawcode.free;
        freemem(impintfindexes,(max+1)*sizeof(longint));
      end;


    function tclassheader.gintfgetcprocdef(proc: tprocdef;const name: string): tprocdef;
      var
        sym: tprocsym;
        implprocdef : pprocdeflist;
      begin
        gintfgetcprocdef:=nil;
        sym:=tprocsym(search_class_member(_class,name));
        if assigned(sym) and (sym.typ=procsym) then
          begin
            implprocdef:=sym.defs;
            while assigned(implprocdef) do
             begin
               if equal_paras(proc.para,implprocdef^.def.para,cp_none) and
                  (proc.proccalloption=implprocdef^.def.proccalloption) then
                begin
                  gintfgetcprocdef:=implprocdef^.def;
                  exit;
                end;
               implprocdef:=implprocdef^.next;
             end;
          end;
      end;


    procedure tclassheader.gintfdoonintf(intf: tobjectdef; intfindex: longint);
      var
        i: longint;
        proc: tprocdef;
        procname: string; { for error }
        mappedname: string;
        nextexist: pointer;
        implprocdef: tprocdef;
      begin
        for i:=1 to intf.symtable.defindex.count do
          begin
            proc:=tprocdef(intf.symtable.defindex.search(i));
            if proc.deftype=procdef then
              begin
                procname:='';
                implprocdef:=nil;
                nextexist:=nil;
                repeat
                  mappedname:=_class.implementedinterfaces.getmappings(intfindex,proc.procsym.name,nextexist);
                  if procname='' then
                    procname:=proc.procsym.name;
                    //mappedname; { for error messages }
                  if mappedname<>'' then
                    implprocdef:=gintfgetcprocdef(proc,mappedname);
                until assigned(implprocdef) or not assigned(nextexist);
                if not assigned(implprocdef) then
                  implprocdef:=gintfgetcprocdef(proc,proc.procsym.name);
                if procname='' then
                  procname:=proc.procsym.name;
                if assigned(implprocdef) then
                  _class.implementedinterfaces.addimplproc(intfindex,implprocdef)
                else
                  Message1(sym_e_id_not_found,procname);
              end;
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
        getdatalabel(intftable);
        dataSegment.concat(Tai_label.Create(intftable));
        gintfwritedata;
        _class.implementedinterfaces.clearimplprocs; { release temporary information }
        genintftable:=intftable;
      end;


  { Write interface identifiers to the data section }
  procedure tclassheader.writeinterfaceids;
    var
      i: longint;
    begin
      if _class.isiidguidvalid then
        begin
          if (cs_create_smart in aktmoduleswitches) then
            dataSegment.concat(Tai_cut.Create);
          dataSegment.concat(Tai_symbol.Createname_global(mangledname_prefix('IID',_class.owner)+_class.objname^,0));
          dataSegment.concat(Tai_const.Create_32bit(longint(_class.iidguid.D1)));
          dataSegment.concat(Tai_const.Create_16bit(_class.iidguid.D2));
          dataSegment.concat(Tai_const.Create_16bit(_class.iidguid.D3));
          for i:=Low(_class.iidguid.D4) to High(_class.iidguid.D4) do
            dataSegment.concat(Tai_const.Create_8bit(_class.iidguid.D4[i]));
        end;
      if (cs_create_smart in aktmoduleswitches) then
        dataSegment.concat(Tai_cut.Create);
      dataSegment.concat(Tai_symbol.Createname_global(mangledname_prefix('IIDSTR',_class.owner)+_class.objname^,0));
      dataSegment.concat(Tai_const.Create_8bit(length(_class.iidstr^)));
      dataSegment.concat(Tai_string.Create(_class.iidstr^));
    end;


    procedure tclassheader.writevirtualmethods(List:TAAsmoutput);
      var
         symcoll : psymcoll;
         procdefcoll : pprocdefcoll;
         i : longint;
      begin
         { walk trough all numbers for virtual methods and search }
         { the method                                             }
         for i:=0 to nextvirtnumber-1 do
           begin
              symcoll:=wurzel;

              { walk trough all symbols }
              while assigned(symcoll) do
                begin

                   { walk trough all methods }
                   procdefcoll:=symcoll^.data;
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
                                    begin
                                       include(_class.objectoptions,oo_has_abstract);
                                       List.concat(Tai_const_symbol.Createname('FPC_ABSTRACTERROR'));
                                    end
                                  else
                                    begin
                                      List.concat(Tai_const_symbol.createname(procdefcoll^.data.mangledname));
                                    end;
                               end;
                          end;
                        procdefcoll:=procdefcoll^.next;
                     end;
                   symcoll:=symcoll^.next;
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

         if (cs_create_smart in aktmoduleswitches) then
           dataSegment.concat(Tai_cut.Create);

         { write tables for classes, this must be done before the actual
           class is written, because we need the labels defined }
         if is_class(_class) then
          begin
            { interface table }
            if _class.implementedinterfaces.count>0 then
             begin
               if (cs_create_smart in aktmoduleswitches) then
                codeSegment.concat(Tai_cut.Create);
               interfacetable:=genintftable;
             end;

            methodnametable:=genpublishedmethodstable;
            fieldtablelabel:=_class.generate_field_table;
            { write class name }
            getdatalabel(classnamelabel);
            dataSegment.concat(Tai_label.Create(classnamelabel));
            dataSegment.concat(Tai_const.Create_8bit(length(_class.objrealname^)));
            dataSegment.concat(Tai_string.Create(_class.objrealname^));
            { generate message and dynamic tables }
            if (oo_has_msgstr in _class.objectoptions) then
              strmessagetable:=genstrmsgtab;
            if (oo_has_msgint in _class.objectoptions) then
              intmessagetable:=genintmsgtab
            else
              dataSegment.concat(Tai_const.Create_32bit(0));
          end;

        { write debug info }
{$ifdef GDB}
        if (cs_debuginfo in aktmoduleswitches) then
         begin
           do_count_dbx:=true;
           if assigned(_class.owner) and assigned(_class.owner.name) then
             dataSegment.concat(Tai_stabs.Create(strpnew('"vmt_'+_class.owner.name^+_class.name+':S'+
               typeglobalnumber('__vtbl_ptr_type')+'",'+tostr(N_STSYM)+',0,0,'+_class.vmt_mangledname)));
         end;
{$endif GDB}
         dataSegment.concat(Tai_symbol.Createdataname_global(_class.vmt_mangledname,0));

         { determine the size with symtable.datasize, because }
         { size gives back 4 for classes                    }
         dataSegment.concat(Tai_const.Create_32bit(_class.symtable.datasize));
         dataSegment.concat(Tai_const.Create_32bit(-_class.symtable.datasize));
{$ifdef WITHDMT}
         if _class.classtype=ct_object then
           begin
              if assigned(dmtlabel) then
                dataSegment.concat(Tai_const_symbol.Create(dmtlabel)))
              else
                dataSegment.concat(Tai_const.Create_32bit(0));
           end;
{$endif WITHDMT}
         { write pointer to parent VMT, this isn't implemented in TP }
         { but this is not used in FPC ? (PM) }
         { it's not used yet, but the delphi-operators as and is need it (FK) }
         { it is not written for parents that don't have any vmt !! }
         if assigned(_class.childof) and
            (oo_has_vmt in _class.childof.objectoptions) then
           dataSegment.concat(Tai_const_symbol.Createname(_class.childof.vmt_mangledname))
         else
           dataSegment.concat(Tai_const.Create_32bit(0));

         { write extended info for classes, for the order see rtl/inc/objpash.inc }
         if is_class(_class) then
          begin
            { pointer to class name string }
            dataSegment.concat(Tai_const_symbol.Create(classnamelabel));
            { pointer to dynamic table }
            if (oo_has_msgint in _class.objectoptions) then
              dataSegment.concat(Tai_const_symbol.Create(intmessagetable))
            else
              dataSegment.concat(Tai_const.Create_32bit(0));
            { pointer to method table }
            if assigned(methodnametable) then
              dataSegment.concat(Tai_const_symbol.Create(methodnametable))
            else
              dataSegment.concat(Tai_const.Create_32bit(0));
            { pointer to field table }
            dataSegment.concat(Tai_const_symbol.Create(fieldtablelabel));
            { pointer to type info of published section }
            if (oo_can_have_published in _class.objectoptions) then
              dataSegment.concat(Tai_const_symbol.Create(_class.get_rtti_label(fullrtti)))
            else
              dataSegment.concat(Tai_const.Create_32bit(0));
            { inittable for con-/destruction, for classes this is always generated }
            dataSegment.concat(Tai_const_symbol.Create(_class.get_rtti_label(initrtti)));
            { auto table }
            dataSegment.concat(Tai_const.Create_32bit(0));
            { interface table }
            if _class.implementedinterfaces.count>0 then
              dataSegment.concat(Tai_const_symbol.Create(interfacetable))
            else
              dataSegment.concat(Tai_const.Create_32bit(0));
            { table for string messages }
            if (oo_has_msgstr in _class.objectoptions) then
              dataSegment.concat(Tai_const_symbol.Create(strmessagetable))
            else
              dataSegment.concat(Tai_const.Create_32bit(0));
          end;
         { write virtual methods }
         writevirtualmethods(dataSegment);
         { write the size of the VMT }
         dataSegment.concat(Tai_symbol_end.Createname(_class.vmt_mangledname));
      end;


initialization
  cclassheader:=tclassheader;
end.
{
  $Log$
  Revision 1.22  2002-07-20 11:57:55  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.21  2002/07/01 18:46:23  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.20  2002/05/18 13:34:10  peter
    * readded missing revisions

  Revision 1.19  2002/05/16 19:46:39  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.17  2002/05/12 16:53:08  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.16  2002/04/20 21:32:24  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.15  2002/04/19 15:46:01  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.14  2002/04/15 18:59:07  carl
  + target_info.size_of_pointer -> pointer_Size

  Revision 1.13  2002/02/11 18:51:35  peter
    * fixed vmt generation for private procedures that were skipped after
      my previous changes

}
