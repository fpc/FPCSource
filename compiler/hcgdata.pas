{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
unit hcgdata;

{$i defines.inc}

interface

    uses
       cutils,cclasses,
       symdef,aasm;

    type
      pprocdeftree = ^tprocdeftree;
      tprocdeftree = record
         data : tprocdef;
         nl   : tasmlabel;
         l,r  : pprocdeftree;
      end;

      pprocdefcoll = ^tprocdefcoll;
      tprocdefcoll = record
         data : tprocdef;
         next : pprocdefcoll;
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
        procedure insertmsgint(p : tnamedindexitem);
        procedure insertmsgstr(p : tnamedindexitem);
        procedure insertint(p : pprocdeftree;var at : pprocdeftree);
        procedure insertstr(p : pprocdeftree;var at : pprocdeftree);
        procedure writenames(p : pprocdeftree);
        procedure writeintentry(p : pprocdeftree);
        procedure writestrentry(p : pprocdeftree);
{$ifdef WITHDMT}
      private
        { dmt }
        procedure insertdmtentry(p : tnamedindexitem);
        procedure writedmtindexentry(p : pprocdeftree);
        procedure writedmtaddressentry(p : pprocdeftree);
{$endif}
      private
        { published methods }
        procedure do_count(p : tnamedindexitem);
        procedure genpubmethodtableentry(p : tnamedindexitem);
      private
        { vmt }
        wurzel : psymcoll;
        nextvirtnumber : integer;
        has_constructor,
        has_virtual_method : boolean;
        procedure eachsym(sym : tnamedindexitem);
        procedure disposevmttree;
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
      public
        constructor create(c:tobjectdef);
        { generates the message tables for a class }
        function  genstrmsgtab : tasmlabel;
        function  genintmsgtab : tasmlabel;
        function  genpublishedmethodstable : tasmlabel;
{$ifdef WITHDMT}
        { generates a DMT for _class }
        function  gendmt : tasmlabel;
{$endif WITHDMT}
        { generates a VMT for _class }
        procedure genvmt(list : TAAsmoutput);
        { interfaces }
        function  genintftable: tasmlabel;

        procedure writevmt;
        procedure writeinterfaceids;
      end;


implementation

    uses
{$ifdef delphi}
       sysutils,
{$else}
       strings,
{$endif}
       globtype,globals,verbose,
       symtable,symconst,symtype,symsym,types,
{$ifdef GDB}
       gdb,
{$endif GDB}
       systems
{$ifdef i386}
       ,n386ic
{$endif}
       ;


{*****************************************************************************
                                TClassHeader
*****************************************************************************}

    constructor tclassheader.create(c:tobjectdef);
      begin
        inherited Create;
        _Class:=c;
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

    procedure tclassheader.insertmsgint(p : tnamedindexitem);

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
                        pt^.data:=hp;
                        pt^.l:=nil;
                        pt^.r:=nil;
                        insertint(pt,root);
                     end;
                   hp:=hp.nextoverloaded;
                end;
           end;
      end;

    procedure tclassheader.insertmsgstr(p : tnamedindexitem);

      var
         hp : tprocdef;
         pt : pprocdeftree;

      begin
         if tsym(p).typ=procsym then
           begin
              hp:=tprocsym(p).definition;
              while assigned(hp) do
                begin
                   if (po_msgstr in hp.procoptions) then
                     begin
                        new(pt);
                        pt^.data:=hp;
                        pt^.l:=nil;
                        pt^.r:=nil;
                        insertstr(pt,root);
                     end;
                   hp:=hp.nextoverloaded;
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
         _class.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}insertmsgstr);

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
         _class.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}insertmsgint);

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

    procedure tclassheader.insertdmtentry(p : tnamedindexitem);

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

    procedure tclassheader.do_count(p : tnamedindexitem);

      begin
         if (tsym(p).typ=procsym) and (sp_published in tsym(p).symoptions) then
           inc(count);
      end;

    procedure tclassheader.genpubmethodtableentry(p : tnamedindexitem);

      var
         hp : tprocdef;
         l : tasmlabel;

      begin
         if (tsym(p).typ=procsym) and (sp_published in tsym(p).symoptions) then
           begin
              hp:=tprocsym(p).definition;
              if assigned(hp.nextoverloaded) then
                internalerror(1209992);
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
         _class.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}do_count);
         if count>0 then
           begin
              getdatalabel(l);
              dataSegment.concat(Tai_label.Create(l));
              dataSegment.concat(Tai_const.Create_32bit(count));
              _class.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}genpubmethodtableentry);
              genpublishedmethodstable:=l;
           end
         else
           genpublishedmethodstable:=nil;
      end;


{**************************************
               VMT
**************************************}

    procedure tclassheader.eachsym(sym : tnamedindexitem);

      var
         procdefcoll : pprocdefcoll;
         hp : tprocdef;
         symcoll : psymcoll;
         _name : string;
         stored : boolean;

      { creates a new entry in the procsym list }
      procedure newentry;

        begin
           { if not, generate a new symbol item }
           new(symcoll);
           symcoll^.name:=stringdup(sym.name);
           symcoll^.next:=wurzel;
           symcoll^.data:=nil;
           wurzel:=symcoll;
           hp:=tprocsym(sym).definition;

           { inserts all definitions }
           while assigned(hp) do
             begin
                new(procdefcoll);
                procdefcoll^.data:=hp;
                procdefcoll^.next:=symcoll^.data;
                symcoll^.data:=procdefcoll;

                { if it's a virtual method }
                if (po_virtualmethod in hp.procoptions) then
                  begin
                     { then it gets a number ... }
                     hp.extnumber:=nextvirtnumber;
                     { and we inc the number }
                     inc(nextvirtnumber);
                     has_virtual_method:=true;
                  end;

                if (hp.proctypeoption=potype_constructor) then
                  has_constructor:=true;

                { check, if a method should be overridden }
                if (po_overridingmethod in hp.procoptions) then
                  MessagePos1(hp.fileinfo,parser_e_nothing_to_be_overridden,_class.objname^+'.'+_name+hp.demangled_paras);
                { next overloaded method }
                hp:=hp.nextoverloaded;
             end;
        end;

      procedure newdefentry;

        begin
           new(procdefcoll);
           procdefcoll^.data:=hp;
           procdefcoll^.next:=symcoll^.data;
           symcoll^.data:=procdefcoll;

           { if it's a virtual method }
           if (po_virtualmethod in hp.procoptions) then
             begin
                { then it gets a number ... }
                hp.extnumber:=nextvirtnumber;
                { and we inc the number }
                inc(nextvirtnumber);
                has_virtual_method:=true;
             end;

           if (hp.proctypeoption=potype_constructor) then
             has_constructor:=true;

           { check, if a method should be overridden }
           if (po_overridingmethod in hp.procoptions) then
             MessagePos1(hp.fileinfo,parser_e_nothing_to_be_overridden,_class.objname^+'.'+_name+hp.demangled_paras);
        end;

      label
         handlenextdef;

      begin
         { put only sub routines into the VMT }
         if tsym(sym).typ=procsym then
           begin
              _name:=sym.name;
              symcoll:=wurzel;
              while assigned(symcoll) do
                begin
                   { does the symbol already exist in the list ? }
                   if _name=symcoll^.name^ then
                     begin
                        { walk through all defs of the symbol }
                        hp:=tprocsym(sym).definition;
                        while assigned(hp) do
                          begin
                             { compare with all stored definitions }
                             procdefcoll:=symcoll^.data;
                             stored:=false;
                             while assigned(procdefcoll) do
                               begin
                                  { compare parameters }
                                  if equal_paras(procdefcoll^.data.para,hp.para,cp_all) and
                                     (
                                       (po_virtualmethod in procdefcoll^.data.procoptions) or
                                       (po_virtualmethod in hp.procoptions)
                                     ) then
                                    begin { same parameters }
                                       { wenn sie gleich sind }
                                       { und eine davon virtual deklariert ist }
                                       { Fehler falls nur eine VIRTUAL }
                                       if (po_virtualmethod in procdefcoll^.data.procoptions)<>
                                          (po_virtualmethod in hp.procoptions) then
                                         begin
                                            { in classes, we hide the old method }
                                            if is_class(_class) then
                                              begin
                                                 { warn only if it is the first time,
                                                   we hide the method }
                                                 if _class=hp._class then
                                                   Message1(parser_w_should_use_override,hp.fullprocname);
                                              end
                                            else
                                              if _class=hp._class then
                                                begin
                                                   if (po_virtualmethod in procdefcoll^.data.procoptions) then
                                                     Message1(parser_w_overloaded_are_not_both_virtual,
                                                              hp.fullprocname)
                                                   else
                                                     Message1(parser_w_overloaded_are_not_both_non_virtual,
                                                              hp.fullprocname);
                                                end;
                                            { was newentry; exit; (FK) }
                                            newdefentry;
                                            goto handlenextdef;
                                         end
                                       else
                                       { the flags have to match      }
                                       { except abstract and override }
                                       { only if both are virtual !!  }
                                       if (procdefcoll^.data.proccalloptions<>hp.proccalloptions) or
                                          (procdefcoll^.data.proctypeoption<>hp.proctypeoption) or
                                          ((procdefcoll^.data.procoptions-
                                              [po_abstractmethod,po_overridingmethod,po_assembler])<>
                                           (hp.procoptions-[po_abstractmethod,po_overridingmethod,po_assembler])) then
                                         Message1(parser_e_header_dont_match_forward,hp.fullprocname);

                                       { check, if the overridden directive is set }
                                       { (povirtualmethod is set! }

                                       { class ? }
                                       if is_class(_class) and
                                          not(po_overridingmethod in hp.procoptions) then
                                         begin
                                            { warn only if it is the first time,
                                              we hide the method }
                                            if _class=hp._class then
                                              Message1(parser_w_should_use_override,hp.fullprocname);
                                            { was newentry; (FK) }
                                            newdefentry;
                                            exit;
                                         end;

                                       { error, if the return types aren't equal }
                                       if not(is_equal(procdefcoll^.data.rettype.def,hp.rettype.def)) and
                                         not((procdefcoll^.data.rettype.def.deftype=objectdef) and
                                           (hp.rettype.def.deftype=objectdef) and
                                           is_class(procdefcoll^.data.rettype.def) and
                                           is_class(hp.rettype.def) and
                                           (tobjectdef(hp.rettype.def).is_related(
                                               tobjectdef(procdefcoll^.data.rettype.def)))) then
                                         Message2(parser_e_overridden_methods_not_same_ret,hp.fullprocnamewithret,
                                           procdefcoll^.data.fullprocnamewithret);


                                       { now set the number }
                                       hp.extnumber:=procdefcoll^.data.extnumber;
                                       { and exchange }
                                       procdefcoll^.data:=hp;
                                       stored:=true;
                                       goto handlenextdef;
                                    end;  { same parameters }
                                  procdefcoll:=procdefcoll^.next;
                               end;
                             { if it isn't saved in the list }
                             { we create a new entry         }
                             if not(stored) then
                               begin
                                  new(procdefcoll);
                                  procdefcoll^.data:=hp;
                                  procdefcoll^.next:=symcoll^.data;
                                  symcoll^.data:=procdefcoll;
                                  { if the method is virtual ... }
                                  if (po_virtualmethod in hp.procoptions) then
                                    begin
                                       { ... it will get a number }
                                       hp.extnumber:=nextvirtnumber;
                                       inc(nextvirtnumber);
                                    end;
                                  { check, if a method should be overridden }
                                  if (po_overridingmethod in hp.procoptions) then
                                   MessagePos1(hp.fileinfo,parser_e_nothing_to_be_overridden,
                                     hp.fullprocname);
                               end;
                          handlenextdef:
                             hp:=hp.nextoverloaded;
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


    procedure tclassheader.genvmt(list : TAAsmoutput);

      procedure do_genvmt(p : tobjectdef);

        begin
           { start with the base class }
           if assigned(p.childof) then
             do_genvmt(p.childof);

           { walk through all public syms }
           p.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}eachsym);
        end;

      var
         symcoll : psymcoll;
         procdefcoll : pprocdefcoll;
         i : longint;

      begin
         wurzel:=nil;
         nextvirtnumber:=0;

         has_constructor:=false;
         has_virtual_method:=false;

         { generates a tree of all used methods }
         do_genvmt(_class);

         if has_virtual_method and not(has_constructor) then
            Message1(parser_w_virtual_without_constructor,_class.objname^);


         { generates the VMT }

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
         disposevmttree;
      end;


{**************************************
           Interface tables
**************************************}

    function  tclassheader.gintfgetvtbllabelname(intfindex: integer): string;
      begin
        gintfgetvtbllabelname:='_$$_'+upper(_class.objname^)+'_$$_'+
          upper(_class.implementedinterfaces.interfaces(intfindex).objname^)+'_$$_VTBL';
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
        rawdata.concat(Tai_symbol.Createname(gintfgetvtbllabelname(intfindex),0));
        proccount:=implintf.implproccount(intfindex);
        for i:=1 to proccount do
          begin
            tmps:=implintf.implprocs(intfindex,i).mangledname+'_$$_'+upper(curintf.objname^);
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
                    if (dataalignment>=target_os.size_of_pointer) then
                      datasize:=align(datasize,dataalignment)
                    else
                      datasize:=align(datasize,target_os.size_of_pointer);
                    _class.implementedinterfaces.ioffsets(i)^:=datasize;
                    datasize:=datasize+target_os.size_of_pointer;
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
        dataSegment.insertlist(rawdata);
        rawdata.free;
        if (cs_create_smart in aktmoduleswitches) then
          rawcode.insert(Tai_cut.Create);
        codeSegment.insertlist(rawcode);
        rawcode.free;
        freemem(impintfindexes,(max+1)*sizeof(longint));
      end;


    function tclassheader.gintfgetcprocdef(proc: tprocdef;const name: string): tprocdef;
      var
        sym: tprocsym;
        implprocdef: tprocdef;
      begin
        implprocdef:=nil;
        sym:=tprocsym(search_class_member(_class,name));
        if assigned(sym) and (sym.typ=procsym) and not (sp_private in sym.symoptions) then
          begin
            implprocdef:=sym.definition;
            while assigned(implprocdef) and not equal_paras(proc.para,implprocdef.para,cp_none) and
                  (proc.proccalloptions<>implprocdef.proccalloptions) do
              implprocdef:=implprocdef.nextoverloaded;
          end;
        gintfgetcprocdef:=implprocdef;
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
                    procname:=mappedname; { for error messages }
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
      s1,s2 : string;
    begin
       if _class.owner.name=nil then
         s1:=''
       else
         s1:=upper(_class.owner.name^);
       if _class.objname=nil then
         s2:=''
       else
         s2:=upper(_class.objname^);
      s1:=s1+'$_'+s2;
      if _class.isiidguidvalid then
        begin
          if (cs_create_smart in aktmoduleswitches) then
            dataSegment.concat(Tai_cut.Create);
          dataSegment.concat(Tai_symbol.Createname_global('IID$_'+s1,0));
          dataSegment.concat(Tai_const.Create_32bit(longint(_class.iidguid.D1)));
          dataSegment.concat(Tai_const.Create_16bit(_class.iidguid.D2));
          dataSegment.concat(Tai_const.Create_16bit(_class.iidguid.D3));
          for i:=Low(_class.iidguid.D4) to High(_class.iidguid.D4) do
            dataSegment.concat(Tai_const.Create_8bit(_class.iidguid.D4[i]));
        end;
      if (cs_create_smart in aktmoduleswitches) then
        dataSegment.concat(Tai_cut.Create);
      dataSegment.concat(Tai_symbol.Createname_global('IIDSTR$_'+s1,0));
      dataSegment.concat(Tai_const.Create_8bit(length(_class.iidstr^)));
      dataSegment.concat(Tai_string.Create(_class.iidstr^));
    end;

    { generates the vmt for classes as well as for objects }
    procedure tclassheader.writevmt;

      var
         vmtlist : taasmoutput;
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
         { this generates the entries }
         vmtlist:=TAasmoutput.Create;
         genvmt(vmtlist);

         { write tables for classes, this must be done before the actual
           class is written, because we need the labels defined }
         if is_class(_class) then
          begin
            methodnametable:=genpublishedmethodstable;
            fieldtablelabel:=_class.generate_field_table;
            { rtti }
            if (oo_can_have_published in _class.objectoptions) then
             _class.generate_rtti;
            { write class name }
            getdatalabel(classnamelabel);
            dataSegment.concat(Tai_label.Create(classnamelabel));
            dataSegment.concat(Tai_const.Create_8bit(length(_class.objname^)));
            dataSegment.concat(Tai_string.Create(_class.objname^));
            { generate message and dynamic tables }
            if (oo_has_msgstr in _class.objectoptions) then
              strmessagetable:=genstrmsgtab;
            if (oo_has_msgint in _class.objectoptions) then
              intmessagetable:=genintmsgtab
            else
              dataSegment.concat(Tai_const.Create_32bit(0));
            { interface table }
            if _class.implementedinterfaces.count>0 then
              interfacetable:=genintftable;
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
              dataSegment.concat(Tai_const_symbol.Createname(_class.rtti_name))
            else
              dataSegment.concat(Tai_const.Create_32bit(0));
            { inittable for con-/destruction }
            {
            if _class.needs_inittable then
            }
            { we generate the init table for classes always, because needs_inittable }
            { for classes is always false, it applies only for objects               }
            dataSegment.concat(Tai_const_symbol.Create(_class.get_inittable_label));
            {
            else
              dataSegment.concat(Tai_const.Create_32bit(0));
            }
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
         dataSegment.concatlist(vmtlist);
         vmtlist.free;
         { write the size of the VMT }
         dataSegment.concat(Tai_symbol_end.Createname(_class.vmt_mangledname));
      end;


end.
{
  $Log$
  Revision 1.19  2001-04-13 01:22:07  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.18  2001/04/04 21:30:43  florian
    * applied several fixes to get the DD8 Delphi Unit compiled
     e.g. "forward"-interfaces are working now

  Revision 1.17  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.16  2000/11/29 00:30:30  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.15  2000/11/19 16:23:35  florian
  *** empty log message ***

  Revision 1.14  2000/11/12 23:24:10  florian
    * interfaces are basically running

  Revision 1.13  2000/11/08 00:07:40  florian
     * potential range check error fixed

  Revision 1.12  2000/11/06 23:13:53  peter
    * uppercase manglednames

  Revision 1.11  2000/11/04 17:31:00  florian
    * fixed some problems of previous commit

  Revision 1.10  2000/11/04 14:25:19  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.9  2000/11/01 23:04:37  peter
    * tprocdef.fullprocname added for better casesensitve writing of
      procedures

  Revision 1.8  2000/10/31 22:02:47  peter
    * symtable splitted, no real code changes

  Revision 1.7  2000/10/14 10:14:47  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.6  2000/09/24 21:19:50  peter
    * delphi compile fixes

  Revision 1.5  2000/09/24 15:06:17  peter
    * use defines.inc

  Revision 1.4  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.3  2000/07/13 12:08:26  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:41  michael
  + removed logs

}
