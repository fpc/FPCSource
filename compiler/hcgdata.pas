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
       symdef,aasm;

    { generates the message tables for a class }
    function genstrmsgtab(_class : pobjectdef) : pasmlabel;
    function genintmsgtab(_class : pobjectdef) : pasmlabel;
    { generates the method name table }
    function genpublishedmethodstable(_class : pobjectdef) : pasmlabel;

    { generates a VMT for _class }
    procedure genvmt(list : paasmoutput;_class : pobjectdef);

{$ifdef WITHDMT}
    { generates a DMT for _class }
    function gendmt(_class : pobjectdef) : pasmlabel;
{$endif WITHDMT}

    function genintftable(_class: pobjectdef): pasmlabel;
    procedure writeinterfaceids(c : pobjectdef);

implementation

    uses
{$ifdef delphi}
       sysutils,
{$else}
       strings,
{$endif}
       cutils,cobjects,
       globtype,globals,verbose,
       symtable,symconst,symtype,symsym,types,
       hcodegen, systems,fmodule
{$ifdef i386}
       ,n386ic
{$endif}
       ;


{*****************************************************************************
                                Message
*****************************************************************************}

    type
       pprocdeftree = ^tprocdeftree;
       tprocdeftree = record
          p   : pprocdef;
          nl  : pasmlabel;
          l,r : pprocdeftree;
       end;

    var
       root : pprocdeftree;
       count : longint;

    procedure insertstr(p : pprocdeftree;var at : pprocdeftree);

      var
         i : longint;

      begin
         if at=nil then
           begin
              at:=p;
              inc(count);
           end
         else
           begin
              i:=strcomp(p^.p^.messageinf.str,at^.p^.messageinf.str);
              if i<0 then
                insertstr(p,at^.l)
              else if i>0 then
                insertstr(p,at^.r)
              else
                Message1(parser_e_duplicate_message_label,strpas(p^.p^.messageinf.str));
           end;
      end;

    procedure disposeprocdeftree(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           disposeprocdeftree(p^.l);
         if assigned(p^.r) then
           disposeprocdeftree(p^.r);
         dispose(p);
      end;

    procedure insertmsgstr(p : pnamedindexobject);

      var
         hp : pprocdef;
         pt : pprocdeftree;

      begin
         if psym(p)^.typ=procsym then
           begin
              hp:=pprocsym(p)^.definition;
              while assigned(hp) do
                begin
                   if (po_msgstr in hp^.procoptions) then
                     begin
                        new(pt);
                        pt^.p:=hp;
                        pt^.l:=nil;
                        pt^.r:=nil;
                        insertstr(pt,root);
                     end;
                   hp:=hp^.nextoverloaded;
                end;
           end;
      end;

    procedure insertint(p : pprocdeftree;var at : pprocdeftree);

      begin
         if at=nil then
           begin
              at:=p;
              inc(count);
           end
         else
           begin
              if p^.p^.messageinf.i<at^.p^.messageinf.i then
                insertint(p,at^.l)
              else if p^.p^.messageinf.i>at^.p^.messageinf.i then
                insertint(p,at^.r)
              else
                Message1(parser_e_duplicate_message_label,tostr(p^.p^.messageinf.i));
           end;
      end;

    procedure insertmsgint(p : pnamedindexobject);

      var
         hp : pprocdef;
         pt : pprocdeftree;

      begin
         if psym(p)^.typ=procsym then
           begin
              hp:=pprocsym(p)^.definition;
              while assigned(hp) do
                begin
                   if (po_msgint in hp^.procoptions) then
                     begin
                        new(pt);
                        pt^.p:=hp;
                        pt^.l:=nil;
                        pt^.r:=nil;
                        insertint(pt,root);
                     end;
                   hp:=hp^.nextoverloaded;
                end;
           end;
      end;

    procedure writenames(p : pprocdeftree);

      begin
         getdatalabel(p^.nl);
         if assigned(p^.l) then
           writenames(p^.l);
         datasegment^.concat(new(pai_label,init(p^.nl)));
         datasegment^.concat(new(pai_const,init_8bit(strlen(p^.p^.messageinf.str))));
         datasegment^.concat(new(pai_string,init_pchar(p^.p^.messageinf.str)));
         if assigned(p^.r) then
           writenames(p^.r);
      end;

    procedure writestrentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writestrentry(p^.l);

         { write name label }
         datasegment^.concat(new(pai_const_symbol,init(p^.nl)));
         datasegment^.concat(new(pai_const_symbol,initname(p^.p^.mangledname)));

         if assigned(p^.r) then
           writestrentry(p^.r);
     end;

    function genstrmsgtab(_class : pobjectdef) : pasmlabel;


      var
         r : pasmlabel;

      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class^.symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}insertmsgstr);

         { write all names }
         if assigned(root) then
           writenames(root);

         { now start writing of the message string table }
         getdatalabel(r);
         datasegment^.concat(new(pai_label,init(r)));
         genstrmsgtab:=r;
         datasegment^.concat(new(pai_const,init_32bit(count)));
         if assigned(root) then
           begin
              writestrentry(root);
              disposeprocdeftree(root);
           end;
      end;


    procedure writeintentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writeintentry(p^.l);

         { write name label }
         datasegment^.concat(new(pai_const,init_32bit(p^.p^.messageinf.i)));
         datasegment^.concat(new(pai_const_symbol,initname(p^.p^.mangledname)));

         if assigned(p^.r) then
           writeintentry(p^.r);
      end;

    function genintmsgtab(_class : pobjectdef) : pasmlabel;

      var
         r : pasmlabel;

      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class^.symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}insertmsgint);

         { now start writing of the message string table }
         getdatalabel(r);
         datasegment^.concat(new(pai_label,init(r)));
         genintmsgtab:=r;
         datasegment^.concat(new(pai_const,init_32bit(count)));
         if assigned(root) then
           begin
              writeintentry(root);
              disposeprocdeftree(root);
           end;
      end;

{$ifdef WITHDMT}

    procedure insertdmtentry(p : pnamedindexobject);

      var
         hp : pprocdef;
         pt : pprocdeftree;

      begin
         if psym(p)^.typ=procsym then
           begin
              hp:=pprocsym(p)^.definition;
              while assigned(hp) do
                begin
                   if (po_msgint in hp^.procoptions) then
                     begin
                        new(pt);
                        pt^.p:=hp;
                        pt^.l:=nil;
                        pt^.r:=nil;
                        insertint(pt,root);
                     end;
                   hp:=hp^.nextoverloaded;
                end;
           end;
      end;

    procedure writedmtindexentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writedmtindexentry(p^.l);
         datasegment^.concat(new(pai_const,init_32bit(p^.p^.messageinf.i)));
         if assigned(p^.r) then
           writedmtindexentry(p^.r);
      end;

    procedure writedmtaddressentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writedmtaddressentry(p^.l);
         datasegment^.concat(new(pai_const_symbol,initname(p^.p^.mangledname)));
         if assigned(p^.r) then
           writedmtaddressentry(p^.r);
      end;

    function gendmt(_class : pobjectdef) : pasmlabel;

      var
         r : pasmlabel;

      begin
         root:=nil;
         count:=0;
         gendmt:=nil;
         { insert all message handlers into a tree, sorted by number }
         _class^.symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}insertdmtentry);

         if count>0 then
           begin
              getdatalabel(r);
              gendmt:=r;
              datasegment^.concat(new(pai_label,init(r)));
              { entries for caching }
              datasegment^.concat(new(pai_const,init_32bit(0)));
              datasegment^.concat(new(pai_const,init_32bit(0)));

              datasegment^.concat(new(pai_const,init_32bit(count)));
              if assigned(root) then
                begin
                   writedmtindexentry(root);
                   writedmtaddressentry(root);
                   disposeprocdeftree(root);
                end;
           end;
      end;

{$endif WITHDMT}

    procedure do_count(p : pnamedindexobject);

      begin
         if (psym(p)^.typ=procsym) and (sp_published in psym(p)^.symoptions) then
           inc(count);
      end;

    procedure genpubmethodtableentry(p : pnamedindexobject);

      var
         hp : pprocdef;
         l : pasmlabel;

      begin
         if (psym(p)^.typ=procsym) and (sp_published in psym(p)^.symoptions) then
           begin
              hp:=pprocsym(p)^.definition;
              if assigned(hp^.nextoverloaded) then
                internalerror(1209992);
              getdatalabel(l);

              consts^.concat(new(pai_label,init(l)));
              consts^.concat(new(pai_const,init_8bit(length(p^.name))));
              consts^.concat(new(pai_string,init(p^.name)));

              datasegment^.concat(new(pai_const_symbol,init(l)));
              datasegment^.concat(new(pai_const_symbol,initname(hp^.mangledname)));
           end;
      end;

    function genpublishedmethodstable(_class : pobjectdef) : pasmlabel;

      var
         l : pasmlabel;

      begin
         count:=0;
         _class^.symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}do_count);
         if count>0 then
           begin
              getdatalabel(l);
              datasegment^.concat(new(pai_label,init(l)));
              datasegment^.concat(new(pai_const,init_32bit(count)));
              _class^.symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}genpubmethodtableentry);
              genpublishedmethodstable:=l;
           end
         else
           genpublishedmethodstable:=nil;
      end;

{*****************************************************************************
                                    VMT
*****************************************************************************}

    type
       pprocdefcoll = ^tprocdefcoll;

       tprocdefcoll = record
          next : pprocdefcoll;
          data : pprocdef;
       end;

       psymcoll = ^tsymcoll;

       tsymcoll = record
          next : psymcoll;
          name : pstring;
          data : pprocdefcoll;
       end;

    var
       wurzel : psymcoll;
       nextvirtnumber : longint;
       _c : pobjectdef;
       has_constructor,has_virtual_method : boolean;

    procedure eachsym(sym : pnamedindexobject);

      var
         procdefcoll : pprocdefcoll;
         hp : pprocdef;
         symcoll : psymcoll;
         _name : string;
         stored : boolean;

      { creates a new entry in the procsym list }
      procedure newentry;

        begin
           { if not, generate a new symbol item }
           new(symcoll);
           symcoll^.name:=stringdup(sym^.name);
           symcoll^.next:=wurzel;
           symcoll^.data:=nil;
           wurzel:=symcoll;
           hp:=pprocsym(sym)^.definition;

           { inserts all definitions }
           while assigned(hp) do
             begin
                new(procdefcoll);
                procdefcoll^.data:=hp;
                procdefcoll^.next:=symcoll^.data;
                symcoll^.data:=procdefcoll;

                { if it's a virtual method }
                if (po_virtualmethod in hp^.procoptions) then
                  begin
                     { then it gets a number ... }
                     hp^.extnumber:=nextvirtnumber;
                     { and we inc the number }
                     inc(nextvirtnumber);
                     has_virtual_method:=true;
                  end;

                if (hp^.proctypeoption=potype_constructor) then
                  has_constructor:=true;

                { check, if a method should be overridden }
                if (po_overridingmethod in hp^.procoptions) then
                  MessagePos1(hp^.fileinfo,parser_e_nothing_to_be_overridden,_c^.objname^+'.'+_name+hp^.demangled_paras);
                { next overloaded method }
                hp:=hp^.nextoverloaded;
             end;
        end;

      procedure newdefentry;

        begin
           new(procdefcoll);
           procdefcoll^.data:=hp;
           procdefcoll^.next:=symcoll^.data;
           symcoll^.data:=procdefcoll;

           { if it's a virtual method }
           if (po_virtualmethod in hp^.procoptions) then
             begin
                { then it gets a number ... }
                hp^.extnumber:=nextvirtnumber;
                { and we inc the number }
                inc(nextvirtnumber);
                has_virtual_method:=true;
             end;

           if (hp^.proctypeoption=potype_constructor) then
             has_constructor:=true;

           { check, if a method should be overridden }
           if (po_overridingmethod in hp^.procoptions) then
             MessagePos1(hp^.fileinfo,parser_e_nothing_to_be_overridden,_c^.objname^+'.'+_name+hp^.demangled_paras);
        end;

      label
         handlenextdef;

      begin
         { put only sub routines into the VMT }
         if psym(sym)^.typ=procsym then
           begin
              _name:=sym^.name;
              symcoll:=wurzel;
              while assigned(symcoll) do
                begin
                   { does the symbol already exist in the list ? }
                   if _name=symcoll^.name^ then
                     begin
                        { walk through all defs of the symbol }
                        hp:=pprocsym(sym)^.definition;
                        while assigned(hp) do
                          begin
                             { compare with all stored definitions }
                             procdefcoll:=symcoll^.data;
                             stored:=false;
                             while assigned(procdefcoll) do
                               begin
                                  { compare parameters }
                                  if equal_paras(procdefcoll^.data^.para,hp^.para,cp_all) and
                                     (
                                       (po_virtualmethod in procdefcoll^.data^.procoptions) or
                                       (po_virtualmethod in hp^.procoptions)
                                     ) then
                                    begin { same parameters }
                                       { wenn sie gleich sind }
                                       { und eine davon virtual deklariert ist }
                                       { Fehler falls nur eine VIRTUAL }
                                       if (po_virtualmethod in procdefcoll^.data^.procoptions)<>
                                          (po_virtualmethod in hp^.procoptions) then
                                         begin
                                            { in classes, we hide the old method }
                                            if is_class(_c) then
                                              begin
                                                 { warn only if it is the first time,
                                                   we hide the method }
                                                 if _c=hp^._class then
                                                   Message1(parser_w_should_use_override,hp^.fullprocname);
                                              end
                                            else
                                              if _c=hp^._class then
                                                begin
                                                   if (po_virtualmethod in procdefcoll^.data^.procoptions) then
                                                     Message1(parser_w_overloaded_are_not_both_virtual,
                                                              hp^.fullprocname)
                                                   else
                                                     Message1(parser_w_overloaded_are_not_both_non_virtual,
                                                              hp^.fullprocname);
                                                end;
                                            { was newentry; exit; (FK) }
                                            newdefentry;
                                            goto handlenextdef;
                                         end
                                       else
                                       { the flags have to match      }
                                       { except abstract and override }
                                       { only if both are virtual !!  }
                                       if (procdefcoll^.data^.proccalloptions<>hp^.proccalloptions) or
                                          (procdefcoll^.data^.proctypeoption<>hp^.proctypeoption) or
                                          ((procdefcoll^.data^.procoptions-
                                              [po_abstractmethod,po_overridingmethod,po_assembler])<>
                                           (hp^.procoptions-[po_abstractmethod,po_overridingmethod,po_assembler])) then
                                         Message1(parser_e_header_dont_match_forward,hp^.fullprocname);

                                       { check, if the overridden directive is set }
                                       { (povirtualmethod is set! }

                                       { class ? }
                                       if is_class(_c) and
                                          not(po_overridingmethod in hp^.procoptions) then
                                         begin
                                            { warn only if it is the first time,
                                              we hide the method }
                                            if _c=hp^._class then
                                              Message1(parser_w_should_use_override,hp^.fullprocname);
                                            { was newentry; (FK) }
                                            newdefentry;
                                            exit;
                                         end;

                                       { error, if the return types aren't equal }
                                       if not(is_equal(procdefcoll^.data^.rettype.def,hp^.rettype.def)) and
                                         not((procdefcoll^.data^.rettype.def^.deftype=objectdef) and
                                           (hp^.rettype.def^.deftype=objectdef) and
                                           is_class(procdefcoll^.data^.rettype.def) and
                                           is_class(hp^.rettype.def) and
                                           (pobjectdef(hp^.rettype.def)^.is_related(
                                               pobjectdef(procdefcoll^.data^.rettype.def)))) then
                                         Message1(parser_e_overloaded_methodes_not_same_ret,hp^.fullprocname);


                                       { now set the number }
                                       hp^.extnumber:=procdefcoll^.data^.extnumber;
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
                                  if (po_virtualmethod in hp^.procoptions) then
                                    begin
                                       { ... it will get a number }
                                       hp^.extnumber:=nextvirtnumber;
                                       inc(nextvirtnumber);
                                    end;
                                  { check, if a method should be overridden }
                                  if (po_overridingmethod in hp^.procoptions) then
                                   MessagePos1(hp^.fileinfo,parser_e_nothing_to_be_overridden,
                                     hp^.fullprocname);
                               end;
                          handlenextdef:
                             hp:=hp^.nextoverloaded;
                          end;
                        exit;
                     end;
                   symcoll:=symcoll^.next;
                end;
             newentry;
           end;
      end;

     procedure disposevmttree;

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

    procedure genvmt(list : paasmoutput;_class : pobjectdef);

      procedure do_genvmt(p : pobjectdef);

        begin
           { start with the base class }
           if assigned(p^.childof) then
             do_genvmt(p^.childof);

           { walk through all public syms }
           { I had to change that to solve bug0260 (PM)}
           { _c:=p; }
           _c:=_class;
           { Florian, please check if you agree (PM)  }
           { no it wasn't correct, but I fixed it at  }
           { another place: your fix hides only a bug }
           { _c is only used to give correct warnings }
           p^.symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}eachsym);
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
            Message1(parser_w_virtual_without_constructor,_class^.objname^);


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
                        if procdefcoll^.data^.extnumber=i then
                          begin
                             if (po_virtualmethod in procdefcoll^.data^.procoptions) then
                               begin
                                  { if a method is abstract, then is also the }
                                  { class abstract and it's not allow to      }
                                  { generates an instance                     }
                                  if (po_abstractmethod in procdefcoll^.data^.procoptions) then
                                    begin
                                       include(_class^.objectoptions,oo_has_abstract);
                                       list^.concat(new(pai_const_symbol,initname('FPC_ABSTRACTERROR')));
                                    end
                                  else
                                    begin
                                      list^.concat(new(pai_const_symbol,
                                        initname(procdefcoll^.data^.mangledname)));
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

    function  gintfgetvtbllabelname(_class: pobjectdef; intfindex: integer): string;
      begin
        gintfgetvtbllabelname:='_$$_'+_class^.objname^+'_$$_'+
          _class^.implementedinterfaces^.interfaces(intfindex)^.objname^+'_$$_VTBL';
      end;

    procedure gintfcreatevtbl(_class: pobjectdef; intfindex: integer; rawdata,rawcode: paasmoutput);
      var
        implintf: pimplementedinterfaces;
        curintf: pobjectdef;
        count: integer;
        tmps: string;
        i: longint;
      begin
        implintf:=_class^.implementedinterfaces;
        curintf:=implintf^.interfaces(intfindex);
        rawdata^.concat(new(pai_symbol,initname(gintfgetvtbllabelname(_class,intfindex),0)));
        count:=implintf^.implproccount(intfindex);
        for i:=1 to count do
          begin
            tmps:=implintf^.implprocs(intfindex,i)^.mangledname+'_$$_'+curintf^.objname^;
            { create wrapper code }
            cgintfwrapper(rawcode,implintf^.implprocs(intfindex,i),tmps,implintf^.ioffsets(intfindex)^);
            { create reference }
            rawdata^.concat(new(pai_const_symbol,initname(tmps)));
          end;
      end;

    procedure gintfgenentry(_class: pobjectdef; intfindex, contintfindex: integer; rawdata: paasmoutput);
      var
        implintf: pimplementedinterfaces;
        curintf: pobjectdef;
        tmplabel: pasmlabel;
        i: longint;
      begin
        implintf:=_class^.implementedinterfaces;
        curintf:=implintf^.interfaces(intfindex);
        { GUID }
        if curintf^.objecttype in [odt_interfacecom] then
          begin
            { label for GUID }
            getdatalabel(tmplabel);
            rawdata^.concat(new(pai_label,init(tmplabel)));
            rawdata^.concat(new(pai_const,init_32bit(curintf^.iidguid.D1)));
            rawdata^.concat(new(pai_const,init_16bit(curintf^.iidguid.D2)));
            rawdata^.concat(new(pai_const,init_16bit(curintf^.iidguid.D3)));
            for i:=Low(curintf^.iidguid.D4) to High(curintf^.iidguid.D4) do
              rawdata^.concat(new(pai_const,init_8bit(curintf^.iidguid.D4[i])));
            datasegment^.concat(new(pai_const_symbol,init(tmplabel)));
          end
        else
          begin
            { nil for Corba interfaces }
            datasegment^.concat(new(pai_const,init_32bit(0))); { nil }
          end;
        { VTable }
        datasegment^.concat(new(pai_const_symbol,initname(gintfgetvtbllabelname(_class,contintfindex))));
        { IOffset field }
        datasegment^.concat(new(pai_const,init_32bit(implintf^.ioffsets(contintfindex)^)));
        { IIDStr }
        getdatalabel(tmplabel);
        rawdata^.concat(new(pai_label,init(tmplabel)));
        rawdata^.concat(new(pai_const,init_8bit(length(curintf^.iidstr^))));
        if curintf^.objecttype=odt_interfacecom then
          rawdata^.concat(new(pai_string,init(upper(curintf^.iidstr^))))
        else
          rawdata^.concat(new(pai_string,init(curintf^.iidstr^)));
        datasegment^.concat(new(pai_const_symbol,init(tmplabel)));
      end;

    type
       tlongintarr = array[0..0] of longint;
       plongintarr = ^tlongintarr;

    procedure gintfoptimizevtbls(_class: pobjectdef; var implvtbl: tlongintarr);
      type
        tcompintfentry = record
          weight: longint;
          compintf: longint;
        end;
        { Max 1000 interface in the class header interfaces it's enough imho }
        tcompintfs = {$ifndef tp} packed {$endif} array[1..1000] of tcompintfentry;
        pcompintfs = ^tcompintfs;
        tequals    = {$ifndef tp} packed {$endif} array[1..1000] of longint;
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
        max:=_class^.implementedinterfaces^.count;
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
                cij:=_class^.implementedinterfaces^.isimplmergepossible(i,j,w);
                cji:=_class^.implementedinterfaces^.isimplmergepossible(j,i,w);
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

    procedure gintfwritedata(_class: pobjectdef);
      var
        rawdata,rawcode: taasmoutput;
        impintfindexes: plongintarr;
        max: longint;
        i: longint;
      begin
        max:=_class^.implementedinterfaces^.count;
        getmem(impintfindexes,(max+1)*sizeof(longint));

        gintfoptimizevtbls(_class,impintfindexes^);

        rawdata.init;
        rawcode.init;
        datasegment^.concat(new(pai_const,init_16bit(max)));
        { Two pass, one for allocation and vtbl creation }
        for i:=1 to max do
          begin
            if impintfindexes^[i]=i then { if implement itself }
              begin
                { allocate a pointer in the object memory }
                with pstoredsymtable(_class^.symtable)^ do
                  begin
                    if (dataalignment>=target_os.size_of_pointer) then
                      datasize:=align(datasize,dataalignment)
                    else
                      datasize:=align(datasize,target_os.size_of_pointer);
                    _class^.implementedinterfaces^.ioffsets(i)^:=datasize;
                    datasize:=datasize+target_os.size_of_pointer;
                  end;
                { write vtbl }
                gintfcreatevtbl(_class,i,@rawdata,@rawcode);
              end;
          end;
        { second pass: for fill interfacetable and remained ioffsets }
        for i:=1 to max do
          begin
            if i<>impintfindexes^[i] then { why execute x:=x ? }
              with _class^.implementedinterfaces^ do ioffsets(i)^:=ioffsets(impintfindexes^[i])^;
            gintfgenentry(_class,i,impintfindexes^[i],@rawdata);
          end;
        datasegment^.insertlist(@rawdata);
        rawdata.done;
        if (cs_create_smart in aktmoduleswitches) then
          rawcode.insert(new(pai_cut,init));
        codesegment^.insertlist(@rawcode);
        rawcode.done;
        freemem(impintfindexes,(max+1)*sizeof(longint));
      end;

    function gintfgetcprocdef(_class: pobjectdef; proc: pprocdef;const name: string): pprocdef;
      var
        sym: pprocsym;
        implprocdef: pprocdef;
      begin
        implprocdef:=nil;
        sym:=pprocsym(search_class_member(_class,name));
        if assigned(sym) and (sym^.typ=procsym) and not (sp_private in sym^.symoptions) then
          begin
            implprocdef:=sym^.definition;
            while assigned(implprocdef) and not equal_paras(proc^.para,implprocdef^.para,cp_none) and
                  (proc^.proccalloptions<>implprocdef^.proccalloptions) do
              implprocdef:=implprocdef^.nextoverloaded;
          end;
        gintfgetcprocdef:=implprocdef;
      end;

    procedure gintfdoonintf(intf, _class: pobjectdef; intfindex: longint);
      var
        i: longint;
        proc: pprocdef;
        procname: string; { for error }
        mappedname: string;
        nextexist: pointer;
        implprocdef: pprocdef;
      begin
        for i:=1 to intf^.symtable^.defindex^.count do
          begin
            proc:=pprocdef(intf^.symtable^.defindex^.search(i));
            if proc^.deftype=procdef then
              begin
                procname:='';
                implprocdef:=nil;
                nextexist:=nil;
                repeat
                  mappedname:=_class^.implementedinterfaces^.getmappings(intfindex,proc^.procsym^.name,nextexist);
                  if procname='' then
                    procname:=mappedname; { for error messages }
                  if mappedname<>'' then
                    implprocdef:=gintfgetcprocdef(_class,proc,mappedname);
                until assigned(implprocdef) or not assigned(nextexist);
                if not assigned(implprocdef) then
                  implprocdef:=gintfgetcprocdef(_class,proc,proc^.procsym^.name);
                if procname='' then
                  procname:=proc^.procsym^.name;
                if assigned(implprocdef) then
                  _class^.implementedinterfaces^.addimplproc(intfindex,implprocdef)
                else
                  Message1(sym_e_id_not_found,procname);
              end;
          end;
      end;

    procedure gintfwalkdowninterface(intf, _class: pobjectdef; intfindex: longint);
      begin
        if assigned(intf^.childof) then
          gintfwalkdowninterface(intf^.childof,_class,intfindex);
        gintfdoonintf(intf,_class,intfindex);
      end;

    function genintftable(_class: pobjectdef): pasmlabel;
      var
        intfindex: longint;
        curintf: pobjectdef;
        intftable: pasmlabel;
      begin
        { 1. step collect implementor functions into the implementedinterfaces^.implprocs }
        for intfindex:=1 to _class^.implementedinterfaces^.count do
          begin
            curintf:=_class^.implementedinterfaces^.interfaces(intfindex);
            gintfwalkdowninterface(curintf,_class,intfindex);
          end;
        { 2. step calc required fieldcount and their offsets in the object memory map
             and write data }
        getdatalabel(intftable);
        datasegment^.concat(new(pai_label,init(intftable)));
        gintfwritedata(_class);
        _class^.implementedinterfaces^.clearimplprocs; { release temporary information }
        genintftable:=intftable;
      end;

  { Write interface identifiers to the data section }
  procedure writeinterfaceids(c : pobjectdef);
    var
      i: longint;
      s1,s2 : string;
    begin
       if c^.owner^.name=nil then
         s1:=''
       else
         s1:=c^.owner^.name^;
       if c^.objname=nil then
         s2:=''
       else
         s2:=upper(c^.objname^);
      s1:=s1+'$_'+s2;
      if c^.isiidguidvalid then
        begin
          if (cs_create_smart in aktmoduleswitches) then
            datasegment^.concat(new(pai_cut,init));
          datasegment^.concat(new(pai_symbol,initname_global('IID$_'+s1,0)));
          datasegment^.concat(new(pai_const,init_32bit(c^.iidguid.D1)));
          datasegment^.concat(new(pai_const,init_16bit(c^.iidguid.D2)));
          datasegment^.concat(new(pai_const,init_16bit(c^.iidguid.D3)));
          for i:=Low(c^.iidguid.D4) to High(c^.iidguid.D4) do
            datasegment^.concat(new(pai_const,init_8bit(c^.iidguid.D4[i])));
        end;
      if (cs_create_smart in aktmoduleswitches) then
        datasegment^.concat(new(pai_cut,init));
      datasegment^.concat(new(pai_symbol,initname_global('IIDSTR$_'+s1,0)));
      datasegment^.concat(new(pai_const,init_8bit(length(c^.iidstr^))));
      datasegment^.concat(new(pai_string,init(c^.iidstr^)));
    end;

end.
{
  $Log$
  Revision 1.11  2000-11-04 17:31:00  florian
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