{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Routines for the code generation of data structures
    like VMT,Messages

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
interface

    uses
       symtable,aasm;

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

implementation

    uses
       strings,cobjects,
       globtype,globals,verbose,
       symconst,types,
       hcodegen;


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

    procedure insertmsgstr(p : pnamedindexobject);{$ifndef FPC}far;{$endif FPC}

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

    procedure insertmsgint(p : pnamedindexobject);{$ifndef FPC}far;{$endif FPC}

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
         _class^.symtable^.foreach({$ifndef TP}@{$endif}insertmsgstr);

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
         _class^.symtable^.foreach({$ifndef TP}@{$endif}insertmsgint);

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

    procedure insertdmtentry(p : pnamedindexobject);{$ifndef FPC}far;{$endif FPC}

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
         _class^.symtable^.foreach({$ifndef TP}@{$endif}insertdmtentry);

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

    procedure do_count(p : pnamedindexobject);{$ifndef FPC}far;{$endif FPC}

      begin
         if (psym(p)^.typ=procsym) and (sp_published in psym(p)^.symoptions) then
           inc(count);
      end;

    procedure genpubmethodtableentry(p : pnamedindexobject);{$ifndef FPC}far;{$endif FPC}

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
         _class^.symtable^.foreach({$ifndef TP}@{$endif}do_count);
         if count>0 then
           begin
              getdatalabel(l);
              datasegment^.concat(new(pai_label,init(l)));
              datasegment^.concat(new(pai_const,init_32bit(count)));
              _class^.symtable^.foreach({$ifndef TP}@{$endif}genpubmethodtableentry);
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

    procedure eachsym(sym : pnamedindexobject);{$ifndef FPC}far;{$endif FPC}

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
                                            if _c^.is_class then
                                              begin
                                                 { warn only if it is the first time,
                                                   we hide the method }
                                                 if _c=hp^._class then
                                                   Message1(parser_w_should_use_override,_c^.objname^+'.'+_name);
                                              end
                                            else
                                              if _c=hp^._class then
                                                begin
                                                   if (po_virtualmethod in procdefcoll^.data^.procoptions) then
                                                     Message1(parser_w_overloaded_are_not_both_virtual,_c^.objname^+'.'+_name)
                                                   else
                                                     Message1(parser_w_overloaded_are_not_both_non_virtual,
                                                       _c^.objname^+'.'+_name);
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
                                         Message1(parser_e_header_dont_match_forward,_c^.objname^+'.'+_name);

                                       { check, if the overridden directive is set }
                                       { (povirtualmethod is set! }

                                       { class ? }
                                       if _c^.is_class and
                                          not(po_overridingmethod in hp^.procoptions) then
                                         begin
                                            { warn only if it is the first time,
                                              we hide the method }
                                            if _c=hp^._class then
                                              Message1(parser_w_should_use_override,_c^.objname^+'.'+_name);
                                            { was newentry; (FK) }
                                            newdefentry;
                                            exit;
                                         end;

                                       { error, if the return types aren't equal }
                                       if not(is_equal(procdefcoll^.data^.rettype.def,hp^.rettype.def)) and
                                         not((procdefcoll^.data^.rettype.def^.deftype=objectdef) and
                                           (hp^.rettype.def^.deftype=objectdef) and
                                           (pobjectdef(procdefcoll^.data^.rettype.def)^.is_class) and
                                           (pobjectdef(hp^.rettype.def)^.is_class) and
                                           (pobjectdef(hp^.rettype.def)^.is_related(
                                               pobjectdef(procdefcoll^.data^.rettype.def)))) then
                                         Message1(parser_e_overloaded_methodes_not_same_ret,_c^.objname^+'.'+_name);


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
                                     _c^.objname^+'.'+_name+hp^.demangled_paras);
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
           p^.symtable^.foreach({$ifndef TP}@{$endif}eachsym);
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


end.
{
  $Log$
  Revision 1.1  2000-07-13 06:29:51  michael
  + Initial import

  Revision 1.30  2000/06/30 22:11:28  peter
    * fixed some getlabel to getdatalabel

  Revision 1.29  2000/06/20 12:47:52  pierre
    * equal_paras and convertable_paras changed by transforming third parameter
      into an enum with three possible values:
      cp_none, cp_value_equal_const and cp_all.

  Revision 1.28  2000/05/11 06:55:28  florian
    * fixed some vmt problems, especially related to overloaded methods
      in objects/classes

  Revision 1.27  2000/04/29 12:49:30  peter
    * fixed long line for tp7

  Revision 1.26  2000/03/06 15:57:42  peter
    * better error pos for overridden error

  Revision 1.25  2000/02/09 13:22:53  peter
    * log truncated

  Revision 1.24  2000/01/28 23:17:53  florian
    * virtual XXXX; support for objects, only if -dWITHDMT is defined

  Revision 1.23  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.22  1999/12/02 19:22:16  peter
    * write also parameters for override info

  Revision 1.21  1999/12/01 12:42:32  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.20  1999/11/30 10:40:43  peter
    + ttype, tsymlist

  Revision 1.19  1999/11/29 23:42:49  pierre
   * fix for form bug 555

  Revision 1.18  1999/10/26 12:30:41  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.17  1999/09/13 16:23:42  peter
    * remvoed unused var

  Revision 1.16  1999/09/12 14:50:50  florian
    + implemented creation of methodname/address tables

  Revision 1.15  1999/09/01 13:44:56  florian
    * fixed writing of class rtti: vmt offset were written wrong

  Revision 1.14  1999/08/03 22:02:52  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}