{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

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
    function genstrmsgtab(_class : pobjectdef) : plabel;
    function genintmsgtab(_class : pobjectdef) : plabel;

    { generates a VMT for _class }
    procedure genvmt(_class : pobjectdef);


implementation

    uses
       strings,cobjects,
       globtype,globals,verbose,
       types,
       hcodegen;


{*****************************************************************************
                                Message
*****************************************************************************}

    type
       pprocdeftree = ^tprocdeftree;
       tprocdeftree = record
          p   : pprocdef;
          nl  : plabel;
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
                   if (hp^.options and pomsgstr)<>0 then
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
              if p^.p^.messageinf.i<at^.p^.messageinf.i then
                insertstr(p,at^.l)
              else if p^.p^.messageinf.i>at^.p^.messageinf.i then
                insertstr(p,at^.r)
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
                   if (hp^.options and pomsgint)<>0 then
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
         datasegment^.concat(new(pai_const_symbol,initname(lab2str(p^.nl))));
         datasegment^.concat(new(pai_const_symbol,initname(p^.p^.mangledname)));
{$ifndef NEWLAB}
         maybe_concat_external(p^.p^.owner,p^.p^.mangledname);
{$endif}

         if assigned(p^.r) then
           writestrentry(p^.r);
      end;

    function genstrmsgtab(_class : pobjectdef) : plabel;


      var
         r : plabel;

      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class^.publicsyms^.foreach(insertmsgstr);

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
{$ifndef NEWLAB}
         maybe_concat_external(p^.p^.owner,p^.p^.mangledname);
{$endif}

         if assigned(p^.r) then
           writeintentry(p^.r);
      end;

    function genintmsgtab(_class : pobjectdef) : plabel;

      var
         r : plabel;

      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class^.publicsyms^.foreach(insertmsgint);

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
                if (hp^.options and povirtualmethod)<>0 then
                  begin
                     { then it gets a number ... }
                     hp^.extnumber:=nextvirtnumber;
                     { and we inc the number }
                     inc(nextvirtnumber);
                     has_virtual_method:=true;
                  end;

                if (hp^.options and poconstructor)<>0 then
                  has_constructor:=true;

                { check, if a method should be overridden }
                if (hp^.options and pooverridingmethod)<>0 then
                  Message1(parser_e_nothing_to_be_overridden,_c^.objname^+'.'+_name);
                { next overloaded method }
                hp:=hp^.nextoverloaded;
             end;
        end;

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
                                  if equal_paras(procdefcoll^.data^.para1,hp^.para1,false) and
                                     (
                                       ((procdefcoll^.data^.options and povirtualmethod)<>0) or
                                       ((hp^.options and povirtualmethod)<>0)
                                     ) then
                                    begin
                                       { wenn sie gleich sind }
                                       { und eine davon virtual deklariert ist }
                                       { Fehler falls nur eine VIRTUAL }
                                       if (procdefcoll^.data^.options and povirtualmethod)<>
                                          (hp^.options and povirtualmethod) then
                                         begin
                                            { in classes, we hide the old method }
                                            if _c^.isclass then
                                              begin
                                                 { warn only if it is the first time,
                                                   we hide the method }
                                                 if _c=hp^._class then
                                                   Message1(parser_w_should_use_override,_c^.objname^+'.'+_name);
                                                 newentry;
                                                 exit;
                                              end
                                            else
                                              if _c=hp^._class then
                                                begin
                                                   if (procdefcoll^.data^.options and povirtualmethod)<>0 then
                                                     Message1(parser_w_overloaded_are_not_both_virtual,_c^.objname^+'.'+_name)
                                                   else
                                                     Message1(parser_w_overloaded_are_not_both_non_virtual,
                                                       _c^.objname^+'.'+_name);
                                                   newentry;
                                                   exit;
                                                end;
                                         end;

                                       { check, if the overridden directive is set }
                                       { (povirtualmethod is set! }

                                       { class ? }
                                       if _c^.isclass and
                                         ((hp^.options and pooverridingmethod)=0) then
                                         begin
                                            { warn only if it is the first time,
                                              we hide the method }
                                            if _c=hp^._class then
                                              Message1(parser_w_should_use_override,_c^.objname^+'.'+_name);
                                            newentry;
                                            exit;
                                         end;

                                       { error, if the return types aren't equal }
                                       if not(is_equal(procdefcoll^.data^.retdef,hp^.retdef)) and
                                         not((procdefcoll^.data^.retdef^.deftype=objectdef) and
                                           (hp^.retdef^.deftype=objectdef) and
                                           (pobjectdef(procdefcoll^.data^.retdef)^.isclass) and
                                           (pobjectdef(hp^.retdef)^.isclass) and
                                           (pobjectdef(hp^.retdef)^.isrelated(pobjectdef(procdefcoll^.data^.retdef)))) then
                                         Message1(parser_e_overloaded_methodes_not_same_ret,_c^.objname^+'.'+_name);


                                       { the flags have to match      }
                                       { except abstract and override }
                                       if (procdefcoll^.data^.options and not(poabstractmethod or pooverridingmethod))<>
                                         (hp^.options and not(poabstractmethod or pooverridingmethod)) then
                                            Message1(parser_e_header_dont_match_forward,_c^.objname^+'.'+_name);

                                       { now set the number }
                                       hp^.extnumber:=procdefcoll^.data^.extnumber;
                                       { and exchange }
                                       procdefcoll^.data:=hp;
                                       stored:=true;
                                    end;
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
                                  if (hp^.options and povirtualmethod)<>0 then
                                    begin
                                       { ... it will get a number }
                                       hp^.extnumber:=nextvirtnumber;
                                       inc(nextvirtnumber);
                                    end;
                                  { check, if a method should be overridden }
                                  if (hp^.options and pooverridingmethod)<>0 then
                                   Message1(parser_e_nothing_to_be_overridden,_c^.objname^+'.'+_name);
                               end;
                             hp:=hp^.nextoverloaded;
                          end;
                        exit;
                     end;
                   symcoll:=symcoll^.next;
                end;
             newentry;
           end;
      end;

    procedure genvmt(_class : pobjectdef);

      procedure do_genvmt(p : pobjectdef);

        begin
           { start with the base class }
           if assigned(p^.childof) then
             do_genvmt(p^.childof);

           { walk through all public syms }
           _c:=_class;
{$ifdef tp}
           p^.publicsyms^.foreach(eachsym);
{$else}
           p^.publicsyms^.foreach(@eachsym);
{$endif}
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
                             if (procdefcoll^.data^.options and povirtualmethod)<>0 then
                               begin
                                  { if a method is abstract, then is also the }
                                  { class abstract and it's not allow to      }
                                  { generates an instance                     }
                                  if (procdefcoll^.data^.options and poabstractmethod)<>0 then
                                    begin
                                       _class^.options:=_class^.options or oo_is_abstract;
                                       datasegment^.concat(new(pai_const_symbol,
                                         initname('FPC_ABSTRACTERROR')));
                                    end
                                  else
                                    begin
                                      datasegment^.concat(new(pai_const_symbol,
                                        initname(procdefcoll^.data^.mangledname)));
{$ifndef NEWLAB}
                                      maybe_concat_external(procdefcoll^.data^.owner,
                                        procdefcoll^.data^.mangledname);
{$endif}
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
  Revision 1.6  1999-05-21 13:55:00  peter
    * NEWLAB for label as symbol

  Revision 1.5  1999/05/17 21:57:07  florian
    * new temporary ansistring handling

  Revision 1.4  1999/05/13 21:59:27  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.3  1999/04/26 13:31:34  peter
    * release storenumber,double_checksum

  Revision 1.2  1999/04/21 09:43:37  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.1  1999/03/24 23:17:00  peter
    * fixed bugs 212,222,225,227,229,231,233

}
