{
    $Id$
    Copyright (c) 1998-2000 by Daniel Mantione,
     and other members of the Free Pascal development team

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
       symtable,aasm,defs;

    { generates the message tables for a class }
    function genstrmsgtab(_class : pobjectdef) : pasmlabel;
    function genintmsgtab(_class : pobjectdef) : pasmlabel;
    { generates the method name table }
    function genpublishedmethodstable(Aclass:Pobjectdef):Pasmlabel;

    { generates a VMT for _class }
    procedure genvmt(list : paasmoutput;_class : pobjectdef);

{$ifdef WITHDMT}
    { generates a DMT for _class }
    function gendmt(_class : pobjectdef) : pasmlabel;
{$endif WITHDMT}

implementation

    uses
       strings,cobjects,globtype,globals,verbose,
       types,hcodegen,symbols,objects,xobjects;


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

    procedure insertmsgstr(p:pnamedindexobject);{$ifndef FPC}far;{$endif FPC}

        procedure inserter(p:pointer);{$IFDEF TP}far;{$ENDIF}

        var pt:Pprocdeftree;

        begin
            if po_msgstr in Pprocdef(p)^.options then
                begin
                    new(pt);
                    pt^.p:=p;
                    pt^.l:=nil;
                    pt^.r:=nil;
                    insertstr(pt,root);
                end;
        end;

    begin
        if typeof(p^)=typeof(Tprocsym) then
            Pprocsym(p)^.foreach(@inserter);
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

    procedure insertmsgint(p:pnamedindexobject);{$ifndef FPC}far;{$endif FPC}

        procedure inserter(p:pointer);{$IFDEF TP}far;{$ENDIF}

        var pt:Pprocdeftree;

        begin
            if po_msgint in Pprocdef(p)^.options then
                begin
                    new(pt);
                    pt^.p:=p;
                    pt^.l:=nil;
                    pt^.r:=nil;
                    insertint(pt,root);
                end;
        end;

    begin
        if typeof(p^)=typeof(Tprocsym) then
            Pprocsym(p)^.foreach(@inserter);
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
         if _class^.privatesyms<>nil then
            _class^.privatesyms^.foreach({$ifndef TP}@{$endif}insertmsgstr);
         if _class^.privatesyms<>nil then
            _class^.protectedsyms^.foreach({$ifndef TP}@{$endif}insertmsgstr);
         if _class^.privatesyms<>nil then
            _class^.publicsyms^.foreach({$ifndef TP}@{$endif}insertmsgstr);

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
         if _class^.privatesyms<>nil then
            _class^.privatesyms^.foreach({$ifndef TP}@{$endif}insertmsgint);
         if _class^.privatesyms<>nil then
            _class^.protectedsyms^.foreach({$ifndef TP}@{$endif}insertmsgint);
         if _class^.privatesyms<>nil then
            _class^.publicsyms^.foreach({$ifndef TP}@{$endif}insertmsgint);

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

    procedure genpubmethodtableentry(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

        procedure do_concat(q:pointer);{$ifndef FPC}far;{$endif}

        var l:Pasmlabel;

        begin
            if (sp_published in Pprocsym(p)^.objprop) then
                begin
                   getlabel(l);

                   consts^.concat(new(pai_label,init(l)));
                   consts^.concat(new(pai_const,init_8bit(length(p^.name))));
                   consts^.concat(new(pai_string,init(p^.name)));

                   datasegment^.concat(new(pai_const_symbol,init(l)));
                   datasegment^.concat(new(pai_const_symbol,initname(Pprocdef(q)^.mangledname)));
                end;
        end;

    begin
        if p^.is_object(typeof(Tprocsym)) then
            Pprocsym(p)^.foreach(@do_concat);
    end;

    procedure sym_do_count(p:Pnamedindexobject);{$ifndef FPC}far;{$endif}

        procedure def_do_count(p:pointer);{$ifndef FPC}far;{$endif}

        begin
            if (sp_published in Pprocsym(p)^.objprop) then
             inc(count);
        end;

    begin
        if Pobject(p)^.is_object(typeof(Tprocsym)) then
            Pprocsym(p)^.foreach(@def_do_count);
    end;

    function genpublishedmethodstable(Aclass:Pobjectdef):Pasmlabel;

    var l:Pasmlabel;

    begin
        count:=0;
        if Aclass^.privatesyms<>nil then
            Aclass^.privatesyms^.foreach({$ifndef TP}@{$endif}sym_do_count);
        if Aclass^.protectedsyms<>nil then
            Aclass^.publicsyms^.foreach({$ifndef TP}@{$endif}sym_do_count);
        if Aclass^.publicsyms<>nil then
            Aclass^.publicsyms^.foreach({$ifndef TP}@{$endif}sym_do_count);
        if count>0 then
            begin
                getlabel(l);
                datasegment^.concat(new(pai_label,init(l)));
                datasegment^.concat(new(pai_const,init_32bit(count)));
                if Aclass^.privatesyms<>nil then
                    Aclass^.privatesyms^.foreach({$ifndef TP}@{$endif}genpubmethodtableentry);
                if Aclass^.protectedsyms<>nil then
                    Aclass^.protectedsyms^.foreach({$ifndef TP}@{$endif}genpubmethodtableentry);
                if Aclass^.publicsyms<>nil then
                    Aclass^.publicsyms^.foreach({$ifndef TP}@{$endif}genpubmethodtableentry);
                genpublishedmethodstable:=l;
            end
        else
            genpublishedmethodstable:=nil;
    end;

{*****************************************************************************
                                    VMT
*****************************************************************************}


procedure genvmt(list:Paasmoutput;_class:Pobjectdef);

var i:longint;

begin
    for i:=0 to _class^.vmt_layout^.count-1 do
        list^.concat(new(pai_const_symbol,
         initname(Pvmtentry(_class^.vmt_layout^.at(i))^.mangledname)));
end;


end.
{
  $Log$
  Revision 1.1  2000-07-13 06:30:13  michael
  + Initial import

  Revision 1.2  2000/03/16 12:52:48  daniel
    *  Changed names of procedures flags
    *  Changed VMT generation

  Revision 1.1  2000/03/11 21:11:25  daniel
    * Ported hcgdata to new symtable.
    * Alignment code changed as suggested by Peter
    + Usage of my is operator replacement, is_object

}
