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
            if pomsgstr in Pprocdef(p)^.options then
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
            if pomsgint in Pprocdef(p)^.options then
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
            if (sp_published in Pprocdef(q)^.objprop) then
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
            if (sp_published in Pprocdef(p)^.objprop) then
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

var wurzel:Pcollection;
    nextvirtnumber : longint;
    _c : pobjectdef;
    has_constructor,has_virtual_method : boolean;

procedure eachsym(sym:Pnamedindexobject);{$ifndef FPC}far;{$endif FPC}

var symcoll:Pcollection;
    _name:string;
    stored:boolean;

    {Creates a new entry in the procsym list.}
    procedure newentry;

        procedure numbervirtual(p:pointer);{$IFDEF TP}far;{$ENDIF TP}

        begin
            { if it's a virtual method }
            if (povirtualmethod in Pprocdef(p)^.options) then
                begin
                    {Then it gets a number ...}
                    Pprocdef(p)^.extnumber:=nextvirtnumber;
                    {And we inc the number }
                    inc(nextvirtnumber);
                    has_virtual_method:=true;
                end;

            if (Pprocdef(p)^.proctype=potype_constructor) then
                has_constructor:=true;

            { check, if a method should be overridden }
            if (pooverridingmethod in Pprocdef(p)^.options) then
                messagepos1(Pprocdef(p)^.fileinfo,parser_e_nothing_to_be_overridden,
                 _c^.objname^+'.'+_name+Pprocdef(p)^.demangled_paras);
        end;

    begin
        symcoll^.insert(sym);
        Pprocsym(sym)^.foreach(@numbervirtual);
    end;

    function match(p:pointer):boolean;{$IFDEF TP}far;{$ENDIF}

    begin
        {Does the symbol already exist in the list ?}
        match:=_name=Psym(p)^.name;
    end;

    procedure eachdef(p:pointer);{$IFDEF TP}far;{$ENDIF}

        function check_override(q:pointer):boolean;{$IFDEF TP}far;{$ENDIF}

        begin
            check_override:=false;
            {Check if the parameters are equal and if one of the methods
             is virtual.}
            if equal_paras(Pprocdef(p)^.parameters,
             Pprocdef(q)^.parameters,false) and
             ((povirtualmethod in Pprocdef(p)^.options) or
              (povirtualmethod in Pprocdef(q)^.options)) then
                begin
                    {Wenn sie gleich sind
                     und eine davon virtual deklariert ist
                     Fehler falls nur eine VIRTUAL }
                    if (povirtualmethod in Pprocdef(p)^.options)<>
                     (povirtualmethod in Pprocdef(q)^.options) then
                        begin
                            { in classes, we hide the old method }
                            if oo_is_class in _c^.options then
                                begin
                                    {Warn only if it is the first time,
                                     we hide the method.}
                                    if _c=Pprocsym(Pprocdef(p)^.sym)^._class then
                                        message1(parser_w_should_use_override,_c^.objname^+'.'+_name);
                                    newentry;
                                    check_override:=true;
                                    exit;
                                end
                            else
                                if _c=Pprocsym(Pprocdef(p)^.sym)^._class then
                                    begin
                                        if (povirtualmethod in Pprocdef(q)^.options) then
                                            message1(parser_w_overloaded_are_not_both_virtual,_c^.objname^+'.'+_name)
                                        else
                                            message1(parser_w_overloaded_are_not_both_non_virtual,
                                             _c^.objname^+'.'+_name);
                                        newentry;
                                        check_override:=true;
                                        exit;
                                    end;
                        end
                    else
                        {The flags have to match except abstract
                         and override, but only if both are virtual!!}
                        if (Pprocdef(q)^.calloptions<>Pprocdef(p)^.calloptions) or
                         (Pprocdef(q)^.proctype<>Pprocdef(p)^.proctype) or
                         ((Pprocdef(q)^.options-[poabstractmethod,pooverridingmethod,poassembler])<>
                         (Pprocdef(p)^.options-[poabstractmethod,pooverridingmethod,poassembler])) then
                            message1(parser_e_header_dont_match_forward,_c^.objname^+'.'+_name);

                    {Check, if the override directive is set
                     (povirtualmethod is set!}

                    {Class ?}
                    if (oo_is_class in _c^.options) and
                     not(pooverridingmethod in Pprocdef(p)^.options) then
                        begin
                            {Warn only if it is the first time,
                             we hide the method.}
                            if _c=Pprocsym(Pprocdef(p)^.sym)^._class then
                                message1(parser_w_should_use_override,_c^.objname^+'.'+_name);
                            newentry;
                            check_override:=true;
                            exit;
                        end;

                    { error, if the return types aren't equal }
                    if not(is_equal(Pprocdef(q)^.retdef,Pprocdef(p)^.retdef)) and
                     not(Pprocdef(q)^.retdef^.is_object(typeof(Tobjectdef)) and
                      Pprocdef(p)^.retdef^.is_object(typeof(Tobjectdef)) and
                      (oo_is_class in Pobjectdef(Pprocdef(q)^.retdef)^.options) and
                      (oo_is_class in Pobjectdef(Pprocdef(p)^.retdef)^.options) and
                      (pobjectdef(Pprocdef(p)^.retdef)^.is_related(
                       pobjectdef(Pprocdef(q)^.retdef)))) then
                        message1(parser_e_overloaded_methodes_not_same_ret,_c^.objname^+'.'+_name);


                    {now set the number }
                    Pprocdef(p)^.extnumber:=Pprocdef(q)^.extnumber;
                end;  { same parameters }
        end;

    begin
        if Pprocsym(sym)^.firstthat(@check_override)=nil then
            newentry;
    end;


begin
    {Put only subroutines into the VMT.}
    if sym^.is_object(typeof(Tprocsym)) then
        begin
            symcoll:=wurzel;
            Pprocsym(symcoll^.firstthat(@match))^.foreach(@eachdef);
            newentry;
        end;
end;

procedure genvmt(list:Paasmoutput;_class:Pobjectdef);

var symcoll:Pcollection;
    i:longint;

    procedure do_genvmt(p:Pobjectdef);

    begin
        {Start with the base class.}
        if assigned(p^.childof) then
            do_genvmt(p^.childof);

        { walk through all public syms }
        { I had to change that to solve bug0260 (PM)}
        _c:=p;
        { Florian, please check if you agree (PM) }
        p^.privatesyms^.foreach({$ifndef TP}@{$endif}eachsym);
        p^.protectedsyms^.foreach({$ifndef TP}@{$endif}eachsym);
        p^.publicsyms^.foreach({$ifndef TP}@{$endif}eachsym);
    end;

 procedure symwritevmt(p:pointer);{$IFDEF TP}far;{$ENDIF}

     procedure defwritevmt(q:pointer);{$IFDEF TP}far;{$ENDIF}

     begin
         { writes the addresses to the VMT }
         { but only this which are declared as virtual }
         if (Pprocdef(q)^.extnumber=i) and
          (povirtualmethod in Pprocdef(q)^.options) then
             begin
                 { if a method is abstract, then is also the }
                 { class abstract and it's not allow to      }
                 { generates an instance                     }
                 if (poabstractmethod in Pprocdef(q)^.options) then
                     begin
                         include(_class^.options,oo_has_abstract);
                         list^.concat(new(pai_const_symbol,initname('FPC_ABSTRACTERROR')));
                     end
                 else
                     begin
                         list^.concat(new(pai_const_symbol,
                          initname(Pprocdef(q)^.mangledname)));
                     end;
             end;
     end;

 begin
     Pprocsym(p)^.foreach(@defwritevmt);
 end;

begin
    new(wurzel,init(64,16));
    nextvirtnumber:=0;

    has_constructor:=false;
    has_virtual_method:=false;

    { generates a tree of all used methods }
    do_genvmt(_class);

    if has_virtual_method and not(has_constructor) then
        message1(parser_w_virtual_without_constructor,_class^.objname^);


    { generates the VMT }

    { walk trough all numbers for virtual methods and search }
    { the method                                             }
    for i:=0 to nextvirtnumber-1 do
        begin
            symcoll:=wurzel;
            symcoll^.foreach(@symwritevmt);
        end;
    dispose(symcoll,done);
end;


end.
{
  $Log$
  Revision 1.1  2000-03-11 21:11:25  daniel
    * Ported hcgdata to new symtable.
    * Alignment code changed as suggested by Peter
    + Usage of my is operator replacement, is_object

}
