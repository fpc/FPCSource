{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

    This unit handles the symbol tables

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ****************************************************************************
}
unit symtype;

{$i defines.inc}

interface

    uses
      { common }
      cutils,cobjects,
      { global }
      globtype,globals,
      { symtable }
      symconst,symbase,
      { aasm }
      aasm
      ;

    type
{************************************************
                Required Forwards
************************************************}

      psym = ^tsym;

{************************************************
                     TRef
************************************************}

      pref = ^tref;
      tref = object
        nextref     : pref;
        posinfo     : tfileposinfo;
        moduleindex : word;
        is_written  : boolean;
        constructor init(ref:pref;pos:pfileposinfo);
        procedure   freechain;
        destructor  done; virtual;
      end;

{************************************************
                     TDef
************************************************}

      tgetsymtable = (gs_none,gs_record,gs_local,gs_para);

      pdef = ^tdef;
      tdef = object(tdefentry)
         typesym    : psym;  { which type the definition was generated this def }
         constructor init;
         procedure deref;virtual;
         function  typename:string;
         function  gettypename:string;virtual;
         function  size:longint;virtual;abstract;
         function  alignment:longint;virtual;abstract;
         function  getsymtable(t:tgetsymtable):psymtable;virtual;
         function  is_publishable:boolean;virtual;abstract;
         function  needs_inittable:boolean;virtual;abstract;
         function  get_rtti_label : string;virtual;abstract;
      end;

{************************************************
                     TSym
************************************************}

      { this object is the base for all symbol objects }
      tsym = object(tsymentry)
         _realname  : pstring;
         fileinfo   : tfileposinfo;
         symoptions : tsymoptions;
         constructor init(const n : string);
         destructor done;virtual;
         function  realname:string;
         procedure prederef;virtual; { needed for ttypesym to be deref'd first }
         procedure deref;virtual;
         function  gettypedef:pdef;virtual;
         function  mangledname : string;virtual;abstract;
      end;

{************************************************
                   TType
************************************************}

      ttype = object
        def : pdef;
        sym : psym;
        procedure reset;
        procedure setdef(p:pdef);
        procedure setsym(p:psym);
        procedure load;
        procedure write;
        procedure resolve;
      end;

{************************************************
                   TSymList
************************************************}

      psymlistitem = ^tsymlistitem;
      tsymlistitem = record
        sym  : psym;
        next : psymlistitem;
      end;

      psymlist = ^tsymlist;
      tsymlist = object
        def      : pdef;
        firstsym,
        lastsym  : psymlistitem;
        constructor init;
        constructor load;
        destructor  done;
        function  empty:boolean;
        procedure setdef(p:pdef);
        procedure addsym(p:psym);
        procedure clear;
        function  getcopy:psymlist;
        procedure resolve;
        procedure write;
      end;


    { resolving }
    procedure resolvesym(var sym:psym);
    procedure resolvedef(var def:pdef);


implementation

    uses
       verbose,
       ppu,symppu,
       finput,fmodule;

{****************************************************************************
                                Tdef
****************************************************************************}

    constructor tdef.init;
      begin
         inherited init;
         deftype:=abstractdef;
         owner := nil;
         typesym := nil;
      end;


    function tdef.typename:string;
      begin
        if assigned(typesym) and
           not(deftype=procvardef) and
           assigned(typesym^._realname) and
           (typesym^._realname^[1]<>'$') then
         typename:=typesym^._realname^
        else
         typename:=gettypename;
      end;


    function tdef.gettypename : string;
      begin
         gettypename:='<unknown type>'
      end;


    procedure tdef.deref;
      begin
        resolvesym(typesym);
      end;


    function tdef.getsymtable(t:tgetsymtable):psymtable;
      begin
        getsymtable:=nil;
      end;


{****************************************************************************
                          TSYM (base for all symtypes)
****************************************************************************}

    constructor tsym.init(const n : string);
      begin
         if n[1]='$' then
          inherited initname(copy(n,2,255))
         else
          inherited initname(upper(n));
         _realname:=stringdup(n);
         typ:=abstractsym;
      end;


    destructor tsym.done;
      begin
        stringdispose(_realname);
        inherited done;
      end;


    procedure tsym.prederef;
      begin
      end;


    procedure tsym.deref;
      begin
      end;

    function tsym.realname : string;
      begin
        if assigned(_realname) then
         realname:=_realname^
        else
         realname:=name;
      end;


    function tsym.gettypedef:pdef;
      begin
        gettypedef:=nil;
      end;


{****************************************************************************
                               TRef
****************************************************************************}

    constructor tref.init(ref :pref;pos : pfileposinfo);
      begin
        nextref:=nil;
        if pos<>nil then
          posinfo:=pos^;
        if assigned(current_module) then
          moduleindex:=current_module^.unit_index;
        if assigned(ref) then
          ref^.nextref:=@self;
        is_written:=false;
      end;

    procedure tref.freechain;
      var
        p,q : pref;
      begin
        p:=nextref;
        nextref:=nil;
        while assigned(p) do
          begin
            q:=p^.nextref;
            dispose(p,done);
            p:=q;
          end;
      end;

    destructor tref.done;
      var
         inputfile : pinputfile;
      begin
         inputfile:=get_source_file(moduleindex,posinfo.fileindex);
         if inputfile<>nil then
           dec(inputfile^.ref_count);
         nextref:=nil;
      end;


{****************************************************************************
                                   TType
****************************************************************************}

    procedure ttype.reset;
      begin
        def:=nil;
        sym:=nil;
      end;


    procedure ttype.setdef(p:pdef);
      begin
        def:=p;
        sym:=nil;
      end;


    procedure ttype.setsym(p:psym);
      begin
        sym:=p;
        def:=p^.gettypedef;
        if not assigned(def) then
         internalerror(1234005);
      end;


    procedure ttype.load;
      begin
        def:=pdef(readderef);
        sym:=psym(readderef);
      end;


    procedure ttype.write;
      begin
        if assigned(sym) then
         begin
           writederef(nil);
           writederef(sym);
         end
        else
         begin
           writederef(def);
           writederef(nil);
         end;
      end;


    procedure ttype.resolve;
      begin
        if assigned(sym) then
         begin
           resolvesym(sym);
           setsym(sym);
         end
        else
         resolvedef(def);
      end;

{****************************************************************************
                                 TSymList
****************************************************************************}

    constructor tsymlist.init;
      begin
        def:=nil; { needed for procedures }
        firstsym:=nil;
        lastsym:=nil;
      end;


    constructor tsymlist.load;
      var
        sym : psym;
      begin
        def:=pdef(readderef);
        firstsym:=nil;
        lastsym:=nil;
        repeat
          sym:=psym(readderef);
          if sym=nil then
           break;
          addsym(sym);
        until false;
      end;


    destructor tsymlist.done;
      begin
        clear;
      end;


    function tsymlist.empty:boolean;
      begin
        empty:=(firstsym=nil);
      end;


    procedure tsymlist.clear;
      var
        hp : psymlistitem;
      begin
        while assigned(firstsym) do
         begin
           hp:=firstsym;
           firstsym:=firstsym^.next;
           dispose(hp);
         end;
        firstsym:=nil;
        lastsym:=nil;
        def:=nil;
      end;


    procedure tsymlist.setdef(p:pdef);
      begin
        def:=p;
      end;


    procedure tsymlist.addsym(p:psym);
      var
        hp : psymlistitem;
      begin
        if not assigned(p) then
         exit;
        new(hp);
        hp^.sym:=p;
        hp^.next:=nil;
        if assigned(lastsym) then
         lastsym^.next:=hp
        else
         firstsym:=hp;
        lastsym:=hp;
      end;


    function tsymlist.getcopy:psymlist;
      var
        hp  : psymlist;
        hp2 : psymlistitem;
      begin
        new(hp,init);
        hp^.def:=def;
        hp2:=firstsym;
        while assigned(hp2) do
         begin
           hp^.addsym(hp2^.sym);
           hp2:=hp2^.next;
         end;
        getcopy:=hp;
      end;


    procedure tsymlist.write;
      var
        hp : psymlistitem;
      begin
        writederef(def);
        hp:=firstsym;
        while assigned(hp) do
         begin
           writederef(hp^.sym);
           hp:=hp^.next;
         end;
        writederef(nil);
      end;


    procedure tsymlist.resolve;
      var
        hp : psymlistitem;
      begin
        resolvedef(def);
        hp:=firstsym;
        while assigned(hp) do
         begin
           resolvesym(hp^.sym);
           hp:=hp^.next;
         end;
      end;


{*****************************************************************************
                        Symbol / Definition Resolving
*****************************************************************************}

    procedure resolvederef(var p:pderef;var st:psymtable;var idx:word);
      var
        hp : pderef;
        pd : pdef;
      begin
        st:=nil;
        idx:=0;
        while assigned(p) do
         begin
           case p^.dereftype of
             derefaktrecordindex :
               begin
                 st:=aktrecordsymtable;
                 idx:=p^.index;
               end;
             derefaktstaticindex :
               begin
                 st:=aktstaticsymtable;
                 idx:=p^.index;
               end;
             derefaktlocal :
               begin
                 st:=aktlocalsymtable;
                 idx:=p^.index;
               end;
             derefunit :
               begin
{$ifdef NEWMAP}
                 st:=psymtable(current_module^.map^[p^.index]^.globalsymtable);
{$else NEWMAP}
                 st:=psymtable(current_module^.map^[p^.index]);
{$endif NEWMAP}
               end;
             derefrecord :
               begin
                 pd:=pdef(st^.getdefnr(p^.index));
                 st:=pd^.getsymtable(gs_record);
                 if not assigned(st) then
                  internalerror(556658);
               end;
             dereflocal :
               begin
                 pd:=pdef(st^.getdefnr(p^.index));
                 st:=pd^.getsymtable(gs_local);
                 if not assigned(st) then
                  internalerror(556658);
               end;
             derefpara :
               begin
                 pd:=pdef(st^.getdefnr(p^.index));
                 st:=pd^.getsymtable(gs_para);
                 if not assigned(st) then
                  internalerror(556658);
               end;
             derefindex :
               begin
                 idx:=p^.index;
               end;
             else
               internalerror(556658);
           end;
           hp:=p;
           p:=p^.next;
           dispose(hp,done);
         end;
      end;


    procedure resolvedef(var def:pdef);
      var
        st   : psymtable;
        idx  : word;
      begin
        resolvederef(pderef(def),st,idx);
        if assigned(st) then
         def:=pdef(st^.getdefnr(idx))
        else
         def:=nil;
      end;


    procedure resolvesym(var sym:psym);
      var
        st   : psymtable;
        idx  : word;
      begin
        resolvederef(pderef(sym),st,idx);
        if assigned(st) then
         sym:=psym(st^.getsymnr(idx))
        else
         sym:=nil;
      end;







end.
{
  $Log$
  Revision 1.1  2000-10-31 22:02:53  peter
    * symtable splitted, no real code changes

}