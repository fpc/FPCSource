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
      cutils,
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

      tsym = class;

{************************************************
                     TRef
************************************************}

      tref = class
        nextref     : tref;
        posinfo     : tfileposinfo;
        moduleindex : longint;
        is_written  : boolean;
        constructor create(ref:tref;pos:pfileposinfo);
        procedure   freechain;
        destructor  destroy;override;
      end;

{************************************************
                     TDef
************************************************}

      tgetsymtable = (gs_none,gs_record,gs_local,gs_para);

      tdef = class(tdefentry)
         typesym    : tsym;  { which type the definition was generated this def }
         constructor create;
         procedure deref;virtual;
         procedure derefimpl;virtual;
         function  typename:string;
         function  gettypename:string;virtual;
         function  size:longint;virtual;abstract;
         function  alignment:longint;virtual;abstract;
         function  getsymtable(t:tgetsymtable):tsymtable;virtual;
         function  is_publishable:boolean;virtual;abstract;
         function  needs_inittable:boolean;virtual;abstract;
         function  get_rtti_label : string;virtual;abstract;
      end;

{************************************************
                     TSym
************************************************}

      { this object is the base for all symbol objects }
      tsym = class(tsymentry)
         _realname  : pstring;
         fileinfo   : tfileposinfo;
         symoptions : tsymoptions;
         constructor create(const n : string);
         destructor destroy;override;
         function  realname:string;
         procedure deref;virtual;
         function  gettypedef:tdef;virtual;
         function  mangledname : string;virtual;abstract;
      end;

{************************************************
                   TType
************************************************}

      ttype = object
        def : tdef;
        sym : tsym;
        procedure reset;
        procedure setdef(p:tdef);
        procedure setsym(p:tsym);
        procedure resolve;
      end;

{************************************************
                   TSymList
************************************************}

      psymlistitem = ^tsymlistitem;
      tsymlistitem = record
        sym  : tsym;
        next : psymlistitem;
      end;

      tsymlist = class
        def      : tdef;
        firstsym,
        lastsym  : psymlistitem;
        constructor create;
        destructor  destroy;override;
        function  empty:boolean;
        procedure setdef(p:tdef);
        procedure addsym(p:tsym);
        procedure clear;
        function  getcopy:tsymlist;
        procedure resolve;
      end;


    { resolving }
    procedure resolvesym(var sym:tsym);
    procedure resolvedef(var def:tdef);


implementation

    uses
       verbose,
       fmodule;

{****************************************************************************
                                Tdef
****************************************************************************}

    constructor tdef.create;
      begin
         inherited create;
         deftype:=abstractdef;
         owner := nil;
         typesym := nil;
      end;


    function tdef.typename:string;
      begin
        if assigned(typesym) and
           not(deftype=procvardef) and
           assigned(typesym._realname) and
           (typesym._realname^[1]<>'$') then
         typename:=typesym._realname^
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


    procedure tdef.derefimpl;
      begin
      end;


    function tdef.getsymtable(t:tgetsymtable):tsymtable;
      begin
        getsymtable:=nil;
      end;


{****************************************************************************
                          TSYM (base for all symtypes)
****************************************************************************}

    constructor tsym.create(const n : string);
      begin
         if n[1]='$' then
          inherited createname(copy(n,2,255))
         else
          inherited createname(upper(n));
         _realname:=stringdup(n);
         typ:=abstractsym;
      end;


    destructor tsym.destroy;
      begin
        stringdispose(_realname);
        inherited destroy;
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


    function tsym.gettypedef:tdef;
      begin
        gettypedef:=nil;
      end;


{****************************************************************************
                               TRef
****************************************************************************}

    constructor tref.create(ref :tref;pos : pfileposinfo);
      begin
        nextref:=nil;
        if pos<>nil then
          posinfo:=pos^;
        if assigned(current_module) then
          moduleindex:=current_module.unit_index;
        if assigned(ref) then
          ref.nextref:=self;
        is_written:=false;
      end;

    procedure tref.freechain;
      var
        p,q : tref;
      begin
        p:=nextref;
        nextref:=nil;
        while assigned(p) do
          begin
            q:=p.nextref;
            p.free;
            p:=q;
          end;
      end;

    destructor tref.destroy;
      begin
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


    procedure ttype.setdef(p:tdef);
      begin
        def:=p;
        sym:=nil;
      end;


    procedure ttype.setsym(p:tsym);
      begin
        sym:=p;
        def:=p.gettypedef;
        if not assigned(def) then
         internalerror(1234005);
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

    constructor tsymlist.create;
      begin
        def:=nil; { needed for procedures }
        firstsym:=nil;
        lastsym:=nil;
      end;


    destructor tsymlist.destroy;
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


    procedure tsymlist.setdef(p:tdef);
      begin
        def:=p;
      end;


    procedure tsymlist.addsym(p:tsym);
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


    function tsymlist.getcopy:tsymlist;
      var
        hp  : tsymlist;
        hp2 : psymlistitem;
      begin
        hp:=tsymlist.create;
        hp.def:=def;
        hp2:=firstsym;
        while assigned(hp2) do
         begin
           hp.addsym(hp2^.sym);
           hp2:=hp2^.next;
         end;
        getcopy:=hp;
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

    procedure resolvederef(var p:tderef;var st:tsymtable;var idx:word);
      var
        hp : tderef;
        pd : tdef;
      begin
        st:=nil;
        idx:=0;
        while assigned(p) do
         begin
           case p.dereftype of
             derefaktrecordindex :
               begin
                 st:=aktrecordsymtable;
                 idx:=p.index;
               end;
             derefaktstaticindex :
               begin
                 st:=aktstaticsymtable;
                 idx:=p.index;
               end;
             derefaktlocal :
               begin
                 st:=aktlocalsymtable;
                 idx:=p.index;
               end;
             derefunit :
               begin
{$ifdef NEWMAP}
                 st:=tsymtable(current_module.map^[p.index]^.globalsymtable);
{$else NEWMAP}
                 st:=tsymtable(current_module.map^[p.index]);
{$endif NEWMAP}
               end;
             derefrecord :
               begin
                 pd:=tdef(st.getdefnr(p.index));
                 st:=pd.getsymtable(gs_record);
                 if not assigned(st) then
                  internalerror(556658);
               end;
             dereflocal :
               begin
                 pd:=tdef(st.getdefnr(p.index));
                 st:=pd.getsymtable(gs_local);
                 if not assigned(st) then
                  internalerror(556658);
               end;
             derefpara :
               begin
                 pd:=tdef(st.getdefnr(p.index));
                 st:=pd.getsymtable(gs_para);
                 if not assigned(st) then
                  internalerror(556658);
               end;
             derefindex :
               begin
                 idx:=p.index;
               end;
             else
               internalerror(556658);
           end;
           hp:=p;
           p:=p.next;
           hp.free;
         end;
      end;


    procedure resolvedef(var def:tdef);
      var
        st   : tsymtable;
        idx  : word;
      begin
        resolvederef(tderef(def),st,idx);
        if assigned(st) then
         def:=tdef(st.getdefnr(idx))
        else
         def:=nil;
      end;


    procedure resolvesym(var sym:tsym);
      var
        st   : tsymtable;
        idx  : word;
      begin
        resolvederef(tderef(sym),st,idx);
        if assigned(st) then
         sym:=tsym(st.getsymnr(idx))
        else
         sym:=nil;
      end;

end.
{
  $Log$
  Revision 1.8  2001-08-06 21:40:49  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.7  2001/05/06 14:49:19  peter
    * ppu object to class rewrite
    * move ppu read and write stuff to fppu

  Revision 1.6  2001/04/13 01:22:17  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.5  2001/04/02 21:20:35  peter
    * resulttype rewrite

  Revision 1.4  2000/12/25 00:07:30  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.3  2000/11/29 00:30:42  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.2  2000/11/07 20:48:33  peter
    * removed ref_count from pinputfile it's not used

  Revision 1.1  2000/10/31 22:02:53  peter
    * symtable splitted, no real code changes

}
