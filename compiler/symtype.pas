{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl, Pierre Muller

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

{$i fpcdefs.inc}

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
         defoptions : tdefoptions;
         constructor create;
         procedure deref;virtual;abstract;
         procedure derefimpl;virtual;abstract;
         function  typename:string;
         function  gettypename:string;virtual;
         function  mangledparaname:string;
         function  getmangledparaname:string;virtual;abstract;
         function  size:longint;virtual;abstract;
         function  alignment:longint;virtual;abstract;
         function  getsymtable(t:tgetsymtable):tsymtable;virtual;
         function  is_publishable:boolean;virtual;abstract;
         function  needs_inittable:boolean;virtual;abstract;
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
         procedure deref;virtual;abstract;
         function  gettypedef:tdef;virtual;
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
        sltype : tsltype;
        sym    : tsym;
        value  : longint;
        next   : psymlistitem;
      end;

      tsymlist = class
        def      : tdef;
        firstsym,
        lastsym  : psymlistitem;
        constructor create;
        destructor  destroy;override;
        function  empty:boolean;
        procedure setdef(p:tdef);
        procedure addsym(slt:tsltype;p:tsym);
        procedure addconst(slt:tsltype;v:longint);
        procedure clear;
        function  getcopy:tsymlist;
        procedure resolve;
      end;


    { resolving }
    procedure resolvesym(var sym:pointer);
    procedure resolvedef(var def:pointer);


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
         defoptions:=[];
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


    function tdef.mangledparaname:string;
      begin
        if assigned(typesym) then
         mangledparaname:=typesym.name
        else
         mangledparaname:=getmangledparaname;
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
         symoptions:=[];
      end;


    destructor tsym.destroy;
      begin
        stringdispose(_realname);
        inherited destroy;
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
           resolvesym(pointer(sym));
           setsym(sym);
         end
        else
         resolvedef(pointer(def));
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


    procedure tsymlist.addsym(slt:tsltype;p:tsym);
      var
        hp : psymlistitem;
      begin
        if not assigned(p) then
         internalerror(200110203);
        new(hp);
        hp^.sltype:=slt;
        hp^.sym:=p;
        hp^.value:=0;
        hp^.next:=nil;
        if assigned(lastsym) then
         lastsym^.next:=hp
        else
         firstsym:=hp;
        lastsym:=hp;
      end;


    procedure tsymlist.addconst(slt:tsltype;v:longint);
      var
        hp : psymlistitem;
      begin
        new(hp);
        hp^.sltype:=slt;
        hp^.sym:=nil;
        hp^.value:=v;
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
        hpn : psymlistitem;
      begin
        hp:=tsymlist.create;
        hp.def:=def;
        hp2:=firstsym;
        while assigned(hp2) do
         begin
           new(hpn);
           hpn^:=hp2^;
           hpn^.next:=nil;
           if assigned(hp.lastsym) then
            hp.lastsym^.next:=hpn
           else
            hp.firstsym:=hpn;
           hp.lastsym:=hpn;
           hp2:=hp2^.next;
         end;
        getcopy:=hp;
      end;


    procedure tsymlist.resolve;
      var
        hp : psymlistitem;
      begin
        resolvedef(pointer(def));
        hp:=firstsym;
        while assigned(hp) do
         begin
           if assigned(hp^.sym) then
            resolvesym(pointer(hp^.sym));
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


    procedure resolvedef(var def:pointer);
      var
        st   : tsymtable;
        idx  : word;
      begin
        resolvederef(tderef(pointer(def)),st,idx);
        if assigned(st) then
         def:=tdef(st.getdefnr(idx))
        else
         def:=nil;
      end;


    procedure resolvesym(var sym:pointer);
      var
        st   : tsymtable;
        idx  : word;
      begin
        resolvederef(tderef(pointer(sym)),st,idx);
        if assigned(st) then
         sym:=tsym(st.getsymnr(idx))
        else
         sym:=nil;
      end;

end.
{
  $Log$
  Revision 1.18  2002-05-18 13:34:21  peter
    * readded missing revisions

  Revision 1.17  2002/05/16 19:46:45  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.15  2002/05/12 16:53:15  peter
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

  Revision 1.14  2002/04/19 15:46:04  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

}
