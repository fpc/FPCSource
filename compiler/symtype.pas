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
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

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
{$ifdef MEMDEBUG}
      cclasses,
{$endif MEMDEBUG}
      { global }
      globtype,globals,
      { symtable }
      symconst,symbase,
      { aasm }
      aasmbase
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
         function  getparentdef:tdef;virtual;
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
                   TDeref
************************************************}

      tderefdata = array[0..31] of byte;

      tderef = object
        len  : longint;
        data : tderefdata;
        procedure reset;
        procedure setdata(l:longint;var d);
        procedure build(s:tsymtableentry);
        function resolve:tsymtableentry;
      end;

{************************************************
                   TType
************************************************}

      ttype = object
        def : tdef;
        sym : tsym;
        deref : tderef;
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
        symderef : tderef;
        value  : longint;
        next   : psymlistitem;
      end;

      tsymlist = class
        procdef  : tdef;
        procdefderef : tderef;
        firstsym,
        lastsym  : psymlistitem;
        constructor create;
        destructor  destroy;override;
        function  empty:boolean;
        procedure addsym(slt:tsltype;p:tsym);
        procedure addsymderef(slt:tsltype;const d:tderef);
        procedure addconst(slt:tsltype;v:longint);
        procedure clear;
        function  getcopy:tsymlist;
        procedure resolve;
      end;


{$ifdef MEMDEBUG}
    var
      membrowser,
      memrealnames,
      memmanglednames,
      memprocparast,
      memproclocalst,
      memprocnodetree : tmemdebug;
{$endif MEMDEBUG}


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
           not(deftype in [procvardef,procdef]) and
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


    function tdef.getparentdef:tdef;
      begin
        result:=nil;
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
{$ifdef MEMDEBUG}
        memrealnames.start;
{$endif MEMDEBUG}
        stringdispose(_realname);
{$ifdef MEMDEBUG}
        memrealnames.stop;
{$endif MEMDEBUG}
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
      var
        p : tsymtableentry;
      begin
        p:=deref.resolve;
        if assigned(p) then
          begin
            if p is tsym then
              begin
                setsym(tsym(p));
                if not assigned(def) then
                 internalerror(200212272);
              end
            else
              begin
                setdef(tdef(p));
              end;
          end
        else
          reset;
      end;


{****************************************************************************
                                 TSymList
****************************************************************************}

    constructor tsymlist.create;
      begin
        procdef:=nil; { needed for procedures }
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
        procdef:=nil;
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
        hp^.symderef.reset;
        hp^.value:=0;
        hp^.next:=nil;
        if assigned(lastsym) then
         lastsym^.next:=hp
        else
         firstsym:=hp;
        lastsym:=hp;
      end;


    procedure tsymlist.addsymderef(slt:tsltype;const d:tderef);
      var
        hp : psymlistitem;
      begin
        new(hp);
        hp^.sltype:=slt;
        hp^.sym:=nil;
        hp^.symderef:=d;
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
        hp^.symderef.reset;
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
        hp.procdef:=procdef;
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
        procdef:=tdef(procdefderef.resolve);
        hp:=firstsym;
        while assigned(hp) do
         begin
           hp^.sym:=tsym(hp^.symderef.resolve);
           hp:=hp^.next;
         end;
      end;


{****************************************************************************
                                Tderef
****************************************************************************}


    procedure tderef.reset;
      begin
        len:=0;
      end;


    procedure tderef.setdata(l:longint;var d);
      begin
        len:=l;
        if l>sizeof(tderefdata) then
          internalerror(200306068);
        move(d,data,len);
      end;


    procedure tderef.build(s:tsymtableentry);

        function is_child(currdef,ownerdef:tdef):boolean;
        begin
          while assigned(currdef) and
                (currdef<>ownerdef) do
            currdef:=currdef.getparentdef;
          result:=assigned(currdef);
        end;

        procedure addowner(s:tsymtableentry);
        begin
          if not assigned(s.owner) then
            internalerror(200306063);
          case s.owner.symtabletype of
            globalsymtable :
              begin
                if s.owner.unitid=0 then
                  begin
                    data[len]:=ord(deref_aktglobal);
                    inc(len);
                  end
                else
                  begin
                    { check if the unit is available in the uses
                      clause, else it's an error }
                    if s.owner.unitid=$ffff then
                      internalerror(200306063);
                    data[len]:=ord(deref_unit);
                    data[len+1]:=s.owner.unitid shr 8;
                    data[len+2]:=s.owner.unitid and $ff;
                    inc(len,3);
                  end;
              end;
            staticsymtable :
              begin
                { only references to the current static symtable are allowed }
                if s.owner<>aktstaticsymtable then
                  internalerror(200306233);
                data[len]:=ord(deref_aktstatic);
                inc(len);
              end;
            localsymtable :
              begin
                addowner(s.owner.defowner);
                data[len]:=ord(deref_def);
                data[len+1]:=s.owner.defowner.indexnr shr 8;
                data[len+2]:=s.owner.defowner.indexnr and $ff;
                data[len+3]:=ord(deref_local);
                inc(len,4);
              end;
            parasymtable :
              begin
                addowner(s.owner.defowner);
                data[len]:=ord(deref_def);
                data[len+1]:=s.owner.defowner.indexnr shr 8;
                data[len+2]:=s.owner.defowner.indexnr and $ff;
                data[len+3]:=ord(deref_para);
                inc(len,4);
              end;
            objectsymtable,
            recordsymtable :
              begin
                addowner(s.owner.defowner);
                data[len]:=ord(deref_def);
                data[len+1]:=s.owner.defowner.indexnr shr 8;
                data[len+2]:=s.owner.defowner.indexnr and $ff;
                data[len+3]:=ord(deref_record);
                inc(len,4);
              end;
            else
              internalerror(200306065);
          end;
          if len+3>sizeof(tderefdata) then
            internalerror(200306062);
        end;

        procedure addparentobject(currdef,ownerdef:tdef);
        var
          nextdef : tdef;
        begin
          if not assigned(currdef) then
            internalerror(200306185);
          { Already handled by derefaktrecordindex }
          if currdef=ownerdef then
            internalerror(200306188);
          { Generate a direct reference to the top parent
            class available in the current unit, this is required because
            the parent class is maybe not resolved yet and therefor
            has the childof value not available yet }
          while (currdef<>ownerdef) do
            begin
              nextdef:=currdef.getparentdef;
              { objects are only allowed in globalsymtable,staticsymtable this check is
                needed because we need the unitid }
              if not(nextdef.owner.symtabletype in [globalsymtable,staticsymtable]) then
                internalerror(200306187);
              { Next parent is in a different unit, then stop }
              if nextdef.owner.unitid<>0 then
                break;
              currdef:=nextdef;
            end;
          { Add reference where to start the parent lookup }
          if currdef=aktrecordsymtable.defowner then
            begin
              data[len]:=ord(deref_aktrecord);
              inc(len);
            end
          else
            begin
              if currdef.owner.symtabletype=globalsymtable then
                data[len]:=ord(deref_aktglobal)
              else
                data[len]:=ord(deref_aktstatic);
              data[len+1]:=ord(deref_def);
              data[len+2]:=currdef.indexnr shr 8;
              data[len+3]:=currdef.indexnr and $ff;
              data[len+4]:=ord(deref_record);
              inc(len,5);
            end;
          { When the current found parent in this module is not the owner we
            add derefs for the parent classes not available in this unit }
          while (currdef<>ownerdef) do
            begin
              data[len]:=ord(deref_parent_object);
              inc(len);
              currdef:=currdef.getparentdef;
              { It should be valid as it is checked by is_child }
              if not assigned(currdef) then
                internalerror(200306186);
            end;
        end;

      begin
        len:=0;
        if assigned(s) then
         begin
           { Static symtable of current unit ? }
           if (s.owner.symtabletype=staticsymtable) and
              (s.owner.unitid=0) then
            begin
              data[len]:=ord(deref_aktstatic);
              inc(len);
            end
           { Global symtable of current unit ? }
           else if (s.owner.symtabletype=globalsymtable) and
                   (s.owner.unitid=0) then
            begin
              data[len]:=ord(deref_aktglobal);
              inc(len);
            end
           { Current record/object symtable ? }
           else if (s.owner=aktrecordsymtable) then
            begin
              data[len]:=ord(deref_aktrecord);
              inc(len);
            end
           { Current local symtable ? }
           else if (s.owner=aktlocalsymtable) then
            begin
              data[len]:=ord(deref_aktlocal);
              inc(len);
            end
           { Current para symtable ? }
           else if (s.owner=aktparasymtable) then
            begin
              data[len]:=ord(deref_aktpara);
              inc(len);
            end
           { Parent class? }
           else if assigned(aktrecordsymtable) and
                   (aktrecordsymtable.symtabletype=objectsymtable) and
                   (s.owner.symtabletype=objectsymtable) and
                   is_child(tdef(aktrecordsymtable.defowner),tdef(s.owner.defowner)) then
            begin
              addparentobject(tdef(aktrecordsymtable.defowner),tdef(s.owner.defowner));
            end
           else
           { Default, start by building from unit symtable }
            begin
              addowner(s);
            end;
           { Add index of the symbol/def }
           if s is tsym then
             data[len]:=ord(deref_sym)
           else
             data[len]:=ord(deref_def);
           data[len+1]:=s.indexnr shr 8;
           data[len+2]:=s.indexnr and $ff;
           inc(len,3);
         end
        else
         begin
           { nil pointer }
           data[len]:=0;
           inc(len);
         end;
      end;


    function tderef.resolve:tsymtableentry;
      var
        pd     : tdef;
        pm     : tmodule;
        typ    : tdereftype;
        st     : tsymtable;
        idx    : word;
        i      : longint;
      begin
        result:=nil;
        { not initialized }
        if len=0 then
          internalerror(200306067);
        st:=nil;
        i:=0;
        while (i<len) do
          begin
            typ:=tdereftype(data[i]);
            inc(i);
            case typ of
              deref_nil :
                begin
                  result:=nil;
                  { Only allowed when no other deref is available }
                  if len<>1 then
                    internalerror(200306232);
                end;
              deref_sym :
                begin
                  if not assigned(st) then
                    internalerror(200309141);
                  idx:=(data[i] shl 8) or data[i+1];
                  inc(i,2);
                  result:=st.getsymnr(idx);
                end;
              deref_def :
                begin
                  if not assigned(st) then
                    internalerror(200309142);
                  idx:=(data[i] shl 8) or data[i+1];
                  inc(i,2);
                  result:=st.getdefnr(idx);
                end;
              deref_aktrecord :
                st:=aktrecordsymtable;
              deref_aktstatic :
                st:=aktstaticsymtable;
              deref_aktglobal :
                st:=aktglobalsymtable;
              deref_aktlocal :
                st:=aktlocalsymtable;
              deref_aktpara :
                st:=aktparasymtable;
              deref_unit :
                begin
                  idx:=(data[i] shl 8) or data[i+1];
                  inc(i,2);
                  if idx>maxunits then
                    internalerror(200306231);
                  pm:=current_module.map^[idx];
                  if not assigned(pm) then
                    internalerror(200212273);
                  st:=pm.globalsymtable;
                end;
              deref_local :
                begin
                  if not assigned(result) then
                    internalerror(200306069);
                  st:=tdef(result).getsymtable(gs_local);
                  result:=nil;
                  if not assigned(st) then
                    internalerror(200212275);
                end;
              deref_para :
                begin
                  if not assigned(result) then
                    internalerror(2003060610);
                  st:=tdef(result).getsymtable(gs_para);
                  result:=nil;
                  if not assigned(st) then
                    internalerror(200212276);
                end;
              deref_record :
                begin
                  if not assigned(result) then
                    internalerror(200306068);
                  st:=tdef(result).getsymtable(gs_record);
                  result:=nil;
                  if not assigned(st) then
                    internalerror(200212274);
                end;
              deref_parent_object :
                begin
                  { load current object symtable if no
                    symtable is available yet }
                  if st=nil then
                    begin
                      st:=aktrecordsymtable;
                      if not assigned(st) then
                        internalerror(200306068);
                    end;
                  if st.symtabletype<>objectsymtable then
                    internalerror(200306189);
                  pd:=tdef(st.defowner).getparentdef;
                  if not assigned(pd) then
                    internalerror(200306184);
                  st:=pd.getsymtable(gs_record);
                  if not assigned(st) then
                    internalerror(200212274);
                end;
              else
                internalerror(200212277);
            end;
          end;
      end;


{$ifdef MEMDEBUG}
initialization
  membrowser:=TMemDebug.create('BrowserRefs');
  membrowser.stop;
  memrealnames:=TMemDebug.create('Realnames');
  memrealnames.stop;
  memmanglednames:=TMemDebug.create('Manglednames');
  memmanglednames.stop;
  memprocparast:=TMemDebug.create('ProcParaSt');
  memprocparast.stop;
  memproclocalst:=TMemDebug.create('ProcLocalSt');
  memproclocalst.stop;
  memprocnodetree:=TMemDebug.create('ProcNodeTree');
  memprocnodetree.stop;

finalization
  membrowser.free;
  memrealnames.free;
  memmanglednames.free;
  memprocparast.free;
  memproclocalst.free;
  memprocnodetree.free;
{$endif MEMDEBUG}

end.
{
  $Log$
  Revision 1.28  2003-10-07 16:06:30  peter
    * tsymlist.def renamed to tsymlist.procdef
    * tsymlist.procdef is now only used to store the procdef

  Revision 1.27  2003/09/14 12:58:29  peter
    * give IE when st is not assigned in deref

  Revision 1.26  2003/06/25 18:31:23  peter
    * sym,def resolving partly rewritten to support also parent objects
      not directly available through the uses clause

  Revision 1.25  2003/06/07 20:26:32  peter
    * re-resolving added instead of reloading from ppu
    * tderef object added to store deref info for resolving

  Revision 1.24  2002/12/29 18:26:31  peter
    * also use gettypename for procdef always

  Revision 1.23  2002/12/29 14:57:50  peter
    * unit loading changed to first register units and load them
      afterwards. This is needed to support uses xxx in yyy correctly
    * unit dependency check fixed

  Revision 1.22  2002/09/05 19:29:46  peter
    * memdebug enhancements

  Revision 1.21  2002/08/18 20:06:28  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.20  2002/08/11 13:24:16  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.19  2002/07/01 18:46:29  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.18  2002/05/18 13:34:21  peter
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
