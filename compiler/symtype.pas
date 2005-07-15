{
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
      aasmbase,ppu,cpuinfo
      ;

    type
{************************************************
                Required Forwards
************************************************}

      tsym = class;
      Tcompilerppufile=class;


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
         procedure buildderef;virtual;abstract;
         procedure buildderefimpl;virtual;abstract;
         procedure deref;virtual;abstract;
         procedure derefimpl;virtual;abstract;
         function  typename:string;
         function  gettypename:string;virtual;
         function  mangledparaname:string;
         function  getmangledparaname:string;virtual;
         function  size:aint;virtual;abstract;
         function  alignment:longint;virtual;abstract;
         function  getvartype:longint;virtual;abstract;
         function  getparentdef:tdef;virtual;
         function  getsymtable(t:tgetsymtable):tsymtable;virtual;
         function  is_publishable:boolean;virtual;abstract;
         function  needs_inittable:boolean;virtual;abstract;
         function  is_related(def:tdef):boolean;virtual;
      end;

{************************************************
                     TSym
************************************************}

      { this object is the base for all symbol objects }
      tsym = class(tsymentry)
      protected
      public
         _realname  : pstring;
         fileinfo   : tfileposinfo;
         symoptions : tsymoptions;
         refs          : longint;
         lastref,
         defref,
         lastwritten : tref;
         refcount    : longint;
{$ifdef GDB}
         isstabwritten : boolean;
         function  get_var_value(const s:string):string;
         function  stabstr_evaluate(const s:string;vars:array of string):Pchar;
         function  stabstring : pchar;virtual;
{$endif GDB}
         constructor create(const n : string);
         destructor destroy;override;
         function  realname:string;
         procedure buildderef;virtual;
         procedure deref;virtual;
         function  gettypedef:tdef;virtual;
         procedure load_references(ppufile:tcompilerppufile;locals:boolean);virtual;
         function  write_references(ppufile:tcompilerppufile;locals:boolean):boolean;virtual;
         { currobjdef is the object def to assume, this is necessary for protected and
           private,
           context is the object def we're really in, this is for the strict stuff
         }
         function is_visible_for_object(currobjdef:tdef;context : tdef):boolean;virtual;
      end;

      tsymarr = array[0..maxlongint div sizeof(pointer)-1] of tsym;
      psymarr = ^tsymarr;

{************************************************
                   TDeref
************************************************}

      tderef = object
        dataidx : longint;
        procedure reset;
        procedure build(s:tsymtableentry);
        function  resolve:tsymtableentry;
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
        procedure buildderef;
      end;

{************************************************
                   TSymList
************************************************}

      psymlistitem = ^tsymlistitem;
      tsymlistitem = record
        sltype : tsltype;
        next   : psymlistitem;
        case byte of
          0 : (sym : tsym; symderef : tderef);
          1 : (value  : TConstExprInt);
          2 : (tt : ttype);
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
        procedure addconst(slt:tsltype;v:TConstExprInt);
        procedure addtype(slt:tsltype;const tt:ttype);
        procedure clear;
        function  getcopy:tsymlist;
        procedure resolve;
        procedure buildderef;
      end;

{************************************************
                Tcompilerppufile
************************************************}
       tcompilerppufile=class(tppufile)
       public
         procedure checkerror;
         procedure getguid(var g: tguid);
         function  getexprint:tconstexprint;
         function  getptruint:TConstPtrUInt;
         procedure getposinfo(var p:tfileposinfo);
         procedure getderef(var d:tderef);
         function  getsymlist:tsymlist;
         procedure gettype(var t:ttype);
         function  getasmsymbol:tasmsymbol;
         procedure putguid(const g: tguid);
         procedure putexprint(v:tconstexprint);
         procedure PutPtrUInt(v:TConstPtrUInt);
         procedure putposinfo(const p:tfileposinfo);
         procedure putderef(const d:tderef);
         procedure putsymlist(p:tsymlist);
         procedure puttype(const t:ttype);
         procedure putasmsymbol(s:tasmsymbol);
       end;

{$ifdef MEMDEBUG}
    var
      membrowser,
      memrealnames,
      memmanglednames,
      memprocpara,
      memprocparast,
      memproclocalst,
      memprocnodetree : tmemdebug;
{$endif MEMDEBUG}

    const
       current_object_option : tsymoptions = [sp_public];


implementation

    uses
       verbose,
       fmodule
{$ifdef GDB}
       ,gdb
{$endif GDB}
       ;


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


    function tdef.getmangledparaname:string;
      begin
         result:='<unknown type>';
      end;


    function tdef.getparentdef:tdef;
      begin
        result:=nil;
      end;


    function tdef.getsymtable(t:tgetsymtable):tsymtable;
      begin
        result:=nil;
      end;


   function  tdef.is_related(def:tdef):boolean;
     begin
       result:=false;
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
         defref:=nil;
         refs:=0;
         lastwritten:=nil;
         refcount:=0;
         fileinfo:=akttokenpos;
         if (cs_browser in aktmoduleswitches) and make_ref then
          begin
            defref:=tref.create(defref,@akttokenpos);
            inc(refcount);
          end;
         lastref:=defref;
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
         symoptions:=current_object_option;
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


    procedure Tsym.buildderef;
      begin
      end;


    procedure Tsym.deref;
      begin
      end;

{$ifdef GDB}
    function Tsym.get_var_value(const s:string):string;

    begin
      if s='name' then
        get_var_value:=name
      else if s='ownername' then
        get_var_value:=owner.name^
      else if s='line' then
        get_var_value:=tostr(fileinfo.line)
      else if s='N_LSYM' then
        get_var_value:=tostr(N_LSYM)
      else if s='N_LCSYM' then
        get_var_value:=tostr(N_LCSYM)
      else if s='N_RSYM' then
        get_var_value:=tostr(N_RSYM)
      else if s='N_TSYM' then
        get_var_value:=tostr(N_TSYM)
      else if s='N_STSYM' then
        get_var_value:=tostr(N_STSYM)
      else if s='N_FUNCTION' then
        get_var_value:=tostr(N_FUNCTION)
      else
        internalerror(200401152);
    end;

    function Tsym.stabstr_evaluate(const s:string;vars:array of string):Pchar;

    begin
      stabstr_evaluate:=string_evaluate(s,@get_var_value,vars);
    end;

    function Tsym.stabstring : pchar;

    begin
       stabstring:=nil;
    end;
{$endif GDB}


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


    procedure Tsym.load_references(ppufile:tcompilerppufile;locals:boolean);
      var
        pos : tfileposinfo;
        move_last : boolean;
      begin
        move_last:=lastwritten=lastref;
        while (not ppufile.endofentry) do
         begin
           ppufile.getposinfo(pos);
           inc(refcount);
           lastref:=tref.create(lastref,@pos);
           lastref.is_written:=true;
           if refcount=1 then
            defref:=lastref;
         end;
        if move_last then
          lastwritten:=lastref;
      end;

    { big problem here :
      wrong refs were written because of
      interface parsing of other units PM
      moduleindex must be checked !! }

    function Tsym.write_references(ppufile:tcompilerppufile;locals:boolean):boolean;
      var
        d : tderef;
        ref   : tref;
        symref_written,move_last : boolean;
      begin
        write_references:=false;
        if lastwritten=lastref then
          exit;
      { should we update lastref }
        move_last:=true;
        symref_written:=false;
      { write symbol refs }
        d.reset;
        if assigned(lastwritten) then
          ref:=lastwritten
        else
          ref:=defref;
        while assigned(ref) do
         begin
           if ref.moduleindex=current_module.unit_index then
             begin
              { write address to this symbol }
                if not symref_written then
                  begin
                     d.build(self);
                     ppufile.putderef(d);
                     symref_written:=true;
                  end;
                ppufile.putposinfo(ref.posinfo);
                ref.is_written:=true;
                if move_last then
                  lastwritten:=ref;
             end
           else if not ref.is_written then
             move_last:=false
           else if move_last then
             lastwritten:=ref;
           ref:=ref.nextref;
         end;
        if symref_written then
          ppufile.writeentry(ibsymref);
        write_references:=symref_written;
      end;


    function tsym.is_visible_for_object(currobjdef:Tdef;context : tdef):boolean;
      begin
        is_visible_for_object:=false;

        { private symbols are allowed when we are in the same
          module as they are defined }
        if (sp_private in symoptions) and
           assigned(owner.defowner) and
           (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
           (not owner.defowner.owner.iscurrentunit) then
          exit;

        if (sp_strictprivate in symoptions) then
          begin
            result:=assigned(currobjdef) and
              (context=tdef(owner.defowner));
            exit;
          end;

        if (sp_strictprotected in symoptions) then
          begin
            result:=assigned(context) and
              context.is_related(tdef(owner.defowner));
            exit;
          end;

        { protected symbols are visible in the module that defines them and
          also visible to related objects }
        if (sp_protected in symoptions) and
           (
            (
             assigned(owner.defowner) and
             (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
             (not owner.defowner.owner.iscurrentunit)
            ) and
            not(
                assigned(currobjdef) and
                (currobjdef.owner.symtabletype in [globalsymtable,staticsymtable]) and
                (currobjdef.owner.iscurrentunit) and
                currobjdef.is_related(tdef(owner.defowner))
               )
           ) then
          exit;

        is_visible_for_object:=true;
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


    procedure ttype.buildderef;
      begin
        { Write symbol references when the symbol is a redefine,
          but don't write symbol references for the current unit
          and for the system unit }
        if assigned(sym) and
           (
            (sym<>def.typesym) or
            (
             not((sym.owner.symtabletype in [globalsymtable,staticsymtable]) and
                 sym.owner.iscurrentunit)
            )
           ) then
          deref.build(sym)
        else
          deref.build(def);
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
        fillchar(hp^,sizeof(tsymlistitem),0);
        hp^.sltype:=slt;
        hp^.sym:=p;
        hp^.symderef.reset;
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
        fillchar(hp^,sizeof(tsymlistitem),0);
        hp^.sltype:=slt;
        hp^.symderef:=d;
        if assigned(lastsym) then
         lastsym^.next:=hp
        else
         firstsym:=hp;
        lastsym:=hp;
      end;


    procedure tsymlist.addconst(slt:tsltype;v:TConstExprInt);
      var
        hp : psymlistitem;
      begin
        new(hp);
        fillchar(hp^,sizeof(tsymlistitem),0);
        hp^.sltype:=slt;
        hp^.value:=v;
        if assigned(lastsym) then
         lastsym^.next:=hp
        else
         firstsym:=hp;
        lastsym:=hp;
      end;


    procedure tsymlist.addtype(slt:tsltype;const tt:ttype);
      var
        hp : psymlistitem;
      begin
        new(hp);
        fillchar(hp^,sizeof(tsymlistitem),0);
        hp^.sltype:=slt;
        hp^.tt:=tt;
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
           case hp^.sltype of
             sl_call,
             sl_load,
             sl_subscript :
               hp^.sym:=tsym(hp^.symderef.resolve);
             sl_typeconv :
               hp^.tt.resolve;
             sl_vec :
               ;
             else
              internalerror(200110205);
           end;
           hp:=hp^.next;
         end;
      end;


    procedure tsymlist.buildderef;
      var
        hp : psymlistitem;
      begin
        procdefderef.build(procdef);
        hp:=firstsym;
        while assigned(hp) do
         begin
           case hp^.sltype of
             sl_call,
             sl_load,
             sl_subscript :
               hp^.symderef.build(hp^.sym);
             sl_typeconv :
               hp^.tt.buildderef;
             sl_vec :
               ;
             else
              internalerror(200110205);
           end;
           hp:=hp^.next;
         end;
      end;


{****************************************************************************
                                Tderef
****************************************************************************}


    procedure tderef.reset;
      begin
        dataidx:=-1;
      end;


    procedure tderef.build(s:tsymtableentry);
      var
        len  : byte;
        data : array[0..255] of byte;

        function is_child(currdef,ownerdef:tdef):boolean;
        begin
          while assigned(currdef) and
                (currdef<>ownerdef) do
            currdef:=currdef.getparentdef;
          result:=assigned(currdef);
        end;

        procedure addowner(s:tsymtableentry);
        var
          idx : longint;
        begin
          if not assigned(s.owner) then
            internalerror(200306063);
          case s.owner.symtabletype of
            globalsymtable :
              begin
                if s.owner.iscurrentunit then
                  begin
                    data[len]:=ord(deref_aktglobal);
                    inc(len);
                  end
                else
                  begin
                    { register that the unit is needed for resolving }
                    idx:=current_module.derefidx_unit(s.owner.moduleid);
                    data[len]:=ord(deref_unit);
                    data[len+1]:=idx shr 8;
                    data[len+2]:=idx and $ff;
                    inc(len,3);
                  end;
              end;
            staticsymtable :
              begin
                { only references to the current static symtable are allowed }
                if not s.owner.iscurrentunit then
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
          if len>252 then
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
              { objects are only allowed in globalsymtable,staticsymtable  }
              if not(nextdef.owner.symtabletype in [globalsymtable,staticsymtable]) then
                internalerror(200306187);
              { Next parent is in a different unit, then stop }
              if not(nextdef.owner.iscurrentunit) then
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
        { skip length byte }
        len:=1;
        if assigned(s) then
         begin
           { Static symtable of current unit ? }
           if (s.owner.symtabletype=staticsymtable) and
              s.owner.iscurrentunit then
            begin
              data[len]:=ord(deref_aktstatic);
              inc(len);
            end
           { Global symtable of current unit ? }
           else if (s.owner.symtabletype=globalsymtable) and
                   s.owner.iscurrentunit then
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
        { store data length in first byte }
        data[0]:=len-1;
        { store index and write to derefdata }
        dataidx:=current_module.derefdata.size;
        current_module.derefdata.write(data,len);
      end;


    function tderef.resolve:tsymtableentry;
      var
        pd     : tdef;
        pm     : tmodule;
        typ    : tdereftype;
        st     : tsymtable;
        idx    : word;
        i      : aint;
        len    : byte;
        data   : array[0..255] of byte;
      begin
        result:=nil;
        { not initialized or error }
        if dataidx<0 then
          internalerror(200306067);
        { read data }
        current_module.derefdata.seek(dataidx);
        if current_module.derefdata.read(len,1)<>1 then
          internalerror(200310221);
        if len>0 then
          begin
            if current_module.derefdata.read(data,len)<>len then
              internalerror(200310222);
          end;
        { process data }
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
                st:=current_module.localsymtable;
              deref_aktglobal :
                st:=current_module.globalsymtable;
              deref_aktlocal :
                st:=aktlocalsymtable;
              deref_aktpara :
                st:=aktparasymtable;
              deref_unit :
                begin
                  idx:=(data[i] shl 8) or data[i+1];
                  inc(i,2);
                  pm:=current_module.resolve_unit(idx);
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

{*****************************************************************************
                            TCompilerPPUFile
*****************************************************************************}

    procedure tcompilerppufile.checkerror;
      begin
        if error then
         Message(unit_f_ppu_read_error);
      end;


    procedure tcompilerppufile.getguid(var g: tguid);
      begin
        getdata(g,sizeof(g));
      end;


    function tcompilerppufile.getexprint:tconstexprint;
      begin
        if sizeof(tconstexprint)=8 then
          result:=tconstexprint(getint64)
        else
          result:=tconstexprint(getlongint);
      end;


    function tcompilerppufile.getPtrUInt:TConstPtrUInt;
      begin
        if sizeof(TConstPtrUInt)=8 then
          result:=tconstptruint(getint64)
        else
          result:=TConstPtrUInt(getlongint);
      end;


    procedure tcompilerppufile.getposinfo(var p:tfileposinfo);
      var
        info : byte;
      begin
        {
          info byte layout in bits:
          0-1 - amount of bytes for fileindex
          2-3 - amount of bytes for line
          4-5 - amount of bytes for column
        }
        info:=getbyte;
        case (info and $03) of
         0 : p.fileindex:=getbyte;
         1 : p.fileindex:=getword;
         2 : p.fileindex:=(getbyte shl 16) or getword;
         3 : p.fileindex:=getlongint;
        end;
        case ((info shr 2) and $03) of
         0 : p.line:=getbyte;
         1 : p.line:=getword;
         2 : p.line:=(getbyte shl 16) or getword;
         3 : p.line:=getlongint;
        end;
        case ((info shr 4) and $03) of
         0 : p.column:=getbyte;
         1 : p.column:=getword;
         2 : p.column:=(getbyte shl 16) or getword;
         3 : p.column:=getlongint;
        end;
      end;


    procedure tcompilerppufile.getderef(var d:tderef);
      begin
        d.dataidx:=getlongint;
      end;


    function tcompilerppufile.getsymlist:tsymlist;
      var
        symderef : tderef;
        tt  : ttype;
        slt : tsltype;
        idx : longint;
        p   : tsymlist;
      begin
        p:=tsymlist.create;
        getderef(p.procdefderef);
        repeat
          slt:=tsltype(getbyte);
          case slt of
            sl_none :
              break;
            sl_call,
            sl_load,
            sl_subscript :
              begin
                getderef(symderef);
                p.addsymderef(slt,symderef);
              end;
            sl_typeconv :
              begin
                gettype(tt);
                p.addtype(slt,tt);
              end;
            sl_vec :
              begin
                idx:=getlongint;
                p.addconst(slt,idx);
              end;
            else
              internalerror(200110204);
          end;
        until false;
        getsymlist:=tsymlist(p);
      end;


    procedure tcompilerppufile.gettype(var t:ttype);
      begin
        getderef(t.deref);
        t.def:=nil;
        t.sym:=nil;
      end;


    function  tcompilerppufile.getasmsymbol:tasmsymbol;
      begin
        getasmsymbol:=tasmsymbol(pointer(ptrint(getlongint)));
      end;


    procedure tcompilerppufile.putposinfo(const p:tfileposinfo);
      var
        oldcrc : boolean;
        info   : byte;
      begin
        { posinfo is not relevant for changes in PPU }
        oldcrc:=do_crc;
        do_crc:=false;
        {
          info byte layout in bits:
          0-1 - amount of bytes for fileindex
          2-3 - amount of bytes for line
          4-5 - amount of bytes for column
        }
        info:=0;
        { calculate info byte }
        if (p.fileindex>$ff) then
         begin
           if (p.fileindex<=$ffff) then
            info:=info or $1
           else
            if (p.fileindex<=$ffffff) then
             info:=info or $2
           else
            info:=info or $3;
          end;
        if (p.line>$ff) then
         begin
           if (p.line<=$ffff) then
            info:=info or $4
           else
            if (p.line<=$ffffff) then
             info:=info or $8
           else
            info:=info or $c;
          end;
        if (p.column>$ff) then
         begin
           if (p.column<=$ffff) then
            info:=info or $10
           else
            if (p.column<=$ffffff) then
             info:=info or $20
           else
            info:=info or $30;
          end;
        { write data }
        putbyte(info);
        case (info and $03) of
         0 : putbyte(p.fileindex);
         1 : putword(p.fileindex);
         2 : begin
               putbyte(p.fileindex shr 16);
               putword(p.fileindex and $ffff);
             end;
         3 : putlongint(p.fileindex);
        end;
        case ((info shr 2) and $03) of
         0 : putbyte(p.line);
         1 : putword(p.line);
         2 : begin
               putbyte(p.line shr 16);
               putword(p.line and $ffff);
             end;
         3 : putlongint(p.line);
        end;
        case ((info shr 4) and $03) of
         0 : putbyte(p.column);
         1 : putword(p.column);
         2 : begin
               putbyte(p.column shr 16);
               putword(p.column and $ffff);
             end;
         3 : putlongint(p.column);
        end;
        do_crc:=oldcrc;
      end;


    procedure tcompilerppufile.putguid(const g: tguid);
      begin
        putdata(g,sizeof(g));
      end;


    procedure tcompilerppufile.putexprint(v:tconstexprint);
      begin
        if sizeof(TConstExprInt)=8 then
          putint64(int64(v))
        else if sizeof(TConstExprInt)=4 then
          putlongint(longint(v))
        else
          internalerror(2002082601);
      end;


    procedure tcompilerppufile.PutPtrUInt(v:TConstPtrUInt);
      begin
        if sizeof(TConstPtrUInt)=8 then
          putint64(int64(v))
        else if sizeof(TConstPtrUInt)=4 then
          putlongint(longint(v))
        else
          internalerror(2002082601);
      end;


    procedure tcompilerppufile.putderef(const d:tderef);
      var
        oldcrc : boolean;
      begin
        oldcrc:=do_crc;
        do_crc:=false;
        putlongint(d.dataidx);
        do_crc:=oldcrc;
      end;


    procedure tcompilerppufile.putsymlist(p:tsymlist);
      var
        hp : psymlistitem;
      begin
        putderef(p.procdefderef);
        hp:=p.firstsym;
        while assigned(hp) do
         begin
           putbyte(byte(hp^.sltype));
           case hp^.sltype of
             sl_call,
             sl_load,
             sl_subscript :
               putderef(hp^.symderef);
             sl_typeconv :
               puttype(hp^.tt);
             sl_vec :
               putlongint(hp^.value);
             else
              internalerror(200110205);
           end;
           hp:=hp^.next;
         end;
        putbyte(byte(sl_none));
      end;


    procedure tcompilerppufile.puttype(const t:ttype);
      begin
        putderef(t.deref);
      end;


    procedure tcompilerppufile.putasmsymbol(s:tasmsymbol);
      begin
        if assigned(s) then
         begin
           if s.ppuidx=-1 then
            begin
              inc(objectlibrary.asmsymbolppuidx);
              s.ppuidx:=objectlibrary.asmsymbolppuidx;
            end;
           putlongint(s.ppuidx);
         end
        else
         putlongint(0);
      end;

{$ifdef MEMDEBUG}
initialization
  membrowser:=TMemDebug.create('BrowserRefs');
  membrowser.stop;
  memrealnames:=TMemDebug.create('Realnames');
  memrealnames.stop;
  memmanglednames:=TMemDebug.create('Manglednames');
  memmanglednames.stop;
  memprocpara:=TMemDebug.create('ProcPara');
  memprocpara.stop;
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
  memprocpara.free;
  memprocparast.free;
  memproclocalst.free;
  memprocnodetree.free;
{$endif MEMDEBUG}

end.
