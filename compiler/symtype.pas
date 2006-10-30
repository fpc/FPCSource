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
         { maybe it's useful to merge the dwarf and stabs debugging info with some hacking }
         { dwarf debugging }
         dwarf_lab : tasmsymbol;
         { stabs debugging }
         stab_number : word;
         dbg_state  : tdefdbgstatus;
         defoptions : tdefoptions;
         constructor create(dt:tdeftype);
         procedure buildderef;virtual;abstract;
         procedure buildderefimpl;virtual;abstract;
         procedure deref;virtual;abstract;
         procedure derefimpl;virtual;abstract;
         function  typename:string;
         function  GetTypeName:string;virtual;
         function  mangledparaname:string;
         function  getmangledparaname:string;virtual;
         function  size:aint;virtual;abstract;
         function  packedbitsize:aint;virtual;
         function  alignment:shortint;virtual;abstract;
         function  getvardef:longint;virtual;abstract;
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
         _realname  : pshortstring;
         fileinfo   : tfileposinfo;
         symoptions : tsymoptions;
         refs          : longint;
         lastref,
         defref,
         lastwritten : tref;
         refcount    : longint;
         isdbgwritten : boolean;
         constructor create(st:tsymtyp;const n : string);
         destructor destroy;override;
         function  realname:string;
         function  mangledname:string; virtual;
         procedure buildderef;virtual;
         procedure deref;virtual;
         function  getderefdef:tdef;virtual;
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
                   tpropaccesslist
************************************************}

      ppropaccesslistitem = ^tpropaccesslistitem;
      tpropaccesslistitem = record
        sltype : tsltype;
        next   : ppropaccesslistitem;
        case byte of
          0 : (sym : tsym; symderef : tderef);
          1 : (value  : TConstExprInt; valuedef: tdef; valuedefderef:tderef);
          2 : (def: tdef; defderef:tderef);
      end;

      tpropaccesslist = class
        procdef  : tdef;
        procdefderef : tderef;
        firstsym,
        lastsym  : ppropaccesslistitem;
        constructor create;
        destructor  destroy;override;
        function  empty:boolean;
        procedure addsym(slt:tsltype;p:tsym);
        procedure addconst(slt:tsltype;v:TConstExprInt;d:tdef);
        procedure addtype(slt:tsltype;d:tdef);
        procedure addsymderef(slt:tsltype;d:tderef);
        procedure addconstderef(slt:tsltype;v:TConstExprInt;d:tderef);
        procedure addtypederef(slt:tsltype;d:tderef);
        procedure clear;
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
         function  getpropaccesslist:tpropaccesslist;
         function  getasmsymbol:tasmsymbol;
         procedure putguid(const g: tguid);
         procedure putexprint(v:tconstexprint);
         procedure PutPtrUInt(v:TConstPtrUInt);
         procedure putposinfo(const p:tfileposinfo);
         procedure putderef(const d:tderef);
         procedure putpropaccesslist(p:tpropaccesslist);
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
       fmodule,symtable
       ;


{****************************************************************************
                                Tdef
****************************************************************************}

    constructor tdef.create(dt:tdeftype);
      begin
         inherited create;
         deftype:=dt;
         owner := nil;
         typesym := nil;
         defoptions:=[];
         dbg_state:=dbg_state_unused;
         stab_number:=0;
      end;


    function tdef.typename:string;
      begin
        if assigned(typesym) and
           not(deftype in [procvardef,procdef]) and
           assigned(typesym._realname) and
           (typesym._realname^[1]<>'$') then
         typename:=typesym._realname^
        else
         typename:=GetTypeName;
      end;


    function tdef.GetTypeName : string;
      begin
         GetTypeName:='<unknown type>'
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


    function tdef.is_related(def:tdef):boolean;
      begin
        result:=false;
      end;


    function tdef.packedbitsize:aint;
      begin
        result:=size * 8;
      end;


{****************************************************************************
                          TSYM (base for all symtypes)
****************************************************************************}

    constructor tsym.create(st:tsymtyp;const n : string);
      begin
         if n[1]='$' then
          inherited createname(copy(n,2,255))
         else
          inherited createname(upper(n));
         _realname:=stringdup(n);
         typ:=st;
         symoptions:=[];
         defref:=nil;
         refs:=0;
         lastwritten:=nil;
         refcount:=0;
         fileinfo:=current_tokenpos;
         if (cs_browser in current_settings.moduleswitches) and make_ref then
          begin
            defref:=tref.create(defref,@current_tokenpos);
            inc(refcount);
          end;
         lastref:=defref;
         isdbgwritten := false;
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


    function tsym.realname : string;
      begin
        if assigned(_realname) then
         realname:=_realname^
        else
         realname:=name;
      end;


    function tsym.mangledname : string;
      begin
        internalerror(200204171);
        result:='';
      end;


    function tsym.getderefdef:tdef;
      begin
        getderefdef:=nil;
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
                                 tpropaccesslist
****************************************************************************}

    constructor tpropaccesslist.create;
      begin
        procdef:=nil; { needed for procedures }
        firstsym:=nil;
        lastsym:=nil;
      end;


    destructor tpropaccesslist.destroy;
      begin
        clear;
      end;


    function tpropaccesslist.empty:boolean;
      begin
        empty:=(firstsym=nil);
      end;


    procedure tpropaccesslist.clear;
      var
        hp : ppropaccesslistitem;
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


    procedure tpropaccesslist.addsym(slt:tsltype;p:tsym);
      var
        hp : ppropaccesslistitem;
      begin
        new(hp);
        fillchar(hp^,sizeof(tpropaccesslistitem),0);
        hp^.sltype:=slt;
        hp^.sym:=p;
        hp^.symderef.reset;
        if assigned(lastsym) then
         lastsym^.next:=hp
        else
         firstsym:=hp;
        lastsym:=hp;
      end;


    procedure tpropaccesslist.addconst(slt:tsltype;v:TConstExprInt;d:tdef);
      var
        hp : ppropaccesslistitem;
      begin
        new(hp);
        fillchar(hp^,sizeof(tpropaccesslistitem),0);
        hp^.sltype:=slt;
        hp^.value:=v;
        hp^.valuedef:=d;
        hp^.valuedefderef.reset;
        if assigned(lastsym) then
         lastsym^.next:=hp
        else
         firstsym:=hp;
        lastsym:=hp;
      end;


    procedure tpropaccesslist.addtype(slt:tsltype;d:tdef);
      var
        hp : ppropaccesslistitem;
      begin
        new(hp);
        fillchar(hp^,sizeof(tpropaccesslistitem),0);
        hp^.sltype:=slt;
        hp^.def:=d;
        hp^.defderef.reset;
        if assigned(lastsym) then
         lastsym^.next:=hp
        else
         firstsym:=hp;
        lastsym:=hp;
      end;


    procedure tpropaccesslist.addsymderef(slt:tsltype;d:tderef);
      begin
        addsym(slt,nil);
        lastsym^.symderef:=d;
      end;


    procedure tpropaccesslist.addconstderef(slt:tsltype;v:TConstExprInt;d:tderef);
      begin
        addconst(slt,v,nil);
        lastsym^.valuedefderef:=d;
      end;


    procedure tpropaccesslist.addtypederef(slt:tsltype;d:tderef);
      begin
        addtype(slt,nil);
        lastsym^.defderef:=d;
      end;


    procedure tpropaccesslist.resolve;
      var
        hp : ppropaccesslistitem;
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
             sl_absolutetype,
             sl_typeconv :
               hp^.def:=tdef(hp^.defderef.resolve);
             sl_vec:
               hp^.valuedef:=tdef(hp^.valuedefderef.resolve);
             else
              internalerror(200110205);
           end;
           hp:=hp^.next;
         end;
      end;


    procedure tpropaccesslist.buildderef;
      var
        hp : ppropaccesslistitem;
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
             sl_absolutetype,
             sl_typeconv :
               hp^.defderef.build(hp^.def);
             sl_vec:
               hp^.valuedefderef.build(hp^.valuedef);
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
        st   : tsymtable;
        data : array[0..255] of byte;
        idx : word;
      begin
        { skip length byte }
        len:=1;

        if assigned(s) then
         begin
           st:=findunitsymtable(s.owner);
           if not st.iscurrentunit then
             begin
               { register that the unit is needed for resolving }
               data[len]:=ord(deref_unit);
               idx:=current_module.derefidx_unit(st.moduleid);
               data[len+1]:=idx shr 8 and $ff;
               data[len+2]:=idx and $ff;
               inc(len,3);
             end;
           if s is tsym then
             begin
               data[len]:=ord(deref_symid);
               data[len+1]:=tsym(s).symid shr 24 and $ff;
               data[len+2]:=tsym(s).symid shr 16 and $ff;
               data[len+3]:=tsym(s).symid shr 8 and $ff;
               data[len+4]:=tsym(s).symid and $ff;
               inc(len,5);
             end
           else
             begin
               data[len]:=ord(deref_defid);
               data[len+1]:=tdef(s).defid shr 24 and $ff;
               data[len+2]:=tdef(s).defid shr 16 and $ff;
               data[len+3]:=tdef(s).defid shr 8 and $ff;
               data[len+4]:=tdef(s).defid and $ff;
               inc(len,5);
             end;
         end
        else
         begin
           { nil pointer }
           data[len]:=ord(deref_nil);
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
        pm     : tmodule;
        typ    : tdereftype;
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
        pm:=current_module;
        i:=0;
        while (i<len) do
          begin
            typ:=tdereftype(data[i]);
            inc(i);
            case typ of
              deref_unit :
                begin
                  idx:=(data[i] shl 8) or data[i+1];
                  inc(i,2);
                  pm:=current_module.resolve_unit(idx);
                end;
              deref_defid :
                begin
                  idx:=(data[i] shl 24) or (data[i+1] shl 16) or (data[i+2] shl 8) or data[i+3];
                  inc(i,4);
                  result:=tdef(pm.deflist[idx]);
                end;
              deref_symid :
                begin
                  idx:=(data[i] shl 24) or (data[i+1] shl 16) or (data[i+2] shl 8) or data[i+3];
                  inc(i,4);
                  result:=tsym(pm.symlist[idx]);
                end;
              deref_nil :
                begin
                  result:=nil;
                  { Only allowed when no other deref is available }
                  if len<>1 then
                    internalerror(200306232);
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
        p.moduleindex:=current_module.unit_index;
      end;


    procedure tcompilerppufile.getderef(var d:tderef);
      begin
        d.dataidx:=getlongint;
      end;


    function tcompilerppufile.getpropaccesslist:tpropaccesslist;
      var
        hderef : tderef;
        slt : tsltype;
        idx : longint;
        p   : tpropaccesslist;
      begin
        p:=tpropaccesslist.create;
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
                getderef(hderef);
                p.addsymderef(slt,hderef);
              end;
            sl_absolutetype,
            sl_typeconv :
              begin
                getderef(hderef);
                p.addtypederef(slt,hderef);
              end;
            sl_vec :
              begin
                idx:=getlongint;
                getderef(hderef);
                p.addconstderef(slt,idx,hderef);
              end;
            else
              internalerror(200110204);
          end;
        until false;
        getpropaccesslist:=tpropaccesslist(p);
      end;


    function  tcompilerppufile.getasmsymbol:tasmsymbol;
      begin
        getlongint;
        getasmsymbol:=nil;
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


    procedure tcompilerppufile.putpropaccesslist(p:tpropaccesslist);
      var
        hp : ppropaccesslistitem;
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
             sl_absolutetype,
             sl_typeconv :
               putderef(hp^.defderef);
             sl_vec :
               begin
                 putlongint(hp^.value);
                 putderef(hp^.valuedefderef);
               end;
             else
              internalerror(200110205);
           end;
           hp:=hp^.next;
         end;
        putbyte(byte(sl_none));
      end;


    procedure tcompilerppufile.putasmsymbol(s:tasmsymbol);
      begin
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
