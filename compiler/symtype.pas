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
      cclasses,
      { global }
      globtype,globals,constexp,
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
                     TDef
************************************************}

      tgeTSymtable = (gs_none,gs_record,gs_local,gs_para);

      tdef = class(TDefEntry)
         typesym    : tsym;  { which type the definition was generated this def }
         { maybe it's useful to merge the dwarf and stabs debugging info with some hacking }
         { dwarf debugging }
         dwarf_lab : tasmsymbol;
         dwarf_ref_lab : tasmsymbol;
         { stabs debugging }
         stab_number : word;
         dbg_state   : tdefdbgstatus;
         defoptions  : tdefoptions;
         defstates   : tdefstates;
         constructor create(dt:tdeftyp);
         procedure buildderef;virtual;abstract;
         procedure buildderefimpl;virtual;abstract;
         procedure deref;virtual;abstract;
         procedure derefimpl;virtual;abstract;
         function  typename:string;
         function  GetTypeName:string;virtual;
         function  mangledparaname:string;
         function  getmangledparaname:string;virtual;
         function  rtti_mangledname(rt:trttitype):string;virtual;abstract;
         function  size:aint;virtual;abstract;
         function  packedbitsize:aint;virtual;
         function  alignment:shortint;virtual;abstract;
         function  getvardef:longint;virtual;abstract;
         function  getparentdef:tdef;virtual;
         function  geTSymtable(t:tgeTSymtable):TSymtable;virtual;
         function  is_publishable:boolean;virtual;abstract;
         function  needs_inittable:boolean;virtual;abstract;
         function  is_related(def:tdef):boolean;virtual;
         procedure ChangeOwner(st:TSymtable);
         procedure register_created_object_type;virtual;
      end;

{************************************************
                     TSym
************************************************}

      { this object is the base for all symbol objects }

      { tsym }

      tsym = class(TSymEntry)
      protected
      public
         fileinfo   : tfileposinfo;
         symoptions : tsymoptions;
         refs       : longint;
         reflist    : TLinkedList;
         visibility : tvisibility;
         { deprecated optionally can have a message }
         deprecatedmsg: pshortstring;
         isdbgwritten : boolean;
         constructor create(st:tsymtyp;const aname:string);
         destructor  destroy;override;
         function  mangledname:string; virtual;
         function  prettyname:string; virtual;
         procedure buildderef;virtual;
         procedure deref;virtual;
         procedure ChangeOwner(st:TSymtable);
         procedure IncRefCount;
         procedure IncRefCountBy(AValue : longint);
         procedure MaybeCreateRefList;
         procedure AddRef;
      end;

      tsymarr = array[0..maxlongint div sizeof(pointer)-1] of tsym;
      psymarr = ^tsymarr;

{************************************************
                   TDeref
************************************************}

      tderef = object
        dataidx : longint;
        procedure reset;
        procedure build(s:TObject);
        function  resolve:TObject;
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
         function  getexprint:Tconstexprint;
         function  getptruint:TConstPtrUInt;
         procedure getposinfo(var p:tfileposinfo);
         procedure getderef(var d:tderef);
         function  getpropaccesslist:tpropaccesslist;
         function  getasmsymbol:tasmsymbol;
         procedure putguid(const g: tguid);
         procedure putexprint(const v:tconstexprint);
         procedure PutPtrUInt(v:TConstPtrUInt);
         procedure putposinfo(const p:tfileposinfo);
         procedure putderef(const d:tderef);
         procedure putpropaccesslist(p:tpropaccesslist);
         procedure putasmsymbol(s:tasmsymbol);
       end;

{$ifdef MEMDEBUG}
    var
      memmanglednames,
      memprocpara,
      memprocparast,
      memproclocalst,
      memprocnodetree : tmemdebug;
{$endif MEMDEBUG}

    function  FindUnitSymtable(st:TSymtable):TSymtable;


implementation

    uses
       crefs,
       verbose,
       fmodule
       ;

{****************************************************************************
                                Utils
****************************************************************************}

    function FindUnitSymtable(st:TSymtable):TSymtable;
      begin
        result:=nil;
        repeat
          if not assigned(st) then
           internalerror(200602034);
          case st.symtabletype of
            localmacrosymtable,
            exportedmacrosymtable,
            staticsymtable,
            globalsymtable :
              begin
                result:=st;
                exit;
              end;
            recordsymtable,
            localsymtable,
            parasymtable,
            ObjectSymtable :
              st:=st.defowner.owner;
            else
              internalerror(200602035);
          end;
        until false;
      end;


{****************************************************************************
                                Tdef
****************************************************************************}

    constructor tdef.create(dt:tdeftyp);
      begin
         inherited create;
         typ:=dt;
         owner := nil;
         typesym := nil;
         defoptions:=[];
         dbg_state:=dbg_state_unused;
         stab_number:=0;
      end;


    function tdef.typename:string;
      begin
        if assigned(typesym) and
           not(typ in [procvardef,procdef]) and
           (typesym.realname[1]<>'$') then
          result:=typesym.realname
        else
          result:=GetTypeName;
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


    function tdef.geTSymtable(t:tgeTSymtable):TSymtable;
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


    procedure tdef.ChangeOwner(st:TSymtable);
      begin
//        if assigned(Owner) then
//          Owner.DefList.List[i]:=nil;
        Owner:=st;
        Owner.DefList.Add(self);
      end;


    procedure tdef.register_created_object_type;
      begin
      end;

{****************************************************************************
                          TSYM (base for all symtypes)
****************************************************************************}

    constructor tsym.create(st:tsymtyp;const aname:string);
      begin
         inherited CreateNotOwned;
         realname:=aname;
         typ:=st;
         RefList:=nil;
         symoptions:=[];
         fileinfo:=current_tokenpos;
         isdbgwritten := false;
         visibility:=vis_public;
         deprecatedmsg:=nil;
      end;

    destructor  Tsym.destroy;
      begin
        stringdispose(deprecatedmsg);
        if assigned(RefList) then
          RefList.Free;
        inherited Destroy;
      end;

    procedure Tsym.IncRefCount;
      begin
        inc(refs);
        if cs_browser in current_settings.moduleswitches then
          begin
            MaybeCreateRefList;
            AddRef;
          end;
      end;

    procedure Tsym.IncRefCountBy(AValue : longint);
      begin
        inc(refs,AValue);
      end;

    procedure Tsym.MaybeCreateRefList;
      begin
        if not assigned(reflist) then
          reflist:=TRefLinkedList.create;
      end;

    procedure Tsym.AddRef;
      var
        RefItem: TRefItem;
      begin
        RefItem:=TRefItem.Create(current_tokenpos);
        RefList.Concat(RefItem);
      end;

    procedure Tsym.buildderef;
      begin
      end;


    procedure Tsym.deref;
      begin
      end;


    function tsym.mangledname : string;
      begin
        internalerror(200204171);
        result:='';
      end;


    function tsym.prettyname : string;
      begin
        result:=realname;
      end;


    procedure tsym.ChangeOwner(st:TSymtable);
      begin
        Owner:=st;
        inherited ChangeOwner(Owner.SymList);
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


    procedure tderef.build(s:TObject);
      var
        len  : byte;
        st   : TSymtable;
        data : array[0..255] of byte;
        idx : word;
      begin
        { skip length byte }
        len:=1;

        if assigned(s) then
         begin
{ TODO: ugly hack}
           if s is tsym then
             st:=FindUnitSymtable(tsym(s).owner)
           else
             st:=FindUnitSymtable(tdef(s).owner);
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


    function tderef.resolve:TObject;
      var
        pm     : tmodule;
        typ    : tdereftype;
        idx    : longint;
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
                  idx:=longint((data[i] shl 24) or (data[i+1] shl 16) or (data[i+2] shl 8) or data[i+3]);
                  inc(i,4);
                  result:=tdef(pm.deflist[idx]);
                end;
              deref_symid :
                begin
                  idx:=longint((data[i] shl 24) or (data[i+1] shl 16) or (data[i+2] shl 8) or data[i+3]);
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
        longint(g.d1):=getlongint;
        g.d2:=getword;
        g.d3:=getword;
        getdata(g.d4,sizeof(g.d4));
      end;


    function tcompilerppufile.getexprint:Tconstexprint;

    begin
      getexprint.overflow:=false;
      getexprint.signed:=boolean(getbyte);
      getexprint.svalue:=getint64;
    end;


    function tcompilerppufile.getPtrUInt:TConstPtrUInt;
      begin
        {$if sizeof(TConstPtrUInt)=8}
          result:=tconstptruint(getint64);
        {$else}
          result:=TConstPtrUInt(getlongint);
        {$endif}
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
           info:=info or $1;
           { uncomment this code if tfileposinfo.fileindex type was changed
           if (p.fileindex<=$ffff) then
            info:=info or $1
           else
            if (p.fileindex<=$ffffff) then
             info:=info or $2
           else
            info:=info or $3;
           }
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
           info:=info or $10;
           { uncomment this code if tfileposinfo.column type was changed
           if (p.column<=$ffff) then
            info:=info or $10
           else
            if (p.column<=$ffffff) then
             info:=info or $20
           else
            info:=info or $30;
           }
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
        putlongint(longint(g.d1));
        putword(g.d2);
        putword(g.d3);
        putdata(g.d4,sizeof(g.d4));
      end;


    procedure Tcompilerppufile.putexprint(const v:Tconstexprint);

    begin
      if v.overflow then
        internalerror(200706102);
      putbyte(byte(v.signed));
      putint64(v.svalue);
    end;


    procedure tcompilerppufile.PutPtrUInt(v:TConstPtrUInt);
      begin
        {$if sizeof(TConstPtrUInt)=8}
          putint64(int64(v));
        {$else}
        {$if sizeof(TConstPtrUInt)=4}
          putlongint(longint(v));
        {$else}
          internalerror(2002082601);
        {$endif} {$endif}
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
                 putlongint(int64(hp^.value));
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
  memmanglednames.free;
  memprocpara.free;
  memprocparast.free;
  memproclocalst.free;
  memprocnodetree.free;
{$endif MEMDEBUG}

end.

