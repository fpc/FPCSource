{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl, Pierre Muller

    Implementation for the symbols types of the symtable

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
unit symsym;

{$i fpcdefs.inc}

interface

    uses
       { common }
       cutils,
       { target }
       cpuinfo,globtype,
       { symtable }
       symconst,symbase,symtype,symdef,
       { ppu }
       ppu,symppu,
{$ifdef var_notification}
       cclasses,symnot,
{$endif}
       { aasm }
       aasmbase,aasmtai,cpubase,
       globals
       ;

    type
{************************************************
                   TSym
************************************************}

       { this object is the base for all symbol objects }
       tstoredsym = class(tsym)
       protected
          _mangledname : pstring;
       public
          refs          : longint;
          lastref,
          defref,
          lastwritten : tref;
          refcount    : longint;
{$ifdef GDB}
          isstabwritten : boolean;
{$endif GDB}
          constructor create(const n : string);
          constructor loadsym(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);virtual;abstract;
          procedure writesym(ppufile:tcompilerppufile);
          procedure deref;override;
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);virtual;
          function  write_references(ppufile:tcompilerppufile;locals:boolean):boolean;virtual;
          function  is_visible_for_proc(currprocdef:tprocdef):boolean;
          function  is_visible_for_object(currobjdef:tobjectdef):boolean;
          function  mangledname : string;
          procedure generate_mangledname;virtual;abstract;
       end;

       tlabelsym = class(tstoredsym)
          lab     : tasmlabel;
          used,
          defined : boolean;
          code : pointer; { should be tnode }
          constructor create(const n : string; l : tasmlabel);
          destructor destroy;override;
          constructor ppuload(ppufile:tcompilerppufile);
          procedure generate_mangledname;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       tunitsym = class(tstoredsym)
          unitsymtable : tsymtable;
          prevsym      : tunitsym;
          constructor create(const n : string;ref : tsymtable);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure restoreunitsym;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       terrorsym = class(tstoredsym)
          constructor create;
       end;

       Tprocdefcallback = procedure(p:Tprocdef;arg:pointer);

       tprocsym = class(tstoredsym)
       protected
          pdlistfirst,
          pdlistlast   : pprocdeflist; { linked list of overloaded procdefs }
          function getprocdef(nr:cardinal):Tprocdef;
       public
          procdef_count : byte;
          is_global : boolean;
          overloadchecked : boolean;
          overloadcount : word;    { amount of overloaded functions in this module }
          property procdef[nr:cardinal]:Tprocdef read getprocdef;
          constructor create(const n : string);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          { writes all declarations except the specified one }
          procedure write_parameter_lists(skipdef:tprocdef);
          { tests, if all procedures definitions are defined and not }
          { only forward                                             }
          procedure check_forward;
          procedure unchain_overload;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          procedure addprocdef(p:tprocdef);
          procedure add_para_match_to(Aprocsym:Tprocsym);
          procedure concat_procdefs_to(s:Tprocsym);
          procedure foreach_procdef_static(proc2call:Tprocdefcallback;arg:pointer);
          function first_procdef:Tprocdef;
          function last_procdef:Tprocdef;
          function search_procdef_nopara_boolret:Tprocdef;
          function search_procdef_bytype(pt:Tproctypeoption):Tprocdef;
          function search_procdef_bypara(params:Tparalinkedlist;
                                         allowconvert,
                                         allowdefault:boolean):Tprocdef;
          function search_procdef_byprocvardef(d:Tprocvardef):Tprocdef;
          function search_procdef_by1paradef(firstpara:Tdef):Tprocdef;
          function search_procdef_assignment_operator(fromdef,todef:tdef):Tprocdef;
          function search_procdef_binary_operator(def1,def2:tdef):Tprocdef;
          function  write_references(ppufile:tcompilerppufile;locals:boolean):boolean;override;
{$ifdef GDB}
          function stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       ttypesym = class(tstoredsym)
          restype    : ttype;
{$ifdef GDB}
          isusedinstab : boolean;
{$endif GDB}
          constructor create(const n : string;const tt : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          function  gettypedef:tdef;override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          function  write_references(ppufile:tcompilerppufile;locals:boolean):boolean;override;
{$ifdef GDB}
          function stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tvarsym = class(tstoredsym)
          address       : longint;
          localvarsym   : tvarsym;
          vartype       : ttype;
          varoptions    : tvaroptions;
          reg           : tregister; { if reg<>R_NO, then the variable is an register variable }
          varspez       : tvarspez;  { sets the type of access }
          varstate      : tvarstate;
          paraitem      : tparaitem;
{$ifdef var_notification}
          notifications : Tlinkedlist;
{$endif}
          constructor create(const n : string;const tt : ttype);
          constructor create_dll(const n : string;const tt : ttype);
          constructor create_C(const n,mangled : string;const tt : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          procedure generate_mangledname;override;
          procedure set_mangledname(const s:string);
          function  getsize : longint;
          function  getvaluesize : longint;
{$ifdef var_notification}
          function register_notification(flags:Tnotification_flags;
                                         callback:Tnotification_callback):cardinal;
{$endif}
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tpropertysym = class(tstoredsym)
          propoptions   : tpropertyoptions;
          propoverriden : tpropertysym;
          proptype,
          indextype     : ttype;
          index,
          default       : longint;
          readaccess,
          writeaccess,
          storedaccess  : tsymlist;
          constructor create(const n : string);
          destructor  destroy;override;
          constructor ppuload(ppufile:tcompilerppufile);
          function  getsize : longint;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  gettypedef:tdef;override;
          procedure deref;override;
          procedure dooverride(overriden:tpropertysym);
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tfuncretsym = class(tstoredsym)
          returntype    : ttype;
          address       : longint;
          funcretstate  : tvarstate;
          constructor create(const n : string;const tt : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tabsolutesym = class(tvarsym)
          abstyp  : absolutetyp;
          absseg  : boolean;
          ref     : tstoredsym;
          asmname : pstring;
          constructor create(const n : string;const tt : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure deref;override;
          function  mangledname : string;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       ttypedconstsym = class(tstoredsym)
          typedconsttype  : ttype;
          is_writable     : boolean;
          constructor create(const n : string;p : tdef;writable : boolean);
          constructor createtype(const n : string;const tt : ttype;writable : boolean);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure generate_mangledname;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          function  getsize:longint;
{$ifdef GDB}
          function  stabstring : pchar;override;
{$endif GDB}
       end;

       tconstvalue = record
         case integer of
         0: (valueord : tconstexprint);
         1: (valueordptr : tconstptruint);
         2: (valueptr : pointer; len : longint);
       end;

       tconstsym = class(tstoredsym)
          consttype   : ttype;
          consttyp    : tconsttyp;
          value : tconstvalue;
          resstrindex  : longint;     { needed for resource strings }
          constructor create_ord(const n : string;t : tconsttyp;v : tconstexprint);
          constructor create_ord_typed(const n : string;t : tconsttyp;v : tconstexprint;const tt:ttype);
          constructor create_ordptr_typed(const n : string;t : tconsttyp;v : tconstptruint;const tt:ttype);
          constructor create_ptr(const n : string;t : tconsttyp;v : pointer);
          constructor create_ptr_typed(const n : string;t : tconsttyp;v : pointer;const tt:ttype);
          constructor create_string(const n : string;t : tconsttyp;str:pchar;l:longint);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          function  mangledname : string;
          procedure deref;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tenumsym = class(tstoredsym)
          value      : longint;
          definition : tenumdef;
          nextenum   : tenumsym;
          constructor create(const n : string;def : tenumdef;v : longint);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure deref;override;
          procedure order;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tsyssym = class(tstoredsym)
          number : longint;
          constructor create(const n : string;l : longint);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       { compiler generated symbol to point to rtti and init/finalize tables }
       trttisym = class(tstoredsym)
          lab     : tasmsymbol;
          rttityp : trttitype;
          constructor create(const n:string;rt:trttitype);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  mangledname:string;
          function  get_label:tasmsymbol;
       end;

       { register variables }
       pregvarinfo = ^tregvarinfo;
       tregvarinfo = record
          regvars : array[1..maxvarregs] of tvarsym;
          regvars_para : array[1..maxvarregs] of boolean;
          regvars_refs : array[1..maxvarregs] of longint;

          fpuregvars : array[1..maxfpuvarregs] of tvarsym;
          fpuregvars_para : array[1..maxfpuvarregs] of boolean;
          fpuregvars_refs : array[1..maxfpuvarregs] of longint;
       end;


    var
       aktprocsym : tprocsym;      { pointer to the symbol for the
                                     currently be parsed procedure }
       aktprocdef : tprocdef;

       aktcallprocdef : tabstractprocdef;  { pointer to the definition of the
                                             currently called procedure,
                                             only set/unset in ncal }

       generrorsym : tsym;

       otsym : tvarsym;

    const
       current_object_option : tsymoptions = [sp_public];

    { rtti and init/final }
    procedure generate_rtti(p:tsym);
    procedure generate_inittable(p:tsym);



implementation

    uses
{$ifdef Delphi}
       sysutils,
{$else Delphi}
       strings,
{$endif Delphi}
       { global }
       verbose,
       { target }
       systems,
       { symtable }
       symtable,defutil,defcmp,
{$ifdef GDB}
       gdb,
{$endif GDB}
       { tree }
       node,
       { aasm }
       aasmcpu,
       { module }
       fmodule,
       { codegen }
       paramgr,cgbase,cresstr
       ;

{****************************************************************************
                               Helpers
****************************************************************************}

{****************************************************************************
                          TSYM (base for all symtypes)
****************************************************************************}

    constructor tstoredsym.create(const n : string);
      begin
         inherited create(n);
         symoptions:=current_object_option;
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
         fileinfo:=akttokenpos;
         defref:=nil;
         refs:=0;
         lastwritten:=nil;
         refcount:=0;
         if (cs_browser in aktmoduleswitches) and make_ref then
          begin
            defref:=tref.create(defref,@akttokenpos);
            inc(refcount);
          end;
         lastref:=defref;
         _mangledname:=nil;
      end;


    constructor tstoredsym.loadsym(ppufile:tcompilerppufile);
      var
        s  : string;
        nr : word;
      begin
         nr:=ppufile.getword;
         s:=ppufile.getstring;
         inherited create(s);
         { force the correct indexnr. must be after create! }
         indexnr:=nr;
         ppufile.getsmallset(symoptions);
         ppufile.getposinfo(fileinfo);
         lastref:=nil;
         defref:=nil;
         refs:=0;
         lastwritten:=nil;
         refcount:=0;
         _mangledname:=nil;
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
      end;


    procedure tstoredsym.deref;
      begin
      end;


    procedure tstoredsym.load_references(ppufile:tcompilerppufile;locals:boolean);
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

    function tstoredsym.write_references(ppufile:tcompilerppufile;locals:boolean):boolean;
      var
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
                     ppufile.putderef(self);
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


    destructor tstoredsym.destroy;
      begin
        if assigned(_mangledname) then
         begin
{$ifdef MEMDEBUG}
           memmanglednames.start;
{$endif MEMDEBUG}
           stringdispose(_mangledname);
{$ifdef MEMDEBUG}
           memmanglednames.stop;
{$endif MEMDEBUG}
         end;
        if assigned(defref) then
         begin
{$ifdef MEMDEBUG}
           membrowser.start;
{$endif MEMDEBUG}
           defref.freechain;
           defref.free;
{$ifdef MEMDEBUG}
           membrowser.stop;
{$endif MEMDEBUG}
         end;
        inherited destroy;
      end;


    procedure tstoredsym.writesym(ppufile:tcompilerppufile);
      begin
         ppufile.putword(indexnr);
         ppufile.putstring(_realname^);
         ppufile.putsmallset(symoptions);
         ppufile.putposinfo(fileinfo);
      end;


{$ifdef GDB}
    function tstoredsym.stabstring : pchar;

      begin
         stabstring:=strpnew('"'+name+'",'+tostr(N_LSYM)+',0,'+
           tostr(fileinfo.line)+',0');
      end;

    procedure tstoredsym.concatstabto(asmlist : taasmoutput);
      var
        stab_str : pchar;
      begin
         if not isstabwritten then
           begin
              stab_str := stabstring;
              { count_dbx(stab_str); moved to GDB.PAS }
              asmList.concat(Tai_stabs.Create(stab_str));
              isstabwritten:=true;
          end;
    end;
{$endif GDB}


    function tstoredsym.is_visible_for_proc(currprocdef:tprocdef):boolean;
      begin
        is_visible_for_proc:=false;

        { private symbols are allowed when we are in the same
          module as they are defined }
        if (sp_private in symoptions) and
           (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
           (owner.defowner.owner.unitid<>0) then
          exit;

        { protected symbols are vissible in the module that defines them and
          also visible to related objects }
        if (sp_protected in symoptions) and
           (
            (
             (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
             (owner.defowner.owner.unitid<>0)
            ) and
            not(
                assigned(currprocdef) and
                assigned(currprocdef._class) and
                currprocdef._class.is_related(tobjectdef(owner.defowner))
               )
           ) then
          exit;

        is_visible_for_proc:=true;
      end;


    function tstoredsym.is_visible_for_object(currobjdef:tobjectdef):boolean;
      begin
        is_visible_for_object:=false;

        { private symbols are allowed when we are in the same
          module as they are defined }
        if (sp_private in symoptions) and
           (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
           (owner.defowner.owner.unitid<>0) then
          exit;

        { protected symbols are vissible in the module that defines them and
          also visible to related objects }
        if (sp_protected in symoptions) and
           (
            (
             (owner.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
             (owner.defowner.owner.unitid<>0)
            ) and
            not(
                assigned(currobjdef) and
                currobjdef.is_related(tobjectdef(owner.defowner))
               )
           ) then
          exit;

        is_visible_for_object:=true;
      end;


    function tstoredsym.mangledname : string;
      begin
        if not assigned(_mangledname) then
         begin
           generate_mangledname;
           if not assigned(_mangledname) then
            internalerror(200204171);
         end;
        mangledname:=_mangledname^
      end;


{****************************************************************************
                                 TLABELSYM
****************************************************************************}

    constructor tlabelsym.create(const n : string; l : tasmlabel);

      begin
         inherited create(n);
         typ:=labelsym;
         lab:=l;
         used:=false;
         defined:=false;
         code:=nil;
      end;

    constructor tlabelsym.ppuload(ppufile:tcompilerppufile);

      begin
         inherited loadsym(ppufile);
         typ:=labelsym;
         { this is all dummy
           it is only used for local browsing }
         lab:=nil;
         code:=nil;
         used:=false;
         defined:=true;
      end;

    destructor tlabelsym.destroy;

      begin
         inherited destroy;
      end;


    procedure tlabelsym.generate_mangledname;
      begin
        _mangledname:=stringdup(lab.name);
      end;


    procedure tlabelsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         if owner.symtabletype=globalsymtable then
           Message(sym_e_ill_label_decl)
         else
           begin
              inherited writesym(ppufile);
              ppufile.writeentry(iblabelsym);
           end;
      end;


{****************************************************************************
                                  TUNITSYM
****************************************************************************}

    constructor tunitsym.create(const n : string;ref : tsymtable);
      var
        old_make_ref : boolean;
      begin
         old_make_ref:=make_ref;
         make_ref:=false;
         inherited create(n);
         make_ref:=old_make_ref;
         typ:=unitsym;
         unitsymtable:=ref;
         if assigned(ref) and
            (ref.symtabletype=globalsymtable) then
          begin
            prevsym:=tglobalsymtable(ref).unitsym;
            tglobalsymtable(ref).unitsym:=self;
          end;
      end;

    constructor tunitsym.ppuload(ppufile:tcompilerppufile);

      begin
         inherited loadsym(ppufile);
         typ:=unitsym;
         unitsymtable:=nil;
         prevsym:=nil;
         refs:=0;
      end;

    { we need to remove it from the prevsym chain ! }

    procedure tunitsym.restoreunitsym;
      var pus,ppus : tunitsym;
      begin
         if assigned(unitsymtable) and
            (unitsymtable.symtabletype=globalsymtable) then
           begin
             ppus:=nil;
             pus:=tglobalsymtable(unitsymtable).unitsym;
             if pus=self then
               tglobalsymtable(unitsymtable).unitsym:=prevsym
             else while assigned(pus) do
               begin
                  if pus=self then
                    begin
                       ppus.prevsym:=prevsym;
                       break;
                    end
                  else
                    begin
                       ppus:=pus;
                       pus:=ppus.prevsym;
                    end;
               end;
           end;
         unitsymtable:=nil;
         prevsym:=nil;
      end;

    destructor tunitsym.destroy;
      begin
         restoreunitsym;
         inherited destroy;
      end;

    procedure tunitsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.writeentry(ibunitsym);
      end;

{$ifdef GDB}
    procedure tunitsym.concatstabto(asmlist : taasmoutput);
      begin
      {Nothing to write to stabs !}
      end;
{$endif GDB}

{****************************************************************************
                                  TPROCSYM
****************************************************************************}

    constructor tprocsym.create(const n : string);

      begin
         inherited create(n);
         typ:=procsym;
         pdlistfirst:=nil;
         pdlistlast:=nil;
         owner:=nil;
         is_global:=false;
         overloadchecked:=false;
         overloadcount:=0;
         procdef_count:=0;
      end;


    constructor tprocsym.ppuload(ppufile:tcompilerppufile);
      var
         pd : tprocdef;
      begin
         inherited loadsym(ppufile);
         typ:=procsym;
         pdlistfirst:=nil;
         pdlistlast:=nil;
         procdef_count:=0;
         repeat
           pd:=tprocdef(ppufile.getderef);
           if pd=nil then
            break;
           addprocdef(pd);
         until false;
         is_global:=false;
         overloadchecked:=false;
         overloadcount:=$ffff; { invalid, not used anymore }
      end;


    destructor tprocsym.destroy;
      var
         hp,p : pprocdeflist;
      begin
         p:=pdlistfirst;
         while assigned(p) do
           begin
              hp:=p^.next;
              dispose(p);
              p:=hp;
           end;
         inherited destroy;
      end;


    procedure tprocsym.write_parameter_lists(skipdef:tprocdef);
      var
         p : pprocdeflist;
      begin
         p:=pdlistfirst;
         while assigned(p) do
           begin
              if p^.def<>skipdef then
                MessagePos1(p^.def.fileinfo,sym_b_param_list,p^.def.fullprocname);
              p:=p^.next;
           end;
      end;


    procedure tprocsym.check_forward;
      var
         p : pprocdeflist;
      begin
         p:=pdlistfirst;
         while assigned(p) do
           begin
              if (p^.def.procsym=self) and
                 (p^.def.forwarddef) then
                begin
                   MessagePos1(p^.def.fileinfo,sym_e_forward_not_resolved,p^.def.fullprocname);
                   { Turn futher error messages off }
                   p^.def.forwarddef:=false;
                end;
              p:=p^.next;
           end;
      end;


    procedure tprocsym.deref;
      var
         p : pprocdeflist;
      begin
         p:=pdlistfirst;
         while assigned(p) do
           begin
             resolvedef(pointer(p^.def));
             p:=p^.next;
           end;
      end;


    procedure tprocsym.addprocdef(p:tprocdef);
      var
        pd : pprocdeflist;
      begin
        new(pd);
        pd^.def:=p;
        pd^.next:=nil;
        { Add at end of list to keep always
          a correct order, also after loading from ppu }
        if assigned(pdlistlast) then
         begin
           pdlistlast^.next:=pd;
           pdlistlast:=pd;
         end
        else
         begin
           pdlistfirst:=pd;
           pdlistlast:=pd;
         end;
        inc(procdef_count);
      end;


    function Tprocsym.getprocdef(nr:cardinal):Tprocdef;
      var
        i : cardinal;
        pd : Pprocdeflist;
      begin
        pd:=pdlistfirst;
        for i:=2 to nr do
          begin
            if not assigned(pd) then
              internalerror(200209051);
            pd:=pd^.next;
          end;
        getprocdef:=pd^.def;
      end;


    procedure Tprocsym.add_para_match_to(Aprocsym:Tprocsym);
      var
        pd:Pprocdeflist;
      begin
        pd:=pdlistfirst;
        while assigned(pd) do
          begin
            if Aprocsym.search_procdef_bypara(pd^.def.para,false,true)=nil then
              Aprocsym.addprocdef(pd^.def);
            pd:=pd^.next;
          end;
      end;


    procedure Tprocsym.concat_procdefs_to(s:Tprocsym);
      var
        pd : Pprocdeflist;
      begin
        pd:=pdlistfirst;
        while assigned(pd) do
         begin
           s.addprocdef(pd^.def);
           pd:=pd^.next;
         end;
      end;


    function Tprocsym.first_procdef:Tprocdef;
      begin
        if assigned(pdlistfirst) then
          first_procdef:=pdlistfirst^.def
        else
          first_procdef:=nil;
      end;


    function Tprocsym.last_procdef:Tprocdef;
      begin
        if assigned(pdlistlast) then
          last_procdef:=pdlistlast^.def
        else
          last_procdef:=nil;
      end;


    procedure Tprocsym.foreach_procdef_static(proc2call:Tprocdefcallback;arg:pointer);
      var
        p : Pprocdeflist;
      begin
        p:=pdlistfirst;
        while assigned(p) do
         begin
           proc2call(p^.def,arg);
           p:=p^.next;
         end;
      end;


    function Tprocsym.search_procdef_nopara_boolret:Tprocdef;
      var
        p : Pprocdeflist;
      begin
        search_procdef_nopara_boolret:=nil;
        p:=pdlistfirst;
        while p<>nil do
         begin
           if p^.def.para.empty and is_boolean(p^.def.rettype.def) then
            begin
              search_procdef_nopara_boolret:=p^.def;
              break;
            end;
           p:=p^.next;
         end;
      end;


    function Tprocsym.search_procdef_bytype(pt:Tproctypeoption):Tprocdef;
      var
        p : Pprocdeflist;
      begin
        search_procdef_bytype:=nil;
        p:=pdlistfirst;
        while p<>nil do
         begin
           if p^.def.proctypeoption=pt then
            begin
              search_procdef_bytype:=p^.def;
              break;
            end;
           p:=p^.next;
         end;
      end;


    function Tprocsym.search_procdef_bypara(params:Tparalinkedlist;
                                            allowconvert,
                                            allowdefault:boolean):Tprocdef;
      var
        pd : Pprocdeflist;
        eq : tequaltype;
      begin
        search_procdef_bypara:=nil;
        pd:=pdlistfirst;
        while assigned(pd) do
         begin
           eq:=compare_paras(pd^.def.para,params,cp_value_equal_const,allowdefault);
           if (eq>=te_equal) or
              (allowconvert and (eq>te_incompatible)) then
             begin
               search_procdef_bypara:=pd^.def;
               break;
             end;
           pd:=pd^.next;
         end;
      end;

    function Tprocsym.search_procdef_byprocvardef(d:Tprocvardef):Tprocdef;
      var
        pd : Pprocdeflist;
        eq,besteq : tequaltype;
        bestpd : tprocdef;
      begin
        { This function will return the pprocdef of pprocsym that
          is the best match for procvardef. When there are multiple
          matches it returns nil.}
        search_procdef_byprocvardef:=nil;
        bestpd:=nil;
        besteq:=te_incompatible;
        pd:=pdlistfirst;
        while assigned(pd) do
         begin
           eq:=proc_to_procvar_equal(pd^.def,d);
           if eq>=te_equal then
            begin
              { multiple procvars with the same equal level }
              if assigned(bestpd) and
                 (besteq=eq) then
                exit;
              if eq>besteq then
               begin
                 besteq:=eq;
                 bestpd:=pd^.def;
               end;
            end;
           pd:=pd^.next;
         end;
        search_procdef_byprocvardef:=bestpd;
      end;


    function Tprocsym.search_procdef_by1paradef(firstpara:Tdef):Tprocdef;
      var
        pd:Pprocdeflist;
      begin
        search_procdef_by1paradef:=nil;
        pd:=pdlistfirst;
        while assigned(pd) do
          begin
            if equal_defs(Tparaitem(pd^.def.para.first).paratype.def,firstpara) and
               (Tparaitem(pd^.def.para.first).next=nil) then
              begin
                search_procdef_by1paradef:=pd^.def;
                break;
              end;
            pd:=pd^.next;
          end;
      end;


    function Tprocsym.search_procdef_assignment_operator(fromdef,todef:tdef):Tprocdef;
      var
        convtyp : tconverttype;
        pd : pprocdeflist;
        bestpd : tprocdef;
        eq,
        besteq : tequaltype;
        hpd : tprocdef;
      begin
        search_procdef_assignment_operator:=nil;
        bestpd:=nil;
        besteq:=te_incompatible;
        pd:=pdlistfirst;
        while assigned(pd) do
          begin
            if equal_defs(todef,pd^.def.rettype.def) then
             begin
               eq:=compare_defs_ext(fromdef,Tparaitem(pd^.def.para.first).paratype.def,
                                    nothingn,false,false,convtyp,hpd);
               if eq=te_exact then
                begin
                  search_procdef_assignment_operator:=pd^.def;
                  exit;
                end;
               if eq>besteq then
                begin
                  bestpd:=pd^.def;
                  besteq:=eq;
                end;
              end;
            pd:=pd^.next;
          end;
        search_procdef_assignment_operator:=bestpd;
      end;


    function Tprocsym.search_procdef_binary_operator(def1,def2:tdef):Tprocdef;
      var
        convtyp : tconverttype;
        pd : pprocdeflist;
        bestpd : tprocdef;
        eq1,eq2 : tequaltype;
        eqlev,
        bestlev : byte;
        hpd : tprocdef;
      begin
        search_procdef_binary_operator:=nil;
        bestpd:=nil;
        bestlev:=0;
        pd:=pdlistfirst;
        while assigned(pd) do
          begin
            { Compare def1 with the last para (right to left order) }
            eq1:=compare_defs_ext(def1,Tparaitem(pd^.def.para.last).paratype.def,
                                 nothingn,false,false,convtyp,hpd);
            if eq1<>te_incompatible then
             begin
               { Compare def2 with the first para (right to left order) }
               eq2:=compare_defs_ext(def2,Tparaitem(pd^.def.para.first).paratype.def,
                                    nothingn,false,false,convtyp,hpd);
               if eq2<>te_incompatible then
                begin
                  eqlev:=byte(eq1)+byte(eq2);
                  if eqlev=(byte(te_exact)+byte(te_exact)) then
                   begin
                     search_procdef_binary_operator:=pd^.def;
                     exit;
                   end;
                  if eqlev>bestlev then
                   begin
                     bestpd:=pd^.def;
                     bestlev:=eqlev;
                   end;
                end;
             end;
            pd:=pd^.next;
          end;
        search_procdef_binary_operator:=bestpd;
      end;


    procedure tprocsym.ppuwrite(ppufile:tcompilerppufile);
      var
         p : pprocdeflist;
      begin
         inherited writesym(ppufile);
         p:=pdlistfirst;
         while assigned(p) do
           begin
             { only write the proc definitions that belong
               to this procsym }
             if (p^.def.procsym=self) then
              ppufile.putderef(p^.def);
             p:=p^.next;
           end;
         ppufile.putderef(nil);
         ppufile.writeentry(ibprocsym);
      end;


    function tprocsym.write_references(ppufile:tcompilerppufile;locals:boolean) : boolean;
      var
        p : pprocdeflist;
      begin
         write_references:=false;
         if not inherited write_references(ppufile,locals) then
           exit;
         write_references:=true;
         p:=pdlistfirst;
         while assigned(p) do
           begin
              if (p^.def.procsym=self) then
                p^.def.write_references(ppufile,locals);
              p:=p^.next;
           end;
      end;


    procedure tprocsym.unchain_overload;
      var
         p,hp : pprocdeflist;
      begin
         { remove all overloaded procdefs from the
           procdeflist that are not in the current symtable }
         p:=pdlistfirst;
         { reset new lists }
         pdlistfirst:=nil;
         pdlistlast:=nil;
         while assigned(p) do
           begin
              hp:=p^.next;
              if (p^.def.procsym=self) then
                begin
                  { keep, add to list }
                  if assigned(pdlistlast) then
                   begin
                     pdlistlast^.next:=p;
                     pdlistlast:=p;
                   end
                  else
                   begin
                     pdlistfirst:=p;
                     pdlistlast:=p;
                   end;
                  p^.next:=nil;
                end
              else
                begin
                  { remove }
                  dispose(p);
                  dec(procdef_count);
                end;
              p:=hp;
           end;
      end;


{$ifdef GDB}
    function tprocsym.stabstring : pchar;
      begin
        internalerror(200111171);
        stabstring:=nil;
      end;

    procedure tprocsym.concatstabto(asmlist : taasmoutput);
    begin
      internalerror(200111172);
    end;
{$endif GDB}


{****************************************************************************
                                  TERRORSYM
****************************************************************************}

    constructor terrorsym.create;
      begin
        inherited create('');
        typ:=errorsym;
      end;

{****************************************************************************
                                TPROPERTYSYM
****************************************************************************}

    constructor tpropertysym.create(const n : string);
      begin
         inherited create(n);
         typ:=propertysym;
         propoptions:=[];
         index:=0;
         default:=0;
         proptype.reset;
         indextype.reset;
         readaccess:=tsymlist.create;
         writeaccess:=tsymlist.create;
         storedaccess:=tsymlist.create;
      end;


    constructor tpropertysym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         typ:=propertysym;
         ppufile.getsmallset(propoptions);
         if (ppo_is_override in propoptions) then
          begin
            propoverriden:=tpropertysym(ppufile.getderef);
            { we need to have these objects initialized }
            readaccess:=tsymlist.create;
            writeaccess:=tsymlist.create;
            storedaccess:=tsymlist.create;
          end
         else
          begin
            ppufile.gettype(proptype);
            index:=ppufile.getlongint;
            default:=ppufile.getlongint;
            ppufile.gettype(indextype);
            readaccess:=ppufile.getsymlist;
            writeaccess:=ppufile.getsymlist;
            storedaccess:=ppufile.getsymlist;
          end;
      end;


    destructor tpropertysym.destroy;
      begin
         readaccess.free;
         writeaccess.free;
         storedaccess.free;
         inherited destroy;
      end;

    function tpropertysym.gettypedef:tdef;
      begin
        gettypedef:=proptype.def;
      end;

    procedure tpropertysym.deref;
      begin
        if (ppo_is_override in propoptions) then
         begin
           resolvesym(pointer(propoverriden));
           dooverride(propoverriden);
         end
        else
         begin
           proptype.resolve;
           indextype.resolve;
           readaccess.resolve;
           writeaccess.resolve;
           storedaccess.resolve;
         end;
      end;


    function tpropertysym.getsize : longint;
      begin
         getsize:=0;
      end;


    procedure tpropertysym.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited writesym(ppufile);
        ppufile.putsmallset(propoptions);
        if (ppo_is_override in propoptions) then
         ppufile.putderef(propoverriden)
        else
         begin
           ppufile.puttype(proptype);
           ppufile.putlongint(index);
           ppufile.putlongint(default);
           ppufile.puttype(indextype);
           ppufile.putsymlist(readaccess);
           ppufile.putsymlist(writeaccess);
           ppufile.putsymlist(storedaccess);
         end;
        ppufile.writeentry(ibpropertysym);
      end;


    procedure tpropertysym.dooverride(overriden:tpropertysym);
      begin
        propoverriden:=overriden;
        proptype:=overriden.proptype;
        propoptions:=overriden.propoptions+[ppo_is_override];
        index:=overriden.index;
        default:=overriden.default;
        indextype:=overriden.indextype;
        readaccess.free;
        readaccess:=overriden.readaccess.getcopy;
        writeaccess.free;
        writeaccess:=overriden.writeaccess.getcopy;
        storedaccess.free;
        storedaccess:=overriden.storedaccess.getcopy;
      end;


{$ifdef GDB}
    function tpropertysym.stabstring : pchar;
      begin
         { !!!! don't know how to handle }
         stabstring:=strpnew('');
      end;

    procedure tpropertysym.concatstabto(asmlist : taasmoutput);
      begin
         { !!!! don't know how to handle }
      end;
{$endif GDB}

{****************************************************************************
                                  TFUNCRETSYM
****************************************************************************}

    constructor tfuncretsym.create(const n : string;const tt:ttype);

      begin
         inherited create(n);
         typ:=funcretsym;
         returntype:=tt;
         funcretstate:=vs_declared;
         { address valid for ret in param only }
         { otherwise set by insert             }
         address:=procinfo.return_offset;
      end;

    constructor tfuncretsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         ppufile.gettype(returntype);
         address:=ppufile.getlongint;
         typ:=funcretsym;
      end;

    destructor tfuncretsym.destroy;
      begin
        inherited destroy;
      end;

    procedure tfuncretsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.puttype(returntype);
         ppufile.putlongint(address);
         ppufile.writeentry(ibfuncretsym);
         funcretstate:=vs_used;
      end;

    procedure tfuncretsym.deref;
      begin
         returntype.resolve;
      end;

{$ifdef GDB}
    procedure tfuncretsym.concatstabto(asmlist : taasmoutput);
      begin
        { Nothing to do here, it is done in genexitcode  }
      end;
{$endif GDB}

{****************************************************************************
                                  TABSOLUTESYM
****************************************************************************}

    constructor tabsolutesym.create(const n : string;const tt : ttype);
      begin
        inherited create(n,tt);
        typ:=absolutesym;
      end;


    constructor tabsolutesym.ppuload(ppufile:tcompilerppufile);
      begin
         { Note: This needs to load everything of tvarsym.write }
         inherited ppuload(ppufile);
         { load absolute }
         typ:=absolutesym;
         ref:=nil;
         address:=0;
         asmname:=nil;
         abstyp:=absolutetyp(ppufile.getbyte);
         absseg:=false;
         case abstyp of
           tovar :
             asmname:=stringdup(ppufile.getstring);
           toasm :
             asmname:=stringdup(ppufile.getstring);
           toaddr :
             begin
               address:=ppufile.getlongint;
               absseg:=boolean(ppufile.getbyte);
             end;
         end;
      end;


    procedure tabsolutesym.ppuwrite(ppufile:tcompilerppufile);
      var
        hvo : tvaroptions;
      begin
         { Note: This needs to write everything of tvarsym.write }
         inherited writesym(ppufile);
         ppufile.putbyte(byte(varspez));
         ppufile.putlongint(address);
         { write only definition or definitionsym }
         ppufile.puttype(vartype);
         hvo:=varoptions-[vo_regable];
         ppufile.putsmallset(hvo);
         ppufile.putbyte(byte(abstyp));
         case abstyp of
           tovar :
             ppufile.putstring(ref.name);
           toasm :
             ppufile.putstring(asmname^);
           toaddr :
             begin
               ppufile.putlongint(address);
               ppufile.putbyte(byte(absseg));
             end;
         end;
        ppufile.writeentry(ibabsolutesym);
      end;


    procedure tabsolutesym.deref;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
         { inheritance of varsym.deref ! }
         vartype.resolve;
         { own absolute deref }
         if (abstyp=tovar) and (asmname<>nil) then
           begin
              { search previous loaded symtables }
              searchsym(asmname^,srsym,srsymtable);
              if not assigned(srsym) then
               srsym:=searchsymonlyin(owner,asmname^);
              if not assigned(srsym) then
               srsym:=generrorsym;
              ref:=tstoredsym(srsym);
              stringdispose(asmname);
           end;
      end;


    function tabsolutesym.mangledname : string;
      begin
         case abstyp of
           tovar :
             begin
               case ref.typ of
                 varsym :
                   mangledname:=tvarsym(ref).mangledname;
                 else
                   internalerror(200111011);
               end;
             end;
           toasm :
             mangledname:=asmname^;
           toaddr :
             mangledname:='$'+tostr(address);
         else
           internalerror(10002);
         end;
      end;


{$ifdef GDB}
    procedure tabsolutesym.concatstabto(asmlist : taasmoutput);
      begin
      { I don't know how to handle this !! }
      end;
{$endif GDB}


{****************************************************************************
                                  TVARSYM
****************************************************************************}

    constructor tvarsym.create(const n : string;const tt : ttype);
      begin
         inherited create(n);
         typ:=varsym;
         vartype:=tt;
         _mangledname:=nil;
         varspez:=vs_value;
         address:=0;
         localvarsym:=nil;
         refs:=0;
         varstate:=vs_used;
         varoptions:=[];
         { can we load the value into a register ? }
         if tstoreddef(tt.def).is_intregable then
           include(varoptions,vo_regable)
         else
           exclude(varoptions,vo_regable);

         if tstoreddef(tt.def).is_fpuregable then
           include(varoptions,vo_fpuregable)
         else
           exclude(varoptions,vo_fpuregable);
         reg:=R_NO;
      end;


    constructor tvarsym.create_dll(const n : string;const tt : ttype);
      begin
         tvarsym(self).create(n,tt);
         include(varoptions,vo_is_dll_var);
      end;


    constructor tvarsym.create_C(const n,mangled : string;const tt : ttype);
      begin
         tvarsym(self).create(n,tt);
         stringdispose(_mangledname);
         _mangledname:=stringdup(mangled);
      end;


    constructor tvarsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         typ:=varsym;
         reg:=R_NO;
         refs := 0;
         varstate:=vs_used;
         varspez:=tvarspez(ppufile.getbyte);
         address:=ppufile.getlongint;
         localvarsym:=nil;
         ppufile.gettype(vartype);
         ppufile.getsmallset(varoptions);
         if (vo_is_C_var in varoptions) then
           _mangledname:=stringdup(ppufile.getstring);
      end;


    destructor tvarsym.destroy;
      begin
      {$ifdef var_notification}
        if assigned(notifications) then
          notifications.destroy;
      {$endif}
        inherited destroy;
      end;


    procedure tvarsym.deref;
      begin
        vartype.resolve;
      end;


    procedure tvarsym.ppuwrite(ppufile:tcompilerppufile);
      var
        hvo : tvaroptions;
      begin
         inherited writesym(ppufile);
         ppufile.putbyte(byte(varspez));
         ppufile.putlongint(address);
         ppufile.puttype(vartype);
         { symbols which are load are never candidates for a register,
           turn off the regable }
         hvo:=varoptions-[vo_regable,vo_fpuregable];
         ppufile.putsmallset(hvo);
         if (vo_is_C_var in varoptions) then
           ppufile.putstring(mangledname);
         ppufile.writeentry(ibvarsym);
      end;


    procedure tvarsym.generate_mangledname;
      begin
        _mangledname:=stringdup(mangledname_prefix('U',owner)+name);
      end;


    procedure tvarsym.set_mangledname(const s:string);
      begin
        stringdispose(_mangledname);
        _mangledname:=stringdup(s);
      end;


    function tvarsym.getsize : longint;
      begin
        if assigned(vartype.def) then
          getsize:=vartype.def.size
        else
          getsize:=0;
      end;


    function tvarsym.getvaluesize : longint;
      begin
        if assigned(vartype.def) and
           (varspez=vs_value) and
           ((vartype.def.deftype<>arraydef) or
            tarraydef(vartype.def).isDynamicArray or
            (tarraydef(vartype.def).highrange>=tarraydef(vartype.def).lowrange)) then
          getvaluesize:=vartype.def.size
        else
          getvaluesize:=0;
      end;


{$ifdef var_notification}
    function Tvarsym.register_notification(flags:Tnotification_flags;callback:
                                           Tnotification_callback):cardinal;

    var n:Tnotification;

    begin
      if not assigned(notifications) then
        notifications:=Tlinkedlist.create;
      n:=Tnotification.create(flags,callback);
      register_notification:=n.id;
      notifications.concat(n);
    end;
{$endif}

{$ifdef GDB}
    function tvarsym.stabstring : pchar;
     var
       st : string;
     begin
       st:=tstoreddef(vartype.def).numberstring;
       if (owner.symtabletype = objectsymtable) and
          (sp_static in symoptions) then
         begin
            if (cs_gdb_gsym in aktglobalswitches) then st := 'G'+st else st := 'S'+st;
            stabstring := strpnew('"'+owner.name^+'__'+name+':'+st+
                     '",'+
                     tostr(N_LCSYM)+',0,'+tostr(fileinfo.line)+','+mangledname);
         end
       else if (owner.symtabletype = globalsymtable) then
         begin
            { Here we used S instead of
              because with G GDB doesn't look at the address field
              but searches the same name or with a leading underscore
              but these names don't exist in pascal !}
            if (cs_gdb_gsym in aktglobalswitches) then st := 'G'+st else st := 'S'+st;
            stabstring := strpnew('"'+name+':'+st+'",'+
                     tostr(N_LCSYM)+',0,'+tostr(fileinfo.line)+','+mangledname);
         end
       else if owner.symtabletype = staticsymtable then
         begin
            stabstring := strpnew('"'+name+':S'+st+'",'+
                  tostr(N_LCSYM)+',0,'+tostr(fileinfo.line)+','+mangledname);
         end
       else if (owner.symtabletype in [parasymtable,inlineparasymtable]) then
         begin
            case varspez of
               vs_out,
               vs_var   : st := 'v'+st;
               vs_value,
               vs_const : if paramanager.push_addr_param(vartype.def,tprocdef(owner.defowner).proccalloption) then
                            st := 'v'+st { should be 'i' but 'i' doesn't work }
                          else
                            st := 'p'+st;
              end;
            stabstring := strpnew('"'+name+':'+st+'",'+
                  tostr(N_tsym)+',0,'+tostr(fileinfo.line)+','+
                  tostr(address+owner.address_fixup));
                  {offset to ebp => will not work if the framepointer is esp
                  so some optimizing will make things harder to debug }
         end
       else if (owner.symtabletype in [localsymtable,inlinelocalsymtable]) then
         if reg<>R_NO then
           begin
              { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
              { this is the register order for GDB}
              stabstring:=strpnew('"'+name+':r'+st+'",'+
                        tostr(N_RSYM)+',0,'+
                        tostr(fileinfo.line)+','+tostr(stab_regindex[reg]));
           end
         else
           { I don't know if this will work (PM) }
           if (vo_is_C_var in varoptions) then
            stabstring := strpnew('"'+name+':S'+st+'",'+
                  tostr(N_LCSYM)+',0,'+tostr(fileinfo.line)+','+mangledname)
           else
           stabstring := strpnew('"'+name+':'+st+'",'+
                  tostr(N_LSYM)+',0,'+tostr(fileinfo.line)+',-'+tostr(address-owner.address_fixup))
       else
         stabstring := inherited stabstring;
  end;

    procedure tvarsym.concatstabto(asmlist : taasmoutput);
      var stab_str : pchar;
      begin
         if (owner.symtabletype in [parasymtable,inlineparasymtable]) and
            (copy(name,1,6)='hidden') then
           exit;
         inherited concatstabto(asmlist);
      if (owner.symtabletype=parasymtable) and
         (reg<>R_NO) then
           begin
           { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
           { this is the register order for GDB}
              stab_str:=strpnew('"'+name+':r'
                     +tstoreddef(vartype.def).numberstring+'",'+
                     tostr(N_RSYM)+',0,'+
                     tostr(fileinfo.line)+','+tostr(stab_regindex[reg]));
              asmList.concat(Tai_stabs.Create(stab_str));
           end;
      end;
{$endif GDB}


{****************************************************************************
                             TTYPEDCONSTSYM
*****************************************************************************}

    constructor ttypedconstsym.create(const n : string;p : tdef;writable : boolean);
      begin
         inherited create(n);
         typ:=typedconstsym;
         typedconsttype.setdef(p);
         is_writable:=writable;
      end;


    constructor ttypedconstsym.createtype(const n : string;const tt : ttype;writable : boolean);
      begin
         inherited create(n);
         typ:=typedconstsym;
         typedconsttype:=tt;
         is_writable:=writable;
      end;


    constructor ttypedconstsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         typ:=typedconstsym;
         ppufile.gettype(typedconsttype);
         is_writable:=boolean(ppufile.getbyte);
      end;


    destructor ttypedconstsym.destroy;
      begin
         inherited destroy;
      end;


    procedure ttypedconstsym.generate_mangledname;
      begin
        _mangledname:=stringdup(mangledname_prefix('TC',owner)+name);
      end;


    function ttypedconstsym.getsize : longint;
      begin
        if assigned(typedconsttype.def) then
         getsize:=typedconsttype.def.size
        else
         getsize:=0;
      end;


    procedure ttypedconstsym.deref;
      begin
        typedconsttype.resolve;
      end;


    procedure ttypedconstsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.puttype(typedconsttype);
         ppufile.putbyte(byte(is_writable));
         ppufile.writeentry(ibtypedconstsym);
      end;


{$ifdef GDB}
    function ttypedconstsym.stabstring : pchar;
    var
      st : char;
    begin
    if (cs_gdb_gsym in aktglobalswitches) and (owner.symtabletype=globalsymtable) then
      st := 'G'
    else
      st := 'S';
    stabstring := strpnew('"'+name+':'+st+
            tstoreddef(typedconsttype.def).numberstring+'",'+tostr(n_STSYM)+',0,'+
            tostr(fileinfo.line)+','+mangledname);
    end;
{$endif GDB}


{****************************************************************************
                                  TCONSTSYM
****************************************************************************}

    constructor tconstsym.create_ord(const n : string;t : tconsttyp;v : TConstExprInt);
      begin
         inherited create(n);
         fillchar(value, sizeof(value), #0);
         typ:=constsym;
         consttyp:=t;
         value.valueord:=v;
         ResStrIndex:=0;
         consttype.reset;
      end;


    constructor tconstsym.create_ord_typed(const n : string;t : tconsttyp;v : tconstexprint;const tt:ttype);
      begin
         inherited create(n);
         fillchar(value, sizeof(value), #0);
         typ:=constsym;
         consttyp:=t;
         value.valueord:=v;
         ResStrIndex:=0;
         consttype:=tt;
      end;


    constructor tconstsym.create_ordptr_typed(const n : string;t : tconsttyp;v : tconstptruint;const tt:ttype);
      begin
         inherited create(n);
         fillchar(value, sizeof(value), #0);
         typ:=constsym;
         consttyp:=t;
         value.valueordptr:=v;
         ResStrIndex:=0;
         consttype:=tt;
      end;


    constructor tconstsym.create_ptr(const n : string;t : tconsttyp;v : pointer);
      begin
         inherited create(n);
         fillchar(value, sizeof(value), #0);
         typ:=constsym;
         consttyp:=t;
         value.valueptr:=v;
         ResStrIndex:=0;
         consttype.reset;
      end;


    constructor tconstsym.create_ptr_typed(const n : string;t : tconsttyp;v : pointer;const tt:ttype);
      begin
         inherited create(n);
         fillchar(value, sizeof(value), #0);
         typ:=constsym;
         consttyp:=t;
         value.valueptr:=v;
         ResStrIndex:=0;
         consttype:=tt;
      end;


    constructor tconstsym.create_string(const n : string;t : tconsttyp;str:pchar;l:longint);
      begin
         inherited create(n);
         fillchar(value, sizeof(value), #0);
         typ:=constsym;
         consttyp:=t;
         value.valueptr:=str;
         consttype.reset;
         value.len:=l;
         if t=constresourcestring then
           ResStrIndex:=ResourceStrings.Register(name,pchar(value.valueptr),value.len);
      end;


    constructor tconstsym.ppuload(ppufile:tcompilerppufile);
      var
         pd : pbestreal;
         ps : pnormalset;
         pc : pchar;
      begin
         inherited loadsym(ppufile);
         typ:=constsym;
         consttype.reset;
         consttyp:=tconsttyp(ppufile.getbyte);
         fillchar(value, sizeof(value), #0);
         case consttyp of
           constint:
             value.valueord:=ppufile.getexprint;
           constwchar,
           constbool,
           constchar :
             value.valueord:=ppufile.getlongint;
           constord :
             begin
               ppufile.gettype(consttype);
               value.valueord:=ppufile.getexprint;
             end;
           constpointer :
             begin
               ppufile.gettype(consttype);
               value.valueordptr:=ppufile.getptruint;
             end;
           conststring,
           constresourcestring :
             begin
               value.len:=ppufile.getlongint;
               getmem(pc,value.len+1);
               ppufile.getdata(pc^,value.len);
               if consttyp=constresourcestring then
                 ResStrIndex:=ppufile.getlongint;
               value.valueptr:=pc;
             end;
           constreal :
             begin
               new(pd);
               pd^:=ppufile.getreal;
               value.valueptr:=pd;
             end;
           constset :
             begin
               ppufile.gettype(consttype);
               new(ps);
               ppufile.getnormalset(ps^);
               value.valueptr:=ps;
             end;
           constguid :
             begin
               new(pguid(value.valueptr));
               ppufile.getdata(value.valueptr^,sizeof(tguid));
             end;
           constnil : ;
           else
             Message1(unit_f_ppu_invalid_entry,tostr(ord(consttyp)));
         end;
      end;


    destructor tconstsym.destroy;
      begin
        case consttyp of
          conststring,
          constresourcestring :
            freemem(pchar(value.valueptr),value.len+1);
          constreal :
            dispose(pbestreal(value.valueptr));
          constset :
            dispose(pnormalset(value.valueptr));
          constguid :
            dispose(pguid(value.valueptr));
        end;
        inherited destroy;
      end;


    function tconstsym.mangledname : string;
      begin
         mangledname:=name;
      end;


    procedure tconstsym.deref;
      begin
        if consttyp in [constord,constpointer,constset] then
         consttype.resolve;
      end;


    procedure tconstsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.putbyte(byte(consttyp));
         case consttyp of
           constnil : ;
           constint:
             ppufile.putexprint(value.valueord);
           constbool,
           constchar :
             ppufile.putlongint(value.valueord);
           constord :
             begin
               ppufile.puttype(consttype);
               ppufile.putexprint(value.valueord);
             end;
           constpointer :
             begin
               ppufile.puttype(consttype);
               ppufile.putptruint(value.valueordptr);
             end;
           conststring,
           constresourcestring :
             begin
               ppufile.putlongint(value.len);
               ppufile.putdata(pchar(value.valueptr)^,value.len);
               if consttyp=constresourcestring then
                 ppufile.putlongint(ResStrIndex);
             end;
           constreal :
             ppufile.putreal(pbestreal(value.valueptr)^);
           constset :
             begin
               ppufile.puttype(consttype);
               ppufile.putnormalset(value.valueptr^);
             end;
           constguid :
             ppufile.putdata(value.valueptr^,sizeof(tguid));
         else
           internalerror(13);
         end;
        ppufile.writeentry(ibconstsym);
      end;

{$ifdef GDB}
    function tconstsym.stabstring : pchar;
    var st : string;
    begin
         {even GDB v4.16 only now 'i' 'r' and 'e' !!!}
         case consttyp of
            conststring : begin
                          st := 's'''+strpas(pchar(value.valueptr))+'''';
                          end;
            constbool,
            constint,
            constord,
            constchar : st := 'i'+int64tostr(value.valueord);
            constpointer :
              st := 'i'+int64tostr(value.valueordptr);
            constreal : begin
                        system.str(pbestreal(value.valueptr)^,st);
                        st := 'r'+st;
                        end;
         { if we don't know just put zero !! }
         else st:='i0';
            {***SETCONST}
            {constset:;}    {*** I don't know what to do with a set.}
         { sets are not recognized by GDB}
            {***}
        end;
    stabstring := strpnew('"'+name+':c='+st+'",'+tostr(N_function)+',0,'+
                    tostr(fileinfo.line)+',0');
    end;

    procedure tconstsym.concatstabto(asmlist : taasmoutput);
      begin
        if consttyp <> conststring then
          inherited concatstabto(asmlist);
      end;
{$endif GDB}


{****************************************************************************
                                  TENUMSYM
****************************************************************************}

    constructor tenumsym.create(const n : string;def : tenumdef;v : longint);
      begin
         inherited create(n);
         typ:=enumsym;
         definition:=def;
         value:=v;
         { check for jumps }
         if v>def.max+1 then
          def.has_jumps:=true;
         { update low and high }
         if def.min>v then
           def.setmin(v);
         if def.max<v then
           def.setmax(v);
         order;
      end;


    constructor tenumsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         typ:=enumsym;
         definition:=tenumdef(ppufile.getderef);
         value:=ppufile.getlongint;
         nextenum := Nil;
      end;


    procedure tenumsym.deref;
      begin
         resolvedef(pointer(definition));
         order;
      end;


   procedure tenumsym.order;
      var
         sym : tenumsym;
      begin
         sym := tenumsym(definition.firstenum);
         if sym = nil then
          begin
            definition.firstenum := self;
            nextenum := nil;
            exit;
          end;
         { reorder the symbols in increasing value }
         if value < sym.value then
          begin
            nextenum := sym;
            definition.firstenum := self;
          end
         else
          begin
            while (sym.value <= value) and assigned(sym.nextenum) do
             sym := sym.nextenum;
            nextenum := sym.nextenum;
            sym.nextenum := self;
          end;
      end;


    procedure tenumsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.putderef(definition);
         ppufile.putlongint(value);
         ppufile.writeentry(ibenumsym);
      end;


{$ifdef GDB}
    procedure tenumsym.concatstabto(asmlist : taasmoutput);
    begin
    {enum elements have no stab !}
    end;
{$EndIf GDB}


{****************************************************************************
                                  TTYPESYM
****************************************************************************}

    constructor ttypesym.create(const n : string;const tt : ttype);

      begin
         inherited create(n);
         typ:=typesym;
         restype:=tt;
{$ifdef GDB}
         isusedinstab := false;
{$endif GDB}
        { register the typesym for the definition }
        if assigned(restype.def) and
           (restype.def.deftype<>errordef) and
           not(assigned(restype.def.typesym)) then
         restype.def.typesym:=self;
      end;


    constructor ttypesym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         typ:=typesym;
{$ifdef GDB}
         isusedinstab := false;
{$endif GDB}
         ppufile.gettype(restype);
      end;


    function  ttypesym.gettypedef:tdef;
      begin
        gettypedef:=restype.def;
      end;


    procedure ttypesym.deref;
      begin
         restype.resolve;
      end;


    procedure ttypesym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.puttype(restype);
         ppufile.writeentry(ibtypesym);
      end;


    procedure ttypesym.load_references(ppufile:tcompilerppufile;locals:boolean);
      begin
         inherited load_references(ppufile,locals);
         if (restype.def.deftype=recorddef) then
           tstoredsymtable(trecorddef(restype.def).symtable).load_references(ppufile,locals);
         if (restype.def.deftype=objectdef) then
           tstoredsymtable(tobjectdef(restype.def).symtable).load_references(ppufile,locals);
      end;


    function ttypesym.write_references(ppufile:tcompilerppufile;locals:boolean):boolean;
      begin
        if not inherited write_references(ppufile,locals) then
         begin
         { write address of this symbol if record or object
           even if no real refs are there
           because we need it for the symtable }
           if (restype.def.deftype in [recorddef,objectdef]) then
            begin
              ppufile.putderef(self);
              ppufile.writeentry(ibsymref);
            end;
         end;
        write_references:=true;
        if (restype.def.deftype=recorddef) then
           tstoredsymtable(trecorddef(restype.def).symtable).write_references(ppufile,locals);
        if (restype.def.deftype=objectdef) then
           tstoredsymtable(tobjectdef(restype.def).symtable).write_references(ppufile,locals);
      end;


{$ifdef GDB}
    function ttypesym.stabstring : pchar;
    var
      stabchar : string[2];
      short : string;
    begin
      if restype.def.deftype in tagtypes then
        stabchar := 'Tt'
      else
        stabchar := 't';
      short := '"'+name+':'+stabchar+tstoreddef(restype.def).numberstring
               +'",'+tostr(N_LSYM)+',0,'+tostr(fileinfo.line)+',0';
      stabstring := strpnew(short);
    end;

    procedure ttypesym.concatstabto(asmlist : taasmoutput);
      begin
      {not stabs for forward defs }
      if assigned(restype.def) then
        if (restype.def.typesym = self) then
          tstoreddef(restype.def).concatstabto(asmlist)
        else
          inherited concatstabto(asmlist);
      end;
{$endif GDB}


{****************************************************************************
                                  TSYSSYM
****************************************************************************}

    constructor tsyssym.create(const n : string;l : longint);
      begin
         inherited create(n);
         typ:=syssym;
         number:=l;
      end;

    constructor tsyssym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         typ:=syssym;
         number:=ppufile.getlongint;
      end;

    destructor tsyssym.destroy;
      begin
        inherited destroy;
      end;

    procedure tsyssym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.putlongint(number);
         ppufile.writeentry(ibsyssym);
      end;

{$ifdef GDB}
    procedure tsyssym.concatstabto(asmlist : taasmoutput);
      begin
      end;
{$endif GDB}


{****************************************************************************
                                  TRTTISYM
****************************************************************************}

    constructor trttisym.create(const n:string;rt:trttitype);
      const
        prefix : array[trttitype] of string[5]=('$rtti','$init');
      begin
        inherited create(prefix[rt]+n);
        typ:=rttisym;
        lab:=nil;
        rttityp:=rt;
      end;


    constructor trttisym.ppuload(ppufile:tcompilerppufile);
      begin
        inherited loadsym(ppufile);
        typ:=rttisym;
        lab:=nil;
        rttityp:=trttitype(ppufile.getbyte);
      end;


    procedure trttisym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.putbyte(byte(rttityp));
         ppufile.writeentry(ibrttisym);
      end;


    function trttisym.mangledname : string;
      const
        prefix : array[trttitype] of string[5]=('RTTI_','INIT_');
      var
        s : string;
        p : tsymtable;
      begin
        s:='';
        p:=owner;
        while assigned(p) and (p.symtabletype=localsymtable) do
         begin
           s:=s+'_'+p.defowner.name;
           p:=p.defowner.owner;
         end;
        if not(p.symtabletype in [globalsymtable,staticsymtable]) then
         internalerror(200108265);
        mangledname:=prefix[rttityp]+p.name^+s+'$_'+Copy(name,5,255);
      end;


    function trttisym.get_label:tasmsymbol;
      begin
        { the label is always a global label }
        if not assigned(lab) then
         lab:=objectlibrary.newasmsymboltype(mangledname,AB_GLOBAL,AT_DATA);
        get_label:=lab;
      end;


    { persistent rtti generation }
    procedure generate_rtti(p:tsym);
      var
        rsym : trttisym;
        def  : tstoreddef;
      begin
        { rtti can only be generated for classes that are always typesyms }
        if not(p.typ=typesym) then
         internalerror(200108261);
        def:=tstoreddef(ttypesym(p).restype.def);
        { only create rtti once for each definition }
        if not(df_has_rttitable in def.defoptions) then
         begin
           { definition should be in the same symtable as the symbol }
           if p.owner<>def.owner then
            internalerror(200108262);
           { create rttisym }
           rsym:=trttisym.create(p.name,fullrtti);
           p.owner.insert(rsym);
           { register rttisym in definition }
           include(def.defoptions,df_has_rttitable);
           def.rttitablesym:=rsym;
           { write rtti data }
           def.write_child_rtti_data(fullrtti);
           if (cs_create_smart in aktmoduleswitches) then
            rttiList.concat(Tai_cut.Create);
           rttilist.concat(tai_align.create(const_align(pointer_size)));
           rttiList.concat(Tai_symbol.Create(rsym.get_label,0));
           def.write_rtti_data(fullrtti);
           rttiList.concat(Tai_symbol_end.Create(rsym.get_label));
         end;
      end;


    { persistent init table generation }
    procedure generate_inittable(p:tsym);
      var
        rsym : trttisym;
        def  : tstoreddef;
      begin
        { anonymous types are also allowed for records that can be varsym }
        case p.typ of
          typesym :
            def:=tstoreddef(ttypesym(p).restype.def);
          varsym :
            def:=tstoreddef(tvarsym(p).vartype.def);
          else
            internalerror(200108263);
        end;
        { only create inittable once for each definition }
        if not(df_has_inittable in def.defoptions) then
         begin
           { definition should be in the same symtable as the symbol }
           if p.owner<>def.owner then
            internalerror(200108264);
           { create rttisym }
           rsym:=trttisym.create(p.name,initrtti);
           p.owner.insert(rsym);
           { register rttisym in definition }
           include(def.defoptions,df_has_inittable);
           def.inittablesym:=rsym;
           { write inittable data }
           def.write_child_rtti_data(initrtti);
           if (cs_create_smart in aktmoduleswitches) then
            rttiList.concat(Tai_cut.Create);
           rttilist.concat(tai_align.create(const_align(pointer_size)));
           rttiList.concat(Tai_symbol.Create(rsym.get_label,0));
           def.write_rtti_data(initrtti);
           rttiList.concat(Tai_symbol_end.Create(rsym.get_label));
         end;
      end;



end.
{
  $Log$
  Revision 1.84  2002-12-20 16:02:22  peter
    * fix stupid copy&paste bug in binary operator search

  Revision 1.83  2002/12/16 22:08:31  peter
    * fix order of procdefs in procsym, procdefs are now always appended
      so that loading from a ppu will keep the same order. This is
      important for the generation of VMTs

  Revision 1.82  2002/12/11 22:39:23  peter
    * better error message when no operator is found for equal

  Revision 1.81  2002/12/07 14:27:10  carl
    * 3% memory optimization
    * changed some types
    + added type checking with different size for call node and for
       parameters

  Revision 1.80  2002/12/06 17:51:11  peter
    * merged cdecl and array fixes

  Revision 1.79  2002/11/27 20:04:10  peter
    * tvarsym.get_push_size replaced by paramanager.push_size

  Revision 1.78  2002/11/27 02:34:20  peter
    * only find real equal procvars

  Revision 1.77  2002/11/25 18:43:34  carl
   - removed the invalid if <> checking (Delphi is strange on this)
   + implemented abstract warning on instance creation of class with
      abstract methods.
   * some error message cleanups

  Revision 1.76  2002/11/25 17:43:26  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.75  2002/11/23 22:50:09  carl
    * some small speed optimizations
    + added several new warnings/hints

  Revision 1.74  2002/11/22 22:48:11  carl
  * memory optimization with tconstsym (1.5%)

  Revision 1.73  2002/11/18 17:31:59  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.72  2002/11/17 16:31:57  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.71  2002/11/09 15:30:07  carl
    + align RTTI tables

  Revision 1.70  2002/10/13 21:33:37  peter
    * give correct fileposition for undefined forward procs

  Revision 1.69  2002/10/05 12:43:29  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.68  2002/10/05 00:52:20  peter
    * split boolean check in two lines for easier debugging

  Revision 1.67  2002/09/26 12:04:53  florian
    + constsym with type=constguid can be written to ppu now,
      fixes web bug 1820

  Revision 1.66  2002/09/16 14:11:13  peter
    * add argument to equal_paras() to support default values or not

  Revision 1.65  2002/09/09 17:34:16  peter
    * tdicationary.replace added to replace and item in a dictionary. This
      is only allowed for the same name
    * varsyms are inserted in symtable before the types are parsed. This
      fixes the long standing "var longint : longint" bug
    - consume_idlist and idstringlist removed. The loops are inserted
      at the callers place and uses the symtable for duplicate id checking

  Revision 1.64  2002/09/08 11:10:17  carl
    * bugfix 2109 (bad imho, but only way)

  Revision 1.63  2002/09/07 18:17:41  florian
    + tvarsym.paraitem added

  Revision 1.62  2002/09/07 15:25:10  peter
    * old logs removed and tabs fixed

  Revision 1.61  2002/09/05 19:29:45  peter
    * memdebug enhancements

  Revision 1.60  2002/09/05 14:51:42  peter
    * internalerror instead of crash in getprocdef

  Revision 1.59  2002/09/03 16:26:27  daniel
    * Make Tprocdef.defs protected

  Revision 1.58  2002/09/01 08:01:16  daniel
   * Removed sets from Tcallnode.det_resulttype
   + Added read/write notifications of variables. These will be usefull
     for providing information for several optimizations. For example
     the value of the loop variable of a for loop does matter is the
     variable is read after the for loop, but if it's no longer used
     or written, it doesn't matter and this can be used to optimize
     the loop code generation.

  Revision 1.57  2002/08/25 19:25:21  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.56  2002/08/25 09:06:21  peter
    * fixed loop in concat_procdefs

  Revision 1.55  2002/08/20 16:54:40  peter
    * write address of varsym always

  Revision 1.54  2002/08/20 10:31:26  daniel
   * Tcallnode.det_resulttype rewritten

  Revision 1.53  2002/08/18 20:06:27  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.52  2002/08/17 09:23:42  florian
    * first part of procinfo rewrite

  Revision 1.51  2002/08/16 14:24:59  carl
    * issameref() to test if two references are the same (then emit no opcodes)
    + ret_in_reg to replace ret_in_acc
      (fix some register allocation bugs at the same time)
    + save_std_register now has an extra parameter which is the
      usedinproc registers

  Revision 1.50  2002/08/13 21:40:57  florian
    * more fixes for ppc calling conventions

  Revision 1.49  2002/08/12 15:08:40  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.48  2002/08/11 14:32:28  peter
    * renamed current_library to objectlibrary

  Revision 1.47  2002/08/11 13:24:14  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.46  2002/07/23 10:13:23  daniel
  * Added important comment

  Revision 1.45  2002/07/23 09:51:26  daniel
  * Tried to make Tprocsym.defs protected. I didn't succeed but the cleanups
    are worth comitting.

  Revision 1.44  2002/07/20 17:45:29  daniel
  * Register variables are now possible for global variables too. This is
    important for small programs without procedures.

  Revision 1.43  2002/07/20 11:57:58  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.42  2002/07/11 14:41:31  florian
    * start of the new generic parameter handling

}
