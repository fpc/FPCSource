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
       globtype,globals,
       { symtable }
       symconst,symbase,symtype,symdef,defcmp,
       { ppu }
       ppu,
       cclasses,symnot,
       { aasm }
       aasmbase,
       cpuinfo,cpubase,cgbase
       ;

    type
{************************************************
                   TSym
***********************************************}

       { this object is the base for all symbol objects }
       tstoredsym = class(tsym)
       protected
          _mangledname : pstring;
       public
          constructor create(const n : string);
          constructor loadsym(ppufile:tcompilerppufile);
          destructor destroy;override;
{$ifdef GDB}
          function  get_var_value(const s:string):string;
          function  stabstr_evaluate(const s:string;vars:array of string):Pchar;
{$endif GDB}
          function  mangledname : string;
          procedure generate_mangledname;virtual;abstract;
       end;

       tlabelsym = class(tstoredsym)
          lab     : tasmlabel;
          used,
          defined : boolean;
          code : pointer; { should be tnode }
          constructor create(const n : string; l : tasmlabel);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure generate_mangledname;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
{$ifdef GDB}
          function  stabstring : pchar;override;
{$endif GDB}
       end;

       tunitsym = class(Tsym)
          unitsymtable : tsymtable;
          constructor create(const n : string;ref : tsymtable);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       terrorsym = class(Tsym)
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
{$ifdef GDB}
          is_global : boolean;
{$endif GDB}
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
          procedure buildderef;override;
          procedure deref;override;
          procedure addprocdef(p:tprocdef);
          procedure addprocdef_deref(const d:tderef);
          procedure add_para_match_to(Aprocsym:Tprocsym;cpoptions:tcompare_paras_options);
          procedure concat_procdefs_to(s:Tprocsym);
          procedure foreach_procdef_static(proc2call:Tprocdefcallback;arg:pointer);
          function first_procdef:Tprocdef;
          function last_procdef:Tprocdef;
          function search_procdef_nopara_boolret:Tprocdef;
          function search_procdef_bytype(pt:Tproctypeoption):Tprocdef;
          function search_procdef_bypara(params:Tlinkedlist;
                                         retdef:tdef;
                                         cpoptions:tcompare_paras_options):Tprocdef;
          function search_procdef_byprocvardef(d:Tprocvardef):Tprocdef;
          function search_procdef_assignment_operator(fromdef,todef:tdef):Tprocdef;
          function  write_references(ppufile:tcompilerppufile;locals:boolean):boolean;override;
{$ifdef GDB}
          function stabstring : pchar;override;
{$endif GDB}
       end;

       ttypesym = class(Tsym)
          restype    : ttype;
{$ifdef GDB}
          isusedinstab : boolean;
{$endif GDB}
          constructor create(const n : string;const tt : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          function  gettypedef:tdef;override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          function  write_references(ppufile:tcompilerppufile;locals:boolean):boolean;override;
{$ifdef GDB}
          function stabstring : pchar;override;
{$endif GDB}
       end;

       tvarsym = class(tstoredsym)
          defaultconstsym : tsym;
          varoptions    : tvaroptions;
          varspez       : tvarspez;  { sets the type of access }
          varstate      : tvarstate;
          localloc      : tparalocation; { register/reference for local var }
          fieldoffset   : longint; { offset in record/object }
          paraitem      : tparaitem;
          notifications : Tlinkedlist;
          constructor create(const n : string;vsp:tvarspez;const tt : ttype);
          constructor create_dll(const n : string;vsp:tvarspez;const tt : ttype);
          constructor create_C(const n,mangled : string;vsp:tvarspez;const tt : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          procedure generate_mangledname;override;
          procedure set_mangledname(const s:string);
          function  getsize : longint;
          function  getvaluesize : longint;
          procedure trigger_notifications(what:Tnotification_flag);
          function register_notification(flags:Tnotification_flags;
                                         callback:Tnotification_callback):cardinal;
          procedure unregister_notification(id:cardinal);
{$ifdef GDB}
          function  stabstring : pchar;override;
{$endif GDB}
         private
          procedure setvartype(const newtype: ttype);
          _vartype       : ttype;
         public
          property vartype: ttype read _vartype write setvartype;
      end;

       tpropertysym = class(Tsym)
          propoptions   : tpropertyoptions;
          propoverriden : tpropertysym;
          propoverridenderef : tderef;
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
          procedure buildderef;override;
          procedure deref;override;
          procedure dooverride(overriden:tpropertysym);
       end;

       tabsolutesym = class(Tvarsym)
          abstyp  : absolutetyp;
{$ifdef i386}
          absseg  : boolean;
{$endif i386}
          asmname : pstring;
          ref     : tsymlist;
          constructor create(const n : string;const tt : ttype);
          constructor create_ref(const n : string;const tt : ttype;_ref:tsymlist);
          destructor  destroy;override;
          constructor ppuload(ppufile:tcompilerppufile);
          procedure buildderef;override;
          procedure deref;override;
          function  mangledname : string;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       {$ifdef gdb}
          function stabstring:Pchar;override;
       {$endif}
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
          procedure buildderef;override;
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
          value       : tconstvalue;
          resstrindex  : longint;     { needed for resource strings }
          constructor create_ord(const n : string;t : tconsttyp;v : tconstexprint);
          constructor create_ord_typed(const n : string;t : tconsttyp;v : tconstexprint;const tt:ttype);
          constructor create_ordptr_typed(const n : string;t : tconsttyp;v : tconstptruint;const tt:ttype);
          constructor create_ptr(const n : string;t : tconsttyp;v : pointer);
          constructor create_ptr_typed(const n : string;t : tconsttyp;v : pointer;const tt:ttype);
          constructor create_string(const n : string;t : tconsttyp;str:pchar;l:longint);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
{          function  mangledname : string;}
          procedure buildderef;override;
          procedure deref;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
{$ifdef GDB}
          function  stabstring : pchar;override;
{$endif GDB}
       end;

       tenumsym = class(Tsym)
          value      : longint;
          definition : tenumdef;
          definitionderef : tderef;
          nextenum   : tenumsym;
          constructor create(const n : string;def : tenumdef;v : longint);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          procedure order;
       end;

       tsyssym = class(Tsym)
          number : longint;
          constructor create(const n : string;l : longint);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
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

    var
       generrorsym : tsym;

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
       defutil,symtable,
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
       paramgr,cresstr,
       procinfo
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
         _mangledname:=nil;
      end;


    constructor tstoredsym.loadsym(ppufile:tcompilerppufile);
      var
        s  : string;
        nr : word;
      begin
         inherited loadsym(ppufile);
         _mangledname:=nil;
      end;

{    procedure tstoredsym.buildderef;
      begin
      end;


    procedure tstoredsym.deref;
      begin
      end;}


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

{$ifdef GDB}
    function Tstoredsym.get_var_value(const s:string):string;

    begin
      if s='mangledname' then
        get_var_value:=mangledname
      else
        get_var_value:=inherited get_var_value(s);
    end;

    function Tstoredsym.stabstr_evaluate(const s:string;vars:array of string):Pchar;

    begin
      stabstr_evaluate:=string_evaluate(s,@get_var_value,vars);
    end;
{$endif GDB}

    function tstoredsym.mangledname : string;

    begin
      if not assigned(_mangledname) then
        begin
          generate_mangledname;
          if not assigned(_mangledname) then
            internalerror(200204171);
        end;
   {$ifdef compress}
      mangledname:=minilzw_decode(_mangledname^);
   {$else}
      mangledname:=_mangledname^;
   {$endif}
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

    procedure tlabelsym.generate_mangledname;

    begin
    {$ifdef compress}
      _mangledname:=stringdup(minilzw_encode(lab.name));
    {$else}
      _mangledname:=stringdup(lab.name);
    {$endif}
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


{$ifdef GDB}
    function Tlabelsym.stabstring : pchar;
      begin
        stabstring:=stabstr_evaluate('"${name}",${N_LSYM},0,${line},0',[]);
      end;
{$endif GDB}


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
      end;

    constructor tunitsym.ppuload(ppufile:tcompilerppufile);

      begin
         inherited loadsym(ppufile);
         typ:=unitsym;
         unitsymtable:=nil;
         refs:=0;
      end;

    destructor tunitsym.destroy;
      begin
         inherited destroy;
      end;

    procedure tunitsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.writeentry(ibunitsym);
      end;

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
{$ifdef GDB}
         is_global:=false;
{$endif GDB}
         overloadchecked:=false;
         overloadcount:=0;
         procdef_count:=0;
      end;


    constructor tprocsym.ppuload(ppufile:tcompilerppufile);
      var
         pdderef : tderef;
         i,n : longint;
      begin
         inherited loadsym(ppufile);
         typ:=procsym;
         pdlistfirst:=nil;
         pdlistlast:=nil;
         procdef_count:=0;
         n:=ppufile.getword;
         for i:=1to n do
          begin
            ppufile.getderef(pdderef);
            addprocdef_deref(pdderef);
          end;
{$ifdef GDB}
         is_global:=false;
{$endif GDB}
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


    procedure tprocsym.ppuwrite(ppufile:tcompilerppufile);
      var
         p : pprocdeflist;
         n : word;
      begin
         inherited writesym(ppufile);
         { count procdefs }
         n:=0;
         p:=pdlistfirst;
         while assigned(p) do
           begin
             { only write the proc definitions that belong
               to this procsym and are in the global symtable }
             if p^.own and
                (p^.def.owner.symtabletype in [globalsymtable,objectsymtable]) then
               inc(n);
             p:=p^.next;
           end;
         ppufile.putword(n);
         { write procdefs }
         p:=pdlistfirst;
         while assigned(p) do
           begin
             { only write the proc definitions that belong
               to this procsym and are in the global symtable }
             if p^.own and
                (p^.def.owner.symtabletype in [globalsymtable,objectsymtable]) then
               ppufile.putderef(p^.defderef);
             p:=p^.next;
           end;
         ppufile.writeentry(ibprocsym);
      end;


    procedure tprocsym.write_parameter_lists(skipdef:tprocdef);
      var
         p : pprocdeflist;
      begin
         p:=pdlistfirst;
         while assigned(p) do
           begin
              if p^.def<>skipdef then
                MessagePos1(p^.def.fileinfo,sym_h_param_list,p^.def.fullprocname(false));
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
              if p^.own and
                 (p^.def.forwarddef) then
                begin
                   MessagePos1(p^.def.fileinfo,sym_e_forward_not_resolved,p^.def.fullprocname(false));
                   { Turn futher error messages off }
                   p^.def.forwarddef:=false;
                end;
              p:=p^.next;
           end;
      end;


    procedure tprocsym.buildderef;
      var
         p : pprocdeflist;
      begin
         p:=pdlistfirst;
         while assigned(p) do
           begin
             if p^.own then
               p^.defderef.build(p^.def);
             p:=p^.next;
           end;
      end;


    procedure tprocsym.deref;
      var
         p : pprocdeflist;
      begin
         { We have removed the overloaded entries, because they
           are not valid anymore and we can't deref them because
           the unit were they come from is not necessary in
           our uses clause (PFV) }
         unchain_overload;
         { Deref our own procdefs }
         p:=pdlistfirst;
         while assigned(p) do
           begin
             if not p^.own then
               internalerror(200310291);
             p^.def:=tprocdef(p^.defderef.resolve);
             p:=p^.next;
           end;
      end;


    procedure tprocsym.addprocdef(p:tprocdef);
      var
        pd : pprocdeflist;
      begin
        new(pd);
        pd^.def:=p;
        pd^.defderef.reset;
        pd^.next:=nil;
        pd^.own:=(pd^.def.procsym=self);
{        if not pd^.own then
          internalerror(2222222);}
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


    procedure tprocsym.addprocdef_deref(const d:tderef);
      var
        pd : pprocdeflist;
      begin
        new(pd);
        pd^.def:=nil;
        pd^.defderef:=d;
        pd^.next:=nil;
        pd^.own:=true;
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
        pd : pprocdeflist;
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


    procedure Tprocsym.add_para_match_to(Aprocsym:Tprocsym;cpoptions:tcompare_paras_options);
      var
        pd:pprocdeflist;
      begin
        pd:=pdlistfirst;
        while assigned(pd) do
          begin
            if Aprocsym.search_procdef_bypara(pd^.def.para,nil,cpoptions)=nil then
              Aprocsym.addprocdef(pd^.def);
            pd:=pd^.next;
          end;
      end;


    procedure Tprocsym.concat_procdefs_to(s:Tprocsym);
      var
        pd : pprocdeflist;
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
        p : pprocdeflist;
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
        p : pprocdeflist;
      begin
        search_procdef_nopara_boolret:=nil;
        p:=pdlistfirst;
        while p<>nil do
         begin
           if (p^.def.maxparacount=0) and
              is_boolean(p^.def.rettype.def) then
            begin
              search_procdef_nopara_boolret:=p^.def;
              break;
            end;
           p:=p^.next;
         end;
      end;


    function Tprocsym.search_procdef_bytype(pt:Tproctypeoption):Tprocdef;
      var
        p : pprocdeflist;
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


    function Tprocsym.search_procdef_bypara(params:Tlinkedlist;
                                            retdef:tdef;
                                            cpoptions:tcompare_paras_options):Tprocdef;
      var
        pd : pprocdeflist;
        eq : tequaltype;
      begin
        search_procdef_bypara:=nil;
        pd:=pdlistfirst;
        while assigned(pd) do
         begin
           if assigned(retdef) then
             eq:=compare_defs(retdef,pd^.def.rettype.def,nothingn)
           else
             eq:=te_equal;
           if (eq>=te_equal) or
              ((cpo_allowconvert in cpoptions) and (eq>te_incompatible)) then
            begin
              eq:=compare_paras(pd^.def.para,params,cp_value_equal_const,cpoptions);
              if (eq>=te_equal) or
                 ((cpo_allowconvert in cpoptions) and (eq>te_incompatible)) then
                begin
                  search_procdef_bypara:=pd^.def;
                  break;
                end;
            end;
           pd:=pd^.next;
         end;
      end;

    function Tprocsym.search_procdef_byprocvardef(d:Tprocvardef):Tprocdef;
      var
        pd : pprocdeflist;
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
           eq:=proc_to_procvar_equal(pd^.def,d,false);
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


    function Tprocsym.search_procdef_assignment_operator(fromdef,todef:tdef):Tprocdef;
      var
        convtyp : tconverttype;
        pd : pprocdeflist;
        bestpd : tprocdef;
        eq,
        besteq : tequaltype;
        hpd : tprocdef;
        currpara : tparaitem;
      begin
        result:=nil;
        bestpd:=nil;
        besteq:=te_incompatible;
        pd:=pdlistfirst;
        while assigned(pd) do
          begin
            if equal_defs(todef,pd^.def.rettype.def) then
             begin
               currpara:=Tparaitem(pd^.def.para.first);
               { ignore vs_hidden parameters }
               while assigned(currpara) and (currpara.is_hidden) do
                currpara:=tparaitem(currpara.next);
               if assigned(currpara) then
                begin
                  eq:=compare_defs_ext(fromdef,currpara.paratype.def,nothingn,convtyp,hpd,[]);
                  if eq=te_exact then
                   begin
                     result:=pd^.def;
                     exit;
                   end;
                  if eq>besteq then
                   begin
                     bestpd:=pd^.def;
                     besteq:=eq;
                   end;
                end;
             end;
            pd:=pd^.next;
          end;
        result:=bestpd;
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
              if p^.own then
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
         overloadchecked:=false;
         p:=pdlistfirst;
         { reset new lists }
         pdlistfirst:=nil;
         pdlistlast:=nil;
         while assigned(p) do
           begin
              hp:=p^.next;
              if p^.own then
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
            ppufile.getderef(propoverridenderef);
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


    procedure tpropertysym.buildderef;
      begin
        if (ppo_is_override in propoptions) then
         begin
           propoverridenderef.build(propoverriden);
         end
        else
         begin
           proptype.buildderef;
           indextype.buildderef;
           readaccess.buildderef;
           writeaccess.buildderef;
           storedaccess.buildderef;
         end;
      end;


    procedure tpropertysym.deref;
      begin
        if (ppo_is_override in propoptions) then
         begin
           propoverriden:=tpropertysym(propoverridenderef.resolve);
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
         ppufile.putderef(propoverridenderef)
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


{****************************************************************************
                                  TABSOLUTESYM
****************************************************************************}

    constructor tabsolutesym.create(const n : string;const tt : ttype);
      begin
        inherited create(n,vs_value,tt);
        typ:=absolutesym;
        ref:=nil;
      end;


    constructor tabsolutesym.create_ref(const n : string;const tt : ttype;_ref:tsymlist);
      begin
        inherited create(n,vs_value,tt);
        typ:=absolutesym;
        ref:=_ref;
      end;


    destructor tabsolutesym.destroy;
      begin
        if assigned(ref) then
          ref.free;
        inherited destroy;
      end;


    constructor tabsolutesym.ppuload(ppufile:tcompilerppufile);
      begin
         { Note: This needs to load everything of tvarsym.write }
         inherited ppuload(ppufile);
         { load absolute }
         typ:=absolutesym;
         ref:=nil;
         fieldoffset:=0;
         asmname:=nil;
         abstyp:=absolutetyp(ppufile.getbyte);
{$ifdef i386}
         absseg:=false;
{$endif i386}
         case abstyp of
           tovar :
             ref:=ppufile.getsymlist;
           toasm :
             asmname:=stringdup(ppufile.getstring);
           toaddr :
             begin
               fieldoffset:=ppufile.getlongint;
{$ifdef i386}
               absseg:=boolean(ppufile.getbyte);
{$endif i386}
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
         ppufile.putlongint(fieldoffset);
         { write only definition or definitionsym }
         ppufile.puttype(vartype);
         hvo:=varoptions-[vo_regable,vo_fpuregable];
         ppufile.putsmallset(hvo);
         ppufile.putbyte(byte(abstyp));
         case abstyp of
           tovar :
             ppufile.putsymlist(ref);
           toasm :
             ppufile.putstring(asmname^);
           toaddr :
             begin
               ppufile.putlongint(fieldoffset);
{$ifdef i386}
               ppufile.putbyte(byte(absseg));
{$endif i386}
             end;
         end;
        ppufile.writeentry(ibabsolutesym);
      end;


    procedure tabsolutesym.buildderef;
      begin
        { inheritance of varsym.deref ! }
        vartype.buildderef;
        if (abstyp=tovar) then
          ref.buildderef;
      end;


    procedure tabsolutesym.deref;
      begin
         { inheritance of varsym.deref ! }
         vartype.resolve;
         { own absolute deref }
         if (abstyp=tovar) then
           ref.resolve;
      end;


    function tabsolutesym.mangledname : string;
      begin
         case abstyp of
           toasm :
             mangledname:=asmname^;
           toaddr :
             mangledname:='$'+tostr(fieldoffset);
         else
           internalerror(10002);
         end;
      end;


{$ifdef GDB}
    function Tabsolutesym.stabstring:Pchar;

    begin
      stabstring:=nil;
    end;
{$endif GDB}


{****************************************************************************
                                  TVARSYM
****************************************************************************}

    constructor tvarsym.create(const n : string;vsp:tvarspez;const tt : ttype);
      begin
         inherited create(n);
         typ:=varsym;
         vartype:=tt;
         _mangledname:=nil;
         varspez:=vsp;
         fieldoffset:=0;
         fillchar(localloc,sizeof(localloc),0);
         defaultconstsym:=nil;
         refs:=0;
         varstate:=vs_declared;
         varoptions:=[];
      end;


    constructor tvarsym.create_dll(const n : string;vsp:tvarspez;const tt : ttype);
      begin
         tvarsym(self).create(n,vsp,tt);
         include(varoptions,vo_is_dll_var);
      end;


    constructor tvarsym.create_C(const n,mangled : string;vsp:tvarspez;const tt : ttype);
      begin
         tvarsym(self).create(n,vsp,tt);
         stringdispose(_mangledname);
       {$ifdef compress}
         _mangledname:=stringdup(minilzw_encode(mangled));
       {$else}
         _mangledname:=stringdup(mangled);
       {$endif}
      end;


    constructor tvarsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         typ:=varsym;
         fillchar(localloc,sizeof(localloc),0);
         refs := 0;
         varstate:=vs_used;
         varspez:=tvarspez(ppufile.getbyte);
         fieldoffset:=ppufile.getlongint;
         defaultconstsym:=nil;
         ppufile.gettype(_vartype);
         ppufile.getsmallset(varoptions);
         if (vo_is_C_var in varoptions) then
           _mangledname:=stringdup(ppufile.getstring);
      end;


    destructor tvarsym.destroy;
      begin
        if assigned(notifications) then
          notifications.destroy;
        inherited destroy;
      end;


    procedure tvarsym.buildderef;
      begin
        vartype.buildderef;
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
         ppufile.putlongint(fieldoffset);
         ppufile.puttype(vartype);
         { symbols which are load are never candidates for a register,
           turn off the regable }
         hvo:=varoptions-[vo_regable,vo_fpuregable];
         ppufile.putsmallset(hvo);
         if (vo_is_C_var in varoptions) then
           ppufile.putstring(_mangledname^);
         ppufile.writeentry(ibvarsym);
      end;


    procedure tvarsym.generate_mangledname;
      begin
      {$ifdef compress}
        _mangledname:=stringdup(minilzw_encode(make_mangledname('U',owner,name)));
      {$else}
        _mangledname:=stringdup(make_mangledname('U',owner,name));
      {$endif}
      end;


    procedure tvarsym.set_mangledname(const s:string);
      begin
        stringdispose(_mangledname);
      {$ifdef compress}
        _mangledname:=stringdup(minilzw_encode(s));
      {$else}
        _mangledname:=stringdup(s);
      {$endif}
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


    procedure Tvarsym.trigger_notifications(what:Tnotification_flag);

    var n:Tnotification;

    begin
        if assigned(notifications) then
          begin
            n:=Tnotification(notifications.first);
            while assigned(n) do
              begin
                if what in n.flags then
                  n.callback(what,self);
                n:=Tnotification(n.next);
              end;
          end;
    end;

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

    procedure Tvarsym.unregister_notification(id:cardinal);

    var n:Tnotification;

    begin
      if not assigned(notifications) then
        internalerror(200212311)
      else
        begin
            n:=Tnotification(notifications.first);
            while assigned(n) do
              begin
                if n.id=id then
                  begin
                    notifications.remove(n);
                    n.destroy;
                    exit;
                  end;
                n:=Tnotification(n.next);
              end;
            internalerror(200212311)
        end;
    end;

{$ifdef GDB}
    function Tvarsym.stabstring:Pchar;

    var st:string;
        threadvaroffset:string;
        regidx:Tregisterindex;
        c:char;
        loc: tcgloc;

    begin
      { set loc to LOC_REFERENCE to get somewhat usable debugging info for -Or }
      { while stabs aren't adapted for regvars yet                             }
      loc := localloc.loc;
      if (vo_is_self in varoptions) then
        begin
          case loc of
            LOC_REGISTER:
              regidx:=findreg_by_number(localloc.register);
            LOC_REFERENCE: ;
            else
              internalerror(2003091815);
          end;
          if (po_classmethod in current_procinfo.procdef.procoptions) or
             (po_staticmethod in current_procinfo.procdef.procoptions) then
            if (loc=LOC_REFERENCE) then
              stabstring:=stabstr_evaluate('"pvmt:p$1",${N_TSYM},0,0,$2',
                [Tstoreddef(pvmttype.def).numberstring,tostr(localloc.reference.offset)])
            else
              stabstring:=stabstr_evaluate('"pvmt:r$1",${N_RSYM},0,0,$2',
                [Tstoreddef(pvmttype.def).numberstring,tostr(regstabs_table[regidx])])
          else
            begin
              if not(is_class(current_procinfo.procdef._class)) then
                c:='v'
              else
                c:='p';
              if (loc=LOC_REFERENCE) then
                stabstring:=stabstr_evaluate('"$$t:$1",${N_TSYM},0,0,$2',
                      [c+current_procinfo.procdef._class.numberstring,tostr(localloc.reference.offset)])
              else
                stabstring:=stabstr_evaluate('"$$t:r$1",${N_RSYM},0,0,$2',
                      [c+current_procinfo.procdef._class.numberstring,tostr(regstabs_table[regidx])]);
            end;
        end
      else
        begin
          stabstring:=nil;
          st:=tstoreddef(vartype.def).numberstring;
          if (vo_is_thread_var in varoptions) then
            threadvaroffset:='+'+tostr(pointer_size)
          else
            threadvaroffset:='';

          case owner.symtabletype of
            objectsymtable:
              if (sp_static in symoptions) then
                begin
                  if (cs_gdb_gsym in aktglobalswitches) then
                    st:='G'+st
                  else
                    st:='S'+st;
                  stabstring:=stabstr_evaluate('"${ownername}__${name}:$1",${N_LCSYM},0,${line},${mangledname}$2',
                                               [st,threadvaroffset]);
                end;
            globalsymtable:
              begin
                { Here we used S instead of
                  because with G GDB doesn't look at the address field
                  but searches the same name or with a leading underscore
                  but these names don't exist in pascal !}
                if (cs_gdb_gsym in aktglobalswitches) then
                  st:='G'+st
                else
                  st:='S'+st;
                stabstring:=stabstr_evaluate('"${name}:$1",${N_LCSYM},0,${line},${mangledname}$2',[st,threadvaroffset]);
              end;
            staticsymtable :
              stabstring:=stabstr_evaluate('"${name}:S$1",${N_LCSYM},0,${line},${mangledname}$2',[st,threadvaroffset]);
            parasymtable,localsymtable:
              begin
                { There is no space allocated for not referenced locals }
                if (owner.symtabletype=localsymtable) and (refs=0) then
                  exit;

                if (vo_is_C_var in varoptions) then
                  begin
                    stabstring:=stabstr_evaluate('"${name}:S$1",${N_LCSYM},0,${line},${mangledname}',[st]);
                    exit;
                  end;
                if (owner.symtabletype=parasymtable) then
                  begin
                    if paramanager.push_addr_param(varspez,vartype.def,tprocdef(owner.defowner).proccalloption) and
                       not(vo_has_local_copy in varoptions) then
                      st := 'v'+st { should be 'i' but 'i' doesn't work }
                    else
                      st := 'p'+st;
                  end;
                case loc of
                  LOC_REGISTER, LOC_FPUREGISTER :
                    begin
                      regidx:=findreg_by_number(localloc.register);
                      { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
                      { this is the register order for GDB}
                      stabstring:=stabstr_evaluate('"${name}:r$1",${N_RSYM},0,${line},$2',[st,tostr(regstabs_table[regidx])]);
                    end;
                  LOC_REFERENCE :
                    { offset to ebp => will not work if the framepointer is esp
                      so some optimizing will make things harder to debug }
                    stabstring:=stabstr_evaluate('"${name}:$1",${N_TSYM},0,${line},$2',[st,tostr(localloc.reference.offset)])
                  else
                    internalerror(2003091814);
                end;
              end;
            else
              stabstring := inherited stabstring;
          end;
        end;
    end;
{$endif GDB}

    procedure tvarsym.setvartype(const newtype: ttype);
      begin
        _vartype := newtype;
         { can we load the value into a register ? }
        if not assigned(owner) or
           (owner.symtabletype in [localsymtable,parasymtable]) then
          begin
            if tstoreddef(vartype.def).is_intregable then
              include(varoptions,vo_regable)
            else
              exclude(varoptions,vo_regable);

            if tstoreddef(vartype.def).is_fpuregable then
              include(varoptions,vo_fpuregable)
            else
              exclude(varoptions,vo_fpuregable);
          end;
      end;


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
      {$ifdef compress}
        _mangledname:=stringdup(make_mangledname('TC',owner,name));
      {$else}
        _mangledname:=stringdup(make_mangledname('TC',owner,name));
      {$endif}
      end;


    function ttypedconstsym.getsize : longint;
      begin
        if assigned(typedconsttype.def) then
         getsize:=typedconsttype.def.size
        else
         getsize:=0;
      end;


    procedure ttypedconstsym.buildderef;
      begin
        typedconsttype.buildderef;
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

    var st:char;

    begin
      if (cs_gdb_gsym in aktglobalswitches) and (owner.symtabletype=globalsymtable) then
        st:='G'
      else
        st:='S';
      stabstring:=stabstr_evaluate('"${name}:$1$2",${N_STSYM},0,${line},${mangledname}',
                  [st,Tstoreddef(typedconsttype.def).numberstring]);
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


    procedure tconstsym.buildderef;
      begin
        if consttyp in [constord,constpointer,constset] then
         consttype.buildderef;
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
           constchar,
           constwchar :
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
    function Tconstsym.stabstring:Pchar;

    var st : string;

    begin
      {even GDB v4.16 only now 'i' 'r' and 'e' !!!}
      case consttyp of
        conststring:
          st:='s'''+backspace_quote(strpas(pchar(value.valueptr)),['''','"','\',#10,#13])+'''';
        constbool,
        constint,
        constord,
        constwchar,
        constchar:
          st:='i'+int64tostr(value.valueord);
        constpointer:
          st:='i'+int64tostr(value.valueordptr);
        constreal:
          begin
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
      stabstring:=stabstr_evaluate('"${name}:c=$1;",${N_FUNCTION},0,${line},0',[st]);
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
{         nextenum:=Tenumsym(def.firstenum);
         def.firstenum:=self;}
      end;


    constructor tenumsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         typ:=enumsym;
         ppufile.getderef(definitionderef);
         value:=ppufile.getlongint;
         nextenum := Nil;
      end;


    procedure tenumsym.buildderef;
      begin
         definitionderef.build(definition);
      end;


    procedure tenumsym.deref;
      begin
         definition:=tenumdef(definitionderef.resolve);
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
         ppufile.putderef(definitionderef);
         ppufile.putlongint(value);
         ppufile.writeentry(ibenumsym);
      end;


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


    procedure ttypesym.buildderef;
      begin
         restype.buildderef;
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
      var
        d : tderef;
      begin
        d.reset;
        if not inherited write_references(ppufile,locals) then
         begin
         { write address of this symbol if record or object
           even if no real refs are there
           because we need it for the symtable }
           if (restype.def.deftype in [recorddef,objectdef]) then
            begin
              d.build(self);
              ppufile.putderef(d);
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

    var stabchar:string[2];

    begin
      stabstring:=nil;
      if restype.def<>nil then
        begin
          if restype.def.deftype in tagtypes then
            stabchar:='Tt'
          else
            stabchar:='t';
          stabstring:=stabstr_evaluate('"${name}:$1$2",${N_LSYM},0,${line},0',[stabchar,tstoreddef(restype.def).numberstring]);
        end;
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


{****************************************************************************
                                  TRTTISYM
****************************************************************************}

    constructor trttisym.create(const n:string;rt:trttitype);
      const
        prefix : array[trttitype] of string[5]=('$rtti','$init');
      begin
        inherited create(prefix[rt]+n);
        include(symoptions,sp_internal);
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
      begin
        result:=make_mangledname(prefix[rttityp],owner,Copy(name,5,255));
      end;


    function trttisym.get_label:tasmsymbol;
      begin
        { the label is always a global label }
        if not assigned(lab) then
         lab:=objectlibrary.newasmsymboldata(mangledname);
        get_label:=lab;
      end;


end.
{
  $Log$
  Revision 1.161  2004-02-24 16:12:39  peter
    * operator overload chooses rewrite
    * overload choosing is now generic and moved to htypechk

  Revision 1.160  2004/02/22 22:13:27  daniel
    * Escape newlines in constant string stabs

  Revision 1.159  2004/02/20 21:54:47  peter
    * use sp_internal flag to silence unused internal variable

  Revision 1.158  2004/02/13 15:42:21  peter
    * compare_defs_ext has now a options argument
    * fixes for variants

  Revision 1.157  2004/02/11 19:59:06  peter
    * fix compilation without GDB

  Revision 1.156  2004/02/08 18:08:59  jonas
    * fixed regvars support. Needs -doldregvars to activate. Only tested with
      ppc, other processors should however only require maxregvars and
      maxfpuregvars constants in cpubase.pas. Remember to take scratch-
      registers into account when defining that value.

  Revision 1.155  2004/02/05 14:13:53  daniel
    *  Tvarsym.highvarsym removed

  Revision 1.154  2004/02/04 23:01:36  daniel
    * Empty destructor Tlabelsym.destroy removed

  Revision 1.153  2004/02/04 22:54:57  daniel
    * Tvarsym.highvarsym commented out (unused by compiler, purpose unknown)

  Revision 1.152  2004/02/04 22:15:15  daniel
    * Rtti generation moved to ncgutil
    * Assmtai usage of symsym removed
    * operator overloading cleanup up

  Revision 1.151  2004/01/31 22:48:31  daniel
    * Fix stabs generation problem reported by Jonas

  Revision 1.150  2004/01/31 21:09:58  daniel
    * Stabs lineinfo problem fixed

  Revision 1.149  2004/01/31 18:40:15  daniel
    * Last steps before removal of aasmtai dependency in symsym can be
      accomplished.

  Revision 1.148  2004/01/31 17:45:17  peter
    * Change several $ifdef i386 to x86
    * Change several OS_32 to OS_INT/OS_ADDR

  Revision 1.147  2004/01/27 22:45:30  daniel
    * Stab generation bug fixed

  Revision 1.146  2004/01/26 22:08:20  daniel
    * Bugfix on constant strings stab generation. Never worked and still
      doesn't work for unknown reasons.

  Revision 1.145  2004/01/26 16:12:28  daniel
    * reginfo now also only allocated during register allocation
    * third round of gdb cleanups: kick out most of concatstabto

  Revision 1.144  2004/01/25 11:33:48  daniel
    * 2nd round of gdb cleanup

  Revision 1.143  2004/01/16 18:08:39  daniel
    * Applied Peters patch

  Revision 1.142  2004/01/15 23:16:32  daniel
    + Cleanup of stabstring generation code. Cleaner, faster, and compiler
      executable reduced by 50 kb,

  Revision 1.141  2004/01/11 23:56:20  daniel
    * Experiment: Compress strings to save memory
      Did not save a single byte of mem; clearly the core size is boosted by
      temporary memory usage...

  Revision 1.140  2004/01/06 15:46:12  peter
    * fix stabs for locals

  Revision 1.139  2003/12/23 22:13:26  peter
    * don't generate rtti for errordef

  Revision 1.138  2003/12/12 12:09:40  marco
   * always generate RTTI patch from peter

  Revision 1.137  2003/12/01 18:44:15  peter
    * fixed some crashes
    * fixed varargs and register calling probs

  Revision 1.136  2003/11/29 18:16:39  jonas
    * don't internalerror when emitting debuginfo for LOC_FPUREGISTER

  Revision 1.135  2003/11/23 17:05:16  peter
    * register calling is left-right
    * parameter ordering
    * left-right calling inserts result parameter last

  Revision 1.134  2003/10/30 16:23:13  peter
    * don't search for overloads in parents for constructors

  Revision 1.133  2003/10/29 21:56:28  peter
    * procsym.deref derefs only own procdefs
    * reset paracount in procdef.deref so a second deref doesn't increase
      the paracounts to invalid values

  Revision 1.132  2003/10/29 19:48:51  peter
    * renamed mangeldname_prefix to make_mangledname and made it more
      generic
    * make_mangledname is now also used for internal threadvar/resstring
      lists
    * Add P$ in front of program modulename to prevent duplicated symbols
      at assembler level, because the main program can have the same name
      as a unit, see webtbs/tw1251b

  Revision 1.131  2003/10/28 15:36:01  peter
    * absolute to object field supported, fixes tb0458

  Revision 1.130  2003/10/22 20:40:00  peter
    * write derefdata in a separate ppu entry

  Revision 1.129  2003/10/22 15:22:33  peter
    * fixed unitsym-globalsymtable relation so the uses of a unit
      is counted correctly

  Revision 1.128  2003/10/21 18:14:30  peter
    * fix writing of widechar to ppu

  Revision 1.127  2003/10/17 14:38:32  peter
    * 64k registers supported
    * fixed some memory leaks

  Revision 1.126  2003/10/13 14:05:12  peter
    * removed is_visible_for_proc
    * search also for class overloads when finding interface
      implementations

  Revision 1.125  2003/10/08 19:19:45  peter
    * set_varstate cleanup

  Revision 1.124  2003/10/07 21:14:33  peter
    * compare_paras() has a parameter to ignore hidden parameters
    * cross unit overload searching ignores hidden parameters when
      comparing parameter lists. Now function(string):string is
      not overriden with procedure(string) which has the same visible
      parameter list

  Revision 1.123  2003/10/07 15:17:07  peter
    * inline supported again, LOC_REFERENCEs are used to pass the
      parameters
    * inlineparasymtable,inlinelocalsymtable removed
    * exitlabel inserting fixed

  Revision 1.122  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.121  2003/09/25 21:25:37  peter
    * has_local_copy gdb fix

  Revision 1.120  2003/09/25 16:18:54  peter
    * fixed stabs for globals,static

  Revision 1.119  2003/09/23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.118  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.117  2003/09/14 13:20:12  peter
    * fix previous commit, also include objectsymtable

  Revision 1.116  2003/09/14 12:58:00  peter
    * support mulitple overloads in implementation, this is delphi
      compatible
    * procsym only stores the overloads available in the interface

  Revision 1.115  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.114  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.113.2.2  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.113.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.113  2003/08/20 20:29:06  daniel
    * Some more R_NO changes
    * Preventive code to loadref added

  Revision 1.112  2003/07/05 22:41:59  peter
    * check if owner.defowner is valid when checking private/protected

  Revision 1.111  2003/07/04 22:41:41  pierre
   * single threadvar debugging support

  Revision 1.110  2003/06/13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.109  2003/06/07 20:26:32  peter
    * re-resolving added instead of reloading from ppu
    * tderef object added to store deref info for resolving

  Revision 1.108  2003/06/05 17:53:30  peter
    * fix to compile without gdb

  Revision 1.107  2003/06/02 22:59:17  florian
    * absolutesyms aren't fpuregable either

  Revision 1.106  2003/05/30 18:48:17  jonas
    * fixed intregister bug
    * fixed error in my previous commit: vo_(fpu)regable should only be set
      for (inline)localsymtable and (inline)parasymtable entries

  Revision 1.105  2003/05/30 13:35:10  jonas
    * the vartype field of tvarsym is now a property, because is_XXXregable
      must be updated when the vartype is changed

  Revision 1.104  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.103  2003/05/12 18:13:57  peter
    * create rtti label using newasmsymboldata and update binding
      only when calling tai_symbol.create
    * tai_symbol.create_global added

  Revision 1.102  2003/05/09 17:47:03  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.101  2003/05/05 14:53:16  peter
    * vs_hidden replaced by is_hidden boolean

  Revision 1.100  2003/04/27 11:21:34  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.99  2003/04/27 10:03:18  jonas
    * fixed stabs generation for local variables on systems where they have
      a positive offset relative to the stack/framepointer

  Revision 1.98  2003/04/27 07:29:51  peter
    * current_procinfo.procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.97  2003/04/25 20:59:35  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.96  2003/04/23 13:13:58  peter
    * fix operator overload search parameter order

  Revision 1.95  2003/04/10 17:57:53  peter
    * vs_hidden released

  Revision 1.94  2003/03/17 15:54:22  peter
    * store symoptions also for procdef
    * check symoptions (private,public) when calculating possible
      overload candidates

  Revision 1.93  2003/01/15 01:44:33  peter
    * merged methodpointer fixes from 1.0.x

  Revision 1.92  2003/01/09 21:52:38  peter
    * merged some verbosity options.
    * V_LineInfo is a verbosity flag to include line info

  Revision 1.91  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.90  2003/01/03 12:15:56  daniel
    * Removed ifdefs around notifications
      ifdefs around for loop optimizations remain

  Revision 1.89  2003/01/02 11:14:02  michael
  + Patch from peter to support initial values for local variables

  Revision 1.88  2003/01/01 22:51:03  peter
    * high value insertion changed so it works also when 2 parameters
      are passed

  Revision 1.87  2002/12/31 09:55:58  daniel
   + Notification implementation complete
   + Add for loop code optimization using notifications
     results in 1.5-1.9% speed improvement in nestloop benchmark
     Optimization incomplete, compiler does not cycle yet with
     notifications enabled.

  Revision 1.86  2002/12/30 22:44:53  daniel
  * Some work on notifications

  Revision 1.85  2002/12/27 18:07:44  peter
    * fix crashes when searching symbols

  Revision 1.84  2002/12/20 16:02:22  peter
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
