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
       cpuinfo,cpubase,cgbase,parabase
       ;

    type
       { this class is the base for all symbol objects }
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
          localloc      : TLocation; { register/reference for local var }
          fieldoffset   : longint;   { offset in record/object }
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
          constructor create_ord(const n : string;t : tconsttyp;v : tconstexprint;const tt:ttype);
          constructor create_ordptr(const n : string;t : tconsttyp;v : tconstptruint;const tt:ttype);
          constructor create_ptr(const n : string;t : tconsttyp;v : pointer;const tt:ttype);
          constructor create_string(const n : string;t : tconsttyp;str:pchar;l:longint);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
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
       { tree }
       node,
       { aasm }
       aasmcpu,
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

    {Makes implicit externals (procedures declared in the interface
     section which do not have a counterpart in the implementation)
     to be an imported procedure. For mode macpas.}
    procedure import_implict_external(pd:tabstractprocdef);

      begin
        tprocdef(pd).forwarddef:=false;
        tprocdef(pd).setmangledname(target_info.CPrefix+tprocdef(pd).procsym.realname);
      end;


    procedure tprocsym.check_forward;
      var
         p : pprocdeflist;
      begin
         p:=pdlistfirst;
         while assigned(p) do
           begin
              if p^.own and (p^.def.forwarddef) then
                begin
                   if (m_mac in aktmodeswitches) and (p^.def.interfacedef) then
                     import_implict_external(p^.def)
                   else
                     begin
                       MessagePos1(p^.def.fileinfo,sym_e_forward_not_resolved,p^.def.fullprocname(false));
                       { Turn further error messages off }
                       p^.def.forwarddef:=false;
                     end
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
              eq:=compare_paras(params,pd^.def.para,cp_value_equal_const,cpoptions);
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
        result:=nil;
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
         if [vo_is_C_var,vo_is_dll_var]*varoptions<>[] then
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
         if [vo_is_C_var,vo_is_dll_var]*varoptions<>[] then
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
            threadvaroffset:='+'+tostr(sizeof(aint))
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
                       not(vo_has_local_copy in varoptions) and
                       not is_open_string(vartype.def) then
                      st := 'v'+st { should be 'i' but 'i' doesn't work }
                    else
                      st := 'p'+st;
                  end;
                case loc of
                  LOC_REGISTER,
                  LOC_CREGISTER,
                  LOC_MMREGISTER,
                  LOC_CMMREGISTER,
                  LOC_FPUREGISTER,
                  LOC_CFPUREGISTER :
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

    constructor tconstsym.create_ord(const n : string;t : tconsttyp;v : tconstexprint;const tt:ttype);
      begin
         inherited create(n);
         fillchar(value, sizeof(value), #0);
         typ:=constsym;
         consttyp:=t;
         value.valueord:=v;
         ResStrIndex:=0;
         consttype:=tt;
      end;


    constructor tconstsym.create_ordptr(const n : string;t : tconsttyp;v : tconstptruint;const tt:ttype);
      begin
         inherited create(n);
         fillchar(value, sizeof(value), #0);
         typ:=constsym;
         consttyp:=t;
         value.valueordptr:=v;
         ResStrIndex:=0;
         consttype:=tt;
      end;


    constructor tconstsym.create_ptr(const n : string;t : tconsttyp;v : pointer;const tt:ttype);
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
          st:='s'''+backspace_quote(octal_quote(strpas(pchar(value.valueptr)),[#0..#9,#11,#12,#14..#31,'''']),['"','\',#10,#13])+'''';
        constord:
          st:='i'+tostr(value.valueord);
        constpointer:
          st:='i'+tostr(value.valueordptr);
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
      { valgrind does not support constants }
      if cs_gdb_valgrind in aktglobalswitches then
        stabstring:=nil
      else
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
         lab:=objectlibrary.newasmsymbol(mangledname,AB_EXTERNAL,AT_DATA);
        get_label:=lab;
      end;


end.
{
  $Log$
  Revision 1.177  2004-09-26 17:45:30  peter
    * simple regvar support, not yet finished

  Revision 1.176  2004/09/21 17:25:12  peter
    * paraloc branch merged

  Revision 1.175.4.1  2004/08/31 20:43:06  peter
    * paraloc patch

  Revision 1.175  2004/08/15 12:06:03  jonas
    * add cprefix to procedures which are autoamtically marked as external in
      macpas mode

  Revision 1.174  2004/06/20 08:55:30  florian
    * logs truncated

  Revision 1.173  2004/06/16 20:07:09  florian
    * dwarf branch merged

  Revision 1.172  2004/05/22 23:32:52  peter
  quote all low ascii chars in stabs

  Revision 1.171  2004/05/11 22:52:48  olle
    * Moved import_implicit_external to symsym

  Revision 1.170  2004/05/11 18:29:41  olle
    + mode macpas: support for implicit external

  Revision 1.169.2.3  2004/05/01 16:02:09  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

}
