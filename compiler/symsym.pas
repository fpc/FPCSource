{
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
       aasmbase,aasmtai,
       cpuinfo,cpubase,cgbase,cgutils,parabase
       ;

    type
       { this class is the base for all symbol objects }
       tstoredsym = class(tsym)
       public
          constructor create(const n : string);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);virtual;
{$ifdef GDB}
          function  get_var_value(const s:string):string;
          function  stabstr_evaluate(const s:string;vars:array of string):Pchar;
          procedure concatstabto(asmlist : taasmoutput);
{$endif GDB}
          function  mangledname : string; virtual;
       end;

       tlabelsym = class(tstoredsym)
          lab     : tasmlabel;
          used,
          defined : boolean;
          code : pointer; { should be tnode }
          constructor create(const n : string; l : tasmlabel);
          constructor ppuload(ppufile:tcompilerppufile);
          function mangledname:string;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
{$ifdef GDB}
          function  stabstring : pchar;override;
{$endif GDB}
       end;

       tunitsym = class(Tstoredsym)
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
          function search_procdef_bypara(para:tlist;retdef:tdef;cpoptions:tcompare_paras_options):Tprocdef;
          function search_procdef_byprocvardef(d:Tprocvardef):Tprocdef;
          function search_procdef_assignment_operator(fromdef,todef:tdef):Tprocdef;
          function  write_references(ppufile:tcompilerppufile;locals:boolean):boolean;override;
          function is_visible_for_object(currobjdef:tdef):boolean;override;
{$ifdef GDB}
          function stabstring : pchar;override;
{$endif GDB}
       end;

       ttypesym = class(Tstoredsym)
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

       tabstractvarsym = class(tstoredsym)
          varoptions    : tvaroptions;
          varspez       : tvarspez;  { sets the type of access }
          varregable    : tvarregable;
          varstate      : tvarstate;
          notifications : Tlinkedlist;
          constructor create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
          function  getsize : longint;
          function  is_regvar:boolean;
          procedure trigger_notifications(what:Tnotification_flag);
          function register_notification(flags:Tnotification_flags;
                                         callback:Tnotification_callback):cardinal;
          procedure unregister_notification(id:cardinal);
         private
          procedure setvartype(const newtype: ttype);
          _vartype       : ttype;
         public
          property vartype: ttype read _vartype write setvartype;
      end;

      tfieldvarsym = class(tabstractvarsym)
          fieldoffset   : aint;   { offset in record/object }
          constructor create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
{$ifdef GDB}
          function stabstring : pchar;override;
{$endif GDB}
      end;

      tabstractnormalvarsym = class(tabstractvarsym)
          defaultconstsym : tsym;
          defaultconstsymderef : tderef;
          localloc      : TLocation; { register/reference for local var }
          constructor create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
      end;

      tlocalvarsym = class(tabstractnormalvarsym)
          constructor create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
{$ifdef GDB}
          function stabstring : pchar;override;
{$endif GDB}
      end;

      tparavarsym = class(tabstractnormalvarsym)
          paraloc       : array[tcallercallee] of TCGPara;
          paranr        : word; { position of this parameter }
{$ifdef EXTDEBUG}
          eqval         : tequaltype;
{$endif EXTDEBUG}
          constructor create(const n : string;nr:word;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
{$ifdef GDB}
          function stabstring : pchar;override;
{$endif GDB}
      end;

      tglobalvarsym = class(tabstractnormalvarsym)
      private
          _mangledname : pstring;
      public
          constructor create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
          constructor create_dll(const n : string;vsp:tvarspez;const tt : ttype);
          constructor create_C(const n,mangled : string;vsp:tvarspez;const tt : ttype);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function mangledname:string;override;
          procedure set_mangledname(const s:string);
{$ifdef GDB}
          function stabstring : pchar;override;
{$endif GDB}
      end;

      tabsolutevarsym = class(tabstractvarsym)
      public
         abstyp  : absolutetyp;
{$ifdef i386}
         absseg  : boolean;
{$endif i386}
         asmname : pstring;
         addroffset : aint;
         ref     : tsymlist;
         constructor create(const n : string;const tt : ttype);
         constructor create_ref(const n : string;const tt : ttype;_ref:tsymlist);
         destructor  destroy;override;
         constructor ppuload(ppufile:tcompilerppufile);
         procedure buildderef;override;
         procedure deref;override;
         function  mangledname : string;override;
         procedure ppuwrite(ppufile:tcompilerppufile);override;
{$ifdef gdb}
         function stabstring:Pchar;override;
{$endif gdb}
      end;

       tpropertysym = class(Tstoredsym)
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

       ttypedconstsym = class(tstoredsym)
       private
          _mangledname : pstring;
       public
          typedconsttype  : ttype;
          is_writable     : boolean;
          constructor create(const n : string;p : tdef;writable : boolean);
          constructor createtype(const n : string;const tt : ttype;writable : boolean);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          function  mangledname : string;override;
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

       tenumsym = class(Tstoredsym)
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

       tsyssym = class(Tstoredsym)
          number : longint;
          constructor create(const n : string;l : longint);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

    const
       maxmacrolen=16*1024;

    type
       pmacrobuffer = ^tmacrobuffer;
       tmacrobuffer = array[0..maxmacrolen-1] of char;

       tmacro = class(tstoredsym)
          {Normally true, but false when a previously defined macro is undef-ed}
          defined : boolean;
          {True if this is a mac style compiler variable, in which case no macro
           substitutions shall be done.}
          is_compiler_var : boolean;
          {Whether the macro was used. NOTE: A use of a macro which was never defined}
          {e. g. an IFDEF which returns false, will not be registered as used,}
          {since there is no place to register its use. }
          is_used : boolean;
          buftext : pchar;
          buflen  : longint;
          constructor create(const n : string);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          destructor  destroy;override;
       end;

       { compiler generated symbol to point to rtti and init/finalize tables }
       trttisym = class(tstoredsym)
       private
          _mangledname : pstring;
       public
          lab     : tasmsymbol;
          rttityp : trttitype;
          constructor create(const n:string;rt:trttitype);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  mangledname:string;override;
          function  get_label:tasmsymbol;
       end;

    var
       generrorsym : tsym;

implementation

    uses
       { global }
       verbose,
       { target }
       systems,
       { symtable }
       defutil,symtable,
       { tree }
       node,
       { aasm }
{$ifdef gdb}
       gdb,
{$endif gdb}
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
      end;


    constructor tstoredsym.ppuload(ppufile:tcompilerppufile);
      var
        nr : word;
        s  : string;
      begin
         nr:=ppufile.getword;
         s:=ppufile.getstring;
         if s[1]='$' then
          inherited createname(copy(s,2,255))
         else
          inherited createname(upper(s));
         _realname:=stringdup(s);
         typ:=abstractsym;
         { force the correct indexnr. must be after create! }
         indexnr:=nr;
         ppufile.getposinfo(fileinfo);
         ppufile.getsmallset(symoptions);
         lastref:=nil;
         defref:=nil;
         refs:=0;
         lastwritten:=nil;
         refcount:=0;
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
      end;


    procedure tstoredsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         ppufile.putword(indexnr);
         ppufile.putstring(_realname^);
         ppufile.putposinfo(fileinfo);
         ppufile.putsmallset(symoptions);
      end;


    destructor tstoredsym.destroy;
      begin
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


    procedure tstoredsym.concatstabto(asmlist : taasmoutput);
      var
        stabstr : Pchar;
      begin
        stabstr:=stabstring;
        if stabstr<>nil then
          asmlist.concat(Tai_stabs.create(stabstr));
      end;
{$endif GDB}


    function tstoredsym.mangledname : string;
      begin
        internalerror(200204171);
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
         inherited ppuload(ppufile);
         typ:=labelsym;
         { this is all dummy
           it is only used for local browsing }
         lab:=nil;
         code:=nil;
         used:=false;
         defined:=true;
      end;


    function tlabelsym.mangledname:string;
      begin
        result:=lab.name;
      end;


    procedure tlabelsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         if owner.symtabletype=globalsymtable then
           Message(sym_e_ill_label_decl)
         else
           begin
              inherited ppuwrite(ppufile);
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
         inherited ppuload(ppufile);
         typ:=unitsym;
         unitsymtable:=nil;
      end;

    destructor tunitsym.destroy;
      begin
         inherited destroy;
      end;

    procedure tunitsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
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
         { the tprocdef have their own symoptions, make the procsym
           always visible }
         symoptions:=[sp_public];
         overloadchecked:=false;
         procdef_count:=0;
      end;


    constructor tprocsym.ppuload(ppufile:tcompilerppufile);
      var
         pdderef : tderef;
         i,n : longint;
      begin
         inherited ppuload(ppufile);
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
         inherited ppuwrite(ppufile);
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
            if Aprocsym.search_procdef_bypara(pd^.def.paras,nil,cpoptions)=nil then
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


    function Tprocsym.search_procdef_bypara(para:tlist;retdef:tdef;
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
              eq:=compare_paras(para,pd^.def.paras,cp_value_equal_const,cpoptions);
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
        pd      : pprocdeflist;
        bestpd  : tprocdef;
        eq,
        besteq  : tequaltype;
        hpd     : tprocdef;
        i       : byte;
      begin
        result:=nil;
        bestpd:=nil;
        besteq:=te_incompatible;
        pd:=pdlistfirst;
        while assigned(pd) do
          begin
            if equal_defs(todef,pd^.def.rettype.def) then
             begin
               i:=0;
               { ignore vs_hidden parameters }
               while assigned(pd^.def.paras[i]) and
                     (vo_is_hidden_para in tparavarsym(pd^.def.paras[i]).varoptions) do
                 inc(i);
               if assigned(pd^.def.paras[i]) then
                begin
                  eq:=compare_defs_ext(fromdef,tparavarsym(pd^.def.paras[i]).vartype.def,nothingn,convtyp,hpd,[]);
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


    function tprocsym.is_visible_for_object(currobjdef:tdef):boolean;
      var
        p : pprocdeflist;
      begin
        { This procsym is visible, when there is at least
          one of the procdefs visible }
        result:=false;
        p:=pdlistfirst;
        while assigned(p) do
          begin
             if p^.own and
                p^.def.is_visible_for_object(tobjectdef(currobjdef)) then
               begin
                 result:=true;
                 exit;
               end;
             p:=p^.next;
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
         inherited ppuload(ppufile);
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
        inherited ppuwrite(ppufile);
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
                            TABSTRACTVARSYM
****************************************************************************}

    constructor tabstractvarsym.create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(n);
         vartype:=tt;
         varspez:=vsp;
         varstate:=vs_declared;
         varoptions:=vopts;
      end;


    constructor tabstractvarsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(ppufile);
         varstate:=vs_used;
         varspez:=tvarspez(ppufile.getbyte);
         varregable:=tvarregable(ppufile.getbyte);
         ppufile.gettype(_vartype);
         ppufile.getsmallset(varoptions);
      end;


    destructor tabstractvarsym.destroy;
      begin
        if assigned(notifications) then
          notifications.destroy;
        inherited destroy;
      end;


    procedure tabstractvarsym.buildderef;
      begin
        vartype.buildderef;
      end;


    procedure tabstractvarsym.deref;
      begin
        vartype.resolve;
      end;


    procedure tabstractvarsym.ppuwrite(ppufile:tcompilerppufile);
      var
        oldintfcrc : boolean;
      begin
         inherited ppuwrite(ppufile);
         ppufile.putbyte(byte(varspez));
         oldintfcrc:=ppufile.do_crc;
         ppufile.do_crc:=false;
         ppufile.putbyte(byte(varregable));
         ppufile.do_crc:=oldintfcrc;
         ppufile.puttype(vartype);
         ppufile.putsmallset(varoptions);
      end;


    function tabstractvarsym.getsize : longint;
      begin
        if assigned(vartype.def) and
           ((vartype.def.deftype<>arraydef) or
            tarraydef(vartype.def).isDynamicArray or
            (tarraydef(vartype.def).highrange>=tarraydef(vartype.def).lowrange)) then
          result:=vartype.def.size
        else
          result:=0;
      end;


    function tabstractvarsym.is_regvar:boolean;
      begin
        { Register variables are not allowed in the following cases:
           - regvars are disabled
           - exceptions are used (after an exception is raised the contents of the
               registers is not valid anymore)
           - it has a local copy
           - the value needs to be in memory (i.e. reference counted) }
        result:=(cs_regvars in aktglobalswitches) and
                not(pi_has_assembler_block in current_procinfo.flags) and
                not(pi_uses_exceptions in current_procinfo.flags) and
                not(vo_has_local_copy in varoptions) and
                (varregable<>vr_none);
      end;


    procedure tabstractvarsym.trigger_notifications(what:Tnotification_flag);

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

    function Tabstractvarsym.register_notification(flags:Tnotification_flags;callback:
                                           Tnotification_callback):cardinal;

    var n:Tnotification;

    begin
      if not assigned(notifications) then
        notifications:=Tlinkedlist.create;
      n:=Tnotification.create(flags,callback);
      register_notification:=n.id;
      notifications.concat(n);
    end;

    procedure Tabstractvarsym.unregister_notification(id:cardinal);

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

    procedure tabstractvarsym.setvartype(const newtype: ttype);
      begin
        _vartype := newtype;
         { can we load the value into a register ? }
        if not assigned(owner) or
           (owner.symtabletype in [localsymtable,parasymtable]) or
           (
            (owner.symtabletype=staticsymtable) and
            not(cs_create_pic in aktmoduleswitches)
           ) then
          begin
            if tstoreddef(vartype.def).is_intregable then
              varregable:=vr_intreg
            else
{$warning TODO: no fpu regvar in staticsymtable yet, need initialization with 0}
              if (
                  not assigned(owner) or
                  (owner.symtabletype<>staticsymtable)
                 ) and
                 tstoreddef(vartype.def).is_fpuregable then
                varregable:=vr_fpureg;
          end;
      end;


{****************************************************************************
                               TFIELDVARSYM
****************************************************************************}

    constructor tfieldvarsym.create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(n,vsp,tt,vopts);
         typ:=fieldvarsym;
         fieldoffset:=0;
      end;


    constructor tfieldvarsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(ppufile);
         typ:=fieldvarsym;
         fieldoffset:=ppufile.getaint;
      end;


    procedure tfieldvarsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         ppufile.putaint(fieldoffset);
         ppufile.writeentry(ibfieldvarsym);
      end;

{$ifdef GDB}
    function tfieldvarsym.stabstring:Pchar;
    var
      st : string;
    begin
      stabstring:=nil;
      case owner.symtabletype of
        objectsymtable :
          begin
            if (sp_static in symoptions) then
              begin
                st:=tstoreddef(vartype.def).numberstring;
                if (cs_gdb_gsym in aktglobalswitches) then
                  st:='G'+st
                else
                  st:='S'+st;
                stabstring:=stabstr_evaluate('"${ownername}__${name}:$1",${N_LCSYM},0,${line},${mangledname}',[st]);
              end;
          end;
      end;
    end;
{$endif GDB}


{****************************************************************************
                        TABSTRACTNORMALVARSYM
****************************************************************************}

    constructor tabstractnormalvarsym.create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(n,vsp,tt,vopts);
         fillchar(localloc,sizeof(localloc),0);
         defaultconstsym:=nil;
      end;


    constructor tabstractnormalvarsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(ppufile);
         fillchar(localloc,sizeof(localloc),0);
         ppufile.getderef(defaultconstsymderef);
      end;


    procedure tabstractnormalvarsym.buildderef;
      begin
        inherited buildderef;
        defaultconstsymderef.build(defaultconstsym);
      end;


    procedure tabstractnormalvarsym.deref;
      begin
        inherited deref;
        defaultconstsym:=tsym(defaultconstsymderef.resolve);
      end;


    procedure tabstractnormalvarsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         ppufile.putderef(defaultconstsymderef);
      end;


{****************************************************************************
                             TGLOBALVARSYM
****************************************************************************}

    constructor tglobalvarsym.create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(n,vsp,tt,vopts);
         typ:=globalvarsym;
         _mangledname:=nil;
      end;


    constructor tglobalvarsym.create_dll(const n : string;vsp:tvarspez;const tt : ttype);
      begin
         tglobalvarsym(self).create(n,vsp,tt,[vo_is_dll_var]);
      end;


    constructor tglobalvarsym.create_C(const n,mangled : string;vsp:tvarspez;const tt : ttype);
      begin
         tglobalvarsym(self).create(n,vsp,tt,[]);
         set_mangledname(mangled);
      end;


    constructor tglobalvarsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(ppufile);
         typ:=globalvarsym;
         if vo_has_mangledname in varoptions then
           _mangledname:=stringdup(ppufile.getstring)
         else
           _mangledname:=nil;
      end;


    destructor tglobalvarsym.destroy;
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
        inherited destroy;
      end;


    procedure tglobalvarsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         if vo_has_mangledname in varoptions then
           ppufile.putstring(_mangledname^);
         ppufile.writeentry(ibglobalvarsym);
      end;


    function tglobalvarsym.mangledname:string;
      begin
        if not assigned(_mangledname) then
          begin
      {$ifdef compress}
            _mangledname:=stringdup(minilzw_encode(make_mangledname('U',owner,name)));
      {$else}
           _mangledname:=stringdup(make_mangledname('U',owner,name));
      {$endif}
          end;
        result:=_mangledname^;
      end;


    procedure tglobalvarsym.set_mangledname(const s:string);
      begin
        stringdispose(_mangledname);
      {$ifdef compress}
        _mangledname:=stringdup(minilzw_encode(s));
      {$else}
        _mangledname:=stringdup(s);
      {$endif}
        include(varoptions,vo_has_mangledname);
      end;


{$ifdef GDB}
    function Tglobalvarsym.stabstring:Pchar;

    var st:string;
        threadvaroffset:string;
        regidx:Tregisterindex;
    begin
      result:=nil;
      st:=tstoreddef(vartype.def).numberstring;
      case localloc.loc of
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
            if regidx<>0 then
              stabstring:=stabstr_evaluate('"${name}:r$1",${N_RSYM},0,${line},$2',[st,tostr(regstabs_table[regidx])]);
          end;
        else
          begin
            if (vo_is_thread_var in varoptions) then
              threadvaroffset:='+'+tostr(sizeof(aint))
            else
              threadvaroffset:='';
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
      end;
    end;
{$endif GDB}


{****************************************************************************
                               TLOCALVARSYM
****************************************************************************}

    constructor tlocalvarsym.create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(n,vsp,tt,vopts);
         typ:=localvarsym;
      end;


    constructor tlocalvarsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(ppufile);
         typ:=localvarsym;
      end;


    procedure tlocalvarsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         ppufile.writeentry(iblocalvarsym);
      end;


{$ifdef GDB}
    function tlocalvarsym.stabstring:Pchar;
    var st:string;
        regidx:Tregisterindex;
    begin
      stabstring:=nil;
      { There is no space allocated for not referenced locals }
      if (owner.symtabletype=localsymtable) and (refs=0) then
        exit;

      st:=tstoreddef(vartype.def).numberstring;
      case localloc.loc of
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
            if regidx<>0 then
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
{$endif GDB}


{****************************************************************************
                              TPARAVARSYM
****************************************************************************}

    constructor tparavarsym.create(const n : string;nr:word;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(n,vsp,tt,vopts);
         typ:=paravarsym;
         paranr:=nr;
         paraloc[calleeside].init;
         paraloc[callerside].init;
      end;


    destructor tparavarsym.destroy;
      begin
        paraloc[calleeside].done;
        paraloc[callerside].done;
        inherited destroy;
      end;


    constructor tparavarsym.ppuload(ppufile:tcompilerppufile);
      var
        b : byte;
      begin
         inherited ppuload(ppufile);
         paranr:=ppufile.getword;
         paraloc[calleeside].init;
         paraloc[callerside].init;
         if vo_has_explicit_paraloc in varoptions then
           begin
             b:=ppufile.getbyte;
             if b<>sizeof(paraloc[callerside].location^) then
               internalerror(200411154);
             ppufile.getdata(paraloc[callerside].add_location^,sizeof(paraloc[callerside].location^));
             paraloc[callerside].size:=paraloc[callerside].location^.size;
             paraloc[callerside].intsize:=tcgsize2size[paraloc[callerside].size];
           end;
         typ:=paravarsym;
      end;


    procedure tparavarsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         ppufile.putword(paranr);
         if vo_has_explicit_paraloc in varoptions then
           begin
             paraloc[callerside].check_simple_location;
             ppufile.putbyte(sizeof(paraloc[callerside].location^));
             ppufile.putdata(paraloc[callerside].location^,sizeof(paraloc[callerside].location^));
           end;
         ppufile.writeentry(ibparavarsym);
      end;

{$ifdef GDB}
    function tparavarsym.stabstring:Pchar;
    var st:string;
        regidx:Tregisterindex;
        c:char;

    begin
      result:=nil;
      { set loc to LOC_REFERENCE to get somewhat usable debugging info for -Or }
      { while stabs aren't adapted for regvars yet                             }
      if (vo_is_self in varoptions) then
        begin
          case localloc.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              regidx:=findreg_by_number(localloc.register);
            LOC_REFERENCE: ;
            else
              internalerror(2003091815);
          end;
          if (po_classmethod in current_procinfo.procdef.procoptions) or
             (po_staticmethod in current_procinfo.procdef.procoptions) then
            begin
              if (localloc.loc=LOC_REFERENCE) then
                stabstring:=stabstr_evaluate('"pvmt:p$1",${N_TSYM},0,0,$2',
                  [Tstoreddef(pvmttype.def).numberstring,tostr(localloc.reference.offset)]);
(*            else
                stabstring:=stabstr_evaluate('"pvmt:r$1",${N_RSYM},0,0,$2',
                  [Tstoreddef(pvmttype.def).numberstring,tostr(regstabs_table[regidx])]) *)
              end
          else
            begin
              if not(is_class(current_procinfo.procdef._class)) then
                c:='v'
              else
                c:='p';
              if (localloc.loc=LOC_REFERENCE) then
                stabstring:=stabstr_evaluate('"$$t:$1",${N_TSYM},0,0,$2',
                      [c+current_procinfo.procdef._class.numberstring,tostr(localloc.reference.offset)]);
(*            else
                stabstring:=stabstr_evaluate('"$$t:r$1",${N_RSYM},0,0,$2',
                      [c+current_procinfo.procdef._class.numberstring,tostr(regstabs_table[regidx])]); *)
            end;
        end
      else
        begin
          st:=tstoreddef(vartype.def).numberstring;

          if paramanager.push_addr_param(varspez,vartype.def,tprocdef(owner.defowner).proccalloption) and
             not(vo_has_local_copy in varoptions) and
             not is_open_string(vartype.def) then
            st := 'v'+st { should be 'i' but 'i' doesn't work }
          else
            st := 'p'+st;
          case localloc.loc of
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
                if regidx<>0 then
                  stabstring:=stabstr_evaluate('"${name}:r$1",${N_RSYM},0,${line},$2',[st,tostr(longint(regstabs_table[regidx]))]);
              end;
            LOC_REFERENCE :
              { offset to ebp => will not work if the framepointer is esp
                so some optimizing will make things harder to debug }
              stabstring:=stabstr_evaluate('"${name}:$1",${N_TSYM},0,${line},$2',[st,tostr(localloc.reference.offset)])
            else
              internalerror(2003091814);
          end;
        end;
    end;
{$endif GDB}


{****************************************************************************
                               TABSOLUTEVARSYM
****************************************************************************}

    constructor tabsolutevarsym.create(const n : string;const tt : ttype);
      begin
        inherited create(n,vs_value,tt,[]);
        typ:=absolutevarsym;
        ref:=nil;
      end;


    constructor tabsolutevarsym.create_ref(const n : string;const tt : ttype;_ref:tsymlist);
      begin
        inherited create(n,vs_value,tt,[]);
        typ:=absolutevarsym;
        ref:=_ref;
      end;


    destructor tabsolutevarsym.destroy;
      begin
        if assigned(ref) then
          ref.free;
        inherited destroy;
      end;


    constructor tabsolutevarsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(ppufile);
         typ:=absolutevarsym;
         ref:=nil;
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
               addroffset:=ppufile.getaint;
{$ifdef i386}
               absseg:=boolean(ppufile.getbyte);
{$endif i386}
             end;
         end;
      end;


    procedure tabsolutevarsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         ppufile.putbyte(byte(abstyp));
         case abstyp of
           tovar :
             ppufile.putsymlist(ref);
           toasm :
             ppufile.putstring(asmname^);
           toaddr :
             begin
               ppufile.putaint(addroffset);
{$ifdef i386}
               ppufile.putbyte(byte(absseg));
{$endif i386}
             end;
         end;
         ppufile.writeentry(ibabsolutevarsym);
      end;


    procedure tabsolutevarsym.buildderef;
      begin
        inherited buildderef;
        if (abstyp=tovar) then
          ref.buildderef;
      end;


    procedure tabsolutevarsym.deref;
      begin
         inherited deref;
         { own absolute deref }
         if (abstyp=tovar) then
           ref.resolve;
      end;


    function tabsolutevarsym.mangledname : string;
      begin
         case abstyp of
           toasm :
             mangledname:=asmname^;
           toaddr :
             mangledname:='$'+tostr(addroffset);
           else
             internalerror(200411061);
         end;
      end;


{$ifdef GDB}
    function tabsolutevarsym.stabstring:Pchar;
      begin
        stabstring:=nil;
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
         inherited ppuload(ppufile);
         typ:=typedconstsym;
         ppufile.gettype(typedconsttype);
         is_writable:=boolean(ppufile.getbyte);
      end;


    destructor ttypedconstsym.destroy;
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
         inherited destroy;
      end;


    function ttypedconstsym.mangledname:string;
      begin
        if not assigned(_mangledname) then
          begin
      {$ifdef compress}
            _mangledname:=stringdup(make_mangledname('TC',owner,name));
      {$else}
            _mangledname:=stringdup(make_mangledname('TC',owner,name));
      {$endif}
          end;
        result:=_mangledname^;
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
         inherited ppuwrite(ppufile);
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
         inherited ppuload(ppufile);
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
         inherited ppuwrite(ppufile);
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
         { First entry? Then we need to set the minval }
         if def.firstenum=nil then
           begin
             if v>0 then
               def.has_jumps:=true;
             def.setmin(v);
             def.setmax(v);
           end
         else
           begin
             { check for jumps }
             if v>def.max+1 then
              def.has_jumps:=true;
             { update low and high }
             if def.min>v then
               def.setmin(v);
             if def.max<v then
               def.setmax(v);
           end;
         order;
      end;


    constructor tenumsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(ppufile);
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
         inherited ppuwrite(ppufile);
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
         inherited ppuload(ppufile);
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
         inherited ppuwrite(ppufile);
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
         inherited ppuload(ppufile);
         typ:=syssym;
         number:=ppufile.getlongint;
      end;

    destructor tsyssym.destroy;
      begin
        inherited destroy;
      end;

    procedure tsyssym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         ppufile.putlongint(number);
         ppufile.writeentry(ibsyssym);
      end;


{*****************************************************************************
                                 TMacro
*****************************************************************************}

    constructor tmacro.create(const n : string);
      begin
         inherited create(n);
         typ:= macrosym;
         owner:= nil;

         defined:=false;
         is_used:=false;
         is_compiler_var:= false;
         buftext:=nil;
         buflen:=0;
      end;

    constructor tmacro.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(ppufile);
         typ:=macrosym;
         name:=ppufile.getstring;
         defined:=boolean(ppufile.getbyte);
         is_compiler_var:=boolean(ppufile.getbyte);
         is_used:=false;
         buflen:= ppufile.getlongint;
         if buflen > 0 then
           begin
             getmem(buftext, buflen);
             ppufile.getdata(buftext^, buflen)
           end
         else
           buftext:=nil;
      end;

    destructor tmacro.destroy;
      begin
         if assigned(buftext) then
           freemem(buftext,buflen);
         inherited destroy;
      end;

    procedure tmacro.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         ppufile.putstring(name);
         ppufile.putbyte(byte(defined));
         ppufile.putbyte(byte(is_compiler_var));
         ppufile.putlongint(buflen);
         if buflen > 0 then
           ppufile.putdata(buftext^,buflen);
         ppufile.writeentry(ibmacrosym);
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


    destructor trttisym.destroy;
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
        inherited destroy;
      end;


    constructor trttisym.ppuload(ppufile:tcompilerppufile);
      begin
        inherited ppuload(ppufile);
        typ:=rttisym;
        lab:=nil;
        rttityp:=trttitype(ppufile.getbyte);
      end;


    procedure trttisym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         ppufile.putbyte(byte(rttityp));
         ppufile.writeentry(ibrttisym);
      end;


    function trttisym.mangledname : string;
      const
        prefix : array[trttitype] of string[5]=('RTTI_','INIT_');
      begin
        if not assigned(_mangledname) then
          _mangledname:=stringdup(make_mangledname(prefix[rttityp],owner,Copy(name,5,255)));
        result:=_mangledname^;
      end;


    function trttisym.get_label:tasmsymbol;
      begin
        { the label is always a global label }
        if not assigned(lab) then
         lab:=objectlibrary.newasmsymbol(mangledname,AB_EXTERNAL,AT_DATA);
        get_label:=lab;
      end;


end.
