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
       globtype,globals,widestr,
       { symtable }
       symconst,symbase,symtype,symdef,defcmp,
       { ppu }
       ppu,finput,
       cclasses,symnot,
       { aasm }
       aasmbase,
       cpuinfo,cpubase,cgbase,cgutils,parabase
       ;

    type
       { this class is the base for all symbol objects }
       tstoredsym = class(tsym)
       public
          constructor create(st:tsymtyp;const n : string);
          constructor ppuload(st:tsymtyp;ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);virtual;
       end;

       tlabelsym = class(tstoredsym)
          used,
          defined : boolean;
          { points to the matching node, only valid resulttype pass is run and
            the goto<->label relation in the node tree is created, should
            be a tnode }
          code : pointer;

          { when the label is defined in an asm block, this points to the
            generated asmlabel }
          asmblocklabel : tasmlabel;
          constructor create(const n : string);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       tunitsym = class(Tstoredsym)
          module : tobject; { tmodule }
          constructor create(const n : string;amodule : tobject);
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
          function search_procdef_bypara(para:TFPObjectList;retdef:tdef;cpoptions:tcompare_paras_options):Tprocdef;
          function search_procdef_byprocvardef(d:Tprocvardef):Tprocdef;
          function search_procdef_assignment_operator(fromdef,todef:tdef;var besteq:tequaltype):Tprocdef;
          function  write_references(ppufile:tcompilerppufile;locals:boolean):boolean;override;
          { currobjdef is the object def to assume, this is necessary for protected and
            private,
            context is the object def we're really in, this is for the strict stuff
          }
          function is_visible_for_object(currobjdef:tdef;context:tdef):boolean;override;
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
       end;

       tabstractvarsym = class(tstoredsym)
          varoptions    : tvaroptions;
          varspez       : tvarspez;  { sets the type of access }
          varregable    : tvarregable;
          varstate      : tvarstate;
          notifications : Tlinkedlist;
          constructor create(st:tsymtyp;const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
          constructor ppuload(st:tsymtyp;ppufile:tcompilerppufile);
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
      end;

      tabstractnormalvarsym = class(tabstractvarsym)
          defaultconstsym : tsym;
          defaultconstsymderef : tderef;
          localloc      : TLocation; { register/reference for local var }
          constructor create(st:tsymtyp;const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
          constructor ppuload(st:tsymtyp;ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderef;override;
          procedure deref;override;
      end;

      tlocalvarsym = class(tabstractnormalvarsym)
          constructor create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
          constructor ppuload(ppufile:tcompilerppufile);
          procedure ppuwrite(ppufile:tcompilerppufile);override;
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
          procedure derefimpl;override;
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
          constructor create_wstring(const n : string;t : tconsttyp;pw:pcompilerwidestring);
          constructor ppuload(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure buildderef;override;
          procedure deref;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
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
          function GetCopy:tmacro;
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
       aasmtai,aasmdata,
       { codegen }
       paramgr,
       procinfo
       ;

{****************************************************************************
                               Helpers
****************************************************************************}

{****************************************************************************
                          TSYM (base for all symtypes)
****************************************************************************}

    constructor tstoredsym.create(st:tsymtyp;const n : string);
      begin
         inherited create(st,n);
      end;


    constructor tstoredsym.ppuload(st:tsymtyp;ppufile:tcompilerppufile);
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
         typ:=st;
         { force the correct indexnr. must be after create! }
         indexnr:=nr;
         ppufile.getposinfo(fileinfo);
         ppufile.getsmallset(symoptions);
         lastref:=nil;
         defref:=nil;
         refs:=0;
         lastwritten:=nil;
         refcount:=0;
         isstabwritten := false;
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


{****************************************************************************
                                 TLABELSYM
****************************************************************************}

    constructor tlabelsym.create(const n : string);
      begin
         inherited create(labelsym,n);
         used:=false;
         defined:=false;
         code:=nil;
      end;


    constructor tlabelsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(labelsym,ppufile);
         code:=nil;
         used:=false;
         defined:=true;
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


{****************************************************************************
                                  TUNITSYM
****************************************************************************}

    constructor tunitsym.create(const n : string;amodule : tobject);
      var
        old_make_ref : boolean;
      begin
         old_make_ref:=make_ref;
         make_ref:=false;
         inherited create(unitsym,n);
         make_ref:=old_make_ref;
         module:=amodule;
      end;

    constructor tunitsym.ppuload(ppufile:tcompilerppufile);

      begin
         inherited ppuload(unitsym,ppufile);
         module:=nil;
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
         inherited create(procsym,n);
         pdlistfirst:=nil;
         pdlistlast:=nil;
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
         inherited ppuload(procsym,ppufile);
         pdlistfirst:=nil;
         pdlistlast:=nil;
         procdef_count:=0;
         n:=ppufile.getword;
         for i:=1to n do
          begin
            ppufile.getderef(pdderef);
            addprocdef_deref(pdderef);
          end;
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
             if p^.def.owner=owner then
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
             if p^.def.owner=owner then
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
              if (p^.def.owner=owner) and (p^.def.forwarddef) then
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
             if p^.def.owner=owner then
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
             if not(
                    (p^.def=nil) or
                    (p^.def.owner=owner)
                   ) then
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


    function Tprocsym.search_procdef_bypara(para:TFPObjectList;retdef:tdef;
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


    function Tprocsym.search_procdef_assignment_operator(fromdef,todef:tdef;var besteq:tequaltype):Tprocdef;
      var
        convtyp : tconverttype;
        pd      : pprocdeflist;
        bestpd  : tprocdef;
        eq      : tequaltype;
        hpd     : tprocdef;
        i       : byte;
      begin
        result:=nil;
        bestpd:=nil;
        besteq:=te_incompatible;
        pd:=pdlistfirst;
        while assigned(pd) do
          begin
            if equal_defs(todef,pd^.def.rettype.def) and
              { the result type must be always really equal and not an alias,
                if you mess with this code, check tw4093 }
              ((todef=pd^.def.rettype.def) or
               (
                 not(df_unique in todef.defoptions) and
                 not(df_unique in pd^.def.rettype.def.defoptions)
               )
              ) then
             begin
               i:=0;
               { ignore vs_hidden parameters }
               while (i<pd^.def.paras.count) and
                     assigned(pd^.def.paras[i]) and
                     (vo_is_hidden_para in tparavarsym(pd^.def.paras[i]).varoptions) do
                 inc(i);
               if (i<pd^.def.paras.count) and
                  assigned(pd^.def.paras[i]) then
                begin
                  eq:=compare_defs_ext(fromdef,tparavarsym(pd^.def.paras[i]).vartype.def,nothingn,convtyp,hpd,[]);

                  { alias? if yes, only l1 choice,
                    if you mess with this code, check tw4093 }
                  if (eq=te_exact) and
                    (fromdef<>tparavarsym(pd^.def.paras[i]).vartype.def) and
                    ((df_unique in fromdef.defoptions) or
                    (df_unique in tparavarsym(pd^.def.paras[i]).vartype.def.defoptions)) then
                    eq:=te_convert_l1;

                  if eq=te_exact then
                   begin
                     besteq:=eq;
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
              if p^.def.owner=owner then
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
             { only keep the proc definitions:
                - are not deref'd (def=nil)
                - are in the same symtable as the procsym (for example both
                  are in the staticsymtable) }
             if (p^.def=nil) or
                (p^.def.owner=owner) then
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


    function tprocsym.is_visible_for_object(currobjdef:tdef;context:tdef):boolean;
      var
        p : pprocdeflist;
      begin
        { This procsym is visible, when there is at least
          one of the procdefs visible }
        result:=false;
        p:=pdlistfirst;
        while assigned(p) do
          begin
             if (p^.def.owner=owner) and
                p^.def.is_visible_for_object(tobjectdef(currobjdef)) then
               begin
                 result:=true;
                 exit;
               end;
             p:=p^.next;
          end;
      end;



{****************************************************************************
                                  TERRORSYM
****************************************************************************}

    constructor terrorsym.create;
      begin
        inherited create(errorsym,'');
      end;

{****************************************************************************
                                TPROPERTYSYM
****************************************************************************}

    constructor tpropertysym.create(const n : string);
      begin
         inherited create(propertysym,n);
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
         inherited ppuload(propertysym,ppufile);
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
        if not(ppo_is_override in propoptions) then
         begin
           proptype.resolve;
           indextype.resolve;
           readaccess.resolve;
           writeaccess.resolve;
           storedaccess.resolve;
         end;
      end;


    procedure tpropertysym.derefimpl;
      begin
        if (ppo_is_override in propoptions) then
         begin
           propoverriden:=tpropertysym(propoverridenderef.resolve);
           dooverride(propoverriden);
         end
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

    constructor tabstractvarsym.create(st:tsymtyp;const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(st,n);
         vartype:=tt;
         varspez:=vsp;
         varstate:=vs_declared;
         varoptions:=vopts;
      end;


    constructor tabstractvarsym.ppuload(st:tsymtyp;ppufile:tcompilerppufile);
      begin
         inherited ppuload(st,ppufile);
         varstate:=vs_readwritten;
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
        result:=(cs_opt_regvar in aktoptimizerswitches) and
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
{ $warning TODO: no fpu regvar in staticsymtable yet, need initialization with 0 }
              if {(
                  not assigned(owner) or
                  (owner.symtabletype<>staticsymtable)
                 ) and }
                 tstoreddef(vartype.def).is_fpuregable then
                 begin
{$ifdef x86}
                   if use_sse(vartype.def) then
                     varregable:=vr_mmreg
                   else
{$else x86}
                     varregable:=vr_fpureg;
{$endif x86}
                 end;
          end;
      end;


{****************************************************************************
                               TFIELDVARSYM
****************************************************************************}

    constructor tfieldvarsym.create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(fieldvarsym,n,vsp,tt,vopts);
         fieldoffset:=-1;
      end;


    constructor tfieldvarsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(fieldvarsym,ppufile);
         fieldoffset:=ppufile.getaint;
      end;


    procedure tfieldvarsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         ppufile.putaint(fieldoffset);
         ppufile.writeentry(ibfieldvarsym);
      end;


{****************************************************************************
                        TABSTRACTNORMALVARSYM
****************************************************************************}

    constructor tabstractnormalvarsym.create(st:tsymtyp;const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(st,n,vsp,tt,vopts);
         fillchar(localloc,sizeof(localloc),0);
         defaultconstsym:=nil;
      end;


    constructor tabstractnormalvarsym.ppuload(st:tsymtyp;ppufile:tcompilerppufile);
      begin
         inherited ppuload(st,ppufile);
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
         inherited create(globalvarsym,n,vsp,tt,vopts);
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
         inherited ppuload(globalvarsym,ppufile);
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


{****************************************************************************
                               TLOCALVARSYM
****************************************************************************}

    constructor tlocalvarsym.create(const n : string;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(localvarsym,n,vsp,tt,vopts);
      end;


    constructor tlocalvarsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(localvarsym,ppufile);
      end;


    procedure tlocalvarsym.ppuwrite(ppufile:tcompilerppufile);
      begin
         inherited ppuwrite(ppufile);
         ppufile.writeentry(iblocalvarsym);
      end;


{****************************************************************************
                              TPARAVARSYM
****************************************************************************}

    constructor tparavarsym.create(const n : string;nr:word;vsp:tvarspez;const tt : ttype;vopts:tvaroptions);
      begin
         inherited create(paravarsym,n,vsp,tt,vopts);
         if (vsp in [vs_var,vs_value,vs_const]) then
           varstate := vs_initialised;
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
         inherited ppuload(paravarsym,ppufile);
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


{****************************************************************************
                               TABSOLUTEVARSYM
****************************************************************************}

    constructor tabsolutevarsym.create(const n : string;const tt : ttype);
      begin
        inherited create(absolutevarsym,n,vs_value,tt,[]);
        ref:=nil;
      end;


    constructor tabsolutevarsym.create_ref(const n : string;const tt : ttype;_ref:tsymlist);
      begin
        inherited create(absolutevarsym,n,vs_value,tt,[]);
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
         inherited ppuload(absolutevarsym,ppufile);
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


{****************************************************************************
                             TTYPEDCONSTSYM
*****************************************************************************}

    constructor ttypedconstsym.create(const n : string;p : tdef;writable : boolean);
      begin
         inherited create(typedconstsym,n);
         typedconsttype.setdef(p);
         is_writable:=writable;
      end;


    constructor ttypedconstsym.createtype(const n : string;const tt : ttype;writable : boolean);
      begin
         inherited create(typedconstsym,n);
         typedconsttype:=tt;
         is_writable:=writable;
      end;


    constructor ttypedconstsym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(typedconstsym,ppufile);
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


{****************************************************************************
                                  TCONSTSYM
****************************************************************************}

    constructor tconstsym.create_ord(const n : string;t : tconsttyp;v : tconstexprint;const tt:ttype);
      begin
         inherited create(constsym,n);
         fillchar(value, sizeof(value), #0);
         consttyp:=t;
         value.valueord:=v;
         ResStrIndex:=0;
         consttype:=tt;
      end;


    constructor tconstsym.create_ordptr(const n : string;t : tconsttyp;v : tconstptruint;const tt:ttype);
      begin
         inherited create(constsym,n);
         fillchar(value, sizeof(value), #0);
         consttyp:=t;
         value.valueordptr:=v;
         ResStrIndex:=0;
         consttype:=tt;
      end;


    constructor tconstsym.create_ptr(const n : string;t : tconsttyp;v : pointer;const tt:ttype);
      begin
         inherited create(constsym,n);
         fillchar(value, sizeof(value), #0);
         consttyp:=t;
         value.valueptr:=v;
         ResStrIndex:=0;
         consttype:=tt;
      end;


    constructor tconstsym.create_string(const n : string;t : tconsttyp;str:pchar;l:longint);
      begin
         inherited create(constsym,n);
         fillchar(value, sizeof(value), #0);
         consttyp:=t;
         value.valueptr:=str;
         consttype.reset;
         value.len:=l;
      end;


    constructor tconstsym.create_wstring(const n : string;t : tconsttyp;pw:pcompilerwidestring);
      begin
         inherited create(constsym,n);
         fillchar(value, sizeof(value), #0);
         consttyp:=t;
         pcompilerwidestring(value.valueptr):=pw;
         consttype.reset;
         value.len:=getlengthwidestring(pw);
      end;


    constructor tconstsym.ppuload(ppufile:tcompilerppufile);
      var
         pd : pbestreal;
         ps : pnormalset;
         pc : pchar;
         pw : pcompilerwidestring;
      begin
         inherited ppuload(constsym,ppufile);
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
           constwstring :
             begin
               initwidestring(pw);
               setlengthwidestring(pw,ppufile.getlongint);
               ppufile.getdata(pw^.data,pw^.len*sizeof(tcompilerwidechar));
               pcompilerwidestring(value.valueptr):=pw;
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
          constwstring :
            donewidestring(pcompilerwidestring(value.valueptr));
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
           constwstring :
             begin
               ppufile.putlongint(getlengthwidestring(pcompilerwidestring(value.valueptr)));
               ppufile.putdata(pcompilerwidestring(value.valueptr)^.data,pcompilerwidestring(value.valueptr)^.len*sizeof(tcompilerwidechar));
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


{****************************************************************************
                                  TENUMSYM
****************************************************************************}

    constructor tenumsym.create(const n : string;def : tenumdef;v : longint);
      begin
         inherited create(enumsym,n);
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
         inherited ppuload(enumsym,ppufile);
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
         inherited create(typesym,n);
         restype:=tt;
        { register the typesym for the definition }
        if assigned(restype.def) and
           (restype.def.deftype<>errordef) and
           not(assigned(restype.def.typesym)) then
         restype.def.typesym:=self;
      end;


    constructor ttypesym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(typesym,ppufile);
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


{****************************************************************************
                                  TSYSSYM
****************************************************************************}

    constructor tsyssym.create(const n : string;l : longint);
      begin
         inherited create(syssym,n);
         number:=l;
      end;

    constructor tsyssym.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(syssym,ppufile);
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
         inherited create(macrosym,n);
         owner:=nil;
         defined:=false;
         is_used:=false;
         is_compiler_var:=false;
         buftext:=nil;
         buflen:=0;
      end;

    constructor tmacro.ppuload(ppufile:tcompilerppufile);
      begin
         inherited ppuload(macrosym,ppufile);
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
           freemem(buftext);
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


    function tmacro.GetCopy:tmacro;
      var
        p : tmacro;
      begin
        p:=tmacro.create(realname);
        p.defined:=defined;
        p.is_used:=is_used;
        p.is_compiler_var:=is_compiler_var;
        p.buflen:=buflen;
        if assigned(buftext) then
          begin
            getmem(p.buftext,buflen);
            move(buftext^,p.buftext^,buflen);
          end;
        Result:=p;
      end;


{****************************************************************************
                                  TRTTISYM
****************************************************************************}

    constructor trttisym.create(const n:string;rt:trttitype);
      const
        prefix : array[trttitype] of string[5]=('$rtti','$init');
      begin
        inherited create(rttisym,prefix[rt]+n);
        include(symoptions,sp_internal);
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
        inherited ppuload(rttisym,ppufile);
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
         lab:=current_asmdata.newasmsymbol(mangledname,AB_EXTERNAL,AT_DATA);
        get_label:=lab;
      end;


end.
