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

{$i defines.inc}

interface

    uses
       { common }
       cutils,
       { target }
       cpuinfo,
       { symtable }
       symconst,symbase,symtype,symdef,
       { ppu }
       ppu,symppu,
       { aasm }
       aasm,cpubase,
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
{$ifdef GDB}
          isstabwritten : boolean;
{$endif GDB}
          refs          : longint;
          lastref,
          defref,
          lastwritten : tref;
          refcount    : longint;
          constructor create(const n : string);
          constructor loadsym(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure write(ppufile:tcompilerppufile);virtual;abstract;
          procedure writesym(ppufile:tcompilerppufile);
          procedure deref;override;
          procedure insert_in_data;virtual;
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
          constructor load(ppufile:tcompilerppufile);
          procedure generate_mangledname;override;
          procedure write(ppufile:tcompilerppufile);override;
       end;

       tunitsym = class(tstoredsym)
          unitsymtable : tsymtable;
          prevsym      : tunitsym;
          constructor create(const n : string;ref : tsymtable);
          constructor load(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure restoreunitsym;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       terrorsym = class(tstoredsym)
          constructor create;
       end;

       tprocsym = class(tstoredsym)
          defs      : pprocdeflist; { linked list of overloaded procdefs }
          is_global : boolean;
          overloadchecked : boolean;
          overloadcount   : longint; { amount of overloaded functions in this module }
          constructor create(const n : string);
          constructor load(ppufile:tcompilerppufile);
          destructor destroy;override;
          { writes all declarations except the specified one }
          procedure write_parameter_lists(skipdef:tprocdef);
          { tests, if all procedures definitions are defined and not }
          { only forward                                             }
          procedure check_forward;
          procedure unchain_overload;
          procedure write(ppufile:tcompilerppufile);override;
          procedure deref;override;
          procedure addprocdef(p:tprocdef);
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
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
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
          constructor create(const n : string;const tt : ttype);
          constructor create_dll(const n : string;const tt : ttype);
          constructor create_C(const n,mangled : string;const tt : ttype);
          constructor load(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure deref;override;
          procedure generate_mangledname;override;
          procedure set_mangledname(const s:string);
          procedure insert_in_data;override;
          function  getsize : longint;
          function  getvaluesize : longint;
          function  getpushsize : longint;
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
          constructor load(ppufile:tcompilerppufile);
          function  getsize : longint;
          procedure write(ppufile:tcompilerppufile);override;
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
          constructor load(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure deref;override;
          procedure insert_in_data;override;
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
          constructor load(ppufile:tcompilerppufile);
          procedure deref;override;
          function  mangledname : string;
          procedure write(ppufile:tcompilerppufile);override;
          procedure insert_in_data;override;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       ttypedconstsym = class(tstoredsym)
          typedconsttype  : ttype;
          is_writable     : boolean;
          constructor create(const n : string;p : tdef;writable : boolean);
          constructor createtype(const n : string;const tt : ttype;writable : boolean);
          constructor load(ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure generate_mangledname;override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure deref;override;
          function  getsize:longint;
          procedure insert_in_data;override;
{$ifdef GDB}
          function  stabstring : pchar;override;
{$endif GDB}
       end;

       tconstsym = class(tstoredsym)
          consttype   : ttype;
          consttyp    : tconsttyp;
          resstrindex,    { needed for resource strings }
          valueord    : tconstexprint; { used for ordinal values }
          valueordptr : TConstPtrUInt; { used for pointer values }
          valueptr    : pointer; { used for string, set, real values }
          len         : longint; { len is needed for string length }
          constructor create_ord(const n : string;t : tconsttyp;v : tconstexprint);
          constructor create_ord_typed(const n : string;t : tconsttyp;v : tconstexprint;const tt:ttype);
          constructor create_ordptr_typed(const n : string;t : tconsttyp;v : tconstptruint;const tt:ttype);
          constructor create_ptr(const n : string;t : tconsttyp;v : pointer);
          constructor create_ptr_typed(const n : string;t : tconsttyp;v : pointer;const tt:ttype);
          constructor create_string(const n : string;t : tconsttyp;str:pchar;l:longint);
          constructor load(ppufile:tcompilerppufile);
          destructor  destroy;override;
          function  mangledname : string;
          procedure deref;override;
          procedure write(ppufile:tcompilerppufile);override;
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
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
          procedure deref;override;
          procedure order;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tsyssym = class(tstoredsym)
          number : longint;
          constructor create(const n : string;l : longint);
          constructor load(ppufile:tcompilerppufile);
          destructor  destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       { compiler generated symbol to point to rtti and init/finalize tables }
       trttisym = class(tstoredsym)
          lab     : tasmsymbol;
          rttityp : trttitype;
          constructor create(const n:string;rt:trttitype);
          constructor load(ppufile:tcompilerppufile);
          procedure write(ppufile:tcompilerppufile);override;
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

       aktvarsym : tvarsym;     { pointer to the symbol for the
                                     currently read var, only used
                                     for variable directives }

       generrorsym : tsym;

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
       globtype,verbose,
       { target }
       systems,
       { symtable }
       symtable,types,
{$ifdef GDB}
       gdb,
{$endif GDB}
       { aasm }
       cpuasm,
       { module }
       fmodule,
       { codegen }
       cgbase,cresstr
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
         stringdispose(_mangledname);
        if assigned(defref) then
         begin
           defref.freechain;
           defref.free;
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


    { for most symbol types there is nothing to do at all }
    procedure tstoredsym.insert_in_data;
      begin
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

    constructor tlabelsym.load(ppufile:tcompilerppufile);

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


    procedure tlabelsym.write(ppufile:tcompilerppufile);
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

    constructor tunitsym.load(ppufile:tcompilerppufile);

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

    procedure tunitsym.write(ppufile:tcompilerppufile);
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
         defs:=nil;
         owner:=nil;
         is_global:=false;
         overloadchecked:=false;
         overloadcount:=0;
      end;


    constructor tprocsym.load(ppufile:tcompilerppufile);
      var
         pd : tprocdef;
      begin
         inherited loadsym(ppufile);
         typ:=procsym;
         defs:=nil;
         repeat
           pd:=tprocdef(ppufile.getderef);
           if pd=nil then
            break;
           addprocdef(pd);
         until false;
         is_global:=false;
         overloadchecked:=false;
         overloadcount:=-1; { invalid, not used anymore }
      end;


    destructor tprocsym.destroy;
      var
         hp,p : pprocdeflist;
      begin
         p:=defs;
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
         p:=defs;
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
         p:=defs;
         while assigned(p) do
           begin
              if (p^.def.procsym=self) and
                 (p^.def.forwarddef) then
                begin
                   MessagePos1(fileinfo,sym_e_forward_not_resolved,p^.def.fullprocname);
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
         p:=defs;
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
        pd^.next:=defs;
        defs:=pd;
      end;


    procedure tprocsym.write(ppufile:tcompilerppufile);
      var
         p : pprocdeflist;
      begin
         inherited writesym(ppufile);
         p:=defs;
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
         p:=defs;
         while assigned(p) do
           begin
              if (p^.def.procsym=self) then
                p^.def.write_references(ppufile,locals);
              p:=p^.next;
           end;
      end;


    procedure tprocsym.unchain_overload;
      var
         p,hp,
         first,
         last : pprocdeflist;
      begin
         { remove all overloaded procdefs from the
           procdeflist that are not in the current symtable }
         first:=nil;
         last:=nil;
         p:=defs;
         while assigned(p) do
           begin
              hp:=p^.next;
              if (p^.def.procsym=self) then
                begin
                  { keep in list }
                  if not assigned(first) then
                   begin
                     first:=p;
                     last:=p;
                   end
                  else
                   last^.next:=p;
                  last:=p;
                  p^.next:=nil;
                end
              else
                begin
                  { remove }
                  dispose(p);
                end;
              p:=hp;
           end;
         defs:=first;
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


    constructor tpropertysym.load(ppufile:tcompilerppufile);
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


    procedure tpropertysym.write(ppufile:tcompilerppufile);
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
        readaccess.clear;
        readaccess:=overriden.readaccess.getcopy;
        writeaccess.clear;
        writeaccess:=overriden.writeaccess.getcopy;
        storedaccess.clear;
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
         address:=pprocinfo(procinfo)^.return_offset;
      end;

    constructor tfuncretsym.load(ppufile:tcompilerppufile);
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

    procedure tfuncretsym.write(ppufile:tcompilerppufile);
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

    procedure tfuncretsym.insert_in_data;
      var
        varalign,l : longint;
      begin
        { if retoffset is already set then reuse it, this is needed
          when inserting the result variable }
        if procinfo^.return_offset<>0 then
         address:=procinfo^.return_offset
        else
         begin
           { allocate space in local if ret in acc or in fpu }
           if ret_in_acc(returntype.def) or
              (returntype.def.deftype=floatdef) then
            begin
              l:=returntype.def.size;
              varalign:=size_2_align(l);
              varalign:=used_align(varalign,aktalignment.localalignmin,owner.dataalignment);
              address:=align(owner.datasize+l,varalign);
              owner.datasize:=address;
              procinfo^.return_offset:=-address;
            end;
         end;
      end;


{****************************************************************************
                                  TABSOLUTESYM
****************************************************************************}

    constructor tabsolutesym.create(const n : string;const tt : ttype);
      begin
        inherited create(n,tt);
        typ:=absolutesym;
      end;


    constructor tabsolutesym.load(ppufile:tcompilerppufile);
      begin
         { Note: This needs to load everything of tvarsym.write }
         inherited load(ppufile);
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


    procedure tabsolutesym.write(ppufile:tcompilerppufile);
      var
        hvo : tvaroptions;
      begin
         { Note: This needs to write everything of tvarsym.write }
         inherited writesym(ppufile);
         ppufile.putbyte(byte(varspez));
         if read_member then
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


    procedure tabsolutesym.insert_in_data;
      begin
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
         include(varoptions,vo_is_C_var);
         stringdispose(_mangledname);
         _mangledname:=stringdup(mangled);
      end;


    constructor tvarsym.load(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         typ:=varsym;
         reg:=R_NO;
         refs := 0;
         varstate:=vs_used;
         varspez:=tvarspez(ppufile.getbyte);
         if read_member then
           address:=ppufile.getlongint
         else
           address:=0;
         localvarsym:=nil;
         ppufile.gettype(vartype);
         ppufile.getsmallset(varoptions);
         if (vo_is_C_var in varoptions) then
           _mangledname:=stringdup(ppufile.getstring);
      end;


    destructor tvarsym.destroy;
      begin
         inherited destroy;
      end;


    procedure tvarsym.deref;
      begin
        vartype.resolve;
      end;


    procedure tvarsym.write(ppufile:tcompilerppufile);
      var
        hvo : tvaroptions;
      begin
         inherited writesym(ppufile);
         ppufile.putbyte(byte(varspez));
         if read_member then
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


    function tvarsym.getpushsize : longint;
      begin
         if assigned(vartype.def) then
           begin
              case varspez of
                vs_out,
                vs_var :
                  getpushsize:=pointer_size;
                vs_value,
                vs_const :
                  begin
                      if push_addr_param(vartype.def) then
                        getpushsize:=pointer_size
                      else
                        getpushsize:=vartype.def.size;
                  end;
              end;
           end
         else
           getpushsize:=0;
      end;


    procedure tvarsym.insert_in_data;
      var
         varalign,
         l,modulo : longint;
         storefilepos : tfileposinfo;
      begin
        if (vo_is_external in varoptions) then
          exit;
        { handle static variables of objects especially }
        if read_member and (owner.symtabletype=objectsymtable) and
           (sp_static in symoptions) then
         begin
            { the data filed is generated in parser.pas
              with a tobject_FIELDNAME variable }
            { this symbol can't be loaded to a register }
            exclude(varoptions,vo_regable);
            exclude(varoptions,vo_fpuregable);
         end
        else
         if not(read_member) then
          begin
             { made problems with parameters etc. ! (FK) }
             {  check for instance of an abstract object or class }
             {
             if (tvarsym(sym).definition.deftype=objectdef) and
               ((tobjectdef(tvarsym(sym).definition).options and oo_is_abstract)<>0) then
               Message(sym_e_no_instance_of_abstract_object);
             }
             storefilepos:=aktfilepos;
             aktfilepos:=akttokenpos;
             if (vo_is_thread_var in varoptions) then
               l:=pointer_size
             else
               l:=getvaluesize;
             case owner.symtabletype of
               stt_exceptsymtable:
                 { can contain only one symbol, address calculated later }
                 ;
               localsymtable :
                 begin
                   varstate:=vs_declared;
                   varalign:=size_2_align(l);
                   varalign:=used_align(varalign,aktalignment.localalignmin,aktalignment.localalignmax);
                   address:=align(owner.datasize+l,varalign);
                   owner.datasize:=address;
                 end;
               staticsymtable :
                 begin
                   { enable unitialized warning for local symbols }
                   varstate:=vs_declared;
                   varalign:=size_2_align(l);
                   varalign:=used_align(varalign,aktalignment.varalignmin,aktalignment.varalignmax);
                   address:=align(owner.datasize,varalign);
                   { insert cut for smartlinking or alignment }
                   if (cs_create_smart in aktmoduleswitches) then
                     bssSegment.concat(Tai_cut.Create)
                   else if (address<>owner.datasize) then
                     bssSegment.concat(Tai_align.create(varalign));
                   owner.datasize:=address+l;
{$ifdef GDB}
                   if cs_debuginfo in aktmoduleswitches then
                      concatstabto(bsssegment);
{$endif GDB}
                   if (cs_create_smart in aktmoduleswitches) or
                      DLLSource or
                      (vo_is_exported in varoptions) or
                      (vo_is_C_var in varoptions) then
                     bssSegment.concat(Tai_datablock.Create_global(mangledname,l))
                   else
                     bssSegment.concat(Tai_datablock.Create(mangledname,l));
                   { this symbol can't be loaded to a register }
                   exclude(varoptions,vo_regable);
                   exclude(varoptions,vo_fpuregable);
                 end;
               globalsymtable :
                 begin
                   varalign:=size_2_align(l);
                   varalign:=used_align(varalign,aktalignment.varalignmin,aktalignment.varalignmax);
                   address:=align(owner.datasize,varalign);
                   { insert cut for smartlinking or alignment }
                   if (cs_create_smart in aktmoduleswitches) then
                     bssSegment.concat(Tai_cut.Create)
                   else if (address<>owner.datasize) then
                     bssSegment.concat(Tai_align.create(varalign));
                   owner.datasize:=address+l;
{$ifdef GDB}
                   if cs_debuginfo in aktmoduleswitches then
                     concatstabto(bsssegment);
{$endif GDB}
                   bssSegment.concat(Tai_datablock.Create_global(mangledname,l));
                   { this symbol can't be loaded to a register }
                   exclude(varoptions,vo_regable);
                   exclude(varoptions,vo_fpuregable);
                 end;
               recordsymtable,
               objectsymtable :
                 begin
                 { this symbol can't be loaded to a register }
                   exclude(varoptions,vo_regable);
                   exclude(varoptions,vo_fpuregable);
                 { get the alignment size }
                   if (aktalignment.recordalignmax=-1) then
                    begin
                      varalign:=vartype.def.alignment;
                      if (varalign>4) and ((varalign mod 4)<>0) and
                        (vartype.def.deftype=arraydef) then
                        begin
                          Message1(sym_w_wrong_C_pack,vartype.def.typename);
                        end;
                      if varalign=0 then
                        varalign:=l;
                      if (owner.dataalignment<aktalignment.maxCrecordalign) then
                       begin
                         if (varalign>16) and (owner.dataalignment<32) then
                          owner.dataalignment:=32
                         else if (varalign>12) and (owner.dataalignment<16) then
                          owner.dataalignment:=16
                         { 12 is needed for long double }
                         else if (varalign>8) and (owner.dataalignment<12) then
                          owner.dataalignment:=12
                         else if (varalign>4) and (owner.dataalignment<8) then
                          owner.dataalignment:=8
                         else if (varalign>2) and (owner.dataalignment<4) then
                          owner.dataalignment:=4
                         else if (varalign>1) and (owner.dataalignment<2) then
                          owner.dataalignment:=2;
                       end;
                      owner.dataalignment:=max(owner.dataalignment,aktalignment.maxCrecordalign);
                    end
                   else
                    varalign:=vartype.def.alignment;
                   if varalign=0 then
                     varalign:=size_2_align(l);
                   varalign:=used_align(varalign,aktalignment.recordalignmin,owner.dataalignment);
                   address:=align(owner.datasize,varalign);
                   owner.datasize:=address+l;
                 end;
               parasymtable :
                 begin
                   { here we need the size of a push instead of the
                     size of the data }
                   l:=getpushsize;
                   varalign:=size_2_align(l);
                   varstate:=vs_assigned;
                   { we need the new datasize already aligned so we can't
                     use the align_address here }
                   address:=owner.datasize;
                   varalign:=used_align(varalign,owner.dataalignment,owner.dataalignment);
                   owner.datasize:=align(address+l,varalign);
                 end
               else
                 begin
                     modulo:=owner.datasize and 3;
                     if (l>=4) and (modulo<>0) then
                       inc(owner.datasize,4-modulo)
                     else
                       if (l>=2) and ((modulo and 1)<>0) then
                         inc(owner.datasize);
                   address:=owner.datasize;
                   inc(owner.datasize,l);
                 end;
               end;
             aktfilepos:=storefilepos;
        end;
      end;

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
            stabstring := strpnew('"'+upper(owner.name^)+'__'+name+':'+st+
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
               vs_const : if push_addr_param(vartype.def) then
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
   {$ifdef i386}
         if reg<>R_NO then
           begin
              { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
              { this is the register order for GDB}
              stabstring:=strpnew('"'+name+':r'+st+'",'+
                        tostr(N_RSYM)+',0,'+
                        tostr(fileinfo.line)+','+tostr(GDB_i386index[reg]));
           end
         else
   {$endif i386}
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
{$ifdef i386}
      var stab_str : pchar;
{$endif i386}
      begin
         inherited concatstabto(asmlist);
{$ifdef i386}
      if (owner.symtabletype=parasymtable) and
         (reg<>R_NO) then
           begin
           { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
           { this is the register order for GDB}
              stab_str:=strpnew('"'+name+':r'
                     +tstoreddef(vartype.def).numberstring+'",'+
                     tostr(N_RSYM)+',0,'+
                     tostr(fileinfo.line)+','+tostr(GDB_i386index[reg]));
              asmList.concat(Tai_stabs.Create(stab_str));
           end;
{$endif i386}
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


    constructor ttypedconstsym.load(ppufile:tcompilerppufile);
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


    procedure ttypedconstsym.write(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.puttype(typedconsttype);
         ppufile.putbyte(byte(is_writable));
         ppufile.writeentry(ibtypedconstsym);
      end;


    procedure ttypedconstsym.insert_in_data;
      var
        curconstsegment : taasmoutput;
        address,l,varalign : longint;
        storefilepos : tfileposinfo;
      begin
        storefilepos:=aktfilepos;
        aktfilepos:=akttokenpos;
        if is_writable then
          curconstsegment:=datasegment
        else
          curconstsegment:=consts;
        l:=getsize;
        varalign:=size_2_align(l);
        varalign:=used_align(varalign,aktalignment.constalignmin,aktalignment.constalignmax);
        address:=align(owner.datasize,varalign);
        { insert cut for smartlinking or alignment }
        if (cs_create_smart in aktmoduleswitches) then
          curconstSegment.concat(Tai_cut.Create)
        else if (address<>owner.datasize) then
          curconstSegment.concat(Tai_align.create(varalign));
        owner.datasize:=address+l;
{$ifdef GDB}
        if cs_debuginfo in aktmoduleswitches then
          concatstabto(curconstsegment);
{$endif GDB}
        if (owner.symtabletype=globalsymtable) then
          begin
             if (owner.unitid=0) then
               curconstSegment.concat(Tai_symbol.Createdataname_global(mangledname,getsize));
          end
        else
          begin
             if (cs_create_smart in aktmoduleswitches) or
                DLLSource then
               curconstSegment.concat(Tai_symbol.Createdataname_global(mangledname,getsize))
             else
               curconstSegment.concat(Tai_symbol.Createdataname(mangledname,getsize));
          end;
        aktfilepos:=storefilepos;
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
         typ:=constsym;
         consttyp:=t;
         valueord:=v;
         valueordptr:=0;
         valueptr:=nil;
         ResStrIndex:=0;
         consttype.reset;
         len:=0;
      end;


    constructor tconstsym.create_ord_typed(const n : string;t : tconsttyp;v : tconstexprint;const tt:ttype);
      begin
         inherited create(n);
         typ:=constsym;
         consttyp:=t;
         valueord:=v;
         valueordptr:=0;
         valueptr:=nil;
         ResStrIndex:=0;
         consttype:=tt;
         len:=0;
      end;


    constructor tconstsym.create_ordptr_typed(const n : string;t : tconsttyp;v : tconstptruint;const tt:ttype);
      begin
         inherited create(n);
         typ:=constsym;
         consttyp:=t;
         valueord:=0;
         valueordptr:=v;
         valueptr:=nil;
         ResStrIndex:=0;
         consttype:=tt;
         len:=0;
      end;


    constructor tconstsym.create_ptr(const n : string;t : tconsttyp;v : pointer);
      begin
         inherited create(n);
         typ:=constsym;
         consttyp:=t;
         valueord:=0;
         valueordptr:=0;
         valueptr:=v;
         ResStrIndex:=0;
         consttype.reset;
         len:=0;
      end;


    constructor tconstsym.create_ptr_typed(const n : string;t : tconsttyp;v : pointer;const tt:ttype);
      begin
         inherited create(n);
         typ:=constsym;
         consttyp:=t;
         valueord:=0;
         valueordptr:=0;
         valueptr:=v;
         ResStrIndex:=0;
         consttype:=tt;
         len:=0;
      end;


    constructor tconstsym.create_string(const n : string;t : tconsttyp;str:pchar;l:longint);
      begin
         inherited create(n);
         typ:=constsym;
         consttyp:=t;
         valueord:=0;
         valueordptr:=0;
         valueptr:=str;
         consttype.reset;
         len:=l;
         if t=constresourcestring then
           ResStrIndex:=ResourceStrings.Register(name,pchar(valueptr),len);
      end;


    constructor tconstsym.load(ppufile:tcompilerppufile);
      var
         pd : pbestreal;
         ps : pnormalset;
         pc : pchar;
         l1,l2 : longint;

      begin
         inherited loadsym(ppufile);
         typ:=constsym;
         consttype.reset;
         consttyp:=tconsttyp(ppufile.getbyte);
         valueord:=0;
         valueordptr:=0;
         valueptr:=nil;
         case consttyp of
           constint:
             if sizeof(tconstexprint)=8 then
               begin
                  l1:=ppufile.getlongint;
                  l2:=ppufile.getlongint;
{$ifopt R+}
  {$define Range_check_on}
{$endif opt R+}
{$R- needed here }
                  valueord:=qword(l1)+(int64(l2) shl 32);
{$ifdef Range_check_on}
  {$R+}
  {$undef Range_check_on}
{$endif Range_check_on}
               end
             else
               valueord:=ppufile.getlongint;
           constwchar,
           constbool,
           constchar :
             valueord:=ppufile.getlongint;
           constord :
             begin
               ppufile.gettype(consttype);
               if sizeof(TConstExprInt)=8 then
                 begin
                    l1:=ppufile.getlongint;
                    l2:=ppufile.getlongint;
{$ifopt R+}
  {$define Range_check_on}
{$endif opt R+}
{$R- needed here }
                    valueord:=qword(l1)+(int64(l2) shl 32);
{$ifdef Range_check_on}
  {$R+}
  {$undef Range_check_on}
{$endif Range_check_on}
                 end
               else
                 valueord:=ppufile.getlongint;
             end;
           constpointer :
             begin
               ppufile.gettype(consttype);
               if sizeof(TConstPtrUInt)=8 then
                 begin
                    l1:=ppufile.getlongint;
                    l2:=ppufile.getlongint;
{$ifopt R+}
  {$define Range_check_on}
{$endif opt R+}
{$R- needed here }
                    valueordptr:=qword(l1)+(int64(l2) shl 32);
{$ifdef Range_check_on}
  {$R+}
  {$undef Range_check_on}
{$endif Range_check_on}
                 end
               else
                 valueordptr:=cardinal(ppufile.getlongint);
             end;
           conststring,
           constresourcestring :
             begin
               len:=ppufile.getlongint;
               getmem(pc,len+1);
               ppufile.getdata(pc^,len);
               if consttyp=constresourcestring then
                 ResStrIndex:=ppufile.getlongint;
               valueptr:=pc;
             end;
           constreal :
             begin
               new(pd);
               pd^:=ppufile.getreal;
               valueptr:=pd;
             end;
           constset :
             begin
               ppufile.gettype(consttype);
               new(ps);
               ppufile.getnormalset(ps^);
               valueptr:=ps;
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
            freemem(pchar(valueptr),len+1);
          constreal :
            dispose(pbestreal(valueptr));
          constset :
            dispose(pnormalset(valueptr));
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


    procedure tconstsym.write(ppufile:tcompilerppufile);
      begin
         inherited writesym(ppufile);
         ppufile.putbyte(byte(consttyp));
         case consttyp of
           constnil : ;
           constint:
             begin
               if sizeof(TConstExprInt)=8 then
                 begin
                    ppufile.putlongint(longint(lo(valueord)));
                    ppufile.putlongint(longint(hi(valueord)));
                 end
               else
                 ppufile.putlongint(valueord);
             end;
           constbool,
           constchar :
             ppufile.putlongint(valueord);
           constord :
             begin
               ppufile.puttype(consttype);
               if sizeof(TConstExprInt)=8 then
                 begin
                    ppufile.putlongint(longint(lo(valueord)));
                    ppufile.putlongint(longint(hi(valueord)));
                 end
               else
                 ppufile.putlongint(valueord);
             end;
           constpointer :
             begin
               ppufile.puttype(consttype);
               if sizeof(TConstPtrUInt)=8 then
                 begin
                    ppufile.putlongint(longint(lo(valueordptr)));
                    ppufile.putlongint(longint(hi(valueordptr)));
                 end
               else
                 ppufile.putlongint(longint(valueordptr));
             end;
           conststring,
           constresourcestring :
             begin
               ppufile.putlongint(len);
               ppufile.putdata(pchar(valueptr)^,len);
               if consttyp=constresourcestring then
                 ppufile.putlongint(ResStrIndex);
             end;
           constreal :
             ppufile.putreal(pbestreal(valueptr)^);
           constset :
             begin
               ppufile.puttype(consttype);
               ppufile.putnormalset(valueptr^);
             end;
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
                          st := 's'''+strpas(pchar(valueptr))+'''';
                          end;
            constbool,
            constint,
            constord,
            constchar : st := 'i'+int64tostr(valueord);
            constpointer :
              st := 'i'+int64tostr(valueordptr);
            constreal : begin
                        system.str(pbestreal(valueptr)^,st);
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


    constructor tenumsym.load(ppufile:tcompilerppufile);
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


    procedure tenumsym.write(ppufile:tcompilerppufile);
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


    constructor ttypesym.load(ppufile:tcompilerppufile);
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


    procedure ttypesym.write(ppufile:tcompilerppufile);
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

    constructor tsyssym.load(ppufile:tcompilerppufile);
      begin
         inherited loadsym(ppufile);
         typ:=syssym;
         number:=ppufile.getlongint;
      end;

    destructor tsyssym.destroy;
      begin
        inherited destroy;
      end;

    procedure tsyssym.write(ppufile:tcompilerppufile);
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


    constructor trttisym.load(ppufile:tcompilerppufile);
      begin
        inherited loadsym(ppufile);
        typ:=rttisym;
        lab:=nil;
        rttityp:=trttitype(ppufile.getbyte);
      end;


    procedure trttisym.write(ppufile:tcompilerppufile);
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
         lab:=newasmsymboltype(mangledname,AB_GLOBAL,AT_DATA);
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
           rttiList.concat(Tai_symbol.Create(rsym.get_label,0));
           def.write_rtti_data(initrtti);
           rttiList.concat(Tai_symbol_end.Create(rsym.get_label));
         end;
      end;



end.
{
  $Log$
  Revision 1.37  2002-05-14 19:34:51  peter
    * removed old logs and updated copyright year

  Revision 1.36  2002/05/12 16:53:15  peter
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

  Revision 1.35  2002/04/19 15:46:03  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.34  2002/04/16 16:12:47  peter
    * give error when using enums with jumps as array index
    * allow char as enum value

  Revision 1.33  2002/04/15 19:08:22  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables

  Revision 1.32  2002/04/07 13:37:29  carl
  + change unit use

  Revision 1.31  2002/02/03 09:30:04  peter
    * more fixes for protected handling

}
