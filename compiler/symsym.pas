{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

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
       { aasm }
       aasm,cpubase
       ;

    type
{************************************************
                   TSym
************************************************}

       { this object is the base for all symbol objects }
       tstoredsym = class(tsym)
{$ifdef GDB}
          isstabwritten : boolean;
{$endif GDB}
          refs          : longint;
          lastref,
          defref,
          lastwritten : tref;
          refcount    : longint;
          constructor create(const n : string);
          constructor loadsym;
          destructor destroy;override;
          procedure write;virtual;abstract;
          procedure writesym;
          function  mangledname : string;override;
          procedure insert_in_data;virtual;
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : taasmoutput);virtual;
{$endif GDB}
          procedure load_references;virtual;
          function  write_references : boolean;virtual;
       end;

       tlabelsym = class(tstoredsym)
          lab     : tasmlabel;
          used,
          defined : boolean;
          code : pointer; { should be ptree! }
          constructor create(const n : string; l : tasmlabel);
          destructor destroy;override;
          constructor load;
          function mangledname : string;override;
          procedure write;override;
       end;

       tunitsym = class(tstoredsym)
          unitsymtable : tsymtable;
          prevsym      : tunitsym;
          constructor create(const n : string;ref : tsymtable);
          constructor load;
          destructor destroy;override;
          procedure write;override;
          procedure restoreunitsym;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       terrorsym = class(tstoredsym)
          constructor create;
       end;

       tprocsym = class(tstoredsym)
          definition  : tprocdef;
{$ifdef CHAINPROCSYMS}
          nextprocsym : tprocsym;
{$endif CHAINPROCSYMS}
          is_global   : boolean;
          constructor create(const n : string);
          constructor load;
          destructor destroy;override;
          function mangledname : string;override;
          { writes all declarations except the specified one }
          procedure write_parameter_lists(skitdef:tprocdef);
          { tests, if all procedures definitions are defined and not }
          { only forward                                             }
          procedure check_forward;
          procedure order_overloaded;
          procedure write;override;
          procedure deref;override;
          procedure load_references;override;
          function  write_references : boolean;override;
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
          constructor load;
          procedure write;override;
          function  gettypedef:tdef;override;
          procedure prederef;override;
          procedure load_references;override;
          function  write_references : boolean;override;
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
          constructor load;
          destructor  destroy;override;
          procedure write;override;
          procedure deref;override;
          procedure setmangledname(const s : string);
          function  mangledname : string;override;
          procedure insert_in_data;override;
          function  getsize : longint;
          function  getvaluesize : longint;
          function  getpushsize : longint;
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       private
          _mangledname  : pchar;
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
          constructor load;
          function  getsize : longint;
          procedure write;override;
          function  gettypedef:tdef;override;
          procedure deref;override;
          procedure dooverride(overriden:tpropertysym);
{$ifdef GDB}
          function  stabstring : pchar;override;
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tfuncretsym = class(tstoredsym)
          funcretprocinfo : pointer{ should be pprocinfo};
          rettype  : ttype;
          address  : longint;
          constructor create(const n : string;approcinfo : pointer{pprocinfo});
          constructor load;
          destructor  destroy;override;
          procedure write;override;
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
          constructor load;
          procedure deref;override;
          function  mangledname : string;override;
          procedure write;override;
          procedure insert_in_data;override;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       ttypedconstsym = class(tstoredsym)
          prefix          : pstring;
          typedconsttype  : ttype;
          is_really_const : boolean;
          constructor create(const n : string;p : tdef;really_const : boolean);
          constructor createtype(const n : string;const tt : ttype;really_const : boolean);
          constructor load;
          destructor destroy;override;
          function  mangledname : string;override;
          procedure write;override;
          procedure deref;override;
          function  getsize:longint;
          procedure insert_in_data;override;
{$ifdef GDB}
          function  stabstring : pchar;override;
{$endif GDB}
       end;

       tconstsym = class(tstoredsym)
          consttype  : ttype;
          consttyp : tconsttyp;
          resstrindex,    { needed for resource strings }
          value      : tconstexprint;
          len        : longint; { len is needed for string length }
          constructor create(const n : string;t : tconsttyp;v : tconstexprint);
          constructor create_typed(const n : string;t : tconsttyp;v : tconstexprint;const tt:ttype);
          constructor create_string(const n : string;t : tconsttyp;str:pchar;l:longint);
          constructor load;
          destructor  destroy;override;
          function  mangledname : string;override;
          procedure deref;override;
          procedure write;override;
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
          constructor load;
          procedure write;override;
          procedure deref;override;
          procedure order;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
       end;

       tsyssym = class(tstoredsym)
          number : longint;
          constructor create(const n : string;l : longint);
          constructor load;
          destructor  destroy;override;
          procedure write;override;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);override;
{$endif GDB}
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

       aktcallprocsym : tprocsym;  { pointer to the symbol for the
                                     currently be called procedure,
                                     only set/unset in firstcall }

       aktvarsym : tvarsym;     { pointer to the symbol for the
                                     currently read var, only used
                                     for variable directives }

       generrorsym : tsym;

       procprefix : string;     { prefix generated for the current compiled proc }

    const
       current_object_option : tsymoptions = [sp_public];


implementation

    uses
{$ifdef Delphi}
       sysutils,
{$else Delphi}
       strings,
{$endif Delphi}
       { global }
       globtype,globals,verbose,
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
       { ppu }
       symppu,ppu,
       { codegen }
       hcodegen,cresstr
       ;

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
      end;


    constructor tstoredsym.loadsym;
      var
        s  : string;
        nr : word;
      begin
         nr:=readword;
         s:=readstring;
         inherited create(s);
         { force the correct indexnr. must be after create! }
         indexnr:=nr;
         readsmallset(symoptions);
         readposinfo(fileinfo);
         lastref:=nil;
         defref:=nil;
         refs:=0;
         lastwritten:=nil;
         refcount:=0;
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
      end;


    procedure tstoredsym.load_references;
      var
        pos : tfileposinfo;
        move_last : boolean;
      begin
        move_last:=lastwritten=lastref;
        while (not current_ppu^.endofentry) do
         begin
           readposinfo(pos);
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

    function tstoredsym.write_references : boolean;
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
                     writederef(self);
                     symref_written:=true;
                  end;
                writeposinfo(ref.posinfo);
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
          current_ppu^.writeentry(ibsymref);
        write_references:=symref_written;
      end;


    destructor tstoredsym.destroy;
      begin
        if assigned(defref) then
         begin
           defref.freechain;
           defref.free;
         end;
        inherited destroy;
      end;


    procedure tstoredsym.writesym;
      begin
         writeword(indexnr);
         writestring(_realname^);
         writesmallset(symoptions);
         writeposinfo(fileinfo);
      end;


    function tstoredsym.mangledname : string;
      begin
         mangledname:=name;
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

    constructor tlabelsym.load;

      begin
         inherited loadsym;
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


    function tlabelsym.mangledname : string;
      begin
         mangledname:=lab.name;
      end;


    procedure tlabelsym.write;
      begin
         if owner.symtabletype=globalsymtable then
           Message(sym_e_ill_label_decl)
         else
           begin
              inherited writesym;
              current_ppu^.writeentry(iblabelsym);
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
         prevsym:=tglobalsymtable(ref).unitsym;
         tglobalsymtable(ref).unitsym:=self;
         refs:=0;
      end;

    constructor tunitsym.load;

      begin
         inherited loadsym;
         typ:=unitsym;
         unitsymtable:=nil;
         prevsym:=nil;
         refs:=0;
      end;

    { we need to remove it from the prevsym chain ! }

    procedure tunitsym.restoreunitsym;
      var pus,ppus : tunitsym;
      begin
         if assigned(unitsymtable) then
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

    procedure tunitsym.write;
      begin
         inherited writesym;
         current_ppu^.writeentry(ibunitsym);
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
         definition:=nil;
         owner:=nil;
         is_global := false;
      end;


    constructor tprocsym.load;
      begin
         inherited loadsym;
         typ:=procsym;
         definition:=tprocdef(readderef);
         is_global := false;
      end;


    destructor tprocsym.destroy;
      begin
         inherited destroy;
      end;


    function tprocsym.mangledname : string;
      begin
         mangledname:=definition.mangledname;
      end;


    procedure tprocsym.write_parameter_lists(skitdef:tprocdef);
      var
         p : tprocdef;
      begin
         p:=definition;
         while assigned(p) do
           begin
              if p<>skitdef then
                MessagePos1(p.fileinfo,sym_b_param_list,p.fullprocname);
              p:=p.nextoverloaded;
           end;
      end;


    procedure tprocsym.check_forward;
      var
         pd : tprocdef;
      begin
         pd:=definition;
         while assigned(pd) do
           begin
              if pd.forwarddef then
                begin
                   MessagePos1(fileinfo,sym_e_forward_not_resolved,pd.fullprocname);
                   { Turn futher error messages off }
                   pd.forwarddef:=false;
                end;
              pd:=pd.nextoverloaded;
              { do not check defs of operators in other units }
              if assigned(pd) and (pd.procsym<>self) then
                pd:=nil;
           end;
      end;


    procedure tprocsym.deref;
      var
        pd : tprocdef;
      begin
         resolvedef(tdef(definition));
         pd:=definition;
         while assigned(pd) do
           begin
              pd.procsym:=self;
              pd:=pd.nextoverloaded;
           end;
      end;

    procedure tprocsym.order_overloaded;
      var firstdef,currdef,lastdef,nextotdef : tprocdef;
      begin
         if not assigned(definition) then
           exit;
         firstdef:=definition;
         currdef:=definition;
         while assigned(currdef) and (currdef.owner=firstdef.owner) do
           begin
             currdef.count:=false;
             currdef:=currdef.nextoverloaded;
           end;
         nextotdef:=currdef;
         definition:=definition.nextoverloaded;
         firstdef.nextoverloaded:=nil;
         while (definition<>nextotdef) do
           begin
             currdef:=firstdef;
             lastdef:=definition;
             definition:=definition.nextoverloaded;
             if lastdef.mangledname<firstdef.mangledname then
               begin
                 lastdef.nextoverloaded:=firstdef;
                 firstdef:=lastdef;
               end
             else
               begin
                 while assigned(currdef.nextoverloaded) and
                    (lastdef.mangledname>currdef.nextoverloaded.mangledname) do
                   currdef:=currdef.nextoverloaded;
                 lastdef.nextoverloaded:=currdef.nextoverloaded;
                 currdef.nextoverloaded:=lastdef;
               end;
           end;
         definition:=firstdef;
         currdef:=definition;
         while assigned(currdef) do
           begin
             currdef.count:=true;
             lastdef:=currdef;
             currdef:=currdef.nextoverloaded;
           end;
         lastdef.nextoverloaded:=nextotdef;
      end;

    procedure tprocsym.write;
      begin
         inherited writesym;
         writederef(definition);
         current_ppu^.writeentry(ibprocsym);
      end;


    procedure tprocsym.load_references;
      (*var
        prdef,prdef2 : tprocdef;
        b : byte; *)
      begin
         inherited load_references;
         (*prdef:=definition;
           done in tsymtable.load_browser (PM)
         { take care about operators !!  }
         if (current_module^.flags and uf_has_browser) <>0 then
           while assigned(prdef) and (prdef.owner=definition.owner) do
             begin
                b:=current_ppu^.readentry;
                if b<>ibdefref then
                  Message(unit_f_ppu_read_error);
                prdef2:=tprocdef(readdefref);
                resolvedef(prdef2);
                if prdef<>prdef2 then
                  Message(unit_f_ppu_read_error);
                prdef.load_references;
                prdef:=prdef.nextoverloaded;
             end; *)
      end;

    function tprocsym.write_references : boolean;
      var
        prdef : tprocdef;
      begin
         write_references:=false;
         if not inherited write_references then
           exit;
         write_references:=true;
         prdef:=definition;
         while assigned(prdef) and (prdef.owner=definition.owner) do
          begin
            prdef.write_references;
            prdef:=prdef.nextoverloaded;
          end;
      end;


{$ifdef GDB}
    function tprocsym.stabstring : pchar;
     Var RetType : Char;
         Obj,Info : String;
         stabsstr : string;
         p : pchar;
    begin
      obj := name;
      info := '';
      if is_global then
       RetType := 'F'
      else
       RetType := 'f';
     if assigned(owner) then
      begin
        if (owner.symtabletype = objectsymtable) then
         obj := upper(owner.name^)+'__'+name;
        { this code was correct only as long as the local symboltable
          of the parent had the same name as the function
          but this is no true anymore !! PM
        if (owner.symtabletype=localsymtable) and assigned(owner.name) then
         info := ','+name+','+owner.name^;  }
        if (owner.symtabletype=localsymtable) and assigned(owner.defowner) and
           assigned(tprocdef(owner.defowner).procsym) then
          info := ','+name+','+tprocdef(owner.defowner).procsym.name;
      end;
     stabsstr:=definition.mangledname;
     getmem(p,length(stabsstr)+255);
     strpcopy(p,'"'+obj+':'+RetType
           +tstoreddef(definition.rettype.def).numberstring+info+'",'+tostr(n_function)
           +',0,'+
           tostr(aktfilepos.line)
           +',');
     strpcopy(strend(p),stabsstr);
     stabstring:=strnew(p);
     freemem(p,length(stabsstr)+255);
    end;

    procedure tprocsym.concatstabto(asmlist : taasmoutput);
    begin
      if (pocall_internproc in definition.proccalloptions) then exit;
      if not isstabwritten then
        asmList.concat(Tai_stabs.Create(stabstring));
      isstabwritten := true;
      if assigned(definition.parast) then
        tstoredsymtable(definition.parast).concatstabto(asmlist);
      { local type defs and vars should not be written
        inside the main proc stab }
      if assigned(definition.localst) and
         (lexlevel>main_program_level) then
        tstoredsymtable(definition.localst).concatstabto(asmlist);
      definition.is_def_stab_written := written;
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


    constructor tpropertysym.load;
      begin
         inherited loadsym;
         typ:=propertysym;
         readsmallset(propoptions);
         if (ppo_is_override in propoptions) then
          begin
            propoverriden:=tpropertysym(readderef);
            { we need to have these objects initialized }
            readaccess:=tsymlist.create;
            writeaccess:=tsymlist.create;
            storedaccess:=tsymlist.create;
          end
         else
          begin
            proptype.load;
            index:=readlong;
            default:=readlong;
            indextype.load;
            readaccess:=tsymlist.load;
            writeaccess:=tsymlist.load;
            storedaccess:=tsymlist.load;
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
           resolvesym(tsym(propoverriden));
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


    procedure tpropertysym.write;
      begin
        inherited writesym;
        writesmallset(propoptions);
        if (ppo_is_override in propoptions) then
         writederef(propoverriden)
        else
         begin
           proptype.write;
           writelong(index);
           writelong(default);
           indextype.write;
           readaccess.write;
           writeaccess.write;
           storedaccess.write;
         end;
        current_ppu^.writeentry(ibpropertysym);
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

    constructor tfuncretsym.create(const n : string;approcinfo : pointer{pprocinfo});

      begin
         inherited create(n);
         typ:=funcretsym;
         funcretprocinfo:=approcinfo;
         rettype:=pprocinfo(approcinfo)^.returntype;
         { address valid for ret in param only }
         { otherwise set by insert             }
         address:=pprocinfo(approcinfo)^.return_offset;
      end;

    constructor tfuncretsym.load;
      begin
         inherited loadsym;
         rettype.load;
         address:=readlong;
         funcretprocinfo:=nil;
         typ:=funcretsym;
      end;

    destructor tfuncretsym.destroy;
      begin
        inherited destroy;
      end;

    procedure tfuncretsym.write;
      begin
         inherited writesym;
         rettype.write;
         writelong(address);
         current_ppu^.writeentry(ibfuncretsym);
      end;

    procedure tfuncretsym.deref;
      begin
         rettype.resolve;
      end;

{$ifdef GDB}
    procedure tfuncretsym.concatstabto(asmlist : taasmoutput);
      begin
        { Nothing to do here, it is done in genexitcode  }
      end;
{$endif GDB}

    procedure tfuncretsym.insert_in_data;
      var
        l : longint;
      begin
        { if retoffset is already set then reuse it, this is needed
          when inserting the result variable }
        if procinfo^.return_offset<>0 then
         address:=procinfo^.return_offset
        else
         begin
           { allocate space in local if ret in acc or in fpu }
           if ret_in_acc(procinfo^.returntype.def) or (procinfo^.returntype.def.deftype=floatdef) then
            begin
              l:=rettype.def.size;
              inc(owner.datasize,l);
{$ifdef m68k}
              { word alignment required for motorola }
              if (l=1) then
               inc(owner.datasize,1)
              else
{$endif}
              if (l>=4) and ((owner.datasize and 3)<>0) then
                inc(owner.datasize,4-(owner.datasize and 3))
              else if (l>=2) and ((owner.datasize and 1)<>0) then
                inc(owner.datasize,2-(owner.datasize and 1));
              address:=owner.datasize;
              procinfo^.return_offset:=-owner.datasize;
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


    constructor tabsolutesym.load;
      begin
         { Note: This needs to load everything of tvarsym.write }
         inherited load;
         { load absolute }
         typ:=absolutesym;
         ref:=nil;
         address:=0;
         asmname:=nil;
         abstyp:=absolutetyp(readbyte);
         absseg:=false;
         case abstyp of
           tovar :
             asmname:=stringdup(readstring);
           toasm :
             asmname:=stringdup(readstring);
           toaddr :
             begin
               address:=readlong;
               absseg:=boolean(readbyte);
             end;
         end;
      end;


    procedure tabsolutesym.write;
      var
        hvo : tvaroptions;
      begin
         { Note: This needs to write everything of tvarsym.write }
         inherited writesym;
         writebyte(byte(varspez));
         if read_member then
           writelong(address);
         { write only definition or definitionsym }
         vartype.write;
         hvo:=varoptions-[vo_regable];
         writesmallset(hvo);
         writebyte(byte(abstyp));
         case abstyp of
           tovar :
             writestring(ref.name);
           toasm :
             writestring(asmname^);
           toaddr :
             begin
               writelong(address);
               writebyte(byte(absseg));
             end;
         end;
        current_ppu^.writeentry(ibabsolutesym);
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
             mangledname:=ref.mangledname;
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
         setmangledname(mangled);
      end;


    constructor tvarsym.load;
      begin
         inherited loadsym;
         typ:=varsym;
         _mangledname:=nil;
         reg:=R_NO;
         refs := 0;
         varstate:=vs_used;
         varspez:=tvarspez(readbyte);
         if read_member then
           address:=readlong
         else
           address:=0;
         localvarsym:=nil;
         vartype.load;
         readsmallset(varoptions);
         if (vo_is_C_var in varoptions) then
           setmangledname(readstring);
      end;


    destructor tvarsym.destroy;
      begin
         strdispose(_mangledname);
         inherited destroy;
      end;


    procedure tvarsym.deref;
      begin
        vartype.resolve;
      end;


    procedure tvarsym.write;
      var
        hvo : tvaroptions;
      begin
         inherited writesym;
         writebyte(byte(varspez));
         if read_member then
          writelong(address);
         vartype.write;
         { symbols which are load are never candidates for a register,
           turn off the regable }
         hvo:=varoptions-[vo_regable];
         writesmallset(hvo);
         if (vo_is_C_var in varoptions) then
           writestring(mangledname);
         current_ppu^.writeentry(ibvarsym);
      end;


    procedure tvarsym.setmangledname(const s : string);
      begin
        _mangledname:=strpnew(s);
      end;


    function tvarsym.mangledname : string;
      var
        prefix : string;
      begin
         if assigned(_mangledname) then
           begin
              mangledname:=strpas(_mangledname);
              exit;
           end;
         case owner.symtabletype of
           staticsymtable :
             if (cs_create_smart in aktmoduleswitches) then
               prefix:='_'+upper(owner.name^)+'$$$_'
             else
               prefix:='_';
           globalsymtable :
             prefix:=
              'U_'+upper(owner.name^)+'_';
           else
             Message(sym_e_invalid_call_tvarsymmangledname);
         end;
         mangledname:=prefix+name;
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
                  getpushsize:=target_info.size_of_pointer;
                vs_value,
                vs_const :
                  begin
                      if push_addr_param(vartype.def) then
                        getpushsize:=target_info.size_of_pointer
                      else
                        getpushsize:=vartype.def.size;
                  end;
              end;
           end
         else
           getpushsize:=0;
      end;


    function  data_align(length : longint) : longint;
      begin
         (* this is useless under go32v2 at least
         because the section are only align to dword
         if length>8 then
           data_align:=16
         else if length>4 then
           data_align:=8
         else *)
         if length>2 then
           data_align:=4
         else
          if length>1 then
           data_align:=2
         else
           data_align:=1;
      end;


    procedure tvarsym.insert_in_data;
      var
         varalign,
         l,ali,modulo : longint;
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
               l:=4
             else
               l:=getvaluesize;
             case owner.symtabletype of
               stt_exceptsymtable:
                 { can contain only one symbol, address calculated later }
                 ;
               localsymtable :
                 begin
                   varstate:=vs_declared;
                   modulo:=owner.datasize and 3;
{$ifdef m68k}
                 { word alignment required for motorola }
                   if (l=1) then
                    l:=2
                   else
{$endif}
{
                   if (cs_optimize in aktglobalswitches) and
                      (aktoptprocessor in [classp5,classp6]) and
                      (l>=8) and ((owner.datasize and 7)<>0) then
                     inc(owner.datasize,8-(owner.datasize and 7))
                   else
}
                     begin
                        if (l>=4) and (modulo<>0) then
                          inc(l,4-modulo)
                        else
                          if (l>=2) and ((modulo and 1)<>0) then
                            inc(l,2-(modulo and 1));
                     end;
                   inc(owner.datasize,l);
                   address:=owner.datasize;
                 end;
               staticsymtable :
                 begin
                   { enable unitialized warning for local symbols }
                   varstate:=vs_declared;
                   if (cs_create_smart in aktmoduleswitches) then
                     bssSegment.concat(Tai_cut.Create);
                   ali:=data_align(l);
                   if ali>1 then
                     begin
                        modulo:=owner.datasize mod ali;
                        if modulo>0 then
                          inc(owner.datasize,ali-modulo);
                     end;
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
                   { increase datasize }
                   inc(owner.datasize,l);
                   { this symbol can't be loaded to a register }
                   exclude(varoptions,vo_regable);
                   exclude(varoptions,vo_fpuregable);
                 end;
               globalsymtable :
                 begin
                   if (cs_create_smart in aktmoduleswitches) then
                     bssSegment.concat(Tai_cut.Create);
                   ali:=data_align(l);
                   if ali>1 then
                     begin
                        modulo:=owner.datasize mod ali;
                        if modulo>0 then
                          inc(owner.datasize,ali-modulo);
                     end;
{$ifdef GDB}
                   if cs_debuginfo in aktmoduleswitches then
                     concatstabto(bsssegment);
{$endif GDB}
                   bssSegment.concat(Tai_datablock.Create_global(mangledname,l));
                   inc(owner.datasize,l);
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
                   if (aktpackrecords=packrecord_C) then
                    begin
                      varalign:=vartype.def.alignment;
                      if (varalign>4) and ((varalign mod 4)<>0) and
                        (vartype.def.deftype=arraydef) then
                        begin
                          Message1(sym_w_wrong_C_pack,vartype.def.typename);
                        end;
                      if varalign=0 then
                        varalign:=l;
                      if (owner.dataalignment<target_info.maxCrecordalignment) then
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
                      if owner.dataalignment>target_info.maxCrecordalignment then
                        owner.dataalignment:=target_info.maxCrecordalignment;
                    end
                   else
                    varalign:=vartype.def.alignment;
                   if varalign=0 then
                     varalign:=l;
                 { align record and object fields }
                   if (varalign=1) or (owner.dataalignment=1) then
                    begin
                      address:=owner.datasize;
                      inc(owner.datasize,l)
                    end
                   else if (varalign=2) or (owner.dataalignment=2) then
                     begin
                       owner.datasize:=(owner.datasize+1) and (not 1);
                       address:=owner.datasize;
                       inc(owner.datasize,l)
                     end
                   else if (varalign<=4) or (owner.dataalignment=4) then
                     begin
                       owner.datasize:=(owner.datasize+3) and (not 3);
                       address:=owner.datasize;
                       inc(owner.datasize,l);
                     end
                   else if (varalign<=8) or (owner.dataalignment=8) then
                     begin
                       owner.datasize:=(owner.datasize+7) and (not 7);
                       address:=owner.datasize;
                       inc(owner.datasize,l);
                     end
                         { 12 is needed for C long double support }
                   else if (varalign<=12) and (owner.dataalignment=12) then
                     begin
                       owner.datasize:=((owner.datasize+11) div 12) * 12;
                       address:=owner.datasize;
                       inc(owner.datasize,l);
                     end
                   else if (varalign<=16) or (owner.dataalignment=16) then
                     begin
                       owner.datasize:=(owner.datasize+15) and (not 15);
                       address:=owner.datasize;
                       inc(owner.datasize,l);
                     end
                   else if (varalign<=32) or (owner.dataalignment=32) then
                     begin
                       owner.datasize:=(owner.datasize+31) and (not 31);
                       address:=owner.datasize;
                       inc(owner.datasize,l);
                     end
                    else
                     internalerror(1000022);
                 end;
               parasymtable :
                 begin
                   { here we need the size of a push instead of the
                     size of the data }
                   l:=getpushsize;
                   varstate:=vs_assigned;
                   address:=owner.datasize;
                   owner.datasize:=align(owner.datasize+l,target_info.stackalignment);
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

    constructor ttypedconstsym.create(const n : string;p : tdef;really_const : boolean);
      begin
         inherited create(n);
         typ:=typedconstsym;
         typedconsttype.setdef(p);
         is_really_const:=really_const;
         prefix:=stringdup(procprefix);
      end;


    constructor ttypedconstsym.createtype(const n : string;const tt : ttype;really_const : boolean);
      begin
         ttypedconstsym(self).create(n,nil,really_const);
         typedconsttype:=tt;
      end;


    constructor ttypedconstsym.load;
      begin
         inherited loadsym;
         typ:=typedconstsym;
         typedconsttype.load;
         prefix:=stringdup(readstring);
         is_really_const:=boolean(readbyte);
      end;


    destructor ttypedconstsym.destroy;
      begin
         stringdispose(prefix);
         inherited destroy;
      end;


    function ttypedconstsym.mangledname : string;
      begin
         mangledname:='TC_'+prefix^+'_'+name;
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


    procedure ttypedconstsym.write;
      begin
         inherited writesym;
         typedconsttype.write;
         writestring(prefix^);
         writebyte(byte(is_really_const));
         current_ppu^.writeentry(ibtypedconstsym);
      end;


    procedure ttypedconstsym.insert_in_data;
      var
        curconstsegment : taasmoutput;
        l,ali,modulo : longint;
        storefilepos : tfileposinfo;
      begin
        storefilepos:=aktfilepos;
        aktfilepos:=akttokenpos;
        if is_really_const then
          curconstsegment:=consts
        else
          curconstsegment:=datasegment;
        if (cs_create_smart in aktmoduleswitches) then
          curconstSegment.concat(Tai_cut.Create);
        l:=getsize;
        ali:=data_align(l);
        if ali>1 then
          begin
             curconstSegment.concat(Tai_align.Create(ali));
             modulo:=owner.datasize mod ali;
             if modulo>0 then
               inc(owner.datasize,ali-modulo);
          end;
        {  Why was there no owner size update here ??? }
        inc(owner.datasize,l);
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

    constructor tconstsym.create(const n : string;t : tconsttyp;v : TConstExprInt);
      begin
         inherited create(n);
         typ:=constsym;
         consttyp:=t;
         value:=v;
         ResStrIndex:=0;
         consttype.reset;
         len:=0;
      end;


    constructor tconstsym.create_typed(const n : string;t : tconsttyp;v : tconstexprint;const tt:ttype);
      begin
         inherited create(n);
         typ:=constsym;
         consttyp:=t;
         value:=v;
         ResStrIndex:=0;
         consttype:=tt;
         len:=0;
      end;


    constructor tconstsym.create_string(const n : string;t : tconsttyp;str:pchar;l:longint);
      begin
         inherited create(n);
         typ:=constsym;
         consttyp:=t;
         value:=longint(str);
         consttype.reset;
         len:=l;
         if t=constresourcestring then
           ResStrIndex:=ResourceStrings.Register(name,pchar(tpointerord(value)),len);
      end;

    constructor tconstsym.load;
      var
         pd : pbestreal;
         ps : pnormalset;
         pc : pchar;
         l1,l2 : longint;

      begin
         inherited loadsym;
         typ:=constsym;
         consttype.reset;
         consttyp:=tconsttyp(readbyte);
         case consttyp of
           constint:
             if sizeof(tconstexprint)=8 then
               begin
                  l1:=readlong;
                  l2:=readlong;
{$ifopt R+}
  {$define Range_check_on}
{$endif opt R+}
{$R- needed here }
                  value:=qword(l1)+(int64(l2) shl 32);
{$ifdef Range_check_on}
  {$R+}
  {$undef Range_check_on}
{$endif Range_check_on}
               end
             else
               value:=readlong;
           constbool,
           constchar :
             value:=readlong;
           constpointer,
           constord :
             begin
               consttype.load;
               if sizeof(TConstExprInt)=8 then
                 begin
                    l1:=readlong;
                    l2:=readlong;
{$ifopt R+}
  {$define Range_check_on}
{$endif opt R+}
{$R- needed here }
                    value:=qword(l1)+(int64(l2) shl 32);
{$ifdef Range_check_on}
  {$R+}
  {$undef Range_check_on}
{$endif Range_check_on}
                 end
               else
                 value:=readlong;
             end;
           conststring,constresourcestring :
             begin
               len:=readlong;
               getmem(pc,len+1);
               current_ppu^.getdata(pc^,len);
               if consttyp=constresourcestring then
                 ResStrIndex:=readlong;
               value:=tpointerord(pc);
             end;
           constreal :
             begin
               new(pd);
               pd^:=readreal;
               value:=tpointerord(pd);
             end;
           constset :
             begin
               consttype.load;
               new(ps);
               readnormalset(ps^);
               value:=tpointerord(ps);
             end;
           constnil : ;
           else
             Message1(unit_f_ppu_invalid_entry,tostr(ord(consttyp)));
         end;
      end;


    destructor tconstsym.destroy;
      begin
        case consttyp of
          conststring,constresourcestring :
            freemem(pchar(tpointerord(value)),len+1);
          constreal :
            dispose(pbestreal(tpointerord(value)));
          constset :
            dispose(pnormalset(tpointerord(value)));
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


    procedure tconstsym.write;
      begin
         inherited writesym;
         writebyte(byte(consttyp));
         case consttyp of
           constnil : ;
           constint:
             if sizeof(TConstExprInt)=8 then
               begin
                  writelong(longint(lo(value)));
                  writelong(longint(hi(value)));
               end
             else
               writelong(value);

           constbool,
           constchar :
             writelong(value);
           constpointer,
           constord :
             begin
               consttype.write;
               if sizeof(TConstExprInt)=8 then
                 begin
                    writelong(longint(lo(value)));
                    writelong(longint(hi(value)));
                 end
               else
                 writelong(value);
             end;
           conststring,constresourcestring :
             begin
               writelong(len);
               current_ppu^.putdata(pchar(TPointerOrd(value))^,len);
               if consttyp=constresourcestring then
                 writelong(ResStrIndex);
             end;
           constreal :
             writereal(pbestreal(TPointerOrd(value))^);
           constset :
             begin
               consttype.write;
               writenormalset(pointer(TPointerOrd(value))^);
             end;
         else
           internalerror(13);
         end;
        current_ppu^.writeentry(ibconstsym);
      end;

{$ifdef GDB}
    function tconstsym.stabstring : pchar;
    var st : string;
    begin
         {even GDB v4.16 only now 'i' 'r' and 'e' !!!}
         case consttyp of
            conststring : begin
                          { I had to remove ibm2ascii !! }
                          st := pstring(TPointerOrd(value))^;
                          {st := ibm2ascii(pstring(value)^);}
                          st := 's'''+st+'''';
                          end;
            constbool,
            constint,
            constpointer,
            constord,
            constchar : st := 'i'+int64tostr(value);
            constreal : begin
                        system.str(pbestreal(TPointerOrd(value))^,st);
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
         if def.min>v then
           def.setmin(v);
         if def.max<v then
           def.setmax(v);
         order;
      end;


    constructor tenumsym.load;
      begin
         inherited loadsym;
         typ:=enumsym;
         definition:=tenumdef(readderef);
         value:=readlong;
         nextenum := Nil;
      end;


    procedure tenumsym.deref;
      begin
         resolvedef(tdef(definition));
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


    procedure tenumsym.write;
      begin
         inherited writesym;
         writederef(definition);
         writelong(value);
         current_ppu^.writeentry(ibenumsym);
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
           not(assigned(restype.def.typesym)) then
         restype.def.typesym:=self;
      end;


    constructor ttypesym.load;
      begin
         inherited loadsym;
         typ:=typesym;
{$ifdef GDB}
         isusedinstab := false;
{$endif GDB}
         restype.load;
      end;


    function  ttypesym.gettypedef:tdef;
      begin
        gettypedef:=restype.def;
      end;


    procedure ttypesym.prederef;
      begin
         restype.resolve;
      end;


    procedure ttypesym.write;
      begin
         inherited writesym;
         restype.write;
         current_ppu^.writeentry(ibtypesym);
      end;


    procedure ttypesym.load_references;
      begin
         inherited load_references;
         if (restype.def.deftype=recorddef) then
           tstoredsymtable(trecorddef(restype.def).symtable).load_browser;
         if (restype.def.deftype=objectdef) then
           tstoredsymtable(tobjectdef(restype.def).symtable).load_browser;
      end;


    function ttypesym.write_references : boolean;
      begin
        if not inherited write_references then
         { write address of this symbol if record or object
           even if no real refs are there
           because we need it for the symtable }
         if (restype.def.deftype=recorddef) or
            (restype.def.deftype=objectdef) then
          begin
            writederef(self);
            current_ppu^.writeentry(ibsymref);
          end;
         write_references:=true;
         if (restype.def.deftype=recorddef) then
           tstoredsymtable(trecorddef(restype.def).symtable).write_browser;
         if (restype.def.deftype=objectdef) then
           tstoredsymtable(tobjectdef(restype.def).symtable).write_browser;
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

    constructor tsyssym.load;
      begin
         inherited loadsym;
         typ:=syssym;
         number:=readlong;
      end;

    destructor tsyssym.destroy;
      begin
        inherited destroy;
      end;

    procedure tsyssym.write;
      begin
         inherited writesym;
         writelong(number);
         current_ppu^.writeentry(ibsyssym);
      end;

{$ifdef GDB}
    procedure tsyssym.concatstabto(asmlist : taasmoutput);
      begin
      end;
{$endif GDB}


end.
{
  $Log$
  Revision 1.11  2001-04-18 22:01:59  peter
    * registration of targets and assemblers

  Revision 1.10  2001/04/13 01:22:16  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.9  2001/04/02 21:20:35  peter
    * resulttype rewrite

  Revision 1.8  2001/03/11 22:58:51  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.7  2000/12/25 00:07:30  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.6  2000/11/28 00:25:17  pierre
   + use int64tostr function for integer consts

  Revision 1.5  2000/11/13 14:44:35  jonas
    * fixes so no more range errors with improved range checking code

  Revision 1.4  2000/11/08 23:15:17  florian
    * tprocdef.procsym must be set also when a tprocdef is loaded from a PPU

  Revision 1.3  2000/11/06 23:13:53  peter
    * uppercase manglednames

  Revision 1.2  2000/11/01 23:04:38  peter
    * tprocdef.fullprocname added for better casesensitve writing of
      procedures

  Revision 1.1  2000/10/31 22:02:52  peter
    * symtable splitted, no real code changes

}
