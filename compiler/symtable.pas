{
    $Id$
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
unit symtable;

{$i fpcdefs.inc}

interface

    uses
       { common }
       cutils,cclasses,
       { global }
       cpuinfo,globtype,tokens,
       { symtable }
       symconst,symbase,symtype,symdef,symsym,
       { ppu }
       ppu,symppu,
       { assembler }
       aasmbase,aasmtai,aasmcpu,
       { cg }
       paramgr
       ;


{****************************************************************************
                             Symtable types
****************************************************************************}

    type
       tstoredsymtable = class(tsymtable)
       private
          b_needs_init_final : boolean;
          procedure _needs_init_final(p : tnamedindexitem;arg:pointer);
          procedure check_forward(sym : TNamedIndexItem;arg:pointer);
          procedure labeldefined(p : TNamedIndexItem;arg:pointer);
          procedure unitsymbolused(p : TNamedIndexItem;arg:pointer);
          procedure varsymbolused(p : TNamedIndexItem;arg:pointer);
          procedure TestPrivate(p : TNamedIndexItem;arg:pointer);
          procedure objectprivatesymbolused(p : TNamedIndexItem;arg:pointer);
{$ifdef GDB}
       private
          procedure concatstab(p : TNamedIndexItem;arg:pointer);
          procedure resetstab(p : TNamedIndexItem;arg:pointer);
          procedure concattypestab(p : TNamedIndexItem;arg:pointer);
{$endif}
          procedure unchain_overloads(p : TNamedIndexItem;arg:pointer);
          procedure loaddefs(ppufile:tcompilerppufile);
          procedure loadsyms(ppufile:tcompilerppufile);
          procedure writedefs(ppufile:tcompilerppufile);
          procedure writesyms(ppufile:tcompilerppufile);
       public
          { load/write }
          procedure ppuload(ppufile:tcompilerppufile);virtual;
          procedure ppuwrite(ppufile:tcompilerppufile);virtual;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);virtual;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);virtual;
          procedure deref;virtual;
          procedure derefimpl;virtual;
          procedure insert(sym : tsymentry);override;
          function  speedsearch(const s : stringid;speedvalue : cardinal) : tsymentry;override;
          procedure allsymbolsused;
          procedure allprivatesused;
          procedure allunitsused;
          procedure check_forwards;
          procedure checklabels;
          function  needs_init_final : boolean;
          procedure unchain_overloaded;
          procedure chainoperators;
{$ifdef GDB}
          procedure concatstabto(asmlist : taasmoutput);virtual;
          function  getnewtypecount : word; override;
{$endif GDB}
          procedure testfordefaultproperty(p : TNamedIndexItem;arg:pointer);
       end;

       tabstractrecordsymtable = class(tstoredsymtable)
       public
          procedure ppuload(ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure insertvardata(sym : tsymentry);override;
       end;

       trecordsymtable = class(tabstractrecordsymtable)
       public
          constructor create;
          procedure insert_in(tsymt : tsymtable;offset : longint);
       end;

       tobjectsymtable = class(tabstractrecordsymtable)
       public
          constructor create(const n:string);
          procedure insert(sym : tsymentry);override;
       end;

       tabstractlocalsymtable = class(tstoredsymtable)
       public
          procedure ppuload(ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);override;
       end;

       tlocalsymtable = class(tabstractlocalsymtable)
       public
          constructor create;
          procedure insert(sym : tsymentry);override;
          procedure insertvardata(sym : tsymentry);override;
          procedure insertconstdata(sym : tsymentry);override;
       end;

       tparasymtable = class(tabstractlocalsymtable)
       public
          constructor create;
          procedure insert(sym : tsymentry);override;
          procedure insertvardata(sym : tsymentry);override;
       end;

       tabstractunitsymtable = class(tstoredsymtable)
       public
{$ifdef GDB}
          dbx_count : longint;
          prev_dbx_counter : plongint;
          dbx_count_ok : boolean;
          is_stab_written : boolean;
{$endif GDB}
          constructor create(const n : string);
{$ifdef GDB}
          procedure concattypestabto(asmlist : taasmoutput);
{$endif GDB}
          procedure insertvardata(sym : tsymentry);override;
          procedure insertconstdata(sym : tsymentry);override;
       end;

       tglobalsymtable = class(tabstractunitsymtable)
       public
          unittypecount : word;
          unitsym       : tunitsym;
          constructor create(const n : string);
          destructor  destroy;override;
          procedure ppuload(ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure insert(sym : tsymentry);override;
          procedure insertvardata(sym : tsymentry);override;
{$ifdef GDB}
          function getnewtypecount : word; override;
{$endif}
       end;

       tstaticsymtable = class(tabstractunitsymtable)
       public
          constructor create(const n : string);
          procedure ppuload(ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure insert(sym : tsymentry);override;
          procedure insertvardata(sym : tsymentry);override;
       end;

       twithsymtable = class(tsymtable)
          direct_with : boolean;
          { in fact it is a tnode }
          withnode : pointer;
          { tnode to load of direct with var }
          { already usable before firstwith
            needed for firstpass of function parameters PM }
          withrefnode : pointer;
          constructor create(aowner:tdef;asymsearch:TDictionary);
          destructor  destroy;override;
          procedure clear;override;
        end;

       tstt_exceptsymtable = class(tsymtable)
       public
          constructor create;
       end;


    var
       constsymtable  : tsymtable;      { symtable were the constants can be inserted }
       systemunit     : tglobalsymtable; { pointer to the system unit }
       read_member    : boolean;        { reading members of an symtable }

       lexlevel       : longint;       { level of code }
                                       { 1 for main procedure }
                                       { 2 for normal function or proc }
                                       { higher for locals }

{****************************************************************************
                             Functions
****************************************************************************}

{*** Misc ***}
    procedure globaldef(const s : string;var t:ttype);
    function  findunitsymtable(st:tsymtable):tsymtable;
    procedure duplicatesym(sym:tsym);

{*** Search ***}
    function  searchsym(const s : stringid;var srsym:tsym;var srsymtable:tsymtable):boolean;
    function  searchsymonlyin(p : tsymtable;const s : stringid):tsym;
    function  searchsym_in_class(classh:tobjectdef;const s : stringid):tsym;
    function  searchsystype(const s: stringid; var srsym: ttypesym): boolean;
    function  searchsysvar(const s: stringid; var srsym: tvarsym; var symowner: tsymtable): boolean;
    function  search_class_member(pd : tobjectdef;const s : string):tsym;

{*** Object Helpers ***}
    function search_default_property(pd : tobjectdef) : tpropertysym;

{*** symtable stack ***}
    procedure dellexlevel;
    procedure RestoreUnitSyms;
{$ifdef DEBUG}
    procedure test_symtablestack;
    procedure list_symtablestack;
{$endif DEBUG}

{$ifdef UNITALIASES}
    type
       punit_alias = ^tunit_alias;
       tunit_alias = object(TNamedIndexItem)
          newname : pstring;
          constructor init(const n:string);
          destructor  done;virtual;
       end;
    var
       unitaliases : pdictionary;

    procedure addunitalias(const n:string);
    function getunitalias(const n:string):string;
{$endif UNITALIASES}

{*** Init / Done ***}
    procedure InitSymtable;
    procedure DoneSymtable;

    type
       toverloaded_operators = array[NOTOKEN..last_overloaded] of tprocsym;
    var
       overloaded_operators : toverloaded_operators;
       { unequal is not equal}
    const
       overloaded_names : array [NOTOKEN..last_overloaded] of string[16] =
         ('error',
          'plus','minus','star','slash','equal',
          'greater','lower','greater_or_equal',
          'lower_or_equal',
          'sym_diff','starstar',
          'as','is','in','or',
          'and','div','mod','not','shl','shr','xor',
          'assign');



implementation

    uses
      { global }
      verbose,globals,
      { target }
      systems,
      { module }
      fmodule,
{$ifdef GDB}
      gdb,
{$endif GDB}
      { codegen }
      cgbase
      ;


{*****************************************************************************
                             TStoredSymtable
*****************************************************************************}

    procedure tstoredsymtable.ppuload(ppufile:tcompilerppufile);
      begin
        { load definitions }
        loaddefs(ppufile);

        { load symbols }
        loadsyms(ppufile);
      end;


    procedure tstoredsymtable.ppuwrite(ppufile:tcompilerppufile);
      begin
         { write definitions }
         writedefs(ppufile);

         { write symbols }
         writesyms(ppufile);
      end;


    procedure tstoredsymtable.loaddefs(ppufile:tcompilerppufile);
      var
        hp : tdef;
        b  : byte;
      begin
      { load start of definition section, which holds the amount of defs }
         if ppufile.readentry<>ibstartdefs then
          Message(unit_f_ppu_read_error);
         ppufile.getlongint;
      { read definitions }
         repeat
           b:=ppufile.readentry;
           case b of
              ibpointerdef : hp:=tpointerdef.ppuload(ppufile);
                ibarraydef : hp:=tarraydef.ppuload(ppufile);
                  iborddef : hp:=torddef.ppuload(ppufile);
                ibfloatdef : hp:=tfloatdef.ppuload(ppufile);
                 ibprocdef : hp:=tprocdef.ppuload(ppufile);
          ibshortstringdef : hp:=tstringdef.loadshort(ppufile);
           iblongstringdef : hp:=tstringdef.loadlong(ppufile);
           ibansistringdef : hp:=tstringdef.loadansi(ppufile);
           ibwidestringdef : hp:=tstringdef.loadwide(ppufile);
               ibrecorddef : hp:=trecorddef.ppuload(ppufile);
               ibobjectdef : hp:=tobjectdef.ppuload(ppufile);
                 ibenumdef : hp:=tenumdef.ppuload(ppufile);
                  ibsetdef : hp:=tsetdef.ppuload(ppufile);
              ibprocvardef : hp:=tprocvardef.ppuload(ppufile);
                 ibfiledef : hp:=tfiledef.ppuload(ppufile);
             ibclassrefdef : hp:=tclassrefdef.ppuload(ppufile);
               ibformaldef : hp:=tformaldef.ppuload(ppufile);
              ibvariantdef : hp:=tvariantdef.ppuload(ppufile);
                 ibenddefs : break;
                     ibend : Message(unit_f_ppu_read_error);
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
           hp.owner:=self;
           defindex.insert(hp);
         until false;
      end;


    procedure tstoredsymtable.loadsyms(ppufile:tcompilerppufile);
      var
        b   : byte;
        sym : tsym;
      begin
      { load start of definition section, which holds the amount of defs }
         if ppufile.readentry<>ibstartsyms then
          Message(unit_f_ppu_read_error);
         { skip amount of symbols, not used currently }
         ppufile.getlongint;
         { load datasize,dataalignment of this symboltable }
         datasize:=ppufile.getlongint;
         dataalignment:=ppufile.getlongint;
      { now read the symbols }
         repeat
           b:=ppufile.readentry;
           case b of
                ibtypesym : sym:=ttypesym.ppuload(ppufile);
                ibprocsym : sym:=tprocsym.ppuload(ppufile);
               ibconstsym : sym:=tconstsym.ppuload(ppufile);
                 ibvarsym : sym:=tvarsym.ppuload(ppufile);
             ibfuncretsym : sym:=tfuncretsym.ppuload(ppufile);
            ibabsolutesym : sym:=tabsolutesym.ppuload(ppufile);
                ibenumsym : sym:=tenumsym.ppuload(ppufile);
          ibtypedconstsym : sym:=ttypedconstsym.ppuload(ppufile);
            ibpropertysym : sym:=tpropertysym.ppuload(ppufile);
                ibunitsym : sym:=tunitsym.ppuload(ppufile);
               iblabelsym : sym:=tlabelsym.ppuload(ppufile);
                 ibsyssym : sym:=tsyssym.ppuload(ppufile);
                ibrttisym : sym:=trttisym.ppuload(ppufile);
                ibendsyms : break;
                    ibend : Message(unit_f_ppu_read_error);
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
           sym.owner:=self;
           symindex.insert(sym);
           symsearch.insert(sym);
         until false;
      end;


    procedure tstoredsymtable.writedefs(ppufile:tcompilerppufile);
      var
         pd : tstoreddef;
      begin
      { each definition get a number, write then the amount of defs to the
         ibstartdef entry }
         ppufile.putlongint(defindex.count);
         ppufile.writeentry(ibstartdefs);
      { now write the definition }
         pd:=tstoreddef(defindex.first);
         while assigned(pd) do
           begin
              pd.ppuwrite(ppufile);
              pd:=tstoreddef(pd.indexnext);
           end;
      { write end of definitions }
         ppufile.writeentry(ibenddefs);
      end;


    procedure tstoredsymtable.writesyms(ppufile:tcompilerppufile);
      var
        pd : tstoredsym;
      begin
       { each definition get a number, write then the amount of syms and the
         datasize to the ibsymdef entry }
         ppufile.putlongint(symindex.count);
         ppufile.putlongint(datasize);
         ppufile.putlongint(dataalignment);
         ppufile.writeentry(ibstartsyms);
       { foreach is used to write all symbols }
         pd:=tstoredsym(symindex.first);
         while assigned(pd) do
           begin
              pd.ppuwrite(ppufile);
              pd:=tstoredsym(pd.indexnext);
           end;
       { end of symbols }
         ppufile.writeentry(ibendsyms);
      end;


    procedure tstoredsymtable.load_references(ppufile:tcompilerppufile;locals:boolean);
      var
        b     : byte;
        sym   : tstoredsym;
        prdef : tstoreddef;
      begin
         b:=ppufile.readentry;
         if b <> ibbeginsymtablebrowser then
           Message1(unit_f_ppu_invalid_entry,tostr(b));
         repeat
           b:=ppufile.readentry;
           case b of
             ibsymref :
               begin
                 sym:=tstoredsym(ppufile.getderef);
                 resolvesym(pointer(sym));
                 if assigned(sym) then
                   sym.load_references(ppufile,locals);
               end;
             ibdefref :
               begin
                 prdef:=tstoreddef(ppufile.getderef);
                 resolvedef(pointer(prdef));
                 if assigned(prdef) then
                   begin
                     if prdef.deftype<>procdef then
                       Message(unit_f_ppu_read_error);
                     tprocdef(prdef).load_references(ppufile,locals);
                   end;
               end;
             ibendsymtablebrowser :
               break;
             else
               Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
         until false;
      end;


    procedure tstoredsymtable.write_references(ppufile:tcompilerppufile;locals:boolean);
      var
        pd : tstoredsym;
      begin
         ppufile.writeentry(ibbeginsymtablebrowser);
         { write all symbols }
         pd:=tstoredsym(symindex.first);
         while assigned(pd) do
           begin
              pd.write_references(ppufile,locals);
              pd:=tstoredsym(pd.indexnext);
           end;
         ppufile.writeentry(ibendsymtablebrowser);
      end;


    procedure tstoredsymtable.deref;
      var
        hp : tdef;
        hs : tsym;
      begin
        { deref the interface definitions }
        hp:=tdef(defindex.first);
        while assigned(hp) do
         begin
           hp.deref;
           hp:=tdef(hp.indexnext);
         end;
        { first deref the interface ttype symbols }
        hs:=tsym(symindex.first);
        while assigned(hs) do
         begin
           if hs.typ=typesym then
             hs.deref;
           hs:=tsym(hs.indexnext);
         end;
        { deref the interface symbols }
        hs:=tsym(symindex.first);
        while assigned(hs) do
         begin
           if hs.typ<>typesym then
             hs.deref;
           hs:=tsym(hs.indexnext);
         end;
      end;


    procedure tstoredsymtable.derefimpl;
      var
        hp : tdef;
      begin
        { deref the implementation part of definitions }
        hp:=tdef(defindex.first);
        while assigned(hp) do
         begin
           hp.derefimpl;
           hp:=tdef(hp.indexnext);
         end;
      end;


    procedure tstoredsymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
         { set owner and sym indexnb }
         sym.owner:=self;

         { check the current symtable }
         hsym:=tsym(search(sym.name));
         if assigned(hsym) then
          begin
            { in TP and Delphi you can have a local with the
              same name as the function, the function is then hidden for
              the user. (Under delphi it can still be accessed using result),
              but don't allow hiding of RESULT }
            if (m_duplicate_names in aktmodeswitches) and
               (hsym.typ=funcretsym) and
               not((m_result in aktmodeswitches) and
                   (hsym.name='RESULT')) then
             hsym.owner.rename(hsym.name,'hidden'+hsym.name)
            else
             begin
               DuplicateSym(hsym);
               exit;
             end;
          end;

         { register definition of typesym }
         if (sym.typ = typesym) and
            assigned(ttypesym(sym).restype.def) then
          begin
            if not(assigned(ttypesym(sym).restype.def.owner)) and
               (ttypesym(sym).restype.def.deftype<>errordef) then
              registerdef(ttypesym(sym).restype.def);
{$ifdef GDB}
            if (cs_debuginfo in aktmoduleswitches) and assigned(debuglist) and
               (symtabletype in [globalsymtable,staticsymtable]) then
              begin
                ttypesym(sym).isusedinstab := true;
                {sym.concatstabto(debuglist);}
              end;
{$endif GDB}
          end;

         { insert in index and search hash }
         symindex.insert(sym);
         symsearch.insert(sym);
      end;


    function tstoredsymtable.speedsearch(const s : stringid;speedvalue : cardinal) : tsymentry;
      var
        hp : tstoredsym;
        newref : tref;
      begin
        hp:=tstoredsym(inherited speedsearch(s,speedvalue));
        if assigned(hp) then
         begin
           { reject non static members in static procedures }
           if (symtabletype=objectsymtable) and
              not(sp_static in hp.symoptions) and
              allow_only_static then
             Message(sym_e_only_static_in_static);

           { unit uses count }
           if (unitid<>0) and
              (symtabletype = globalsymtable) and
              assigned(tglobalsymtable(self).unitsym) then
             inc(tglobalsymtable(self).unitsym.refs);

{$ifdef GDB}
           { if it is a type, we need the stabs of this type
             this might be the cause of the class debug problems
             as TCHILDCLASS.Create did not generate appropriate
             stabs debug info if TCHILDCLASS wasn't used anywhere else PM }
           if (cs_debuginfo in aktmoduleswitches) and
              (hp.typ=typesym) and
              make_ref then
             begin
               if assigned(ttypesym(hp).restype.def) then
                 tstoreddef(ttypesym(hp).restype.def).numberstring
               else
                 ttypesym(hp).isusedinstab:=true;
             end;
{$endif GDB}

           { unitsym are only loaded for browsing PM    }
           { this was buggy anyway because we could use }
           { unitsyms from other units in _USES !!      }
           {if (symtabletype=unitsymtable) and (hp.typ=unitsym) and
              assigned(current_module) and (current_module.globalsymtable<>.load) then
             hp:=nil;}
           if assigned(hp) and
              make_ref and
              (cs_browser in aktmoduleswitches) then
             begin
                newref:=tref.create(hp.lastref,@akttokenpos);
                { for symbols that are in tables without
                browser info or syssyms (PM) }
                if hp.refcount=0 then
                  begin
                    hp.defref:=newref;
                    hp.lastref:=newref;
                  end
                else
                if resolving_forward and assigned(hp.defref) then
                { put it as second reference }
                  begin
                   newref.nextref:=hp.defref.nextref;
                   hp.defref.nextref:=newref;
                   hp.lastref.nextref:=nil;
                  end
                else
                  hp.lastref:=newref;
                inc(hp.refcount);
             end;
           if assigned(hp) and make_ref then
             begin
               inc(hp.refs);
             end;
         end;
        speedsearch:=hp;
      end;


{**************************************
             Callbacks
**************************************}

    procedure TStoredSymtable.check_forward(sym : TNamedIndexItem;arg:pointer);
      begin
         if tsym(sym).typ=procsym then
           tprocsym(sym).check_forward
         { check also object method table            }
         { we needn't to test the def list          }
         { because each object has to have a type sym }
         else
          if (tsym(sym).typ=typesym) and
             assigned(ttypesym(sym).restype.def) and
             (ttypesym(sym).restype.def.deftype=objectdef) then
           tobjectdef(ttypesym(sym).restype.def).check_forwards;
      end;


    procedure TStoredSymtable.labeldefined(p : TNamedIndexItem;arg:pointer);
      begin
        if (tsym(p).typ=labelsym) and
           not(tlabelsym(p).defined) then
         begin
           if tlabelsym(p).used then
            Message1(sym_e_label_used_and_not_defined,tlabelsym(p).realname)
           else
            Message1(sym_w_label_not_defined,tlabelsym(p).realname);
         end;
      end;


    procedure TStoredSymtable.unitsymbolused(p : TNamedIndexItem;arg:pointer);
      begin
         if (tsym(p).typ=unitsym) and
            (tunitsym(p).refs=0) and
            { do not claim for unit name itself !! }
            assigned(tunitsym(p).unitsymtable) and
            (tunitsym(p).unitsymtable.symtabletype=globalsymtable) then
           MessagePos2(tsym(p).fileinfo,sym_n_unit_not_used,p.name,current_module.modulename^);
      end;


    procedure TStoredSymtable.varsymbolused(p : TNamedIndexItem;arg:pointer);
      begin
         if (tsym(p).typ=varsym) and
            ((tsym(p).owner.symtabletype in
             [parasymtable,localsymtable,objectsymtable,staticsymtable])) then
          begin
           { unused symbol should be reported only if no }
           { error is reported                     }
           { if the symbol is in a register it is used   }
           { also don't count the value parameters which have local copies }
           { also don't claim for high param of open parameters (PM) }
           if (Errorcount<>0) or
              (copy(p.name,1,3)='val') or
              (copy(p.name,1,4)='high') then
             exit;
           if (tvarsym(p).refs=0) then
             begin
                if (tsym(p).owner.symtabletype=parasymtable) or (vo_is_local_copy in tvarsym(p).varoptions) then
                  begin
                    MessagePos1(tsym(p).fileinfo,sym_h_para_identifier_not_used,tsym(p).realname);
                  end
                else if (tsym(p).owner.symtabletype=objectsymtable) then
                  MessagePos2(tsym(p).fileinfo,sym_n_private_identifier_not_used,tsym(p).owner.realname^,tsym(p).realname)
                else
                  MessagePos1(tsym(p).fileinfo,sym_n_local_identifier_not_used,tsym(p).realname);
             end
           else if tvarsym(p).varstate=vs_assigned then
             begin
                if (tsym(p).owner.symtabletype=parasymtable) then
                  begin
                    if not(tvarsym(p).varspez in [vs_var,vs_out])  then
                      MessagePos1(tsym(p).fileinfo,sym_h_para_identifier_only_set,tsym(p).realname)
                  end
                else if (vo_is_local_copy in tvarsym(p).varoptions) then
                  begin
                    if not(tvarsym(p).varspez in [vs_var,vs_out]) then
                      MessagePos1(tsym(p).fileinfo,sym_h_para_identifier_only_set,tsym(p).realname);
                  end
                else if (tsym(p).owner.symtabletype=objectsymtable) then
                  MessagePos2(tsym(p).fileinfo,sym_n_private_identifier_only_set,tsym(p).owner.realname^,tsym(p).realname)
                else if (tsym(p).owner.symtabletype<>parasymtable) then
                  if not (vo_is_exported in tvarsym(p).varoptions) then
                    MessagePos1(tsym(p).fileinfo,sym_n_local_identifier_only_set,tsym(p).realname);
             end;
         end
      else if ((tsym(p).owner.symtabletype in
              [objectsymtable,parasymtable,localsymtable,staticsymtable])) then
          begin
           if (Errorcount<>0) then
             exit;
           { do not claim for inherited private fields !! }
           if (tstoredsym(p).refs=0) and (tsym(p).owner.symtabletype=objectsymtable) then
             MessagePos2(tsym(p).fileinfo,sym_n_private_method_not_used,tsym(p).owner.realname^,tsym(p).realname)
           { units references are problematic }
           else if (tstoredsym(p).refs=0) and not(tsym(p).typ in [funcretsym,enumsym,unitsym]) then
             if (tsym(p).typ<>procsym) or not (tprocsym(p).is_global) or
             { all program functions are declared global
               but unused should still be signaled PM }
                ((tsym(p).owner.symtabletype=staticsymtable) and
                not current_module.is_unit) then
             MessagePos2(tsym(p).fileinfo,sym_h_local_symbol_not_used,SymTypeName[tsym(p).typ],tsym(p).realname);
          end;
      end;


    procedure TStoredSymtable.TestPrivate(p : TNamedIndexItem;arg:pointer);
      begin
        if sp_private in tsym(p).symoptions then
          varsymbolused(p,arg);
      end;


    procedure TStoredSymtable.objectprivatesymbolused(p : TNamedIndexItem;arg:pointer);
      begin
         {
           Don't test simple object aliases PM
         }
         if (tsym(p).typ=typesym) and
            (ttypesym(p).restype.def.deftype=objectdef) and
            (ttypesym(p).restype.def.typesym=tsym(p)) then
           tobjectdef(ttypesym(p).restype.def).symtable.foreach({$ifdef FPCPROCVAR}@{$endif}TestPrivate,nil);
      end;


    procedure tstoredsymtable.unchain_overloads(p : TNamedIndexItem;arg:pointer);
      begin
         if tsym(p).typ=procsym then
           tprocsym(p).unchain_overload;
      end;

{$ifdef GDB}

    procedure TStoredSymtable.concatstab(p : TNamedIndexItem;arg:pointer);
      begin
        if tsym(p).typ <> procsym then
          tstoredsym(p).concatstabto(TAAsmOutput(arg));
      end;

    procedure TStoredSymtable.resetstab(p : TNamedIndexItem;arg:pointer);
      begin
        if tsym(p).typ <> procsym then
          tstoredsym(p).isstabwritten:=false;
      end;

    procedure TStoredSymtable.concattypestab(p : TNamedIndexItem;arg:pointer);
      begin
        if tsym(p).typ = typesym then
         begin
           tstoredsym(p).isstabwritten:=false;
           tstoredsym(p).concatstabto(TAAsmOutput(arg));
         end;
      end;

   function tstoredsymtable.getnewtypecount : word;
      begin
         getnewtypecount:=pglobaltypecount^;
         inc(pglobaltypecount^);
      end;
{$endif GDB}


    procedure tstoredsymtable.chainoperators;
      var
        t : ttoken;
        srsym : tsym;
        srsymtable,
        storesymtablestack : tsymtable;
      begin
         storesymtablestack:=symtablestack;
         symtablestack:=self;
         make_ref:=false;
         for t:=first_overloaded to last_overloaded do
           begin
              overloaded_operators[t]:=nil;
              { each operator has a unique lowercased internal name PM }
              while assigned(symtablestack) do
                begin
                  searchsym(overloaded_names[t],srsym,srsymtable);
                  if not assigned(srsym) then
                   begin
                     if (t=_STARSTAR) then
                      begin
                        symtablestack:=systemunit;
                        searchsym('POWER',srsym,srsymtable);
                      end;
                   end;
                  if assigned(srsym) then
                    begin
                       if (srsym.typ<>procsym) then
                         internalerror(12344321);
                       { use this procsym as start ? }
                       if not assigned(overloaded_operators[t]) then
                          overloaded_operators[t]:=tprocsym(srsym)
                       else
                          { already got a procsym, only add defs of the current procsym }
                          Tprocsym(srsym).concat_procdefs_to(overloaded_operators[t]);
                       symtablestack:=srsym.owner.next;
                    end
                  else
                    begin
                      symtablestack:=nil;
                    end;
                  { search for same procsym in other units }
                end;
              symtablestack:=self;
           end;
         make_ref:=true;
         symtablestack:=storesymtablestack;
      end;


{***********************************************
           Process all entries
***********************************************}

    { checks, if all procsyms and methods are defined }
    procedure tstoredsymtable.check_forwards;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}check_forward,nil);
      end;


    procedure tstoredsymtable.checklabels;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}labeldefined,nil);
      end;


    procedure tstoredsymtable.allunitsused;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}unitsymbolused,nil);
      end;


    procedure tstoredsymtable.allsymbolsused;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}varsymbolused,nil);
      end;


    procedure tstoredsymtable.allprivatesused;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}objectprivatesymbolused,nil);
      end;


    procedure tstoredsymtable.unchain_overloaded;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}unchain_overloads,nil);
      end;


{$ifdef GDB}
    procedure tstoredsymtable.concatstabto(asmlist : taasmoutput);
      begin
        if symtabletype in [inlineparasymtable,inlinelocalsymtable] then
          foreach({$ifdef FPCPROCVAR}@{$endif}resetstab,nil);

        foreach({$ifdef FPCPROCVAR}@{$endif}concatstab,asmlist);
      end;
{$endif}


    procedure TStoredSymtable._needs_init_final(p : tnamedindexitem;arg:pointer);
      begin
         if b_needs_init_final then
          exit;
         case tsym(p).typ of
           varsym :
             begin
               if not(is_class(tvarsym(p).vartype.def)) and
                  tstoreddef(tvarsym(p).vartype.def).needs_inittable then
                 b_needs_init_final:=true;
             end;
           typedconstsym :
             begin
               if ttypedconstsym(p).is_writable and
                  tstoreddef(ttypedconstsym(p).typedconsttype.def).needs_inittable then
                 b_needs_init_final:=true;
             end;
         end;
      end;


    { returns true, if p contains data which needs init/final code }
    function tstoredsymtable.needs_init_final : boolean;
      begin
         b_needs_init_final:=false;
         foreach({$ifdef FPCPROCVAR}@{$endif}_needs_init_final,nil);
         needs_init_final:=b_needs_init_final;
      end;


{****************************************************************************
                          TAbstractRecordSymtable
****************************************************************************}

    procedure tabstractrecordsymtable.ppuload(ppufile:tcompilerppufile);
      var
        storesymtable : tsymtable;
      begin
        storesymtable:=aktrecordsymtable;
        aktrecordsymtable:=self;

        inherited ppuload(ppufile);

        aktrecordsymtable:=storesymtable;
      end;


    procedure tabstractrecordsymtable.ppuwrite(ppufile:tcompilerppufile);
      var
        oldtyp : byte;
        storesymtable : tsymtable;
      begin
         storesymtable:=aktrecordsymtable;
         aktrecordsymtable:=self;
         oldtyp:=ppufile.entrytyp;
         ppufile.entrytyp:=subentryid;

         inherited ppuwrite(ppufile);

         ppufile.entrytyp:=oldtyp;
         aktrecordsymtable:=storesymtable;
      end;


    procedure tabstractrecordsymtable.load_references(ppufile:tcompilerppufile;locals:boolean);
      var
        storesymtable : tsymtable;
      begin
        storesymtable:=aktrecordsymtable;
        aktrecordsymtable:=self;

        inherited load_references(ppufile,locals);

        aktrecordsymtable:=storesymtable;
      end;


    procedure tabstractrecordsymtable.write_references(ppufile:tcompilerppufile;locals:boolean);
      var
        storesymtable : tsymtable;
      begin
        storesymtable:=aktrecordsymtable;
        aktrecordsymtable:=self;

        inherited write_references(ppufile,locals);

        aktrecordsymtable:=storesymtable;
      end;


    procedure tabstractrecordsymtable.insertvardata(sym : tsymentry);
      var
        l,varalign : longint;
        vardef : tdef;
      begin
        if sym.typ<>varsym then
         internalerror(200208251);
        l:=tvarsym(sym).getvaluesize;
        vardef:=tvarsym(sym).vartype.def;
        { this symbol can't be loaded to a register }
        exclude(tvarsym(sym).varoptions,vo_regable);
        exclude(tvarsym(sym).varoptions,vo_fpuregable);
        { get the alignment size }
        if (aktalignment.recordalignmax=-1) then
         begin
           varalign:=vardef.alignment;
           if (varalign>4) and
              ((varalign mod 4)<>0) and
              (vardef.deftype=arraydef) then
             Message1(sym_w_wrong_C_pack,vardef.typename);
           if varalign=0 then
             varalign:=l;
           if (dataalignment<aktalignment.maxCrecordalign) then
            begin
              if (varalign>16) and (dataalignment<32) then
               dataalignment:=32
              else if (varalign>12) and (dataalignment<16) then
               dataalignment:=16
              { 12 is needed for long double }
              else if (varalign>8) and (dataalignment<12) then
               dataalignment:=12
              else if (varalign>4) and (dataalignment<8) then
               dataalignment:=8
              else if (varalign>2) and (dataalignment<4) then
               dataalignment:=4
              else if (varalign>1) and (dataalignment<2) then
               dataalignment:=2;
            end;
           dataalignment:=min(dataalignment,aktalignment.maxCrecordalign);
         end
        else
         varalign:=vardef.alignment;
        if varalign=0 then
          varalign:=size_2_align(l);
        varalign:=used_align(varalign,aktalignment.recordalignmin,dataalignment);
        tvarsym(sym).address:=align(datasize,varalign);
        datasize:=tvarsym(sym).address+l;
      end;


{****************************************************************************
                              TRecordSymtable
****************************************************************************}

    constructor trecordsymtable.create;
      begin
        inherited create('');
        symtabletype:=recordsymtable;
      end;


   { this procedure is reserved for inserting case variant into
      a record symtable }
    { the offset is the location of the start of the variant
      and datasize and dataalignment corresponds to
      the complete size (see code in pdecl unit) PM }
    procedure trecordsymtable.insert_in(tsymt : tsymtable;offset : longint);
      var
        ps,nps : tvarsym;
        pd,npd : tdef;
        storesize,storealign : longint;
      begin
        storesize:=tsymt.datasize;
        storealign:=tsymt.dataalignment;
        tsymt.datasize:=offset;
        ps:=tvarsym(symindex.first);
        while assigned(ps) do
          begin
            nps:=tvarsym(ps.indexnext);
            { remove from current symtable }
            symindex.deleteindex(ps);
            ps.left:=nil;
            ps.right:=nil;
            { add to symt }
            ps.owner:=tsymt;
            tsymt.datasize:=ps.address+offset;
            tsymt.symindex.insert(ps);
            tsymt.symsearch.insert(ps);
            { update address }
            ps.address:=tsymt.datasize;
            { next }
            ps:=nps;
          end;
        pd:=tdef(defindex.first);
        while assigned(pd) do
          begin
            npd:=tdef(pd.indexnext);
            defindex.deleteindex(pd);
            pd.left:=nil;
            pd.right:=nil;
            tsymt.registerdef(pd);
            pd:=npd;
          end;
        tsymt.datasize:=storesize;
        tsymt.dataalignment:=storealign;
      end;


{****************************************************************************
                              TObjectSymtable
****************************************************************************}

    constructor tobjectsymtable.create(const n:string);
      begin
        inherited create(n);
        symtabletype:=objectsymtable;
      end;


    procedure tobjectsymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
         { check for duplicate field id in inherited classes }
         if (sym.typ=varsym) and
            assigned(defowner) and
            (
             not(m_delphi in aktmodeswitches) or
             is_object(tdef(defowner))
            ) then
           begin
              { but private ids can be reused }
              hsym:=search_class_member(tobjectdef(defowner),sym.name);
              if assigned(hsym) and
                 tstoredsym(hsym).is_visible_for_object(tobjectdef(defowner)) then
               begin
                 DuplicateSym(hsym);
                 exit;
               end;
           end;
         inherited insert(sym);
      end;


{****************************************************************************
                          TAbstractLocalSymtable
****************************************************************************}

    procedure tabstractlocalsymtable.ppuload(ppufile:tcompilerppufile);
      var
        storesymtable : tsymtable;
      begin
        storesymtable:=aktlocalsymtable;
        aktlocalsymtable:=self;

        inherited ppuload(ppufile);

        aktlocalsymtable:=storesymtable;
      end;


   procedure tabstractlocalsymtable.ppuwrite(ppufile:tcompilerppufile);
      var
        oldtyp : byte;
        storesymtable : tsymtable;
      begin
         storesymtable:=aktlocalsymtable;
         aktlocalsymtable:=self;
         oldtyp:=ppufile.entrytyp;
         ppufile.entrytyp:=subentryid;

         { write definitions }
         writedefs(ppufile);
         { write symbols }
         writesyms(ppufile);

         ppufile.entrytyp:=oldtyp;
         aktlocalsymtable:=storesymtable;
      end;


    procedure tabstractlocalsymtable.load_references(ppufile:tcompilerppufile;locals:boolean);
      var
        storesymtable : tsymtable;
      begin
        storesymtable:=aktlocalsymtable;
        aktlocalsymtable:=self;

        inherited load_references(ppufile,locals);

        aktlocalsymtable:=storesymtable;
      end;


    procedure tabstractlocalsymtable.write_references(ppufile:tcompilerppufile;locals:boolean);
      var
        storesymtable : tsymtable;
      begin
        storesymtable:=aktlocalsymtable;
        aktlocalsymtable:=self;

        inherited write_references(ppufile,locals);

        aktlocalsymtable:=storesymtable;
      end;


{****************************************************************************
                              TLocalSymtable
****************************************************************************}

    constructor tlocalsymtable.create;
      begin
        inherited create('');
        symtabletype:=localsymtable;
      end;


    procedure tlocalsymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
         if assigned(next) then
          begin
            if (next.symtabletype=parasymtable) then
             begin
               hsym:=tsym(next.search(sym.name));
               if assigned(hsym) then
                begin
                  { a parameter and the function can have the same
                    name in TP and Delphi, but RESULT not }
                  if (m_duplicate_names in aktmodeswitches) and
                     (sym.typ=funcretsym) and
                     not((m_result in aktmodeswitches) and
                         (sym.name='RESULT')) then
                   sym.name:='hidden'+sym.name
                  else
                   begin
                     DuplicateSym(hsym);
                     exit;
                   end;
                end;
             end;

            { check for duplicate id in local symtable of methods }
            if assigned(next.next) and
               { funcretsym is allowed !! }
               (sym.typ <> funcretsym) and
               (next.next.symtabletype=objectsymtable) then
             begin
               hsym:=search_class_member(tobjectdef(next.next.defowner),sym.name);
               if assigned(hsym) and
                 { private ids can be reused }
                  (not(sp_private in hsym.symoptions) or
                   (hsym.owner.defowner.owner.symtabletype<>globalsymtable)) then
                begin
                  { delphi allows to reuse the names in a class, but not
                    in object (tp7 compatible) }
                  if not((m_delphi in aktmodeswitches) and
                         is_class(tdef(next.next.defowner))) then
                   begin
                     DuplicateSym(hsym);
                     exit;
                   end;
                end;
             end;
          end;

         inherited insert(sym);
      end;


    procedure tlocalsymtable.insertvardata(sym : tsymentry);
      var
        l,varalign : longint;
      begin
        if not(sym.typ in [varsym,funcretsym]) then
          internalerror(200208255);
        case sym.typ of
          varsym :
            begin
              tvarsym(sym).varstate:=vs_declared;
              l:=tvarsym(sym).getvaluesize;
              varalign:=size_2_align(l);
              varalign:=used_align(varalign,aktalignment.localalignmin,aktalignment.localalignmax);
{$ifdef powerpc}
              { on the powerpc, the local variables are accessed with a positiv offset }
              tvarsym(sym).address:=align(datasize,varalign);
              datasize:=tvarsym(sym).address+l;
{$else powerpc}
              tvarsym(sym).address:=align(datasize+l,varalign);
              datasize:=tvarsym(sym).address;
{$endif powerpc}
            end;
          funcretsym :
            begin
              { if retoffset is already set then reuse it, this is needed
                when inserting the result variable }
              if procinfo.return_offset<>0 then
               tfuncretsym(sym).address:=procinfo.return_offset
              else
               begin
                 { allocate space in local if ret in register }
                 if paramanager.ret_in_reg(tfuncretsym(sym).returntype.def) then
                  begin
                    l:=tfuncretsym(sym).returntype.def.size;
                    varalign:=size_2_align(l);
                    varalign:=used_align(varalign,aktalignment.localalignmin,dataalignment);
                    tfuncretsym(sym).address:=align(datasize+l,varalign);
                    datasize:=tfuncretsym(sym).address;
                    procinfo.return_offset:=-tfuncretsym(sym).address;
                  end;
               end;
            end;
        end;
      end;


    procedure tlocalsymtable.insertconstdata(sym : tsymentry);
      var
        storefilepos : tfileposinfo;
        curconstsegment : taasmoutput;
        address,l,varalign : longint;
      begin
        { Note: this is the same code as tabstractunitsymtable.insertconstdata }
        if sym.typ<>typedconstsym then
         internalerror(200208254);
        storefilepos:=aktfilepos;
        aktfilepos:=tsym(sym).fileinfo;
        if ttypedconstsym(sym).is_writable then
          curconstsegment:=datasegment
        else
          curconstsegment:=consts;
        l:=ttypedconstsym(sym).getsize;
        varalign:=size_2_align(l);
        varalign:=used_align(varalign,aktalignment.constalignmin,aktalignment.constalignmax);
        address:=align(datasize,varalign);
        { insert cut for smartlinking or alignment }
        if (cs_create_smart in aktmoduleswitches) then
          curconstSegment.concat(Tai_cut.Create)
        else if (address<>datasize) then
          curconstSegment.concat(Tai_align.create(varalign));
        datasize:=address+l;
{$ifdef GDB}
        if cs_debuginfo in aktmoduleswitches then
          concatstabto(curconstsegment);
{$endif GDB}
        if (cs_create_smart in aktmoduleswitches) or
           DLLSource then
          begin
            curconstSegment.concat(Tai_symbol.Createdataname_global(
                ttypedconstsym(sym).mangledname,ttypedconstsym(sym).getsize));
          end
        else
          begin
            curconstSegment.concat(Tai_symbol.Createdataname(
                ttypedconstsym(sym).mangledname,ttypedconstsym(sym).getsize));
          end;
        aktfilepos:=storefilepos;
      end;


{****************************************************************************
                              TParaSymtable
****************************************************************************}

    constructor tparasymtable.create;
      begin
        inherited create('');
        symtabletype:=parasymtable;
        dataalignment:=aktalignment.paraalign;
      end;


    procedure tparasymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
         { check for duplicate id in para symtable of methods }
         if assigned(procinfo) and
            assigned(procinfo._class) and
            { but not in nested procedures !}
            (not(assigned(procinfo.parent)) or
             (assigned(procinfo.parent) and
              not(assigned(procinfo.parent._class)))
            ) and
            { funcretsym is allowed !! }
            (sym.typ<>funcretsym) then
           begin
              hsym:=search_class_member(procinfo._class,sym.name);
              { private ids can be reused }
              if assigned(hsym) and
                 tstoredsym(hsym).is_visible_for_object(procinfo._class) then
               begin
                 { delphi allows to reuse the names in a class, but not
                   in object (tp7 compatible) }
                 if not((m_delphi in aktmodeswitches) and
                        is_class_or_interface(procinfo._class)) then
                  begin
                    DuplicateSym(hsym);
                    exit;
                  end;
               end;
           end;

         inherited insert(sym);
      end;


    procedure tparasymtable.insertvardata(sym : tsymentry);
      var
        l,varalign : longint;
        is_cdecl : boolean;
      begin
        if sym.typ<>varsym then
          internalerror(200208253);
        { retrieve cdecl status }
        if defowner.deftype<>procdef then
          internalerror(200208256);
        is_cdecl:=(tprocdef(defowner).proccalloption in [pocall_cdecl,pocall_cppdecl]);
        { here we need the size of a push instead of the
          size of the data }
        l:=tvarsym(sym).getpushsize(is_cdecl);
        varalign:=size_2_align(l);
        tvarsym(sym).varstate:=vs_assigned;
        { we need the new datasize already aligned so we can't
          use the align_address here }
        tvarsym(sym).address:=datasize;
        varalign:=used_align(varalign,dataalignment,dataalignment);
        datasize:=align(tvarsym(sym).address+l,varalign);
      end;


{****************************************************************************
                         TAbstractUnitSymtable
****************************************************************************}

    constructor tabstractunitsymtable.create(const n : string);
      begin
        inherited create(n);
        symsearch.usehash;
{$ifdef GDB}
         { reset GDB things }
         prev_dbx_counter := dbx_counter;
         dbx_counter := nil;
         is_stab_written:=false;
         dbx_count := -1;
{$endif GDB}
      end;


    procedure tabstractunitsymtable.insertvardata(sym : tsymentry);
      var
        l,varalign : longint;
        storefilepos : tfileposinfo;
      begin
        if sym.typ<>varsym then
         internalerror(200208252);
        storefilepos:=aktfilepos;
        aktfilepos:=tsym(sym).fileinfo;
        l:=tvarsym(sym).getvaluesize;
        if (vo_is_thread_var in tvarsym(sym).varoptions) then
          inc(l,pointer_size);
        varalign:=size_2_align(l);
        varalign:=used_align(varalign,aktalignment.varalignmin,aktalignment.varalignmax);
        tvarsym(sym).address:=align(datasize,varalign);
        { insert cut for smartlinking or alignment }
        if (cs_create_smart in aktmoduleswitches) then
          bssSegment.concat(Tai_cut.Create)
        else if (tvarsym(sym).address<>datasize) then
          bssSegment.concat(Tai_align.create(varalign));
        datasize:=tvarsym(sym).address+l;
{$ifdef GDB}
        if cs_debuginfo in aktmoduleswitches then
           concatstabto(bsssegment);
{$endif GDB}
        if (symtabletype=globalsymtable) or
           (cs_create_smart in aktmoduleswitches) or
           DLLSource or
           (vo_is_exported in tvarsym(sym).varoptions) or
           (vo_is_C_var in tvarsym(sym).varoptions) then
          bssSegment.concat(Tai_datablock.Create_global(tvarsym(sym).mangledname,l))
        else
          bssSegment.concat(Tai_datablock.Create(tvarsym(sym).mangledname,l));
        aktfilepos:=storefilepos;
      end;


    procedure tabstractunitsymtable.insertconstdata(sym : tsymentry);
      var
        storefilepos : tfileposinfo;
        curconstsegment : taasmoutput;
        address,l,varalign : longint;
      begin
        if sym.typ<>typedconstsym then
         internalerror(200208254);
        storefilepos:=aktfilepos;
        aktfilepos:=tsym(sym).fileinfo;
        if ttypedconstsym(sym).is_writable then
          curconstsegment:=datasegment
        else
          curconstsegment:=consts;
        l:=ttypedconstsym(sym).getsize;
        varalign:=size_2_align(l);
        varalign:=used_align(varalign,aktalignment.constalignmin,aktalignment.constalignmax);
        address:=align(datasize,varalign);
        { insert cut for smartlinking or alignment }
        if (cs_create_smart in aktmoduleswitches) then
          curconstSegment.concat(Tai_cut.Create)
        else if (address<>datasize) then
          curconstSegment.concat(Tai_align.create(varalign));
        datasize:=address+l;
{$ifdef GDB}
        if cs_debuginfo in aktmoduleswitches then
          concatstabto(curconstsegment);
{$endif GDB}
        if (symtabletype=globalsymtable) or
           (cs_create_smart in aktmoduleswitches) or
           DLLSource then
          begin
            curconstSegment.concat(Tai_symbol.Createdataname_global(
                ttypedconstsym(sym).mangledname,ttypedconstsym(sym).getsize));
          end
        else
          begin
            curconstSegment.concat(Tai_symbol.Createdataname(
                ttypedconstsym(sym).mangledname,ttypedconstsym(sym).getsize));
          end;
        aktfilepos:=storefilepos;
      end;


{$ifdef GDB}
      procedure tabstractunitsymtable.concattypestabto(asmlist : taasmoutput);
        var prev_dbx_count : plongint;
        begin
           if is_stab_written then
             exit;
           if not assigned(name) then
             name := stringdup('Main_program');
           if (symtabletype = globalsymtable) and
              (current_module.globalsymtable<>self) then
             begin
                unitid:=current_module.unitcount;
                inc(current_module.unitcount);
             end;
           asmList.concat(tai_comment.Create(strpnew('Begin unit '+name^+' has index '+tostr(unitid))));
           if cs_gdb_dbx in aktglobalswitches then
             begin
                if dbx_count_ok then
                  begin
                     asmList.concat(tai_comment.Create(strpnew('"repeated" unit '+name^
                              +' has index '+tostr(unitid)+' dbx count = '+tostr(dbx_count))));
                     asmList.concat(Tai_stabs.Create(strpnew('"'+name^+'",'
                       +tostr(N_EXCL)+',0,0,'+tostr(dbx_count))));
                     exit;
                  end
                else if (current_module.globalsymtable<>self) then
                  begin
                    prev_dbx_count := dbx_counter;
                    dbx_counter := nil;
                    do_count_dbx:=false;
                    if (symtabletype = globalsymtable) and (unitid<>0) then
                      asmList.concat(Tai_stabs.Create(strpnew('"'+name^+'",'+tostr(N_BINCL)+',0,0,0')));
                    dbx_counter := @dbx_count;
                    dbx_count:=0;
                    do_count_dbx:=assigned(dbx_counter);
                  end;
             end;
           foreach({$ifdef FPCPROCVAR}@{$endif}concattypestab,asmlist);
           if cs_gdb_dbx in aktglobalswitches then
             begin
                if (current_module.globalsymtable<>self) then
                  begin
                    dbx_counter := prev_dbx_count;
                    do_count_dbx:=false;
                    asmList.concat(tai_comment.Create(strpnew('End unit '+name^
                      +' has index '+tostr(unitid))));
                    asmList.concat(Tai_stabs.Create(strpnew('"'+name^+'",'
                      +tostr(N_EINCL)+',0,0,0')));
                    do_count_dbx:=assigned(dbx_counter);
                    dbx_count_ok := {true}false;
                  end;
             end;
           is_stab_written:=true;
        end;
{$endif GDB}


{****************************************************************************
                              TStaticSymtable
****************************************************************************}

    constructor tstaticsymtable.create(const n : string);
      begin
        inherited create(n);
        symtabletype:=staticsymtable;
      end;


    procedure tstaticsymtable.ppuload(ppufile:tcompilerppufile);
      begin
        aktstaticsymtable:=self;

        next:=symtablestack;
        symtablestack:=self;

        inherited ppuload(ppufile);

        { now we can deref the syms and defs }
        deref;

        { restore symtablestack }
        symtablestack:=next;
      end;


    procedure tstaticsymtable.ppuwrite(ppufile:tcompilerppufile);
      begin
        aktstaticsymtable:=self;

        inherited ppuwrite(ppufile);
      end;


    procedure tstaticsymtable.load_references(ppufile:tcompilerppufile;locals:boolean);
      begin
        aktstaticsymtable:=self;

        inherited load_references(ppufile,locals);
      end;


    procedure tstaticsymtable.write_references(ppufile:tcompilerppufile;locals:boolean);
      begin
        aktstaticsymtable:=self;

        inherited write_references(ppufile,locals);
      end;


    procedure tstaticsymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
         { also check the global symtable }
         if assigned(next) and
            (next.unitid=0) then
          begin
            hsym:=tsym(next.search(sym.name));
            if assigned(hsym) then
             begin
               DuplicateSym(hsym);
               exit;
             end;
          end;

         inherited insert(sym);
      end;


    procedure tstaticsymtable.insertvardata(sym : tsymentry);
      begin
        inherited insertvardata(sym);
        { enable unitialized warning for local symbols }
        if sym.typ=varsym then
          tvarsym(sym).varstate:=vs_declared;
      end;


{****************************************************************************
                              TGlobalSymtable
****************************************************************************}

    constructor tglobalsymtable.create(const n : string);
      begin
         inherited create(n);
         symtabletype:=globalsymtable;
         unitid:=0;
         unitsym:=nil;
{$ifdef GDB}
         if cs_gdb_dbx in aktglobalswitches then
           begin
             dbx_count := 0;
             unittypecount:=1;
             pglobaltypecount := @unittypecount;
             {unitid:=current_module.unitcount;}
             debugList.concat(tai_comment.Create(strpnew('Global '+name^+' has index '+tostr(unitid))));
             debugList.concat(Tai_stabs.Create(strpnew('"'+name^+'",'+tostr(N_BINCL)+',0,0,0')));
             {inc(current_module.unitcount);}
             dbx_count_ok:=false;
             dbx_counter:=@dbx_count;
             do_count_dbx:=true;
           end;
{$endif GDB}
      end;


     destructor tglobalsymtable.destroy;
       var
          pus : tunitsym;
       begin
          pus:=unitsym;
          while assigned(pus) do
            begin
               unitsym:=pus.prevsym;
               pus.prevsym:=nil;
               pus.unitsymtable:=nil;
               pus:=unitsym;
            end;
          inherited destroy;
       end;


    procedure tglobalsymtable.ppuload(ppufile:tcompilerppufile);
      begin
{$ifdef GDB}
         if cs_gdb_dbx in aktglobalswitches then
           begin
              UnitTypeCount:=1;
              PglobalTypeCount:=@UnitTypeCount;
           end;
{$endif GDB}

         symtablelevel:=0;
{$ifndef NEWMAP}
         current_module.map^[0]:=self;
{$else NEWMAP}
         current_module.globalsymtable:=self;
{$endif NEWMAP}

         next:=symtablestack;
         symtablestack:=self;

         inherited ppuload(ppufile);

         { now we can deref the syms and defs }
         deref;

         { restore symtablestack }
         symtablestack:=next;

{$ifdef NEWMAP}
         { necessary for dependencies }
         current_module.globalsymtable:=nil;
{$endif NEWMAP}
      end;


    procedure tglobalsymtable.ppuwrite(ppufile:tcompilerppufile);
      begin
        { write the symtable entries }
        inherited ppuwrite(ppufile);

        { write dbx count }
{$ifdef GDB}
        if cs_gdb_dbx in aktglobalswitches then
         begin
{$IfDef EXTDEBUG}
           writeln('Writing dbx_count ',dbx_count,' in unit ',name^,'.ppu');
{$ENDIF EXTDEBUG}
           ppufile.do_crc:=false;
           ppufile.putlongint(dbx_count);
           ppufile.writeentry(ibdbxcount);
           ppufile.do_crc:=true;
         end;
{$endif GDB}
      end;


    procedure tglobalsymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
         { also check the global symtable }
         if assigned(next) and
            (next.unitid=0) then
          begin
            hsym:=tsym(next.search(sym.name));
            if assigned(hsym) then
             begin
               DuplicateSym(hsym);
               exit;
             end;
          end;

         hsym:=tsym(search(sym.name));
         if assigned(hsym) then
          begin
            { Delphi you can have a symbol with the same name as the
              unit, the unit can then not be accessed anymore using
              <unit>.<id>, so we can hide the symbol }
            if (m_duplicate_names in aktmodeswitches) and
               (hsym.typ=symconst.unitsym) then
             hsym.owner.rename(hsym.name,'hidden'+hsym.name)
            else
             begin
               DuplicateSym(hsym);
               exit;
             end;
          end;

         inherited insert(sym);
      end;


    procedure tglobalsymtable.insertvardata(sym : tsymentry);
      begin
        inherited insertvardata(sym);
        { this symbol can't be loaded to a register }
        if sym.typ=varsym then
         begin
           exclude(tvarsym(sym).varoptions,vo_regable);
           exclude(tvarsym(sym).varoptions,vo_fpuregable);
         end;
      end;


{$ifdef GDB}
   function tglobalsymtable.getnewtypecount : word;
      begin
         if not (cs_gdb_dbx in aktglobalswitches) then
           getnewtypecount:=inherited getnewtypecount
         else
           begin
              getnewtypecount:=unittypecount;
              inc(unittypecount);
           end;
      end;
{$endif}


{****************************************************************************
                              TWITHSYMTABLE
****************************************************************************}

    constructor twithsymtable.create(aowner:tdef;asymsearch:TDictionary);
      begin
         inherited create('');
         symtabletype:=withsymtable;
         direct_with:=false;
         withnode:=nil;
         withrefnode:=nil;
         { we don't need the symsearch }
         symsearch.free;
         { set the defaults }
         symsearch:=asymsearch;
         defowner:=aowner;
      end;


    destructor twithsymtable.destroy;
      begin
        symsearch:=nil;
        inherited destroy;
      end;


    procedure twithsymtable.clear;
      begin
         { remove no entry from a withsymtable as it is only a pointer to the
           recorddef  or objectdef symtable }
      end;


{****************************************************************************
                          TSTT_ExceptionSymtable
****************************************************************************}

    constructor tstt_exceptsymtable.create;
      begin
        inherited create('');
        symtabletype:=stt_exceptsymtable;
      end;


{*****************************************************************************
                             Helper Routines
*****************************************************************************}

    function findunitsymtable(st:tsymtable):tsymtable;
      begin
        findunitsymtable:=nil;
        repeat
          if not assigned(st) then
           internalerror(5566561);
          case st.symtabletype of
            localsymtable,
            parasymtable,
            staticsymtable :
              exit;
            globalsymtable :
              begin
                findunitsymtable:=st;
                exit;
              end;
            objectsymtable :
              st:=st.defowner.owner;
            recordsymtable :
              begin
                { don't continue when the current
                  symtable is used for variant records }
                if trecorddef(st.defowner).isunion then
                 begin
                   findunitsymtable:=nil;
                   exit;
                 end
                else
                 st:=st.defowner.owner;
              end;
            else
              internalerror(5566562);
          end;
        until false;
      end;


     procedure duplicatesym(sym:tsym);
       var
         st : tsymtable;
       begin
         Message1(sym_e_duplicate_id,sym.realname);
         st:=findunitsymtable(sym.owner);
         with sym.fileinfo do
           begin
             if assigned(st) and (st.unitid<>0) then
               Message2(sym_h_duplicate_id_where,'unit '+st.name^,tostr(line))
             else
               Message2(sym_h_duplicate_id_where,current_module.sourcefiles.get_file_name(fileindex),tostr(line));
           end;
       end;


{*****************************************************************************
                                  Search
*****************************************************************************}

    function  searchsym(const s : stringid;var srsym:tsym;var srsymtable:tsymtable):boolean;
      var
        speedvalue : cardinal;
      begin
         speedvalue:=getspeedvalue(s);
         srsymtable:=symtablestack;
         while assigned(srsymtable) do
           begin
              srsym:=tsym(srsymtable.speedsearch(s,speedvalue));
              if assigned(srsym) and
                 tstoredsym(srsym).is_visible_for_proc(aktprocdef) then
               begin
                 searchsym:=true;
                 exit;
               end
              else
               srsymtable:=srsymtable.next;
           end;
         searchsym:=false;
      end;


    function  searchsymonlyin(p : tsymtable;const s : stringid):tsym;
      var
        srsym      : tsym;
      begin
         { the caller have to take care if srsym=nil }
         if assigned(p) then
           begin
              srsym:=tsym(p.search(s));
              if assigned(srsym) then
               begin
                 searchsymonlyin:=srsym;
                 exit;
               end;
              { also check in the local symtbale if it exists }
              if (p=tsymtable(current_module.globalsymtable)) then
                begin
                   srsym:=tsym(current_module.localsymtable.search(s));
                   if assigned(srsym) then
                    begin
                      searchsymonlyin:=srsym;
                      exit;
                    end;
                end
           end;
         searchsymonlyin:=nil;
       end;


    function  searchsym_in_class(classh:tobjectdef;const s : stringid):tsym;
      var
        speedvalue : cardinal;
        topclassh  : tobjectdef;
        sym        : tsym;
      begin
         speedvalue:=getspeedvalue(s);
         { when the class passed is defined in this unit we
           need to use the scope of that class. This is a trick
           that can be used to access protected members in other
           units. At least kylix supports it this way (PFV) }
         if (classh.owner.symtabletype in [globalsymtable,staticsymtable]) and
            (classh.owner.unitid=0) then
           topclassh:=classh
         else
           topclassh:=nil;
         sym:=nil;
         while assigned(classh) do
          begin
            sym:=tsym(classh.symtable.speedsearch(s,speedvalue));
            if assigned(sym) then
             begin
               if assigned(topclassh) then
                begin
                  if tstoredsym(sym).is_visible_for_object(topclassh) then
                   break;
                end
               else
                begin
                  if tstoredsym(sym).is_visible_for_proc(aktprocdef) then
                   break;
                end;
             end;
            classh:=classh.childof;
          end;
         searchsym_in_class:=sym;
      end;


    function searchsystype(const s: stringid; var srsym: ttypesym): boolean;
      var
        symowner: tsymtable;
      begin
        if not(cs_compilesystem in aktmoduleswitches) then
          srsym := ttypesym(searchsymonlyin(systemunit,s))
        else
          searchsym(s,tsym(srsym),symowner);
        searchsystype :=
          assigned(srsym) and
          (srsym.typ = typesym);
      end;


    function searchsysvar(const s: stringid; var srsym: tvarsym; var symowner: tsymtable): boolean;
      begin
        if not(cs_compilesystem in aktmoduleswitches) then
          begin
            srsym := tvarsym(searchsymonlyin(systemunit,s));
            symowner := systemunit;
          end
        else
          searchsym(s,tsym(srsym),symowner);
        searchsysvar :=
          assigned(srsym) and
          (srsym.typ = varsym);
      end;


    function search_class_member(pd : tobjectdef;const s : string):tsym;
    { searches n in symtable of pd and all anchestors }
      var
        speedvalue : cardinal;
        srsym      : tsym;
      begin
        speedvalue:=getspeedvalue(s);
        while assigned(pd) do
         begin
           srsym:=tsym(pd.symtable.speedsearch(s,speedvalue));
           if assigned(srsym) then
            begin
              search_class_member:=srsym;
              exit;
            end;
           pd:=pd.childof;
         end;
        search_class_member:=nil;
      end;


{*****************************************************************************
                            Definition Helpers
*****************************************************************************}

    procedure globaldef(const s : string;var t:ttype);

      var st : string;
          symt : tsymtable;
          srsym      : tsym;
          srsymtable : tsymtable;
      begin
         srsym := nil;
         if pos('.',s) > 0 then
           begin
           st := copy(s,1,pos('.',s)-1);
           searchsym(st,srsym,srsymtable);
           st := copy(s,pos('.',s)+1,255);
           if assigned(srsym) then
             begin
             if srsym.typ = unitsym then
               begin
               symt := tunitsym(srsym).unitsymtable;
               srsym := tsym(symt.search(st));
               end else srsym := nil;
             end;
           end else st := s;
         if srsym = nil then
          searchsym(st,srsym,srsymtable);
         if srsym = nil then
           srsym:=searchsymonlyin(systemunit,st);
         if (not assigned(srsym)) or
            (srsym.typ<>typesym) then
           begin
             Message(type_e_type_id_expected);
             t:=generrortype;
             exit;
           end;
         t := ttypesym(srsym).restype;
      end;

{****************************************************************************
                              Object Helpers
****************************************************************************}

   var
      _defaultprop : tpropertysym;

   procedure tstoredsymtable.testfordefaultproperty(p : TNamedIndexItem;arg:pointer);
     begin
        if (tsym(p).typ=propertysym) and
           (ppo_defaultproperty in tpropertysym(p).propoptions) then
          _defaultprop:=tpropertysym(p);
     end;


   function search_default_property(pd : tobjectdef) : tpropertysym;
   { returns the default property of a class, searches also anchestors }
     begin
        _defaultprop:=nil;
        while assigned(pd) do
          begin
             pd.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}tstoredsymtable(pd.symtable).testfordefaultproperty,nil);
             if assigned(_defaultprop) then
               break;
             pd:=pd.childof;
          end;
        search_default_property:=_defaultprop;
     end;


{$ifdef UNITALIASES}
{****************************************************************************
                              TUNIT_ALIAS
 ****************************************************************************}

    constructor tunit_alias.create(const n:string);
      var
        i : longint;
      begin
        i:=pos('=',n);
        if i=0 then
         fail;
        inherited createname(Copy(n,1,i-1));
        newname:=stringdup(Copy(n,i+1,255));
      end;


    destructor tunit_alias.destroy;
      begin
        stringdispose(newname);
        inherited destroy;
      end;


    procedure addunitalias(const n:string);
      begin
        unitaliases^.insert(tunit_alias,init(Upper(n))));
      end;


    function getunitalias(const n:string):string;
      var
        p : punit_alias;
      begin
        p:=punit_alias(unitaliases^.search(Upper(n)));
        if assigned(p) then
         getunitalias:=punit_alias(p).newname^
        else
         getunitalias:=n;
      end;
{$endif UNITALIASES}


{****************************************************************************
                            Symtable Stack
****************************************************************************}

    procedure dellexlevel;
      var
         p : tsymtable;
      begin
         p:=symtablestack;
         symtablestack:=p.next;
         { symbol tables of unit interfaces are never disposed }
         { this is handle by the unit unitm                 }
         if not(p.symtabletype in [globalsymtable,stt_exceptsymtable]) then
          p.free;
      end;

    procedure RestoreUnitSyms;
      var
         p : tsymtable;
      begin
         p:=symtablestack;
         while assigned(p) do
           begin
             if (p.symtabletype=globalsymtable) and
                assigned(tglobalsymtable(p).unitsym) and
                ((tglobalsymtable(p).unitsym.owner=current_module.globalsymtable) or
                 (tglobalsymtable(p).unitsym.owner=current_module.localsymtable)) then
               tglobalsymtable(p).unitsym.restoreunitsym;
             p:=p.next;
           end;
      end;

{$ifdef DEBUG}
    procedure test_symtablestack;
      var
         p : tsymtable;
         i : longint;
      begin
         p:=symtablestack;
         i:=0;
         while assigned(p) do
           begin
              inc(i);
              p:=p.next;
              if i>500 then
               Message(sym_f_internal_error_in_symtablestack);
           end;
      end;

    procedure list_symtablestack;
      var
         p : tsymtable;
         i : longint;
      begin
         p:=symtablestack;
         i:=0;
         while assigned(p) do
           begin
              inc(i);
              writeln(i,' ',p.name^);
              p:=p.next;
              if i>500 then
               Message(sym_f_internal_error_in_symtablestack);
           end;
      end;
{$endif DEBUG}


{****************************************************************************
                           Init/Done Symtable
****************************************************************************}

   procedure InitSymtable;
     var
       token : ttoken;
     begin
      { Reset symbolstack }
        registerdef:=false;
        read_member:=false;
        symtablestack:=nil;
        systemunit:=nil;
{$ifdef GDB}
        firstglobaldef:=nil;
        lastglobaldef:=nil;
        globaltypecount:=1;
        pglobaltypecount:=@globaltypecount;
{$endif GDB}
     { create error syms and def }
        generrorsym:=terrorsym.create;
        generrortype.setdef(terrordef.create);
{$ifdef UNITALIASES}
     { unit aliases }
        unitaliases:=tdictionary.create;
{$endif}
       for token:=first_overloaded to last_overloaded do
         overloaded_operators[token]:=nil;
     end;


   procedure DoneSymtable;
      begin
        generrorsym.free;
        generrortype.def.free;
{$ifdef UNITALIASES}
        unitaliases.free;
{$endif}
     end;

end.
{
  $Log$
  Revision 1.75  2002-10-14 19:44:43  peter
    * threadvars need 4 bytes extra for storing the threadvar index

  Revision 1.74  2002/10/06 19:41:31  peter
    * Add finalization of typed consts
    * Finalization of globals in the main program

  Revision 1.73  2002/10/05 12:43:29  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.72  2002/09/09 19:41:46  peter
    * real fix internalerror for dup ids in union sym

  Revision 1.71  2002/09/09 17:34:16  peter
    * tdicationary.replace added to replace and item in a dictionary. This
      is only allowed for the same name
    * varsyms are inserted in symtable before the types are parsed. This
      fixes the long standing "var longint : longint" bug
    - consume_idlist and idstringlist removed. The loops are inserted
      at the callers place and uses the symtable for duplicate id checking

  Revision 1.70  2002/09/05 19:29:45  peter
    * memdebug enhancements

  Revision 1.69  2002/08/25 19:25:21  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.68  2002/08/18 20:06:27  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.67  2002/08/17 09:23:43  florian
    * first part of procinfo rewrite

  Revision 1.66  2002/08/11 13:24:15  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.65  2002/07/23 09:51:27  daniel
  * Tried to make Tprocsym.defs protected. I didn't succeed but the cleanups
    are worth comitting.

  Revision 1.64  2002/07/16 15:34:21  florian
    * exit is now a syssym instead of a keyword

  Revision 1.63  2002/07/15 19:44:53  florian
    * fixed crash with default parameters and stdcall calling convention

  Revision 1.62  2002/07/01 18:46:28  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.61  2002/05/18 13:34:19  peter
    * readded missing revisions

  Revision 1.60  2002/05/16 19:46:45  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.58  2002/05/12 16:53:15  peter
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

  Revision 1.57  2002/04/04 19:06:05  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.56  2002/03/04 19:10:11  peter
    * removed compiler warnings

  Revision 1.55  2002/02/03 09:30:07  peter
    * more fixes for protected handling

  Revision 1.54  2002/01/29 21:30:25  peter
    * allow also dup id in delphi mode in interfaces

  Revision 1.53  2002/01/29 19:46:00  peter
    * fixed recordsymtable.insert_in() for inserting variant record fields
      to not used symtable.insert() because that also updates alignmentinfo
      which was already set

  Revision 1.52  2002/01/24 18:25:50  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

}
