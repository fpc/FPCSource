{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

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

{$i defines.inc}

interface

    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,tokens,
       { symtable }
       symconst,symbase,symtype,symdef,symsym,
       { ppu }
       ppu,symppu,
       { assembler }
       aasm
       ;


{****************************************************************************
                             Symtable types
****************************************************************************}

    type
       tstoredsymtable = class(tsymtable)
       private
          b_needs_init_final : boolean;
          procedure _needs_init_final(p : tnamedindexitem);
          procedure check_forward(sym : TNamedIndexItem);
          procedure labeldefined(p : TNamedIndexItem);
          procedure unitsymbolused(p : TNamedIndexItem);
          procedure varsymbolused(p : TNamedIndexItem);
          procedure TestPrivate(p : TNamedIndexItem);
          procedure objectprivatesymbolused(p : TNamedIndexItem);
{$ifdef GDB}
       private
          asmoutput : taasmoutput;
          procedure concatstab(p : TNamedIndexItem);
          procedure resetstab(p : TNamedIndexItem);
          procedure concattypestab(p : TNamedIndexItem);
{$endif}
          procedure unchain_overloads(p : TNamedIndexItem);
          procedure loaddefs(ppufile:tcompilerppufile);
          procedure loadsyms(ppufile:tcompilerppufile);
          procedure writedefs(ppufile:tcompilerppufile);
          procedure writesyms(ppufile:tcompilerppufile);
       public
          { load/write }
          procedure load(ppufile:tcompilerppufile);virtual;
          procedure write(ppufile:tcompilerppufile);virtual;
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
          procedure testfordefaultproperty(p : TNamedIndexItem);
       end;

       tabstractrecordsymtable = class(tstoredsymtable)
       public
          procedure load(ppufile:tcompilerppufile);override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);override;
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
          procedure load(ppufile:tcompilerppufile);override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);override;
       end;

       tlocalsymtable = class(tabstractlocalsymtable)
       public
          constructor create;
          procedure insert(sym : tsymentry);override;
       end;

       tparasymtable = class(tabstractlocalsymtable)
       public
          constructor create;
          procedure insert(sym : tsymentry);override;
          { change alignment for args  only parasymtable }
          procedure set_alignment(_alignment : longint);
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
       end;

       tglobalsymtable = class(tabstractunitsymtable)
       public
          unittypecount : word;
          unitsym       : tunitsym;
          constructor create(const n : string);
          destructor  destroy;override;
          procedure load(ppufile:tcompilerppufile);override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure insert(sym : tsymentry);override;
{$ifdef GDB}
          function getnewtypecount : word; override;
{$endif}
       end;

       tstaticsymtable = class(tabstractunitsymtable)
       public
          constructor create(const n : string);
          procedure load(ppufile:tcompilerppufile);override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure insert(sym : tsymentry);override;
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
      version,verbose,globals,
      { target }
      systems,
      { module }
      finput,fmodule,
{$ifdef GDB}
      gdb,
{$endif GDB}
      { codegen }
      cgbase
      ;


    var
      in_loading : boolean;  { remove !!! }


{*****************************************************************************
                             TStoredSymtable
*****************************************************************************}

    procedure tstoredsymtable.load(ppufile:tcompilerppufile);
      begin
        { load definitions }
        loaddefs(ppufile);

        { load symbols }
        loadsyms(ppufile);
      end;


    procedure tstoredsymtable.write(ppufile:tcompilerppufile);
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
              ibpointerdef : hp:=tpointerdef.load(ppufile);
                ibarraydef : hp:=tarraydef.load(ppufile);
                  iborddef : hp:=torddef.load(ppufile);
                ibfloatdef : hp:=tfloatdef.load(ppufile);
                 ibprocdef : hp:=tprocdef.load(ppufile);
          ibshortstringdef : hp:=tstringdef.loadshort(ppufile);
           iblongstringdef : hp:=tstringdef.loadlong(ppufile);
           ibansistringdef : hp:=tstringdef.loadansi(ppufile);
           ibwidestringdef : hp:=tstringdef.loadwide(ppufile);
               ibrecorddef : hp:=trecorddef.load(ppufile);
               ibobjectdef : hp:=tobjectdef.load(ppufile);
                 ibenumdef : hp:=tenumdef.load(ppufile);
                  ibsetdef : hp:=tsetdef.load(ppufile);
              ibprocvardef : hp:=tprocvardef.load(ppufile);
                 ibfiledef : hp:=tfiledef.load(ppufile);
             ibclassrefdef : hp:=tclassrefdef.load(ppufile);
               ibformaldef : hp:=tformaldef.load(ppufile);
              ibvariantdef : hp:=tvariantdef.load(ppufile);
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
                ibtypesym : sym:=ttypesym.load(ppufile);
                ibprocsym : sym:=tprocsym.load(ppufile);
               ibconstsym : sym:=tconstsym.load(ppufile);
                 ibvarsym : sym:=tvarsym.load(ppufile);
             ibfuncretsym : sym:=tfuncretsym.load(ppufile);
            ibabsolutesym : sym:=tabsolutesym.load(ppufile);
                ibenumsym : sym:=tenumsym.load(ppufile);
          ibtypedconstsym : sym:=ttypedconstsym.load(ppufile);
            ibpropertysym : sym:=tpropertysym.load(ppufile);
                ibunitsym : sym:=tunitsym.load(ppufile);
               iblabelsym : sym:=tlabelsym.load(ppufile);
                 ibsyssym : sym:=tsyssym.load(ppufile);
                ibrttisym : sym:=trttisym.load(ppufile);
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
              pd.write(ppufile);
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
              pd.write(ppufile);
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
                 resolvesym(tsym(sym));
                 if assigned(sym) then
                   sym.load_references(ppufile,locals);
               end;
             ibdefref :
               begin
                 prdef:=tstoreddef(ppufile.getderef);
                 resolvedef(tdef(prdef));
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
         { writes the symbol in data segment if required }
         { also sets the datasize of owner             }
         if not in_loading then
           tstoredsym(sym).insert_in_data;

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

    procedure TStoredSymtable.check_forward(sym : TNamedIndexItem);
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


    procedure TStoredSymtable.labeldefined(p : TNamedIndexItem);
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


    procedure TStoredSymtable.unitsymbolused(p : TNamedIndexItem);
      begin
         if (tsym(p).typ=unitsym) and
            (tunitsym(p).refs=0) and
            { do not claim for unit name itself !! }
            (tunitsym(p).unitsymtable.symtabletype=globalsymtable) then
           MessagePos2(tsym(p).fileinfo,sym_n_unit_not_used,
             p.name,current_module.modulename^);
      end;


    procedure TStoredSymtable.varsymbolused(p : TNamedIndexItem);
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


    procedure TStoredSymtable.TestPrivate(p : TNamedIndexItem);
      begin
        if sp_private in tsym(p).symoptions then
          varsymbolused(p);
      end;


    procedure TStoredSymtable.objectprivatesymbolused(p : TNamedIndexItem);
      begin
         {
           Don't test simple object aliases PM
         }
         if (tsym(p).typ=typesym) and
            (ttypesym(p).restype.def.deftype=objectdef) and
            (ttypesym(p).restype.def.typesym=tsym(p)) then
           tobjectdef(ttypesym(p).restype.def).symtable.foreach({$ifdef FPCPROCVAR}@{$endif}TestPrivate);
      end;


    procedure tstoredsymtable.unchain_overloads(p : TNamedIndexItem);
      begin
         if tsym(p).typ=procsym then
           tprocsym(p).unchain_overload;
      end;

{$ifdef GDB}

    procedure TStoredSymtable.concatstab(p : TNamedIndexItem);
      begin
        if tsym(p).typ <> procsym then
          tstoredsym(p).concatstabto(asmoutput);
      end;

    procedure TStoredSymtable.resetstab(p : TNamedIndexItem);
      begin
        if tsym(p).typ <> procsym then
          tstoredsym(p).isstabwritten:=false;
      end;

    procedure TStoredSymtable.concattypestab(p : TNamedIndexItem);
      begin
        if tsym(p).typ = typesym then
         begin
           tstoredsym(p).isstabwritten:=false;
           tstoredsym(p).concatstabto(asmoutput);
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
        pd : pprocdeflist;
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
                        begin
                          { already got a procsym, only add defs of the current procsym }
                          pd:=tprocsym(srsym).defs;
                          while assigned(pd) do
                           begin
                             overloaded_operators[t].addprocdef(pd^.def);
                             pd:=pd^.next;
                           end;
                        end;
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
         foreach({$ifdef FPCPROCVAR}@{$endif}check_forward);
      end;


    procedure tstoredsymtable.checklabels;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}labeldefined);
      end;


    procedure tstoredsymtable.allunitsused;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}unitsymbolused);
      end;


    procedure tstoredsymtable.allsymbolsused;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}varsymbolused);
      end;


    procedure tstoredsymtable.allprivatesused;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}objectprivatesymbolused);
      end;


    procedure tstoredsymtable.unchain_overloaded;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}unchain_overloads);
      end;


{$ifdef GDB}
    procedure tstoredsymtable.concatstabto(asmlist : taasmoutput);
      begin
        asmoutput:=asmlist;
        if symtabletype in [inlineparasymtable,inlinelocalsymtable] then
          foreach({$ifdef FPCPROCVAR}@{$endif}resetstab);

        foreach({$ifdef FPCPROCVAR}@{$endif}concatstab);
      end;
{$endif}


    { returns true, if p contains data which needs init/final code }
    function tstoredsymtable.needs_init_final : boolean;
      begin
         b_needs_init_final:=false;
         foreach({$ifdef FPCPROCVAR}@{$endif}_needs_init_final);
         needs_init_final:=b_needs_init_final;
      end;


{****************************************************************************
                          TAbstractRecordSymtable
****************************************************************************}

    procedure tabstractrecordsymtable.load(ppufile:tcompilerppufile);
      var
        storesymtable : tsymtable;
      begin
        storesymtable:=aktrecordsymtable;
        aktrecordsymtable:=self;

        inherited load(ppufile);

        aktrecordsymtable:=storesymtable;
      end;


    procedure tabstractrecordsymtable.write(ppufile:tcompilerppufile);
      var
        oldtyp : byte;
        storesymtable : tsymtable;
      begin
         storesymtable:=aktrecordsymtable;
         aktrecordsymtable:=self;
         oldtyp:=ppufile.entrytyp;
         ppufile.entrytyp:=subentryid;

         inherited write(ppufile);

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


    procedure TStoredSymtable._needs_init_final(p : tnamedindexitem);
      begin
         if (not b_needs_init_final) and
            (tsym(p).typ=varsym) and
            assigned(tvarsym(p).vartype.def) and
            not is_class(tvarsym(p).vartype.def) and
            tstoreddef(tvarsym(p).vartype.def).needs_inittable then
           b_needs_init_final:=true;
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

    procedure tabstractlocalsymtable.load(ppufile:tcompilerppufile);
      var
        storesymtable : tsymtable;
      begin
        storesymtable:=aktlocalsymtable;
        aktlocalsymtable:=self;

        inherited load(ppufile);

        aktlocalsymtable:=storesymtable;
      end;


   procedure tabstractlocalsymtable.write(ppufile:tcompilerppufile);
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
         if assigned(procinfo^._class) and
         { but not in nested procedures !}
            (not(assigned(procinfo^.parent)) or
             (assigned(procinfo^.parent) and
              not(assigned(procinfo^.parent^._class)))
            ) and
          { funcretsym is allowed !! }
           (sym.typ <> funcretsym) then
           begin
              hsym:=search_class_member(procinfo^._class,sym.name);
              { private ids can be reused }
              if assigned(hsym) and
                 tstoredsym(hsym).is_visible_for_object(procinfo^._class) then
               begin
                 { delphi allows to reuse the names in a class, but not
                   in object (tp7 compatible) }
                 if not((m_delphi in aktmodeswitches) and
                        is_class(procinfo^._class)) then
                  begin
                    DuplicateSym(hsym);
                    exit;
                  end;
               end;
           end;

         inherited insert(sym);
      end;


    procedure tparasymtable.set_alignment(_alignment : longint);
      var
         sym : tvarsym;
         l : longint;
      begin
        dataalignment:=_alignment;
        sym:=tvarsym(symindex.first);
        datasize:=0;
        { there can be only varsyms }
        while assigned(sym) do
          begin
             l:=sym.getpushsize;
             sym.address:=datasize;
             datasize:=align(datasize+l,dataalignment);
             sym:=tvarsym(sym.indexnext);
          end;
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
           asmList.concat(Tai_asm_comment.Create(strpnew('Begin unit '+name^+' has index '+tostr(unitid))));
           if cs_gdb_dbx in aktglobalswitches then
             begin
                if dbx_count_ok then
                  begin
                     asmList.concat(Tai_asm_comment.Create(strpnew('"repeated" unit '+name^
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
           asmoutput:=asmlist;
           foreach({$ifdef FPCPROCVAR}@{$endif}concattypestab);
           if cs_gdb_dbx in aktglobalswitches then
             begin
                if (current_module.globalsymtable<>self) then
                  begin
                    dbx_counter := prev_dbx_count;
                    do_count_dbx:=false;
                    asmList.concat(Tai_asm_comment.Create(strpnew('End unit '+name^
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


    procedure tstaticsymtable.load(ppufile:tcompilerppufile);
      begin
        aktstaticsymtable:=self;

        next:=symtablestack;
        symtablestack:=self;

        inherited load(ppufile);

        { now we can deref the syms and defs }
        deref;

        { restore symtablestack }
        symtablestack:=next;
      end;


    procedure tstaticsymtable.write(ppufile:tcompilerppufile);
      begin
        aktstaticsymtable:=self;

        inherited write(ppufile);
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
             debugList.concat(Tai_asm_comment.Create(strpnew('Global '+name^+' has index '+tostr(unitid))));
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


    procedure tglobalsymtable.load(ppufile:tcompilerppufile);
{$ifdef GDB}
      var
        storeGlobalTypeCount : pword;
{$endif GDB}
      begin
{$ifdef GDB}
         if cs_gdb_dbx in aktglobalswitches then
           begin
              UnitTypeCount:=1;
              storeGlobalTypeCount:=PGlobalTypeCount;
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

         inherited load(ppufile);

         { now we can deref the syms and defs }
         deref;

         { restore symtablestack }
         symtablestack:=next;

{$ifdef NEWMAP}
         { necessary for dependencies }
         current_module.globalsymtable:=nil;
{$endif NEWMAP}
      end;


    procedure tglobalsymtable.write(ppufile:tcompilerppufile);
      begin
        { write the symtable entries }
        inherited write(ppufile);

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
              break;
            globalsymtable :
              begin
                findunitsymtable:=st;
                break;
              end;
            objectsymtable,
            recordsymtable :
              st:=st.defowner.owner;
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


    function searchsystype(const s: stringid; var srsym: ttypesym): boolean;
      var
        symowner: tsymtable;
      begin
        if not(cs_compilesystem in aktmoduleswitches) then
          srsym := ttypesym(searchsymonlyin(systemunit,s))
        else
          searchsym(s,srsym,symowner);
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
          searchsym(s,srsym,symowner);
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

   procedure tstoredsymtable.testfordefaultproperty(p : TNamedIndexItem);
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
             pd.symtable.foreach({$ifdef FPCPROCVAR}@{$endif}tstoredsymtable(pd.symtable).testfordefaultproperty);
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
  Revision 1.53  2002-01-29 19:46:00  peter
    * fixed recordsymtable.insert_in() for inserting variant record fields
      to not used symtable.insert() because that also updates alignmentinfo
      which was already set

  Revision 1.52  2002/01/24 18:25:50  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.51  2001/12/31 16:59:43  peter
    * protected/private symbols parsing fixed

  Revision 1.50  2001/11/18 18:43:17  peter
    * overloading supported in child classes
    * fixed parsing of classes with private and virtual and overloaded
      so it is compatible with delphi

  Revision 1.49  2001/11/02 23:16:52  peter
    * removed obsolete chainprocsym and test_procsym code

  Revision 1.48  2001/11/02 22:58:08  peter
    * procsym definition rewrite

  Revision 1.47  2001/10/12 20:27:43  jonas
    * fixed crashing bug in unit reference counting

  Revision 1.46  2001/09/30 21:29:47  peter
    * gdb fixes merged

  Revision 1.45  2001/09/19 11:06:03  michael
  * realname updated for some hints
  * realname used for consts,labels

  Revision 1.44  2001/09/04 11:38:55  jonas
    + searchsystype() and searchsystype() functions in symtable
    * changed ninl and nadd to use these functions
    * i386 set comparison functions now return their results in al instead
      of in the flags so that they can be sued as compilerprocs
    - removed all processor specific code from n386add.pas that has to do
      with set handling, it's now all done in nadd.pas
    * fixed fpc_set_contains_sets in genset.inc
    * fpc_set_in_byte is now coded inline in n386set.pas and doesn't use a
      helper anymore
    * some small fixes in compproc.inc/set.inc regarding the declaration of
      internal helper types (fpc_small_set and fpc_normal_set)

  Revision 1.43  2001/08/30 20:13:56  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.42  2001/08/26 13:36:51  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.41  2001/08/19 09:39:29  peter
    * local browser support fixed

  Revision 1.40  2001/08/06 21:40:49  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.39  2001/07/29 22:12:58  peter
    * skip private symbols when found in withsymtable

  Revision 1.38  2001/07/01 20:16:18  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.37  2001/06/04 11:53:14  peter
    + varargs directive

  Revision 1.36  2001/06/03 21:57:38  peter
    + hint directive parsing support

  Revision 1.35  2001/05/06 14:49:18  peter
    * ppu object to class rewrite
    * move ppu read and write stuff to fppu

  Revision 1.34  2001/04/18 22:01:59  peter
    * registration of targets and assemblers

  Revision 1.33  2001/04/13 20:05:15  peter
    * better check for globalsymtable

  Revision 1.32  2001/04/13 18:08:37  peter
    * scanner object to class

  Revision 1.31  2001/04/13 01:22:16  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.30  2001/04/02 21:20:35  peter
    * resulttype rewrite

  Revision 1.29  2001/03/22 00:10:58  florian
    + basic variant type support in the compiler

  Revision 1.28  2001/03/13 18:45:07  peter
    * fixed some memory leaks

  Revision 1.27  2001/03/11 22:58:51  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.26  2001/02/21 19:37:19  peter
    * moved deref to be done after loading of implementation units. prederef
      is still done directly after loading of symbols and definitions.

  Revision 1.25  2001/02/20 21:41:16  peter
    * new fixfilename, findfile for unix. Look first for lowercase, then
      NormalCase and last for UPPERCASE names.

  Revision 1.24  2001/01/08 21:40:27  peter
    * fixed crash with unsupported token overloading

  Revision 1.23  2000/12/25 00:07:30  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.22  2000/12/23 19:50:09  peter
    * fixed mem leak with withsymtable

  Revision 1.21  2000/12/10 20:25:32  peter
    * fixed missing typecast

  Revision 1.20  2000/12/10 14:14:51  florian
    * fixed web bug 1203: class fields can be now redefined
      in Delphi mode though I don't like this :/

  Revision 1.19  2000/11/30 22:16:49  florian
    * moved to i386

  Revision 1.18  2000/11/29 00:30:42  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.17  2000/11/28 00:28:07  pierre
   * stabs fixing

  Revision 1.1.2.8  2000/11/17 11:14:37  pierre
   * one more class stabs fix

  Revision 1.16  2000/11/12 22:17:47  peter
    * some realname updates for messages

  Revision 1.15  2000/11/06 15:54:15  florian
    * fixed two bugs to get make cycle work, but it's not enough

  Revision 1.14  2000/11/04 14:25:22  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.13  2000/11/01 23:04:38  peter
    * tprocdef.fullprocname added for better casesensitve writing of
      procedures

  Revision 1.12  2000/10/31 22:02:52  peter
    * symtable splitted, no real code changes

  Revision 1.1.2.7  2000/10/16 19:43:04  pierre
   * trying to correct class stabss once more

  Revision 1.11  2000/10/15 07:47:53  peter
    * unit names and procedure names are stored mixed case

  Revision 1.10  2000/10/14 10:14:53  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.9  2000/10/01 19:48:25  peter
    * lot of compile updates for cg11

  Revision 1.8  2000/09/24 15:06:29  peter
    * use defines.inc

  Revision 1.7  2000/08/27 16:11:54  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.6  2000/08/21 11:27:45  pierre
   * fix the stabs problems

  Revision 1.5  2000/08/20 14:58:41  peter
    * give fatal if objfpc/delphi mode things are found (merged)

  Revision 1.1.2.6  2000/08/20 14:56:46  peter
    * give fatal if objfpc/delphi mode things are found

  Revision 1.4  2000/08/16 18:33:54  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.3  2000/08/08 19:28:57  peter
    * memdebug/memory patches (merged)
    * only once illegal directive (merged)

}
