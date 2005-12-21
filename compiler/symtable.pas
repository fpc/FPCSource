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
       ppu,
       { assembler }
       aasmtai
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
          procedure varsymbolused(p : TNamedIndexItem;arg:pointer);
          procedure TestPrivate(p : TNamedIndexItem;arg:pointer);
          procedure objectprivatesymbolused(p : TNamedIndexItem;arg:pointer);
          procedure unchain_overloads(p : TNamedIndexItem;arg:pointer);
          procedure loaddefs(ppufile:tcompilerppufile);
          procedure loadsyms(ppufile:tcompilerppufile);
          procedure reset_def(def:Tnamedindexitem;arg:pointer);
          procedure writedefs(ppufile:tcompilerppufile);
          procedure writesyms(ppufile:tcompilerppufile);
       public
          { load/write }
          procedure ppuload(ppufile:tcompilerppufile);virtual;
          procedure ppuwrite(ppufile:tcompilerppufile);virtual;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);virtual;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);virtual;
          procedure buildderef;virtual;
          procedure buildderefimpl;virtual;
          procedure deref;virtual;
          procedure derefimpl;virtual;
          procedure insert(sym : tsymentry);override;
          procedure reset_all_defs;virtual;
          function  speedsearch(const s : stringid;speedvalue : cardinal) : tsymentry;override;
          procedure allsymbolsused;
          procedure allprivatesused;
          procedure check_forwards;
          procedure checklabels;
          function  needs_init_final : boolean;
          procedure unchain_overloaded;
          procedure testfordefaultproperty(p : TNamedIndexItem;arg:pointer);
       end;

       tabstractrecordsymtable = class(tstoredsymtable)
       public
          datasize       : aint;
          usefieldalignment,     { alignment to use for fields (PACKRECORDS value), -1 is C style }
          recordalignment,       { alignment required when inserting this record }
          fieldalignment,        { alignment current alignment used when fields are inserted }
          padalignment : shortint;   { size to a multiple of which the symtable has to be rounded up }
          constructor create(const n:string;usealign:shortint);
          procedure ppuload(ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure insertfield(sym:tfieldvarsym;addsym:boolean);
          procedure addalignmentpadding;
       end;

       trecordsymtable = class(tabstractrecordsymtable)
       public
          constructor create(usealign:shortint);
          procedure insertunionst(unionst : trecordsymtable;offset : longint);
       end;

       tobjectsymtable = class(tabstractrecordsymtable)
       public
          constructor create(const n:string;usealign:shortint);
          procedure insert(sym : tsymentry);override;
       end;

       tabstractlocalsymtable = class(tstoredsymtable)
       public
          procedure ppuwrite(ppufile:tcompilerppufile);override;
       end;

       tlocalsymtable = class(tabstractlocalsymtable)
       public
          constructor create(level:byte);
          procedure insert(sym : tsymentry);override;
       end;

       tparasymtable = class(tabstractlocalsymtable)
       public
          constructor create(level:byte);
          procedure insert(sym : tsymentry);override;
       end;

       tabstractunitsymtable = class(tstoredsymtable)
       public
          constructor create(const n : string;id:word);
          function iscurrentunit:boolean;override;
       end;

       tglobalsymtable = class(tabstractunitsymtable)
       public
          unittypecount : word;
          constructor create(const n : string;id:word);
          procedure ppuload(ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure insert(sym : tsymentry);override;
       end;

       tstaticsymtable = class(tabstractunitsymtable)
       public
          constructor create(const n : string;id:word);
          procedure ppuload(ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure load_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure write_references(ppufile:tcompilerppufile;locals:boolean);override;
          procedure insert(sym : tsymentry);override;
       end;

       twithsymtable = class(tsymtable)
          withrefnode : pointer; { tnode }
          constructor create(aowner:tdef;asymsearch:TDictionary;refnode:pointer{tnode});
          destructor  destroy;override;
          procedure clear;override;
        end;

       tstt_exceptsymtable = class(tsymtable)
       public
          constructor create;
       end;

       tmacrosymtable = class(tstoredsymtable)
       public
          constructor create(exported: boolean);
          procedure ppuload(ppufile:tcompilerppufile);override;
       end;

    var
       constsymtable  : tsymtable;      { symtable were the constants can be inserted }
       systemunit     : tglobalsymtable; { pointer to the system unit }

{****************************************************************************
                             Functions
****************************************************************************}

{*** Misc ***}
    procedure globaldef(const s : string;var t:ttype);
    function  findunitsymtable(st:tsymtable):tsymtable;
    function  FullTypeName(def,otherdef:tdef):string;
    procedure incompatibletypes(def1,def2:tdef);
    procedure hidesym(sym:tsymentry);
    procedure duplicatesym(dupsym,sym:tsymentry);

{*** Search ***}
    function  searchsym(const s : stringid;var srsym:tsym;var srsymtable:tsymtable):boolean;
    function  searchsym_type(const s : stringid;var srsym:tsym;var srsymtable:tsymtable):boolean;
    function  searchsymonlyin(p : tsymtable;const s : stringid):tsym;
    function  searchsym_in_class(classh:tobjectdef;const s : stringid):tsym;
    function  searchsym_in_class_by_msgint(classh:tobjectdef;i:longint):tsym;
    function  searchsym_in_class_by_msgstr(classh:tobjectdef;const s:string):tsym;
    function  searchsystype(const s: stringid; var srsym: ttypesym): boolean;
{$ifdef notused}
    function  searchsysvar(const s: stringid; var srsym: tsym; var symowner: tsymtable): boolean;
{$endif notused}
    function  search_class_member(pd : tobjectdef;const s : string):tsym;
    function  search_assignment_operator(from_def,to_def:Tdef):Tprocdef;
    {Looks for macro s (must be given in upper case) in the macrosymbolstack, }
    {and returns it if found. Returns nil otherwise.}
    function  search_macro(const s : string):tsym;

{*** Object Helpers ***}
    procedure search_class_overloads(aprocsym : tprocsym);
    function search_default_property(pd : tobjectdef) : tpropertysym;

{*** Macro Helpers ***}
    {If called initially, the following procedures manipulate macros in }
    {initialmacrotable, otherwise they manipulate system macros local to a module.}
    {Name can be given in any case (it will be converted to upper case).}
    procedure def_system_macro(const name : string);
    procedure set_system_macro(const name, value : string);
    procedure set_system_compvar(const name, value : string);
    procedure undef_system_macro(const name : string);

{*** symtable stack ***}
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
      { symtable }
      symutil,defcmp,
      { module }
      fmodule,
      { codegen }
      procinfo
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
            ibundefineddef : hp:=tundefineddef.ppuload(ppufile);
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
         { now read the symbols }
         repeat
           b:=ppufile.readentry;
           case b of
                ibtypesym : sym:=ttypesym.ppuload(ppufile);
                ibprocsym : sym:=tprocsym.ppuload(ppufile);
               ibconstsym : sym:=tconstsym.ppuload(ppufile);
           ibglobalvarsym : sym:=tglobalvarsym.ppuload(ppufile);
            iblocalvarsym : sym:=tlocalvarsym.ppuload(ppufile);
             ibparavarsym : sym:=tparavarsym.ppuload(ppufile);
            ibfieldvarsym : sym:=tfieldvarsym.ppuload(ppufile);
         ibabsolutevarsym : sym:=tabsolutevarsym.ppuload(ppufile);
                ibenumsym : sym:=tenumsym.ppuload(ppufile);
          ibtypedconstsym : sym:=ttypedconstsym.ppuload(ppufile);
            ibpropertysym : sym:=tpropertysym.ppuload(ppufile);
                ibunitsym : sym:=tunitsym.ppuload(ppufile);
               iblabelsym : sym:=tlabelsym.ppuload(ppufile);
                 ibsyssym : sym:=tsyssym.ppuload(ppufile);
                ibrttisym : sym:=trttisym.ppuload(ppufile);
               ibmacrosym : sym:=tmacro.ppuload(ppufile);
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
        pd : Tstoredsym;
      begin
         { each definition get a number, write then the amount of syms and the
           datasize to the ibsymdef entry }
         ppufile.putlongint(symindex.count);
         ppufile.writeentry(ibstartsyms);
         { foreach is used to write all symbols }
         pd:=Tstoredsym(symindex.first);
         while assigned(pd) do
           begin
              pd.ppuwrite(ppufile);
              pd:=Tstoredsym(pd.indexnext);
           end;
         { end of symbols }
         ppufile.writeentry(ibendsyms);
      end;


    procedure tstoredsymtable.load_references(ppufile:tcompilerppufile;locals:boolean);
      var
        b     : byte;
        d     : tderef;
        sym   : Tsym;
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
                 ppufile.getderef(d);
                 sym:=Tsym(d.resolve);
                 if assigned(sym) then
                   sym.load_references(ppufile,locals);
               end;
             ibdefref :
               begin
                 ppufile.getderef(d);
                 prdef:=tstoreddef(d.resolve);
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
        pd : Tsym;
      begin
         ppufile.writeentry(ibbeginsymtablebrowser);
         { write all symbols }
         pd:=Tsym(symindex.first);
         while assigned(pd) do
           begin
              pd.write_references(ppufile,locals);
              pd:=Tsym(pd.indexnext);
           end;
         ppufile.writeentry(ibendsymtablebrowser);
      end;


    procedure tstoredsymtable.buildderef;
      var
        hp : tdef;
        hs : tsym;
      begin
        { interface definitions }
        hp:=tdef(defindex.first);
        while assigned(hp) do
         begin
           hp.buildderef;
           hp:=tdef(hp.indexnext);
         end;
        { interface symbols }
        hs:=tsym(symindex.first);
        while assigned(hs) do
         begin
           hs.buildderef;
           hs:=tsym(hs.indexnext);
         end;
      end;


    procedure tstoredsymtable.buildderefimpl;
      var
        hp : tdef;
      begin
        { definitions }
        hp:=tdef(defindex.first);
        while assigned(hp) do
         begin
           hp.buildderefimpl;
           hp:=tdef(hp.indexnext);
         end;
      end;


    procedure tstoredsymtable.deref;
      var
        hp : tdef;
        hs : tsym;
      begin
        { first deref the interface ttype symbols. This is needs
          to be done before the interface defs are derefed, because
          the interface defs can contain references to the type symbols
          which then already need to contain a resolved restype field (PFV) }
        hs:=tsym(symindex.first);
        while assigned(hs) do
         begin
           if hs.typ=typesym then
             hs.deref;
           hs:=tsym(hs.indexnext);
         end;
        { deref the interface definitions }
        hp:=tdef(defindex.first);
        while assigned(hp) do
         begin
           hp.deref;
           hp:=tdef(hp.indexnext);
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
        { definitions }
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
               (sym.typ in [localvarsym,paravarsym,absolutevarsym]) and
               (vo_is_funcret in tabstractvarsym(sym).varoptions) and
               not((m_result in aktmodeswitches) and
                   (vo_is_result in tabstractvarsym(sym).varoptions)) then
             sym.name:='hidden'+sym.name
            else
             DuplicateSym(sym,hsym);
          end;

         { register definition of typesym }
         if (sym.typ = typesym) and
            assigned(ttypesym(sym).restype.def) then
          begin
            if not(assigned(ttypesym(sym).restype.def.owner)) and
               (ttypesym(sym).restype.def.deftype<>errordef) then
              registerdef(ttypesym(sym).restype.def);
          end;

         { insert in index and search hash }
         symindex.insert(sym);
         symsearch.insert(sym);
      end;


    function tstoredsymtable.speedsearch(const s : stringid;speedvalue : cardinal) : tsymentry;
      var
        hp : Tsym;
        newref : tref;
      begin
        hp:=Tsym(inherited speedsearch(s,speedvalue));
        if assigned(hp) then
         begin
           { reject non static members in static procedures }
           if (symtabletype=objectsymtable) and
              not(sp_static in hp.symoptions) and
              allow_only_static then
             Message(sym_e_only_static_in_static);

           { unit uses count }
           if assigned(current_module) and
              (symtabletype=globalsymtable) then
             begin
               if tglobalsymtable(self).moduleid>=current_module.unitmapsize then
                 internalerror(200501152);
               inc(current_module.unitmap[tglobalsymtable(self).moduleid].refs);
             end;

           if make_ref and (cs_browser in aktmoduleswitches) then
             begin
                newref:=tref.create(hp.lastref,@akttokenpos);
                { for symbols that are in tables without browser info or syssyms }
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
           if make_ref then
               inc(hp.refs);
         end; { value was not found }
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
         { because each object has to have a type sym,
           only test objects declarations, not type renamings }
         else
          if (tsym(sym).typ=typesym) and
             assigned(ttypesym(sym).restype.def) and
             (ttypesym(sym).restype.def.typesym=ttypesym(sym)) and
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


    procedure TStoredSymtable.varsymbolused(p : TNamedIndexItem;arg:pointer);
      begin
         if (tsym(p).typ in [globalvarsym,localvarsym,paravarsym,fieldvarsym]) and
            ((tsym(p).owner.symtabletype in
             [parasymtable,localsymtable,objectsymtable,staticsymtable])) then
          begin
           { unused symbol should be reported only if no }
           { error is reported                     }
           { if the symbol is in a register it is used   }
           { also don't count the value parameters which have local copies }
           { also don't claim for high param of open parameters (PM) }
           if (Errorcount<>0) or
              ([vo_is_hidden_para,vo_is_funcret] * tabstractvarsym(p).varoptions = [vo_is_hidden_para]) then
             exit;
           if (tstoredsym(p).refs=0) then
             begin
                if (vo_is_funcret in tabstractvarsym(p).varoptions) then
                  begin
                    { don't warn about the result of constructors }
                    if (tsym(p).owner.symtabletype<>localsymtable) or
                       (tprocdef(tsym(p).owner.defowner).proctypeoption<>potype_constructor) then
                      MessagePos(tsym(p).fileinfo,sym_w_function_result_not_set)
                  end
                else if (tsym(p).owner.symtabletype=parasymtable) then
                  MessagePos1(tsym(p).fileinfo,sym_h_para_identifier_not_used,tsym(p).realname)
                else if (tsym(p).owner.symtabletype=objectsymtable) then
                  MessagePos2(tsym(p).fileinfo,sym_n_private_identifier_not_used,tsym(p).owner.realname^,tsym(p).realname)
                else
                  MessagePos1(tsym(p).fileinfo,sym_n_local_identifier_not_used,tsym(p).realname);
             end
           else if tabstractvarsym(p).varstate in [vs_written,vs_initialised] then
             begin
                if (tsym(p).owner.symtabletype=parasymtable) then
                  begin
                    if not(tabstractvarsym(p).varspez in [vs_var,vs_out]) and
                       not(vo_is_funcret in tabstractvarsym(p).varoptions) then
                      MessagePos1(tsym(p).fileinfo,sym_h_para_identifier_only_set,tsym(p).realname)
                  end
                else if (tsym(p).owner.symtabletype=objectsymtable) then
                  MessagePos2(tsym(p).fileinfo,sym_n_private_identifier_only_set,tsym(p).owner.realname^,tsym(p).realname)
                else if not(vo_is_exported in tabstractvarsym(p).varoptions) and
                        not(vo_is_funcret in tabstractvarsym(p).varoptions) then
                  MessagePos1(tsym(p).fileinfo,sym_n_local_identifier_only_set,tsym(p).realname);
             end;
         end
      else if ((tsym(p).owner.symtabletype in
              [objectsymtable,parasymtable,localsymtable,staticsymtable])) then
          begin
           if (Errorcount<>0) or
              (sp_internal in tsym(p).symoptions) then
             exit;
           { do not claim for inherited private fields !! }
           if (Tsym(p).refs=0) and (tsym(p).owner.symtabletype=objectsymtable) then
             MessagePos2(tsym(p).fileinfo,sym_n_private_method_not_used,tsym(p).owner.realname^,tsym(p).realname)
           { units references are problematic }
           else
            begin
              if (Tsym(p).refs=0) and
                 not(tsym(p).typ in [enumsym,unitsym]) and
                 not(is_funcret_sym(tsym(p))) and
                 (
                  (tsym(p).typ<>procsym) or
                  ((tsym(p).owner.symtabletype=staticsymtable) and
                   not current_module.is_unit)
                 ) then
                MessagePos2(tsym(p).fileinfo,sym_h_local_symbol_not_used,SymTypeName[tsym(p).typ],tsym(p).realname);
            end;
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
           tobjectdef(ttypesym(p).restype.def).symtable.foreach(@TestPrivate,nil);
      end;


    procedure tstoredsymtable.unchain_overloads(p : TNamedIndexItem;arg:pointer);
      begin
         if tsym(p).typ=procsym then
           tprocsym(p).unchain_overload;
      end;


    procedure Tstoredsymtable.reset_def(def:Tnamedindexitem;arg:pointer);
      begin
        Tstoreddef(def).reset;
      end;


{***********************************************
           Process all entries
***********************************************}

    procedure Tstoredsymtable.reset_all_defs;
      begin
        defindex.foreach(@reset_def,nil);
      end;


    { checks, if all procsyms and methods are defined }
    procedure tstoredsymtable.check_forwards;
      begin
         foreach(@check_forward,nil);
      end;


    procedure tstoredsymtable.checklabels;
      begin
         foreach(@labeldefined,nil);
      end;


    procedure tstoredsymtable.allsymbolsused;
      begin
         foreach(@varsymbolused,nil);
      end;


    procedure tstoredsymtable.allprivatesused;
      begin
         foreach(@objectprivatesymbolused,nil);
      end;


    procedure tstoredsymtable.unchain_overloaded;
      begin
         foreach(@unchain_overloads,nil);
      end;


    procedure TStoredSymtable._needs_init_final(p : tnamedindexitem;arg:pointer);
      begin
         if b_needs_init_final then
          exit;
         case tsym(p).typ of
           fieldvarsym,
           globalvarsym,
           localvarsym,
           paravarsym :
             begin
               if not(is_class(tabstractvarsym(p).vartype.def)) and
                  tstoreddef(tabstractvarsym(p).vartype.def).needs_inittable then
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
         foreach(@_needs_init_final,nil);
         needs_init_final:=b_needs_init_final;
      end;


{****************************************************************************
                          TAbstractRecordSymtable
****************************************************************************}

    constructor tabstractrecordsymtable.create(const n:string;usealign:shortint);
      begin
        inherited create(n);
        datasize:=0;
        recordalignment:=1;
        usefieldalignment:=usealign;
        padalignment:=1;
        { recordalign -1 means C record packing, that starts
          with an alignment of 1 }
        if usealign=-1 then
          fieldalignment:=1
        else
          fieldalignment:=usealign;
      end;


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


    procedure tabstractrecordsymtable.insertfield(sym : tfieldvarsym;addsym:boolean);
      var
        l      : aint;
        varalignrecord,
        varalignfield,
        varalign : longint;
        vardef : tdef;
      begin
        if addsym then
          insert(sym);
        { this symbol can't be loaded to a register }
        sym.varregable:=vr_none;
        { Calculate field offset }
        l:=sym.getsize;
        vardef:=sym.vartype.def;
        varalign:=vardef.alignment;
        { Calc the alignment size for C style records }
        if (usefieldalignment=-1) then
         begin
           if (varalign>4) and
              ((varalign mod 4)<>0) and
              (vardef.deftype=arraydef) then
             Message1(sym_w_wrong_C_pack,vardef.typename);
           if varalign=0 then
             varalign:=l;
           if (fieldalignment<aktalignment.maxCrecordalign) then
            begin
              if (varalign>16) and (fieldalignment<32) then
               fieldalignment:=32
              else if (varalign>12) and (fieldalignment<16) then
               fieldalignment:=16
              { 12 is needed for long double }
              else if (varalign>8) and (fieldalignment<12) then
               fieldalignment:=12
              else if (varalign>4) and (fieldalignment<8) then
               fieldalignment:=8
              else if (varalign>2) and (fieldalignment<4) then
               fieldalignment:=4
              else if (varalign>1) and (fieldalignment<2) then
               fieldalignment:=2;
              { darwin/x86 aligns long doubles on 16 bytes }
              if (target_info.system = system_i386_darwin) and
                 (fieldalignment = 12) then
                fieldalignment := 16;
            end;
           fieldalignment:=min(fieldalignment,aktalignment.maxCrecordalign);
         end;
        if varalign=0 then
          varalign:=size_2_align(l);
        varalignfield:=used_align(varalign,aktalignment.recordalignmin,fieldalignment);
        sym.fieldoffset:=align(datasize,varalignfield);
        if (aword(l)+sym.fieldoffset)>high(aint) then
          begin
            Message(sym_e_segment_too_large);
            datasize:=high(aint);
          end
        else
          datasize:=sym.fieldoffset+l;
        { Calc alignment needed for this record }
        if (usefieldalignment=-1) then
          varalignrecord:=used_align(varalign,aktalignment.recordalignmin,aktalignment.maxCrecordalign)
        else
          if (usefieldalignment=0) then
            varalignrecord:=used_align(varalign,aktalignment.recordalignmin,aktalignment.recordalignmax)
        else
          begin
            { packrecords is set explicit, ignore recordalignmax limit }
            varalignrecord:=used_align(varalign,aktalignment.recordalignmin,usefieldalignment);
          end;
        recordalignment:=max(recordalignment,varalignrecord);
      end;


    procedure tabstractrecordsymtable.addalignmentpadding;
      begin
        { make the record size aligned correctly so it can be
          used as elements in an array. For C records we
          use the fieldalignment, because that is updated with the
          used alignment. }
        if (padalignment = 1) then
          if usefieldalignment=-1 then
            padalignment:=fieldalignment
          else
            padalignment:=recordalignment;
        datasize:=align(datasize,padalignment);
      end;


{****************************************************************************
                              TRecordSymtable
****************************************************************************}

    constructor trecordsymtable.create(usealign:shortint);
      begin
        inherited create('',usealign);
        symtabletype:=recordsymtable;
      end;


   { this procedure is reserved for inserting case variant into
      a record symtable }
    { the offset is the location of the start of the variant
      and datasize and dataalignment corresponds to
      the complete size (see code in pdecl unit) PM }
    procedure trecordsymtable.insertunionst(unionst : trecordsymtable;offset : longint);
      var
        ps,nps : tfieldvarsym;
        pd,npd : tdef;
        varalignrecord,varalign,
        storesize,storealign : longint;
      begin
        storesize:=datasize;
        storealign:=fieldalignment;
        datasize:=offset;
        ps:=tfieldvarsym(unionst.symindex.first);
        while assigned(ps) do
          begin
            nps:=tfieldvarsym(ps.indexnext);
            { remove from current symtable }
            unionst.symindex.deleteindex(ps);
            ps.left:=nil;
            ps.right:=nil;
            { add to this record }
            ps.owner:=self;
            datasize:=ps.fieldoffset+offset;
            symindex.insert(ps);
            symsearch.insert(ps);
            { update address }
            ps.fieldoffset:=datasize;

            { update alignment of this record }
            varalign:=ps.vartype.def.alignment;
            if varalign=0 then
              varalign:=size_2_align(ps.getsize);
            varalignrecord:=used_align(varalign,aktalignment.recordalignmin,fieldalignment);
            recordalignment:=max(recordalignment,varalignrecord);

            { next }
            ps:=nps;
          end;
        pd:=tdef(unionst.defindex.first);
        while assigned(pd) do
          begin
            npd:=tdef(pd.indexnext);
            unionst.defindex.deleteindex(pd);
            pd.left:=nil;
            pd.right:=nil;
            registerdef(pd);
            pd:=npd;
          end;
        datasize:=storesize;
        fieldalignment:=storealign;
      end;


{****************************************************************************
                              TObjectSymtable
****************************************************************************}

    constructor tobjectsymtable.create(const n:string;usealign:shortint);
      begin
        inherited create(n,usealign);
        symtabletype:=objectsymtable;
      end;


    procedure tobjectsymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
         { check for duplicate field id in inherited classes }
         if (sym.typ=fieldvarsym) and
            assigned(defowner) and
            (
             not(m_delphi in aktmodeswitches) or
             is_object(tdef(defowner))
            ) then
           begin
              { but private ids can be reused }
              hsym:=search_class_member(tobjectdef(defowner),sym.name);
              if assigned(hsym) and
                 tsym(hsym).is_visible_for_object(tobjectdef(defowner),tobjectdef(defowner)) then
                DuplicateSym(sym,hsym);
           end;
         inherited insert(sym);
      end;


{****************************************************************************
                          TAbstractLocalSymtable
****************************************************************************}

   procedure tabstractlocalsymtable.ppuwrite(ppufile:tcompilerppufile);
      var
        oldtyp : byte;
      begin
         oldtyp:=ppufile.entrytyp;
         ppufile.entrytyp:=subentryid;

         { write definitions }
         writedefs(ppufile);
         { write symbols }
         writesyms(ppufile);

         ppufile.entrytyp:=oldtyp;
      end;


{****************************************************************************
                              TLocalSymtable
****************************************************************************}

    constructor tlocalsymtable.create(level:byte);
      begin
        inherited create('');
        symtabletype:=localsymtable;
        symtablelevel:=level;
      end;


    procedure tlocalsymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
        { need to hide function result? }
        hsym:=tsym(search(sym.name));
        if assigned(hsym) then
          begin
            { a local and the function can have the same
              name in TP and Delphi, but RESULT not }
            if (m_duplicate_names in aktmodeswitches) and
               (hsym.typ in [absolutevarsym,localvarsym]) and
               (vo_is_funcret in tabstractvarsym(hsym).varoptions) and
               not((m_result in aktmodeswitches) and
                   (vo_is_result in tabstractvarsym(hsym).varoptions)) then
              HideSym(hsym)
            else
              DuplicateSym(sym,hsym);
          end;

        if assigned(next) and
           (next.symtabletype=parasymtable) then
          begin
            { check para symtable }
            hsym:=tsym(next.search(sym.name));
            if assigned(hsym) then
              begin
                { a local and the function can have the same
                  name in TP and Delphi, but RESULT not }
                if (m_duplicate_names in aktmodeswitches) and
                   (sym.typ in [absolutevarsym,paravarsym]) and
                   (vo_is_funcret in tabstractvarsym(sym).varoptions) and
                   not((m_result in aktmodeswitches) and
                       (vo_is_result in tabstractvarsym(sym).varoptions)) then
                  sym.name:='hidden'+sym.name
                else
                  DuplicateSym(sym,hsym);
              end;
            { check for duplicate id in local symtable of methods }
            if assigned(next.next) and
               { funcretsym is allowed !! }
               (not is_funcret_sym(sym)) and
               (next.next.symtabletype=objectsymtable) then
             begin
               hsym:=search_class_member(tobjectdef(next.next.defowner),sym.name);
               if assigned(hsym) and
                 { private ids can be reused }
                  (hsym.is_visible_for_object(tobjectdef(next.next.defowner),tobjectdef(next.next.defowner)) or
                   (hsym.owner.defowner.owner.symtabletype<>globalsymtable)) then
                begin
                  { delphi allows to reuse the names in a class, but not
                    in object (tp7 compatible) }
                  if not((m_delphi in aktmodeswitches) and
                         is_class(tdef(next.next.defowner))) then
                    DuplicateSym(sym,hsym);
                end;
             end;
          end;

         inherited insert(sym);
      end;


{****************************************************************************
                              TParaSymtable
****************************************************************************}

    constructor tparasymtable.create(level:byte);
      begin
        inherited create('');
        symtabletype:=parasymtable;
        symtablelevel:=level;
      end;


    procedure tparasymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
         { check for duplicate id in para symtable of methods }
         if assigned(next) and
            (next.symtabletype=objectsymtable) and
            { funcretsym is allowed }
            (not is_funcret_sym(sym)) then
           begin
              hsym:=search_class_member(tobjectdef(next.defowner),sym.name);
              { private ids can be reused }
              if assigned(hsym) and
                 Tsym(hsym).is_visible_for_object(tobjectdef(next.defowner),tobjectdef(next.defowner)) then
               begin
                 { delphi allows to reuse the names in a class, but not
                   in object (tp7 compatible) }
                 if not((m_delphi in aktmodeswitches) and
                        is_class_or_interface(tobjectdef(next.defowner))) then
                   DuplicateSym(sym,hsym);
               end;
           end;

         inherited insert(sym);
      end;


{****************************************************************************
                         TAbstractUnitSymtable
****************************************************************************}

    constructor tabstractunitsymtable.create(const n : string;id:word);
      begin
        inherited create(n);
        moduleid:=id;
        symsearch.usehash;
      end;


    function tabstractunitsymtable.iscurrentunit:boolean;
      begin
        result:=assigned(current_module) and
                (
                 (current_module.globalsymtable=self) or
                 (current_module.localsymtable=self)
                );
      end;


{****************************************************************************
                              TStaticSymtable
****************************************************************************}

    constructor tstaticsymtable.create(const n : string;id:word);
      begin
        inherited create(n,id);
        symtabletype:=staticsymtable;
        symtablelevel:=main_program_level;
      end;


    procedure tstaticsymtable.ppuload(ppufile:tcompilerppufile);
      begin
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
        inherited ppuwrite(ppufile);
      end;


    procedure tstaticsymtable.load_references(ppufile:tcompilerppufile;locals:boolean);
      begin
        inherited load_references(ppufile,locals);
      end;


    procedure tstaticsymtable.write_references(ppufile:tcompilerppufile;locals:boolean);
      begin
        inherited write_references(ppufile,locals);
      end;


    procedure tstaticsymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
         { also check the global symtable }
         if assigned(next) and
            (next.symtabletype=globalsymtable) and
            (next.iscurrentunit) then
          begin
            hsym:=tsym(next.search(sym.name));
            if assigned(hsym) then
             begin
               { Delphi you can have a symbol with the same name as the
                 unit, the unit can then not be accessed anymore using
                 <unit>.<id>, so we can hide the symbol }
               if (m_duplicate_names in aktmodeswitches) and
                  (hsym.typ=symconst.unitsym) then
                HideSym(hsym)
               else
                DuplicateSym(sym,hsym);
             end;
          end;

         inherited insert(sym);
      end;


{****************************************************************************
                              TGlobalSymtable
****************************************************************************}

    constructor tglobalsymtable.create(const n : string;id:word);
      begin
         inherited create(n,id);
         symtabletype:=globalsymtable;
         symtablelevel:=main_program_level;
      end;


    procedure tglobalsymtable.ppuload(ppufile:tcompilerppufile);
      begin
         next:=symtablestack;
         symtablestack:=self;

         inherited ppuload(ppufile);

         { now we can deref the syms and defs }
         deref;

         { restore symtablestack }
         symtablestack:=next;
      end;


    procedure tglobalsymtable.ppuwrite(ppufile:tcompilerppufile);
      begin
        { write the symtable entries }
        inherited ppuwrite(ppufile);
      end;


    procedure tglobalsymtable.load_references(ppufile:tcompilerppufile;locals:boolean);
      begin
        inherited load_references(ppufile,locals);
      end;


    procedure tglobalsymtable.write_references(ppufile:tcompilerppufile;locals:boolean);
      begin
        inherited write_references(ppufile,locals);
      end;


    procedure tglobalsymtable.insert(sym:tsymentry);
      var
         hsym : tsym;
      begin
         hsym:=tsym(search(sym.name));
         if assigned(hsym) then
          begin
            { Delphi you can have a symbol with the same name as the
              unit, the unit can then not be accessed anymore using
              <unit>.<id>, so we can hide the symbol }
            if (m_duplicate_names in aktmodeswitches) and
               (hsym.typ=symconst.unitsym) then
             HideSym(hsym)
            else
             DuplicateSym(sym,hsym);
          end;

         inherited insert(sym);
      end;


{****************************************************************************
                              TWITHSYMTABLE
****************************************************************************}

    constructor twithsymtable.create(aowner:tdef;asymsearch:TDictionary;refnode:pointer{tnode});
      begin
         inherited create('');
         symtabletype:=withsymtable;
         withrefnode:=refnode;
         { we don't need the symsearch }
         symsearch.free;
         { set the defaults }
         symsearch:=asymsearch;
         defowner:=aowner;
      end;


    destructor twithsymtable.destroy;
      begin
        tobject(withrefnode).free;
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


{****************************************************************************
                          TMacroSymtable
****************************************************************************}

    constructor tmacrosymtable.create(exported: boolean);
      begin
        inherited create('');
        if exported then
          symtabletype:=exportedmacrosymtable
        else
          symtabletype:=localmacrosymtable;
        symtablelevel:=main_program_level;
      end;


    procedure tmacrosymtable.ppuload(ppufile:tcompilerppufile);
      begin
        next:=macrosymtablestack;
        macrosymtablestack:=self;

        inherited ppuload(ppufile);

        { restore symtablestack }
        macrosymtablestack:=next;
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


    function FullTypeName(def,otherdef:tdef):string;
      var
        s1,s2 : string;
      begin
        s1:=def.typename;
        { When the names are the same try to include the unit name }
        if assigned(otherdef) and
           (def.owner.symtabletype in [globalsymtable,staticsymtable]) then
          begin
            s2:=otherdef.typename;
            if upper(s1)=upper(s2) then
              s1:=def.owner.realname^+'.'+s1;
          end;
        FullTypeName:=s1;
      end;


    procedure incompatibletypes(def1,def2:tdef);
      begin
        { When there is an errordef there is already an error message show }
        if (def2.deftype=errordef) or
           (def1.deftype=errordef) then
          exit;
        CGMessage2(type_e_incompatible_types,FullTypeName(def1,def2),FullTypeName(def2,def1));
      end;


    procedure hidesym(sym:tsymentry);
      var
        s : string;
      begin
        if assigned(sym.owner) then
          sym.owner.rename(sym.name,'hidden'+sym.name)
        else
          sym.name:='hidden'+sym.name;
        s:='hidden'+tsym(sym).realname;
        stringdispose(tsym(sym)._realname);
        tsym(sym)._realname:=stringdup(s);
      end;


      var
        dupnr : longint; { unique number for duplicate symbols }

    procedure duplicatesym(dupsym,sym:tsymentry);
      var
        st : tsymtable;
      begin
        Message1(sym_e_duplicate_id,tsym(sym).realname);
        st:=findunitsymtable(sym.owner);
        with tsym(sym).fileinfo do
          begin
            if assigned(st) and
               (st.symtabletype=globalsymtable) and
               (not st.iscurrentunit) then
              Message2(sym_h_duplicate_id_where,'unit '+st.name^,tostr(line))
            else
              Message2(sym_h_duplicate_id_where,current_module.sourcefiles.get_file_name(fileindex),tostr(line));
          end;
        { Rename duplicate sym to an unreachable name, but it can be
          inserted in the symtable without errors }
        if assigned(dupsym) then
          begin
            inc(dupnr);
            dupsym.name:='dup'+tostr(dupnr)+dupsym.name;
          end;
      end;


{*****************************************************************************
                                  Search
*****************************************************************************}

    function  searchsym(const s : stringid;var srsym:tsym;var srsymtable:tsymtable):boolean;
      var
        speedvalue : cardinal;
        topclass   : tobjectdef;
        context : tobjectdef;
      begin
         speedvalue:=getspeedvalue(s);
         srsymtable:=symtablestack;
         while assigned(srsymtable) do
           begin
             srsym:=tsym(srsymtable.speedsearch(s,speedvalue));
             if assigned(srsym) then
               begin
                 topclass:=nil;
                 { use the class from withsymtable only when it is
                   defined in this unit }
                 if (srsymtable.symtabletype=withsymtable) and
                    assigned(srsymtable.defowner) and
                    (srsymtable.defowner.deftype=objectdef) and
                    (srsymtable.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
                    (srsymtable.defowner.owner.iscurrentunit) then
                   topclass:=tobjectdef(srsymtable.defowner)
                 else
                   begin
                     if assigned(current_procinfo) then
                       topclass:=current_procinfo.procdef._class;
                   end;
                 if assigned(current_procinfo) then
                   context:=current_procinfo.procdef._class
                 else
                   context:=nil;
                 if tsym(srsym).is_visible_for_object(topclass,context) then
                   begin
                     { we need to know if a procedure references symbols
                       in the static symtable, because then it can't be
                       inlined from outside this unit }
                     if assigned(current_procinfo) and
                        (srsym.owner.symtabletype=staticsymtable) then
                       include(current_procinfo.flags,pi_uses_static_symtable);
                     searchsym:=true;
                     exit;
                   end;
               end;
             srsymtable:=srsymtable.next;
           end;
         searchsym:=false;
      end;


    function  searchsym_type(const s : stringid;var srsym:tsym;var srsymtable:tsymtable):boolean;
      var
        speedvalue : cardinal;
      begin
         speedvalue:=getspeedvalue(s);
         srsymtable:=symtablestack;
         while assigned(srsymtable) do
           begin
              {
                It is not possible to have type defintions in:
                  records
                  objects
                  parameters
              }
              if not(srsymtable.symtabletype in [recordsymtable,objectsymtable,parasymtable]) or
                 (assigned(srsymtable.defowner) and
                  (
                   (df_generic in tdef(srsymtable.defowner).defoptions) or
                   (df_specialization in tdef(srsymtable.defowner).defoptions))
                  ) then
                begin
                  srsym:=tsym(srsymtable.speedsearch(s,speedvalue));
                  if assigned(srsym) and
                     (not assigned(current_procinfo) or
                      tsym(srsym).is_visible_for_object(current_procinfo.procdef._class,current_procinfo.procdef._class)) then
                    begin
                      result:=true;
                      exit;
                    end
                end;
              srsymtable:=srsymtable.next;
           end;
         result:=false;
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
              if (p.symtabletype=globalsymtable) and
                 (p.iscurrentunit) then
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


    function searchsym_in_class(classh:tobjectdef;const s : stringid):tsym;
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
         if assigned(classh) and
            (classh.owner.symtabletype in [globalsymtable,staticsymtable]) and
            classh.owner.iscurrentunit then
           topclassh:=classh
         else
           begin
             if assigned(current_procinfo) then
               topclassh:=current_procinfo.procdef._class
             else
               topclassh:=nil;
           end;
         sym:=nil;
         while assigned(classh) do
          begin
            sym:=tsym(classh.symtable.speedsearch(s,speedvalue));
            if assigned(sym) and
               tsym(sym).is_visible_for_object(topclassh,current_procinfo.procdef._class) then
              break
            else
              sym:=nil;
            classh:=classh.childof;
          end;
         searchsym_in_class:=sym;
      end;


    function searchsym_in_class_by_msgint(classh:tobjectdef;i:longint):tsym;
      var
        topclassh  : tobjectdef;
        def        : tdef;
        sym        : tsym;
      begin
         { when the class passed is defined in this unit we
           need to use the scope of that class. This is a trick
           that can be used to access protected members in other
           units. At least kylix supports it this way (PFV) }
         if assigned(classh) and
            (classh.owner.symtabletype in [globalsymtable,staticsymtable]) and
            classh.owner.iscurrentunit then
           topclassh:=classh
         else
           begin
             if assigned(current_procinfo) then
               topclassh:=current_procinfo.procdef._class
             else
               topclassh:=nil;
           end;
         sym:=nil;
         def:=nil;
         while assigned(classh) do
          begin
            def:=tdef(classh.symtable.defindex.first);
            while assigned(def) do
             begin
               if (def.deftype=procdef) and
                  (po_msgint in tprocdef(def).procoptions) and
                  (tprocdef(def).messageinf.i=i) then
                begin
                  sym:=tprocdef(def).procsym;
                  if assigned(topclassh) then
                   begin
                     if tprocdef(def).is_visible_for_object(topclassh) then
                      break;
                   end
                  else
                   break;
                end;
               def:=tdef(def.indexnext);
             end;
            if assigned(sym) then
             break;
            classh:=classh.childof;
          end;
         searchsym_in_class_by_msgint:=sym;
      end;


    function searchsym_in_class_by_msgstr(classh:tobjectdef;const s:string):tsym;
      var
        topclassh  : tobjectdef;
        def        : tdef;
        sym        : tsym;
      begin
         { when the class passed is defined in this unit we
           need to use the scope of that class. This is a trick
           that can be used to access protected members in other
           units. At least kylix supports it this way (PFV) }
         if assigned(classh) and
            (classh.owner.symtabletype in [globalsymtable,staticsymtable]) and
            classh.owner.iscurrentunit then
           topclassh:=classh
         else
           begin
             if assigned(current_procinfo) then
               topclassh:=current_procinfo.procdef._class
             else
               topclassh:=nil;
           end;
         sym:=nil;
         def:=nil;
         while assigned(classh) do
          begin
            def:=tdef(classh.symtable.defindex.first);
            while assigned(def) do
             begin
               if (def.deftype=procdef) and
                  (po_msgstr in tprocdef(def).procoptions) and
                  (tprocdef(def).messageinf.str=s) then
                begin
                  sym:=tprocdef(def).procsym;
                  if assigned(topclassh) then
                   begin
                     if tprocdef(def).is_visible_for_object(topclassh) then
                      break;
                   end
                  else
                   break;
                end;
               def:=tdef(def.indexnext);
             end;
            if assigned(sym) then
             break;
            classh:=classh.childof;
          end;
         searchsym_in_class_by_msgstr:=sym;
      end;


    function search_assignment_operator(from_def,to_def:Tdef):Tprocdef;

    var st:Tsymtable;
        sym:Tprocsym;
        sv:cardinal;
        curreq,
        besteq : tequaltype;
        currpd,
        bestpd : tprocdef;
    begin
      st:=symtablestack;
      sv:=getspeedvalue('assign');
      besteq:=te_incompatible;
      bestpd:=nil;
      while st<>nil do
        begin
          sym:=Tprocsym(st.speedsearch('assign',sv));
          if sym<>nil then
            begin
              if sym.typ<>procsym then
                internalerror(200402031);
              { if the source type is an alias then this is only the second choice,
                if you mess with this code, check tw4093 }
              currpd:=sym.search_procdef_assignment_operator(from_def,to_def,curreq);
              if curreq>besteq then
                begin
                  besteq:=curreq;
                  bestpd:=currpd;
                  if (besteq=te_exact) then
                    break;
                end;
            end;
          st:=st.next;
        end;
      result:=bestpd;
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


{$ifdef notused}
    function searchsysvar(const s: stringid; var srsym: tsym; var symowner: tsymtable): boolean;
      begin
        if not(cs_compilesystem in aktmoduleswitches) then
          begin
            srsym := searchsymonlyin(systemunit,s);
            symowner := systemunit;
          end
        else
          searchsym(s,tsym(srsym),symowner);
        searchsysvar :=
          assigned(srsym) and
          (srsym.typ = globalvarsym);
      end;
{$endif notused}


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

    function search_macro(const s : string):tsym;
      var
        p : tsymtable;
        speedvalue : cardinal;
        srsym      : tsym;

      begin
        speedvalue:= getspeedvalue(s);
        p:=macrosymtablestack;
        while assigned(p) do
          begin
             srsym:=tsym(p.speedsearch(s,speedvalue));
             if assigned(srsym) then
               begin
                 search_macro:= srsym;
                 exit;
               end;
             p:=p.next;
          end;
        search_macro:= nil;
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

    procedure search_class_overloads(aprocsym : tprocsym);
    { searches n in symtable of pd and all anchestors }
      var
        speedvalue : cardinal;
        srsym      : tprocsym;
        s          : string;
        objdef     : tobjectdef;
      begin
        if aprocsym.overloadchecked then
         exit;
        aprocsym.overloadchecked:=true;
        if (aprocsym.owner.symtabletype<>objectsymtable) then
         internalerror(200111021);
        objdef:=tobjectdef(aprocsym.owner.defowner);
        { we start in the parent }
        if not assigned(objdef.childof) then
         exit;
        objdef:=objdef.childof;
        s:=aprocsym.name;
        speedvalue:=getspeedvalue(s);
        while assigned(objdef) do
         begin
           srsym:=tprocsym(objdef.symtable.speedsearch(s,speedvalue));
           if assigned(srsym) then
            begin
              if (srsym.typ<>procsym) then
               internalerror(200111022);
              if srsym.is_visible_for_object(tobjectdef(aprocsym.owner.defowner),tobjectdef(aprocsym.owner.defowner)) then
               begin
                 srsym.add_para_match_to(Aprocsym,[cpo_ignorehidden,cpo_allowdefaults]);
                 { we can stop if the overloads were already added
                  for the found symbol }
                 if srsym.overloadchecked then
                  break;
               end;
            end;
           { next parent }
           objdef:=objdef.childof;
         end;
      end;


   procedure tstoredsymtable.testfordefaultproperty(p : TNamedIndexItem;arg:pointer);
     begin
        if (tsym(p).typ=propertysym) and
           (ppo_defaultproperty in tpropertysym(p).propoptions) then
          ppointer(arg)^:=p;
     end;


   function search_default_property(pd : tobjectdef) : tpropertysym;
   { returns the default property of a class, searches also anchestors }
     var
       _defaultprop : tpropertysym;
     begin
        _defaultprop:=nil;
        while assigned(pd) do
          begin
             pd.symtable.foreach(@tstoredsymtable(pd.symtable).testfordefaultproperty,@_defaultprop);
             if assigned(_defaultprop) then
               break;
             pd:=pd.childof;
          end;
        search_default_property:=_defaultprop;
     end;

{****************************************************************************
                              Macro Helpers
****************************************************************************}
{NOTE: Initially, macrosymtablestack contains initialmacrosymtable.}

    procedure def_system_macro(const name : string);
      var
        mac : tmacro;
        s: string;
      begin
         if name = '' then
           internalerror(2004121201);
         s:= upper(name);
         mac:=tmacro(search_macro(s));
         if not assigned(mac) then
           begin
             mac:=tmacro.create(s);
             if macrosymtablestack.symtabletype=localmacrosymtable then
               macrosymtablestack.insert(mac)
             else
               macrosymtablestack.next.insert(mac)
           end;
         if not mac.defined then
           Message1(parser_c_macro_defined,mac.name);
         mac.defined:=true;
      end;

    procedure set_system_macro(const name, value : string);
      var
        mac : tmacro;
        s: string;
      begin
        if name = '' then
          internalerror(2004121201);
         s:= upper(name);
         mac:=tmacro(search_macro(s));
         if not assigned(mac) then
           begin
             mac:=tmacro.create(s);
             if macrosymtablestack.symtabletype=localmacrosymtable then
               macrosymtablestack.insert(mac)
             else
               macrosymtablestack.next.insert(mac)
           end
         else
           begin
             mac.is_compiler_var:=false;
             if assigned(mac.buftext) then
               freemem(mac.buftext,mac.buflen);
           end;
         Message2(parser_c_macro_set_to,mac.name,value);
         mac.buflen:=length(value);
         getmem(mac.buftext,mac.buflen);
         move(value[1],mac.buftext^,mac.buflen);
         mac.defined:=true;
      end;

    procedure set_system_compvar(const name, value : string);
      var
        mac : tmacro;
        s: string;
      begin
        if name = '' then
          internalerror(2004121201);
         s:= upper(name);
         mac:=tmacro(search_macro(s));
         if not assigned(mac) then
           begin
             mac:=tmacro.create(s);
             mac.is_compiler_var:=true;
             if macrosymtablestack.symtabletype=localmacrosymtable then
               macrosymtablestack.insert(mac)
             else
               macrosymtablestack.next.insert(mac)
           end
         else
           begin
             mac.is_compiler_var:=true;
             if assigned(mac.buftext) then
               freemem(mac.buftext,mac.buflen);
           end;
         Message2(parser_c_macro_set_to,mac.name,value);
         mac.buflen:=length(value);
         getmem(mac.buftext,mac.buflen);
         move(value[1],mac.buftext^,mac.buflen);
         mac.defined:=true;
      end;

    procedure undef_system_macro(const name : string);
      var
        mac : tmacro;
        s: string;
      begin
         if name = '' then
           internalerror(2004121201);
         s:= upper(name);
         mac:=tmacro(search_macro(s));
         if not assigned(mac) then
           {If not found, then it's already undefined.}
         else
           begin
             if mac.defined then
               Message1(parser_c_macro_undefined,mac.name);
             mac.defined:=false;
             mac.is_compiler_var:=false;
             { delete old definition }
             if assigned(mac.buftext) then
               begin
                  freemem(mac.buftext,mac.buflen);
                  mac.buftext:=nil;
               end;
           end;
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
     begin
       { Reset symbolstack }
       registerdef:=false;
       symtablestack:=nil;
       macrosymtablestack:=nil;
       systemunit:=nil;
       { create error syms and def }
       generrorsym:=terrorsym.create;
       generrortype.setdef(terrordef.create);
{$ifdef UNITALIASES}
       { unit aliases }
       unitaliases:=tdictionary.create;
{$endif}
       initialmacrosymtable:= tmacrosymtable.create(false);
       macrosymtablestack:= initialmacrosymtable;

       { set some global vars to nil, might be important for the ide }
       class_tobject:=nil;
       interface_iunknown:=nil;
       rec_tguid:=nil;

       dupnr:=0;
     end;


   procedure DoneSymtable;
      begin
        generrorsym.free;
        generrortype.def.free;
{$ifdef UNITALIASES}
        unitaliases.free;
{$endif}
        initialmacrosymtable.Free;
     end;

end.
