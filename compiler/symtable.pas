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
       aasmtai,aasmdata
       ;


{****************************************************************************
                             Symtable types
****************************************************************************}

    type
       tstoredsymtable = class(TSymtable)
       private
          b_needs_init_final : boolean;
          procedure _needs_init_final(sym:TObject;arg:pointer);
          procedure check_forward(sym:TObject;arg:pointer);
          procedure labeldefined(sym:TObject;arg:pointer);
          procedure varsymbolused(sym:TObject;arg:pointer);
          procedure TestPrivate(sym:TObject;arg:pointer);
          procedure objectprivatesymbolused(sym:TObject;arg:pointer);
          procedure unchain_overloads(sym:TObject;arg:pointer);
          procedure loaddefs(ppufile:tcompilerppufile);
          procedure loadsyms(ppufile:tcompilerppufile);
          procedure writedefs(ppufile:tcompilerppufile);
          procedure writesyms(ppufile:tcompilerppufile);
       public
          { load/write }
          procedure ppuload(ppufile:tcompilerppufile);virtual;
          procedure ppuwrite(ppufile:tcompilerppufile);virtual;
          procedure buildderef;virtual;
          procedure buildderefimpl;virtual;
          procedure deref;virtual;
          procedure derefimpl;virtual;
          function  checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;override;
          procedure reset_all_defs;virtual;
          procedure allsymbolsused;
          procedure allprivatesused;
          procedure check_forwards;
          procedure checklabels;
          function  needs_init_final : boolean;
          procedure unchain_overloaded;
          procedure testfordefaultproperty(sym:TObject;arg:pointer);
       end;

       tabstractrecordsymtable = class(tstoredsymtable)
       public
          usefieldalignment,     { alignment to use for fields (PACKRECORDS value), C_alignment is C style }
          recordalignment,       { alignment desired when inserting this record }
          fieldalignment,        { alignment current alignment used when fields are inserted }
          padalignment : shortint;   { size to a multiple of which the symtable has to be rounded up }
          constructor create(const n:string;usealign:shortint);
          procedure ppuload(ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure addfield(sym:tfieldvarsym);
          procedure insertfield(sym:tfieldvarsym);
          procedure addalignmentpadding;
          procedure insertdef(def:TDefEntry);override;
          function is_packed: boolean;
        protected
          procedure setdatasize(val: aint);
          _datasize       : aint;
          { size in bits of the data in case of bitpacked record. Only important during construction, }
          { no need to save in/restore from ppu file. datasize is always (databitsize+7) div 8.       }
          databitsize    : aint;
          { bitpacked? -> all fieldvarsym offsets are in bits instead of bytes }
        public
          property datasize : aint read _datasize write setdatasize;
       end;

       trecordsymtable = class(tabstractrecordsymtable)
       public
          constructor create(usealign:shortint);
          procedure insertunionst(unionst : trecordsymtable;offset : longint);
       end;

       tObjectSymtable = class(tabstractrecordsymtable)
       public
          constructor create(adefowner:tdef;const n:string;usealign:shortint);
          function  checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;override;
       end;

       { tabstractlocalsymtable }

       tabstractlocalsymtable = class(tstoredsymtable)
       public
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function count_locals:longint;
       end;

       tlocalsymtable = class(tabstractlocalsymtable)
       public
          constructor create(adefowner:tdef;level:byte);
          function  checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;override;
       end;

       tparasymtable = class(tabstractlocalsymtable)
       public
          constructor create(adefowner:tdef;level:byte);
          function  checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;override;
       end;

       tabstractuniTSymtable = class(tstoredsymtable)
       public
          constructor create(const n : string;id:word);
          function iscurrentunit:boolean;override;
       end;

       tglobalsymtable = class(tabstractuniTSymtable)
       public
          unittypecount : word;
          constructor create(const n : string;id:word);
          procedure ppuload(ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;override;
       end;

       tstaticsymtable = class(tabstractuniTSymtable)
       public
          constructor create(const n : string;id:word);
          procedure ppuload(ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function  checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;override;
       end;

       twithsymtable = class(TSymtable)
          withrefnode : tobject; { tnode }
          constructor create(aowner:tdef;ASymList:TFPHashObjectList;refnode:tobject{tnode});
          destructor  destroy;override;
          procedure clear;override;
          procedure insertdef(def:TDefEntry);override;
        end;

       tstt_excepTSymtable = class(TSymtable)
       public
          constructor create;
       end;

       tmacrosymtable = class(tstoredsymtable)
       public
          constructor create(exported: boolean);
       end;

    var
       systemunit     : tglobalsymtable; { pointer to the system unit }


{****************************************************************************
                             Functions
****************************************************************************}

{*** Misc ***}
    function  FullTypeName(def,otherdef:tdef):string;
    procedure incompatibletypes(def1,def2:tdef);
    procedure hidesym(sym:TSymEntry);
    procedure duplicatesym(var hashedid:THashedIDString;dupsym,origsym:TSymEntry);

{*** Search ***}
    procedure addsymref(sym:tsym);
    function  searchsym(const s : TIDString;out srsym:tsym;out srsymtable:TSymtable):boolean;
    function  searchsym_type(const s : TIDString;out srsym:tsym;out srsymtable:TSymtable):boolean;
    function  searchsym_in_module(pm:pointer;const s : TIDString;out srsym:tsym;out srsymtable:TSymtable):boolean;
    function  searchsym_in_class(classh,contextclassh:tobjectdef;const s : TIDString;out srsym:tsym;out srsymtable:TSymtable):boolean;
    function  searchsym_in_class_by_msgint(classh:tobjectdef;msgid:longint;out srdef : tdef;out srsym:tsym;out srsymtable:TSymtable):boolean;
    function  searchsym_in_class_by_msgstr(classh:tobjectdef;const s:string;out srsym:tsym;out srsymtable:TSymtable):boolean;
    function  search_system_type(const s: TIDString): ttypesym;
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
{ $ifdef DEBUG
    procedure test_symtablestack;
    procedure list_symtablestack;
 $endif DEBUG}

{$ifdef UNITALIASES}
    type
       punit_alias = ^tunit_alias;
       tunit_alias = object(TNamedIndexItem)
          newname : pshortstring;
          constructor init(const n:string);
          destructor  done;virtual;
       end;
    var
       unitaliases : pdictionary;

    procedure addunitalias(const n:string);
    function getunitalias(const n:string):string;
{$endif UNITALIASES}

{*** Init / Done ***}
    procedure IniTSymtable;
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
      symutil,defcmp,defutil,
      { module }
      fmodule,
      { codegen }
      procinfo
      ;


    var
      dupnr : longint; { unique number for duplicate symbols }


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
        def : tdef;
        b   : byte;
      begin
         { load start of definition section, which holds the amount of defs }
         if ppufile.readentry<>ibstartdefs then
           Message(unit_f_ppu_read_error);
         { read definitions }
         repeat
           b:=ppufile.readentry;
           case b of
             ibpointerdef : def:=tpointerdef.ppuload(ppufile);
             ibarraydef : def:=tarraydef.ppuload(ppufile);
             iborddef : def:=torddef.ppuload(ppufile);
             ibfloatdef : def:=tfloatdef.ppuload(ppufile);
             ibprocdef : def:=tprocdef.ppuload(ppufile);
             ibshortstringdef : def:=tstringdef.loadshort(ppufile);
             iblongstringdef : def:=tstringdef.loadlong(ppufile);
             ibansistringdef : def:=tstringdef.loadansi(ppufile);
             ibwidestringdef : def:=tstringdef.loadwide(ppufile);
             ibunicodestringdef : def:=tstringdef.loadunicode(ppufile);
             ibrecorddef : def:=trecorddef.ppuload(ppufile);
             ibobjectdef : def:=tobjectdef.ppuload(ppufile);
             ibenumdef : def:=tenumdef.ppuload(ppufile);
             ibsetdef : def:=tsetdef.ppuload(ppufile);
             ibprocvardef : def:=tprocvardef.ppuload(ppufile);
             ibfiledef : def:=tfiledef.ppuload(ppufile);
             ibclassrefdef : def:=tclassrefdef.ppuload(ppufile);
             ibformaldef : def:=tformaldef.ppuload(ppufile);
             ibvariantdef : def:=tvariantdef.ppuload(ppufile);
             ibundefineddef : def:=tundefineddef.ppuload(ppufile);
             ibenddefs : break;
             ibend : Message(unit_f_ppu_read_error);
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
           InsertDef(def);
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
         { now read the symbols }
         repeat
           b:=ppufile.readentry;
           case b of
                ibtypesym : sym:=ttypesym.ppuload(ppufile);
                ibprocsym : sym:=tprocsym.ppuload(ppufile);
               ibconstsym : sym:=tconstsym.ppuload(ppufile);
           ibstaticvarsym : sym:=tstaticvarsym.ppuload(ppufile);
            iblocalvarsym : sym:=tlocalvarsym.ppuload(ppufile);
             ibparavarsym : sym:=tparavarsym.ppuload(ppufile);
            ibfieldvarsym : sym:=tfieldvarsym.ppuload(ppufile);
         ibabsolutevarsym : sym:=tabsolutevarsym.ppuload(ppufile);
                ibenumsym : sym:=tenumsym.ppuload(ppufile);
            ibpropertysym : sym:=tpropertysym.ppuload(ppufile);
                ibunitsym : sym:=tunitsym.ppuload(ppufile);
               iblabelsym : sym:=tlabelsym.ppuload(ppufile);
                 ibsyssym : sym:=tsyssym.ppuload(ppufile);
               ibmacrosym : sym:=tmacro.ppuload(ppufile);
                ibendsyms : break;
                    ibend : Message(unit_f_ppu_read_error);
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
           Insert(sym,false);
         until false;
      end;


    procedure tstoredsymtable.writedefs(ppufile:tcompilerppufile);
      var
        i   : longint;
        def : tstoreddef;
      begin
        { each definition get a number, write then the amount of defs to the
          ibstartdef entry }
        ppufile.putlongint(DefList.count);
        ppufile.writeentry(ibstartdefs);
        { now write the definition }
        for i:=0 to DefList.Count-1 do
          begin
            def:=tstoreddef(DefList[i]);
            def.ppuwrite(ppufile);
          end;
        { write end of definitions }
        ppufile.writeentry(ibenddefs);
      end;


    procedure tstoredsymtable.writesyms(ppufile:tcompilerppufile);
      var
        i   : longint;
        sym : Tstoredsym;
      begin
        { each definition get a number, write then the amount of syms and the
          datasize to the ibsymdef entry }
        ppufile.putlongint(SymList.count);
        ppufile.writeentry(ibstartsyms);
        { foreach is used to write all symbols }
        for i:=0 to SymList.Count-1 do
          begin
            sym:=tstoredsym(SymList[i]);
            sym.ppuwrite(ppufile);
          end;
        { end of symbols }
        ppufile.writeentry(ibendsyms);
      end;


    procedure tstoredsymtable.buildderef;
      var
        i   : longint;
        def : tstoreddef;
        sym : tstoredsym;
      begin
        { interface definitions }
        for i:=0 to DefList.Count-1 do
          begin
            def:=tstoreddef(DefList[i]);
            def.buildderef;
          end;
        { interface symbols }
        for i:=0 to SymList.Count-1 do
          begin
            sym:=tstoredsym(SymList[i]);
            sym.buildderef;
          end;
      end;


    procedure tstoredsymtable.buildderefimpl;
      var
        i   : longint;
        def : tstoreddef;
      begin
        { implementation definitions }
        for i:=0 to DefList.Count-1 do
          begin
            def:=tstoreddef(DefList[i]);
            def.buildderefimpl;
          end;
      end;


    procedure tstoredsymtable.deref;
      var
        i   : longint;
        def : tstoreddef;
        sym : tstoredsym;
      begin
        { first deref the interface ttype symbols. This is needs
          to be done before the interface defs are derefed, because
          the interface defs can contain references to the type symbols
          which then already need to contain a resolved typedef field (PFV) }
        for i:=0 to SymList.Count-1 do
          begin
            sym:=tstoredsym(SymList[i]);
            if sym.typ=typesym then
              sym.deref;
          end;
        { interface definitions }
        for i:=0 to DefList.Count-1 do
          begin
            def:=tstoreddef(DefList[i]);
            def.deref;
          end;
        { interface symbols }
        for i:=0 to SymList.Count-1 do
          begin
            sym:=tstoredsym(SymList[i]);
            if sym.typ<>typesym then
              sym.deref;
          end;
      end;


    procedure tstoredsymtable.derefimpl;
      var
        i   : longint;
        def : tstoreddef;
      begin
        { implementation definitions }
        for i:=0 to DefList.Count-1 do
          begin
            def:=tstoreddef(DefList[i]);
            def.derefimpl;
          end;
      end;


    function tstoredsymtable.checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;
      var
        hsym : tsym;
      begin
        hsym:=tsym(FindWithHash(hashedid));
        if assigned(hsym) then
          DuplicateSym(hashedid,sym,hsym);
        result:=assigned(hsym);
      end;


{**************************************
             Callbacks
**************************************}

    procedure TStoredSymtable.check_forward(sym:TObject;arg:pointer);
      begin
         if tsym(sym).typ=procsym then
           tprocsym(sym).check_forward
         { check also object method table            }
         { we needn't to test the def list          }
         { because each object has to have a type sym,
           only test objects declarations, not type renamings }
         else
          if (tsym(sym).typ=typesym) and
             assigned(ttypesym(sym).typedef) and
             (ttypesym(sym).typedef.typesym=ttypesym(sym)) and
             (ttypesym(sym).typedef.typ=objectdef) then
           tobjectdef(ttypesym(sym).typedef).check_forwards;
      end;


    procedure TStoredSymtable.labeldefined(sym:TObject;arg:pointer);
      begin
        if (tsym(sym).typ=labelsym) and
           not(tlabelsym(sym).defined) then
         begin
           if tlabelsym(sym).used then
            Message1(sym_e_label_used_and_not_defined,tlabelsym(sym).realname)
           else
            Message1(sym_w_label_not_defined,tlabelsym(sym).realname);
         end;
      end;


    procedure TStoredSymtable.varsymbolused(sym:TObject;arg:pointer);
      begin
         if (tsym(sym).typ in [staticvarsym,localvarsym,paravarsym,fieldvarsym]) and
            ((tsym(sym).owner.symtabletype in
             [parasymtable,localsymtable,ObjectSymtable,staticsymtable])) then
          begin
           { unused symbol should be reported only if no }
           { error is reported                     }
           { if the symbol is in a register it is used   }
           { also don't count the value parameters which have local copies }
           { also don't claim for high param of open parameters (PM) }
           if (Errorcount<>0) or
              ([vo_is_hidden_para,vo_is_funcret] * tabstractvarsym(sym).varoptions = [vo_is_hidden_para]) then
             exit;
           if (tstoredsym(sym).refs=0) then
             begin
                if (vo_is_funcret in tabstractvarsym(sym).varoptions) then
                  begin
                    { don't warn about the result of constructors }
                    if ((tsym(sym).owner.symtabletype<>localsymtable) or
                       (tprocdef(tsym(sym).owner.defowner).proctypeoption<>potype_constructor)) and
                       not(cs_opt_nodedfa in current_settings.optimizerswitches) then
                      MessagePos(tsym(sym).fileinfo,sym_w_function_result_not_set)
                  end
                else if (tsym(sym).owner.symtabletype=parasymtable) then
                  MessagePos1(tsym(sym).fileinfo,sym_h_para_identifier_not_used,tsym(sym).realname)
                else if (tsym(sym).owner.symtabletype=ObjectSymtable) then
                  MessagePos2(tsym(sym).fileinfo,sym_n_private_identifier_not_used,tsym(sym).owner.realname^,tsym(sym).realname)
                else
                  MessagePos1(tsym(sym).fileinfo,sym_n_local_identifier_not_used,tsym(sym).realname);
             end
           else if tabstractvarsym(sym).varstate in [vs_written,vs_initialised] then
             begin
                if (tsym(sym).owner.symtabletype=parasymtable) then
                  begin
                    if not(tabstractvarsym(sym).varspez in [vs_var,vs_out]) and
                       not(vo_is_funcret in tabstractvarsym(sym).varoptions) then
                      MessagePos1(tsym(sym).fileinfo,sym_h_para_identifier_only_set,tsym(sym).realname)
                  end
                else if (tsym(sym).owner.symtabletype=ObjectSymtable) then
                  MessagePos2(tsym(sym).fileinfo,sym_n_private_identifier_only_set,tsym(sym).owner.realname^,tsym(sym).realname)
                else if tabstractvarsym(sym).varoptions*[vo_is_funcret,vo_is_public,vo_is_external]=[] then
                  MessagePos1(tsym(sym).fileinfo,sym_n_local_identifier_only_set,tsym(sym).realname);
             end
           else if (tabstractvarsym(sym).varstate = vs_read_not_warned) and
                   ([vo_is_public,vo_is_external] * tabstractvarsym(sym).varoptions = []) then
             MessagePos1(tsym(sym).fileinfo,sym_w_identifier_only_read,tsym(sym).realname)
         end
      else if ((tsym(sym).owner.symtabletype in
              [ObjectSymtable,parasymtable,localsymtable,staticsymtable])) then
          begin
           if (Errorcount<>0) or
              (sp_internal in tsym(sym).symoptions) then
             exit;
           { do not claim for inherited private fields !! }
           if (Tsym(sym).refs=0) and (tsym(sym).owner.symtabletype=ObjectSymtable) then
             MessagePos2(tsym(sym).fileinfo,sym_n_private_method_not_used,tsym(sym).owner.realname^,tsym(sym).realname)
           { units references are problematic }
           else
            begin
              if (Tsym(sym).refs=0) and
                 not(tsym(sym).typ in [enumsym,unitsym]) and
                 not(is_funcret_sym(tsym(sym))) and
                 (
                  (tsym(sym).typ<>procsym) or
                  ((tsym(sym).owner.symtabletype=staticsymtable) and
                   not current_module.is_unit)
                 ) then
                MessagePos2(tsym(sym).fileinfo,sym_h_local_symbol_not_used,SymTypeName[tsym(sym).typ],tsym(sym).realname);
            end;
          end;
      end;


    procedure TStoredSymtable.TestPrivate(sym:TObject;arg:pointer);
      begin
        if sp_private in tsym(sym).symoptions then
          varsymbolused(sym,arg);
      end;


    procedure TStoredSymtable.objectprivatesymbolused(sym:TObject;arg:pointer);
      begin
         {
           Don't test simple object aliases PM
         }
         if (tsym(sym).typ=typesym) and
            (ttypesym(sym).typedef.typ=objectdef) and
            (ttypesym(sym).typedef.typesym=tsym(sym)) then
           tobjectdef(ttypesym(sym).typedef).symtable.SymList.ForEachCall(@TestPrivate,nil);
      end;


    procedure tstoredsymtable.unchain_overloads(sym:TObject;arg:pointer);
      begin
         if tsym(sym).typ=procsym then
           tprocsym(sym).unchain_overload;
      end;


{***********************************************
           Process all entries
***********************************************}

    procedure Tstoredsymtable.reset_all_defs;
      var
        i   : longint;
        def : tstoreddef;
      begin
        for i:=0 to DefList.Count-1 do
          begin
            def:=tstoreddef(DefList[i]);
            def.reset;
          end;
      end;


    { checks, if all procsyms and methods are defined }
    procedure tstoredsymtable.check_forwards;
      begin
         SymList.ForEachCall(@check_forward,nil);
      end;


    procedure tstoredsymtable.checklabels;
      begin
         SymList.ForEachCall(@labeldefined,nil);
      end;


    procedure tstoredsymtable.allsymbolsused;
      begin
         SymList.ForEachCall(@varsymbolused,nil);
      end;


    procedure tstoredsymtable.allprivatesused;
      begin
         SymList.ForEachCall(@objectprivatesymbolused,nil);
      end;


    procedure tstoredsymtable.unchain_overloaded;
      begin
         SymList.ForEachCall(@unchain_overloads,nil);
      end;


    procedure TStoredSymtable._needs_init_final(sym:TObject;arg:pointer);
      begin
         if b_needs_init_final then
          exit;
         case tsym(sym).typ of
           fieldvarsym,
           staticvarsym,
           localvarsym,
           paravarsym :
             begin
               if not(is_class(tabstractvarsym(sym).vardef)) and
                  tstoreddef(tabstractvarsym(sym).vardef).needs_inittable then
                 b_needs_init_final:=true;
             end;
         end;
      end;


    { returns true, if p contains data which needs init/final code }
    function tstoredsymtable.needs_init_final : boolean;
      begin
         b_needs_init_final:=false;
         SymList.ForEachCall(@_needs_init_final,nil);
         needs_init_final:=b_needs_init_final;
      end;


{****************************************************************************
                          TAbstractRecordSymtable
****************************************************************************}

    constructor tabstractrecordsymtable.create(const n:string;usealign:shortint);
      begin
        inherited create(n);
        _datasize:=0;
        databitsize:=0;
        recordalignment:=1;
        usefieldalignment:=usealign;
        padalignment:=1;
        { recordalign C_alignment means C record packing, that starts
          with an alignment of 1 }
        case usealign of
          C_alignment,
          bit_alignment:
            fieldalignment:=1
          else
            fieldalignment:=usealign;
        end;
      end;


    procedure tabstractrecordsymtable.ppuload(ppufile:tcompilerppufile);
      begin
        inherited ppuload(ppufile);
      end;


    procedure tabstractrecordsymtable.ppuwrite(ppufile:tcompilerppufile);
      var
        oldtyp : byte;
      begin
         oldtyp:=ppufile.entrytyp;
         ppufile.entrytyp:=subentryid;

         inherited ppuwrite(ppufile);

         ppufile.entrytyp:=oldtyp;
      end;


    function field2recordalignment(fieldoffs, fieldalign: aint): aint;
      begin
        { optimal alignment of the record when declaring a variable of this }
        { type is independent of the packrecords setting                    }
        if (fieldoffs mod fieldalign) = 0 then
          result:=fieldalign
        else if (fieldalign >= 16) and
                ((fieldoffs mod 16) = 0) and
                ((fieldalign mod 16) = 0) then
          result:=16
        else if (fieldalign >= 8) and
                ((fieldoffs mod 8) = 0) and
                ((fieldalign mod 8) = 0) then
          result:=8
        else if (fieldalign >= 4) and
                ((fieldoffs mod 4) = 0) and
                ((fieldalign mod 4) = 0) then
          result:=4
        else if (fieldalign >= 2) and
                ((fieldoffs mod 2) = 0) and
                ((fieldalign mod 2) = 0) then
          result:=2
        else
          result:=1;
      end;


    procedure tabstractrecordsymtable.addfield(sym:tfieldvarsym);
      var
        l      : aint;
        varalignrecord,
        varalignfield,
        varalign : shortint;
        vardef : tdef;
      begin
        if (sym.owner<>self) then
          internalerror(200602031);
        if sym.fieldoffset<>-1 then
          internalerror(200602032);
        { this symbol can't be loaded to a register }
        sym.varregable:=vr_none;
        { Calculate field offset }
        l:=sym.getsize;
        vardef:=sym.vardef;
        varalign:=vardef.alignment;

        if (usefieldalignment=bit_alignment) then
          begin
            { bitpacking only happens for ordinals, the rest is aligned at }
            { 1 byte (compatible with GPC/GCC)                             }
            if is_ordinal(vardef) then
              begin
                sym.fieldoffset:=databitsize;
                l:=sym.getpackedbitsize;
              end
            else
              begin
                databitsize:=_datasize*8;
                sym.fieldoffset:=databitsize;
                if (l>high(aint) div 8) then
                  Message(sym_e_segment_too_large);
                l:=l*8;
              end;
            if varalign=0 then
              varalign:=size_2_align(l);
            recordalignment:=max(recordalignment,field2recordalignment(databitsize mod 8,varalign));
            { bit packed records are limited to high(aint) bits }
            { instead of bytes to avoid double precision        }
            { arithmetic in offset calculations                 }
            if int64(l)>high(aint)-sym.fieldoffset then
              begin
                Message(sym_e_segment_too_large);
                _datasize:=high(aint);
                databitsize:=high(aint);
              end
            else
              begin
                databitsize:=sym.fieldoffset+l;
                _datasize:=(databitsize+7) div 8;
              end;
            { rest is not applicable }
            exit;
          end;
        { Calc the alignment size for C style records }
        if (usefieldalignment=C_alignment) then
          begin
            if (varalign>4) and
              ((varalign mod 4)<>0) and
              (vardef.typ=arraydef) then
              Message1(sym_w_wrong_C_pack,vardef.typename);
            if varalign=0 then
              varalign:=l;
            if (fieldalignment<current_settings.alignment.maxCrecordalign) then
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
              end;
            fieldalignment:=min(fieldalignment,current_settings.alignment.maxCrecordalign);
          end;
        if varalign=0 then
          varalign:=size_2_align(l);
        varalignfield:=used_align(varalign,current_settings.alignment.recordalignmin,fieldalignment);

        sym.fieldoffset:=align(_datasize,varalignfield);
        if l>high(aint)-sym.fieldoffset then
          begin
            Message(sym_e_segment_too_large);
            _datasize:=high(aint);
          end
        else
          _datasize:=sym.fieldoffset+l;
        { Calc alignment needed for this record }
        if (usefieldalignment=C_alignment) then
          varalignrecord:=used_align(varalign,current_settings.alignment.recordalignmin,current_settings.alignment.maxCrecordalign)
        else
          varalignrecord:=field2recordalignment(sym.fieldoffset,varalign);
        recordalignment:=max(recordalignment,varalignrecord);
      end;


    procedure tabstractrecordsymtable.insertfield(sym:tfieldvarsym);
      begin
        insert(sym);
        addfield(sym);
      end;


    procedure tabstractrecordsymtable.addalignmentpadding;
      begin
        { make the record size aligned correctly so it can be
          used as elements in an array. For C records we
          use the fieldalignment, because that is updated with the
          used alignment. }
        if (padalignment = 1) then
          case usefieldalignment of
            C_alignment:
              padalignment:=fieldalignment;
            { bitpacked }
            bit_alignment:
              padalignment:=1;
            { default/no packrecords specified }
            0:
              padalignment:=recordalignment
            { specific packrecords setting -> use as upper limit }
            else
              padalignment:=min(recordalignment,usefieldalignment);
          end;
        _datasize:=align(_datasize,padalignment);
      end;


    procedure tabstractrecordsymtable.insertdef(def:TDefEntry);
      begin
        { Enums must also be available outside the record scope,
          insert in the owner of this symtable }
        if def.typ=enumdef then
          defowner.owner.insertdef(def)
        else
          inherited insertdef(def);
      end;


    function tabstractrecordsymtable.is_packed: boolean;
      begin
        result:=usefieldalignment=bit_alignment;
      end;


    procedure tabstractrecordsymtable.setdatasize(val: aint);
      begin
        _datasize:=val;
        if (usefieldalignment=bit_alignment) then
          { can overflow in non bitpacked records }
          databitsize:=val*8;
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
        sym : tsym;
        def : tdef;
        i : integer;
        varalignrecord,varalign,
        storesize,storealign : aint;
        bitsize: aint;
      begin
        storesize:=_datasize;
        storealign:=fieldalignment;
        _datasize:=offset;
        if (usefieldalignment=bit_alignment) then
          databitsize:=offset*8;

        { We move the ownership of the defs and symbols to the new recordsymtable.
          The old unionsymtable keeps the references, but doesn't own the
          objects anymore }
        unionst.DefList.OwnsObjects:=false;
        unionst.SymList.OwnsObjects:=false;

        { copy symbols }
        for i:=0 to unionst.SymList.Count-1 do
          begin
            sym:=TSym(unionst.SymList[i]);
            if sym.typ<>fieldvarsym then
              internalerror(200601272);
            { add to this record symtable }
//            unionst.SymList.List.List^[i].Data:=nil;
            sym.ChangeOwner(self);
            varalign:=tfieldvarsym(sym).vardef.alignment;
            if varalign=0 then
              varalign:=size_2_align(tfieldvarsym(sym).getsize);
            { retrieve size }
            if (usefieldalignment=bit_alignment) then
              begin
                { bit packed records are limited to high(aint) bits }
                { instead of bytes to avoid double precision        }
                { arithmetic in offset calculations                 }
                if is_ordinal(tfieldvarsym(sym).vardef) then
                  bitsize:=tfieldvarsym(sym).getpackedbitsize
                else
                  begin
                    bitsize:=tfieldvarsym(sym).getsize;
                    if (bitsize>high(aint) div 8) then
                      Message(sym_e_segment_too_large);
                    bitsize:=bitsize*8;
                  end;
                if bitsize>high(aint)-databitsize then
                  begin
                    Message(sym_e_segment_too_large);
                    _datasize:=high(aint);
                    databitsize:=high(aint);
                  end
                else
                  begin
                    databitsize:=tfieldvarsym(sym).fieldoffset+offset*8;
                    _datasize:=(databitsize+7) div 8;
                  end;
                tfieldvarsym(sym).fieldoffset:=databitsize;
              varalignrecord:=field2recordalignment(tfieldvarsym(sym).fieldoffset div 8,varalign);
              end
            else
              begin
                if tfieldvarsym(sym).getsize>high(aint)-_datasize then
                  begin
                    Message(sym_e_segment_too_large);
                    _datasize:=high(aint);
                  end
                else
                  _datasize:=tfieldvarsym(sym).fieldoffset+offset;
                { update address }
                tfieldvarsym(sym).fieldoffset:=_datasize;
                varalignrecord:=field2recordalignment(tfieldvarsym(sym).fieldoffset,varalign);
              end;
            { update alignment of this record }
            if (usefieldalignment<>C_alignment) then
              recordalignment:=max(recordalignment,varalignrecord);
          end;
        { update alignment for C records }
        if (usefieldalignment=C_alignment) then
          recordalignment:=max(recordalignment,unionst.recordalignment);
        { Register defs in the new record symtable }
        for i:=0 to unionst.DefList.Count-1 do
          begin
            def:=TDef(unionst.DefList[i]);
            def.ChangeOwner(self);
          end;
        _datasize:=storesize;
        fieldalignment:=storealign;
      end;


{****************************************************************************
                              TObjectSymtable
****************************************************************************}

    constructor tObjectSymtable.create(adefowner:tdef;const n:string;usealign:shortint);
      begin
        inherited create(n,usealign);
        symtabletype:=ObjectSymtable;
        defowner:=adefowner;
      end;


    function tObjectSymtable.checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;
      var
         hsym : tsym;
      begin
         result:=false;
         if not assigned(defowner) then
           internalerror(200602061);

         { procsym and propertysym have special code
           to override values in inherited classes. For other
           symbols check for duplicates }
         if not(sym.typ in [procsym,propertysym]) then
           begin
              { but private ids can be reused }
              hsym:=search_class_member(tobjectdef(defowner),hashedid.id);
              if assigned(hsym) and
                 (
                  (not(m_delphi in current_settings.modeswitches) and
                   tsym(hsym).is_visible_for_object(tobjectdef(defowner),tobjectdef(defowner))
                  ) or
                  (
                   { In Delphi, you can repeat members of a parent class. You can't }
                   { do this for objects however, and you (obviouly) can't          }
                   { declare two fields with the same name in a single class        }
                   (m_delphi in current_settings.modeswitches) and
                   (
                    is_object(tdef(defowner)) or
                    (hsym.owner = self)
                   )
                  )
                 ) then
                begin
                  DuplicateSym(hashedid,sym,hsym);
                  result:=true;
                end;
           end
         else
           begin
             if not(m_duplicate_names in current_settings.modeswitches) then
               result:=inherited checkduplicate(hashedid,sym);
           end;
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


    function tabstractlocalsymtable.count_locals:longint;
      var
        i   : longint;
        sym : tsym;
      begin
        result:=0;
        for i:=0 to SymList.Count-1 do
          begin
            sym:=tsym(SymList[i]);
            { Count only varsyms, but ignore the funcretsym }
            if (tsym(sym).typ in [localvarsym,paravarsym]) and
               (tsym(sym)<>current_procinfo.procdef.funcretsym) and
               (not(vo_is_parentfp in tabstractvarsym(sym).varoptions) or
                (tstoredsym(sym).refs>0)) then
              inc(result);
         end;
      end;


{****************************************************************************
                              TLocalSymtable
****************************************************************************}

    constructor tlocalsymtable.create(adefowner:tdef;level:byte);
      begin
        inherited create('');
        defowner:=adefowner;
        symtabletype:=localsymtable;
        symtablelevel:=level;
      end;


    function tlocalsymtable.checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;
      var
        hsym : tsym;
      begin
        if not assigned(defowner) or
           (defowner.typ<>procdef) then
          internalerror(200602042);

        result:=false;
        hsym:=tsym(FindWithHash(hashedid));
        if assigned(hsym) then
          begin
            { a local and the function can have the same
              name in TP and Delphi, but RESULT not }
            if (m_duplicate_names in current_settings.modeswitches) and
               (hsym.typ in [absolutevarsym,localvarsym]) and
               (vo_is_funcret in tabstractvarsym(hsym).varoptions) and
               not((m_result in current_settings.modeswitches) and
                   (vo_is_result in tabstractvarsym(hsym).varoptions)) then
              HideSym(hsym)
            else
              DuplicateSym(hashedid,sym,hsym);
            result:=true;
            exit;
          end;

        { check also parasymtable, this needs to be done here becuase
          of the special situation with the funcret sym that needs to be
          hidden for tp and delphi modes }
        hsym:=tsym(tabstractprocdef(defowner).parast.FindWithHash(hashedid));
        if assigned(hsym) then
          begin
            { a local and the function can have the same
              name in TP and Delphi, but RESULT not }
            if (m_duplicate_names in current_settings.modeswitches) and
               (sym.typ in [absolutevarsym,localvarsym]) and
               (vo_is_funcret in tabstractvarsym(sym).varoptions) and
               not((m_result in current_settings.modeswitches) and
                   (vo_is_result in tabstractvarsym(sym).varoptions)) then
              Hidesym(sym)
            else
              DuplicateSym(hashedid,sym,hsym);
            result:=true;
            exit;
          end;

        { check ObjectSymtable, skip this for funcret sym because
          that will always be positive because it has the same name
          as the procsym }
        if not is_funcret_sym(sym) and
           (defowner.typ=procdef) and
           assigned(tprocdef(defowner)._class) and
           (tprocdef(defowner).owner.defowner=tprocdef(defowner)._class) and
           (
            not(m_delphi in current_settings.modeswitches) or
            is_object(tprocdef(defowner)._class)
           ) then
          result:=tprocdef(defowner)._class.symtable.checkduplicate(hashedid,sym);
      end;


{****************************************************************************
                              TParaSymtable
****************************************************************************}

    constructor tparasymtable.create(adefowner:tdef;level:byte);
      begin
        inherited create('');
        defowner:=adefowner;
        symtabletype:=parasymtable;
        symtablelevel:=level;
      end;


    function tparasymtable.checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;
      begin
        result:=inherited checkduplicate(hashedid,sym);
        if result then
          exit;
        if not(m_duplicate_names in current_settings.modeswitches) and
           (defowner.typ=procdef) and
           assigned(tprocdef(defowner)._class) and
           (tprocdef(defowner).owner.defowner=tprocdef(defowner)._class) and
           (
            not(m_delphi in current_settings.modeswitches) or
            is_object(tprocdef(defowner)._class)
           ) then
          result:=tprocdef(defowner)._class.symtable.checkduplicate(hashedid,sym);
      end;


{****************************************************************************
                         TAbstractUniTSymtable
****************************************************************************}

    constructor tabstractuniTSymtable.create(const n : string;id:word);
      begin
        inherited create(n);
        moduleid:=id;
      end;


    function tabstractuniTSymtable.iscurrentunit:boolean;
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
        inherited ppuload(ppufile);

        { now we can deref the syms and defs }
        deref;
      end;


    procedure tstaticsymtable.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
      end;


    function tstaticsymtable.checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;
      var
        hsym : tsym;
      begin
        result:=false;
        hsym:=tsym(FindWithHash(hashedid));
        if assigned(hsym) then
          begin
            { Delphi (contrary to TP) you can have a symbol with the same name as the
              unit, the unit can then not be accessed anymore using
              <unit>.<id>, so we can hide the symbol }
            if (m_delphi in current_settings.modeswitches) and
               (hsym.typ=symconst.unitsym) then
              HideSym(hsym)
            else
              DuplicateSym(hashedid,sym,hsym);
            result:=true;
            exit;
          end;

        if (current_module.localsymtable=self) and
           assigned(current_module.globalsymtable) then
          result:=tglobalsymtable(current_module.globalsymtable).checkduplicate(hashedid,sym);
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
         inherited ppuload(ppufile);

         { now we can deref the syms and defs }
         deref;
      end;


    procedure tglobalsymtable.ppuwrite(ppufile:tcompilerppufile);
      begin
        { write the symtable entries }
        inherited ppuwrite(ppufile);
      end;


    function tglobalsymtable.checkduplicate(var hashedid:THashedIDString;sym:TSymEntry):boolean;
      var
        hsym : tsym;
      begin
        result:=false;
        hsym:=tsym(FindWithHash(hashedid));
        if assigned(hsym) then
          begin
            { Delphi (contrary to TP) you can have a symbol with the same name as the
              unit, the unit can then not be accessed anymore using
              <unit>.<id>, so we can hide the symbol }
            if (m_delphi in current_settings.modeswitches) and
               (hsym.typ=symconst.unitsym) then
              HideSym(hsym)
            else
              DuplicateSym(hashedid,sym,hsym);
            result:=true;
            exit;
          end;
      end;


{****************************************************************************
                              TWITHSYMTABLE
****************************************************************************}

    constructor twithsymtable.create(aowner:tdef;ASymList:TFPHashObjectList;refnode:tobject{tnode});
      begin
         inherited create('');
         symtabletype:=withsymtable;
         withrefnode:=refnode;
         { Replace SymList with the passed symlist }
         SymList.free;
         SymList:=ASymList;
         defowner:=aowner;
      end;


    destructor twithsymtable.destroy;
      begin
        withrefnode.free;
        { Disable SymList because we don't Own it }
        SymList:=nil;
        inherited destroy;
      end;


    procedure twithsymtable.clear;
      begin
         { remove no entry from a withsymtable as it is only a pointer to the
           recorddef  or objectdef symtable }
      end;


    procedure twithsymtable.insertdef(def:TDefEntry);
      begin
        { Definitions can't be registered in the withsymtable
          because the withsymtable is removed after the with block.
          We can't easily solve it here because the next symtable in the
          stack is not known. }
        internalerror(200602046);
      end;

{****************************************************************************
                          TSTT_ExceptionSymtable
****************************************************************************}

    constructor tstt_excepTSymtable.create;
      begin
        inherited create('');
        symtabletype:=stt_excepTSymtable;
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


{*****************************************************************************
                             Helper Routines
*****************************************************************************}

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
        if (def2.typ=errordef) or
           (def1.typ=errordef) then
          exit;
        CGMessage2(type_e_incompatible_types,FullTypeName(def1,def2),FullTypeName(def2,def1));
      end;


    procedure hidesym(sym:TSymEntry);
      begin
        sym.realname:='$hidden'+sym.realname;
        include(tsym(sym).symoptions,sp_hidden);
      end;


    procedure duplicatesym(var hashedid:THashedIDString;dupsym,origsym:TSymEntry);
      var
        st : TSymtable;
      begin
        Message1(sym_e_duplicate_id,tsym(origsym).realname);
        { Write hint where the original symbol was found }
        st:=finduniTSymtable(origsym.owner);
        with tsym(origsym).fileinfo do
          begin
            if assigned(st) and
               (st.symtabletype=globalsymtable) and
               st.iscurrentunit then
              Message2(sym_h_duplicate_id_where,current_module.sourcefiles.get_file_name(fileindex),tostr(line))
            else if assigned(st.name) then
              Message2(sym_h_duplicate_id_where,'unit '+st.name^,tostr(line));
          end;
        { Rename duplicate sym to an unreachable name, but it can be
          inserted in the symtable without errors }
        inc(dupnr);
        hashedid.id:='dup'+tostr(dupnr)+hashedid.id;
        if assigned(dupsym) then
          include(tsym(dupsym).symoptions,sp_implicitrename);
      end;


{*****************************************************************************
                                  Search
*****************************************************************************}

     procedure addsymref(sym:tsym);
       begin
         { symbol uses count }
         sym.IncRefCount;
         { unit uses count }
         if assigned(current_module) and
            (sym.owner.symtabletype=globalsymtable) then
             begin
               if tglobalsymtable(sym.owner).moduleid>=current_module.unitmapsize then
                 internalerror(200501152);
               inc(current_module.unitmap[tglobalsymtable(sym.owner).moduleid].refs);
             end;
       end;


    function  searchsym(const s : TIDString;out srsym:tsym;out srsymtable:TSymtable):boolean;
      var
        hashedid   : THashedIDString;
        topclass   : tobjectdef;
        context    : tobjectdef;
        stackitem  : psymtablestackitem;
      begin
        result:=false;
        hashedid.id:=s;
        stackitem:=symtablestack.stack;
        while assigned(stackitem) do
          begin
            srsymtable:=stackitem^.symtable;
            srsym:=tsym(srsymtable.FindWithHash(hashedid));
            if assigned(srsym) then
              begin
                topclass:=nil;
                { use the class from withsymtable only when it is
                  defined in this unit }
                if (srsymtable.symtabletype=withsymtable) and
                   assigned(srsymtable.defowner) and
                   (srsymtable.defowner.typ=objectdef) and
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
                    addsymref(srsym);
                    result:=true;
                    exit;
                  end;
              end;
            stackitem:=stackitem^.next;
          end;
        srsym:=nil;
        srsymtable:=nil;
      end;


    function searchsym_type(const s : TIDString;out srsym:tsym;out srsymtable:TSymtable):boolean;
      var
        hashedid  : THashedIDString;
        stackitem : psymtablestackitem;
      begin
        result:=false;
        hashedid.id:=s;
        stackitem:=symtablestack.stack;
        while assigned(stackitem) do
          begin
            {
              It is not possible to have type symbols in:
                records
                objects
                parameters
              Exception are generic definitions and specializations
              that have the parameterized types inserted in the symtable.
            }
            srsymtable:=stackitem^.symtable;
            if not(srsymtable.symtabletype in [recordsymtable,ObjectSymtable,parasymtable]) or
               (assigned(srsymtable.defowner) and
                (
                 (df_generic in tdef(srsymtable.defowner).defoptions) or
                 (df_specialization in tdef(srsymtable.defowner).defoptions))
                ) then
              begin
                srsym:=tsym(srsymtable.FindWithHash(hashedid));
                if assigned(srsym) and
                   not(srsym.typ in [fieldvarsym,paravarsym]) and
                   (not assigned(current_procinfo) or
                    tsym(srsym).is_visible_for_object(current_procinfo.procdef._class,current_procinfo.procdef._class)) then
                  begin
                    { we need to know if a procedure references symbols
                      in the static symtable, because then it can't be
                      inlined from outside this unit }
                    if assigned(current_procinfo) and
                       (srsym.owner.symtabletype=staticsymtable) then
                      include(current_procinfo.flags,pi_uses_static_symtable);
                    addsymref(srsym);
                    result:=true;
                    exit;
                  end;
              end;
            stackitem:=stackitem^.next;
          end;
        result:=false;
        srsym:=nil;
        srsymtable:=nil;
      end;


    function searchsym_in_module(pm:pointer;const s : TIDString;out srsym:tsym;out srsymtable:TSymtable):boolean;
      var
        pmod : tmodule;
      begin
        pmod:=tmodule(pm);
        result:=false;
        if assigned(pmod.globalsymtable) then
          begin
            srsym:=tsym(pmod.globalsymtable.Find(s));
            if assigned(srsym) then
              begin
                srsymtable:=pmod.globalsymtable;
                addsymref(srsym);
                result:=true;
                exit;
              end;
          end;
        { If the module is the current unit we also need
          to search the local symtable }
        if (pmod=current_module) and
           assigned(pmod.localsymtable) then
          begin
            srsym:=tsym(pmod.localsymtable.Find(s));
            if assigned(srsym) then
              begin
                srsymtable:=pmod.localsymtable;
                addsymref(srsym);
                result:=true;
                exit;
              end;
          end;
        srsym:=nil;
        srsymtable:=nil;
      end;


    function searchsym_in_class(classh,contextclassh:tobjectdef;const s : TIDString;out srsym:tsym;out srsymtable:TSymtable):boolean;
      var
        hashedid      : THashedIDString;
        currentclassh : tobjectdef;
      begin
        result:=false;
        hashedid.id:=s;
        if assigned(current_procinfo) and assigned(current_procinfo.procdef) then
          currentclassh:=current_procinfo.procdef._class
        else
          currentclassh:=nil;
        while assigned(classh) do
          begin
            srsymtable:=classh.symtable;
            srsym:=tsym(srsymtable.FindWithHash(hashedid));
            if assigned(srsym) and
               tsym(srsym).is_visible_for_object(contextclassh,currentclassh) then
              begin
                addsymref(srsym);
                result:=true;
                exit;
              end;
            classh:=classh.childof;
          end;
        srsym:=nil;
        srsymtable:=nil;
      end;


    function searchsym_in_class_by_msgint(classh:tobjectdef;msgid:longint;out srdef : tdef;out srsym:tsym;out srsymtable:TSymtable):boolean;
      var
        def : tdef;
        i   : longint;
      begin
        result:=false;
        def:=nil;
        while assigned(classh) do
          begin
            for i:=0 to classh.symtable.DefList.Count-1 do
              begin
                def:=tstoreddef(classh.symtable.DefList[i]);
                { Find also all hidden private methods to
                  be compatible with delphi, see tw6203 (PFV) }
                if (def.typ=procdef) and
                   (po_msgint in tprocdef(def).procoptions) and
                   (tprocdef(def).messageinf.i=msgid) then
                  begin
                    srdef:=def;
                    srsym:=tprocdef(def).procsym;
                    srsymtable:=classh.symtable;
                    addsymref(srsym);
                    result:=true;
                    exit;
                  end;
              end;
            classh:=classh.childof;
          end;
        srdef:=nil;
        srsym:=nil;
        srsymtable:=nil;
      end;


    function searchsym_in_class_by_msgstr(classh:tobjectdef;const s:string;out srsym:tsym;out srsymtable:TSymtable):boolean;
      var
        def : tdef;
        i   : longint;
      begin
        result:=false;
        def:=nil;
        while assigned(classh) do
          begin
            for i:=0 to classh.symtable.DefList.Count-1 do
              begin
                def:=tstoreddef(classh.symtable.DefList[i]);
                { Find also all hidden private methods to
                  be compatible with delphi, see tw6203 (PFV) }
                if (def.typ=procdef) and
                   (po_msgstr in tprocdef(def).procoptions) and
                   (tprocdef(def).messageinf.str^=s) then
                  begin
                    srsym:=tprocdef(def).procsym;
                    srsymtable:=classh.symtable;
                    addsymref(srsym);
                    result:=true;
                    exit;
                  end;
              end;
            classh:=classh.childof;
          end;
        srsym:=nil;
        srsymtable:=nil;
      end;


    function search_assignment_operator(from_def,to_def:Tdef):Tprocdef;
      var
        sym : Tprocsym;
        hashedid : THashedIDString;
        curreq,
        besteq : tequaltype;
        currpd,
        bestpd : tprocdef;
        stackitem : psymtablestackitem;
      begin
        hashedid.id:='assign';
        besteq:=te_incompatible;
        bestpd:=nil;
        stackitem:=symtablestack.stack;
        while assigned(stackitem) do
          begin
            sym:=Tprocsym(stackitem^.symtable.FindWithHash(hashedid));
            if sym<>nil then
              begin
                if sym.typ<>procsym then
                  internalerror(200402031);
                { if the source type is an alias then this is only the second choice,
                  if you mess with this code, check tw4093 }
                currpd:=sym.find_procdef_assignment_operator(from_def,to_def,curreq);
                if curreq>besteq then
                  begin
                    besteq:=curreq;
                    bestpd:=currpd;
                    if (besteq=te_exact) then
                      break;
                  end;
              end;
            stackitem:=stackitem^.next;
          end;
        result:=bestpd;
      end;


    function search_system_type(const s: TIDString): ttypesym;
      var
        sym : tsym;
      begin
        sym:=tsym(systemunit.Find(s));
        if not assigned(sym) or
           (sym.typ<>typesym) then
          cgmessage1(cg_f_unknown_system_type,s);
        result:=ttypesym(sym);
      end;


    function search_class_member(pd : tobjectdef;const s : string):tsym;
    { searches n in symtable of pd and all anchestors }
      var
        hashedid : THashedIDString;
        srsym      : tsym;
      begin
        hashedid.id:=s;
        while assigned(pd) do
         begin
           srsym:=tsym(pd.symtable.FindWithHash(hashedid));
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
        stackitem  : psymtablestackitem;
        hashedid   : THashedIDString;
        srsym      : tsym;
      begin
        hashedid.id:=s;

        { First search the localmacrosymtable before searching the
          global macrosymtables from the units }
        if assigned(current_module) then
          begin
            srsym:=tsym(current_module.localmacrosymtable.FindWithHash(hashedid));
            if assigned(srsym) then
              begin
                result:= srsym;
                exit;
              end;
          end;

        stackitem:=macrosymtablestack.stack;
        while assigned(stackitem) do
          begin
            srsym:=tsym(stackitem^.symtable.FindWithHash(hashedid));
            if assigned(srsym) then
              begin
                result:= srsym;
                exit;
              end;
            stackitem:=stackitem^.next;
          end;
        result:= nil;
      end;


{****************************************************************************
                              Object Helpers
****************************************************************************}

    procedure search_class_overloads(aprocsym : tprocsym);
    { searches n in symtable of pd and all anchestors }
      var
        hashedid : THashedIDString;
        srsym    : tprocsym;
        objdef   : tobjectdef;
      begin
        if aprocsym.overloadchecked then
         exit;
        aprocsym.overloadchecked:=true;
        if (aprocsym.owner.symtabletype<>ObjectSymtable) then
         internalerror(200111021);
        objdef:=tobjectdef(aprocsym.owner.defowner);
        { we start in the parent }
        if not assigned(objdef.childof) then
         exit;
        objdef:=objdef.childof;
        hashedid.id:=aprocsym.name;
        while assigned(objdef) do
         begin
           srsym:=tprocsym(objdef.symtable.FindWithHash(hashedid));
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


   procedure tstoredsymtable.testfordefaultproperty(sym:TObject;arg:pointer);
     begin
        if (tsym(sym).typ=propertysym) and
           (ppo_defaultproperty in tpropertysym(sym).propoptions) then
          ppointer(arg)^:=sym;
     end;


   function search_default_property(pd : tobjectdef) : tpropertysym;
   { returns the default property of a class, searches also anchestors }
     var
       _defaultprop : tpropertysym;
     begin
        _defaultprop:=nil;
        while assigned(pd) do
          begin
             pd.symtable.SymList.ForEachCall(@tstoredsymtable(pd.symtable).testfordefaultproperty,@_defaultprop);
             if assigned(_defaultprop) then
               break;
             pd:=pd.childof;
          end;
        search_default_property:=_defaultprop;
     end;


{****************************************************************************
                              Macro Helpers
****************************************************************************}

    procedure def_system_macro(const name : string);
      var
        mac : tmacro;
        s: string;
      begin
         if name = '' then
           internalerror(2004121202);
         s:= upper(name);
         mac:=tmacro(search_macro(s));
         if not assigned(mac) then
           begin
             mac:=tmacro.create(s);
             if assigned(current_module) then
               current_module.localmacrosymtable.insert(mac)
             else
               initialmacrosymtable.insert(mac);
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
          internalerror(2004121203);
         s:= upper(name);
         mac:=tmacro(search_macro(s));
         if not assigned(mac) then
           begin
             mac:=tmacro.create(s);
             if assigned(current_module) then
               current_module.localmacrosymtable.insert(mac)
             else
               initialmacrosymtable.insert(mac);
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
          internalerror(2004121204);
         s:= upper(name);
         mac:=tmacro(search_macro(s));
         if not assigned(mac) then
           begin
             mac:=tmacro.create(s);
             mac.is_compiler_var:=true;
             if assigned(current_module) then
               current_module.localmacrosymtable.insert(mac)
             else
               initialmacrosymtable.insert(mac);
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
           internalerror(2004121205);
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
        p:=punit_alias(unitaliases^.Find(Upper(n)));
        if assigned(p) then
         getunitalias:=punit_alias(p).newname^
        else
         getunitalias:=n;
      end;
{$endif UNITALIASES}


{****************************************************************************
                           Init/Done Symtable
****************************************************************************}

   procedure IniTSymtable;
     begin
       { Reset symbolstack }
       symtablestack:=nil;
       systemunit:=nil;
       { create error syms and def }
       generrorsym:=terrorsym.create;
       generrordef:=terrordef.create;
       { macros }
       initialmacrosymtable:=tmacrosymtable.create(false);
       macrosymtablestack:=TSymtablestack.create;
       macrosymtablestack.push(initialmacrosymtable);
{$ifdef UNITALIASES}
       { unit aliases }
       unitaliases:=TFPHashObjectList.create;
{$endif}
       { set some global vars to nil, might be important for the ide }
       class_tobject:=nil;
       interface_iunknown:=nil;
       rec_tguid:=nil;
       dupnr:=0;
     end;


   procedure DoneSymtable;
      begin
        generrorsym.owner:=nil;
        generrorsym.free;
        generrordef.owner:=nil;
        generrordef.free;
        initialmacrosymtable.free;
        macrosymtablestack.free;
{$ifdef UNITALIASES}
        unitaliases.free;
{$endif}
     end;

end.
