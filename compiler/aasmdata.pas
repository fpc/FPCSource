{
    Copyright (c) 1998-2006 by Florian Klaempfl

    This unit implements an abstract asmoutput class for all processor types

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
{ @abstract(This unit implements an abstract asm output class for all processor types)
  This unit implements an abstract assembler output class for all processors, these
  are then overridden for each assembler writer to actually write the data in these
  classes to an assembler file.
}

unit aasmdata;

{$i fpcdefs.inc}

interface

    uses
       cutils,cclasses,
       globtype,systems,
       cgbase,
       symtype,
       aasmbase;

    type
      { Type of AsmLists. The order is important for the layout of the
        information in the .o file. The stabs for the types must be defined
        before they can be referenced and therefor they need to be written
        first (PFV) }
      TAsmListType=(
        al_start,
        al_stabs,
        { pure assembler routines }
        al_pure_assembler,
        al_procedures,
        al_globals,
        al_const,
        al_typedconsts,
        al_rotypedconsts,
        al_threadvars,
        al_imports,
        al_exports,
        al_resources,
        al_rtti,
        { all symbols with indirect suffix }
        al_indirectglobals,
        al_dwarf_frame,
        al_dwarf_info,
        al_dwarf_abbrev,
        al_dwarf_line,
        al_dwarf_aranges,
        al_dwarf_ranges,
        al_dwarf_loc,
        al_dwarf_loclists,
        al_picdata,
        al_indirectpicdata,
        al_resourcestrings,
        { Objective-C related sections }
        al_objc_data,
        { keep pool data separate, so we can generate new pool entries
          while emitting other data }
        al_objc_pools,
        al_end
      );
    {$push}{$j-}
    const
      globaldataasmlisttypes: array[1..5] of TAsmListType = (
        al_globals,
        al_const,
        al_typedconsts,
        al_rotypedconsts,
        al_threadvars
      );
    {$pop}

      { Type of constant 'pools'. Mostly for string types, but usable for
        floating point and large set constants, too. }
    type
      TConstPoolType = (
         sp_invalid,
         sp_conststr,
         sp_shortstr,
         sp_longstr,
         sp_ansistr,
         sp_widestr,
         sp_unicodestr,
         sp_objcclassnamerefs,
         sp_varnamerefs,
         sp_objcclassnames,
         sp_objcvarnames,
         sp_objcvartypes,
         sp_objcprotocolrefs,
         sp_varsets,
         sp_floats,
         sp_guids,
         sp_paraloc
      );
      
    const
      AsmListTypeStr : array[TAsmListType] of string[24] =(
        'al_begin',
        'al_stabs',
        'al_pure_assembler',
        'al_procedures',
        'al_globals',
        'al_const',
        'al_typedconsts',
        'al_rotypedconsts',
        'al_threadvars',
        'al_imports',
        'al_exports',
        'al_resources',
        'al_rtti',
        'al_indirectglobals',
        'al_dwarf_frame',
        'al_dwarf_info',
        'al_dwarf_abbrev',
        'al_dwarf_line',
        'al_dwarf_aranges',
        'al_dwarf_ranges',
        'al_dwarf_loc',
        'al_dwarf_loclists',
        'al_picdata',
        'al_indirectpicdata',
        'al_resourcestrings',
        'al_objc_data',
        'al_objc_pools',
        'al_end'
      );

    type
      TAsmList = class(tlinkedlist)
         section_count : longint;
         constructor create;
         function  getlasttaifilepos : pfileposinfo;
         { inserts another List at the begin and make this List empty }
         procedure insertList(p : TLinkedList); override;
         { inserts another List before the provided item and make this List empty }
         procedure insertListBefore(Item:TLinkedListItem;p : TLinkedList); override;
         { inserts another List after the provided item and make this List empty }
         procedure insertListAfter(Item:TLinkedListItem;p : TLinkedList); override;
         { concats another List at the end and make this List empty }
         procedure concatList(p : TLinkedList); override;
         { concats another List at the start and makes a copy
           the list is ordered in reverse.
         }
         procedure insertListcopy(p : TLinkedList); override;
         { concats another List at the end and makes a copy }
         procedure concatListcopy(p : TLinkedList); override;
         { removes all items from the list, the items are not freed }
         procedure RemoveAll; override;
      end;

      TAsmCFI=class
      public
        constructor create;virtual;
        destructor destroy;override;
        procedure generate_code(list:TAsmList);virtual;
        procedure start_frame(list:TAsmList);virtual;
        procedure end_frame(list:TAsmList);virtual;
        procedure outmost_frame(list:TAsmList);virtual;
        procedure cfa_offset(list:TAsmList;reg:tregister;ofs:longint);virtual;
        procedure cfa_restore(list:TAsmList;reg:tregister);virtual;
        procedure cfa_def_cfa(list:TAsmList;reg:tregister;ofs:longint);virtual;
        procedure cfa_def_cfa_register(list:TAsmList;reg:tregister);virtual;
        procedure cfa_def_cfa_offset(list:TAsmList;ofs:longint);virtual;
        function get_frame_start: TAsmLabel;virtual;
        function get_cfa_list : TAsmList;virtual;
      end;
      TAsmCFIClass=class of TAsmCFI;

      { TAsmData }

      TAsmData = class
      private
        { Symbols }
        FAsmSymbolDict : TFPHashObjectList;
        FAltSymbolList : TFPObjectList;
        FNextAltNr     : longint;
        FNextLabelNr   : array[TAsmLabeltype] of longint;
        { Call Frame Information for stack unwinding}
        FAsmCFI        : TAsmCFI;
        FConstPools    : array[TConstPoolType] of THashSet;
        function GetConstPools(APoolType: TConstPoolType): THashSet;
      protected
        function  DefineAsmSymbolByClassBase(symclass: TAsmSymbolClass; const s : TSymStr;_bind:TAsmSymBind;_typ:Tasmsymtype; def: tdef; out wasdefined: boolean) : TAsmSymbol;
      public
        name          : pshortstring;       { owned by tmodule }
        NextVTEntryNr : longint;
        { Assembler lists }
        AsmLists      : array[TAsmListType] of TAsmList;
        CurrAsmList   : TAsmList;
        WideInits     : TLinkedList;
        ResStrInits   : TLinkedList;
        constructor create(n: pshortstring);
        destructor  destroy;override;
        { asmsymbol }
        function  DefineAsmSymbolByClass(symclass: TAsmSymbolClass; const s : TSymStr;_bind:TAsmSymBind;_typ:Tasmsymtype; def: tdef) : TAsmSymbol; virtual;
        function  DefineAsmSymbol(const s : TSymStr;_bind:TAsmSymBind;_typ:Tasmsymtype; def: tdef) : TAsmSymbol;
        function  DefineProcAsmSymbol(pd: tdef; const s: TSymStr; global: boolean): TAsmSymbol;
        function  WeakRefAsmSymbolByClass(symclass: TAsmSymbolClass; const s : TSymStr;_typ:Tasmsymtype) : TAsmSymbol;
        function  WeakRefAsmSymbol(const s : TSymStr;_typ:Tasmsymtype) : TAsmSymbol;
        function  RefAsmSymbolByClass(symclass: TAsmSymbolClass; const s : TSymStr;_typ:Tasmsymtype;indirect:boolean=false) : TAsmSymbol;
        function  RefAsmSymbol(const s : TSymStr;_typ:Tasmsymtype;indirect:boolean=false) : TAsmSymbol;
        function  GetAsmSymbol(const s : TSymStr) : TAsmSymbol;
        { create new assembler label }
        procedure getlabel(out l : TAsmLabel;alt:TAsmLabeltype);
        procedure getjumplabel(out l : TAsmLabel);
        procedure getglobaljumplabel(out l : TAsmLabel);
        procedure getaddrlabel(out l : TAsmLabel);
        { visible from outside current object }
        procedure getglobaldatalabel(out l : TAsmLabel);
        { visible only inside current object, but doesn't start with
          target_asm.label_prefix (treated the Darwin linker as the start of a
          dead-strippable data block) }
        procedure getstaticdatalabel(out l : TAsmLabel);
        { visible only inside the current object and does start with
          target_asm.label_prefix (not treated by the Darwin linker as the start
          of a dead-strippable data block, and references to such labels are
          also ignored to determine whether a data block should be live) }
        procedure getlocaldatalabel(out l : TAsmLabel);
        { generate an alternative (duplicate) symbol }
        procedure GenerateAltSymbol(p:TAsmSymbol);
        procedure ResetAltSymbols;
        property AsmSymbolDict:TFPHashObjectList read FAsmSymbolDict;
        property AsmCFI:TAsmCFI read FAsmCFI;
        { hash tables for reusing constant storage }
        property ConstPools[APoolType:TConstPoolType]: THashSet read GetConstPools;
      end;
      TAsmDataClass = class of TAsmData;

      TTCInitItem = class(TLinkedListItem)
        sym: tsym;
        offset: aint;
        datalabel: TAsmSymbol;
        datadef: TDef;
        constructor Create(asym: tsym; aoffset: aint; alabel: TAsmSymbol; alabeldef: tdef);
      end;

    var
      casmdata: TAsmDataClass;


    var
      CAsmCFI : TAsmCFIClass;
      current_asmdata : TAsmData;


implementation

    uses
      verbose,
      globals,
      symconst,
      aasmtai;

{$ifdef MEMDEBUG}
    var
      memasmsymbols,
      memasmcfi,
      memasmlists : TMemDebug;
{$endif MEMDEBUG}


{*****************************************************************************
                                 TAsmCFI
*****************************************************************************}

    constructor TAsmCFI.create;
      begin
      end;


    destructor TAsmCFI.destroy;
      begin
      end;


    procedure TAsmCFI.generate_code(list:TAsmList);
      begin
      end;


    procedure TAsmCFI.start_frame(list:TAsmList);
      begin
      end;


    procedure TAsmCFI.end_frame(list:TAsmList);
      begin
      end;


    procedure TAsmCFI.outmost_frame(list: TAsmList);
      begin
      end;


    procedure TAsmCFI.cfa_offset(list:TAsmList;reg:tregister;ofs:longint);
      begin
      end;


    procedure TAsmCFI.cfa_restore(list:TAsmList;reg:tregister);
      begin
      end;


    procedure TAsmCFI.cfa_def_cfa(list:TAsmList;reg:tregister;ofs:longint);
      begin
      end;


    procedure TAsmCFI.cfa_def_cfa_register(list:TAsmList;reg:tregister);
      begin
      end;


    procedure TAsmCFI.cfa_def_cfa_offset(list:TAsmList;ofs:longint);
      begin
      end;


    function TAsmCFI.get_frame_start: TAsmLabel;
      begin
        Result:=nil;
      end;


    function TAsmCFI.get_cfa_list: TAsmList;
      begin
        Result:=nil;
      end;

{*****************************************************************************
                                 TTCInitItem
*****************************************************************************}


    constructor TTCInitItem.Create(asym: tsym; aoffset: aint; alabel: TAsmSymbol; alabeldef: tdef);
      begin
        inherited Create;
        sym:=asym;
        offset:=aoffset;
        datalabel:=alabel;
        datadef:=alabeldef;
      end;

{*****************************************************************************
                                 TAsmList
*****************************************************************************}

    constructor TAsmList.create;
      begin
        inherited create;
      end;


    function TAsmList.getlasttaifilepos : pfileposinfo;
      var
       hp : tlinkedlistitem;
      begin
         getlasttaifilepos := nil;
         if assigned(last) then
           begin
              { find the last file information record }
              if not (tai(last).typ in SkipLineInfo) then
                getlasttaifilepos:=@tailineinfo(last).fileinfo
              else
               { go through list backwards to find the first entry
                 with line information
               }
               begin
                 hp:=tai(last);
                 while assigned(hp) and (tai(hp).typ in SkipLineInfo) do
                    hp:=hp.Previous;
                 { found entry }
                 if assigned(hp) then
                   getlasttaifilepos:=@tailineinfo(hp).fileinfo
               end;
           end;
      end;


    procedure TAsmList.insertList(p : TLinkedList);
      begin
        inherited insertList(p);
        inc(section_count,TAsmList(p).section_count);
        TAsmList(p).section_count:=0;
      end;


    procedure TAsmList.insertListBefore(Item : TLinkedListItem; p : TLinkedList);
      begin
        inherited insertListBefore(Item,p);
        inc(section_count,TAsmList(p).section_count);
        TAsmList(p).section_count:=0;
      end;


    procedure TAsmList.insertListAfter(Item : TLinkedListItem; p : TLinkedList);
      begin
        inherited insertListAfter(Item,p);
        inc(section_count,TAsmList(p).section_count);
        TAsmList(p).section_count:=0;
      end;


    procedure TAsmList.concatList(p : TLinkedList);
      begin
        inherited concatList(p);
        inc(section_count,TAsmList(p).section_count);
        TAsmList(p).section_count:=0;
      end;


    procedure TAsmList.insertListcopy(p : TLinkedList);
      begin
        inherited insertListcopy(p);
        inc(section_count,TAsmList(p).section_count);
     end;


    procedure TAsmList.concatListcopy(p : TLinkedList);
      begin
        inherited concatListcopy(p);
        inc(section_count,TAsmList(p).section_count);
      end;


    procedure TAsmList.RemoveAll;
      begin
         inherited RemoveAll;
         section_count:=0;
      end;


{****************************************************************************
                                TAsmData
****************************************************************************}

    function TAsmData.GetConstPools(APoolType: TConstPoolType): THashSet;
      begin
        if FConstPools[APoolType] = nil then
          case APoolType of
            sp_ansistr: FConstPools[APoolType] := TTagHashSet.Create(64, True, False);
          else
            FConstPools[APoolType] := THashSet.Create(64, True, False);
          end;
        Result := FConstPools[APoolType];
      end;


    function TAsmData.DefineAsmSymbolByClassBase(symclass: TAsmSymbolClass; const s: TSymStr; _bind: TAsmSymBind; _typ: Tasmsymtype; def: tdef; out wasdefined: boolean): TAsmSymbol;
      var
        hp : TAsmSymbol;
        namestr : TSymStr;
      begin
        { this difference is only necessary to determine whether we always need
          indirect references or not }
        if _typ in [AT_DATA_FORCEINDIRECT,AT_DATA_NOINDIRECT] then
          _typ:=AT_DATA;
        namestr:=s;
        if _bind in asmsymbindindirect then
          namestr:=namestr+suffix_indirect;
        hp:=TAsmSymbol(FAsmSymbolDict.Find(namestr));
        if assigned(hp) then
         begin
           { Redefine is allowed, but the types must be the same. The redefine
             is needed for Darwin where the labels are first allocated }
           wasdefined:=not(hp.bind in [AB_EXTERNAL,AB_WEAK_EXTERNAL]);
           if wasdefined then
             begin
               if (hp.bind<>_bind) and
                  (hp.typ<>_typ) then
                 internalerror(200603261);
             end;
           hp.typ:=_typ;
           { Changing bind from AB_GLOBAL to AB_LOCAL is wrong
             if bind is already AB_GLOBAL or AB_EXTERNAL,
             GOT might have been used, so change might be harmful. }
           if (_bind<>hp.bind) and (hp.getrefs>0) then
             begin
{$ifdef extdebug}
               { the changes that matter must become internalerrors, the rest
                 should be ignored; a used cannot change anything about this,
                 so printing a warning/hint is not useful }
               if (_bind=AB_LOCAL) then
                 Message3(asmw_w_changing_bind_type,namestr,asmsymbindname[hp.bind],asmsymbindname[_bind])
               else
                 Message3(asmw_h_changing_bind_type,namestr,asmsymbindname[hp.bind],asmsymbindname[_bind]);
{$endif extdebug}
             end;
           hp.bind:=_bind;
         end
        else
         begin
           wasdefined:=false;
           { Not found, insert it. }
           hp:=symclass.create(AsmSymbolDict,namestr,_bind,_typ);
         end;
        result:=hp;
      end;


    constructor TAsmData.create(n:pshortstring);
      var
        alt : TAsmLabelType;
        hal : TAsmListType;
      begin
        inherited create;
        name:=n;
        { symbols }
        FAsmSymbolDict:=TFPHashObjectList.create(true);
        FAltSymbolList:=TFPObjectList.Create(false);
        { labels }
        FNextAltNr:=1;
        for alt:=low(TAsmLabelType) to high(TAsmLabelType) do
          FNextLabelNr[alt]:=1;
        { AsmLists }
        CurrAsmList:=TAsmList.create;
        for hal:=low(TAsmListType) to high(TAsmListType) do
          AsmLists[hal]:=TAsmList.create;
        WideInits :=TAsmList.create;
        ResStrInits:=TAsmList.create;
        { CFI }
        FAsmCFI:=CAsmCFI.Create;
      end;


    destructor TAsmData.destroy;
      var
        hal : TAsmListType;
        hp  : TConstPoolType;
      begin
        { Symbols }
{$ifdef MEMDEBUG}
        memasmsymbols.start;
{$endif}
        FAltSymbolList.free;
        FAsmSymbolDict.free;
{$ifdef MEMDEBUG}
        memasmsymbols.stop;
{$endif}
        { CFI }
{$ifdef MEMDEBUG}
        memasmcfi.start;
{$endif}
        FAsmCFI.free;
{$ifdef MEMDEBUG}
        memasmcfi.stop;
{$endif}
        { Lists }
{$ifdef MEMDEBUG}
         memasmlists.start;
{$endif}
        ResStrInits.free;
        WideInits.free;
         for hal:=low(TAsmListType) to high(TAsmListType) do
           AsmLists[hal].free;
         CurrAsmList.free;
{$ifdef MEMDEBUG}
         memasmlists.stop;
{$endif}
         for hp := low(TConstPoolType) to high(TConstPoolType) do
           FConstPools[hp].Free;
      end;

    function TAsmData.DefineAsmSymbolByClass(symclass: TAsmSymbolClass; const s: TSymStr; _bind: TAsmSymBind; _typ: Tasmsymtype; def: tdef): TAsmSymbol;
      var
        wasdefined: boolean;
      begin
        result:=DefineAsmSymbolByClassBase(symclass,s,_bind,_typ,def,wasdefined);
      end;


    function TAsmData.DefineAsmSymbol(const s: TSymStr; _bind: TAsmSymBind; _typ: Tasmsymtype; def: tdef): TAsmSymbol;
      begin
        result:=DefineAsmSymbolByClass(TAsmSymbol,s,_bind,_typ,def);
      end;


    function TAsmData.DefineProcAsmSymbol(pd: tdef; const s: TSymStr; global: boolean): TAsmSymbol;
      begin
        { The condition to use global or local symbol must match
          the code written in hlcg.gen_proc_symbol to
          avoid change from AB_LOCAL to AB_GLOBAL, which generates
          erroneous code (at least for targets using GOT) }
        if global or
           (cs_profile in current_settings.moduleswitches) then
          result:=DefineAsmSymbol(s,AB_GLOBAL,AT_FUNCTION,pd)
        else if tf_supports_hidden_symbols in target_info.flags then
          result:=DefineAsmSymbol(s,AB_PRIVATE_EXTERN,AT_FUNCTION,pd)
        else
          result:=DefineAsmSymbol(s,AB_LOCAL,AT_FUNCTION,pd);
      end;


    function TAsmData.RefAsmSymbolByClass(symclass: TAsmSymbolClass; const s : TSymStr;_typ:Tasmsymtype;indirect:boolean) : TAsmSymbol;
      var
        namestr : TSymStr;
        bind : tasmsymbind;
      begin
        namestr:=s;
        if indirect then
          begin
            namestr:=namestr+suffix_indirect;
            bind:=AB_EXTERNAL_INDIRECT;
          end
        else
          begin
            bind:=AB_EXTERNAL;
          end;
        result:=TAsmSymbol(FAsmSymbolDict.Find(namestr));
        if not assigned(result) then
          result:=symclass.create(AsmSymbolDict,namestr,bind,_typ)
        { one normal reference removes the "weak" character of a symbol }
        else if (result.bind=AB_WEAK_EXTERNAL) then
          result.bind:=bind;
      end;


    function TAsmData.RefAsmSymbol(const s : TSymStr;_typ:Tasmsymtype;indirect:boolean) : TAsmSymbol;
      begin
        result:=RefAsmSymbolByClass(TAsmSymbol,s,_typ,indirect);
      end;


    function TAsmData.WeakRefAsmSymbolByClass(symclass: TAsmSymbolClass; const s : TSymStr;_typ:Tasmsymtype) : TAsmSymbol;
      begin
        result:=TAsmSymbol(FAsmSymbolDict.Find(s));
        if not assigned(result) then
          result:=symclass.create(AsmSymbolDict,s,AB_WEAK_EXTERNAL,_typ);
      end;


    function TAsmData.WeakRefAsmSymbol(const s : TSymStr;_typ:Tasmsymtype) : TAsmSymbol;
      begin
        result:=WeakRefAsmSymbolByClass(TAsmSymbol,s,_typ);
      end;


    function TAsmData.GetAsmSymbol(const s : TSymStr) : TAsmSymbol;
      begin
        result:=TAsmSymbol(FAsmSymbolDict.Find(s));
      end;


    procedure TAsmData.GenerateAltSymbol(p:TAsmSymbol);
      begin
        if not assigned(p.altsymbol) then
         begin
           p.altsymbol:=p.getaltcopy(AsmSymbolDict,FNextAltNr);
           FAltSymbolList.Add(p);
         end;
      end;


    procedure TAsmData.ResetAltSymbols;
      var
        i  : longint;
      begin
        for i:=0 to FAltSymbolList.Count-1 do
          TAsmSymbol(FAltSymbolList[i]).altsymbol:=nil;
        FAltSymbolList.Clear;
      end;


    procedure TAsmData.getlabel(out l : TAsmLabel;alt:TAsmLabeltype);
      begin
        if (target_info.system in (systems_linux + systems_bsd + systems_android)) and
           { the next condition was
             (cs_create_smart in current_settings.moduleswitches) and
             but if we create_smartlink_sections, this is useless }
           (create_smartlink_library) and
           (alt = alt_dbgline) then
          l:=TAsmLabel.createglobal(AsmSymbolDict,name^,FNextLabelNr[alt],alt)
        else
          l:=TAsmLabel.createlocal(AsmSymbolDict,FNextLabelNr[alt],alt);
        inc(FNextLabelNr[alt]);
      end;


    procedure TAsmData.getjumplabel(out l : TAsmLabel);
      begin
        l:=TAsmLabel.createlocal(AsmSymbolDict,FNextLabelNr[alt_jump],alt_jump);
        inc(FNextLabelNr[alt_jump]);
      end;

    procedure TAsmData.getglobaljumplabel(out l : TAsmLabel);
      begin
        l:=TAsmLabel.createglobal(AsmSymbolDict,name^,FNextLabelNr[alt_jump],alt_jump);
        inc(FNextLabelNr[alt_jump]);
      end;


    procedure TAsmData.getglobaldatalabel(out l : TAsmLabel);
      begin
        l:=TAsmLabel.createglobal(AsmSymbolDict,name^,FNextLabelNr[alt_data],alt_data);
        inc(FNextLabelNr[alt_data]);
      end;


    procedure TAsmData.getstaticdatalabel(out l : TAsmLabel);
      begin
        l:=TAsmLabel.createstatic(AsmSymbolDict,FNextLabelNr[alt_data],alt_data);
        inc(FNextLabelNr[alt_data]);
      end;


    procedure TAsmData.getlocaldatalabel(out l: TAsmLabel);
      begin
        l:=TAsmLabel.createlocal(AsmSymbolDict,FNextLabelNr[alt_data],alt_data);
        inc(FNextLabelNr[alt_data]);
      end;


    procedure TAsmData.getaddrlabel(out l : TAsmLabel);
      begin
        l:=TAsmLabel.createlocal(AsmSymbolDict,FNextLabelNr[alt_addr],alt_addr);
        inc(FNextLabelNr[alt_addr]);
      end;

initialization
{$ifdef MEMDEBUG}
  memasmsymbols:=TMemDebug.create('AsmSymbols');
  memasmsymbols.stop;
  memasmcfi:=TMemDebug.create('AsmCFI');
  memasmcfi.stop;
  memasmlists:=TMemDebug.create('AsmLists');
  memasmlists.stop;
{$endif MEMDEBUG}
  if not(assigned(CAsmCFI)) then
    CAsmCFI:=TAsmCFI;

finalization
{$ifdef MEMDEBUG}
  memasmsymbols.free;
  memasmcfi.free;
  memasmlists.free;
{$endif MEMDEBUG}

end.
