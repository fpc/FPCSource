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
  are then overriden for each assembler writer to actually write the data in these
  classes to an assembler file.
}

unit aasmdata;

{$i fpcdefs.inc}

interface

    uses
       cutils,cclasses,
       globtype,globals,systems,
       cpuinfo,cpubase,
       cgbase,cgutils,
       symtype,
       aasmbase,ogbase;

    type
      { Type of AsmLists. The order is important for the layout of the
        information in the .o file. The stabs for the types must be defined
        before they can be referenced and therefor they need to be written
        first (PFV) }
      TAsmListType=(
        al_start,
        al_stabs,
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
        al_dwarf,
        al_dwarf_info,
        al_dwarf_abbrev,
        al_dwarf_line,
        al_picdata,
        al_resourcestrings,
        al_end
      );

    const
      AsmListTypeStr : array[TAsmListType] of string[24] =(
        'al_begin',
        'al_stabs',
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
        'al_dwarf',
        'al_dwarf_info',
        'al_dwarf_abbrev',
        'al_dwarf_line',
        'al_picdata',
        'al_resourcestrings',
        'al_end'
      );

    type
      TAsmList = class(tlinkedlist)
         constructor create;
         function  empty : boolean;
         function  getlasttaifilepos : pfileposinfo;
      end;

      TAsmCFI=class
      public
        constructor create;virtual;
        destructor destroy;override;
        procedure generate_code(list:TAsmList);virtual;
        procedure start_frame(list:TAsmList);virtual;
        procedure end_frame(list:TAsmList);virtual;
        procedure cfa_offset(list:TAsmList;reg:tregister;ofs:longint);virtual;
        procedure cfa_restore(list:TAsmList;reg:tregister);virtual;
        procedure cfa_def_cfa_register(list:TAsmList;reg:tregister);virtual;
        procedure cfa_def_cfa_offset(list:TAsmList;ofs:longint);virtual;
      end;
      TAsmCFIClass=class of TAsmCFI;

      TAsmData = class
      private
        { Symbols }
        FAsmSymbolDict : TDictionary;
        FAltSymbolList : TFPObjectList;
        FNextAltNr     : longint;
        FNextLabelNr   : array[Tasmlabeltype] of longint;
        { Call Frame Information for stack unwinding}
        FAsmCFI        : TAsmCFI;
      public
        name,
        realname      : string[80];
        NextVTEntryNr : longint;
        { Assembler lists }
        AsmLists      : array[TAsmListType] of TAsmList;
        CurrAsmList   : TAsmList;
        constructor create(const n:string);
        destructor  destroy;override;
        { asmsymbol }
        function  DefineAsmSymbol(const s : string;_bind:TAsmSymBind;_typ:Tasmsymtype) : tasmsymbol;
        function  RefAsmSymbol(const s : string) : tasmsymbol;
        function  getasmsymbol(const s : string) : tasmsymbol;
        { create new assembler label }
        procedure getlabel(var l : tasmlabel;alt:tasmlabeltype);
        procedure getjumplabel(var l : tasmlabel);
        procedure getaddrlabel(var l : tasmlabel);
        procedure getdatalabel(var l : tasmlabel);
        { generate an alternative (duplicate) symbol }
        procedure GenerateAltSymbol(p:tasmsymbol);
        procedure ResetAltSymbols;
        property AsmSymbolDict:TDictionary read FAsmSymbolDict;
        property AsmCFI:TAsmCFI read FAsmCFI;
      end;

    var
      CAsmCFI : TAsmCFIClass;
      current_asmdata : TAsmData;


implementation

    uses
      verbose,
      aasmtai;


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


    procedure TAsmCFI.cfa_offset(list:TAsmList;reg:tregister;ofs:longint);
      begin
      end;


    procedure TAsmCFI.cfa_restore(list:TAsmList;reg:tregister);
      begin
      end;


    procedure TAsmCFI.cfa_def_cfa_register(list:TAsmList;reg:tregister);
      begin
      end;


    procedure TAsmCFI.cfa_def_cfa_offset(list:TAsmList;ofs:longint);
      begin
      end;


{*****************************************************************************
                                 TAsmList
*****************************************************************************}

    constructor TAsmList.create;
      begin
        inherited create;
        { make sure the optimizer won't remove the first tai of this list}
        insert(tai_marker.create(mark_BlockStart));
      end;


    function TAsmList.empty : boolean;
      begin
        { there is always a mark_BlockStart available,
          see TAsmList.create }
        result:=(count<=1);
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


{****************************************************************************
                                TAsmData
****************************************************************************}

    constructor TAsmData.create(const n:string);
      var
        alt : TAsmLabelType;
        hal : TAsmListType;
      begin
        inherited create;
        realname:=n;
        name:=upper(n);
        { symbols }
        FAsmSymbolDict:=TDictionary.create;
        FAsmSymbolDict.usehash;
        FAltSymbolList:=TFPObjectList.Create(false);
        { labels }
        FNextAltNr:=1;
        for alt:=low(TAsmLabelType) to high(TAsmLabelType) do
          FNextLabelNr[alt]:=1;
        { AsmLists }
        CurrAsmList:=TAsmList.create;
        for hal:=low(TAsmListType) to high(TAsmListType) do
          AsmLists[hal]:=TAsmList.create;
        { PIC data }
        if (target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
          AsmLists[al_picdata].concat(tai_directive.create(asd_non_lazy_symbol_pointer,''));
        { CFI }
        FAsmCFI:=CAsmCFI.Create;
      end;


    destructor TAsmData.destroy;
      var
{$ifdef MEMDEBUG}
        d : tmemdebug;
{$endif}
        hal : TAsmListType;
      begin
{$ifdef MEMDEBUG}
         d:=tmemdebug.create(name+' - AsmSymbols');
{$endif}
        FAltSymbolList.free;
        FAsmSymbolDict.free;
{$ifdef MEMDEBUG}
         d.free;
{$endif}
{$ifdef MEMDEBUG}
         d:=tmemdebug.create(name+' - AsmCFI');
{$endif}
        FAsmCFI.free;
{$ifdef MEMDEBUG}
         d.free;
{$endif}
{$ifdef MEMDEBUG}
         d:=tmemdebug.create(name+' - AsmLists');
{$endif}
         for hal:=low(TAsmListType) to high(TAsmListType) do
           AsmLists[hal].free;
{$ifdef MEMDEBUG}
         d.free;
{$endif}
      end;


    function TAsmData.DefineAsmSymbol(const s : string;_bind:TAsmSymBind;_typ:Tasmsymtype) : tasmsymbol;
      var
        hp : tasmsymbol;
      begin
        hp:=tasmsymbol(FAsmSymbolDict.search(s));
        if assigned(hp) then
         begin
           { Redefine is allowed, but the types must be the same. The redefine
             is needed for Darwin where the labels are first allocated }
           if (hp.bind<>AB_EXTERNAL) then
             begin
               if (hp.bind<>_bind) and
                  (hp.typ<>_typ) then
                 internalerror(200603261);
             end;
           hp.typ:=_typ;
           hp.bind:=_bind;
         end
        else
         begin
           { Not found, insert it. }
           hp:=tasmsymbol.create(s,_bind,_typ);
           FAsmSymbolDict.insert(hp);
         end;
        result:=hp;
      end;


    function TAsmData.RefAsmSymbol(const s : string) : tasmsymbol;
      var
        hp : tasmsymbol;
      begin
        hp:=tasmsymbol(FAsmSymbolDict.search(s));
        if not assigned(hp) then
         begin
           { Not found, insert it. }
           hp:=tasmsymbol.create(s,AB_EXTERNAL,AT_NONE);
           FAsmSymbolDict.insert(hp);
         end;
        result:=hp;
      end;


    function TAsmData.getasmsymbol(const s : string) : tasmsymbol;
      begin
        getasmsymbol:=tasmsymbol(FAsmSymbolDict.search(s));
      end;


    procedure TAsmData.GenerateAltSymbol(p:tasmsymbol);
      begin
        if not assigned(p.altsymbol) then
         begin
           p.altsymbol:=tasmsymbol.create(p.name+'_'+tostr(FNextAltNr),p.bind,p.typ);
           FAsmSymbolDict.insert(p.altsymbol);
           FAltSymbolList.Add(p);
         end;
      end;


    procedure TAsmData.ResetAltSymbols;
      var
        i  : longint;
      begin
        for i:=0 to FAltSymbolList.Count-1 do
          tasmsymbol(FAltSymbolList[i]).altsymbol:=nil;
        FAltSymbolList.Clear;
      end;


    procedure TAsmData.getlabel(var l : tasmlabel;alt:tasmlabeltype);
      begin
        l:=tasmlabel.createlocal(FNextLabelNr[alt],alt);
        inc(FNextLabelNr[alt]);
        FAsmSymbolDict.insert(l);
      end;


    procedure TAsmData.getjumplabel(var l : tasmlabel);
      begin
        l:=tasmlabel.createlocal(FNextLabelNr[alt_jump],alt_jump);
        inc(FNextLabelNr[alt_jump]);
        FAsmSymbolDict.insert(l);
      end;


    procedure TAsmData.getdatalabel(var l : tasmlabel);
      begin
        l:=tasmlabel.createglobal(name,FNextLabelNr[alt_data],alt_data);
        inc(FNextLabelNr[alt_data]);
        FAsmSymbolDict.insert(l);
      end;


    procedure TAsmData.getaddrlabel(var l : tasmlabel);
      begin
        l:=tasmlabel.createlocal(FNextLabelNr[alt_addr],alt_addr);
        inc(FNextLabelNr[alt_addr]);
        FAsmSymbolDict.insert(l);
      end;

begin
  CAsmCFI:=TAsmCFI;
end.
