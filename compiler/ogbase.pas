{
    Copyright (c) 1998-2006 by Peter Vreman

    Contains the base stuff for binary object file writers

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
unit ogbase;

{$i fpcdefs.inc}

interface

    uses
      { common }
      cutils,
      cclasses,
      { targets }
      systems,globtype,
      { outputwriters }
      owbase,
      { assembler }
      aasmbase;

    type
      TObjSection = class;
      TObjData = class;

      TExeSection = class;
      TExeSymbol  = class;

      TObjRelocationType = (
         { Relocation to absolute address }
         RELOC_ABSOLUTE,
{$ifdef x86_64}
         { 32bit Relocation to absolute address }
         RELOC_ABSOLUTE32,
         { 64 bit coff only }
         RELOC_RELATIVE_1,
         RELOC_RELATIVE_2,
         RELOC_RELATIVE_3,
         RELOC_RELATIVE_4,
         RELOC_RELATIVE_5,
         { PIC }
         RELOC_GOTPCREL,
         RELOC_PLT32,
{$endif x86_64}
{$ifdef arm}
         RELOC_RELATIVE_24,
{$endif arm}
         { Relative relocation }
         RELOC_RELATIVE,
         { PECoff (Windows) RVA relocation }
         RELOC_RVA,
         { PECoff (Windows) section relocation, required by DWARF2 debug info }
         RELOC_SECREL32,
         { Generate a 0 value at the place of the relocation,
           this is used to remove unused vtable entries }
         RELOC_ZERO,
         { No relocation is needed. It is used in ARM object files.
           Also internal linker use this reloc to make virtual (not real)
           links to some sections }
         RELOC_NONE
      );

{$ifndef x86_64}
    const
      RELOC_ABSOLUTE32 = RELOC_ABSOLUTE;
{$endif x86_64}

    const
      { stab types }
      N_GSYM = $20;
      N_STSYM = 38;     { initialized const }
      N_LCSYM = 40;     { non initialized variable}
      N_Function = $24; { function or const }
      N_TextLine = $44;
      N_DataLine = $46;
      N_BssLine = $48;
      N_RSYM = $40;     { register variable }
      N_LSYM = $80;
      N_tsym = 160;
      N_SourceFile = $64;
      N_IncludeFile = $84;
      N_BINCL = $82;
      N_EINCL = $A2;
      N_LBRAC = $C0;
      N_EXCL  = $C2;
      N_RBRAC = $E0;

      { GNU extensions }
      debuglinkname='.gnu_debuglink';

    type
      TObjSectionOption = (
       { Has Data available in the file }
       oso_Data,
       { Is loaded into memory }
       oso_load,
       { Not loaded into memory }
       oso_noload,
       { Read only }
       oso_readonly,
       { Read/Write }
       oso_write,
       { Contains executable instructions }
       oso_executable,
       { Never discard section }
       oso_keep,
       { Special common symbols }
       oso_common,
       { Contains debug info and can be stripped }
       oso_debug,
       { Contains only strings }
       oso_strings
     );

     TObjSectionOptions = set of TObjSectionOption;

     TObjSymbol = class(TFPHashObject)
     public
       bind       : TAsmsymbind;
       typ        : TAsmsymtype;
       { Current assemble pass, used to detect duplicate labels }
       pass       : byte;
       objsection : TObjSection;
       symidx     : longint;
       offset,
       size       : aword;
       { Used for external and common solving during linking }
       exesymbol  : TExeSymbol;
       constructor create(AList:TFPHashObjectList;const AName:string);
       function  address:aword;
       procedure SetAddress(apass:byte;aobjsec:TObjSection;abind:TAsmsymbind;atyp:Tasmsymtype);
     end;

     { Stabs is common for all targets }
     TObjStabEntry=packed record
        strpos  : longint;
        ntype   : byte;
        nother  : byte;
        ndesc   : word;
        nvalue  : longint;
     end;
     PObjStabEntry=^TObjStabEntry;

     TObjRelocation = class
        DataOffset,
        orgsize    : aword;  { original size of the symbol to Relocate, required for COFF }
        symbol     : TObjSymbol;
        objsection : TObjSection; { only used if symbol=nil }
        typ        : TObjRelocationType;
        constructor CreateSymbol(ADataOffset:aword;s:TObjSymbol;Atyp:TObjRelocationType);
        constructor CreateSymbolSize(ADataOffset:aword;s:TObjSymbol;Aorgsize:aword;Atyp:TObjRelocationType);
        constructor CreateSection(ADataOffset:aword;aobjsec:TObjSection;Atyp:TObjRelocationType);
     end;

     TObjSection = class(TFPHashObject)
     private
       FData       : TDynamicArray;
       FSecOptions : TObjSectionOptions;
       FCachedFullName : pshortstring;
       procedure SetSecOptions(Aoptions:TObjSectionOptions);
     public
       ObjData    : TObjData;
       SecSymIdx  : longint;   { index for the section in symtab }
       SecAlign   : shortint;   { alignment of the section }
       { section Data }
       Size,
       DataPos,
       MemPos     : aword;
       DataAlignBytes : shortint;
       { Relocations (=references) to other sections }
       ObjRelocations : TFPObjectList;
       { Symbols this defines }
       ObjSymbolDefines : TFPObjectList;
       { executable linking }
       ExeSection  : TExeSection;
       USed        : Boolean;
       VTRefList : TFPObjectList;
       constructor create(AList:TFPHashObjectList;const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);virtual;
       destructor  destroy;override;
       function  write(const d;l:aword):aword;
       function  writestr(const s:string):aword;
       function  WriteZeros(l:longword):aword;
       function  setmempos(mpos:qword):qword;
       procedure setDatapos(var dpos:aword);
       procedure alloc(l:aword);
       procedure addsymReloc(ofs:aword;p:TObjSymbol;Reloctype:TObjRelocationType);
       procedure addsectionReloc(ofs:aword;aobjsec:TObjSection;Reloctype:TObjRelocationType);
       procedure AddSymbolDefine(p:TObjSymbol);
       procedure FixupRelocs;virtual;
       procedure ReleaseData;
       function  FullName:string;
       property  Data:TDynamicArray read FData;
       property  SecOptions:TObjSectionOptions read FSecOptions write SetSecOptions;
     end;
     TObjSectionClass = class of TObjSection;

     TString80 = string[80];

     TObjData = class(TLinkedListItem)
     private
       FName       : TString80;
       FCurrObjSec : TObjSection;
       FObjSectionList  : TFPHashObjectList;
       FCObjSection     : TObjSectionClass;
       { Symbols that will be defined in this object file }
       FObjSymbolList    : TFPHashObjectList;
       FCachedAsmSymbolList : TFPObjectList;
       { Special info sections that are written to during object generation }
       FStabsObjSec,
       FStabStrObjSec : TObjSection;
       procedure section_reset(p:TObject;arg:pointer);
       procedure section_afteralloc(p:TObject;arg:pointer);
       procedure section_afterwrite(p:TObject;arg:pointer);
     protected
       property CObjSection:TObjSectionClass read FCObjSection write FCObjSection;
     public
       CurrPass  : byte;
       ImageBase : aword;
       constructor create(const n:string);virtual;
       destructor  destroy;override;
       { Sections }
       function  sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;virtual;
       function  sectiontype2options(atype:TAsmSectiontype):TObjSectionOptions;virtual;
       function  sectiontype2align(atype:TAsmSectiontype):shortint;virtual;
       function  createsection(atype:TAsmSectionType;const aname:string='';aorder:TAsmSectionOrder=secorder_default):TObjSection;
       function  createsection(const aname:string;aalign:shortint;aoptions:TObjSectionOptions;DiscardDuplicate:boolean=true):TObjSection;virtual;
       procedure CreateDebugSections;virtual;
       function  findsection(const aname:string):TObjSection;
       procedure setsection(asec:TObjSection);
       { Symbols }
       function  createsymbol(const aname:string):TObjSymbol;
       function  symboldefine(asmsym:TAsmSymbol):TObjSymbol;
       function  symboldefine(const aname:string;abind:TAsmsymbind;atyp:Tasmsymtype):TObjSymbol;
       function  symbolref(asmsym:TAsmSymbol):TObjSymbol;
       function  symbolref(const aname:string):TObjSymbol;
       procedure ResetCachedAsmSymbols;
       { Allocation }
       procedure alloc(len:aword);
       procedure allocalign(len:shortint);
       procedure writebytes(const Data;len:aword);
       procedure writeReloc(Data:aint;len:aword;p:TObjSymbol;Reloctype:TObjRelocationType);virtual;abstract;
       procedure beforealloc;virtual;
       procedure beforewrite;virtual;
       procedure afteralloc;virtual;
       procedure afterwrite;virtual;
       procedure resetsections;
       property Name:TString80 read FName;
       property CurrObjSec:TObjSection read FCurrObjSec;
       property ObjSymbolList:TFPHashObjectList read FObjSymbolList;
       property ObjSectionList:TFPHashObjectList read FObjSectionList;
       property StabsSec:TObjSection read FStabsObjSec write FStabsObjSec;
       property StabStrSec:TObjSection read FStabStrObjSec write FStabStrObjSec;
     end;
     TObjDataClass = class of TObjData;

     TObjOutput = class
      private
        FCObjData : TObjDataClass;
      protected
        { writer }
        FWriter    : TObjectwriter;
        function  writeData(Data:TObjData):boolean;virtual;abstract;
        property CObjData : TObjDataClass read FCObjData write FCObjData;
      public
        constructor create(AWriter:TObjectWriter);virtual;
        destructor  destroy;override;
        function  newObjData(const n:string):TObjData;
        function  startObjectfile(const fn:string):boolean;
        function  writeobjectfile(Data:TObjData):boolean;
        procedure exportsymbol(p:TObjSymbol);
        property Writer:TObjectWriter read FWriter;
      end;
      TObjOutputClass=class of TObjOutput;

      TObjInput = class
      private
        FCObjData : TObjDataClass;
      protected
        { reader }
        FReader    : TObjectReader;
        InputFileName : string;
        property CObjData : TObjDataClass read FCObjData write FCObjData;
      public
        constructor create;virtual;
        destructor  destroy;override;
        function  newObjData(const n:string):TObjData;
        function  ReadObjData(AReader:TObjectreader;Data:TObjData):boolean;virtual;abstract;
        procedure inputerror(const s : string);
      end;
      TObjInputClass=class of TObjInput;

      TVTableEntry=record
        ObjRelocation : TObjRelocation;
        orgreloctype  : TObjRelocationType;
        Enabled,
        Used  : Boolean;
      end;
      PVTableEntry=^TVTableEntry;

      TExeVTable = class
      private
        procedure CheckIdx(VTableIdx:longint);
      public
        ExeSymbol    : TExeSymbol;
        EntryCnt     : Longint;
        EntryArray   : PVTableEntry;
        Consolidated : Boolean;
        ChildList    : TFPObjectList;
        constructor Create(AExeSymbol:TExeSymbol);
        destructor Destroy;override;
        procedure AddChild(vt:TExeVTable);
        procedure AddEntry(VTableIdx:Longint);
        procedure SetVTableSize(ASize:longint);
        function  VTableRef(VTableIdx:Longint):TObjRelocation;
      end;

      TSymbolState = (symstate_undefined,symstate_defined,symstate_common);

      TExeSymbol = class(TFPHashObject)
        ObjSymbol  : TObjSymbol;
        ExeSection : TExeSection;
        State      : TSymbolState;
        { Used for vmt references optimization }
        VTable     : TExeVTable;
      end;

      TExeSection = class(TFPHashObject)
      private
        FSecSymIdx : longint;
        FObjSectionList : TFPObjectList;
      public
        Size,
        DataPos,
        MemPos     : aword;
        SecAlign   : shortint;
        SecOptions : TObjSectionOptions;
        constructor create(AList:TFPHashObjectList;const AName:string);virtual;
        destructor  destroy;override;
        procedure AddObjSection(objsec:TObjSection);
        property ObjSectionList:TFPObjectList read FObjSectionList;
        property SecSymIdx:longint read FSecSymIdx write FSecSymIdx;
      end;
      TExeSectionClass=class of TExeSection;

      TStaticLibrary = class(TFPHashObject)
      private
        FArReader : TObjectReader;
        FObjInputClass : TObjInputClass;
      public
        constructor create(AList:TFPHashObjectList;const AName:string;AReader:TObjectReader;AObjInputClass:TObjInputClass);
        destructor  destroy;override;
        property ArReader:TObjectReader read FArReader;
        property ObjInputClass:TObjInputClass read FObjInputClass;
      end;

      TImportLibrary = class(TFPHashObject)
      private
        FImportSymbolList : TFPHashObjectList;
      public
        constructor create(AList:TFPHashObjectList;const AName:string);
        destructor  destroy;override;
        property ImportSymbolList:TFPHashObjectList read FImportSymbolList;
      end;

      TImportSymbol = class(TFPHashObject)
      private
        FOrdNr  : longint;
        FIsVar  : boolean;
        FMangledName : string;
      public
        constructor create(AList:TFPHashObjectList;const AName:string;AOrdNr:longint;AIsVar:boolean);
        property OrdNr: longint read FOrdNr;
        property MangledName: string read FMangledName;
        property IsVar: boolean read FIsVar;
      end;

      TExeWriteMode = (ewm_exefull,ewm_dbgonly,ewm_exeonly);

      TExeOutput = class
      private
        { ExeSectionList }
        FCObjData         : TObjDataClass;
        FCExeSection      : TExeSectionClass;
        FCurrExeSec       : TExeSection;
        FExeSectionList   : TFPHashObjectList;
        Fzeronr           : longint;
        { Symbols }
        FExeSymbolList    : TFPHashObjectList;
        FUnresolvedExeSymbols : TFPObjectList;
        FExternalObjSymbols,
        FCommonObjSymbols   : TFPObjectList;
        FEntryName          : string;
        FExeVTableList     : TFPObjectList;
        { Objects }
        FObjDataList  : TFPObjectList;
        { Position calculation }
        FImageBase    : aword;
        FCurrMemPos       : qword;
        procedure SetCurrMemPos(const AValue: qword);
      protected
        { writer }
        FExeWriteMode : TExeWriteMode;
        FWriter : TObjectwriter;
        commonObjSection : TObjSection;
        internalObjData : TObjData;
        EntrySym  : TObjSymbol;
        SectionDataAlign,
        SectionMemAlign : aword;
        function  writeData:boolean;virtual;abstract;
        property CExeSection:TExeSectionClass read FCExeSection write FCExeSection;
        property CObjData:TObjDataClass read FCObjData write FCObjData;
        procedure Order_ObjSectionList(ObjSectionList : TFPObjectList);virtual;
      public
        CurrDataPos  : aword;
        MaxMemPos    : qword;
        IsSharedLibrary : boolean;
        constructor create;virtual;
        destructor  destroy;override;
        function  FindExeSection(const aname:string):TExeSection;
        procedure AddObjData(ObjData:TObjData);
        procedure Load_Start;virtual;
        procedure Load_EntryName(const aname:string);virtual;
        procedure Load_Symbol(const aname:string);virtual;
        procedure Load_IsSharedLibrary;
        procedure Load_ImageBase(const avalue:string);
        procedure Order_Start;virtual;
        procedure Order_End;virtual;
        procedure Order_ExeSection(const aname:string);virtual;
        procedure Order_Align(const avalue:string);virtual;
        procedure Order_Zeros(const avalue:string);virtual;
        procedure Order_Symbol(const aname:string);virtual;
        procedure Order_EndExeSection;virtual;
        procedure Order_ObjSection(const aname:string);virtual;
        procedure MemPos_Start;virtual;
        procedure MemPos_Header;virtual;
        procedure MemPos_ExeSection(const aname:string);virtual;
        procedure MemPos_EndExeSection;virtual;
        procedure DataPos_Start;virtual;
        procedure DataPos_Header;virtual;
        procedure DataPos_ExeSection(const aname:string);virtual;
        procedure DataPos_EndExeSection;virtual;
        procedure DataPos_Symbols;virtual;
        procedure BuildVTableTree(VTInheritList,VTEntryList:TFPObjectList);
        procedure PackUnresolvedExeSymbols(const s:string);
        procedure ResolveSymbols(StaticLibraryList:TFPHashObjectList);
        procedure PrintMemoryMap;
        procedure FixupSymbols;
        procedure FixupRelocations;
        procedure MergeStabs;
        procedure RemoveUnreferencedSections;
        procedure RemoveEmptySections;
        procedure RemoveDebugInfo;
        procedure GenerateLibraryImports(ImportLibraryList:TFPHashObjectList);virtual;
        procedure GenerateDebugLink(const dbgname:string;dbgcrc:cardinal);
        function WriteExeFile(const fn:string):boolean;
        property Writer:TObjectWriter read FWriter;
        property ExeSectionList:TFPHashObjectList read FExeSectionList;
        property ObjDataList:TFPObjectList read FObjDataList;
        property ExeSymbolList:TFPHashObjectList read FExeSymbolList;
        property UnresolvedExeSymbols:TFPObjectList read FUnresolvedExeSymbols;
        property ExternalObjSymbols:TFPObjectList read FExternalObjSymbols;
        property CommonObjSymbols:TFPObjectList read FCommonObjSymbols;
        property ExeVTableList:TFPObjectList read FExeVTableList;
        property EntryName:string read FEntryName write FEntryName;
        property ImageBase:aword read FImageBase write FImageBase;
        property CurrExeSec:TExeSection read FCurrExeSec;
        property ExeWriteMode:TExeWriteMode read FExeWriteMode write FExeWriteMode;
        property CurrMemPos:qword read FCurrMemPos write SetCurrMemPos;
      end;
      TExeOutputClass=class of TExeOutput;

    var
      exeoutput : TExeOutput;


implementation

    uses
      SysUtils,
      globals,verbose,fmodule,ogmap;

    const
      SectionDataMaxGrow = 4096;

{$ifdef MEMDEBUG}
    var
      memobjsymbols,
      memobjsections : TMemDebug;
{$endif MEMDEBUG}

{*****************************************************************************
                                 TObjSymbol
*****************************************************************************}

    constructor TObjSymbol.create(AList:TFPHashObjectList;const AName:string);
      begin;
        inherited create(AList,AName);
        bind:=AB_EXTERNAL;
        typ:=AT_NONE;
        symidx:=-1;
        size:=0;
        offset:=0;
        objsection:=nil;
      end;


    function TObjSymbol.address:aword;
      begin
        if assigned(objsection) then
          result:=offset+objsection.mempos
        else
          result:=0;
      end;


    procedure TObjSymbol.SetAddress(apass:byte;aobjsec:TObjSection;abind:TAsmsymbind;atyp:Tasmsymtype);
      begin
        if not(abind in [AB_GLOBAL,AB_LOCAL,AB_COMMON]) then
          internalerror(200603016);
        if not assigned(aobjsec) then
          internalerror(200603017);
        if (bind=AB_EXTERNAL) then
          begin
            bind:=abind;
            typ:=atyp;
          end
        else
          begin
            if pass=apass then
              begin
                Message1(asmw_e_duplicate_label,name);
                exit;
              end;
          end;
        pass:=apass;
        { Code can never grow after a pass }
        if assigned(objsection) and
           (objsection=aobjsec) and
           (aobjsec.size>offset) then
          internalerror(200603014);
        objsection:=aobjsec;
        offset:=aobjsec.size;
      end;

{****************************************************************************
                              TObjRelocation
****************************************************************************}

    constructor TObjRelocation.CreateSymbol(ADataOffset:aword;s:TObjSymbol;Atyp:TObjRelocationType);
      begin
        if not assigned(s) then
          internalerror(200603034);
        DataOffset:=ADataOffset;
        Symbol:=s;
        OrgSize:=0;
        ObjSection:=nil;
        Typ:=Atyp;
      end;


    constructor TObjRelocation.CreateSymbolSize(ADataOffset:aword;s:TObjSymbol;Aorgsize:aword;Atyp:TObjRelocationType);
      begin
        if not assigned(s) then
          internalerror(200603035);
        DataOffset:=ADataOffset;
        Symbol:=s;
        OrgSize:=Aorgsize;
        ObjSection:=nil;
        Typ:=Atyp;
      end;


    constructor TObjRelocation.CreateSection(ADataOffset:aword;aobjsec:TObjSection;Atyp:TObjRelocationType);
      begin
        if not assigned(aobjsec) then
          internalerror(200603036);
        DataOffset:=ADataOffset;
        Symbol:=nil;
        OrgSize:=0;
        ObjSection:=aobjsec;
        Typ:=Atyp;
      end;


{****************************************************************************
                              TObjSection
****************************************************************************}

    constructor TObjSection.create(AList:TFPHashObjectList;const Aname:string;Aalign:shortint;Aoptions:TObjSectionOptions);
      begin
        inherited Create(AList,Aname);
        { Data }
        Size:=0;
        Datapos:=0;
        mempos:=0;
        FData:=Nil;
        { Setting the secoptions allocates Data if needed }
        secoptions:=Aoptions;
        secalign:=Aalign;
        secsymidx:=0;
        { relocation }
        ObjRelocations:=TFPObjectList.Create(true);
        ObjSymbolDefines:=TFPObjectList.Create(false);
        VTRefList:=TFPObjectList.Create(false);
      end;


    destructor TObjSection.destroy;
      begin
        if assigned(Data) then
          Data.Free;
        stringdispose(FCachedFullName);
        ObjRelocations.Free;
        ObjSymbolDefines.Free;
        VTRefList.Free;
        inherited destroy;
      end;


    procedure TObjSection.SetSecOptions(Aoptions:TObjSectionOptions);
      begin
        FSecOptions:=FSecOptions+AOptions;
        if (oso_Data in secoptions) and
           not assigned(FData) then
          FData:=TDynamicArray.Create(SectionDataMaxGrow);
      end;


    function TObjSection.write(const d;l:aword):aword;
      begin
        result:=size;
        if assigned(Data) then
          begin
            if Size<>Data.size then
              internalerror(200602281);
            Data.write(d,l);
            inc(Size,l);
          end
        else
          internalerror(200602289);
      end;


    function TObjSection.writestr(const s:string):aword;
      begin
        result:=Write(s[1],length(s));
      end;


    function TObjSection.WriteZeros(l:longword):aword;
      var
        empty : array[0..1023] of byte;
      begin
        if l>sizeof(empty) then
          internalerror(200404082);
        if l>0 then
          begin
            fillchar(empty,l,0);
            result:=Write(empty,l);
          end
        else
          result:=Size;
      end;


    procedure TObjSection.setDatapos(var dpos:aword);
      begin
        if oso_Data in secoptions then
          begin
            { get aligned Datapos }
            Datapos:=align(dpos,secalign);
            Dataalignbytes:=Datapos-dpos;
            { return updated Datapos }
            dpos:=Datapos+size;
          end
        else
          Datapos:=dpos;
      end;


    function TObjSection.setmempos(mpos:qword):qword;
      begin
        mempos:=align(mpos,secalign);
        { return updated mempos }
        result:=mempos+size;
      end;


    procedure TObjSection.alloc(l:aword);
      begin
        inc(size,l);
      end;


    procedure TObjSection.addsymReloc(ofs:aword;p:TObjSymbol;Reloctype:TObjRelocationType);
      begin
        ObjRelocations.Add(TObjRelocation.CreateSymbol(ofs,p,reloctype));
      end;


    procedure TObjSection.addsectionReloc(ofs:aword;aobjsec:TObjSection;Reloctype:TObjRelocationType);
      begin
        ObjRelocations.Add(TObjRelocation.CreateSection(ofs,aobjsec,reloctype));
      end;


    procedure TObjSection.AddSymbolDefine(p:TObjSymbol);
      begin
        if p.bind<>AB_GLOBAL then
          exit;
        ObjSymbolDefines.Add(p);
      end;


    procedure TObjSection.FixupRelocs;
      begin
      end;


    procedure TObjSection.ReleaseData;
      begin
        if assigned(FData) then
          begin
            FData.free;
            FData:=nil;
          end;
        ObjRelocations.free;
        ObjRelocations:=nil;
        ObjSymbolDefines.Free;
        ObjSymbolDefines:=nil;
        if assigned(FCachedFullName) then
          begin
            stringdispose(FCachedFullName);
            FCachedFullName:=nil;
          end;
      end;


    function  TObjSection.FullName:string;
      begin
        if not assigned(FCachedFullName) then
          begin
            if assigned(ObjData) then
              FCachedFullName:=stringdup(ObjData.Name+'('+Name+')')
            else
              FCachedFullName:=stringdup(Name);
          end;
        result:=FCachedFullName^;
      end;


{****************************************************************************
                                TObjData
****************************************************************************}

    constructor TObjData.create(const n:string);
      begin
        inherited create;
        FName:=ExtractFileName(n);
        FObjSectionList:=TFPHashObjectList.Create(true);
        FStabsObjSec:=nil;
        FStabStrObjSec:=nil;
        { symbols }
        FObjSymbolList:=TFPHashObjectList.Create(true);
        FCachedAsmSymbolList:=TFPObjectList.Create(false);
        { section class type for creating of new sections }
        FCObjSection:=TObjSection;
      end;


    destructor TObjData.destroy;
      begin
        { Symbols }
{$ifdef MEMDEBUG}
        MemObjSymbols.Start;
{$endif}
        ResetCachedAsmSymbols;
        FCachedAsmSymbolList.free;
        FObjSymbolList.free;
{$ifdef MEMDEBUG}
        MemObjSymbols.Stop;
{$endif}
        { Sections }
{$ifdef MEMDEBUG}
        MemObjSections.Start;
{$endif}
        FObjSectionList.free;
{$ifdef MEMDEBUG}
        MemObjSections.Stop;
{$endif}
        inherited destroy;
      end;


    function TObjData.sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;
      const
        secnames : array[TAsmSectiontype] of string[16] = ('',
          'code',
          'Data',
          'Data',
          'roData',
          'bss',
          'threadvar',
          'pdata',
          'stub',
          'stab','stabstr',
          'iData2','iData4','iData5','iData6','iData7','eData',
          'eh_frame',
          'debug_frame','debug_info','debug_line','debug_abbrev',
          'fpc',
          'toc',
          'init',
          'fini'
        );
      var
        sep : string[3];
      begin
        if aname<>'' then
          begin
            case aorder of
              secorder_begin :
                sep:='.b_';
              secorder_end :
                sep:='.z_';
              else
                sep:='.n_';
            end;
            result:=secnames[atype]+sep+aname
          end
        else
          result:=secnames[atype];
      end;


    function TObjData.sectiontype2options(atype:TAsmSectiontype):TObjSectionOptions;
      const
        secoptions : array[TAsmSectiontype] of TObjSectionOptions = ([],
          {code} [oso_Data,oso_load,oso_readonly,oso_executable,oso_keep],
          {Data} [oso_Data,oso_load,oso_write,oso_keep],
{ TODO: Fix sec_rodata be read-only-with-relocs}
          {roData} [oso_Data,oso_load,oso_write,oso_keep],
{ TODO: Fix sec_rodata_norel be read-only/constant}
          {roData_norel} [oso_Data,oso_load,oso_write,oso_keep],
          {bss} [oso_load,oso_write,oso_keep],
          {threadvar} [oso_load,oso_write],
          {pdata} [oso_load,oso_readonly,oso_keep],
          {stub} [oso_Data,oso_load,oso_readonly,oso_executable],
          {stab} [oso_Data,oso_noload,oso_debug],
          {stabstr} [oso_Data,oso_noload,oso_strings,oso_debug],
          {iData2} [oso_Data,oso_load,oso_write],
          {iData4} [oso_Data,oso_load,oso_write],
          {iData5} [oso_Data,oso_load,oso_write],
          {iData6} [oso_Data,oso_load,oso_write],
          {iData7} [oso_Data,oso_load,oso_write],
          {eData} [oso_Data,oso_load,oso_readonly],
          {eh_frame} [oso_Data,oso_load,oso_readonly],
          {debug_frame} [oso_Data,oso_noload,oso_debug],
          {debug_info} [oso_Data,oso_noload,oso_debug],
          {debug_line} [oso_Data,oso_noload,oso_debug],
          {debug_abbrev} [oso_Data,oso_noload,oso_debug],
          {fpc} [oso_Data,oso_load,oso_write,oso_keep],
          {toc} [oso_Data,oso_load,oso_readonly],
          {init} [oso_Data,oso_load,oso_readonly,oso_executable,oso_keep],
          {fini} [oso_Data,oso_load,oso_readonly,oso_executable,oso_keep]
        );
      begin
        result:=secoptions[atype];
      end;


    function TObjData.sectiontype2align(atype:TAsmSectiontype):shortint;
      begin
        case atype of
          sec_stabstr,sec_debug_info,sec_debug_line,sec_debug_abbrev:
            result:=1;
          sec_code,
          sec_bss,
          sec_data:
            result:=16;
          { For idata (at least idata2) it must be 4 bytes, because
            an entry is always (also in win64) 20 bytes and aligning
            on 8 bytes will insert 4 bytes between the entries resulting
            in a corrupt idata section }
          sec_idata2,sec_idata4,sec_idata5,sec_idata6,sec_idata7:
            result:=4;
          else
            result:=sizeof(pint);
        end;
      end;


    function TObjData.createsection(atype:TAsmSectionType;const aname:string;aorder:TAsmSectionOrder):TObjSection;
      begin
        result:=createsection(sectionname(atype,aname,aorder),sectiontype2align(atype),sectiontype2options(atype));
      end;


    function TObjData.createsection(const aname:string;aalign:shortint;aoptions:TObjSectionOptions;DiscardDuplicate:boolean):TObjSection;
      begin
        if DiscardDuplicate then
          result:=TObjSection(FObjSectionList.Find(aname))
        else
          result:=nil;
        if not assigned(result) then
          begin
            result:=CObjSection.create(FObjSectionList,aname,aalign,aoptions);
            result.ObjData:=self;
          end;
        FCurrObjSec:=result;
      end;


    procedure TObjData.CreateDebugSections;
      begin
      end;


    function TObjData.FindSection(const aname:string):TObjSection;
      begin
        result:=TObjSection(FObjSectionList.Find(aname));
      end;


    procedure TObjData.setsection(asec:TObjSection);
      begin
        if asec.ObjData<>self then
          internalerror(200403041);
        FCurrObjSec:=asec;
      end;


    function TObjData.createsymbol(const aname:string):TObjSymbol;
      begin
        result:=TObjSymbol(FObjSymbolList.Find(aname));
        if not assigned(result) then
          result:=TObjSymbol.Create(FObjSymbolList,aname);
      end;


    function TObjData.symboldefine(asmsym:TAsmSymbol):TObjSymbol;
      begin
        if assigned(asmsym) then
          begin
            if not assigned(asmsym.cachedObjSymbol) then
              begin
                result:=symboldefine(asmsym.name,asmsym.bind,asmsym.typ);
                asmsym.cachedObjSymbol:=result;
                FCachedAsmSymbolList.add(asmsym);
              end
            else
              begin
                result:=TObjSymbol(asmsym.cachedObjSymbol);
                result.SetAddress(CurrPass,CurrObjSec,asmsym.bind,asmsym.typ);
                { Register also in TObjSection }
                CurrObjSec.AddSymbolDefine(result);
              end;
          end
        else
          result:=nil;
      end;


    function TObjData.symboldefine(const aname:string;abind:TAsmsymbind;atyp:Tasmsymtype):TObjSymbol;
      begin
        if not assigned(CurrObjSec) then
          internalerror(200603051);
        result:=CreateSymbol(aname);
        { Register also in TObjSection }
        CurrObjSec.AddSymbolDefine(result);
        result.SetAddress(CurrPass,CurrObjSec,abind,atyp);
      end;


    function TObjData.symbolref(asmsym:TAsmSymbol):TObjSymbol;
      begin
        if assigned(asmsym) then
          begin
            if not assigned(asmsym.cachedObjSymbol) then
              begin
                result:=symbolref(asmsym.name);
                asmsym.cachedObjSymbol:=result;
                FCachedAsmSymbolList.add(asmsym);
              end
            else
              result:=TObjSymbol(asmsym.cachedObjSymbol);
          end
        else
          result:=nil;
      end;


    function TObjData.symbolref(const aname:string):TObjSymbol;
      begin
        if not assigned(CurrObjSec) then
          internalerror(200603052);
        result:=CreateSymbol(aname);
      end;


    procedure TObjData.ResetCachedAsmSymbols;
      var
        i  : longint;
      begin
        for i:=0 to FCachedAsmSymbolList.Count-1 do
          tasmsymbol(FCachedAsmSymbolList[i]).cachedObjSymbol:=nil;
        FCachedAsmSymbolList.Clear;
      end;


    procedure TObjData.writebytes(const Data;len:aword);
      begin
        if not assigned(CurrObjSec) then
          internalerror(200402251);
        CurrObjSec.write(Data,len);
      end;


    procedure TObjData.alloc(len:aword);
      begin
        if not assigned(CurrObjSec) then
          internalerror(200402252);
        CurrObjSec.alloc(len);
      end;


    procedure TObjData.allocalign(len:shortint);
      begin
        if not assigned(CurrObjSec) then
          internalerror(200402253);
        CurrObjSec.alloc(align(CurrObjSec.size,len)-CurrObjSec.size);
      end;


    procedure TObjData.section_afteralloc(p:TObject;arg:pointer);
      begin
        with TObjSection(p) do
          alloc(align(size,secalign)-size);
      end;


    procedure TObjData.section_afterwrite(p:TObject;arg:pointer);
      begin
        with TObjSection(p) do
          begin
            if assigned(Data) then
              writezeros(align(size,secalign)-size);
          end;
      end;


    procedure TObjData.section_reset(p:TObject;arg:pointer);
      begin
        with TObjSection(p) do
          begin
            Size:=0;
            Datapos:=0;
            mempos:=0;
          end;
      end;


    procedure TObjData.beforealloc;
      begin
        { create stabs sections if debugging }
        if assigned(StabsSec) then
          begin
            StabsSec.Alloc(sizeof(TObjStabEntry));
            StabStrSec.Alloc(1);
          end;
      end;


    procedure TObjData.beforewrite;
      var
        s : string[1];
        hstab : TObjStabEntry;
      begin
        { create stabs sections if debugging }
        if assigned(StabsSec) then
         begin
           { Create dummy HdrSym stab, it will be overwritten in AfterWrite }
           fillchar(hstab,sizeof(hstab),0);
           StabsSec.Write(hstab,sizeof(hstab));
           { start of stabstr }
           s:=#0;
           StabStrSec.write(s[1],length(s));
         end;
      end;


    procedure TObjData.afteralloc;
      begin
        FObjSectionList.ForEachCall(@section_afteralloc,nil);
      end;


    procedure TObjData.afterwrite;
      var
        s : string[1];
        hstab : TObjStabEntry;
      begin
        FObjSectionList.ForEachCall(@section_afterwrite,nil);
        { For the stab section we need an HdrSym which can now be
          calculated more easily }
        if assigned(StabsSec) then
          begin
            { end of stabstr }
            s:=#0;
            StabStrSec.write(s[1],length(s));
            { header stab }
            hstab.strpos:=1;
            hstab.ntype:=0;
            hstab.nother:=0;
            hstab.ndesc:=(StabsSec.Size div sizeof(TObjStabEntry))-1;
            hstab.nvalue:=StabStrSec.Size;
            StabsSec.Data.seek(0);
            StabsSec.Data.write(hstab,sizeof(hstab));
          end;
      end;


    procedure TObjData.resetsections;
      begin
        FObjSectionList.ForEachCall(@section_reset,nil);
      end;


{****************************************************************************
                                TObjOutput
****************************************************************************}

    constructor TObjOutput.create(AWriter:TObjectWriter);
      begin
        FWriter:=AWriter;
        CObjData:=TObjData;
      end;


    destructor TObjOutput.destroy;
      begin
        inherited destroy;
      end;


    function TObjOutput.newObjData(const n:string):TObjData;
      begin
        result:=CObjData.create(n);
        if (cs_use_lineinfo in current_settings.globalswitches) or
           (cs_debuginfo in current_settings.moduleswitches) then
          result.CreateDebugSections;
      end;


    function TObjOutput.startObjectfile(const fn:string):boolean;
      begin
        result:=false;
        { start the writer already, so the .a generation can initialize
          the position of the current objectfile }
        if not FWriter.createfile(fn) then
         Comment(V_Fatal,'Can''t create object '+fn);
        result:=true;
      end;


    function TObjOutput.writeobjectfile(Data:TObjData):boolean;
      begin
        if errorcount=0 then
         result:=writeData(Data)
        else
         result:=true;
        { close the writer }
        FWriter.closefile;
      end;


    procedure TObjOutput.exportsymbol(p:TObjSymbol);
      begin
        { export globals and common symbols, this is needed
          for .a files }
        if p.bind in [AB_GLOBAL,AB_COMMON] then
         FWriter.writesym(p.name);
      end;


{****************************************************************************
                                 TExeVTable
****************************************************************************}

    constructor TExeVTable.Create(AExeSymbol:TExeSymbol);
      begin
        ExeSymbol:=AExeSymbol;
        if ExeSymbol.State=symstate_undefined then
          internalerror(200604012);
        ChildList:=TFPObjectList.Create(false);
      end;


    destructor TExeVTable.Destroy;
      begin
        ChildList.Free;
        if assigned(EntryArray) then
          Freemem(EntryArray);
      end;


    procedure TExeVTable.CheckIdx(VTableIdx:longint);
      var
        OldEntryCnt : longint;
      begin
        if VTableIdx>=EntryCnt then
          begin
            OldEntryCnt:=EntryCnt;
            EntryCnt:=VTableIdx+1;
            ReAllocMem(EntryArray,EntryCnt*sizeof(TVTableEntry));
            FillChar(EntryArray[OldEntryCnt],(EntryCnt-OldEntryCnt)*sizeof(TVTableEntry),0);
          end;
      end;


    procedure TExeVTable.AddChild(vt:TExeVTable);
      begin
        ChildList.Add(vt);
      end;


    procedure TExeVTable.AddEntry(VTableIdx:Longint);
      var
        i : longint;
        objreloc : TObjRelocation;
        vtblentryoffset : aword;
      begin
        CheckIdx(VTableIdx);
        vtblentryoffset:=ExeSymbol.ObjSymbol.Offset+longword(VTableIdx)*sizeof(pint);
        { Find and disable relocation }
        for i:=0 to ExeSymbol.ObjSymbol.ObjSection.ObjRelocations.Count-1 do
          begin
            objreloc:=TObjRelocation(ExeSymbol.ObjSymbol.ObjSection.ObjRelocations[i]);
            if objreloc.dataoffset=vtblentryoffset then
              begin
                EntryArray[VTableIdx].ObjRelocation:=objreloc;
                EntryArray[VTableIdx].OrgRelocType:=objreloc.typ;
                objreloc.typ:=RELOC_ZERO;
                break;
              end;
          end;
        if not assigned(EntryArray[VTableIdx].ObjRelocation) then
          internalerror(200604011);
      end;


    procedure TExeVTable.SetVTableSize(ASize:longint);
      begin
        if EntryCnt<>0 then
          internalerror(200603313);
        EntryCnt:=ASize div sizeof(pint);
        EntryArray:=AllocMem(EntryCnt*sizeof(TVTableEntry));
      end;


    function TExeVTable.VTableRef(VTableIdx:Longint):TObjRelocation;
      begin
        result:=nil;
        CheckIdx(VTableIdx);
        if EntryArray[VTableIdx].Used then
          exit;
        { Restore relocation if available }
        if assigned(EntryArray[VTableIdx].ObjRelocation) then
          begin
            EntryArray[VTableIdx].ObjRelocation.typ:=EntryArray[VTableIdx].OrgRelocType;
            result:=EntryArray[VTableIdx].ObjRelocation;
          end;
        EntryArray[VTableIdx].Used:=true;
      end;


{****************************************************************************
                                TExeSection
****************************************************************************}

    constructor TExeSection.create(AList:TFPHashObjectList;const AName:string);
      begin
        inherited create(AList,AName);
        Size:=0;
        MemPos:=0;
        DataPos:=0;
        FSecSymIdx:=0;
        FObjSectionList:=TFPObjectList.Create(false);
      end;


    destructor TExeSection.destroy;
      begin
        ObjSectionList.Free;
        inherited destroy;
      end;


    procedure TExeSection.AddObjSection(objsec:TObjSection);
      begin
        ObjSectionList.Add(objsec);
        if (SecOptions<>[]) then
          begin
            { Only if the section contains (un)initialized data the
              data flag must match. This check is not needed if the
              section is empty for a symbol allocation }
            if (objsec.size>0) and
               ((oso_Data in SecOptions)<>(oso_Data in objsec.SecOptions)) then
              Comment(V_Error,'Incompatible section options');
          end
        else
          begin
            { inherit section options }
            SecAlign:=objsec.SecAlign;
            SecOptions:=SecOptions+objsec.SecOptions;
          end;
        { relate ObjSection to ExeSection, and mark it Used by default }
        objsec.ExeSection:=self;
        objsec.Used:=true;
      end;


{****************************************************************************
                                TStaticLibrary
****************************************************************************}

    constructor TStaticLibrary.create(AList:TFPHashObjectList;const AName:string;AReader:TObjectReader;AObjInputClass:TObjInputClass);
      begin
        inherited create(AList,AName);
        FArReader:=AReader;
        FObjInputClass:=AObjInputClass;
      end;


    destructor TStaticLibrary.destroy;
      begin
        ArReader.Free;
        inherited destroy;
      end;


{****************************************************************************
                                TImportLibrary
****************************************************************************}

    constructor TImportLibrary.create(AList:TFPHashObjectList;const AName:string);
      begin
        inherited create(AList,AName);
        FImportSymbolList:=TFPHashObjectList.Create(true);
      end;


    destructor TImportLibrary.destroy;
      begin
        ImportSymbolList.Free;
        inherited destroy;
      end;


{****************************************************************************
                                TImportSymbol
****************************************************************************}

    constructor TImportSymbol.create(AList:TFPHashObjectList;const AName:string;AOrdNr:longint;AIsVar:boolean);
      begin
        inherited Create(AList, AName);
        FOrdNr:=AOrdNr;
        FIsVar:=AIsVar;
        FMangledName:=AName;
        { Replace ? and @ in import name, since GNU AS does not allow these characters in symbol names. }
        { This allows to import VC++ mangled names from DLLs. }
        if target_info.system in system_all_windows then
          begin
            Replace(FMangledName,'?','__q$$');
{$ifdef arm}
            { @ symbol is not allowed in ARM assembler only }
            Replace(FMangledName,'@','__a$$');
{$endif arm}
          end;
      end;


{****************************************************************************
                                TExeOutput
****************************************************************************}

    constructor TExeOutput.create;
      begin
        { init writer }
        FWriter:=TObjectwriter.create;
        FExeWriteMode:=ewm_exefull;
        { object files }
        FObjDataList:=TFPObjectList.Create(true);
        { symbols }
        FExeSymbolList:=TFPHashObjectList.Create(true);
        FUnresolvedExeSymbols:=TFPObjectList.Create(false);
        FExternalObjSymbols:=TFPObjectList.Create(false);
        FCommonObjSymbols:=TFPObjectList.Create(false);
        FExeVTableList:=TFPObjectList.Create(false);
        FEntryName:='start';
        { sections }
        FExeSectionList:=TFPHashObjectList.Create(true);
        FImageBase:=0;
        SectionMemAlign:=$1000;
        SectionDataAlign:=$200;
        FCExeSection:=TExeSection;
        FCObjData:=TObjData;
      end;


    destructor TExeOutput.destroy;
      begin
        FExeSymbolList.free;
        UnresolvedExeSymbols.free;
        ExternalObjSymbols.free;
        CommonObjSymbols.free;
        ExeVTableList.free;
        FExeSectionList.free;
        ObjDatalist.free;
        FWriter.free;
        inherited destroy;
      end;


    function TExeOutput.WriteExeFile(const fn:string):boolean;
      begin
        result:=false;
        if FWriter.createfile(fn) then
         begin
           { Only write the .o if there are no errors }
           if errorcount=0 then
             result:=writedata
           else
             result:=true;
           { close the writer }
           FWriter.closefile;
         end
        else
         Comment(V_Fatal,'Can''t create executable '+fn);
      end;


    function  TExeOutput.FindExeSection(const aname:string):TExeSection;
      begin
        result:=TExeSection(ExeSectionList.Find(aname));
      end;


    procedure TExeOutput.AddObjData(ObjData:TObjData);
      begin
        if ObjData.classtype<>FCObjData then
          Comment(V_Error,'Invalid input object format for '+ObjData.name+' got '+ObjData.classname+' expected '+FCObjData.classname);
        ObjDataList.Add(ObjData);
      end;


    procedure TExeOutput.Load_Start;
      begin
        ObjDataList.Clear;
        { Globals defined in the linker script }
        if not assigned(internalObjData) then
          internalObjData:=CObjData.create('*Internal*');
        AddObjData(internalObjData);
        { Common Data section }
        commonObjSection:=internalObjData.createsection(sec_bss,'');
      end;


    procedure TExeOutput.Load_EntryName(const aname:string);
      begin
        EntryName:=aname;
      end;


    procedure TExeOutput.Load_IsSharedLibrary;
      begin
        IsSharedLibrary:=true;
      end;


    procedure TExeOutput.Load_ImageBase(const avalue:string);
      var
        code : integer;
        objsec : TObjSection;
        objsym : TObjSymbol;
        exesym : TExeSymbol;
      begin
        val(avalue,ImageBase,code);
        if code<>0 then
          Comment(V_Error,'Invalid number '+avalue);
        { Create __image_base__ symbol, create the symbol
          in a section with adress 0 and at offset 0 }
        objsec:=internalObjData.createsection('*__image_base__',0,[]);
        internalObjData.setsection(objsec);
        objsym:=internalObjData.SymbolDefine('__image_base__',AB_GLOBAL,AT_FUNCTION);
        exesym:=texesymbol.Create(FExeSymbolList,objsym.name);
        exesym.ObjSymbol:=objsym;
      end;


    procedure TExeOutput.Load_Symbol(const aname:string);
      begin
        internalObjData.createsection('*'+aname,0,[]);
        internalObjData.SymbolDefine(aname,AB_GLOBAL,AT_FUNCTION);
      end;


    procedure TExeOutput.Order_Start;
      begin
      end;


    procedure TExeOutput.Order_End;
      begin
        internalObjData.afterwrite;
      end;


    procedure TExeOutput.Order_ExeSection(const aname:string);
      var
        sec : TExeSection;
      begin
        sec:=FindExeSection(aname);
        if not assigned(sec) then
          sec:=CExeSection.create(ExeSectionList,aname);
        { Clear ExeSection contents }
        FCurrExeSec:=sec;
      end;


    procedure TExeOutput.Order_EndExeSection;
      begin
        if not assigned(CurrExeSec) then
          internalerror(200602184);
        FCurrExeSec:=nil;
      end;


    procedure TExeOutput.Order_ObjSection(const aname:string);
      var
        i,j     : longint;
        ObjData : TObjData;
        objsec  : TObjSection;
        TmpObjSectionList : TFPObjectList;
      begin
        if not assigned(CurrExeSec) then
          internalerror(200602181);
        TmpObjSectionList:=TFPObjectList.Create(false);
        for i:=0 to ObjDataList.Count-1 do
          begin
            ObjData:=TObjData(ObjDataList[i]);
            for j:=0 to ObjData.ObjSectionList.Count-1 do
              begin
                objsec:=TObjSection(ObjData.ObjSectionList[j]);
                if (not objsec.Used) and
                   MatchPattern(aname,objsec.name) then
                  TmpObjSectionList.Add(objsec);
              end;
          end;
        { Order list if needed }
        Order_ObjSectionList(TmpObjSectionList);
        { Add the (ordered) list to the current ExeSection }
        for i:=0 to TmpObjSectionList.Count-1 do
          begin
            objsec:=TObjSection(TmpObjSectionList[i]);
            CurrExeSec.AddObjSection(objsec);
          end;
        TmpObjSectionList.Free;
      end;


    procedure TExeOutput.Order_ObjSectionList(ObjSectionList : TFPObjectList);
      begin
      end;


    procedure TExeOutput.Order_Symbol(const aname:string);
      var
        ObjSection : TObjSection;
      begin
        ObjSection:=internalObjData.findsection('*'+aname);
        if not assigned(ObjSection) then
          internalerror(200603041);
        CurrExeSec.AddObjSection(ObjSection);
      end;


    procedure TExeOutput.Order_Align(const avalue:string);
      var
        code     : integer;
        alignval : shortint;
        objsec   : TObjSection;
      begin
        val(avalue,alignval,code);
        if code<>0 then
          Comment(V_Error,'Invalid number '+avalue);
        if alignval<=0 then
          exit;
        { Create an empty section with the required aligning }
        inc(Fzeronr);
        objsec:=internalObjData.createsection('*align'+tostr(Fzeronr),alignval,CurrExeSec.SecOptions+[oso_Data,oso_keep]);
        CurrExeSec.AddObjSection(objsec);
      end;


    procedure TExeOutput.Order_Zeros(const avalue:string);
      var
        zeros : array[0..1023] of byte;
        code  : integer;
        len   : longint;
        objsec : TObjSection;
      begin
        val(avalue,len,code);
        if code<>0 then
          Comment(V_Error,'Invalid number '+avalue);
        if len<=0 then
          exit;
        if len>sizeof(zeros) then
          internalerror(200602254);
        fillchar(zeros,len,0);
        inc(Fzeronr);
        objsec:=internalObjData.createsection('*zeros'+tostr(Fzeronr),0,CurrExeSec.SecOptions+[oso_Data,oso_keep]);
        internalObjData.writebytes(zeros,len);
        CurrExeSec.AddObjSection(objsec);
      end;


    procedure TExeOutput.MemPos_Start;
      begin
        CurrMemPos:=0;
      end;


    procedure TExeOutput.MemPos_Header;
      begin
      end;


    procedure TExeOutput.MemPos_ExeSection(const aname:string);
      var
        i      : longint;
        objsec : TObjSection;
      begin
        { Section can be removed }
        FCurrExeSec:=FindExeSection(aname);
        if not assigned(CurrExeSec) then
          exit;

        { Alignment of ExeSection }
        CurrMemPos:=align(CurrMemPos,SectionMemAlign);
        CurrExeSec.MemPos:=CurrMemPos;

        { set position of object ObjSections }
        for i:=0 to CurrExeSec.ObjSectionList.Count-1 do
          begin
            objsec:=TObjSection(CurrExeSec.ObjSectionList[i]);
            CurrMemPos:=objsec.setmempos(CurrMemPos);
          end;

        { calculate size of the section }
        CurrExeSec.Size:=CurrMemPos-CurrExeSec.MemPos;
      end;


    procedure TExeOutput.MemPos_EndExeSection;
      begin
        if not assigned(CurrExeSec) then
          exit;
        FCurrExeSec:=nil;
      end;


    procedure TExeOutput.DataPos_Start;
      begin
      end;


    procedure TExeOutput.DataPos_Header;
      begin
      end;


    procedure TExeOutput.DataPos_ExeSection(const aname:string);
      var
        i      : longint;
        objsec : TObjSection;
      begin
        { Section can be removed }
        FCurrExeSec:=FindExeSection(aname);
        if not assigned(CurrExeSec) then
          exit;

        { don't write normal section if writing only debug info }
        if (ExeWriteMode=ewm_dbgonly) and
           not(oso_debug in CurrExeSec.SecOptions) then
          exit;

        if (oso_Data in currexesec.SecOptions) then
          begin
            CurrDataPos:=align(CurrDataPos,SectionDataAlign);
            CurrExeSec.DataPos:=CurrDataPos;
          end;

        { set position of object ObjSections }
        for i:=0 to CurrExeSec.ObjSectionList.Count-1 do
          begin
            objsec:=TObjSection(CurrExeSec.ObjSectionList[i]);
            if (oso_Data in objsec.SecOptions) then
              begin
                if not(oso_Data in currexesec.SecOptions) then
                  internalerror(200603043);
                if not assigned(objsec.Data) then
                  internalerror(200603044);
                objsec.setDatapos(CurrDataPos);
              end;
          end;
      end;


    procedure TExeOutput.DataPos_EndExeSection;
      begin
        if not assigned(CurrExeSec) then
          exit;
        FCurrExeSec:=nil;
      end;


    procedure TExeOutput.DataPos_Symbols;
      var
        i : longint;
        sym : TExeSymbol;
      begin
        { Removing unused symbols }
        for i:=0 to ExeSymbolList.Count-1 do
          begin
            sym:=TExeSymbol(ExeSymbolList[i]);
            if not sym.ObjSymbol.objsection.Used then
              ExeSymbolList[i]:=nil;
          end;
        ExeSymbolList.Pack;
      end;


    procedure TExeOutput.BuildVTableTree(VTInheritList,VTEntryList:TFPObjectList);
      var
        hs : string;
        code : integer;
        i,k,
        vtableidx : longint;
        vtableexesym,
        childexesym,
        parentexesym : TExeSymbol;
        objsym : TObjSymbol;
      begin
        { Build inheritance tree from VTINHERIT }
        for i:=0 to VTInheritList.Count-1 do
          begin
            objsym:=TObjSymbol(VTInheritList[i]);
            hs:=objsym.name;
            { VTINHERIT_<ChildVMTName>$$<ParentVMTName> }
            Delete(hs,1,Pos('_',hs));
            k:=Pos('$$',hs);
            if k=0 then
              internalerror(200603311);
            childexesym:=texesymbol(FExeSymbolList.Find(Copy(hs,1,k-1)));
            parentexesym:=texesymbol(FExeSymbolList.Find(Copy(hs,k+2,length(hs)-k-1)));
            if not assigned(childexesym) or
               not assigned(parentexesym)then
              internalerror(200603312);
            if not assigned(childexesym.vtable) then
              begin
                childexesym.vtable:=TExeVTable.Create(childexesym);
                ExeVTableList.Add(childexesym.vtable);
              end;
            if not assigned(parentexesym.vtable) then
              begin
                parentexesym.vtable:=TExeVTable.Create(parentexesym);
                ExeVTableList.Add(parentexesym.vtable);
              end;
            childexesym.vtable.SetVTableSize(childexesym.ObjSymbol.Size);
            if parentexesym<>childexesym then
              parentexesym.vtable.AddChild(childexesym.vtable);
          end;

        { Find VTable entries from VTENTRY }
        for i:=0 to VTEntryList.Count-1 do
          begin
            objsym:=TObjSymbol(VTEntryList[i]);
            hs:=objsym.name;
            { VTENTRY_<VTableName>$$<Index> }
            Delete(hs,1,Pos('_',hs));
            k:=Pos('$$',hs);
            if k=0 then
              internalerror(200603319);
            vtableexesym:=texesymbol(FExeSymbolList.Find(Copy(hs,1,k-1)));
            val(Copy(hs,k+2,length(hs)-k-1),vtableidx,code);
            if (code<>0) then
              internalerror(200603318);
            if not assigned(vtableexesym) then
              internalerror(2006033110);
            vtableexesym.vtable.AddEntry(vtableidx);
          end;
      end;


    procedure TExeOutput.PackUnresolvedExeSymbols(const s:string);
      var
        i : longint;
        exesym : TExeSymbol;
      begin
        { Generate a list of Unresolved External symbols }
        for i:=0 to UnresolvedExeSymbols.count-1 do
          begin
            exesym:=TExeSymbol(UnresolvedExeSymbols[i]);
            if exesym.State<>symstate_undefined then
              UnresolvedExeSymbols[i]:=nil;
          end;
        UnresolvedExeSymbols.Pack;
        Comment(V_Debug,'Number of unresolved externals '+s+' '+tostr(UnresolvedExeSymbols.Count));
      end;


    procedure TExeOutput.ResolveSymbols(StaticLibraryList:TFPHashObjectList);
      var
        ObjData   : TObjData;
        exesym    : TExeSymbol;
        objsym,
        commonsym : TObjSymbol;
        objinput : TObjInput;
        StaticLibrary : TStaticLibrary;
        firstarchive,
        firstcommon : boolean;
        i,j       : longint;
        VTEntryList,
        VTInheritList : TFPObjectList;

        procedure LoadObjDataSymbols(ObjData:TObjData);
        var
          j      : longint;
          hs     : string;
          exesym : TExeSymbol;
          objsym : TObjSymbol;
        begin
          for j:=0 to ObjData.ObjSymbolList.Count-1 do
            begin
              objsym:=TObjSymbol(ObjData.ObjSymbolList[j]);
              { From the local symbols we are only interressed in the
                VTENTRY and VTINHERIT symbols }
              if objsym.bind=AB_LOCAL then
                begin
                  if cs_link_opt_vtable in current_settings.globalswitches then
                    begin
                      hs:=objsym.name;
                      if (hs[1]='V') then
                        begin
                          if Copy(hs,1,5)='VTREF' then
                            begin
                              if not assigned(objsym.ObjSection.VTRefList) then
                                objsym.ObjSection.VTRefList:=TFPObjectList.Create(false);
                              objsym.ObjSection.VTRefList.Add(objsym);
                            end
                          else if Copy(hs,1,7)='VTENTRY' then
                            VTEntryList.Add(objsym)
                          else if Copy(hs,1,9)='VTINHERIT' then
                            VTInheritList.Add(objsym);
                        end;
                    end;
                  continue;
                end;
              { Search for existing exesymbol }
              exesym:=texesymbol(FExeSymbolList.Find(objsym.name));
              if not assigned(exesym) then
                begin
                  exesym:=texesymbol.Create(FExeSymbolList,objsym.name);
                  exesym.ObjSymbol:=objsym;
                end;
              objsym.ExeSymbol:=exesym;
              case objsym.bind of
                AB_GLOBAL :
                  begin
                    if exesym.State<>symstate_defined then
                      begin
                        exesym.ObjSymbol:=objsym;
                        exesym.State:=symstate_defined;
                      end
                    else
                      Comment(V_Error,'Multiple defined symbol '+objsym.name);
                  end;
                AB_EXTERNAL :
                  begin
                    ExternalObjSymbols.add(objsym);
                    { Register unresolved symbols only the first time they
                      are registered }
                    if exesym.ObjSymbol=objsym then
                      UnresolvedExeSymbols.Add(exesym);
                  end;
                AB_COMMON :
                  begin
                    if exesym.State=symstate_undefined then
                      begin
                        exesym.ObjSymbol:=objsym;
                        exesym.State:=symstate_common;
                      end;
                    CommonObjSymbols.add(objsym);
                  end;
              end;
            end;
        end;

      begin
        VTEntryList:=TFPObjectList.Create(false);
        VTInheritList:=TFPObjectList.Create(false);

        {
          The symbol resolving is done in 3 steps:
           1. Register symbols from objects
           2. Find symbols in static libraries
           3. Define stil undefined common symbols
        }

        { Step 1, Register symbols from objects }
        for i:=0 to ObjDataList.Count-1 do
          begin
            ObjData:=TObjData(ObjDataList[i]);
            LoadObjDataSymbols(ObjData);
          end;
        PackUnresolvedExeSymbols('in objects');

        { Step 2, Find unresolved symbols in the libraries }
        firstarchive:=true;
        for i:=0 to StaticLibraryList.Count-1 do
          begin
            StaticLibrary:=TStaticLibrary(StaticLibraryList[i]);
            { Process list of Unresolved External symbols, we need
              to use a while loop because the list can be extended when
              we load members from the library. }
            j:=0;
            while (j<UnresolvedExeSymbols.count) do
              begin
                exesym:=TExeSymbol(UnresolvedExeSymbols[j]);
                { Check first if the symbol is still undefined }
                if exesym.State=symstate_undefined then
                  begin
                    if StaticLibrary.ArReader.OpenFile(exesym.name) then
                      begin
                        if assigned(exemap) then
                          begin
                            if firstarchive then
                              begin
                                exemap.Add('');
                                exemap.Add('Archive member included because of file (symbol)');
                                exemap.Add('');
                                firstarchive:=false;
                              end;
                            exemap.Add(StaticLibrary.ArReader.FileName+' - '+
                              {exesym.ObjSymbol.ObjSection.FullName+}
                              '('+exesym.Name+')');
                          end;
                        objinput:=StaticLibrary.ObjInputClass.Create;
                        objdata:=objinput.newObjData(StaticLibrary.ArReader.FileName);
                        objinput.ReadObjData(StaticLibrary.ArReader,objdata);
                        objinput.free;
                        AddObjData(objdata);
                        LoadObjDataSymbols(objdata);
                        StaticLibrary.ArReader.CloseFile;
                      end;
                   end;
                inc(j);
              end;
          end;
        PackUnresolvedExeSymbols('after static libraries');

        { Step 3, Match common symbols or add to the globals }
        firstcommon:=true;
        for i:=0 to CommonObjSymbols.count-1 do
          begin
            objsym:=TObjSymbol(CommonObjSymbols[i]);
            if objsym.exesymbol.State=symstate_defined then
              begin
                if objsym.exesymbol.ObjSymbol.size<>objsym.size then
                  Comment(V_Debug,'Size of common symbol '+objsym.name+' is different, expected '+tostr(objsym.size)+' got '+tostr(objsym.exesymbol.ObjSymbol.size));
              end
            else
              begin
                { allocate new objsymbol in .bss of *COMMON* and assign
                  it to the exesymbol }
                if firstcommon then
                  begin
                    if assigned(exemap) then
                      exemap.AddCommonSymbolsHeader;
                    firstcommon:=false;
                  end;
                internalObjData.setsection(commonObjSection);
                internalObjData.allocalign(var_align(objsym.size));
                commonsym:=internalObjData.symboldefine(objsym.name,AB_GLOBAL,AT_FUNCTION);
                commonsym.size:=objsym.size;
                internalObjData.alloc(objsym.size);
                if assigned(exemap) then
                  exemap.AddCommonSymbol(commonsym);
                { Assign to the exesymbol }
                objsym.exesymbol.objsymbol:=commonsym;
                objsym.exesymbol.state:=symstate_defined;
              end;
          end;
        PackUnresolvedExeSymbols('after defining COMMON symbols');

        { Find entry symbol and print in map }
        exesym:=texesymbol(ExeSymbolList.Find(EntryName));
        if assigned(exesym) then
          begin
            EntrySym:=exesym.ObjSymbol;
            if assigned(exemap) then
              begin
                exemap.Add('');
                exemap.Add('Entry symbol '+EntryName);
              end;
          end
        else
          Comment(V_Error,'Entrypoint '+EntryName+' not defined');

        { Generate VTable tree }
        if cs_link_opt_vtable in current_settings.globalswitches then
          BuildVTableTree(VTInheritList,VTEntryList);
        VTInheritList.Free;
        VTEntryList.Free;
      end;


    procedure TExeOutput.GenerateDebugLink(const dbgname:string;dbgcrc:cardinal);
      var
        debuglink : array[0..1023] of byte;
        len   : longint;
        objsec : TObjSection;
        exesec : TExeSection;
      begin
        { From the gdb manual  chapter 15. GDB Files:

           * A filename, with any leading directory components removed, followed by a zero byte,
           * zero to three bytes of padding, as needed to reach the next four-byte boundary within the section, and
           * a four-byte CRC checksum, stored in the same endianness used for the executable file itself. The checksum is computed
             on the debugging information file's full contents by the function given below, passing zero as the crc argument.
        }
        fillchar(debuglink,sizeof(debuglink),0);
        len:=0;
        move(dbgname[1],debuglink[len],length(dbgname));
        inc(len,length(dbgname)+1);
        len:=align(len,4);
        if source_info.endian<>target_info.endian then
          SwapEndian(dbgcrc);
        move(dbgcrc,debuglink[len],sizeof(cardinal));
        inc(len,4);
        { Add section }
        exesec:=FindExeSection(debuglinkname);
        if not assigned(exesec) then
          exesec:=CExeSection.create(ExeSectionList,debuglinkname);
        exesec.SecOptions:=[oso_data,oso_keep];
        exesec.SecAlign:=4;
        objsec:=internalObjData.createsection(exesec.name,0,exesec.SecOptions);
        internalObjData.writebytes(debuglink,len);
        exesec.AddObjSection(objsec);
      end;


    procedure TExeOutput.GenerateLibraryImports(ImportLibraryList:TFPHashObjectList);
      begin
      end;


    procedure TExeOutput.PrintMemoryMap;
      var
        exesec : TExeSection;
        objsec : TObjSection;
        objsym : TObjSymbol;
        i,j,k  : longint;
      begin
        if not assigned(exemap) then
          exit;
        exemap.AddMemoryMapHeader(ImageBase);
        for i:=0 to ExeSectionList.Count-1 do
          begin
            exesec:=TExeSection(ExeSectionList[i]);
            exemap.AddMemoryMapExeSection(exesec);
            for j:=0 to exesec.ObjSectionList.count-1 do
              begin
                objsec:=TObjSection(exesec.ObjSectionList[j]);
                exemap.AddMemoryMapObjectSection(objsec);
                for k:=0 to objsec.ObjSymbolDefines.Count-1 do
                  begin
                    objsym:=TObjSymbol(objsec.ObjSymbolDefines[k]);
                    exemap.AddMemoryMapSymbol(objsym);
                  end;
              end;
          end;
      end;


    procedure TExeOutput.FixupSymbols;

        procedure UpdateSymbol(objsym:TObjSymbol);
        begin
          objsym.bind:=objsym.ExeSymbol.ObjSymbol.bind;
          objsym.offset:=objsym.ExeSymbol.ObjSymbol.offset;
          objsym.size:=objsym.ExeSymbol.ObjSymbol.size;
          objsym.typ:=objsym.ExeSymbol.ObjSymbol.typ;
          objsym.ObjSection:=objsym.ExeSymbol.ObjSymbol.ObjSection;
        end;

      var
        i      : longint;
        objsym : TObjSymbol;
        exesym : TExeSymbol;
      begin
        { Print list of Unresolved External symbols }
        for i:=0 to UnresolvedExeSymbols.count-1 do
          begin
            exesym:=TExeSymbol(UnresolvedExeSymbols[i]);
            if exesym.State<>symstate_defined then
              Comment(V_Error,'Undefined symbol: '+exesym.name);
          end;

        { Update ImageBase to ObjData so it can access from ObjSymbols }
        for i:=0 to ObjDataList.Count-1 do
          TObjData(ObjDataList[i]).imagebase:=imagebase;

        {
          Fixing up symbols is done in the following steps:
           1. Update common references
           2. Update external references
        }

        { Step 1, Update commons }
        for i:=0 to CommonObjSymbols.count-1 do
          begin
            objsym:=TObjSymbol(CommonObjSymbols[i]);
            if objsym.bind<>AB_COMMON then
              internalerror(200606241);
            UpdateSymbol(objsym);
          end;

        { Step 2, Update externals }
        for i:=0 to ExternalObjSymbols.count-1 do
          begin
            objsym:=TObjSymbol(ExternalObjSymbols[i]);
            if objsym.bind<>AB_EXTERNAL then
              internalerror(200606242);
            UpdateSymbol(objsym);
          end;
      end;


    procedure TExeOutput.MergeStabs;
      var
        stabexesec,
        stabstrexesec : TExeSection;
        relocsec,
        currstabsec,
        currstabstrsec,
        mergedstabsec,
        mergedstabstrsec : TObjSection;
        hstabreloc,
        currstabreloc : TObjRelocation;
        i,j : longint;
        currstabrelocidx,
        mergestabcnt,
        stabcnt : longword;
        skipstab : boolean;
        skipfun : boolean;
        hstab   : TObjStabEntry;
        stabrelocofs : longword;
        buf     : array[0..1023] of byte;
        bufend,
        bufsize  : longint;
      begin
        stabexesec:=FindExeSection('.stab');
        stabstrexesec:=FindExeSection('.stabstr');
        if (stabexesec=nil) or
           (stabstrexesec=nil) or
           (stabexesec.ObjSectionlist.count=0) then
          exit;
        { Create new stabsection }
        stabRelocofs:=pbyte(@hstab.nvalue)-pbyte(@hstab);
        mergedstabsec:=internalObjData.CreateSection(sec_stab,'');
        mergedstabstrsec:=internalObjData.CreateSection(sec_stabstr,'');

        { write stab for hdrsym }
        fillchar(hstab,sizeof(TObjStabEntry),0);
        mergedstabsec.write(hstab,sizeof(TObjStabEntry));
        mergestabcnt:=1;

        { .stabstr starts with a #0 }
        buf[0]:=0;
        mergedstabstrsec.write(buf[0],1);

        skipfun:=false;
        { Copy stabs and corresponding Relocations }
        for i:=0 to stabexesec.ObjSectionList.Count-1 do
          begin
            currstabsec:=TObjSection(stabexesec.ObjSectionList[i]);
            currstabstrsec:=currstabsec.ObjData.findsection('.stabstr');
            if assigned(currstabstrsec) then
              begin
                stabcnt:=currstabsec.Data.size div sizeof(TObjStabEntry);
                currstabsec.Data.seek(0);
                currstabrelocidx:=0;
                for j:=0 to stabcnt-1 do
                  begin
                    hstabreloc:=nil;
                    skipstab:=false;
                    currstabsec.Data.read(hstab,sizeof(TObjStabEntry));
                    { Only include first hdrsym stab }
                    if hstab.ntype=0 then
                      skipstab:=true;
                    if skipfun then
                      begin
                        { Skip all stabs for function body until N_RBRAC }
                        skipfun:=hstab.ntype<>N_RBRAC;
                        skipstab:=true;
                      end;
                    if not skipstab then
                      begin
                        { Find corresponding Relocation }
                        currstabreloc:=nil;
                        while (currstabrelocidx<longword(currstabsec.ObjRelocations.Count)) do
                          begin
                            currstabreloc:=TObjRelocation(currstabsec.ObjRelocations[currstabrelocidx]);
                            if assigned(currstabreloc) and
                               (currstabreloc.dataoffset>=longword(j)*sizeof(TObjStabEntry)+stabrelocofs) then
                              break;
                            inc(currstabrelocidx);
                          end;
                        if assigned(currstabreloc) and
                           (currstabreloc.dataoffset=longword(j)*sizeof(TObjStabEntry)+stabrelocofs) then
                          begin
                            hstabReloc:=currstabReloc;
                            inc(currstabrelocidx);
                          end;

                        { Check if the stab is refering to a removed section }
                        if assigned(hstabreloc) then
                          begin
                            if assigned(hstabreloc.Symbol) then
                              relocsec:=hstabreloc.Symbol.ObjSection
                            else
                              relocsec:=hstabreloc.ObjSection;
                            if not assigned(relocsec) then
                              internalerror(200603302);
                            if not relocsec.Used then
                              begin
                                skipstab:=true;
                                if (hstab.ntype=N_Function) and (hstab.strpos<>0) then
                                  begin
                                    currstabstrsec.Data.seek(hstab.strpos);
                                    bufsize:=currstabstrsec.Data.read(buf,sizeof(buf));
                                    bufend:=indexbyte(buf,bufsize,Ord(':'));
                                    if (bufend<>-1) and (bufend<bufsize-1) and (buf[bufend+1]=Ord('F')) then
                                      skipfun:=true;
                                  end;
                              end;
                          end;
                      end;
                    if not skipstab then
                      begin
                        { Copy string in stabstr }
                        if hstab.strpos<>0 then
                          begin
                            currstabstrsec.Data.seek(hstab.strpos);
                            hstab.strpos:=mergedstabstrsec.Size;
                            repeat
                              bufsize:=currstabstrsec.Data.read(buf,sizeof(buf));
                              bufend:=indexbyte(buf,bufsize,0);
                              if bufend=-1 then
                                bufend:=bufsize
                              else
                                begin
                                  { include the #0 }
                                  inc(bufend);
                                end;
                              mergedstabstrsec.write(buf,bufend);
                            until (buf[bufend-1]=0) or (bufsize<sizeof(buf));
                          end;
                        { Copy and Update the relocation }
                        if assigned(hstabreloc) then
                          begin
                            hstabreloc.Dataoffset:=mergestabcnt*sizeof(TObjStabEntry)+stabRelocofs;
                            { Remove from List without freeing the object }
                            currstabsec.ObjRelocations.List[currstabrelocidx-1]:=nil;
                            mergedstabsec.ObjRelocations.Add(hstabreloc);
                          end;
                        { Write updated stab }
                        mergedstabsec.write(hstab,sizeof(hstab));
                        inc(mergestabcnt);
                      end;
                  end;
              end;

            { Unload stabs }
            if assigned(currstabstrsec) then
              begin
                currstabstrsec.Used:=False;
                currstabstrsec.ReleaseData;
              end;
            currstabsec.Used:=false;
            currstabsec.ReleaseData;
          end;

        { Generate new HdrSym }
        if mergedstabsec.Size>0 then
          begin
            hstab.strpos:=1;
            hstab.ntype:=0;
            hstab.nother:=0;
            hstab.ndesc:=word(mergestabcnt-1);
            hstab.nvalue:=mergedstabstrsec.Size;
            mergedstabsec.Data.seek(0);
            mergedstabsec.Data.write(hstab,sizeof(hstab));
          end;

        { Replace all sections with our combined stabsec }
        stabexesec.ObjSectionList.Clear;
        stabstrexesec.ObjSectionList.Clear;
        stabexesec.AddObjSection(mergedstabsec);
        stabstrexesec.AddObjSection(mergedstabstrsec);
      end;


    procedure TExeOutput.RemoveEmptySections;
      var
        i, j   : longint;
        exesec : TExeSection;
        doremove : boolean;
      begin
        for i:=0 to ExeSectionList.Count-1 do
          begin
            exesec:=TExeSection(ExeSectionList[i]);

            doremove:=not(oso_keep in exesec.SecOptions) and
                (
                 (exesec.ObjSectionlist.count=0) or
                 (
                  (cs_link_strip in current_settings.globalswitches) and
                  not(cs_link_separate_dbg_file in current_settings.globalswitches) and
                  (oso_debug in exesec.SecOptions)
                 )
                );
            if not doremove then
              begin
               { Check if section has no actual data }
                doremove:=true;
                for j:=0 to exesec.ObjSectionList.Count-1 do
                  if TObjSection(exesec.ObjSectionList[j]).Size<>0 then
                    begin
                      doremove:=false;
                      break;
                    end;
              end;
            if doremove and not (RelocSection and (exesec.Name='.reloc')) then
              begin
                Comment(V_Debug,'Deleting empty section '+exesec.name);
                ExeSectionList[i]:=nil;
              end;
          end;
        ExeSectionList.Pack;
      end;


    procedure TExeOutput.RemoveDebugInfo;
      var
        i      : longint;
        exesec : TExeSection;
      begin
        for i:=0 to ExeSectionList.Count-1 do
          begin
            exesec:=TExeSection(ExeSectionList[i]);
            if (oso_debug in exesec.SecOptions) then
              ExeSectionList[i]:=nil;
          end;
        ExeSectionList.Pack;
      end;


    procedure TExeOutput.RemoveUnreferencedSections;
      var
        ObjSectionWorkList : TFPObjectList;

        procedure AddToObjSectionWorkList(aobjsec:TObjSection);
        begin
          if not aobjsec.Used then
            begin
              aobjsec.Used:=true;
              ObjSectionWorkList.Add(aobjsec);
            end;
        end;

        procedure DoReloc(objreloc:TObjRelocation);
        var
          objsym : TObjSymbol;
          refobjsec : TObjSection;
        begin
          { Disabled Relocation to 0  }
          if objreloc.typ=RELOC_ZERO then
            exit;
          if assigned(objreloc.symbol) then
            begin
              objsym:=objreloc.symbol;
              if objsym.bind<>AB_LOCAL then
                begin
                  if not(assigned(objsym.exesymbol) and
                         (objsym.exesymbol.State=symstate_defined)) then
                    internalerror(200603063);
                  objsym:=objsym.exesymbol.objsymbol;
                end;
              if not assigned(objsym.objsection) then
                internalerror(200603062);
              refobjsec:=objsym.objsection;
            end
          else
            if assigned(objreloc.objsection) then
              refobjsec:=objreloc.objsection
          else
            internalerror(200603316);
          if assigned(exemap) then
            begin
              objsym:=objreloc.symbol;
              if assigned(objsym) then
                exemap.Add('  References  '+objsym.name+' in '
                  +refobjsec.fullname)
              else
                exemap.Add('  References '+refobjsec.fullname);
            end;
          AddToObjSectionWorkList(refobjsec);
        end;

        procedure DoVTableRef(vtable:TExeVTable;VTableIdx:longint);
        var
          i : longint;
          objreloc : TObjRelocation;
        begin
          objreloc:=vtable.VTableRef(VTableIdx);
          if assigned(objreloc) then
            begin
              { Process the relocation now if the ObjSection is
                already processed and marked as used. Otherwise we leave it
                unprocessed. It'll then be resolved when the ObjSection is
                changed to Used }
              if vtable.ExeSymbol.ObjSymbol.ObjSection.Used then
                DoReloc(objreloc);
            end;
          { This recursive walking is done here instead of
            in TExeVTable.VTableRef because we can now process
            all needed relocations }
          for i:=0 to vtable.ChildList.Count-1 do
            DoVTableRef(TExeVTable(vtable.ChildList[i]),VTableIdx);
        end;

      var
        hs        : string;
        i,j,k     : longint;
        exesec    : TExeSection;
        objdata   : TObjData;
        objsec    : TObjSection;
        objsym    : TObjSymbol;
        code      : integer;
        vtableidx : longint;
        vtableexesym : TExeSymbol;
      begin
        ObjSectionWorkList:=TFPObjectList.Create(false);

        if assigned(exemap) then
          exemap.AddHeader('Removing unreferenced sections');

        { Initialize by marking all sections unused and
          adding the sections with oso_keep flags to the ObjSectionWorkList }
        for i:=0 to ObjDataList.Count-1 do
          begin
            ObjData:=TObjData(ObjDataList[i]);
            for j:=0 to ObjData.ObjSectionList.Count-1 do
              begin
                objsec:=TObjSection(ObjData.ObjSectionList[j]);
                objsec.Used:=false;
{ TODO: remove debug section always keep}
                if oso_debug in objsec.secoptions then
                  objsec.Used:=true;
                if (oso_keep in objsec.secoptions) then
                  begin
                    AddToObjSectionWorkList(objsec);
                    if objsec.name='.fpc.n_links' then
                      objsec.Used:=false;
                  end;
              end;
          end;
        AddToObjSectionWorkList(entrysym.exesymbol.objsymbol.objsection);

        { Process all sections, add new sections to process based
          on the symbol references  }
        while ObjSectionWorkList.Count>0 do
          begin
            objsec:=TObjSection(ObjSectionWorkList.Last);
            if assigned(exemap) then
              exemap.Add('Keeping '+objsec.FullName+' '+ToStr(objsec.ObjRelocations.Count)+' references');
            ObjSectionWorkList.Delete(ObjSectionWorkList.Count-1);

            { Process Relocations }
            for i:=0 to objsec.ObjRelocations.count-1 do
              DoReloc(TObjRelocation(objsec.ObjRelocations[i]));

            { Process Virtual Entry calls }
            if cs_link_opt_vtable in current_settings.globalswitches then
              begin
                for i:=0 to objsec.VTRefList.count-1 do
                  begin
                    objsym:=TObjSymbol(objsec.VTRefList[i]);
                    hs:=objsym.name;
                    Delete(hs,1,Pos('_',hs));
                    k:=Pos('$$',hs);
                    if k=0 then
                      internalerror(200603314);
                    vtableexesym:=texesymbol(FExeSymbolList.Find(Copy(hs,1,k-1)));
                    val(Copy(hs,k+2,length(hs)-k-1),vtableidx,code);
                    if (code<>0) then
                      internalerror(200603317);
                    if not assigned(vtableexesym) then
                      internalerror(200603315);
                    if not assigned(vtableexesym.vtable) then
                      internalerror(200603316);
                    DoVTableRef(vtableexesym.vtable,vtableidx);
                  end;
              end;
          end;
        ObjSectionWorkList.Free;
        ObjSectionWorkList:=nil;

        { Remove unused objsections from ExeSectionList }
        for i:=0 to ExeSectionList.Count-1 do
          begin
            exesec:=TExeSection(ExeSectionList[i]);
            for j:=0 to exesec.ObjSectionlist.count-1 do
              begin
                objsec:=TObjSection(exesec.ObjSectionlist[j]);
                if not objsec.used then
                  begin
                    if assigned(exemap) then
                      exemap.Add('Removing '+objsec.FullName);
                    exesec.ObjSectionlist[j]:=nil;
                    objsec.ReleaseData;
                  end;
              end;
            exesec.ObjSectionlist.Pack;
          end;
      end;


    procedure TExeOutput.FixupRelocations;
      var
        i,j     : longint;
        exesec  : TExeSection;
        objsec  : TObjSection;
      begin
        for i:=0 to ExeSectionList.Count-1 do
          begin
            exesec:=TExeSection(ExeSectionList[i]);
            if not assigned(exesec) then
              continue;
            for j:=0 to exesec.ObjSectionlist.count-1 do
              begin
                objsec:=TObjSection(exesec.ObjSectionlist[j]);
                if not objsec.Used then
                  internalerror(200603301);
                objsec.FixupRelocs;
              end;
          end;
      end;


    procedure TExeOutput.SetCurrMemPos(const AValue: qword);
      begin
        if AValue>MaxMemPos then
          Message1(link_f_executable_too_big, target_os_string);
        FCurrMemPos:=AValue;
      end;


{****************************************************************************
                                TObjInput
****************************************************************************}

    constructor TObjInput.create;
      begin
      end;


    destructor TObjInput.destroy;
      begin
        inherited destroy;
      end;


    function TObjInput.newObjData(const n:string):TObjData;
      begin
        result:=CObjData.create(n);
      end;


    procedure TObjInput.inputerror(const s : string);
      begin
        Comment(V_Error,s+' while reading '+InputFileName);
      end;


{$ifdef MEMDEBUG}
initialization
  memobjsymbols:=TMemDebug.create('ObjSymbols');
  memobjsymbols.stop;
  memobjsections:=TMemDebug.create('ObjSections');
  memobjsections.stop;

finalization
  memobjsymbols.free;
  memobjsections.free;
{$endif MEMDEBUG}
end.
