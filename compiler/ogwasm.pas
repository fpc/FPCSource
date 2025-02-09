{
    Copyright (c) 2021 by Nikolay Nikolov

    Contains the WebAssembly binary module format reader and writer

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
unit ogwasm;

{$i fpcdefs.inc}

interface

    uses
      { common }
      cclasses,globtype,
      { target }
      systems,cpubase,
      { assembler }
      aasmbase,assemble,aasmcpu,
      { WebAssembly module format definitions }
      wasmbase,
      { output }
      ogbase,
      owbase;

    type
      TWasmObjSymbolExtraData = class;

      TGlobalInitializer = record
        case typ:TWasmBasicType of
          wbt_i32: (init_i32: Int32);
          wbt_i64: (init_i64: Int64);
          wbt_f32: (init_f32: Single);
          wbt_f64: (init_f64: Double);
      end;

      { TWasmObjSymbolLinkingData }

      TWasmObjSymbolLinkingData = class
      public
        ImportModule: string;
        ImportName: string;

        FuncType: TWasmFuncType;
        ExeFunctionIndex: Integer;
        ExeIndirectFunctionTableIndex: Integer;
        ExeTypeIndex: Integer;

        ExeTagIndex: Integer;

        GlobalType: TWasmBasicType;
        GlobalIsMutable: Boolean;
        GlobalInitializer: TGlobalInitializer;

        IsExported: Boolean;
        ExportName: ansistring;

        constructor Create;
        destructor Destroy;override;
      end;

      { TWasmObjSymbol }

      TWasmObjSymbol = class(TObjSymbol)
        FuncIndex: Integer;
        SymbolIndex: Integer;
        GlobalIndex: Integer;
        TagIndex: Integer;
        AliasOf: string;
        ExtraData: TWasmObjSymbolExtraData;
        LinkingData: TWasmObjSymbolLinkingData;
        TlsGlobalSym: TWasmObjSymbol;
        TlsDataSym: TWasmObjSymbol;
        constructor create(AList:TFPHashObjectList;const AName:string);override;
        destructor Destroy;override;
        function IsAlias: Boolean;
      end;

      { TWasmObjRelocation }

      TWasmObjRelocation = class(TObjRelocation)
      public
        TypeIndex: Integer;
        Addend: LongInt;

        { used during linking }
        FuncType: TWasmFuncType;
        ExeTypeIndex: Integer;
        IsFunctionOffsetI32: Boolean;

        constructor CreateTypeIndex(ADataOffset:TObjSectionOfs; ATypeIndex: Integer);
        constructor CreateFuncType(ADataOffset:TObjSectionOfs; AFuncType: TWasmFuncType);
        destructor Destroy;override;
      end;

      { TWasmObjSymbolExtraData }

      TWasmObjSymbolExtraData = class(TFPHashObject)
        TypeIdx: Integer;
        ExceptionTagTypeIdx: Integer;
        ImportModule: string;
        ImportName: string;
        ExportName: string;
        GlobalType: TWasmBasicType;
        GlobalIsImmutable: Boolean;
        Locals: array of TWasmBasicType;
        EncodedLocals: tdynamicarray;
        constructor Create(HashObjectList: TFPHashObjectList; const s: TSymStr);
        destructor Destroy; override;
        procedure AddLocals(alocals: TWasmLocalsDynArray);
      end;

      { TWasmObjSection }

      TWasmObjSection = class(TObjSection)
      public
        SegIdx: Integer;
        SegSymIdx: Integer;
        SegOfs: qword;
        FileSectionOfs: qword;
        MainFuncSymbol: TWasmObjSymbol;
        CustomSectionIdx: Integer;
        constructor create(AList:TFPHashObjectList;const Aname:string;Aalign:longint;Aoptions:TObjSectionOptions);override;
        function IsCode: Boolean;
        function IsData: Boolean;
        function IsDebug: Boolean;
      end;

      { TWasmFuncTypeTable }

      TWasmFuncTypeTable = class
      private
        FFuncTypes: array of TWasmFuncType;
        function GetCount: Integer;
        function GetItem(Index: Integer): TWasmFuncType;
      public
        destructor Destroy; override;

        function AddOrGetFuncType(wft: TWasmFuncType): integer;
        procedure WriteTo(d: tdynamicarray);
        property Count: Integer read GetCount;
        property Items[Index: Integer]: TWasmFuncType read GetItem; default;
      end;

      { TWasmObjData }

      TWasmObjData = class(TObjData)
      private
        FFuncTypes: TWasmFuncTypeTable;
        FObjSymbolsExtraDataList: TFPHashObjectList;
        FLastFuncName: string;

        function is_smart_section(atype:TAsmSectiontype):boolean;
        function sectionname_gas(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;
      public
        constructor create(const n:string);override;
        destructor destroy; override;
        function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;override;
        procedure writeReloc(Data:TRelocDataInt;len:aword;p:TObjSymbol;Reloctype:TObjRelocationType);override;
        function AddOrCreateObjSymbolExtraData(const symname:TSymStr): TWasmObjSymbolExtraData;
        function globalref(asmsym:TAsmSymbol):TObjSymbol;
        function ExceptionTagRef(asmsym:TAsmSymbol):TObjSymbol;
        procedure DeclareGlobalType(gt: tai_globaltype);
        procedure DeclareFuncType_Pass0(ft: tai_functype);
        procedure DeclareFuncType_Pass1(ft: tai_functype);
        procedure DeclareFuncType_Pass2(ft: tai_functype);
        procedure DeclareTagType(tt: tai_tagtype);
        procedure DeclareExportName(en: tai_export_name);
        procedure DeclareImportModule(aim: tai_import_module);
        procedure DeclareImportName(ain: tai_import_name);
        procedure DeclareLocals_Pass0(al: tai_local);
        procedure DeclareLocals_Pass1(al: tai_local);
        procedure WriteLocals_Pass2(al: tai_local);
        procedure symbolpairdefine(akind: TSymbolPairKind;const asym, avalue: string);override;
        property FuncTypes: TWasmFuncTypeTable read FFuncTypes;
      end;

      { TWasmObjOutput }

      TWasmObjOutput = class(tObjOutput)
      private
        FData: TWasmObjData;
        FWasmRelocationCodeTable: tdynamicarray;
        FWasmRelocationCodeTableEntriesCount: Integer;
        FWasmRelocationDataTable: tdynamicarray;
        FWasmRelocationDataTableEntriesCount: Integer;
        FWasmRelocationDebugFrameTable: tdynamicarray;
        FWasmRelocationDebugFrameTableEntriesCount: Integer;
        FWasmRelocationDebugInfoTable: tdynamicarray;
        FWasmRelocationDebugInfoTableEntriesCount: Integer;
        FWasmRelocationDebugLineTable: tdynamicarray;
        FWasmRelocationDebugLineTableEntriesCount: Integer;
        FWasmRelocationDebugAbbrevTable: tdynamicarray;
        FWasmRelocationDebugAbbrevTableEntriesCount: Integer;
        FWasmRelocationDebugArangesTable: tdynamicarray;
        FWasmRelocationDebugArangesTableEntriesCount: Integer;
        FWasmRelocationDebugRangesTable: tdynamicarray;
        FWasmRelocationDebugRangesTableEntriesCount: Integer;
        FWasmRelocationDebugStrTable: tdynamicarray;
        FWasmRelocationDebugStrTableEntriesCount: Integer;
        FWasmSymbolTable: tdynamicarray;
        FWasmSymbolTableEntriesCount: Integer;
        FWasmSections: array [TWasmSectionID] of tdynamicarray;
        FWasmCustomSections: array [TWasmCustomSectionType] of tdynamicarray;
        FWasmLinkingSubsections: array [low(TWasmLinkingSubsectionType)..high(TWasmLinkingSubsectionType)] of tdynamicarray;
        procedure WriteWasmSection(wsid: TWasmSectionID);
        procedure WriteWasmCustomSection(wcst: TWasmCustomSectionType);
        function IsExternalFunction(sym: TObjSymbol): Boolean;
        function IsExportedFunction(sym: TWasmObjSymbol): Boolean;
        procedure WriteFunctionCode(dest: tdynamicarray; objsym: TObjSymbol);
        procedure WriteSymbolTable;
        procedure WriteRelocationCodeTable(CodeSectionIndex: Integer);
        procedure WriteRelocationDataTable(DataSectionIndex: Integer);
        procedure MaybeWriteRelocationDebugTable(cst: TWasmCustomSectionType; SectionIndex: Integer; EntriesCount: Integer; Table: tdynamicarray);
        procedure WriteLinkingSubsection(wlst: TWasmLinkingSubsectionType);
        procedure DoRelocations;
        procedure WriteRelocations;
        function FindFunctionSymbol(Symbol: TWasmObjSymbol): TWasmObjSymbol;
      protected
        function writeData(Data:TObjData):boolean;override;
      public
        constructor create(AWriter:TObjectWriter);override;
        destructor destroy;override;
      end;

      { TWasmObjInput }

      TWasmObjInput = class(TObjInput)
      private
        FFuncTypes: array of TWasmFuncType;
      public
        constructor create;override;
        destructor Destroy;override;
        class function CanReadObjData(AReader:TObjectreader):boolean;override;
        function ReadObjData(AReader:TObjectreader;out ObjData:TObjData):boolean;override;
      end;

      { TWasmExeOutput }

      TWasmExeOutput = class(TExeOutput)
      private
        const
          DataSections: array [1..3] of string = (
            '.rodata',
            '.data',
            'fpc.resources');
        WasmPageSize = 65536;

        type
          TCustomSectionNameMapEntry = record
            idx: UInt32;
            name: string;
          end;
          TCustomSectionNameMap = array of TCustomSectionNameMapEntry;
      private
        FImports: TFPHashObjectList;
        FFuncTypes: TWasmFuncTypeTable;

        FFunctionImports: array of record
          ModName: ansistring;
          Name: ansistring;
          TypeIdx: uint32;
        end;

        FTagImports: array of record
        end;

        FIndirectFunctionTable: array of record
          FuncIdx: Integer;
        end;

        FWasmSections: array [TWasmSectionID] of tdynamicarray;
        FWasmCustomSections: array [TWasmCustomSectionType] of tdynamicarray;
        FWasmNameSubsections: array [TWasmNameSubsectionType] of tdynamicarray;
        FStackPointerSym: TWasmObjSymbol;
        FTlsBaseSym: TWasmObjSymbol;
        FTlsSizeSym: TWasmObjSymbol;
        FTlsAlignSym: TWasmObjSymbol;
        FInitTlsFunctionSym: TWasmObjSymbol;
        FInitSharedMemoryFunctionSym: TWasmObjSymbol;
        FMinMemoryPages,
        FMaxMemoryPages: Integer;
        { use for the Name section }
        FFunctionNameMap: TCustomSectionNameMap;
        FGlobalNameMap: TCustomSectionNameMap;
        FDataNameMap: TCustomSectionNameMap;
        FTagNameMap: TCustomSectionNameMap;
        procedure AddToNameMap(var nm: TCustomSectionNameMap; aidx: UInt32; const aname: string);
        procedure AddToFunctionNameMap(aidx: UInt32; const aname: string);
        procedure AddToGlobalNameMap(aidx: UInt32; const aname: string);
        procedure AddToDataNameMap(aidx: UInt32; const aname: string);
        procedure AddToTagNameMap(aidx: UInt32; const aname: string);
        procedure WriteWasmSection(wsid: TWasmSectionID);
        procedure WriteWasmSectionIfNotEmpty(wsid: TWasmSectionID);
        procedure WriteWasmCustomSection(wcst: TWasmCustomSectionType);
        procedure PrepareImports;
        procedure PrepareFunctions;
        procedure PrepareTags;
        function AddOrGetIndirectFunctionTableIndex(FuncIdx: Integer): integer;
        procedure SetStackPointer;
        procedure SetTlsSizeAlignAndBase;
        procedure SetThreadVarGlobalsInitValues;
        procedure GenerateCode_InitTls;
        procedure GenerateCode_InitSharedMemory;
        procedure GenerateCode_InvokeHelper;
        procedure WriteExeSectionToDynArray(exesec: TExeSection; dynarr: tdynamicarray);
      protected
        function writeData:boolean;override;
        procedure DoRelocationFixup(objsec:TObjSection);override;
      public
        constructor create;override;
        destructor destroy;override;
        procedure GenerateLibraryImports(ImportLibraryList:TFPHashObjectList);override;
        procedure AfterUnusedSectionRemoval;override;
        procedure MemPos_ExeSection(const aname:string);override;
        procedure Load_Symbol(const aname: string);override;
      end;

      { TWasmAssembler }

      TWasmAssembler = class(tinternalassembler)
        constructor create(info: pasminfo; smart:boolean);override;
      end;

implementation

    uses
      cutils,verbose,version,globals,fmodule,ogmap;

    const
      StackPointerSymStr='__stack_pointer';

    procedure WriteUleb5(d: tdynamicarray; v: uint64);
      var
        b: byte;
        i: Integer;
      begin
        for i:=1 to 5 do
          begin
            b:=byte(v) and 127;
            v:=v shr 7;
            if i<>5 then
              b:=b or 128;
            d.write(b,1);
          end;
      end;

    procedure WriteUleb5(d: tobjsection; v: uint64);
      var
        b: byte;
        i: Integer;
      begin
        for i:=1 to 5 do
          begin
            b:=byte(v) and 127;
            v:=v shr 7;
            if i<>5 then
              b:=b or 128;
            d.write(b,1);
          end;
      end;

    procedure WriteSleb5(d: tdynamicarray; v: int64);
      var
        b: byte;
        i: Integer;
      begin
        for i:=1 to 5 do
          begin
            b:=byte(v) and 127;
            v:=SarInt64(v,7);
            if i<>5 then
              b:=b or 128;
            d.write(b,1);
          end;
      end;

    procedure WriteSleb5(d: tobjsection; v: int64);
      var
        b: byte;
        i: Integer;
      begin
        for i:=1 to 5 do
          begin
            b:=byte(v) and 127;
            v:=SarInt64(v,7);
            if i<>5 then
              b:=b or 128;
            d.write(b,1);
          end;
      end;
    procedure WriteUleb(d: tdynamicarray; v: uint64);
      var
        b: byte;
      begin
        repeat
          b:=byte(v) and 127;
          v:=v shr 7;
          if v<>0 then
            b:=b or 128;
          d.write(b,1);
        until v=0;
      end;

    procedure WriteUleb(d: tobjsection; v: uint64);
      var
        b: byte;
      begin
        repeat
          b:=byte(v) and 127;
          v:=v shr 7;
          if v<>0 then
            b:=b or 128;
          d.write(b,1);
        until v=0;
      end;

    procedure WriteUleb(w: TObjectWriter; v: uint64);
      var
        b: byte;
      begin
        repeat
          b:=byte(v) and 127;
          v:=v shr 7;
          if v<>0 then
            b:=b or 128;
          w.write(b,1);
        until v=0;
      end;

    procedure WriteSleb(d: tdynamicarray; v: int64);
      var
        b: byte;
        Done: Boolean=false;
      begin
        repeat
          b:=byte(v) and 127;
          v:=SarInt64(v,7);
          if ((v=0) and ((b and 64)=0)) or ((v=-1) and ((b and 64)<>0)) then
            Done:=true
          else
            b:=b or 128;
          d.write(b,1);
        until Done;
      end;

    procedure WriteSleb(d: tobjsection; v: int64);
      var
        b: byte;
        Done: Boolean=false;
      begin
        repeat
          b:=byte(v) and 127;
          v:=SarInt64(v,7);
          if ((v=0) and ((b and 64)=0)) or ((v=-1) and ((b and 64)<>0)) then
            Done:=true
          else
            b:=b or 128;
          d.write(b,1);
        until Done;
      end;

    function UlebEncodingSize(v: uint64): Integer;
      var
        b: byte;
      begin
        Result:=0;
        repeat
          b:=byte(v) and 127;
          v:=v shr 7;
          if v<>0 then
            b:=b or 128;
          Inc(Result);
        until v=0;
      end;

{$ifdef FPC_LITTLE_ENDIAN}
    procedure WriteF32LE(d: tdynamicarray; v: Single);
      begin
        d.write(v,4);
      end;

    procedure WriteF64LE(d: tdynamicarray; v: Double);
      begin
        d.write(v,8);
      end;
{$else FPC_LITTLE_ENDIAN}
    procedure WriteF32LE(d: tdynamicarray; v: Single);
      var
        tmpI: UInt32;
      begin
        Move(v,tmpI,4);
        tmpI:=SwapEndian(tmpI);
        d.write(tmpI,4);
      end;

    procedure WriteF64LE(d: tdynamicarray; v: Double);
      var
        tmpI: UInt64;
      begin
        Move(v,tmpI,8);
        tmpI:=SwapEndian(tmpI);
        d.write(tmpI,8);
      end;
{$endif FPC_LITTLE_ENDIAN}

    procedure WriteByte(d: tdynamicarray; b: byte);
      begin
        d.write(b,1);
      end;

    procedure WriteName(d: tdynamicarray; const s: string);
      begin
        WriteUleb(d,Length(s));
        d.writestr(s);
      end;

    procedure WriteWasmBasicType(dest: tdynamicarray; wbt: TWasmBasicType);
      begin
        WriteByte(dest,encode_wasm_basic_type(wbt));
      end;

    procedure WriteWasmResultType(dest: tdynamicarray; wrt: TWasmResultType);
      var
        i: Integer;
      begin
        WriteUleb(dest,Length(wrt));
        for i:=low(wrt) to high(wrt) do
          WriteWasmBasicType(dest,wrt[i]);
      end;

    function ReadUleb(d: tdynamicarray): uint64;
      var
        b: byte;
        shift:integer;
      begin
        b:=0;
        result:=0;
        shift:=0;
        repeat
          d.read(b,1);
          result:=result or (uint64(b and 127) shl shift);
          inc(shift,7);
        until (b and 128)=0;
      end;

    function ReadSleb(d: tdynamicarray): int64;
      var
        b: byte;
        shift:integer;
      begin
        b:=0;
        result:=0;
        shift:=0;
        repeat
          d.read(b,1);
          result:=result or (uint64(b and 127) shl shift);
          inc(shift,7);
        until (b and 128)=0;
{$ifopt Q+}
{$define overflowon}
{$Q-}
{$endif}
{$ifopt R+}
{$define rangeon}
{$R-}
{$endif}
        if (b and 64)<>0 then
          result:=result or (high(uint64) shl shift);
      end;
{$ifdef overflowon}
{$Q+}
{$undef overflowon}
{$endif}
{$ifdef rangeon}
{$R+}
{$undef rangeon}
{$endif}

    procedure AddSleb5(d: tdynamicarray; v: int64);
      var
        q: Int64;
        p: LongWord;
      begin
        p:=d.Pos;
        q:=ReadSleb(d);
        q:=q+v;
        d.seek(p);
        WriteSleb5(d,q);
      end;

    procedure AddUleb5(d: tdynamicarray; v: int64);
      var
        q: UInt64;
        p: LongWord;
      begin
        p:=d.Pos;
        q:=ReadUleb(d);
        q:=q+v;
        d.seek(p);
        WriteUleb5(d,q);
      end;

    procedure AddInt32(d: tdynamicarray; v: int32);
      var
        q: int32;
        p: LongWord;
      begin
        p:=d.Pos;

        d.read(q,4);
{$ifdef FPC_BIG_ENDIAN}
        q:=SwapEndian(q);
{$endif FPC_BIG_ENDIAN}
        q:=q+v;
{$ifdef FPC_BIG_ENDIAN}
        q:=SwapEndian(q);
{$endif FPC_BIG_ENDIAN}

        d.seek(p);
        d.write(q,4);
      end;

    procedure CopyDynamicArray(src, dest: tdynamicarray; size: QWord);
      var
        buf: array [0..4095] of byte;
        bs: Integer;
      begin
        while size>0 do
          begin
            if size<SizeOf(buf) then
              bs:=Integer(size)
            else
              bs:=SizeOf(buf);
            src.read(buf,bs);
            dest.write(buf,bs);
            dec(size,bs);
          end;
      end;

    procedure WriteZeros(dest: tdynamicarray; size: QWord);
      var
        buf : array[0..1023] of byte;
        bs: Integer;
      begin
        fillchar(buf,sizeof(buf),0);
        while size>0 do
          begin
            if size<SizeOf(buf) then
              bs:=Integer(size)
            else
              bs:=SizeOf(buf);
            dest.write(buf,bs);
            dec(size,bs);
          end;
      end;


{****************************************************************************
                         TWasmObjSymbolLinkingData
****************************************************************************}

    constructor TWasmObjSymbolLinkingData.Create;
      begin
        ExeFunctionIndex:=-1;
        ExeIndirectFunctionTableIndex:=-1;
        ExeTypeIndex:=-1;
        ExeTagIndex:=-1;
      end;

    destructor TWasmObjSymbolLinkingData.Destroy;
      begin
        FuncType.Free;
        inherited Destroy;
      end;

{****************************************************************************
                             TWasmObjRelocation
****************************************************************************}

    constructor TWasmObjRelocation.CreateTypeIndex(ADataOffset: TObjSectionOfs; ATypeIndex: Integer);
      begin
        DataOffset:=ADataOffset;
        Symbol:=nil;
        OrgSize:=0;
        Group:=nil;
        ObjSection:=nil;
        ftype:=ord(RELOC_TYPE_INDEX_LEB);
        TypeIndex:=ATypeIndex;
        FuncType:=nil;
        ExeTypeIndex:=-1;
      end;

    constructor TWasmObjRelocation.CreateFuncType(ADataOffset: TObjSectionOfs; AFuncType: TWasmFuncType);
      begin
        DataOffset:=ADataOffset;
        Symbol:=nil;
        OrgSize:=0;
        Group:=nil;
        ObjSection:=nil;
        ftype:=ord(RELOC_TYPE_INDEX_LEB);
        TypeIndex:=-1;
        ExeTypeIndex:=-1;
        FuncType:=TWasmFuncType.Create(AFuncType);
      end;

    destructor TWasmObjRelocation.Destroy;
      begin
        FuncType.Free;
        inherited Destroy;
      end;

{****************************************************************************
                               TWasmObjSymbol
****************************************************************************}

    constructor TWasmObjSymbol.create(AList: TFPHashObjectList; const AName: string);
      begin
        inherited create(AList,AName);
        FuncIndex:=-1;
        SymbolIndex:=-1;
        GlobalIndex:=-1;
        TagIndex:=-1;
        AliasOf:='';
        ExtraData:=nil;
        LinkingData:=TWasmObjSymbolLinkingData.Create;
      end;

    destructor TWasmObjSymbol.Destroy;
      begin
        LinkingData.Free;
        inherited Destroy;
      end;

    function TWasmObjSymbol.IsAlias: Boolean;
      begin
        result:=AliasOf<>'';
      end;

{****************************************************************************
                              TWasmObjSymbolExtraData
****************************************************************************}

    constructor TWasmObjSymbolExtraData.Create(HashObjectList: TFPHashObjectList; const s: TSymStr);
      begin
        EncodedLocals:=nil;
        inherited Create(HashObjectList,s);
        TypeIdx:=-1;
        ExceptionTagTypeIdx:=-1;
      end;

    destructor TWasmObjSymbolExtraData.Destroy;
      begin
        EncodedLocals.Free;
        inherited Destroy;
      end;

    procedure TWasmObjSymbolExtraData.AddLocals(alocals: TWasmLocalsDynArray);
      var
        i,
        rle_entries,
        cnt: Integer;
        lasttype: TWasmBasicType;
      begin
        Locals:=alocals;
        if Assigned(EncodedLocals) then
          internalerror(2024081502);
        EncodedLocals:=tdynamicarray.Create(64);
        if Length(Locals)=0 then
          begin
            WriteUleb(EncodedLocals,0);
            exit;
          end;

        rle_entries:=1;
        for i:=low(Locals)+1 to high(Locals) do
          if Locals[i]<>Locals[i-1] then
            inc(rle_entries);

        WriteUleb(EncodedLocals,rle_entries);
        lasttype:=Locals[Low(Locals)];
        cnt:=1;
        for i:=low(Locals)+1 to high(Locals) do
          if Locals[i]=Locals[i-1] then
            inc(cnt)
          else
            begin
              WriteUleb(EncodedLocals,cnt);
              WriteWasmBasicType(EncodedLocals,lasttype);
              lasttype:=Locals[i];
              cnt:=1;
            end;
        WriteUleb(EncodedLocals,cnt);
        WriteWasmBasicType(EncodedLocals,lasttype);
      end;

{****************************************************************************
                              TWasmObjSection
****************************************************************************}

    constructor TWasmObjSection.create(AList: TFPHashObjectList; const Aname: string; Aalign: longint; Aoptions: TObjSectionOptions);
      begin
        inherited create(AList, Aname, Aalign, Aoptions);
        SegIdx:=-1;
        SegSymIdx:=-1;
        CustomSectionIdx:=-1;
        MainFuncSymbol:=nil;
      end;

    function TWasmObjSection.IsCode: Boolean;
      const
        CodePrefix = '.text';
      begin
        result:=(Length(Name)>=Length(CodePrefix)) and
          (Copy(Name,1,Length(CodePrefix))=CodePrefix);
      end;

    function TWasmObjSection.IsData: Boolean;
      begin
        result:=not (IsCode or IsDebug);
      end;

    function TWasmObjSection.IsDebug: Boolean;
      const
        DebugPrefix = '.debug';
      begin
        result:=(Length(Name)>=Length(DebugPrefix)) and
          (Copy(Name,1,Length(DebugPrefix))=DebugPrefix);
      end;

{****************************************************************************
                             TWasmFuncTypeTable
****************************************************************************}

    function TWasmFuncTypeTable.GetCount: Integer;
      begin
        Result:=Length(FFuncTypes);
      end;

    function TWasmFuncTypeTable.GetItem(Index: Integer): TWasmFuncType;
      begin
        if (Index<Low(FFuncTypes)) or (Index>High(FFuncTypes)) then
          internalerror(2023123101);
        Result:=FFuncTypes[Index];
      end;

    destructor TWasmFuncTypeTable.Destroy;
      var
        i: Integer;
      begin
        for i:=low(FFuncTypes) to high(FFuncTypes) do
          begin
            FFuncTypes[i].free;
            FFuncTypes[i]:=nil;
          end;
      end;

    function TWasmFuncTypeTable.AddOrGetFuncType(wft: TWasmFuncType): integer;
      var
        i: Integer;
      begin
        for i:=low(FFuncTypes) to high(FFuncTypes) do
          if wft.Equals(FFuncTypes[i]) then
            exit(i);

        result:=Length(FFuncTypes);
        SetLength(FFuncTypes,result+1);
        FFuncTypes[result]:=TWasmFuncType.Create(wft);
      end;

    procedure TWasmFuncTypeTable.WriteTo(d: tdynamicarray);
      var
        types_count, i: Integer;
      begin
        types_count:=Count;
        WriteUleb(d,types_count);
        for i:=0 to types_count-1 do
          with Items[i] do
            begin
              WriteByte(d,$60);
              WriteWasmResultType(d,params);
              WriteWasmResultType(d,results);
            end;
      end;

{****************************************************************************
                                TWasmObjData
****************************************************************************}

    function TWasmObjData.is_smart_section(atype: TAsmSectiontype): boolean;
      begin
        { For bss we need to set some flags that are target dependent,
          it is easier to disable it for smartlinking. It doesn't take up
          filespace }
        result:=not(target_info.system in systems_darwin) and
           create_smartlink_sections and
           (atype<>sec_toc) and
           (atype<>sec_user) and
           { on embedded systems every byte counts, so smartlink bss too }
           ((atype<>sec_bss) or (target_info.system in (systems_embedded+systems_freertos)));
      end;

    function TWasmObjData.sectionname_gas(atype: TAsmSectiontype;
        const aname: string; aorder: TAsmSectionOrder): string;
      const
        secnames : array[TAsmSectiontype] of string[length('__DATA, __datacoal_nt,coalesced')] = ('','',
          '.text',
          '.data',
{ why doesn't .rodata work? (FK) }
{ sometimes we have to create a data.rel.ro instead of .rodata, e.g. for  }
{ vtables (and anything else containing relocations), otherwise those are }
{ not relocated properly on e.g. linux/ppc64. g++ generates there for a   }
{ vtable for a class called Window:                                       }
{ .section .data.rel.ro._ZTV6Window,"awG",@progbits,_ZTV6Window,comdat    }
{ TODO: .data.ro not yet working}
{$if defined(arm) or defined(riscv64) or defined(powerpc)}
          '.rodata',
{$else defined(arm) or defined(riscv64) or defined(powerpc)}
          '.data',
{$endif defined(arm) or defined(riscv64) or defined(powerpc)}
          '.rodata',
          '.bss',
          '.tbss',
          '.pdata',
          '', { stubs }
          '__DATA,__nl_symbol_ptr',
          '__DATA,__la_symbol_ptr',
          '__DATA,__mod_init_func',
          '__DATA,__mod_term_func',
          '.stab',
          '.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame','.debug_info','.debug_line','.debug_abbrev','.debug_aranges','.debug_ranges','.debug_loc','.debug_loclists',
          '.fpc',
          '.toc',
          '.init',
          '.fini',
          '.objc_class',
          '.objc_meta_class',
          '.objc_cat_cls_meth',
          '.objc_cat_inst_meth',
          '.objc_protocol',
          '.objc_string_object',
          '.objc_cls_meth',
          '.objc_inst_meth',
          '.objc_cls_refs',
          '.objc_message_refs',
          '.objc_symbols',
          '.objc_category',
          '.objc_class_vars',
          '.objc_instance_vars',
          '.objc_module_info',
          '.objc_class_names',
          '.objc_meth_var_types',
          '.objc_meth_var_names',
          '.objc_selector_strs',
          '.objc_protocol_ext',
          '.objc_class_ext',
          '.objc_property',
          '.objc_image_info',
          '.objc_cstring_object',
          '.objc_sel_fixup',
          '__DATA,__objc_data',
          '__DATA,__objc_const',
          '.objc_superrefs',
          '__DATA, __datacoal_nt,coalesced',
          '.objc_classlist',
          '.objc_nlclasslist',
          '.objc_catlist',
          '.obcj_nlcatlist',
          '.objc_protolist',
          '.stack',
          '.heap',
          '.gcc_except_table',
          '.ARM.attributes'
        );
      var
        sep     : string[3];
        secname : string;
      begin
        secname:=secnames[atype];

        if (atype=sec_fpc) and (Copy(aname,1,3)='res') then
          begin
            result:=secname+'.'+aname;
            exit;
          end;

        { go32v2 stub only loads .text and .data sections, and allocates space for .bss.
          Thus, data which normally goes into .rodata and .rodata_norel sections must
          end up in .data section }
        if (atype in [sec_rodata,sec_rodata_norel]) and
          (target_info.system in [system_i386_go32v2,system_m68k_palmos]) then
          secname:='.data';

        { Windows correctly handles reallocations in readonly sections }
        if (atype=sec_rodata) and
          (target_info.system in systems_all_windows+systems_nativent-[system_i8086_win16]) then
          secname:='.rodata';

        { section type user gives the user full controll on the section name }
        if atype=sec_user then
          secname:=aname;

        if is_smart_section(atype) and (aname<>'') then
          begin
            case aorder of
              secorder_begin :
                sep:='.b_';
              secorder_end :
                sep:='.z_';
              else
                sep:='.n_';
            end;
            result:=secname+sep+aname
          end
        else
          result:=secname;
      end;

    constructor TWasmObjData.create(const n: string);
      begin
        inherited;
        CObjSection:=TWasmObjSection;
        CObjSymbol:=TWasmObjSymbol;
        FObjSymbolsExtraDataList:=TFPHashObjectList.Create;
        FFuncTypes:=TWasmFuncTypeTable.Create;
      end;

    destructor TWasmObjData.destroy;
      var
        i: Integer;
      begin
        FObjSymbolsExtraDataList.Free;
        FFuncTypes.Free;
        inherited destroy;
      end;

    function TWasmObjData.sectionname(atype: TAsmSectiontype;
        const aname: string; aorder: TAsmSectionOrder): string;
      begin
        if (atype=sec_fpc) or
           ((atype=sec_threadvar) and not (ts_wasm_threads in current_settings.targetswitches)) then
          atype:=sec_data;
        Result:=sectionname_gas(atype, aname, aorder);
      end;

    procedure TWasmObjData.writeReloc(Data: TRelocDataInt; len: aword;
        p: TObjSymbol; Reloctype: TObjRelocationType);
      const
        leb_zero: array[0..4] of byte=($80,$80,$80,$80,$00);
      var
        objreloc: TWasmObjRelocation;
      begin
        if CurrObjSec=nil then
          internalerror(200403072);
        { workaround crash, when generating debug info for threadvars, when multithreading is turned off.
          todo: ensure the debug info for threadvars is actually correct, once we've got WebAssembly debug info working in general }
        if (Reloctype=RELOC_DTPOFF) and not (ts_wasm_threads in current_settings.targetswitches) then
          Reloctype:=RELOC_ABSOLUTE;
        objreloc:=nil;
        case Reloctype of
          RELOC_FUNCTION_INDEX_LEB:
            begin
              if Data<>0 then
                internalerror(2021092502);
              if len<>5 then
                internalerror(2021092503);
              if not assigned(p) then
                internalerror(2021092504);
              objreloc:=TWasmObjRelocation.CreateSymbol(CurrObjSec.Size,p,Reloctype);
              CurrObjSec.ObjRelocations.Add(objreloc);
              writebytes(leb_zero,5);
            end;
          RELOC_MEMORY_ADDR_LEB,
          RELOC_MEMORY_ADDR_OR_TABLE_INDEX_SLEB:
            begin
              if (Reloctype=RELOC_MEMORY_ADDR_LEB) and (Data<0) then
                internalerror(2021092602);
              if len<>5 then
                internalerror(2021092503);
              if not assigned(p) then
                internalerror(2021092504);
              objreloc:=TWasmObjRelocation.CreateSymbol(CurrObjSec.Size,p,Reloctype);
              objreloc.Addend:=Data;
              CurrObjSec.ObjRelocations.Add(objreloc);
              if RelocType=RELOC_MEMORY_ADDR_LEB then
                WriteUleb5(CurrObjSec,Data)
              else
                WriteSleb5(CurrObjSec,Data);
            end;
          RELOC_ABSOLUTE:
            begin
              if len<>4 then
                internalerror(2021092607);
              if not assigned(p) then
                internalerror(2021092608);
              if (p.objsection<>nil) and TWasmObjSection(p.objsection).IsDebug and
                 (p.bind<>AB_COMMON) and (p.bind<>AB_EXTERNAL) then
                begin
                  objreloc:=TWasmObjRelocation.CreateSection(CurrObjSec.Size,p.objsection,RELOC_ABSOLUTE);
                  objreloc.Addend:=Data+p.Address;
                  CurrObjSec.ObjRelocations.Add(objreloc);
                  {inc(data,p.address);}
                  data:=0;
                  Data:=NtoLE(Data);
                  writebytes(Data,4);
                end
              else
                begin
                  objreloc:=TWasmObjRelocation.CreateSymbol(CurrObjSec.Size,p,Reloctype);
                  objreloc.Addend:=Data;
                  CurrObjSec.ObjRelocations.Add(objreloc);
                  Data:=NtoLE(Data);
                  writebytes(Data,4);
                end;
            end;
          RELOC_TYPE_INDEX_LEB:
            begin
              if len<>5 then
                internalerror(2021092612);
              if assigned(p) then
                internalerror(2021092613);
              objreloc:=TWasmObjRelocation.CreateTypeIndex(CurrObjSec.Size,Data);
              CurrObjSec.ObjRelocations.Add(objreloc);
              WriteUleb5(CurrObjSec,Data);
            end;
          RELOC_GLOBAL_INDEX_LEB:
            begin
              if len<>5 then
                internalerror(2021092701);
              if Data<>0 then
                internalerror(2021092702);
              if not assigned(p) then
                internalerror(2021092703);
              objreloc:=TWasmObjRelocation.CreateSymbol(CurrObjSec.Size,p,Reloctype);
              CurrObjSec.ObjRelocations.Add(objreloc);
              WriteUleb5(CurrObjSec,0);
            end;
          RELOC_TAG_INDEX_LEB:
            begin
              if len<>5 then
                internalerror(2021092712);
              if Data<>0 then
                internalerror(2021092713);
              if not assigned(p) then
                internalerror(2021092714);
              objreloc:=TWasmObjRelocation.CreateSymbol(CurrObjSec.Size,p,Reloctype);
              CurrObjSec.ObjRelocations.Add(objreloc);
              WriteSleb5(CurrObjSec,0);
            end;
          else
            internalerror(2021092501);
        end;
      end;

    function TWasmObjData.AddOrCreateObjSymbolExtraData(const symname: TSymStr): TWasmObjSymbolExtraData;
      begin
        result:=TWasmObjSymbolExtraData(FObjSymbolsExtraDataList.Find(symname));
        if not assigned(result) then
          result:=TWasmObjSymbolExtraData.Create(FObjSymbolsExtraDataList,symname);
      end;

    function TWasmObjData.globalref(asmsym: TAsmSymbol): TObjSymbol;
      begin
        if assigned(asmsym) then
          begin
            if (asmsym.typ<>AT_WASM_GLOBAL) and (asmsym.typ<>AT_TLS) then
              internalerror(2021092706);
            result:=symbolref(asmsym);
            result.typ:=asmsym.typ;
          end
        else
          result:=nil;
      end;

    function TWasmObjData.ExceptionTagRef(asmsym: TAsmSymbol): TObjSymbol;
      begin
        if assigned(asmsym) then
          begin
            if asmsym.typ<>AT_WASM_EXCEPTION_TAG then
              internalerror(2021092707);
            result:=symbolref(asmsym);
            result.typ:=AT_WASM_EXCEPTION_TAG;
          end
        else
          result:=nil;
      end;

    procedure TWasmObjData.DeclareGlobalType(gt: tai_globaltype);
      var
        ObjSymExtraData: TWasmObjSymbolExtraData;
        ObjSym: TObjSymbol;
      begin
        if not gt.is_external then
          begin
            ObjSym:=symboldefine(gt.sym);
            ObjSym.typ:=AT_WASM_GLOBAL;
          end;
        ObjSymExtraData:=AddOrCreateObjSymbolExtraData(gt.globalname);
        ObjSymExtraData.GlobalType:=gt.gtype;
        ObjSymExtraData.GlobalIsImmutable:=gt.immutable;
      end;

    procedure TWasmObjData.DeclareFuncType_Pass0(ft: tai_functype);
      var
        i: Integer;
        ObjSymExtraData: TWasmObjSymbolExtraData;
      begin
        FLastFuncName:=ft.funcname;
        i:=FFuncTypes.AddOrGetFuncType(ft.functype);
        ObjSymExtraData:=AddOrCreateObjSymbolExtraData(ft.funcname);
        ObjSymExtraData.TypeIdx:=i;
      end;

    procedure TWasmObjData.DeclareFuncType_Pass1(ft: tai_functype);
      begin
        FLastFuncName:=ft.funcname;
      end;

    procedure TWasmObjData.DeclareFuncType_Pass2(ft: tai_functype);
      begin
        FLastFuncName:=ft.funcname;
      end;

    procedure TWasmObjData.DeclareTagType(tt: tai_tagtype);
      var
        ObjSymExtraData: TWasmObjSymbolExtraData;
        ft: TWasmFuncType;
        i: Integer;
      begin
        ObjSymExtraData:=AddOrCreateObjSymbolExtraData(tt.tagname);
        ft:=TWasmFuncType.Create([],tt.params);
        i:=FFuncTypes.AddOrGetFuncType(ft);
        ft.free;
        ObjSymExtraData.ExceptionTagTypeIdx:=i;
      end;

    procedure TWasmObjData.DeclareExportName(en: tai_export_name);
      var
        ObjSymExtraData: TWasmObjSymbolExtraData;
      begin
        ObjSymExtraData:=AddOrCreateObjSymbolExtraData(en.intname);
        ObjSymExtraData.ExportName:=en.extname;
      end;

    procedure TWasmObjData.DeclareImportModule(aim: tai_import_module);
      var
        ObjSymExtraData: TWasmObjSymbolExtraData;
      begin
        ObjSymExtraData:=AddOrCreateObjSymbolExtraData(aim.symname);
        ObjSymExtraData.ImportModule:=aim.importmodule;
      end;

    procedure TWasmObjData.DeclareImportName(ain: tai_import_name);
      var
        ObjSymExtraData: TWasmObjSymbolExtraData;
      begin
        ObjSymExtraData:=AddOrCreateObjSymbolExtraData(ain.symname);
        ObjSymExtraData.ImportName:=ain.importname;
      end;

    procedure TWasmObjData.DeclareLocals_Pass0(al: tai_local);
      var
        ObjSymExtraData: TWasmObjSymbolExtraData;
      begin
        ObjSymExtraData:=TWasmObjSymbolExtraData(FObjSymbolsExtraDataList.Find(FLastFuncName));
        ObjSymExtraData.AddLocals(al.locals);
        alloc(ObjSymExtraData.EncodedLocals.size);
      end;

    procedure TWasmObjData.DeclareLocals_Pass1(al: tai_local);
      var
        ObjSymExtraData: TWasmObjSymbolExtraData;
      begin
        ObjSymExtraData:=TWasmObjSymbolExtraData(FObjSymbolsExtraDataList.Find(FLastFuncName));
        alloc(ObjSymExtraData.EncodedLocals.size);
      end;

    procedure TWasmObjData.WriteLocals_Pass2(al: tai_local);
      var
        ObjSymExtraData: TWasmObjSymbolExtraData;
        d: tdynamicarray;
        buf: array [0..4095] of byte;
        bs,size: Integer;
      begin
        ObjSymExtraData:=TWasmObjSymbolExtraData(FObjSymbolsExtraDataList.Find(FLastFuncName));
        d:=ObjSymExtraData.EncodedLocals;
        d.seek(0);
        size:=d.size;
        while size>0 do
          begin
            if size<SizeOf(buf) then
              bs:=Integer(size)
            else
              bs:=SizeOf(buf);
            d.read(buf,bs);
            writebytes(buf,bs);
            dec(size,bs);
          end;
      end;

    procedure TWasmObjData.symbolpairdefine(akind: TSymbolPairKind; const asym, avalue: string);
      var
        valsym: TObjSymbol;
        aliassym: TWasmObjSymbol;
      begin
        valsym:=CreateSymbol(avalue);
        aliassym:=TWasmObjSymbol(symboldefine(asym,valsym.bind,valsym.typ));
        aliassym.AliasOf:=valsym.Name;
      end;

{****************************************************************************
                               TWasmObjOutput
****************************************************************************}

    procedure TWasmObjOutput.WriteWasmSection(wsid: TWasmSectionID);
      var
        b: byte;
      begin
        b:=ord(wsid);
        Writer.write(b,1);
        WriteUleb(Writer,FWasmSections[wsid].size);
        Writer.writearray(FWasmSections[wsid]);
      end;

    procedure TWasmObjOutput.WriteWasmCustomSection(wcst: TWasmCustomSectionType);
      var
        b: byte;
      begin
        b:=0;
        Writer.write(b,1);
        WriteUleb(Writer,FWasmCustomSections[wcst].size);
        Writer.writearray(FWasmCustomSections[wcst]);
      end;

    function TWasmObjOutput.IsExternalFunction(sym: TObjSymbol): Boolean;
      var
        ExtraData: TWasmObjSymbolExtraData;
      begin
        if sym.bind=AB_EXTERNAL then
          begin
            ExtraData:=TWasmObjSymbolExtraData(TWasmObjData(sym.ObjData).FObjSymbolsExtraDataList.Find(sym.Name));
            result:=(ExtraData<>nil) and (ExtraData.TypeIdx<>-1);
          end
        else
          result:=false;

      end;

    function TWasmObjOutput.IsExportedFunction(sym: TWasmObjSymbol): Boolean;
      var
        ExtraData: TWasmObjSymbolExtraData;
      begin
        if (sym.typ=AT_FUNCTION) and not sym.IsAlias then
          begin
            ExtraData:=TWasmObjSymbolExtraData(TWasmObjData(sym.ObjData).FObjSymbolsExtraDataList.Find(sym.Name));
            result:=(ExtraData<>nil) and (ExtraData.ExportName<>'');
          end
        else
          result:=false;
      end;

    procedure TWasmObjOutput.WriteFunctionCode(dest: tdynamicarray; objsym: TObjSymbol);
      var
        ObjSymExtraData: TWasmObjSymbolExtraData;
        ObjSection: TWasmObjSection;
        codelen: QWord;
      begin
        ObjSymExtraData:=TWasmObjSymbolExtraData(FData.FObjSymbolsExtraDataList.Find(objsym.Name));
        ObjSection:=TWasmObjSection(objsym.objsection);
        ObjSection.Data.seek(objsym.address);
        codelen:=objsym.size;

        WriteUleb(dest,codelen);
        ObjSection.FileSectionOfs:=dest.size-objsym.offset;
        CopyDynamicArray(ObjSection.Data,dest,codelen);
      end;

    procedure TWasmObjOutput.WriteSymbolTable;
      begin
        WriteUleb(FWasmLinkingSubsections[WASM_SYMBOL_TABLE],FWasmSymbolTableEntriesCount);
        FWasmSymbolTable.seek(0);
        CopyDynamicArray(FWasmSymbolTable,FWasmLinkingSubsections[WASM_SYMBOL_TABLE],FWasmSymbolTable.size);
      end;

    procedure TWasmObjOutput.WriteRelocationCodeTable(CodeSectionIndex: Integer);
      begin
        WriteUleb(FWasmCustomSections[wcstRelocCode],CodeSectionIndex);
        WriteUleb(FWasmCustomSections[wcstRelocCode],FWasmRelocationCodeTableEntriesCount);
        FWasmRelocationCodeTable.seek(0);
        CopyDynamicArray(FWasmRelocationCodeTable,FWasmCustomSections[wcstRelocCode],FWasmRelocationCodeTable.size);
      end;

    procedure TWasmObjOutput.WriteRelocationDataTable(DataSectionIndex: Integer);
      begin
        WriteUleb(FWasmCustomSections[wcstRelocData],DataSectionIndex);
        WriteUleb(FWasmCustomSections[wcstRelocData],FWasmRelocationDataTableEntriesCount);
        FWasmRelocationDataTable.seek(0);
        CopyDynamicArray(FWasmRelocationDataTable,FWasmCustomSections[wcstRelocData],FWasmRelocationDataTable.size);
      end;

    procedure TWasmObjOutput.MaybeWriteRelocationDebugTable(cst: TWasmCustomSectionType; SectionIndex: Integer; EntriesCount: Integer; Table: tdynamicarray);
      begin
        if EntriesCount>0 then
          begin
            WriteUleb(FWasmCustomSections[cst],SectionIndex);
            WriteUleb(FWasmCustomSections[cst],EntriesCount);
            Table.seek(0);
            CopyDynamicArray(Table,FWasmCustomSections[cst],Table.size);
            WriteWasmCustomSection(cst);
          end;
      end;

    procedure TWasmObjOutput.WriteLinkingSubsection(wlst: TWasmLinkingSubsectionType);
      begin
        if FWasmLinkingSubsections[wlst].size>0 then
          begin
            WriteByte(FWasmCustomSections[wcstLinking],Ord(wlst));
            WriteUleb(FWasmCustomSections[wcstLinking],FWasmLinkingSubsections[wlst].size);
            FWasmLinkingSubsections[wlst].seek(0);
            CopyDynamicArray(FWasmLinkingSubsections[wlst],FWasmCustomSections[wcstLinking],FWasmLinkingSubsections[wlst].size);
          end;
      end;

    procedure TWasmObjOutput.DoRelocations;
      var
        si, ri: Integer;
        objsec: TWasmObjSection;
        objrel: TWasmObjRelocation;
      begin
        for si:=0 to FData.ObjSectionList.Count-1 do
          begin
            objsec:=TWasmObjSection(FData.ObjSectionList[si]);
            for ri:=0 to objsec.ObjRelocations.Count-1 do
              begin
                objrel:=TWasmObjRelocation(objsec.ObjRelocations[ri]);
                case objrel.typ of
                  RELOC_FUNCTION_INDEX_LEB:
                    begin
                      if not assigned(objrel.symbol) then
                        internalerror(2021092509);
                      objsec.Data.seek(objrel.DataOffset);
                      if TWasmObjSymbol(objrel.symbol).FuncIndex<0 then
                        message1(asmw_e_illegal_unset_index,objrel.symbol.name)
                      else
                        WriteUleb5(objsec.Data,TWasmObjSymbol(objrel.symbol).FuncIndex);
                    end;
                  RELOC_MEMORY_ADDR_OR_TABLE_INDEX_SLEB:
                    begin
                      if not assigned(objrel.symbol) then
                        internalerror(2021092605);
                      if not (IsExternalFunction(objrel.symbol) or (objrel.symbol.typ=AT_FUNCTION) or (objrel.symbol.bind=AB_EXTERNAL)) then
                        begin
                          objsec.Data.seek(objrel.DataOffset);
                          AddSleb5(objsec.Data,objrel.symbol.offset+TWasmObjSection(objrel.symbol.objsection).SegOfs);
                        end;
                    end;
                  RELOC_MEMORY_ADDR_LEB:
                    begin
                      if not assigned(objrel.symbol) then
                        internalerror(2021092606);
                      if IsExternalFunction(objrel.symbol) or (objrel.symbol.typ=AT_FUNCTION) then
                        internalerror(2021092628);
                      if objrel.symbol.bind<>AB_EXTERNAL then
                        begin
                          objsec.Data.seek(objrel.DataOffset);
                          AddUleb5(objsec.Data,objrel.symbol.offset+TWasmObjSection(objrel.symbol.objsection).SegOfs);
                        end;
                    end;
                  RELOC_ABSOLUTE:
                    begin
                      if assigned(objrel.ObjSection) then
                        begin
                          { todo: should we do something here? }
                          //Writeln('todo: section relocation');
                        end
                      else if not (IsExternalFunction(objrel.symbol) or (objrel.symbol.typ=AT_FUNCTION) or (objrel.symbol.bind=AB_EXTERNAL)) then
                        begin
                          objsec.Data.seek(objrel.DataOffset);
                          AddInt32(objsec.Data,objrel.symbol.offset+TWasmObjSection(objrel.symbol.objsection).SegOfs);
                        end;
                    end;
                  RELOC_TYPE_INDEX_LEB:
                    ;
                  RELOC_GLOBAL_INDEX_LEB:
                    begin
                      if not assigned(objrel.symbol) then
                        internalerror(2021092509);
                      objsec.Data.seek(objrel.DataOffset);
                      if TWasmObjSymbol(objrel.symbol).GlobalIndex<0 then
                        message1(asmw_e_illegal_unset_index,objrel.symbol.name)
                      else
                        WriteUleb5(objsec.Data,TWasmObjSymbol(objrel.symbol).GlobalIndex);
                    end;
                  RELOC_TAG_INDEX_LEB:
                    begin
                      if not assigned(objrel.symbol) then
                        internalerror(2021092716);
                      objsec.Data.seek(objrel.DataOffset);
                      if TWasmObjSymbol(objrel.symbol).TagIndex<0 then
                        message1(asmw_e_illegal_unset_index,objrel.symbol.name)
                      else
                        WriteSleb5(objsec.Data,TWasmObjSymbol(objrel.symbol).TagIndex);
                    end;
                  else
                    internalerror(2021092510);
                end;
              end;
          end;
      end;

    procedure TWasmObjOutput.WriteRelocations;
      var
        si, ri: Integer;
        objsec: TWasmObjSection;
        objrel: TWasmObjRelocation;
        relout: tdynamicarray;
        relcount: PInteger;
        FuncSym: TWasmObjSymbol;
      begin
        for si:=0 to FData.ObjSectionList.Count-1 do
          begin
            objsec:=TWasmObjSection(FData.ObjSectionList[si]);
            if objsec.IsCode then
              begin
                relout:=FWasmRelocationCodeTable;
                relcount:=@FWasmRelocationCodeTableEntriesCount;
              end
            else if objsec.IsData then
              begin
                relout:=FWasmRelocationDataTable;
                relcount:=@FWasmRelocationDataTableEntriesCount;
              end
            else if objsec.IsDebug then
              begin
                case objsec.Name of
                  '.debug_frame':
                    begin
                      relout:=FWasmRelocationDebugFrameTable;
                      relcount:=@FWasmRelocationDebugFrameTableEntriesCount;
                    end;
                  '.debug_info':
                    begin
                      relout:=FWasmRelocationDebugInfoTable;
                      relcount:=@FWasmRelocationDebugInfoTableEntriesCount;
                    end;
                  '.debug_line':
                    begin
                      relout:=FWasmRelocationDebugLineTable;
                      relcount:=@FWasmRelocationDebugLineTableEntriesCount;
                    end;
                  '.debug_abbrev':
                    begin
                      relout:=FWasmRelocationDebugAbbrevTable;
                      relcount:=@FWasmRelocationDebugAbbrevTableEntriesCount;
                    end;
                  '.debug_aranges':
                    begin
                      relout:=FWasmRelocationDebugArangesTable;
                      relcount:=@FWasmRelocationDebugArangesTableEntriesCount;
                    end;
                  '.debug_ranges':
                    begin
                      relout:=FWasmRelocationDebugRangesTable;
                      relcount:=@FWasmRelocationDebugRangesTableEntriesCount;
                    end;
                  '.debug_str':
                    begin
                      relout:=FWasmRelocationDebugStrTable;
                      relcount:=@FWasmRelocationDebugStrTableEntriesCount;
                    end;
                  else
                    internalerror(2022071601);
                end;
              end
            else
              continue;
            for ri:=0 to objsec.ObjRelocations.Count-1 do
              begin
                objrel:=TWasmObjRelocation(objsec.ObjRelocations[ri]);
                case objrel.typ of
                  RELOC_FUNCTION_INDEX_LEB:
                    begin
                      if not assigned(objrel.symbol) then
                        internalerror(2021092508);
                      Inc(relcount^);
                      WriteByte(relout,Ord(R_WASM_FUNCTION_INDEX_LEB));
                      WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
                      WriteUleb(relout,TWasmObjSymbol(objrel.symbol).SymbolIndex);
                    end;
                  RELOC_MEMORY_ADDR_LEB:
                    begin
                      if not assigned(objrel.symbol) then
                        internalerror(2021092603);
                      Inc(relcount^);
                      if IsExternalFunction(objrel.symbol) or (objrel.symbol.typ=AT_FUNCTION) then
                        internalerror(2021092628);
                      WriteByte(relout,Ord(R_WASM_MEMORY_ADDR_LEB));
                      WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
                      WriteUleb(relout,TWasmObjSymbol(objrel.symbol).SymbolIndex);
                      WriteSleb(relout,objrel.Addend);  { addend to add to the address }
                    end;
                  RELOC_MEMORY_ADDR_OR_TABLE_INDEX_SLEB:
                    begin
                      if not assigned(objrel.symbol) then
                        internalerror(2021092604);
                      Inc(relcount^);
                      if IsExternalFunction(objrel.symbol) or (objrel.symbol.typ=AT_FUNCTION) then
                        begin
                          WriteByte(relout,Ord(R_WASM_TABLE_INDEX_SLEB));
                          WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
                          WriteUleb(relout,TWasmObjSymbol(objrel.symbol).SymbolIndex);
                        end
                      else
                        begin
                          WriteByte(relout,Ord(R_WASM_MEMORY_ADDR_SLEB));
                          WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
                          WriteUleb(relout,TWasmObjSymbol(objrel.symbol).SymbolIndex);
                          WriteSleb(relout,objrel.Addend);  { addend to add to the address }
                        end;
                    end;
                  RELOC_ABSOLUTE:
                    begin
                      // todo: figure this out, why do these exist?
                      //if assigned(objrel.symbol) and not assigned(objrel.symbol.objsection) then
                      //  Writeln('!!! ', objrel.symbol.name);
                      if assigned(objrel.objsection) then
                        begin
                          Inc(relcount^);
                          WriteByte(relout,Ord(R_WASM_SECTION_OFFSET_I32));
                          WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
			  if (TWasmObjSection(objrel.objsection).SegSymIdx<0) then
                            message1(asmw_e_illegal_unset_index,objrel.objsection.name)
                          else
                            WriteUleb(relout,TWasmObjSection(objrel.objsection).SegSymIdx);
                          WriteSleb(relout,objrel.Addend);  { addend to add to the address }
                        end
                      else if (IsExternalFunction(objrel.symbol) or (objrel.symbol.typ=AT_FUNCTION)) and not objsec.IsDebug then
                        begin
                          Inc(relcount^);
                          WriteByte(relout,Ord(R_WASM_TABLE_INDEX_I32));
                          WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
                          WriteUleb(relout,TWasmObjSymbol(objrel.symbol).SymbolIndex);
                        end
                      else if assigned(objrel.symbol) and assigned(objrel.symbol.objsection) and TWasmObjSection(objrel.symbol.objsection).IsCode then
                        begin
                          Inc(relcount^);
                          WriteByte(relout,Ord(R_WASM_FUNCTION_OFFSET_I32));
                          WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
                          FuncSym:=FindFunctionSymbol(TWasmObjSymbol(objrel.Symbol));
			  if FuncSym.SymbolIndex<0 then
                            message1(asmw_e_illegal_unset_index,FuncSym.Name)
                          else
                            WriteUleb(relout,FuncSym.SymbolIndex);
                          WriteSleb(relout,objrel.Addend+objrel.symbol.address)  { addend to add to the address }
                        end
                      else if assigned(objrel.symbol) and (objrel.symbol.typ=AT_WASM_GLOBAL) then
                        begin
                          Inc(relcount^);
                          WriteByte(relout,Ord(R_WASM_GLOBAL_INDEX_I32));
                          WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
			  if (TWasmObjSymbol(objrel.symbol).SymbolIndex<0) then
                            message1(asmw_e_illegal_unset_index,objrel.symbol.name)
                          else
                            WriteUleb(relout,TWasmObjSymbol(objrel.symbol).SymbolIndex);
                        end
                      else
                        begin
                          Inc(relcount^);
                          WriteByte(relout,Ord(R_WASM_MEMORY_ADDR_I32));
                          WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
			  if (TWasmObjSymbol(objrel.symbol).SymbolIndex<0) then
                            begin
                              Writeln(objrel.symbol.objsection.Name, ' ', objrel.symbol.name, ' ', objsec.Name);
                              message1(asmw_e_illegal_unset_index,objrel.symbol.name);
                            end
                          else
                            WriteUleb(relout,TWasmObjSymbol(objrel.symbol).SymbolIndex);
                          WriteSleb(relout,objrel.Addend);  { addend to add to the address }
                        end;
                    end;
                  RELOC_TYPE_INDEX_LEB:
                    begin
                      Inc(relcount^);
                      WriteByte(relout,Ord(R_WASM_TYPE_INDEX_LEB));
                      WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
                      WriteUleb(relout,objrel.TypeIndex);
                    end;
                  RELOC_GLOBAL_INDEX_LEB:
                    begin
                      if not assigned(objrel.symbol) then
                        internalerror(2021092704);
                      Inc(relcount^);
                      WriteByte(relout,Ord(R_WASM_GLOBAL_INDEX_LEB));
                      WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
                      if (TWasmObjSymbol(objrel.symbol).SymbolIndex<0) then
                        message1(asmw_e_illegal_unset_index,objrel.symbol.name)
                      else
                        WriteUleb(relout,TWasmObjSymbol(objrel.symbol).SymbolIndex);
                    end;
                  RELOC_TAG_INDEX_LEB:
                    begin
                      if not assigned(objrel.symbol) then
                        internalerror(2021092717);
                      Inc(relcount^);
                      WriteByte(relout,Ord(R_WASM_TAG_INDEX_LEB));
                      WriteUleb(relout,objrel.DataOffset+objsec.FileSectionOfs);
                      if (TWasmObjSymbol(objrel.symbol).SymbolIndex<0) then
                        message1(asmw_e_illegal_unset_index,objrel.symbol.name)
                      else
                        WriteUleb(relout,TWasmObjSymbol(objrel.symbol).SymbolIndex);
                    end;
                  else
                    internalerror(2021092507);
                end;
              end;
          end;
      end;

    function TWasmObjOutput.FindFunctionSymbol(Symbol: TWasmObjSymbol): TWasmObjSymbol;
      begin
        Result:=TWasmObjSection(Symbol.objsection).MainFuncSymbol;
      end;

    function TWasmObjOutput.writeData(Data:TObjData):boolean;
      var
        section_nr: Integer;

        procedure MaybeAddDebugSectionToSymbolTable(st: TWasmCustomDebugSectionType; var debug_section_nr: Integer);
          var
            objsec: TWasmObjSection;
          begin
            objsec:=TWasmObjSection(Data.ObjSectionList.Find(WasmCustomSectionName[st]));
            if Assigned(objsec) then
              begin
                debug_section_nr:=section_nr;
                Inc(section_nr);
                objsec.SegSymIdx:=FWasmSymbolTableEntriesCount;
                objsec.CustomSectionIdx:=debug_section_nr;
                Inc(FWasmSymbolTableEntriesCount);
                WriteByte(FWasmSymbolTable,Ord(SYMTAB_SECTION));
                WriteUleb(FWasmSymbolTable,WASM_SYM_BINDING_LOCAL);
                WriteUleb(FWasmSymbolTable,debug_section_nr);
              end;
          end;

        procedure MaybeWriteDebugSection(st: TWasmCustomDebugSectionType);
          var
            objsec: TWasmObjSection;
          begin
            objsec:=TWasmObjSection(Data.ObjSectionList.Find(WasmCustomSectionName[st]));
            if Assigned(objsec) then
              begin
                if oso_Data in objsec.SecOptions then
                  begin
                    objsec.Data.seek(0);
                    CopyDynamicArray(objsec.Data,FWasmCustomSections[st],objsec.Size);
                  end
                else
                  WriteZeros(FWasmCustomSections[st],objsec.Size);
                WriteWasmCustomSection(st);
              end;
          end;

      var
        i: Integer;
        objsec: TWasmObjSection;
        segment_count: Integer = 0;
        cur_seg_ofs: qword = 0;
        imports_count, NextImportFunctionIndex, NextFunctionIndex,
        code_section_nr, data_section_nr,
        debug_abbrev_section_nr,debug_info_section_nr,debug_str_section_nr,
        debug_line_section_nr,debug_frame_section_nr,debug_aranges_section_nr,
        debug_ranges_section_nr,
        NextGlobalIndex, NextTagIndex: Integer;
        import_globals_count: Integer = 0;
        globals_count: Integer = 0;
        import_functions_count: Integer = 0;
        export_functions_count: Integer = 0;
        functions_count: Integer = 0;
        import_exception_tags_count: Integer = 0;
        exception_tags_count: Integer = 0;
        objsym, ObjSymAlias: TWasmObjSymbol;
        cust_sec: TWasmCustomSectionType;
        SegmentFlags, SymbolFlags: UInt64;
      begin
        FData:=TWasmObjData(Data);

        { each custom sections starts with its name }
        for cust_sec in TWasmCustomSectionType do
          WriteName(FWasmCustomSections[cust_sec],WasmCustomSectionName[cust_sec]);

        WriteUleb(FWasmCustomSections[wcstLinking],2);  { linking metadata version }

        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TWasmObjSymbol(Data.ObjSymbolList[i]);
            if objsym.typ=AT_WASM_EXCEPTION_TAG then
              if objsym.bind=AB_EXTERNAL then
                Inc(import_exception_tags_count)
              else
                Inc(exception_tags_count);
            if objsym.typ=AT_WASM_GLOBAL then
              if objsym.bind=AB_EXTERNAL then
                Inc(import_globals_count)
              else
                Inc(globals_count);
            if (objsym.typ=AT_TLS) and (ts_wasm_threads in current_settings.targetswitches) then
              Inc(import_globals_count);
            if IsExternalFunction(objsym) then
              Inc(import_functions_count);
            if (objsym.typ=AT_FUNCTION) and not objsym.IsAlias then
              begin
                TWasmObjSection(objsym.objsection).MainFuncSymbol:=objsym;
                Inc(functions_count);
              end;
            if IsExportedFunction(objsym) then
              Inc(export_functions_count);
          end;

        FData.FFuncTypes.WriteTo(FWasmSections[wsiType]);

        for i:=0 to Data.ObjSectionList.Count-1 do
          begin
            objsec:=TWasmObjSection(Data.ObjSectionList[i]);
            if objsec.IsCode then
              objsec.SegIdx:=-1
            else if objsec.IsData then
              begin
                objsec.SegIdx:=segment_count;
                objsec.SegOfs:=cur_seg_ofs;
                Inc(segment_count);
                Inc(cur_seg_ofs,objsec.Size);
              end;
          end;

        imports_count:=2+import_globals_count+import_functions_count+import_exception_tags_count;
        WriteUleb(FWasmSections[wsiImport],imports_count);
        { import memories }
        WriteName(FWasmSections[wsiImport],'env');
        WriteName(FWasmSections[wsiImport],'__linear_memory');
        WriteByte(FWasmSections[wsiImport],$02);  { mem }
        WriteByte(FWasmSections[wsiImport],$00);  { min }
        WriteUleb(FWasmSections[wsiImport],1);    { 1 page }
        { import globals }
        NextGlobalIndex:=0;
        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TWasmObjSymbol(Data.ObjSymbolList[i]);
            if (objsym.bind=AB_EXTERNAL) and (objsym.typ=AT_WASM_GLOBAL) then
              begin
                objsym.GlobalIndex:=NextGlobalIndex;
                Inc(NextGlobalIndex);
                objsym.ExtraData:=TWasmObjSymbolExtraData(FData.FObjSymbolsExtraDataList.Find(objsym.Name));
                if objsym.ExtraData.ImportModule<>'' then
                  WriteName(FWasmSections[wsiImport],objsym.ExtraData.ImportModule)
                else
                  WriteName(FWasmSections[wsiImport],'env');
                WriteName(FWasmSections[wsiImport],objsym.Name);
                WriteByte(FWasmSections[wsiImport],$03);  { global }
                WriteWasmBasicType(FWasmSections[wsiImport],objsym.ExtraData.GlobalType);
                if objsym.ExtraData.GlobalIsImmutable then
                  WriteByte(FWasmSections[wsiImport],$00)   { const }
                else
                  WriteByte(FWasmSections[wsiImport],$01);  { var }
              end
            else if (objsym.typ=AT_TLS) and (ts_wasm_threads in current_settings.targetswitches) then
              begin
                objsym.GlobalIndex:=NextGlobalIndex;
                Inc(NextGlobalIndex);
                objsym.ExtraData:=nil;
                WriteName(FWasmSections[wsiImport],'GOT.mem');
                WriteName(FWasmSections[wsiImport],objsym.Name);
                WriteByte(FWasmSections[wsiImport],$03);  { global }
                WriteWasmBasicType(FWasmSections[wsiImport],wbt_i32);  { i32 }
                WriteByte(FWasmSections[wsiImport],$01);  { var }
              end;
          end;
        { import functions }
        NextImportFunctionIndex:=0;
        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TWasmObjSymbol(Data.ObjSymbolList[i]);
            if IsExternalFunction(objsym) then
              begin
                objsym.FuncIndex:=NextImportFunctionIndex;
                Inc(NextImportFunctionIndex);
                objsym.ExtraData:=TWasmObjSymbolExtraData(FData.FObjSymbolsExtraDataList.Find(objsym.Name));
                if objsym.ExtraData.ImportModule<>'' then
                  WriteName(FWasmSections[wsiImport],objsym.ExtraData.ImportModule)
                else
                  WriteName(FWasmSections[wsiImport],'env');
                WriteName(FWasmSections[wsiImport],objsym.Name);
                WriteByte(FWasmSections[wsiImport],$00);  { func }
                WriteUleb(FWasmSections[wsiImport],TWasmObjSymbolExtraData(FData.FObjSymbolsExtraDataList.Find(objsym.Name)).TypeIdx);
              end;
          end;
        { import tables }
        WriteName(FWasmSections[wsiImport],'env');
        WriteName(FWasmSections[wsiImport],'__indirect_function_table');
        WriteByte(FWasmSections[wsiImport],$01);  { table }
        WriteByte(FWasmSections[wsiImport],$70);  { funcref }
        WriteByte(FWasmSections[wsiImport],$00);  { min }
        WriteUleb(FWasmSections[wsiImport],1);    { 1 }
        { import tags }
        NextTagIndex:=0;
        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TWasmObjSymbol(Data.ObjSymbolList[i]);
            if (objsym.typ=AT_WASM_EXCEPTION_TAG) and (objsym.bind=AB_EXTERNAL) then
              begin
                objsym.TagIndex:=NextTagIndex;
                Inc(NextTagIndex);
                objsym.ExtraData:=TWasmObjSymbolExtraData(FData.FObjSymbolsExtraDataList.Find(objsym.Name));
                if objsym.ExtraData.ImportModule<>'' then
                  WriteName(FWasmSections[wsiImport],objsym.ExtraData.ImportModule)
                else
                  WriteName(FWasmSections[wsiImport],'env');
                WriteName(FWasmSections[wsiImport],objsym.Name);
                WriteByte(FWasmSections[wsiImport],$04);  { tag }
                WriteByte(FWasmSections[wsiImport],$00);  { exception }
                WriteUleb(FWasmSections[wsiImport],objsym.ExtraData.ExceptionTagTypeIdx);
              end;
          end;

        WriteUleb(FWasmSections[wsiFunction],functions_count);
        NextFunctionIndex:=NextImportFunctionIndex;
        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TWasmObjSymbol(Data.ObjSymbolList[i]);
            if (objsym.typ=AT_FUNCTION) and not objsym.IsAlias then
              begin
                objsym.FuncIndex:=NextFunctionIndex;
                Inc(NextFunctionIndex);
                WriteUleb(FWasmSections[wsiFunction],TWasmObjSymbolExtraData(FData.FObjSymbolsExtraDataList.Find(objsym.Name)).TypeIdx);
              end;
          end;

        if exception_tags_count>0 then
          begin
            WriteUleb(FWasmSections[wsiTag],exception_tags_count);
            for i:=0 to Data.ObjSymbolList.Count-1 do
              begin
                objsym:=TWasmObjSymbol(Data.ObjSymbolList[i]);
                if (objsym.typ=AT_WASM_EXCEPTION_TAG) and (objsym.bind<>AB_EXTERNAL) then
                  begin
                    objsym.TagIndex:=NextTagIndex;
                    Inc(NextTagIndex);
                    objsym.ExtraData:=TWasmObjSymbolExtraData(FData.FObjSymbolsExtraDataList.Find(objsym.Name));
                    WriteByte(FWasmSections[wsiTag],$00);  { exception }
                    WriteUleb(FWasmSections[wsiTag],objsym.ExtraData.ExceptionTagTypeIdx);
                  end;
              end;
          end;

        if globals_count>0 then
          begin
            WriteUleb(FWasmSections[wsiGlobal],globals_count);
            for i:=0 to Data.ObjSymbolList.Count-1 do
              begin
                objsym:=TWasmObjSymbol(Data.ObjSymbolList[i]);
                if (objsym.typ=AT_WASM_GLOBAL) and (objsym.bind<>AB_EXTERNAL) then
                  begin
                    objsym.GlobalIndex:=NextGlobalIndex;
                    Inc(NextGlobalIndex);
                    objsym.ExtraData:=TWasmObjSymbolExtraData(FData.FObjSymbolsExtraDataList.Find(objsym.Name));
                    WriteWasmBasicType(FWasmSections[wsiGlobal],objsym.ExtraData.GlobalType);
                    if objsym.ExtraData.GlobalIsImmutable then
                      WriteByte(FWasmSections[wsiGlobal],$00)   { const }
                    else
                      WriteByte(FWasmSections[wsiGlobal],$01);  { var }
                    { init expr }
                    case objsym.ExtraData.GlobalType of
                      wbt_i32:
                        begin
                          WriteByte(FWasmSections[wsiGlobal],$41);  { i32.const }
                          WriteByte(FWasmSections[wsiGlobal],0);    { 0 (in signed LEB128 format) }
                          WriteByte(FWasmSections[wsiGlobal],$0B);  { end }
                        end;
                      wbt_i64:
                        begin
                          WriteByte(FWasmSections[wsiGlobal],$42);  { i64.const }
                          WriteByte(FWasmSections[wsiGlobal],0);    { 0 (in signed LEB128 format) }
                          WriteByte(FWasmSections[wsiGlobal],$0B);  { end }
                        end;
                      wbt_f32:
                        begin
                          WriteByte(FWasmSections[wsiGlobal],$43);  { f32.const }
                          WriteByte(FWasmSections[wsiGlobal],$00);  { 0 (in little endian IEEE single precision floating point format) }
                          WriteByte(FWasmSections[wsiGlobal],$00);
                          WriteByte(FWasmSections[wsiGlobal],$00);
                          WriteByte(FWasmSections[wsiGlobal],$00);
                          WriteByte(FWasmSections[wsiGlobal],$0B);  { end }
                        end;
                      wbt_f64:
                        begin
                          WriteByte(FWasmSections[wsiGlobal],$44);  { f64.const }
                          WriteByte(FWasmSections[wsiGlobal],$00);  { 0 (in little endian IEEE double precision floating point format) }
                          WriteByte(FWasmSections[wsiGlobal],$00);
                          WriteByte(FWasmSections[wsiGlobal],$00);
                          WriteByte(FWasmSections[wsiGlobal],$00);
                          WriteByte(FWasmSections[wsiGlobal],$00);
                          WriteByte(FWasmSections[wsiGlobal],$00);
                          WriteByte(FWasmSections[wsiGlobal],$00);
                          WriteByte(FWasmSections[wsiGlobal],$00);
                          WriteByte(FWasmSections[wsiGlobal],$0B);  { end }
                        end;
                      wbt_externref:
                        begin
                          WriteByte(FWasmSections[wsiGlobal],$D0); { ref.null extern }
                          WriteByte(FWasmSections[wsiGlobal],$6F);
                          WriteByte(FWasmSections[wsiGlobal],$0B);  { end }
                        end;
                      wbt_funcref:
                        begin
                          WriteByte(FWasmSections[wsiGlobal],$D0); { ref.null func }
                          WriteByte(FWasmSections[wsiGlobal],$70);
                          WriteByte(FWasmSections[wsiGlobal],$0B);  { end }
                        end;
                      else
                        internalerror(2022052801);
                    end;
                  end;
              end;
          end;

        if export_functions_count>0 then
          begin
            WriteUleb(FWasmSections[wsiExport],export_functions_count);
            for i:=0 to Data.ObjSymbolList.Count-1 do
              begin
                objsym:=TWasmObjSymbol(Data.ObjSymbolList[i]);
                if IsExportedFunction(objsym) then
                  begin
                    WriteName(FWasmSections[wsiExport],TWasmObjSymbolExtraData(FData.FObjSymbolsExtraDataList.Find(objsym.Name)).ExportName);
                    WriteByte(FWasmSections[wsiExport],0);  { func }
                    if (objsym.FuncIndex<0) then
                      message1(asmw_e_illegal_unset_index,objsym.name)
                    else
                      WriteUleb(FWasmSections[wsiExport],objsym.FuncIndex);
                  end;
              end;
          end;

        Writer.write(WasmModuleMagic,SizeOf(WasmModuleMagic));
        Writer.write(WasmVersion,SizeOf(WasmVersion));

        if ts_wasm_threads in current_settings.targetswitches then
          begin
            WriteUleb(FWasmCustomSections[wcstTargetFeatures],4);
            WriteUleb(FWasmCustomSections[wcstTargetFeatures],$2B);
            WriteName(FWasmCustomSections[wcstTargetFeatures],'atomics');
            WriteUleb(FWasmCustomSections[wcstTargetFeatures],$2B);
            WriteName(FWasmCustomSections[wcstTargetFeatures],'bulk-memory');
            WriteUleb(FWasmCustomSections[wcstTargetFeatures],$2B);
            WriteName(FWasmCustomSections[wcstTargetFeatures],'mutable-globals');
            WriteUleb(FWasmCustomSections[wcstTargetFeatures],$2B);
            WriteName(FWasmCustomSections[wcstTargetFeatures],'sign-ext');
          end
        else
          begin
            WriteUleb(FWasmCustomSections[wcstTargetFeatures],3);
            WriteUleb(FWasmCustomSections[wcstTargetFeatures],$2B);
            WriteName(FWasmCustomSections[wcstTargetFeatures],'bulk-memory');
            WriteUleb(FWasmCustomSections[wcstTargetFeatures],$2B);
            WriteName(FWasmCustomSections[wcstTargetFeatures],'mutable-globals');
            WriteUleb(FWasmCustomSections[wcstTargetFeatures],$2B);
            WriteName(FWasmCustomSections[wcstTargetFeatures],'sign-ext');
          end;

        { Write the producers section:
          https://github.com/WebAssembly/tool-conventions/blob/main/ProducersSection.md }
        WriteUleb(FWasmCustomSections[wcstProducers],2);
        WriteName(FWasmCustomSections[wcstProducers],'language');
        WriteUleb(FWasmCustomSections[wcstProducers],1);
        WriteName(FWasmCustomSections[wcstProducers],'Pascal');
        WriteName(FWasmCustomSections[wcstProducers],'');
        WriteName(FWasmCustomSections[wcstProducers],'processed-by');
        WriteUleb(FWasmCustomSections[wcstProducers],1);
        WriteName(FWasmCustomSections[wcstProducers],'Free Pascal Compiler (FPC)');
        WriteName(FWasmCustomSections[wcstProducers],full_version_string+' ['+date_string+'] for '+target_cpu_string+' - '+target_info.shortname);

        code_section_nr:=-1;
        data_section_nr:=-1;
        debug_abbrev_section_nr:=-1;
        debug_info_section_nr:=-1;
        debug_str_section_nr:=-1;
        debug_line_section_nr:=-1;
        debug_frame_section_nr:=-1;
        debug_aranges_section_nr:=-1;
        debug_ranges_section_nr:=-1;
        section_nr:=0;

        WriteWasmSection(wsiType);
        Inc(section_nr);
        WriteWasmSection(wsiImport);
        Inc(section_nr);
        WriteWasmSection(wsiFunction);
        Inc(section_nr);
        if exception_tags_count>0 then
          begin
            WriteWasmSection(wsiTag);
            Inc(section_nr);
          end;
        if globals_count>0 then
          begin
            WriteWasmSection(wsiGlobal);
            Inc(section_nr);
          end;
        if export_functions_count>0 then
          begin
            WriteWasmSection(wsiExport);
            Inc(section_nr);
          end;

        { determine the section numbers for the datacount, code, data and debug sections ahead of time }
        if segment_count>0 then
          Inc(section_nr);  { the DataCount section }
        code_section_nr:=section_nr;  { the Code section }
        Inc(section_nr);
        if segment_count>0 then
          begin
            data_section_nr:=section_nr; { the Data section }
            Inc(section_nr);
          end;
        { the debug sections }
        MaybeAddDebugSectionToSymbolTable(wcstDebugAbbrev,debug_abbrev_section_nr);
        MaybeAddDebugSectionToSymbolTable(wcstDebugInfo,debug_info_section_nr);
        MaybeAddDebugSectionToSymbolTable(wcstDebugStr,debug_str_section_nr);
        MaybeAddDebugSectionToSymbolTable(wcstDebugLine,debug_line_section_nr);
        MaybeAddDebugSectionToSymbolTable(wcstDebugFrame,debug_frame_section_nr);
        MaybeAddDebugSectionToSymbolTable(wcstDebugAranges,debug_aranges_section_nr);
        MaybeAddDebugSectionToSymbolTable(wcstDebugRanges,debug_ranges_section_nr);

        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TWasmObjSymbol(Data.ObjSymbolList[i]);
            if objsym.typ=AT_WASM_EXCEPTION_TAG then
              begin
                objsym.SymbolIndex:=FWasmSymbolTableEntriesCount;
                Inc(FWasmSymbolTableEntriesCount);
                WriteByte(FWasmSymbolTable,Ord(SYMTAB_EVENT));
                if objsym.bind=AB_GLOBAL then
                  WriteUleb(FWasmSymbolTable,0)
                else if objsym.bind=AB_LOCAL then
                  WriteUleb(FWasmSymbolTable,WASM_SYM_BINDING_LOCAL)
                else if objsym.bind=AB_EXTERNAL then
                  WriteUleb(FWasmSymbolTable,WASM_SYM_UNDEFINED)
                else if objsym.bind=AB_WEAK then
                  WriteUleb(FWasmSymbolTable,WASM_SYM_BINDING_WEAK)
                else
                  internalerror(2021092715);
                if (objsym.TagIndex<0) then
                  message1(asmw_e_illegal_unset_index,objsym.name)
                else
                  WriteUleb(FWasmSymbolTable,objsym.TagIndex);
                if objsym.bind<>AB_EXTERNAL then
                  WriteName(FWasmSymbolTable,objsym.Name);
              end
            else if objsym.typ=AT_WASM_GLOBAL then
              begin
                objsym.SymbolIndex:=FWasmSymbolTableEntriesCount;
                Inc(FWasmSymbolTableEntriesCount);
                WriteByte(FWasmSymbolTable,Ord(SYMTAB_GLOBAL));
                if objsym.bind=AB_EXTERNAL then
                  begin
                    WriteUleb(FWasmSymbolTable,WASM_SYM_UNDEFINED);
                    if (objsym.GlobalIndex<0) then
                      message1(asmw_e_illegal_unset_index,objsym.name)
                    else
                      WriteUleb(FWasmSymbolTable,objsym.GlobalIndex);
                  end
                else
                  begin
                    WriteUleb(FWasmSymbolTable,0);
                    if (objsym.GlobalIndex<0) then
                      message1(asmw_e_illegal_unset_index,objsym.name)
                    else
                      WriteUleb(FWasmSymbolTable,objsym.GlobalIndex);
                    WriteName(FWasmSymbolTable,objsym.Name);
                  end;
              end
            else if IsExternalFunction(objsym) then
              begin
                objsym.SymbolIndex:=FWasmSymbolTableEntriesCount;
                Inc(FWasmSymbolTableEntriesCount);
                WriteByte(FWasmSymbolTable,Ord(SYMTAB_FUNCTION));
                if objsym.ExtraData.ImportModule<>'' then
                  begin
                    WriteUleb(FWasmSymbolTable,WASM_SYM_UNDEFINED or WASM_SYM_EXPLICIT_NAME);
                    if (objsym.FuncIndex<0) then
                      message1(asmw_e_illegal_unset_index,objsym.name)
                    else
                      WriteUleb(FWasmSymbolTable,objsym.FuncIndex);
                    WriteName(FWasmSymbolTable,objsym.Name);
                  end
                else
                  begin
                    WriteUleb(FWasmSymbolTable,WASM_SYM_UNDEFINED);
                    if (objsym.FuncIndex<0) then
                      message1(asmw_e_illegal_unset_index,objsym.name)
                    else
                      WriteUleb(FWasmSymbolTable,objsym.FuncIndex);
                  end;
              end
            else if objsym.typ=AT_FUNCTION then
              begin
                objsym.SymbolIndex:=FWasmSymbolTableEntriesCount;
                Inc(FWasmSymbolTableEntriesCount);
                WriteByte(FWasmSymbolTable,Ord(SYMTAB_FUNCTION));
                if objsym.IsAlias then
                  begin
                    ObjSymAlias:=TWasmObjSymbol(Data.ObjSymbolList.Find(objsym.AliasOf));
                    ObjSym.FuncIndex:=ObjSymAlias.FuncIndex;
                    WriteUleb(FWasmSymbolTable,WASM_SYM_EXPLICIT_NAME or WASM_SYM_NO_STRIP);
                    WriteUleb(FWasmSymbolTable,ObjSymAlias.FuncIndex);
                  end
                else
                  begin
                    if IsExportedFunction(objsym) then
                      WriteUleb(FWasmSymbolTable,WASM_SYM_EXPORTED)
                    else
                      WriteUleb(FWasmSymbolTable,0);
                    if (objsym.FuncIndex<0) then
                      message1(asmw_e_illegal_unset_index,objsym.name)
                    else
                      WriteUleb(FWasmSymbolTable,objsym.FuncIndex);
                  end;
                WriteName(FWasmSymbolTable,objsym.Name);
              end
            else if (objsym.typ in [AT_DATA,AT_TLS,AT_METADATA]) or ((objsym.typ=AT_NONE) and (objsym.bind=AB_EXTERNAL)) then
              begin
                if (objsym.bind<>AB_EXTERNAL) and TWasmObjSection(objsym.objsection).IsDebug then
                  begin
                    objsym.SymbolIndex:=FWasmSymbolTableEntriesCount;
                    Inc(FWasmSymbolTableEntriesCount);
                    WriteByte(FWasmSymbolTable,Ord(SYMTAB_FPC_CUSTOM));
                    if objsym.bind=AB_GLOBAL then
                      SymbolFlags:=0
                    else if objsym.bind=AB_LOCAL then
                      SymbolFlags:=WASM_SYM_BINDING_LOCAL
                    else if objsym.bind=AB_EXTERNAL then
                      SymbolFlags:=WASM_SYM_UNDEFINED
                    else
                      internalerror(2024090701);
                    WriteUleb(FWasmSymbolTable,SymbolFlags);
                    WriteName(FWasmSymbolTable,objsym.Name);
                    WriteUleb(FWasmSymbolTable,TWasmObjSection(objsym.objsection).CustomSectionIdx);
                    WriteUleb(FWasmSymbolTable,objsym.offset);
                    WriteUleb(FWasmSymbolTable,objsym.size);
                  end
                else
                  begin
                    objsym.SymbolIndex:=FWasmSymbolTableEntriesCount;
                    Inc(FWasmSymbolTableEntriesCount);
                    WriteByte(FWasmSymbolTable,Ord(SYMTAB_DATA));
                    if objsym.bind=AB_GLOBAL then
                      SymbolFlags:=0
                    else if objsym.bind=AB_LOCAL then
                      SymbolFlags:=WASM_SYM_BINDING_LOCAL
                    else if objsym.bind=AB_EXTERNAL then
                      SymbolFlags:=WASM_SYM_UNDEFINED
                    else
                      internalerror(2021092506);
                    if (objsym.typ=AT_TLS) and (ts_wasm_threads in current_settings.targetswitches) then
                      SymbolFlags:=(SymbolFlags and not WASM_SYM_BINDING_LOCAL) or WASM_SYM_TLS;
                    WriteUleb(FWasmSymbolTable,SymbolFlags);
                    WriteName(FWasmSymbolTable,objsym.Name);
                    if objsym.bind<>AB_EXTERNAL then
                      begin
                        WriteUleb(FWasmSymbolTable,TWasmObjSection(objsym.objsection).SegIdx);
                        WriteUleb(FWasmSymbolTable,objsym.offset);
                        WriteUleb(FWasmSymbolTable,objsym.size);
                      end;
                  end;
              end;
          end;

        DoRelocations;

        if segment_count>0 then
          begin
            WriteUleb(FWasmSections[wsiData],segment_count);
            WriteUleb(FWasmSections[wsiDataCount],segment_count);
            WriteUleb(FWasmLinkingSubsections[WASM_SEGMENT_INFO],segment_count);
            for i:=0 to Data.ObjSectionList.Count-1 do
              begin
                objsec:=TWasmObjSection(Data.ObjSectionList[i]);
                if objsec.IsData then
                  begin
                    WriteName(FWasmLinkingSubsections[WASM_SEGMENT_INFO],objsec.Name);
                    WriteUleb(FWasmLinkingSubsections[WASM_SEGMENT_INFO],BsrQWord(objsec.SecAlign));
                    SegmentFlags:=0;
                    if (ts_wasm_threads in current_settings.targetswitches) and
                       (oso_threadvar in objsec.SecOptions) then
                      SegmentFlags:=SegmentFlags or WASM_SEG_FLAG_TLS;
                    WriteUleb(FWasmLinkingSubsections[WASM_SEGMENT_INFO],SegmentFlags);  { flags }

                    WriteByte(FWasmSections[wsiData],0);
                    WriteByte(FWasmSections[wsiData],$41);
                    WriteSleb(FWasmSections[wsiData],objsec.SegOfs);
                    WriteByte(FWasmSections[wsiData],$0b);
                    WriteUleb(FWasmSections[wsiData],objsec.Size);
                    objsec.FileSectionOfs:=FWasmSections[wsiData].size;
                    if oso_Data in objsec.SecOptions then
                      begin
                        objsec.Data.seek(0);
                        CopyDynamicArray(objsec.Data,FWasmSections[wsiData],objsec.Size);
                      end
                    else
                      begin
                        WriteZeros(FWasmSections[wsiData],objsec.Size);
                      end;
                  end;
              end;
          end;

        WriteUleb(FWasmSections[wsiCode],functions_count);
        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TWasmObjSymbol(Data.ObjSymbolList[i]);
            if (objsym.typ=AT_FUNCTION) and not objsym.IsAlias then
              WriteFunctionCode(FWasmSections[wsiCode],objsym);
          end;

        if segment_count>0 then
          WriteWasmSection(wsiDataCount);
        WriteWasmSection(wsiCode);
        if segment_count>0 then
          WriteWasmSection(wsiData);

        MaybeWriteDebugSection(wcstDebugAbbrev);
        MaybeWriteDebugSection(wcstDebugInfo);
        MaybeWriteDebugSection(wcstDebugStr);
        MaybeWriteDebugSection(wcstDebugLine);
        MaybeWriteDebugSection(wcstDebugFrame);
        MaybeWriteDebugSection(wcstDebugAranges);
        MaybeWriteDebugSection(wcstDebugRanges);

        WriteRelocations;

        WriteSymbolTable;
        WriteLinkingSubsection(WASM_SYMBOL_TABLE);
        if segment_count>0 then
          WriteLinkingSubsection(WASM_SEGMENT_INFO);

        WriteRelocationCodeTable(code_section_nr);
        if segment_count>0 then
          WriteRelocationDataTable(data_section_nr);

        WriteWasmCustomSection(wcstLinking);
        Inc(section_nr);
        WriteWasmCustomSection(wcstRelocCode);
        Inc(section_nr);
        if segment_count>0 then
          begin
            WriteWasmCustomSection(wcstRelocData);
            Inc(section_nr);
          end;
        MaybeWriteRelocationDebugTable(wcstRelocDebugAbbrev,debug_abbrev_section_nr,FWasmRelocationDebugAbbrevTableEntriesCount,FWasmRelocationDebugAbbrevTable);
        MaybeWriteRelocationDebugTable(wcstRelocDebugInfo,debug_info_section_nr,FWasmRelocationDebugInfoTableEntriesCount,FWasmRelocationDebugInfoTable);
        MaybeWriteRelocationDebugTable(wcstRelocDebugStr,debug_str_section_nr,FWasmRelocationDebugStrTableEntriesCount,FWasmRelocationDebugStrTable);
        MaybeWriteRelocationDebugTable(wcstRelocDebugLine,debug_line_section_nr,FWasmRelocationDebugLineTableEntriesCount,FWasmRelocationDebugLineTable);
        MaybeWriteRelocationDebugTable(wcstRelocDebugFrame,debug_frame_section_nr,FWasmRelocationDebugFrameTableEntriesCount,FWasmRelocationDebugFrameTable);
        MaybeWriteRelocationDebugTable(wcstRelocDebugAranges,debug_aranges_section_nr,FWasmRelocationDebugArangesTableEntriesCount,FWasmRelocationDebugArangesTable);
        MaybeWriteRelocationDebugTable(wcstRelocDebugRanges,debug_ranges_section_nr,FWasmRelocationDebugRangesTableEntriesCount,FWasmRelocationDebugRangesTable);
        WriteWasmCustomSection(wcstProducers);
        Inc(section_nr);
        WriteWasmCustomSection(wcstTargetFeatures);
        Inc(section_nr);

        result:=true;
      end;

    constructor TWasmObjOutput.create(AWriter: TObjectWriter);
      var
        i: TWasmSectionID;
        j: TWasmCustomSectionType;
        k: TWasmLinkingSubsectionType;
      begin
        inherited;
        cobjdata:=TWasmObjData;
        for i in TWasmSectionID do
          FWasmSections[i] := tdynamicarray.create(SectionDataMaxGrow);
        for j in TWasmCustomSectionType do
          FWasmCustomSections[j] := tdynamicarray.create(SectionDataMaxGrow);
        for k:=low(TWasmLinkingSubsectionType) to high(TWasmLinkingSubsectionType) do
          FWasmLinkingSubsections[k] := tdynamicarray.create(SectionDataMaxGrow);
        FWasmSymbolTable:=tdynamicarray.create(SectionDataMaxGrow);
        FWasmSymbolTableEntriesCount:=0;
        FWasmRelocationCodeTable:=tdynamicarray.create(SectionDataMaxGrow);
        FWasmRelocationCodeTableEntriesCount:=0;
        FWasmRelocationDataTable:=tdynamicarray.create(SectionDataMaxGrow);
        FWasmRelocationDataTableEntriesCount:=0;
        FWasmRelocationDebugFrameTable:=tdynamicarray.create(SectionDataMaxGrow);
        FWasmRelocationDebugFrameTableEntriesCount:=0;
        FWasmRelocationDebugInfoTable:=tdynamicarray.create(SectionDataMaxGrow);
        FWasmRelocationDebugInfoTableEntriesCount:=0;
        FWasmRelocationDebugLineTable:=tdynamicarray.create(SectionDataMaxGrow);
        FWasmRelocationDebugLineTableEntriesCount:=0;
        FWasmRelocationDebugAbbrevTable:=tdynamicarray.create(SectionDataMaxGrow);
        FWasmRelocationDebugAbbrevTableEntriesCount:=0;
        FWasmRelocationDebugArangesTable:=tdynamicarray.create(SectionDataMaxGrow);
        FWasmRelocationDebugArangesTableEntriesCount:=0;
        FWasmRelocationDebugRangesTable:=tdynamicarray.create(SectionDataMaxGrow);
        FWasmRelocationDebugRangesTableEntriesCount:=0;
        FWasmRelocationDebugStrTable:=tdynamicarray.create(SectionDataMaxGrow);
        FWasmRelocationDebugStrTableEntriesCount:=0;
      end;

    destructor TWasmObjOutput.destroy;
      var
        i: TWasmSectionID;
        j: TWasmCustomSectionType;
        k: TWasmLinkingSubsectionType;
      begin
        for i in TWasmSectionID do
          FWasmSections[i].Free;
        for j in TWasmCustomSectionType do
          FWasmCustomSections[j].Free;
        for k:=low(TWasmLinkingSubsectionType) to high(TWasmLinkingSubsectionType) do
          FWasmLinkingSubsections[k].Free;
        FWasmSymbolTable.Free;
        FWasmRelocationCodeTable.Free;
        FWasmRelocationDataTable.Free;
        FWasmRelocationDebugFrameTable.Free;
        FWasmRelocationDebugInfoTable.Free;
        FWasmRelocationDebugLineTable.Free;
        FWasmRelocationDebugAbbrevTable.Free;
        FWasmRelocationDebugArangesTable.Free;
        FWasmRelocationDebugRangesTable.Free;
        FWasmRelocationDebugStrTable.Free;
        inherited destroy;
      end;

{****************************************************************************
                               TWasmObjInput
****************************************************************************}

    constructor TWasmObjInput.create;
      begin
        inherited create;
        cobjdata:=TWasmObjData;
      end;

    destructor TWasmObjInput.Destroy;
      var
        i: Integer;
      begin
        for i:=low(FFuncTypes) to high(FFuncTypes) do
          begin
            FFuncTypes[i].free;
            FFuncTypes[i]:=nil;
          end;
        inherited Destroy;
      end;

    class function TWasmObjInput.CanReadObjData(AReader: TObjectreader): boolean;
      var
        ModuleMagic: array [0..3] of Byte;
        ModuleVersion: array [0..3] of Byte;
        i: Integer;
      begin
        result:=false;
        if not AReader.read(ModuleMagic,4) then
          exit;
        for i:=0 to 3 do
          if ModuleMagic[i]<>WasmModuleMagic[i] then
            exit;
        if not AReader.read(ModuleVersion,4) then
          exit;
        for i:=0 to 3 do
          if ModuleVersion[i]<>WasmVersion[i] then
            exit;
        result:=true;
      end;

    function TWasmObjInput.ReadObjData(AReader: TObjectreader; out ObjData: TObjData): boolean;

      type
        TLimits = record
          Min, Max: uint32;
          HasMax: Boolean;
        end;

      var
        SectionIndex: Integer = -1;
        SectionId: Byte;
        SectionSize: uint32;
        SectionStart: LongInt;
        CheckSectionBounds: Boolean;

        TypeSectionRead: Boolean = false;
        ImportSectionRead: Boolean = false;
        FunctionSectionRead: Boolean = false;
        GlobalSectionRead: Boolean = false;
        ExportSectionRead: Boolean = false;
        ElementSectionRead: Boolean = false;
        TagSectionRead: Boolean = false;
        CodeSectionRead: Boolean = false;
        DataSectionRead: Boolean = false;
        DataCountSectionRead: Boolean = false;

        SegmentInfoSectionRead: Boolean = false;
        SymbolTableSectionRead: Boolean = false;

        CodeSectionIndex: Integer = -1;
        DataSectionIndex: Integer = -1;
        DebugSectionIndex: array [TWasmCustomDebugSectionType] of Integer = (-1,-1,-1,-1,-1,-1,-1);

        FuncTypes: array of record
          IsImport: Boolean;
          ImportName: ansistring;
          ImportModName: ansistring;
          typidx: uint32;
          IsExported: Boolean;
          ExportName: ansistring;
        end;
        FuncTypeImportsCount: uint32;

        TableTypes: array of record
          IsImport: Boolean;
          ImportName: ansistring;
          ImportModName: ansistring;
          reftype: TWasmBAsicType;
          limits: TLimits;
          IsExported: Boolean;
          ExportName: ansistring;
        end;
        TableTypeImportsCount: uint32;

        MemTypes: array of record
          IsImport: Boolean;
          ImportName: ansistring;
          ImportModName: ansistring;
          limits: TLimits;
          IsExported: Boolean;
          ExportName: ansistring;
        end;
        MemTypeImportsCount: uint32;

        GlobalTypes: array of record
          IsImport: Boolean;
          ImportName: ansistring;
          ImportModName: ansistring;
          valtype: TWasmBasicType;
          IsMutable: Boolean;
          IsExported: Boolean;
          ExportName: ansistring;
          GlobalInit: TGlobalInitializer;
        end;
        GlobalTypeImportsCount: uint32;

        TagTypes: array of record
          IsImport: Boolean;
          ImportName: ansistring;
          ImportModName: ansistring;
          TagAttr: Byte;
          TagTypeIdx: uint32;
          IsExported: Boolean;
          ExportName: ansistring;
        end;
        TagTypeImportsCount: uint32;

        CodeSegments: array of record
          CodeSectionOffset: uint32;
          CodeSize: uint32;
          DataPos: LongInt;
          SegName: ansistring;
          SegIsExported: Boolean;
        end;

        DataSegments: array of record
          DataSectionOffset: uint32;
          Active: Boolean;
          MemIdx: uint32;
          Len: uint32;
          Offset: int32;
          DataPos: LongInt;
          SegName: ansistring;
          SegAlignment: uint32;
          SegFlags: uint32;
        end;

        SymbolTable: array of record
          SymFlags: uint32;
          TargetSection: uint32;
          SymIndex: uint32;
          SymOffset: uint32;
          SymSize: uint32;
          SymCustomSectionIndex: uint32;
          SymCustomSectionType: TWasmCustomDebugSectionType;
          SymKind: TWasmSymbolType;
          SymName: ansistring;
          ObjSym: TWasmObjSymbol;
          ObjSec: TWasmObjSection;
        end;

        { meaning of first index: }
        {   table 0 is code relocs }
        {   table 1 is data relocs }
        {   tables 2.. are custom section relocs for debug sections }
        RelocationTable: array of array of record
          RelocType: TWasmRelocationType;
          RelocOffset: uint32;
          RelocIndex: uint32;
          RelocAddend: int32;
        end;

      function FindDebugSectionByIndex(SectionIndex: Integer; out res: TWasmCustomDebugSectionType): Boolean;
        var
          ds: TWasmCustomDebugSectionType;
        begin
          for ds in TWasmCustomDebugSectionType do
            if DebugSectionIndex[ds]=SectionIndex then
              begin
                Res:=ds;
                Result:=True;
                exit;
              end;
          Res:=low(TWasmCustomDebugSectionType);
          Result:=False;
        end;

      function ReadSection: Boolean;

        function read(out b;len:longint):boolean;
          begin
            result:=false;
            if not CheckSectionBounds or ((AReader.Pos+len)<=(SectionStart+SectionSize)) then
              result:=AReader.read(b,len)
            else
              begin
                { trying to read beyond the end of the section }
                AReader.read(b,SectionStart+SectionSize-AReader.Pos);
                result:=false;
              end;
          end;

        function ReadUleb(out v: uint64): boolean;
          var
            b: byte;
            shift:integer;
          begin
            result:=false;
            b:=0;
            v:=0;
            shift:=0;
            repeat
              if not read(b,1) then
                exit;
              v:=v or (uint64(b and 127) shl shift);
              inc(shift,7);
            until (b and 128)=0;
            result:=true;
          end;

        function ReadUleb32(out v: uint32): boolean;
          var
            vv: uint64;
          begin
            result:=false;
            v:=default(uint32);
            if not ReadUleb(vv) then
              exit;
            if vv>high(uint32) then
              exit;
            v:=vv;
            result:=true;
          end;

        function ReadSleb(out v: int64): boolean;
          var
            b: byte;
            shift:integer;
          begin
            result:=false;
            b:=0;
            v:=0;
            shift:=0;
            repeat
              if not read(b,1) then
                exit;
              v:=v or (uint64(b and 127) shl shift);
              inc(shift,7);
            until (b and 128)=0;
{$ifopt Q+}
{$define overflowon}
{$Q-}
{$endif}
{$ifopt R+}
{$define rangeon}
{$R-}
{$endif}
            if (b and 64)<>0 then
              v:=v or (high(uint64) shl shift);
            result:=true;
          end;
{$ifdef overflowon}
{$Q+}
{$undef overflowon}
{$endif}
{$ifdef rangeon}
{$R+}
{$undef rangeon}
{$endif}

        function ReadSleb32(out v: int32): boolean;
          var
            vv: int64;
          begin
            result:=false;
            v:=default(int32);
            if not ReadSleb(vv) then
              exit;
            if (vv>high(int32)) or (vv<low(int32)) then
              exit;
            v:=vv;
            result:=true;
          end;

        function ReadName(out v: ansistring): boolean;
          var
            len: uint32;
          begin
            result:=false;
            if not ReadUleb32(len) then
              exit;
            SetLength(v,len);
            if len>0 then
              result:=read(v[1],len)
            else
              result:=true;
          end;

        function ReadCustomSection: Boolean;

          function ReadRelocationSection: Boolean;
            var
              TargetSection, RelocCount: uint32;
              i: Integer;
              RelocTableIndex: Integer;
              ds: TWasmCustomDebugSectionType;
            begin
              Result:=False;
              if not ReadUleb32(TargetSection) then
                begin
                  InputError('Error reading the index of the target section of a relocation section');
                  exit;
                end;
              if TargetSection=CodeSectionIndex then
                RelocTableIndex:=0
              else if TargetSection=DataSectionIndex then
                RelocTableIndex:=1
              else
                begin
                  RelocTableIndex:=-1;
                  for ds:=Low(DebugSectionIndex) to High(DebugSectionIndex) do
                    if DebugSectionIndex[ds]=TargetSection then
                      begin
                        RelocTableIndex:=2+(Ord(ds)-Ord(Low(TWasmCustomDebugSectionType)));
                        break;
                      end;
                  if RelocTableIndex=-1 then
                    begin
                      InputError('Relocation found for a custom section, that is not supported');
                      exit;
                    end;
                end;
              if not ReadUleb32(RelocCount) then
                begin
                  InputError('Error reading the relocation entries count from a relocation section');
                  exit;
                end;
              SetLength(RelocationTable[RelocTableIndex],RelocCount);
              for i:=0 to RelocCount-1 do
                with RelocationTable[RelocTableIndex,i] do
                  begin
                    if not Read(RelocType,1) then
                      begin
                        InputError('Error reading the relocation type of a relocation entry');
                        exit;
                      end;
                    if not (RelocType in [R_WASM_FUNCTION_INDEX_LEB,
                                          R_WASM_MEMORY_ADDR_LEB,
                                          R_WASM_TABLE_INDEX_SLEB,
                                          R_WASM_MEMORY_ADDR_SLEB,
                                          R_WASM_SECTION_OFFSET_I32,
                                          R_WASM_TABLE_INDEX_I32,
                                          R_WASM_FUNCTION_OFFSET_I32,
                                          R_WASM_MEMORY_ADDR_I32,
                                          R_WASM_TYPE_INDEX_LEB,
                                          R_WASM_GLOBAL_INDEX_LEB,
                                          R_WASM_TAG_INDEX_LEB,
                                          R_WASM_GLOBAL_INDEX_I32]) then
                      begin
                        InputError('Unsupported relocation type: ' + tostr(Ord(RelocType)));
                        exit;
                      end;
                    if not ReadUleb32(RelocOffset) then
                      begin
                        InputError('Error reading the relocation offset of a relocation entry');
                        exit;
                      end;
                    if not ReadUleb32(RelocIndex) then
                      begin
                        InputError('Error reading the relocation index of a relocation entry');
                        exit;
                      end;
                    if RelocType in [R_WASM_FUNCTION_OFFSET_I32,R_WASM_SECTION_OFFSET_I32,R_WASM_MEMORY_ADDR_LEB,R_WASM_MEMORY_ADDR_SLEB,R_WASM_MEMORY_ADDR_I32] then
                      begin
                        if not ReadSleb32(RelocAddend) then
                          begin
                            InputError('Error reading the relocation addend of a relocation entry');
                            exit;
                          end;
                      end;
                    if (RelocType in [
                          R_WASM_SECTION_OFFSET_I32,
                          R_WASM_FUNCTION_INDEX_LEB,
                          R_WASM_TABLE_INDEX_SLEB,
                          R_WASM_TABLE_INDEX_I32,
                          R_WASM_MEMORY_ADDR_LEB,
                          R_WASM_MEMORY_ADDR_SLEB,
                          R_WASM_MEMORY_ADDR_I32,
                          R_WASM_FUNCTION_OFFSET_I32,
                          R_WASM_GLOBAL_INDEX_LEB,
                          R_WASM_GLOBAL_INDEX_I32]) and (RelocIndex>High(SymbolTable)) then
                      begin
                        InputError('Relocation index outside the bounds of the symbol table');
                        exit;
                      end;
                    if (RelocType=R_WASM_TYPE_INDEX_LEB) and (RelocIndex>High(FFuncTypes)) then
                      begin
                        InputError('Relocation index of R_WASM_TYPE_INDEX_LEB outside the bounds of the func types, defined in the func section of the module');
                        exit;
                      end;
                    if (RelocType=R_WASM_SECTION_OFFSET_I32) and (SymbolTable[RelocIndex].SymKind<>SYMTAB_SECTION) then
                      begin
                        InputError('R_WASM_SECTION_OFFSET_I32 must point to a SYMTAB_SECTION symbol');
                        exit;
                      end;
                    if (RelocType in [R_WASM_GLOBAL_INDEX_LEB,R_WASM_GLOBAL_INDEX_I32]) and
                       not ((SymbolTable[RelocIndex].SymKind=SYMTAB_GLOBAL) or
                            ((ts_wasm_threads in current_settings.targetswitches) and
                             (SymbolTable[RelocIndex].SymKind=SYMTAB_DATA) and
                             ((SymbolTable[RelocIndex].SymFlags and WASM_SYM_TLS)<>0))) then
                      begin
                        if ts_wasm_threads in current_settings.targetswitches then
                          InputError('Relocation must point to a SYMTAB_GLOBAL symbol or a SYMTAB_DATA symbol with the WASM_SYM_TLS flag set')
                        else
                          InputError('Relocation must point to a SYMTAB_GLOBAL symbol');
                        exit;
                      end;
                    if (RelocType=R_WASM_TAG_INDEX_LEB) and (SymbolTable[RelocIndex].SymKind<>SYMTAB_EVENT) then
                      begin
                        InputError('Relocation must point to a SYMTAB_EVENT symbol');
                        exit;
                      end;
                    if (RelocType in [
                          R_WASM_FUNCTION_INDEX_LEB,
                          R_WASM_TABLE_INDEX_SLEB,
                          R_WASM_TABLE_INDEX_I32,
                          R_WASM_FUNCTION_OFFSET_I32]) and (SymbolTable[RelocIndex].SymKind<>SYMTAB_FUNCTION) then
                      begin
                        InputError('Relocation must point to a SYMTAB_FUNCTION symbol');
                        exit;
                      end;
                    if (RelocType in [
                          R_WASM_MEMORY_ADDR_LEB,
                          R_WASM_MEMORY_ADDR_SLEB,
                          R_WASM_MEMORY_ADDR_I32]) and (SymbolTable[RelocIndex].SymKind<>SYMTAB_DATA) then
                      begin
                        InputError('Relocation must point to a SYMTAB_DATA symbol');
                        exit;
                      end;
                  end;
              if AReader.Pos<>(SectionStart+SectionSize) then
                begin
                  InputError('Unexpected relocation section size');
                  exit;
                end;
              Result:=True;
            end;

          function ReadLinkingSection: Boolean;

            function ReadSegmentInfo: Boolean;
              var
                SegmentCount: uint32;
                i: Integer;
              begin
                Result:=False;
                if SegmentInfoSectionRead then
                  begin
                    InputError('The WASM_SEGMENT_INFO subsection is duplicated');
                    exit;
                  end;
                SegmentInfoSectionRead:=True;
                if not ReadUleb32(SegmentCount) then
                  begin
                    InputError('Error reading the segment count from the WASM_SEGMENT_INFO subsection of the ''linking'' section');
                    exit;
                  end;
                if SegmentCount<>Length(DataSegments) then
                  begin
                    InputError('Segment count in the WASM_SEGMENT_INFO subsection does not match the data count in the data section');
                    exit;
                  end;
                for i:=0 to SegmentCount-1 do
                  with DataSegments[i] do
                    begin
                      if not ReadName(SegName) then
                        begin
                          InputError('Error reading segment name from the WASM_SEGMENT_INFO subsection of the ''linking'' section');
                          exit;
                        end;
                      if not ReadUleb32(SegAlignment) then
                        begin
                          InputError('Error reading segment alignment from the WASM_SEGMENT_INFO subsection of the ''linking'' section');
                          exit;
                        end;
                      if not ReadUleb32(SegFlags) then
                        begin
                          InputError('Error reading segment flags from the WASM_SEGMENT_INFO subsection of the ''linking'' section');
                          exit;
                        end;
                    end;
                if AReader.Pos<>(SectionStart+SectionSize) then
                  begin
                    InputError('Unexpected WASM_SEGMENT_INFO section size');
                    exit;
                  end;
                Result:=True;
              end;

            function ReadSymbolTable: Boolean;
              var
                SymCount: uint32;
                i: Integer;
                SymKindName: string;
                SymKindB: Byte;
              begin
                Result:=False;
                if SymbolTableSectionRead then
                  begin
                    InputError('The WASM_SYMBOL_TABLE subsection is duplicated');
                    exit;
                  end;
                SymbolTableSectionRead:=True;
                if not ReadUleb32(SymCount) then
                  begin
                    InputError('Error reading the symbol count from the WASM_SYMBOL_TABLE subsection of the ''linking'' section');
                    exit;
                  end;
                SetLength(SymbolTable,SymCount);
                for i:=0 to SymCount-1 do
                  with SymbolTable[i] do
                    begin
                      if not Read(SymKindB,1) then
                        begin
                          InputError('Error reading symbol type from the WASM_SYMBOL_TABLE subsection of the ''linking'' section');
                          exit;
                        end;
                      if SymKindB>Ord(High(TWasmSymbolType)) then
                        begin
                          InputError('Unsupported symbol type from the WASM_SYMBOL_TABLE subsection of the ''linking'' section');
                          exit;
                        end;
                      SymKind:=TWasmSymbolType(SymKindB);
                      if not ReadUleb32(SymFlags) then
                        begin
                          InputError('Error reading symbol flags from the WASM_SYMBOL_TABLE subsection of the ''linking'' section');
                          exit;
                        end;
                      case SymKind of
                        SYMTAB_FUNCTION,
                        SYMTAB_GLOBAL,
                        SYMTAB_EVENT,
                        SYMTAB_TABLE:
                          begin
                            WriteStr(SymKindName, SymKind);
                            if not ReadUleb32(SymIndex) then
                              begin
                                InputError('Error reading the index of a ' + SymKindName + ' symbol');
                                exit;
                              end;
                            if ((SymKind=SYMTAB_FUNCTION) and (SymIndex>high(FuncTypes))) or
                               ((SymKind=SYMTAB_EVENT) and (SymIndex>high(TagTypes))) then
                              begin
                                InputError('Symbol index too high');
                                exit;
                              end;
                            if ((SymFlags and WASM_SYM_EXPLICIT_NAME)<>0) or
                               ((SymFlags and WASM_SYM_UNDEFINED)=0) then
                              begin
                                if not ReadName(SymName) then
                                  begin
                                    InputError('Error reading symbol name of a ' + SymKindName + ' symbol');
                                    exit;
                                  end;
                              end;
                          end;
                        SYMTAB_DATA:
                          begin
                            if not ReadName(SymName) then
                              begin
                                InputError('Error reading symbol name of a SYMTAB_DATA symbol');
                                exit;
                              end;
                            if (SymFlags and WASM_SYM_UNDEFINED)=0 then
                              begin
                                if not ReadUleb32(SymIndex) then
                                  begin
                                    InputError('Error reading the data segment index of a SYMTAB_DATA symbol');
                                    exit;
                                  end;
                                if SymIndex>high(DataSegments) then
                                  begin
                                    InputError('Data segment index of SYMTAB_DATA symbol out of bounds');
                                    exit;
                                  end;
                                if not ReadUleb32(SymOffset) then
                                  begin
                                    InputError('Error reading the offset of a SYMTAB_DATA symbol');
                                    exit;
                                  end;
                                if not ReadUleb32(SymSize) then
                                  begin
                                    InputError('Error reading the size of a SYMTAB_DATA symbol');
                                    exit;
                                  end;
                              end;
                          end;
                        SYMTAB_FPC_CUSTOM:
                          begin
                            if not ReadName(SymName) then
                              begin
                                InputError('Error reading symbol name of a SYMTAB_FPC_CUSTOM symbol');
                                exit;
                              end;
                            if (SymFlags and WASM_SYM_UNDEFINED)=0 then
                              begin
                                if not ReadUleb32(SymCustomSectionIndex) then
                                  begin
                                    InputError('Error reading the custom section index of a SYMTAB_FPC_CUSTOM symbol');
                                    exit;
                                  end;
                                if not FindDebugSectionByIndex(SymCustomSectionIndex,SymCustomSectionType) then
                                  begin
                                    InputError('Custom section index of SYMTAB_FPC_CUSTOM symbol not pointing to a debug section');
                                    exit;
                                  end;
                                if not ReadUleb32(SymOffset) then
                                  begin
                                    InputError('Error reading the offset of a SYMTAB_FPC_CUSTOM symbol');
                                    exit;
                                  end;
                                if not ReadUleb32(SymSize) then
                                  begin
                                    InputError('Error reading the size of a SYMTAB_FPC_CUSTOM symbol');
                                    exit;
                                  end;
                              end;
                          end;
                        SYMTAB_SECTION:
                          begin
                            if not ReadUleb32(TargetSection) then
                              begin
                                InputError('Error reading the target section of a SYMTAB_SECTION symbol');
                                exit;
                              end;
                          end;
                      end;
                    end;
                if AReader.Pos<>(SectionStart+SectionSize) then
                  begin
                    InputError('Unexpected WASM_SYMBOL_TABLE section size');
                    exit;
                  end;
                Result:=True;
              end;

            const
              ExpectedVersion = 2;
            var
              Version, SubsectionSize, SaveSectionSize: uint32;
              SubsectionType: Byte;
              SaveSectionStart: LongInt;
            begin
              Result:=False;
              if not ReadUleb32(Version) then
                begin
                  InputError('Error reading the version of the ''linking'' section');
                  exit;
                end;
              if Version<>ExpectedVersion then
                begin
                  InputError('The ''linking'' section has an unsupported version (expected version ' + tostr(ExpectedVersion) + ', got version ' + tostr(Version) + ')');
                  exit;
                end;
              while AReader.Pos<(SectionStart+SectionSize) do
                begin
                  if not read(SubsectionType, 1) then
                    begin
                      InputError('Error reading subsection type in the ''linking'' section');
                      exit;
                    end;
                  if not ReadUleb32(SubsectionSize) then
                    begin
                      InputError('Error reading subsection size in the ''linking'' section');
                      exit;
                    end;
                  if (AReader.Pos+SubsectionSize)>(SectionStart+SectionSize) then
                    begin
                      InputError('Subsection size exceeds bounds of its parent ''linking'' section');
                      exit;
                    end;
                  SaveSectionStart:=SectionStart;
                  SaveSectionSize:=SectionSize;
                  SectionStart:=AReader.Pos;
                  SectionSize:=SubsectionSize;
                  case SubsectionType of
                    Byte(WASM_SEGMENT_INFO):
                      if not ReadSegmentInfo then
                        begin
                          InputError('Error reading the WASM_SEGMENT_INFO subsection of the ''linking'' section');
                          exit;
                        end;
                    Byte(WASM_SYMBOL_TABLE):
                      if not ReadSymbolTable then
                        begin
                          InputError('Error reading the WASM_SYMBOL_TABLE subsection of the ''linking'' section');
                          exit;
                        end;
                    else
                      begin
                        InputError('Unsupported ''linking'' section subsection type ' + tostr(SubsectionType));
                        exit;
                      end;
                  end;
                  AReader.Seek(SectionStart+SectionSize);
                  SectionStart:=SaveSectionStart;
                  SectionSize:=SaveSectionSize;
                end;
              result:=True;
            end;

          function ReadProducersSection: Boolean;
            begin
              Result:=False;
            end;

          function ReadTargetFeaturesSection: Boolean;
            begin
              Result:=False;
            end;

          function ReadDebugSection(const SectionName: string; SectionType: TWasmCustomDebugSectionType): Boolean;
            var
              ObjSec: TObjSection;
            begin
              Result:=False;
              if DebugSectionIndex[SectionType]<>-1 then
                begin
                  InputError('Duplicated debug section: ' + SectionName);
                  exit;
                end;
              DebugSectionIndex[SectionType]:=SectionIndex;
              ObjSec:=ObjData.createsection(SectionName,1,[oso_Data,oso_debug],false);
              ObjSec.DataPos:=AReader.Pos;
              ObjSec.Size:=SectionStart+SectionSize-AReader.Pos;
              Result:=True;
            end;

          const
            RelocationSectionPrefix = 'reloc.';
          var
            SectionName: ansistring;
          begin
            Result:=False;
            ReadName(SectionName);
            if Copy(SectionName,1,Length(RelocationSectionPrefix)) = RelocationSectionPrefix then
              begin
                if not ReadRelocationSection then
                  begin
                    InputError('Error reading the relocation section ''' + SectionName + '''');
                    exit;
                  end;
              end
            else
              case SectionName of
                'linking':
                  if not ReadLinkingSection then
                    begin
                      InputError('Error reading the ''linking'' section');
                      exit;
                    end;
                'producers':
                  Result:=ReadProducersSection;
                'target_features':
                  Result:=ReadTargetFeaturesSection;
                '.debug_frame':
                  if not ReadDebugSection(SectionName, wcstDebugFrame) then
                    begin
                      InputError('Error reading section ' + SectionName);
                      exit;
                    end;
                '.debug_info':
                  if not ReadDebugSection(SectionName, wcstDebugInfo) then
                    begin
                      InputError('Error reading section ' + SectionName);
                      exit;
                    end;
                '.debug_line':
                  if not ReadDebugSection(SectionName, wcstDebugLine) then
                    begin
                      InputError('Error reading section ' + SectionName);
                      exit;
                    end;
                '.debug_abbrev':
                  if not ReadDebugSection(SectionName, wcstDebugAbbrev) then
                    begin
                      InputError('Error reading section ' + SectionName);
                      exit;
                    end;
                '.debug_aranges':
                  if not ReadDebugSection(SectionName, wcstDebugAranges) then
                    begin
                      InputError('Error reading section ' + SectionName);
                      exit;
                    end;
                '.debug_ranges':
                  if not ReadDebugSection(SectionName, wcstDebugRanges) then
                    begin
                      InputError('Error reading section ' + SectionName);
                      exit;
                    end;
                '.debug_str':
                  if not ReadDebugSection(SectionName, wcstDebugStr) then
                    begin
                      InputError('Error reading section ' + SectionName);
                      exit;
                    end;
                else
                  InputError('Unsupported custom section: ''' + SectionName + '''');
              end;
            Result:=True;
          end;

        function ReadTypeSection: Boolean;
          var
            FuncTypesCount, ParamsCount, ResultsCount: uint32;
            FuncTypeId, WasmTypeId: Byte;
            i, j: Integer;
            wbt: TWasmBasicType;
          begin
            Result:=False;
            if TypeSectionRead then
              begin
                InputError('Type section is duplicated');
                exit;
              end;
            TypeSectionRead:=True;
            if not ReadUleb32(FuncTypesCount) then
              begin
                InputError('Error reading the func types count');
                exit;
              end;
            SetLength(FFuncTypes,FuncTypesCount);
            for i:=0 to FuncTypesCount - 1 do
              begin
                FFuncTypes[i]:=TWasmFuncType.Create([],[]);
                if not AReader.read(FuncTypeId,1) then
                  begin
                    InputError('Error reading the function type identifier');
                    exit;
                  end;
                if FuncTypeId<>$60 then
                  begin
                    InputError('Incorrect function type identifier (expected $60, got $' + HexStr(FuncTypeId,2) + ')');
                    exit;
                  end;
                if not ReadUleb32(ParamsCount) then
                  begin
                    InputError('Error reading the function parameters count');
                    exit;
                  end;
                for j:=0 to ParamsCount-1 do
                  begin
                    if not AReader.read(WasmTypeId,1) then
                      begin
                        InputError('Error reading a function parameter basic type');
                        exit;
                      end;
                    if not decode_wasm_basic_type(WasmTypeId,wbt) then
                      begin
                        InputError('Unknown function parameter basic type: $' + HexStr(WasmTypeId,2));
                        exit;
                      end;
                    FFuncTypes[i].add_param(wbt);
                  end;
                if not ReadUleb32(ResultsCount) then
                  begin
                    InputError('Error reading the function results count');
                    exit;
                  end;
                for j:=0 to ResultsCount-1 do
                  begin
                    if not AReader.read(WasmTypeId,1) then
                      begin
                        InputError('Error reading a function result basic type');
                        exit;
                      end;
                    if not decode_wasm_basic_type(WasmTypeId,wbt) then
                      begin
                        InputError('Unknown function result basic type: $' + HexStr(WasmTypeId,2));
                        exit;
                      end;
                    FFuncTypes[i].add_result(wbt);
                  end;
              end;
            if AReader.Pos<>(SectionStart+SectionSize) then
              begin
                InputError('Unexpected type section size');
                exit;
              end;
            Result:=true;
          end;

        function ReadImportSection: Boolean;
          var
            ImportsCount: uint32;
            i: Integer;
            ModName, Name: ansistring;
            ImportType, TableElemTyp, TableLimitsKind, MemoryLimitsKind,
            GlobalType, GlobalMutabilityType: Byte;
          begin
            Result:=False;
            if ImportSectionRead then
              begin
                InputError('Import section is duplicated');
                exit;
              end;
            ImportSectionRead:=True;
            if not ReadUleb32(ImportsCount) then
              begin
                InputError('Error reading the imports count');
                exit;
              end;
            for i:=0 to ImportsCount-1 do
              begin
                if not ReadName(ModName) then
                  begin
                    InputError('Error reading import module name');
                    exit;
                  end;
                if not ReadName(Name) then
                  begin
                    InputError('Error import name');
                    exit;
                  end;
                if not AReader.Read(ImportType,1) then
                  begin
                    InputError('Error reading import type');
                    exit;
                  end;
                case ImportType of
                  $00:  { func }
                    begin
                      Inc(FuncTypeImportsCount);
                      SetLength(FuncTypes,FuncTypeImportsCount);
                      with FuncTypes[FuncTypeImportsCount-1] do
                        begin
                          IsImport:=True;
                          ImportName:=Name;
                          ImportModName:=ModName;
                          if not ReadUleb32(typidx) then
                            begin
                              InputError('Error reading type index for func import');
                              exit;
                            end;
                          if typidx>high(FFuncTypes) then
                            begin
                              InputError('Type index in func import exceeds bounds of the types table');
                              exit;
                            end;
                        end;
                    end;
                  $01:  { table }
                    begin
                      Inc(TableTypeImportsCount);
                      SetLength(TableTypes,TableTypeImportsCount);
                      with TableTypes[TableTypeImportsCount-1] do
                        begin
                          IsImport:=True;
                          ImportName:=Name;
                          ImportModName:=ModName;
                          if not AReader.read(TableElemTyp,1) then
                            begin
                              InputError('Error reading table element type for table import');
                              exit;
                            end;
                          if not decode_wasm_basic_type(TableElemTyp,reftype) then
                            begin
                              InputError('Invalid table element type for table import: $' + HexStr(TableElemTyp,2));
                              exit;
                            end;
                          if not (reftype in WasmReferenceTypes) then
                            begin
                              InputError('Table element type for table import must be a reference type');
                              exit;
                            end;
                          if not AReader.read(TableLimitsKind,1) then
                            begin
                              InputError('Error reading table limits kind for table import');
                              exit;
                            end;
                          case TableLimitsKind of
                            $00:
                              begin
                                limits.HasMax:=False;
                                limits.Max:=high(limits.Max);
                                if not ReadUleb32(limits.min) then
                                  begin
                                    InputError('Error reading table limits min for table import');
                                    exit;
                                  end;
                              end;
                            $01:
                              begin
                                limits.HasMax:=True;
                                if not ReadUleb32(limits.min) then
                                  begin
                                    InputError('Error reading table limits min for table import');
                                    exit;
                                  end;
                                if not ReadUleb32(limits.max) then
                                  begin
                                    InputError('Error reading table limits max for table import');
                                    exit;
                                  end;
                                if limits.min>limits.max then
                                  begin
                                    InputError('Table limits min exceed table limits max in table import');
                                    exit;
                                  end;
                              end;
                            else
                              begin
                                InputError('Unsupported table limits kind for table import: $' + HexStr(TableLimitsKind,2));
                                exit;
                              end;
                          end;
                        end;
                    end;
                  $02:  { mem }
                    begin
                      Inc(MemTypeImportsCount);
                      SetLength(MemTypes,MemTypeImportsCount);
                      with MemTypes[MemTypeImportsCount-1] do
                        begin
                          IsImport:=True;
                          ImportName:=Name;
                          ImportModName:=ModName;
                          if not AReader.read(MemoryLimitsKind,1) then
                            begin
                              InputError('Error reading memory limits kind for memory import');
                              exit;
                            end;
                          case MemoryLimitsKind of
                            $00:
                              begin
                                limits.HasMax:=False;
                                limits.Max:=high(limits.Max);
                                if not ReadUleb32(limits.min) then
                                  begin
                                    InputError('Error reading memory limits min for memory import');
                                    exit;
                                  end;
                              end;
                            $01:
                              begin
                                limits.HasMax:=True;
                                if not ReadUleb32(limits.min) then
                                  begin
                                    InputError('Error reading memory limits min for memory import');
                                    exit;
                                  end;
                                if not ReadUleb32(limits.max) then
                                  begin
                                    InputError('Error reading memory limits max for memory import');
                                    exit;
                                  end;
                                if limits.Min>limits.Max then
                                  begin
                                    InputError('Memory limits min exceed memory limits max in memory import');
                                    exit;
                                  end;
                              end;
                            else
                              begin
                                InputError('Unsupported memory limits kind for memory import: $' + HexStr(MemoryLimitsKind,2));
                                exit;
                              end;
                          end;
                        end;
                    end;
                  $03:  { global }
                    begin
                      Inc(GlobalTypeImportsCount);
                      SetLength(GlobalTypes,GlobalTypeImportsCount);
                      with GlobalTypes[GlobalTypeImportsCount-1] do
                        begin
                          IsImport:=True;
                          ImportName:=Name;
                          ImportModName:=ModName;
                          if not AReader.read(GlobalType,1) then
                            begin
                              InputError('Error reading global type for global import');
                              exit;
                            end;
                          if not decode_wasm_basic_type(GlobalType,valtype) then
                            begin
                              InputError('Unsupported global type for global import: ' + HexStr(GlobalType,2));
                              exit;
                            end;
                          if not AReader.read(GlobalMutabilityType,1) then
                            begin
                              InputError('Error reading global mutability flag for global import');
                              exit;
                            end;
                          case GlobalMutabilityType of
                            $00:
                              IsMutable:=False;
                            $01:
                              IsMutable:=True;
                            else
                              begin
                                InputError('Unknown global mutability flag for global import: $' + HexStr(GlobalMutabilityType,2));
                                exit;
                              end;
                          end;
                        end;
                    end;
                  $04: { tag }
                    begin
                      Inc(TagTypeImportsCount);
                      SetLength(TagTypes,TagTypeImportsCount);
                      with TagTypes[TagTypeImportsCount-1] do
                        begin
                          IsImport:=True;
                          ImportName:=Name;
                          ImportModName:=ModName;
                          if not Read(TagAttr,1) then
                            begin
                              InputError('Error reading import tag attribute');
                              exit;
                            end;
                          if not ReadUleb32(TagTypeIdx) then
                            begin
                              InputError('Error reading import tag type index');
                              exit;
                            end;
                          if TagTypeIdx>high(FFuncTypes) then
                            begin
                              InputError('Type index in tag import exceeds bounds of the types table');
                              exit;
                            end;
                        end;
                    end;
                  else
                    begin
                      InputError('Unknown import type: $' + HexStr(ImportType,2));
                      exit;
                    end;
                end;
              end;
            if AReader.Pos<>(SectionStart+SectionSize) then
              begin
                InputError('Unexpected import section size');
                exit;
              end;
            Result:=true;
          end;

        function ReadFunctionSection: Boolean;
          var
            FunctionsCount: uint32;
            i: Integer;
          begin
            Result:=False;
            if FunctionSectionRead then
              begin
                InputError('Function section is duplicated');
                exit;
              end;
            FunctionSectionRead:=True;
            if not ReadUleb32(FunctionsCount) then
              begin
                InputError('Error reading the functions count');
                exit;
              end;
            SetLength(FuncTypes, FuncTypeImportsCount + FunctionsCount);
            for i:=0 to FunctionsCount-1 do
              with FuncTypes[i + FuncTypeImportsCount] do
                begin
                  IsImport:=False;
                  if not ReadUleb32(typidx) then
                    begin
                      InputError('Error reading type index for function');
                      exit;
                    end;
                  if typidx>high(FFuncTypes) then
                    begin
                      InputError('Type index in the function section exceeds bounds of the types table');
                      exit;
                    end;
                end;
            if AReader.Pos<>(SectionStart+SectionSize) then
              begin
                InputError('Unexpected function section size');
                exit;
              end;
            Result:=true;
          end;

        function ReadGlobalSection: Boolean;

          function ParseExpr(out Init: TGlobalInitializer): Boolean;
            var
              B, B2: Byte;
              tmpU32: UInt32;
              tmpU64: UInt64;
            begin
              Result:=False;
              repeat
                if not Read(B, 1) then
                  exit;
                case B of
                  $0B:  { end }
                    ;
                  $41:  { i32.const }
                    begin
                      Init.typ:=wbt_i32;
                      if not ReadSleb32(Init.init_i32) then
                        exit;
                    end;
                  $42:  { i64.const }
                    begin
                      Init.typ:=wbt_i64;
                      if not ReadSleb(Init.init_i64) then
                        exit;
                    end;
                  $43:  { f32.const }
                    begin
                      Init.typ:=wbt_f32;
                      if not Read(tmpU32, 4) then
                        exit;
{$ifdef FPC_BIG_ENDIAN}
                      tmpU32:=SwapEndian(tmpU32);
{$endif FPC_BIG_ENDIAN}
                      Move(tmpU32,Init.init_f32,4);
                    end;
                  $44:  { f64.const }
                    begin
                      Init.typ:=wbt_f64;
                      if not Read(tmpU64, 8) then
                        exit;
{$ifdef FPC_BIG_ENDIAN}
                      tmpU64:=SwapEndian(tmpU64);
{$endif FPC_BIG_ENDIAN}
                      Move(tmpU64,Init.init_f64,8);
                    end;
                  $D0:  { ref.null }
                    begin
                      if not Read(B2, 1) then
                        exit;
                      if not decode_wasm_basic_type(B2, Init.typ) then
                        exit;
                      if not (Init.typ in WasmReferenceTypes) then
                        exit;
                    end;
                  else
                    begin
                      InputError('Unsupported opcode in global initializer');
                      exit;
                    end;
                end;
              until b = $0B;
              Result:=True;
            end;

          var
            GlobalsCount: uint32;
            i: Integer;
            vt: Byte;
            mut: Byte;
          begin
            Result:=False;
            if GlobalSectionRead then
              begin
                InputError('Global section is duplicated');
                exit;
              end;
            GlobalSectionRead:=True;
            if not ReadUleb32(GlobalsCount) then
              begin
                InputError('Error reading the globals count from the global section');
                exit;
              end;
            SetLength(GlobalTypes,Length(GlobalTypes)+GlobalsCount);
            for i:=0 to GlobalsCount-1 do
              with GlobalTypes[i + GlobalTypeImportsCount] do
                begin
                  if not read(vt,1) then
                    begin
                      InputError('Error reading the type of a global from the global section');
                      exit;
                    end;
                  if not decode_wasm_basic_type(vt,valtype) then
                    begin
                      InputError('Unsupported type of global in the global section');
                      exit;
                    end;
                  if not read(mut,1) then
                    begin
                      InputError('Error reading the mutability flag of a global in the global section');
                      exit;
                    end;
                  case mut of
                    $00:
                      IsMutable:=False;
                    $01:
                      IsMutable:=True;
                    else
                      begin
                        InputError('Unsupported value (' + tostr(mut) + ') for the mutability flag of a global in the global section');
                        exit;
                      end;
                  end;
                  if not ParseExpr(GlobalInit) then
                    begin
                      InputError('Error parsing the global initializer expression in the global section');
                      exit;
                    end;
                  if GlobalInit.typ<>valtype then
                    begin
                      InputError('Initializer expression for global produces a type, which does not match the type of the global');
                      exit;
                    end;
                end;
            if AReader.Pos<>(SectionStart+SectionSize) then
              begin
                InputError('Unexpected global section size');
                exit;
              end;
            Result:=True;
          end;

        function ReadExportSection: Boolean;
          var
            ExportsCount, FuncIdx, TableIdx, MemIdx, GlobalIdx, TagIdx: uint32;
            i: Integer;
            Name: ansistring;
            ExportType: Byte;
          begin
            Result:=False;
            if ExportSectionRead then
              begin
                InputError('Export section is duplicated');
                exit;
              end;
            ExportSectionRead:=True;
            if not ReadUleb32(ExportsCount) then
              begin
                InputError('Error reading the exports count from the export section');
                exit;
              end;
            for i:=0 to ExportsCount-1 do
              begin
                if not ReadName(Name) then
                  begin
                    InputError('Error reading an export name from the export section');
                    exit;
                  end;
                if not Read(ExportType,1) then
                  begin
                    InputError('Error reading an export type from the export section');
                    exit;
                  end;
                case ExportType of
                  $00:  { func }
                    begin
                      if not ReadUleb32(FuncIdx) then
                        begin
                          InputError('Error reading a func index from the export section');
                          exit;
                        end;
                      if FuncIdx>high(FuncTypes) then
                        begin
                          InputError('Func index too high in the export section');
                          exit;
                        end;
                      with FuncTypes[FuncIdx] do
                        begin
                          IsExported:=True;
                          ExportName:=Name;
                        end;
                    end;
                  $01:  { table }
                    begin
                      if not ReadUleb32(TableIdx) then
                        begin
                          InputError('Error reading a table index from the export section');
                          exit;
                        end;
                      if TableIdx>high(TableTypes) then
                        begin
                          InputError('Table index too high in the export section');
                          exit;
                        end;
                      with TableTypes[TableIdx] do
                        begin
                          IsExported:=True;
                          ExportName:=Name;
                        end;
                    end;
                  $02:  { mem }
                    begin
                      if not ReadUleb32(MemIdx) then
                        begin
                          InputError('Error reading a mem index from the export section');
                          exit;
                        end;
                      if MemIdx>high(MemTypes) then
                        begin
                          InputError('Mem index too high in the export section');
                          exit;
                        end;
                      with MemTypes[MemIdx] do
                        begin
                          IsExported:=True;
                          ExportName:=Name;
                        end;
                    end;
                  $03:  { global }
                    begin
                      if not ReadUleb32(GlobalIdx) then
                        begin
                          InputError('Error reading a global index from the export section');
                          exit;
                        end;
                      if GlobalIdx>high(GlobalTypes) then
                        begin
                          InputError('Global index too high in the export section');
                          exit;
                        end;
                      with GlobalTypes[GlobalIdx] do
                        begin
                          IsExported:=True;
                          ExportName:=Name;
                        end;
                    end;
                  $04:  { tag }
                    begin
                      if not ReadUleb32(TagIdx) then
                        begin
                          InputError('Error reading a tag index from the export section');
                          exit;
                        end;
                      if TagIdx>high(TagTypes) then
                        begin
                          InputError('Tag index too high in the export section');
                          exit;
                        end;
                      with TagTypes[TagIdx] do
                        begin
                          IsExported:=True;
                          ExportName:=Name;
                        end;
                    end;
                  else
                    begin
                      InputError('Unsupported export type in the export section: ' + tostr(ExportType));
                      exit;
                    end;
                end;
              end;
            if AReader.Pos<>(SectionStart+SectionSize) then
              begin
                InputError('Unexpected export section size');
                exit;
              end;
            Result:=True;
          end;

        function ReadElementSection: Boolean;
          begin
            Result:=False;
            if ElementSectionRead then
              begin
                InputError('Element section is duplicated');
                exit;
              end;
            ElementSectionRead:=True;
            { We skip the element section for now }
            { TODO: implement reading it (and linking of tables) }
            Result:=True;
          end;

        function ReadTagSection: Boolean;
          var
            TagCount: uint32;
            i: Integer;
          begin
            Result:=False;
            if TagSectionRead then
              begin
                InputError('Tag section is duplicated');
                exit;
              end;
            TagSectionRead:=True;
            if not ReadUleb32(TagCount) then
              begin
                InputError('Error reading the tag count from the tag section');
                exit;
              end;
            SetLength(TagTypes,Length(TagTypes)+TagCount);
            for i:=0 to TagCount-1 do
              with TagTypes[i + TagTypeImportsCount] do
                begin
                  if not Read(TagAttr,1) then
                    begin
                      InputError('Error reading tag attribute');
                      exit;
                    end;
                  if not ReadUleb32(TagTypeIdx) then
                    begin
                      InputError('Error reading tag type index');
                      exit;
                    end;
                  if TagTypeIdx>high(FFuncTypes) then
                    begin
                      InputError('Type index in tag import exceeds bounds of the types table');
                      exit;
                    end;
                end;
            if AReader.Pos<>(SectionStart+SectionSize) then
              begin
                InputError('Unexpected tag section size');
                exit;
              end;
            Result:=True;
          end;

        function ReadCodeSection: Boolean;
          var
            CodeEntriesCount: uint32;
            i: Integer;
          begin
            Result:=False;
            if CodeSectionRead then
              begin
                InputError('Code section is duplicated');
                exit;
              end;
            CodeSectionRead:=True;
            CodeSectionIndex:=SectionIndex;
            if not ReadUleb32(CodeEntriesCount) then
              begin
                InputError('Error reading the code entries cound from the code section');
                exit;
              end;
            if CodeEntriesCount <> (Length(FuncTypes) - FuncTypeImportsCount) then
              begin
                InputError('Code segment count in the code section does not match the function definition count in the function section');
                exit;
              end;
            SetLength(CodeSegments,CodeEntriesCount);
            for i:=0 to CodeEntriesCount-1 do
              with CodeSegments[i] do
                begin
                  if not ReadUleb32(CodeSize) then
                    begin
                      InputError('Error reading the code size of an entry in the code section');
                      exit;
                    end;
                  if (AReader.Pos+CodeSize)>(SectionStart+SectionSize) then
                    begin
                      InputError('Code segment exceeds the bounds of the code section');
                      exit;
                    end;
                  DataPos:=AReader.Pos;
                  CodeSectionOffset:=AReader.Pos-SectionStart;
                  AReader.Seek(AReader.Pos+CodeSize);
                end;
            if AReader.Pos<>(SectionStart+SectionSize) then
              begin
                InputError('Unexpected code section size');
                exit;
              end;
            Result:=true;
          end;

        function ReadDataSection: Boolean;

          function ReadExpr(out ExprV: int32): Boolean;
            var
              b: Byte;
            begin
              Result:=False;
              if not Read(b,1) then
                exit;
              if b<>$41 then
                begin
                  InputError('Only i32.const expressions supported');
                  exit;
                end;
              if not ReadSleb32(ExprV) then
                exit;
              if not Read(b,1) then
                exit;
              if b<>$0B then
                begin
                  InputError('Only single const expressions supported');
                  exit;
                end;
              Result:=True;
            end;

          var
            DataCount: uint32;
            DataType: Byte;
            i: Integer;
          begin
            Result:=False;
            if DataSectionRead then
              begin
                InputError('Data section is duplicated');
                exit;
              end;
            DataSectionRead:=True;
            DataSectionIndex:=SectionIndex;
            if not ReadUleb32(DataCount) then
              begin
                InputError('Error reading the data entries count from the data section');
                exit;
              end;
            if DataCountSectionRead then
              begin
                if Length(DataSegments)<>DataCount then
                  begin
                    InputError('Data entries count in the data section do not match the number, specified in the data count section');
                    exit;
                  end;
              end
            else
              SetLength(DataSegments,DataCount);
            for i:=0 to DataCount-1 do
              with DataSegments[i] do
                begin
                  if not read(DataType, 1) then
                    begin
                      InputError('Error reading data type of segment from the data section');
                      exit;
                    end;
                  case DataType of
                    0:
                      begin
                        Active:=True;
                        MemIdx:=0;
                        if not ReadExpr(Offset) then
                          begin
                            InputError('Error reading memory offset of segment from the data section');
                            exit;
                          end;
                      end;
                    1:
                      Active:=False;
                    2:
                      begin
                        Active:=True;
                        if not ReadUleb32(MemIdx) then
                          begin
                            InputError('Error reading MemIdx of segment from the data section');
                            exit;
                          end;
                        if not ReadExpr(Offset) then
                          begin
                            InputError('Error reading memory offset of segment from the data section');
                            exit;
                          end;
                      end;
                    else
                      begin
                        InputError('Unsupported data type of segment in the data section: ' + tostr(DataType));
                        exit;
                      end;
                  end;
                  if MemIdx<>0 then
                    begin
                      InputError('Memory index other than 0 not supported (got ' + tostr(MemIdx) + ')');
                      exit;
                    end;
                  if not Active then
                    begin
                      InputError('Passive memory segments not supported');
                      exit;
                    end;
                  if not ReadUleb32(Len) then
                    begin
                      InputError('Error reading data segment length');
                      exit;
                    end;
                  if (AReader.Pos+Len)>(SectionStart+SectionSize) then
                    begin
                      InputError('Data segment exceeds the bounds of the data section');
                      exit;
                    end;
                  DataPos:=AReader.Pos;
                  DataSectionOffset:=AReader.Pos-SectionStart;
                  AReader.Seek(AReader.Pos+Len);
                end;
            if AReader.Pos<>(SectionStart+SectionSize) then
              begin
                InputError('Unexpected data section size');
                exit;
              end;
            Result:=true;
          end;

        function ReadDataCountSection: Boolean;
          var
            DataCount: uint32;
          begin
            Result:=False;
            if DataCountSectionRead then
              begin
                InputError('Data count section is duplicated');
                exit;
              end;
            DataCountSectionRead:=True;
            if DataSectionRead then
              begin
                InputError('The data count section must occur before the data section');
                exit;
              end;
            if not ReadUleb32(DataCount) then
              begin
                InputError('Error reading the data count from the data count section');
                exit;
              end;
            if AReader.Pos<>(SectionStart+SectionSize) then
              begin
                InputError('Unexpected data count section size');
                exit;
              end;
            SetLength(DataSegments, DataCount);
            Result:=true;
          end;

        begin
          Result:=False;
          Inc(SectionIndex);
          if not AReader.read(SectionId,1) then
            begin
              InputError('Error reading section ID');
              exit;
            end;
          CheckSectionBounds:=false;
          if not ReadUleb32(SectionSize) then
            begin
              InputError('Error reading section size');
              exit;
            end;
          if (AReader.Pos+SectionSize)>AReader.size then
            begin
              InputError('Section exceeds beyond the end of file');
              exit;
            end;
          SectionStart:=AReader.Pos;
          CheckSectionBounds:=true;
          case SectionId of
            Byte(wsiCustom):
              if not ReadCustomSection then
                begin
                  InputError('Error encountered, while reading a custom section');
                  exit;
                end;
            Byte(wsiType):
              if not ReadTypeSection then
                begin
                  InputError('Error reading the type section');
                  exit;
                end;
            Byte(wsiImport):
              if not ReadImportSection then
                begin
                  InputError('Error reading the import section');
                  exit;
                end;
            Byte(wsiFunction):
              if not ReadFunctionSection then
                begin
                  InputError('Error reading the function section');
                  exit;
                end;
            Byte(wsiGlobal):
              if not ReadGlobalSection then
                begin
                  InputError('Error reading the global section');
                  exit;
                end;
            Byte(wsiExport):
              if not ReadExportSection then
                begin
                  InputError('Error reading the export section');
                  exit;
                end;
            Byte(wsiElement):
              if not ReadElementSection then
                begin
                  InputError('Error reading the element section');
                  exit;
                end;
            Byte(wsiTag):
              if not ReadTagSection then
                begin
                  InputError('Error reading the tag section');
                  exit;
                end;
            Byte(wsiCode):
              if not ReadCodeSection then
                begin
                  InputError('Error reading the code section');
                  exit;
                end;
            Byte(wsiData):
              if not ReadDataSection then
                begin
                  InputError('Error reading the data section');
                  exit;
                end;
            Byte(wsiDataCount):
              begin
                if not ReadDataCountSection then
                  begin
                    InputError('Error reading the data count section');
                    exit;
                  end;
              end
            else
              begin
                InputError('Unknown section: ' + ToStr(SectionId));
                exit;
              end;
          end;
          if SectionSize>0 then
            AReader.seek(SectionStart+SectionSize);
          Result:=True;
        end;

        function FindCodeSegment(Ofs: uint32): Integer;
          var
            L, R, M: Integer;
          begin
            L:=Low(CodeSegments);
            R:=High(CodeSegments);
            while L<=R do
              begin
                M:=(L+R) div 2;
                if (CodeSegments[M].CodeSectionOffset+CodeSegments[M].CodeSize-1) < Ofs then
                  L:=M+1
                else if CodeSegments[M].CodeSectionOffset > Ofs then
                  R:=M-1
                else
                  begin
                    Result:=M;
                    exit;
                  end;
              end;
            Result:=-1;
          end;

        function FindDataSegment(Ofs: uint32): Integer;
          var
            L, R, M: Integer;
          begin
            L:=Low(DataSegments);
            R:=High(DataSegments);
            while L<=R do
              begin
                M:=(L+R) div 2;
                if (DataSegments[M].DataSectionOffset+DataSegments[M].Len-1) < Ofs then
                  L:=M+1
                else if DataSegments[M].DataSectionOffset > Ofs then
                  R:=M-1
                else
                  begin
                    Result:=M;
                    exit;
                  end;
              end;
            Result:=-1;
          end;

      var
        ModuleMagic: array [0..3] of Byte;
        ModuleVersion: array [0..3] of Byte;
        i, j, FirstCodeSegmentIdx, FirstDataSegmentIdx, SegI: Integer;
        CurrSec, ObjSec: TObjSection;
        BaseSectionOffset: UInt32;
        ObjReloc: TWasmObjRelocation;
        ds: TWasmCustomDebugSectionType;
      begin
        FReader:=AReader;
        InputFileName:=AReader.FileName;
        objdata:=CObjData.Create(InputFileName);
        result:=false;
        CodeSegments:=nil;
        DataSegments:=nil;
        SymbolTable:=nil;
        RelocationTable:=nil;
        SetLength(RelocationTable,2+(Ord(High(TWasmCustomDebugSectionType))-Ord(Low(TWasmCustomDebugSectionType))+1));
        FuncTypes:=nil;
        FuncTypeImportsCount:=0;
        TableTypes:=nil;
        TableTypeImportsCount:=0;
        MemTypes:=nil;
        MemTypeImportsCount:=0;
        GlobalTypes:=nil;
        GlobalTypeImportsCount:=0;
        TagTypes:=nil;
        TagTypeImportsCount:=0;

        if not AReader.read(ModuleMagic,4) then
          exit;
        for i:=0 to 3 do
          if ModuleMagic[i]<>WasmModuleMagic[i] then
            exit;
        if not AReader.read(ModuleVersion,4) then
          exit;
        for i:=0 to 3 do
          if ModuleVersion[i]<>WasmVersion[i] then
            exit;
        while AReader.Pos<AReader.size do
          if not ReadSection then
            exit;

        { fill the code segment names }
        for i:=low(SymbolTable) to high(SymbolTable) do
          with SymbolTable[i] do
            if (SymKind=SYMTAB_FUNCTION) and ((SymFlags and WASM_SYM_UNDEFINED)=0) then
              begin
                if FuncTypes[SymIndex].IsImport then
                  begin
                    InputError('WASM_SYM_UNDEFINED not set on a SYMTAB_FUNCTION symbol, that is an import');
                    exit;
                  end;
                if (SymFlags and WASM_SYM_EXPLICIT_NAME)=0 then
                  begin
                    with CodeSegments[SymIndex-FuncTypeImportsCount] do
                      begin
                        SegName:='.text.n_'+SymName;
                        SegIsExported:=FuncTypes[SymIndex].IsExported;
                      end;
                  end;
              end;

        { create segments }
        FirstCodeSegmentIdx:=ObjData.ObjSectionList.Count;
        for i:=low(CodeSegments) to high(CodeSegments) do
          with CodeSegments[i] do
            begin
              if SegName='' then
                begin
                  InputError('Code section ' + tostr(i) + ' does not have a main symbol defined in the symbol table');
                  exit;
                end;
              if SegIsExported or not (cs_link_smart in current_settings.globalswitches) then
                CurrSec:=ObjData.createsection(SegName,1,[oso_executable,oso_Data,oso_load,oso_keep],false)
              else
                CurrSec:=ObjData.createsection(SegName,1,[oso_executable,oso_Data,oso_load],false);
              CurrSec.DataPos:=DataPos;
              CurrSec.Size:=CodeSize;
            end;
        FirstDataSegmentIdx:=ObjData.ObjSectionList.Count;
        for i:=low(DataSegments) to high(DataSegments) do
          with DataSegments[i] do
            if Active then
              begin
                if not (cs_link_smart in current_settings.globalswitches) then
                  CurrSec:=ObjData.createsection(SegName,1 shl SegAlignment,[oso_Data,oso_load,oso_write,oso_keep],false)
                else
                  CurrSec:=ObjData.createsection(SegName,1 shl SegAlignment,[oso_Data,oso_load,oso_write],false);
                CurrSec.DataPos:=DataPos;
                CurrSec.MemPos:=Offset;
                CurrSec.Size:=Len;
              end;
        ReadSectionContent(ObjData);

        for i:=low(SymbolTable) to high(SymbolTable) do
          with SymbolTable[i] do
            case SymKind of
              SYMTAB_DATA:
                if (SymFlags and WASM_SYM_UNDEFINED)<>0 then
                  begin
                    objsym:=TWasmObjSymbol(ObjData.CreateSymbol(SymName));
                    objsym.bind:=AB_EXTERNAL;
                    if (SymFlags and WASM_SYM_TLS)<>0 then
                      begin
                        objsym.typ:=AT_TLS;
                        objsym.TlsGlobalSym:=TWasmObjSymbol(ObjData.CreateSymbol('GOT.mem.'+SymName));
                        objsym.TlsGlobalSym.TlsDataSym:=objsym;
                        objsym.TlsGlobalSym.bind:=AB_EXTERNAL;
                        objsym.TlsGlobalSym.typ:=AT_WASM_GLOBAL;
                        objsym.TlsGlobalSym.objsection:=nil;
                        objsym.TlsGlobalSym.offset:=0;
                        objsym.TlsGlobalSym.size:=1;
                        objsym.TlsGlobalSym.LinkingData.GlobalType:=wbt_i32;
                        objsym.TlsGlobalSym.LinkingData.GlobalIsMutable:=true;
                      end
                    else
                      objsym.typ:=AT_DATA;
                    objsym.objsection:=nil;
                    objsym.offset:=0;
                    objsym.size:=0;
                  end
                else
                  begin
                    objsym:=TWasmObjSymbol(ObjData.CreateSymbol(SymName));
                    if (SymFlags and WASM_SYM_BINDING_LOCAL)<> 0 then
                      objsym.bind:=AB_LOCAL
                    else
                      objsym.bind:=AB_GLOBAL;
                    if (SymFlags and WASM_SYM_TLS)<>0 then
                      begin
                        objsym.typ:=AT_TLS;
                        objsym.TlsGlobalSym:=TWasmObjSymbol(ObjData.CreateSymbol('GOT.mem.'+SymName));
                        objsym.TlsGlobalSym.TlsDataSym:=objsym;
                        objsym.TlsGlobalSym.bind:=objsym.bind;
                        objsym.TlsGlobalSym.typ:=AT_WASM_GLOBAL;
                        objsym.TlsGlobalSym.objsection:=ObjData.createsection('.wasm_globals.n_'+objsym.TlsGlobalSym.Name,1,[oso_Data,oso_load],true);
                        if objsym.TlsGlobalSym.objsection.Size=0 then
                          objsym.TlsGlobalSym.objsection.WriteZeros(1);
                        TWasmObjSection(objsym.TlsGlobalSym.objsection).MainFuncSymbol:=objsym.TlsGlobalSym;
                        objsym.TlsGlobalSym.offset:=0;
                        objsym.TlsGlobalSym.size:=1;
                        objsym.TlsGlobalSym.LinkingData.GlobalType:=wbt_i32;
                        objsym.TlsGlobalSym.LinkingData.GlobalIsMutable:=true;
                      end
                    else
                      objsym.typ:=AT_DATA;
                    objsym.objsection:=TObjSection(ObjData.ObjSectionList[FirstDataSegmentIdx+SymIndex]);
                    objsym.offset:=SymOffset;
                    objsym.size:=SymSize;
                  end;
              SYMTAB_FPC_CUSTOM:
                if (SymFlags and WASM_SYM_UNDEFINED)<>0 then
                  begin
                    objsym:=TWasmObjSymbol(ObjData.CreateSymbol(SymName));
                    objsym.bind:=AB_EXTERNAL;
                    if (SymFlags and WASM_SYM_TLS)<>0 then
                      internalerror(2024080702);
                    objsym.typ:=AT_DATA;
                    objsym.objsection:=nil;
                    objsym.offset:=0;
                    objsym.size:=0;
                  end
                else
                  begin
                    objsym:=TWasmObjSymbol(ObjData.CreateSymbol(SymName));
                    if (SymFlags and WASM_SYM_BINDING_LOCAL)<> 0 then
                      objsym.bind:=AB_LOCAL
                    else
                      objsym.bind:=AB_GLOBAL;
                    if (SymFlags and WASM_SYM_TLS)<>0 then
                      internalerror(2024080703);
                    objsym.typ:=AT_DATA;
                    objsym.objsection:=TObjSection(ObjData.ObjSectionList.Find(WasmCustomSectionName[SymCustomSectionType]));
                    objsym.offset:=SymOffset;
                    objsym.size:=SymSize;
                  end;
              SYMTAB_FUNCTION:
                begin
                  if (SymFlags and WASM_SYM_UNDEFINED)<>0 then
                    begin
                      if not FuncTypes[SymIndex].IsImport then
                        begin
                          InputError('WASM_SYM_UNDEFINED set on a SYMTAB_FUNCTION symbol, that is not an import');
                          exit;
                        end;
                      if (SymFlags and WASM_SYM_EXPLICIT_NAME)<>0 then
                        begin
                          objsym:=TWasmObjSymbol(ObjData.CreateSymbol(SymName));
                          objsym.bind:=AB_EXTERNAL;
                          objsym.typ:=AT_FUNCTION;
                          objsym.objsection:=nil;
                          objsym.offset:=0;
                          objsym.size:=0;
                          objsym.LinkingData.ImportModule:=FuncTypes[SymIndex].ImportModName;
                          objsym.LinkingData.ImportName:=FuncTypes[SymIndex].ImportName;
                        end
                      else
                        begin
                          if FuncTypes[SymIndex].ImportModName = 'env' then
                            objsym:=TWasmObjSymbol(ObjData.CreateSymbol(FuncTypes[SymIndex].ImportName))
                          else
                            objsym:=TWasmObjSymbol(ObjData.CreateSymbol(FuncTypes[SymIndex].ImportModName + '.' + FuncTypes[SymIndex].ImportName));
                          objsym.bind:=AB_EXTERNAL;
                          objsym.typ:=AT_FUNCTION;
                          objsym.objsection:=nil;
                          objsym.offset:=0;
                          objsym.size:=0;
                        end;
                    end
                  else
                    begin
                      objsym:=TWasmObjSymbol(ObjData.CreateSymbol(SymName));
                      objsym.bind:=AB_GLOBAL;
                      objsym.typ:=AT_FUNCTION;
                      objsym.objsection:=TObjSection(ObjData.ObjSectionList[FirstCodeSegmentIdx+SymIndex-FuncTypeImportsCount]);
                      if (SymFlags and WASM_SYM_EXPLICIT_NAME)=0 then
                        TWasmObjSection(ObjData.ObjSectionList[FirstCodeSegmentIdx+SymIndex-FuncTypeImportsCount]).MainFuncSymbol:=objsym;
                      objsym.offset:=0;
                      objsym.size:=objsym.objsection.Size;
                    end;
                  objsym.LinkingData.FuncType:=TWasmFuncType.Create(FFuncTypes[FuncTypes[SymIndex].typidx]);
                  objsym.LinkingData.IsExported:=FuncTypes[SymIndex].IsExported;
                  objsym.LinkingData.ExportName:=FuncTypes[SymIndex].ExportName;
                end;
              SYMTAB_GLOBAL:
                begin
                  if (SymFlags and WASM_SYM_UNDEFINED)<>0 then
                    begin
                      if not GlobalTypes[SymIndex].IsImport then
                        begin
                          InputError('WASM_SYM_UNDEFINED set on a SYMTAB_GLOBAL symbol, that is not an import');
                          exit;
                        end;
                      if (SymFlags and WASM_SYM_EXPLICIT_NAME)<>0 then
                        begin
                          objsym:=TWasmObjSymbol(ObjData.CreateSymbol(SymName));
                          objsym.bind:=AB_EXTERNAL;
                          objsym.typ:=AT_WASM_GLOBAL;
                          objsym.objsection:=nil;
                          objsym.offset:=0;
                          objsym.size:=1;
                          objsym.LinkingData.ImportModule:=GlobalTypes[SymIndex].ImportModName;
                          objsym.LinkingData.ImportName:=GlobalTypes[SymIndex].ImportName;
                        end
                      else
                        begin
                          if GlobalTypes[SymIndex].ImportModName = 'env' then
                            objsym:=TWasmObjSymbol(ObjData.CreateSymbol(GlobalTypes[SymIndex].ImportName))
                          else
                            objsym:=TWasmObjSymbol(ObjData.CreateSymbol(GlobalTypes[SymIndex].ImportModName + '.' + GlobalTypes[SymIndex].ImportName));
                          objsym.bind:=AB_EXTERNAL;
                          objsym.typ:=AT_WASM_GLOBAL;
                          objsym.objsection:=nil;
                          objsym.offset:=0;
                          objsym.size:=1;
                        end;
                    end
                  else
                    begin
                      if GlobalTypes[SymIndex].IsImport then
                        begin
                          InputError('WASM_SYM_UNDEFINED not set on a SYMTAB_GLOBAL symbol, that is an import');
                          exit;
                        end;
                      objsym:=TWasmObjSymbol(ObjData.CreateSymbol(SymName));
                      objsym.bind:=AB_GLOBAL;
                      objsym.typ:=AT_WASM_GLOBAL;
                      objsym.objsection:=ObjData.createsection('.wasm_globals.n_'+SymName,1,[oso_Data,oso_load],true);
                      if objsym.objsection.Size=0 then
                        objsym.objsection.WriteZeros(1);
                      if (SymFlags and WASM_SYM_EXPLICIT_NAME)=0 then
                        TWasmObjSection(objsym.objsection).MainFuncSymbol:=objsym;
                      objsym.offset:=0;
                      objsym.size:=1;
                      objsym.LinkingData.GlobalInitializer:=GlobalTypes[SymIndex].GlobalInit;
                    end;
                  objsym.LinkingData.GlobalType:=GlobalTypes[SymIndex].valtype;
                  objsym.LinkingData.GlobalIsMutable:=GlobalTypes[SymIndex].IsMutable;
                  objsym.LinkingData.IsExported:=GlobalTypes[SymIndex].IsExported;
                  objsym.LinkingData.ExportName:=GlobalTypes[SymIndex].ExportName;
                end;
              SYMTAB_SECTION:
                begin
                  for ds:=Low(DebugSectionIndex) to High(DebugSectionIndex) do
                    if DebugSectionIndex[ds]=TargetSection then
                      begin
                        ObjSec:=TWasmObjSection(ObjData.findsection(WasmCustomSectionName[ds]));
                        break;
                      end;
                  if ObjSec=nil then
                    begin
                      InputError('SYMTAB_SECTION entry points to an unsupported section');
                      exit;
                    end;
                end;
              SYMTAB_EVENT:
                begin
                  if (SymFlags and WASM_SYM_UNDEFINED)<>0 then
                    begin
                      if not TagTypes[SymIndex].IsImport then
                        begin
                          InputError('WASM_SYM_UNDEFINED set on a SYMTAB_EVENT symbol, that is not an import');
                          exit;
                        end;
                      if (SymFlags and WASM_SYM_EXPLICIT_NAME)<>0 then
                        begin
                          objsym:=TWasmObjSymbol(ObjData.CreateSymbol(SymName));
                          objsym.bind:=AB_EXTERNAL;
                          objsym.typ:=AT_WASM_EXCEPTION_TAG;
                          objsym.objsection:=nil;
                          objsym.offset:=0;
                          objsym.size:=1;
                          objsym.LinkingData.ImportModule:=TagTypes[SymIndex].ImportModName;
                          objsym.LinkingData.ImportName:=TagTypes[SymIndex].ImportName;
                        end
                      else
                        begin
                          if GlobalTypes[SymIndex].ImportModName = 'env' then
                            objsym:=TWasmObjSymbol(ObjData.CreateSymbol(GlobalTypes[SymIndex].ImportName))
                          else
                            objsym:=TWasmObjSymbol(ObjData.CreateSymbol(GlobalTypes[SymIndex].ImportModName + '.' + GlobalTypes[SymIndex].ImportName));
                          objsym.bind:=AB_EXTERNAL;
                          objsym.typ:=AT_WASM_EXCEPTION_TAG;
                          objsym.objsection:=nil;
                          objsym.offset:=0;
                          objsym.size:=1;
                        end;
                    end
                  else
                    begin
                      if TagTypes[SymIndex].IsImport then
                        begin
                          InputError('WASM_SYM_UNDEFINED not set on a SYMTAB_EVENT symbol, that is an import');
                          exit;
                        end;
                      objsym:=TWasmObjSymbol(ObjData.CreateSymbol(SymName));
                      if (symflags and WASM_SYM_BINDING_WEAK) <> 0 then
                        objsym.bind:=AB_WEAK_EXTERNAL
                      else if (symflags and WASM_SYM_BINDING_LOCAL) <> 0 then
                        objsym.bind:=AB_LOCAL
                      else
                        objsym.bind:=AB_GLOBAL;
                      objsym.typ:=AT_WASM_EXCEPTION_TAG;
                      objsym.objsection:=ObjData.createsection('.wasm_tags.n_'+SymName,1,[oso_Data,oso_load],true);
                      if objsym.objsection.Size=0 then
                        objsym.objsection.WriteZeros(1);
                      if (SymFlags and WASM_SYM_EXPLICIT_NAME)=0 then
                        TWasmObjSection(objsym.objsection).MainFuncSymbol:=objsym;
                      objsym.offset:=0;
                      objsym.size:=1;
                    end;
                  objsym.LinkingData.FuncType:=TWasmFuncType.Create(FFuncTypes[TagTypes[SymIndex].TagTypeIdx]);
                  objsym.LinkingData.IsExported:=TagTypes[SymIndex].IsExported;
                  objsym.LinkingData.ExportName:=TagTypes[SymIndex].ExportName;
                end;
              SYMTAB_TABLE:
                {TODO};
            end;

        for j:=0 to high(RelocationTable) do
          for i:=0 to high(RelocationTable[j]) do
            with RelocationTable[j,i] do
              begin
                case j of
                  0:
                    begin
                      SegI:=FindCodeSegment(RelocOffset);
                      if SegI=-1 then
                        begin
                          InputError('Relocation offset not found in code segment');
                          Exit;
                        end;
                      BaseSectionOffset:=CodeSegments[SegI].CodeSectionOffset;
                      ObjSec:=TObjSection(ObjData.ObjSectionList[FirstCodeSegmentIdx+SegI]);
                    end;
                  1:
                    begin
                      SegI:=FindDataSegment(RelocOffset);
                      if SegI=-1 then
                        begin
                          InputError('Relocation offset not found in data segment');
                          Exit;
                        end;
                      BaseSectionOffset:=DataSegments[SegI].DataSectionOffset;
                      ObjSec:=TObjSection(ObjData.ObjSectionList[FirstDataSegmentIdx+SegI]);
                    end;
                  2..2+(Ord(High(TWasmCustomDebugSectionType))-Ord(Low(TWasmCustomDebugSectionType))):
                    begin
                      BaseSectionOffset:=0;
                      ObjSec:=ObjData.findsection(WasmCustomSectionName[TWasmCustomSectionType((j-2)+Ord(Low(TWasmCustomDebugSectionType)))]);
                    end;
                  else
                    internalerror(2023122801);
                end;
                case RelocType of
                  R_WASM_FUNCTION_INDEX_LEB:
                    ObjSec.ObjRelocations.Add(TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym,RELOC_FUNCTION_INDEX_LEB));
                  R_WASM_TABLE_INDEX_SLEB:
                    ObjSec.ObjRelocations.Add(TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym,RELOC_MEMORY_ADDR_OR_TABLE_INDEX_SLEB));
                  R_WASM_TABLE_INDEX_I32:
                    ObjSec.ObjRelocations.Add(TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym,RELOC_ABSOLUTE));
                  R_WASM_MEMORY_ADDR_LEB:
                    begin
                      ObjReloc:=TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym,RELOC_MEMORY_ADDR_LEB);
                      ObjReloc.Addend:=RelocAddend;
                      ObjSec.ObjRelocations.Add(ObjReloc);
                    end;
                  R_WASM_MEMORY_ADDR_SLEB:
                    begin
                      ObjReloc:=TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym,RELOC_MEMORY_ADDR_OR_TABLE_INDEX_SLEB);
                      ObjReloc.Addend:=RelocAddend;
                      ObjSec.ObjRelocations.Add(ObjReloc);
                    end;
                  R_WASM_MEMORY_ADDR_I32:
                    begin
                      ObjReloc:=TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym,RELOC_ABSOLUTE);
                      ObjReloc.Addend:=RelocAddend;
                      ObjSec.ObjRelocations.Add(ObjReloc);
                    end;
                  R_WASM_TYPE_INDEX_LEB:
                    ObjSec.ObjRelocations.Add(TWasmObjRelocation.CreateFuncType(RelocOffset-BaseSectionOffset,FFuncTypes[RelocIndex]));
                  R_WASM_FUNCTION_OFFSET_I32:
                    begin
                      ObjReloc:=TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym,RELOC_ABSOLUTE);
                      ObjReloc.Addend:=RelocAddend;
                      ObjReloc.IsFunctionOffsetI32:=True;
                      ObjSec.ObjRelocations.Add(ObjReloc);
                    end;
                  R_WASM_SECTION_OFFSET_I32:
                    begin
                      ObjReloc:=TWasmObjRelocation.CreateSection(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSec,RELOC_ABSOLUTE);
                      ObjReloc.Addend:=RelocAddend;
                      ObjSec.ObjRelocations.Add(ObjReloc);
                    end;
                  R_WASM_GLOBAL_INDEX_LEB:
                    begin
                      ObjSec.ObjRelocations.Add(TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym,RELOC_GLOBAL_INDEX_LEB));
                      if Assigned(SymbolTable[RelocIndex].ObjSym.TlsGlobalSym) then
                        ObjSec.ObjRelocations.Add(TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym.TlsGlobalSym,RELOC_GLOBAL_INDEX_LEB));
                    end;
                  R_WASM_GLOBAL_INDEX_I32:
                    begin
                      ObjSec.ObjRelocations.Add(TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym,RELOC_ABSOLUTE));
                      if Assigned(SymbolTable[RelocIndex].ObjSym.TlsGlobalSym) then
                        ObjSec.ObjRelocations.Add(TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym.TlsGlobalSym,RELOC_ABSOLUTE));
                    end;
                  R_WASM_TAG_INDEX_LEB:
                    ObjSec.ObjRelocations.Add(TWasmObjRelocation.CreateSymbol(RelocOffset-BaseSectionOffset,SymbolTable[RelocIndex].ObjSym,RELOC_TAG_INDEX_LEB));
                  else
                    internalerror(2023122802);
                end;
              end;

        Result:=True;
      end;

{****************************************************************************
                               TWasmExeOutput
****************************************************************************}

    procedure TWasmExeOutput.AddToNameMap(var nm: TCustomSectionNameMap; aidx: UInt32; const aname: string);
      begin
        SetLength(nm,Length(nm)+1);
        with nm[High(nm)] do
          begin
            idx:=aidx;
            name:=aname;
          end;
      end;

    procedure TWasmExeOutput.AddToFunctionNameMap(aidx: UInt32; const aname: string);
      begin
        AddToNameMap(FFunctionNameMap,aidx,aname);
      end;

    procedure TWasmExeOutput.AddToGlobalNameMap(aidx: UInt32; const aname: string);
      begin
        AddToNameMap(FGlobalNameMap,aidx,aname);
      end;

    procedure TWasmExeOutput.AddToDataNameMap(aidx: UInt32; const aname: string);
      begin
        AddToNameMap(FDataNameMap,aidx,aname);
      end;

    procedure TWasmExeOutput.AddToTagNameMap(aidx: UInt32; const aname: string);
      begin
        AddToNameMap(FTagNameMap,aidx,aname);
      end;

    procedure TWasmExeOutput.WriteWasmSection(wsid: TWasmSectionID);
      var
        b: byte;
      begin
        b:=ord(wsid);
        Writer.write(b,1);
        WriteUleb(Writer,FWasmSections[wsid].size);
        Writer.writearray(FWasmSections[wsid]);
      end;

    procedure TWasmExeOutput.WriteWasmSectionIfNotEmpty(wsid: TWasmSectionID);
      begin
        if FWasmSections[wsid].size>0 then
          WriteWasmSection(wsid);
      end;

    procedure TWasmExeOutput.WriteWasmCustomSection(wcst: TWasmCustomSectionType);
      var
        b: byte;
      begin
        b:=0;
        Writer.write(b,1);
        WriteUleb(Writer,FWasmCustomSections[wcst].size);
        Writer.writearray(FWasmCustomSections[wcst]);
      end;

    function TWasmExeOutput.writeData: boolean;

      procedure WriteImportSection;
        var
          imports_count,
          memory_imports: SizeInt;
          i: Integer;
        begin
          if ts_wasm_threads in current_settings.targetswitches then
            memory_imports:=1
          else
            memory_imports:=0;
          imports_count:=Length(FFunctionImports)+memory_imports;
          WriteUleb(FWasmSections[wsiImport],imports_count);
          if ts_wasm_threads in current_settings.targetswitches then
            begin
              WriteName(FWasmSections[wsiImport],'env');
              WriteName(FWasmSections[wsiImport],'memory');
              WriteByte(FWasmSections[wsiImport],$02);  { mem }
              WriteByte(FWasmSections[wsiImport],$03);  { shared }
              WriteUleb(FWasmSections[wsiImport],FMinMemoryPages);
              WriteUleb(FWasmSections[wsiImport],Max(FMinMemoryPages,FMaxMemoryPages));  { max pages }
            end;
          for i:=0 to Length(FFunctionImports)-1 do
            with FFunctionImports[i] do
              begin
                WriteName(FWasmSections[wsiImport],ModName);
                WriteName(FWasmSections[wsiImport],Name);
                WriteByte(FWasmSections[wsiImport],$00);  { func }
                WriteUleb(FWasmSections[wsiImport],TypeIdx);
              end;
        end;

      procedure WriteCodeSegments;
        var
          i: Integer;
          exesec: TExeSection;
          objsec: TWasmObjSection;
        begin
          exesec:=FindExeSection('.text');
          if not assigned(exesec) then
            internalerror(2023123102);
          if not (oso_Data in exesec.SecOptions) then
            internalerror(2023123103);
          WriteUleb(FWasmSections[wsiFunction],exesec.ObjSectionList.Count);
          WriteUleb(FWasmSections[wsiCode],exesec.ObjSectionList.Count);
          for i:=0 to exesec.ObjSectionList.Count-1 do
            begin
              objsec:=TWasmObjSection(exesec.ObjSectionList[i]);
              if not (oso_data in objsec.secoptions) then
                internalerror(2023123104);
              if not assigned(objsec.data) then
                internalerror(2023123105);
              if objsec.MainFuncSymbol.LinkingData.ExeFunctionIndex<>(i+Length(FFunctionImports)) then
                internalerror(2024010101);
              WriteUleb(FWasmSections[wsiFunction],objsec.MainFuncSymbol.LinkingData.ExeTypeIndex);
              WriteUleb(FWasmSections[wsiCode],objsec.Data.size);
              objsec.Data.seek(0);
              CopyDynamicArray(objsec.Data,FWasmSections[wsiCode],objsec.Data.size);
            end;
        end;

      procedure WriteDataSegments;

        procedure WriteExeSection(exesec: TExeSection);
          var
            i: Integer;
            objsec: TObjSection;
            exesecdatapos: LongWord;
            dpos, pad: QWord;
          begin
            AddToDataNameMap(Length(FDataNameMap),exesec.Name);
            if ts_wasm_threads in current_settings.targetswitches then
              WriteByte(FWasmSections[wsiData],1)  { mode passive }
            else
              begin
                WriteByte(FWasmSections[wsiData],0);  { mode active, memory 0, offset e }

                WriteByte(FWasmSections[wsiData],$41);  { i32.const }
                WriteSleb(FWasmSections[wsiData],longint(exesec.MemPos));
                WriteByte(FWasmSections[wsiData],$0B);  { end }
              end;

            WriteUleb(FWasmSections[wsiData],exesec.Size);
            exesecdatapos:=FWasmSections[wsiData].size;
            for i:=0 to exesec.ObjSectionList.Count-1 do
              begin
                objsec:=TObjSection(exesec.ObjSectionList[i]);
                if not (oso_data in objsec.secoptions) then
                  internalerror(2024010104);
                if not assigned(objsec.data) then
                  internalerror(2024010105);

                dpos:=objsec.MemPos-exesec.MemPos+exesecdatapos;
                pad:=dpos-FWasmSections[wsiData].size;
                { objsection must be within SecAlign bytes from the previous one }
                if (dpos<FWasmSections[wsiData].Size) or
                  (pad>=max(objsec.SecAlign,1)) then
                  internalerror(2024010106);
                writeZeros(FWasmSections[wsiData],pad);

                objsec.data.seek(0);
                CopyDynamicArray(objsec.data,FWasmSections[wsiData],objsec.data.size);
              end;
            if (FWasmSections[wsiData].size-exesecdatapos)<>exesec.Size then
              internalerror(2024010107);
          end;

        var
          DataCount: Integer;
          DataSecName: string;
          ExeSec: TExeSection;
        begin
          DataCount:=0;
          for DataSecName in DataSections do
            begin
              ExeSec:=FindExeSection(DataSecName);
              if Assigned(ExeSec) and (ExeSec.Size>0) then
                Inc(DataCount);
            end;
          WriteUleb(FWasmSections[wsiDataCount],DataCount);
          WriteUleb(FWasmSections[wsiData],DataCount);
          for DataSecName in DataSections do
            begin
              ExeSec:=FindExeSection(DataSecName);
              if Assigned(ExeSec) and (ExeSec.Size>0) then
                WriteExeSection(ExeSec);
            end;
        end;

      procedure WriteTableAndElemSections;
        const
          TableCount=1;
        var
          i: Integer;
        begin
          { Table section }

          WriteUleb(FWasmSections[wsiTable],TableCount);
          { table 0 }
          {   table type }
          WriteByte(FWasmSections[wsiTable],encode_wasm_basic_type(wbt_funcref));
          {   table limits }
          WriteByte(FWasmSections[wsiTable],$01);  { has min & max }
          WriteUleb(FWasmSections[wsiTable],Length(FIndirectFunctionTable));  { min }
          WriteUleb(FWasmSections[wsiTable],Length(FIndirectFunctionTable));  { max }

          { Elem section }

          WriteUleb(FWasmSections[wsiElement],1);  { 1 element segment }
          { element segment 0 }
          WriteByte(FWasmSections[wsiElement],0);  { type funcref, init((ref.func y) end)*, mode active <table 0, offset e> }
          { e:expr }
          WriteByte(FWasmSections[wsiElement],$41);  { i32.const }
          WriteSleb(FWasmSections[wsiElement],1);    { starting from 1 (table entry 0 is ref.null) }
          WriteByte(FWasmSections[wsiElement],$0B);  { end }
          { y*:vec(funcidx) }
          WriteUleb(FWasmSections[wsiElement],Length(FIndirectFunctionTable)-1);
          for i:=1 to Length(FIndirectFunctionTable)-1 do
            WriteUleb(FWasmSections[wsiElement],FIndirectFunctionTable[i].FuncIdx);
        end;

      procedure WriteGlobalSection;
        var
          exesec: TExeSection;
          globals_count, i: Integer;
          objsec: TWasmObjSection;
        begin
          exesec:=FindExeSection('.wasm_globals');
          if not assigned(exesec) then
            internalerror(2024010112);
          globals_count:=exesec.ObjSectionList.Count;
          if globals_count<>exesec.Size then
            internalerror(2024010113);
          WriteUleb(FWasmSections[wsiGlobal],globals_count);
          for i:=0 to exesec.ObjSectionList.Count-1 do
            begin
              objsec:=TWasmObjSection(exesec.ObjSectionList[i]);
              WriteByte(FWasmSections[wsiGlobal],encode_wasm_basic_type(objsec.MainFuncSymbol.LinkingData.GlobalType));
              if objsec.MainFuncSymbol.LinkingData.GlobalIsMutable then
                WriteByte(FWasmSections[wsiGlobal],1)
              else
                WriteByte(FWasmSections[wsiGlobal],0);
              { initializer expr }
              with objsec.MainFuncSymbol.LinkingData.GlobalInitializer do
                case typ of
                  wbt_i32:
                    begin
                      WriteByte(FWasmSections[wsiGlobal],$41);  { i32.const }
                      WriteSleb(FWasmSections[wsiGlobal],init_i32);
                    end;
                  wbt_i64:
                    begin
                      WriteByte(FWasmSections[wsiGlobal],$42);  { i64.const }
                      WriteSleb(FWasmSections[wsiGlobal],init_i64);
                    end;
                  wbt_f32:
                    begin
                      WriteByte(FWasmSections[wsiGlobal],$43);  { f32.const }
                      WriteF32LE(FWasmSections[wsiGlobal],init_f32);
                    end;
                  wbt_f64:
                    begin
                      WriteByte(FWasmSections[wsiGlobal],$44);  { f64.const }
                      WriteF64LE(FWasmSections[wsiGlobal],init_f64);
                    end;
                  wbt_funcref,
                  wbt_externref:
                    begin
                      WriteByte(FWasmSections[wsiGlobal],$D0);  { ref.null }
                      WriteByte(FWasmSections[wsiGlobal],encode_wasm_basic_type(typ));
                    end;
                  else
                    internalerror(2024010114);
                end;
              WriteByte(FWasmSections[wsiGlobal],$0B);  { end }
              { add entry for the name section }
              AddToGlobalNameMap(i,objsec.MainFuncSymbol.Name);
            end;
        end;

      procedure WriteTagSection;
        var
          exesec: TExeSection;
          tags_count, i: Integer;
          objsec: TWasmObjSection;
        begin
          exesec:=FindExeSection('.wasm_tags');
          if not assigned(exesec) then
            exit;
          tags_count:=exesec.ObjSectionList.Count;
          if tags_count<>exesec.Size then
            internalerror(2024010702);
          if tags_count=0 then
            exit;
          WriteUleb(FWasmSections[wsiTag],tags_count);
          for i:=0 to exesec.ObjSectionList.Count-1 do
            begin
              objsec:=TWasmObjSection(exesec.ObjSectionList[i]);
              WriteByte(FWasmSections[wsiTag],0);
              WriteUleb(FWasmSections[wsiTag],objsec.MainFuncSymbol.LinkingData.ExeTypeIndex);
              AddToTagNameMap(i,objsec.MainFuncSymbol.Name);
            end;
        end;

      procedure WriteExportSection;
        const
          MemoryExportsCount=1;
        var
          FunctionExportsCount: Integer;
          ExportsCount: Integer;
          textsec: TExeSection;
          i: Integer;
          objsec: TWasmObjSection;
        begin
          FunctionExportsCount:=0;
          textsec:=FindExeSection('.text');
          if not assigned(textsec) then
            internalerror(2024010115);
          for i:=0 to textsec.ObjSectionList.Count-1 do
            begin
              objsec:=TWasmObjSection(textsec.ObjSectionList[i]);
              if objsec.MainFuncSymbol.LinkingData.IsExported then
                Inc(FunctionExportsCount)
            end;

          ExportsCount:=MemoryExportsCount+FunctionExportsCount;

          WriteUleb(FWasmSections[wsiExport],ExportsCount);
          { export 0 }
          WriteName(FWasmSections[wsiExport],'memory');
          WriteByte(FWasmSections[wsiExport],$02);  { mem }
          WriteUleb(FWasmSections[wsiExport],0);    { memidx = 0 }

          for i:=0 to textsec.ObjSectionList.Count-1 do
            begin
              objsec:=TWasmObjSection(textsec.ObjSectionList[i]);
              if objsec.MainFuncSymbol.LinkingData.IsExported then
                begin
                  WriteName(FWasmSections[wsiExport],objsec.MainFuncSymbol.LinkingData.ExportName);
                  WriteByte(FWasmSections[wsiExport],$00);  { func }
                  WriteUleb(FWasmSections[wsiExport],objsec.MainFuncSymbol.LinkingData.ExeFunctionIndex);    { funcidx }
                end;
            end;
        end;

      procedure MaybeWriteDebugSection(st: TWasmCustomDebugSectionType);
        var
          exesec: TExeSection;
        begin
          exesec:=FindExeSection(WasmCustomSectionName[st]);
          if assigned(exesec) then
            begin
              WriteExeSectionToDynArray(exesec,FWasmCustomSections[st]);
              WriteWasmCustomSection(st);
            end;
        end;

      procedure WriteNameMap(const nm: TCustomSectionNameMap; dest: tdynamicarray);
        var
          i: Integer;
        begin
          WriteUleb(dest,Length(nm));
          for i:=low(nm) to high(nm) do
            with nm[i] do
              begin
                WriteUleb(dest,idx);
                WriteName(dest,name);
              end;
        end;

      procedure WriteNameSubsection(wnst: TWasmNameSubsectionType);
        begin
          if FWasmNameSubsections[wnst].size>0 then
            begin
              WriteByte(FWasmCustomSections[wcstName],Ord(wnst));
              WriteUleb(FWasmCustomSections[wcstName],FWasmNameSubsections[wnst].size);
              FWasmNameSubsections[wnst].seek(0);
              CopyDynamicArray(FWasmNameSubsections[wnst],FWasmCustomSections[wcstName],FWasmNameSubsections[wnst].size);
            end;
        end;

      procedure WriteNameSection;
        begin
          WriteName(FWasmNameSubsections[wnstModuleName],current_module.exefilename);
          WriteNameSubsection(wnstModuleName);

          WriteNameMap(FFunctionNameMap,FWasmNameSubsections[wnstFunctionNames]);
          WriteNameSubsection(wnstFunctionNames);

          WriteNameMap(FGlobalNameMap,FWasmNameSubsections[wnstGlobalNames]);
          WriteNameSubsection(wnstGlobalNames);

          WriteNameMap(FDataNameMap,FWasmNameSubsections[wnstDataNames]);
          WriteNameSubsection(wnstDataNames);

          if Length(FTagNameMap)>0 then
            begin
              WriteNameMap(FTagNameMap,FWasmNameSubsections[wnstTagNames]);
              WriteNameSubsection(wnstTagNames);
            end;
        end;

      var
        cust_sec: TWasmCustomSectionType;
      begin
        result:=false;
        FMaxMemoryPages:=align(maxheapsize,WasmPageSize) div WasmPageSize;

        { each custom sections starts with its name }
        for cust_sec in TWasmCustomSectionType do
          WriteName(FWasmCustomSections[cust_sec],WasmCustomSectionName[cust_sec]);

        SetStackPointer;
        SetTlsSizeAlignAndBase;
        SetThreadVarGlobalsInitValues;
        GenerateCode_InitTls;
        GenerateCode_InitSharedMemory;
        GenerateCode_InvokeHelper;

        FFuncTypes.WriteTo(FWasmSections[wsiType]);
        WriteImportSection;
        WriteCodeSegments;
        WriteDataSegments;
        WriteTableAndElemSections;
        WriteGlobalSection;
        WriteTagSection;
        WriteExportSection;

        if not (ts_wasm_threads in current_settings.targetswitches) then
          begin
            WriteUleb(FWasmSections[wsiMemory],1);
            if FMaxMemoryPages>=FMinMemoryPages then
              begin
                WriteByte(FWasmSections[wsiMemory],1);
                WriteUleb(FWasmSections[wsiMemory],FMinMemoryPages);
                WriteUleb(FWasmSections[wsiMemory],FMaxMemoryPages);
              end
            else
              begin
                WriteByte(FWasmSections[wsiMemory],0);
                WriteUleb(FWasmSections[wsiMemory],FMinMemoryPages);
              end;
          end;

        if ts_wasm_threads in current_settings.targetswitches then
          WriteUleb(FWasmSections[wsiStart],FInitSharedMemoryFunctionSym.LinkingData.ExeFunctionIndex);

        WriteNameSection;

        Writer.write(WasmModuleMagic,SizeOf(WasmModuleMagic));
        Writer.write(WasmVersion,SizeOf(WasmVersion));
        WriteWasmSection(wsiType);
        WriteWasmSection(wsiImport);
        WriteWasmSection(wsiFunction);
        WriteWasmSection(wsiTable);
        if not (ts_wasm_threads in current_settings.targetswitches) then
          WriteWasmSection(wsiMemory);
        WriteWasmSectionIfNotEmpty(wsiTag);
        WriteWasmSection(wsiGlobal);
        WriteWasmSection(wsiExport);
        if ts_wasm_threads in current_settings.targetswitches then
          WriteWasmSection(wsiStart);
        WriteWasmSection(wsiElement);
        WriteWasmSection(wsiDataCount);
        WriteWasmSection(wsiCode);
        WriteWasmSection(wsiData);

        MaybeWriteDebugSection(wcstDebugAbbrev);
        MaybeWriteDebugSection(wcstDebugInfo);
        MaybeWriteDebugSection(wcstDebugStr);
        MaybeWriteDebugSection(wcstDebugLine);
        MaybeWriteDebugSection(wcstDebugFrame);
        MaybeWriteDebugSection(wcstDebugAranges);
        MaybeWriteDebugSection(wcstDebugRanges);

        WriteWasmCustomSection(wcstName);

        result := true;
      end;

    procedure TWasmExeOutput.DoRelocationFixup(objsec: TObjSection);

      procedure writeUInt32LE(v: uint32);
        begin
{$ifdef FPC_BIG_ENDIAN}
          v:=SwapEndian(v);
{$endif FPC_BIG_ENDIAN}
          objsec.data.write(v,4);
        end;

      var
        i: Integer;
        objreloc: TWasmObjRelocation;
        objsym: TWasmObjSymbol;
      begin
        for i:=0 to objsec.ObjRelocations.Count-1 do
          begin
            objreloc:=TWasmObjRelocation(objsec.ObjRelocations[i]);
            if assigned(objreloc.symbol) then
              begin
                objsym:=TWasmObjSymbol(objreloc.symbol);
                case objreloc.typ of
                  RELOC_FUNCTION_INDEX_LEB:
                    begin
                      if objsym.LinkingData.ExeFunctionIndex=-1 then
                        internalerror(2024010103);
                      objsec.Data.seek(objreloc.DataOffset);
                      WriteUleb5(objsec.Data,objsym.LinkingData.ExeFunctionIndex);
                    end;
                  RELOC_ABSOLUTE:
                    begin
                      case objsym.typ of
                        AT_FUNCTION:
                          begin
                            if objreloc.IsFunctionOffsetI32 then
                              begin
                                { R_WASM_FUNCTION_OFFSET_I32 }
                                objsec.Data.seek(objreloc.DataOffset);
                                writeUInt32LE(UInt32(objsym.objsection.MemPos+objreloc.Addend));
                              end
                            else
                              begin
                                { R_WASM_TABLE_INDEX_I32 }
                                if objsym.LinkingData.ExeFunctionIndex=-1 then
                                  internalerror(2024010103);
                                if objsym.LinkingData.ExeIndirectFunctionTableIndex=-1 then
                                  objsym.LinkingData.ExeIndirectFunctionTableIndex:=AddOrGetIndirectFunctionTableIndex(objsym.LinkingData.ExeFunctionIndex);
                                objsec.Data.seek(objreloc.DataOffset);
                                writeUInt32LE(UInt32(objsym.LinkingData.ExeIndirectFunctionTableIndex));
                              end;
                          end;
                        AT_DATA:
                          begin
                            if objreloc.IsFunctionOffsetI32 then
                              internalerror(2024010602);
                            objsec.Data.seek(objreloc.DataOffset);
                            writeUInt32LE(UInt32((objsym.offset+objsym.objsection.MemPos)+objreloc.Addend));
                          end;
                        AT_TLS:
                          begin
                            if objreloc.IsFunctionOffsetI32 then
                              internalerror(2024010602);
                            objsec.Data.seek(objreloc.DataOffset);
                            writeUInt32LE(UInt32((objsym.offset+objsym.objsection.MemPos-objsym.objsection.ExeSection.MemPos)+objreloc.Addend));
                          end;
                        AT_WASM_GLOBAL:
                          begin
                            if objreloc.IsFunctionOffsetI32 then
                              internalerror(2024010602);
                            objsec.Data.seek(objreloc.DataOffset);
                            writeUInt32LE(UInt32(objsym.offset+objsym.objsection.MemPos));
                          end;
                        else
                          internalerror(2024010108);
                      end;
                    end;
                  RELOC_MEMORY_ADDR_LEB:
                    begin
                      if objsym.typ<>AT_DATA then
                        internalerror(2024010109);
                      objsec.Data.seek(objreloc.DataOffset);
                      WriteUleb5(objsec.Data,UInt32((objsym.offset+objsym.objsection.MemPos)+objreloc.Addend));
                    end;
                  RELOC_MEMORY_ADDR_OR_TABLE_INDEX_SLEB:
                    begin
                      case objsym.typ of
                        AT_FUNCTION:
                          begin
                            if objsym.LinkingData.ExeFunctionIndex=-1 then
                              internalerror(2024010103);
                            if objsym.LinkingData.ExeIndirectFunctionTableIndex=-1 then
                              objsym.LinkingData.ExeIndirectFunctionTableIndex:=AddOrGetIndirectFunctionTableIndex(objsym.LinkingData.ExeFunctionIndex);
                            objsec.Data.seek(objreloc.DataOffset);
                            WriteSleb5(objsec.Data,Int32(objsym.LinkingData.ExeIndirectFunctionTableIndex));
                          end;
                        AT_DATA:
                          begin
                            objsec.Data.seek(objreloc.DataOffset);
                            WriteSleb5(objsec.Data,Int32((objsym.offset+objsym.objsection.MemPos)+objreloc.Addend));
                          end;
                        else
                          internalerror(2024010110);
                      end;
                    end;
                  RELOC_GLOBAL_INDEX_LEB:
                    if objsym.typ=AT_WASM_GLOBAL then
                      begin
                        objsec.Data.seek(objreloc.DataOffset);
                        WriteUleb5(objsec.Data,UInt32(objsym.offset+objsym.objsection.MemPos));
                      end
                    else if (ts_wasm_threads in current_settings.targetswitches) and
                            (objsym.typ=AT_TLS) then
                      begin
                        { Nothing to do here. A second RELOC_GLOBAL_INDEX_LEB
                          relocation, overlaid on top of this one, pointing to
                          an AT_WASM_GLOBAL should have already done the job. }
                      end
                    else
                      internalerror(2024010111);
                  RELOC_TAG_INDEX_LEB:
                    begin
                      if objsym.typ<>AT_WASM_EXCEPTION_TAG then
                        internalerror(2024010708);
                      objsec.Data.seek(objreloc.DataOffset);
                      WriteUleb5(objsec.Data,UInt32(objsym.offset+objsym.objsection.MemPos));
                    end;
                  else
                    internalerror(2024010109);
                end;
              end
            else if assigned(objreloc.objsection) then
              begin
                if objreloc.typ<>RELOC_ABSOLUTE then
                  internalerror(2024010601);
                objsec.Data.seek(objreloc.DataOffset);
                writeUInt32LE(UInt32((objreloc.objsection.MemPos)+objreloc.Addend));
              end
            else if objreloc.typ=RELOC_TYPE_INDEX_LEB then
              begin
                objreloc.ExeTypeIndex:=FFuncTypes.AddOrGetFuncType(objreloc.FuncType);
                objsec.Data.seek(objreloc.DataOffset);
                WriteUleb5(objsec.Data,objreloc.ExeTypeIndex);
              end
            else
              internalerror(2024010110);
          end;
      end;

    constructor TWasmExeOutput.create;
      var
        i: TWasmSectionID;
        j: TWasmCustomSectionType;
        k: TWasmNameSubsectionType;
      begin
        inherited create;
        CObjData:=TWasmObjData;
        SectionMemAlign:=16;
        MaxMemPos:=$FFFFFFFF;
        FFuncTypes:=TWasmFuncTypeTable.Create;
        for i in TWasmSectionID do
          FWasmSections[i] := tdynamicarray.create(SectionDataMaxGrow);
        for j in TWasmCustomSectionType do
          FWasmCustomSections[j] := tdynamicarray.create(SectionDataMaxGrow);
        for k:=low(FWasmNameSubsections) to high(FWasmNameSubsections) do
          FWasmNameSubsections[k] := tdynamicarray.create(SectionDataMaxGrow);
        SetLength(FIndirectFunctionTable,1);
        FIndirectFunctionTable[0].FuncIdx:=-1;
      end;

    destructor TWasmExeOutput.destroy;
      var
        i: TWasmSectionID;
        j: TWasmCustomSectionType;
        k: TWasmNameSubsectionType;
      begin
        for i in TWasmSectionID do
          FWasmSections[i].Free;
        for j in TWasmCustomSectionType do
          FWasmCustomSections[j].Free;
        for k:=low(FWasmNameSubsections) to high(FWasmNameSubsections) do
          FWasmNameSubsections[k].Free;
        FFuncTypes.Free;
        inherited destroy;
      end;

    procedure TWasmExeOutput.GenerateLibraryImports(ImportLibraryList: TFPHashObjectList);
      var
        i, j: Integer;
        ImportLibrary: TImportLibrary;
        ImportSymbol: TImportSymbol;
        exesym: TExeSymbol;
      begin
        { Here map import symbols to exe symbols and create necessary sections.
          Actual import generation is done after unused sections (and symbols) are removed. }
        FImports:=ImportLibraryList;
        for i:=0 to ImportLibraryList.Count-1 do
          begin
            ImportLibrary:=TImportLibrary(ImportLibraryList[i]);
            for j:=0 to ImportLibrary.ImportSymbolList.Count-1 do
              begin
                ImportSymbol:=TImportSymbol(ImportLibrary.ImportSymbolList[j]);
                exesym:=TExeSymbol(ExeSymbolList.Find(ImportSymbol.MangledName));
                if assigned(exesym) and
                   (exesym.State<>symstate_defined) then
                  begin
                    ImportSymbol.CachedExeSymbol:=exesym;
                    exesym.State:=symstate_defined;
                  end;
              end;
          end;
        PackUnresolvedExeSymbols('after module imports');
      end;

    procedure TWasmExeOutput.AfterUnusedSectionRemoval;
      begin
        PrepareImports;
        PrepareFunctions;
        PrepareTags;
      end;

    procedure TWasmExeOutput.MemPos_ExeSection(const aname: string);
      const
        DebugPrefix = '.debug_';
      var
        ExeSec: TExeSection;
        i: Integer;
        objsec: TObjSection;
        firstdatasec: string;
      begin
        { WebAssembly is a Harvard architecture.
          Data lives in a separate address space, so start addressing back from 0
          (the LLVM leaves the first 1024 bytes in the data segment empty, so we
          start at 1024). }
        if ts_wasm_threads in current_settings.targetswitches then
          firstdatasec:='.tbss'
        else
          firstdatasec:='.rodata';
        if aname=firstdatasec then
          begin
            CurrMemPos:=1024;
            inherited;
          end
        else if aname='.text' then
          begin
            CurrMemPos:=0;
            ExeSec:=FindExeSection(aname);
            if not assigned(ExeSec) then
              exit;
            exesec.MemPos:=CurrMemPos;

            CurrMemPos:=CurrMemPos+UlebEncodingSize(exesec.ObjSectionList.Count);

            { set position of object ObjSections }
            for i:=0 to exesec.ObjSectionList.Count-1 do
              begin
                objsec:=TObjSection(exesec.ObjSectionList[i]);
                CurrMemPos:=CurrMemPos+UlebEncodingSize(objsec.Size);
                CurrMemPos:=objsec.setmempos(CurrMemPos);
              end;

            { calculate size of the section }
            exesec.Size:=CurrMemPos-exesec.MemPos;
          end
        else if (aname='.wasm_globals') or (aname='.wasm_tags') or
                (Copy(aname,1,Length(DebugPrefix))=DebugPrefix) then
          begin
            CurrMemPos:=0;
            inherited;
          end
        else
          inherited;
      end;

    procedure TWasmExeOutput.Load_Symbol(const aname: string);
      begin
        if aname=StackPointerSymStr then
          begin
            internalObjData.createsection('*'+aname,1,[oso_Data,oso_load]);
            FStackPointerSym:=TWasmObjSymbol(internalObjData.SymbolDefine(aname,AB_GLOBAL,AT_WASM_GLOBAL));
            FStackPointerSym.size:=1;
            FStackPointerSym.ObjSection.WriteZeros(1);
            TWasmObjSection(FStackPointerSym.ObjSection).MainFuncSymbol:=FStackPointerSym;
            FStackPointerSym.LinkingData.GlobalType:=wbt_i32;
            FStackPointerSym.LinkingData.GlobalIsMutable:=True;
            FStackPointerSym.LinkingData.GlobalInitializer.typ:=wbt_i32;
            FStackPointerSym.LinkingData.GlobalInitializer.init_i32:=0;
          end
        else if (ts_wasm_threads in current_settings.targetswitches) and (aname='__tls_base') then
          begin
            internalObjData.createsection('*'+aname,1,[oso_Data,oso_load]);
            FTlsBaseSym:=TWasmObjSymbol(internalObjData.SymbolDefine(aname,AB_GLOBAL,AT_WASM_GLOBAL));
            FTlsBaseSym.size:=1;
            FTlsBaseSym.ObjSection.WriteZeros(1);
            TWasmObjSection(FTlsBaseSym.ObjSection).MainFuncSymbol:=FTlsBaseSym;
            FTlsBaseSym.LinkingData.GlobalType:=wbt_i32;
            FTlsBaseSym.LinkingData.GlobalIsMutable:=True;
            FTlsBaseSym.LinkingData.GlobalInitializer.typ:=wbt_i32;
            FTlsBaseSym.LinkingData.GlobalInitializer.init_i32:=0;
          end
        else if (ts_wasm_threads in current_settings.targetswitches) and (aname='__tls_size') then
          begin
            internalObjData.createsection('*'+aname,1,[oso_Data,oso_load]);
            FTlsSizeSym:=TWasmObjSymbol(internalObjData.SymbolDefine(aname,AB_GLOBAL,AT_WASM_GLOBAL));
            FTlsSizeSym.size:=1;
            FTlsSizeSym.ObjSection.WriteZeros(1);
            TWasmObjSection(FTlsSizeSym.ObjSection).MainFuncSymbol:=FTlsSizeSym;
            FTlsSizeSym.LinkingData.GlobalType:=wbt_i32;
            FTlsSizeSym.LinkingData.GlobalIsMutable:=False;
            FTlsSizeSym.LinkingData.GlobalInitializer.typ:=wbt_i32;
            FTlsSizeSym.LinkingData.GlobalInitializer.init_i32:=0;
          end
        else if (ts_wasm_threads in current_settings.targetswitches) and (aname='__tls_align') then
          begin
            internalObjData.createsection('*'+aname,1,[oso_Data,oso_load]);
            FTlsAlignSym:=TWasmObjSymbol(internalObjData.SymbolDefine(aname,AB_GLOBAL,AT_WASM_GLOBAL));
            FTlsAlignSym.size:=1;
            FTlsAlignSym.ObjSection.WriteZeros(1);
            TWasmObjSection(FTlsAlignSym.ObjSection).MainFuncSymbol:=FTlsAlignSym;
            FTlsAlignSym.LinkingData.GlobalType:=wbt_i32;
            FTlsAlignSym.LinkingData.GlobalIsMutable:=False;
            FTlsAlignSym.LinkingData.GlobalInitializer.typ:=wbt_i32;
            FTlsAlignSym.LinkingData.GlobalInitializer.init_i32:=0;
          end
        else if (ts_wasm_threads in current_settings.targetswitches) and (aname='__wasm_init_tls') then
          begin
            internalObjData.createsection('*'+aname,0,[]);
            FInitTlsFunctionSym:=TWasmObjSymbol(internalObjData.SymbolDefine(aname,AB_GLOBAL,AT_FUNCTION));
            TWasmObjSection(FInitTlsFunctionSym.ObjSection).MainFuncSymbol:=FInitTlsFunctionSym;
            FInitTlsFunctionSym.LinkingData.FuncType:=TWasmFuncType.Create([wbt_i32],[]);
          end
        else if (ts_wasm_threads in current_settings.targetswitches) and (aname='__fpc_wasm_init_shared_memory') then
          begin
            internalObjData.createsection('*'+aname,0,[]);
            FInitSharedMemoryFunctionSym:=TWasmObjSymbol(internalObjData.SymbolDefine(aname,AB_GLOBAL,AT_FUNCTION));
            TWasmObjSection(FInitSharedMemoryFunctionSym.ObjSection).MainFuncSymbol:=FInitSharedMemoryFunctionSym;
            FInitSharedMemoryFunctionSym.ObjSection.SecOptions:=FInitSharedMemoryFunctionSym.ObjSection.SecOptions+[oso_keep];
            FInitSharedMemoryFunctionSym.LinkingData.FuncType:=TWasmFuncType.Create([],[]);
          end
        else
          inherited;
      end;

    procedure TWasmExeOutput.PrepareImports;

      function AddFunctionImport(const libname,symname:TCmdStr; functype: TWasmFuncType): Integer;
        begin
          SetLength(FFunctionImports,Length(FFunctionImports)+1);
          Result:=High(FFunctionImports);
          if assigned(exemap) then
            exemap.Add('  Importing Function[' + tostr(Result) + '] ' + symname + functype.ToString);
          with FFunctionImports[Result] do
            begin
              ModName:=libname;
              Name:=symname;
              TypeIdx:=FFuncTypes.AddOrGetFuncType(functype);
            end;
        end;

      var
        i, j: Integer;
        ImportLibrary: TImportLibrary;
        ImportSymbol: TImportSymbol;
        exesym: TExeSymbol;
        newdll: Boolean;
        fsym: TWasmObjSymbol;
        objdata: TObjData;
      begin
        for i:=0 to FImports.Count-1 do
          begin
            ImportLibrary:=TImportLibrary(FImports[i]);
            newdll:=False;
            for j:=0 to ImportLibrary.ImportSymbolList.Count-1 do
              begin
                ImportSymbol:=TImportSymbol(ImportLibrary.ImportSymbolList[j]);
                exesym:=ImportSymbol.CachedExeSymbol;
                if assigned(exesym) and
                   exesym.Used then
                  begin
                    if (not newdll) and assigned(exemap) then
                      begin
                        exemap.Add('');
                        exemap.Add('Importing from module '+ImportLibrary.Name);
                      end;
                    newdll:=True;
                    TWasmObjSymbol(exesym.ObjSymbol).LinkingData.ExeFunctionIndex:=
                      AddFunctionImport(ImportLibrary.Name,ImportSymbol.Name,TWasmObjSymbol(exesym.ObjSymbol).LinkingData.FuncType);
                    AddToFunctionNameMap(TWasmObjSymbol(exesym.ObjSymbol).LinkingData.ExeFunctionIndex,ImportSymbol.MangledName);
                  end;
              end;
          end;

        { set ExeFunctionIndex to the alias symbols as well }
        for i:=0 to ObjDataList.Count-1 do
          begin
            objdata:=TObjData(ObjDataList[i]);
            for j:=0 to objdata.ObjSymbolList.Count-1 do
              begin
                fsym:=TWasmObjSymbol(objdata.ObjSymbolList[j]);
                if (fsym.LinkingData.ExeFunctionIndex=-1) and assigned(fsym.exesymbol) and (TWasmObjSymbol(fsym.exesymbol.ObjSymbol).LinkingData.ExeFunctionIndex<>-1) then
                  fsym.LinkingData.ExeFunctionIndex:=TWasmObjSymbol(fsym.exesymbol.ObjSymbol).LinkingData.ExeFunctionIndex;
              end;
          end;
      end;

    procedure TWasmExeOutput.PrepareFunctions;
      var
        i, j: Integer;
        exesec: TExeSection;
        objsec: TWasmObjSection;
        fsym: TWasmObjSymbol;
        objdata: TObjData;
      begin
        if assigned(exemap) then
          begin
            exemap.Add('');
            exemap.Add('Functions, defined in this module:');
          end;
        exesec:=FindExeSection('.text');
        if not assigned(exesec) then
          internalerror(2023123106);
        for i:=0 to exesec.ObjSectionList.Count-1 do
          begin
            objsec:=TWasmObjSection(exesec.ObjSectionList[i]);
            fsym:=objsec.MainFuncSymbol;
            if not assigned(fsym) then
              internalerror(2023123107);
            if not assigned(fsym.LinkingData.FuncType) then
              internalerror(2023123108);
            if fsym.LinkingData.ExeFunctionIndex<>-1 then
              internalerror(2023123109);
            if fsym.LinkingData.ExeTypeIndex<>-1 then
              internalerror(2023123109);
            fsym.LinkingData.ExeTypeIndex:=FFuncTypes.AddOrGetFuncType(fsym.LinkingData.FuncType);
            fsym.LinkingData.ExeFunctionIndex:=i+Length(FFunctionImports);
            if assigned(exemap) then
              begin
                exemap.Add('  Function[' + tostr(fsym.LinkingData.ExeFunctionIndex) + '] ' + fsym.Name + fsym.LinkingData.FuncType.ToString);
              end;
            AddToFunctionNameMap(fsym.LinkingData.ExeFunctionIndex,fsym.Name);
          end;
        { set ExeFunctionIndex to the alias symbols as well }
        for i:=0 to ObjDataList.Count-1 do
          begin
            objdata:=TObjData(ObjDataList[i]);
            for j:=0 to objdata.ObjSymbolList.Count-1 do
              begin
                fsym:=TWasmObjSymbol(objdata.ObjSymbolList[j]);
                if assigned(fsym.objsection) and fsym.objsection.USed and (fsym.typ=AT_FUNCTION) and (fsym.LinkingData.ExeFunctionIndex=-1) then
                  begin
                    fsym.LinkingData.ExeFunctionIndex:=TWasmObjSection(fsym.objsection).MainFuncSymbol.LinkingData.ExeFunctionIndex;
                    if fsym.LinkingData.ExeFunctionIndex=-1 then
                      internalerror(2024010102);
                  end;
              end;
          end;
      end;

    procedure TWasmExeOutput.PrepareTags;
      var
        exesec: TExeSection;
        i, j: Integer;
        objsec: TWasmObjSection;
        fsym: TWasmObjSymbol;
        objdata: TObjData;
      begin
        exesec:=FindExeSection('.wasm_tags');
        if not assigned(exesec) then
          exit;
        if assigned(exemap) then
          begin
            exemap.Add('');
            exemap.Add('Tags, defined in this module:');
          end;
        for i:=0 to exesec.ObjSectionList.Count-1 do
          begin
            objsec:=TWasmObjSection(exesec.ObjSectionList[i]);
            fsym:=objsec.MainFuncSymbol;
            if not assigned(fsym) then
              internalerror(2024010703);
            if not assigned(fsym.LinkingData.FuncType) then
              internalerror(2024010704);
            if fsym.LinkingData.ExeTagIndex<>-1 then
              internalerror(2024010705);
            if fsym.LinkingData.ExeTypeIndex<>-1 then
              internalerror(2024010706);
            fsym.LinkingData.ExeTypeIndex:=FFuncTypes.AddOrGetFuncType(fsym.LinkingData.FuncType);
            fsym.LinkingData.ExeTagIndex:=i+Length(FTagImports);
            if assigned(exemap) then
              begin
                exemap.Add('  Tag[' + tostr(fsym.LinkingData.ExeTagIndex) + '] ' + fsym.Name + fsym.LinkingData.FuncType.ToString);
              end;
          end;
        { set ExeTagIndex to the alias symbols as well }
        for i:=0 to ObjDataList.Count-1 do
          begin
            objdata:=TObjData(ObjDataList[i]);
            for j:=0 to objdata.ObjSymbolList.Count-1 do
              begin
                fsym:=TWasmObjSymbol(objdata.ObjSymbolList[j]);
                if assigned(fsym.objsection) and fsym.objsection.USed and (fsym.typ=AT_WASM_EXCEPTION_TAG) and (fsym.LinkingData.ExeTagIndex=-1) then
                  begin
                    fsym.LinkingData.ExeTagIndex:=TWasmObjSection(fsym.objsection).MainFuncSymbol.LinkingData.ExeTagIndex;
                    if fsym.LinkingData.ExeTagIndex=-1 then
                      internalerror(2024010707);
                  end;
              end;
          end;
      end;

    function TWasmExeOutput.AddOrGetIndirectFunctionTableIndex(FuncIdx: Integer): integer;
      var
        i: Integer;
      begin
        for i:=1 to length(FIndirectFunctionTable)-1 do
          if FIndirectFunctionTable[i].FuncIdx=FuncIdx then
            begin
              Result:=i;
              exit;
            end;
        SetLength(FIndirectFunctionTable,Length(FIndirectFunctionTable)+1);
        Result:=High(FIndirectFunctionTable);
        FIndirectFunctionTable[Result].FuncIdx:=FuncIdx;
      end;

    procedure TWasmExeOutput.SetStackPointer;
      var
        BssSec: TExeSection;
        StackStart, InitialStackPtrAddr: QWord;
      begin
        BssSec:=FindExeSection('.bss');
        InitialStackPtrAddr := (BssSec.MemPos+BssSec.Size+stacksize+15) and (not 15);
        FMinMemoryPages := Max(
          QWord(Align(QWord(InitialStackPtrAddr),QWord(WasmPageSize)) div WasmPageSize),
          QWord(Align(QWord(heapsize),QWord(WasmPageSize)) div WasmPageSize));
        FStackPointerSym.LinkingData.GlobalInitializer.init_i32:=Int32(InitialStackPtrAddr);
      end;

    procedure TWasmExeOutput.SetTlsSizeAlignAndBase;
      var
        TBssSec: TExeSection;
      begin
        if not (ts_wasm_threads in current_settings.targetswitches) then
          exit;
        TBssSec:=FindExeSection('.tbss');
        FTlsSizeSym.LinkingData.GlobalInitializer.init_i32:=Int32(TBssSec.Size);
        FTlsAlignSym.LinkingData.GlobalInitializer.init_i32:=Int32(TBssSec.SecAlign);
        FTlsBaseSym.LinkingData.GlobalInitializer.init_i32:=Int32(TBssSec.MemPos);
      end;

    procedure TWasmExeOutput.SetThreadVarGlobalsInitValues;
      var
        exesec: TExeSection;
        i: Integer;
        objsec: TWasmObjSection;
        objsym: TWasmObjSymbol;
      begin
        if not (ts_wasm_threads in current_settings.targetswitches) then
          exit;
        exesec:=FindExeSection('.wasm_globals');
        if not assigned(exesec) then
          internalerror(2024010112);
        for i:=0 to exesec.ObjSectionList.Count-1 do
          begin
            objsec:=TWasmObjSection(exesec.ObjSectionList[i]);
            objsym:=objsec.MainFuncSymbol;
            if Assigned(objsym.TlsDataSym) then
              begin
                objsym.LinkingData.GlobalInitializer.typ:=wbt_i32;
                objsym.LinkingData.GlobalInitializer.init_i32:=objsym.TlsDataSym.offset+objsym.TlsDataSym.objsection.MemPos;
              end;
          end;
      end;

    procedure TWasmExeOutput.GenerateCode_InitTls;
      var
        Sec: TObjSection;
        globalexesec: TExeSection;
        i: Integer;
        globalobjsec: TWasmObjSection;
        globalobjsym: TWasmObjSymbol;
        OffsetInTls: QWord;
      begin
        if not (ts_wasm_threads in current_settings.targetswitches) then
          exit;

        globalexesec:=FindExeSection('.wasm_globals');
        if not assigned(globalexesec) then
          internalerror(2024010112);

        Sec:=FInitTlsFunctionSym.objsection;
        Sec.SecOptions:=Sec.SecOptions+[oso_Data];

        { locals }
        Sec.writeUInt8($00);

        { local.get 0 }
        Sec.writeUInt16BE($2000);
        { global.set $__tls_base }
        Sec.writeUInt8($24);
        WriteUleb(sec,FTlsBaseSym.offset+FTlsBaseSym.objsection.MemPos);

        for i:=0 to globalexesec.ObjSectionList.Count-1 do
          begin
            globalobjsec:=TWasmObjSection(globalexesec.ObjSectionList[i]);
            globalobjsym:=globalobjsec.MainFuncSymbol;
            if Assigned(globalobjsym.TlsDataSym) then
              begin
                OffsetInTls:=globalobjsym.TlsDataSym.offset+globalobjsym.TlsDataSym.objsection.MemPos-globalobjsym.TlsDataSym.objsection.ExeSection.MemPos;
                { local.get 0 }
                Sec.writeUInt16BE($2000);
                if OffsetInTls<>0 then
                  begin
                    { i32.const $OffsetInTls }
                    Sec.writeUInt8($41);
                    WriteSleb(Sec,Int32(OffsetInTls));
                    { i32.add }
                    Sec.writeUInt8($6A);
                  end;
                { global.set y }
                Sec.writeUInt8($24);
                WriteUleb(sec,globalobjsym.offset+globalobjsym.objsection.MemPos);
              end;
          end;

        Sec.writeUInt8($0B);  { end }
      end;

    procedure TWasmExeOutput.GenerateCode_InitSharedMemory;
      const
        InitFlagOfs=256;
      var
        Sec: TObjSection;
        DataSecName: string;
        DataSecIdx: Integer;
        ExeSec: TExeSection;
      begin
        if not (ts_wasm_threads in current_settings.targetswitches) then
          exit;
        Sec:=FInitSharedMemoryFunctionSym.objsection;
        Sec.SecOptions:=Sec.SecOptions+[oso_Data];

        { locals }
        Sec.writeUInt8($00);

        { block }
        Sec.writeUInt16BE($0240);
        { block }
        Sec.writeUInt16BE($0240);
        { block }
        Sec.writeUInt16BE($0240);

        { i32.const $InitFlag }
        Sec.writeUInt8($41);
        WriteSleb(sec,InitFlagOfs);
        { i32.const 0 }
        Sec.writeUInt16BE($4100);
        { i32.const 1 }
        Sec.writeUInt16BE($4101);
        { i32.atomic.rmw.cmpxchg 2 0 }
        Sec.writeUInt32BE($fe480200);
        { br_table 0 1 2 }
        Sec.writebytes(#$0e#$02#$00#$01#$02);
        { end }
        Sec.writeUInt8($0B);

        DataSecIdx:=-1;
        for DataSecName in DataSections do
          begin
            ExeSec:=FindExeSection(DataSecName);
            if Assigned(ExeSec) and (ExeSec.Size>0) then
              begin
                Inc(DataSecIdx);
                { i32.const $memPos }
                Sec.writeUInt8($41);
                WriteSleb(sec,Int32(ExeSec.MemPos));
                { i32.const 0 }
                Sec.writeUInt16BE($4100);
                { i32.const size }
                Sec.writeUInt8($41);
                WriteSleb(sec,Int32(ExeSec.Size));
                { memory.init $DataSecIdx 0 }
                Sec.writeUInt16BE($fc08);
                WriteUleb(sec,DataSecIdx);
                Sec.writeUInt8(0);
              end;
          end;

        { i32.const $InitFlag }
        Sec.writeUInt8($41);
        WriteSleb(sec,InitFlagOfs);
        { i32.const 2 }
        Sec.writeUInt16BE($4102);
        { i32.atomic.store 2 0 }
        Sec.writeUInt32BE($fe170200);
        { i32.const $InitFlag }
        Sec.writeUInt8($41);
        WriteSleb(sec,InitFlagOfs);
        { i32.const 4294967295 }
        Sec.writeUInt16BE($417f);
        { memory.atomic.notify 2 0 }
        Sec.writeUInt32BE($fe000200);
        { drop }
        Sec.writeUInt8($1A);
        { br 1 }
        Sec.writeUInt16BE($0C01);
        { end }
        Sec.writeUInt8($0B);
        { i32.const $InitFlag }
        Sec.writeUInt8($41);
        WriteSleb(sec,InitFlagOfs);
        { i32.const 1 }
        Sec.writeUInt16BE($4101);
        { i64.const -1 }
        Sec.writeUInt16BE($427f);
        { memory.atomic.wait32 2 0 }
        Sec.writeUInt32BE($fe010200);
        { drop }
        Sec.writeUInt8($1A);
        { end }
        Sec.writeUInt8($0B);
        DataSecIdx:=-1;
        for DataSecName in DataSections do
          begin
            ExeSec:=FindExeSection(DataSecName);
            if Assigned(ExeSec) and (ExeSec.Size>0) then
              begin
                Inc(DataSecIdx);
                { data.drop $DataSecIdx }
                Sec.writeUInt16BE($fc09);
                WriteUleb(sec,DataSecIdx);
              end;
          end;
        { end }
        Sec.writeUInt8($0B);
      end;

    procedure TWasmExeOutput.GenerateCode_InvokeHelper;
      var
        Sec: TObjSection;
        IndirectFunctionTableMap: array of Integer;

      procedure InvokeFuncType(typidx: Integer);
        var
          ft: TWasmFuncType;
          i, nextofs: Integer;
        begin
          ft:=FFuncTypes[typidx];
          for i:=0 to Length(ft.results)-1 do
            { local.get 2 }
            Sec.writeUInt16BE($2002);
          nextofs:=0;
          for i:=0 to Length(ft.params)-1 do
            begin
              { local.get 1 }
              Sec.writeUInt16BE($2001);
              case ft.params[i] of
                wbt_i32:
                  begin
                    { i32.load nextofs }
                    Sec.writeUInt16BE($2802);
                    WriteUleb(Sec, nextofs);
                    Inc(nextofs,4);
                  end;
                wbt_i64:
                  begin
                    { i64.load nextofs }
                    Sec.writeUInt16BE($2902);
                    WriteUleb(Sec, nextofs);
                    Inc(nextofs,8);
                  end;
                wbt_f32:
                  begin
                    { f32.load nextofs }
                    Sec.writeUInt16BE($2A02);
                    WriteUleb(Sec, nextofs);
                    Inc(nextofs,4);
                  end;
                wbt_f64:
                  begin
                    { f64.load nextofs }
                    Sec.writeUInt16BE($2B02);
                    WriteUleb(Sec, nextofs);
                    Inc(nextofs,8);
                  end;
                wbt_v128:
                  begin
                    { v128.load nextofs }
                    Sec.writeUInt16BE($FD00);
                    Sec.writeUInt8($02);  { align: 4 bytes }
                    WriteUleb(Sec, nextofs);
                    Inc(nextofs,16);
                  end;
                wbt_externref,
                wbt_funcref:
                  begin
                    { unreachable }
                    Sec.writeUInt8($00);
                  end;
                else
                  internalerror(2025012501);
              end;
            end;
          { local.get 0 }
          Sec.writeUInt16BE($2000);
          { call_indirect }
          Sec.writeUInt8($11);
          WriteUleb(Sec,typidx);
          Sec.writeUInt8($0);  { table index 0 }
          nextofs:=0;
          for i:=0 to Length(ft.results)-1 do
            begin
              case ft.results[i] of
                wbt_i32:
                  begin
                    { i32.store nextofs }
                    Sec.writeUInt16BE($3602);
                    WriteUleb(Sec, nextofs);
                    Inc(nextofs,4);
                  end;
                wbt_i64:
                  begin
                    { i64.store nextofs }
                    Sec.writeUInt16BE($3702);
                    WriteUleb(Sec, nextofs);
                    Inc(nextofs,8);
                  end;
                wbt_f32:
                  begin
                    { f32.store nextofs }
                    Sec.writeUInt16BE($3802);
                    WriteUleb(Sec, nextofs);
                    Inc(nextofs,4);
                  end;
                wbt_f64:
                  begin
                    { f64.store nextofs }
                    Sec.writeUInt16BE($3902);
                    WriteUleb(Sec, nextofs);
                    Inc(nextofs,8);
                  end;
                wbt_v128:
                  begin
                    { v128.store nextofs }
                    Sec.writeUInt16BE($FD0B);
                    Sec.writeUInt8($02);  { align: 4 bytes }
                    WriteUleb(Sec, nextofs);
                    Inc(nextofs,16);
                  end;
                wbt_externref,
                wbt_funcref:
                  begin
                    { unreachable }
                    Sec.writeUInt8($00);
                  end;
                else
                  internalerror(2025012501);
              end;
            end;
          { return }
          Sec.writeUInt8($0F);
        end;

      function FuncIdx2TypeIdx(fi: Integer): Integer;
        var
          exesec: TExeSection;
          objsec: TWasmObjSection;
          fsym: TWasmObjSymbol;
        begin
          if fi<Length(FFunctionImports) then
            Result:=FFunctionImports[fi].TypeIdx
          else
            begin
              exesec:=FindExeSection('.text');
              if not assigned(exesec) then
                internalerror(2023123106);
              objsec:=TWasmObjSection(exesec.ObjSectionList[fi-Length(FFunctionImports)]);
              fsym:=objsec.MainFuncSymbol;
              Result:=fsym.LinkingData.ExeTypeIndex;
            end;
        end;

      procedure WriteBrTable(l,h: Integer; addend: Integer=0);
        const
          { max len of br_table instruction }
          MaxLen=1000;
        var
          i, len, m: Integer;
        begin
          { local.get 0 }
          Sec.writeUInt16BE($2000);
          len:=h-l+1;
          if len<=MaxLen then
            begin
              if l>0 then
                begin
                  { i32.const l }
                  Sec.writeUInt8($41);
                  WriteSleb(sec,l);
                  { i32.sub }
                  Sec.writeUInt8($6B);
                end;
              { br_table }
              Sec.writeUInt8($0E);
              if h=high(IndirectFunctionTableMap) then
                begin
                  WriteUleb(Sec,len);
                  for i:=l to h do
                    WriteUleb(Sec,IndirectFunctionTableMap[i]+addend);
                  WriteUleb(Sec,addend);
                end
              else
                begin
                  WriteUleb(Sec,len-1);
                  for i:=l to h do
                    WriteUleb(Sec,IndirectFunctionTableMap[i]+addend);
                end;
            end
          else
            begin
              m:=(l+h) div 2;
              { i32.const m }
              Sec.writeUInt8($41);
              WriteSleb(sec,m);
              { i32.lt_u }
              Sec.writeUInt8($49);
              { if }
              Sec.writeUInt16BE($0440);
              WriteBrTable(l,m-1,addend+1);
              { else }
              Sec.writeUInt8($05);
              WriteBrTable(m,h,addend+1);
              { end }
              Sec.writeUInt8($0B);
            end;
        end;

      var
        exesym: TExeSymbol;
        objsym: TObjSymbol;
        i, j, TypIdx: Integer;
        InvokableTypeIndices: array of Integer;
      begin
        exesym:=TExeSymbol(ExeSymbolList.Find('fpc_wasm_invoke_helper'));
        if not Assigned(exesym) then
          exit;

        SetLength(IndirectFunctionTableMap, Length(FIndirectFunctionTable));
        SetLength(InvokableTypeIndices, 1);
        InvokableTypeIndices[0] := -1;
        for i:=1 to Length(FIndirectFunctionTable)-1 do
          begin
            IndirectFunctionTableMap[i]:=0;
            TypIdx := FuncIdx2TypeIdx(FIndirectFunctionTable[i].FuncIdx);
            for j := 1 to Length(InvokableTypeIndices)-1 do
              if InvokableTypeIndices[j]=TypIdx then
                begin
                  IndirectFunctionTableMap[i]:=j;
                  break;
                end;
            if IndirectFunctionTableMap[i]=0 then
              begin
                SetLength(InvokableTypeIndices,Length(InvokableTypeIndices)+1);
                InvokableTypeIndices[High(InvokableTypeIndices)]:=TypIdx;
                IndirectFunctionTableMap[i]:=High(InvokableTypeIndices);
              end;
          end;

        objsym:=exesym.ObjSymbol;
        Sec:=objsym.objsection;
        Sec.Size:=0;
        Sec.Data.reset;

        { locals }
        Sec.writeUInt8($00);

        for i:=1 to Length(InvokableTypeIndices)-1 do
          { block }
          Sec.writeUInt16BE($0240);

        { block }
        Sec.writeUInt16BE($0240);
        { local.get 0 + br_table }
        WriteBrTable(low(IndirectFunctionTableMap),high(IndirectFunctionTableMap));
        { end }
        Sec.writeUInt8($0B);
        { unreachable }
        Sec.writeUInt8($00);

        for i:=1 to Length(InvokableTypeIndices)-1 do
          begin
            { end }
            Sec.writeUInt8($0B);
            InvokeFuncType(InvokableTypeIndices[i]);
          end;

        { end }
        Sec.writeUInt8($0B);
      end;

    procedure TWasmExeOutput.WriteExeSectionToDynArray(exesec: TExeSection; dynarr: tdynamicarray);
      var
        exesecdatapos: LongWord;
        i: Integer;
        objsec: TObjSection;
        dpos, pad: QWord;
      begin
        exesecdatapos:=dynarr.size;
        for i:=0 to exesec.ObjSectionList.Count-1 do
          begin
            objsec:=TObjSection(exesec.ObjSectionList[i]);
            if not (oso_data in objsec.secoptions) then
              internalerror(2024010104);
            if not assigned(objsec.data) then
              internalerror(2024010105);

            dpos:=objsec.MemPos-exesec.MemPos+exesecdatapos;
            pad:=dpos-dynarr.size;
            { objsection must be within SecAlign bytes from the previous one }
            if (dpos<dynarr.Size) or
              (pad>=max(objsec.SecAlign,1)) then
              internalerror(2024010106);
            writeZeros(dynarr,pad);

            objsec.data.seek(0);
            CopyDynamicArray(objsec.data,dynarr,objsec.data.size);
          end;
        if (dynarr.size-exesecdatapos)<>exesec.Size then
          internalerror(2024010107);
      end;


{****************************************************************************
                               TWasmAssembler
****************************************************************************}

    constructor TWasmAssembler.Create(info: pasminfo; smart:boolean);
      begin
        inherited;
        CObjOutput:=TWasmObjOutput;
      end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}
{$ifdef wasm32}
    const
       as_wasm32_wasm_info : tasminfo =
          (
            id     : as_wasm32_wasm;
            idtxt  : 'WASM';
            asmbin : '';
            asmcmd : '';
            supported_targets : [system_wasm32_embedded,system_wasm32_wasi];
            flags : [af_outputbinary,af_smartlink_sections];
            labelprefix : '..@';
            labelmaxlen : -1;
            comment : '; ';
            dollarsign: '$';
          );
{$endif wasm32}

initialization
{$ifdef wasm32}
  RegisterAssembler(as_wasm32_wasm_info,TWasmAssembler);
{$endif wasm32}
end.
