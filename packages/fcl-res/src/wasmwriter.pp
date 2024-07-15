{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi
    Copyright (c) 2024 by Nikolay Nikolov

    Resource writer for WebAssembly files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit wasmwriter;
{$ENDIF FPC_DOTTEDUNITS}

{$MODE OBJFPC} {$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Resources.Resource, System.Resources.Tree, System.Resources.StringTable.Types, System.Resources.Types, System.Resources.WebAssembly.Consts, System.Resources.WebAssembly.Types;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, resource, resourcetree, strtable, fpcrestypes, wasmconsts, wasmtypes;
{$ENDIF FPC_DOTTEDUNITS}

type

  { TWasmResourceWriter }

  TWasmResourceWriter = class(TAbstractResourceWriter)
  private
    fExtensions : string;
    fDescription : string;
    fRoot : TRootResTreeNode;
    fResStringTable : TResStringTable;
    fOppositeEndianess : boolean;
    fDataAlignment : longword;
    fDataCurOfs : longword;
    fCurOfs : longword;
    FWasmSections: array [TWasmSectionID] of TMemoryStream;
    FDataSegments: array [TWasmResourceDataSegment] of TMemoryStream;
    FDataSegmentFileSectionOfs: array [TWasmResourceDataSegment] of Int64;
    FDataRelocations: array of TWasmRelocationEntry;
    FWasmCustomSections: array [TWasmCustomSectionType] of TMemoryStream;
    FWasmLinkingSubsections: array [low(TWasmLinkingSubsectionType)..high(TWasmLinkingSubsectionType)] of TMemoryStream;
    FWasmSymbolTable: TMemoryStream;
    FWasmSymbolTableEntriesCount: Integer;
    procedure Align(aBound : integer; aStream : TStream);
    function NextAligned(aBound, aValue : longword) : longword;
    procedure PrescanResourceTree;
    function PrescanNode(aNode : TResourceTreeNode; aNodeSize : longword) : longword;
    procedure AddDataRelocation(aTyp: TWasmRelocationType; aOffset: UInt32; aIndex: UInt32; aAddend: Int32 = 0);
    procedure WriteRelocationDataTable(DataSectionIndex: Integer);
    procedure WriteResHeader(aResources : TResources);
    procedure WriteNodeInfos;
    procedure WriteNodeInfo(aNode : TResourceTreeNode);
    procedure WriteSubNodes(aNode : TResourceTreeNode);
    procedure WriteResStringTable;
    procedure WriteRawData;
    procedure WriteResData(aStream : TStream; aNode : TResourceTreeNode);
    procedure AddEmptySections(aResources : TResources);
    procedure WriteWasmSection(aStream: TStream; wsid: TWasmSectionID);
    procedure WriteWasmSectionIfNotEmpty(aStream: TStream; wsid: TWasmSectionID);
    procedure WriteWasmCustomSection(aStream: TStream; wcst: TWasmCustomSectionType);
    procedure WriteLinkingSubsection(wlst: TWasmLinkingSubsectionType);
    procedure WriteCustomSectionNames;
    procedure WriteImportSection;
    procedure WriteDataSegments;
    procedure WriteSymbolTable;
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Write(aResources : TResources; aStream : TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

procedure WriteSleb(aStream: TStream; v: int64);
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
    aStream.WriteByte(b);
  until Done;
end;

procedure WriteUleb(aStream: TStream; v: uint64);
var
  b: byte;
begin
  repeat
    b:=byte(v) and 127;
    v:=v shr 7;
    if v<>0 then
      b:=b or 128;
    aStream.WriteByte(b);
  until v=0;
end;

procedure WriteName(aStream: TStream; const s: string);
begin
  WriteUleb(aStream,Length(s));
  if Length(s)>0 then
    aStream.WriteBuffer(s[1],Length(s));
end;

{ TWasmResourceWriter }

procedure TWasmResourceWriter.Align(aBound: integer; aStream: TStream);
var topad,tmp : integer;
    qw : qword;
begin
  qw:=0;
  topad:=aBound-(aStream.Position mod aBound);
  if topad<>aBound then
    while topad>0 do
    begin
      if topad>8 then tmp:=8 else tmp:=topad;
      aStream.WriteBuffer(qw,tmp);
      dec(topad,tmp);
    end;
end;

function TWasmResourceWriter.NextAligned(aBound, aValue: longword): longword;
var topad : longword;
begin
  Result:=aValue;
  topad:=aBound-(aValue mod aBound);
  if topad<>aBound then inc(Result,topad);
end;

procedure TWasmResourceWriter.PrescanResourceTree;
begin
  fResStringTable.Clear;
  fRoot.SubDirRVA:=sizeof(TResHdr32)+sizeof(TResInfoNode32);
  fResStringTable.StartOfs:=PrescanNode(fRoot,sizeof(TResInfoNode32));
  if fResStringTable.Used then
    fDataCurOfs:=NextAligned(fDataAlignment,fResStringTable.StartOfs+fResStringTable.Size)
  else
    fDataCurOfs:=fResStringTable.StartOfs;
end;

function TWasmResourceWriter.PrescanNode(aNode: TResourceTreeNode;
  aNodeSize: longword): longword;
var curofs : longword;
    i : integer;
    subnode : TResourceTreeNode;
begin
  if aNode.IsLeaf then
  begin
    Result:=aNode.SubDirRVA;
    exit;
  end;

  if aNode.Desc.DescType=dtName then
    aNode.NameRVA:=fResStringTable.Add(aNode.Desc.Name);

  //first node subnodes begin at curofs (after all node headers)
  curofs:=aNode.SubDirRva+(aNode.NamedCount+aNode.IDCount)*aNodeSize;
  for i:=0 to aNode.NamedCount-1 do
  begin
    subnode:=aNode.NamedEntries[i];
    subnode.SubDirRVA:=curofs;
    curofs:=PrescanNode(subnode,aNodeSize);
  end;
  for i:=0 to aNode.IDCount-1 do
  begin
    subnode:=aNode.IDEntries[i];
    subnode.SubDirRVA:=curofs;
    curofs:=PrescanNode(subnode,aNodeSize);
  end;
  Result:=curofs;
end;

procedure TWasmResourceWriter.AddDataRelocation(aTyp: TWasmRelocationType;
  aOffset: UInt32; aIndex: UInt32; aAddend: Int32);
begin
  SetLength(FDataRelocations,Length(FDataRelocations)+1);
  with FDataRelocations[High(FDataRelocations)] do
  begin
    Typ:=aTyp;
    Offset:=aOffset;
    Index:=aIndex;
    Addend:=aAddend;
  end;
end;

procedure TWasmResourceWriter.WriteRelocationDataTable(DataSectionIndex: Integer);
var
  i: Integer;
  s: TWasmResourceDataSegment;
begin
  WriteUleb(FWasmCustomSections[wcstRelocData],DataSectionIndex);
  WriteUleb(FWasmCustomSections[wcstRelocData],Length(FDataRelocations));
  for i:=0 to Length(FDataRelocations)-1 do
    with FDataRelocations[i] do
    begin
      if Offset<FDataSegments[wrdsResources].Size then
        s:=wrdsResources
      else
        s:=wrdsResHandles;
      FWasmCustomSections[wcstRelocData].WriteByte(Ord(Typ));
      WriteUleb(FWasmCustomSections[wcstRelocData],Offset+FDataSegmentFileSectionOfs[s]);
      WriteUleb(FWasmCustomSections[wcstRelocData],Index);
      if Typ in [R_WASM_MEMORY_ADDR_I32,
                 R_WASM_MEMORY_ADDR_I64,
                 R_WASM_MEMORY_ADDR_LEB,
                 R_WASM_MEMORY_ADDR_LEB64,
                 R_WASM_MEMORY_ADDR_SLEB,
                 R_WASM_MEMORY_ADDR_SLEB64,
                 R_WASM_FUNCTION_OFFSET_I32,
                 R_WASM_SECTION_OFFSET_I32] then
        WriteSleb(FWasmCustomSections[wcstRelocData],Addend);
    end;
end;

procedure TWasmResourceWriter.WriteResHeader(aResources: TResources);
var hdr : TResHdr32;
begin
  hdr.count:=aResources.Count;
  hdr.usedhandles:=0;
  hdr.handles:=0;

  //fSymbolTable.AddSection(RSRCSECT_IDX);
  //fSymbolTable.AddSection(HANDLESECT_IDX);
  //case fRelocInfo.SectionType of
  //  SHT_REL  : hdr.rootptr:=sizeof(hdr);
  //  SHT_RELA : hdr.rootptr:=0;
  //end;
  hdr.rootptr:=sizeof(hdr);

  //fRelocTable.Add(0,sizeof(hdr),RSRCSECT_IDX);
  AddDataRelocation(R_WASM_MEMORY_ADDR_I32,0,Ord(wrdsResources),sizeof(hdr));
  //fRelocTable.Add(sizeof(hdr.rootptr)+sizeof(hdr.count)+sizeof(hdr.usedhandles),0,HANDLESECT_IDX);
  AddDataRelocation(R_WASM_MEMORY_ADDR_I32,sizeof(hdr.rootptr)+sizeof(hdr.count)+sizeof(hdr.usedhandles),Ord(wrdsResHandles),0);
  if fOppositeEndianess then
  begin
    hdr.rootptr:=SwapEndian(hdr.rootptr);
    hdr.count:=SwapEndian(hdr.count);
    //handles must be fixed later
//    hdr.usedhandles:=SwapEndian(hdr.usedhandles);
//    hdr.handles:=SwapEndian(hdr.handles);
  end;
  FDataSegments[wrdsResources].WriteBuffer(hdr,sizeof(hdr));
end;

procedure TWasmResourceWriter.WriteNodeInfos;
begin
  fCurOfs:=sizeof(TResHdr32);
  WriteNodeInfo(fRoot);
  WriteSubNodes(fRoot);
end;

procedure TWasmResourceWriter.WriteNodeInfo(aNode: TResourceTreeNode);
var infonode : TResInfoNode32;
begin
  if aNode.Desc.DescType=dtID then
    infonode.nameid:=aNode.Desc.ID
  else
  begin
    infonode.nameid:=fResStringTable.StartOfs+aNode.NameRVA;
    //fRelocTable.Add(fCurOfs,infonode.nameid,RSRCSECT_IDX);
    AddDataRelocation(R_WASM_MEMORY_ADDR_I32,fCurOfs,Ord(wrdsResources),infonode.nameid);

    //if fRelocInfo.SectionType=SHT_RELA then infonode.nameid:=0;
  end;
  infonode.ncount:=aNode.NamedCount;
  if aNode.IsLeaf then
  begin
    infonode.idcountsize:=aNode.Data.RawData.Size;
    infonode.subptr:=fDataCurOfs;
    fDataCurOfs:=NextAligned(fDataAlignment,fDataCurOfs+infonode.idcountsize);
  end
  else
  begin
    infonode.idcountsize:=aNode.IDCount;
    infonode.subptr:=aNode.SubDirRVA;
  end;
  //fRelocTable.Add(
  //  fCurOfs+sizeof(infonode.nameid)+sizeof(infonode.ncount)+
  //  sizeof(infonode.idcountsize),infonode.subptr,RSRCSECT_IDX);
  AddDataRelocation(
    R_WASM_MEMORY_ADDR_I32,
    fCurOfs+sizeof(infonode.nameid)+sizeof(infonode.ncount)+sizeof(infonode.idcountsize),
    Ord(wrdsResources),
    infonode.subptr);
//  if fRelocInfo.SectionType=SHT_RELA then infonode.subptr:=0;
  if fOppositeEndianess then
  begin
    infonode.nameid:=SwapEndian(infonode.nameid);
    infonode.ncount:=SwapEndian(infonode.ncount);
    infonode.idcountsize:=SwapEndian(infonode.idcountsize);
    infonode.subptr:=SwapEndian(infonode.subptr);
  end;
  FDataSegments[wrdsResources].WriteBuffer(infonode,sizeof(infonode));
  inc(fCurOfs,sizeof(infonode));
end;

procedure TWasmResourceWriter.WriteSubNodes(aNode: TResourceTreeNode);
var i : integer;
begin
  for i:=0 to aNode.NamedCount-1 do
    WriteNodeInfo(aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteNodeInfo(aNode.IDEntries[i]);

  for i:=0 to aNode.NamedCount-1 do
    WriteSubNodes(aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteSubNodes(aNode.IDEntries[i]);
end;

procedure TWasmResourceWriter.WriteResStringTable;
begin
  if fResStringTable.Used then
    fResStringTable.WriteToStream(FDataSegments[wrdsResources]);
  Align(fDataAlignment,FDataSegments[wrdsResources]);
end;

procedure TWasmResourceWriter.WriteRawData;
begin
  WriteResData(FDataSegments[wrdsResources],fRoot);
end;

procedure TWasmResourceWriter.WriteResData(aStream: TStream;
  aNode: TResourceTreeNode);
var rawdata : TStream;
    i : integer;
begin
  if aNode.IsLeaf then
  begin
    rawdata:=aNode.Data.RawData;
    rawdata.Position:=0;
    aStream.CopyFrom(rawdata,rawdata.Size);
    Align(fDataAlignment,aStream);
    exit;
  end;
  for i:=0 to aNode.NamedCount-1 do
    WriteResData(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteResData(aStream,aNode.IDEntries[i]);
end;

procedure TWasmResourceWriter.AddEmptySections(aResources: TResources);
begin
  Align(fDataAlignment,FDataSegments[wrdsResources]);
  //fSections.Add(HandlesSectName,SHT_NOBITS,SHF_ALLOC or SHF_WRITE,
  //  aStream.Position,fDataAlignment*aResources.Count,fDataAlignment);
  FDataSegments[wrdsResHandles].SetSize(fDataAlignment*aResources.Count);
end;

procedure TWasmResourceWriter.WriteWasmSection(aStream: TStream;
  wsid: TWasmSectionID);
var
  b: byte;
begin
  b:=ord(wsid);
  aStream.WriteByte(b);
  WriteUleb(aStream,FWasmSections[wsid].size);
  aStream.CopyFrom(FWasmSections[wsid],0);
end;

procedure TWasmResourceWriter.WriteWasmSectionIfNotEmpty(aStream: TStream;
  wsid: TWasmSectionID);
begin
  if FWasmSections[wsid].size>0 then
    WriteWasmSection(aStream,wsid);
end;

procedure TWasmResourceWriter.WriteWasmCustomSection(aStream: TStream;
  wcst: TWasmCustomSectionType);
begin
  aStream.WriteByte(0);
  WriteUleb(aStream,FWasmCustomSections[wcst].size);
  aStream.CopyFrom(FWasmCustomSections[wcst],0);
end;

procedure TWasmResourceWriter.WriteLinkingSubsection(
  wlst: TWasmLinkingSubsectionType);
begin
  if FWasmLinkingSubsections[wlst].size>0 then
    begin
      FWasmCustomSections[wcstLinking].WriteByte(Ord(wlst));
      WriteUleb(FWasmCustomSections[wcstLinking],FWasmLinkingSubsections[wlst].size);
      FWasmCustomSections[wcstLinking].CopyFrom(FWasmLinkingSubsections[wlst],0);
    end;
end;

procedure TWasmResourceWriter.WriteCustomSectionNames;
var
  cust_sec: TWasmCustomSectionType;
begin
  { each custom sections starts with its name }
  for cust_sec in TWasmCustomSectionType do
    WriteName(FWasmCustomSections[cust_sec],WasmCustomSectionName[cust_sec]);
end;

procedure TWasmResourceWriter.WriteImportSection;
const
  ImportsCount = 1;
begin
  WriteUleb(FWasmSections[wsiImport],ImportsCount);
  { import memories }
  WriteName(FWasmSections[wsiImport],'env');
  WriteName(FWasmSections[wsiImport],'__linear_memory');
  FWasmSections[wsiImport].WriteByte($02);  { mem }
  FWasmSections[wsiImport].WriteByte($00);  { min }
  WriteUleb(FWasmSections[wsiImport],1);    { 1 page }
end;

procedure TWasmResourceWriter.WriteDataSegments;
const
  DataSegmentCount = Ord(High(FDataSegments)) - Ord(Low(FDataSegments)) + 1;
var
  ofs: int64;
  ds: TWasmResourceDataSegment;
begin
  WriteUleb(FWasmSections[wsiData],DataSegmentCount);
  WriteUleb(FWasmSections[wsiDataCount],DataSegmentCount);
  WriteUleb(FWasmLinkingSubsections[WASM_SEGMENT_INFO],DataSegmentCount);

  ofs:=0;
  for ds:=Low(FDataSegments) to High(FDataSegments) do
  begin
    WriteName(FWasmLinkingSubsections[WASM_SEGMENT_INFO],WasmResourceDataSegmentNames[ds]);
    WriteUleb(FWasmLinkingSubsections[WASM_SEGMENT_INFO],BsrQWord(4));  { align }
    WriteUleb(FWasmLinkingSubsections[WASM_SEGMENT_INFO],0);  { flags }

    FWasmSections[wsiData].WriteByte(0);
    FWasmSections[wsiData].WriteByte($41);
    WriteSleb(FWasmSections[wsiData],ofs);
    FWasmSections[wsiData].WriteByte($0b);
    WriteUleb(FWasmSections[wsiData],FDataSegments[ds].Size);
    FDataSegmentFileSectionOfs[ds]:=FWasmSections[wsiData].size;
    if FDataSegments[ds].Size>0 then
    begin
      FWasmSections[wsiData].CopyFrom(FDataSegments[ds], 0);
      Inc(ofs,FDataSegments[ds].Size);
    end;
  end;
end;

procedure TWasmResourceWriter.WriteSymbolTable;
begin
  WriteUleb(FWasmLinkingSubsections[WASM_SYMBOL_TABLE],FWasmSymbolTableEntriesCount);
  FWasmLinkingSubsections[WASM_SYMBOL_TABLE].CopyFrom(FWasmSymbolTable,0);
end;

function TWasmResourceWriter.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TWasmResourceWriter.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TWasmResourceWriter.Write(aResources: TResources; aStream: TStream);
begin
  WriteCustomSectionNames;

  WriteUleb(FWasmCustomSections[wcstLinking],2);  { linking metadata version }

  fRoot:=TRootResTreeNode(GetTree(aResources));
  PrescanResourceTree;
  WriteResHeader(aResources);
  WriteNodeInfos;
  WriteResStringTable;
  WriteRawData;
  AddEmptySections(aResources);

  WriteImportSection;
  WriteDataSegments;

  Inc(FWasmSymbolTableEntriesCount);
  FWasmSymbolTable.WriteByte(Ord(SYMTAB_DATA));
  WriteUleb(FWasmSymbolTable,WASM_SYM_BINDING_LOCAL);  { symbol flags }
  WriteName(FWasmSymbolTable,WasmResourceDataSegmentNames[wrdsResources]);
  WriteUleb(FWasmSymbolTable,0);  { segment index }
  WriteUleb(FWasmSymbolTable,0);  { offset }
  WriteUleb(FWasmSymbolTable,0);  { size }

  Inc(FWasmSymbolTableEntriesCount);
  FWasmSymbolTable.WriteByte(Ord(SYMTAB_DATA));
  WriteUleb(FWasmSymbolTable,WASM_SYM_BINDING_LOCAL);  { symbol flags }
  WriteName(FWasmSymbolTable,WasmResourceDataSegmentNames[wrdsResHandles]);
  WriteUleb(FWasmSymbolTable,1);  { segment index }
  WriteUleb(FWasmSymbolTable,0);  { offset }
  WriteUleb(FWasmSymbolTable,0);  { size }

  Inc(FWasmSymbolTableEntriesCount);
  FWasmSymbolTable.WriteByte(Ord(SYMTAB_DATA));
  WriteUleb(FWasmSymbolTable,0);  { symbol flags }
  WriteName(FWasmSymbolTable,'FPC_RESSYMBOL');
  WriteUleb(FWasmSymbolTable,0);  { segment index }
  WriteUleb(FWasmSymbolTable,0);  { offset }
  WriteUleb(FWasmSymbolTable,0);  { size }

  WriteSymbolTable;
  WriteLinkingSubsection(WASM_SYMBOL_TABLE);
  WriteLinkingSubsection(WASM_SEGMENT_INFO);

  aStream.WriteBuffer(WasmModuleMagic,SizeOf(WasmModuleMagic));
  aStream.WriteBuffer(WasmVersion,SizeOf(WasmVersion));
  WriteWasmSection(aStream,wsiImport);
  WriteWasmSection(aStream,wsiDataCount);
  WriteWasmSection(aStream,wsiData);
  WriteRelocationDataTable(2);
  WriteWasmCustomSection(aStream,wcstLinking);
  WriteWasmCustomSection(aStream,wcstRelocData);
end;

constructor TWasmResourceWriter.Create;
var
  i: TWasmSectionID;
  j: TWasmResourceDataSegment;
  k: TWasmCustomSectionType;
  l: TWasmLinkingSubsectionType;
begin
  fExtensions:='.o .or';
  fDescription:='WebAssembly resource writer';
  fResStringTable:=TResStringTable.Create;
  fDataCurOfs:=0;
  fDataAlignment:=4;
{$IFDEF FPC_BIG_ENDIAN}
  fOppositeEndianess := True;
{$ELSE FPC_BIG_ENDIAN}
  fOppositeEndianess := False;
{$ENDIF FPC_BIG_ENDIAN}
  FWasmSymbolTable:=TMemoryStream.Create;
  FWasmSymbolTableEntriesCount:=0;
  for i in TWasmSectionID do
    FWasmSections[i] := TMemoryStream.Create;
  for j in TWasmResourceDataSegment do
    FDataSegments[j] := TMemoryStream.Create;
  for k in TWasmCustomSectionType do
    FWasmCustomSections[k] := TMemoryStream.Create;
  for l:=low(TWasmLinkingSubsectionType) to high(TWasmLinkingSubsectionType) do
    FWasmLinkingSubsections[l] := TMemoryStream.Create;
end;

destructor TWasmResourceWriter.Destroy;
var
  i: TWasmSectionID;
  j: TWasmResourceDataSegment;
  k: TWasmCustomSectionType;
  l: TWasmLinkingSubsectionType;
begin
  for l:=low(TWasmLinkingSubsectionType) to high(TWasmLinkingSubsectionType) do
    FreeAndNil(FWasmLinkingSubsections[l]);
  for k in TWasmCustomSectionType do
    FreeAndNil(FWasmCustomSections[k]);
  for j in TWasmResourceDataSegment do
    FreeAndNil(FDataSegments[j]);
  for i in TWasmSectionID do
    FreeAndNil(FWasmSections[i]);
  FreeAndNil(FWasmSymbolTable);
  FreeAndNil(fResStringTable);
  inherited Destroy;
end;

initialization
  TResources.RegisterWriter('.o',TWasmResourceWriter);
  TResources.RegisterWriter('.or',TWasmResourceWriter);

end.
