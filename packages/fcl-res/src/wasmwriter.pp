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
    FWasmSections: array [TWasmSectionID] of TMemoryStream;
    FDataSegments: array [TWasmResourceDataSegment] of TMemoryStream;
    function NextAligned(aBound, aValue : longword) : longword;
    procedure PrescanResourceTree;
    function PrescanNode(aNode : TResourceTreeNode; aNodeSize : longword) : longword;
    procedure WriteResHeader(aResources : TResources);
    procedure WriteWasmSection(aStream: TStream; wsid: TWasmSectionID);
    procedure WriteWasmSectionIfNotEmpty(aStream: TStream; wsid: TWasmSectionID);
    procedure WriteImportSection;
    procedure WriteDataSegments;
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

  //fRelocTable.Add(0,sizeof(hdr),RSRCSECT_IDX);
  //fRelocTable.Add(sizeof(hdr.rootptr)+sizeof(hdr.count)+sizeof(hdr.usedhandles),0,HANDLESECT_IDX);
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
  DataSegmentCount = 2;
var
  ds: TMemoryStream;
  ofs: int64;
begin
  WriteUleb(FWasmSections[wsiData],DataSegmentCount);
  WriteUleb(FWasmSections[wsiDataCount],DataSegmentCount);
  ofs:=0;
  for ds in FDataSegments do
  begin
    FWasmSections[wsiData].WriteByte(0);
    FWasmSections[wsiData].WriteByte($41);
    WriteSleb(FWasmSections[wsiData],ofs);
    FWasmSections[wsiData].WriteByte($0b);
    WriteUleb(FWasmSections[wsiData],ds.Size);
    if ds.Size>0 then
    begin
      FWasmSections[wsiData].CopyFrom(ds, 0);
      Inc(ofs,ds.Size);
    end;
  end;
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
  fRoot:=TRootResTreeNode(GetTree(aResources));
  PrescanResourceTree;
  WriteResHeader(aResources);

  WriteImportSection;
  WriteDataSegments;

  aStream.WriteBuffer(WasmModuleMagic,SizeOf(WasmModuleMagic));
  aStream.WriteBuffer(WasmVersion,SizeOf(WasmVersion));
  WriteWasmSection(aStream,wsiImport);
  WriteWasmSection(aStream,wsiDataCount);
  WriteWasmSection(aStream,wsiData);
end;

constructor TWasmResourceWriter.Create;
var
  i: TWasmSectionID;
  j: TWasmResourceDataSegment;
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
  for i in TWasmSectionID do
    FWasmSections[i] := TMemoryStream.Create;
  for j in TWasmResourceDataSegment do
    FDataSegments[j] := TMemoryStream.Create;
end;

destructor TWasmResourceWriter.Destroy;
var
  i: TWasmSectionID;
  j: TWasmResourceDataSegment;
begin
  for j in TWasmResourceDataSegment do
    FreeAndNil(FDataSegments[j]);
  for i in TWasmSectionID do
    FreeAndNil(FWasmSections[i]);
  FreeAndNil(fResStringTable);
  inherited Destroy;
end;

initialization
  TResources.RegisterWriter('.o',TWasmResourceWriter);
  TResources.RegisterWriter('.or',TWasmResourceWriter);

end.
