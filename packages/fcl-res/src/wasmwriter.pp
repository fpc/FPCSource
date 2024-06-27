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
    fDataAlignment : longword;
    fDataCurOfs : longword;
    FWasmSections: array [TWasmSectionID] of TMemoryStream;
    function NextAligned(aBound, aValue : longword) : longword;
    procedure PrescanResourceTree;
    function PrescanNode(aNode : TResourceTreeNode; aNodeSize : longword) : longword;
    procedure WriteWasmSection(aStream: TStream; wsid: TWasmSectionID);
    procedure WriteWasmSectionIfNotEmpty(aStream: TStream; wsid: TWasmSectionID);
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Write(aResources : TResources; aStream : TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

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

function TWasmResourceWriter.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TWasmResourceWriter.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TWasmResourceWriter.Write(aResources: TResources; aStream: TStream);
const
  DataSegmentCount = 0;
begin
  fRoot:=TRootResTreeNode(GetTree(aResources));
  PrescanResourceTree;

  WriteUleb(FWasmSections[wsiData],DataSegmentCount);
  WriteUleb(FWasmSections[wsiDataCount],DataSegmentCount);

  aStream.WriteBuffer(WasmModuleMagic,SizeOf(WasmModuleMagic));
  aStream.WriteBuffer(WasmVersion,SizeOf(WasmVersion));
  WriteWasmSection(aStream,wsiDataCount);
  WriteWasmSection(aStream,wsiData);
end;

constructor TWasmResourceWriter.Create;
var
  i: TWasmSectionID;
begin
  fExtensions:='.o .or';
  fDescription:='WebAssembly resource writer';
  fResStringTable:=TResStringTable.Create;
  fDataCurOfs:=0;
  fDataAlignment:=4;
  for i in TWasmSectionID do
    FWasmSections[i] := TMemoryStream.Create;
end;

destructor TWasmResourceWriter.Destroy;
var
  i: TWasmSectionID;
begin
  for i in TWasmSectionID do
    FreeAndNil(FWasmSections[i]);
  FreeAndNil(fResStringTable);
  inherited Destroy;
end;

initialization
  TResources.RegisterWriter('.o',TWasmResourceWriter);
  TResources.RegisterWriter('.or',TWasmResourceWriter);

end.
