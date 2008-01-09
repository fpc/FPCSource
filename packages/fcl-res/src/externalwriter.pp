{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource writer for external resource types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit externalwriter;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource, resourcetree, externaltypes, strtable;
  
type
  EExternalResourceWriterException = class(EResourceWriterException);
  EExternalResInvalidEndianessException = class(EExternalResourceWriterException);

type

  { TExternalResourceWriter }

  TExternalResourceWriter = class(TAbstractResourceWriter)
  private
    fExtensions : string;
    fDescription : string;
    fFileHeader : TExtHeader;
    fRoot : TRootResTreeNode;
    fEndianess : byte;
    fOppositeEndianess : boolean;
    fStrTable : TResStringTable;
    fDataCurOfs : longword;

    function NextAlignedQword(aValue : longword) : longword;
    procedure AlignQword(aStream : TStream);

    procedure PrescanResourceTree;
    function PrescanNode(aNode : TResourceTreeNode) : longword;
    procedure WriteEmptyFileHeader(aStream : TStream);
    procedure WriteNodeInfos(aStream : TStream);
    procedure WriteNodeInfo(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteSubNodes(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteStringTable(aStream : TStream);
    procedure WriteRawData(aStream : TStream);
    procedure WriteResData(aStream : TStream; aNode : TResourceTreeNode);
    procedure FixHeader(aResources : TResources; aStream : TStream);
    procedure SetEndianess(aEndianess : byte);
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Write(aResources : TResources; aStream : TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Endianess : byte read fEndianess write SetEndianess;
  end;

implementation

{ TExternalResourceWriter }

function TExternalResourceWriter.NextAlignedQword(aValue: longword): longword;
var topad : longword;
begin
  Result:=aValue;
  topad:=8-(aValue mod 8);
  if topad<>8 then inc(Result,topad);
end;

procedure TExternalResourceWriter.AlignQword(aStream: TStream);
var topad : integer;
    qw : qword;
begin
  qw:=0;
  topad:=8-(aStream.Position mod 8);
  if topad<>8 then aStream.WriteBuffer(qw,topad);
end;

procedure TExternalResourceWriter.PrescanResourceTree;
begin
  fStrTable.Clear;
  fRoot.SubDirRVA:=sizeof(TExtHeader)+sizeof(TResInfoNode);
  fStrTable.StartOfs:=PrescanNode(fRoot);
  if fStrTable.Used then
    fDataCurOfs:=NextAlignedQword(fStrTable.StartOfs+fStrTable.Size)
  else
    fDataCurOfs:=fStrTable.StartOfs;
  fFileHeader.nodesize:=fStrTable.StartOfs;
  fFileHeader.hdrsize:=fDataCurOfs;
end;

function TExternalResourceWriter.PrescanNode(aNode: TResourceTreeNode
  ): longword;
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
    aNode.NameRVA:=fStrTable.Add(aNode.Desc.Name);

  //first node subnodes begin at curofs (after all node headers)
  curofs:=aNode.SubDirRva+(aNode.NamedCount+aNode.IDCount)*sizeof(TResInfoNode);
  for i:=0 to aNode.NamedCount-1 do
  begin
    subnode:=aNode.NamedEntries[i];
    subnode.SubDirRVA:=curofs;
    curofs:=PrescanNode(subnode);
  end;
  for i:=0 to aNode.IDCount-1 do
  begin
    subnode:=aNode.IDEntries[i];
    subnode.SubDirRVA:=curofs;
    curofs:=PrescanNode(subnode);
  end;
  Result:=curofs;
end;

procedure TExternalResourceWriter.WriteEmptyFileHeader(aStream: TStream);
begin
  FillByte(fFileHeader,sizeof(fFileHeader),0);
  aStream.WriteBuffer(fFileHeader,sizeof(fFileHeader));
end;

procedure TExternalResourceWriter.WriteNodeInfos(aStream: TStream);
begin
  WriteNodeInfo(aStream,fRoot);
  WriteSubNodes(aStream,fRoot);
end;

procedure TExternalResourceWriter.WriteNodeInfo(aStream: TStream;
  aNode: TResourceTreeNode);
var infonode : TResInfoNode;
begin
  if aNode.Desc.DescType=dtID then
    infonode.nameid:=aNode.Desc.ID
  else
    infonode.nameid:=fStrTable.StartOfs+aNode.NameRVA;
  infonode.ncount:=aNode.NamedCount;
  if aNode.IsLeaf then
  begin
    infonode.idcountsize:=aNode.Data.RawData.Size;
    infonode.subptr:=fDataCurOfs;
    fDataCurOfs:=NextAlignedQword(fDataCurOfs+infonode.idcountsize);
  end
  else
  begin
    infonode.idcountsize:=aNode.IDCount;
    infonode.subptr:=aNode.SubDirRVA;
  end;
  if fOppositeEndianess then
  begin
    infonode.nameid:=SwapEndian(infonode.nameid);
    infonode.ncount:=SwapEndian(infonode.ncount);
    infonode.idcountsize:=SwapEndian(infonode.idcountsize);
    infonode.subptr:=SwapEndian(infonode.subptr);
  end;
  aStream.WriteBuffer(infonode,sizeof(infonode));
end;

procedure TExternalResourceWriter.WriteSubNodes(aStream: TStream;
  aNode: TResourceTreeNode);
var i : integer;
begin
  for i:=0 to aNode.NamedCount-1 do
    WriteNodeInfo(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteNodeInfo(aStream,aNode.IDEntries[i]);
    
  for i:=0 to aNode.NamedCount-1 do
    WriteSubNodes(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteSubNodes(aStream,aNode.IDEntries[i]);
end;

procedure TExternalResourceWriter.WriteStringTable(aStream: TStream);
begin
  if fStrTable.Used then
    fStrTable.WriteToStream(aStream);
  AlignQword(aStream);
end;

procedure TExternalResourceWriter.WriteRawData(aStream: TStream);
begin
  WriteResData(aStream,fRoot);
end;

procedure TExternalResourceWriter.WriteResData(aStream: TStream;
  aNode: TResourceTreeNode);
var rawdata : TStream;
    i : integer;
begin
  if aNode.IsLeaf then
  begin
    rawdata:=aNode.Data.RawData;
    rawdata.Position:=0;
    aStream.CopyFrom(rawdata,rawdata.Size);
    AlignQword(aStream);
    exit;
  end;
  for i:=0 to aNode.NamedCount-1 do
    WriteResData(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteResData(aStream,aNode.IDEntries[i]);
end;

procedure TExternalResourceWriter.FixHeader(aResources : TResources;
  aStream: TStream);
var oldpos : int64;
begin
  fFileHeader.magic:=EXTERNAL_RESMAGIC;
  fFileHeader.version:=EXT_CURRENT_VERSION;
  fFileHeader.endianess:=fEndianess;
  fFileHeader.count:=aResources.Count;
  //nodesize and hdrsize have already been set
  if fOppositeEndianess then
  begin
    fFileHeader.count:=SwapEndian(fFileHeader.count);
    fFileHeader.nodesize:=SwapEndian(fFileHeader.nodesize);
    fFileHeader.hdrsize:=SwapEndian(fFileHeader.hdrsize);
  end;
  oldpos:=aStream.Position;
  aStream.Position:=0;
  aStream.WriteBuffer(fFileHeader,sizeof(fFileHeader));
  aStream.Position:=oldpos;
end;

procedure TExternalResourceWriter.SetEndianess(aEndianess: byte);
begin
  if not (aEndianess in [EXT_ENDIAN_BIG,EXT_ENDIAN_LITTLE]) then
    raise EExternalResInvalidEndianessException.Create('');
  if aEndianess=fEndianess then exit;
  fEndianess:=aEndianess;
  fOppositeEndianess:=not fOppositeEndianess;
end;

function TExternalResourceWriter.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TExternalResourceWriter.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TExternalResourceWriter.Write(aResources: TResources;
  aStream: TStream);
begin
  WriteEmptyFileHeader(aStream);
  fRoot:=TRootResTreeNode(GetTree(aResources));
  PrescanResourceTree;
  FixHeader(aResources,aStream);
  WriteNodeInfos(aStream);
  WriteStringTable(aStream);
  WriteRawData(aStream);
end;

constructor TExternalResourceWriter.Create;
begin
  fRoot:=nil;
  fExtensions:='.fpcres .frs';
  fDescription:='External file resource writer';
  {$IFDEF ENDIAN_BIG}
  fEndianess:=EXT_ENDIAN_BIG;
  {$ELSE}
  fEndianess:=EXT_ENDIAN_LITTLE;
  {$ENDIF}
  fOppositeEndianess:=false;
  fStrTable:=TResStringTable.Create;
end;

destructor TExternalResourceWriter.Destroy;
begin
  fStrTable.Free;
end;

initialization
  TResources.RegisterWriter('.fpcres',TExternalResourceWriter);
  TResources.RegisterWriter('.frs',TExternalResourceWriter);

end.
