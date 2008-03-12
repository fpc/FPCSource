{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource reader for external resource files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit externalreader;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource, resourcetree, externaltypes;
  
type

  { TExternalResourceReader }

  TExternalResourceReader = class(TAbstractResourceReader)
  private
    fExtensions : string;
    fDescription : string;
    fNativeEndianess : byte;
    fEndianess : byte;
    fOppositeEndianess : boolean;
    fFileHeader : TExtHeader;
    fRoot : TRootResTreeNode;

    function ReadString(aStream : TStream; aOfs : longword) : string;
    function ReadFileHeader(aStream : TStream) : boolean;
    procedure ReadResourceTree(aStream : TStream; aResources : TResources);
    procedure ReadNode(aStream : TStream; aParent : TResourceTreeNode;
      aResources : TResources; named : boolean);
    procedure ReadResData(aStream : TStream; aNode : TResourceTreeNode;
      aResources : TResources; datasize : longword);
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Load(aResources : TResources; aStream : TStream); override;
    function CheckMagic(aStream : TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Endianess : byte read fEndianess;
  end;

implementation

uses
  resdatastream, resfactory;

{ TExternalResourceReader }

function TExternalResourceReader.ReadString(aStream: TStream; aOfs: longword
  ): string;
var oldpos : int64;
    c : char;
    maxleft : int64;
begin
  Result:='';
  oldpos:=aStream.Position;
  aStream.Position:=aOfs;
  
  aStream.ReadBuffer(c,1);
  maxleft:=aStream.Size-aStream.Position;
  while (c<>#0) and (maxleft>=0) do
  begin
    Result:=Result+c;
    aStream.ReadBuffer(c,1);
    dec(maxleft);
  end;
  aStream.Position:=oldpos;
end;

function TExternalResourceReader.ReadFileHeader(aStream: TStream): boolean;
var hdr : TExtHeader;
begin
  Result:=false;

  try
    aStream.ReadBuffer(hdr,sizeof(hdr));
  except
    on e : EReadError do exit;
  end;
  
  if hdr.magic<>EXTERNAL_RESMAGIC then exit;
  if hdr.version<>EXT_CURRENT_VERSION then exit;
  if hdr.endianess<>fNativeEndianess then
  begin
    fEndianess:=hdr.endianess;
    fOppositeEndianess:=true;
  end;
  if fOppositeEndianess then
  begin
    hdr.count:=SwapEndian(hdr.count);
    hdr.nodesize:=SwapEndian(hdr.nodesize);
    hdr.hdrsize:=SwapEndian(hdr.hdrsize);
  end;
  fFileHeader:=hdr;
  Result:=true;
end;

procedure TExternalResourceReader.ReadResourceTree(aStream: TStream;
  aResources: TResources);
begin
  aStream.Position:=sizeof(TExtHeader);
  fRoot:=TRootResTreeNode(GetTree(aResources));
  ReadNode(aStream,nil,aResources,false);
end;

procedure TExternalResourceReader.ReadNode(aStream: TStream;
  aParent: TResourceTreeNode; aResources: TResources; named: boolean);
var infonode : TResInfoNode;
    aNode : TResourceTreeNode;
    i : integer;
    oldpos : int64;
    desc : TResourceDesc;
begin
  aStream.ReadBuffer(infonode,sizeof(infonode));
  oldpos:=aStream.Position;
  if fOppositeEndianess then
  begin
    infonode.nameid:=SwapEndian(infonode.nameid);
    infonode.ncount:=SwapEndian(infonode.ncount);
    infonode.idcountsize:=SwapEndian(infonode.idcountsize);
    infonode.subptr:=SwapEndian(infonode.subptr);
  end;
  if aParent=nil then aNode:=fRoot
  else
  begin
    desc:=TResourceDesc.Create;
    try
      if named then desc.Name:=ReadString(aStream,infonode.nameid)
      else desc.ID:=infonode.nameid;
      aNode:=aParent.CreateSubNode(desc);
    finally
      desc.Free;
    end;
  end;
  aStream.Position:=infonode.subptr;
  if aNode.IsLeaf then
    ReadResData(aStream,aNode,aResources,infonode.idcountsize)
  else
  begin
    for i:=1 to infonode.ncount do
      ReadNode(aStream,aNode,aResources,true);
    for i:=1 to infonode.idcountsize do
      ReadNode(aStream,aNode,aResources,false);
  end;
  aStream.Position:=oldpos;
end;

procedure TExternalResourceReader.ReadResData(aStream: TStream;
  aNode: TResourceTreeNode; aResources: TResources; datasize: longword);
var aRes : TAbstractResource;
    RawData : TResourceDataStream;
begin
  aRes:=aNode.CreateResource;
  if aRes=nil then
    raise EResourceDuplicateException.CreateFmt(SResDuplicate,[
      aNode.Data._Type.Name,aNode.Data.Name.Name,aNode.Data.LangID]);
  SetDataSize(aRes,datasize);
  SetDataOffset(aRes,aStream.Position);
  RawData:=TResourceDataStream.Create(aStream,aRes,aRes.DataSize,TCachedResourceDataStream);
  SetRawData(aRes,RawData);
  AddNoTree(aResources,aRes);
end;

function TExternalResourceReader.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TExternalResourceReader.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TExternalResourceReader.Load(aResources: TResources; aStream: TStream
  );
begin
  if not ReadFileHeader(aStream) then
    raise EResourceReaderWrongFormatException.Create('');
  try
    if fFileHeader.count=0 then exit; //no resources in this file.
    ReadResourceTree(aStream,aResources);
  except
    on e : EReadError do
      raise EResourceReaderUnexpectedEndOfStreamException.Create('');
  end;
end;

function TExternalResourceReader.CheckMagic(aStream: TStream): boolean;
begin
  Result:=ReadFileHeader(aStream);
end;

constructor TExternalResourceReader.Create;
begin
  fExtensions:='.fpcres .frs';
  fDescription:='External file resource reader';
  {$IFDEF ENDIAN_BIG}
  fNativeEndianess:=EXT_ENDIAN_BIG;
  {$ELSE}
  fNativeEndianess:=EXT_ENDIAN_LITTLE;
  {$ENDIF}
  fEndianess:=fNativeEndianess;
  fOppositeEndianess:=false;
  fRoot:=nil;
end;

destructor TExternalResourceReader.Destroy;
begin

end;

initialization
  TResources.RegisterReader('.fpcres',TExternalResourceReader);
  TResources.RegisterReader('.frs',TExternalResourceReader);

end.
