{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Base classes for group cursor and group icon resource types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit groupresource;

{$MODE OBJFPC}

interface

uses
  Classes, SysUtils, resource, resdatastream;

type

  { TGroupResource }

  TGroupResource = class(TAbstractResource)
  protected
    fType : TResourceDesc;
    fName : TResourceDesc;
    fItemData : TStream;
    fItemList : TFPList;
    dummyType : TResourceDesc;
    dummyName : TResourceDesc;
    procedure FindSubResources;
    procedure ReadResourceItemHeader; virtual; abstract;
    procedure CheckBuildItemStream;
    function GetItemData : TStream;
    procedure WriteHeader(aStream : TStream); virtual; abstract;
    function WriteResHeader : word;
    procedure CreateSubItems;
    procedure CreateSubItem; virtual; abstract;
    procedure UpdateItemOwner(index : integer); virtual; abstract;
    procedure ClearItemList; virtual; abstract;
    procedure DeleteSubItems; virtual; abstract;
    function GetSubStreamCount : integer;
    function GetSubStream(const index : integer; out aSize : int64) : TStream; virtual; abstract;
    procedure SetOwnerList(aResources : TResources); override;
    procedure NotifyResourcesLoaded; override;
  public
    destructor Destroy; override;
    function CompareContents(aResource: TAbstractResource): boolean; override;
    procedure SetCustomItemDataStream(aStream : TStream);
    procedure UpdateRawData; override;
    property ItemData : TStream read GetItemData;
  end;

  { TGroupCachedDataStream }

  TGroupCachedDataStream = class(TCachedDataStream)
  private
    fHeader : TMemoryStream;
    fStreams : TFPList;
    function ReadFromSubStream(aStream : TStream; var Buffer; aPosition : int64; aCount : longint) : longint;
  protected
  public
    constructor Create(aStream : TStream;  aResource : TAbstractResource; aSize : int64); override;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

implementation

uses
  icocurtypes;

{ TGroupResource }

procedure TGroupResource.FindSubResources;
var nh : TNewHeader;
    i : integer;
begin
  if fItemList<>nil then exit;
  fItemList:=TFPList.Create;
  //read NewHeader from resource
  RawData.Position:=0;
  try
    RawData.ReadBuffer(nh,sizeof(nh));
  except
    on e : EReadError do exit; //empty stream
  end;
  {$IFDEF ENDIAN_BIG}
  nh.reserved:=SwapEndian(nh.reserved);
  nh.restype:=SwapEndian(nh.restype);
  nh.rescount:=SwapEndian(nh.rescount);
  {$ENDIF}
  for i:=1 to nh.rescount do
    ReadResourceItemHeader;
end;

procedure TGroupResource.CheckBuildItemStream;
begin
  if fItemData<>nil then exit;
  FindSubResources;
  fItemData:=TResourceDataStream.Create(RawData,self,DataSize,TGroupCachedDataStream);
end;

function TGroupResource.GetItemData: TStream;
begin
  CheckBuildItemStream;
  Result:=fItemData;
end;

function TGroupResource.WriteResHeader: word;
var nh : TNewHeader;
begin
  //copy RES header from the ICO/CUR one (they are identical)
  ItemData.Position:=0;
  ItemData.ReadBuffer(nh,sizeof(nh));
  RawData.Size:=0;
  RawData.Position:=0;
  RawData.WriteBuffer(nh,sizeof(nh));
  Result:=nh.rescount;
  {$IFDEF ENDIAN_BIG}
  Result:=SwapEndian(Result);
  {$ENDIF}
end;

procedure TGroupResource.CreateSubItems;
var itemcount : word;
    i : integer;
begin
  if fItemList=nil then fItemList:=TFPList.Create;
  itemcount:=WriteResHeader;
  for i:=1 to itemcount do
    CreateSubItem;
end;

function TGroupResource.GetSubStreamCount: integer;
begin
  Result:=fItemList.Count;
end;

procedure TGroupResource.SetOwnerList(aResources: TResources);
var i : integer;
begin
  inherited SetOwnerList(aResources);
  if fItemList=nil then exit;
  for i:=0 to fItemList.Count-1 do
    UpdateItemOwner(i);
end;

procedure TGroupResource.NotifyResourcesLoaded;
begin
  //all resources have been loaded, so find all sub resources and tell them
  //we are the owners
  FindSubResources;
end;

destructor TGroupResource.Destroy;
begin
  if fItemData<>nil then fItemData.Free;
  ClearItemList;
  fItemList.Free;
  fType.Free;
  fName.Free;
  dummyType.Free;
  dummyName.Free;
  inherited Destroy;
end;

function TGroupResource.CompareContents(aResource: TAbstractResource): boolean;
begin
  if aResource is TGroupResource then
    Result:=TResourceDataStream(ItemData).Compare(TGroupResource(aResource).ItemData)
  else
    Result:=inherited CompareContents(aResource);
end;

procedure TGroupResource.SetCustomItemDataStream(aStream: TStream);
begin
  TResourceDataStream(ItemData).SetCustomStream(aStream);
end;

procedure TGroupResource.UpdateRawData;
begin
  if (fItemData=nil) or  TResourceDataStream(ItemData).Cached then exit; //no need to update rawdata
  DeleteSubItems;
  CreateSubItems;
  FreeAndNil(fItemData);
end;

  { TGroupCachedDataStream }

function TGroupCachedDataStream.ReadFromSubStream(aStream: TStream;
  var Buffer; aPosition: int64; aCount: longint): longint;
var oldpos : int64;
begin
  Result:=aStream.Size-aPosition;
  if aCount<Result then Result:=aCount;
  if Result<0 then Result:=0;
  oldpos:=aStream.Position;
  aStream.Position:=aPosition;
  Result:=aStream.Read(Buffer,Result);
  aStream.Position:=oldpos;
end;

constructor TGroupCachedDataStream.Create(aStream: TStream;  aResource : TAbstractResource; aSize: int64);
var i, strcount : integer;
    tmpstr : TStream;
begin
  inherited Create(aStream,aResource,aSize);
  fHeader:=TMemoryStream.Create;
  fStreams:=TFPList.Create;
  TGroupResource(aResource).WriteHeader(fHeader);
  strcount:=TGroupResource(aResource).GetSubStreamCount;
  fSize:=fHeader.Size;
  for i:=0 to strcount-1 do
  begin
    tmpstr:=TGroupResource(aResource).GetSubStream(i,aSize);
    tmpstr:=TCachedResourceDataStream.Create(tmpstr,aResource,aSize);
    fStreams.Add(tmpstr);
    inc(fSize,aSize);
  end;
end;

destructor TGroupCachedDataStream.Destroy;
var i : integer;
begin
  for i:=0 to fStreams.Count-1 do
    TStream(fStreams[i]).Free; //free the cached streams
  fStreams.Free;
  fHeader.Free;
end;

function TGroupCachedDataStream.Read(var Buffer; Count: Longint): Longint;
var toread,read_in,delta : longint;
    b : pbyte;
    i : integer;
begin
  Result:=0;
  toread:=fSize-Position;
  if Count<toread then toread:=Count;
  if toread<0 then toread:=0;
  b:=@buffer;

  read_in:=ReadFromSubStream(fHeader,b^,fPosition,toread);
  inc(fPosition,read_in);
  inc(b,read_in);
  inc(Result,read_in);
  dec(toread,read_in);
  delta:=fHeader.Size;

  for i:=0 to fStreams.Count-1 do
  begin
    if toread<=0 then exit;
    read_in:=ReadFromSubStream(TStream(fStreams[i]),b^,fPosition-delta,toread);
    inc(fPosition,read_in);
    inc(b,read_in);
    inc(Result,read_in);
    dec(toread,read_in);
    inc(delta,TStream(fStreams[i]).Size);
  end;
end;

end.
