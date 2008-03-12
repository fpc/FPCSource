{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Group icon resource type

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit groupiconresource;

{$MODE OBJFPC}

interface

uses
  Classes, SysUtils, resource, groupresource;
  
type

  { TGroupIconResource }

  TGroupIconResource = class(TGroupResource)
  private
    function WriteIcoIconHeader(aStream : TStream; const index : integer; const start : longword) : longword;
  protected
    procedure ReadResourceItemHeader; override;
    procedure WriteHeader(aStream : TStream); override;
    procedure CreateSubItem; override;
    procedure UpdateItemOwner(index : integer); override;
    procedure ClearItemList; override;
    procedure DeleteSubItems; override;
    function GetSubStream(const index : integer; out aSize : int64) : TStream; override;
    function GetType : TResourceDesc; override;
    function GetName : TResourceDesc; override;
    function ChangeDescTypeAllowed(aDesc : TResourceDesc) : boolean; override;
    function ChangeDescValueAllowed(aDesc : TResourceDesc) : boolean; override;
  public
    constructor Create; override;
    constructor Create(aType,aName : TResourceDesc); override;
  end;


implementation

uses
  resfactory, resdatastream, icocurtypes;

type
  TIconInfo = record
    res : TAbstractResource;
    header : TIconDir;
  end;
  PIconInfo = ^TIconInfo;

{ TGroupIconResource }

procedure TGroupIconResource.ReadResourceItemHeader;
var pii : PIconInfo;
    res : TAbstractResource;
    offsetid : longword;
begin
  if OwnerList=nil then exit;
  GetMem(pii,sizeof(TIconInfo));
  try
   //TIconDir is slightly different in resources and in .ico files
    pii^.header.offsetId:=0;
    RawData.ReadBuffer(pii^.header,sizeof(TIconDir)-2);
    offsetid:=pii^.header.offsetId;
    {$IFDEF ENDIAN_BIG}
    offsetId:=SwapEndian(offsetId);
    {$ENDIF}
    res:=OwnerList.Find(RT_ICON,offsetID,LangID);
    pii^.res:=res;
    SetChildOwner(res);
    fItemList.Add(pii);
   except
     FreeMem(pii);
     raise;
   end;
end;

function TGroupIconResource.WriteIcoIconHeader(aStream : TStream;
  const index : integer; const start : longword) : longword;
var pii : PIconInfo;
    hdr : TIconDir;
begin
  pii:=PIconInfo(fItemList[index]);
  hdr:=pii^.header;
  hdr.offsetId:=start;
  {$IFDEF ENDIAN_BIG}
  hdr.offsetId:=SwapEndian(hdr.offsetId);
  {$ENDIF}
  aStream.WriteBuffer(hdr,sizeof(hdr));
  Result:=start+pii^.res.RawData.Size;
end;

procedure TGroupIconResource.WriteHeader(aStream: TStream);
var nh : TNewHeader;
    i : integer;
    addrcount : longword;
begin
  //write ICO file header (identical to the resource icon header)
  nh.reserved:=0;
  nh.restype:=RES_ICON;
  nh.rescount:=fItemList.Count;
  {$IFDEF ENDIAN_BIG}
  nh.reserved:=SwapEndian(nh.reserved);
  nh.restype:=SwapEndian(nh.restype);
  nh.rescount:=SwapEndian(nh.rescount);
  {$ENDIF}
  aStream.Position:=0;
  aStream.WriteBuffer(nh,sizeof(nh));
  addrcount:=sizeof(TNewHeader)+sizeof(TIconDir)*fItemList.Count;
  for i:=0 to fItemList.Count-1 do
    addrcount:=WriteIcoIconHeader(aStream,i,addrcount);
end;

procedure TGroupIconResource.ClearItemList;
var pii : PIconInfo;
    i : integer;
begin
  if fItemList=nil then exit;
  for i:=0 to fItemList.Count-1 do
  begin
    pii:=PIconInfo(fItemList[i]);
     //if we are not in a TResources, free all subitems by ourselves.
    if OwnerList=nil then pii^.res.Free;
    FreeMem(pii);
  end;
  fItemList.Clear;
end;

procedure TGroupIconResource.DeleteSubItems;
var pii : PIconInfo;
    i : integer;
begin
  if fItemList=nil then exit;
  for i:=0 to fItemList.Count-1 do
  begin
    pii:=PIconInfo(fItemList[i]);
    if OwnerList<>nil then
      OwnerList.Remove(pii^.res);
    pii^.res.Free;
    FreeMem(pii);
  end;
  fItemList.Clear;
end;

procedure TGroupIconResource.CreateSubItem;
var res : TAbstractResource;
    pii : PIconInfo;
    oldpos : int64;
    bytesinres : longword;
    offsetid : longword;
    index : word;
begin
  index:=fItemList.Count+1;
  dummyName.ID:=index;
  res:=TResourceFactory.CreateResource(dummyType,dummyName);
  res.LangID:=LangID;
  if OwnerList<>nil then
    index:=OwnerList.AddAutoID(res);
    
  GetMem(pii,sizeof(TIconInfo));
  fItemList.Add(pii);
  pii^.res:=res;
  ItemData.ReadBuffer(pii^.header,sizeof(TIconDir));
  bytesinres:=pii^.header.bytesinres;
  offsetid:=pii^.header.offsetid;
  {$IFDEF ENDIAN_BIG}
  bytesinres:=SwapEndian(bytesinres);
  offsetID:=SwapEndian(offsetID);
  {$ENDIF}
  oldpos:=ItemData.Position;
  try
    ItemData.Position:=offsetid;
    res.RawData.Size:=0;
    res.RawData.Position:=0;
    res.RawData.CopyFrom(ItemData,bytesinres);
  finally
    ItemData.Position:=oldpos;
  end;
  pii^.header.offsetId:=index;
  {$IFDEF ENDIAN_BIG}
  pii^.header.offsetID:=SwapEndian(pii^.header.offsetID);
  {$ENDIF}
   //TIconDir is slightly different in resources and in .ico files
  RawData.WriteBuffer(pii^.header,sizeof(TIconDir)-2);
end;

procedure TGroupIconResource.UpdateItemOwner(index: integer);
var pii : PIconInfo;
    theid : longword;
    oldpos : int64;
begin
  pii:=PIconInfo(fItemList[index]);
  if pii^.res.OwnerList=OwnerList then exit;
  if OwnerList=nil then
  begin
    pii^.res.OwnerList.Remove(pii^.res);
    exit;
  end;
  theid:=pii^.res.Name.ID;
  OwnerList.AddAutoID(pii^.res);
  if theid<>pii^.res.Name.ID then //id changed, update
  begin
    theid:=pii^.res.Name.ID;
    pii^.header.offsetId:=theid; //update header id value
    {$IFDEF ENDIAN_BIG}
    pii^.header.offsetID:=SwapEndian(pii^.header.offsetID);
    {$ENDIF}
    //update id in rawdata (ItemStream, if present, is ok)
    if (fItemData=nil) or TResourceDataStream(ItemData).Cached then
    begin
      oldpos:=RawData.Position;
      try
        RawData.Position:=sizeof(TNewHeader)+(index+1)*(sizeof(TIconDir)-2)-2;
        RawData.WriteBuffer(pii^.header.offsetID,2); //update id (it's a word)
      finally
        RawData.Position:=oldpos;
      end;
    end;
  end;
end;

function TGroupIconResource.GetSubStream(const index: integer; out aSize : int64): TStream;
begin
  Result:=PIconInfo(fItemList[index])^.res.RawData;
  Result.Position:=0;
  aSize:=Result.Size;
end;

function TGroupIconResource.GetType: TResourceDesc;
begin
  Result:=fType;
end;

function TGroupIconResource.GetName: TResourceDesc;
begin
  Result:=fName;
end;

function TGroupIconResource.ChangeDescTypeAllowed(aDesc: TResourceDesc
  ): boolean;
begin
  Result:=aDesc=fName;
end;

function TGroupIconResource.ChangeDescValueAllowed(aDesc: TResourceDesc
  ): boolean;
begin
  Result:=aDesc=fName;
end;

constructor TGroupIconResource.Create;
begin
  inherited Create;
  fItemList:=nil;
  fItemData:=nil;
  fType:=TResourceDesc.Create(RT_GROUP_ICON);
  fName:=TResourceDesc.Create(1);
  SetDescOwner(fType);
  SetDescOwner(fName);
  dummyType:=TResourceDesc.Create(RT_ICON);
  dummyName:=TResourceDesc.Create(1);
end;

constructor TGroupIconResource.Create(aType, aName: TResourceDesc);
begin
  Create;
  fName.Assign(aName);
end;

initialization
  TResourceFactory.RegisterResourceClass(RT_GROUP_ICON,TGroupIconResource);

end.
