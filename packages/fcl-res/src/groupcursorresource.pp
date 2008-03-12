{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Group cursor resource type

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit groupcursorresource;

{$MODE OBJFPC}

interface

uses
  Classes, SysUtils, resource, groupresource;
  
type

  { TGroupCursorResource }

  TGroupCursorResource = class(TGroupResource)
  private
    function WriteCurCursorHeader(aStream : TStream; const index : integer; const start : longword) : longword;
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
  TCurInfo = record
    res : TAbstractResource;
    header : TResCursorDir;
  end;
  PCurInfo = ^TCurInfo;

{ TGroupCursorResource }

procedure TGroupCursorResource.ReadResourceItemHeader;
var pci : PCurInfo;
    res : TAbstractResource;
    cursorid : word;
begin
  if OwnerList=nil then exit;
  GetMem(pci,sizeof(TCurInfo));
  try
    RawData.ReadBuffer(pci^.header,sizeof(TResCursorDir));
    cursorid:=pci^.header.cursorId;
    {$IFDEF ENDIAN_BIG}
    cursorId:=SwapEndian(cursorId);
    {$ENDIF}
    res:=OwnerList.Find(RT_CURSOR,cursorid,LangID);
    pci^.res:=res;
    SetChildOwner(res);
    fItemList.Add(pci);
   except
     FreeMem(pci);
     raise;
   end;
end;

function TGroupCursorResource.WriteCurCursorHeader(aStream: TStream;
  const index: integer; const start: longword): longword;
var pci : PCurInfo;
    hdr : TCurCursorDir;
    tmpw,tmph : word;
begin
  pci:=PCurInfo(fItemList[index]);
  tmpw:=pci^.header.width;
  tmph:=pci^.header.height;
  {$IFDEF ENDIAN_BIG}
  tmpw:=SwapEndian(tmpw);
  tmph:=SwapEndian(tmph);
  {$ENDIF}
  tmph:=tmph div 2;                    //in cursor resources, height is doubled.

  hdr.width:=tmpw;                     //it's a byte now, no need to swap
  hdr.height:=tmph;                    //it's a byte now, no need to swap
  hdr.reserved:=0;
  hdr.bytesincur:=pci^.header.bytesinres;
  hdr.curoffset:=start;
  pci^.res.RawData.Position:=0;
  pci^.res.RawData.ReadBuffer(hdr.xhotspot,2);
  pci^.res.RawData.ReadBuffer(hdr.yhotspot,2);
  {$IFDEF ENDIAN_BIG}
  hdr.curoffset:=SwapEndian(hdr.curoffset);
  hdr.bytesincur:=SwapEndian(hdr.bytesincur);
  {$ENDIF}
  dec(hdr.bytesincur,4); //in resources, cursor has 2 words more for hotspots
  {$IFDEF ENDIAN_BIG}
  hdr.bytesincur:=SwapEndian(hdr.bytesincur);
  {$ENDIF}
  aStream.WriteBuffer(hdr,sizeof(hdr));
  Result:=start+pci^.res.RawData.Size-4;
end;

procedure TGroupCursorResource.WriteHeader(aStream: TStream);
var nh : TNewHeader;
    i : integer;
    addrcount : longword;
begin
  //write CUR file header (identical to the resource cursor header)
  nh.reserved:=0;
  nh.restype:=RES_CURSOR;
  nh.rescount:=fItemList.Count;
  {$IFDEF ENDIAN_BIG}
  nh.reserved:=SwapEndian(nh.reserved);
  nh.restype:=SwapEndian(nh.restype);
  nh.rescount:=SwapEndian(nh.rescount);
  {$ENDIF}
  aStream.Position:=0;
  aStream.WriteBuffer(nh,sizeof(nh));
  addrcount:=sizeof(TNewHeader)+sizeof(TCurCursorDir)*fItemList.Count;
  for i:=0 to fItemList.Count-1 do
    addrcount:=WriteCurCursorHeader(aStream,i,addrcount);
end;

procedure TGroupCursorResource.ClearItemList;
var pci : PCurInfo;
    i : integer;
begin
  if fItemList=nil then exit;
  for i:=0 to fItemList.Count-1 do
  begin
    pci:=PCurInfo(fItemList[i]);
     //if we are not in a TResources, free all subitems by ourselves.
    if OwnerList=nil then pci^.res.Free;
    FreeMem(pci);
  end;
  fItemList.Clear;
end;

procedure TGroupCursorResource.DeleteSubItems;
var pci : PCurInfo;
    i : integer;
begin
  if fItemList=nil then exit;
  for i:=0 to fItemList.Count-1 do
  begin
    pci:=PCurInfo(fItemList[i]);
    if OwnerList<>nil then
      OwnerList.Remove(pci^.res);
    pci^.res.Free;
    FreeMem(pci);
  end;
  fItemList.Clear;
end;

procedure TGroupCursorResource.CreateSubItem;
var res : TAbstractResource;
    pci : PCurInfo;
    curhdr : TCurCursorDir;
    oldpos : int64;
    bytesinres : longword;
    curoffset : longword;
    index : word;
begin
  index:=fItemList.Count+1;
  dummyName.ID:=index;
  res:=TResourceFactory.CreateResource(dummyType,dummyName);
  res.LangID:=LangID;
  if OwnerList<>nil then
    index:=OwnerList.AddAutoID(res);

  GetMem(pci,sizeof(TCurInfo));
  fItemList.Add(pci);
  pci^.res:=res;
  ItemData.ReadBuffer(curhdr,sizeof(TCurCursorDir));
  pci^.header.width:=curhdr.width;     //it was a byte, no need to swap
  pci^.header.height:=curhdr.height*2; //in cursor resources, height is doubled.
  pci^.header.planes:=1;
  pci^.header.bitcount:=1;
  pci^.header.cursorId:=index;
  bytesinres:=curhdr.bytesincur;
  curoffset:=curhdr.curoffset;
  {$IFDEF ENDIAN_BIG}
  bytesinres:=SwapEndian(bytesinres);
  curoffset:=SwapEndian(curoffset);
  {$ENDIF}
  oldpos:=ItemData.Position;
  try
    ItemData.Position:=curoffset;
    res.RawData.Size:=0;
    res.RawData.Position:=0;
    res.RawData.WriteBuffer(curhdr.xhotspot,2);
    res.RawData.WriteBuffer(curhdr.yhotspot,2);
    res.RawData.CopyFrom(ItemData,bytesinres);
  finally
    ItemData.Position:=oldpos;
  end;
  inc(bytesinres,4);       //in resources, cursor has 2 words more for hotspots
  pci^.header.bytesinres:=bytesinres;
  {$IFDEF ENDIAN_BIG}
  pci^.header.width:=SwapEndian(pci^.header.width);
  pci^.header.height:=SwapEndian(pci^.header.height);
  pci^.header.planes:=SwapEndian(pci^.header.planes);
  pci^.header.bitcount:=SwapEndian(pci^.header.bitcount);
  pci^.header.bytesinres:=SwapEndian(pci^.header.bytesinres);
  pci^.header.cursorId:=SwapEndian(pci^.header.cursorId);
  {$ENDIF}
  RawData.WriteBuffer(pci^.header,sizeof(TResCursorDir));
end;

procedure TGroupCursorResource.UpdateItemOwner(index: integer);
var pci : PCurInfo;
    theid : word;
    oldpos : int64;
begin
  pci:=PCurInfo(fItemList[index]);
  if pci^.res.OwnerList=OwnerList then exit;
  if OwnerList=nil then
  begin
    pci^.res.OwnerList.Remove(pci^.res);
    exit;
  end;
  theid:=pci^.res.Name.ID;
  OwnerList.AddAutoID(pci^.res);
  if theid<>pci^.res.Name.ID then //id changed, update
  begin
    theid:=pci^.res.Name.ID;
    pci^.header.cursorId:=theid; //update header id value
    {$IFDEF ENDIAN_BIG}
    pci^.header.cursorId:=SwapEndian(pci^.header.cursorId);
    {$ENDIF}
    //update id in rawdata (ItemStream, if present, is ok)
    if (fItemData=nil) or TResourceDataStream(ItemData).Cached then
    begin
      oldpos:=RawData.Position;
      try
        RawData.Position:=sizeof(TNewHeader)+(index+1)*sizeof(TResCursorDir)-2;
        RawData.WriteBuffer(pci^.header.cursorId,2);
      finally
        RawData.Position:=oldpos;
      end;
    end;
  end;
end;

function TGroupCursorResource.GetSubStream(const index: integer; out aSize : int64): TStream;
begin
  Result:=PCurInfo(fItemList[index])^.res.RawData;
  Result.Position:=4;
  aSize:=Result.Size-4;
end;

function TGroupCursorResource.GetType: TResourceDesc;
begin
  Result:=fType;
end;

function TGroupCursorResource.GetName: TResourceDesc;
begin
  Result:=fName;
end;

function TGroupCursorResource.ChangeDescTypeAllowed(aDesc: TResourceDesc
  ): boolean;
begin
  Result:=aDesc=fName;
end;

function TGroupCursorResource.ChangeDescValueAllowed(aDesc: TResourceDesc
  ): boolean;
begin
  Result:=aDesc=fName;
end;

constructor TGroupCursorResource.Create;
begin
  inherited Create;
  fItemList:=nil;
  fItemData:=nil;
  fType:=TResourceDesc.Create(RT_GROUP_CURSOR);
  fName:=TResourceDesc.Create(1);
  SetDescOwner(fType);
  SetDescOwner(fName);
  dummyType:=TResourceDesc.Create(RT_CURSOR);
  dummyName:=TResourceDesc.Create(1);
end;

constructor TGroupCursorResource.Create(aType, aName: TResourceDesc);
begin
  Create;
  fName.Assign(aName);
end;

initialization
  TResourceFactory.RegisterResourceClass(RT_GROUP_CURSOR,TGroupCursorResource);

end.
