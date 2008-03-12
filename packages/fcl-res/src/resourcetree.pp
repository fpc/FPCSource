{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Implements an ordered tree of resources

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit resourcetree;

{$MODE OBJFPC}

interface

uses
  Classes, SysUtils, resource;

type

  { TResourceTreeNode }

  TResourceTreeNode = class
  protected
    fParent : TResourceTreeNode;
    fNamedEntries : TFPList;
    fIDEntries : TFPList;
    fSubDirRVA : longword;
    fDataRVA : longword;
    fNameRva : longword;
    fDesc : TResourceDesc;
    function GetNamedCount : longword;
    function GetNamedEntry(index : integer) : TResourceTreeNode;
    function GetIDCount : longword;
    function GetIDEntry(index : integer) : TResourceTreeNode;
    function GetData : TAbstractResource; virtual;
    function InternalFind(aList : TFPList; aDesc : TResourceDesc; out index : integer) : boolean; overload;
    function InternalFind(aList : TFPList; aLangID : TLangID; out index : integer) : boolean; overload;
    function InternalFind(aType, aName : TResourceDesc; const aLangID : TLangID; const noLangID,toDelete : boolean) : TAbstractResource; virtual; abstract; overload;
    constructor Create; virtual; overload;
    property Parent : TResourceTreeNode read fParent;
  public
    destructor Destroy; override;
    procedure Add(aResource : TAbstractResource); virtual; abstract;
    function CreateSubNode(aDesc : TResourceDesc) : TResourceTreeNode; virtual; abstract;
    function CreateResource : TAbstractResource; virtual;
    procedure Clear;
    function Remove(aType, aName : TResourceDesc) : TAbstractResource; overload;
    function Remove(aType, aName : TResourceDesc; const aLangID : TLangID) : TAbstractResource; overload;
    function Find(aType, aName : TResourceDesc) : TAbstractResource; overload;
    function Find(aType, aName : TResourceDesc; const aLangID : TLangID) : TAbstractResource; overload;
    function FindFreeID(aType : TResourceDesc) : TResID; virtual;
    function IsLeaf : boolean; virtual;
    property Desc : TResourceDesc read fDesc;
    property NamedCount : longword read GetNamedCount;
    property NamedEntries[index : integer] : TResourceTreeNode read GetNamedEntry;
    property IDCount : longword read GetIDCount;
    property IDEntries[index : integer] : TResourceTreeNode read GetIDEntry;
    property NameRVA : longword read fNameRVA write fNameRVA;
    property SubDirRVA : longword read fSubDirRVA write fSubDirRVA;
    property DataRVA : longword read fDataRVA write fDataRVA;
    property Data : TAbstractResource read GetData;
  end;

  { TRootResTreeNode }

  TRootResTreeNode = class (TResourceTreeNode)
  protected
    function InternalFind(aType, aName : TResourceDesc; const aLangID : TLangID; const noLangID,toDelete : boolean) : TAbstractResource; override;
  public
    constructor Create; override;
    function CreateSubNode(aDesc : TResourceDesc) : TResourceTreeNode; override;
    procedure Add(aResource : TAbstractResource); override;
    function FindFreeID(aType : TResourceDesc) : TResID; override;
  end;

implementation

uses resfactory;

  { TTypeResTreeNode }

type
  TTypeResTreeNode = class (TResourceTreeNode)
  protected
    function InternalFind(aType, aName : TResourceDesc; const aLangID : TLangID; const noLangID,toDelete : boolean) : TAbstractResource; override;
  public
    constructor Create(aType : TResourceDesc; aParent : TResourceTreeNode); overload;
    function CreateSubNode(aDesc : TResourceDesc) : TResourceTreeNode; override;
    procedure Add(aResource : TAbstractResource); override;
    function FindFreeID(aType : TResourceDesc) : TResID; override;
  end;

  { TNameResTreeNode }

  TNameResTreeNode = class (TResourceTreeNode)
  protected
    function InternalFind(aType, aName : TResourceDesc; const aLangID : TLangID; const noLangID,toDelete : boolean) : TAbstractResource; override;
  public
    constructor Create(aName : TResourceDesc; aParent : TResourceTreeNode); overload;
    function CreateSubNode(aDesc : TResourceDesc) : TResourceTreeNode; override;
    procedure Add(aResource : TAbstractResource); override;
  end;

  { TLangIDResTreeNode }

  TLangIDResTreeNode = class (TResourceTreeNode)
  private
    fData : TAbstractResource;
  protected
    function GetData : TAbstractResource; override;
    function InternalFind(aType, aName : TResourceDesc; const aLangID : TLangID; const noLangID,toDelete : boolean) : TAbstractResource; override;
  public
    constructor Create(aLangID : TLangID; aResource : TAbstractResource; aParent : TResourceTreeNode); overload;
    function CreateResource : TAbstractResource; override;
    function CreateSubNode(aDesc : TResourceDesc) : TResourceTreeNode; override;
    procedure Add(aResource : TAbstractResource); override;
    function IsLeaf : boolean; override;
  end;

function CompareDesc(desc1, desc2 : TResourceDesc) : integer;
begin
  if desc1.DescType=desc2.DescType then
  begin
    case desc1.DescType of
      dtID : Result:=desc1.ID - desc2.ID;
      dtName : Result:=CompareStr(desc1.Name,desc2.Name);
    end;
  end
  else
    case desc1.DescType of
      dtID : Result:=1;
      dtName : Result:=-1;
    end;
end;

function ResListCompare(Item1: Pointer;Item2: Pointer) : Integer;
var node1, node2 : TResourceTreeNode;
begin
  node1:=TResourceTreeNode(Item1);
  node2:=TResourceTreeNode(Item2);
  Result:=CompareDesc(node1.Desc,node2.Desc);
end;

{ TResourceTreeNode }

function TResourceTreeNode.GetIDEntry(index : integer): TResourceTreeNode;
begin
  Result:=TResourceTreeNode(fIDEntries[index]);
end;

function TResourceTreeNode.GetData: TAbstractResource;
begin
  Result:=nil;
end;

function TResourceTreeNode.InternalFind(aList: TFPList; aDesc: TResourceDesc; out index: integer
  ): boolean;
var l, r, p,res : integer;
begin
  Result:=true;
  l:=0;
  r:=aList.Count-1;

  while l<=r do
  begin
    p:=(l+r) div 2;
    res:=CompareDesc(TResourceTreeNode(aList[p]).Desc, aDesc);
    if res<0 then l:=p+1
    else if res>0 then r:=p-1
    else if res=0 then
    begin
      index:=p;
      exit;
    end;
  end;
  index:=l; //the item can be inserted here
  Result:=false;
end;

function TResourceTreeNode.InternalFind(aList: TFPList; aLangID: TLangID; out
  index: integer): boolean;
var l, r, p,res : integer;
begin
  Result:=true;
  l:=0;
  r:=aList.Count-1;

  while l<=r do
  begin
    p:=(l+r) div 2;
    if TResourceTreeNode(aList[p]).Desc.DescType=dtName then res:=-1
    else res:=TResourceTreeNode(aList[p]).Desc.ID - aLangID;
    if res<0 then l:=p+1
    else if res>0 then r:=p-1
    else if res=0 then
    begin
      index:=p;
      exit;
    end;
  end;
  index:=l; //the item can be inserted here
  Result:=false;
end;

function TResourceTreeNode.GetNamedEntry(index : integer): TResourceTreeNode;
begin
  Result:=TResourceTreeNode(fNamedEntries[index]);
end;

function TResourceTreeNode.GetNamedCount: longword;
begin
  Result:=fNamedEntries.Count;
end;

function TResourceTreeNode.GetIDCount: longword;
begin
  Result:=fIDEntries.Count;
end;

constructor TResourceTreeNode.Create;
begin
  fDesc:=TResourceDesc.Create(0);
  fNamedEntries:=TFPList.Create;
  fIDEntries:=TFPList.Create;
  fNameRVA:=0;
  fSubDirRva:=0;
  fDataRVA:=0;
  fParent:=nil;
end;

destructor TResourceTreeNode.Destroy;
begin
  fDesc.Free;
  Clear;
  fNamedEntries.Free;
  fIDEntries.Free;
end;

function TResourceTreeNode.CreateResource: TAbstractResource;
begin
  Result:=nil;
end;

procedure TResourceTreeNode.Clear;
var i : integer;
begin
  for i:=0 to fNamedEntries.Count-1 do
    TResourceTreeNode(fNamedEntries[i]).Free;
  for i:=0 to fIDEntries.Count-1 do
    TResourceTreeNode(fIDEntries[i]).Free;
  fNamedEntries.Clear;
  fIDEntries.Clear;
end;

function TResourceTreeNode.Remove(aType, aName: TResourceDesc
  ): TAbstractResource;
begin
  Result:=InternalFind(aType,aName,0,true,true);
end;

function TResourceTreeNode.Remove(aType, aName: TResourceDesc;
  const aLangID: TLangID): TAbstractResource;
begin
  Result:=InternalFind(aType,aName,aLangID,false,true);
end;

function TResourceTreeNode.Find(aType, aName: TResourceDesc
  ): TAbstractResource;
begin
  Result:=InternalFind(aType,aName,0,true,false);
end;

function TResourceTreeNode.Find(aType, aName: TResourceDesc;
  const aLangID: TLangID): TAbstractResource;
begin
  Result:=InternalFind(aType,aName,aLangID,false,false);
end;

function TResourceTreeNode.FindFreeID(aType: TResourceDesc): TResID;
begin
  Result:=0;
end;

function TResourceTreeNode.IsLeaf: boolean;
begin
  Result:=false;
end;

{ TRootResTreeNode }

constructor TRootResTreeNode.Create;
begin
  inherited Create;
end;

function TRootResTreeNode.CreateSubNode(aDesc: TResourceDesc
  ): TResourceTreeNode;
var theList : TFPList;
begin
  case aDesc.DescType of
    dtID : theList:=fIDEntries;
    dtName : theList:=fNamedEntries;
  end;
  Result:=TTypeResTreeNode.Create(aDesc,self);
  thelist.Add(Result);
end;

procedure TRootResTreeNode.Add(aResource: TAbstractResource);
var theList : TFPList;
    idx : integer;
    subitem : TResourceTreeNode;
begin
  case aResource._Type.DescType of
    dtID : theList:=fIDEntries;
    dtName : theList:=fNamedEntries;
  end;
  if InternalFind(theList,aResource._Type,idx) then
    subitem:=TResourceTreeNode(theList[idx])
  else
  begin
    subitem:=TTypeResTreeNode.Create(aResource._Type,self);
    theList.Insert(idx,subitem);
  end;
  subitem.Add(aResource);
end;

function TRootResTreeNode.FindFreeID(aType: TResourceDesc): TResID;
var theList : TFPList;
    idx : integer;
    subitem : TResourceTreeNode;
begin
  Result:=1;
  case aType.DescType of
    dtID : theList:=fIDEntries;
    dtName : theList:=fNamedEntries;
  end;
  if InternalFind(theList,aType,idx) then
    subitem:=TResourceTreeNode(theList[idx])
  else exit;
  Result:=subitem.FindFreeID(aType);
end;

function TRootResTreeNode.InternalFind(aType, aName : TResourceDesc;
  const aLangID : TLangID; const noLangID,toDelete : boolean) : TAbstractResource;
var theList : TFPList;
    idx : integer;
    subitem : TResourceTreeNode;
begin
  Result:=nil;
  case aType.DescType of
    dtID : theList:=fIDEntries;
    dtName : theList:=fNamedEntries;
  end;
  if InternalFind(theList,aType,idx) then
    subitem:=TResourceTreeNode(theList[idx])
  else exit;
  Result:=subitem.InternalFind(aType,aName,aLangID,noLangID,toDelete);
  if toDelete and ((subitem.IDCount+subitem.NamedCount)=0) then
  begin
    subitem.Free;
    theList.Delete(idx);
  end;
end;

{ TTypeResTreeNode }

constructor TTypeResTreeNode.Create(aType: TResourceDesc;
  aParent: TResourceTreeNode);
begin
  inherited Create;
  fDesc.Assign(aType);
  fParent:=aParent;
end;

function TTypeResTreeNode.CreateSubNode(aDesc: TResourceDesc
  ): TResourceTreeNode;
var theList : TFPList;
begin
  case aDesc.DescType of
    dtID : theList:=fIDEntries;
    dtName : theList:=fNamedEntries;
  end;
  Result:=TNameResTreeNode.Create(aDesc,self);
  thelist.Add(Result);
end;

procedure TTypeResTreeNode.Add(aResource: TAbstractResource);
var theList : TFPList;
    idx : integer;
    subitem : TResourceTreeNode;
begin
  case aResource.Name.DescType of
    dtID : theList:=fIDEntries;
    dtName : theList:=fNamedEntries;
  end;
  if InternalFind(theList,aResource.Name,idx) then
    subitem:=TResourceTreeNode(theList[idx])
  else
  begin
    subitem:=TNameResTreeNode.Create(aResource.Name,self);
    theList.Insert(idx,subitem);
  end;
  subitem.Add(aResource);
end;

function TTypeResTreeNode.FindFreeID(aType: TResourceDesc): TResID;
var i : integer;
begin
  Result:=1;
  if IDCount<=0 then exit; //no items, use 1
  Result:=IDEntries[IDCount-1].Desc.ID+1; //try last one+1
  if Result>$FFFF then
    if IDEntries[0].Desc.ID>1 then Result:=IDEntries[0].Desc.ID-1 //try first one-1
    else
    begin //scan the whole list to find the first free id.
      Result:=1;
      for i:=0 to IDCount-1 do
      begin
        if IDEntries[i].Desc.ID<>Result then exit;
        inc(Result);
      end;
      raise ENoMoreFreeIDsException.Create('');
    end;
end;

function TTypeResTreeNode.InternalFind(aType, aName : TResourceDesc;
  const aLangID : TLangID; const noLangID,toDelete : boolean) : TAbstractResource;
var theList : TFPList;
    idx : integer;
    subitem : TResourceTreeNode;
begin
  Result:=nil;
  case aName.DescType of
    dtID : theList:=fIDEntries;
    dtName : theList:=fNamedEntries;
  end;
  if InternalFind(theList,aName,idx) then
    subitem:=TResourceTreeNode(theList[idx])
  else exit;
  Result:=subitem.InternalFind(aType,aName,aLangID,noLangID,toDelete);
  if toDelete and ((subitem.IDCount+subitem.NamedCount)=0) then
  begin
    subitem.Free;
    theList.Delete(idx);
  end;
end;

{ TNameResTreeNode }

constructor TNameResTreeNode.Create(aName: TResourceDesc;
  aParent: TResourceTreeNode);
begin
  inherited Create;
  fDesc.Assign(aName);
  fParent:=aParent;
end;

function TNameResTreeNode.CreateSubNode(aDesc: TResourceDesc
  ): TResourceTreeNode;
var theList : TFPList;
begin
  case aDesc.DescType of
    dtID : theList:=fIDEntries;
    dtName : theList:=fNamedEntries;
  end;
  Result:=TLangIDResTreeNode.Create(aDesc.ID,nil,self);
  thelist.Add(Result);
end;

procedure TNameResTreeNode.Add(aResource: TAbstractResource);
var idx : integer;
    subitem : TResourceTreeNode;
begin
  if InternalFind(fIDEntries,aResource.LangID,idx) then
    raise EResourceDuplicateException.Create('');

  subitem:=TLangIDResTreeNode.Create(aResource.LangID,aResource,self);
  fIDEntries.Insert(idx,subitem);
end;

function TNameResTreeNode.InternalFind(aType, aName : TResourceDesc;
  const aLangID : TLangID; const noLangID,toDelete : boolean) : TAbstractResource;
var idx : integer;
begin
  Result:=nil;
  if noLangID then
  begin
    if IDCount<=0 then exit
    else idx:=0;
  end
  else
  if not InternalFind(fIDEntries,aLangID,idx) then exit;

  Result:=IDEntries[idx].Data;
  if toDelete then
  begin
    IDEntries[idx].Free;
    fIDEntries.Delete(idx);
  end;
end;

{ TLangIDResTreeNode }

function TLangIDResTreeNode.GetData: TAbstractResource;
begin
  Result:=fData;
end;

constructor TLangIDResTreeNode.Create(aLangID: TLangID;
  aResource: TAbstractResource; aParent: TResourceTreeNode);
begin
  inherited Create;
  fDesc.ID:=aLangID;
  fData:=aResource;
  fParent:=aParent;
end;

function TLangIDResTreeNode.CreateResource: TAbstractResource;
var theType, theName : TResourceDesc;
begin
  Result:=nil;
  if fData<>nil then exit;
  theType:=Parent.Parent.Desc;
  theName:=Parent.Desc;
  fData:=TResourceFactory.CreateResource(theType,theName);
  fData.LangID:=fDesc.ID;
  Result:=fData;
end;

function TLangIDResTreeNode.CreateSubNode(aDesc: TResourceDesc
  ): TResourceTreeNode;
begin
  Result:=nil;
end;

procedure TLangIDResTreeNode.Add(aResource: TAbstractResource);
begin
  //can't add, it's a leaf node
end;

function TLangIDResTreeNode.InternalFind(aType, aName : TResourceDesc;
  const aLangID : TLangID; const noLangID,toDelete : boolean) : TAbstractResource;
begin
  //can't find, it's a leaf node
  Result:=nil;
end;

function TLangIDResTreeNode.IsLeaf: boolean;
begin
  Result:=true;
end;

end.
