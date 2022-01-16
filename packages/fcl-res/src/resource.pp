{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Base classes for resource handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit resource;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, Sysutils;

const
  RT_CURSOR       =  1;  //Hardware-dependent cursor resource.
  RT_BITMAP       =  2;  //Bitmap resource.
  RT_ICON         =  3;  //Hardware-dependent icon resource.
  RT_MENU         =  4;  //Menu resource.
  RT_DIALOG       =  5;  //Dialog box.
  RT_STRING       =  6;  //String-table entry.
  RT_FONTDIR      =  7;  //Font directory resource.
  RT_FONT         =  8;  //Font resource.
  RT_ACCELERATOR  =  9;  //Accelerator table.
  RT_RCDATA       = 10;  //Application-defined resource (raw data).
  RT_MESSAGETABLE = 11;  //Message-table entry.
  RT_GROUP_CURSOR = 12;  //Hardware-independent cursor resource.
  RT_GROUP_ICON   = 14;  //Hardware-independent icon resource.
  RT_VERSION      = 16;  //Version resource.
  RT_DLGINCLUDE   = 17;  //Never present in compiled form
  RT_PLUGPLAY     = 19;  //Plug and Play resource.
  RT_VXD          = 20;  //VXD.
  RT_ANICURSOR    = 21;  //Animated cursor.
  RT_ANIICON      = 22;  //Animated icon.
  RT_HTML         = 23;  //HTML.
  RT_MANIFEST     = 24;  //Microsoft Windows XP: Side-by-Side Assembly XML Manifest.
  RT_DLGINIT      = 240; //Never present in compiled form

  CREATEPROCESS_MANIFEST_RESOURCE_ID                 = 1;
  ISOLATIONAWARE_MANIFEST_RESOURCE_ID                = 2;
  ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID = 3;
  MINIMUM_RESERVED_MANIFEST_RESOURCE_ID              = 1;   //inclusive
  MAXIMUM_RESERVED_MANIFEST_RESOURCE_ID              = 16;  //inclusive

const
  MF_MOVEABLE    = $0010;
  MF_PURE        = $0020;
  MF_PRELOAD     = $0040;
  MF_DISCARDABLE = $1000;

resourcestring
  SReaderNotFoundExt   = 'Cannot find resource reader for extension ''%s''';
  SReaderNotFoundProbe = 'Cannot find a resource reader: unknown format.';
  SWriterNotFoundExt   = 'Cannot find resource writer for extension ''%s''';
  SDescChangeNotAllowed = 'Cannot modify %s resource description';
  SLangIDChangeNotAllowed = 'Cannot modify %s resource language ID';
  SResDuplicate = 'Duplicate resource: Type = %s, Name = %s, Lang ID = %.4x';
  SResourceNotFound = 'Cannot find resource: Type = %s, Name = %s';
  SResourceNotFoundLang = 'Cannot find resource: Type = %s, Name = %s, Lang ID = %.4x';

type
  TLangID = word;
  TResName = string;
  TResID = LongWord;
  TDescType = (dtName, dtID);

type
  EResourceException = class(Exception);
  EResourceDescTypeException = class(EResourceException);
  EResourceDescChangeNotAllowedException = class(EResourceException);
  EResourceLangIDChangeNotAllowedException = class(EResourceException);
  EResourceDuplicateException = class(EResourceException);
  EResourceNotFoundException = class(EResourceException);
  ENoMoreFreeIDsException = class(EResourceException);

  EResourceReaderException = class(EResourceException);
  EResourceReaderNotFoundException = class(EResourceReaderException);
  EResourceReaderWrongFormatException = class(EResourceReaderException);
  EResourceReaderUnexpectedEndOfStreamException = class (EResourceReaderException);

  EResourceWriterException = class(EResourceException);
  EResourceWriterNotFoundException = class(EResourceWriterException);

type

  TAbstractResource = class;

  { TResourceDesc }

  TResourceDesc = class
  private
    fName : TResName;
    fID : TResID;
    fDescType : TDescType;
    fOwner : TAbstractresource;
    
    function GetID : TResID;
    function GetName : TResName;
    procedure SetID(const aID : TResID);
    procedure SetName(const aName : TResName);
    procedure CanChangeType(newType : TDescType);
    procedure CanChangeValue;
  protected
    procedure SetOwner(aOwner : TAbstractResource);
  public
    constructor Create; overload;
    constructor Create(const aID : TResID); overload;
    constructor Create(const aName : TResName); overload;
    procedure Assign(aResourceDesc : TResourceDesc);
    function Equals(aResDesc : TResourceDesc) : boolean;
    property Name : TResName read GetName write SetName;
    property ID : TResID read GetID write SetID;
    property DescType : TDescType read fDescType;
  end;

  TResources = class;

  { TAbstractResource }

  TAbstractResource = class
  private
    fLangId : TLangID;
    fDataSize : longword;
    fHeaderSize : longword;
    fDataVersion : longword;
    fMemoryFlags : word;
    fVersion : longword;
    fCharacteristics : longword;
    fDataOffset : longword;
    fCodePage : longword;
    fRawData : TStream;
    fOwnerList : TResources;
    fOwner : TAbstractResource;
    function GetRawData : TStream;
    function GetCacheData : boolean;
    procedure SetCacheData(const aValue : boolean);
    function GetDataSize : longword;
    procedure SetLangID(aLangID : TLangID);
  protected
    procedure SetDescOwner(aDesc : TResourceDesc);
    procedure SetOwnerList(aResources : TResources); virtual;
    procedure SetChildOwner(aChild : TAbstractResource);
    function GetType : TResourceDesc; virtual; abstract;
    function GetName : TResourceDesc; virtual; abstract;
    function ChangeDescTypeAllowed(aDesc : TResourceDesc) : boolean; virtual; abstract;
    function ChangeDescValueAllowed(aDesc : TResourceDesc) : boolean; virtual; abstract;
    procedure NotifyResourcesLoaded; virtual; abstract;
    constructor Create; virtual; overload;
  public
    constructor Create(aType,aName : TResourceDesc); virtual; abstract; overload;
    destructor Destroy; override;
    function CompareContents(aResource: TAbstractResource): boolean; virtual;
    procedure UpdateRawData; virtual; abstract;
    procedure SetCustomRawDataStream(aStream : TStream);
    property _Type : TResourceDesc read GetType;
    property Name : TResourceDesc read GetName;
    property LangID : TLangID read fLangID write SetLangID;
    property DataSize : longword read GetDataSize;
    property HeaderSize : longword read fHeaderSize;
    property DataVersion : longword read fDataVersion write fDataVersion;
    property MemoryFlags : word read fMemoryFlags write fMemoryFlags;
    property Version : longword read fVersion write fVersion;
    property Characteristics : longword read fCharacteristics write fCharacteristics;
    property DataOffset : longword read fDataOffset;
    property CodePage : longword read fCodePage write fCodePage;
    property RawData : TStream read GetRawData;
    property CacheData : boolean read GetCacheData write SetCacheData;
    property OwnerList : TResources read fOwnerList;
    property Owner : TAbstractResource read fOwner;
  end;
  
  TResourceClass = class of TAbstractResource;
  
  { TGenericResource }

  TGenericResource = class(TAbstractResource)
  private
    fType : TResourceDesc;
    fName : TResourceDesc;
  protected
    function GetType : TResourceDesc; override;
    function GetName : TResourceDesc; override;
    function ChangeDescTypeAllowed(aDesc : TResourceDesc) : boolean; override;
    function ChangeDescValueAllowed(aDesc : TResourceDesc) : boolean; override;
    procedure NotifyResourcesLoaded; override;
  public
    constructor Create(aType,aName : TResourceDesc); override;
    destructor Destroy; override;
    procedure UpdateRawData; override;
  end;

type
  TAbstractResourceReader = class;
  TAbstractResourceWriter = class;
  TResourceReaderClass = class of TAbstractResourceReader;
  TResourceWriterClass = class of TAbstractResourceWriter;

  { TResources }

  TResources = class
  private
    fList : TFPList;
    fTree : TObject;
    dummyType : TResourceDesc;
    dummyName : TResourceDesc;
    fCacheData : boolean;
    fMoveFromCount : integer;
    fRemovedCount : integer;
    function GetItem(index : integer) : TAbstractResource;
    function GetCount : longword;
    procedure SetCacheData(const aValue : boolean);
    procedure NotifyLoaded;
//  protected
  private
    fTempRStream : TStream;
    class procedure InitReaderList;
    class procedure InitWriterList;
    class procedure DisposeStreamerList(aList : TFPList);
    class procedure DisposeReaderList;
    class procedure DisposeWriterList;
    class function FindWriterClass(aExtension : string) : TResourceWriterClass;
    class procedure RegisterStreamer(aList : TFPList; aExtension : string; aClass : TClass);

    procedure SendUpdateRawData;
    procedure InternalRemove(aResource: TAbstractResource);
    procedure InternalRemove(aIndex : integer);
    procedure QuietRemove(aResource : TAbstractResource; aIndex : integer; aIndexValid : boolean);
    procedure InternalClear;
    procedure InternalAdd(aResource : TAbstractResource; prevIdx : integer; prevIdxValid : boolean);
    procedure AddNoTree(aResource : TAbstractResource);
    function InternalFind(aType, aName : TResourceDesc; const aLangID : TLangID) : TAbstractResource; overload;
    function InternalFind(aType, aName : TResourceDesc) : TAbstractResource; overload;
    procedure BeginMoveFrom;
    procedure EndMoveFrom;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aResource : TAbstractResource);
    function AddAutoID(aResource : TAbstractResource) : TResID;
    procedure Clear;
    function Find(aType, aName : TResourceDesc; const aLangID : TLangID) : TAbstractResource; overload;
    function Find(aType, aName : TResourceDesc) : TAbstractResource; overload;
    function Find(const aType : TResName; const aName : TResName; const aLangID : TLangID) : TAbstractResource; overload;
    function Find(const aType : TResName; const aName : TResID; const aLangID : TLangID) : TAbstractResource; overload;
    function Find(const aType : TResID; const aName : TResName; const aLangID : TLangID) : TAbstractResource; overload;
    function Find(const aType : TResID; const aName : TResID; const aLangID : TLangID) : TAbstractResource; overload;
    function Find(const aType : TResName; const aName : TResName) : TAbstractResource; overload;
    function Find(const aType : TResName; const aName : TResID) : TAbstractResource; overload;
    function Find(const aType : TResID; const aName : TResName) : TAbstractResource; overload;
    function Find(const aType : TResID; const aName : TResID) : TAbstractResource; overload;
    class function FindReader(aStream: TStream; aExtension: string) : TAbstractResourceReader;
    class function FindReader(aStream : TStream) : TAbstractResourceReader;
    procedure MoveFrom(aResources : TResources);
    function Remove(aType,aName : TResourceDesc; const aLangID : TLangID) : TAbstractResource; overload;
    function Remove(aType,aName : TResourceDesc) : TAbstractResource; overload;
    function Remove(aResource : TAbstractResource) : TAbstractResource; overload;
    function Remove(aIndex : integer) : TAbstractResource; overload;
    procedure LoadFromStream(aStream : TStream); overload;
    procedure LoadFromStream(aStream : TStream; aReader : TAbstractResourceReader); overload;
    procedure LoadFromFile(aFileName : string); overload;
    procedure LoadFromFile(aFileName : string; aReader : TAbstractResourceReader); overload;
    class procedure RegisterReader(const aExtension : string; aClass : TResourceReaderClass);
    class procedure RegisterWriter(const aExtension : string; aClass : TResourceWriterClass);
    procedure WriteToStream(aStream : TStream; aWriter : TAbstractResourceWriter);
    procedure WriteToFile(aFileName : string); overload;
    procedure WriteToFile(aFileName : string; aWriter : TAbstractResourceWriter); overload;
    property Count : longword read GetCount;
    property Items[Index : integer] : TAbstractResource read GetItem; default;
    property CacheData : boolean read fCacheData write SetCacheData;
  end;

  { TAbstractResourceReader }

  TAbstractResourceReader = class
  private
  protected
    procedure SetDataSize(aResource : TAbstractResource; aValue : longword);
    procedure SetHeaderSize(aResource : TAbstractResource; aValue : longword);
    procedure SetDataOffset(aResource : TAbstractResource; aValue : longword);
    procedure SetRawData(aResource : TAbstractResource; aStream : TStream);
    procedure CallSubReaderLoad(aReader: TAbstractResourceReader; aResources : TResources; aStream : TStream);
    procedure AddNoTree(aResources : TResources; aResource: TAbstractResource);
    function GetTree(aResources : TResources) : TObject;
    function GetExtensions : string; virtual; abstract;
    function GetDescription : string; virtual; abstract;
    procedure Load(aResources : TResources; aStream : TStream); virtual; abstract;
    function CheckMagic(aStream : TStream) : boolean; virtual; abstract;
  public
    constructor Create; virtual; abstract;
    property Extensions : string read GetExtensions;
    property Description : string read GetDescription;
  end;

  { TAbstractResourceWriter }

  TAbstractResourceWriter = class
  private
  protected
    function GetTree(aResources : TResources) : TObject;
    function GetExtensions : string; virtual; abstract;
    function GetDescription : string; virtual; abstract;
    procedure Write(aResources : TResources; aStream : TStream); virtual; abstract;
  public
    constructor Create; virtual; abstract;
    property Extensions : string read GetExtensions;
    property Description : string read GetDescription;
  end;

implementation

uses resdatastream, resourcetree, resmerger;

type
  PRegisteredStreamerEntry = ^TRegisteredStreamerEntry;
  TRegisteredStreamerEntry = record
    ext : shortstring;
    _class : TClass;
    next : PRegisteredStreamerEntry;
  end;

var RegisteredReaders : TFPList = nil;
    RegisteredWriters : TFPList = nil;

{ TResourceDesc }

function TResourceDesc.GetID: TResID;
begin
  if fDescType<>dtId then
    raise EResourceDescTypeException.Create('');
  Result:=fId;
end;

function TResourceDesc.GetName: TResName;
begin
  if fDescType = dtName then
    Result:=fName
  else Result:=IntToStr(fId);
end;

procedure TResourceDesc.CanChangeType(newType : TDescType);
begin
  if fOwner=nil then exit;
  if newType=fDescType then exit;
  if (fOwner.OwnerList<>nil) or (not fOwner.ChangeDescTypeAllowed(self)) then
    raise EResourceDescChangeNotAllowedException.CreateFmt(SDescChangeNotAllowed,[Name]);
end;

procedure TResourceDesc.CanChangeValue;
begin
  if fOwner=nil then exit;
  if (fOwner.OwnerList<>nil) or (not fOwner.ChangeDescValueAllowed(self)) then
    raise EResourceDescChangeNotAllowedException.CreateFmt(SDescChangeNotAllowed,[Name]);
end;

procedure TResourceDesc.SetOwner(aOwner: TAbstractResource);
begin
  fOwner:=aOwner;
end;

procedure TResourceDesc.SetID(const aID: TResID);
begin
  CanChangeType(dtID);
  CanChangeValue;
  fDescType:=dtID;
  fId:=aID;
end;

procedure TResourceDesc.SetName(const aName: TResName);
begin
  CanChangeType(dtName);
  CanChangeValue;
  fDescType:=dtName;
  fName:=UpperCase(aName);
end;

constructor TResourceDesc.Create;
begin
  fName:='';
  fID:=0;
  fDescType:=dtName;
  fOwner:=nil;
end;

constructor TResourceDesc.Create(const aID: TResID);
begin
  Create;
  SetID(aID);
end;

constructor TResourceDesc.Create(const aName: TResName);
begin
  Create;
  SetName(aName);
end;

procedure TResourceDesc.Assign(aResourceDesc: TResourceDesc);
begin
  CanChangeType(aResourceDesc.fDescType);
  CanChangeValue;
  fDescType:=aResourceDesc.fDescType;
  case fDescType of
    dtID   : begin fID:=aResourceDesc.fID; fName:=''; end;
    dtName : begin fName:=aResourceDesc.fName; fID:=0; end;
  end;
end;

function TResourceDesc.Equals(aResDesc: TResourceDesc): boolean;
begin
  Result:=aResDesc.DescType=fDescType;
  if not Result then exit;
  case fDescType of
    dtName : Result:=aResDesc.Name=fName;
    dtID   : Result:=aResDesc.ID=fID;
  end;
end;

{ TAbstractResource }

function TAbstractResource.GetRawData: TStream;
begin
  if fRawData = nil then
    fRawData:=TResourceDataStream.Create(nil,self,DataSize,TCachedResourceDataStream);
  Result:=fRawData;
end;

function TAbstractResource.GetCacheData: boolean;
begin
  Result:=TResourceDataStream(RawData).Cached;
end;

procedure TAbstractResource.SetCacheData(const aValue: boolean);
begin
  TResourceDataStream(RawData).Cached:=aValue;
end;

function TAbstractResource.GetDataSize: longword;
begin
  if fRawData=nil then Result:=fDataSize
  else Result:=fRawData.Size;
end;

procedure TAbstractResource.SetLangID(aLangID: TLangID);
begin
  if OwnerList<>nil then
    raise EResourceLangIDChangeNotAllowedException.CreateFmt(SLangIDChangeNotAllowed,[Name]);
  fLangId:=aLangID;
end;

procedure TAbstractResource.SetDescOwner(aDesc: TResourceDesc);
begin
  aDesc.SetOwner(self);
end;

procedure TAbstractResource.SetOwnerList(aResources: TResources);
begin
  fOwnerList:=aResources;
end;

procedure TAbstractResource.SetChildOwner(aChild: TAbstractResource);
begin
  aChild.fOwner:=self;
end;

constructor TAbstractResource.Create;
begin
  fLangID:=0;
  
  fDataSize:=0;
  fHeaderSize:=0;
  fDataVersion:=0;
  fMemoryFlags:=MF_MOVEABLE or MF_DISCARDABLE;
  fVersion:=0;
  fCharacteristics:=0;
  fDataOffset:=0;
  fCodePage:=0;
  fRawData:=nil;
  fOwnerList:=nil;
  fOwner:=nil;
end;

destructor TAbstractResource.Destroy;
begin
  if fRawData<>nil then fRawData.Free;
end;

function TAbstractResource.CompareContents(aResource: TAbstractResource
  ): boolean;
begin
  Result:=TResourceDataStream(RawData).Compare(aResource.RawData);
end;

procedure TAbstractResource.SetCustomRawDataStream(aStream: TStream);
begin
  TResourceDataStream(RawData).SetCustomStream(aStream);
end;

{ TResources }

function TResources.GetItem(index: integer): TAbstractResource;
begin
  Result:=TAbstractResource(fList[index]);
end;

function TResources.GetCount: longword;
begin
  Result:=fList.Count;
end;

procedure TResources.SetCacheData(const aValue: boolean);
var i : integer;
begin
  if aValue=fCacheData then exit;
  fCacheData:=aValue;
  if fCacheData then exit;  //single resources cache data by default
  //don't cache data: load everything and free the temporary stream.
  for i:=0 to Count-1 do
    Items[i].CacheData:=aValue;
  if fTempRStream<>nil then FreeAndNil(fTempRStream);
end;

procedure TResources.NotifyLoaded;
var i : integer;
begin
  for i:=0 to Count-1 do
    Items[i].NotifyResourcesLoaded;
end;

class procedure TResources.InitReaderList;
begin
  if RegisteredReaders=nil then
    RegisteredReaders:=TFPList.Create;
end;

class procedure TResources.InitWriterList;
begin
  if RegisteredWriters=nil then
    RegisteredWriters:=TFPList.Create;
end;

class procedure TResources.DisposeStreamerList(aList: TFPList);
var p,p2 : PRegisteredStreamerEntry;
    i : integer;
begin
  if aList=nil then exit;
  for i:=0 to aList.Count-1 do
  begin
    p:=PRegisteredStreamerEntry(aList[i]);
    while p<>nil do
    begin
      p2:=p^.next;
      Freemem(p);
      p:=p2;
    end;
  end;
end;

class procedure TResources.DisposeReaderList;
begin
  DisposeStreamerList(RegisteredReaders);
  FreeAndNil(RegisteredReaders);
end;

class procedure TResources.DisposeWriterList;
begin
  DisposeStreamerList(RegisteredWriters);
  FreeAndNil(RegisteredWriters);
end;

class function TResources.FindReader(aStream: TStream; aExtension: string) :
  TAbstractResourceReader;
var i : integer;
    p : PRegisteredStreamerEntry;
    position : int64;
    found : boolean;
begin
  Result:=nil;
  InitReaderList;
  position:=aStream.Position;
  aExtension:=lowercase(aExtension);
  for i:=0 to RegisteredReaders.Count-1 do
  begin
    p:=PRegisteredStreamerEntry(RegisteredReaders[i]);
    if p^.ext=aExtension then //try all readers registered for this extension
    begin
      while p<>nil do
      begin
        Result:=TResourceReaderClass(p^._class).Create;
        found:=Result.CheckMagic(aStream);
        aStream.Position:=position; //rewind
        if found then exit;
        FreeAndNil(Result);
        p:=p^.next;
      end;
      // There are readers for this extension, but no one seems to be able
      // to read the file.
      // So, return the first reader, and it will fail later.
      p:=PRegisteredStreamerEntry(RegisteredReaders[i]);
      Result:=TResourceReaderClass(p^._class).Create;
      exit;
    end;
  end;
  raise EResourceReaderNotFoundException.Create(Format(SReaderNotFoundExt,[aExtension]));
end;

class function TResources.FindReader(aStream: TStream
  ): TAbstractResourceReader;
var i : integer;
    p : PRegisteredStreamerEntry;
    position : int64;
    found : boolean;
begin
  Result:=nil;
  InitReaderList;
  position:=aStream.Position;
  for i:=0 to RegisteredReaders.Count-1 do
  begin
    p:=PRegisteredStreamerEntry(RegisteredReaders[i]);
    while p<>nil do
    begin
      Result:=TResourceReaderClass(p^._class).Create;
      found:=Result.CheckMagic(aStream);
      aStream.Position:=position; //rewind
      if found then exit;
      FreeAndNil(Result);
      p:=p^.next;
    end;
  end;
  raise EResourceReaderNotFoundException.Create(SReaderNotFoundProbe);
end;

procedure TResources.MoveFrom(aResources: TResources);
var res : TAbstractResource;
    i : integer;
begin
  aResources.BeginMoveFrom;
  try
    for i:=0 to aResources.Count-1 do
    begin
      res:=aResources.Items[i];
      if res=nil then continue;
      if res.Owner<>nil then           //If we are adding an owned resource, add
        InternalAdd(res.Owner,0,false) //the owner resource instead (it will take
      else                             //care of adding its sub-resources)
        InternalAdd(res,i,true);
    end;
  finally
    aResources.EndMoveFrom;
  end;
end;

class function TResources.FindWriterClass(aExtension: string
  ): TResourceWriterClass;
var i : integer;
    p : PRegisteredStreamerEntry;
begin
  Result:=nil;
  InitWriterList;
  aExtension:=lowercase(aExtension);
  for i:=0 to RegisteredWriters.Count-1 do
  begin
    p:=PRegisteredStreamerEntry(RegisteredWriters[i]);
    if p^.ext=aExtension then
    begin
      Result:=TResourceWriterClass(p^._class);
      exit;
    end;
  end;
  raise EResourceWriterNotFoundException.Create(Format(SWriterNotFoundExt,[aExtension]));
end;

procedure TResources.InternalAdd(aResource : TAbstractResource; prevIdx :
  integer; prevIdxValid : boolean);
var resold : TAbstractResource;
begin
  resold:=InternalFind(aResource._Type,aResource.Name,aResource.LangID);
  if resold<>nil then
  begin
    if TResourceMerger.Merge(resold,aResource) then exit;
    raise EResourceDuplicateException.CreateFmt(SResDuplicate,[aResource._Type.Name,aResource.Name.Name,aResource.LangID]);
  end;
  fList.Add(aResource);
  TRootResTreeNode(fTree).Add(aResource);
  if aResource.OwnerList<>nil then
    aResource.OwnerList.QuietRemove(aResource,prevIdx,prevIdxValid);
  aResource.SetOwnerList(self);
  aResource.CacheData:=fCacheData;
end;

procedure TResources.AddNoTree(aResource: TAbstractResource);
begin
  fList.Add(aResource);
  aResource.SetOwnerList(self);
  aResource.CacheData:=fCacheData;
end;

function TResources.InternalFind(aType, aName: TResourceDesc;
  const aLangID: TLangID): TAbstractResource;
begin
  Result:=TRootResTreeNode(fTree).Find(aType,aName,aLangID);
end;

function TResources.InternalFind(aType, aName: TResourceDesc
  ): TAbstractResource;
begin
  Result:=TRootResTreeNode(fTree).Find(aType,aName);
end;

procedure TResources.BeginMoveFrom;
begin
  inc(fMoveFromCount);
  fRemovedCount:=0;
end;

procedure TResources.EndMoveFrom;
begin
  dec(fMoveFromCount);
  if fMoveFromCount=0 then
    if fRemovedCount=fList.Count then //all items removed: clear the list
      fList.Clear
    else
      fList.Pack; //for some reason, not all items were removed. remove only nils
end;

procedure TResources.Add(aResource: TAbstractResource);
begin
  InternalAdd(aResource,0,false);
end;

function TResources.AddAutoID(aResource: TAbstractResource): TResID;
var newid : TResID;
begin
  newid:=TRootResTreeNode(fTree).FindFreeID(aResource._Type);

  //if we reached this point, ENoMoreFreeIDsException hasn't been raised.
  if aResource.OwnerList<>nil then aResource.OwnerList.Remove(aResource);
  aResource.Name.ID:=newid;
  InternalAdd(aResource,0,false);
  Result:=newid;
end;

//clear without freeing fTempRStream
procedure TResources.InternalClear;
var i : integer;
begin
  TRootResTreeNode(fTree).Clear;
  for i:=0 to Count-1 do
    TAbstractResource(fList[i]).Free;
  fList.Clear;
end;

procedure TResources.Clear;
begin
  InternalClear;
  if fTempRStream<>nil then FreeAndNil(fTempRStream);
end;

function TResources.Find(aType, aName: TResourceDesc; const aLangID : TLangID):
  TAbstractResource;
begin
  Result:=TRootResTreeNode(fTree).Find(aType,aName,aLangID);
  if Result=nil then
    raise EResourceNotFoundException.CreateFmt(SResourceNotFoundLang,[aType.Name,aName.Name,aLangID]);
end;

function TResources.Find(aType, aName: TResourceDesc):
  TAbstractResource;
begin
  Result:=TRootResTreeNode(fTree).Find(aType,aName);
  if Result=nil then
    raise EResourceNotFoundException.CreateFmt(SResourceNotFound,[aType.Name,aName.Name]);
end;

function TResources.Find(const aType: TResName; const aName: TResName; const
  aLangID : TLangID): TAbstractResource;
begin
  dummyType.Name:=aType;
  dummyName.Name:=aName;
  Result:=Find(dummyType,dummyName,aLangID);
end;

function TResources.Find(const aType: TResName; const aName: TResID; const
  aLangID : TLangID  ): TAbstractResource;
begin
  dummyType.Name:=aType;
  dummyName.ID:=aName;
  Result:=Find(dummyType,dummyName,aLangID);
end;

function TResources.Find(const aType: TResID; const aName: TResName; const
  aLangID : TLangID  ): TAbstractResource;
begin
  dummyType.ID:=aType;
  dummyName.Name:=aName;
  Result:=Find(dummyType,dummyName,aLangID);
end;

function TResources.Find(const aType: TResID; const aName: TResID; const
  aLangID : TLangID  ): TAbstractResource;
begin
  dummyType.ID:=aType;
  dummyName.ID:=aName;
  Result:=Find(dummyType,dummyName,aLangID);
end;

function TResources.Find(const aType: TResName; const aName: TResName
  ): TAbstractResource;
begin
  dummyType.Name:=aType;
  dummyName.Name:=aName;
  Result:=Find(dummyType,dummyName);
end;

function TResources.Find(const aType: TResName; const aName: TResID
  ): TAbstractResource;
begin
  dummyType.Name:=aType;
  dummyName.ID:=aName;
  Result:=Find(dummyType,dummyName);
end;

function TResources.Find(const aType: TResID; const aName: TResName
  ): TAbstractResource;
begin
  dummyType.ID:=aType;
  dummyName.Name:=aName;
  Result:=Find(dummyType,dummyName);
end;

function TResources.Find(const aType: TResID; const aName: TResID
  ): TAbstractResource;
begin
  dummyType.ID:=aType;
  dummyName.ID:=aName;
  Result:=Find(dummyType,dummyName);
end;

function TResources.Remove(aType,aName : TResourceDesc;
  const aLangID : TLangID) : TAbstractResource;
begin
  Result:=TRootResTreeNode(fTree).Remove(aType,aName,aLangID);
  InternalRemove(Result);
  Result.SetOwnerList(nil);
end;

function TResources.Remove(aType,aName : TResourceDesc) : TAbstractResource;
begin
  Result:=TRootResTreeNode(fTree).Remove(aType,aName);
  InternalRemove(Result);
  Result.SetOwnerList(nil);
end;

function TResources.Remove(aResource: TAbstractResource) : TAbstractResource;
begin
  InternalRemove(aResource);
  Result:=TRootResTreeNode(fTree).Remove(aResource._Type,aResource.Name,aResource.LangID);
  Result.SetOwnerList(nil);
end;

function TResources.Remove(aIndex: integer): TAbstractResource;
begin
  Result:=Items[aIndex];
  InternalRemove(aIndex);
  Result:=TRootResTreeNode(fTree).Remove(Result._Type,Result.Name,Result.LangID);
  Result.SetOwnerList(nil);
end;

procedure TResources.InternalRemove(aResource: TAbstractResource);
var idx : integer;
begin
  if aResource=nil then exit;
  idx:=fList.IndexOf(aResource);
  if idx=-1 then
    raise EResourceNotFoundException.CreateFmt(SResourceNotFoundLang,[
      aResource._Type.Name,aResource.Name.Name,aResource.LangID]);
  if fMoveFromCount>0 then fList[idx]:=nil
  else fList.Delete(idx);
  inc(fRemovedCount);
end;

procedure TResources.InternalRemove(aIndex: integer);
begin
  if fMoveFromCount>0 then fList[aIndex]:=nil
  else fList.Delete(aIndex);
  inc(fRemovedCount);
end;

//removes without calling setownerlist
procedure TResources.QuietRemove(aResource : TAbstractResource; aIndex :
  integer; aIndexValid : boolean);
begin
  if aIndexValid then InternalRemove(aIndex)
  else InternalRemove(aResource);
  TRootResTreeNode(fTree).Remove(aResource._Type,aResource.Name,aResource.LangID);
end;

procedure TResources.LoadFromStream(aStream: TStream);
var aReader : TAbstractResourceReader;
begin
  aReader:=FindReader(aStream);
  try
    LoadFromStream(aStream,aReader);
  finally
    aReader.Free;
  end;
end;

procedure TResources.LoadFromStream(aStream: TStream;
  aReader: TAbstractResourceReader);
begin
  InternalClear;
  aReader.Load(self,aStream);
  NotifyLoaded;
end;

procedure TResources.LoadFromFile(aFileName: string);
var ext : string;
    aReader : TAbstractResourceReader;
begin
  ext:=ExtractFileExt(aFileName);
  if fTempRStream<>nil then FreeAndNil(fTempRStream);
  fTempRStream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  aReader:=FindReader(fTempRStream,ext);
  try
    LoadFromStream(fTempRStream,aReader);
  finally
    aReader.Free;
    if not fCacheData then FreeAndNil(fTempRStream);
  end;
end;

procedure TResources.LoadFromFile(aFileName: string;
  aReader: TAbstractResourceReader);
begin
  if fTempRStream<>nil then FreeAndNil(fTempRStream);
  fTempRStream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fTempRStream,aReader);
  finally
    if not fCacheData then FreeAndNil(fTempRStream);
  end;
end;

class procedure TResources.RegisterStreamer(aList : TFPList; aExtension :
  string; aClass : TClass);
var newp,p : PRegisteredStreamerEntry;
    i : integer;
begin
  aExtension:=lowercase(aExtension);
  newp:=GetMem(sizeof(TRegisteredStreamerEntry));
  newp^.next:=nil;
  newp^.ext:=aExtension;
  newp^._class:=aClass;

  for i:=0 to aList.Count-1 do
  begin
    p:=PRegisteredStreamerEntry(aList[i]);
    if p^.ext=aExtension then
    begin
      while p^.next<>nil do
        p:=p^.next;
      p^.next:=newp;
      exit;
    end;
  end;
  aList.Add(newp);
end;

procedure TResources.SendUpdateRawData;
var i : integer;
begin
  for i:=0 to Count-1 do
    Items[i].UpdateRawData;
end;

class procedure TResources.RegisterReader(const aExtension : string;
  aClass: TResourceReaderClass);
begin
  InitReaderList;
  RegisterStreamer(RegisteredReaders,aExtension,aClass);
end;

class procedure TResources.RegisterWriter(const aExtension : string;
  aClass: TResourceWriterClass);
begin
  InitWriterList;
  RegisterStreamer(RegisteredWriters,aExtension,aClass);
end;

procedure TResources.WriteToStream(aStream: TStream;
  aWriter: TAbstractResourceWriter);
begin
  SendUpdateRawData;
  aWriter.Write(self,aStream);
end;

procedure TResources.WriteToFile(aFileName: string);
var ext : string;
    aWriter : TAbstractResourceWriter;
begin
  ext:=ExtractFileExt(aFileName);
  aWriter:=FindWriterClass(ext).Create;
  try
    WriteToFile(aFileName,aWriter);
  finally
    aWriter.Free;
  end;
end;

procedure TResources.WriteToFile(aFileName: string;
  aWriter: TAbstractResourceWriter);
var OutStream : TFileStream;
begin
  OutStream:=TFileStream.Create(aFileName,fmCreate or fmShareDenyWrite);
  try
    WriteToStream(OutStream,aWriter);
  finally
    OutStream.Free;
  end;
end;

constructor TResources.Create;
begin
  fList:=TFPList.Create;
  fTree:=TRootResTreeNode.Create;
  dummyType:=TResourceDesc.Create;
  dummyName:=TResourceDesc.Create;
  fTempRStream:=nil;
  fCacheData:=true;
  fMoveFromCount:=0;
  fRemovedCount:=0;
end;

destructor TResources.Destroy;
begin
  Clear;
  fList.Free;
  fTree.Free;
  dummyType.Free;
  dummyName.Free;
end;

{ TAbstractResourceReader }

procedure TAbstractResourceReader.SetDataSize(aResource: TAbstractResource;
  aValue: longword);
begin
  aResource.fDataSize:=aValue;
end;

procedure TAbstractResourceReader.SetHeaderSize(aResource: TAbstractResource;
  aValue: longword);
begin
  aResource.fHeaderSize:=aValue;
end;

procedure TAbstractResourceReader.SetDataOffset(aResource: TAbstractResource;
  aValue: longword);
begin
  aResource.fDataOffset:=aValue;
end;

procedure TAbstractResourceReader.SetRawData(aResource: TAbstractResource;
  aStream: TStream);
begin
  if aResource.fRawData<>nil then aResource.fRawData.Free; //should never happen!
  aResource.fRawData:=aStream;
end;

procedure TAbstractResourceReader.CallSubReaderLoad(
  aReader: TAbstractResourceReader; aResources: TResources; aStream: TStream);
begin
  aReader.Load(aResources,aStream);
end;

procedure TAbstractResourceReader.AddNoTree(aResources: TResources;
  aResource: TAbstractResource);
begin
  aResources.AddNoTree(aResource);
end;

function TAbstractResourceReader.GetTree(aResources: TResources): TObject;
begin
  Result:=aResources.fTree;
end;

{ TGenericResource }

function TGenericResource.ChangeDescTypeAllowed(aDesc: TResourceDesc): boolean;
begin
  Result:=true;
end;

function TGenericResource.ChangeDescValueAllowed(aDesc: TResourceDesc
  ): boolean;
begin
  Result:=true;
end;

procedure TGenericResource.NotifyResourcesLoaded;
begin
end;

procedure TGenericResource.UpdateRawData;
begin
end;

function TGenericResource.GetType : TResourceDesc;
begin
  Result:=fType;
end;

function TGenericResource.GetName : TResourceDesc;
begin
  Result:=fName;
end;

constructor TGenericResource.Create(aType, aName: TResourceDesc);
begin
  Create;
  fType:=TResourceDesc.Create;
  fType.Assign(aType);
  fName:=TResourceDesc.Create;
  fName.Assign(aName);
  SetDescOwner(fType);
  SetDescOwner(fName);
end;

destructor TGenericResource.Destroy;
begin
  fName.Free;
  fType.Free;
  inherited Destroy;
end;

{ TAbstractResourceWriter }

function TAbstractResourceWriter.GetTree(aResources: TResources): TObject;
begin
  Result:=aResources.fTree;
end;

finalization
   TResources.DisposeReaderList;
   TResources.DisposeWriterList;

end.
