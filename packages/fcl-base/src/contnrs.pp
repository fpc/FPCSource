{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2002 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}
{$mode objfpc}
{$endif}
{$H+}
{$ifdef CLASSESINLINE}{$inline on}{$endif}

unit contnrs;

interface

uses
  SysUtils,Classes;


Type
  TObjectListCallback = procedure(data:TObject;arg:pointer) of object;
  TObjectListStaticCallback = procedure(data:TObject;arg:pointer);

  TFPObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FList: TFPList;
    function GetCount: integer;
    procedure SetCount(const AValue: integer);
  protected
    function GetItem(Index: Integer): TObject; {$ifdef CLASSESINLINE}inline;{$endif}
    procedure SetItem(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
    procedure SetCapacity(NewCapacity: Integer);
    function GetCapacity: integer;
  public
    constructor Create;
    constructor Create(FreeObjects : Boolean);
    destructor Destroy; override;
    procedure Clear;
    function Add(AObject: TObject): Integer; {$ifdef CLASSESINLINE}inline;{$endif}
    procedure Delete(Index: Integer); {$ifdef CLASSESINLINE}inline;{$endif}
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TFPObjectList;
    function Extract(Item: TObject): TObject;
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer;
    function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    procedure Insert(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
    function First: TObject;
    function Last: TObject;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign(Obj:TFPObjectList);
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure ForEachCall(proc2call:TObjectListCallback;arg:pointer);
    procedure ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TFPList read FList;
  end;


  TObjectList = class(TList)
  private
    ffreeobjects : boolean;
  Protected
    Procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetItem(Index: Integer): TObject;
    Procedure SetItem(Index: Integer; AObject: TObject);
  public
    constructor create;
    constructor create(freeobjects : boolean);
    function Add(AObject: TObject): Integer;
    function Extract(Item: TObject): TObject;
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer;
    function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Insert(Index: Integer; AObject: TObject);
    function First: TObject;
    Function Last: TObject;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  TComponentList = class(TObjectList)
  Private
    FNotifier : TComponent;
  Protected
    Procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    Function GetItems(Index: Integer): TComponent;
    Procedure SetItems(Index: Integer; AComponent: TComponent);
    Procedure HandleFreeNotify(Sender: TObject; AComponent: TComponent);
  public
    destructor Destroy; override;
    Function Add(AComponent: TComponent): Integer;
    Function Extract(Item: TComponent): TComponent;
    Function Remove(AComponent: TComponent): Integer;
    Function IndexOf(AComponent: TComponent): Integer;
    Function First: TComponent;
    Function Last: TComponent;
    Procedure Insert(Index: Integer; AComponent: TComponent);
    property Items[Index: Integer]: TComponent read GetItems write SetItems; default;
  end;

  TClassList = class(TList)
  protected
    Function GetItems(Index: Integer): TClass;
    Procedure SetItems(Index: Integer; AClass: TClass);
  public
    Function Add(AClass: TClass): Integer;
    Function Extract(Item: TClass): TClass;
    Function Remove(AClass: TClass): Integer;
    Function IndexOf(AClass: TClass): Integer;
    Function First: TClass;
    Function Last: TClass;
    Procedure Insert(Index: Integer; AClass: TClass);
    property Items[Index: Integer]: TClass read GetItems write SetItems; default;
  end;

  TOrderedList = class(TObject)
  private
    FList: TList;
  protected
    Procedure PushItem(AItem: Pointer); virtual; abstract;
    Function PopItem: Pointer; virtual;
    Function PeekItem: Pointer; virtual;
    property List: TList read FList;
  public
    constructor Create;
    destructor Destroy; override;
    Function Count: Integer;
    Function AtLeast(ACount: Integer): Boolean;
    Function Push(AItem: Pointer): Pointer;
    Function Pop: Pointer;
    Function Peek: Pointer;
  end;

{ TStack class }

  TStack = class(TOrderedList)
  protected
    Procedure PushItem(AItem: Pointer); override;
  end;

{ TObjectStack class }

  TObjectStack = class(TStack)
  public
    Function Push(AObject: TObject): TObject;
    Function Pop: TObject;
    Function Peek: TObject;
  end;

{ TQueue class }

  TQueue = class(TOrderedList)
  protected
    Procedure PushItem(AItem: Pointer); override;
  end;

{ TObjectQueue class }

  TObjectQueue = class(TQueue)
  public
    Function Push(AObject: TObject): TObject;
    Function Pop: TObject;
    Function Peek: TObject;
  end;

{ ---------------------------------------------------------------------
    TFPList with Hash support
  ---------------------------------------------------------------------}

type
  THashItem=record
    HashValue : LongWord;
    StrIndex  : Integer;
    NextIndex : Integer;
    Data      : Pointer;
  end;
  PHashItem=^THashItem;

const
  MaxHashListSize = Maxint div 16;
  MaxHashStrSize  = Maxint;
  MaxHashTableSize = Maxint div 4;
  MaxItemsPerHash = 3;

type
  PHashItemList = ^THashItemList;
  THashItemList = array[0..MaxHashListSize - 1] of THashItem;
  PHashTable = ^THashTable;
  THashTable = array[0..MaxHashTableSize - 1] of Integer;

  TFPHashList = class(TObject)
  private
    { ItemList }
    FHashList     : PHashItemList;
    FCount,
    FCapacity : Integer;
    { Hash }
    FHashTable    : PHashTable;
    FHashCapacity : Integer;
    { Strings }
    FStrs     : PChar;
    FStrCount,
    FStrCapacity : Integer;
    function InternalFind(AHash:LongWord;const AName:shortstring;out PrevIndex:Integer):Integer;
  protected
    function Get(Index: Integer): Pointer; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Put(Index: Integer; Item: Pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    Procedure RaiseIndexError(Index : Integer);
    function  AddStr(const s:shortstring): Integer;
    procedure AddToHashTable(Index: Integer);
    procedure StrExpand(MinIncSize:Integer);
    procedure SetStrCapacity(NewCapacity: Integer);
    procedure SetHashCapacity(NewCapacity: Integer);
    procedure ReHash;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName:shortstring;Item: Pointer): Integer;
    procedure Clear;
    function NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
    function HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: PtrInt);
    function Expand: TFPHashList;
    function Extract(item: Pointer): Pointer;
    function IndexOf(Item: Pointer): Integer;
    function Find(const AName:shortstring): Pointer;
    function FindIndexOf(const AName:shortstring): Integer;
    function FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
    function Rename(const AOldName,ANewName:shortstring): Integer;
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure ShowStatistics;
    procedure ForEachCall(proc2call:TListCallback;arg:pointer);
    procedure ForEachCall(proc2call:TListStaticCallback;arg:pointer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PHashItemList read FHashList;
    property Strs: PChar read FStrs;
  end;


{*******************************************************
        TFPHashObjectList (From fcl/inc/contnrs.pp)
********************************************************}

  TFPHashObjectList = class;

  { TFPHashObject }

  TFPHashObject = class
  private
    FOwner     : TFPHashObjectList;
    FCachedStr : pshortstring;
    FStrIndex  : Integer;
    procedure InternalChangeOwner(HashObjectList:TFPHashObjectList;const s:shortstring);
  protected
    function GetName:shortstring;virtual;
    function GetHash:Longword;virtual;
  public
    constructor CreateNotOwned;
    constructor Create(HashObjectList:TFPHashObjectList;const s:shortstring);
    procedure ChangeOwner(HashObjectList:TFPHashObjectList); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ChangeOwnerAndName(HashObjectList:TFPHashObjectList;const s:shortstring); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Rename(const ANewName:shortstring);
    property Name:shortstring read GetName;
    property Hash:Longword read GetHash;
  end;

  TFPHashObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FHashList: TFPHashList;
    function GetCount: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetCount(const AValue: integer); {$ifdef CCLASSESINLINE}inline;{$endif}
  protected
    function GetItem(Index: Integer): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetItem(Index: Integer; AObject: TObject); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetCapacity(NewCapacity: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    function GetCapacity: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
  public
    constructor Create(FreeObjects : boolean = True);
    destructor Destroy; override;
    procedure Clear;
    function Add(const AName:shortstring;AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
    function HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Delete(Index: Integer);
    function Expand: TFPHashObjectList; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Extract(Item: TObject): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Find(const s:shortstring): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindIndexOf(const s:shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
    function Rename(const AOldName,ANewName:shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    procedure Pack; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ShowStatistics; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ForEachCall(proc2call:TObjectListCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TFPHashList read FHashList;
  end;

{ ---------------------------------------------------------------------
    Hash support, implemented by Dean Zobec
  ---------------------------------------------------------------------}


  { Must return a Longword value in the range 0..TableSize,
   usually via a mod operator;  }
  THashFunction = function(const S: string; const TableSize: Longword): Longword;


  { THTNode }

  THTCustomNode = class(TObject)
  private
    FKey: string;
  public
    constructor CreateWith(const AString: String);
    function HasKey(const AKey: string): boolean;
    property Key: string read FKey;
  end;
  THTCustomNodeClass = Class of THTCustomNode;


  { TFPCustomHashTable }

  TFPCustomHashTable = class(TObject)
  private
    FHashTable: TFPObjectList;
    FHashTableSize: Longword;
    FHashFunction: THashFunction;
    FCount: Longword;
    function GetDensity: Longword;
    function GetNumberOfCollisions: Longword;
    procedure SetHashTableSize(const Value: Longword);
    procedure InitializeHashTable;
    function GetVoidSlots: Longword;
    function GetLoadFactor: double;
    function GetAVGChainLen: double;
    function GetMaxChainLength: Longword;
    function Chain(const index: Longword):TFPObjectList;
  protected
    Function CreateNewNode(const aKey : string) : THTCustomNode; virtual; abstract;
    Procedure AddNode(ANode : THTCustomNode); virtual; abstract;
    function ChainLength(const ChainIndex: Longword): Longword; virtual;
    function FindOrCreateNew(const aKey: string): THTCustomNode; virtual;
    procedure SetHashFunction(AHashFunction: THashFunction); virtual;
    Function FindChainForAdd(Const aKey : String) : TFPObjectList;
  public
    constructor Create;
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction);
    destructor Destroy; override;
    procedure ChangeTableSize(const ANewSize: Longword); virtual;
    procedure Clear; virtual;
    procedure Delete(const aKey: string); virtual;
    function Find(const aKey: string): THTCustomNode;
    function IsEmpty: boolean;
    property HashFunction: THashFunction read FHashFunction write SetHashFunction;
    property Count: Longword read FCount;
    property HashTableSize: Longword read FHashTableSize write SetHashTableSize;
    property HashTable: TFPObjectList read FHashTable;
    property VoidSlots: Longword read GetVoidSlots;
    property LoadFactor: double read GetLoadFactor;
    property AVGChainLen: double read GetAVGChainLen;
    property MaxChainLength: Longword read GetMaxChainLength;
    property NumberOfCollisions: Longword read GetNumberOfCollisions;
    property Density: Longword read GetDensity;
  end;

  { TFPDataHashTable : Hash table with simple data pointers }

  THTDataNode = Class(THTCustomNode)
  Private
    FData: pointer;
  public
    property Data: pointer read FData write FData;
  end;
  // For compatibility
  THTNode = THTDataNode;

  TDataIteratorMethod = procedure(Item: Pointer; const Key: string; var Continue: Boolean) of object;
  // For compatibility
  TIteratorMethod = TDataIteratorMethod;

  TFPDataHashTable = Class(TFPCustomHashTable)
  Protected
    Function CreateNewNode(const aKey : String) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    procedure SetData(const index: string; const AValue: Pointer); virtual;
    function GetData(const index: string):Pointer; virtual;
    function ForEachCall(aMethod: TDataIteratorMethod): THTDataNode; virtual;
  Public
    procedure Add(const aKey: string; AItem: pointer); virtual;
    property Items[const index: string]: Pointer read GetData write SetData; default;
  end;

  { TFPStringHashTable : Hash table with simple strings as data }
  THTStringNode = Class(THTCustomNode)
  Private
    FData : String;
  public
    property Data: String read FData write FData;
  end;
  TStringIteratorMethod = procedure(Item: String; const Key: string; var Continue: Boolean) of object;

  TFPStringHashTable = Class(TFPCustomHashTable)
  Protected
    Function CreateNewNode(const aKey : String) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    procedure SetData(const Index, AValue: string); virtual;
    function GetData(const index: string): String; virtual;
    function ForEachCall(aMethod: TStringIteratorMethod): THTStringNode; virtual;
  Public
    procedure Add(const aKey,aItem: string); virtual;
    property Items[const index: string]: String read GetData write SetData; default;
  end;

  { TFPStringHashTable : Hash table with simple strings as data }


  THTObjectNode = Class(THTCustomNode)
  Private
    FData : TObject;
  public
    property Data: TObject read FData write FData;
  end;

  THTOwnedObjectNode = Class(THTObjectNode)
  public
    Destructor Destroy; override;
  end;
  TObjectIteratorMethod = procedure(Item: TObject; const Key: string; var Continue: Boolean) of object;

  TFPObjectHashTable = Class(TFPCustomHashTable)
  Private
    FOwnsObjects : Boolean;
  Protected
    Function CreateNewNode(const aKey : String) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    procedure SetData(const Index: string; AObject : TObject); virtual;
    function GetData(const index: string): TObject; virtual;
    function ForEachCall(aMethod: TObjectIteratorMethod): THTObjectNode; virtual;
  Public
    constructor Create(AOwnsObjects : Boolean = True);
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction; AOwnsObjects : Boolean = True);
    procedure Add(const aKey: string; AItem : TObject); virtual;
    property Items[const index: string]: TObject read GetData write SetData; default;
    Property OwnsObjects : Boolean Read FOwnsObjects Write FOwnsObjects;
  end;

  EDuplicate = class(Exception);
  EKeyNotFound = class(Exception);

  function RSHash(const S: string; const TableSize: Longword): Longword;

{ ---------------------------------------------------------------------
    Bucket lists as in Delphi
  ---------------------------------------------------------------------}
  

Type
  TBucketItem = record
    Item, Data: Pointer;
  end;
  TBucketItemArray = array of TBucketItem;

  TBucket = record
    Count : Integer;
    Items : TBucketItemArray;
  end;
  PBucket = ^TBucket;
  TBucketArray = array of TBucket;

  TBucketProc = procedure(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
  TBucketProcObject = procedure(AItem, AData: Pointer; out AContinue: Boolean) of Object;

{ ---------------------------------------------------------------------
  TCustomBucketList
  ---------------------------------------------------------------------}

  { TCustomBucketList }

  TCustomBucketList = class(TObject)
  private
    FBuckets: TBucketArray;
    function GetBucketCount: Integer;
    function GetData(AItem: Pointer): Pointer;
    procedure SetData(AItem: Pointer; const AData: Pointer);
    procedure SetBucketCount(const Value: Integer);
  protected
    Procedure GetBucketItem(AItem: Pointer; out ABucket, AIndex: Integer);
    function AddItem(ABucket: Integer; AItem, AData: Pointer): Pointer; virtual;
    function BucketFor(AItem: Pointer): Integer; virtual; abstract;
    function DeleteItem(ABucket: Integer; AIndex: Integer): Pointer; virtual;
    Procedure Error(Msg : String; Args : Array of Const);
    function FindItem(AItem: Pointer; out ABucket, AIndex: Integer): Boolean; virtual;
    property Buckets: TBucketArray read FBuckets;
    property BucketCount: Integer read GetBucketCount write SetBucketCount;
  public
    destructor Destroy; override;
    procedure Clear;
    function Add(AItem, AData: Pointer): Pointer;
    procedure Assign(AList: TCustomBucketList);
    function Exists(AItem: Pointer): Boolean;
    function Find(AItem: Pointer; out AData: Pointer): Boolean;
    function ForEach(AProc: TBucketProc; AInfo: Pointer = nil): Boolean;
    function ForEach(AProc: TBucketProcObject): Boolean;
    function Remove(AItem: Pointer): Pointer;
    property Data[AItem: Pointer]: Pointer read GetData write SetData; default;
  end;

{ ---------------------------------------------------------------------
  TBucketList
  ---------------------------------------------------------------------}
  

  TBucketListSizes = (bl2, bl4, bl8, bl16, bl32, bl64, bl128, bl256);

  { TBucketList }

  TBucketList = class(TCustomBucketList)
  private
    FBucketMask: Byte;
  protected
    function BucketFor(AItem: Pointer): Integer; override;
  public
    constructor Create(ABuckets: TBucketListSizes = bl16);
  end;

{ ---------------------------------------------------------------------
  TObjectBucketList
  ---------------------------------------------------------------------}
  
  { TObjectBucketList }

  TObjectBucketList = class(TBucketList)
  protected
    function GetData(AItem: TObject): TObject;
    procedure SetData(AItem: TObject; const AData: TObject);
  public
    function Add(AItem, AData: TObject): TObject;
    function Remove(AItem: TObject): TObject;
    property Data[AItem: TObject]: TObject read GetData write SetData; default;
  end;


implementation

uses
  RtlConsts;

ResourceString
  DuplicateMsg   = 'An item with key %0:s already exists';
  KeyNotFoundMsg = 'Method: %0:s key [''%1:s''] not found in container';
  NotEmptyMsg    = 'Hash table not empty.';
  SErrNoSuchItem = 'No item in list for %p';
  SDuplicateItem = 'Item already exists in list: %p';

const
  NPRIMES = 28;

  PRIMELIST: array[0 .. NPRIMES-1] of Longword =
  ( 53,         97,         193,       389,       769,
    1543,       3079,       6151,      12289,     24593,
    49157,      98317,      196613,    393241,    786433,
    1572869,    3145739,    6291469,   12582917,  25165843,
    50331653,   100663319,  201326611, 402653189, 805306457,
    1610612741, 3221225473, 4294967291 );

constructor TFPObjectList.Create(FreeObjects : boolean);
begin
  Create;
  FFreeObjects := Freeobjects;
end;

destructor TFPObjectList.Destroy;
begin
  if (FList <> nil) then
  begin
    Clear;
    FList.Destroy;
  end;
  inherited Destroy;
end;

procedure TFPObjectList.Clear;
var
  i: integer;
begin
  if FFreeObjects then
    for i := 0 to FList.Count - 1 do
      TObject(FList[i]).Free;
  FList.Clear;
end;

constructor TFPObjectList.Create;
begin
  inherited Create;
  FList := TFPList.Create;
  FFreeObjects := True;
end;

function TFPObjectList.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TFPObjectList.SetCount(const AValue: integer);
begin
  if FList.Count <> AValue then
    FList.Count := AValue;
end;

function TFPObjectList.GetItem(Index: Integer): TObject; {$ifdef CLASSESINLINE}inline;{$endif}
begin
  Result := TObject(FList[Index]);
end;

procedure TFPObjectList.SetItem(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
begin
  if OwnsObjects then
    TObject(FList[Index]).Free;
  FList[index] := AObject;
end;

procedure TFPObjectList.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity := NewCapacity;
end;

function TFPObjectList.GetCapacity: integer;
begin
  Result := FList.Capacity;
end;

function TFPObjectList.Add(AObject: TObject): Integer; {$ifdef CLASSESINLINE}inline;{$endif}
begin
  Result := FList.Add(AObject);
end;

procedure TFPObjectList.Delete(Index: Integer); {$ifdef CLASSESINLINE}inline;{$endif}
begin
  if OwnsObjects then
    TObject(FList[Index]).Free;
  FList.Delete(Index);
end;

procedure TFPObjectList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TFPObjectList.Expand: TFPObjectList;
begin
  FList.Expand;
  Result := Self;
end;

function TFPObjectList.Extract(Item: TObject): TObject;
begin
  Result := TObject(FList.Extract(Item));
end;

function TFPObjectList.Remove(AObject: TObject): Integer;
begin
  Result := IndexOf(AObject);
  if (Result <> -1) then
  begin
    if OwnsObjects then
      TObject(FList[Result]).Free;
    FList.Delete(Result);
  end;
end;

function TFPObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result := FList.IndexOf(Pointer(AObject));
end;

function TFPObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  If AExact then
    while (I<Count) and (Result=-1) do
      If Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      If Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;

procedure TFPObjectList.Insert(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
begin
  FList.Insert(Index, Pointer(AObject));
end;

procedure TFPObjectList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TFPObjectList.Assign(Obj: TFPObjectList);
var
  i: Integer;
begin
  Clear;
  for I := 0 to Obj.Count - 1 do
    Add(Obj[i]);
end;

procedure TFPObjectList.Pack;
begin
  FList.Pack;
end;

procedure TFPObjectList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

function TFPObjectList.First: TObject;
begin
  Result := TObject(FList.First);
end;

function TFPObjectList.Last: TObject;
begin
  Result := TObject(FList.Last);
end;

procedure TFPObjectList.ForEachCall(proc2call:TObjectListCallback;arg:pointer);
begin
  FList.ForEachCall(TListCallBack(proc2call),arg);
end;

procedure TFPObjectList.ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer);
begin
  FList.ForEachCall(TListStaticCallBack(proc2call),arg);
end;


{ TObjectList }

constructor tobjectlist.create(freeobjects : boolean);

begin
  inherited create;
  ffreeobjects:=freeobjects;
end;

Constructor tobjectlist.create;

begin
  inherited create;
  ffreeobjects:=True;
end;

Procedure TObjectList.Notify(Ptr: Pointer; Action: TListNotification);

begin
  if FFreeObjects then
    if (Action=lnDeleted) then
      TObject(Ptr).Free;
  inherited Notify(Ptr,Action);
end;


Function TObjectList.GetItem(Index: Integer): TObject;

begin
  Result:=TObject(Inherited Get(Index));
end;


Procedure TObjectList.SetItem(Index: Integer; AObject: TObject);

begin
  // Put will take care of deleting old one in Notify.
  Put(Index,Pointer(AObject));
end;


Function TObjectList.Add(AObject: TObject): Integer;

begin
  Result:=Inherited Add(Pointer(AObject));
end;


Function TObjectList.Extract(Item: TObject): TObject;

begin
  Result:=Tobject(Inherited Extract(Pointer(Item)));
end;


Function TObjectList.Remove(AObject: TObject): Integer;

begin
  Result:=Inherited Remove(Pointer(AObject));
end;


Function TObjectList.IndexOf(AObject: TObject): Integer;

begin
  Result:=Inherited indexOF(Pointer(AObject));
end;


Function TObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;

Var
  I : Integer;

begin
  I:=AStartAt;
  Result:=-1;
  If AExact then
    While (I<Count) and (Result=-1) do
      If Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    While (I<Count) and (Result=-1) do
      If Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;


procedure TObjectList.Insert(Index: Integer; AObject: TObject);
begin
  Inherited Insert(Index,Pointer(AObject));
end;


function TObjectList.First: TObject;

begin
  Result := TObject(Inherited First);
end;


function TObjectList.Last: TObject;

begin
  Result := TObject(Inherited Last);
end;

{ TListComponent }

Type
  TlistComponent = Class(TComponent)
  Private
    Flist : TComponentList;
  Public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

procedure TlistComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  If (Operation=opremove) then
    Flist.HandleFreeNotify(Self,AComponent);
  inherited;
end;

{ TComponentList }

Function TComponentList.Add(AComponent: TComponent): Integer;
begin
  Result:=Inherited Add(AComponent);
end;

destructor TComponentList.Destroy;
begin
  inherited;
  FreeAndNil(FNotifier);
end;

Function TComponentList.Extract(Item: TComponent): TComponent;
begin
  Result:=TComponent(Inherited Extract(Item));
end;

Function TComponentList.First: TComponent;
begin
  Result:=TComponent(Inherited First);
end;

Function TComponentList.GetItems(Index: Integer): TComponent;
begin
  Result:=TComponent(Inherited Items[Index]);
end;

Procedure TComponentList.HandleFreeNotify(Sender: TObject;
  AComponent: TComponent);
begin
  Extract(Acomponent);
end;

Function TComponentList.IndexOf(AComponent: TComponent): Integer;
begin
  Result:=Inherited IndexOf(AComponent);
end;

Procedure TComponentList.Insert(Index: Integer; AComponent: TComponent);
begin
  Inherited Insert(Index,Acomponent)
end;

Function TComponentList.Last: TComponent;
begin
  Result:=TComponent(Inherited Last);
end;

Procedure TComponentList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  If FNotifier=NIl then
    begin
    FNotifier:=TlistComponent.Create(nil);
    TlistComponent(FNotifier).FList:=Self;
    end;
  If Assigned(Ptr) then
    With TComponent(Ptr) do
      case Action of
        lnAdded : FreeNotification(FNotifier);
        lnExtracted, lnDeleted: RemoveFreeNotification(FNotifier);
      end;
  inherited Notify(Ptr, Action);
end;

Function TComponentList.Remove(AComponent: TComponent): Integer;
begin
  Result:=Inherited Remove(AComponent);
end;

Procedure TComponentList.SetItems(Index: Integer; AComponent: TComponent);
begin
  Put(Index,AComponent);
end;

{ TClassList }

Function TClassList.Add(AClass: TClass): Integer;
begin
  Result:=Inherited Add(Pointer(AClass));
end;

Function TClassList.Extract(Item: TClass): TClass;
begin
  Result:=TClass(Inherited Extract(Pointer(Item)));
end;

Function TClassList.First: TClass;
begin
  Result:=TClass(Inherited First);
end;

Function TClassList.GetItems(Index: Integer): TClass;
begin
  Result:=TClass(Inherited Items[Index]);
end;

Function TClassList.IndexOf(AClass: TClass): Integer;
begin
  Result:=Inherited IndexOf(Pointer(AClass));
end;

Procedure TClassList.Insert(Index: Integer; AClass: TClass);
begin
  Inherited Insert(index,Pointer(AClass));
end;

Function TClassList.Last: TClass;
begin
  Result:=TClass(Inherited Last);
end;

Function TClassList.Remove(AClass: TClass): Integer;
begin
  Result:=Inherited Remove(Pointer(AClass));
end;

Procedure TClassList.SetItems(Index: Integer; AClass: TClass);
begin
  Put(Index,Pointer(Aclass));
end;

{ TOrderedList }

Function TOrderedList.AtLeast(ACount: Integer): Boolean;
begin
  Result:=(FList.Count>=Acount)
end;

Function TOrderedList.Count: Integer;
begin
  Result:=FList.Count;
end;

constructor TOrderedList.Create;
begin
  FList:=Tlist.Create;
end;

destructor TOrderedList.Destroy;
begin
  FList.Free;
end;

Function TOrderedList.Peek: Pointer;
begin
  If AtLeast(1) then
    Result:=PeekItem
  else
    Result:=Nil;
end;

Function TOrderedList.PeekItem: Pointer;
begin
  With Flist do
    Result:=Items[Count-1]
end;

Function TOrderedList.Pop: Pointer;
begin
  If Atleast(1) then
    Result:=PopItem
  else
    Result:=Nil;
end;

Function TOrderedList.PopItem: Pointer;
begin
  With FList do
    If Count>0 then
      begin
      Result:=Items[Count-1];
      Delete(Count-1);
      end
    else
      Result:=Nil;
end;

Function TOrderedList.Push(AItem: Pointer): Pointer;
begin
  PushItem(Aitem);
  Result:=AItem;
end;

{ TStack }

Procedure TStack.PushItem(AItem: Pointer);
begin
  FList.Add(Aitem);
end;

{ TObjectStack }

Function TObjectStack.Peek: TObject;
begin
  Result:=TObject(Inherited Peek);
end;

Function TObjectStack.Pop: TObject;
begin
  Result:=TObject(Inherited Pop);
end;

Function TObjectStack.Push(AObject: TObject): TObject;
begin
  Result:=TObject(Inherited Push(Pointer(AObject)));
end;

{ TQueue }

Procedure TQueue.PushItem(AItem: Pointer);
begin
  With Flist Do
    Insert(0,AItem);
end;

{ TObjectQueue }

Function TObjectQueue.Peek: TObject;
begin
  Result:=TObject(Inherited Peek);
end;

Function TObjectQueue.Pop: TObject;
begin
  Result:=TObject(Inherited Pop);
end;

Function TObjectQueue.Push(AObject: TObject): TObject;
begin
  Result:=TObject(Inherited Push(Pointer(Aobject)));
end;


{*****************************************************************************
                            TFPHashList
*****************************************************************************}

    function FPHash1(const s:shortstring):LongWord;
      Var
        g : LongWord;
        p,pmax : pchar;
      begin
        result:=0;
        p:=@s[1];
        pmax:=@s[length(s)+1];
        while (p<pmax) do
          begin
            result:=result shl 4 + LongWord(p^);
            g:=result and LongWord($F0000000);
            if g<>0 then
              result:=result xor (g shr 24) xor g;
            inc(p);
          end;
        If result=0 then
          result:=$ffffffff;
      end;

    function FPHash(const s:shortstring):LongWord;
      Var
        p,pmax : pchar;
      begin
{$ifopt Q+}
{$define overflowon}
{$Q-}
{$endif}
        result:=0;
        p:=@s[1];
        pmax:=@s[length(s)+1];
        while (p<pmax) do
          begin
            result:=LongWord((result shl 5) - result) xor LongWord(P^);
            inc(p);
          end;
{$ifdef overflowon}
{$Q+}
{$undef overflowon}
{$endif}
      end;


procedure TFPHashList.RaiseIndexError(Index : Integer);
begin
  Error(SListIndexError, Index);
end;


function TFPHashList.Get(Index: Integer): Pointer;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FHashList^[Index].Data;
end;


procedure TFPHashList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  FHashList^[Index].Data:=Item;;
end;


function TFPHashList.NameOfIndex(Index: Integer): shortstring;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  with FHashList^[Index] do
    begin
      if StrIndex>=0 then
        Result:=PShortString(@FStrs[StrIndex])^
      else
        Result:='';
    end;
end;


function TFPHashList.HashOfIndex(Index: Integer): LongWord;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FHashList^[Index].HashValue;
end;


function TFPHashList.Extract(item: Pointer): Pointer;
var
  i : Integer;
begin
  result := nil;
  i := IndexOf(item);
  if i >= 0 then
   begin
     Result := item;
     Delete(i);
   end;
end;


procedure TFPHashList.SetCapacity(NewCapacity: Integer);
begin
  If (NewCapacity < FCount) or (NewCapacity > MaxHashListSize) then
     Error (SListCapacityError, NewCapacity);
  if NewCapacity = FCapacity then
    exit;
  ReallocMem(FHashList, NewCapacity*SizeOf(THashItem));
  FCapacity := NewCapacity;
end;


procedure TFPHashList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxHashListSize)then
    Error(SListCountError, NewCount);
  If NewCount > FCount then
    begin
      If NewCount > FCapacity then
        SetCapacity(NewCount);
      If FCount < NewCount then
        FillChar(FHashList^[FCount], (NewCount-FCount) div Sizeof(THashItem), 0);
    end;
  FCount := Newcount;
end;


procedure TFPHashList.SetStrCapacity(NewCapacity: Integer);
begin
  If (NewCapacity < FStrCount) or (NewCapacity > MaxHashStrSize) then
     Error (SListCapacityError, NewCapacity);
  if NewCapacity = FStrCapacity then
    exit;
  ReallocMem(FStrs, NewCapacity);
  FStrCapacity := NewCapacity;
end;


procedure TFPHashList.SetHashCapacity(NewCapacity: Integer);
begin
  If (NewCapacity < 1) then
    Error (SListCapacityError, NewCapacity);
  if FHashCapacity=NewCapacity then
    exit;
  FHashCapacity:=NewCapacity;
  ReallocMem(FHashTable, FHashCapacity*sizeof(Integer));
  ReHash;
end;


procedure TFPHashList.ReHash;
var
  i : Integer;
begin
  FillDword(FHashTable^,FHashCapacity,LongWord(-1));
  For i:=0 To FCount-1 Do
    AddToHashTable(i);
end;


constructor TFPHashList.Create;
begin
  SetHashCapacity(1);
end;


destructor TFPHashList.Destroy;
begin
  Clear;
  if assigned(FHashTable) then
    FreeMem(FHashTable);
  inherited Destroy;
end;


function TFPHashList.AddStr(const s:shortstring): Integer;
var
  Len : Integer;
begin
  len:=length(s)+1;
  if FStrCount+Len >= FStrCapacity then
    StrExpand(Len);
  System.Move(s[0],FStrs[FStrCount],Len);
  result:=FStrCount;
  inc(FStrCount,Len);
end;


procedure TFPHashList.AddToHashTable(Index: Integer);
var
  HashIndex : Integer;
begin
  with FHashList^[Index] do
    begin
      if not assigned(Data) then
        exit;
      HashIndex:=HashValue mod LongWord(FHashCapacity);
      NextIndex:=FHashTable^[HashIndex];
      FHashTable^[HashIndex]:=Index;
    end;
end;


function TFPHashList.Add(const AName:shortstring;Item: Pointer): Integer;
begin
  if FCount = FCapacity then
    Expand;
  with FHashList^[FCount] do
    begin
      HashValue:=FPHash(AName);
      Data:=Item;
      StrIndex:=AddStr(AName);
    end;
  AddToHashTable(FCount);
  Result := FCount;
  inc(FCount);
end;

procedure TFPHashList.Clear;
begin
  if Assigned(FHashList) then
    begin
      FCount:=0;
      SetCapacity(0);
      FHashList := nil;
    end;
  SetHashCapacity(1);
  FHashTable^[0]:=longword(-1); // sethashcapacity does not always call rehash
  if Assigned(FStrs) then
    begin
      FStrCount:=0;
      SetStrCapacity(0);
      FStrs := nil;
    end;
end;

procedure TFPHashList.Delete(Index: Integer);
begin
  If (Index<0) or (Index>=FCount) then
    Error (SListIndexError, Index);
  { Remove from HashList }
  dec(FCount);
  System.Move (FHashList^[Index+1], FHashList^[Index], (FCount - Index) * Sizeof(THashItem));
  { All indexes are updated, we need to build the hashtable again }
  Rehash;
  { Shrink the list if appropriate }
  if (FCapacity > 256) and (FCount < FCapacity shr 2) then
    begin
      FCapacity := FCapacity shr 1;
      ReallocMem(FHashList, Sizeof(THashItem) * FCapacity);
    end;
end;

function TFPHashList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  If Result <> -1 then
    Self.Delete(Result);
end;

class procedure TFPHashList.Error(const Msg: string; Data: PtrInt);
begin
  Raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
end;

function TFPHashList.Expand: TFPHashList;
var
  IncSize : Longint;
begin
  Result := Self;
  if FCount < FCapacity then
    exit;
  IncSize := sizeof(ptrint)*2;
  if FCapacity > 127 then
    Inc(IncSize, FCapacity shr 2)
  else if FCapacity > sizeof(ptrint)*3 then
    Inc(IncSize, FCapacity shr 1)
  else if FCapacity >= sizeof(ptrint) then
    inc(IncSize,sizeof(ptrint));
  SetCapacity(FCapacity + IncSize);
  { Maybe expand hash also }
  if FCount>FHashCapacity*MaxItemsPerHash then
    SetHashCapacity(FCount div MaxItemsPerHash);
end;

procedure TFPHashList.StrExpand(MinIncSize:Integer);
var
  IncSize : Longint;
begin
  if FStrCount+MinIncSize < FStrCapacity then
    exit;
  IncSize := 64;
  if FStrCapacity > 255 then
    Inc(IncSize, FStrCapacity shr 2);
  SetStrCapacity(FStrCapacity + IncSize + MinIncSize);
end;

function TFPHashList.IndexOf(Item: Pointer): Integer;
var
  psrc  : PHashItem;
  Index : integer;
begin
  Result:=-1;
  psrc:=@FHashList^[0];
  For Index:=0 To FCount-1 Do
    begin
      if psrc^.Data=Item then
        begin
          Result:=Index;
          exit;
        end;
      inc(psrc);
    end;
end;

function TFPHashList.InternalFind(AHash:LongWord;const AName:shortstring;out PrevIndex:Integer):Integer;
var
  HashIndex : Integer;
  Len,
  LastChar  : Char;
begin
  HashIndex:=AHash mod LongWord(FHashCapacity);
  Result:=FHashTable^[HashIndex];
  Len:=Char(Length(AName));
  LastChar:=AName[Byte(Len)];
  PrevIndex:=-1;
  while Result<>-1 do
    begin
      with FHashList^[Result] do
        begin
          if assigned(Data) and
             (HashValue=AHash) and
             (Len=FStrs[StrIndex]) and
             (LastChar=FStrs[StrIndex+Byte(Len)]) and
             (AName=PShortString(@FStrs[StrIndex])^) then
            exit;
          PrevIndex:=Result;
          Result:=NextIndex;
        end;
    end;
end;


function TFPHashList.Find(const AName:shortstring): Pointer;
var
  Index,
  PrevIndex : Integer;
begin
  Result:=nil;
  Index:=InternalFind(FPHash(AName),AName,PrevIndex);
  if Index=-1 then
    exit;
  Result:=FHashList^[Index].Data;
end;


function TFPHashList.FindIndexOf(const AName:shortstring): Integer;
var
  PrevIndex : Integer;
begin
  Result:=InternalFind(FPHash(AName),AName,PrevIndex);
end;


function TFPHashList.FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
var
  Index,
  PrevIndex : Integer;
begin
  Result:=nil;
  Index:=InternalFind(AHash,AName,PrevIndex);
  if Index=-1 then
    exit;
  Result:=FHashList^[Index].Data;
end;


function TFPHashList.Rename(const AOldName,ANewName:shortstring): Integer;
var
  PrevIndex,
  Index : Integer;
  OldHash : LongWord;
begin
  Result:=-1;
  OldHash:=FPHash(AOldName);
  Index:=InternalFind(OldHash,AOldName,PrevIndex);
  if Index=-1 then
    exit;
  { Remove from current Hash }
  if PrevIndex<>-1 then
    FHashList^[PrevIndex].NextIndex:=FHashList^[Index].NextIndex
  else
    FHashTable^[OldHash mod LongWord(FHashCapacity)]:=FHashList^[Index].NextIndex;
  { Set new name and hash }
  with FHashList^[Index] do
    begin
      HashValue:=FPHash(ANewName);
      StrIndex:=AddStr(ANewName);
    end;
  { Insert back in Hash }
  AddToHashTable(Index);
  { Return Index }
  Result:=Index;
end;

procedure TFPHashList.Pack;
var
  NewCount,
  i : integer;
  pdest,
  psrc : PHashItem;
begin
  NewCount:=0;
  psrc:=@FHashList^[0];
  pdest:=psrc;
  For I:=0 To FCount-1 Do
    begin
      if assigned(psrc^.Data) then
        begin
          pdest^:=psrc^;
          inc(pdest);
          inc(NewCount);
        end;
      inc(psrc);
    end;
  FCount:=NewCount;
  { We need to ReHash to update the IndexNext }
  ReHash;
  { Release over-capacity }
  SetCapacity(FCount);
  SetStrCapacity(FStrCount);
end;


procedure TFPHashList.ShowStatistics;
var
  HashMean,
  HashStdDev : Double;
  Index,
  i,j : Integer;
begin
  { Calculate Mean and StdDev }
  HashMean:=0;
  HashStdDev:=0;
  for i:=0 to FHashCapacity-1 do
    begin
      j:=0;
      Index:=FHashTable^[i];
      while (Index<>-1) do
        begin
          inc(j);
          Index:=FHashList^[Index].NextIndex;
        end;
      HashMean:=HashMean+j;
      HashStdDev:=HashStdDev+Sqr(j);
    end;
  HashMean:=HashMean/FHashCapacity;
  HashStdDev:=(HashStdDev-FHashCapacity*Sqr(HashMean));
  If FHashCapacity>1 then
    HashStdDev:=Sqrt(HashStdDev/(FHashCapacity-1))
  else
    HashStdDev:=0;
  { Print info to stdout }
  Writeln('HashSize   : ',FHashCapacity);
  Writeln('HashMean   : ',HashMean:1:4);
  Writeln('HashStdDev : ',HashStdDev:1:4);
  Writeln('ListSize   : ',FCount,'/',FCapacity);
  Writeln('StringSize : ',FStrCount,'/',FStrCapacity);
end;


procedure TFPHashList.ForEachCall(proc2call:TListCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  For I:=0 To Count-1 Do
    begin
      p:=FHashList^[i].Data;
      if assigned(p) then
        proc2call(p,arg);
    end;
end;


procedure TFPHashList.ForEachCall(proc2call:TListStaticCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  For I:=0 To Count-1 Do
    begin
      p:=FHashList^[i].Data;
      if assigned(p) then
        proc2call(p,arg);
    end;
end;


{*****************************************************************************
                               TFPHashObject
*****************************************************************************}

procedure TFPHashObject.InternalChangeOwner(HashObjectList:TFPHashObjectList;const s:shortstring);
var
  Index : integer;
begin
  FOwner:=HashObjectList;
  Index:=HashObjectList.Add(s,Self);
  FStrIndex:=HashObjectList.List.List^[Index].StrIndex;
  FCachedStr:=PShortString(@FOwner.List.Strs[FStrIndex]);
end;


constructor TFPHashObject.CreateNotOwned;
begin
  FStrIndex:=-1;
end;


constructor TFPHashObject.Create(HashObjectList:TFPHashObjectList;const s:shortstring);
begin
  InternalChangeOwner(HashObjectList,s);
end;


procedure TFPHashObject.ChangeOwner(HashObjectList:TFPHashObjectList);
begin
  InternalChangeOwner(HashObjectList,PShortString(@FOwner.List.Strs[FStrIndex])^);
end;


procedure TFPHashObject.ChangeOwnerAndName(HashObjectList:TFPHashObjectList;const s:shortstring);
begin
  InternalChangeOwner(HashObjectList,s);
end;


procedure TFPHashObject.Rename(const ANewName:shortstring);
var
  Index : integer;
begin
  Index:=FOwner.Rename(PShortString(@FOwner.List.Strs[FStrIndex])^,ANewName);
  if Index<>-1 then
    begin
      FStrIndex:=FOwner.List.List^[Index].StrIndex;
      FCachedStr:=PShortString(@FOwner.List.Strs[FStrIndex]);
    end;
end;


function TFPHashObject.GetName:shortstring;
begin
  if FOwner<>nil then
    begin
      FCachedStr:=PShortString(@FOwner.List.Strs[FStrIndex]);
      Result:=FCachedStr^;
    end
  else
    Result:='';
end;


function TFPHashObject.GetHash:Longword;
begin
  if FOwner<>nil then
    Result:=FPHash(PShortString(@FOwner.List.Strs[FStrIndex])^)
  else
    Result:=$ffffffff;
end;


{*****************************************************************************
            TFPHashObjectList (Copied from rtl/objpas/classes/lists.inc)
*****************************************************************************}

constructor TFPHashObjectList.Create(FreeObjects : boolean = True);
begin
  inherited Create;
  FHashList := TFPHashList.Create;
  FFreeObjects := Freeobjects;
end;

destructor TFPHashObjectList.Destroy;
begin
  if (FHashList <> nil) then
    begin
      Clear;
      FHashList.Destroy;
    end;
  inherited Destroy;
end;

procedure TFPHashObjectList.Clear;
var
  i: integer;
begin
  if FFreeObjects then
    for i := 0 to FHashList.Count - 1 do
      TObject(FHashList[i]).Free;
  FHashList.Clear;
end;

function TFPHashObjectList.GetCount: integer;
begin
  Result := FHashList.Count;
end;

procedure TFPHashObjectList.SetCount(const AValue: integer);
begin
  if FHashList.Count <> AValue then
    FHashList.Count := AValue;
end;

function TFPHashObjectList.GetItem(Index: Integer): TObject;
begin
  Result := TObject(FHashList[Index]);
end;

procedure TFPHashObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList[index] := AObject;
end;

procedure TFPHashObjectList.SetCapacity(NewCapacity: Integer);
begin
  FHashList.Capacity := NewCapacity;
end;

function TFPHashObjectList.GetCapacity: integer;
begin
  Result := FHashList.Capacity;
end;

function TFPHashObjectList.Add(const AName:shortstring;AObject: TObject): Integer;
begin
  Result := FHashList.Add(AName,AObject);
end;

function TFPHashObjectList.NameOfIndex(Index: Integer): shortstring;
begin
  Result := FHashList.NameOfIndex(Index);
end;

function TFPHashObjectList.HashOfIndex(Index: Integer): LongWord;
begin
  Result := FHashList.HashOfIndex(Index);
end;

procedure TFPHashObjectList.Delete(Index: Integer);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList.Delete(Index);
end;

function TFPHashObjectList.Expand: TFPHashObjectList;
begin
  FHashList.Expand;
  Result := Self;
end;

function TFPHashObjectList.Extract(Item: TObject): TObject;
begin
  Result := TObject(FHashList.Extract(Item));
end;

function TFPHashObjectList.Remove(AObject: TObject): Integer;
begin
  Result := IndexOf(AObject);
  if (Result <> -1) then
    begin
      if OwnsObjects then
        TObject(FHashList[Result]).Free;
      FHashList.Delete(Result);
    end;
end;

function TFPHashObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result := FHashList.IndexOf(Pointer(AObject));
end;


function TFPHashObjectList.Find(const s:shortstring): TObject;
begin
  result:=TObject(FHashList.Find(s));
end;


function TFPHashObjectList.FindIndexOf(const s:shortstring): Integer;
begin
  result:=FHashList.FindIndexOf(s);
end;


function TFPHashObjectList.FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
begin
  Result:=TObject(FHashList.FindWithHash(AName,AHash));
end;


function TFPHashObjectList.Rename(const AOldName,ANewName:shortstring): Integer;
begin
  Result:=FHashList.Rename(AOldName,ANewName);
end;


function TFPHashObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  If AExact then
    while (I<Count) and (Result=-1) do
      If Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      If Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;


procedure TFPHashObjectList.Pack;
begin
  FHashList.Pack;
end;


procedure TFPHashObjectList.ShowStatistics;
begin
  FHashList.ShowStatistics;
end;


procedure TFPHashObjectList.ForEachCall(proc2call:TObjectListCallback;arg:pointer);
begin
  FHashList.ForEachCall(TListCallBack(proc2call),arg);
end;


procedure TFPHashObjectList.ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer);
begin
  FHashList.ForEachCall(TListStaticCallBack(proc2call),arg);
end;


{ ---------------------------------------------------------------------
    Hash support, by Dean Zobec
  ---------------------------------------------------------------------}

{ Default hash function }

function RSHash(const S: string; const TableSize: Longword): Longword;
const
  b = 378551;
var
  a: Longword;
  i: Longword;
begin
 a := 63689;
 Result := 0;
 if length(s)>0 then
   for i := 1 to Length(S) do
   begin
     Result := Result * a + Ord(S[i]);
     a := a * b;
   end;
 Result := (Result and $7FFFFFFF) mod TableSize;
end;

{ THTNode }

constructor THTCustomNode.CreateWith(const AString: string);
begin
  inherited Create;
  FKey := AString;
end;

function THTCustomNode.HasKey(const AKey: string): boolean;
begin
  if Length(AKey) <> Length(FKey) then
  begin
    Result := false;
    exit;
  end
  else
    Result := CompareMem(PChar(FKey), PChar(AKey), length(AKey));
end;

{ TFPCustomHashTable }

constructor TFPCustomHashTable.Create;
begin
  CreateWith(196613,@RSHash);
end;

constructor TFPCustomHashTable.CreateWith(AHashTableSize: Longword;
  aHashFunc: THashFunction);
begin
  Inherited Create;
  FHashTable := TFPObjectList.Create(True);
  HashTableSize := AHashTableSize;
  FHashFunction := aHashFunc;
end;

destructor TFPCustomHashTable.Destroy;
begin
  FHashTable.Free;
  inherited Destroy;
end;

function TFPCustomHashTable.GetDensity: Longword;
begin
  Result := FHashTableSize - VoidSlots
end;

function TFPCustomHashTable.GetNumberOfCollisions: Longword;
begin
  Result := FCount -(FHashTableSize - VoidSlots)
end;

procedure TFPCustomHashTable.SetHashTableSize(const Value: Longword);
var
  i: Longword;
  newSize: Longword;
begin
  if Value <> FHashTableSize then
  begin
    i := 0;
    while (PRIMELIST[i] < Value) and (i < 27) do
     inc(i);
    newSize := PRIMELIST[i];
    if Count = 0 then
    begin
      FHashTableSize := newSize;
      InitializeHashTable;
    end
    else
      ChangeTableSize(newSize);
  end;
end;

procedure TFPCustomHashTable.InitializeHashTable;
var
  i: LongWord;
begin
  if FHashTableSize>0 Then
    for i := 0 to FHashTableSize-1 do
     FHashTable.Add(nil);
  FCount := 0;
end;

procedure TFPCustomHashTable.ChangeTableSize(const ANewSize: Longword);
var
  SavedTable: TFPObjectList;
  SavedTableSize: Longword;
  i, j: Longword;
  temp: THTCustomNode;
begin
  SavedTable := FHashTable;
  SavedTableSize := FHashTableSize;
  FHashTableSize := ANewSize;
  FHashTable := TFPObjectList.Create(True);
  InitializeHashTable;
  If SavedTableSize>0 Then
    for i := 0 to SavedTableSize-1 do
    begin
      if Assigned(SavedTable[i]) then
      for j := 0 to TFPObjectList(SavedTable[i]).Count -1 do
      begin
        temp := THTCustomNode(TFPObjectList(SavedTable[i])[j]);
        AddNode(temp);
      end;
    end;
  SavedTable.Free;
end;

procedure TFPCustomHashTable.SetHashFunction(AHashFunction: THashFunction);
begin
  if IsEmpty then
    FHashFunction := AHashFunction
  else
    raise Exception.Create(NotEmptyMsg);
end;

function TFPCustomHashTable.Find(const aKey: string): THTCustomNode;
var
  hashCode: Longword;
  chn: TFPObjectList;
  i: Longword;
begin
  hashCode := FHashFunction(aKey, FHashTableSize);
  chn := Chain(hashCode);
  if Assigned(chn) then
  begin
    if chn.count>0 then
     for i := 0 to chn.Count - 1 do
      if THTCustomNode(chn[i]).HasKey(aKey) then
      begin
        result := THTCustomNode(chn[i]);
        exit;
      end;
  end;
  Result := nil;
end;

Function TFPCustomHashTable.FindChainForAdd(Const aKey : String) : TFPObjectList;

var
  hashCode: Longword;
  i: Longword;

begin
  hashCode := FHashFunction(aKey, FHashTableSize);
  Result := Chain(hashCode);
  if Assigned(Result)  then
    begin
    if Result.count>0 then
      for i := 0 to Result.Count - 1 do
        if THTCustomNode(Result[i]).HasKey(aKey) then
          Raise EDuplicate.CreateFmt(DuplicateMsg, [aKey]);
    end
  else
    begin
    FHashTable[hashcode] := TFPObjectList.Create(true);
    Result := Chain(hashcode);
    end;
  inc(FCount);
end;


procedure TFPCustomHashTable.Delete(const aKey: string);
var
  hashCode: Longword;
  chn: TFPObjectList;
  i: Longword;
begin
  hashCode := FHashFunction(aKey, FHashTableSize);
  chn := Chain(hashCode);
  if Assigned(chn) then
  begin
    if chn.count>0 then
    for i := 0 to chn.Count - 1 do
      if THTCustomNode(chn[i]).HasKey(aKey) then
      begin
        chn.Delete(i);
        dec(FCount);
        exit;
      end;
  end;
  raise EKeyNotFound.CreateFmt(KeyNotFoundMsg, ['Delete', aKey]);
end;

function TFPCustomHashTable.IsEmpty: boolean;
begin
  Result := (FCount = 0);
end;

function TFPCustomHashTable.Chain(const index: Longword): TFPObjectList;
begin
  Result := TFPObjectList(FHashTable[index]);
end;

function TFPCustomHashTable.GetVoidSlots: Longword;
var
  i: Longword;
  num: Longword;
begin
  num := 0;
  if FHashTableSize>0 Then
    for i:= 0 to FHashTableSize-1 do
      if Not Assigned(Chain(i)) then
        inc(num);
  result := num;
end;

function TFPCustomHashTable.GetLoadFactor: double;
begin
  Result := Count / FHashTableSize;
end;

function TFPCustomHashTable.GetAVGChainLen: double;
begin
  result := Count / (FHashTableSize - VoidSlots);
end;

function TFPCustomHashTable.GetMaxChainLength: Longword;
var
  i: Longword;
begin
  Result := 0;
  if FHashTableSize>0 Then
   for i := 0 to FHashTableSize-1 do
      if ChainLength(i) > Result then
        Result := ChainLength(i);
end;

function TFPCustomHashTable.FindOrCreateNew(const aKey: string): THTCustomNode;
var
  hashCode: Longword;
  chn: TFPObjectList;
  i: Longword;
begin
  hashCode := FHashFunction(aKey, FHashTableSize);
  chn := Chain(hashCode);
  if Assigned(chn)  then
  begin
    if chn.count>0 then
     for i := 0 to chn.Count - 1 do
      if THTCustomNode(chn[i]).HasKey(aKey) then
        begin
          Result := THTNode(chn[i]);
          exit;
        end
  end
  else
    begin
      FHashTable[hashcode] := TFPObjectList.Create(true);
      chn := Chain(hashcode);
    end;
  inc(FCount);
  Result := CreateNewNode(aKey);
  chn.Add(Result);
end;

function TFPCustomHashTable.ChainLength(const ChainIndex: Longword): Longword;
begin
  if Assigned(Chain(ChainIndex)) then
    Result := Chain(ChainIndex).Count
  else
    Result := 0;
end;

procedure TFPCustomHashTable.Clear;
var
  i: Longword;
begin
  if FHashTableSize>0 Then
    for i := 0 to FHashTableSize - 1 do
      begin
        if Assigned(Chain(i)) then
          Chain(i).Clear;
      end;
  FCount := 0;
end;



{ TFPDataHashTable }

procedure TFPDataHashTable.Add(const aKey: string; aItem: pointer);
var
  chn: TFPObjectList;
  NewNode: THtDataNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode := THtDataNode(CreateNewNode(aKey));
  NewNode.Data := aItem;
  chn.Add(NewNode);
end;

function TFPDataHashTable.GetData(const Index: string): Pointer;
var
  node: THTDataNode;
begin
  node := THTDataNode(Find(Index));
  if Assigned(node) then
    Result := node.Data
  else
    Result := nil;
end;

procedure TFPDataHashTable.SetData(const index: string; const AValue: Pointer);
begin
  THTDataNode(FindOrCreateNew(index)).Data := AValue;
end;

Function TFPDataHashTable.CreateNewNode(const aKey : string) : THTCustomNode;

begin
  Result:=THTDataNode.CreateWith(aKey);
end;

function TFPDataHashTable.ForEachCall(aMethod: TDataIteratorMethod): THTDataNode;
var
  i, j: Longword;
  continue: boolean;
begin
  Result := nil;
  continue := true;
  if FHashTableSize>0 then
   for i := 0 to FHashTableSize-1 do
    begin
      if assigned(Chain(i)) then
      begin
       if chain(i).count>0 then
        for j := 0 to Chain(i).Count-1 do
        begin
          aMethod(THTDataNode(Chain(i)[j]).Data, THTDataNode(Chain(i)[j]).Key, continue);
          if not continue then
          begin
            Result := THTDataNode(Chain(i)[j]);
            Exit;
          end;
        end;
      end;
    end;
end;

Procedure TFPDataHashTable.AddNode(ANode : THTCustomNode);

begin
  With THTDataNode(ANode) do
    Add(Key,Data);
end;

{ TFPStringHashTable }

Procedure TFPStringHashTable.AddNode(ANode : THTCustomNode);

begin
  With THTStringNode(ANode) do
    Add(Key,Data);
end;

function TFPStringHashTable.GetData(const Index: string): String;
var
  node: THTStringNode;
begin
  node := THTStringNode(Find(Index));
  if Assigned(node) then
    Result := node.Data
  else
    Result := '';
end;

procedure TFPStringHashTable.SetData(const index, AValue: string);
begin
  THTStringNode(FindOrCreateNew(index)).Data := AValue;
end;

procedure TFPStringHashTable.Add(const aKey, aItem: string);
var
  chn: TFPObjectList;
  NewNode: THtStringNode;

begin
  chn:=FindChainForAdd(akey);
  NewNode := THtStringNode(CreateNewNode(aKey));
  NewNode.Data := aItem;
  chn.Add(NewNode);
end;

Function TFPStringHashTable.CreateNewNode(const aKey : string) : THTCustomNode;

begin
  Result:=THTStringNode.CreateWith(aKey);
end;


function TFPStringHashTable.ForEachCall(aMethod: TStringIteratorMethod): THTStringNode;
var
  i, j: Longword;
  continue: boolean;
begin
  Result := nil;
  continue := true;
  if FHashTableSize>0 then
   for i := 0 to FHashTableSize-1 do
    begin
      if assigned(Chain(i)) then
      begin
       if chain(i).count>0 then
        for j := 0 to Chain(i).Count-1 do
        begin
          aMethod(THTStringNode(Chain(i)[j]).Data, THTStringNode(Chain(i)[j]).Key, continue);
          if not continue then
          begin
            Result := THTStringNode(Chain(i)[j]);
            Exit;
          end;
        end;
      end;
    end;
end;

{ TFPObjectHashTable }

Procedure TFPObjectHashTable.AddNode(ANode : THTCustomNode);

begin
  With THTObjectNode(ANode) do
    Add(Key,Data);
end;

function TFPObjectHashTable.GetData(const Index: string): TObject;
var
  node: THTObjectNode;
begin
  node := THTObjectNode(Find(Index));
  if Assigned(node) then
    Result := node.Data
  else
    Result := Nil;
end;

procedure TFPObjectHashTable.SetData(const index : string; AObject : TObject);
begin
  THTObjectNode(FindOrCreateNew(index)).Data := AObject;
end;

procedure TFPObjectHashTable.Add(const aKey: string; AItem : TObject);
var
  chn: TFPObjectList;
  NewNode: THTObjectNode;

begin
  chn:=FindChainForAdd(akey);
  NewNode := THTObjectNode(CreateNewNode(aKey));
  NewNode.Data := aItem;
  chn.Add(NewNode);
end;

Function TFPObjectHashTable.CreateNewNode(const aKey : string) : THTCustomNode;

begin
  If OwnsObjects then
    Result:=THTOwnedObjectNode.CreateWith(aKey)
  else
    Result:=THTObjectNode.CreateWith(aKey);
end;


function TFPObjectHashTable.ForEachCall(aMethod: TObjectIteratorMethod): THTObjectNode;
var
  i, j: Longword;
  continue: boolean;
begin
  Result := nil;
  continue := true;
  if FHashTableSize>0 then
   for i := 0 to FHashTableSize-1 do
    begin
      if assigned(Chain(i)) then
      begin
       if chain(i).count>0 then
        for j := 0 to Chain(i).Count-1 do
        begin
          aMethod(THTObjectNode(Chain(i)[j]).Data, THTObjectNode(Chain(i)[j]).Key, continue);
          if not continue then
          begin
            Result := THTObjectNode(Chain(i)[j]);
            Exit;
          end;
        end;
      end;
    end;
end;

constructor TFPObjectHashTable.Create(AOwnsObjects : Boolean = True);

begin
  Inherited Create;
  FOwnsObjects:=AOwnsObjects;
end;

constructor TFPObjectHashTable.CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction; AOwnsObjects : Boolean = True);

begin
  Inherited CreateWith(AHashTableSize,AHashFunc);
  FOwnsObjects:=AOwnsObjects;
end;

Destructor THTOwnedObjectNode.Destroy;

begin
  FreeAndNil(FData);
  Inherited;
end;

{ TCustomBucketList }

function TCustomBucketList.GetData(AItem: Pointer): Pointer;

Var
  B,I : Integer;

begin
  GetBucketItem(AItem,B,I);
  Result:=FBuckets[B].Items[I].Data;
end;

function TCustomBucketList.GetBucketCount: Integer;
begin
  Result:=Length(FBuckets);
end;

procedure TCustomBucketList.SetData(AItem: Pointer; const AData: Pointer);

Var
  B,I : Integer;

begin
  GetBucketItem(AItem,B,I);
  FBuckets[B].Items[I].Data:=AData;
end;

procedure TCustomBucketList.SetBucketCount(const Value: Integer);

begin
  If (Value<>GetBucketCount) then
    SetLength(FBuckets,Value);
end;

procedure TCustomBucketList.GetBucketItem(AItem: Pointer; out ABucket,
  AIndex: Integer);
begin
  If Not FindItem(AItem,ABucket,AIndex) then
    Error(SErrNoSuchItem,[AItem]);
end;

function TCustomBucketList.AddItem(ABucket: Integer; AItem, AData: Pointer
  ): Pointer;
  
Var
  B : PBucket;
  L : Integer;
  
begin
  B:=@FBuckets[ABucket];
  L:=Length(B^.Items);
  If (B^.Count=L) then
    begin
    If L<8 then
      L:=8
    else
      L:=L+L div 2;
    SetLength(B^.Items,L);
    end;
  With B^ do
    begin
    Items[Count].Item:=AItem;
    Items[Count].Data:=AData;
    Result:=AData;
    Inc(Count);
    end;
end;

function TCustomBucketList.DeleteItem(ABucket: Integer; AIndex: Integer): Pointer;
  
Var
  B : PBucket;
  L : Integer;
  
begin
  B:=@FBuckets[ABucket];
  Result:=B^.Items[Aindex].Data;
  If B^.Count=1 then
    SetLength(B^.Items,0)
  else
    begin
    L:=(B^.Count-AIndex-1);// No point in moving if last one...
    If L>0 then
      Move(B^.Items[AIndex+1],B^.Items[AIndex],L*SizeOf(TBucketItem));
    end;
  Dec(B^.Count);
end;

procedure TCustomBucketList.Error(Msg: String; Args: array of const);
begin
  Raise ElistError.CreateFmt(Msg,Args);
end;

function TCustomBucketList.FindItem(AItem: Pointer; out ABucket, AIndex: Integer
  ): Boolean;
  
Var
  I : Integer;
  B : TBucket;
  
begin
  ABucket:=BucketFor(AItem);
  B:=FBuckets[ABucket];
  I:=B.Count-1;
  While (I>=0) And (B.Items[I].Item<>AItem) do
    Dec(I);
  Result:=I>=0;
  If Result then
    AIndex:=I;
end;

destructor TCustomBucketList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCustomBucketList.Clear;

Var
  B : TBucket;
  I,J : Integer;

begin
  For I:=0 to Length(FBuckets)-1 do
    begin
    B:=FBuckets[I];
    For J:=B.Count-1 downto 0 do
      DeleteItem(I,J);
    end;
  SetLength(FBuckets,0);
end;

function TCustomBucketList.Add(AItem, AData: Pointer): Pointer;

Var
  B,I : Integer;

begin
  If FindItem(AItem,B,I) then
    Error(SDuplicateItem,[AItem]);
  Result:=AddItem(B,AItem,AData);
end;

procedure TCustomBucketList.Assign(AList: TCustomBucketList);

Var
  I,J : Integer;

begin
  Clear;
  SetLength(FBuckets,Length(Alist.FBuckets));
  For I:=0 to BucketCount-1 do
    begin
    SetLength(FBuckets[i].Items,Length(AList.Fbuckets[I].Items));
    For J:=0 to AList.Fbuckets[I].Count-1 do
      With AList.Fbuckets[I].Items[J] do
        AddItem(I,Item,Data);
    end;
end;

function TCustomBucketList.Exists(AItem: Pointer): Boolean;

Var
  B,I : Integer;

begin
  Result:=FindItem(Aitem,B,I);
end;

function TCustomBucketList.Find(AItem: Pointer; out AData: Pointer): Boolean;

Var
  B,I : integer;

begin
  Result:=FindItem(AItem,B,I);
  If Result then
    AData:=FBuckets[B].Items[I].Data;
end;

function TCustomBucketList.ForEach(AProc: TBucketProc; AInfo: Pointer
  ): Boolean;
  
Var
  I,J,S : Integer;
  Bu : TBucket;
  
begin
  I:=0;
  Result:=True;
  S:=GetBucketCount;
  While Result and (I<S) do
    begin
    J:=0;
    Bu:=FBuckets[I];
    While Result and (J<Bu.Count) do
      begin
      With Bu.Items[J] do
        AProc(AInfo,Item,Data,Result);
      Inc(J);
      end;
    Inc(I);
    end;
end;

function TCustomBucketList.ForEach(AProc: TBucketProcObject): Boolean;

Var
  I,J,S : Integer;
  Bu : TBucket;

begin
  I:=0;
  Result:=True;
  S:=GetBucketCount;
  While Result and (I<S) do
    begin
    J:=0;
    Bu:=FBuckets[I];
    While Result and (J<Bu.Count) do
      begin
      With Bu.Items[J] do
        AProc(Item,Data,Result);
      Inc(J);
      end;
    Inc(I);
    end;
end;

function TCustomBucketList.Remove(AItem: Pointer): Pointer;

Var
  B,I : integer;

begin
  If FindItem(AItem,B,I) then
    begin
    Result:=FBuckets[B].Items[I].Data;
    DeleteItem(B,I);
    end
  else
    Result:=Nil;
end;

{ TBucketList }

function TBucketList.BucketFor(AItem: Pointer): Integer;
begin
  // Pointers on average have a granularity of 4
  Result:=(PtrInt(AItem) shr 2) and FBucketMask;
end;

constructor TBucketList.Create(ABuckets: TBucketListSizes);

Var
  L : Integer;
  
begin
  Inherited Create;
  L:=1 shl (Ord(Abuckets)+1);
  SetBucketCount(L);
  FBucketMask:=L-1;
end;

{ TObjectBucketList }

function TObjectBucketList.GetData(AItem: TObject): TObject;
begin
  Result:=TObject(Inherited GetData(AItem));
end;

procedure TObjectBucketList.SetData(AItem: TObject; const AData: TObject);
begin
  Inherited SetData(Pointer(AItem),Pointer(AData));
end;

function TObjectBucketList.Add(AItem, AData: TObject): TObject;
begin
  Result:=TObject(Inherited Add(Pointer(AItem),Pointer(AData)));
end;

function TObjectBucketList.Remove(AItem: TObject): TObject;
begin
  Result:=TObject(Inherited Remove(Pointer(AItem)));
end;

end.
