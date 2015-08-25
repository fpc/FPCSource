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
  TObjectListCallback = Procedure(data:TObject;arg:pointer) of object;
  TObjectListStaticCallback = Procedure(data:TObject;arg:pointer);

  TFPObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FList: TFPList;
    Function GetCount: integer;
    Procedure SetCount(const AValue: integer);
  protected
    Function GetItem(Index: Integer): TObject; {$ifdef CLASSESINLINE}inline;{$endif}
    Procedure SetItem(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
    Procedure SetCapacity(NewCapacity: Integer);
    Function GetCapacity: integer;
  public
    constructor Create;
    constructor Create(FreeObjects : Boolean);
    destructor Destroy; override;
    Procedure Clear;
    Function Add(AObject: TObject): Integer; {$ifdef CLASSESINLINE}inline;{$endif}
    Procedure Delete(Index: Integer); {$ifdef CLASSESINLINE}inline;{$endif}
    Procedure Exchange(Index1, Index2: Integer);
    Function Expand: TFPObjectList;
    Function Extract(Item: TObject): TObject;
    Function Remove(AObject: TObject): Integer;
    Function IndexOf(AObject: TObject): Integer;
    Function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Insert(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
    Function First: TObject;
    Function Last: TObject;
    Procedure Move(CurIndex, NewIndex: Integer);
    Procedure Assign(Obj:TFPObjectList);
    Procedure Pack;
    Procedure Sort(Compare: TListSortCompare);
    Procedure ForEachCall(proc2call:TObjectListCallback;arg:pointer);
    Procedure ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TFPList read FList;
  end;


  { TObjectList }

  TObjectList = class(TList)
  private
    FFreeObjects : Boolean;
  Protected
    Procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    Function GetItem(Index: Integer): TObject;
    Procedure SetItem(Index: Integer; AObject: TObject);
  public
    constructor Create;
    constructor Create(FreeObjects : boolean);
    Function Add(AObject: TObject): Integer;
    Function Extract(Item: TObject): TObject;
    Function Remove(AObject: TObject): Integer;
    Function IndexOf(AObject: TObject): Integer;
    Function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Insert(Index: Integer; AObject: TObject);
    Function First: TObject;
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
{$ifdef CPU16}
  MaxHashListSize = maxsmallint div 16;
  MaxHashStrSize  = maxsmallint;
  MaxHashTableSize = maxsmallint div 4;
{$else CPU16}
  MaxHashListSize = Maxint div 16;
  MaxHashStrSize  = Maxint;
  MaxHashTableSize = Maxint div 4;
{$endif CPU16}
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
    Function InternalFind(AHash:LongWord;const AName:shortstring;out PrevIndex:Integer):Integer;
  protected
    Function Get(Index: Integer): Pointer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure Put(Index: Integer; Item: Pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetCapacity(NewCapacity: Integer);
    Procedure SetCount(NewCount: Integer);
    Procedure RaiseIndexError(Index : Integer);
    Function  AddStr(const s:shortstring): Integer;
    Procedure AddToHashTable(Index: Integer);
    Procedure StrExpand(MinIncSize:Integer);
    Procedure SetStrCapacity(NewCapacity: Integer);
    Procedure SetHashCapacity(NewCapacity: Integer);
    Procedure ReHash;
  public
    constructor Create;
    destructor Destroy; override;
    Function Add(const AName:shortstring;Item: Pointer): Integer;
    Procedure Clear;
    Function NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function GetNextCollision(Index: Integer): Integer;
    Procedure Delete(Index: Integer);
    class Procedure Error(const Msg: string; Data: PtrInt);
    Function Expand: TFPHashList;
    Function Extract(item: Pointer): Pointer;
    Function IndexOf(Item: Pointer): Integer;
    Function Find(const AName:shortstring): Pointer;
    Function FindIndexOf(const AName:shortstring): Integer;
    Function FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
    Function Rename(const AOldName,ANewName:shortstring): Integer;
    Function Remove(Item: Pointer): Integer;
    Procedure Pack;
    Procedure ShowStatistics;
    Procedure ForEachCall(proc2call:TListCallback;arg:pointer);
    Procedure ForEachCall(proc2call:TListStaticCallback;arg:pointer);
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
    Procedure InternalChangeOwner(HashObjectList:TFPHashObjectList;const s:shortstring);
  protected
    Function GetName:shortstring;virtual;
    Function GetHash:Longword;virtual;
  public
    constructor CreateNotOwned;
    constructor Create(HashObjectList:TFPHashObjectList;const s:shortstring);
    Procedure ChangeOwner(HashObjectList:TFPHashObjectList); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ChangeOwnerAndName(HashObjectList:TFPHashObjectList;const s:shortstring); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure Rename(const ANewName:shortstring);
    property Name:shortstring read GetName;
    property Hash:Longword read GetHash;
  end;

  TFPHashObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FHashList: TFPHashList;
    Function GetCount: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetCount(const AValue: integer); {$ifdef CCLASSESINLINE}inline;{$endif}
  protected
    Function GetItem(Index: Integer): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetItem(Index: Integer; AObject: TObject); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetCapacity(NewCapacity: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    Function GetCapacity: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
  public
    constructor Create(FreeObjects : boolean = True);
    destructor Destroy; override;
    Procedure Clear;
    Function Add(const AName:shortstring;AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function GetNextCollision(Index: Integer): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure Delete(Index: Integer);
    Function Expand: TFPHashObjectList; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function Extract(Item: TObject): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function Remove(AObject: TObject): Integer;
    Function IndexOf(AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function Find(const s:shortstring): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function FindIndexOf(const s:shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
    Function Rename(const AOldName,ANewName:shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Pack; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ShowStatistics; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ForEachCall(proc2call:TObjectListCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
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
  THashFunction = Function(const S: string; const TableSize: Longword): Longword;


  { THTNode }

  THTCustomNode = class(TObject)
  private
    FKey: string;
  public
    constructor CreateWith(const AString: String);
    Function HasKey(const AKey: string): boolean;
    property Key: string read FKey;
  end;
  THTCustomNodeClass = Class of THTCustomNode;


  { TFPCustomHashTable }

  TFPCustomHashTable = class(TObject)
  private
    FHashTable: TFPObjectList;
    FHashFunction: THashFunction;
    FCount: Longword;
    Function GetDensity: Longword;
    Function GetNumberOfCollisions: Longword;
    Procedure SetHashTableSize(const Value: Longword);
    Procedure InitializeHashTable;
    Function GetVoidSlots: Longword;
    Function GetLoadFactor: double;
    Function GetAVGChainLen: double;
    Function GetMaxChainLength: Longword;
  protected
    FHashTableSize: Longword;
    Function Chain(const index: Longword):TFPObjectList;
    Function CreateNewNode(const aKey : string) : THTCustomNode; virtual; abstract;
    Procedure AddNode(ANode : THTCustomNode); virtual; abstract;
    Function ChainLength(const ChainIndex: Longword): Longword; virtual;
    Function FindOrCreateNew(const aKey: string): THTCustomNode; virtual;
    Procedure SetHashFunction(AHashFunction: THashFunction); virtual;
    Function FindChainForAdd(Const aKey : String) : TFPObjectList;
  public
    constructor Create;
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction);
    destructor Destroy; override;
    Procedure ChangeTableSize(const ANewSize: Longword); virtual;
    Procedure Clear; virtual;
    Procedure Delete(const aKey: string); virtual;
    Function Find(const aKey: string): THTCustomNode;
    Function IsEmpty: boolean;
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

  TDataIteratorMethod = Procedure(Item: Pointer; const Key: string; var Continue: Boolean) of object;
  // For compatibility
  TIteratorMethod = TDataIteratorMethod;

  TFPDataHashTable = Class(TFPCustomHashTable)
  Protected
    Function CreateNewNode(const aKey : String) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    Procedure SetData(const index: string; const AValue: Pointer); virtual;
    Function GetData(const index: string):Pointer; virtual;
    Function ForEachCall(aMethod: TDataIteratorMethod): THTDataNode; virtual;
  Public
    Function Iterate(aMethod: TDataIteratorMethod): Pointer; virtual;
    Procedure Add(const aKey: string; AItem: pointer); virtual;
    property Items[const index: string]: Pointer read GetData write SetData; default;
  end;

  { TFPStringHashTable : Hash table with simple strings as data }
  THTStringNode = Class(THTCustomNode)
  Private
    FData : String;
  public
    property Data: String read FData write FData;
  end;
  TStringIteratorMethod = Procedure(Item: String; const Key: string; var Continue: Boolean) of object;

  TFPStringHashTable = Class(TFPCustomHashTable)
  Protected
    Function CreateNewNode(const aKey : String) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    Procedure SetData(const Index, AValue: string); virtual;
    Function GetData(const index: string): String; virtual;
    Function ForEachCall(aMethod: TStringIteratorMethod): THTStringNode; virtual;
  Public
    Function Iterate(aMethod: TStringIteratorMethod): String; virtual;
    Procedure Add(const aKey,aItem: string); virtual;
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
    destructor Destroy; override;
  end;
  TObjectIteratorMethod = Procedure(Item: TObject; const Key: string; var Continue: Boolean) of object;

  TFPObjectHashTable = Class(TFPCustomHashTable)
  Private
    FOwnsObjects : Boolean;
  Protected
    Function CreateNewNode(const aKey : String) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    Procedure SetData(const Index: string; AObject : TObject); virtual;
    Function GetData(const index: string): TObject; virtual;
    Function ForEachCall(aMethod: TObjectIteratorMethod): THTObjectNode; virtual;
  Public
    constructor Create(AOwnsObjects : Boolean = True);
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction; AOwnsObjects : Boolean = True);
    Function Iterate(aMethod: TObjectIteratorMethod): TObject; virtual;
    Procedure Add(const aKey: string; AItem : TObject); virtual;
    property Items[const index: string]: TObject read GetData write SetData; default;
    Property OwnsObjects : Boolean Read FOwnsObjects Write FOwnsObjects;
  end;

  EDuplicate = class(Exception);
  EKeyNotFound = class(Exception);

  Function RSHash(const S: string; const TableSize: Longword): Longword;

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

  TBucketProc = Procedure(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
  TBucketProcObject = Procedure(AItem, AData: Pointer; out AContinue: Boolean) of Object;

{ ---------------------------------------------------------------------
  TCustomBucketList
  ---------------------------------------------------------------------}

  { TCustomBucketList }

  TCustomBucketList = class(TObject)
  private
    FBuckets: TBucketArray;
    Function GetBucketCount: Integer;
    Function GetData(AItem: Pointer): Pointer;
    Procedure SetData(AItem: Pointer; const AData: Pointer);
    Procedure SetBucketCount(const Value: Integer);
  protected
    Procedure GetBucketItem(AItem: Pointer; out ABucket, AIndex: Integer);
    Function AddItem(ABucket: Integer; AItem, AData: Pointer): Pointer; virtual;
    Function BucketFor(AItem: Pointer): Integer; virtual; abstract;
    Function DeleteItem(ABucket: Integer; AIndex: Integer): Pointer; virtual;
    Procedure Error(Msg : String; Args : Array of Const);
    Function FindItem(AItem: Pointer; out ABucket, AIndex: Integer): Boolean; virtual;
    property Buckets: TBucketArray read FBuckets;
    property BucketCount: Integer read GetBucketCount write SetBucketCount;
  public
    destructor Destroy; override;
    Procedure Clear;
    Function Add(AItem, AData: Pointer): Pointer;
    Procedure Assign(AList: TCustomBucketList);
    Function Exists(AItem: Pointer): Boolean;
    Function Find(AItem: Pointer; out AData: Pointer): Boolean;
    Function ForEach(AProc: TBucketProc; AInfo: Pointer = nil): Boolean;
    Function ForEach(AProc: TBucketProcObject): Boolean;
    Function Remove(AItem: Pointer): Pointer;
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
    Function BucketFor(AItem: Pointer): Integer; override;
  public
    constructor Create(ABuckets: TBucketListSizes = bl16);
  end;

{ ---------------------------------------------------------------------
  TObjectBucketList
  ---------------------------------------------------------------------}

  { TObjectBucketList }

  TObjectBucketList = class(TBucketList)
  protected
    Function GetData(AItem: TObject): TObject;
    Procedure SetData(AItem: TObject; const AData: TObject);
  public
    Function Add(AItem, AData: TObject): TObject;
    Function Remove(AItem: TObject): TObject;
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
  FFreeObjects:=Freeobjects;
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

Procedure TFPObjectList.Clear;
var
  i: integer;
begin
  if FFreeObjects then
    for i:=FList.Count-1 downto 0  do
      TObject(FList[i]).Free;
  FList.Clear;
end;

constructor TFPObjectList.Create;
begin
  inherited Create;
  FList:=TFPList.Create;
  FFreeObjects:=True;
end;

Function TFPObjectList.GetCount: integer;
begin
  Result:=FList.Count;
end;

Procedure TFPObjectList.SetCount(const AValue: integer);
begin
  if FList.Count <> AValue then
    FList.Count:=AValue;
end;

Function TFPObjectList.GetItem(Index: Integer): TObject; {$ifdef CLASSESINLINE}inline;{$endif}
begin
  Result:=TObject(FList[Index]);
end;

Procedure TFPObjectList.SetItem(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
begin
  if OwnsObjects then
    TObject(FList[Index]).Free;
  FList[index]:=AObject;
end;

Procedure TFPObjectList.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity:=NewCapacity;
end;

Function TFPObjectList.GetCapacity: integer;
begin
  Result:=FList.Capacity;
end;

Function TFPObjectList.Add(AObject: TObject): Integer; {$ifdef CLASSESINLINE}inline;{$endif}
begin
  Result:=FList.Add(AObject);
end;

Procedure TFPObjectList.Delete(Index: Integer); {$ifdef CLASSESINLINE}inline;{$endif}
begin
  if OwnsObjects then
    TObject(FList[Index]).Free;
  FList.Delete(Index);
end;

Procedure TFPObjectList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

Function TFPObjectList.Expand: TFPObjectList;
begin
  FList.Expand;
  Result:=Self;
end;

Function TFPObjectList.Extract(Item: TObject): TObject;
begin
  Result:=TObject(FList.Extract(Item));
end;

Function TFPObjectList.Remove(AObject: TObject): Integer;
begin
  Result:=IndexOf(AObject);
  if (Result <> -1) then
    begin
    if OwnsObjects then
      TObject(FList[Result]).Free;
    FList.Delete(Result);
    end;
end;

Function TFPObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result:=FList.IndexOf(Pointer(AObject));
end;

Function TFPObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  if AExact then
    while (I<Count) and (Result=-1) do
      if Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      if Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;

Procedure TFPObjectList.Insert(Index: Integer; AObject: TObject); {$ifdef CLASSESINLINE}inline;{$endif}
begin
  FList.Insert(Index, Pointer(AObject));
end;

Procedure TFPObjectList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

Procedure TFPObjectList.Assign(Obj: TFPObjectList);
var
  i: Integer;
begin
  Clear;
  for i:=0 to Obj.Count - 1 do
    Add(Obj[i]);
end;

Procedure TFPObjectList.Pack;
begin
  FList.Pack;
end;

Procedure TFPObjectList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

Function TFPObjectList.First: TObject;
begin
  Result:=TObject(FList.First);
end;

Function TFPObjectList.Last: TObject;
begin
  Result:=TObject(FList.Last);
end;

Procedure TFPObjectList.ForEachCall(proc2call:TObjectListCallback;arg:pointer);
begin
  FList.ForEachCall(TListCallBack(proc2call),arg);
end;

Procedure TFPObjectList.ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer);
begin
  FList.ForEachCall(TListStaticCallBack(proc2call),arg);
end;


{ TObjectList }

constructor TObjectList.Create(FreeObjects: boolean);
begin
  inherited Create;
  FFreeObjects:=FreeObjects;
end;

constructor TObjectList.Create;
begin
  inherited Create;
  FFreeObjects:=True;
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
  Result:=TObject(inherited Get(Index));
end;


Procedure TObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  // Put will take care of deleting old one in Notify.
  Put(Index,Pointer(AObject));
end;


Function TObjectList.Add(AObject: TObject): Integer;
begin
  Result:=inherited Add(Pointer(AObject));
end;


Function TObjectList.Extract(Item: TObject): TObject;
begin
  Result:=TObject(inherited Extract(Pointer(Item)));
end;


Function TObjectList.Remove(AObject: TObject): Integer;
begin
  Result:=inherited Remove(Pointer(AObject));
end;


Function TObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result:=inherited IndexOf(Pointer(AObject));
end;


Function TObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean;
  AStartAt: Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  if AExact then
    while (I<Count) and (Result=-1) do
      if Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      if Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;


Procedure TObjectList.Insert(Index: Integer; AObject: TObject);
begin
  Inherited Insert(Index,Pointer(AObject));
end;


Function TObjectList.First: TObject;
begin
  Result:=TObject(inherited First);
end;


Function TObjectList.Last: TObject;
begin
  Result:=TObject(inherited Last);
end;

{ TListComponent }

type
  TlistComponent = class(TComponent)
  private
    Flist : TComponentList;
  public
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

Procedure TlistComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation=opRemove) then
    Flist.HandleFreeNotify(Self,AComponent);
  inherited;
end;

{ TComponentList }

Function TComponentList.Add(AComponent: TComponent): Integer;
begin
  Result:=inherited Add(AComponent);
end;

destructor TComponentList.Destroy;
begin
  inherited;
  FreeAndNil(FNotifier);
end;

Function TComponentList.Extract(Item: TComponent): TComponent;
begin
  Result:=TComponent(inherited Extract(Item));
end;

Function TComponentList.First: TComponent;
begin
  Result:=TComponent(inherited First);
end;

Function TComponentList.GetItems(Index: Integer): TComponent;
begin
  Result:=TComponent(inherited Items[Index]);
end;

Procedure TComponentList.HandleFreeNotify(Sender: TObject;
  AComponent: TComponent);
begin
  Extract(AComponent);
end;

Function TComponentList.IndexOf(AComponent: TComponent): Integer;
begin
  Result:=inherited IndexOf(AComponent);
end;

Procedure TComponentList.Insert(Index: Integer; AComponent: TComponent);
begin
  inherited Insert(Index,AComponent)
end;

Function TComponentList.Last: TComponent;
begin
  Result:=TComponent(inherited Last);
end;

Procedure TComponentList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if FNotifier=nil then
    begin
    FNotifier:=TlistComponent.Create(nil);
    TlistComponent(FNotifier).FList:=Self;
    end;
  if Assigned(Ptr) then
    with TComponent(Ptr) do
      case Action of
        lnAdded : FreeNotification(FNotifier);
        lnExtracted, lnDeleted: RemoveFreeNotification(FNotifier);
      end;
  inherited Notify(Ptr, Action);
end;

Function TComponentList.Remove(AComponent: TComponent): Integer;
begin
  Result:=inherited Remove(AComponent);
end;

Procedure TComponentList.SetItems(Index: Integer; AComponent: TComponent);
begin
  Put(Index,AComponent);
end;

{ TClassList }

Function TClassList.Add(AClass: TClass): Integer;
begin
  Result:=inherited Add(Pointer(AClass));
end;

Function TClassList.Extract(Item: TClass): TClass;
begin
  Result:=TClass(inherited Extract(Pointer(Item)));
end;

Function TClassList.First: TClass;
begin
  Result:=TClass(inherited First);
end;

Function TClassList.GetItems(Index: Integer): TClass;
begin
  Result:=TClass(inherited Items[Index]);
end;

Function TClassList.IndexOf(AClass: TClass): Integer;
begin
  Result:=inherited IndexOf(Pointer(AClass));
end;

Procedure TClassList.Insert(Index: Integer; AClass: TClass);
begin
  inherited Insert(Index,Pointer(AClass));
end;

Function TClassList.Last: TClass;
begin
  Result:=TClass(inherited Last);
end;

Function TClassList.Remove(AClass: TClass): Integer;
begin
  Result:=inherited Remove(Pointer(AClass));
end;

Procedure TClassList.SetItems(Index: Integer; AClass: TClass);
begin
  Put(Index,Pointer(AClass));
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
  if AtLeast(1) then
    Result:=PeekItem
  else
    Result:=nil;
end;

Function TOrderedList.PeekItem: Pointer;
begin
  with Flist do
    Result:=Items[Count-1]
end;

Function TOrderedList.Pop: Pointer;
begin
  If Atleast(1) then
    Result:=PopItem
  else
    Result:=nil;
end;

Function TOrderedList.PopItem: Pointer;
begin
  with FList do
    if Count>0 then
      begin
      Result:=Items[Count-1];
      Delete(Count-1);
      end
    else
      Result:=nil;
end;

Function TOrderedList.Push(AItem: Pointer): Pointer;
begin
  PushItem(AItem);
  Result:=AItem;
end;

{ TStack }

Procedure TStack.PushItem(AItem: Pointer);
begin
  FList.Add(AItem);
end;

{ TObjectStack }

Function TObjectStack.Peek: TObject;
begin
  Result:=TObject(inherited Peek);
end;

Function TObjectStack.Pop: TObject;
begin
  Result:=TObject(Inherited Pop);
end;

Function TObjectStack.Push(AObject: TObject): TObject;
begin
  Result:=TObject(inherited Push(Pointer(AObject)));
end;

{ TQueue }

Procedure TQueue.PushItem(AItem: Pointer);
begin
  with FList Do
    Insert(0,AItem);
end;

{ TObjectQueue }

Function TObjectQueue.Peek: TObject;
begin
  Result:=TObject(inherited Peek);
end;

Function TObjectQueue.Pop: TObject;
begin
  Result:=TObject(inherited Pop);
end;

Function TObjectQueue.Push(AObject: TObject): TObject;
begin
  Result:=TObject(inherited Push(Pointer(AObject)));
end;


{*****************************************************************************
                            TFPHashList
*****************************************************************************}

    Function FPHash(const s:shortstring):LongWord;
    var
      p,pmax : PChar;
    begin
{$push}
{$Q-}
      Result:=0;
      p:=@s[1];
      pmax:=@s[length(s)+1];
      while (p<pmax) do
        begin
          Result:=LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
          Inc(p);
        end;
{$pop}
    end;

    Function FPHash(P: PChar; Len: Integer): LongWord;
    var
      pmax : PChar;
    begin
{$push}
{$Q-}
      Result:=0;
      pmax:=p+len;
      while (p<pmax) do
        begin
          Result:=LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
          Inc(p);
        end;
{$pop}
    end;


Procedure TFPHashList.RaiseIndexError(Index : Integer);
begin
  Error(SListIndexError, Index);
end;


Function TFPHashList.Get(Index: Integer): Pointer;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FHashList^[Index].Data;
end;


Procedure TFPHashList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  FHashList^[Index].Data:=Item;
end;


Function TFPHashList.NameOfIndex(Index: Integer): shortstring;
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  with FHashList^[Index] do
    begin
    if StrIndex>=0 then
      Result:=PShortString(@FStrs[StrIndex])^
    else
      Result:='';
    end;
end;


Function TFPHashList.HashOfIndex(Index: Integer): LongWord;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FHashList^[Index].HashValue;
end;


Function TFPHashList.GetNextCollision(Index: Integer): Integer;
begin
  Result:=-1;
  if ((Index > -1) and (Index < FCount)) then
    Result:=FHashList^[Index].NextIndex;
end;


Function TFPHashList.Extract(item: Pointer): Pointer;
var
  i : Integer;
begin
  Result:=nil;
  i:=IndexOf(item);
  if i >= 0 then
    begin
    Result:=item;
    Delete(i);
    end;
end;


Procedure TFPHashList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxHashListSize) then
     Error (SListCapacityError, NewCapacity);
  if NewCapacity = FCapacity then
    Exit;
  ReallocMem(FHashList, NewCapacity*SizeOf(THashItem));
  FCapacity:=NewCapacity;
  { Maybe expand hash also }
  if FCapacity>FHashCapacity*MaxItemsPerHash then
    SetHashCapacity(FCapacity div MaxItemsPerHash);
end;


Procedure TFPHashList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxHashListSize)then
    Error(SListCountError, NewCount);
  if NewCount > FCount then
    begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if FCount < NewCount then
      FillChar(FHashList^[FCount], (NewCount-FCount) div SizeOf(THashItem), 0);
    end;
  FCount:=NewCount;
end;


Procedure TFPHashList.SetStrCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FStrCount) or (NewCapacity > MaxHashStrSize) then
    Error(SListCapacityError, NewCapacity);
  if NewCapacity = FStrCapacity then
    Exit;
  ReallocMem(FStrs, NewCapacity);
  FStrCapacity:=NewCapacity;
end;


Procedure TFPHashList.SetHashCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < 1) then
    Error(SListCapacityError, NewCapacity);
  if FHashCapacity=NewCapacity then
    Exit;
  FHashCapacity:=NewCapacity;
  ReallocMem(FHashTable, FHashCapacity*SizeOf(Integer));
  ReHash;
end;


Procedure TFPHashList.ReHash;
var
  i : Integer;
begin
  FillDword(FHashTable^,FHashCapacity,LongWord(-1));
  for i:=0 to FCount-1 do
    AddToHashTable(i);
end;


constructor TFPHashList.Create;
begin
  SetHashCapacity(1);
end;


destructor TFPHashList.Destroy;
begin
  Clear;
  if Assigned(FHashTable) then
    FreeMem(FHashTable);
  inherited Destroy;
end;


Function TFPHashList.AddStr(const s:shortstring): Integer;
var
  Len : Integer;
begin
  len:=Length(s)+1;
  if FStrCount+Len >= FStrCapacity then
    StrExpand(Len);
  System.Move(s[0],FStrs[FStrCount],Len);
  Result:=FStrCount;
  Inc(FStrCount,Len);
end;


Procedure TFPHashList.AddToHashTable(Index: Integer);
var
  HashIndex : Integer;
begin
  with FHashList^[Index] do
    begin
    if not Assigned(Data) then
      Exit;
    HashIndex:=HashValue mod LongWord(FHashCapacity);
    NextIndex:=FHashTable^[HashIndex];
    FHashTable^[HashIndex]:=Index;
    end;
end;


Function TFPHashList.Add(const AName:shortstring;Item: Pointer): Integer;
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
  Result:=FCount;
  Inc(FCount);
end;

Procedure TFPHashList.Clear;
begin
  if Assigned(FHashList) then
    begin
    FCount:=0;
    SetCapacity(0);
    FHashList:=nil;
    end;
  SetHashCapacity(1);
  FHashTable^[0]:=(-1); // sethashcapacity does not always call rehash
  if Assigned(FStrs) then
    begin
    FStrCount:=0;
    SetStrCapacity(0);
    FStrs:=nil;
    end;
end;

Procedure TFPHashList.Delete(Index: Integer);
begin
  if (Index<0) or (Index>=FCount) then
    Error(SListIndexError, Index);
  { Remove from HashList }
  Dec(FCount);
  System.Move(FHashList^[Index+1], FHashList^[Index], (FCount - Index) * SizeOf(THashItem));
  { All indexes are updated, we need to build the hashtable again }
  ReHash;
  { Shrink the list if appropriate }
  if (FCapacity > 256) and (FCount < FCapacity shr 2) then
    begin
    FCapacity:=FCapacity shr 1;
    ReAllocMem(FHashList, SizeOf(THashItem) * FCapacity);
    end;
end;

Function TFPHashList.Remove(Item: Pointer): Integer;
begin
  Result:=IndexOf(Item);
  If Result <> -1 then
    Self.Delete(Result);
end;

class Procedure TFPHashList.Error(const Msg: string; Data: PtrInt);
begin
  raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame), get_caller_frame(get_frame);
end;

Function TFPHashList.Expand: TFPHashList;
var
  IncSize : Longint;
begin
  Result:=Self;
  if FCount < FCapacity then
    Exit;
  IncSize:=SizeOf(PtrInt)*2;
  if FCapacity > 127 then
    Inc(IncSize, FCapacity shr 2)
  else if FCapacity > SizeOf(PtrInt)*3 then
    Inc(IncSize, FCapacity shr 1)
  else if FCapacity >= SizeOf(PtrInt) then
    Inc(IncSize,sizeof(PtrInt));
  SetCapacity(FCapacity + IncSize);
end;

Procedure TFPHashList.StrExpand(MinIncSize:Integer);
var
  IncSize : Longint;
begin
  if FStrCount+MinIncSize < FStrCapacity then
    Exit;
  IncSize:=64;
  if FStrCapacity > 255 then
    Inc(IncSize, FStrCapacity shr 2);
  SetStrCapacity(FStrCapacity + IncSize + MinIncSize);
end;

Function TFPHashList.IndexOf(Item: Pointer): Integer;
var
  psrc  : PHashItem;
  Index : integer;
begin
  Result:=-1;
  psrc:=@FHashList^[0];
  for Index:=0 to FCount-1 do
    begin
    if psrc^.Data=Item then
      begin
      Result:=Index;
      Exit;
      end;
    Inc(psrc);
    end;
end;

Function TFPHashList.InternalFind(AHash:LongWord;const AName:shortstring;out PrevIndex:Integer):Integer;
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
    with FHashList^[Result] do
      begin
      if Assigned(Data) and
         (HashValue=AHash) and
         (Len=FStrs[StrIndex]) and
         (LastChar=FStrs[StrIndex+Byte(Len)]) and
         (AName=PShortString(@FStrs[StrIndex])^) then
        Exit;
      PrevIndex:=Result;
      Result:=NextIndex;
      end;
end;


Function TFPHashList.Find(const AName:shortstring): Pointer;
var
  Index,
  PrevIndex : Integer;
begin
  Result:=nil;
  Index:=InternalFind(FPHash(AName),AName,PrevIndex);
  if Index=-1 then
    Exit;
  Result:=FHashList^[Index].Data;
end;


Function TFPHashList.FindIndexOf(const AName:shortstring): Integer;
var
  PrevIndex : Integer;
begin
  Result:=InternalFind(FPHash(AName),AName,PrevIndex);
end;


Function TFPHashList.FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
var
  Index,
  PrevIndex : Integer;
begin
  Result:=nil;
  Index:=InternalFind(AHash,AName,PrevIndex);
  if Index=-1 then
    Exit;
  Result:=FHashList^[Index].Data;
end;


Function TFPHashList.Rename(const AOldName,ANewName:shortstring): Integer;
var
  PrevIndex,
  Index : Integer;
  OldHash : LongWord;
begin
  Result:=-1;
  OldHash:=FPHash(AOldName);
  Index:=InternalFind(OldHash,AOldName,PrevIndex);
  if Index=-1 then
    Exit;
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

Procedure TFPHashList.Pack;
var
  NewCount,
  i : integer;
  pdest,
  psrc : PHashItem;
  FOldStr : Pchar;
begin
  NewCount:=0;
  psrc:=@FHashList^[0];
  FOldStr:=FStrs;
  try
    FStrs:=nil;
    FStrCount:=0;
    FStrCapacity:=0;
    pdest:=psrc;
    for I:=0 to FCount-1 do
      begin
      if Assigned(psrc^.Data) then
        begin
        pdest^:=psrc^;
        pdest^.StrIndex:=AddStr(PShortString(@FOldStr[PDest^.StrIndex])^);
        Inc(pdest);
        Inc(NewCount);
        end;
      Inc(psrc);
      end;
  finally
    FreeMem(FoldStr);
  end;
  FCount:=NewCount;
  { We need to ReHash to update the IndexNext }
  ReHash;
  { Release over-capacity }
  SetCapacity(FCount);
  SetStrCapacity(FStrCount);
end;


Procedure TFPHashList.ShowStatistics;
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
      Inc(j);
      Index:=FHashList^[Index].NextIndex;
      end;
    HashMean:=HashMean+j;
    HashStdDev:=HashStdDev+Sqr(j);
    end;
  HashMean:=HashMean/FHashCapacity;
  HashStdDev:=(HashStdDev-FHashCapacity*Sqr(HashMean));
  if FHashCapacity>1 then
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


Procedure TFPHashList.ForEachCall(proc2call:TListCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  for i:=0 to Count-1 Do
    begin
    p:=FHashList^[i].Data;
    if Assigned(p) then
      proc2call(p,arg);
    end;
end;


Procedure TFPHashList.ForEachCall(proc2call:TListStaticCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  for i:=0 to Count-1 Do
    begin
    p:=FHashList^[i].Data;
    if Assigned(p) then
      proc2call(p,arg);
    end;
end;


{*****************************************************************************
                               TFPHashObject
*****************************************************************************}

Procedure TFPHashObject.InternalChangeOwner(HashObjectList:TFPHashObjectList;const s:shortstring);
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


Procedure TFPHashObject.ChangeOwner(HashObjectList:TFPHashObjectList);
begin
  InternalChangeOwner(HashObjectList,PShortString(@FOwner.List.Strs[FStrIndex])^);
end;


Procedure TFPHashObject.ChangeOwnerAndName(HashObjectList:TFPHashObjectList;const s:shortstring);
begin
  InternalChangeOwner(HashObjectList,s);
end;


Procedure TFPHashObject.Rename(const ANewName:shortstring);
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


Function TFPHashObject.GetName:shortstring;
begin
  if FOwner<>nil then
    begin
    FCachedStr:=PShortString(@FOwner.List.Strs[FStrIndex]);
    Result:=FCachedStr^;
    end
  else
    Result:='';
end;


Function TFPHashObject.GetHash:Longword;
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
  FHashList:=TFPHashList.Create;
  FFreeObjects:=Freeobjects;
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

Procedure TFPHashObjectList.Clear;
var
  i: integer;
begin
  if FFreeObjects then
    for i:=0 to FHashList.Count - 1 do
      TObject(FHashList[i]).Free;
  FHashList.Clear;
end;

Function TFPHashObjectList.GetCount: integer;
begin
  Result:=FHashList.Count;
end;

Procedure TFPHashObjectList.SetCount(const AValue: integer);
begin
  if FHashList.Count <> AValue then
    FHashList.Count:=AValue;
end;

Function TFPHashObjectList.GetItem(Index: Integer): TObject;
begin
  Result:=TObject(FHashList[Index]);
end;

Procedure TFPHashObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList[Index]:=AObject;
end;

Procedure TFPHashObjectList.SetCapacity(NewCapacity: Integer);
begin
  FHashList.Capacity:=NewCapacity;
end;

Function TFPHashObjectList.GetCapacity: integer;
begin
  Result:=FHashList.Capacity;
end;

Function TFPHashObjectList.Add(const AName:shortstring;AObject: TObject): Integer;
begin
  Result:=FHashList.Add(AName,AObject);
end;

Function TFPHashObjectList.NameOfIndex(Index: Integer): shortstring;
begin
  Result:=FHashList.NameOfIndex(Index);
end;

Function TFPHashObjectList.HashOfIndex(Index: Integer): LongWord;
begin
  Result:=FHashList.HashOfIndex(Index);
end;

Function TFPHashObjectList.GetNextCollision(Index: Integer): Integer;
begin
  Result:=FHashList.GetNextCollision(Index);
end;

Procedure TFPHashObjectList.Delete(Index: Integer);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList.Delete(Index);
end;

Function TFPHashObjectList.Expand: TFPHashObjectList;
begin
  FHashList.Expand;
  Result:=Self;
end;

Function TFPHashObjectList.Extract(Item: TObject): TObject;
begin
  Result:=TObject(FHashList.Extract(Item));
end;

Function TFPHashObjectList.Remove(AObject: TObject): Integer;
begin
  Result:=IndexOf(AObject);
  if (Result <> -1) then
    begin
    if OwnsObjects then
      TObject(FHashList[Result]).Free;
    FHashList.Delete(Result);
    end;
end;

Function TFPHashObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result:=FHashList.IndexOf(Pointer(AObject));
end;


Function TFPHashObjectList.Find(const s:shortstring): TObject;
begin
  Result:=TObject(FHashList.Find(s));
end;


Function TFPHashObjectList.FindIndexOf(const s:shortstring): Integer;
begin
  Result:=FHashList.FindIndexOf(s);
end;


Function TFPHashObjectList.FindWithHash(const AName:shortstring;AHash:LongWord): Pointer;
begin
  Result:=TObject(FHashList.FindWithHash(AName,AHash));
end;


Function TFPHashObjectList.Rename(const AOldName,ANewName:shortstring): Integer;
begin
  Result:=FHashList.Rename(AOldName,ANewName);
end;


Function TFPHashObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  if AExact then
    while (I<Count) and (Result=-1) do
      if Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      if Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;


Procedure TFPHashObjectList.Pack;
begin
  FHashList.Pack;
end;


Procedure TFPHashObjectList.ShowStatistics;
begin
  FHashList.ShowStatistics;
end;


Procedure TFPHashObjectList.ForEachCall(proc2call:TObjectListCallback;arg:pointer);
begin
  FHashList.ForEachCall(TListCallBack(proc2call),arg);
end;


Procedure TFPHashObjectList.ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer);
begin
  FHashList.ForEachCall(TListStaticCallBack(proc2call),arg);
end;


{ ---------------------------------------------------------------------
    Hash support, by Dean Zobec
  ---------------------------------------------------------------------}

{ Default hash Function }

Function RSHash(const S: string; const TableSize: Longword): Longword;
const
  b = 378551;
var
  a: Longword;
  i: Longword;
begin
  a:=63689;
  Result:=0;
  if length(s)>0 then
    for i:=1 to Length(S) do
      begin
      Result:=Result * a + Ord(S[i]);
      a:=a * b;
      end;
  Result:=(Result and $7FFFFFFF) mod TableSize;
end;

{ THTNode }

constructor THTCustomNode.CreateWith(const AString: string);
begin
  inherited Create;
  FKey:=AString;
end;

Function THTCustomNode.HasKey(const AKey: string): boolean;
begin
  if Length(AKey) <> Length(FKey) then
    begin
    Result:=false;
    Exit;
    end
  else
    Result:=CompareMem(PChar(FKey), PChar(AKey), Length(AKey));
end;

{ TFPCustomHashTable }

constructor TFPCustomHashTable.Create;
begin
  CreateWith(196613,@RSHash);
end;

constructor TFPCustomHashTable.CreateWith(AHashTableSize: Longword;
  aHashFunc: THashFunction);
begin
  inherited Create;
  FHashTable:=TFPObjectList.Create(True);
  HashTableSize:=AHashTableSize;
  FHashFunction:=aHashFunc;
end;

destructor TFPCustomHashTable.Destroy;
begin
  FHashTable.Free;
  inherited Destroy;
end;

Function TFPCustomHashTable.GetDensity: Longword;
begin
  Result:=FHashTableSize - VoidSlots
end;

Function TFPCustomHashTable.GetNumberOfCollisions: Longword;
begin
  Result:=FCount -(FHashTableSize - VoidSlots)
end;

Procedure TFPCustomHashTable.SetHashTableSize(const Value: Longword);
var
  i: Longword;
  newSize: Longword;
begin
  if Value <> FHashTableSize then
    begin
    i:=0;
    while (PRIMELIST[i] < Value) and (i < 27) do
     Inc(i);
    newSize:=PRIMELIST[i];
    if Count = 0 then
      begin
      FHashTableSize:=newSize;
      InitializeHashTable;
      end
    else
      ChangeTableSize(newSize);
    end;
end;

Procedure TFPCustomHashTable.InitializeHashTable;
var
  i: LongWord;
begin
  if FHashTableSize>0 Then
    for i:=0 to FHashTableSize-1 do
      FHashTable.Add(nil);
  FCount:=0;
end;

Procedure TFPCustomHashTable.ChangeTableSize(const ANewSize: Longword);
var
  SavedTable: TFPObjectList;
  SavedTableSize: Longword;
  i, j: Longword;
  temp: THTCustomNode;
begin
  SavedTable:=FHashTable;
  SavedTableSize:=FHashTableSize;
  FHashTableSize:=ANewSize;
  FHashTable:=TFPObjectList.Create(True);
  InitializeHashTable;
  if SavedTableSize>0 Then
    for i:=0 to SavedTableSize-1 do
      if Assigned(SavedTable[i]) then
        for j:=0 to TFPObjectList(SavedTable[i]).Count -1 do
          begin
          temp:=THTCustomNode(TFPObjectList(SavedTable[i])[j]);
          AddNode(temp);
          end;
  SavedTable.Free;
end;

Procedure TFPCustomHashTable.SetHashFunction(AHashFunction: THashFunction);
begin
  if IsEmpty then
    FHashFunction:=AHashFunction
  else
    raise Exception.Create(NotEmptyMsg);
end;

Function TFPCustomHashTable.Find(const aKey: string): THTCustomNode;
var
  hashCode: Longword;
  chn: TFPObjectList;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  chn:=Chain(hashCode);
  if Assigned(chn) then
    if chn.count>0 then
      for i:=0 to chn.Count - 1 do
        if THTCustomNode(chn[i]).HasKey(aKey) then
          begin
          Result:=THTCustomNode(chn[i]);
          Exit;
          end;
  Result:=nil;
end;

Function TFPCustomHashTable.FindChainForAdd(Const aKey : String) : TFPObjectList;
var
  hashCode: Longword;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  Result:=Chain(hashCode);
  if Assigned(Result)  then
    begin
    if Result.count>0 then
      for i:=0 to Result.Count - 1 do
        if THTCustomNode(Result[i]).HasKey(aKey) then
          raise EDuplicate.CreateFmt(DuplicateMsg, [aKey]);
    end
  else
    begin
    FHashTable[hashcode]:=TFPObjectList.Create(True);
    Result:=Chain(hashCode);
    end;
  Inc(FCount);
end;


Procedure TFPCustomHashTable.Delete(const aKey: string);
var
  hashCode: Longword;
  chn: TFPObjectList;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  chn:=Chain(hashCode);
  if Assigned(chn) then
    if chn.count>0 then
      for i:=0 to chn.Count - 1 do
        if THTCustomNode(chn[i]).HasKey(aKey) then
          begin
          chn.Delete(i);
          dec(FCount);
          Exit;
          end;
end;

Function TFPCustomHashTable.IsEmpty: boolean;
begin
  Result:=(FCount = 0);
end;

Function TFPCustomHashTable.Chain(const index: Longword): TFPObjectList;
begin
  Result:=TFPObjectList(FHashTable[index]);
end;

Function TFPCustomHashTable.GetVoidSlots: Longword;
var
  i: Longword;
  num: Longword;
begin
  num:=0;
  if FHashTableSize>0 then
    for i:= 0 to FHashTableSize-1 do
      if not Assigned(Chain(i)) then
        Inc(num);
  Result:=num;
end;

Function TFPCustomHashTable.GetLoadFactor: double;
begin
  Result:=Count / FHashTableSize;
end;

Function TFPCustomHashTable.GetAVGChainLen: double;
begin
  Result:=Count / (FHashTableSize - VoidSlots);
end;

Function TFPCustomHashTable.GetMaxChainLength: Longword;
var
  i: Longword;
begin
  Result:=0;
  if FHashTableSize>0 Then
   for i:=0 to FHashTableSize-1 do
      if ChainLength(i) > Result then
        Result:=ChainLength(i);
end;

Function TFPCustomHashTable.FindOrCreateNew(const aKey: string): THTCustomNode;
var
  hashCode: Longword;
  chn: TFPObjectList;
  i: Longword;
begin
  hashCode:=FHashFunction(aKey, FHashTableSize);
  chn:=Chain(hashCode);
  if Assigned(chn)  then
    begin
    if chn.count>0 then
      for i:=0 to chn.Count - 1 do
        if THTCustomNode(chn[i]).HasKey(aKey) then
          begin
          Result:=THTNode(chn[i]);
          Exit;
          end
    end
  else
    begin
    FHashTable[hashcode]:=TFPObjectList.Create(true);
    chn:=Chain(hashcode);
    end;
  Inc(FCount);
  Result:=CreateNewNode(aKey);
  chn.Add(Result);
end;

Function TFPCustomHashTable.ChainLength(const ChainIndex: Longword): Longword;
begin
  if Assigned(Chain(ChainIndex)) then
    Result:=Chain(ChainIndex).Count
  else
    Result:=0;
end;

Procedure TFPCustomHashTable.Clear;
var
  i: Longword;
begin
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize - 1 do
      if Assigned(Chain(i)) then
        Chain(i).Clear;
  FCount:=0;
end;



{ TFPDataHashTable }

Procedure TFPDataHashTable.Add(const aKey: string; aItem: pointer);
var
  chn: TFPObjectList;
  NewNode: THtDataNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=THtDataNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TFPDataHashTable.GetData(const Index: string): Pointer;
var
  node: THTDataNode;
begin
  node:=THTDataNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:=nil;
end;

Procedure TFPDataHashTable.SetData(const index: string; const AValue: Pointer);
begin
  THTDataNode(FindOrCreateNew(index)).Data:=AValue;
end;

Function TFPDataHashTable.CreateNewNode(const aKey : string) : THTCustomNode;

begin
  Result:=THTDataNode.CreateWith(aKey);
end;

Function TFPDataHashTable.Iterate(aMethod: TDataIteratorMethod): Pointer;
var
  N : THTDataNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
  else
    Result:=nil;
end;

Function TFPDataHashTable.ForEachCall(aMethod: TDataIteratorMethod): THTDataNode;
var
  i, j: Longword;
  continue: Boolean;
begin
  Result:=nil;
  continue:=true;
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize-1 do
      if Assigned(Chain(i)) then
        if chain(i).count>0 then
          for j:=0 to Chain(i).Count-1 do
            begin
            aMethod(THTDataNode(Chain(i)[j]).Data, THTDataNode(Chain(i)[j]).Key, continue);
            if not continue then
              begin
              Result:=THTDataNode(Chain(i)[j]);
              Exit;
              end;
           end;
end;

Procedure TFPDataHashTable.AddNode(ANode : THTCustomNode);
begin
  with THTDataNode(ANode) do
    Add(Key,Data);
end;

{ TFPStringHashTable }

Procedure TFPStringHashTable.AddNode(ANode : THTCustomNode);
begin
  with THTStringNode(ANode) do
    Add(Key,Data);
end;

Function TFPStringHashTable.GetData(const Index: string): String;
var
  node: THTStringNode;
begin
  node:=THTStringNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:='';
end;

Procedure TFPStringHashTable.SetData(const index, AValue: string);
begin
  THTStringNode(FindOrCreateNew(index)).Data:=AValue;
end;

Procedure TFPStringHashTable.Add(const aKey, aItem: string);
var
  chn: TFPObjectList;
  NewNode: THtStringNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=THtStringNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TFPStringHashTable.CreateNewNode(const aKey : string) : THTCustomNode;
begin
  Result:=THTStringNode.CreateWith(aKey);
end;

Function TFPStringHashTable.Iterate(aMethod: TStringIteratorMethod): String;
var
  N : THTStringNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
  else
    Result:='';
end;

Function TFPStringHashTable.ForEachCall(aMethod: TStringIteratorMethod): THTStringNode;
var
  i, j: Longword;
  continue: boolean;
begin
  Result:=nil;
  continue:=True;
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize-1 do
      if Assigned(Chain(i)) then
        if chain(i).Count>0 then
          for j:=0 to Chain(i).Count-1 do
            begin
            aMethod(THTStringNode(Chain(i)[j]).Data, THTStringNode(Chain(i)[j]).Key, continue);
            if not continue then
              begin
              Result:=THTStringNode(Chain(i)[j]);
              Exit;
              end;
            end;
end;

{ TFPObjectHashTable }

Procedure TFPObjectHashTable.AddNode(ANode : THTCustomNode);
begin
  With THTObjectNode(ANode) do
    Add(Key,Data);
end;

Function TFPObjectHashTable.GetData(const Index: string): TObject;
var
  node: THTObjectNode;
begin
  node:=THTObjectNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:=nil;
end;

Procedure TFPObjectHashTable.SetData(const index : string; AObject : TObject);
begin
  THTObjectNode(FindOrCreateNew(index)).Data:=AObject;
end;

Procedure TFPObjectHashTable.Add(const aKey: string; AItem : TObject);
var
  chn: TFPObjectList;
  NewNode: THTObjectNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=THTObjectNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TFPObjectHashTable.CreateNewNode(const aKey : string) : THTCustomNode;
begin
  if OwnsObjects then
    Result:=THTOwnedObjectNode.CreateWith(aKey)
  else
    Result:=THTObjectNode.CreateWith(aKey);
end;


Function TFPObjectHashTable.Iterate(aMethod: TObjectIteratorMethod): TObject;
var
  N : THTObjectNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
  else
    Result:=nil;
end;

Function TFPObjectHashTable.ForEachCall(aMethod: TObjectIteratorMethod): THTObjectNode;
var
  i, j: Longword;
  continue: boolean;
begin
  Result:=nil;
  continue:=true;
  if FHashTableSize>0 then
    for i:=0 to FHashTableSize-1 do
      if Assigned(Chain(i)) then
        if Chain(i).Count>0 then
          for j:=0 to Chain(i).Count-1 do
           begin
           aMethod(THTObjectNode(Chain(i)[j]).Data, THTObjectNode(Chain(i)[j]).Key, continue);
           if not continue then
             begin
             Result:=THTObjectNode(Chain(i)[j]);
             Exit;
             end;
           end;
end;

constructor TFPObjectHashTable.Create(AOwnsObjects : Boolean = True);
begin
  inherited Create;
  FOwnsObjects:=AOwnsObjects;
end;

constructor TFPObjectHashTable.CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction; AOwnsObjects : Boolean = True);
begin
  inherited CreateWith(AHashTableSize,AHashFunc);
  FOwnsObjects:=AOwnsObjects;
end;

destructor THTOwnedObjectNode.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

{ TCustomBucketList }

Function TCustomBucketList.GetData(AItem: Pointer): Pointer;
var
  B,I : Integer;
begin
  GetBucketItem(AItem,B,I);
  Result:=FBuckets[B].Items[I].Data;
end;

Function TCustomBucketList.GetBucketCount: Integer;
begin
  Result:=Length(FBuckets);
end;

Procedure TCustomBucketList.SetData(AItem: Pointer; const AData: Pointer);
var
  B,I : Integer;
begin
  GetBucketItem(AItem,B,I);
  FBuckets[B].Items[I].Data:=AData;
end;

Procedure TCustomBucketList.SetBucketCount(const Value: Integer);
begin
  if (Value<>GetBucketCount) then
    SetLength(FBuckets,Value);
end;

Procedure TCustomBucketList.GetBucketItem(AItem: Pointer; out ABucket,
  AIndex: Integer);
begin
  if not FindItem(AItem,ABucket,AIndex) then
    Error(SErrNoSuchItem,[AItem]);
end;

Function TCustomBucketList.AddItem(ABucket: Integer; AItem, AData: Pointer
  ): Pointer;
var
  B : PBucket;
  L : Integer;
begin
  B:=@FBuckets[ABucket];
  L:=Length(B^.Items);
  if (B^.Count=L) then
    begin
    if L<8 then
      L:=8
    else
      L:=L+L div 2;
    SetLength(B^.Items,L);
    end;
  with B^ do
    begin
    Items[Count].Item:=AItem;
    Items[Count].Data:=AData;
    Result:=AData;
    Inc(Count);
    end;
end;

Function TCustomBucketList.DeleteItem(ABucket: Integer; AIndex: Integer): Pointer;
var
  B : PBucket;
  L : Integer;
begin
  B:=@FBuckets[ABucket];
  Result:=B^.Items[AIndex].Data;
  if B^.Count=1 then
    SetLength(B^.Items,0)
  else
    begin
    L:=(B^.Count-AIndex-1);// No point in moving if last one...
    if L>0 then
      Move(B^.Items[AIndex+1],B^.Items[AIndex],L*SizeOf(TBucketItem));
    end;
  Dec(B^.Count);
end;

Procedure TCustomBucketList.Error(Msg: String; Args: array of const);
begin
  raise ElistError.CreateFmt(Msg,Args);
end;

Function TCustomBucketList.FindItem(AItem: Pointer; out ABucket, AIndex: Integer
  ): Boolean;
var
  I : Integer;
  B : TBucket;
begin
  ABucket:=BucketFor(AItem);
  B:=FBuckets[ABucket];
  I:=B.Count-1;
  while (I>=0) and (B.Items[I].Item<>AItem) do
    Dec(I);
  Result:=I>=0;
  if Result then
    AIndex:=I;
end;

destructor TCustomBucketList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

Procedure TCustomBucketList.Clear;
var
  B : TBucket;
  I,J : Integer;
begin
  for I:=0 to Length(FBuckets)-1 do
    begin
    B:=FBuckets[I];
    for J:=B.Count-1 downto 0 do
      DeleteItem(I,J);
    end;
  SetLength(FBuckets,0);
end;

Function TCustomBucketList.Add(AItem, AData: Pointer): Pointer;
var
  B,I : Integer;
begin
  if FindItem(AItem,B,I) then
    Error(SDuplicateItem,[AItem]);
  Result:=AddItem(B,AItem,AData);
end;

Procedure TCustomBucketList.Assign(AList: TCustomBucketList);
var
  I,J : Integer;
begin
  Clear;
  SetLength(FBuckets,Length(Alist.FBuckets));
  for I:=0 to BucketCount-1 do
    begin
    SetLength(FBuckets[i].Items,Length(AList.Fbuckets[I].Items));
    for J:=0 to AList.Fbuckets[I].Count-1 do
      with AList.Fbuckets[I].Items[J] do
        AddItem(I,Item,Data);
    end;
end;

Function TCustomBucketList.Exists(AItem: Pointer): Boolean;
var
  B,I : Integer;
begin
  Result:=FindItem(AItem,B,I);
end;

Function TCustomBucketList.Find(AItem: Pointer; out AData: Pointer): Boolean;
var
  B,I : integer;
begin
  Result:=FindItem(AItem,B,I);
  if Result then
    AData:=FBuckets[B].Items[I].Data;
end;

Function TCustomBucketList.ForEach(AProc: TBucketProc; AInfo: Pointer
  ): Boolean;
var
  I,J,S : Integer;
  Bu : TBucket;
begin
  I:=0;
  Result:=True;
  S:=GetBucketCount;
  while Result and (I<S) do
    begin
    J:=0;
    Bu:=FBuckets[I];
    while Result and (J<Bu.Count) do
      begin
      with Bu.Items[J] do
        AProc(AInfo,Item,Data,Result);
      Inc(J);
      end;
    Inc(I);
    end;
end;

Function TCustomBucketList.ForEach(AProc: TBucketProcObject): Boolean;
var
  I,J,S : Integer;
  Bu : TBucket;
begin
  I:=0;
  Result:=True;
  S:=GetBucketCount;
  while Result and (I<S) do
    begin
    J:=0;
    Bu:=FBuckets[I];
    while Result and (J<Bu.Count) do
      begin
      with Bu.Items[J] do
        AProc(Item,Data,Result);
      Inc(J);
      end;
    Inc(I);
    end;
end;

Function TCustomBucketList.Remove(AItem: Pointer): Pointer;
var
  B,I : integer;
begin
  if FindItem(AItem,B,I) then
    begin
    Result:=FBuckets[B].Items[I].Data;
    DeleteItem(B,I);
    end
  else
    Result:=nil;
end;

{ TBucketList }

Function TBucketList.BucketFor(AItem: Pointer): Integer;
begin
  // Pointers on average have a granularity of 4
  Result:=(PtrInt(AItem) shr 2) and FBucketMask;
end;

constructor TBucketList.Create(ABuckets: TBucketListSizes);
var
  L : Integer;
begin
  inherited Create;
  L:=1 shl (Ord(Abuckets)+1);
  SetBucketCount(L);
  FBucketMask:=L-1;
end;

{ TObjectBucketList }

Function TObjectBucketList.GetData(AItem: TObject): TObject;
begin
  Result:=TObject(inherited GetData(AItem));
end;

Procedure TObjectBucketList.SetData(AItem: TObject; const AData: TObject);
begin
  inherited SetData(Pointer(AItem),Pointer(AData));
end;

Function TObjectBucketList.Add(AItem, AData: TObject): TObject;
begin
  Result:=TObject(inherited Add(Pointer(AItem),Pointer(AData)));
end;

Function TObjectBucketList.Remove(AItem: TObject): TObject;
begin
  Result:=TObject(inherited Remove(Pointer(AItem)));
end;

end.
