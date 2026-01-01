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
{$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}

{$IFNDEF FPC_DOTTEDUNITS}
unit Contnrs;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,System.Classes;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils,Classes;
{$ENDIF FPC_DOTTEDUNITS}


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


{ "Vi" stands for variable-sized indices.
  Variable-sized indices use less space and reduce the size of a region with potentially chaotic accesses (FHash).

  Indices are bitpacked. For speed and simplicity, bitfield base type is the same as index type (SizeUint),
  and maximum bit size is bitsizeof(SizeUint) - 1, to allow unconditional masking with "1 shl bitsPerIndex - 1", etc. }

  function ViGet(data: PSizeUint; index, bitsPerIndex: SizeUint): SizeUint;
  procedure ViSet(data: PSizeUint; index, bitsPerIndex, value: SizeUint);
  function ViDataSize(n, bitsPerIndex: SizeUint): SizeUint;

const
  ViEmpty = 0;
  ViRealIndexOffset = 1;

type
  PViHashListItem = ^TFPHashListItem;
  TFPHashListItem = record
    HashValue: uint32;
    Next: int32;
    Str: RawByteString;
    Data: Pointer;
  end;

  TViRehashMode = (vi_Auto, vi_Tight, vi_Pack);

  { TFPHashList }

  TFPHashList = class(TObject)
  private
    { When not special "empty list", that is, when Assigned(FItems), FHash is a memory region containing FHash + FItems. }
    FHash: PSizeUint; { Bitpacked hash table. ViEmpty means empty cell, ViRealIndexOffset+i references FItems[i]. }
    FItems: PViHashListItem;
    FBitsPerIndex: uint8; { Size of indices in FHash. }
    FHashMask: uint32; { Count of indices in FHash is always "FHashMask + 1" and is always a power of two. }
    FCount: int32;
    FCapacity: uint32; { Allocation size of FItems. Generally speaking, can be arbitrary, without any relation to "FHashMask + 1". }
    function Get(Index: SizeInt): Pointer;
    procedure Put(Index: SizeInt; Item: Pointer);
    class procedure RaiseIndexError(Index: SizeInt); static;
    procedure SetupEmptyTable;
    procedure Rehash(ForItems: SizeUint; mode: TViRehashMode=vi_Auto);
    procedure Shrink;
    procedure AddToHashTable(Item: PViHashListItem; Index: SizeUint);
    function InternalFind(AHash:LongWord;const AName:RawByteString;out PrevIndex:SizeInt):SizeInt;
    procedure RemoveFromHashTable(AHash:LongWord;Index, PrevIndex: SizeInt);
    procedure SetCapacity(NewCapacity: uint32);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName:RawByteString;Item: Pointer): SizeInt;
    procedure Clear;
    function NameOfIndex(Index: SizeInt): RawByteString;
    function HashOfIndex(Index: SizeInt): LongWord;
    function GetNextCollision(Index: SizeInt): SizeInt; inline;
    procedure Delete(Index: SizeInt);
    class procedure Error(const Msg: AnsiString; Data: PtrInt);
    function Extract(item: Pointer): Pointer;
    function IndexOf(Item: Pointer): SizeInt;
    function Find(const AName:RawByteString): Pointer; inline;
    function FindIndexOf(const AName:RawByteString): SizeInt; inline;
    function FindWithHash(const AName:RawByteString;AHash:LongWord): Pointer;
    function Rename(const AOldName,ANewName:RawByteString): SizeInt;
    function Remove(Item: Pointer): SizeInt;
    procedure Pack;
    procedure ForEachCall(proc2call:TListCallback;arg:pointer);
    procedure ForEachCall(proc2call:TListStaticCallback;arg:pointer);
    property Count: int32 read FCount;
    property Capacity: uint32 read FCapacity write SetCapacity;
    property Items[Index: SizeInt]: Pointer read Get write Put; default;
    property List: PViHashListItem read FItems;
  end;


{*******************************************************
        TFPHashObjectList
********************************************************}

  TFPHashObjectList = class;

  { TFPHashObject }

   TFPHashObject = class
  private
    FOwner     : TFPHashObjectList;
    FStr       : RawByteString;
    FHash      : LongWord;
    procedure InternalChangeOwner(HashObjectList:TFPHashObjectList;const s:RawByteString);
  protected
    function GetName:RawByteString;virtual;
    function GetHash:Longword;virtual;
  public
    constructor CreateNotOwned;
    constructor Create(HashObjectList:TFPHashObjectList;const s:RawByteString);
    procedure ChangeOwner(HashObjectList:TFPHashObjectList);
    procedure ChangeOwnerAndName(HashObjectList:TFPHashObjectList;const s:RawByteString); inline;
    procedure Rename(const ANewName:RawByteString);
    property Name:RawByteString read GetName;
    property Hash:Longword read GetHash;
    property OwnerList: TFPHashObjectList read FOwner;
  end;

  { TFPHashObjectList }

  TFPHashObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FHashList: TFPHashList;
    function GetCount: integer; inline;
  protected
    function GetItem(Index: Integer): TObject; inline;
    procedure SetItem(Index: Integer; AObject: TObject);
    procedure SetCapacity(NewCapacity: Integer); inline;
    function GetCapacity: integer; inline;
  public
    constructor Create(FreeObjects : boolean = True);
    destructor Destroy; override;
    procedure Clear;
    function Add(const AName:RawByteString;AObject: TObject): Integer;
    function NameOfIndex(Index: Integer): RawByteString; inline;
    function HashOfIndex(Index: Integer): LongWord; inline;
    function GetNextCollision(Index: Integer): Integer; inline;
    procedure Delete(Index: Integer);
    function Extract(Item: TObject): TObject; inline;
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer; inline;
    function Find(const s:RawByteString): TObject;
    function FindIndexOf(const s:RawByteString): Integer;
    function FindWithHash(const AName:RawByteString;AHash:LongWord): Pointer; inline;
    function Rename(const AOldName,ANewName:RawByteString): Integer;
    function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    procedure Pack; inline;
    procedure ForEachCall(proc2call:TObjectListCallback;arg:pointer); inline;
    procedure ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer); inline;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TFPHashList read FHashList;
  end;

{ ---------------------------------------------------------------------
    Hash support, implemented by Dean Zobec
  ---------------------------------------------------------------------}


  { Must return a Longword value in the range 0..TableSize,
   usually via a mod operator;  }
  THashFunction = Function(const S: AnsiString; const TableSize: Longword): Longword;


  { THTNode }

  THTCustomNode = class(TObject)
  private
    FKey: AnsiString;
  public
    constructor CreateWith(const AString: AnsiString);
    Function HasKey(const AKey: AnsiString): boolean;
    property Key: AnsiString read FKey;
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
    Function CreateNewNode(const aKey : AnsiString) : THTCustomNode; virtual; abstract;
    Procedure AddNode(ANode : THTCustomNode); virtual; abstract;
    Function ChainLength(const ChainIndex: Longword): Longword; virtual;
    Function FindOrCreateNew(const aKey: AnsiString): THTCustomNode; virtual;
    Procedure SetHashFunction(AHashFunction: THashFunction); virtual;
    Function FindChainForAdd(Const aKey : AnsiString) : TFPObjectList;
  public
    constructor Create;
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction);
    destructor Destroy; override;
    Procedure ChangeTableSize(const ANewSize: Longword); virtual;
    Procedure Clear; virtual;
    Procedure Delete(const aKey: AnsiString); virtual;
    Function Find(const aKey: AnsiString): THTCustomNode;
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

  TDataIteratorMethod = Procedure(Item: Pointer; const Key: AnsiString; var Continue: Boolean) of object;
  TDataIteratorCallBack = Procedure(Item: Pointer; const Key: AnsiString; var Continue: Boolean);

  // For compatibility
  TIteratorMethod = TDataIteratorMethod;

  TFPDataHashTable = Class(TFPCustomHashTable)
  Private
    FIteratorCallBack: TDataIteratorCallBack;
    Procedure CallbackIterator(Item: Pointer; const Key: AnsiString; var Continue: Boolean);
  Protected
    Function CreateNewNode(const aKey : AnsiString) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    Procedure SetData(const index: AnsiString; const AValue: Pointer); virtual;
    Function GetData(const index: AnsiString):Pointer; virtual;
    Function ForEachCall(aMethod: TDataIteratorMethod): THTDataNode; virtual;
  Public
    Function Iterate(aMethod: TDataIteratorMethod): Pointer; virtual;
    Function Iterate(aMethod: TDataIteratorCallBack): Pointer; virtual;
    Procedure Add(const aKey: AnsiString; AItem: pointer); virtual;
    property Items[const index: AnsiString]: Pointer read GetData write SetData; default;
  end;

  { TFPStringHashTable : Hash table with simple strings as data }
  THTStringNode = Class(THTCustomNode)
  Private
    FData : AnsiString;
  public
    property Data: AnsiString  read FData write FData;
  end;

  TStringIteratorMethod = Procedure(Item: AnsiString; const Key: AnsiString; var Continue: Boolean) of object;
  TStringIteratorCallback = Procedure(Item: AnsiString; const Key: AnsiString; var Continue: Boolean);

  TFPStringHashTable = Class(TFPCustomHashTable)
  Private
    FIteratorCallBack: TStringIteratorCallback;
    Procedure CallbackIterator(Item: AnsiString; const Key: AnsiString; var Continue: Boolean);
  Protected
    Function CreateNewNode(const aKey : AnsiString) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    Procedure SetData(const Index, AValue: AnsiString); virtual;
    Function GetData(const index: AnsiString): AnsiString; virtual;
    Function ForEachCall(aMethod: TStringIteratorMethod): THTStringNode; virtual;
  Public
    Function Iterate(aMethod: TStringIteratorMethod): AnsiString; virtual;
    Function Iterate(aMethod: TStringIteratorCallback): AnsiString; virtual;
    Procedure Add(const aKey,aItem: AnsiString); virtual;
    property Items[const index: AnsiString]: AnsiString read GetData write SetData; default;
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

  TObjectIteratorMethod = Procedure(Item: TObject; const Key: AnsiString; var Continue: Boolean) of object;
  TObjectIteratorCallback = Procedure(Item: TObject; const Key: AnsiString; var Continue: Boolean);

  TFPObjectHashTable = Class(TFPCustomHashTable)
  Private
    FOwnsObjects : Boolean;
    FIteratorCallBack: TObjectIteratorCallback;
    procedure CallbackIterator(Item: TObject; const Key: AnsiString; var Continue: Boolean);
  Protected
    Function CreateNewNode(const aKey : AnsiString) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    Procedure SetData(const Index: AnsiString; AObject : TObject); virtual;
    Function GetData(const index: AnsiString): TObject; virtual;
    Function ForEachCall(aMethod: TObjectIteratorMethod): THTObjectNode; virtual;
  Public
    constructor Create(AOwnsObjects : Boolean = True);
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction; AOwnsObjects : Boolean = True);
    Function Iterate(aMethod: TObjectIteratorMethod): TObject; virtual;
    Function Iterate(aMethod: TObjectIteratorCallback): TObject; virtual;
    Procedure Add(const aKey: AnsiString; AItem : TObject); virtual;
    property Items[const index:Ansistring ]: TObject read GetData write SetData; default;
    Property OwnsObjects : Boolean Read FOwnsObjects;
  end;

  EDuplicate = class(Exception);
  EKeyNotFound = class(Exception);

  Function RSHash(const S: AnsiString; const TableSize: Longword): Longword;

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
    Procedure Error(Msg : AnsiString; Args : Array of Const);
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

function HashString(P: PAnsiChar; Len: Integer): LongWord; inline;
function HashString(const s: shortstring): LongWord; inline;
function HashString(const a: ansistring): LongWord; inline;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.RtlConsts;
{$ELSE FPC_DOTTEDUNITS}
uses
  RtlConsts;
{$ENDIF FPC_DOTTEDUNITS}

ResourceString
  DuplicateMsg   = 'An item with key %0:s already exists';
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

// MurmurHash3_32
function HashString(P: PAnsiChar; Len: Integer; Tag: LongWord): LongWord;
const
  C1 = uint32($cc9e2d51);
  C2 = uint32($1b873593);
var
  h, tail: uint32;
  e4: pAnsiChar;
  len4, nTail: SizeUint;
begin
{$push}
{$q-,r-}
  h := tag;

  len4 := len and not integer(sizeof(uint32) - 1); { len div sizeof(uint32) * sizeof(uint32) }
  e4 := p + len4;
  nTail := len - len4;
  while p < e4 do
    begin
      { If independence on endianness is desired, unaligned(pUint32(p)^) can be replaced with LEtoN(unaligned(pUint32(p)^)). }
      h := RolDWord(h xor (RolDWord(unaligned(pUint32(p)^) * C1, 15) * C2), 13) * 5 + $e6546b64;
      p := p + sizeof(uint32);
    end;

  if nTail > 0 then
    begin
      { tail is 1 to 3 bytes }
      case nTail of
        3: tail := unaligned(pUint16(p)^) or uint32(p[2]) shl 16; { unaligned(pUint16(p^)) can be LEtoNed for portability }
        2: tail := unaligned(pUint16(p)^); { unaligned(pUint16(p^)) can be LEtoNed for portability }
        {1:} else tail := uint32(p^);
      end;
      h := h xor (RolDWord(tail * C1, 15) * C2);
    end;

  h := h xor uint32(len);
  h := (h xor (h shr 16)) * $85ebca6b;
  h := (h xor (h shr 13)) * $c2b2ae35;
  result := h xor (h shr 16);
{$pop}
end;

function HashString(P: PAnsiChar; Len: Integer): LongWord;
begin
  Result:=HashString(P, Len, 0);
end;


function HashString(const s: shortstring): LongWord;
begin
  Result:=HashString(PAnsiChar(@s[1]), length(s), 0);
end;


function HashString(const a: ansistring): LongWord;
begin
  Result:=HashString(PAnsiChar(a), length(a), 0);
end;

function ViGet(data: PSizeUint; index, bitsPerIndex: SizeUint): SizeUint;
begin
  index:=index*bitsPerIndex;
  data:=data+index div bitsizeof(SizeUint);
  index:=index mod bitsizeof(SizeUint);
  result:=data^ shr index;
  index:=bitsizeof(data^)-index;
  if bitsPerIndex<=index then
    result:=result and (SizeUint(1) shl bitsPerIndex-1)
  else
    result:=result or data[1] shl index and (SizeUint(1) shl bitsPerIndex-1);
end;


procedure ViSet(data: PSizeUint; index, bitsPerIndex, value: SizeUint);
begin
  index:=index*bitsPerIndex;
  data:=data+index div bitsizeof(SizeUint);
  index:=index mod bitsizeof(SizeUint);
  if index+bitsPerIndex<=bitsizeof(data^) then
    data^:=data^ and not ((SizeUint(1) shl bitsPerIndex-1) shl index) or value shl index
  else
  begin
    data^:=SizeUint(data^ and (SizeUint(1) shl index - 1) or value shl index);
    index:=bitsizeof(data^)-index;
    value:=value shr index;
    index:=bitsPerIndex-index;
    data[1]:=data[1] shr index shl index or value;
  end;
end;


function ViDataSize(n, bitsPerIndex: SizeUint): SizeUint;
begin
  result:=(n*bitsPerIndex+(bitsizeof(SizeUint)-1)) div bitsizeof(SizeUint)*sizeof(SizeUint);
end;



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


function TFPHashList.Get(Index: SizeInt): Pointer;
begin
  If SizeUint(Index)>=SizeUint(FCount) then
    RaiseIndexError(Index);
  Result:=FItems[Index].Data;
end;


procedure TFPHashList.Put(Index: SizeInt; Item: Pointer);
begin
  If SizeUint(Index)>=SizeUint(FCount) then
    RaiseIndexError(Index);
  FItems[Index].Data:=Item;
end;


class procedure TFPHashList.RaiseIndexError(Index: SizeInt);
begin
  TFPList.Error(SListIndexError, Index);
end;


procedure TFPHashList.SetupEmptyTable;
const
  { 1-element FHash array containing one zero, which is ViEmpty.
    Any searches will answer "not found", and any additions will instantly rehash. }
  EmptyFHash: SizeUint = 0;
begin
  FHash:=@EmptyFHash;
  FItems:=nil;
  FBitsPerIndex:=1;
  FHashMask:=0;
  FCapacity:=0;
end;


procedure TFPHashList.Rehash(ForItems: SizeUint; mode: TViRehashMode=vi_Auto);
var
  newCapacity, newHashMask, newBitsPerIndex, itemsOffset, regionSize: SizeUint;
  i: SizeInt;
  newHash: PSizeUint;
  newItems: PViHashListItem;
  shortcutReAdd: boolean;
begin
  if ForItems=0 then
    begin
      Clear;
      exit;
    end;
  if ForItems>MaxHashListSize then
    TFPList.Error(SListCapacityError, ForItems);

  { Can be something like "137.5% ForItems", but with bitwise indices, better to just derive the capacity later from chosen index type limit,
    which will be 200% at most -
    this way, both capacity and hash mask size become beautiful powers of two,
    saving on rehashes ("shortcutReAdd" branch, while still required for degenerate scenarios, becomes de facto unreachable),
    and often even on memory (though the reason for the latter is unclear to me; maybe "137.5%" in conjunction with "UpToPow2" introduces extra breakpoints). }
  newCapacity:=ForItems;

  { Max index for "capacity" items is "ViRealIndexOffset + (capacity - 1)", which can be rewritten as "capacity + (ViRealIndexOffset - 1)". }
  newBitsPerIndex:=1+BsrDWord(newCapacity+(ViRealIndexOffset-1));
  if not ((newBitsPerIndex>=1) and (newBitsPerIndex<=bitsizeof(SizeUint)-1)) then
    Error('TFPHashList.Rehash - newBitsPerIndex : %d', newBitsPerIndex);

  { In place of explicit over-allocation, increase capacity to index type limit. }
  if mode<>vi_Tight then
    newCapacity:=(SizeUint(1) shl newBitsPerIndex-1)-(ViRealIndexOffset-1);

  { Take item list capacity rounded up to power of two. This can give 50% to 100% load factor.
    If it gives more than 3/4, double the hash capacity again. After that, possible load factors will range from 37.5% to 75%.
    Even load factors greater than 100% will work though. Low factors are just slightly faster, at the expense of memory. }
  newHashMask:=SizeUint(1) shl (1+BsrDWord((newCapacity-1) or 1))-1; { UpToPow2(newCapacity)-1 }
  if newHashMask div 4*3<newCapacity then
    newHashMask:=newHashMask*2+1;

  { Allocating and marking up the region for FHash + FItems. }
  itemsOffset:=Align(ViDataSize(newHashMask+1,newBitsPerIndex), SizeUint(sizeof(pointer)));
  regionSize:=itemsOffset+sizeof(TFPHashListItem)*newCapacity;
  newHash:=GetMem(regionSize);
  newItems:=pointer(newHash)+itemsOffset;

  { If hash mask hasn't changed (this is possible because of arbitrariness of FCapacity),
    items re-adding can be, and is, shortcutted.
    .Pack corrupts indices and expects from .Rehash to recalculate them, so is incompatible with this. }
  shortcutReAdd:=(FHashMask=newHashMask) and (mode<>vi_Pack);
  if shortcutReAdd then
    begin
      { If even index type hasn't changed, just copy FHash. Else convert. }
      if newBitsPerIndex=FBitsPerIndex then
        Move(FHash^, newHash^, ViDataSize(newHashMask+1,newBitsPerIndex))
      else
        for i:=0 to newHashMask do
          ViSet(newHash, i, newBitsPerIndex, ViGet(FHash, i, FBitsPerIndex));
    end
  else
    { Otherwise set all indices to ViEmpty. }
    FillChar(newHash^, ViDataSize(newHashMask+1,newBitsPerIndex), 0);

  { Move items as raw memory, even managed (old area is then deallocated without finalizing). }
  Move(FItems^, newItems^, FCount*sizeof(TFPHashListItem));

  { Free the old table. "Assigned(FItems)" means that the table was not the fake table set up by SetupEmptyTable.
    Items were just moved into a new place so shouldn't be finalized. }
  if Assigned(FItems) then
    FreeMem(FHash);

  FHash:=newHash;
  FItems:=newItems;
  FBitsPerIndex:=newBitsPerIndex;
  FHashMask:=newHashMask;
  FCapacity:=newCapacity;

  { Re-add items if re-adding was not shortcutted before. }
  if not shortcutReAdd then
    for i:=0 to FCount-1 do
      AddToHashTable(FItems+i, i);
end;


procedure TFPHashList.Shrink;
begin
  if (FCapacity >= 64) and (uint32(FCount) < FCapacity div 4) then
    Rehash(uint32(FCount)+uint32(FCount) div 4);
end;


procedure TFPHashList.AddToHashTable(Item: PViHashListItem; Index: SizeUint);
var
  HashIndex: SizeUint;
begin
  if not Assigned(Item^.Data) then
    exit;
  HashIndex:=Item^.HashValue and FHashMask;
  FItems[Index].Next:=SizeInt(ViGet(FHash, HashIndex, FBitsPerIndex))-ViRealIndexOffset;
  ViSet(FHash, HashIndex, FBitsPerIndex, ViRealIndexOffset+Index);
end;


function TFPHashList.InternalFind(AHash:LongWord;const AName:RawByteString;out PrevIndex:SizeInt):SizeInt;
var
  it: PViHashListItem;
begin
  Result:=SizeInt(ViGet(FHash, AHash and FHashMask, FBitsPerIndex))-ViRealIndexOffset;
  PrevIndex:=-1;
  repeat
    if Result<0 then
      exit;
    it:=FItems+Result;
    if Assigned(it^.Data) and (AHash=it^.HashValue) and (AName=it^.Str) then
      exit;
    PrevIndex:=Result;
    Result:=FItems[Result].Next;
  until false;
end;


procedure TFPHashList.RemoveFromHashTable(AHash:LongWord;Index, PrevIndex: SizeInt);
var
  next: SizeInt;
begin
  next:=SizeInt(FItems[Index].Next);
  if PrevIndex<0 then
    ViSet(FHash, AHash and FHashMask, FBitsPerIndex, ViRealIndexOffset+next)
  else
    FItems[PrevIndex].Next:=next;
end;


procedure TFPHashList.SetCapacity(NewCapacity: uint32);
begin
  if NewCapacity < uint32(FCount) then Error(SListCountError, NewCapacity);
  Rehash(NewCapacity, vi_Tight);
end;


constructor TFPHashList.Create;
begin
  inherited Create;
  SetupEmptyTable;
end;


destructor TFPHashList.Destroy;
begin
  Clear;
  inherited Destroy;
end;


function TFPHashList.Add(const AName:RawByteString;Item: Pointer): SizeInt;
var
  it: PViHashListItem;
begin
  result:=FCount;
  if uint32(result)=FCapacity then
    Rehash(result+1);

  it:=FItems+result;
  Initialize(it^);
  it^.HashValue:=HashString(AName);
  it^.Data:=Item;
  it^.Str:=AName;

  AddToHashTable(it, result);
  FCount:=result+1;
end;


procedure TFPHashList.Clear;
begin
  if Assigned(FItems) then
    begin
      Finalize(FItems^, FCount);
      FreeMem(FHash);
      SetupEmptyTable;
      FCount:=0;
    end;
end;


function TFPHashList.NameOfIndex(Index: SizeInt): RawByteString;
begin
  if SizeUint(Index)>=SizeUint(FCount) then
    RaiseIndexError(Index);
  result:=FItems[Index].Str;
end;


function TFPHashList.HashOfIndex(Index: SizeInt): LongWord;
begin
  if SizeUint(Index)>=SizeUint(FCount) then
    RaiseIndexError(Index);
  result:=FItems[Index].HashValue;
end;


function TFPHashList.GetNextCollision(Index: SizeInt): SizeInt;
begin
  Result:=FItems[Index].Next;
end;


procedure TFPHashList.Delete(Index: SizeInt);
var
  i: SizeInt;
begin
  If SizeUint(Index)>=SizeUint(FCount) then
    RaiseIndexError(Index);

  { Remove from array, shifting items above. }
  Finalize(FItems[Index]);
  Move(FItems[Index+1], FItems[Index], (FCount-Index-1)*sizeof(TFPHashListItem));
  dec(FCount);

  { Rebuild the table. This is much faster than trying to fix up indices. :( }
  FillChar(FHash^, ViDataSize(FHashMask+1, FBitsPerIndex), 0);
  for i:=0 to FCount-1 do
    AddToHashTable(FItems+i, i);
  Shrink;
end;


class procedure TFPHashList.Error(const Msg: AnsiString; Data: PtrInt);
 begin
  raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame), get_caller_frame(get_frame);
 end;


function TFPHashList.Extract(item: Pointer): Pointer;
var
  i : SizeInt;
begin
  result:=nil;
  i:=IndexOf(item);
  if i>=0 then
   begin
     Result:=item;
     Delete(i);
   end;
end;


function TFPHashList.IndexOf(Item: Pointer): SizeInt;
var
  itemp, iteme: PViHashListItem;
begin
  Result:=0;
  itemp:=FItems;
  iteme:=itemp+FCount;
  while itemp<iteme do
    begin
      if itemp^.Data=Item then
        exit;
      inc(itemp);
      inc(Result);
    end;
  Result:=-1;
end;


function TFPHashList.Find(const AName:RawByteString): Pointer;
begin
  Result:=FindWithHash(AName, HashString(ANAme));
end;


function TFPHashList.FindIndexOf(const AName:RawByteString): SizeInt;
var
  PrevIndex : SizeInt;
begin
  Result:=InternalFind(HashString(AName),AName,PrevIndex);
end;


function TFPHashList.FindWithHash(const AName:RawByteString;AHash:LongWord): Pointer;
var
  Index,
  PrevIndex : SizeInt;
begin
  Result:=nil;
  Index:=InternalFind(AHash,AName,PrevIndex);
  if Index>=0 then
    Result:=FItems[Index].Data;
end;


function TFPHashList.Rename(const AOldName,ANewName:RawByteString): SizeInt;
var
  PrevIndex : SizeInt;
  OldHash : LongWord;
  it: PViHashListItem;
begin
  OldHash:=HashString(AOldName);
  result:=InternalFind(OldHash,AOldName,PrevIndex);
  if result<0 then
    exit;
  RemoveFromHashTable(OldHash, result, PrevIndex);
  it:=FItems+result;
  it^.HashValue:=HashString(ANewName);
  it^.Str:=ANewName;
  AddToHashTable(it, result);
end;


function TFPHashList.Remove(Item: Pointer): SizeInt;
begin
  Result:=IndexOf(Item);
  if Result>=0 then
    Delete(Result);
end;


procedure TFPHashList.Pack;
var
  itemp, iteme, target: PViHashListItem;
  removed: SizeUint;
begin
  itemp:=FItems;
  iteme:=itemp+FCount;
  while itemp<iteme do
    if Assigned(itemp^.Data) then
      inc(itemp)
    else
      break;
  if itemp<iteme then
    begin
      target:=itemp;
      inc(itemp);
      while itemp<iteme do
        begin
          if Assigned(itemp^.data) then
            begin
              target^:=itemp^;
              inc(target);
            end;
          inc(itemp);
        end;
      removed:=SizeUint(pointer(iteme)-pointer(target)) div sizeof(TFPHashListItem);
      Finalize(target^, removed);
      FCount:=FCount-removed;
    end;
  if uint32(FCount)<>FCapacity then
    Rehash(FCount, vi_Pack);
end;



procedure TFPHashList.ForEachCall(proc2call:TListCallback;arg:pointer);
var
  i: SizeInt;
  p: pointer;
begin
  i:=0;
  if FCount>0 then
    repeat { Just in case callback deletes items. (The iterated sequence might be wrong in this case, but at least the iteration won’t crash.) }
      p:=FItems[i].Data;
      if assigned(p) then
        proc2call(p,arg);
      inc(i);
    until int32(i)>=FCount;
end;


procedure TFPHashList.ForEachCall(proc2call:TListStaticCallback;arg:pointer);
var
  i: SizeInt;
  p: pointer;
begin
  i:=0;
  if FCount>0 then
    repeat
      p:=FItems[i].Data;
      if assigned(p) then
        proc2call(p,arg);
      inc(i);
    until int32(i)>=FCount;
end;


{*****************************************************************************
                               TFPHashObject
*****************************************************************************}

procedure TFPHashObject.InternalChangeOwner(HashObjectList:TFPHashObjectList;const s:RawByteString);
var
  Index : SizeInt;
  it : PViHashListItem;
begin
  FOwner:=HashObjectList;
  Index:=HashObjectList.Add(s,Self);
  it:=HashObjectList.List.List+Index;
  FStr:=s;
  FHash:=it^.HashValue;
end;


constructor TFPHashObject.CreateNotOwned;
begin
  FStr:='';
  int32(FHash):=-1;
end;


constructor TFPHashObject.Create(HashObjectList:TFPHashObjectList;const s:RawByteString);
begin
  InternalChangeOwner(HashObjectList,s);
end;


procedure TFPHashObject.ChangeOwner(HashObjectList:TFPHashObjectList);
begin
  InternalChangeOwner(HashObjectList, FStr);
end;


procedure TFPHashObject.ChangeOwnerAndName(HashObjectList:TFPHashObjectList;const s:RawByteString);
begin
  InternalChangeOwner(HashObjectList,s);
end;


procedure TFPHashObject.Rename(const ANewName:RawByteString);
var
  Index : integer;
  it : PViHashListItem;
begin
  Index:=FOwner.Rename(FStr,ANewName);
  if Index>=0 then
    begin
      it:=FOwner.List.List+Index;
      FStr:=ANewName;
      FHash:=it^.HashValue;
    end;
end;


function TFPHashObject.GetName:RawByteString;
begin
  Result:=FStr;
end;


function TFPHashObject.GetHash:Longword;
begin
  Result:=FHash;
end;


{*****************************************************************************
            TFPHashObjectList
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
      FHashList:=nil;
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

function TFPHashObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result := FHashList.IndexOf(Pointer(AObject));
end;

function TFPHashObjectList.GetCount: integer;
begin
  Result := FHashList.Count;
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

function TFPHashObjectList.Add(const AName:RawByteString;AObject: TObject): Integer;
begin
  Result := FHashList.Add(AName,AObject);
end;

function TFPHashObjectList.NameOfIndex(Index: Integer): RawByteString;
begin
  Result := FHashList.NameOfIndex(Index);
end;

function TFPHashObjectList.HashOfIndex(Index: Integer): LongWord;
begin
  Result := FHashList.HashOfIndex(Index);
end;

function TFPHashObjectList.GetNextCollision(Index: Integer): Integer;
begin
  Result := FHashList.GetNextCollision(Index);
end;

procedure TFPHashObjectList.Delete(Index: Integer);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList.Delete(Index);
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

function TFPHashObjectList.Find(const s:RawByteString): TObject;
begin
  result:=TObject(FHashList.Find(s));
end;


function TFPHashObjectList.FindIndexOf(const s:RawByteString): Integer;
begin
  result:=FHashList.FindIndexOf(s);
end;


function TFPHashObjectList.FindWithHash(const AName:RawByteString;AHash:LongWord): Pointer;
begin
  Result:=TObject(FHashList.FindWithHash(AName,AHash));
end;


function TFPHashObjectList.Rename(const AOldName,ANewName:RawByteString): Integer;
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

{ Default hash Function }

{$IFDEF RangeChecking}{$R-}{$ENDIF}
Function RSHash(const S: AnsiString; const TableSize: Longword): Longword;
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
{$IFDEF RangeChecking}{$R+}{$ENDIF}

{ THTNode }

constructor THTCustomNode.CreateWith(const AString: AnsiString);
begin
  inherited Create;
  FKey:=AString;
end;

Function THTCustomNode.HasKey(const AKey: AnsiString): boolean;
begin
  Result:=(AKey=FKey);
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

Function TFPCustomHashTable.Find(const aKey: AnsiString): THTCustomNode;
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
        if THTCustomNode(chn[i]).Key=aKey then
          Exit(THTCustomNode(chn[i]));
  Result:=nil;
end;

Function TFPCustomHashTable.FindChainForAdd(Const aKey : AnsiString) : TFPObjectList;
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
        if (THTCustomNode(Result[i]).Key=aKey) then
          raise EDuplicate.CreateFmt(DuplicateMsg, [aKey]);
    end
  else
    begin
    FHashTable[hashcode]:=TFPObjectList.Create(True);
    Result:=Chain(hashCode);
    end;
  Inc(FCount);
end;


Procedure TFPCustomHashTable.Delete(const aKey: AnsiString);
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
        if THTCustomNode(chn[i]).Key=aKey then
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

Function TFPCustomHashTable.FindOrCreateNew(const aKey: AnsiString): THTCustomNode;
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
        if (THTCustomNode(chn[i]).Key=aKey) then
          Exit(THTNode(chn[i]));
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

Procedure TFPDataHashTable.Add(const aKey: AnsiString; aItem: pointer);
var
  chn: TFPObjectList;
  NewNode: THtDataNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=THtDataNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TFPDataHashTable.GetData(const Index: AnsiString): Pointer;
var
  node: THTDataNode;
begin
  node:=THTDataNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:=nil;
end;

Procedure TFPDataHashTable.SetData(const index: AnsiString; const AValue: Pointer);
begin
  THTDataNode(FindOrCreateNew(index)).Data:=AValue;
end;

Function TFPDataHashTable.CreateNewNode(const aKey : AnsiString) : THTCustomNode;

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

Procedure TFPDataHashTable.CallbackIterator(Item: Pointer; const Key: AnsiString; var Continue: Boolean);
begin
  FIteratorCallBack(Item, Key, Continue);
end;

Function TFPDataHashTable.Iterate(aMethod: TDataIteratorCallBack): Pointer;
begin
  FIteratorCallBack := aMethod;
  Result := Iterate(@CallbackIterator);
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

Function TFPStringHashTable.GetData(const Index: AnsiString): AnsiString;
var
  node: THTStringNode;
begin
  node:=THTStringNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:='';
end;

Procedure TFPStringHashTable.SetData(const index, AValue: AnsiString);
begin
  THTStringNode(FindOrCreateNew(index)).Data:=AValue;
end;

Procedure TFPStringHashTable.Add(const aKey, aItem: AnsiString);
var
  chn: TFPObjectList;
  NewNode: THtStringNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=THtStringNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TFPStringHashTable.CreateNewNode(const aKey : AnsiString) : THTCustomNode;
begin
  Result:=THTStringNode.CreateWith(aKey);
end;

Function TFPStringHashTable.Iterate(aMethod: TStringIteratorMethod): AnsiString;
var
  N : THTStringNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
  else
    Result:='';
end;

Procedure TFPStringHashTable.CallbackIterator(Item: AnsiString; const Key: AnsiString; var Continue: Boolean);
begin
  FIteratorCallBack(Item, Key, Continue);
end;

Function TFPStringHashTable.Iterate(aMethod: TStringIteratorCallback): AnsiString;
begin
  FIteratorCallBack := aMethod;
  Result := Iterate(@CallbackIterator);
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

Function TFPObjectHashTable.GetData(const Index: AnsiString): TObject;
var
  node: THTObjectNode;
begin
  node:=THTObjectNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data
  else
    Result:=nil;
end;

Procedure TFPObjectHashTable.SetData(const index : AnsiString; AObject : TObject);
begin
  THTObjectNode(FindOrCreateNew(index)).Data:=AObject;
end;

Procedure TFPObjectHashTable.Add(const aKey: AnsiString; AItem : TObject);
var
  chn: TFPObjectList;
  NewNode: THTObjectNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=THTObjectNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

Function TFPObjectHashTable.CreateNewNode(const aKey : AnsiString) : THTCustomNode;
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

Procedure TFPObjectHashTable.CallbackIterator(Item: TObject; const Key: AnsiString; var Continue: Boolean);
begin
  FIteratorCallBack(Item, Key, Continue);
end;

Function TFPObjectHashTable.Iterate(aMethod: TObjectIteratorCallback): TObject;
begin
  FIteratorCallBack := aMethod;
  Result := Iterate(@CallbackIterator);
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

Procedure TCustomBucketList.Error(Msg: AnsiString; Args: array of const);
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
  SetLength(FBuckets,0);
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
