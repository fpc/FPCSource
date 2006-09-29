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
    Hash support, implemented by Dean Zobec
  ---------------------------------------------------------------------}


  { Must return a Longword value in the range 0..TableSize,
   usually via a mod operator;  }
  THashFunction = function(const S: string; const TableSize: Longword): Longword;

  TIteratorMethod = procedure(Item: Pointer; const Key: string;
     var Continue: Boolean) of object;

  { THTNode }

  THTNode = class(TObject)
  private
    FData: pointer;
    FKey: string;
  public
    constructor CreateWith(const AString: String);
    function HasKey(const AKey: string): boolean;
    property Key: string read FKey;
    property Data: pointer read FData write FData;
  end;

  { TFPHashTable }

  TFPHashTable = class(TObject)
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
    function ChainLength(const ChainIndex: Longword): Longword; virtual;
    procedure SetData(const index: string; const AValue: Pointer); virtual;
    function GetData(const index: string):Pointer; virtual;
    function FindOrCreateNew(const aKey: string): THTNode; virtual;
    function ForEachCall(aMethod: TIteratorMethod): THTNode; virtual;
    procedure SetHashFunction(AHashFunction: THashFunction); virtual;
  public
    constructor Create;
    constructor CreateWith(AHashTableSize: Longword; aHashFunc: THashFunction);
    destructor Destroy; override;
    procedure ChangeTableSize(const ANewSize: Longword); virtual;
    procedure Clear; virtual;
    procedure Add(const aKey: string; AItem: pointer); virtual;
    procedure Delete(const aKey: string); virtual;
    function Find(const aKey: string): THTNode;
    function IsEmpty: boolean;
    property HashFunction: THashFunction read FHashFunction write SetHashFunction;
    property Count: Longword read FCount;
    property HashTableSize: Longword read FHashTableSize write SetHashTableSize;
    property Items[const index: string]: Pointer read GetData write SetData; default;
    property HashTable: TFPObjectList read FHashTable;
    property VoidSlots: Longword read GetVoidSlots;
    property LoadFactor: double read GetLoadFactor;
    property AVGChainLen: double read GetAVGChainLen;
    property MaxChainLength: Longword read GetMaxChainLength;
    property NumberOfCollisions: Longword read GetNumberOfCollisions;
    property Density: Longword read GetDensity;
  end;

  EDuplicate = class(Exception);
  EKeyNotFound = class(Exception);


  function RSHash(const S: string; const TableSize: Longword): Longword;

implementation

ResourceString
  DuplicateMsg = 'An item with key %0:s already exists';
  KeyNotFoundMsg = 'Method: %0:s key [''%1:s''] not found in container';
  NotEmptyMsg = 'Hash table not empty.';

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

Var
  O : TObject;

begin
  if OwnsObjects then
    begin
    O:=GetItem(Index);
    O.Free;
    end;
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

constructor THTNode.CreateWith(const AString: string);
begin
  inherited Create;
  FKey := AString;
end;

function THTNode.HasKey(const AKey: string): boolean;
begin
  if Length(AKey) <> Length(FKey) then
  begin
    Result := false;
    exit;
  end
  else
    Result := CompareMem(PChar(FKey), PChar(AKey), length(AKey));
end;

{ TFPHashTable }

constructor TFPHashTable.Create;
begin
  Inherited Create;
  FHashTable := TFPObjectList.Create(True);
  HashTableSize := 196613;
  FHashFunction := @RSHash;
end;

constructor TFPHashTable.CreateWith(AHashTableSize: Longword;
  aHashFunc: THashFunction);
begin
  Inherited Create;
  FHashTable := TFPObjectList.Create(True);
  HashTableSize := AHashTableSize;
  FHashFunction := aHashFunc;
end;

destructor TFPHashTable.Destroy;
begin
  FHashTable.Free;
  inherited Destroy;
end;

function TFPHashTable.GetDensity: Longword;
begin
  Result := FHashTableSize - VoidSlots
end;

function TFPHashTable.GetNumberOfCollisions: Longword;
begin
  Result := FCount -(FHashTableSize - VoidSlots)
end;

procedure TFPHashTable.SetData(const index: string; const AValue: Pointer);
begin
  FindOrCreateNew(index).Data := AValue;
end;

procedure TFPHashTable.SetHashTableSize(const Value: Longword);
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

procedure TFPHashTable.InitializeHashTable;
var
  i: LongWord;
begin
  if FHashTableSize>0 Then
    for i := 0 to FHashTableSize-1 do
     FHashTable.Add(nil);
  FCount := 0;
end;

procedure TFPHashTable.ChangeTableSize(const ANewSize: Longword);
var
  SavedTable: TFPObjectList;
  SavedTableSize: Longword;
  i, j: Longword;
  temp: THTNode;
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
        temp := THTNode(TFPObjectList(SavedTable[i])[j]);
        Add(temp.Key, temp.Data);
      end;
    end;
  SavedTable.Free;
end;

procedure TFPHashTable.SetHashFunction(AHashFunction: THashFunction);
begin
  if IsEmpty then
    FHashFunction := AHashFunction
  else
    raise Exception.Create(NotEmptyMsg);
end;

function TFPHashTable.Find(const aKey: string): THTNode;
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
      if THTNode(chn[i]).HasKey(aKey) then
      begin
        result := THTNode(chn[i]);
        exit;
      end;
  end;
  Result := nil;
end;

function TFPHashTable.GetData(const Index: string): Pointer;
var
  node: THTNode;
begin
  node := Find(Index);
  if Assigned(node) then
    Result := node.Data
  else
    Result := nil;
end;

function TFPHashTable.FindOrCreateNew(const aKey: string): THTNode;
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
      if THTNode(chn[i]).HasKey(aKey) then
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
  Result := THTNode.CreateWith(aKey);
  chn.Add(Result);
end;

function TFPHashTable.ChainLength(const ChainIndex: Longword): Longword;
begin
  if Assigned(Chain(ChainIndex)) then
    Result := Chain(ChainIndex).Count
  else
    Result := 0;
end;

procedure TFPHashTable.Clear;
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

function TFPHashTable.ForEachCall(aMethod: TIteratorMethod): THTNode;
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
          aMethod(THTNode(Chain(i)[j]).Data, THTNode(Chain(i)[j]).Key, continue);
          if not continue then
          begin
            Result := THTNode(Chain(i)[j]);
            Exit;
          end;
        end;
      end;
    end;
end;

procedure TFPHashTable.Add(const aKey: string; aItem: pointer);
var
  hashCode: Longword;
  chn: TFPObjectList;
  i: Longword;
  NewNode: THtNode;
begin
  hashCode := FHashFunction(aKey, FHashTableSize);
  chn := Chain(hashCode);
  if Assigned(chn)  then
  begin
    if chn.count>0 then
      for i := 0 to chn.Count - 1 do
        if THTNode(chn[i]).HasKey(aKey) then
          Raise EDuplicate.CreateFmt(DuplicateMsg, [aKey]);
  end
  else
    begin
      FHashTable[hashcode] := TFPObjectList.Create(true);
      chn := Chain(hashcode);
    end;
  inc(FCount);
  NewNode := THTNode.CreateWith(aKey);
  NewNode.Data := aItem;
  chn.Add(NewNode);
end;

procedure TFPHashTable.Delete(const aKey: string);
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
      if THTNode(chn[i]).HasKey(aKey) then
      begin
        chn.Delete(i);
        dec(FCount);
        exit;
      end;
  end;
  raise EKeyNotFound.CreateFmt(KeyNotFoundMsg, ['Delete', aKey]);
end;

function TFPHashTable.IsEmpty: boolean;
begin
  Result := (FCount = 0);
end;

function TFPHashTable.Chain(const index: Longword): TFPObjectList;
begin
  Result := TFPObjectList(FHashTable[index]);
end;

function TFPHashTable.GetVoidSlots: Longword;
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

function TFPHashTable.GetLoadFactor: double;
begin
  Result := Count / FHashTableSize;
end;

function TFPHashTable.GetAVGChainLen: double;
begin
  result := Count / (FHashTableSize - VoidSlots);
end;

function TFPHashTable.GetMaxChainLength: Longword;
var
  i: Longword;
begin
  Result := 0;
  if FHashTableSize>0 Then
   for i := 0 to FHashTableSize-1 do
      if ChainLength(i) > Result then
        Result := ChainLength(i);
end;

end.
