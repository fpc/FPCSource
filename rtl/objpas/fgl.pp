{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Micha Nelissen
    member of the Free Pascal development team

    It contains the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

{ be aware, this unit is a prototype and subject to be changed heavily }
unit fgl;

interface

uses
  types, sysutils;

const
  MaxListSize = Maxint div 16;

type
  EListError = class(Exception);

  TFPSList = class;
  TFPSListCompareFunc = function(Key1, Key2: Pointer): Integer of object;

  TFPSList = class(TObject)
  protected
    FList: PByte;
    FCount: Integer;
    FCapacity: Integer; { list is one longer than capacity, for temp }
    FItemSize: Integer;
    procedure CopyItem(Src, Dest: Pointer); virtual;
    procedure Deref(Item: Pointer); virtual; overload;
    procedure Deref(FromIndex, ToIndex: Integer); overload;
    function Get(Index: Integer): Pointer;
    procedure InternalExchange(Index1, Index2: Integer);
    function  InternalGet(Index: Integer): Pointer; {$ifdef CLASSESINLINE} inline; {$endif}
    procedure InternalPut(Index: Integer; NewItem: Pointer);
    procedure Put(Index: Integer; Item: Pointer);
    procedure QuickSort(L, R: Integer; Compare: TFPSListCompareFunc);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure RaiseIndexError(Index : Integer);
    property InternalItems[Index: Integer]: Pointer read InternalGet write InternalPut;
  public
    constructor Create(AItemSize: Integer = sizeof(Pointer));
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: PtrInt);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TFPSList;
    function Extract(Item: Pointer): Pointer;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Insert(Index: Integer): Pointer;
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign(Obj: TFPSList);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TFPSListCompareFunc);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property ItemSize: Integer read FItemSize;
    property List: PByte read FList;
  end;

{$ifndef VER2_0}

const
  MaxGListSize = MaxInt div 1024;

type
  generic TFPGList<T> = class(TFPSList)
  type public
    TCompareFunc = function(const Item1, Item2: T): Integer;
    TTypeList = array[0..MaxGListSize] of T;
    PTypeList = ^TTypeList;
    PT = ^T;
  var protected
    FOnCompare: TCompareFunc;
    procedure CopyItem(Src, Dest: Pointer); override;
    procedure Deref(Item: Pointer); override;
    function  Get(Index: Integer): T; {$ifdef CLASSESINLINE} inline; {$endif}
    function  GetList: PTypeList; {$ifdef CLASSESINLINE} inline; {$endif}
    function  ItemPtrCompare(Item1, Item2: Pointer): Integer;
    procedure Put(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
  public
    constructor Create;
    function Add(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    function Extract(const Item: T): T; {$ifdef CLASSESINLINE} inline; {$endif}
    function First: T; {$ifdef CLASSESINLINE} inline; {$endif}
    function IndexOf(const Item: T): Integer;
    procedure Insert(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
    function Last: T; {$ifdef CLASSESINLINE} inline; {$endif}
    {$info FIXME: bug #10479: implement TFPGList<T>.Assign(TFPGList) to work somehow}
    {procedure Assign(Source: TFPGList);}
    function Remove(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    procedure Sort(Compare: TCompareFunc);
    property Items[Index: Integer]: T read Get write Put; default;
    property List: PTypeList read GetList;
  end;

  generic TFPGObjectList<T> = class(TFPSList)
  type public
    TCompareFunc = function(const Item1, Item2: T): Integer;
    TTypeList = array[0..MaxGListSize] of T;
    PTypeList = ^TTypeList;
    PT = ^T;
  var protected
    FOnCompare: TCompareFunc;
    procedure CopyItem(Src, Dest: Pointer); override;
    procedure Deref(Item: Pointer); override;
    function  Get(Index: Integer): T; {$ifdef CLASSESINLINE} inline; {$endif}
    function  GetList: PTypeList; {$ifdef CLASSESINLINE} inline; {$endif}
    function  ItemPtrCompare(Item1, Item2: Pointer): Integer;
    procedure Put(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
  public
    constructor Create;
    function Add(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    function Extract(const Item: T): T; {$ifdef CLASSESINLINE} inline; {$endif}
    function First: T; {$ifdef CLASSESINLINE} inline; {$endif}
    function IndexOf(const Item: T): Integer;
    procedure Insert(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
    function Last: T; {$ifdef CLASSESINLINE} inline; {$endif}
    {$info FIXME: bug #10479: implement TFPGObjectList<T>.Assign(TFPGList) to work somehow}
    {procedure Assign(Source: TFPGList);}
    function Remove(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    procedure Sort(Compare: TCompareFunc);
    property Items[Index: Integer]: T read Get write Put; default;
    property List: PTypeList read GetList;
  end;

  generic TFPGInterfacedObjectList<T> = class(TFPSList)
  type public
    TCompareFunc = function(const Item1, Item2: T): Integer;
    TTypeList = array[0..MaxGListSize] of T;
    PTypeList = ^TTypeList;
    PT = ^T;
  var protected
    FOnCompare: TCompareFunc;
    procedure CopyItem(Src, Dest: Pointer); override;
    procedure Deref(Item: Pointer); override;
    function  Get(Index: Integer): T; {$ifdef CLASSESINLINE} inline; {$endif}
    function  GetList: PTypeList; {$ifdef CLASSESINLINE} inline; {$endif}
    function  ItemPtrCompare(Item1, Item2: Pointer): Integer;
    procedure Put(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
  public
    constructor Create;
    function Add(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    function Extract(const Item: T): T; {$ifdef CLASSESINLINE} inline; {$endif}
    function First: T; {$ifdef CLASSESINLINE} inline; {$endif}
    function IndexOf(const Item: T): Integer;
    procedure Insert(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
    function Last: T; {$ifdef CLASSESINLINE} inline; {$endif}
    {$info FIXME: bug #10479: implement TFPGInterfacedObjectList<T>.Assign(TFPGList) to work somehow}
    {procedure Assign(Source: TFPGList);}
    function Remove(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    procedure Sort(Compare: TCompareFunc);
    property Items[Index: Integer]: T read Get write Put; default;
    property List: PTypeList read GetList;
  end;

{$endif}

  TFPSMap = class(TFPSList)
  private
    FKeySize: Integer;
    FDataSize: Integer;
    FDuplicates: TDuplicates;
    FSorted: Boolean;
    FOnPtrCompare: TFPSListCompareFunc;
    procedure SetSorted(Value: Boolean);
  protected
    function BinaryCompare(Key1, Key2: Pointer): Integer;
    procedure CopyKey(Src, Dest: Pointer); virtual;
    procedure CopyData(Src, Dest: Pointer); virtual;
    function GetKey(Index: Integer): Pointer;
    function GetKeyData(AKey: Pointer): Pointer;
    function GetData(Index: Integer): Pointer;
    procedure InitOnPtrCompare; virtual;
    function LinearIndexOf(AKey: Pointer): Integer;
    procedure PutKey(Index: Integer; AKey: Pointer);
    procedure PutKeyData(AKey: Pointer; NewData: Pointer);
    procedure PutData(Index: Integer; AData: Pointer);
  public
    constructor Create(AKeySize: Integer = sizeof(Pointer);
      ADataSize: integer = sizeof(Pointer));
    function Add(AKey, AData: Pointer): Integer;
    function Add(AKey: Pointer): Integer;
    function Find(AKey: Pointer; var Index: Integer): Boolean;
    function IndexOf(AKey: Pointer): Integer;
    function IndexOfData(AData: Pointer): Integer;
    function Insert(Index: Integer): Pointer;
    procedure Insert(Index: Integer; var AKey, AData: Pointer);
    procedure InsertKey(Index: Integer; AKey: Pointer);
    procedure InsertKeyData(Index: Integer; AKey, AData: Pointer);
    function Remove(AKey: Pointer): Integer;
    procedure Sort;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property KeySize: Integer read FKeySize;
    property DataSize: Integer read FDataSize;
    property Keys[Index: Integer]: Pointer read GetKey write PutKey;
    property Data[Index: Integer]: Pointer read GetData write PutData;
    property KeyData[Key: Pointer]: Pointer read GetKeyData write PutKeyData; default;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnPtrCompare: TFPSListCompareFunc read FOnPtrCompare write FOnPtrCompare;
  end;

{$ifndef VER2_0}

  generic TFPGMap<TKey, TData> = class(TFPSMap)
  type public
    TCompareFunc = function(const Key1, Key2: TKey): Integer;
    PKey = ^TKey;
    PData = ^TData;
  var protected
    FOnCompare: TCompareFunc;
    procedure CopyItem(Src, Dest: Pointer); override;
    procedure CopyKey(Src, Dest: Pointer); override;
    procedure CopyData(Src, Dest: Pointer); override;
    procedure Deref(Item: Pointer); override;
    procedure InitOnPtrCompare; override;
    function GetKey(Index: Integer): TKey; {$ifdef CLASSESINLINE} inline; {$endif}
    function GetKeyData(const AKey: TKey): TData; {$ifdef CLASSESINLINE} inline; {$endif}
    function GetData(Index: Integer): TData; {$ifdef CLASSESINLINE} inline; {$endif}
    function KeyCompare(Key1, Key2: Pointer): Integer;
    function KeyCustomCompare(Key1, Key2: Pointer): Integer;
    procedure PutKey(Index: Integer; const NewKey: TKey); {$ifdef CLASSESINLINE} inline; {$endif}
    procedure PutKeyData(const AKey: TKey; const NewData: TData); {$ifdef CLASSESINLINE} inline; {$endif}
    procedure PutData(Index: Integer; const NewData: TData); {$ifdef CLASSESINLINE} inline; {$endif}
    procedure SetOnCompare(NewCompare: TCompareFunc);
  public
    constructor Create;
    function Add(const AKey: TKey; const AData: TData): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    function Add(const AKey: TKey): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    function Find(const AKey: TKey; var Index: Integer): Boolean; {$ifdef CLASSESINLINE} inline; {$endif}
    function IndexOf(const AKey: TKey): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    function IndexOfData(const AData: TData): Integer;
    procedure InsertKey(Index: Integer; const AKey: TKey);
    procedure InsertKeyData(Index: Integer; const AKey: TKey; const AData: TData);
    function Remove(const AKey: TKey): Integer;
    property Keys[Index: Integer]: TKey read GetKey write PutKey;
    property Data[Index: Integer]: TData read GetData write PutData;
    property KeyData[const AKey: TKey]: TData read GetKeyData write PutKeyData; default;
    property OnCompare: TCompareFunc read FOnCompare write SetOnCompare;
  end;

{$endif}

implementation

uses
  rtlconsts;

{****************************************************************************
                             TFPSList
 ****************************************************************************}

constructor TFPSList.Create(AItemSize: integer);
begin
  inherited Create;
  FItemSize := AItemSize;
end;

destructor TFPSList.Destroy;
begin
  Clear;
  // Clear() does not clear the whole list; there is always a single temp entry
  // at the end which is never freed. Take care of that one here.
  FreeMem(FList);
  inherited Destroy;
end;

procedure TFPSList.CopyItem(Src, Dest: Pointer);
begin
  System.Move(Src^, Dest^, FItemSize);
end;

procedure TFPSList.RaiseIndexError(Index : Integer);
begin
  Error(SListIndexError, Index);
end;

function TFPSList.InternalGet(Index: Integer): Pointer;
begin
  Result:=FList+Index*ItemSize;
end;

procedure TFPSList.InternalPut(Index: Integer; NewItem: Pointer);
var
  ListItem: Pointer;
begin
  ListItem := InternalItems[Index];
  CopyItem(NewItem, ListItem);
end;

function TFPSList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result := InternalItems[Index];
end;

procedure TFPSList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  InternalItems[Index] := Item;
end;

procedure TFPSList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(SListCapacityError, NewCapacity);
  if NewCapacity = FCapacity then
    exit;
  ReallocMem(FList, (NewCapacity+1) * FItemSize);
  FillChar(InternalItems[FCapacity]^, (NewCapacity+1-FCapacity) * FItemSize, #0);
  FCapacity := NewCapacity;
end;

procedure TFPSList.Deref(Item: Pointer);
begin
end;

procedure TFPSList.Deref(FromIndex, ToIndex: Integer);
var
  ListItem, ListItemLast: Pointer;
begin
  ListItem := InternalItems[FromIndex];
  ListItemLast := InternalItems[ToIndex];
  repeat
    Deref(ListItem);
    if ListItem = ListItemLast then
      break;
    ListItem := PByte(ListItem) + ItemSize;
  until false;
end;

procedure TFPSList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(SListCountError, NewCount);
  if NewCount > FCount then
  begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if NewCount > FCount then
      FillByte(InternalItems[FCount]^, (NewCount-FCount) * FItemSize, 0)
    else if NewCount < FCount then
      Deref(NewCount, FCount-1);
  end;
  FCount := NewCount;
end;

function TFPSList.Add(Item: Pointer): Integer;
begin
  if FCount = FCapacity then
    Self.Expand;
  CopyItem(Item, InternalItems[FCount]);
  Result := FCount;
  Inc(FCount);
end;

procedure TFPSList.Clear;
begin
  if Assigned(FList) then
  begin
    SetCount(0);
    SetCapacity(0);
  end;
end;

procedure TFPSList.Delete(Index: Integer);
var
  ListItem: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  Dec(FCount);
  ListItem := InternalItems[Index];
  Deref(ListItem);
  System.Move(InternalItems[Index+1]^, ListItem^, (FCount - Index) * FItemSize);
  // Shrink the list if appropriate
  if (FCapacity > 256) and (FCount < FCapacity shr 2) then
  begin
    FCapacity := FCapacity shr 1;
    ReallocMem(FList, (FCapacity+1) * FItemSize);
  end;
end;

function TFPSList.Extract(Item: Pointer): Pointer;
var
  i : Integer;
begin
  Result := nil;
  i := IndexOf(Item);
  if i >= 0 then
  begin
    Result := InternalItems[i];
    System.Move(Result^, InternalItems[FCapacity]^, FItemSize);
    Delete(i);
  end;
end;

class procedure TFPSList.Error(const Msg: string; Data: PtrInt);
begin
  raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
end;

procedure TFPSList.Exchange(Index1, Index2: Integer);
begin
  if ((Index1 >= FCount) or (Index1 < 0)) then
    Error(SListIndexError, Index1);
  if ((Index2 >= FCount) or (Index2 < 0)) then
    Error(SListIndexError, Index2);
  InternalExchange(Index1, Index2);
end;

procedure TFPSList.InternalExchange(Index1, Index2: Integer);
begin
  System.Move(InternalItems[Index1]^, InternalItems[FCapacity]^, FItemSize);
  System.Move(InternalItems[Index2]^, InternalItems[Index1]^, FItemSize);
  System.Move(InternalItems[FCapacity]^, InternalItems[Index2]^, FItemSize);
end;

function TFPSList.Expand: TFPSList;
var
  IncSize : Longint;
begin
  if FCount < FCapacity then exit;
  IncSize := 4;
  if FCapacity > 3 then IncSize := IncSize + 4;
  if FCapacity > 8 then IncSize := IncSize + 8;
  if FCapacity > 127 then Inc(IncSize, FCapacity shr 2);
  SetCapacity(FCapacity + IncSize);
  Result := Self;
end;

function TFPSList.First: Pointer;
begin
  If FCount = 0 then
    Result := Nil
  else
    Result := InternalItems[0];
end;

function TFPSList.IndexOf(Item: Pointer): Integer;
var
  ListItem: Pointer;
begin
  Result := 0;
  ListItem := First;
  while (Result < FCount) and (CompareByte(ListItem^, Item^, FItemSize) <> 0) do
  begin
    Inc(Result);
    ListItem := PByte(ListItem)+FItemSize;
  end;
  if Result = FCount then Result := -1;
end;

function TFPSList.Insert(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexError, Index);
  if FCount = FCapacity then Self.Expand;
  if Index<FCount then
    System.Move(InternalItems[Index]^, InternalItems[Index+1]^, (FCount - Index) * FItemSize);
  Result := InternalItems[Index];
  Inc(FCount);
end;

procedure TFPSList.Insert(Index: Integer; Item: Pointer);
begin
  CopyItem(Item, Insert(Index));
end;

function TFPSList.Last: Pointer;
begin
  if FCount = 0 then
    Result := nil
  else
    Result := InternalItems[FCount - 1];
end;

procedure TFPSList.Move(CurIndex, NewIndex: Integer);
var
  CurItem, NewItem, TmpItem, Src, Dest: Pointer;
  MoveCount: Integer;
begin
  if (CurIndex < 0) or (CurIndex >= Count) then
    Error(SListIndexError, CurIndex);
  if (NewIndex < 0) or (NewIndex >= Count) then
    Error(SListIndexError, NewIndex);
  if CurIndex = NewIndex then
    exit;
  CurItem := InternalItems[CurIndex];
  NewItem := InternalItems[NewIndex];
  TmpItem := InternalItems[FCapacity];
  System.Move(CurItem^, TmpItem^, FItemSize);
  if NewIndex > CurIndex then
  begin
    Src := InternalItems[CurIndex+1];
    Dest := CurItem;
    MoveCount := NewIndex - CurIndex;
  end else begin
    Src := NewItem;
    Dest := InternalItems[NewIndex+1];
    MoveCount := CurIndex - NewIndex;
  end;
  System.Move(Src^, Dest^, MoveCount * FItemSize);
  System.Move(TmpItem^, NewItem^, FItemSize);
end;

function TFPSList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

procedure TFPSList.Pack;
var
  NewCount,
  i : integer;
  pdest,
  psrc : Pointer;
begin
  NewCount:=0;
  psrc:=First;
  pdest:=psrc;
  For I:=0 To FCount-1 Do
    begin
      if assigned(pointer(psrc^)) then
        begin
          System.Move(psrc^, pdest^, FItemSize);
          inc(pdest);
          inc(NewCount);
        end;
      inc(psrc);
    end;
  FCount:=NewCount;
end;

// Needed by Sort method.

procedure TFPSList.QuickSort(L, R: Integer; Compare: TFPSListCompareFunc);
var
  I, J, P: Integer;
  PivotItem: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) div 2;
    repeat
      PivotItem := InternalItems[P];
      while Compare(PivotItem, InternalItems[I]) > 0 do
        Inc(I);
      while Compare(PivotItem, InternalItems[J]) < 0 do
        Dec(J);
      if I <= J then
      begin
        InternalExchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

procedure TFPSList.Sort(Compare: TFPSListCompareFunc);
begin
  if not Assigned(FList) or (FCount < 2) then exit;
  QuickSort(0, FCount-1, Compare);
end;

procedure TFPSList.Assign(Obj: TFPSList);
var
  i: Integer;
begin
  if Obj.ItemSize <> FItemSize then
    Error(SListItemSizeError, 0);
  Clear;
  for I := 0 to Obj.Count - 1 do
    Add(Obj[i]);
end;

{$ifndef VER2_0}

{****************************************************************************}
{*                TFPGList                                                  *}
{****************************************************************************}

constructor TFPGList.Create;
begin
  inherited Create(sizeof(T));
end;

procedure TFPGList.CopyItem(Src, Dest: Pointer);
begin
  T(Dest^) := T(Src^);
end;

procedure TFPGList.Deref(Item: Pointer);
begin
  Finalize(T(Item^));
end;

function TFPGList.Get(Index: Integer): T;
begin
  Result := T(inherited Get(Index)^);
end;

function TFPGList.GetList: PTypeList;
begin
  Result := PTypeList(FList);
end;

function TFPGList.ItemPtrCompare(Item1, Item2: Pointer): Integer;
begin
  Result := FOnCompare(T(Item1^), T(Item2^));
end;

procedure TFPGList.Put(Index: Integer; const Item: T);
begin
  inherited Put(Index, @Item);
end;

function TFPGList.Add(const Item: T): Integer;
begin
  Result := inherited Add(@Item);
end;

function TFPGList.Extract(const Item: T): T;
var
  ResPtr: Pointer;
begin
  ResPtr := inherited Extract(@Item);
  if ResPtr <> nil then
    Result := T(ResPtr^)
  else
    FillByte(Result, 0, sizeof(T));
end;

function TFPGList.First: T;
begin
  Result := T(inherited First^);
end;

function TFPGList.IndexOf(const Item: T): Integer;
begin
  Result := 0;
  {$info TODO: fix inlining to work! InternalItems[Result]^}
  while (Result < FCount) and (PT(FList)[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TFPGList.Insert(Index: Integer; const Item: T);
begin
  T(inherited Insert(Index)^) := Item;
end;

function TFPGList.Last: T;
begin
  Result := T(inherited Last^);
end;

function TFPGList.Remove(const Item: T): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TFPGList.Sort(Compare: TCompareFunc);
begin
  FOnCompare := Compare;
  inherited Sort(@ItemPtrCompare);
end;


{****************************************************************************}
{*                TFPGObjectList                                            *}
{****************************************************************************}

constructor TFPGObjectList.Create;
begin
  inherited Create;
end;

procedure TFPGObjectList.CopyItem(Src, Dest: Pointer);
begin
  T(Dest^) := T(Src^);
  {if TObject(Dest^) is TInterfacedObject then
    T(Dest^)._AddRef;}
end;

procedure TFPGObjectList.Deref(Item: Pointer);
begin
  {if TObject(Item^) is TInterfacedObject then
    T(Item^)._Release
  else}
    T(Item^).Free;
end;

function TFPGObjectList.Get(Index: Integer): T;
begin
  Result := T(inherited Get(Index)^);
end;

function TFPGObjectList.GetList: PTypeList;
begin
  Result := PTypeList(FList);
end;

function TFPGObjectList.ItemPtrCompare(Item1, Item2: Pointer): Integer;
begin
  Result := FOnCompare(T(Item1^), T(Item2^));
end;

procedure TFPGObjectList.Put(Index: Integer; const Item: T);
begin
  inherited Put(Index, @Item);
end;

function TFPGObjectList.Add(const Item: T): Integer;
begin
  Result := inherited Add(@Item);
end;

function TFPGObjectList.Extract(const Item: T): T;
var
  ResPtr: Pointer;
begin
  ResPtr := inherited Extract(@Item);
  if ResPtr <> nil then
    Result := T(ResPtr^)
  else
    FillByte(Result, 0, sizeof(T));
end;

function TFPGObjectList.First: T;
begin
  Result := T(inherited First^);
end;

function TFPGObjectList.IndexOf(const Item: T): Integer;
begin
  Result := 0;
  {$info TODO: fix inlining to work! InternalItems[Result]^}
  while (Result < FCount) and (PT(FList)[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TFPGObjectList.Insert(Index: Integer; const Item: T);
begin
  T(inherited Insert(Index)^) := Item;
end;

function TFPGObjectList.Last: T;
begin
  Result := T(inherited Last^);
end;

function TFPGObjectList.Remove(const Item: T): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TFPGObjectList.Sort(Compare: TCompareFunc);
begin
  FOnCompare := Compare;
  inherited Sort(@ItemPtrCompare);
end;


{****************************************************************************}
{*                TFPGInterfacedObjectList                                  *}
{****************************************************************************}

constructor TFPGInterfacedObjectList.Create;
begin
  inherited Create;
end;

procedure TFPGInterfacedObjectList.CopyItem(Src, Dest: Pointer);
begin
  T(Dest^) := T(Src^);
  if Assigned(Pointer(Dest^)) then
    T(Dest^)._AddRef;
end;

procedure TFPGInterfacedObjectList.Deref(Item: Pointer);
begin
  if Assigned(Pointer(Item^)) then
    T(Item^)._Release;
end;

function TFPGInterfacedObjectList.Get(Index: Integer): T;
begin
  Result := T(inherited Get(Index)^);
end;

function TFPGInterfacedObjectList.GetList: PTypeList;
begin
  Result := PTypeList(FList);
end;

function TFPGInterfacedObjectList.ItemPtrCompare(Item1, Item2: Pointer): Integer;
begin
  Result := FOnCompare(T(Item1^), T(Item2^));
end;

procedure TFPGInterfacedObjectList.Put(Index: Integer; const Item: T);
begin
  inherited Put(Index, @Item);
end;

function TFPGInterfacedObjectList.Add(const Item: T): Integer;
begin
  Result := inherited Add(@Item);
end;

function TFPGInterfacedObjectList.Extract(const Item: T): T;
var
  ResPtr: Pointer;
begin
  ResPtr := inherited Extract(@Item);
  if ResPtr <> nil then
    Result := T(ResPtr^)
  else
    FillByte(Result, 0, sizeof(T));
end;

function TFPGInterfacedObjectList.First: T;
begin
  Result := T(inherited First^);
end;

function TFPGInterfacedObjectList.IndexOf(const Item: T): Integer;
begin
  Result := 0;
  {$info TODO: fix inlining to work! InternalItems[Result]^}
  while (Result < FCount) and (PT(FList)[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TFPGInterfacedObjectList.Insert(Index: Integer; const Item: T);
begin
  T(inherited Insert(Index)^) := Item;
end;

function TFPGInterfacedObjectList.Last: T;
begin
  Result := T(inherited Last^);
end;

function TFPGInterfacedObjectList.Remove(const Item: T): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TFPGInterfacedObjectList.Sort(Compare: TCompareFunc);
begin
  FOnCompare := Compare;
  inherited Sort(@ItemPtrCompare);
end;

{$endif}

{****************************************************************************
                             TFPSMap
 ****************************************************************************}

constructor TFPSMap.Create(AKeySize: Integer; ADataSize: integer);
begin
  inherited Create(AKeySize+ADataSize);
  FKeySize := AKeySize;
  FDataSize := ADataSize;
  InitOnPtrCompare;
end;

procedure TFPSMap.CopyKey(Src, Dest: Pointer);
begin
  System.Move(Src^, Dest^, FKeySize);
end;

procedure TFPSMap.CopyData(Src, Dest: Pointer);
begin
  System.Move(Src^, Dest^, FDataSize);
end;

function TFPSMap.BinaryCompare(Key1, Key2: Pointer): Integer;
begin
  Result := CompareByte(Key1^, Key2^, FKeySize);
end;

function TFPSMap.GetKey(Index: Integer): Pointer;
begin
  Result := Items[Index];
end;

function TFPSMap.GetData(Index: Integer): Pointer;
begin
  Result := PByte(Items[Index])+FKeySize;
end;

function TFPSMap.GetKeyData(AKey: Pointer): Pointer;
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I >= 0 then
    Result := InternalItems[I]+FKeySize
  else
    Error(SMapKeyError, PtrUInt(AKey));
end;

procedure TFPSMap.InitOnPtrCompare;
begin
  FOnPtrCompare := @BinaryCompare;
end;

procedure TFPSMap.PutKey(Index: Integer; AKey: Pointer);
begin
  if FSorted then
    Error(SSortedListError, 0);
  CopyKey(AKey, Items[Index]);
end;

procedure TFPSMap.PutData(Index: Integer; AData: Pointer);
begin
  CopyData(AData, PByte(Items[Index])+FKeySize);
end;

procedure TFPSMap.PutKeyData(AKey: Pointer; NewData: Pointer);
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I >= 0 then
    Data[I] := NewData
  else
    Add(AKey, NewData);
end;

procedure TFPSMap.SetSorted(Value: Boolean);
begin
  if Value = FSorted then exit;
  FSorted := Value;
  if Value then Sort;
end;

function TFPSMap.Add(AKey: Pointer): Integer;
begin
  if Sorted then
  begin
    if Find(AKey, Result) then
      case Duplicates of
        dupIgnore: exit;
        dupError: Error(SDuplicateItem, 0)
      end;
  end else
    Result := Count;
  CopyKey(AKey, inherited Insert(Result));
end;

function TFPSMap.Add(AKey, AData: Pointer): Integer;
begin
  Result := Add(AKey);
  Data[Result] := AData;
end;

function TFPSMap.Find(AKey: Pointer; var Index: Integer): Boolean;
{ Searches for the first item <= Key, returns True if exact match,
  sets index to the index f the found string. }
var
  I,L,R,Dir: Integer;
begin
  Result := false;
  // Use binary search.
  L := 0;
  R := FCount-1;
  while L<=R do
  begin
    I := (L+R) div 2;
    Dir := FOnPtrCompare(Items[I], AKey);
    if Dir < 0 then
      L := I+1
    else begin
      R := I-1;
      if Dir = 0 then
      begin
        Result := true;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

function TFPSMap.LinearIndexOf(AKey: Pointer): Integer;
var
  ListItem: Pointer;
begin
  Result := 0;
  ListItem := First;
  while (Result < FCount) and (FOnPtrCompare(ListItem, AKey) <> 0) do
  begin
    Inc(Result);
    ListItem := PByte(ListItem)+FItemSize;
  end;
  if Result = FCount then Result := -1;
end;

function TFPSMap.IndexOf(AKey: Pointer): Integer;
begin
  if Sorted then
  begin
    if not Find(AKey, Result) then
      Result := -1;
  end else
    Result := LinearIndexOf(AKey);
end;

function TFPSMap.IndexOfData(AData: Pointer): Integer;
var
  ListItem: Pointer;
begin
  Result := 0;
  ListItem := First+FKeySize;
  while (Result < FCount) and (CompareByte(ListItem^, AData^, FDataSize) <> 0) do
  begin
    Inc(Result);
    ListItem := PByte(ListItem)+FItemSize;
  end;
  if Result = FCount then Result := -1;
end;

function TFPSMap.Insert(Index: Integer): Pointer;
begin
  if FSorted then
    Error(SSortedListError, 0);
  Result := inherited Insert(Index);
end;

procedure TFPSMap.Insert(Index: Integer; var AKey, AData: Pointer);
begin
  AKey := Insert(Index);
  AData := PByte(AKey) + FKeySize;
end;

procedure TFPSMap.InsertKey(Index: Integer; AKey: Pointer);
begin
  CopyKey(AKey, Insert(Index));
end;

procedure TFPSMap.InsertKeyData(Index: Integer; AKey, AData: Pointer);
var
  ListItem: Pointer;
begin
  ListItem := Insert(Index);
  CopyKey(AKey, ListItem);
  CopyData(AData, PByte(ListItem)+FKeySize);
end;

function TFPSMap.Remove(AKey: Pointer): Integer;
begin
  Result := IndexOf(AKey);
  if Result >= 0 then
    Delete(Result);
end;

procedure TFPSMap.Sort;
begin
  inherited Sort(FOnPtrCompare);
end;

{****************************************************************************
                             TFPGMap
 ****************************************************************************}

{$ifndef VER2_0}

constructor TFPGMap.Create;
begin
  inherited Create(SizeOf(TKey), SizeOf(TData));
end;

procedure TFPGMap.CopyItem(Src, Dest: Pointer);
begin
  CopyKey(Src, Dest);
  CopyData(PByte(Src)+KeySize, PByte(Dest)+KeySize);
end;

procedure TFPGMap.CopyKey(Src, Dest: Pointer);
begin
  TKey(Dest^) := TKey(Src^);
end;

procedure TFPGMap.CopyData(Src, Dest: Pointer);
begin
  TData(Dest^) := TData(Src^);
end;

procedure TFPGMap.Deref(Item: Pointer);
begin
  Finalize(TKey(Item^));
  Finalize(TData(Pointer(PByte(Item)+KeySize)^));
end;

function TFPGMap.GetKey(Index: Integer): TKey;
begin
  Result := TKey(inherited GetKey(Index)^);
end;

function TFPGMap.GetData(Index: Integer): TData;
begin
  Result := TData(inherited GetData(Index)^);
end;

function TFPGMap.GetKeyData(const AKey: TKey): TData;
begin
  Result := TData(inherited GetKeyData(@AKey)^);
end;

procedure TFPGMap.InitOnPtrCompare;
begin
  OnPtrCompare := @KeyCompare;
end;

function TFPGMap.KeyCompare(Key1, Key2: Pointer): Integer;
begin
  if PKey(Key1)^ < PKey(Key2)^ then
    Result := -1
  else if PKey(Key1)^ > PKey(Key2)^ then
    Result := 1
  else
    Result := 0;
end;

function TFPGMap.KeyCustomCompare(Key1, Key2: Pointer): Integer;
begin
  Result := FOnCompare(TKey(Key1^), TKey(Key2^));
end;

procedure TFPGMap.PutKey(Index: Integer; const NewKey: TKey);
begin
  inherited PutKey(Index, @NewKey);
end;

procedure TFPGMap.PutData(Index: Integer; const NewData: TData);
begin
  inherited PutData(Index, @NewData);
end;

procedure TFPGMap.PutKeyData(const AKey: TKey; const NewData: TData);
begin
  inherited PutKeyData(@AKey, @NewData);
end;

procedure TFPGMap.SetOnCompare(NewCompare: TCompareFunc);
begin
  FOnCompare := NewCompare;
  if NewCompare <> nil then
    OnPtrCompare := @KeyCustomCompare
  else
    InitOnPtrCompare;
end;

function TFPGMap.Add(const AKey: TKey): Integer;
begin
  Result := inherited Add(@AKey);
end;

function TFPGMap.Add(const AKey: TKey; const AData: TData): Integer;
begin
  Result := inherited Add(@AKey, @AData);
end;

function TFPGMap.Find(const AKey: TKey; var Index: Integer): Boolean;
begin
  Result := inherited Find(@AKey, Index);
end;

function TFPGMap.IndexOf(const AKey: TKey): Integer;
begin
  Result := inherited IndexOf(@AKey);
end;

function TFPGMap.IndexOfData(const AData: TData): Integer;
begin
  { TODO: loop ? }
  Result := inherited IndexOfData(@AData);
end;

procedure TFPGMap.InsertKey(Index: Integer; const AKey: TKey);
begin
  inherited InsertKey(Index, @AKey);
end;

procedure TFPGMap.InsertKeyData(Index: Integer; const AKey: TKey; const AData: TData);
begin
  inherited InsertKeyData(Index, @AKey, @AData);
end;

function TFPGMap.Remove(const AKey: TKey): Integer;
begin
  Result := inherited Remove(@AKey);
end;

{$endif}

end.
