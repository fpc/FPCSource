{
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

    This module provides some basic classes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit cclasses;

{$i fpcdefs.inc}

{$ifndef VER2_0}
  { Disabled for now, gives an IE 200311075 when compiling the IDE }
  { $define CCLASSESINLINE}
{$endif}

interface

    uses
{$IFNDEF USE_FAKE_SYSUTILS}
      SysUtils,
{$ELSE}
      fksysutl,
{$ENDIF}
      globtype,
      CUtils,CStreams;

{********************************************
                TMemDebug
********************************************}

    type
       tmemdebug = class
       private
          totalmem,
          startmem : integer;
          infostr  : string[40];
       public
          constructor Create(const s:string);
          destructor  Destroy;override;
          procedure show;
          procedure start;
          procedure stop;
       end;

{*******************************************************
      TFPList (From rtl/objpas/classes/classesh.inc)
********************************************************}

const
   SListIndexError = 'List index exceeds bounds (%d)';
   SListCapacityError = 'The maximum list capacity is reached (%d)';
   SListCountError = 'List count too large (%d)';
type
   EListError = class(Exception);

const
  MaxListSize = Maxint div 16;
type
  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;
  TListSortCompare = function (Item1, Item2: Pointer): Integer;
  TListCallback = procedure(data,arg:pointer) of object;
  TListStaticCallback = procedure(data,arg:pointer);

  TFPList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Put(Index: Integer; Item: Pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetCapacity(NewCapacity: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetCount(NewCount: Integer);
    Procedure RaiseIndexError(Index : Integer);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: PtrInt);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TFPList;
    function Extract(item: Pointer): Pointer;
    function First: Pointer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign(Obj:TFPList);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure ForEachCall(proc2call:TListCallback;arg:pointer);
    procedure ForEachCall(proc2call:TListStaticCallback;arg:pointer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read FList;
  end;


{*******************************************************
        TFPObjectList (From fcl/inc/contnrs.pp)
********************************************************}

  TObjectListCallback = procedure(data:TObject;arg:pointer) of object;
  TObjectListStaticCallback = procedure(data:TObject;arg:pointer);

  TFPObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FList: TFPList;
    function GetCount: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetCount(const AValue: integer); {$ifdef CCLASSESINLINE}inline;{$endif}
  protected
    function GetItem(Index: Integer): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetItem(Index: Integer; AObject: TObject); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetCapacity(NewCapacity: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    function GetCapacity: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
  public
    constructor Create;
    constructor Create(FreeObjects : Boolean);
    destructor Destroy; override;
    procedure Clear;
    function Add(AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Delete(Index: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Exchange(Index1, Index2: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    function Expand: TFPObjectList;{$ifdef CCLASSESINLINE}inline;{$endif}
    function Extract(Item: TObject): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    procedure Insert(Index: Integer; AObject: TObject); {$ifdef CCLASSESINLINE}inline;{$endif}
    function First: TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Last: TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Move(CurIndex, NewIndex: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Assign(Obj:TFPObjectList); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Pack; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Sort(Compare: TListSortCompare); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ForEachCall(proc2call:TObjectListCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TFPList read FList;
  end;

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
    function GetNextCollision(Index: Integer): Integer;
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
    function GetNextCollision(Index: Integer): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
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


{********************************************
                TLinkedList
********************************************}

    type
       TLinkedListItem = class
       public
          Previous,
          Next : TLinkedListItem;
          Constructor Create;
          Destructor Destroy;override;
          Function GetCopy:TLinkedListItem;virtual;
       end;

       TLinkedListItemClass = class of TLinkedListItem;

       TLinkedList = class
       private
          FCount : integer;
          FFirst,
          FLast  : TLinkedListItem;
          FNoClear : boolean;
       public
          constructor Create;
          destructor  Destroy;override;
          { true when the List is empty }
          function  Empty:boolean; {$ifdef CCLASSESINLINE}inline;{$endif}
          { deletes all Items }
          procedure Clear;
          { inserts an Item }
          procedure Insert(Item:TLinkedListItem);
          { inserts an Item before Loc }
          procedure InsertBefore(Item,Loc : TLinkedListItem);
          { inserts an Item after Loc }
          procedure InsertAfter(Item,Loc : TLinkedListItem);virtual;
          { concats an Item }
          procedure Concat(Item:TLinkedListItem);
          { deletes an Item }
          procedure Remove(Item:TLinkedListItem);
          { Gets First Item }
          function  GetFirst:TLinkedListItem;
          { Gets last Item }
          function  GetLast:TLinkedListItem;
          { inserts another List at the begin and make this List empty }
          procedure insertList(p : TLinkedList);
          { inserts another List before the provided item and make this List empty }
          procedure insertListBefore(Item:TLinkedListItem;p : TLinkedList);
          { inserts another List after the provided item and make this List empty }
          procedure insertListAfter(Item:TLinkedListItem;p : TLinkedList);
          { concats another List at the end and make this List empty }
          procedure concatList(p : TLinkedList);
          { concats another List at the start and makes a copy
            the list is ordered in reverse.
          }
          procedure insertListcopy(p : TLinkedList);
          { concats another List at the end and makes a copy }
          procedure concatListcopy(p : TLinkedList);
          property First:TLinkedListItem read FFirst;
          property Last:TLinkedListItem read FLast;
          property Count:Integer read FCount;
          property NoClear:boolean write FNoClear;
       end;

{********************************************
                TCmdStrList
********************************************}

       { string containerItem }
       TCmdStrListItem = class(TLinkedListItem)
          FPStr : TCmdStr;
       public
          constructor Create(const s:TCmdStr);
          destructor  Destroy;override;
          function GetCopy:TLinkedListItem;override;
          function Str:TCmdStr; {$ifdef CCLASSESINLINE}inline;{$endif}
       end;

       { string container }
       TCmdStrList = class(TLinkedList)
       private
          FDoubles : boolean;  { if this is set to true, doubles are allowed }
       public
          constructor Create;
          constructor Create_No_Double;
          { inserts an Item }
          procedure Insert(const s:TCmdStr);
          { concats an Item }
          procedure Concat(const s:TCmdStr);
          { deletes an Item }
          procedure Remove(const s:TCmdStr);
          { Gets First Item }
          function  GetFirst:TCmdStr;
          { Gets last Item }
          function  GetLast:TCmdStr;
          { true if string is in the container, compare case sensitive }
          function FindCase(const s:TCmdStr):TCmdStrListItem;
          { true if string is in the container }
          function Find(const s:TCmdStr):TCmdStrListItem;
          { inserts an item }
          procedure InsertItem(item:TCmdStrListItem); {$ifdef CCLASSESINLINE}inline;{$endif}
          { concats an item }
          procedure ConcatItem(item:TCmdStrListItem); {$ifdef CCLASSESINLINE}inline;{$endif}
          property Doubles:boolean read FDoubles write FDoubles;
       end;


{********************************************
              DynamicArray
********************************************}

     type
       { can't use sizeof(integer) because it crashes gdb }
       tdynamicblockdata=array[0..1024*1024-1] of byte;

       pdynamicblock = ^tdynamicblock;
       tdynamicblock = record
         pos,
         size,
         used : longword;
         Next : pdynamicblock;
         data : tdynamicblockdata;
       end;

     const
       dynamicblockbasesize = sizeof(tdynamicblock)-sizeof(tdynamicblockdata);

     type
       tdynamicarray = class
       private
         FPosn       : longword;
         FPosnblock  : pdynamicblock;
         FCurrBlocksize,
         FMaxBlocksize  : longword;
         FFirstblock,
         FLastblock  : pdynamicblock;
         procedure grow;
       public
         constructor Create(Ablocksize:longword);
         destructor  Destroy;override;
         procedure reset;
         function  size:longword;
         procedure align(i:longword);
         procedure seek(i:longword);
         function  read(var d;len:longword):longword;
         procedure write(const d;len:longword);
         procedure writestr(const s:string); {$ifdef CCLASSESINLINE}inline;{$endif}
         procedure readstream(f:TCStream;maxlen:longword);
         procedure writestream(f:TCStream);
         property  CurrBlockSize : longword read FCurrBlocksize;
         property  FirstBlock : PDynamicBlock read FFirstBlock;
         property  Pos : longword read FPosn;
       end;


{******************************************************************
   THashSet (keys not limited to ShortString, no indexed access)
*******************************************************************}

       PPHashSetItem = ^PHashSetItem;
       PHashSetItem = ^THashSetItem;
       THashSetItem = record
         Next: PHashSetItem;
         Key: Pointer;
         KeyLength: Integer;
         HashValue: LongWord;
         Data: TObject;
       end;

       THashSet = class(TObject)
       private
         FCount: LongWord;
         FBucketCount: LongWord;
         FBucket: PPHashSetItem;
         FOwnsObjects: Boolean;
         FOwnsKeys: Boolean;
         function Lookup(Key: Pointer; KeyLen: Integer; var Found: Boolean;
           CanCreate: Boolean): PHashSetItem;
         procedure Resize(NewCapacity: LongWord);
       public
         constructor Create(InitSize: Integer; OwnKeys, OwnObjects: Boolean);
         destructor Destroy; override;
         procedure Clear;
         { finds an entry by key }
         function Find(Key: Pointer; KeyLen: Integer): PHashSetItem;
         { finds an entry, creates one if not exists }
         function FindOrAdd(Key: Pointer; KeyLen: Integer;
           var Found: Boolean): PHashSetItem;
         { finds an entry, creates one if not exists }
         function FindOrAdd(Key: Pointer; KeyLen: Integer): PHashSetItem;
         { returns Data by given Key }
         function Get(Key: Pointer; KeyLen: Integer): TObject;
         { removes an entry, returns False if entry wasn't there }
         function Remove(Entry: PHashSetItem): Boolean;
         property Count: LongWord read FCount;
      end;


    function FPHash(const s:shortstring):LongWord;
    function FPHash(P: PChar; Len: Integer): LongWord;


implementation

{*****************************************************************************
                                    Memory debug
*****************************************************************************}

    constructor tmemdebug.create(const s:string);
      begin
        infostr:=s;
        totalmem:=0;
        Start;
      end;


    procedure tmemdebug.start;

      var
        status : TFPCHeapStatus;

      begin
        status:=GetFPCHeapStatus;
        startmem:=status.CurrHeapUsed;
      end;


    procedure tmemdebug.stop;
      var
        status : TFPCHeapStatus;
      begin
        if startmem<>0 then
         begin
           status:=GetFPCHeapStatus;
           inc(TotalMem,startmem-status.CurrHeapUsed);
           startmem:=0;
         end;
      end;


    destructor tmemdebug.destroy;
      begin
        Stop;
        show;
      end;


    procedure tmemdebug.show;
      begin
        write('memory [',infostr,'] ');
        if TotalMem>0 then
         writeln(DStr(TotalMem shr 10),' Kb released')
        else
         writeln(DStr((-TotalMem) shr 10),' Kb allocated');
      end;


{*****************************************************************************
               TFPObjectList (Copied from rtl/objpas/classes/lists.inc)
*****************************************************************************}

procedure TFPList.RaiseIndexError(Index : Integer);
begin
  Error(SListIndexError, Index);
end;

function TFPList.Get(Index: Integer): Pointer;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FList^[Index];
end;

procedure TFPList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Flist^[Index] := Item;
end;

function TFPList.Extract(item: Pointer): Pointer;
var
  i : Integer;
begin
  result := nil;
  i := IndexOf(item);
  if i >= 0 then
   begin
     Result := item;
     FList^[i] := nil;
     Delete(i);
   end;
end;

procedure TFPList.SetCapacity(NewCapacity: Integer);
begin
  If (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
     Error (SListCapacityError, NewCapacity);
  if NewCapacity = FCapacity then
    exit;
  ReallocMem(FList, SizeOf(Pointer)*NewCapacity);
  FCapacity := NewCapacity;
end;

procedure TFPList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxListSize)then
    Error(SListCountError, NewCount);
  If NewCount > FCount then
    begin
    If NewCount > FCapacity then
      SetCapacity(NewCount);
    If FCount < NewCount then
      FillChar(Flist^[FCount], (NewCount-FCount) *  sizeof(Pointer), 0);
    end;
  FCount := Newcount;
end;

destructor TFPList.Destroy;
begin
  Self.Clear;
  inherited Destroy;
end;

function TFPList.Add(Item: Pointer): Integer;
begin
  if FCount = FCapacity then
    Self.Expand;
  FList^[FCount] := Item;
  Result := FCount;
  inc(FCount);
end;

procedure TFPList.Clear;
begin
  if Assigned(FList) then
  begin
    SetCount(0);
    SetCapacity(0);
    FList := nil;
  end;
end;

procedure TFPList.Delete(Index: Integer);
begin
  If (Index<0) or (Index>=FCount) then
    Error (SListIndexError, Index);
  dec(FCount);
  System.Move (FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(Pointer));
  { Shrink the list if appropriate }
  if (FCapacity > 256) and (FCount < FCapacity shr 2) then
  begin
    FCapacity := FCapacity shr 1;
    ReallocMem(FList, SizeOf(Pointer) * FCapacity);
  end;
end;

class procedure TFPList.Error(const Msg: string; Data: PtrInt);
begin
  Raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
end;

procedure TFPList.Exchange(Index1, Index2: Integer);
var
  Temp : Pointer;
begin
  If ((Index1 >= FCount) or (Index1 < 0)) then
    Error(SListIndexError, Index1);
  If ((Index2 >= FCount) or (Index2 < 0)) then
    Error(SListIndexError, Index2);
  Temp := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Temp;
end;

function TFPList.Expand: TFPList;
var
  IncSize : Longint;
begin
  Result := Self;
  if FCount < FCapacity then
    exit;
  IncSize := sizeof(ptrint)*2;
  if FCapacity > 127 then
    Inc(IncSize, FCapacity shr 2)
  else if FCapacity > sizeof(ptrint)*4 then
    Inc(IncSize, FCapacity shr 1)
  else if FCapacity >= sizeof(ptrint) then
    inc(IncSize,sizeof(ptrint));
  SetCapacity(FCapacity + IncSize);
end;

function TFPList.First: Pointer;
begin
  If FCount = 0 then
    Result := Nil
  else
    Result := Items[0];
end;

function TFPList.IndexOf(Item: Pointer): Integer;
var
  psrc  : PPointer;
  Index : Integer;
begin
  Result:=-1;
  psrc:=@FList^[0];
  For Index:=0 To FCount-1 Do
    begin
      if psrc^=Item then
        begin
          Result:=Index;
          exit;
        end;
      inc(psrc);
    end;
end;

procedure TFPList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount )then
    Error(SlistIndexError, Index);
  iF FCount = FCapacity then Self.Expand;
  if Index<FCount then
    System.Move(Flist^[Index], Flist^[Index+1], (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  FCount := FCount + 1;
end;

function TFPList.Last: Pointer;
begin
{ Wouldn't it be better to return nil if the count is zero ?}
  If FCount = 0 then
    Result := nil
  else
    Result := Items[FCount - 1];
end;

procedure TFPList.Move(CurIndex, NewIndex: Integer);
var
  Temp : Pointer;
begin
  if ((CurIndex < 0) or (CurIndex > Count - 1)) then
    Error(SListIndexError, CurIndex);
  if (NewINdex < 0) then
    Error(SlistIndexError, NewIndex);
  Temp := FList^[CurIndex];
  FList^[CurIndex] := nil;
  Self.Delete(CurIndex);
  Self.Insert(NewIndex, nil);
  FList^[NewIndex] := Temp;
end;

function TFPList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  If Result <> -1 then
    Self.Delete(Result);
end;

procedure TFPList.Pack;
var
  NewCount,
  i : integer;
  pdest,
  psrc : PPointer;
begin
  NewCount:=0;
  psrc:=@FList^[0];
  pdest:=psrc;
  For I:=0 To FCount-1 Do
    begin
      if assigned(psrc^) then
        begin
          pdest^:=psrc^;
          inc(pdest);
          inc(NewCount);
        end;
      inc(psrc);
    end;
  FCount:=NewCount;
end;


Procedure QuickSort(FList: PPointerList; L, R : Longint;Compare: TListSortCompare);
var
  I, J, P: Longint;
  PItem, Q : Pointer;
begin
 repeat
   I := L;
   J := R;
   P := (L + R) div 2;
   repeat
     PItem := FList^[P];
     while Compare(PItem, FList^[i]) > 0 do
       I := I + 1;
     while Compare(PItem, FList^[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FList^[I];
       Flist^[I] := FList^[J];
       FList^[J] := Q;
       if P = I then
        P := J
       else if P = J then
        P := I;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   if L < J then
     QuickSort(FList, L, J, Compare);
   L := I;
 until I >= R;
end;

procedure TFPList.Sort(Compare: TListSortCompare);
begin
  if Not Assigned(FList) or (FCount < 2) then exit;
  QuickSort(Flist, 0, FCount-1, Compare);
end;

procedure TFPList.Assign(Obj: TFPList);
var
  i: Integer;
begin
  Clear;
  for I := 0 to Obj.Count - 1 do
    Add(Obj[i]);
end;


procedure TFPList.ForEachCall(proc2call:TListCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  For I:=0 To Count-1 Do
    begin
      p:=FList^[i];
      if assigned(p) then
        proc2call(p,arg);
    end;
end;


procedure TFPList.ForEachCall(proc2call:TListStaticCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  For I:=0 To Count-1 Do
    begin
      p:=FList^[i];
      if assigned(p) then
        proc2call(p,arg);
    end;
end;


{*****************************************************************************
            TFPObjectList (Copied from rtl/objpas/classes/lists.inc)
*****************************************************************************}

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

function TFPObjectList.GetItem(Index: Integer): TObject;
begin
  Result := TObject(FList[Index]);
end;

procedure TFPObjectList.SetItem(Index: Integer; AObject: TObject);
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

function TFPObjectList.Add(AObject: TObject): Integer;
begin
  Result := FList.Add(AObject);
end;

procedure TFPObjectList.Delete(Index: Integer);
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

procedure TFPObjectList.Insert(Index: Integer; AObject: TObject);
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


{*****************************************************************************
                            TFPHashList
*****************************************************************************}

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
            result:=LongWord(LongInt(result shl 5) - LongInt(result)) xor LongWord(P^);
            inc(p);
          end;
{$ifdef overflowon}
{$Q+}
{$undef overflowon}
{$endif}
      end;

    function FPHash(P: PChar; Len: Integer): LongWord;
      Var
        pmax : pchar;
      begin
{$ifopt Q+}
{$define overflowon}
{$Q-}
{$endif}
        result:=0;
        pmax:=p+len;
        while (p<pmax) do
          begin
            result:=LongWord(LongInt(result shl 5) - LongInt(result)) xor LongWord(P^);
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
  FHashList^[Index].Data:=Item;
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


function TFPHashList.GetNextCollision(Index: Integer): Integer;
begin
  Result:=-1;
  if ((Index > -1) and (Index < FCount)) then
    Result:=FHashList^[Index].NextIndex;
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
  { Maybe expand hash also }
  if FCapacity>FHashCapacity*MaxItemsPerHash then
    SetHashCapacity(FCapacity div MaxItemsPerHash);
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
  FHashTable^[0]:=-1; // sethashcapacity does not always call rehash
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


{****************************************************************************
                             TLinkedListItem
 ****************************************************************************}

    constructor TLinkedListItem.Create;
      begin
        Previous:=nil;
        Next:=nil;
      end;


    destructor TLinkedListItem.Destroy;
      begin
      end;


    function TLinkedListItem.GetCopy:TLinkedListItem;
      var
        p : TLinkedListItem;
        l : integer;
      begin
        p:=TLinkedListItemClass(ClassType).Create;
        l:=InstanceSize;
        Move(pointer(self)^,pointer(p)^,l);
        Result:=p;
      end;


{****************************************************************************
                                   TLinkedList
 ****************************************************************************}

    constructor TLinkedList.Create;
      begin
        FFirst:=nil;
        Flast:=nil;
        FCount:=0;
        FNoClear:=False;
      end;


    destructor TLinkedList.destroy;
      begin
        if not FNoClear then
         Clear;
      end;


    function TLinkedList.empty:boolean;
      begin
        Empty:=(FFirst=nil);
      end;


    procedure TLinkedList.Insert(Item:TLinkedListItem);
      begin
        if FFirst=nil then
         begin
           FLast:=Item;
           Item.Previous:=nil;
           Item.Next:=nil;
         end
        else
         begin
           FFirst.Previous:=Item;
           Item.Previous:=nil;
           Item.Next:=FFirst;
         end;
        FFirst:=Item;
        inc(FCount);
      end;


    procedure TLinkedList.InsertBefore(Item,Loc : TLinkedListItem);
      begin
         Item.Previous:=Loc.Previous;
         Item.Next:=Loc;
         Loc.Previous:=Item;
         if assigned(Item.Previous) then
           Item.Previous.Next:=Item
         else
           { if we've no next item, we've to adjust FFist }
           FFirst:=Item;
         inc(FCount);
      end;


    procedure TLinkedList.InsertAfter(Item,Loc : TLinkedListItem);
      begin
         Item.Next:=Loc.Next;
         Loc.Next:=Item;
         Item.Previous:=Loc;
         if assigned(Item.Next) then
           Item.Next.Previous:=Item
         else
           { if we've no next item, we've to adjust FLast }
           FLast:=Item;
         inc(FCount);
      end;


    procedure TLinkedList.Concat(Item:TLinkedListItem);
      begin
        if FFirst=nil then
         begin
           FFirst:=Item;
           Item.Previous:=nil;
           Item.Next:=nil;
         end
        else
         begin
           Flast.Next:=Item;
           Item.Previous:=Flast;
           Item.Next:=nil;
         end;
        Flast:=Item;
        inc(FCount);
      end;


    procedure TLinkedList.remove(Item:TLinkedListItem);
      begin
         if Item=nil then
           exit;
         if (FFirst=Item) and (Flast=Item) then
           begin
              FFirst:=nil;
              Flast:=nil;
           end
         else if FFirst=Item then
           begin
              FFirst:=Item.Next;
              if assigned(FFirst) then
                FFirst.Previous:=nil;
           end
         else if Flast=Item then
           begin
              Flast:=Flast.Previous;
              if assigned(Flast) then
                Flast.Next:=nil;
           end
         else
           begin
              Item.Previous.Next:=Item.Next;
              Item.Next.Previous:=Item.Previous;
           end;
         Item.Next:=nil;
         Item.Previous:=nil;
         dec(FCount);
      end;


    procedure TLinkedList.clear;
      var
        NewNode, Next : TLinkedListItem;
      begin
        NewNode:=FFirst;
        while assigned(NewNode) do
         begin
           Next:=NewNode.Next;
           NewNode.Free;
           NewNode:=Next;
          end;
        FLast:=nil;
        FFirst:=nil;
        FCount:=0;
      end;


    function TLinkedList.GetFirst:TLinkedListItem;
      begin
         if FFirst=nil then
          GetFirst:=nil
         else
          begin
            GetFirst:=FFirst;
            if FFirst=FLast then
             FLast:=nil;
            FFirst:=FFirst.Next;
            dec(FCount);
          end;
      end;


    function TLinkedList.GetLast:TLinkedListItem;
      begin
         if FLast=nil then
          Getlast:=nil
         else
          begin
            Getlast:=FLast;
            if FLast=FFirst then
             FFirst:=nil;
            FLast:=FLast.Previous;
            dec(FCount);
          end;
      end;


    procedure TLinkedList.insertList(p : TLinkedList);
      begin
         { empty List ? }
         if (p.FFirst=nil) then
           exit;
         p.Flast.Next:=FFirst;
         { we have a double Linked List }
         if assigned(FFirst) then
           FFirst.Previous:=p.Flast;
         FFirst:=p.FFirst;
         if (FLast=nil) then
           Flast:=p.Flast;
         inc(FCount,p.FCount);
         { p becomes empty }
         p.FFirst:=nil;
         p.Flast:=nil;
         p.FCount:=0;
      end;


    procedure TLinkedList.insertListBefore(Item:TLinkedListItem;p : TLinkedList);
      begin
         { empty List ? }
         if (p.FFirst=nil) then
           exit;
         if (Item=nil) then
           begin
             { Insert at begin }
             InsertList(p);
             exit;
           end
         else
           begin
             p.FLast.Next:=Item;
             p.FFirst.Previous:=Item.Previous;
             if assigned(Item.Previous) then
               Item.Previous.Next:=p.FFirst
             else
               FFirst:=p.FFirst;
             Item.Previous:=p.FLast;
             inc(FCount,p.FCount);
           end;
         { p becomes empty }
         p.FFirst:=nil;
         p.Flast:=nil;
         p.FCount:=0;
      end;


    procedure TLinkedList.insertListAfter(Item:TLinkedListItem;p : TLinkedList);
      begin
         { empty List ? }
         if (p.FFirst=nil) then
           exit;
         if (Item=nil) then
           begin
             { Insert at begin }
             InsertList(p);
             exit;
           end
         else
           begin
             p.FFirst.Previous:=Item;
             p.FLast.Next:=Item.Next;
             if assigned(Item.Next) then
               Item.Next.Previous:=p.FLast
             else
               FLast:=p.FLast;
             Item.Next:=p.FFirst;
             inc(FCount,p.FCount);
           end;
         { p becomes empty }
         p.FFirst:=nil;
         p.Flast:=nil;
         p.FCount:=0;
      end;


    procedure TLinkedList.concatList(p : TLinkedList);
      begin
        if (p.FFirst=nil) then
         exit;
        if FFirst=nil then
         FFirst:=p.FFirst
        else
         begin
           FLast.Next:=p.FFirst;
           p.FFirst.Previous:=Flast;
         end;
        Flast:=p.Flast;
        inc(FCount,p.FCount);
        { make p empty }
        p.Flast:=nil;
        p.FFirst:=nil;
        p.FCount:=0;
      end;


    procedure TLinkedList.insertListcopy(p : TLinkedList);
      var
        NewNode,NewNode2 : TLinkedListItem;
      begin
        NewNode:=p.Last;
        while assigned(NewNode) do
         begin
           NewNode2:=NewNode.Getcopy;
           if assigned(NewNode2) then
            Insert(NewNode2);
           NewNode:=NewNode.Previous;
         end;
      end;


    procedure TLinkedList.concatListcopy(p : TLinkedList);
      var
        NewNode,NewNode2 : TLinkedListItem;
      begin
        NewNode:=p.First;
        while assigned(NewNode) do
         begin
           NewNode2:=NewNode.Getcopy;
           if assigned(NewNode2) then
            Concat(NewNode2);
           NewNode:=NewNode.Next;
         end;
      end;


{****************************************************************************
                             TCmdStrListItem
 ****************************************************************************}

    constructor TCmdStrListItem.Create(const s:TCmdStr);
      begin
        inherited Create;
        FPStr:=s;
      end;


    destructor TCmdStrListItem.Destroy;
      begin
        FPStr:='';
      end;


    function TCmdStrListItem.Str:TCmdStr;
      begin
        Str:=FPStr;
      end;


    function TCmdStrListItem.GetCopy:TLinkedListItem;
      begin
        Result:=(inherited GetCopy);
        TCmdStrListItem(Result).FPStr:=FPstr;
      end;


{****************************************************************************
                           TCmdStrList
 ****************************************************************************}

    constructor TCmdStrList.Create;
      begin
         inherited Create;
         FDoubles:=true;
      end;


    constructor TCmdStrList.Create_no_double;
      begin
         inherited Create;
         FDoubles:=false;
      end;


    procedure TCmdStrList.insert(const s : TCmdStr);
      begin
         if (s='') or
            ((not FDoubles) and (find(s)<>nil)) then
          exit;
         inherited insert(TCmdStrListItem.create(s));
      end;


    procedure TCmdStrList.concat(const s : TCmdStr);
      begin
         if (s='') or
            ((not FDoubles) and (find(s)<>nil)) then
          exit;
         inherited concat(TCmdStrListItem.create(s));
      end;


    procedure TCmdStrList.remove(const s : TCmdStr);
      var
        p : TCmdStrListItem;
      begin
        if s='' then
         exit;
        p:=find(s);
        if assigned(p) then
         begin
           inherited Remove(p);
           p.Free;
         end;
      end;


    function TCmdStrList.GetFirst : TCmdStr;
      var
         p : TCmdStrListItem;
      begin
         p:=TCmdStrListItem(inherited GetFirst);
         if p=nil then
          GetFirst:=''
         else
          begin
            GetFirst:=p.FPStr;
            p.free;
          end;
      end;


    function TCmdStrList.Getlast : TCmdStr;
      var
         p : TCmdStrListItem;
      begin
         p:=TCmdStrListItem(inherited Getlast);
         if p=nil then
          Getlast:=''
         else
          begin
            Getlast:=p.FPStr;
            p.free;
          end;
      end;


    function TCmdStrList.FindCase(const s:TCmdStr):TCmdStrListItem;
      var
        NewNode : TCmdStrListItem;
      begin
        result:=nil;
        if s='' then
         exit;
        NewNode:=TCmdStrListItem(FFirst);
        while assigned(NewNode) do
         begin
           if NewNode.FPStr=s then
            begin
              result:=NewNode;
              exit;
            end;
           NewNode:=TCmdStrListItem(NewNode.Next);
         end;
      end;


    function TCmdStrList.Find(const s:TCmdStr):TCmdStrListItem;
      var
        NewNode : TCmdStrListItem;
      begin
        result:=nil;
        if s='' then
         exit;
        NewNode:=TCmdStrListItem(FFirst);
        while assigned(NewNode) do
         begin
           if SysUtils.CompareText(s, NewNode.FPStr)=0 then
            begin
              result:=NewNode;
              exit;
            end;
           NewNode:=TCmdStrListItem(NewNode.Next);
         end;
      end;


    procedure TCmdStrList.InsertItem(item:TCmdStrListItem);
      begin
        inherited Insert(item);
      end;


    procedure TCmdStrList.ConcatItem(item:TCmdStrListItem);
      begin
        inherited Concat(item);
      end;


{****************************************************************************
                                tdynamicarray
****************************************************************************}

    constructor tdynamicarray.create(Ablocksize:longword);
      begin
        FPosn:=0;
        FPosnblock:=nil;
        FFirstblock:=nil;
        FLastblock:=nil;
        FCurrBlockSize:=0;
        FMaxBlockSize:=Ablocksize;
        grow;
      end;


    destructor tdynamicarray.destroy;
      var
        hp : pdynamicblock;
      begin
        while assigned(FFirstblock) do
         begin
           hp:=FFirstblock;
           FFirstblock:=FFirstblock^.Next;
           Freemem(hp);
         end;
      end;


    function  tdynamicarray.size:longword;
      begin
        if assigned(FLastblock) then
         size:=FLastblock^.pos+FLastblock^.used
        else
         size:=0;
      end;


    procedure tdynamicarray.reset;
      var
        hp : pdynamicblock;
      begin
        while assigned(FFirstblock) do
         begin
           hp:=FFirstblock;
           FFirstblock:=FFirstblock^.Next;
           Freemem(hp);
         end;
        FPosn:=0;
        FPosnblock:=nil;
        FFirstblock:=nil;
        FLastblock:=nil;
        grow;
      end;


    procedure tdynamicarray.grow;
      var
        nblock  : pdynamicblock;
        OptBlockSize,
        IncSize : integer;
      begin
        if CurrBlockSize<FMaxBlocksize then
          begin
            IncSize := sizeof(ptrint)*8;
            if FCurrBlockSize > 255 then
              Inc(IncSize, FCurrBlockSize shr 2);
            inc(FCurrBlockSize,IncSize);
          end;
        if CurrBlockSize>FMaxBlocksize then
          FCurrBlockSize:=FMaxBlocksize;
        { Calculate the most optimal size so there is no alignment overhead
          lost in the heap manager }
        OptBlockSize:=cutils.Align(CurrBlockSize+dynamicblockbasesize,16)-dynamicblockbasesize-sizeof(ptrint);
        Getmem(nblock,OptBlockSize+dynamicblockbasesize);
        if not assigned(FFirstblock) then
         begin
           FFirstblock:=nblock;
           FPosnblock:=nblock;
           nblock^.pos:=0;
         end
        else
         begin
           FLastblock^.Next:=nblock;
           nblock^.pos:=FLastblock^.pos+FLastblock^.size;
         end;
        nblock^.used:=0;
        nblock^.size:=OptBlockSize;
        nblock^.Next:=nil;
        fillchar(nblock^.data,nblock^.size,0);
        FLastblock:=nblock;
      end;


    procedure tdynamicarray.align(i:longword);
      var
        j : longword;
      begin
        j:=(FPosn mod i);
        if j<>0 then
         begin
           j:=i-j;
           if FPosnblock^.used+j>FPosnblock^.size then
            begin
              dec(j,FPosnblock^.size-FPosnblock^.used);
              FPosnblock^.used:=FPosnblock^.size;
              grow;
              FPosnblock:=FLastblock;
            end;
           inc(FPosnblock^.used,j);
           inc(FPosn,j);
         end;
      end;


    procedure tdynamicarray.seek(i:longword);
      begin
        if (i<FPosnblock^.pos) or (i>=FPosnblock^.pos+FPosnblock^.size) then
         begin
           { set FPosnblock correct if the size is bigger then
             the current block }
           if FPosnblock^.pos>i then
            FPosnblock:=FFirstblock;
           while assigned(FPosnblock) do
            begin
              if FPosnblock^.pos+FPosnblock^.size>i then
               break;
              FPosnblock:=FPosnblock^.Next;
            end;
           { not found ? then increase blocks }
           if not assigned(FPosnblock) then
            begin
              repeat
                { the current FLastblock is now also fully used }
                FLastblock^.used:=FLastblock^.size;
                grow;
                FPosnblock:=FLastblock;
              until FPosnblock^.pos+FPosnblock^.size>=i;
            end;
         end;
        FPosn:=i;
        if FPosn-FPosnblock^.pos>FPosnblock^.used then
         FPosnblock^.used:=FPosn-FPosnblock^.pos;
      end;


    procedure tdynamicarray.write(const d;len:longword);
      var
        p : pchar;
        i,j : longword;
      begin
        p:=pchar(@d);
        while (len>0) do
         begin
           i:=FPosn-FPosnblock^.pos;
           if i+len>=FPosnblock^.size then
            begin
              j:=FPosnblock^.size-i;
              move(p^,FPosnblock^.data[i],j);
              inc(p,j);
              inc(FPosn,j);
              dec(len,j);
              FPosnblock^.used:=FPosnblock^.size;
              if assigned(FPosnblock^.Next) then
               FPosnblock:=FPosnblock^.Next
              else
               begin
                 grow;
                 FPosnblock:=FLastblock;
               end;
            end
           else
            begin
              move(p^,FPosnblock^.data[i],len);
              inc(p,len);
              inc(FPosn,len);
              i:=FPosn-FPosnblock^.pos;
              if i>FPosnblock^.used then
               FPosnblock^.used:=i;
              len:=0;
            end;
         end;
      end;


    procedure tdynamicarray.writestr(const s:string);
      begin
        write(s[1],length(s));
      end;


    function tdynamicarray.read(var d;len:longword):longword;
      var
        p : pchar;
        i,j,res : longword;
      begin
        res:=0;
        p:=pchar(@d);
        while (len>0) do
         begin
           i:=FPosn-FPosnblock^.pos;
           if i+len>=FPosnblock^.used then
            begin
              j:=FPosnblock^.used-i;
              move(FPosnblock^.data[i],p^,j);
              inc(p,j);
              inc(FPosn,j);
              inc(res,j);
              dec(len,j);
              if assigned(FPosnblock^.Next) then
               FPosnblock:=FPosnblock^.Next
              else
               break;
            end
           else
            begin
              move(FPosnblock^.data[i],p^,len);
              inc(p,len);
              inc(FPosn,len);
              inc(res,len);
              len:=0;
            end;
         end;
        read:=res;
      end;


    procedure tdynamicarray.readstream(f:TCStream;maxlen:longword);
      var
        i,left : longword;
      begin
        repeat
          left:=FPosnblock^.size-FPosnblock^.used;
          if left>maxlen then
           left:=maxlen;
          i:=f.Read(FPosnblock^.data[FPosnblock^.used],left);
          dec(maxlen,i);
          inc(FPosnblock^.used,i);
          if FPosnblock^.used=FPosnblock^.size then
           begin
             if assigned(FPosnblock^.Next) then
              FPosnblock:=FPosnblock^.Next
             else
              begin
                grow;
                FPosnblock:=FLastblock;
              end;
           end;
        until (i<left) or (maxlen=0);
      end;


    procedure tdynamicarray.writestream(f:TCStream);
      var
        hp : pdynamicblock;
      begin
        hp:=FFirstblock;
        while assigned(hp) do
         begin
           f.Write(hp^.data,hp^.used);
           hp:=hp^.Next;
         end;
      end;

{****************************************************************************
                                thashset
****************************************************************************}

    constructor THashSet.Create(InitSize: Integer; OwnKeys, OwnObjects: Boolean);
      var
        I: Integer;
      begin
        inherited Create;
        FOwnsObjects := OwnObjects;
        FOwnsKeys := OwnKeys;
        I := 64;
        while I < InitSize do I := I shl 1;
        FBucketCount := I;
        FBucket := AllocMem(I * sizeof(PHashSetItem));
      end;


    destructor THashSet.Destroy;
      begin
        Clear;
        FreeMem(FBucket);
        inherited Destroy;
      end;


    procedure THashSet.Clear;
      var
        I: Integer;
        item, next: PHashSetItem;
      begin
        for I := 0 to FBucketCount-1 do
        begin
          item := FBucket[I];
          while Assigned(item) do
          begin
            next := item^.Next;
            if FOwnsObjects then
              item^.Data.Free;
            if FOwnsKeys then
              FreeMem(item^.Key);
            Dispose(item);
            item := next;
          end;
        end;
        FillChar(FBucket^, FBucketCount * sizeof(PHashSetItem), 0);
      end;


    function THashSet.Find(Key: Pointer; KeyLen: Integer): PHashSetItem;
      var
        Dummy: Boolean;
      begin
        Result := Lookup(Key, KeyLen, Dummy, False);
      end;


    function THashSet.FindOrAdd(Key: Pointer; KeyLen: Integer;
        var Found: Boolean): PHashSetItem;
      begin
        Result := Lookup(Key, KeyLen, Found, True);
      end;


    function THashSet.FindOrAdd(Key: Pointer; KeyLen: Integer): PHashSetItem;
      var
        Dummy: Boolean;
      begin
        Result := Lookup(Key, KeyLen, Dummy, True);
      end;


    function THashSet.Get(Key: Pointer; KeyLen: Integer): TObject;
      var
        e: PHashSetItem;
        Dummy: Boolean;
      begin
        e := Lookup(Key, KeyLen, Dummy, False);
        if Assigned(e) then
          Result := e^.Data
        else
          Result := nil;
      end;


    function THashSet.Lookup(Key: Pointer; KeyLen: Integer;
      var Found: Boolean; CanCreate: Boolean): PHashSetItem;
      var
        Entry: PPHashSetItem;
        h: LongWord;
      begin
        h := FPHash(Key, KeyLen);
        Entry := @FBucket[h mod FBucketCount];
        while Assigned(Entry^) and
          not ((Entry^^.HashValue = h) and (Entry^^.KeyLength = KeyLen) and
            (CompareByte(Entry^^.Key^, Key^, KeyLen) = 0)) do
              Entry := @Entry^^.Next;
        Found := Assigned(Entry^);
        if Found or (not CanCreate) then
          begin
            Result := Entry^;
            Exit;
          end;
        if FCount > FBucketCount then  { arbitrary limit, probably too high }
          begin
            { rehash and repeat search }
            Resize(FBucketCount * 2);
            Result := Lookup(Key, KeyLen, Found, CanCreate);
          end
        else
          begin
            New(Result);
            if FOwnsKeys then
            begin
              GetMem(Result^.Key, KeyLen);
              Move(Key^, Result^.Key^, KeyLen);
            end
            else
              Result^.Key := Key;
            Result^.KeyLength := KeyLen;
            Result^.HashValue := h;
            Result^.Data := nil;
            Result^.Next := nil;
            Inc(FCount);
            Entry^ := Result;
          end;
        end;


    procedure THashSet.Resize(NewCapacity: LongWord);
      var
        p, chain: PPHashSetItem;
        i: Integer;
        e, n: PHashSetItem;
      begin
        p := AllocMem(NewCapacity * sizeof(PHashSetItem));
        for i := 0 to FBucketCount-1 do
          begin
            e := FBucket[i];
            while Assigned(e) do
            begin
              chain := @p[e^.HashValue mod NewCapacity];
              n := e^.Next;
              e^.Next := chain^;
              chain^ := e;
              e := n;
            end;
          end;
        FBucketCount := NewCapacity;
        FreeMem(FBucket);
        FBucket := p;
      end;


    function THashSet.Remove(Entry: PHashSetItem): Boolean;
      var
        chain: PPHashSetItem;
      begin
        chain := @FBucket[Entry^.HashValue mod FBucketCount];
        while Assigned(chain^) do
          begin
            if chain^ = Entry then
              begin
                chain^ := Entry^.Next;
                if FOwnsObjects then
                  Entry^.Data.Free;
                if FOwnsKeys then
                  FreeMem(Entry^.Key);
                Dispose(Entry);
                Dec(FCount);
                Result := True;
                Exit;
              end;
            chain := @chain^^.Next;
          end;
        Result := False;
      end;

end.
