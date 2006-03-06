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

interface

    uses
      SysUtils,
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
     TFPObjectList (From rtl/objpas/classes/classesh.inc)
********************************************************}

const
   MaxListSize = Maxint div 16;
   SListIndexError = 'List index exceeds bounds (%d)';
   SListCapacityError = 'The maximum list capacity is reached (%d)';
   SListCountError = 'List count too large (%d)';
type
   EListError = class(Exception);

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
    function Get(Index: Integer): Pointer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure Put(Index: Integer; Item: Pointer); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    Procedure RaiseIndexError(Index : Integer);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure Clear;
    procedure Delete(Index: Integer); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    class procedure Error(const Msg: string; Data: PtrInt);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TFPList; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function Extract(item: Pointer): Pointer;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function Last: Pointer;
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
          function  Empty:boolean;
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
                TStringList
********************************************}

       { string containerItem }
       TStringListItem = class(TLinkedListItem)
          FPStr : PString;
       public
          constructor Create(const s:string);
          destructor  Destroy;override;
          function GetCopy:TLinkedListItem;override;
          function Str:string;
       end;

       { string container }
       TStringList = class(TLinkedList)
       private
          FDoubles : boolean;  { if this is set to true, doubles are allowed }
       public
          constructor Create;
          constructor Create_No_Double;
          { inserts an Item }
          procedure Insert(const s:string);
          { concats an Item }
          procedure Concat(const s:string);
          { deletes an Item }
          procedure Remove(const s:string);
          { Gets First Item }
          function  GetFirst:string;
          { Gets last Item }
          function  GetLast:string;
          { true if string is in the container, compare case sensitive }
          function FindCase(const s:string):TStringListItem;
          { true if string is in the container }
          function Find(const s:string):TStringListItem;
          { inserts an item }
          procedure InsertItem(item:TStringListItem);
          { concats an item }
          procedure ConcatItem(item:TStringListItem);
          property Doubles:boolean read FDoubles write FDoubles;
          procedure readstream(f:TCStream);
          procedure writestream(f:TCStream);
       end;


{********************************************
                Dictionary
********************************************}

    const
       { the real size will be [0..hasharray-1] ! }
       hasharraysize = 512;

    type
       { namedindexobect for use with dictionary and indexarray }
       TNamedIndexItem=class
       private
       { indexarray }
         FIndexNr    : integer;
         FIndexNext  : TNamedIndexItem;
       { dictionary }
         FLeft,
         FRight      : TNamedIndexItem;
         FSpeedValue : cardinal;
         FName       : Pstring;
       protected
         function  GetName:string;virtual;
         procedure SetName(const n:string);virtual;
       public
         constructor Create;
         constructor CreateName(const n:string);
         destructor  Destroy;override;
         property IndexNr:integer read FIndexNr write FIndexNr;
         property IndexNext:TNamedIndexItem read FIndexNext write FIndexNext;
         property Name:string read GetName write SetName;
         property SpeedValue:cardinal read FSpeedValue;
         property Left:TNamedIndexItem read FLeft write FLeft;
         property Right:TNamedIndexItem read FRight write FRight;
       end;

       Pdictionaryhasharray=^Tdictionaryhasharray;
       Tdictionaryhasharray=array[0..hasharraysize-1] of TNamedIndexItem;

       TnamedIndexCallback = procedure(p:TNamedIndexItem;arg:pointer) of object;
       TnamedIndexStaticCallback = procedure(p:TNamedIndexItem;arg:pointer);

       Tdictionary=class
       private
         FRoot      : TNamedIndexItem;
         FCount     : longint;
         FHashArray : Pdictionaryhasharray;
         procedure cleartree(var obj:TNamedIndexItem);
         function  insertNode(NewNode:TNamedIndexItem;var currNode:TNamedIndexItem):TNamedIndexItem;
         procedure inserttree(currtree,currroot:TNamedIndexItem);
       public
         noclear   : boolean;
         delete_doubles : boolean;
         constructor Create;
         destructor  Destroy;override;
         procedure usehash;
         procedure clear;
         function  delete(const s:string):TNamedIndexItem;
         function  empty:boolean;
         procedure foreach(proc2call:TNamedIndexcallback;arg:pointer);
         procedure foreach_static(proc2call:TNamedIndexStaticCallback;arg:pointer);
         function  insert(obj:TNamedIndexItem):TNamedIndexItem;
         function  replace(oldobj,newobj:TNamedIndexItem):boolean;
         function  rename(const olds,News : string):TNamedIndexItem;
         function  search(const s:string):TNamedIndexItem;
         function  speedsearch(const s:string;SpeedValue:cardinal):TNamedIndexItem;
         property  Items[const s:string]:TNamedIndexItem read Search;default;
         property  Count:longint read FCount;
       end;

      tindexobjectarray=array[1..16000] of TNamedIndexItem;
      pnamedindexobjectarray=^tindexobjectarray;

      tindexarray=class
        noclear : boolean;
        First   : TNamedIndexItem;
        count   : integer;
        constructor Create(Agrowsize:integer);
        destructor  destroy;override;
        procedure clear;
        procedure foreach(proc2call : Tnamedindexcallback;arg:pointer);
        procedure foreach_static(proc2call : Tnamedindexstaticcallback;arg:pointer);
        procedure deleteindex(p:TNamedIndexItem);
        procedure delete(var p:TNamedIndexItem);
        procedure insert(p:TNamedIndexItem);
        procedure replace(oldp,newp:TNamedIndexItem);
        function  search(nr:integer):TNamedIndexItem;
        property  Items[Index: Integer]: TNamedIndexItem read Search; default;
      private
        growsize,
        size  : integer;
        data  : pnamedindexobjectarray;
        procedure grow(gsize:integer);
      end;


{********************************************
              DynamicArray
********************************************}

     const
       dynamicblockbasesize = 12;

     type
       pdynamicblock = ^tdynamicblock;
       tdynamicblock = record
         pos,
         used : integer;
         Next : pdynamicblock;
         { can't use sizeof(integer) because it crashes gdb }
         data : array[0..1024*1024] of byte;
       end;

       tdynamicarray = class
       private
         FPosn       : integer;
         FPosnblock  : pdynamicblock;
         FBlocksize  : integer;
         FFirstblock,
         FLastblock  : pdynamicblock;
         procedure grow;
       public
         constructor Create(Ablocksize:integer);
         destructor  Destroy;override;
         procedure reset;
         function  size:integer;
         procedure align(i:integer);
         procedure seek(i:integer);
         function  read(var d;len:integer):integer;
         procedure write(const d;len:integer);
         procedure writestr(const s:string);
         procedure readstream(f:TCStream;maxlen:longint);
         procedure writestream(f:TCStream);
         property  BlockSize : integer read FBlocksize;
         property  FirstBlock : PDynamicBlock read FFirstBlock;
         property  Pos : integer read FPosn;
       end;


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

Const
  // Ratio of Pointer and Word Size.
  WordRatio = SizeOf(Pointer) Div SizeOf(Word);

procedure TFPList.RaiseIndexError(Index : Integer);
begin
  Error(SListIndexError, Index);
end;

function TFPList.Get(Index: Integer): Pointer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FList^[Index];
end;

procedure TFPList.Put(Index: Integer; Item: Pointer); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
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
      FillWord(Flist^[FCount], (NewCount-FCount) *  WordRatio, 0);
    end;
  FCount := Newcount;
end;

destructor TFPList.Destroy;
begin
  Self.Clear;
  inherited Destroy;
end;

function TFPList.Add(Item: Pointer): Integer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  if FCount = FCapacity then
    Self.Expand;
  FList^[FCount] := Item;
  Result := FCount;
  FCount := FCount + 1;
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

procedure TFPList.Delete(Index: Integer); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  If (Index<0) or (Index>=FCount) then
    Error (SListIndexError, Index);
  FCount := FCount-1;
  System.Move (FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(Pointer));
  // Shrink the list if appropriate
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

function TFPList.Expand: TFPList; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
var
  IncSize : Longint;
begin
  if FCount < FCapacity then exit;
  IncSize := 4;
  if FCapacity > 3 then IncSize := IncSize + 4;
  if FCapacity > 8 then IncSize := IncSize+8;
  if FCapacity > 127 then Inc(IncSize, FCapacity shr 2);
  SetCapacity(FCapacity + IncSize);
  Result := Self;
end;

function TFPList.First: Pointer;
begin
  If FCount = 0 then
    Result := Nil
  else
    Result := Items[0];
end;

function TFPList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while(Result < FCount) and (Flist^[Result] <> Item) do Result := Result + 1;
  If Result = FCount  then Result := -1;
end;

procedure TFPList.Insert(Index: Integer; Item: Pointer); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
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
Var
  {Last,I,J,}
  Runner : Longint;
begin
  // Not the fastest; but surely correct
  for Runner := Fcount - 1 downto 0 do
    if Items[Runner] = Nil then
      Self.Delete(Runner);
{ The following may be faster in case of large and defragmented lists
  If count=0 then exit;
  Runner:=0;I:=0;
  TheLast:=Count;
  while runner<count do
    begin
    // Find first Nil
    While (FList^[Runner]<>Nil) and (Runner<Count) do Runner:=Runner+1;
    if Runner<Count do
      begin
      // Start searching for non-nil from last known nil+1
      if i<Runner then I:=Runner+1;
      While (Flist[I]^=Nil) and (I<Count) do I:=I+1;
      // Start looking for last non-nil of block.
      J:=I+1;
      While (Flist^[J]<>Nil) and (J<Count) do J:=J+1;
      // Move block and zero out
      Move (Flist^[I],Flist^[Runner],J*SizeOf(Pointer));
      FillWord (Flist^[I],(J-I)*WordRatio,0);
      // Update Runner and Last to point behind last block
      TheLast:=Runner+(J-I);
      If J=Count then
         begin
         // Shortcut, when J=Count we checked all pointers
         Runner:=Count
      else
         begin
         Runner:=TheLast;
         I:=j;
      end;
    end;
  Count:=TheLast;
}
end;

// Needed by Sort method.

Procedure QuickSort(FList: PPointerList; L, R : Longint;
                     Compare: TListSortCompare);
var
  I, J : Longint;
  P, Q : Pointer;
begin
 repeat
   I := L;
   J := R;
   P := FList^[ (L + R) div 2 ];
   repeat
     while Compare(P, FList^[i]) > 0 do
       I := I + 1;
     while Compare(P, FList^[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FList^[I];
       Flist^[I] := FList^[J];
       FList^[J] := Q;
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
        NewNode : TLinkedListItem;
      begin
        NewNode:=FFirst;
        while assigned(NewNode) do
         begin
           FFirst:=NewNode.Next;
           NewNode.Free;
           NewNode:=FFirst;
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
        NewNode:=p.First;
        while assigned(NewNode) do
         begin
           NewNode2:=NewNode.Getcopy;
           if assigned(NewNode2) then
            Insert(NewNode2);
           NewNode:=NewNode.Next;
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
                             TStringListItem
 ****************************************************************************}

    constructor TStringListItem.Create(const s:string);
      begin
        inherited Create;
        FPStr:=stringdup(s);
      end;


    destructor TStringListItem.Destroy;
      begin
        stringdispose(FPStr);
      end;


    function TStringListItem.Str:string;
      begin
        Str:=FPStr^;
      end;


    function TStringListItem.GetCopy:TLinkedListItem;
      begin
        Result:=(inherited GetCopy);
        TStringListItem(Result).FPStr:=stringdup(FPstr^);
      end;


{****************************************************************************
                           TSTRINGList
 ****************************************************************************}

    constructor tstringList.Create;
      begin
         inherited Create;
         FDoubles:=true;
      end;


    constructor tstringList.Create_no_double;
      begin
         inherited Create;
         FDoubles:=false;
      end;


    procedure tstringList.insert(const s : string);
      begin
         if (s='') or
            ((not FDoubles) and (find(s)<>nil)) then
          exit;
         inherited insert(tstringListItem.create(s));
      end;


    procedure tstringList.concat(const s : string);
      begin
         if (s='') or
            ((not FDoubles) and (find(s)<>nil)) then
          exit;
         inherited concat(tstringListItem.create(s));
      end;


    procedure tstringList.remove(const s : string);
      var
        p : tstringListItem;
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


    function tstringList.GetFirst : string;
      var
         p : tstringListItem;
      begin
         p:=tstringListItem(inherited GetFirst);
         if p=nil then
          GetFirst:=''
         else
          begin
            GetFirst:=p.FPStr^;
            p.free;
          end;
      end;


    function tstringList.Getlast : string;
      var
         p : tstringListItem;
      begin
         p:=tstringListItem(inherited Getlast);
         if p=nil then
          Getlast:=''
         else
          begin
            Getlast:=p.FPStr^;
            p.free;
          end;
      end;


    function tstringList.FindCase(const s:string):TstringListItem;
      var
        NewNode : tstringListItem;
      begin
        result:=nil;
        if s='' then
         exit;
        NewNode:=tstringListItem(FFirst);
        while assigned(NewNode) do
         begin
           if NewNode.FPStr^=s then
            begin
              result:=NewNode;
              exit;
            end;
           NewNode:=tstringListItem(NewNode.Next);
         end;
      end;


    function tstringList.Find(const s:string):TstringListItem;
      var
        NewNode : tstringListItem;
        ups     : string;
      begin
        result:=nil;
        if s='' then
         exit;
        ups:=upper(s);
        NewNode:=tstringListItem(FFirst);
        while assigned(NewNode) do
         begin
           if upper(NewNode.FPStr^)=ups then
            begin
              result:=NewNode;
              exit;
            end;
           NewNode:=tstringListItem(NewNode.Next);
         end;
      end;


    procedure TStringList.InsertItem(item:TStringListItem);
      begin
        inherited Insert(item);
      end;


    procedure TStringList.ConcatItem(item:TStringListItem);
      begin
        inherited Concat(item);
      end;


    procedure TStringList.readstream(f:TCStream);
      const
        BufSize = 16384;
      var
        Hsp,
        p,maxp,
        Buf    : PChar;
        Prev   : Char;
        HsPos,
        ReadLen,
        BufPos,
        BufEnd : Longint;
        hs     : string;

        procedure ReadBuf;
        begin
          if BufPos<BufEnd then
            begin
              Move(Buf[BufPos],Buf[0],BufEnd-BufPos);
              Dec(BufEnd,BufPos);
              BufPos:=0;
            end;
          ReadLen:=f.Read(buf[BufEnd],BufSize-BufEnd);
          inc(BufEnd,ReadLen);
        end;

      begin
        Getmem(Buf,Bufsize);
        BufPos:=0;
        BufEnd:=0;
        HsPos:=1;
        ReadBuf;
        repeat
          hsp:=@hs[hsPos];
          p:=@Buf[BufPos];
          maxp:=@Buf[BufEnd];
          while (p<maxp) and not(P^ in [#10,#13]) do
            begin
              hsp^:=p^;
              inc(p);
              if hsp-@hs[1]<255 then
                inc(hsp);
            end;
          inc(BufPos,maxp-p);
          inc(HsPos,maxp-p);
          prev:=p^;
          inc(BufPos);
          { no system uses #10#13 as line seperator (#10 = *nix, #13 = Mac, }
          { #13#10 = Dos), so if we've got #10, we can safely exit          }
          if (prev<>#10) then
            begin
              if (BufPos>=BufEnd) then
                begin
                  ReadBuf;
                  if BufPos>=BufEnd then
                    break;
                end;
              { is there also a #10 after it? }
              if prev=#13 then
                begin
                  if (Buf[BufPos]=#10) then
                    inc(BufPos);
                  prev:=#10;
                end;
            end;
          if prev=#10 then
            begin
              hs[0]:=char(hsp-@hs[1]);
              Concat(hs);
              HsPos:=1;
            end;
        until BufPos>=BufEnd;
        hs[0]:=char(hsp-@hs[1]);
        Concat(hs);
        freemem(buf);
      end;


    procedure TStringList.writestream(f:TCStream);
      var
        Node : TStringListItem;
        LineEnd : string[2];
      begin
        Case DefaultTextLineBreakStyle Of
          tlbsLF: LineEnd := #10;
          tlbsCRLF: LineEnd := #13#10;
          tlbsCR: LineEnd := #13;
        End;
        Node:=tstringListItem(FFirst);
        while assigned(Node) do
          begin
            f.Write(Node.FPStr^[1],Length(Node.FPStr^));
            f.Write(LineEnd[1],length(LineEnd));
            Node:=tstringListItem(Node.Next);
          end;
      end;


{****************************************************************************
                               TNamedIndexItem
 ****************************************************************************}

    constructor TNamedIndexItem.Create;
      begin
        { index }
        Findexnr:=-1;
        FindexNext:=nil;
        { dictionary }
        Fleft:=nil;
        Fright:=nil;
        FName:=nil;
        Fspeedvalue:=cardinal($ffffffff);
      end;

    constructor TNamedIndexItem.Createname(const n:string);
      begin
        { index }
        Findexnr:=-1;
        FindexNext:=nil;
        { dictionary }
        Fleft:=nil;
        Fright:=nil;
        fspeedvalue:=getspeedvalue(n);
      {$ifdef compress}
        FName:=stringdup(minilzw_encode(n));
      {$else}
        FName:=stringdup(n);
      {$endif}
      end;


    destructor TNamedIndexItem.destroy;
      begin
        stringdispose(FName);
      end;


    procedure TNamedIndexItem.setname(const n:string);
      begin
        if assigned(FName) then
          stringdispose(FName);
        fspeedvalue:=getspeedvalue(n);
      {$ifdef compress}
        FName:=stringdup(minilzw_encode(n));
      {$else}
        FName:=stringdup(n);
      {$endif}
      end;


    function TNamedIndexItem.GetName:string;
      begin
        if assigned(FName) then
        {$ifdef compress}
         Getname:=minilzw_decode(FName^)
        {$else}
         Getname:=FName^
        {$endif}
        else
         Getname:='';
      end;


{****************************************************************************
                               TDICTIONARY
****************************************************************************}

    constructor Tdictionary.Create;
      begin
        FRoot:=nil;
        FHashArray:=nil;
        noclear:=false;
        delete_doubles:=false;
      end;


    procedure Tdictionary.usehash;
      begin
        if not(assigned(FRoot)) and
           not(assigned(FHashArray)) then
         begin
           New(FHashArray);
           fillchar(FHashArray^,sizeof(FHashArray^),0);
         end;
      end;


    function counttree(p: tnamedindexitem): longint;
      begin
        counttree:=0;
        if not assigned(p) then
          exit;
        result := 1;
        inc(result,counttree(p.fleft));
        inc(result,counttree(p.fright));
      end;

    destructor Tdictionary.destroy;
      begin
        if not noclear then
         clear;
        if assigned(FHashArray) then
         begin
           dispose(FHashArray);
         end;
      end;


    procedure Tdictionary.cleartree(var obj:TNamedIndexItem);
      begin
        if assigned(obj.Fleft) then
          cleartree(obj.FLeft);
        if assigned(obj.FRight) then
          cleartree(obj.FRight);
        obj.free;
        obj:=nil;
      end;


    procedure Tdictionary.clear;
      var
        w : integer;
      begin
        if assigned(FRoot) then
          cleartree(FRoot);
        if assigned(FHashArray) then
         for w:= low(FHashArray^) to high(FHashArray^) do
          if assigned(FHashArray^[w]) then
           cleartree(FHashArray^[w]);
      end;


    function Tdictionary.delete(const s:string):TNamedIndexItem;
      var
        p,SpeedValue : cardinal;
        n : TNamedIndexItem;
      {$ifdef compress}
        senc:string;
      {$else}
        senc:string absolute s;
      {$endif}

        procedure insert_right_bottom(var root,Atree:TNamedIndexItem);
          begin
            while root.FRight<>nil do
             root:=root.FRight;
            root.FRight:=Atree;
          end;

        function delete_from_tree(root:TNamedIndexItem):TNamedIndexItem;
          type
            leftright=(left,right);
          var
            lr : leftright;
            oldroot : TNamedIndexItem;
          begin
            oldroot:=nil;
            while (root<>nil) and (root.SpeedValue<>SpeedValue) do
             begin
               oldroot:=root;
               if SpeedValue<root.SpeedValue then
                begin
                  root:=root.FRight;
                  lr:=right;
                end
               else
                begin
                  root:=root.FLeft;
                  lr:=left;
                end;
             end;
            while (root<>nil) and (root.FName^<>senc) do
             begin
               oldroot:=root;
               if senc<root.FName^ then
                begin
                  root:=root.FRight;
                  lr:=right;
                end
               else
                begin
                  root:=root.FLeft;
                  lr:=left;
                end;
             end;
            if root<>nil then
              begin
                dec(FCount);
                if root.FLeft<>nil then
                 begin
                   { Now the Node pointing to root must point to the left
                     subtree of root. The right subtree of root must be
                     connected to the right bottom of the left subtree.}
                   if lr=left then
                    oldroot.FLeft:=root.FLeft
                   else
                    oldroot.FRight:=root.FLeft;
                   if root.FRight<>nil then
                    insert_right_bottom(root.FLeft,root.FRight);
                 end
                else
                 begin
                   { There is no left subtree. So we can just replace the Node to
                     delete with the right subtree.}
                   if lr=left then
                    oldroot.FLeft:=root.FRight
                   else
                    oldroot.FRight:=root.FRight;
                 end;
              end;
            delete_from_tree:=root;
          end;

      begin
      {$ifdef compress}
        senc:=minilzw_encode(s);
      {$endif}
        SpeedValue:=GetSpeedValue(s);
        n:=FRoot;
        if assigned(FHashArray) then
         begin
           { First, check if the Node to delete directly located under
             the hasharray.}
           p:=SpeedValue mod hasharraysize;
           n:=FHashArray^[p];
           if (n<>nil) and (n.SpeedValue=SpeedValue) and
              (n.FName^=senc) then
            begin
              { The Node to delete is directly located under the
                hasharray. Make the hasharray point to the left
                subtree of the Node and place the right subtree on
                the right-bottom of the left subtree.}
              if n.FLeft<>nil then
               begin
                 FHashArray^[p]:=n.FLeft;
                 if n.FRight<>nil then
                  insert_right_bottom(n.FLeft,n.FRight);
               end
              else
               FHashArray^[p]:=n.FRight;
              delete:=n;
              dec(FCount);
              exit;
            end;
         end
        else
         begin
           { First check if the Node to delete is the root.}
           if (FRoot<>nil) and (n.SpeedValue=SpeedValue) and
              (n.FName^=senc) then
            begin
              if n.FLeft<>nil then
               begin
                 FRoot:=n.FLeft;
                 if n.FRight<>nil then
                  insert_right_bottom(n.FLeft,n.FRight);
               end
              else
               FRoot:=n.FRight;
              delete:=n;
              dec(FCount);
              exit;
            end;
         end;
        delete:=delete_from_tree(n);
      end;

    function Tdictionary.empty:boolean;
      var
        w : integer;
      begin
        if assigned(FHashArray) then
         begin
           empty:=false;
           for w:=low(FHashArray^) to high(FHashArray^) do
            if assigned(FHashArray^[w]) then
             exit;
           empty:=true;
         end
        else
         empty:=(FRoot=nil);
      end;


    procedure Tdictionary.foreach(proc2call:TNamedIndexcallback;arg:pointer);

        procedure a(p:TNamedIndexItem;arg:pointer);
        begin
          proc2call(p,arg);
          if assigned(p.FLeft) then
           a(p.FLeft,arg);
          if assigned(p.FRight) then
           a(p.FRight,arg);
        end;

      var
        i : integer;
      begin
        if assigned(FHashArray) then
         begin
           for i:=low(FHashArray^) to high(FHashArray^) do
            if assigned(FHashArray^[i]) then
             a(FHashArray^[i],arg);
         end
        else
         if assigned(FRoot) then
          a(FRoot,arg);
      end;


    procedure Tdictionary.foreach_static(proc2call:TNamedIndexStaticCallback;arg:pointer);

        procedure a(p:TNamedIndexItem;arg:pointer);
        begin
          proc2call(p,arg);
          if assigned(p.FLeft) then
           a(p.FLeft,arg);
          if assigned(p.FRight) then
           a(p.FRight,arg);
        end;

      var
        i : integer;
      begin
        if assigned(FHashArray) then
         begin
           for i:=low(FHashArray^) to high(FHashArray^) do
            if assigned(FHashArray^[i]) then
             a(FHashArray^[i],arg);
         end
        else
         if assigned(FRoot) then
          a(FRoot,arg);
      end;


    function Tdictionary.replace(oldobj,newobj:TNamedIndexItem):boolean;
      var
        hp : TNamedIndexItem;
      begin
        hp:=nil;
        Replace:=false;
        { must be the same name and hash }
        if (oldobj.FSpeedValue<>newobj.FSpeedValue) or
           (oldobj.FName^<>newobj.FName^) then
         exit;
        { copy tree info }
        newobj.FLeft:=oldobj.FLeft;
        newobj.FRight:=oldobj.FRight;
        { update treeroot }
        if assigned(FHashArray) then
         begin
           hp:=FHashArray^[newobj.FSpeedValue mod hasharraysize];
           if hp=oldobj then
            begin
              FHashArray^[newobj.FSpeedValue mod hasharraysize]:=newobj;
              hp:=nil;
            end;
         end
        else
         begin
           hp:=FRoot;
           if hp=oldobj then
            begin
              FRoot:=newobj;
              hp:=nil;
            end;
         end;
        { update parent entry }
        while assigned(hp) do
         begin
           { is the node to replace the left or right, then
             update this node and stop }
           if hp.FLeft=oldobj then
            begin
              hp.FLeft:=newobj;
              break;
            end;
           if hp.FRight=oldobj then
            begin
              hp.FRight:=newobj;
              break;
            end;
           { First check SpeedValue, to allow a fast insert }
           if hp.SpeedValue>oldobj.SpeedValue then
            hp:=hp.FRight
           else
            if hp.SpeedValue<oldobj.SpeedValue then
             hp:=hp.FLeft
           else
            begin
              if (hp.FName^=oldobj.FName^) then
               begin
                 { this can never happend, return error }
                 exit;
               end
              else
               if oldobj.FName^>hp.FName^ then
                hp:=hp.FLeft
              else
               hp:=hp.FRight;
            end;
         end;
        Replace:=true;
      end;


    function Tdictionary.insert(obj:TNamedIndexItem):TNamedIndexItem;
      begin
        inc(FCount);
        if assigned(FHashArray) then
          insert:=insertNode(obj,FHashArray^[obj.SpeedValue mod hasharraysize])
        else
          insert:=insertNode(obj,FRoot);
      end;


    function tdictionary.insertNode(NewNode:TNamedIndexItem;var currNode:TNamedIndexItem):TNamedIndexItem;
      begin
        if currNode=nil then
         begin
           currNode:=NewNode;
           insertNode:=NewNode;
         end
        { First check SpeedValue, to allow a fast insert }
        else
         if currNode.SpeedValue>NewNode.SpeedValue then
          insertNode:=insertNode(NewNode,currNode.FRight)
        else
         if currNode.SpeedValue<NewNode.SpeedValue then
          insertNode:=insertNode(NewNode,currNode.FLeft)
        else
         begin
           if currNode.FName^>NewNode.FName^ then
            insertNode:=insertNode(NewNode,currNode.FRight)
           else
            if currNode.FName^<NewNode.FName^ then
             insertNode:=insertNode(NewNode,currNode.FLeft)
           else
            begin
              if (delete_doubles) and
                 assigned(currNode) then
                begin
                  NewNode.FLeft:=currNode.FLeft;
                  NewNode.FRight:=currNode.FRight;
                  if delete_doubles then
                    begin
                      currnode.FLeft:=nil;
                      currnode.FRight:=nil;
                      currnode.free;
                    end;
                  currNode:=NewNode;
                  insertNode:=NewNode;
                end
              else
               insertNode:=currNode;
             end;
         end;
      end;


    procedure tdictionary.inserttree(currtree,currroot:TNamedIndexItem);
      begin
        if assigned(currtree) then
         begin
           inserttree(currtree.FLeft,currroot);
           inserttree(currtree.FRight,currroot);
           currtree.FRight:=nil;
           currtree.FLeft:=nil;
           insertNode(currtree,currroot);
         end;
      end;


    function tdictionary.rename(const olds,News : string):TNamedIndexItem;
      var
        spdval : cardinal;
        lasthp,
        hp,hp2,hp3 : TNamedIndexItem;
      {$ifdef compress}
        oldsenc,newsenc:string;
      {$else}
        oldsenc:string absolute olds;
        newsenc:string absolute news;
      {$endif}
      begin
      {$ifdef compress}
        oldsenc:=minilzw_encode(olds);
        newsenc:=minilzw_encode(news);
      {$endif}
        spdval:=GetSpeedValue(olds);
        if assigned(FHashArray) then
         hp:=FHashArray^[spdval mod hasharraysize]
        else
         hp:=FRoot;
        lasthp:=nil;
        while assigned(hp) do
          begin
            if spdval>hp.SpeedValue then
             begin
               lasthp:=hp;
               hp:=hp.FLeft
             end
            else
             if spdval<hp.SpeedValue then
              begin
                lasthp:=hp;
                hp:=hp.FRight
              end
            else
             begin
               if (hp.FName^=oldsenc) then
                begin
                  { Get in hp2 the replacer for the root or hasharr }
                  hp2:=hp.FLeft;
                  hp3:=hp.FRight;
                  if not assigned(hp2) then
                   begin
                     hp2:=hp.FRight;
                     hp3:=hp.FLeft;
                   end;
                  { remove entry from the tree }
                  if assigned(lasthp) then
                   begin
                     if lasthp.FLeft=hp then
                      lasthp.FLeft:=hp2
                     else
                      lasthp.FRight:=hp2;
                   end
                  else
                   begin
                     if assigned(FHashArray) then
                      FHashArray^[spdval mod hasharraysize]:=hp2
                     else
                      FRoot:=hp2;
                   end;
                  { reinsert the hp3 in the tree from hp2 }
                  inserttree(hp3,hp2);
                  { reset Node with New values }
                  hp.FLeft:=nil;
                  hp.FRight:=nil;
                  stringdispose(hp.FName);
                  hp.FName:=stringdup(newsenc);
                  hp.FSpeedValue:=GetSpeedValue(news);
                  { reinsert }
                  if assigned(FHashArray) then
                   rename:=insertNode(hp,FHashArray^[hp.SpeedValue mod hasharraysize])
                  else
                   rename:=insertNode(hp,FRoot);
                  exit;
                end
               else
                if oldsenc>hp.FName^ then
                 begin
                   lasthp:=hp;
                   hp:=hp.FLeft
                 end
                else
                 begin
                   lasthp:=hp;
                   hp:=hp.FRight;
                 end;
             end;
          end;
        result := nil;
      end;


    function Tdictionary.search(const s:string):TNamedIndexItem;

    begin
      search:=speedsearch(s,getspeedvalue(s));
    end;


    function Tdictionary.speedsearch(const s:string;SpeedValue:cardinal):TNamedIndexItem;
      var
        NewNode:TNamedIndexItem;
      {$ifdef compress}
        decn:string;
      {$endif}
      begin
        if assigned(FHashArray) then
         NewNode:=FHashArray^[SpeedValue mod hasharraysize]
        else
         NewNode:=FRoot;
        while assigned(NewNode) do
         begin
           if SpeedValue>NewNode.SpeedValue then
            NewNode:=NewNode.FLeft
           else
            if SpeedValue<NewNode.SpeedValue then
             NewNode:=NewNode.FRight
           else
            begin
            {$ifdef compress}
              decn:=minilzw_decode(newnode.fname^);
              if (decn=s) then
               begin
                 speedsearch:=NewNode;
                 exit;
               end
              else
               if s>decn then
                NewNode:=NewNode.FLeft
              else
               NewNode:=NewNode.FRight;
            {$else}
              if (NewNode.FName^=s) then
               begin
                 speedsearch:=NewNode;
                 exit;
               end
              else
               if s>NewNode.FName^ then
                NewNode:=NewNode.FLeft
              else
               NewNode:=NewNode.FRight;
            {$endif}
            end;
         end;
        speedsearch:=nil;
      end;

{****************************************************************************
                               tindexarray
 ****************************************************************************}

    constructor tindexarray.create(Agrowsize:integer);
      begin
        growsize:=Agrowsize;
        size:=0;
        count:=0;
        data:=nil;
        First:=nil;
        noclear:=false;
      end;


    destructor tindexarray.destroy;
      begin
        if assigned(data) then
          begin
             if not noclear then
              clear;
             freemem(data);
             data:=nil;
          end;
      end;


    function tindexarray.search(nr:integer):TNamedIndexItem;
      begin
        if nr<=count then
         search:=data^[nr]
        else
         search:=nil;
      end;


    procedure tindexarray.clear;
      var
        i : integer;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          begin
            data^[i].free;
            data^[i]:=nil;
          end;
        count:=0;
        First:=nil;
      end;


    procedure tindexarray.foreach(proc2call : Tnamedindexcallback;arg:pointer);
      var
        i : integer;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          proc2call(data^[i],arg);
      end;


    procedure tindexarray.foreach_static(proc2call : Tnamedindexstaticcallback;arg:pointer);
      var
        i : integer;
      begin
        for i:=1 to count do
         if assigned(data^[i]) then
          proc2call(data^[i],arg);
      end;


    procedure tindexarray.grow(gsize:integer);
      var
        osize : integer;
      begin
        osize:=size;
        inc(size,gsize);
        reallocmem(data,size*sizeof(pointer));
        fillchar(data^[osize+1],gsize*sizeof(pointer),0);
      end;


    procedure tindexarray.deleteindex(p:TNamedIndexItem);
      var
        i : integer;
      begin
        i:=p.Findexnr;
        { update counter }
        if i=count then
         dec(count);
        { update Linked List }
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i].FindexNext:=data^[p.Findexnr].FindexNext;
              break;
            end;
         end;
        if i=0 then
         First:=p.FindexNext;
        data^[p.FIndexnr]:=nil;
        { clear entry }
        p.FIndexnr:=-1;
        p.FIndexNext:=nil;
      end;


    procedure tindexarray.delete(var p:TNamedIndexItem);
      begin
        deleteindex(p);
        p.free;
        p:=nil;
      end;


    procedure tindexarray.insert(p:TNamedIndexItem);
      var
        i  : integer;
      begin
        if p.FIndexnr=-1 then
         begin
           inc(count);
           p.FIndexnr:=count;
         end;
        if p.FIndexnr>count then
         count:=p.FIndexnr;
        if count>size then
         grow(((count div growsize)+1)*growsize);
        Assert(not assigned(data^[p.FIndexnr]) or (p=data^[p.FIndexnr]));
        data^[p.FIndexnr]:=p;
        { update Linked List backward }
        i:=p.FIndexnr;
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i].FIndexNext:=p;
              break;
            end;
         end;
        if i=0 then
         First:=p;
        { update Linked List forward }
        i:=p.FIndexnr;
        while (i<=count) do
         begin
           inc(i);
           if (i<=count) and assigned(data^[i]) then
            begin
              p.FIndexNext:=data^[i];
              exit;
            end;
         end;
        if i>count then
         p.FIndexNext:=nil;
      end;


    procedure tindexarray.replace(oldp,newp:TNamedIndexItem);
      var
        i : integer;
      begin
        newp.FIndexnr:=oldp.FIndexnr;
        newp.FIndexNext:=oldp.FIndexNext;
        data^[newp.FIndexnr]:=newp;
        if First=oldp then
          First:=newp;
        { update Linked List backward }
        i:=newp.FIndexnr;
        while (i>0) do
         begin
           dec(i);
           if (i>0) and assigned(data^[i]) then
            begin
              data^[i].FIndexNext:=newp;
              break;
            end;
         end;
      end;


{****************************************************************************
                                tdynamicarray
****************************************************************************}

    constructor tdynamicarray.create(Ablocksize:integer);
      begin
        FPosn:=0;
        FPosnblock:=nil;
        FFirstblock:=nil;
        FLastblock:=nil;
        Fblocksize:=Ablocksize;
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


    function  tdynamicarray.size:integer;
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
        nblock : pdynamicblock;
      begin
        Getmem(nblock,blocksize+dynamicblockbasesize);
        if not assigned(FFirstblock) then
         begin
           FFirstblock:=nblock;
           FPosnblock:=nblock;
           nblock^.pos:=0;
         end
        else
         begin
           FLastblock^.Next:=nblock;
           nblock^.pos:=FLastblock^.pos+FLastblock^.used;
         end;
        nblock^.used:=0;
        nblock^.Next:=nil;
        fillchar(nblock^.data,blocksize,0);
        FLastblock:=nblock;
      end;


    procedure tdynamicarray.align(i:integer);
      var
        j : integer;
      begin
        j:=(FPosn mod i);
        if j<>0 then
         begin
           j:=i-j;
           if FPosnblock^.used+j>blocksize then
            begin
              dec(j,blocksize-FPosnblock^.used);
              FPosnblock^.used:=blocksize;
              grow;
              FPosnblock:=FLastblock;
            end;
           inc(FPosnblock^.used,j);
           inc(FPosn,j);
         end;
      end;


    procedure tdynamicarray.seek(i:integer);
      begin
        if (i<FPosnblock^.pos) or (i>=FPosnblock^.pos+blocksize) then
         begin
           { set FPosnblock correct if the size is bigger then
             the current block }
           if FPosnblock^.pos>i then
            FPosnblock:=FFirstblock;
           while assigned(FPosnblock) do
            begin
              if FPosnblock^.pos+blocksize>i then
               break;
              FPosnblock:=FPosnblock^.Next;
            end;
           { not found ? then increase blocks }
           if not assigned(FPosnblock) then
            begin
              repeat
                { the current FLastblock is now also fully used }
                FLastblock^.used:=blocksize;
                grow;
                FPosnblock:=FLastblock;
              until FPosnblock^.pos+blocksize>=i;
            end;
         end;
        FPosn:=i;
        if FPosn mod blocksize>FPosnblock^.used then
         FPosnblock^.used:=FPosn mod blocksize;
      end;


    procedure tdynamicarray.write(const d;len:integer);
      var
        p : pchar;
        i,j : integer;
      begin
        p:=pchar(@d);
        while (len>0) do
         begin
           i:=FPosn mod blocksize;
           if i+len>=blocksize then
            begin
              j:=blocksize-i;
              move(p^,FPosnblock^.data[i],j);
              inc(p,j);
              inc(FPosn,j);
              dec(len,j);
              FPosnblock^.used:=blocksize;
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
              i:=FPosn mod blocksize;
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


    function tdynamicarray.read(var d;len:integer):integer;
      var
        p : pchar;
        i,j,res : integer;
      begin
        res:=0;
        p:=pchar(@d);
        while (len>0) do
         begin
           i:=FPosn mod blocksize;
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


    procedure tdynamicarray.readstream(f:TCStream;maxlen:longint);
      var
        i,left : integer;
      begin
        if maxlen=-1 then
         maxlen:=maxlongint;
        repeat
          left:=blocksize-FPosnblock^.used;
          if left>maxlen then
           left:=maxlen;
          i:=f.Read(FPosnblock^.data[FPosnblock^.used],left);
          dec(maxlen,i);
          inc(FPosnblock^.used,i);
          if FPosnblock^.used=blocksize then
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


end.
