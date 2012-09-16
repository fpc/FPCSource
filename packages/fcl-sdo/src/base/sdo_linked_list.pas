{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements a doubly linked list.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$INCLUDE sdo_global.inc}
{$RANGECHECKS OFF}
unit sdo_linked_list;

interface
uses
  SysUtils, Classes, Types,
  sdo_types;

type

  EListException = class(Exception);

  PLinkedNode = ^TLinkedNode;
  TLinkedNode = packed record
    Previous :PLinkedNode;
    Next : PLinkedNode;
    Data : array[0..0] of Byte;
  end;

  TMoveType = ( mtRelative, mtAbsolute );
  TLinkedListBookmark = TByteDynArray;//type TStringBufferType;
  ILinkedListCursor = interface
    ['{10586C3A-E8C9-4E68-A338-1605ECEB5C31}']
    function IsPosValid() : Boolean;
    function Eof() : Boolean;
    function Bof() : Boolean;
    function GetCurrent() : PLinkedNode;
    function MoveNext() : Boolean;
    function MovePrevious() : Boolean;
    function MoveFirst() : Boolean;
    function MoveLast() : Boolean;
    function GetPosition({the position is zero-based}) : PtrInt;
    function MoveTo(const APosition{the position is zero-based} : PtrInt) : Boolean;
    procedure Reset();
    function GetBookmark() : TLinkedListBookmark;
    function GotoBookmark(const ABookmark : TLinkedListBookmark) : Boolean;
    //function Clone() : ILinkedListCursor;
  end;

  TNotifyReason = ( nrDelete, nrNew, nrDestroy );
  ILinkedListNotify = interface
    ['{D53E46B0-8709-4662-B91E-0D746828A27C}']
    procedure Notify(const AReason : TNotifyReason; const ANode : PLinkedNode);
  end;

  TDoubleLinkedList = class
  private
    FGeneration : PtrUInt;
    FCount : PtrInt;
    FDataSize : PtrInt;
    FItemSize : PtrInt;
    FFirst : PLinkedNode;
    FLast : PLinkedNode;
    FNotifyList : IInterfaceList;
  private
    function AllocateBuffer() : PLinkedNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Notify(const AReason : TNotifyReason; const ANode : PLinkedNode);
  public
    constructor Create(const ADataSize : PtrInt);
    destructor Destroy();override;
    procedure RegisterForNotification(const AListener : ILinkedListNotify);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure UnregisterForNotification(const AListener : ILinkedListNotify);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure FreeBuffer(const AItem : PLinkedNode);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Clear();
    function GetLength() : PtrInt;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function IsEmpty() : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetFirst() : PLinkedNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetLast() : PLinkedNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function InsertFirst() : PLinkedNode;
    function Append() : PLinkedNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function InsertBefore(const AItem : PLinkedNode) : PLinkedNode;
    function InsertAfter(const AItem : PLinkedNode) : PLinkedNode;
    //remove links, do not delete the buffer
    procedure Remove(const AItem : PLinkedNode);
    //remove links, delete the buffer
    procedure Delete(const AItem : PLinkedNode);{$IFDEF USE_INLINE}inline;{$ENDIF}
    property DataSize : PtrInt read FDataSize;
    property Generation : PtrUInt read FGeneration;
  end;


  function CreateIterator(AList : TDoubleLinkedList) : ILinkedListCursor;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetData(const ANode : PLinkedNode) : Pointer ;{$IFDEF USE_INLINE}inline;{$ENDIF}

implementation

type
  TLinkedListBookmarkRecord = packed record
    _Data : TDoubleLinkedList;
    _Node : PLinkedNode;
    _Generation : PtrUInt;
    _Position : PtrInt;
  end;

const
  BOOKMARK_SIZE = SizeOf(TLinkedListBookmarkRecord);

type

  TLinkedListIterator = class(
    TInterfacedObject,
    IInterface,
    ILinkedListNotify,
    ILinkedListCursor
  )
  private
    FData : TDoubleLinkedList;
    FCurrent : PLinkedNode;
    FPosition : PtrInt;
  protected
    // -- IDoubleLinkedList
    function IsPosValid() : Boolean;
    function Eof() : Boolean;
    function Bof() : Boolean;
    function GetCurrent() : PLinkedNode;
    function MoveNext() : Boolean;
    function MovePrevious() : Boolean;
    function MoveFirst() : Boolean;
    function MoveLast() : Boolean;
    function MoveTo(const APosition{the position is zero-based} : PtrInt) : Boolean;
    procedure Reset();
    function GetBookmark() : TLinkedListBookmark;
    function GotoBookmark(const ABookmark : TLinkedListBookmark) : Boolean;
    function GetPosition({the position is zero-based}) : PtrInt;
    function Clone() : ILinkedListCursor;
    // -- ILinkedListNotify
    procedure Notify(const AReason : TNotifyReason; const ANode : PLinkedNode);
  public
    constructor Create(AData : TDoubleLinkedList);
    destructor Destroy();override;
  end;

function CreateIterator(AList : TDoubleLinkedList) : ILinkedListCursor;
begin
  Result := TLinkedListIterator.Create(AList) as ILinkedListCursor;
end;

function GetData(const ANode : PLinkedNode) : Pointer ;
begin
  Result := @(ANode^.Data[0]);
end;

{ TSDOLinkedList }

function TDoubleLinkedList.InsertBefore(const AItem : PLinkedNode): PLinkedNode;
begin
  if Assigned(AItem) then begin
    Result := AllocateBuffer();
    Result^.Previous := AItem^.Previous;
    if Assigned(AItem^.Previous) then
      AItem^.Previous^.Next := Result
    else
      FFirst := Result;
    Result^.Next := AItem;
    AItem^.Previous := Result;
    Inc(FCount);
    Notify(nrNew,Result);
  end else begin
    raise EListException.Create('Invalid parameter.');
  end;
end;

procedure TDoubleLinkedList.Clear();
begin
  while ( FCount > 0 ) do begin
    Delete(GetFirst());
  end;
end;

constructor TDoubleLinkedList.Create(const ADataSize : PtrInt);
begin
  if ( ADataSize < 1 ) then
    raise EListException.CreateFmt('Invalid Data size : %d.',[ADataSize]);
  inherited Create();
  FDataSize := ADataSize;
  FItemSize :=
    SizeOf(PLinkedNode) + //Previous
    SizeOf(PLinkedNode) + //Next
    FDataSize;
  FNotifyList := TInterfaceList.Create() as IInterfaceList;
end;

procedure TDoubleLinkedList.Delete(const AItem : PLinkedNode);
begin
  Remove(AItem);
  FreeBuffer(AItem);
end;

destructor TDoubleLinkedList.Destroy();
begin
  if ( FNotifyList <> nil ) then begin
    Notify(nrDestroy,nil);
    FNotifyList.Clear();
  end;
  Clear();
  FNotifyList := nil;
  inherited;
end;

function TDoubleLinkedList.GetFirst() : PLinkedNode;
begin
  if ( GetLength() > 0 ) then
    Result := FFirst
  else
    raise EListException.Create('The list is empty.');
end;

function TDoubleLinkedList.GetLength(): PtrInt;
begin
  Result := FCount;
end;

function TDoubleLinkedList.IsEmpty() : Boolean;
begin
  Result := ( FCount = 0 );
end;

function TDoubleLinkedList.GetLast() : PLinkedNode;
begin
  if ( GetLength() > 0 ) then
    Result := FLast
  else
    raise EListException.Create('The list is empty.');
end;

procedure TDoubleLinkedList.Remove(const AItem : PLinkedNode);
begin
  if ( AItem = nil ) then
    raise EListException.Create('Invalid parameter.');
  if Assigned(AItem^.Previous) then begin
    AItem^.Previous^.Next := AItem^.Next;
  end else begin
    FFirst := AItem^.Next;
    if Assigned(FFirst) then
      FFirst^.Previous := nil;
  end;

  if Assigned(AItem^.Next) then begin
    AItem^.Next^.Previous := AItem^.Previous;
  end else begin
    FLast := AItem^.Previous;
    if Assigned(FLast) then
      FLast^.Next := nil;
  end;
  Dec(FCount);
  Notify(nrDelete,AItem);
end;

{ TLinkedListIterator }

function TLinkedListIterator.Bof() : Boolean;
begin
  //Result := FData.IsEmpty() or ( FCurrent = FData.GetFirst() );
  Result := FData.IsEmpty() or ( FPosition = 0 );
end;

function TLinkedListIterator.Clone() : ILinkedListCursor;
var
  bm : TLinkedListBookmark;
begin
  Result := TLinkedListIterator.Create(FData);
  bm := GetBookmark();
  Result.GotoBookmark(bm);
end;

constructor TLinkedListIterator.Create(AData: TDoubleLinkedList);
var
  selfNotify : ILinkedListNotify;
begin
  if ( AData = nil ) then
    raise EListException.Create('Invalid parameter.');
  inherited Create();
  FData := AData;
  selfNotify := Self as ILinkedListNotify;
  FData.RegisterForNotification(selfNotify);
  Reset();
end;

destructor TLinkedListIterator.Destroy();
var
  selfNotify : ILinkedListNotify;
begin
  if ( FData <> nil ) then begin
    selfNotify := Self as ILinkedListNotify;
    FData.UnregisterForNotification(selfNotify);
  end;
  FData := nil;
  FCurrent := nil;
  inherited;
end;

function TLinkedListIterator.Eof() : Boolean;
begin
  Result := FData.IsEmpty() or (FPosition = FData.GetLength());
end;

function TLinkedListIterator.GetBookmark(): TLinkedListBookmark;
var
  rec : TLinkedListBookmarkRecord;
begin
  rec._Data := FData;
  rec._Node := FCurrent;
  rec._Generation := FData.Generation;
  rec._Position := FPosition;
  SetLength(Result,BOOKMARK_SIZE);
  Move(rec,Pointer(Result)^,BOOKMARK_SIZE);
end;

function TLinkedListIterator.GetCurrent() : PLinkedNode;
begin
  Result := FCurrent;
end;

function TLinkedListIterator.GetPosition() : PtrInt;
begin
  if FData.IsEmpty() then begin
    Result := FPosition
  end else begin
    if (FPosition >= FData.GetLength()) then
      Result := (FPosition - 1)
    else
      Result := FPosition;
  end;
end;

function TLinkedListIterator.GotoBookmark(const ABookmark: TLinkedListBookmark): Boolean;
var
  rec : TLinkedListBookmarkRecord;
begin
  Result := False;
  if ( Length(ABookmark) = BOOKMARK_SIZE ) then begin
    Move(Pointer(ABookmark)^,rec,BOOKMARK_SIZE);
    if ( rec._Data = FData ) and ( rec._Generation = FData.Generation ) then begin
      FCurrent := rec._Node;
      FPosition := rec._Position;
      Result := True;
    end;
  end;
end;

function TLinkedListIterator.IsPosValid() : Boolean;
begin
  Result := Assigned(FCurrent) and ( not FData.IsEmpty() );
end;

function TLinkedListIterator.MoveFirst() : Boolean;
begin
  Reset();
  Result := MoveNext();
end;

function TLinkedListIterator.MoveLast() : Boolean;
begin
  if FData.IsEmpty() then begin
    Result := False;
  end else begin
    FCurrent := FData.GetLast();
    Result := True;
  end;
  if Result then
    FPosition := FData.GetLength()// - 1;
end;

function TLinkedListIterator.MoveNext() : Boolean;
begin
  if ( FCurrent = nil ) then begin
    if FData.IsEmpty() then begin
      Result := False;
    end else begin
      FCurrent := FData.GetFirst();
      Result := True;
    end;
  end else begin
    if ( FCurrent = FData.GetLast() ) then begin
      FPosition := FData.GetLength();
      Result := False;
    end else begin
      FCurrent := FCurrent^.Next;
      Result := True;
    end;
  end;
  if Result then
    Inc(FPosition);
end;

function TLinkedListIterator.MovePrevious() : Boolean;
begin
  if ( FCurrent = nil ) or
     FData.IsEmpty() or
     ( FCurrent = FData.GetFirst() )
  then begin
    Result := False;
  end else begin
    FCurrent := FCurrent^.Previous;
    Result := True;
  end;
  if Result then begin
    if (FPosition = FData.GetLength()) then
      Dec(FPosition);
    Dec(FPosition);
  end;
end;

function TLinkedListIterator.MoveTo(const APosition: PtrInt): Boolean;
var
  oldPos : TLinkedListBookmark;
  d1, d2, idx : PtrInt;
  neg : Boolean;
begin
  Result := False;
  if (APosition < 0) then
    Exit;
  if FData.IsEmpty() then
    Exit;
  if (APosition < FData.GetLength()) then begin
    d1 := APosition - 0;
    d2 := APosition - GetPosition();
    neg := ( d2 < 0 );
    if neg then
      d2 := -d2;
    if ( d1 <= d2 ) then begin
      idx := 1
    end else begin
      d1 := d2;
      idx := 2;
    end;
    d2 := ( Pred(FData.GetLength()) - APosition );
    if ( d2 < d1 ) then begin
      d1 := d2;
      idx := 3;
    end;
    if ( d1 = 0 ) and ( idx = 2 ) then begin
      Result := True;
    end else begin
      oldPos := GetBookmark();
      try
        case idx of
          1 :
            begin
              if MoveFirst() then begin
                while ( d1 > 0 ) and MoveNext() do begin
                  Dec(d1);
                end;
              end;
            end;
          2 :
            begin
              if neg then begin
                while ( d1 > 0 ) and MovePrevious() do begin
                  Dec(d1);
                end;
              end else begin
                while ( d1 > 0 ) and MoveNext() do begin
                  Dec(d1);
                end;
              end;
            end;
          3 :
            begin
              if MoveLast() then begin
                while ( d1 > 0 ) and MovePrevious() do begin
                  Dec(d1);
                end;
              end;
            end;
        end;
        Result := ( d1 = 0 );
      except
        GotoBookmark(oldPos);
        raise;
      end;
    end;
  end else if (APosition = FData.GetLength()) and (FPosition < APosition ) then begin
    Inc(FPosition);
  end;
end;

procedure TLinkedListIterator.Notify(const AReason: TNotifyReason; const ANode: PLinkedNode);
begin
  Reset();
  if ( AReason = nrDestroy ) then
    FData := nil;
end;

procedure TLinkedListIterator.Reset();
begin
  FCurrent := nil;
  FPosition := -1;
end;

function TDoubleLinkedList.InsertFirst() : PLinkedNode;
begin
  if Assigned(FFirst) then begin
    Result := InsertBefore(FFirst);
  end else begin
    Result := AllocateBuffer();
    FFirst := Result;
    FLast := FFirst;
    Inc(FCount);
    Notify(nrNew,FFirst);
  end;
end;

function TDoubleLinkedList.AllocateBuffer() : PLinkedNode;
begin
  Inc(FGeneration);
  GetMem(Result,FItemSize);
  FillChar(Result^,FItemSize,#0);
end;

function TDoubleLinkedList.InsertAfter(const AItem: PLinkedNode): PLinkedNode;
begin
  if Assigned(AItem) then begin
    Result := AllocateBuffer();
    Result^.Previous := AItem;
    if Assigned(AItem^.Next) then
      AItem^.Next^.Previous := Result
    else
      FLast := Result;
    Result^.Next := AItem^.Next;
    AItem^.Next := Result;
    Inc(FCount);
    Notify(nrNew,Result);
  end else begin
    raise EListException.Create('Invalid parameter.');
  end;
end;

procedure TDoubleLinkedList.FreeBuffer(const AItem: PLinkedNode);
begin
  Inc(FGeneration);
  FreeMem(AItem,FItemSize);
end;

function TDoubleLinkedList.Append() : PLinkedNode;
begin
  if IsEmpty() then
    Result := InsertFirst()
  else
    Result := InsertAfter(GetLast());
end;

procedure TDoubleLinkedList.RegisterForNotification(const AListener: ILinkedListNotify);
begin
  if ( FNotifyList.IndexOf(AListener) = -1 ) then
    FNotifyList.Add(AListener);
end;

procedure TDoubleLinkedList.UnregisterForNotification(const AListener: ILinkedListNotify);
begin
  FNotifyList.Remove(AListener);
end;

procedure TDoubleLinkedList.Notify(
  const AReason : TNotifyReason;
  const ANode : PLinkedNode
);
var
  c, i : PtrInt;
  ntfy : ILinkedListNotify;
begin
  FNotifyList.Lock();
  try
    c := FNotifyList.Count;
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        ntfy := FNotifyList[i] as ILinkedListNotify;
        ntfy.Notify(AReason,ANode);
      end;
    end;
  finally
    FNotifyList.Unlock();
  end;
end;

end.
