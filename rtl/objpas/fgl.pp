{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Florian Klaempfl

    It contains the Free Pascal generics library

    member of the Free Pascal development team

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

   type
     { TFPList class }
     generic TGList<TG> = class(TObject)
     type
       PTGList = ^TPointerList;
       TTGList = array[0..MaxListSize - 1] of TG;
       TListSortCompare = function (Item1, Item2: TG): Integer;
       TListCallback = procedure(data,arg: TG) of object;
       TListStaticCallback = procedure(data,arg: TG);
     private
       FList: PTGList;
       FCount: Integer;
       FCapacity: Integer;
     protected
       function Get(Index: Integer): TG; inline;
       procedure Put(Index: Integer; Item: TG); inline;
       procedure SetCapacity(NewCapacity: Integer);
       procedure SetCount(NewCount: Integer);
       Procedure RaiseIndexError(Index : Integer);
     public
       destructor Destroy; override;
       function Add(const Item: TG): Integer; inline;
       procedure Clear;
       procedure Delete(Index: Integer); inline;
       class procedure Error(const Msg: string; Data: PtrInt);
       procedure Exchange(Index1, Index2: Integer);
       function Expand: TGList; inline;
       function Extract(const item: TG): TG;
       function First: TG;
       function IndexOf(const Item: TG): Integer;
       procedure Insert(Index: Integer; Item: TG); inline;
       function Last: TG;
       procedure Move(CurIndex, NewIndex: Integer);
       procedure Assign(Obj:TGList);
       function Remove(const Item: TG): Integer;
       procedure Pack;
       procedure Sort(Compare: TListSortCompare);
       procedure ForEachCall(proc2call:TListCallback;arg:pointer);
       procedure ForEachCall(proc2call:TListStaticCallback;arg:pointer);
       property Capacity: Integer read FCapacity write SetCapacity;
       property Count: Integer read FCount write SetCount;
       property Items[Index: Integer]: TG read Get write Put; default;
       property List: PTGList read FList;
     end;

  implementation

{****************************************************************************}
{*                           TGList                                        *}
{****************************************************************************}

    procedure TGList.RaiseIndexError(Index : Integer);
      begin
        Error(SListIndexError, Index);
      end;


    function TGList.Get(Index: Integer): Pointer; inline;
      begin
        If (Index < 0) or (Index >= FCount) then
          RaiseIndexError(Index);
        Result:=FList^[Index];
      end;


    procedure TGList.Put(Index: Integer; Item: Pointer); inline;
      begin
        if (Index < 0) or (Index >= FCount) then
          RaiseIndexError(Index);
        Flist^[Index] := Item;
      end;


    function TGList.Extract(item: Pointer): Pointer;
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


    procedure TGList.SetCapacity(NewCapacity: Integer);
      begin
        If (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
           Error (SListCapacityError, NewCapacity);
        if NewCapacity = FCapacity then
          exit;
        ReallocMem(FList, SizeOf(Pointer)*NewCapacity);
        FCapacity := NewCapacity;
      end;


    procedure TGList.SetCount(NewCount: Integer);
      Const
        // Ratio of Pointer and Word Size.
        WordRatio = SizeOf(TG) Div SizeOf(Word);

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


    destructor TGList.Destroy;
      begin
        Self.Clear;
        inherited Destroy;
      end;


    function TGList.Add(Item: Pointer): Integer; inline;
      begin
        if FCount = FCapacity then
          Self.Expand;
        FList^[FCount] := Item;
        Result := FCount;
        FCount := FCount + 1;
      end;


    procedure TGList.Clear;
      begin
        if Assigned(FList) then
        begin
          SetCount(0);
          SetCapacity(0);
          FList := nil;
        end;
      end;


    procedure TGList.Delete(Index: Integer); inline;
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


    class procedure TGList.Error(const Msg: string; Data: PtrInt);
      begin
        Raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
      end;


    procedure TGList.Exchange(Index1, Index2: Integer);
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


    function TGList.Expand: TGList; inline;
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


    function TGList.First: Pointer;
      begin
        If FCount = 0 then
          Result := Nil
        else
          Result := Items[0];
      end;


    function TGList.IndexOf(Item: Pointer): Integer;
      begin
        Result := 0;
        while(Result < FCount) and (Flist^[Result] <> Item) do Result := Result + 1;
        If Result = FCount  then Result := -1;
      end;


    procedure TGList.Insert(Index: Integer; Item: Pointer); inline;
      begin
        if (Index < 0) or (Index > FCount )then
          Error(SlistIndexError, Index);
        iF FCount = FCapacity then Self.Expand;
        if Index<FCount then
          System.Move(Flist^[Index], Flist^[Index+1], (FCount - Index) * SizeOf(Pointer));
        FList^[Index] := Item;
        FCount := FCount + 1;
      end;


    function TGList.Last: Pointer;
      begin
      { Wouldn't it be better to return nil if the count is zero ?}
        If FCount = 0 then
          Result := nil
        else
          Result := Items[FCount - 1];
      end;


    procedure TGList.Move(CurIndex, NewIndex: Integer);
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


    function TGList.Remove(Item: Pointer): Integer;
      begin
        Result := IndexOf(Item);
        If Result <> -1 then
          Self.Delete(Result);
      end;


    procedure TGList.Pack;
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

    procedure TGList.Sort(Compare: TListSortCompare);
      begin
        if Not Assigned(FList) or (FCount < 2) then exit;
        QuickSort(Flist, 0, FCount-1, Compare);
      end;


    procedure TGList.Assign(Obj: TGList);
      var
        i: Integer;
      begin
        Clear;
        for I := 0 to Obj.Count - 1 do
          Add(Obj[i]);
      end;


    procedure TGList.ForEachCall(proc2call:TListCallback;arg:pointer);
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


    procedure TGList.ForEachCall(proc2call:TListStaticCallback;arg:pointer);
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

end.
