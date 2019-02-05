{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 1999-2019 by the Free Pascal development team

    This file provides alternative pluggable sorting algorithms,
    which can be used instead of the default QuickSort implementation
    in unit SortBase.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit SortAlgs;

{$MODE objfpc}

interface

uses
  SortBase;

{
                       HeapSort

  Average performance: O(n log n)
    Worst performance: O(n log n)
     Extra memory use: O(1)
               Stable: no
     Additional notes: Usually slower in practice, compared to QuickSort (in the
                       average case), but has a much better worst-case
                       performance of O(n log n) (versus O(n*n) for QuickSort).
                       Can be used instead of QuickSort where the risk of
                       QuickSort's worst case scenario is not acceptable - e.g.
                       high risk applications, security-conscious applications
                       or applications with hard real-time requirements.

                       On systems with small or no data caches it might perform
                       better or comparable to QuickSort even in the average
                       case, so might be a good general purpose choice for
                       embedded systems as well. It's O(1) extra memory use and
                       the fact it's not recursive also makes it a good
                       candidate for embedded use.
}

procedure HeapSort_PtrList_NoContext(
                ItemPtrs: PPointer;
                ItemCount: SizeUInt;
                Comparer: TListSortComparer_NoContext);
procedure HeapSort_PtrList_Context(
                ItemPtrs: PPointer;
                ItemCount: SizeUInt;
                Comparer: TListSortComparer_Context;
                Context: Pointer);
procedure HeapSort_ItemList_Context(
                Items: Pointer;
                ItemCount, ItemSize: SizeUInt;
                Comparer: TListSortComparer_Context;
                Context: Pointer);
procedure HeapSort_ItemList_CustomItemExchanger_Context(
                Items: Pointer;
                ItemCount, ItemSize: SizeUInt;
                Comparer: TListSortComparer_Context;
                Exchanger: TListSortCustomItemExchanger_Context;
                Context: Pointer);

const
  HeapSort: TSortingAlgorithm = (
    PtrListSorter_NoContextComparer: @HeapSort_PtrList_NoContext;
    PtrListSorter_ContextComparer: @HeapSort_PtrList_Context;
    ItemListSorter_ContextComparer: @HeapSort_ItemList_Context;
    ItemListSorter_CustomItemExchanger_ContextComparer: @HeapSort_ItemList_CustomItemExchanger_Context;
  );

implementation

{$GOTO on}

function HeapSort_Parent(i: SizeUInt): SizeUInt; inline;
begin
  Result := (i - 1) div 2;
end;

function HeapSort_Left(i: SizeUInt): SizeUInt; inline;
begin
  Result := 2*i + 1;
end;

function HeapSort_Right(i: SizeUInt): SizeUInt; inline;
begin
  Result := 2*i + 2;
end;

procedure HeapSort_PtrList_NoContext(
                ItemPtrs: PPointer;
                ItemCount: SizeUInt;
                Comparer: TListSortComparer_NoContext);
var
  HeapSize: SizeUInt;

  procedure Heapify(I: SizeUInt);
  label
    again;
  var
    L, R, Largest: SizeUInt;
    Q: Pointer;
  begin
again:
    L := HeapSort_Left(I);
    R := HeapSort_Right(I);
    if (L < HeapSize) and (Comparer(ItemPtrs[L], ItemPtrs[I]) > 0) then
      Largest := L
    else
      Largest := I;
    if (R < HeapSize) and (Comparer(ItemPtrs[R], ItemPtrs[Largest]) > 0) then
      Largest := R;
    if Largest <> I then
    begin
      Q := ItemPtrs[I];
      ItemPtrs[I] := ItemPtrs[Largest];
      ItemPtrs[Largest] := Q;
      { we use goto instead of tail recursion }
      I := Largest;
      goto again;
    end;
  end;

var
  I: SizeUInt;
  Q: Pointer;
begin
  if not Assigned(ItemPtrs) or (ItemCount < 2) then
    exit;
  HeapSize := ItemCount;
  for I := HeapSort_Parent(ItemCount - 1) downto 0 do
    Heapify(I);
  for I := ItemCount - 1 downto 1 do
  begin
    Q := ItemPtrs[0];
    ItemPtrs[0] := ItemPtrs[I];
    ItemPtrs[I] := Q;
    Dec(HeapSize);
    Heapify(0);
  end;
end;

procedure HeapSort_PtrList_Context(
                ItemPtrs: PPointer;
                ItemCount: SizeUInt;
                Comparer: TListSortComparer_Context;
                Context: Pointer);
var
  HeapSize: SizeUInt;

  procedure Heapify(I: SizeUInt);
  label
    again;
  var
    L, R, Largest: SizeUInt;
    Q: Pointer;
  begin
again:
    L := HeapSort_Left(I);
    R := HeapSort_Right(I);
    if (L < HeapSize) and (Comparer(ItemPtrs[L], ItemPtrs[I], Context) > 0) then
      Largest := L
    else
      Largest := I;
    if (R < HeapSize) and (Comparer(ItemPtrs[R], ItemPtrs[Largest], Context) > 0) then
      Largest := R;
    if Largest <> I then
    begin
      Q := ItemPtrs[I];
      ItemPtrs[I] := ItemPtrs[Largest];
      ItemPtrs[Largest] := Q;
      { we use goto instead of tail recursion }
      I := Largest;
      goto again;
    end;
  end;

var
  I: SizeUInt;
  Q: Pointer;
begin
  if not Assigned(ItemPtrs) or (ItemCount < 2) then
    exit;
  HeapSize := ItemCount;
  for I := HeapSort_Parent(ItemCount - 1) downto 0 do
    Heapify(I);
  for I := ItemCount - 1 downto 1 do
  begin
    Q := ItemPtrs[0];
    ItemPtrs[0] := ItemPtrs[I];
    ItemPtrs[I] := Q;
    Dec(HeapSize);
    Heapify(0);
  end;
end;

procedure HeapSort_ItemList_Context(
                Items: Pointer;
                ItemCount, ItemSize: SizeUInt;
                Comparer: TListSortComparer_Context;
                Context: Pointer);
var
  HeapSize: SizeUInt;
  TempBuf: Pointer;

  procedure Heapify(I: SizeUInt);
  label
    again;
  var
    L, R, Largest: SizeUInt;
  begin
again:
    L := HeapSort_Left(I);
    R := HeapSort_Right(I);
    if (L < HeapSize) and (Comparer(Items + ItemSize*L, Items + ItemSize*I, Context) > 0) then
      Largest := L
    else
      Largest := I;
    if (R < HeapSize) and (Comparer(Items + ItemSize*R, Items + ItemSize*Largest, Context) > 0) then
      Largest := R;
    if Largest <> I then
    begin
      Move((Items + ItemSize*I)^, TempBuf^, ItemSize);
      Move((Items + ItemSize*Largest)^, (Items + ItemSize*I)^, ItemSize);
      Move(TempBuf^, (Items + ItemSize*Largest)^, ItemSize);
      { we use goto instead of tail recursion }
      I := Largest;
      goto again;
    end;
  end;

var
  I: SizeUInt;
begin
  if not Assigned(Items) or (ItemCount < 2) or (ItemSize < 1) then
    exit;

  GetMem(TempBuf, ItemSize);
  try
    HeapSize := ItemCount;
    for I := HeapSort_Parent(ItemCount - 1) downto 0 do
      Heapify(I);
    for I := ItemCount - 1 downto 1 do
    begin
      Move((Items + ItemSize*0)^, TempBuf^, ItemSize);
      Move((Items + ItemSize*I)^, (Items + ItemSize*0)^, ItemSize);
      Move(TempBuf^, (Items + ItemSize*I)^, ItemSize);
      Dec(HeapSize);
      Heapify(0);
    end;
  finally
    FreeMem(TempBuf, ItemSize);
  end;
end;

procedure HeapSort_ItemList_CustomItemExchanger_Context(
                Items: Pointer;
                ItemCount, ItemSize: SizeUInt;
                Comparer: TListSortComparer_Context;
                Exchanger: TListSortCustomItemExchanger_Context;
                Context: Pointer);
var
  HeapSize: SizeUInt;

  procedure Heapify(I: SizeUInt);
  label
    again;
  var
    L, R, Largest: SizeUInt;
  begin
again:
    L := HeapSort_Left(I);
    R := HeapSort_Right(I);
    if (L < HeapSize) and (Comparer(Items + ItemSize*L, Items + ItemSize*I, Context) > 0) then
      Largest := L
    else
      Largest := I;
    if (R < HeapSize) and (Comparer(Items + ItemSize*R, Items + ItemSize*Largest, Context) > 0) then
      Largest := R;
    if Largest <> I then
    begin
      Exchanger(Items + ItemSize*I, Items + ItemSize*Largest, Context);
      { we use goto instead of tail recursion }
      I := Largest;
      goto again;
    end;
  end;

var
  I: SizeUInt;
begin
  if not Assigned(Items) or (ItemCount < 2) or (ItemSize < 1) then
    exit;

  HeapSize := ItemCount;
  for I := HeapSort_Parent(ItemCount - 1) downto 0 do
    Heapify(I);
  for I := ItemCount - 1 downto 1 do
  begin
    Exchanger(Items + ItemSize*0, Items + ItemSize*I, Context);
    Dec(HeapSize);
    Heapify(0);
  end;
end;

end.
