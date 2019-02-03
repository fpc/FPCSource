{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 1999-2019 by the Free Pascal development team

    This file provides the base for the pluggable sorting algorithm
    support. It also provides a default QuickSort implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit sortbase;

{$MODE objfpc}

interface

type
  TListSortComparer_NoContext = function(Item1, Item2: Pointer): Integer;
  TPtrListSorter_NoContext = procedure(ItemPtrs: PPointer; ItemCount: SizeUInt; Comparer: TListSortComparer_NoContext);
  TItemListSorter_NoContext = procedure(Items: Pointer; ItemCount, ItemSize: SizeUInt; Comparer: TListSortComparer_NoContext);

  TListSortComparer_Context = function(Item1, Item2, Context: Pointer): Integer;
  TListSortCustomItemExchanger_Context = procedure(Item1, Item2, Context: Pointer);
  TPtrListSorter_Context = procedure(ItemPtrs: PPointer; ItemCount: SizeUInt; Comparer: TListSortComparer_Context; Context: Pointer);
  TItemListSorter_Context = procedure(Items: Pointer; ItemCount, ItemSize: SizeUInt; Comparer: TListSortComparer_Context; Context: Pointer);
  TItemListSorter_CustomItemExchanger_Context = procedure(Items: Pointer;
    ItemCount, ItemSize: SizeUInt; Comparer: TListSortComparer_Context;
    Exchanger: TListSortCustomItemExchanger_Context; Context: Pointer);

  PSortingAlgorithm = ^TSortingAlgorithm;
  TSortingAlgorithm = record
    PtrListSorter_NoContextComparer: TPtrListSorter_NoContext;
    PtrListSorter_ContextComparer: TPtrListSorter_Context;
    ItemListSorter_ContextComparer: TItemListSorter_Context;
    ItemListSorter_CustomItemExchanger_ContextComparer: TItemListSorter_CustomItemExchanger_Context;
  end;

procedure QuickSort_PtrList_NoContext(ItemPtrs: PPointer; ItemCount: SizeUInt; Comparer: TListSortComparer_NoContext);
procedure QuickSort_PtrList_Context(ItemPtrs: PPointer; ItemCount: SizeUInt; Comparer: TListSortComparer_Context; Context: Pointer);
procedure QuickSort_ItemList_Context(Items: Pointer; ItemCount, ItemSize: SizeUInt; Comparer: TListSortComparer_Context; Context: Pointer);
procedure QuickSort_ItemList_CustomItemExchanger_Context(
                Items: Pointer;
                ItemCount, ItemSize: SizeUInt;
                Comparer: TListSortComparer_Context;
                Exchanger: TListSortCustomItemExchanger_Context;
                Context: Pointer);

const
  QuickSort: TSortingAlgorithm = (
    PtrListSorter_NoContextComparer: @QuickSort_PtrList_NoContext;
    PtrListSorter_ContextComparer: @QuickSort_PtrList_Context;
    ItemListSorter_ContextComparer: @QuickSort_ItemList_Context;
    ItemListSorter_CustomItemExchanger_ContextComparer: @QuickSort_ItemList_CustomItemExchanger_Context;
  );

var
  DefaultSortingAlgorithm: PSortingAlgorithm = @QuickSort;

implementation

Procedure QuickSort_PtrList_NoContext(ItemPtrs: PPointer; L, R : Longint;
                                      Comparer: TListSortComparer_NoContext);
var
  I, J : Longint;
  P, Q : Pointer;
begin
 repeat
   I := L;
   J := R;
   P := ItemPtrs[ (L + R) div 2 ];
   repeat
     while Comparer(P, ItemPtrs[i]) > 0 do
       Inc(I);
     while Comparer(P, ItemPtrs[J]) < 0 do
       Dec(J);
     If I <= J then
     begin
       Q := ItemPtrs[I];
       ItemPtrs[I] := ItemPtrs[J];
       ItemPtrs[J] := Q;
       Inc(I);
       Dec(J);
     end;
   until I > J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
   if J - L < R - I then
   begin
     if L < J then
       QuickSort_PtrList_NoContext(ItemPtrs, L, J, Comparer);
     L := I;
   end
   else
   begin
     if I < R then
       QuickSort_PtrList_NoContext(ItemPtrs, I, R, Comparer);
     R := J;
   end;
 until L >= R;
end;

procedure QuickSort_PtrList_NoContext(ItemPtrs: PPointer; ItemCount: SizeUInt; Comparer: TListSortComparer_NoContext);
begin
  if not Assigned(ItemPtrs) or (ItemCount < 2) then
    exit;
  QuickSort_PtrList_NoContext(ItemPtrs, 0, ItemCount - 1, Comparer);
end;

procedure QuickSort_PtrList_Context(ItemPtrs: PPointer; ItemCount: SizeUInt; Comparer: TListSortComparer_Context; Context: Pointer);

  procedure QuickSort(L, R : Longint);
  var
    I, J : Longint;
    P, Q : Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := ItemPtrs[ (L + R) div 2 ];
      repeat
        while Comparer(P, ItemPtrs[I], Context) > 0 do
          Inc(I);
        while Comparer(P, ItemPtrs[J], Context) < 0 do
          Dec(J);
        If I <= J then
        begin
          Q := ItemPtrs[I];
          ItemPtrs[I] := ItemPtrs[J];
          ItemPtrs[J] := Q;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      // sort the smaller range recursively
      // sort the bigger range via the loop
      // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
      if J - L < R - I then
      begin
        if L < J then
          QuickSort(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSort(I, R);
        R := J;
      end;
    until L >= R;
  end;

begin
  if not Assigned(ItemPtrs) or (ItemCount < 2) then
    exit;
  QuickSort(0, ItemCount - 1);
end;

procedure QuickSort_ItemList_Context(Items: Pointer; ItemCount, ItemSize: SizeUInt; Comparer: TListSortComparer_Context; Context: Pointer);

var
  TempBuf: Pointer;

  procedure QuickSort(L, R : Longint);
  var
    I, J : Longint;
    P : Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := Items + ItemSize*((L + R) div 2);
      repeat
        while Comparer(P, Items + ItemSize*I, Context) > 0 do
          Inc(I);
        while Comparer(P, Items + ItemSize*J, Context) < 0 do
          Dec(J);
        If I <= J then
        begin
          if I < J then
          begin
            Move((Items + ItemSize*I)^, TempBuf^, ItemSize);
            Move((Items + ItemSize*J)^, (Items + ItemSize*I)^, ItemSize);
            Move(TempBuf^, (Items + ItemSize*J)^, ItemSize);
            if P = (Items + ItemSize*I) then
              P := Items + ItemSize*J
            else if P = (Items + ItemSize*J) then
              P := Items + ItemSize*I;
          end;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      // sort the smaller range recursively
      // sort the bigger range via the loop
      // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
      if J - L < R - I then
      begin
        if L < J then
          QuickSort(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSort(I, R);
        R := J;
      end;
    until L >= R;
  end;

begin
  if not Assigned(Items) or (ItemCount < 2) or (ItemSize < 1) then
    exit;
  GetMem(TempBuf, ItemSize);
  QuickSort(0, ItemCount - 1);
  FreeMem(TempBuf, ItemSize);
end;

procedure QuickSort_ItemList_CustomItemExchanger_Context(
                Items: Pointer;
                ItemCount, ItemSize: SizeUInt;
                Comparer: TListSortComparer_Context;
                Exchanger: TListSortCustomItemExchanger_Context;
                Context: Pointer);

  procedure QuickSort(L, R : Longint);
  var
    I, J : Longint;
    P : Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := Items + ItemSize*((L + R) div 2);
      repeat
        while Comparer(P, Items + ItemSize*I, Context) > 0 do
          Inc(I);
        while Comparer(P, Items + ItemSize*J, Context) < 0 do
          Dec(J);
        If I <= J then
        begin
          if I < J then
          begin
            Exchanger(Items + ItemSize*I, Items + ItemSize*J, Context);
            if P = (Items + ItemSize*I) then
              P := Items + ItemSize*J
            else if P = (Items + ItemSize*J) then
              P := Items + ItemSize*I;
          end;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      // sort the smaller range recursively
      // sort the bigger range via the loop
      // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
      if J - L < R - I then
      begin
        if L < J then
          QuickSort(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSort(I, R);
        R := J;
      end;
    until L >= R;
  end;

begin
  if not Assigned(Items) or (ItemCount < 2) or (ItemSize < 1) then
    exit;
  QuickSort(0, ItemCount - 1);
end;

end.
