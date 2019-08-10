program tsortbase;

{$MODE objfpc}

uses
  sortbase;

const
  Max = 100;

  RelTestMin = 1;
  RelTestMax = 7;

type
  PElement = ^Integer;
  TElement = Integer;
  TArray = array [0..Max] of TElement;
  TPtrArray = array [0..Max] of PElement;

procedure Fail;
begin
  Writeln('Err!');
  Halt(1);
end;

procedure CheckEqual(const Arr1, Arr2: TArray; N: Integer);
var
  I: Integer;
begin
  for I := 0 to N - 1 do
    if Arr1[I] <> Arr2[I] then
      Fail;
end;

procedure CheckPtrArrayDerefEqual(const PtrArr: TPtrArray; const Arr: TArray; N: Integer);
var
  I: Integer;
begin
  for I := 0 to N - 1 do
    if PtrArr[I]^ <> Arr[I] then
      Fail;
end;

procedure InitPtrArr(const Arr: TArray; var PtrArr: TPtrArray; N: Integer);
var
  I: Integer;
begin
  for I := 0 to N - 1 do
    PtrArr[I] := @Arr[I];
end;

procedure Sort(var Arr: TArray; N: Integer);
var
  I, J: Integer;
  tmp: TElement;
begin
  for J := 1 to N - 1 do
  begin
    I := J;
    tmp := Arr[I];
    while (I > 0) and (Arr[I - 1] > tmp) do
    begin
      Arr[I] := Arr[I - 1];
      Dec(I);
    end;
    Arr[I] := tmp;
  end;
end;

function ListSortComparer_NoContext(Item1, Item2: Pointer): Integer;
begin
  if PElement(Item1)^ > PElement(Item2)^ then
    Result := 1
  else if PElement(Item1)^ < PElement(Item2)^ then
    Result := -1
  else
    Result := 0;
end;

function ListSortComparer_Context(Item1, Item2, Context: Pointer): Integer;
begin
  if PElement(Item1)^ > PElement(Item2)^ then
    Result := 1
  else if PElement(Item1)^ < PElement(Item2)^ then
    Result := -1
  else
    Result := 0;
end;

procedure ListSortCustomItemExchanger_Context(Item1, Item2, Context: Pointer);
var
  tmp: TElement;
begin
  tmp := PElement(Item1)^;
  PElement(Item1)^ := PElement(Item2)^;
  PElement(Item2)^ := tmp;
end;

procedure TestSort(const OrigArr: TArray; N: Integer);
var
  Arr, SortArr: TArray;
  PtrArr: TPtrArray;
begin
  SortArr := OrigArr;
  Sort(SortArr, N);

  Arr := OrigArr;
  DefaultSortingAlgorithm^.ItemListSorter_ContextComparer(@Arr[0], N, SizeOf(TElement), @ListSortComparer_Context, nil);
  CheckEqual(Arr, SortArr, N);

  Arr := OrigArr;
  DefaultSortingAlgorithm^.ItemListSorter_CustomItemExchanger_ContextComparer(@Arr[0], N, SizeOf(TElement), @ListSortComparer_Context, @ListSortCustomItemExchanger_Context, nil);
  CheckEqual(Arr, SortArr, N);

  Arr := OrigArr;
  InitPtrArr(Arr, PtrArr, N);
  DefaultSortingAlgorithm^.PtrListSorter_ContextComparer(@PtrArr, N, @ListSortComparer_Context, nil);
  CheckEqual(Arr, OrigArr, N);
  CheckPtrArrayDerefEqual(PtrArr, SortArr, N);

  Arr := OrigArr;
  InitPtrArr(Arr, PtrArr, N);
  DefaultSortingAlgorithm^.PtrListSorter_NoContextComparer(@PtrArr, N, @ListSortComparer_NoContext);
  CheckEqual(Arr, OrigArr, N);
  CheckPtrArrayDerefEqual(PtrArr, SortArr, N);
end;

{ brute force tests the sorting algorithms by generating all variations with
  repetition of N elements chosen from the numbers [0..N-1]. This grows
  extremely fast (O(N**N)), so should be used for small values of N only. }
procedure TestAllVariations(N: Integer);
var
  Arr: TArray;
  SortArr: TArray;

  procedure Gen(P: Integer);
  var
    I: Integer;
  begin
    if P = N then
    begin
      TestSort(Arr, N);
      exit;
    end;
    for I := 0 to N - 1 do
    begin
      Arr[P] := I;
      Gen(P + 1);
    end;
  end;

begin
  Gen(0);
end;

procedure TestAllVariations;
var
  I: Integer;
begin
  for I := RelTestMin to RelTestMax do
    TestAllVariations(I);
end;

begin
  TestAllVariations;
  Writeln('Ok!');
end.
