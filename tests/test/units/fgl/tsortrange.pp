{ SortRange of the fgl lists and maps }
program tsortrange;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, SortBase, fgl;

type
  TIntList = specialize TFPGList<Integer>;
  TStrList = specialize TFPGList<string>;

  TItem = class
    FValue: Integer;
    // Creates an item with aValue
    constructor Create(aValue: Integer);
  end;

  TItemList = specialize TFPGObjectList<TItem>;

  IValue = interface
    ['{6B1D4E1A-6C55-4C9E-9C07-2F3A1B0D9E41}']
    // Returns the value of the item
    function GetValue: Integer;
  end;

  TValue = class(TInterfacedObject, IValue)
  private
    FValue: Integer;
  public
    // Creates an item with aValue
    constructor Create(aValue: Integer);
    // Returns the value of the item
    function GetValue: Integer;
  end;

  TValueList = specialize TFPGInterfacedObjectList<IValue>;
  TIntStrMap = specialize TFPGMap<Integer, string>;

var
  lErrors: Integer;
  lCalls: Integer;
  lCallItems: Pointer;
  lCallCount, lCallSize: SizeUInt;
  lRecorder: TSortingAlgorithm;


constructor TItem.Create(aValue: Integer);

begin
  FValue:=aValue;
end;


constructor TValue.Create(aValue: Integer);

begin
  FValue:=aValue;
end;


function TValue.GetValue: Integer;

begin
  Result:=FValue;
end;


{ Reports a failed check }
procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
      writeln('FAIL: ', aMsg);
      Inc(lErrors);
    end;
end;


{ Orders integers ascending }
function CompareInt(const aLeft, aRight: Integer): Integer;

begin
  Result:=aLeft-aRight;
end;


{ Orders strings ascending }
function CompareStr(const aLeft, aRight: string): Integer;

begin
  Result:=CompareText(aLeft, aRight);
end;


{ Orders items by value }
function CompareItem(const aLeft, aRight: TItem): Integer;

begin
  Result:=aLeft.FValue-aRight.FValue;
end;


{ Orders interfaced items by value }
function CompareValue(const aLeft, aRight: IValue): Integer;

begin
  Result:=aLeft.GetValue-aRight.GetValue;
end;


{ Records the arguments of the sorter call and sorts with the default algorithm }
procedure RecordingSorter(aItems: Pointer; aItemCount, aItemSize: SizeUInt; aComparer: TListSortComparer_Context; aContext: Pointer);

begin
  Inc(lCalls);
  lCallItems:=aItems;
  lCallCount:=aItemCount;
  lCallSize:=aItemSize;
  DefaultSortingAlgorithm^.ItemListSorter_ContextComparer(aItems, aItemCount, aItemSize, aComparer, aContext);
end;


{ Returns the items of aList as a comma separated string }
function IntsToStr(aList: TIntList): string;

var
  i: Integer;

begin
  Result:='';
  for i:=0 to aList.Count-1 do
    begin
      if i>0 then
        Result:=Result+',';
      Result:=Result+IntToStr(aList[i]);
    end;
end;


{ Returns a list with 9 down to 0 }
function MakeDescending: TIntList;

var
  i: Integer;

begin
  Result:=TIntList.Create;
  for i:=9 downto 0 do
    Result.Add(i);
end;


{ Checks that SortRange raises EListError for aStartIndex..aEndIndex and leaves the list unchanged }
procedure CheckRangeError(aList: TIntList; aStartIndex, aEndIndex: Integer);

var
  lBefore: string;
  lRaised: Boolean;

begin
  lBefore:=IntsToStr(aList);
  lRaised:=False;
  try
    aList.SortRange(aStartIndex, aEndIndex, @CompareInt);
  except
    on EListError do
      lRaised:=True;
  end;
  Check(lRaised, 'SortRange('+IntToStr(aStartIndex)+', '+IntToStr(aEndIndex)+') raises EListError');
  Check(IntsToStr(aList)=lBefore, 'SortRange('+IntToStr(aStartIndex)+', '+IntToStr(aEndIndex)+') leaves the list unchanged');
end;


{ Checks range sorting of a list of integers }
procedure TestIntList;

var
  lList, lFull: TIntList;

begin
  lList:=MakeDescending;
  lList.SortRange(2, 6, @CompareInt);
  Check(IntsToStr(lList)='9,8,3,4,5,6,7,2,1,0', 'SortRange(2, 6) sorts only the middle, got '+IntsToStr(lList));
  lList.SortRange(0, 0, @CompareInt);
  Check(IntsToStr(lList)='9,8,3,4,5,6,7,2,1,0', 'SortRange of one item changes nothing');
  lList.SortRange(5, 4, @CompareInt);
  Check(IntsToStr(lList)='9,8,3,4,5,6,7,2,1,0', 'SortRange of an empty range changes nothing');
  lList.SortRange(7, 9, @CompareInt);
  Check(IntsToStr(lList)='9,8,3,4,5,6,7,0,1,2', 'SortRange(7, 9) sorts the tail, got '+IntsToStr(lList));
  lList.SortRange(0, 1, @CompareInt);
  Check(IntsToStr(lList)='8,9,3,4,5,6,7,0,1,2', 'SortRange(0, 1) sorts the head, got '+IntsToStr(lList));
  CheckRangeError(lList, -1, 3);
  CheckRangeError(lList, 2, 10);
  CheckRangeError(lList, -5, 20);
  lFull:=MakeDescending;
  lFull.SortRange(0, lFull.Count-1, @CompareInt);
  Check(IntsToStr(lFull)='0,1,2,3,4,5,6,7,8,9', 'SortRange over all items sorts the list');
  lFull.Free;
  lList.Clear;
  lList.SortRange(0, -1, @CompareInt);
  Check(lList.Count=0, 'SortRange(0, -1) on an empty list does nothing');
  CheckRangeError(lList, 0, 0);
  lList.Free;
end;


{ Checks that the sorting algorithm overload gets the start, count and item size of the range }
procedure TestAlgorithm;

var
  lList: TIntList;

begin
  lRecorder:=DefaultSortingAlgorithm^;
  lRecorder.ItemListSorter_ContextComparer:=@RecordingSorter;
  lList:=MakeDescending;
  lCalls:=0;
  lList.SortRange(3, 7, @CompareInt, @lRecorder);
  Check(lCalls=1, 'The sorting algorithm is called once');
  Check(lCallItems=Pointer(@lList.List^[3]), 'The sorter gets the address of the first item of the range');
  Check(lCallCount=5, 'The sorter gets the number of items in the range, got '+IntToStr(lCallCount));
  Check(lCallSize=SizeOf(Integer), 'The sorter gets the item size, got '+IntToStr(lCallSize));
  Check(IntsToStr(lList)='9,8,7,2,3,4,5,6,1,0', 'SortRange with an algorithm sorts the range, got '+IntsToStr(lList));
  lCalls:=0;
  lList.SortRange(4, 3, @CompareInt, @lRecorder);
  Check(lCalls=0, 'The sorting algorithm is not called for an empty range');
  lList.Free;
end;


{ Checks range sorting of a list of strings }
procedure TestStrList;

var
  lList: TStrList;

begin
  lList:=TStrList.Create;
  lList.Add('delta');
  lList.Add('charlie');
  lList.Add('bravo');
  lList.Add('alpha');
  lList.Add('echo');
  lList.SortRange(1, 3, @CompareStr);
  Check((lList[0]='delta') and (lList[1]='alpha') and (lList[2]='bravo') and (lList[3]='charlie') and (lList[4]='echo'),
    'String SortRange(1, 3) sorts the middle');
  lList.Free;
end;


{ Checks range sorting of an object list }
procedure TestObjectList;

var
  lList: TItemList;
  i: Integer;

begin
  lList:=TItemList.Create(True);
  for i:=5 downto 1 do
    lList.Add(TItem.Create(i));
  lList.SortRange(0, 2, @CompareItem);
  Check((lList[0].FValue=3) and (lList[1].FValue=4) and (lList[2].FValue=5) and (lList[3].FValue=2) and (lList[4].FValue=1),
    'Object SortRange(0, 2) sorts the head');
  lList.SortRange(2, 4, @CompareItem, DefaultSortingAlgorithm);
  Check((lList[2].FValue=1) and (lList[3].FValue=2) and (lList[4].FValue=5), 'Object SortRange(2, 4) with an algorithm sorts the tail');
  lList.Free;
end;


{ Checks range sorting of an interfaced object list }
procedure TestInterfacedList;

var
  lList: TValueList;
  i: Integer;

begin
  lList:=TValueList.Create;
  for i:=4 downto 1 do
    lList.Add(TValue.Create(i));
  lList.SortRange(1, 3, @CompareValue);
  Check((lList[0].GetValue=4) and (lList[1].GetValue=1) and (lList[2].GetValue=2) and (lList[3].GetValue=3),
    'Interfaced SortRange(1, 3) sorts the tail');
  lList.SortRange(0, 1, @CompareValue, DefaultSortingAlgorithm);
  Check((lList[0].GetValue=1) and (lList[1].GetValue=4), 'Interfaced SortRange(0, 1) with an algorithm sorts the head');
  lList.Free;
end;


{ Checks range sorting of an unsorted map by key }
procedure TestMap;

var
  lMap: TIntStrMap;
  i: Integer;
  lRaised: Boolean;

begin
  lMap:=TIntStrMap.Create;
  lMap.OnKeyCompare:=@CompareInt;
  for i:=5 downto 1 do
    lMap.Add(i, 'v'+IntToStr(i));
  lMap.SortRange(1, 3);
  Check((lMap.Keys[0]=5) and (lMap.Keys[1]=2) and (lMap.Keys[2]=3) and (lMap.Keys[3]=4) and (lMap.Keys[4]=1),
    'Map SortRange(1, 3) sorts the keys of the middle');
  for i:=0 to lMap.Count-1 do
    Check(lMap.Data[i]='v'+IntToStr(lMap.Keys[i]), 'Map entry '+IntToStr(i)+' keeps its data with its key');
  lMap.SortRange(3, 4, DefaultSortingAlgorithm);
  Check((lMap.Keys[3]=1) and (lMap.Keys[4]=4), 'Map SortRange(3, 4) with an algorithm sorts the tail');
  lRaised:=False;
  try
    lMap.SortRange(0, 5);
  except
    on EListError do
      lRaised:=True;
  end;
  Check(lRaised, 'Map SortRange(0, 5) raises EListError');
  lMap.Free;
end;


begin
  lErrors:=0;
  TestIntList;
  TestAlgorithm;
  TestStrList;
  TestObjectList;
  TestInterfacedList;
  TestMap;
  if lErrors>0 then
    begin
      writeln(lErrors, ' errors');
      Halt(1);
    end;
  writeln('ok');
end.
