{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2018 by Maciej Izak (hnb),
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains tests for the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Acknowledgment

    Thanks to Sphere 10 Software (http://sphere10.com) for sponsoring
    many new types, tests and major refactoring of entire library

    Thanks to Castle Game Engine (https://castle-engine.sourceforge.io)
    Part of tests for this module was copied from Castle Game Engine tests

 **********************************************************************}

unit tests.generics.stdcollections;

{$mode delphi}

interface

uses
  fpcunit, testutils, testregistry, tests.generics.utils,
  Classes, SysUtils, Types, Generics.Collections, Generics.Defaults;

type
  TTestStdCollections = class(TTestCollections)
  private
    FRemoved: Integer;
    FRaiseAt: Integer;
    procedure Test_TList_Notification(AList: TList<string>); overload;
    procedure CountRemovedInteger(ASender: TObject; const AItem: Integer; AAction: TCollectionNotification);
    procedure CountRemovedString(ASender: TObject; const AItem: string; AAction: TCollectionNotification);
  published
    // Tests from Castle Game Engine
    procedure Test_List;
    procedure Test_FreeingManually;
    procedure Test_AddingLists;
    procedure Test_Sort;
    procedure Test_Pack;
    procedure Test_RecordsList;
    procedure Test_VectorsList;
    procedure Test_MethodsList;

    // My (c) tests
    procedure Test_SortedList;
    procedure Test_Queue;
    procedure Test_GenericListBox;

    procedure Test_TList_Notification; overload;
    procedure Test_TSortedList_Notification;
    procedure Test_TQueue_Notification;
    procedure Test_TStack_Notification;
    procedure Test_TObjectList_Notification;
    procedure Test_TObjectQueue_Notification;
    procedure Test_TObjectStack_Notification;

    procedure Test_TrimExcess;

    // TList<T>.IndexOfItem in both directions.
    procedure Test_TList_IndexOfItem;
    // TList<T>.ExtractItem in both directions, with notifications.
    procedure Test_TList_ExtractItem;
    // TList<T>.RemoveItem in both directions, with notifications.
    procedure Test_TList_RemoveItem;
    // TObjectList<T>.ExtractItem does not free the object, RemoveItem does.
    procedure Test_TObjectList_ExtractItem;
    // TList<T>.GetPtrEnumerator is public and yields pointers into the list.
    procedure Test_TList_GetPtrEnumerator;
    // Removing items asks the memory manager for nothing, unless a notification
    // is due for a range too large for the stack.
    procedure Test_TList_RemoveWithoutMemory;
    // A range's items are released when a notification raises.
    procedure Test_TList_DeleteRange_RaisingNotification;
    // Removed items are notified whole and in ascending order.
    procedure Test_TList_DeleteRange_WholeItemsInOrder;
    // An item whose release runs code finds the list as it will be.
    procedure Test_TList_ReleaseSeesFinalList;
  end;

  // Object that counts its destruction.
  TDestroyCounted = class
  public
    // Increments the destroy count.
    destructor Destroy; override;
  end;

  TGenericListBox<T> = class
  private class var
    F : TList<TComponentClass>;
    class procedure Test(ATest: TTestCase);
  end;

implementation

var
  gDestroyCount: Integer;

destructor TDestroyCounted.Destroy;

begin
  Inc(gDestroyCount);
  inherited Destroy;
end;


class procedure TGenericListBox<T>.Test(ATest: TTestCase);
begin
  F := TList<TComponentClass>.Create;
  F.Add(TDataModule);
  F.Add(nil);
  with TList<TComponentClass>.Create(F) do
  begin
    ATest.AssertTrue(Count = 2);
    ATest.AssertTrue(F[0] = Items[0]);
    ATest.AssertTrue(F[1] = Items[1]);
    ATest.AssertTrue(F[0] = TDataModule);
    ATest.AssertTrue(F[1] = nil);
    Free;
  end;
  F.Free;
end;

type
  TApple = class
    Name: string;
  end;

type
  TAppleList = class(TObjectList<TApple>)
    procedure Pack;
  end;

procedure TAppleList.Pack;
begin
  while Remove(nil) <> -1 do ;
end;

procedure TTestStdCollections.Test_List;
var
  A: TApple;
  Apples: TAppleList;
begin
  Apples := TAppleList.Create(true);
  try
    A := TApple.Create;
    Apples.Add(A);
    Apples.Add(TApple.Create);
    A := TApple.Create;
    Apples.Add(A);

    AssertEquals(3, Apples.Count);
    AssertEquals(2, Apples.IndexOf(A));

    Apples.Delete(0);

    AssertEquals(2, Apples.Count);
    AssertEquals(1, Apples.IndexOf(A));

    Apples.Remove(A);

    AssertEquals(1, Apples.Count);

    Apples.Delete(0);

    AssertEquals(0, Apples.Count);
  finally FreeAndNil(Apples) end;
end;

procedure TTestStdCollections.Test_FreeingManually;
var
  A: TApple;
  Apples: TAppleList;
begin
  Apples := TAppleList.Create(false);
  try
    A := TApple.Create;
    Apples.Add(A);
    Apples.Add(A);
    Apples.Add(TApple.Create);

    { This freeing would be invalid on a list that owns children,
      as we free something twice, and we leave some invalid references
      (to already freed items) in the list at various stages.
      But it should be OK with list that has OwnsChildren = false. }

    Apples[0].Free;
    Apples[0] := nil;
    Apples[1] := nil;
    Apples[2].Free;
  finally FreeAndNil(Apples) end;
end;

procedure TTestStdCollections.Test_AddingLists;
var
  A: TApple;
  Apples, Apples2: TAppleList;
begin
  Apples := TAppleList.Create(true);
  try
    A := TApple.Create;
    A.Name := 'One';
    Apples.Add(A);

    A := TApple.Create;
    A.Name := 'Two';
    Apples.Add(A);

    Apples2 := TAppleList.Create(false);
    try
      Apples2.AddRange(Apples);
      Apples2.AddRange(Apples);
      Apples2.AddRange(Apples);
      AssertEquals(6, Apples2.Count);
      AssertEquals('One', Apples2[0].Name);
      AssertEquals('Two', Apples2[1].Name);
      AssertEquals('One', Apples2[2].Name);
      AssertEquals('Two', Apples2[3].Name);
      AssertEquals('One', Apples2[4].Name);
      AssertEquals('Two', Apples2[5].Name);
    finally FreeAndNil(Apples2) end;
  finally FreeAndNil(Apples) end;
end;

function CompareApples(const Left, Right: TApple): Integer;
begin
  Result := AnsiCompareStr(Left.Name, Right.Name);
end;

procedure TTestStdCollections.Test_Sort;
type
  TAppleComparer = TComparer<TApple>;
var
  A: TApple;
  L: TAppleList;
begin
  L := TAppleList.Create(true);
  try
    A := TApple.Create;
    A.Name := '11';
    L.Add(A);

    A := TApple.Create;
    A.Name := '33';
    L.Add(A);

    A := TApple.Create;
    A.Name := '22';
    L.Add(A);

    L.Sort(TAppleComparer.Construct(@CompareApples));

    AssertEquals(3, L.Count);
    AssertEquals('11', L[0].Name);
    AssertEquals('22', L[1].Name);
    AssertEquals('33', L[2].Name);
  finally FreeAndNil(L) end;
end;

procedure TTestStdCollections.Test_Pack;
var
  A: TApple;
  L: TAppleList;
begin
  L := TAppleList.Create(true);
  try
    L.Add(nil);

    A := TApple.Create;
    A.Name := '11';
    L.Add(A);

    L.Add(nil);

    A := TApple.Create;
    A.Name := '33';
    L.Add(A);

    A := TApple.Create;
    A.Name := '22';
    L.Add(A);

    L.Add(nil);
    L.Add(nil);

    L.Pack;

    AssertEquals(3, L.Count);
    AssertEquals('11', L[0].Name);
    AssertEquals('33', L[1].Name);
    AssertEquals('22', L[2].Name);
  finally FreeAndNil(L) end;
end;

procedure TTestStdCollections.Test_RecordsList;
type
  TMyRecord = packed record
    A, B: Integer;
  end;
  TMyRecordList = TList<TMyRecord>;
var
  List: TMyRecordList;
  R1, R2, R: TMyRecord;
begin
  List := TMyRecordList.Create;
  try
    R1.A := 11;
    R1.B := 22;
    List.Add(R1);

    R2.A := 33;
    R2.B := 44;
    List.Add(R2);

    R2.A := 33;
    R2.B := 44;
    List.Add(R2);

    AssertEquals(3, List.Count);
    AssertEquals(11, List[0].A);
    AssertEquals(22, List[0].B);
    AssertEquals(33, List[1].A);
    AssertEquals(44, List[1].B);
    AssertEquals(33, List[2].A);
    AssertEquals(44, List[2].B);

    List.Delete(2);

    AssertEquals(2, List.Count);
    AssertEquals(11, List[0].A);
    AssertEquals(22, List[0].B);
    AssertEquals(33, List[1].A);
    AssertEquals(44, List[1].B);

    AssertEquals(0, List.IndexOf(R1));
    AssertEquals(1, List.IndexOf(R2));

    // change R1 and R2, to make sure it doesn't matter for tests
    R1.A := 111111;
    R1.B := 222222;
    R2.A := 333333;
    R2.B := 444444;
    AssertEquals(-1, List.IndexOf(R1));
    AssertEquals(-1, List.IndexOf(R2));

    R.A := 11;
    R.B := 22;
    AssertEquals(0, List.IndexOf(R));

    R.A := 33;
    R.B := 44;
    AssertEquals(1, List.IndexOf(R));

    R.A := 11;
    R.B := 22;
    List.Remove(R);
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0].A);
    AssertEquals(44, List[0].B);

    R.A := 666;
    R.B := 22;
    List.Remove(R); // does nothing, no such record
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0].A);
    AssertEquals(44, List[0].B);
  finally FreeAndNil(List) end;
end;

procedure TTestStdCollections.Test_VectorsList;
type
  TMyVector = packed array [0..1] of Single;
  TMyVectorList = TList<TMyVector>;
var
  List: TMyVectorList;
  R1, R2, R: TMyVector;
begin
  List := TMyVectorList.Create;
  try
    R1[0] := 11;
    R1[1] := 22;
    List.Add(R1);

    R2[0] := 33;
    R2[1] := 44;
    List.Add(R2);

    R2[0] := 33;
    R2[1] := 44;
    List.Add(R2);

    AssertEquals(3, List.Count);
    AssertEquals(11, List[0][0]);
    AssertEquals(22, List[0][1]);
    AssertEquals(33, List[1][0]);
    AssertEquals(44, List[1][1]);
    AssertEquals(33, List[2][0]);
    AssertEquals(44, List[2][1]);

    List.Delete(2);

    AssertEquals(2, List.Count);
    AssertEquals(11, List[0][0]);
    AssertEquals(22, List[0][1]);
    AssertEquals(33, List[1][0]);
    AssertEquals(44, List[1][1]);

    AssertEquals(0, List.IndexOf(R1));
    AssertEquals(1, List.IndexOf(R2));

    // change R1 and R2, to make sure it doesn't matter for tests
    R1[0] := 111111;
    R1[1] := 222222;
    R2[0] := 333333;
    R2[1] := 444444;
    AssertEquals(-1, List.IndexOf(R1));
    AssertEquals(-1, List.IndexOf(R2));

    R[0] := 11;
    R[1] := 22;
    AssertEquals(0, List.IndexOf(R));

    R[0] := 33;
    R[1] := 44;
    AssertEquals(1, List.IndexOf(R));

    R[0] := 11;
    R[1] := 22;
    List.Remove(R);
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0][0]);
    AssertEquals(44, List[0][1]);

    R[0] := 666;
    R[1] := 22;
    List.Remove(R); // does nothing, no such item
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0][0]);
    AssertEquals(44, List[0][1]);
  finally FreeAndNil(List) end;
end;

type
  TSomeClass = class
    procedure Foo(A: Integer);
  end;

procedure TSomeClass.Foo(A: Integer);
begin
end;

procedure TTestStdCollections.Test_MethodsList;
type
  TMyMethod = procedure (A: Integer) of object;
  TMyMethodList = TList<TMyMethod>;

  procedure AssertMethodsEqual(const M1, M2: TMyMethod);
  begin
    AssertTrue(TMethod(M1).Code = TMethod(M2).Code);
    AssertTrue(TMethod(M1).Data = TMethod(M2).Data);
  end;

var
  List: TMyMethodList;
  C1, C2, C3: TSomeClass;
  M: TMyMethod;
begin
  C1 := TSomeClass.Create;
  C2 := TSomeClass.Create;
  C3 := TSomeClass.Create;

  List := TMyMethodList.Create;
  try
    List.Add(C1.Foo);
    List.Add(C2.Foo);
    List.Add(C2.Foo);

    AssertEquals(3, List.Count);
    M := C1.Foo;
    AssertMethodsEqual(List[0], M);
    M := C2.Foo;
    AssertMethodsEqual(List[1], M);
    AssertMethodsEqual(List[2], M);

    List.Delete(2);

    AssertEquals(2, List.Count);
    M := C1.Foo;
    AssertMethodsEqual(List[0], M);
    M := C2.Foo;
    AssertMethodsEqual(List[1], M);

    AssertEquals(0, List.IndexOf(C1.Foo));
    AssertEquals(1, List.IndexOf(C2.Foo));

    AssertEquals(-1, List.IndexOf(C3.Foo));

    List.Remove(C1.Foo);
    AssertEquals(1, List.Count);
    M := C2.Foo;
    AssertMethodsEqual(List[0], M);

    List.Remove(C3.Foo); // does nothing, no such item
    AssertEquals(1, List.Count);
    M := C2.Foo;
    AssertMethodsEqual(List[0], M);
  finally FreeAndNil(List) end;

  C1.Free;
  C2.Free;
  C3.Free;
end;

procedure TTestStdCollections.Test_SortedList;
var
  LSortedList: TSortedList<Integer>;
  i: integer;
  LRandomOrder: TArray<Integer>;
begin
  LRandomOrder := TArray<Integer>.Create(
    10, 8, 17, 19, 2, 0, 13, 15, 5, 7, 12, 14, 4, 6, 11, 9, 16, 18, 3, 1);

  LSortedList := TSortedList<Integer>.Create;
  for i in LRandomOrder do
    LSortedList.Add(i);

  AssertEquals('Wrong Count value for TSortedList', Length(LRandomOrder), LSortedList.Count);

  for i := 0 to 19 do
    AssertEquals(Format('Wrong item (%d) index (%d) in TSortedList',[LSortedList[i], i]), i, LSortedList[i]);

  LSortedList.Free;
end;

procedure TTestStdCollections.Test_Queue;
const
  NUMBERS: array[0..2] of Integer = (3,4,5);
var
  LQueue: TQueue<Integer>;
  i: Integer;
  j: Integer;
  pi: Pinteger;
begin
  LQueue := TQueue<Integer>.Create;

  for i := 1 to 5 do
  begin
    LQueue.Enqueue(i);
    AssertEquals(LQueue.Peek, 1);
  end;

  AssertEquals(LQueue.Dequeue, 1);
  AssertEquals(LQueue.Extract, 2);

  j := 0;
  for i in LQueue do
  begin
    AssertEquals(i, NUMBERS[j]);
    Inc(j);
  end;

  j := 0;
  for pi in LQueue.Ptr^ do
  begin
    AssertEquals(pi^, NUMBERS[j]);
    Inc(j);
  end;

  LQueue.Free;
end;

procedure TTestStdCollections.Test_TList_Notification(AList: TList<string>);
var
  LStringsObj: TEnumerable<string>;
  LStringsIntf: IEnumerable<string>;
begin
  try
    LStringsObj := EnumerableStringsObj(['Ddd', 'Eee']);
    LStringsIntf := EnumerableStringsIntf(['Fff', 'Ggg']);
    AList.OnNotify := NotifyTestStr;

    { Add + AddRange }

    NotificationAdd(AList, ['Aaa', 'Bbb', 'Ccc', 'Ddd', 'Eee', 'Fff', 'Ggg'], cnAdded);
    AList.Add('Aaa');
    AList.AddRange(['Bbb', 'Ccc']);
    AList.AddRange(LStringsObj);
    AList.AddRange(LStringsIntf);
    AssertNotificationsExecutedStr;

    { Clear }

    NotificationAdd(AList, ['Aaa', 'Bbb', 'Ccc', 'Ddd', 'Eee', 'Fff', 'Ggg'], cnRemoved);
    AList.Clear;
    AssertNotificationsExecutedStr;

    { Insert + InsertRange }

    NotificationAdd(AList, ['Aaa', 'Bbb', 'Ccc', 'Ddd', 'Eee', 'Fff', 'Ggg'], cnAdded);
    AList.Insert(0, 'Aaa');
    AList.InsertRange(1, ['Bbb', 'Ccc']);
    AList.InsertRange(3, LStringsObj);
    AList.InsertRange(5, LStringsIntf);
    AssertNotificationsExecutedStr;

    { Remove + Delete + DeleteRange }

    NotificationAdd(AList, ['Aaa', 'Bbb', 'Ccc', 'Ddd', 'Eee', 'Fff', 'Ggg'], cnRemoved);
    AList.Remove('Aaa');
    AList.Delete(0);
    AList.DeleteRange(0, 5);
    AssertEquals(AList.Count, 0);
    AssertNotificationsExecutedStr;

    { ExtractIndex, Extract }

    NotificationAdd(AList, ['Aaa', 'Bbb', 'Ccc'], cnAdded);
    AList.AddRange(['Aaa', 'Bbb', 'Ccc']);
    AssertNotificationsExecutedStr;
    NotificationAdd(AList, ['Aaa', 'Bbb'], cnExtracted);
    AssertEquals(AList.ExtractIndex(0), 'Aaa');
    AssertEquals(AList.Extract('Bbb'), 'Bbb');
    AssertNotificationsExecutedStr;

    { SetItem }
    NotificationAdd(AList, 'Ccc', cnRemoved);
    NotificationAdd(AList, 'FPC', cnAdded);
    AList[0] := 'FPC';
    AssertNotificationsExecutedStr;

  finally
    LStringsObj.Free;
    { Free }
    NotificationAdd(AList, 'FPC', cnRemoved);
    AList.Free;
    AssertNotificationsExecutedStr;
  end;
end;

procedure TTestStdCollections.Test_TList_Notification;
begin
  Test_TList_Notification(TList<string>.Create);
end;

procedure TTestStdCollections.Test_TSortedList_Notification;
var
  LList: TSortedList<string>;
begin
  LList := TSortedList<string>.Create;
  LList.SortStyle := cssUser;
  Test_TList_Notification(LList);
end;

procedure TTestStdCollections.Test_TQueue_Notification;
var
  LQueue: TQueue<string>;
begin
  LQueue := TQueue<string>.Create();
  try
    LQueue.OnNotify := NotifyTestStr;

    { Enqueue }
    NotificationAdd(LQueue, ['Aaa', 'Bbb', 'Ccc', 'Ddd'], cnAdded);
    LQueue.Enqueue('Aaa');
    LQueue.Enqueue('Bbb');
    LQueue.Enqueue('Ccc');
    LQueue.Enqueue('Ddd');
    AssertNotificationsExecutedStr;

    { Dequeue }
    NotificationAdd(LQueue, 'Aaa', cnRemoved);
    AssertEquals(LQueue.Dequeue, 'Aaa');
    AssertNotificationsExecutedStr;

    { Extract }
    NotificationAdd(LQueue, 'Bbb', cnExtracted);
    AssertEquals(LQueue.Extract, 'Bbb');
    AssertNotificationsExecutedStr;

    { Clear }
    NotificationAdd(LQueue, ['Ccc', 'Ddd'], cnRemoved);
    LQueue.Clear;
    AssertNotificationsExecutedStr;

    { Enqueue }
    NotificationAdd(LQueue, ['FPC', 'Polandball'], cnAdded);
    LQueue.Enqueue('FPC');
    LQueue.Enqueue('Polandball');
    AssertNotificationsExecutedStr;
  finally
    NotificationAdd(LQueue, ['FPC', 'Polandball'], cnRemoved);
    LQueue.Free;
    AssertNotificationsExecutedStr;
  end;
end;

procedure TTestStdCollections.Test_TStack_Notification;
var
  LStack: TStack<string>;
begin
  LStack := TStack<string>.Create();
  try
    LStack.OnNotify := NotifyTestStr;

    { Push }
    NotificationAdd(LStack, ['Aaa', 'Bbb', 'Ccc', 'Ddd'], cnAdded);
    LStack.Push('Aaa');
    LStack.Push('Bbb');
    LStack.Push('Ccc');
    LStack.Push('Ddd');
    AssertNotificationsExecutedStr;

    { Pop }
    NotificationAdd(LStack, 'Ddd', cnRemoved);
    AssertEquals(LStack.Pop, 'Ddd');
    AssertNotificationsExecutedStr;

    { Extract }
    NotificationAdd(LStack, 'Ccc', cnExtracted);
    AssertEquals(LStack.Extract, 'Ccc');
    AssertNotificationsExecutedStr;

    { Clear }
    NotificationAdd(LStack, ['Bbb', 'Aaa'], cnRemoved);
    LStack.Clear;
    AssertNotificationsExecutedStr;

    { Push }
    NotificationAdd(LStack, ['FPC', 'Polandball'], cnAdded);
    LStack.Push('FPC');
    LStack.Push('Polandball');
    AssertNotificationsExecutedStr;
  finally
    NotificationAdd(LStack, ['Polandball', 'FPC'], cnRemoved);
    LStack.Free;
    AssertNotificationsExecutedStr;
  end;
end;

procedure TTestStdCollections.Test_TObjectList_Notification;
var
  LObj: TEnumerable<TObject>;
  LIntf: IEnumerable<TObject>;
  O: TArray<TObject>;
  LList: TObjectList<TObject>;
  i: Integer;
begin
  try
    CreateObjects(O, 8);

    LList := TObjectList<TObject>.Create(false);
    LList.OnNotify := NotifyTestObj;

    LObj := EnumerableObjectsObj([O[3], O[4]]);
    LIntf := EnumerableObjectsIntf([O[5], O[6]]);

    { Add + AddRange }

    NotificationAdd(LList, [O[0], O[1], O[2], O[3], O[4], O[5], O[6]], cnAdded);
    LList.Add(O[0]);
    LList.AddRange([O[1], O[2]]);
    LList.AddRange(LObj);
    LList.AddRange(LIntf);
    AssertNotificationsExecutedObj;

    { Clear }

    NotificationAdd(LList, [O[0], O[1], O[2], O[3], O[4], O[5], O[6]], cnRemoved);
    LList.Clear;
    AssertNotificationsExecutedObj;

    { Insert + InsertRange }

    NotificationAdd(LList, [O[0], O[1], O[2], O[3], O[4], O[5], O[6]], cnAdded);
    LList.Insert(0, O[0]);
    LList.InsertRange(1, [O[1], O[2]]);
    LList.InsertRange(3, LObj);
    LList.InsertRange(5, LIntf);
    AssertNotificationsExecutedObj;

    { Remove + Delete + DeleteRange }

    NotificationAdd(LList, [O[0], O[1], O[2], O[3], O[4], O[5], O[6]], cnRemoved);
    LList.Remove(O[0]);
    LList.Delete(0);
    LList.DeleteRange(0, 5);
    AssertEquals(LList.Count, 0);
    AssertNotificationsExecutedObj;

    { ExtractIndex, Extract }

    NotificationAdd(LList, [O[0], O[1], O[2]], cnAdded);
    LList.AddRange([O[0], O[1], O[2]]);
    AssertNotificationsExecutedObj;
    NotificationAdd(LList, [O[0], O[1]], cnExtracted);
    AssertTrue(LList.ExtractIndex(0) = O[0]);
    AssertTrue(LList.Extract(O[1]) = O[1]);
    AssertNotificationsExecutedObj;

    { SetItem }
    NotificationAdd(LList, O[2], cnRemoved);
    NotificationAdd(LList, O[7], cnAdded);
    LList[0] := O[7];
    AssertNotificationsExecutedObj;

  finally
    LObj.Free;
    { Free }
    NotificationAdd(LList, O[7], cnRemoved);
    FreeObjects(O);
    LList.Free;
    AssertNotificationsExecutedObj;
  end;
end;

procedure TTestStdCollections.Test_TObjectQueue_Notification;
var
  LQueue: TObjectQueue<TObject>;
  O: TArray<TObject>;
begin
  LQueue := TObjectQueue<TObject>.Create(false);
  try
    CreateObjects(O, 6);
    LQueue.OnNotify := NotifyTestObj;

    { Enqueue }
    NotificationAdd(LQueue, [O[0], O[1], O[2], O[3]], cnAdded);
    LQueue.Enqueue(O[0]);
    LQueue.Enqueue(O[1]);
    LQueue.Enqueue(O[2]);
    LQueue.Enqueue(O[3]);
    AssertNotificationsExecutedObj;

    { Dequeue }
    NotificationAdd(LQueue, O[0], cnRemoved);
    LQueue.Dequeue;
    AssertNotificationsExecutedObj;

    { Extract }
    NotificationAdd(LQueue, O[1], cnExtracted);
    AssertTrue(LQueue.Extract = O[1]);
    AssertNotificationsExecutedObj;

    { Clear }
    NotificationAdd(LQueue, [O[2], O[3]], cnRemoved);
    LQueue.Clear;
    AssertNotificationsExecutedObj;

    { Enqueue }
    NotificationAdd(LQueue, [O[4], O[5]], cnAdded);
    LQueue.Enqueue(O[4]);
    LQueue.Enqueue(O[5]);
    AssertNotificationsExecutedObj;
  finally
    NotificationAdd(LQueue, [O[4], O[5]], cnRemoved);
    FreeObjects(O);
    LQueue.Free;
    AssertNotificationsExecutedObj;
  end;
end;

procedure TTestStdCollections.Test_TObjectStack_Notification;
var
  LStack: TStack<TObject>;
  O: TArray<TObject>;
begin
  LStack := TObjectStack<TObject>.Create(false);
  try
    CreateObjects(O, 6);
    LStack.OnNotify := NotifyTestObj;

    { Push }
    NotificationAdd(LStack, [O[0], O[1], O[2], O[3]], cnAdded);
    LStack.Push(O[0]);
    LStack.Push(O[1]);
    LStack.Push(O[2]);
    LStack.Push(O[3]);
    AssertNotificationsExecutedObj;

    { Pop }
    NotificationAdd(LStack, O[3], cnRemoved);
    AssertTrue(LStack.Pop = O[3]);
    AssertNotificationsExecutedObj;

    { Extract }
    NotificationAdd(LStack, O[2], cnExtracted);
    AssertTrue(LStack.Extract = O[2]);
    AssertNotificationsExecutedObj;

    { Clear }
    NotificationAdd(LStack, [O[1], O[0]], cnRemoved);
    LStack.Clear;
    AssertNotificationsExecutedObj;

    { Pop }
    NotificationAdd(LStack, [O[4], O[5]], cnAdded);
    LStack.Push(O[4]);
    LStack.Push(O[5]);
    AssertNotificationsExecutedObj;
  finally
    NotificationAdd(LStack, [O[5], O[4]], cnRemoved);
    FreeObjects(O);
    LStack.Free;
    AssertNotificationsExecutedObj;
  end;
end;

procedure TTestStdCollections.Test_GenericListBox;
begin
  TGenericListBox<Integer>.Test(Self);
end;

procedure TTestStdCollections.Test_TrimExcess;
var
  LList: TList<Integer>;
  LQueue: TQueue<Integer>;
  LStack: TStack<Integer>;
begin
  LList := TList<Integer>.Create;
  LQueue := TQueue<Integer>.Create;
  LStack := TStack<Integer>.Create;

  try
    LList.AddRange([1, 2, 3, 4, 5, 6]);
    LList.DeleteRange(2, 3);
    CheckNotEquals(LList.Capacity, LList.Count);
    LList.TrimExcess;
    AssertEquals(LList.Capacity, LList.Count);

    LQueue.Enqueue(1);
    LQueue.Enqueue(2);
    LQueue.Dequeue;
    CheckNotEquals(LQueue.Capacity, LQueue.Count);
    LQueue.TrimExcess;
    AssertEquals(LQueue.Capacity, LQueue.Count);

    LStack.Push(1);
    LStack.Push(2);
    LStack.Pop;
    CheckNotEquals(LStack.Capacity, LStack.Count);
    LStack.TrimExcess;
    AssertEquals(LStack.Capacity, LStack.Count);
  finally
    LStack.Free;
    LQueue.Free;
    LList.Free;
  end;
end;


procedure TTestStdCollections.Test_TList_IndexOfItem;

var
  lList: TList<string>;

begin
  lList:=TList<string>.Create;
  try
    AssertEquals('Empty list from beginning', -1, lList.IndexOfItem('a',TDirection.FromBeginning));
    AssertEquals('Empty list from end', -1, lList.IndexOfItem('a',TDirection.FromEnd));
    lList.AddRange(['a','b','a','c']);
    AssertEquals('First a', 0, lList.IndexOfItem('a',TDirection.FromBeginning));
    AssertEquals('Last a', 2, lList.IndexOfItem('a',TDirection.FromEnd));
    AssertEquals('Single c from beginning', 3, lList.IndexOfItem('c',TDirection.FromBeginning));
    AssertEquals('Single c from end', 3, lList.IndexOfItem('c',TDirection.FromEnd));
    AssertEquals('Missing from beginning', -1, lList.IndexOfItem('x',TDirection.FromBeginning));
    AssertEquals('Missing from end', -1, lList.IndexOfItem('x',TDirection.FromEnd));
  finally
    lList.Free;
  end;
end;


procedure TTestStdCollections.Test_TList_ExtractItem;

var
  lList: TList<string>;

begin
  lList:=TList<string>.Create;
  try
    lList.AddRange(['a','b','a','c','a']);
    lList.OnNotify:=NotifyTestStr;
    NotificationAdd(lList,'a',cnExtracted);
    AssertEquals('Extract last a', 'a', lList.ExtractItem('a',TDirection.FromEnd));
    AssertNotificationsExecutedStr;
    AssertEquals('Count after extracting the last a', 4, lList.Count);
    AssertEquals('Last element after extracting the last a', 'c', lList.Last);
    NotificationAdd(lList,'a',cnExtracted);
    AssertEquals('Extract first a', 'a', lList.ExtractItem('a',TDirection.FromBeginning));
    AssertNotificationsExecutedStr;
    AssertEquals('Count after extracting the first a', 3, lList.Count);
    AssertEquals('First element after extracting the first a', 'b', lList.First);
    AssertEquals('Remaining a', 1, lList.IndexOf('a'));
    AssertEquals('Missing from end gives Default(T)', '', lList.ExtractItem('x',TDirection.FromEnd));
    AssertEquals('Missing from beginning gives Default(T)', '', lList.ExtractItem('x',TDirection.FromBeginning));
    AssertNotificationsExecutedStr;
    AssertEquals('Count unchanged after a missing item', 3, lList.Count);
  finally
    lList.OnNotify:=nil;
    lList.Free;
  end;
end;


procedure TTestStdCollections.Test_TList_RemoveItem;

var
  lList: TList<string>;

begin
  lList:=TList<string>.Create;
  try
    lList.AddRange(['a','b','a','c']);
    lList.OnNotify:=NotifyTestStr;
    NotificationAdd(lList,'a',cnRemoved);
    AssertEquals('Index of the removed last a', 2, lList.RemoveItem('a',TDirection.FromEnd));
    AssertNotificationsExecutedStr;
    AssertEquals('Element 2 after removing the last a', 'c', lList[2]);
    NotificationAdd(lList,'a',cnRemoved);
    AssertEquals('Index of the removed first a', 0, lList.RemoveItem('a',TDirection.FromBeginning));
    AssertNotificationsExecutedStr;
    AssertEquals('Missing item', -1, lList.RemoveItem('a',TDirection.FromEnd));
    AssertNotificationsExecutedStr;
    AssertEquals('Remaining count', 2, lList.Count);
  finally
    lList.OnNotify:=nil;
    lList.Free;
  end;
end;


procedure TTestStdCollections.Test_TObjectList_ExtractItem;

var
  lList: TObjectList<TDestroyCounted>;
  lFirst, lSecond, lExtracted: TDestroyCounted;

begin
  gDestroyCount:=0;
  lList:=TObjectList<TDestroyCounted>.Create(True);
  try
    lFirst:=TDestroyCounted.Create;
    lSecond:=TDestroyCounted.Create;
    lList.AddRange([lFirst,lSecond,lFirst]);
    lExtracted:=lList.ExtractItem(lFirst,TDirection.FromEnd);
    AssertSame('Extracted object', lFirst, lExtracted);
    AssertEquals('Extracting does not free', 0, gDestroyCount);
    AssertEquals('Count after extracting', 2, lList.Count);
    AssertSame('First occurrence stays', lFirst, lList[0]);
    lExtracted:=lList.ExtractItem(lFirst,TDirection.FromBeginning);
    AssertSame('Extracted first occurrence', lFirst, lExtracted);
    AssertEquals('Extracting the first occurrence does not free', 0, gDestroyCount);
    lFirst.Free;
    gDestroyCount:=0;
    AssertEquals('Index of removed object', 0, lList.RemoveItem(lSecond,TDirection.FromEnd));
    AssertEquals('Removing frees', 1, gDestroyCount);
    AssertEquals('Empty after removing', 0, lList.Count);
  finally
    lList.Free;
  end;
end;


procedure TTestStdCollections.Test_TList_GetPtrEnumerator;

type
  TIndexRec = record
    Index: Integer;
  end;
  TIndexRecList = TList<TIndexRec>;

var
  lList: TIndexRecList;
  lRec: TIndexRec;
  lEnum: TEnumerator<TIndexRecList.PT>;
  lVisited, i: Integer;

begin
  lList:=TIndexRecList.Create;
  try
    for i:=1 to 3 do
      begin
      lRec.Index:=i;
      lList.Add(lRec);
      end;
    lVisited:=0;
    lEnum:=lList.GetPtrEnumerator;
    try
      while lEnum.MoveNext do
        begin
        AssertTrue('Pointer to list item '+IntToStr(lVisited), lEnum.Current=@lList.List[lVisited]);
        Inc(lEnum.Current^.Index,10);
        Inc(lVisited);
        end;
    finally
      lEnum.Free;
    end;
    AssertEquals('Items visited', 3, lVisited);
    for i:=0 to lList.Count-1 do
      AssertEquals('Item '+IntToStr(i)+' modified through pointer', i+11, lList[i].Index);
  finally
    lList.Free;
  end;
end;


var
  // The memory manager below counts requests for memory while this is set.
  gCountRequests: Boolean;
  gRequests: Integer;
  gPlainManager: TMemoryManager;

function CountingGetMem(Size: PtrUInt): Pointer;

begin
  if gCountRequests then
    Inc(gRequests);
  Result:=gPlainManager.GetMem(Size);
end;

function CountingAllocMem(Size: PtrUInt): Pointer;

begin
  if gCountRequests then
    Inc(gRequests);
  Result:=gPlainManager.AllocMem(Size);
end;

function CountingReAllocMem(var P: Pointer; Size: PtrUInt): Pointer;

begin
  if gCountRequests and (Size>0) then
    Inc(gRequests);
  Result:=gPlainManager.ReAllocMem(P,Size);
end;

procedure TTestStdCollections.CountRemovedInteger(ASender: TObject; const AItem: Integer; AAction: TCollectionNotification);

begin
  if AAction=cnRemoved then
    Inc(FRemoved);
end;

procedure TTestStdCollections.CountRemovedString(ASender: TObject; const AItem: string; AAction: TCollectionNotification);

begin
  if AAction<>cnRemoved then
    Exit;
  Inc(FRemoved);
  if FRemoved=FRaiseAt then
    raise EAbort.Create('notification');
end;

procedure TTestStdCollections.Test_TList_RemoveWithoutMemory;

var
  lCounting: TMemoryManager;
  lInts: TList<Integer>;
  lStrings: TList<string>;
  lObjects: TObjectList<TDestroyCounted>;
  lText: string;
  lOwn: SizeInt;
  i, lFit: Integer;

  procedure Start;
  begin
    gRequests:=0;
    gCountRequests:=True;
  end;

  function Requests: Integer;
  begin
    gCountRequests:=False;
    Result:=gRequests;
  end;

begin
  lInts:=nil;
  lStrings:=nil;
  lObjects:=nil;
  GetMemoryManager(gPlainManager);
  lCounting:=gPlainManager;
  lCounting.GetMem:=@CountingGetMem;
  lCounting.AllocMem:=@CountingAllocMem;
  lCounting.ReAllocMem:=@CountingReAllocMem;
  SetMemoryManager(lCounting);
  try
    // No notification: nothing is asked for, whatever the size.
    lInts:=TList<Integer>.Create;
    for i:=0 to 99999 do
      lInts.Add(i);
    Start;
    lInts.DeleteRange(10,90000);
    AssertEquals('DeleteRange, integers', 0, Requests);
    AssertEquals('Count after DeleteRange', 10000, lInts.Count);
    AssertEquals('First item after the range', 90010, lInts[10]);
    AssertEquals('Last item', 99999, lInts[9999]);
    Start;
    lInts.Count:=5;
    AssertEquals('Count := 5', 0, Requests);
    Start;
    lInts.Clear;
    AssertEquals('Clear, integers', 0, Requests);
    for i:=0 to 99999 do
      lInts.Add(i);
    Start;
    FreeAndNil(lInts);
    AssertEquals('Free, integers', 0, Requests);

    // Managed items are released where they are.
    lText:=IntToStr(Random(1000)+1000);
    // The references the string has before a list holds it: not 1 on every
    // target (x86_64-linux keeps the function result in a temporary).
    lOwn:=StringRefCount(lText);
    lStrings:=TList<string>.Create;
    for i:=1 to 5 do
      lStrings.Add(lText);
    lStrings[0]:='first';
    lStrings[4]:='last';
    AssertEquals('References before', lOwn+3, StringRefCount(lText));
    Start;
    lStrings.DeleteRange(1,2);
    AssertEquals('DeleteRange, strings', 0, Requests);
    AssertEquals('References after DeleteRange', lOwn+1, StringRefCount(lText));
    AssertEquals('Count after DeleteRange', 3, lStrings.Count);
    AssertEquals('Item 0', 'first', lStrings[0]);
    AssertEquals('Item 1', lText, lStrings[1]);
    AssertEquals('Item 2', 'last', lStrings[2]);
    Start;
    lStrings.Clear;
    AssertEquals('Clear, strings', 0, Requests);
    AssertEquals('References after Clear', lOwn, StringRefCount(lText));
    for i:=1 to 5 do
      lStrings.Add(lText);
    Start;
    FreeAndNil(lStrings);
    AssertEquals('Free, strings', 0, Requests);
    AssertEquals('References after Free', lOwn, StringRefCount(lText));
    // Many strings, each owned by the list alone.
    lStrings:=TList<string>.Create;
    for i:=1 to 50000 do
      lStrings.Add(IntToStr(i));
    Start;
    lStrings.DeleteRange(100,40000);
    AssertEquals('DeleteRange, 40000 strings', 0, Requests);
    AssertEquals('First string after the range', '40101', lStrings[100]);
    Start;
    FreeAndNil(lStrings);
    AssertEquals('Free, 10000 strings', 0, Requests);

    // A notification for a few items needs no memory either: up to 64 pointers
    // of items wait on the stack, one more takes a block.
    lInts:=TList<Integer>.Create;
    lInts.OnNotify:=CountRemovedInteger;
    for i:=0 to 4 do
      lInts.Add(i);
    FRemoved:=0;
    Start;
    FreeAndNil(lInts);
    AssertEquals('Free with OnNotify, 5 items', 0, Requests);
    AssertEquals('Removed notifications', 5, FRemoved);
    lFit:=(64*SizeOf(Pointer)) div SizeOf(Integer);
    lInts:=TList<Integer>.Create;
    for i:=0 to 3*lFit do
      lInts.Add(i);
    lInts.OnNotify:=CountRemovedInteger;
    FRemoved:=0;
    Start;
    lInts.DeleteRange(1,lFit);
    AssertEquals('DeleteRange with OnNotify, as many as fit', 0, Requests);
    AssertEquals('Removed notifications', lFit, FRemoved);
    AssertEquals('First item after the range', lFit+1, lInts[1]);
    Start;
    lInts.DeleteRange(1,lFit+1);
    AssertEquals('DeleteRange with OnNotify, one more than fit', 1, Requests);
    AssertEquals('Removed notifications', 2*lFit+1, FRemoved);
    AssertEquals('First item after the range', 2*lFit+2, lInts[1]);
    lInts.OnNotify:=nil;
    FreeAndNil(lInts);
    lObjects:=TObjectList<TDestroyCounted>.Create(True);
    for i:=0 to 4 do
      lObjects.Add(TDestroyCounted.Create);
    gDestroyCount:=0;
    Start;
    lObjects.Clear;
    AssertEquals('Clear of an owning list, 5 objects', 0, Requests);
    AssertEquals('Objects destroyed', 5, gDestroyCount);
    FreeAndNil(lObjects);

    // A large range with a notification takes one block for the items.
    lInts:=TList<Integer>.Create;
    lInts.OnNotify:=CountRemovedInteger;
    for i:=0 to 99999 do
      lInts.Add(i);
    FRemoved:=0;
    Start;
    lInts.DeleteRange(10,90000);
    AssertEquals('DeleteRange with OnNotify, 90000 items', 1, Requests);
    AssertEquals('Removed notifications', 90000, FRemoved);
    AssertEquals('Count after DeleteRange', 10000, lInts.Count);
    AssertEquals('First item after the range', 90010, lInts[10]);
    lInts.OnNotify:=nil;
    FreeAndNil(lInts);
  finally
    gCountRequests:=False;
    SetMemoryManager(gPlainManager);
    if Assigned(lInts) then
      lInts.OnNotify:=nil;
    lInts.Free;
    lStrings.Free;
    lObjects.Free;
  end;
end;

procedure TTestStdCollections.Test_TList_DeleteRange_RaisingNotification;

var
  lStrings: TList<string>;
  lText: string;
  lOwn: SizeInt;
  lRaised: Boolean;
  i: Integer;

begin
  lText:=IntToStr(Random(1000)+1000);
  // The references the string has before the list holds it (see
  // Test_TList_RemoveWithoutMemory).
  lOwn:=StringRefCount(lText);
  lStrings:=TList<string>.Create;
  try
    for i:=1 to 6 do
      lStrings.Add(lText);
    lStrings.OnNotify:=CountRemovedString;
    FRemoved:=0;
    FRaiseAt:=2;
    lRaised:=False;
    try
      lStrings.DeleteRange(1,4);
    except
      on EAbort do
        lRaised:=True;
    end;
    FRaiseAt:=0;
    AssertTrue('The notification raised', lRaised);
    AssertEquals('Notifications before the exception', 2, FRemoved);
    AssertEquals('The range left the list', 2, lStrings.Count);
    AssertEquals('Every removed item was released', lOwn+2, StringRefCount(lText));
    // The same with more items than wait on the stack.
    lStrings.OnNotify:=nil;
    for i:=1 to 3000 do
      lStrings.Add(lText);
    lStrings.OnNotify:=CountRemovedString;
    FRemoved:=0;
    FRaiseAt:=2;
    lRaised:=False;
    try
      lStrings.DeleteRange(1,2500);
    except
      on EAbort do
        lRaised:=True;
    end;
    FRaiseAt:=0;
    AssertTrue('The notification raised, large range', lRaised);
    AssertEquals('The large range left the list', 502, lStrings.Count);
    AssertEquals('Every removed item was released, large range', lOwn+502, StringRefCount(lText));
  finally
    lStrings.OnNotify:=nil;
    lStrings.Free;
  end;
  AssertEquals('References at the end', lOwn, StringRefCount(lText));
end;


type
  TTwoStrings = array[0..1] of string;

  // Checks each removed item as a whole and the order of removal.
  TTwoStringsList = class(TList<TTwoStrings>)
  public
    Seen: Integer;
    InOrder: Boolean;
  protected
    procedure Notify(const AValue: TTwoStrings; ACollectionNotification: TCollectionNotification); override;
  end;

procedure TTwoStringsList.Notify(const AValue: TTwoStrings; ACollectionNotification: TCollectionNotification);

begin
  inherited Notify(AValue,ACollectionNotification);
  if ACollectionNotification<>cnRemoved then
    Exit;
  if (AValue[0]<>IntToStr(Seen)) or (AValue[1]<>'second of '+IntToStr(Seen)) then
    InOrder:=False;
  Inc(Seen);
end;

procedure TTestStdCollections.Test_TList_DeleteRange_WholeItemsInOrder;

var
  lList: TTwoStringsList;
  lItem: TTwoStrings;
  i: Integer;

begin
  lList:=TTwoStringsList.Create;
  try
    // A few items, then more than wait on the stack.
    for i:=0 to 2999 do
      begin
      lItem[0]:=IntToStr(i);
      lItem[1]:='second of '+IntToStr(i);
      lList.Add(lItem);
      end;
    lList.InOrder:=True;
    lList.Seen:=0;
    lList.DeleteRange(0,3);
    AssertEquals('Small range: notifications', 3, lList.Seen);
    AssertTrue('Small range: whole items, in ascending order', lList.InOrder);
    lList.DeleteRange(0,2000);
    AssertEquals('Large range: notifications', 2003, lList.Seen);
    AssertTrue('Large range: whole items, in ascending order', lList.InOrder);
    AssertEquals('Items left', 997, lList.Count);
    AssertEquals('First item left', '2003', lList[0][0]);
    lList.Clear;
    AssertEquals('Clear: notifications', 3000, lList.Seen);
    AssertTrue('Clear: whole items, in ascending order', lList.InOrder);
  finally
    lList.Free;
  end;
end;


type
  // Looks at the list it is in when it is destroyed.
  TListWatcher = class(TInterfacedObject)
  public
    destructor Destroy; override;
  end;

var
  gWatchedList: TList<IInterface>;
  gWatcherCounts: Integer;
  gWatchersLeft: Integer;
  gWatcherInList: Boolean;

destructor TListWatcher.Destroy;

var
  i: Integer;

begin
  Dec(gWatchersLeft);
  if Assigned(gWatchedList) then
    begin
    Inc(gWatcherCounts,gWatchedList.Count);
    for i:=0 to gWatchedList.Count-1 do
      if (gWatchedList[i] as TObject)=Self then
        gWatcherInList:=True;
    end;
  inherited Destroy;
end;

procedure TTestStdCollections.Test_TList_ReleaseSeesFinalList;

  procedure Fill(ACount: Integer);
  var
    i: Integer;
  begin
    for i:=1 to ACount do
      gWatchedList.Add(TListWatcher.Create);
    gWatchersLeft:=ACount;
    gWatcherCounts:=0;
    gWatcherInList:=False;
  end;

begin
  gWatchedList:=TList<IInterface>.Create;
  try
    // A few items, and more than a small buffer holds.
    Fill(5);
    gWatchedList.Clear;
    AssertEquals('Clear of 5: all destroyed', 0, gWatchersLeft);
    AssertEquals('Clear of 5: each saw an empty list', 0, gWatcherCounts);
    Fill(1000);
    gWatchedList.DeleteRange(100,800);
    AssertEquals('DeleteRange of 800: 200 left', 200, gWatchersLeft);
    AssertEquals('DeleteRange of 800: each saw 200 items', 800*200, gWatcherCounts);
    AssertFalse('DeleteRange of 800: none found itself in the list', gWatcherInList);
    gWatcherCounts:=0;
    gWatchedList.Count:=50;
    AssertEquals('Count := 50: 50 left', 50, gWatchersLeft);
    AssertEquals('Count := 50: each saw 50 items', 150*50, gWatcherCounts);
    AssertFalse('Count := 50: none found itself in the list', gWatcherInList);
    // Free: the variable still names the list while its items go.
    gWatcherCounts:=0;
    gWatchedList.Free;
    gWatchedList:=nil;
    AssertEquals('Free: all destroyed', 0, gWatchersLeft);
    AssertEquals('Free: each saw an empty list', 0, gWatcherCounts);
  finally
    FreeAndNil(gWatchedList);
  end;
end;

begin
  RegisterTest(TTestStdCollections);
end.
