{$mode objfpc}

unit garrayutilstest;

interface

uses fpcunit, testregistry, gvector, garrayutils, gutil;

type vectorlli=specialize TVector<longint>;
     lesslli=specialize TLess<longint>;
     sortlli=specialize TOrderingArrayUtils<vectorlli, longint, lesslli>;

type TGArrayUtilsTest = class(TTestCase)
  Published
    procedure SortRandomTest;
    procedure SortZeroOneTest;
    procedure NextPermutationTest1;
    procedure NextPermutationTest2;
    procedure NextPermutationTest3;
    procedure NextPermutationTest4;
  public
    procedure Setup;override;
  private 
    data:vectorlli;
  end;

implementation

procedure TGArrayUtilsTest.SortRandomTest;
var i:longint;
begin
  for i:=0 to 5000 do
    data.pushBack(random(10000));
  sortlli.sort(data, 5001);
  for i:=0 to 4999 do
    AssertEquals('Wrong order', false, data[i+1]<data[i]);
end;

procedure TGArrayUtilsTest.SortZeroOneTest;
var i:longint;
begin
  for i:=0 to 5000 do
    data.pushBack(random(2));
  sortlli.sort(data, 5001);
  for i:=0 to 4999 do
    AssertEquals('Wrong order', false, data[i+1]<data[i]);
end;

procedure TGArrayUtilsTest.NextPermutationTest1;
begin
  data.pushBack(1);
  data.pushBack(2);
  data.pushBack(3);
  data.pushBack(4);
  AssertEquals('Wrong ret', true, sortlli.NextPermutation(data, 4));
  AssertEquals('Wrong perm 1', 1, data[0]);
  AssertEquals('Wrong perm 2', 2, data[1]);
  AssertEquals('Wrong perm 3', 4, data[2]);
  AssertEquals('Wrong perm 4', 3, data[3]);
end;

procedure TGArrayUtilsTest.NextPermutationTest2;
begin
  data.pushBack(4);
  data.pushBack(3);
  data.pushBack(2);
  data.pushBack(1);
  AssertEquals('Wrong ret', false, sortlli.NextPermutation(data, 4));
  AssertEquals('Wrong perm 1', 4, data[0]);
  AssertEquals('Wrong perm 2', 3, data[1]);
  AssertEquals('Wrong perm 3', 2, data[2]);
  AssertEquals('Wrong perm 4', 1, data[3]);
end;

procedure TGArrayUtilsTest.NextPermutationTest3;
begin
  data.pushBack(5);
  data.pushBack(10);
  data.pushBack(9);
  data.pushBack(8);
  data.pushBack(7);
  data.pushBack(3);
  AssertEquals('Wrong ret', true, sortlli.NextPermutation(data, 6));
  AssertEquals('Wrong perm 1', 7, data[0]);
  AssertEquals('Wrong perm 2', 3, data[1]);
  AssertEquals('Wrong perm 3', 5, data[2]);
  AssertEquals('Wrong perm 4', 8, data[3]);
  AssertEquals('Wrong perm 5', 9, data[4]);
  AssertEquals('Wrong perm 6', 10, data[5]);
end;

procedure TGArrayUtilsTest.NextPermutationTest4;
begin
  data.pushBack(0);
  data.pushBack(1);
  data.pushBack(0);
  data.pushBack(1);
  data.pushBack(1);
  data.pushBack(0);
  AssertEquals('Wrong ret', true, sortlli.NextPermutation(data, 6));
  AssertEquals('Wrong perm 1', 0, data[0]);
  AssertEquals('Wrong perm 2', 1, data[1]);
  AssertEquals('Wrong perm 3', 1, data[2]);
  AssertEquals('Wrong perm 4', 0, data[3]);
  AssertEquals('Wrong perm 5', 0, data[4]);
  AssertEquals('Wrong perm 6', 1, data[5]);
end;

procedure TGArrayUtilsTest.Setup;
begin
  data:=vectorlli.create;
end;

initialization
  RegisterTest(TGArrayUtilsTest);
end.
