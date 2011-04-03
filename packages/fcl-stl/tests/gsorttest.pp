{$mode objfpc}

unit gsorttest;

interface

uses fpcunit, testregistry, gvector, garrayutils, gutil;

type vectorlli=specialize TVector<longint>;
     lesslli=specialize TLess<longint>;
     sortlli=specialize TOrderingArrayUtils<vectorlli, longint, lesslli>;

type TGSortTest = class(TTestCase)
  Published
    procedure SortRandomTest;
    procedure SortZeroOneTest;
  public
    procedure Setup;override;
  private 
    data:vectorlli;
  end;

implementation

procedure TGSortTest.SortRandomTest;
var i:longint;
begin
  for i:=0 to 5000 do
    data.pushBack(random(10000));
  sortlli.sort(data, 5001);
  for i:=0 to 4999 do
    AssertEquals('Wrong order', false, data[i+1]<data[i]);
end;

procedure TGSortTest.SortZeroOneTest;
var i:longint;
begin
  for i:=0 to 5000 do
    data.pushBack(random(2));
  sortlli.sort(data, 5001);
  for i:=0 to 4999 do
    AssertEquals('Wrong order', false, data[i+1]<data[i]);
end;

procedure TGSortTest.Setup;
begin
  data:=vectorlli.create;
end;

initialization
  RegisterTest(TGSortTest);
end.
