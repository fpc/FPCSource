{$mode objfpc}

unit gcompositetest;

interface

uses fpcunit, testregistry, gvector, gset;

type vectorlli=specialize TVector<longint>;
     matrix = specialize TVector<vectorlli>;
     vectorcmp = class
       class function c(a,b:vectorlli):boolean;
     end;
     setvectorlli = specialize TSet<vectorlli, vectorcmp>;

type TGCompositeTest = class(TTestCase)
  Published
    procedure MatrixTest;
    procedure SetVectorTest;
  public
    procedure Setup;override;
  private 
    data:matrix;
  end;

implementation

class function vectorcmp.c(a,b:vectorlli):boolean;
var i:SizeUInt;
begin
  if (a.size < b.size) then exit(true);
  if (a.size > b.size) then exit(false);
  i:=0;
  while i < a.size do begin
    if (a[i] < b[i]) then exit(true);
    inc(i);
  end;
  exit(false);
end;

procedure TGCompositeTest.SetVectorTest;
var sv:setvectorlli;
    v:vectorlli;
begin
  sv:=setvectorlli.create;
  v:=vectorlli.create;
  v.pushback(5);
  v.pushback(7);
  sv.insert(v);
  if sv.find(v) = nil then
    Fail('stuff not found');
  v:=vectorlli.create;
  v.pushback(5);
  v.pushback(7);
  if sv.find(v) = nil then
    Fail('equal stuff not found');
  v.pushback(9);
  if sv.find(v) <> nil then
    Fail('not equal stuff found');
end;

procedure TGCompositeTest.MatrixTest;
var i,j:longint;
begin
  data.resize(1000);
  for i:=0 to 999 do begin
    data[i] := vectorlli.create;
    data[i].resize(1000);
    data[i][0] := 1;
    data[0][i] := 1;
  end;
  for i:=1 to 999 do begin
    for j:=1 to 999 do begin
      data[i][j] := (data[i-1][j]+data[i][j-1]) mod 1000000009;
    end;
  end;
  AssertEquals('bad val 5 1', 6, data[5][1]);
  AssertEquals('bad val 5 2', 21, data[5][2]); 
  AssertEquals('bad val 5 5', 252, data[5][5]);
  AssertEquals('bad val 50 50', 933591892, data[50][50]);
end;

procedure TGCompositeTest.Setup;
begin
  data:=matrix.create;
end;

initialization
  RegisterTest(TGCompositeTest);
end.
