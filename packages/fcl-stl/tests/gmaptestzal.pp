{$mode objfpc}

unit gmaptest;

interface

uses fpcunit, testregistry, gmap, gutil;

type lesslli=specialize TLess<longint>;
     maplli=specialize TMap<longint,longint, lesslli>;

type TGMapTest = class(TTestCase)
  Published
    procedure MapTest;
  public
    procedure Setup;override;
  private 
    data:maplli;
  end;

implementation

procedure TGMapTest.MapTest;
var it:maplli.TMSet.pnode;
begin
  data[3]:=3;
  data[5]:=5;
  data[7]:=7;
  AssertEquals('Wrong min key', 3, data.min()^.key);
  AssertEquals('Wrong max key', 7, data.max()^.key);
  AssertEquals('Wrong min val', 3, data.min()^.value);
  AssertEquals('Wrong max val', 7, data.max()^.value);

  AssertEquals('Wrong val', 5, data[5]);

  data.delete(3);
  AssertEquals('Wrong min key', 5, data.min()^.key);
  AssertEquals('Wrong max key', 7, data.max()^.key);
  AssertEquals('Wrong min val', 5, data.min()^.value);
  AssertEquals('Wrong max val', 7, data.max()^.value);


  data[3]:=3;
  data[3]:=47;
  AssertEquals('Wrong val 2', 47, data[3]);

  if(data.find(4)<>nil) then 
    AssertEquals('Found key which not there', 0, 1);

  data[17]:=42;

  it:=data.min;
  AssertEquals('Wrong min', 3, it^.key);
  it:=data.next(it);
  AssertEquals('Wrong next', 5, it^.key);
  it:=data.next(it);
  AssertEquals('Wrong next', 7, it^.key);
  it:=data.next(it);
  AssertEquals('Wrong next', 17, it^.key);
  it:=data.next(it);
  if(it<>nil) then
    AssertEquals('Last not nil', 0, 1);

  it:=data.max;
  AssertEquals('Wrong max', 17, it^.key);
  it:=data.prev(it);
  AssertEquals('Wrong prev', 7, it^.key);
  it:=data.prev(it);
  AssertEquals('Wrong prev', 5, it^.key);
  it:=data.prev(it);
  AssertEquals('Wrong prev', 3, it^.key);
  it:=data.prev(it);
  if(it<>nil) then
    AssertEquals('First not nil', 0, 1);
end;

procedure TGMapTest.Setup;
begin
  data:=maplli.create;
end;

initialization
  RegisterTest(TGMapTest);
end.
