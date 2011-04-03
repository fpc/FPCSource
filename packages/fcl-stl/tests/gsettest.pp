{$mode objfpc}

unit gsettest;

interface

uses fpcunit, testregistry, gset, gutil;

type lesslli=specialize TLess<longint>;
     setlli=specialize TSet<longint,lesslli>;

type TGSetTest = class(TTestCase)
  Published
    procedure SetTest;
  public
    procedure Setup;override;
  private 
    data:setlli;
  end;

implementation

procedure TGSetTest.SetTest;
var it:setlli.pnode;
begin
  data.insert(3);
  data.insert(5);
  data.insert(7);
  AssertEquals('Wrong min', 3, data.min()^.data);
  AssertEquals('Wrong max', 7, data.max()^.data);
  data.delete(3);
  AssertEquals('Wrong size', 2, data.size);
  AssertEquals('Wrong min', 5, data.min()^.data);
  data.insert(3);
  data.insert(3);
  data.insert(3);
  AssertEquals('Wrong size', 3, data.size);
  AssertEquals('Wrong min', 3, data.min()^.data);
  if(data.find(4)<>nil) then 
    Fail('Found key which not there');
  if(data.find(5)=nil) then
    Fail('Not found key which was there');

  if(data.FindLess(8)^.data<>7) then
    Fail('Wrong less than 8');
  if(data.FindLess(7)^.data<>5) then
    Fail('Wrong less than 7');
  if(data.FindLess(3)<>nil) then
    Fail('Wrong less than 3');

  if(data.FindLessEqual(8)^.data<>7) then
    Fail('Wrong less equal than 8');
  if(data.FindLessEqual(7)^.data<>7) then
    Fail('Wrong less equal than 7');
  if(data.FindLessEqual(6)^.data<>5) then
    Fail('Wrong less equal than 6');
  if(data.FindLessEqual(2)<>nil) then
    Fail('Wrong less equal than 2');

  if(data.FindGreater(2)^.data<>3) then
    Fail('Wrong greater than 2');
  if(data.Findgreater(3)^.data<>5) then
    Fail('Wrong greater than 3');
  if(data.Findgreater(7)<>nil) then
    Fail('Wrong greater than 7');

  if(data.FindGreaterEqual(2)^.data<>3) then
    Fail('Wrong greater equal than 2');
  if(data.FindGreaterEqual(3)^.data<>3) then
    Fail('Wrong greater equal than 3');
  if(data.FindGreaterEqual(4)^.data<>5) then
    Fail('Wrong greater equal than 4');
  if(data.FindGreaterEqual(8)<>nil) then
    Fail('Wrong greater equal than 8');

  data.insert(17);

  it:=data.min;
  AssertEquals('Wrong min', 3, it^.data);
  it:=data.next(it);
  AssertEquals('Wrong next', 5, it^.data);
  it:=data.next(it);
  AssertEquals('Wrong next', 7, it^.data);
  it:=data.next(it);
  AssertEquals('Wrong next', 17, it^.data);
  it:=data.next(it);
  if(it<>nil) then
    AssertEquals('Last not nil', 0, 1);

  it:=data.max;
  AssertEquals('Wrong max', 17, it^.data);
  it:=data.prev(it);
  AssertEquals('Wrong prev', 7, it^.data);
  it:=data.prev(it);
  AssertEquals('Wrong prev', 5, it^.data);
  it:=data.prev(it);
  AssertEquals('Wrong prev', 3, it^.data);
  it:=data.prev(it);
  if(it<>nil) then
    AssertEquals('First not nil', 0, 1);
end;

procedure TGSetTest.Setup;
begin
  data:=setlli.create;
end;

initialization
  RegisterTest(TGSetTest);
end.
