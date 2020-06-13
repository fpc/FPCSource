{$mode objfpc}

unit gpriorityqueuetest;

interface

uses fpcunit, testregistry, gpriorityqueue, gutil;

type lesslli=specialize TLess<longint>;
     queuelli=specialize TPriorityQueue<longint,lesslli>;

type TGPQueueTest = class(TTestCase)
  Published
    procedure QueueTest;
  public
    procedure Setup;override;
  private
    data:queuelli;
  end;

implementation

procedure TGPQueueTest.QueueTest;
var i,last:longint;
begin
  AssertEquals('Not IsEmpty', true, data.IsEmpty);
  for i:=0 to 10 do
    data.push(random(10000));
  last:=data.top;
  data.pop;
  for i:=0 to 9 do begin
    AssertEquals('Wrong order', true, data.top<last);
    AssertEquals('Wrong size', 10-i, SizeInt(data.size));
    last:=data.top;
    data.pop;
  end;
  AssertEquals('Not IsEmpty', true, data.IsEmpty);
end;

procedure TGPQueueTest.Setup;
begin
  data:=queuelli.create;
end;

initialization
  RegisterTest(TGPQueueTest);
end.
