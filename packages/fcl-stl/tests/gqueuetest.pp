{$mode objfpc}

unit gqueuetest;

interface

uses fpcunit, testregistry, gqueue;

type TQueuelli=specialize TQueue<longint>;

type TGTQueueTest = class(TTestCase)
  Published
    procedure TQueueTest;
  public
    procedure Setup;override;
  private 
    data:TQueuelli;
  end;

implementation

procedure TGTQueueTest.TQueueTest;
var i:longint;
begin
  AssertEquals('Not IsEmpty', true, data.IsEmpty);
  for i:=0 to 10 do
    data.push(i);
  for i:=0 to 10 do begin
    AssertEquals('Wrong data', i, data.front);
    AssertEquals('Wrong size', 11-i, data.size);
    data.pop;
  end;
  AssertEquals('Not IsEmpty', true, data.IsEmpty);
end;

procedure TGTQueueTest.Setup;
begin
  data:=TQueuelli.create;
end;

initialization
  RegisterTest(TGTQueueTest);
end.
