{$mode objfpc}

unit gdequetest;

interface

uses fpcunit, testregistry, gdeque;

type dequelli=specialize TDeque<longint>;

type TGDequeTest = class(TTestCase)
  Published
    procedure BackTest;
    procedure PushTest;
  public
    procedure Setup;override;
  private
    data:dequelli;
  end;

implementation

procedure TGDequeTest.BackTest;
var i:longint;
begin
  AssertEquals('Not IsEmpty', true, data.IsEmpty);
  for i:=0 to 10 do
    data.pushback(i);
  for i:=0 to 10 do begin
    AssertEquals('Wrong data', 10-i, data.back);
    AssertEquals('Wrong size', 11-i, SizeInt(data.size));
    data.popback;
  end;
  AssertEquals('Not IsEmpty', true, data.IsEmpty);
end;

procedure TGDequeTest.PushTest;
var i:longint;
begin
  for i:=6 to 10 do
    data.pushBack(i);
  for i:=5 downto 0 do
    data.pushFront(i);
  for i:=0 to 10 do
    AssertEquals('Wrong data', i, data[i]);
end;

procedure TGDequeTest.Setup;
begin
  data:=dequelli.create;
end;

initialization
  RegisterTest(TGDequeTest);
end.
