{$mode objfpc}

unit gstacktest;

interface

uses fpcunit, testregistry, gstack;

type TStacklli=specialize TStack<longint>;

type TGTStackTest = class(TTestCase)
  Published
    procedure TStackTest;
  public
    procedure Setup;override;
  private 
    data:TStacklli;
  end;

implementation

procedure TGTStackTest.TStackTest;
var i:longint;
begin
  AssertEquals('Not IsEmpty', true, data.IsEmpty);
  for i:=0 to 10 do
    data.push(i);
  for i:=0 to 10 do begin
    AssertEquals('Wrong data', 10-i, data.top);
    AssertEquals('Wrong size', 11-i, data.size);
    data.pop;
  end;
  AssertEquals('Not IsEmpty', true, data.IsEmpty);
end;

procedure TGTStackTest.Setup;
begin
  data:=TStacklli.create;
end;

initialization
  RegisterTest(TGTStackTest);
end.
