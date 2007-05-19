{ %fail }
program BugTest;

{$mode objfpc}

type
  TTest = class
  private
    FTest: Byte;
    procedure SetTest(const Value: Byte);
  public
    property Test: Byte read FTest write SetTest;
  end;

procedure p(var i : byte);
  begin
  end;

{ TTest }

procedure TTest.SetTest(const Value: Byte);
begin
  Writeln('SetTest called!');
  FTest := Value;
end;

var
  Test: TTest;

begin
  Test := TTest.Create;
  Test.Test := 2;
  ReadLn(Test.Test);
end.
