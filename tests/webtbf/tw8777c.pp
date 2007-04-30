{ %fail }
program BugTest;

{$mode objfpc}

type
  TTest = class
  private
    FTest: Integer;
    procedure SetTest(const Value: Integer);
  public
    property Test: Integer read FTest write SetTest;
  end;

procedure p(var i : longint);
  begin
  end;

{ TTest }

procedure TTest.SetTest(const Value: Integer);
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
