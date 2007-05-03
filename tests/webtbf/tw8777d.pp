{ %fail }
{ %norun }

{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  TTest = class
  private
    FTest: Integer;
    procedure SetTest(const Value: Integer);
  public
    property Test: Integer read FTest write SetTest;
  end;

{ TTest }

procedure TTest.SetTest(const Value: Integer);
begin
//  Writeln('SetTest called!');
//  FTest := Value;
end;

var
  Test: TTest;
  f: text;

begin

  Test := TTest.Create;
{
  Writeln('Test.Test = ', Test.Test);
  Test.Test := 2;
  Writeln('Test.Test = ', Test.Test);
}
  ReadLn(f,Test.Test);
//  Writeln('Test.Test = ', Test.Test);

//  ReadLn;
end.

