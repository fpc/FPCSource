{$mode objfpc}

program testpunit;

uses punit;

Function DoTest : AnsiString;

begin
  Result:='test failed';
end;

begin
  RunTest(@DoTest);
end.

