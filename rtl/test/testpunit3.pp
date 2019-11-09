{$mode objfpc}

program testpunit2;

uses punit, sysutils;

Function DoTest : AnsiString;

begin
  Result:='test failed';
end;

begin
  SetTimeHook(@SysUtils.Now);
  RunTest(@DoTest);
end.

