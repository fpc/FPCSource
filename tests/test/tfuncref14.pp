{ %FAIL }

program tfuncref14;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

uses
  ufuncref10;

var
  l: LongInt;
  i: ITestFunc7;
begin
  { if it doesn't inherit from a function reference then it can't be called
    directly }
  l := i();
end.
