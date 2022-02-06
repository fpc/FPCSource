{ %FAIL }

program tfuncref12;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

uses
  ufuncref10;

var
  l: LongInt;
  i: ITestFunc4;
begin
  { both are available, but since overloads by result type are not possible
    only the last one is available }
  l := i();
end.
