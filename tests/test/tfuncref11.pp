{ %FAIL }

program tfuncref11;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

uses
  ufuncref10;

var
  l: LongInt;
  i: ITestFunc3;
begin
  { only the String Invoke is available }
  l := i();
end.
