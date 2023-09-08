{ %FAIL }

program thintdir4a;

{$mode objfpc}
{$warn 5043 error}

type
  TTest = 1..9 deprecated;

  TTestArray = array[TTest] of LongInt;

begin

end.
