{ %FAIL }

program thintdir4b;

{$mode objfpc}
{$warn 5043 error}

type
  TTest = 1..9 deprecated;

  TTestArray = array of array of TTest;

begin

end.
