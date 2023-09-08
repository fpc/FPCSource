{ %FAIL }

program thintdir3b;

{$mode objfpc}
{$warn 5043 error}

type
  TTest = 1..9 deprecated;

  TRec = record
    f: array of array of TTest;
  end;

begin

end.
