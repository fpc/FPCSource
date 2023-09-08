{ %FAIL }

program thintdir3a;

{$mode objfpc}
{$warn 5043 error}

type
  TTest = 1..9 deprecated;

  TRec = record
    f: array of TTest;
  end;

begin

end.
