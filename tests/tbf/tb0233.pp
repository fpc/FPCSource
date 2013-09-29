{ %FAIL }

program tb0233;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
    class operator + (aLeft, aRight: TTest): TTest; static;
  end;

class operator TTest.+(aLeft, aRight: TTest): TTest;
begin

end;

begin

end.
