{ %NORUN }

program tb0680;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

type
  TTest = record
    class operator + (aLeft, aRight: TTest): TTest;
    function Plus(aLeft, aRight: TTest): TTest;
  end;

class operator TTest.+(aLeft, aRight: TTest): TTest;
begin

end;

function TTest.Plus(aLeft, aRight: TTest): TTest;
begin

end;

begin

end.
