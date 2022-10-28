unit ugeneric108a;

{$mode objfpc}
{$modeswitch advancedrecords}

interface

type
  generic TTest<T> = record
    f: T;
    function Test: LongInt;
    class function Test2: LongInt; static;
  end;

implementation

function TTest.Test: LongInt;
begin
  Result := 1;
end;

class function TTest.Test2: LongInt;
begin
  Result := 1;
end;

end.

