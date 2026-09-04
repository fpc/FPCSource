{ %NORUN }

program tgeneric131;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  generic TCurve<T> = record
    Parameters: T;
    function Sample1(Progress: Double): LongInt;
    function Sample2(Progress: Double): LongInt;
  end;

function TCurve.Sample1(Progress: Double): LongInt;
begin
  Result := Parameters.Evaluate(Progress);
end;

function TCurve.Sample2(Progress: Double): LongInt;
begin
  Result := Parameters.Evaluate2();
end;

begin
end.
