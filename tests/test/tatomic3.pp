{ %FAIL }

program tatomic3;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
    f: LongInt;
    class operator Inc(const aArg: TTest): TTest;
  end;

class operator TTest.Inc(const aArg: TTest): TTest;
begin
  Result.f := aArg.f + 1;
end;

var
  t: TTest = ( f: 42 );
begin
  AtomicIncrement(t);
end.
