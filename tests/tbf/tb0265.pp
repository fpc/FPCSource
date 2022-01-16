{ %FAIL }

program tb0265;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
  public
    a, b: LongInt;
  public const
    Test: array of record
      t: TTest;
    end = ((t: (a: 42; b: 21)), (t: (a: 21; b: 42)));
  end;

begin

end.
