{ %FAIL }

program tb0263;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
  public
    a, b: LongInt;
  public const
    Test: array[0..1] of record
      t: TTest;
    end = ((t: (a: 42; b: 21)), (t: (a: 21; b: 42)));
  end;

begin

end.
