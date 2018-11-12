{ %FAIL }

program tb0262;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
  public
    a, b: LongInt;
  public const
    Test: array[0..1] of TTest = ((a: 42; b: 21), (a: 21; b: 42));
  end;

begin

end.
