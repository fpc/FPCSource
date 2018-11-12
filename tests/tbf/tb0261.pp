{ %FAIL }

program tb0261;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
  public
    a, b: LongInt;
  public const
    Test: TTest = (a: 42; b: 21);
  end;

begin

end.
