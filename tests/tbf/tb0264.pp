{ %FAIL }

program tb0264;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
  public
    a, b: LongInt;
  public type
    TSubType = record
    public const
      Test: TTest = (a: 42; b: 21);
    end;
  end;

begin

end.
