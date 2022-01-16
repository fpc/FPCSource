{ %FAIL }

program terecs21;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = class
  public threadvar
    Test: LongInt;
  end;

begin
end.
