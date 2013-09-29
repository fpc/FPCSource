{ %FAIL }

program terecs19;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TRecord = record
  strict protected
    f1: LongInt;
  protected
    f2: LongInt;
  end;

begin

end.
