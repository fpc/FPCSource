{ %FAIL }
program trecinit1;

{$mode delphi}
{$ModeSwitch advancedrecords}

type
  TTestRec = record
  A: Integer;
  property B: Integer read A;
  end;

const
  rec: TTestRec = (A:42; B:32);
begin
end.
