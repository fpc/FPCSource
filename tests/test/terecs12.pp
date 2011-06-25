{ %FAIL }
{ %NORUN }
program terecs12;

{$ifdef fpc}
  {$mode delphi}
{$endif}

procedure Test;
type
  TRecord = record
  private type
    TTestRange = 0..42;
  end;
begin
end;

begin
end.