{ %FAIL }
{ %NORUN }
program terecs12a;

{$ifdef fpc}
  {$mode delphi}
{$endif}

procedure Test;
type
  TRecord = record
  private const
    TestConst = 0;
  end;
begin
end;

begin
end.