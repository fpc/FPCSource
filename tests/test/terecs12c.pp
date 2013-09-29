{ %FAIL }
{ %NORUN }
program terecs12c;

{$ifdef fpc}
  {$mode delphi}
{$endif}

procedure Test;
type
  TRecord = record
  class var
    TestField: Integer;
  end;
begin
end;

begin
end.