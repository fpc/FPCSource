{ %FAIL }
{ %NORUN }
program terecs13;

{$ifdef fpc}
  {$mode delphi}
{$endif}

var
  R: record
    private type
      TTestRange = 0..42;
  end;

begin
end.
