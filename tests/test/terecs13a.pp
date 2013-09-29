{ %FAIL }
{ %NORUN }
program terecs13a;

{$ifdef fpc}
  {$mode delphi}
{$endif}

var
  R: record
    private const
      TestConst = 0;
  end;

begin
end.
