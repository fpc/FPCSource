{ %FAIL }
{ %NORUN }
program terecs13c;

{$ifdef fpc}
  {$mode delphi}
{$endif}

var
  R: record
    class var
      TestField: Integer;
  end;

begin
end.
