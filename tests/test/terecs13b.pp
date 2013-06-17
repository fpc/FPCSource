{ %FAIL }
{ %NORUN }
program terecs13b;

{$ifdef fpc}
  {$mode delphi}
{$endif}

var
  R: record
    var
      TestField: Integer;
      property TestProperty: Integer read TestField;
  end;

begin
end.
