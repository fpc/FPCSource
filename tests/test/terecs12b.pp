{ %FAIL }
{ %NORUN }
program terecs12b;

{$ifdef fpc}
  {$mode delphi}
{$endif}

procedure Test;
type
  TRecord = record
  var
    TestField: Integer;
    property TestProperty: Integer read TestField;
  end;
begin
end;

begin
end.