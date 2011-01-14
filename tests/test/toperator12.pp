program toperator12;

{$ifdef FPC}
  {$mode Delphi}
{$endif}

type
  R = record
    F: Integer;
    class operator Implicit(const v: integer): R;
  end;

class operator R.Implicit(const v: integer): R;
begin
  Result.F := v;
end;

var
  x: R;
begin
  x := 42;
  if x.F <> 42 then
    halt(1);
end.
