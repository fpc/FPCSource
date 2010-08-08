{ %fail }
program tassignmentoperator1;

{$mode objfpc}

operator := (S1, S2: String): Integer;
begin
  Result := Length(S1);
end;

var
  S: String;
  V: Integer;
begin
  V := S;
end.

