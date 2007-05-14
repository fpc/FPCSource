{ %fail }

{ from GPC testsuite }

program fjf569i;

procedure foo (const a: String);
begin
  WriteStr (a, '')  { WRONG }
end;

begin
  WriteLn ('')
end.

