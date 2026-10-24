{ %FAIL }

{ non-constant default parameter value in a program-level routine
  must give an error, not crash in set_varstate }
program tb0299;

{$mode objfpc}

var
  a: integer;

procedure p(b: byte = 1 + a);
begin
end;

begin
end.
