{ %fail }
{$CODEALIGN 3}
{$CODEALIGN PROC=3}

program test1;

var
  v: integer = 1;

procedure A;
begin
  Inc(v);
end;

procedure B;
begin
  Dec(v);
end;

begin
  A;
  B;
end.

