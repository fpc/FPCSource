{ %opt=-O4 }
program project1;

// {$O-}

procedure vec3(y: single);
begin
{$i-}  WriteLn(y);
end;

var
  u: single = 0;
const
  achse = 3.0;
begin
  vec3(u + 5 - achse);
end.
