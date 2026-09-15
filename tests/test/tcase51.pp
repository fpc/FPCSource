{ ISO Pascal: otherwise starts the else branch of a case statement }
{$mode iso}
program tcase51(output);

var
  i, r: integer;
begin
  r:=0;
  i:=5;
  case i of
    1, 2: r:=1;
    3: ;
    otherwise
      r:=2;
      r:=r+1
  end;
  if r<>3 then
    halt(1);
  case i of
    1: r:=4;
    otherwise
  end;
  if r<>3 then
    halt(2);
  writeln('ok');
end.
