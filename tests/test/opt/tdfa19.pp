{ %OPT=-Oodfa -vw -Sew }
{ %norun }

{$mode objfpc}

program project1;

type
  trange=0..5;

function f(r: trange): longint;
begin
  case r of
    0..5: result:=r;
  end;
end;

begin
  writeln(f(2));
end.
