{ %OPT=-Oodfa -vw -Sew -vm6060 }
{ %FAIL }

{$mode objfpc}

program project1;

type
  trange=0..5;

function f(r: trange): longint;
begin
  { must give a warning about unset function result; warning about incomplete
    case statement is suppressed with -vm6060 }
  case r of
    0..4: result:=r;
  end;
end;

begin
  writeln(f(2));
end.
