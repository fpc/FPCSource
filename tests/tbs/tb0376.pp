{%OPT=-Sew}
{$mode objfpc}

function f: longint;
var
   a: longint absolute result;
begin
   a := 5;
end;

begin
  if f<>5 then
   begin
     writeln('error!');
     halt(1);
   end;
end.
