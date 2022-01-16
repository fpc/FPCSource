{$mode objfpc}
var
  counter : longint;

function f : Integer;
  begin
    inc(counter);
    f:=1;
  end;


var
  s : array[0..10] of Integer;

begin
  s[1]:=1234;
  counter:=0;

  s[f]:=succ(s[f]);
  s[f]:=pred(s[f]);

  if s[1]<>1234 then
    halt(1);

  if counter<>4 then
    halt(2);

  writeln('ok');
end.


