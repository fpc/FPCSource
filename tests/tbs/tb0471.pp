{$mode delphi}

const
  err : boolean = true;

type
  tf = function:longint;
procedure p1(l:longint);overload;
begin
  writeln('longint');
end;


procedure p1(f:tf);overload;
begin
  writeln('procvar');
  err:=false;
end;

function vf:longint;
begin
  vf:=10;
end;

var
  v : tf;
begin
  v:=vf;
  p1(v);
  if err then
    halt(1);
end.
