{$mode tp}

type
  tproc=procedure;
var
  p : tproc;

function f:tproc;
begin
  exit(p);
end;

begin
end.
