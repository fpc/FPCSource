type
  tproc = procedure(self:longint);

procedure p(l:longint);
begin
end;

var
  pv : tproc;
begin
  pv:={$ifdef fpc}@{$endif}p;
end.
