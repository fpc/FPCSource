type
  tproc = procedure(self,l2:longint);

procedure p(l1,l2:longint);
begin
end;

var
  pv : tproc;
begin
  pv:={$ifdef fpc}@{$endif}p;
end.
