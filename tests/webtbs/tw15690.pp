{$mode objfpc}

begin
  if tobject.inheritsfrom(nil) then
    halt(1);
end.
