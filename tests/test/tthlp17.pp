{ %FAIL }

{ method usage should fail if no helper is in scope }

program tthlp17;

{$mode objfpc}

var
  i: LongInt;
begin
  i.Test;
end.
