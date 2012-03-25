{ %opt=-O2 }
{$mode delphi}

type
  tc1 = class
  end;

  tc2 = class
  end;

var
  c: tobject;
begin
  c:=tc2.create;
  if (c is tc1) or
     (c is tc2) then
    halt(0);
  halt(1);
end.
