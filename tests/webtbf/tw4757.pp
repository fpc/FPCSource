{ %fail }

{$mode objfpc}

type
  tc1 = class
    fnext: tc1;
  end;

  tc2 = class(tc1)
    property next: tc2 read fnext write fnext;
  end;

begin
end.

