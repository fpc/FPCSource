{ %opt=-CR -S2 }

type
  tc1 = class
  end;

  tc2 = class(tc1)
  end;

procedure t(var c: tc2);
begin
end;

var
  c: tc1;
begin
  c := tc2.create;
  t(tc2(c));
end.
