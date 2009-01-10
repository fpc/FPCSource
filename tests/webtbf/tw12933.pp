{ %fail }

{$mode objfpc}

type
  ta = interface
  end;

  tb = interface(ta)
  end;

procedure test(var a: ta);
begin
end;

var
  b: tb;
begin
  test(b);
end.
