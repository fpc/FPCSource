{ %fail }
{$mode objfpc}

type
  ta = class
  end;

  tb = class(ta)
  end;

procedure test(var a: ta);
begin
  a.free;
  a:=ta.create;
  // now b contains an instance of type "ta"
end;

var
  b: tb;
begin
  test(b);
end.