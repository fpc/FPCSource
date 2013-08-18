{ %NORUN }
program tw24865;

{$mode delphi}

type
  TTest = class
  public
    class var fc3: integer;
    class procedure c1();
    class procedure c2(); static;
    class property c3: integer read fc3 write fc3;
  end;

class procedure TTest.c1;
begin
end;

class procedure TTest.c2;

  function nested: integer;
  begin
    c1;
    fc3 := 1;
    c3 := 2;
    result := c3;
  end;

begin
end;

begin
end.
