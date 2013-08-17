{ %NORUN }
program tw24865;

{$mode delphi}

type
  TTest = class
    class procedure c1();
    class procedure c2(); static;
  end;

class procedure TTest.c1;
begin
end;

class procedure TTest.c2;
  procedure nested;
  begin
    c1;
  end;

begin
end;

begin
end.

