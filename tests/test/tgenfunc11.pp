{ %FAIL }

program tgenfunc11;

{$mode objfpc}

type
  TTest = class
    generic procedure Test<T>; virtual;
  end;

generic procedure TTest.Test<T>;
begin
end;

begin

end.
