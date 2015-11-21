{ %FAIL }

{ constraints must not be repeated in the definition }

program tgenfunc13;

{$mode objfpc}

type
  TTest = class
    generic procedure Test<T: class>;
  end;

generic procedure TTest.Test<T: class>;
begin

end;

begin

end.
