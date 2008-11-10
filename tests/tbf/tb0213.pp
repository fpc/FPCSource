{ %fail }
{$mode objfpc}
type
  tc1 = class
    fp : longint;
    class procedure p;
    property prop : longint read fp;
  end;

  class procedure tc1.p;
    begin
      writeln(prop);
    end;

begin
end.
