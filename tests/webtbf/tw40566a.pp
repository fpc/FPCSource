{ %fail }
{$mode objfpc}

program Project1;
type
  Tbar = type class
    f:integer;
  end;

  // tabc = specialize TBar<integer>; // Internal error 2012101001

begin
end.
