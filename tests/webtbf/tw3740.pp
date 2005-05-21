{ %fail }

program fjf915a;

type
  t = object
    constructor Init (a: Boolean);
  end;

constructor t.Init (a: Boolean);
begin
end;

var
  p: ^t;

begin
  New (p, Init (Init (False)))  { WRONG }
end.
