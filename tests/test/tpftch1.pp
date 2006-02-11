{ %OPT=-S2 -O1 }


{ check if we don't cause sigsegvs when attempting to prefetch if }
{ typecasts are involved                                          }

type
  tc1 = class
    a: tc1;
    x: boolean;
  end;

  tc2 = class(tc1)
    b: array[1..1000000000] of byte;
    c: tc1;
  end;

var
  a: tc1;

begin
  a := tc1.create;
  while assigned(a) do
    if a.x then
      a := tc2(a).c
    else
      a := a.a;
end.
