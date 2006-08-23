{$bitpacking on}
program sam8;

{ from the gpc testsuite (sam8.pas) }

{
 Using a subrange type in a packed recrod seems to break passing the type
 by reference
}
type
  t1 = 0..100; {Has to be subrange}

r1 = packed record { Has to be packed }
  f1 : t1;
end;

procedure proc1(var a1: t1); { Has to be var }
begin
  if a1 = 42 then writeln ('OK') else writeln ('failed')
end;

procedure proc2(var a1: t1); { Also has to be var }
begin
proc1(a1);
end;

var a1 : t1;

begin
  a1 := 42;
  proc2 (a1)
end.
