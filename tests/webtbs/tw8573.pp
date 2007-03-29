program overflowbug;

{$mode objfpc}{$Q+}

const
  zero=0;
  one=1;

var
  x,y,z: cardinal;

begin
  x := 0;
  y := one + x;

  // the next line sets the carry flag, so a overflow error will be generated
  if x>y then;
  // here the overflow error will be generated.
  // the addition of zero is optimized away, but the check for the carry flag
  // is not removed, so it is using the result of the compile in line 17
  z := zero + y;
end.

