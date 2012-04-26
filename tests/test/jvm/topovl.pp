{$mode delphi}

program topovl;

type
  complex = record
    re,im: real;

    class operator multiply(r : real; const z1 : complex): complex;
  end;

class Operator complex.multiply (r : real; const z1 : complex): complex;

begin
  result.re := z1.re * r;
  result.im := z1.im * r;
end;

var
  R : real;
  C,Z : complex;

begin
  r:=2.0;
  c.re:=3.0;
  c.im:=4.0;
  C:=R*Z;
end.

