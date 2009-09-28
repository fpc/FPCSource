program printf_example2;

{$mode objfpc}{$H+}

uses
  sysutils, math, gmp;

const
  N = 2;
  NBITS = 100;

var
  f: mpfloat;
  bits, digits: integer;
begin
  f_set_default_prec(NBITS);
  bits := f_get_default_prec;
  digits := floor(bits * LOG_10_2);
  f := f_sqrt_ui(N);
  writeln(format('Sqrt(%d) to %d digits (%d bits) = %s', [N, digits, bits, string(f)]));
  f := f ** 2;
  writeln(format('Squared back = %s', [string(f)]));
  f -= N;
  writeln(format('Minus %d = %s', [N, string(f)]));
end.

