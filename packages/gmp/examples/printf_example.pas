program printf_example;

{$mode objfpc}{$H+}

uses
  math, gmp;

const
  N = 2;
  NBITS = 100;

var
  f: mpf_t;
  bits, digits: integer;
begin
  mpf_init2(f, NBITS);
  bits := mpf_get_prec(f);
  digits := floor(bits * LOG_10_2);
  mpf_sqrt_ui(f, N);
  mp_printf('Sqrt(%d) to %d digits (%d bits) = %.*Fg'#10, N, digits, bits, digits, @f);
  mpf_pow_ui(f, f, 2);
  mp_printf('Squared back = %.*Fg'#10, digits, @f);
  mpf_sub_ui(f, f, N);
  mp_printf('Minus %d = %.*Fg'#10, N, digits, @f);
  mpf_clear(f);
end.

