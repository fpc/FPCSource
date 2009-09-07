program scanf_example;

{$mode objfpc}{$H+}

uses
  gmp;

var n, sqr: mpz_t;

begin
  mpz_init(n);
  mpz_init(sqr);
  write('Please enter an integer of any length: ');
  if mp_scanf('%Zd', @n) = 1 then begin
    mpz_pow_ui(sqr, n, 2);
    mp_printf('%Zd^2 = %Zd'#10, @n, @sqr);
  end else
    writeln('Failed to parse an integer from your input');
  mpz_clear(sqr);
  mpz_clear(n);
end.

