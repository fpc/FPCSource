{ The Computer Language Benchmarks Game
  http://shootout.alioth.debian.org

  contributed by Vincent Snijders
  gmp headers by Karl-Michael Schindler
}

{$mode objfpc}

program pidigits_example;

uses
  gmp;

procedure PrintPiDigits(NumDigits: integer);
var
  q, r, s, t: mpz_t; // Transformation matrix components.
  u, v, w: mpz_t; // Temporary variables
  i, k, digit, c: integer;
  line: string[10];

  function Extract(x:cardinal): integer;
  begin
    mpz_mul_ui(u, q, x);
    mpz_add(u, u, r);
    mpz_mul_ui(v, s, x);
    mpz_add(v, v, t);
    mpz_tdiv_q(w, u, v);
    result := mpz_get_ui(w);
  end;

  function IsSafe: boolean;
  begin
    result := digit = Extract(4);
  end;

  procedure Produce;
  begin
    mpz_mul_si(r, r, 10);
    mpz_mul_si(v, t, -10 * digit);
    mpz_add(r, r, v);
    mpz_mul_si(q, q, 10);
  end;

  procedure Consume;
  begin
    inc(k);
    mpz_mul_si(r, r, 2*k+1);
    mpz_mul_si(u, q, 4*k+2);
    mpz_add(r, r, u);
    mpz_mul_si(t, t, 2*k+1);
    mpz_mul_si(v, s, 4*k+2);
    mpz_add(t, t, v);
    mpz_mul_si(s, s, k);
    mpz_mul_si(q, q, k);
  end;

begin
  k := 0;
  i := 0;
  c := 0;
  setlength(line, 10);
  mpz_init_set_ui(q, 1);
  mpz_init_set_ui(r, 0);
  mpz_init_set_ui(s, 0);
  mpz_init_set_ui(t, 1);
  mpz_init(u);
  mpz_init(v);
  mpz_init(w);
  while (i < NumDigits) do begin
    digit := Extract(3);
    while not IsSafe do begin
      Consume;
      digit:= Extract(3);
    end;
    Produce;
    inc(c);
    line[c] := chr(ord('0') + digit);
    inc(i);
    if c = 10 then begin
      writeln(line, #9':', i);
      c := 0;
    end;
  end;
  if c <> 0 then begin
    SetLength(line, c);
    writeln(line);
  end;
  mpz_clear(q);
  mpz_clear(r);
  mpz_clear(s);
  mpz_clear(t);
  mpz_clear(u);
  mpz_clear(v);
  mpz_clear(w);
end;

var
  n: integer = 27;

begin
  if (ParamCount = 1) then
    val(ParamStr(1), n);
  PrintPiDigits(n);
end.

