{ The Computer Language Benchmarks Game
  http://shootout.alioth.debian.org

  contributed by Vincent Snijders
  gmp headers by Karl-Michael Schindler
}

{$mode objfpc}

program pidigits_example2;

uses
  gmp;

procedure PrintPiDigits(NumDigits: integer);
var
  q, r, s, t: MPInteger;  // Transformation matrix components.
  i, k, digit, c: integer;
  line: string[10];

  function Extract(x: cardinal): integer;
  begin
    result := (q * x + r) / (s * x + t);
  end;

  function IsSafe: boolean;
  begin
    result := digit = Extract(4);
  end;

  procedure Produce;
  begin
    r := 10 * (r - t * digit);
    q *= 10;
  end;

  procedure Consume;
  begin
    inc(k);
    r := r * (2 * k + 1) + q * (4 * k + 2);
    t := t * (2 * k + 1) + s * (4 * k + 2);
    s *= k;
    q *= k;
  end;

begin
  k := 0;
  i := 0;
  c := 0;
  setlength(line, 10);
  q := 1;
  r := 0;
  s := 0;
  t := 1;
  while (i < NumDigits) do begin
    digit := Extract(3);
    while not IsSafe do begin
      Consume;
      digit := Extract(3);
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
end;

var
  n: integer = 27;

begin
  if (ParamCount = 1) then
    val(ParamStr(1), n);
  PrintPiDigits(n);
end.

