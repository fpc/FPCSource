{ The Computer Language Shootout
  http://shootout.alioth.debian.org

  contributed by Ales Katona
  modified by Vincent Snijders
}

program mandelbrot;

{$FPUTYPE SSE2}{$I-}

var n, x, y, bits,bit: Longint;
    Cx, Cy: double;

procedure CalculatePoint; nostackframe;
const
  Limit: double =4.0;
  zero: double = 0.0;
var
  i: longint;
  OutOfLimit: boolean;
  Cr, Ci, Zr, Zi, Ti, Tr: Double;
  
begin
  Cr := Cx; Ci := Cy;
  Zr := zero;  Zi := zero; Tr := zero; Ti := zero;
  i := 0;
  repeat
    Zi := 2*Zr*Zi + Ci;
    Zr := Tr - Ti + Cr;
    Ti := Zi * Zi;
    Tr := Zr * Zr;
    inc(i);
    OutOfLimit := (Tr + Ti>=limit);
  until OutOfLimit or (i=50);

  if OutOfLimit then
    bits := bits xor bit;
end;

{$FPUTYPE X87}

begin
  Val(ParamStr(1), n);
  writeln('P4');
  writeln(n,' ',n);
  for y := 0 to n-1 do
  begin
    Cy := y * 2 / n - 1;
    bits := 255;  bit := 128;
    for x := 0 to n-1 do
    begin
      Cx := x * 2 / n  - 1.5;

      CalculatePoint;

      if bit > 1 then
        bit := bit shr 1
      else
      begin
        write(chr(bits));
        bits := 255;  bit := 128;
      end;
    end;
    if bit < 128 then write(chr(bits xor((bit shl 1)-1)));
  end;
end.
