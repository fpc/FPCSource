program FloatCastTest;
{$mode Delphi}
uses Variants, Math;
var
  C: Currency;
  D, D2: Double;
  S: Single;
  Cp: Comp;
  V: Variant;
begin
  C := 5.5556;
  D := Double(C);
  S := Single(D);
  D2 := Single(D);
  Cp := Comp(D);
  V := Double(C);

  Writeln('C: ', C);
  Writeln('D: ', D);
  if not SameValue(C, D) then // C and D must be the same
    Halt(1);

  Writeln('D2:', D2);
  if SameValue(D, D2) then // D and D2 must be different -> precision reduced with Single() conversion
    Halt(2);

  Writeln('S: ', S);
  if not SameValue(D2, S) then // D2 and S must be the same
    Halt(3);

  Writeln('Cp:', Cp);
  if not SameValue(Cp, 6) then // Cp is 6
    Halt(4);

  Writeln('V:  ', V);
  if not SameValue(D, V) then // D and V must be the same
    Halt(5);
end.
