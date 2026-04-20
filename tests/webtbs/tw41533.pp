program FloatCastTest;
{$mode Delphi}
uses Variants, Math;

{$ifndef COMP_IS_INT64}
  {$define TEST_COMP_REAL_TYPE}
{$endif}
var
  C: Currency;
  D, D2: Double;
  S: Single;
  V: Variant;
{$ifdef TEST_COMP_REAL_TYPE}
  Cp: Comp;
  I: int64;
{$endif}

begin
  C := 5.5556;
  D := Double(C);
  S := Single(D);
  D2 := Single(D);
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

  Writeln('V:  ', V);
  if not SameValue(D, V) then // D and V must be the same
    Halt(4);

{$ifdef TEST_COMP_REAL_TYPE}
  Cp := Comp(D);
  Writeln('Cp:', Cp);
  if not SameValue(Cp, 6) then // Cp is 6
    Halt(5);
{$else not TEST_COMP_REAL_TYPE}
  Cp := Comp(D);
  I := int64(D);
  Writeln('Cp:', Cp);
  Writeln('I:', I);
  if not SameValue(Cp, I) then // Cp is a binary copy of D bit-pattern, as is I
    Halt(6);
{$endif not TEST_COMP_REAL_TYPE}
end.
