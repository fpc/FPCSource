{$q+}
{$r+}

Var A, B, C : Byte;
    D : SmallInt;
Begin
A := 6;
B := 8;
C := 20;
D := -C+A+B;
if (d<>-6) then
  halt(1);
Writeln(D);
D := A+B-C;
if (d<>-6) then
  halt(1);
Writeln(D)
End.
