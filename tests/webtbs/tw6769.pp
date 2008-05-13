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
{$ifndef cpu64}
{ On 32 bit systems, A+B becomes cardinal and the -C turns the expression
  into int64 -> calculated ok.
  On 64 bit systems, A+B becomes qword and the -C keeps it qword ->
  overflow error. This can only be properly supported in 64 bit with the
  introduction of a 128 bit signed type, except if we'd use a different
  rule set in 64 bit (such as byte+byte -> cardinal and cardinal+cardinal
  -> qword, or so)
}
D := A+B-C;
if (d<>-6) then
  halt(1);
Writeln(D)
{$endif cpu64}
End.
