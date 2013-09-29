program tb0591;

uses
  Math;

procedure TestValue(aActual, aExpected: Double);
begin
  if not SameValue(aActual, aExpected) then
    Halt(1);
end;

const
  f1 = 2.;
  f2 = 2.e10;
  f3 = 2.e-10;
  f4 = 2.e+10;
  f5 = 2.8e10; // ensure that scanning of normal floating points is not broken

begin
  TestValue(2., 2.0);
  TestValue(2.e10, 2.0e10);
  TestValue(2.e-10, 2.0e-10);
  TestValue(2.e+10, 2.0e+10);

  TestValue(f1, 2.0);
  TestValue(f2, 2.0e10);
  TestValue(f3, 2.0e-10);
  TestValue(f4, 2.0e+10);
end.
