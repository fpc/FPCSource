Program Example67;

{ This program demonstrates the FloatToStr function }

Uses sysutils;

Procedure Testit (Value : Extended);

begin
  Writeln (Value,' -> ',FloatToStr(Value));
  Writeln (-Value,' -> ',FloatToStr(-Value));
end;

Begin
  Testit (0.0);
  Testit (1.1);
  Testit (1.1e-3);
  Testit (1.1e-20);
  Testit (1.1e-200);
  Testit (1.1e+3);
  Testit (1.1e+20);
  Testit (1.1e+200);
End.