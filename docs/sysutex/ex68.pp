Program Example68;

{ This program demonstrates the FloatToStrF function }

Uses sysutils;

Const Fmt : Array [TFloatFormat] of string[10] =
         ('general','exponent','fixed','number','Currency');

Procedure Testit (Value :  Extended);

Var I,J : longint;
    FF : TFloatFormat;

begin
  For I:=5 to 15 do
    For J:=1 to 4 do
      For FF:=ffgeneral to ffcurrency do
        begin
        Write (Value,'(Prec: ',I:2,', Dig: ',J,', fmt : ',Fmt[ff],') : ');
        Writeln (FloatToStrf(Value,FF,I,J));
        Write (-Value,'(Prec: ',I:2,', Dig: ',J,', fmt : ',Fmt[ff],') : ');
        Writeln (FloatToStrf(-Value,FF,I,J));
        end;
end;

Begin
  Testit (1.1);
  Testit (1.1E1);
  Testit (1.1E-1);
  Testit (1.1E5);
  Testit (1.1E-5);
  Testit (1.1E10);
  Testit (1.1E-10);
  Testit (1.1E15);
  Testit (1.1E-15);
  Testit (1.1E100);
  Testit (1.1E-100);
End.