program tgenfunc9;

uses
  ugenfunc7;

var
  t: TTest;
begin
  t := TTest.Create;
  Writeln(t.specialize GetStrictPrivate<LongInt>);
  Writeln(t.specialize GetPrivate<LongInt>);
  Writeln(t.specialize GetStrictProtected<LongInt>);
  Writeln(t.specialize GetProtected<LongInt>);
end.
