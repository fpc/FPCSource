program tw41011;
{$mode DELPHI}
uses
  Rtti
  ;

var
  ErrorCount: Integer;

procedure AreEqual(const AExpected, AActual: Double; const AMessage: string);
begin
  if Abs(AExpected - AActual) > 0.001 then
  begin
    WriteLn(AExpected, ' <> ', AActual, ': ', AMessage);
    Inc(ErrorCount);
  end;
end;

begin
  AreEqual(-10, TValue.From<Int8>(-10).Cast<Single>().AsType<Single>, 'TValue.From<Int8>(-10).Cast<Single>().AsType<Single>');
  AreEqual(205, TValue.From<UInt8>(205).Cast<Single>().AsType<Single>, 'TValue.From<UInt8>(205).Cast<Single>().AsType<Single>');
  AreEqual(-30012, TValue.From<Int16>(-30012).Cast<Single>().AsType<Single>, 'TValue.From<Int16>(-30012).Cast<Single>().AsType<Single>');
  AreEqual(60123, TValue.From<UInt16>(60123).Cast<Single>().AsType<Single>, 'TValue.From<UInt16>(60123).Cast<Single>().AsType<Single>');
  AreEqual(-12, TValue.From<Int32>(-12).Cast<Single>().AsType<Single>, 'TValue.From<Int32>(-12).Cast<Single>().AsType<Single>');
  AreEqual(42, TValue.From<Int32>(42).Cast<Single>().AsType<Single>, 'TValue.From<Int32>(42).Cast<Single>().AsType<Single>');

  AreEqual(-10, TValue.From<Int8>(-10).Cast<Double>().AsType<Double>, 'TValue.From<Int8>(-10).Cast<Double>().AsType<Double>');
  AreEqual(205, TValue.From<UInt8>(205).Cast<Double>().AsType<Double>, 'TValue.From<UInt8>(205).Cast<Double>().AsType<Double>');
  AreEqual(-30012, TValue.From<Int16>(-30012).Cast<Double>().AsType<Double>, 'TValue.From<Int16>(-30012).Cast<Double>().AsType<Double>');
  AreEqual(60123, TValue.From<UInt16>(60123).Cast<Double>().AsType<Double>, 'TValue.From<UInt16>(60123).Cast<Double>().AsType<Double>');
  AreEqual(-12, TValue.From<Int32>(-12).Cast<Double>().AsType<Double>, 'TValue.From<Int32>(-12).Cast<Double>().AsType<Double>');
  AreEqual(42, TValue.From<Int32>(42).Cast<Double>().AsType<Double>, 'TValue.From<Int32>(42).Cast<Double>().AsType<Double>');

  AreEqual(-10, TValue.From<Int8>(-10).Cast<Extended>().AsType<Extended>, 'TValue.From<Int8>(-10).Cast<Extended>().AsType<Extended>');
  AreEqual(205, TValue.From<UInt8>(205).Cast<Extended>().AsType<Extended>, 'TValue.From<UInt8>(205).Cast<Extended>().AsType<Extended>');
  AreEqual(-30012, TValue.From<Int16>(-30012).Cast<Extended>().AsType<Extended>, 'TValue.From<Int16>(-30012).Cast<Extended>().AsType<Extended>');
  AreEqual(60123, TValue.From<UInt16>(60123).Cast<Extended>().AsType<Extended>, 'TValue.From<UInt16>(60123).Cast<Extended>().AsType<Extended>');
  AreEqual(-12, TValue.From<Int32>(-12).Cast<Extended>().AsType<Extended>, 'TValue.From<Int32>(-12).Cast<Extended>().AsType<Extended>');
  AreEqual(42, TValue.From<Int32>(42).Cast<Extended>().AsType<Extended>, 'TValue.From<Int32>(42).Cast<Extended>().AsType<Extended>');

  AreEqual(45.9, TValue.From<Single>(45.9).Cast<Double>().AsType<Double>, 'TValue.From<Single>(45.9).Cast<Double>().AsType<Double>');
  AreEqual(45.9, TValue.From<Single>(45.9).Cast<Extended>().AsType<Extended>, 'TValue.From<Single>(45.9).Cast<Extended>().AsType<Extended>');
  AreEqual(-45689.46, TValue.From<Double>(-45689.46).Cast<Single>().AsType<Single>, 'TValue.From<Double>(-45689.46).Cast<Single>().AsType<Single>');
  AreEqual(-45689.46, TValue.From<Double>(-45689.46).Cast<Extended>().AsType<Extended>, 'TValue.From<Double>(-45689.46).Cast<Extended>().AsType<Extended>');
  AreEqual(662.546, TValue.From<Extended>(662.546).Cast<Single>().AsType<Single>, 'TValue.From<Extended>(662.546).Cast<Single>().AsType<Single>');
  AreEqual(662.546, TValue.From<Extended>(662.546).Cast<Double>().AsType<Double>, 'TValue.From<Extended>(662.546).Cast<Double>().AsType<Double>');

  if ErrorCount > 0 then
    Halt(ErrorCount);
end.