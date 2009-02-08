uses
  stdconvs,math;
begin
  if CompareValue(FahrenheitToCelsius(32.0),0.0,0.000000001)<>EqualsValue then
    halt(1);
  if CompareValue(FahrenheitToCelsius(89.6),32.0,0.000000001)<>EqualsValue then
    halt(2);
  if CompareValue(CelsiusToFahrenheit(32.0),89.6,0.000000001)<>EqualsValue then
    halt(3);
  if CompareValue(CelsiusToFahrenheit(0.0),32.0,0.000000001)<>EqualsValue then
    halt(4);
end.
