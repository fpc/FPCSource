uses
  sysutils, math;

begin
  DecimalSeparator:=',';
  if FloatToStrF(nan, ffExponent, 15, 1)<>'Nan' then
    halt(1);
  if FloatToStrF(1.3, ffExponent, 15, 1)[2]<>',' then
    halt(1);
  writeln('ok');
end.
