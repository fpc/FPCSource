{$MODE ISO}
{$Q+}
program nabs(input, output);
  type halfword = 0..65535;
  var a, b: halfword;
begin
  a := 4095;
  b := 4096;
  if abs(int(a+b-8192)) < 4096 then halt(0) else halt(1);
end.
