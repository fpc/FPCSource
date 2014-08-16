uses
  sysutils;

var b: extended;
    s: string;
begin
{$ifdef FPC_SUPPORTS_EXTENDED}
  b := 999999999999999999;
  writeln(b:25:25);
  str(b:25:25,s);
  if s<>'999999999999999999.0000000000000000000000000' then
    halt(1);
  writeln(FloatToStrF(b, ffFixed, 25, 0));
  if FloatToStrF(b, ffFixed, 25, 0)<>'999999999999999999' then
    halt(2);
{$endif}
end.

