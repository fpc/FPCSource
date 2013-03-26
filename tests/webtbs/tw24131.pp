uses
  SysUtils;
var
  s: single;
begin
  s := 1.00999;
  FormatSettings.DecimalSeparator:='.';
  writeln(FloatToStrF(s, ffGeneral, 8, 0, FormatSettings));
  writeln(FloatToStrF(s, ffGeneral, 7, 0, FormatSettings));
  writeln(FloatToStrF(s, ffGeneral, 6, 0, FormatSettings));
  writeln(FloatToStrF(s, ffGeneral, 5, 0, FormatSettings));
  writeln(FloatToStrF(s, ffGeneral, 4, 0, FormatSettings));
  writeln(FloatToStrF(s, ffGeneral, 3, 0, FormatSettings));
  writeln(FloatToStrF(s, ffGeneral, 2, 0, FormatSettings));

  if FloatToStrF(s, ffGeneral, 8, 0, FormatSettings)<>'1.00999' then
    halt(1);
  if FloatToStrF(s, ffGeneral, 7, 0, FormatSettings)<>'1.00999' then
    halt(2);
  if FloatToStrF(s, ffGeneral, 6, 0, FormatSettings)<>'1.00999' then
    halt(3);
  if FloatToStrF(s, ffGeneral, 5, 0, FormatSettings)<>'1.01' then
    halt(4);
  if FloatToStrF(s, ffGeneral, 4, 0, FormatSettings)<>'1.01' then
    halt(5);
  if FloatToStrF(s, ffGeneral, 3, 0, FormatSettings)<>'1.01' then
    halt(6);
  if FloatToStrF(s, ffGeneral, 2, 0, FormatSettings)<>'1' then
    halt(7);


end.
