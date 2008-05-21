uses
  sysutils;

var
  s: string;
begin
  str(1.575:0:2,s);
  writeln(s);
  if (s<>'1.58') then
    halt(1);
  str(0.575:0:2,s);
  writeln(s);
  if (s<>'0.58') then
    halt(2);
//  writeln(FloatToStrF(1.575 ,ffFixed,19,2));
//  writeln(FloatToStrF(0.575 ,ffFixed,19,2));
end.
