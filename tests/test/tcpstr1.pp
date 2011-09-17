type
  tcpstr437 = string<437>;
  tcpstr850 = string<850>;
var
  a1 : tcpstr437;
  a2 : utf8string;
  a3 : tcpstr850;
  u1 : unicodestring;
begin
  a1:=#132;
  a2:=a1;
  if ord(a2[1])<>195 then
    halt(1);
  if ord(a2[2])<>164 then
    halt(1);

  writeln('---');

  a3:=a1;
  if ord(a3[1])<>132 then
    halt(1);

  writeln('---');

  u1:=a1;
  if ord(u1[1])<>228 then
    halt(1);

  writeln('ok');
end.
