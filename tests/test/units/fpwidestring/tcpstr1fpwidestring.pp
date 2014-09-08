uses
  unicodeducet, fpwidestring, cp1251, cp1253;

type
  tcpstr1 = type AnsiString(1253);
  tcpstr2 = type AnsiString(1251);
var
  a1 : tcpstr1;
  a2 : utf8string;
  a3 : tcpstr2;
  u1 : unicodestring;
begin
  a1:=' ';
  a1[1]:=char($80); // Euro symbol in cp1253
  a2:=a1;
  if ord(a2[1])<>$E2 then
    halt(1);
  if ord(a2[2])<>$82 then
    halt(2);

  writeln('---');

  a3:=a1;
  if ord(a3[1])<>$88 then
    halt(3);

  writeln('---');

  u1:=a1;
  if ord(u1[1])<>$20AC then
    halt(4);

  writeln('ok');
end.
