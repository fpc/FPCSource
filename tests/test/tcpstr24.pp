{$codepage utf8}


type
  str866 = type ansistring(866);
const
  x = utf8string('abcdef');
  y = utf8string('®†◊√');
  z = str866('abc');

procedure test(const x: shortstring);
begin
  writeln('shortstring!');
  halt(1);
end;

procedure test(const x: rawbytestring; cp: tsystemcodepage);
begin
  writeln('ansistring(',stringcodepage(x),')');
  if stringcodepage(x)<>cp then
    halt(2);
end;

begin
  test(x,CP_UTF8);
  test(y,CP_UTF8);
  test(z,866);
end.
