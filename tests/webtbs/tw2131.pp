program bug2131;

var
  TestStr : string;
begin
  Str (1.789e12:1, TestStr);
  if (teststr <> ' 1.8E+0012') then
    begin
      writeln('error, got "',teststr,'" expected " 1.8E+0012"');
      halt(1);
    end
end.

