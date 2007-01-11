program bug2131;

var
  TestStr : string;
  correct : string;
begin
  if sizeof(extended) > sizeof(double) then
    correct := ' 1.8E+0012'
  else
    correct := ' 1.8E+012';
  Str (extended(1.789e12):1, TestStr);
  if (teststr <> correct) then
    begin
      writeln('error, got "',teststr,'" expected "',correct,'"');
      halt(1);
    end
end.
