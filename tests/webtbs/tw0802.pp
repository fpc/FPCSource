program test;
  function testf (a:byte;b:integer;c:char):char;
  begin
    testf:=c;
  end;
begin
  writeln('"',testf(0,-1,'A'),'"');
end.
