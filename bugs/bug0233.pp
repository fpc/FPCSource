program except_test;

type byteset = set of byte;
     enumset = set of (zero,one,two,three);
     
procedure test(s : byteset);
begin
  if 0 in s then
    Writeln('Contains zero !');
end;

procedure testenum(s : enumset);
begin
  if zero in s then
    Writeln('Contains zero !');
end;

begin
  test([1..5,8]);
  test([0,8,15]);
  testenum([zero,two]);
end.