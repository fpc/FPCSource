type testfunc = function:longint;

var test: testfunc;

function test_temp: longint;
begin
  test_temp:=12;
end;

procedure sound(test: testfunc);
begin
  writeln(test);
end; { proc. sound }

begin
  sound(test_temp);
end.
