program test_interrupt;



procedure test1;interrupt;
begin
  Writeln('Test1 interrupt');
end;

procedure test2(var a,b : longint);interrupt;
begin
  Writeln('Test2 interrupt');
  a:=1;
  b:=2;
end;

function test3 : longint; interrupt;
begin
  Writeln('test3 called');
  test3:=55;
end;

  var
    x,y : longint;

begin
  x:=-1;
  test1;
  test2(x,y);
  if (x<>1) or (y<>2) then
    Writeln('Error with interrupt');
  if test3<>55 then
    Writeln('Error with interrupt function');
end.