{$inline on}
procedure test(var a : longint;b : longint);inline;

begin
  a:=32-b;
end;

procedure test2(var a : longint;b : longint);

begin
  a:=32-b;
end;

  var
    a,b : longint;
begin
  test2(a,16);
  Writeln('a=',a,' should be 16');
  if (a<>16) then halt(1);
  test(a,16);
  Writeln('a=',a,' should be 16');
  if (a<>16) then halt(1);
end.
