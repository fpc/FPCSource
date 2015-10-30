program taddr3;

{$ifndef FPC}
type
  codepointer = pointer;
{$endif}

procedure testproc;
begin
end;

function testfunc: codepointer;
begin
  testfunc:=nil;
end;

var
  p1, p2: codepointer;
begin
  p1 := @testproc;
  p2 := Addr(testproc);
  if p1<>p2 then
    Halt(1);
  p1 := @testfunc;
  p2 := Addr(testfunc);
  if p1<>p2 then
    Halt(2);
end.
