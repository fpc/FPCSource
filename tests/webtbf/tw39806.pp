{ %fail }
program test;

type
  TByteDynArr = array of byte;

procedure proc(a: TByteDynArr);
begin
  Writeln(Length(a));
end;

var
  x: array of byte;
begin
  x:= [1,2,3,4];
  proc(Slice(x, 2));
  // <source>(15,19) Fatal: Internal error 2005101501
end.
