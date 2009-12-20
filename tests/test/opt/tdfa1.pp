{ %OPT=-Oodfa -Sew -vw}
{ %NORUN}
{ %FAIL}

program tdfa1;

procedure p;
var
  counter: Integer;
  c1: Word;
begin
  repeat
    c1 := counter;  // counter not initialized
    counter:=15;
  until counter>=10;
end;

begin
end.