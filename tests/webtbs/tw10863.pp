function f(b,c,d : boolean) : longint;
 begin
   f:=ord(b);
 end;

procedure test;
var
 b,c : boolean;

begin
 b:=true;
 c:=false;
 b:=1<>f(b,not(b),c);
 c:=true;
 if (b) then
   halt(1);
 writeln(c);
end;

begin
  test;
end.
