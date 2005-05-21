{ Old file: tbs0018.pp }
{  tests for the possibility to declare all types using pointers "forward" : type p = ^x; x=byte;     OK 0.9.3 }

type
   p = ^x;
   x = byte;

var
   b : p;

begin
   new(b);
   b^:=12;
end.
