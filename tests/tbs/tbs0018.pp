type
   p = ^x;
   x = byte;

var
   b : p;

begin
   new(b);
   b^:=12;
end.

