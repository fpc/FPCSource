type
   b = ^x;

   x = byte;

var
   pb : b;

begin
   new(pb);
   pb^:=10;
end.   
      
