uses Dos;

const MAX_VAL = 128;
var
   r : real;
   f : file of byte;
   b1, b2: byte;
begin
   assign(f,'bug2296.dat');
   rewrite(f);
   r := 12.2;
   write(f,Byte(round(sin(r)*(MAX_VAL-1)+MAX_VAL)));
   b1 := Byte(round(sin(r)*(MAX_VAL-1)+MAX_VAL));
   close(f);
   reset(f);
   read(f,b2);
   if b1 <> b2 then
     begin
       writeln('b1: ',b1,' <> b2: ',b2);
       halt(1);
     end;
   close(f);
   erase(f);
end.
