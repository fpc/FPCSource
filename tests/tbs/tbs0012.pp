var
   a,b : longint;

begin
   a:=1;
   b:=2;
   if byte(a>b)=byte(a<b) then
     begin
        writeln('Ohhhh');
        Halt(1);
    end;
end.

