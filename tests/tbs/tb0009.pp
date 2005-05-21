{ Old file: tbs0012.pp }
{  tests type conversation byte(a>b)                 OK 0.9.9 (FK) }

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
