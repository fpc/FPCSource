{ Old file: tbs0004.pp }
{  tests the continue instruction in the for loop      OK 0.9.2 }

var
   i : longint;

begin
   for i:=1 to 100 do
     begin
        writeln('Hello');
        continue;
        writeln('ohh');
        Halt(1);
     end;
end.
