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

