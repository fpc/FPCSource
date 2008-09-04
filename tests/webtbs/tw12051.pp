program doubleconv;
var
 s: string;
begin
 str(double(10000000000000000000),s);
 writeln(s);
 if (pos('-',s)<>0) then
   halt(1);
end.

