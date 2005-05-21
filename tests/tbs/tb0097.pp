{ Old file: tbs0115.pp }
{ missing writeln for comp data type                    OK 0.99.6 (FK) }

var
   c : comp;

begin
   c:=1234;
   writeln(c);
   {readln(c);}
   c:=-258674;
   writeln(c);
end.
