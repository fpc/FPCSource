{ Old file: tbs0032.pp }
{  tests for a bugs with the stack                      OK 0.9.9 }

var
   p : procedure(w : word);

  procedure pp(w :word);
    begin
       Writeln(w);
    end;

begin
   p:=@pp;
   p(1234);
end.
