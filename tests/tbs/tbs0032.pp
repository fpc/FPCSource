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
