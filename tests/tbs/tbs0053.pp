procedure abc(var a : array of char);

  begin
     // error: a:='asdf';
  end;

var
   c : array[0..10] of char;

begin
   abc(c);
   writeln(c);
   // error: writeln(a);
end.

