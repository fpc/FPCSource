{ Old file: tbs0053.pp }
{  shows a problem with open arrays                     OK 0.99.1 (FK) }

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
