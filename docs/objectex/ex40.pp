Program ex40;

{ Program to demonstrate the NewStr function }

Uses Objects;

Var S : String;
    P : PString;

begin
  S:='Some really cute string';
  Writeln ('Memavail : ',Memavail);
  P:=NewStr(S);
  If P^<>S then
    Writeln ('Oh-oh... Something is wrong !!');
  Writeln ('Allocated string. Memavail : ',Memavail);
  DisposeStr(P);
  Writeln ('Deallocated string. Memavail : ',Memavail);
end.