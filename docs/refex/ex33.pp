Program Example33;

{ Program to demonstrate the Insert function. }

Var S : String;

begin
  S:='Free Pascal is difficult to use !';
  Insert ('NOT ',S,pos('difficult',S));
  writeln (s);
end.
