Program Example36;

{ Program to demonstrate the Length function. }

Var S : String;
    I : Integer;

begin
  S:='';
  for i:=1 to 10 do
    begin
    S:=S+'*';
    Writeln (Length(S):2,' : ',s);
    end;
end.
