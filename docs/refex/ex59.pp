Program Example59;

{ Program to demonstrate the Ptr function. }

Var P : ^String;
    S : String;
    
begin
  S:='Hello, World !';
  P:=Ptr(Seg(S),Longint(Ofs(S)));
  {P now points to S !}
  Writeln (P^);
end.
