Program Example59;

{ Program to demonstrate the Ptr (compability) function.
}

type pString = ^String;

Var P : pString;
    S : String;

begin
  S:='Hello, World !';
  P:=pString(Ptr(Seg(S),Longint(Ofs(S))));
  {P now points to S !}
  Writeln (P^);
end.
