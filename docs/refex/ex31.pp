Program Example31;

{ Program to demonstrate the Hi function. }

var
  L : Longint;
  W : Word;

begin
  L:=1 Shl 16;     { = $10000 }
  W:=1 Shl 8;      { = $100 }
  Writeln (Hi(L)); { Prints 1 }
  Writeln (Hi(W)); { Prints 1 }
end.
