Program Example38;

{ Program to demonstrate the Lo function. }

Var L : Longint;
    W : Word;

begin
  L:=(1 Shl 16) + (1 Shl 4);  { $10010 }
  Writeln (Lo(L));            { Prints 16 }
  W:=(1 Shl 8) + (1 Shl 4);   { $110   }
  Writeln (Lo(W));            { Prints 16 }
end.
