Program Example45;

{ Program to demonstrate the Ord,Pred,Succ functions. }

Type
  TEnum = (Zero, One, Two, Three, Four);

Var
  X : Longint;
  Y : TEnum;

begin
  X:=125;
  Writeln (Ord(X));  { Prints 125 }
  X:=Pred(X);
  Writeln (Ord(X));  { prints 124 }
  Y:= One;
  Writeln (Ord(y));  { Prints 1 }
  Y:=Succ(Y);
  Writeln (Ord(Y));  { Prints 2}
end.
