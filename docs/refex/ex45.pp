Program Example45;

{ Program to demonstrate the Ord function. }

Type
  TEnum = (Zero, One, Two, Three, Four);
  
Var 
  X : Longint;
  Y : TEnum;
  
begin
  X:=125;
  Writeln (Ord(X));   { Prints 125 }
  Y:= One;
  Writeln (Ord(y));   { Prints 1 }
end.
