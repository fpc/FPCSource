Program Example63;

{ Program to demonstrate the SizeOf function. }
Var
  I : Longint;
  S : String [10];

begin
  Writeln (SizeOf(I));  { Prints 4  }
  Writeln (SizeOf(S));  { Prints 11 }
end.
