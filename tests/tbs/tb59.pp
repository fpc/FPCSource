{ Old file: tbs0065.pp }
{  shows that frac() doesn't work correctly.            OK 0.99.1 (PFV) }

Program Example27;

{ Program to demonstrate the Frac function. }

Var R : Real;

begin
  Writeln (Frac (123.456):0:3);  { Prints  O.456 }
  Writeln (Frac (-123.456):0:3); { Prints -O.456 }
end.
