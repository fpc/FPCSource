Program Example97;

{$H+}

{ Program to demonstrate the StringOfChar function. }

Var S : String;

begin
  S:=StringOfChar(' ',40)+'Aligned at column 41.';
  Writeln(s);
end.
