Program Example77;

{ Program to demonstrate the Rename function. }
Var F : Text;

begin
  Assign (F,paramstr(1));
  Rename (F,paramstr(2));
end.
