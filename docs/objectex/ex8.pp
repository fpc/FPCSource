program ex8;

{ Program to demonstrate the TObject.Done call }

Uses Objects;

Var O : PObject;

begin
  Writeln ('Memavail : ',Memavail);
  // Allocate memory for object.
  O:=New(PObject,Init);
  Writeln ('Memavail : ',Memavail);
  O^.Done;
  Writeln ('Memavail : ',Memavail);
end.