program ex7;

{ Program to demonstrate the TObject.Free call }

Uses Objects;

Var O : PObject;

begin
  Writeln ('Memavail : ',Memavail);
  // Allocate memory for object.
  O:=New(PObject,Init);
  Writeln ('Memavail : ',Memavail);
  // Free memory of object.
  O^.free;
  Writeln ('Memavail : ',Memavail);
end.