program test;

uses typinfo;

Type
  E1 = (en1,en2,en3,en4,en5);
  E2 = en3..en5;
begin
  if (GetEnumValue(TypeInfo(E1),'en5') <> 4) then
    halt(1);
  if (GetEnumValue(TypeInfo(E2),'en5') <> 4) then
    halt(1);
  writeln('ok');
end.
