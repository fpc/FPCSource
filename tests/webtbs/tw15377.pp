program test;

uses typinfo;

Type
  E1 = (en1,en2,en3,en4,en5);
  E2 = en3..en5;
begin
  if (GetEnumName(TypeInfo(E1),Ord(High(E1))) <> 'en5') then
    halt(1);
  if (GetEnumName(TypeInfo(E2),Ord(High(E2))) <> 'en5') then
    halt(1);
end.
