{ %opt=-Cr -Sew }

var
  a: PtrUInt;
  q: qword;
begin
  a := not(ptruint(7));
  if a<>$fffffff8 then
    halt(1);
  q := not(qword(7));
  if q<>qword($fffffffffffffff8) then
    halt(2);
  a := 99;
  WriteLn((a + 9) and not PtrUInt(7));
  if ((a + 9) and not PtrUInt(7))<>$68 then
    halt(3);
end.

