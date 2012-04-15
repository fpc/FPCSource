{ %opt=-Cr -CO- -Sew -vw }

var
  a: PtrUInt;
  q: qword;
begin
  a := not(ptruint(7));
{$ifdef cpu64}
  if a<>ptruint($fffffffffffffff8) then
{$else}
  if a<>$fffffff8 then
{$endif}
    halt(1);
  q := not(qword(7));
  if q<>qword($fffffffffffffff8) then
    halt(2);
  a := 99;
  WriteLn((a + 9) and not PtrUInt(7));
  if ((a + 9) and not PtrUInt(7))<>$68 then
    halt(3);
end.

