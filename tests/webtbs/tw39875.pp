{$codepage utf8}
program tw39875;
var
  ar: PChar = 'Ёжик';
  ar2: PChar;
  l, i: SizeInt;
begin
 Writeln(strlen(ar));  //Error
  Writeln(Length(Ar)); //Error
  l := Length(ar);
  ar2 := 'Ёжик';
  Writeln(strlen(ar2)); //Ok
  Writeln(Length(Ar2)); //Ok
  if l <> Length(ar2) then
    Halt(1);
  for i := 0 to l - 1 do
    if ar[i] <> ar2[i] then
      Halt(2 + i);
  //Readln;
end.

