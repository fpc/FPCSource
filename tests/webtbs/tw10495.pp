uses
  variants;
var
  tmp: OleVariant;
  a: OleVariant;
  b: OleVariant;
begin
  tmp:=VarArrayCreate([0,2], varVariant);
  a:=1234;
  b:=4321;
  tmp[0]:=a;
  tmp[1]:=b;
  a:=tmp[0];
  b:=tmp[1];
  if a<>1234 then
    halt(1);
  if b<>4321 then
    halt(1);
  writeln('ok');
end.
