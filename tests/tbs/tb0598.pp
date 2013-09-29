
program tb0598;

{$R-}

var
  a: Cardinal;
  b: QWord;
  c1, c2: QWord;
begin
  a := 1000000;
  b := 10000000000000000000;
  c1 := b div a;
  c2 := 10000000000000000000 div a;
  Write(c1, ' = ', c2, ': ');
  if (c1 <> c2) or (c2 <> 10000000000000) then
  begin
    Writeln('FAIL');
    halt(1);
  end
  else
    Writeln('OK');
end.
