{ %opt=-Fasysutils}
{$mode iso}
{$modeswitch exceptions+}
program modulo( Output);
var
  i, j, m : integer;
begin
  i := -10;
  j := 100;

  m := i mod j;
  if m<>90 then
    halt(1);
  Writeln( ' m = (', i: 3, ') mod ', j: 3, ' = ', m: 3);


  m := -10 mod 100;
  Writeln( '-10 mod 100 = ', m :3);
  if m<>-10 then
    halt(1);

  i := 10;
  j := 100;
  m := -i mod j;
  Writeln( '-10 mod 100 = ', m :3);
  if m<>-10 then
    halt(1);


  m:=(-10) mod 100;
  Writeln( '(-10) mod 100 = ', m :3);
  if m<>90 then
    halt(1);

  m:=10 mod 100;
  Writeln( '10 mod 100 = ', m :3);
  if m<>10 then
    halt(1);

  i:=10;
  j:=-100;
  try
    m := 0;
    m := i mod j;
  except
    on e : EDivByZero do
      m:=$1234;
  end;
  if m<>$1234 then
    halt(1);

  writeln('ok');
end.
