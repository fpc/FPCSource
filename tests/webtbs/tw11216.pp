program TestRC;
{$R+}

var Li, Lj : Int64;
const I = $7fffffffffffffff;

begin
  if (1-I)<>(-I+1) then
    halt(1);

  writeln(1-I);
  writeln(-I+1);

  Li := 1-I;
  Lj := -I + 1;
  if Li<>Lj then
    halt(2);

  if (Li<>-9223372036854775806) then
    halt(3);
end.
