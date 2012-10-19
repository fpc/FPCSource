{ %fail }
{%opt=-O- }
{ This function defines a local var which needs stack space,
  so nostackframe should always be invalid.
  -O- option used because with -O3 both result
  and x variable can be regvars, so that there is no error! }

function test : longint; nostackframe;
var
  x : longint;
begin
  x:=4;
  test:=5*x;
end;

begin
  if test<>20 then
    begin
      writeln('Wrong result in  nostackframe non-assembler function');
      halt(1);
    end
  else
    writeln('Pascal function nostackframe works OK');
end.
