{ %cpu=avr }
{ %norun }
const
  foo = %00011000;
begin
  asm
    andi r16, ~foo
  end;
end.
