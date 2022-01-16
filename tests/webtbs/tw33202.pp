{ %CPU=avr }
{ %OPT=-Sm -Wpavrsim }
program macrotest;
const
  sramMax = FPC_SRAMBASE + FPC_SRAMSIZE - 1;
begin
  asm
    sts sramMax , r31 // macrotest.pp(13,16) Error: Constant value out of bounds
  end;
end.
