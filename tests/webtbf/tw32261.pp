{%fail}
{%CPU=avr}

program webtbf32261;

begin
  asm
    cp r24, 8  // 2nd operand should be a register
  end;
end.
