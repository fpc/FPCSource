{ %CPU=avr }
{ %fail }
{$goto on}
program test;
label lbl;

begin
  asm
    lbl:
    nop; nop; nop; nop; nop; nop; nop; nop; nop; nop;
    nop; nop; nop; nop; nop; nop; nop; nop; nop; nop;
    nop; nop; nop; nop; nop; nop; nop; nop; nop; nop;
    nop; nop; nop;
    breq lbl
  end;
end.
