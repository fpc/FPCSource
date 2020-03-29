{ %cpu=avr }
{ %norun }
const
  c0 = 0;
  c1 = 1;
  c2 = $02;
  c10 = $10;
  c22 = $22;
  c100 = $100;
  c1000 = $1000;
  c3070 = $3070;

procedure something; assembler; nostackframe;
asm
  ldi ZL, lo8(c0);
  ldi ZH, hi8(c0);
  ldi ZL, lo8(c1);
  ldi ZH, hi8(c1);
  ldi ZL, lo8(c2);
  ldi ZH, hi8(c2);
  ldi ZL, lo8(c10);
  ldi ZH, hi8(c10);
  ldi ZL, lo8(c22);
  ldi ZH, hi8(c22);
  ldi ZL, lo8(c100);
  ldi ZH, hi8(c100);
  ldi ZL, lo8(c1000);
  ldi ZH, hi8(c1000);
  ldi ZL, lo8(c3070);
  ldi ZH, hi8(c3070);

  ldi ZL, lo8(0x3070);
  ldi ZH, hi8(0x3070);
  ldi ZL, lo8(0x300);
  ldi ZH, hi8(0x300);
end;


begin
end.
