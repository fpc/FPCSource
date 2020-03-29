{ %CPU=avr}
{ %norun }

program webtbs29758;

type
  TDoubleByte = packed record
    l, h: byte;
  end;

var a: word;

procedure check2(aparam: word); assembler;
asm
  mov r20, TDoubleByte(aparam).l
end;

begin
  check2(a);
end.
