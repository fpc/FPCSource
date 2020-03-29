{ %cpu=avr }
{ %fail }
program test;

type
  TUintRecord = packed record
    l:byte;
    h:byte;
  end;

procedure delayloop2(const counter: TUintRecord); assembler;
asm
  mov XH, counter.h
  mov XL, counter.l
end;


var
  t: TUintRecord;

begin
   t.l := 1;
   t.h := 0;
   delayloop2(t);
end.
