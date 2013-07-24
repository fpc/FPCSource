{ %cpu=arm }
program tb0597;

var x: longword;

procedure test; assembler; nostackframe;
asm
   ldr r0, =x
   ldr r1, =0x12345678
   str r1, [r0]
end;

begin
   test;
   
   if x <> $12345678 then
      halt(1);
end.
