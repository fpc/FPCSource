{ %CPU=i8086 }

program tasm23;

procedure t; assembler;
asm
  mov ax, [bx[5]][di][54][-17][45][4]      { mov ax, [bx+di+5Bh] }
  mov ax, [[bx+5]+[di+54]+[-17]+[45]+[4]]  { mov ax, [bx+di+5Bh] }
end;

begin
end.
