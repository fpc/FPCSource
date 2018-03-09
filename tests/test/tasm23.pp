{ %CPU=i8086 }

program tasm23;

procedure t; assembler;
asm
  mov ax, [bx[5]][di][54][-17][45][4]      { mov ax, [bx+di+5Bh] }
  mov ax, [[bx+5]+[di+54]+[-17]+[45]+[4]]  { mov ax, [bx+di+5Bh] }
  mov ax, [5[7]]                           { mov ax, [000Ch] }
  mov ax, [5+[7]]                          { mov ax, [000Ch] }
end;

procedure t2; assembler;
var
  locl: word;
asm
  mov ax, locl                             { mov ax, [bp-02] }
  mov ax, cs:locl                          { mov ax, cs:[bp-02] }
  mov ax, [cs:locl]                        { mov ax, cs:[bp-02] }
  mov ax, [cs:[locl]]                      { mov ax, cs:[bp-02] }
  mov ax, cs:locl[5]                       { mov ax, cs:[bp+03] }
  mov ax, cs:5[locl]                       { mov ax, cs:[bp+03] }
end;

begin
end.
