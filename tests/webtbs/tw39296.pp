{ %cpu=x86_64 }
{ %skiptarget=win64 }

function bytepara(b: byte; s: shortint): boolean; assembler; nostackframe;
asm
  xorl %eax, %eax
  cmpl $5, %edi
  seteb %al
  cmpl $-3, %esi
  seteb %dl
  andb %dl, %al
end;

var
  b1: byte;
  s1: shortint;
begin
  b1:=5;
  s1:=-3;
  asm
    movl $0x12345678, %edi
    movl $0xabcdef01, %esi
  end ['rsi', 'rdi'];
  if not bytepara(b1,s1) then
    halt(1);
end.
