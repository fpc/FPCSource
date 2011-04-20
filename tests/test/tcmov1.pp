{ %CPU=x86_64 }
{$mode objfpc}{$asmmode att}

// Test correct assembling of cmov instructions
// they should receive condition and size separately,
// iow cmovbq should not be processed as cmov+bq

function test: Pointer; assembler; nostackframe;
asm
    jmp    .L3
.L1:
    cmovbq   %rcx,%rax     // 48 0f 42 c1
    cmovbl   %ecx,%eax     // 0f 42 c1
    cmovbw   %cx,%ax       // 66 0f 42 c1
    cmovlq   %rcx,%rax     // 48 0f 4c c1
.L3:
    lea    .L1(%rip), %rax
end;

var
  x: PByte;

begin
  x := test;
  if (x[0]<>$48) or (x[1]<>$0F) or (x[2]<>$42) or (x[3]<>$C1) then
    Halt(1);
  Inc(x,4);
  if (x[0]<>$0F) or (x[1]<>$42) or (x[2]<>$C1) then
    Halt(2);
  Inc(x,3);
  if (x[0]<>$66) or (x[1]<>$0F) or (x[2]<>$42) or (x[3]<>$C1) then
    Halt(3);
  Inc(x,4);
  if (x[0]<>$48) or (x[1]<>$0F) or (x[2]<>$4C) or (x[3]<>$C1) then
    Halt(4);
end.
