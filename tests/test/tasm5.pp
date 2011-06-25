{ %CPU=x86_64 }
{ %fail }

{$asmmode att}
// Immediates must fit into 32 bits (except in 'mov' instruction)
procedure test; assembler; nostackframe;
asm
      andq   $0x0000000105060708, %rax
      orq    $0xffffffff00000000, %rax
end;


begin
end.
