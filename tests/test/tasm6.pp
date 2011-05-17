{ %CPU=x86_64 }
{ %fail }

// push and pop with 32-bit operands aren't encodable in x86_64
{$asmmode att}
procedure test; assembler; nostackframe;
asm
     push   %eax
     pop    %r8d
     pushl  (%rax)
     popl   (%r8)
end;

begin
end.
