{ %CPU=x86_64 }
{ %fail }

// AH/BH/CH/DH and x86_64 specific registers cannot be used in a single instruction
{$asmmode att}
procedure test; assembler; nostackframe;
asm
     adcb  %ah, (%r8)
end;

begin
end.
