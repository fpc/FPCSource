{ %CPU=x86_64 }
{ %fail }

// in 64-bit mode is needed exact size of memory operand (32- or 64 bit)
{$asmmode intel}
procedure test; assembler; nostackframe;
asm
   cvtsi2sd  xmm0, [RAX]
end;

begin
end.
