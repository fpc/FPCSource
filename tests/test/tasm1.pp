{ %CPU=i386,x86_64 }
{ %fail }

// ADDPS is an SSE-only instruction, it must not accept MMX registers
{$asmmode intel}
procedure test; assembler; nostackframe;
asm
     addps  mm0, mm1
end;

begin
end.
