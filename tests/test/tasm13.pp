{ %CPU=i386,x86_64 }
{ %fail }

// need exact size of memory operand (128- or 256 bit)
{$asmmode intel}
procedure test; assembler; nostackframe;
asm
   vcvttpd2dq  xmm0, [RAX]
end;

begin
end.
