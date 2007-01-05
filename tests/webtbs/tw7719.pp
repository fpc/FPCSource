{ %cpu=i386,x86_64 }
{$asmmode intel}
program test;

{$APPTYPE CONSOLE}

begin

asm
psrldq xmm2,4
end;

end.
