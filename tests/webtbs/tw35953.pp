{ %CPU=x86_64,i386 }
{$mode objfpc}{$H+}
{$asmmode INTEL}

function f: longint; assembler;
asm
  mov ecx, ebx {shift by initial common exponent e}
  vaddpd              XMM0 {k1} {z}, XMM0, [RAX + RDI + $10] {1to2}
end;


begin
end.