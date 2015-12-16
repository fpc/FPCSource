{ %cpu=i386,x86_64 }
program Project1;

{$mode delphi}
{$asmmode intel}

function RDTSCP: Int64; assembler; nostackframe;
asm
  RDTSCP
end;

begin
  if (PByte(@RDTSCP)[0]<>$0f) or (PByte(@RDTSCP)[1]<>$01) or (PByte(@RDTSCP)[2]<>$f9) then
    halt(1);
  writeln('ok');
end.
