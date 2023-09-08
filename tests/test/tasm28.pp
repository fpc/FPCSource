{ %NORUN }
{ %CPU=i386,x86_64 }

program tasm28;

{$mode objfpc}

{$asmmode att}
procedure Test(aArr: array of LongInt); {$ifdef cpu386}register;{$endif} assembler; nostackframe;
asm
  movb $5, (aArr, __HIGH(aArr))
  movb $5, (__HIGH(aArr), aArr)
end;

begin

end.
