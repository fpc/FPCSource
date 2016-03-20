{ %skiptarget=android }
{ %cpu=i386,x86_64 }
{ %opt=-Cg- }

program movdtest;
var 
  a: int64 = 128133443 or (int64(123455) shl 32);
  b: int64;
begin
  asm
{$ifdef cpui386}
      movd a, %xmm0
      movd %xmm0, b
{$else}
      movd a(%rip), %xmm0
      movd %xmm0, b(%rip)
{$endif}
  end;
  if b<>128133443 then
    halt(1);
end.
