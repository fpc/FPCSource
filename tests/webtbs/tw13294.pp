{ %skiptarget=android }
{ %cpu=i386,x86_64 }
{ %opt=-Cg- }

program movdtest;
var 
  a: int64 = 128133443 or (int64(123455) shl 32);
  b: int64;
  al: longint absolute a;
  bl: longint absolute b;
begin
  b:=0;
  asm
{$ifdef cpui386}
      movd al, %xmm0
      movd %xmm0, bl
{$else}
      movd al(%rip), %xmm0
      movd %xmm0, bl(%rip)
{$endif}
  end;
  if b<>128133443 then
    halt(1);
end.
