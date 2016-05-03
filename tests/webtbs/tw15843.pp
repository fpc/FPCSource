{ %opt=-Cg- }
{ %cpu=i386 }

{$asmmode intel}
var
  a: array[0..3] of byte;
  l: longint;
begin
  a[0]:=1;
  a[1]:=2;
  a[2]:=3;
  a[2]:=4;
  asm
    lea ecx,[a]
    inc ecx
    movzx eax, byte ptr[ecx-1+1]  // bug in this line (-2)
    mov [l],eax
  end;
  if l<>2 then
    halt(1);
end.
