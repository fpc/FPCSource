{ %cpu=x86_64}
{ %opt=-Cg -vew }

var
  global: boolean;
begin
  asm
    movq global@GOTPCREL(%rip), %rax
    movb $1, (%rax)
  end;
  if not global then
    halt(1);
end.
