{ %fail }
{ %cpu=i386 }
{ %opt=-Cg- }

var
  a: longint;
begin
  {$asmmode att}
  asm
.LPIC:
    movl a-.LPIC-.LPIC(%ecx),%eax
  end;
end.
