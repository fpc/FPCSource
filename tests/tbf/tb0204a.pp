{ %fail }
{ %cpu=i386 }
{ %opt=-Cg- }

var
  a: longint;
begin
  {$asmmode intel}
  asm
@@LPIC:
    mov eax,[a-@@LPIC-@@LPIC+ecx]
  end;
end.
