{ %cpu=i386 }
{$goto on}
label
  l1,l2;
begin
  if PtrInt(@l2-@l1)>3 then
    halt(1);
  halt(0);
  asm
     l1:
        JMP l1 // E9 01000000 instead EB 01
        NOP
     l2:
  end;
end.
