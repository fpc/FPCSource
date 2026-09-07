{ %fail }
{ %cpu=i386,x86_64 }
{$asmmode intel}

{ The formal's Single type must not silently override an explicit qword. }
procedure BadWidth(var V: Single); assembler;
asm
  movss xmm0, qword ptr [V]
end;

begin
end.
