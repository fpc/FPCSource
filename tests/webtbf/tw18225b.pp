{ %fail }
{ %cpu=i386,x86_64 }
{$asmmode intel}
procedure CallProc(p: pointer); assembler;
asm
          call dword ptr p
          call dword ptr CallProc
end; { CallProc }
begin
end.
