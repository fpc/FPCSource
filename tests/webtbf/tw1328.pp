{ %FAIL }
{ %CPU=i386 }
{$asmmode intel}
begin
asm
movsx edx, edi
movsx eax, dword ptr [esi]
movzx edx, edi
movzx eax, dword ptr [esi]
end;
end.
