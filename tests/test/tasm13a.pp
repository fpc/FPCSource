{ %CPU=i8086,i386,x86_64 }
{ %fail }

{ Tests the 'Cannot override ES segment' error message }

{$asmmode intel}

begin
  asm
{$if defined(cpui8086)}
    movs byte ptr ds:[di], byte ptr [si]
{$elseif defined(cpui386)}
    movs byte ptr ds:[edi], byte ptr [esi]
{$elseif defined(cpux86_64)}
    movs byte ptr fs:[rdi], byte ptr [rsi]
{$endif}
  end;
end.
