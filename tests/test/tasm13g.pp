{ %CPU=i8086,i386,x86_64 }
{ %fail }

{ Tests the 'Cannot override ES segment' error message }

{$asmmode att}

begin
  asm
{$if defined(cpui8086)}
    cmpsb %ds:(%di), (%si)
{$elseif defined(cpui386)}
    cmpsb %ds:(%edi), (%esi)
{$elseif defined(cpux86_64)}
    cmpsb %fs:(%rdi), (%rsi)
{$endif}
  end;
end.
