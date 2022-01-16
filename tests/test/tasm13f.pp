{ %CPU=i8086,i386,x86_64 }
{ %fail }

{ Tests the 'Cannot override ES segment' error message }

{$asmmode att}

begin
  asm
{$if defined(cpui8086)}
    movsb (%si), %ds:(%di)
{$elseif defined(cpui386)}
    movsb (%esi), %ds:(%edi)
{$elseif defined(cpux86_64)}
    movsb (%rsi), %fs:(%rdi)
{$endif}
  end;
end.
