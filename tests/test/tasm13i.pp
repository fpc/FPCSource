{ %CPU=i8086,i386,x86_64 }
{ %fail }

{ Tests the 'Cannot override ES segment' error message }

{$asmmode att}

begin
  asm
{$if defined(cpui8086)}
    stosb %ds:(%di)
{$elseif defined(cpui386)}
    stosb %ds:(%edi)
{$elseif defined(cpux86_64)}
    stosb %fs:(%rdi)
{$endif}
  end;
end.
