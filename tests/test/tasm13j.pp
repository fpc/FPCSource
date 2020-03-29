{ %CPU=i8086,i386,x86_64 }
{ %fail }

{ Tests the 'Cannot override ES segment' error message }

{$asmmode att}

begin
  asm
{$if defined(cpui8086)}
    insb %dx,%ds:(%di)
{$elseif defined(cpui386)}
    insb %dx,%ds:(%edi)
{$elseif defined(cpux86_64)}
    insb %dx,%fs:(%rdi)
{$endif}
  end;
end.
