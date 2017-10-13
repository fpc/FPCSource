{ %CPU=i8086,i386,x86_64 }
{ %fail }

{ Tests the 'Cannot override ES segment' error message }

{$asmmode intel}

begin
  asm
{$if defined(cpui8086)}
    ins byte ptr ds:[di], dx
{$elseif defined(cpui386)}
    ins byte ptr ds:[edi], dx
{$elseif defined(cpux86_64)}
    ins byte ptr fs:[rdi], dx
{$endif}
  end;
end.
