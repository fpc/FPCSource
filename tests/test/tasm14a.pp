{ %CPU=i386,x86_64 }
{ %fail }

{ Pop CS should produce an error on i386 and x86_64 }

{$asmmode intel}

begin
  asm
    pop cs
  end;
end.
