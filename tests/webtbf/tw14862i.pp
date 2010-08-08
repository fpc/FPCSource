{ %cpu=x86_64 }
{ %fail }

{$asmmode intel}

begin
  asm
    popfd
  end;
end.
