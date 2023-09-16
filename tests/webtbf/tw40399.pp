{ %cpu=i386 }
{ %fail }
{$asmmode intel}

begin
  asm
    fnstcw [esi]
    imul [esi]
  end;
end.
