{ %CPU=i8086 }
{ %OPT=-Sew }
{ %fail }

{ Pop CS should produce a warning on i8086 }

{$asmmode intel}

begin
  asm
    pop cs
  end;
end.
