{ %CPU=i8086 }
{ %OPT=-Sew }
{ %fail }

{ Pop CS should produce a warning on i8086 }

{$asmmode att}

begin
  asm
    popw %cs
  end;
end.
