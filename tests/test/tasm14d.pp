{ %CPU=i8086 }
{ %NORUN }

{ Pop CS should produce a warning on i8086 }

{$asmmode att}

begin
  asm
    popw %cs
  end;
end.
