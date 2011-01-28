{ %cpu=arm }
{ %norun }

procedure test; assembler;
  asm
    cps #0
    cpsie aif, #0
    cpsid aif, #0
    cpsie aif
    cpsid aif
  end;

begin
end.
