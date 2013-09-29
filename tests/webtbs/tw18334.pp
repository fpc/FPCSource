{ %cpu=arm }
{ %norun }

procedure test; assembler;
  asm
(* not sure which CPUs really support it (FK) *)
{$ifdef CPU_ARMV7M}
    cps #0
    cpsie aif, #0
    cpsid aif, #0
    cpsie aif
    cpsid aif
{$endif CPU_ARMV7M}
  end;

begin
end.
