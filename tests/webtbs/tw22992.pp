{ %norun }
{ %cpu=arm }

begin
  asm
    MRS R0, CPSR
    MRS R0, SPSR
    MSR CPSR_CX, R0
    LDMIA SP, {R0-R15}^
  end;
end.

