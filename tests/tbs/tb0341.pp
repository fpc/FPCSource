{ %cpu=i386 }
program test_assembler;

procedure test_att;
begin
{$asmmode att}
 asm
   ret
   lret
   iret
   iretw
 end;
end;

procedure test_intel;
begin
{$asmmode intel}
 asm
   ret
   retf
   retn
   iret
   iretd
   iretw
 end;
end;

begin
  Writeln('This is just to test special assembler instructions');
end.
