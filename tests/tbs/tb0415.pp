{ %CPU=i386 }

{
  Testing if using the same local label in two
   procedures does not create an error PM
}

program test_local_labels;


{$asmmode att}

procedure att_test1; assembler;

asm
  jmp .Llocal
.Llocal:
end;

procedure att_test2; assembler;

asm
  jmp .Llocal
.Llocal:
end;

{$asmmode intel}

procedure intel_test1; assembler;

asm
  jmp @@Llocal
@@Llocal:
end;

procedure intel_test2; assembler;

asm
  jmp @@Llocal
@@Llocal:
end;

begin
  att_test1;
  att_test2;
  intel_test1;
  intel_test2;
end.
