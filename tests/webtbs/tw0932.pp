{ %CPU=i386 }
program test;

{$ASMMODE Intel }

procedure TestProc;
const
  TestConst: String = 'Test';
begin
  asm
    mov edi, offset TestConst
  end;
end;

begin
  TestProc;
end.
