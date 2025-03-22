{ %cpu=x86_64 }
program test;
{$mode objfpc}

//{$define INTEL}

{$IFDEF INTEL}
  {$asmmode intel}
  {$push}{$asmmode att}{$pop}
  procedure proc2; assembler;
  asm
    mov rax, 0 // err
  end;
{$ELSE}
  {$asmmode att}
  {$push}{$asmmode intel}{$pop}
  procedure proc2; assembler;
  asm
    movq $0, %rax // err
  end;
{$ENDIF}

begin
end.
