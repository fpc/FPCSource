{ %cpu=i8086 }

{ i8086 test for correct assembly of 32-bit instructions on the i8086 target }

{ this one tests an instruction with a 32-bit const operand }

program tasm16_32_1;

{$asmmode intel}
{$asmcpu 80386}

var
  lo_word, hi_word: Word;
begin
  asm
    db 66h, 31h, 0c0h  { xor eax, eax }

    { the actual instruction being tested: }
    mov eax, 12345678h

    db 66h, 50h  { push eax }
    
    pop word ptr [lo_word]
    pop word ptr [hi_word]
  end;
  if (lo_word=$5678) and (hi_word=$1234) then
    Writeln('Ok!')
  else
  begin
    Writeln('Error!');
    Halt(1);
  end;
end.
