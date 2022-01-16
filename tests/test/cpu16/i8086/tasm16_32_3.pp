{ %cpu=i8086 }

{ i8086 test for correct assembly of 32-bit instructions on the i8086 target }

{ this one tests an instruction with a 32-bit const operand }

program tasm16_32_3;

{$asmmode intel}
{$asmcpu 80386}

var
  lo_word, hi_word: Word;
begin
  asm
    db 66h, 31h, 0c0h  { xor eax, eax }

    { the actual instruction being tested: }
    or eax, 80000001h

    db 66h, 50h  { push eax }
    
    pop word ptr [lo_word]
    pop word ptr [hi_word]
  end;
  if (lo_word=$0001) and (hi_word=$8000) then
    Writeln('Ok!')
  else
  begin
    Writeln('Error!');
    Halt(1);
  end;
end.
