{ %cpu=i8086 }

{ i8086 test for correct assembly of 32-bit instructions on the i8086 target }

{ this one tests an instruction with a 32-bit const operand }

program tasm16_32_4;

{$asmmode att}
{$asmcpu 80386}

var
  lo_word, hi_word: Word;
begin
  asm
    .byte 0x66, 0x31, 0xc0  { xor eax, eax }

    { the actual instruction being tested: }
    orl $0x80000001, %eax

    .byte 0x66, 0x50  { push eax }

    popw lo_word
    popw hi_word
  end;
  if (lo_word=$0001) and (hi_word=$8000) then
    Writeln('Ok!')
  else
  begin
    Writeln('Error!');
    Halt(1);
  end;
end.
