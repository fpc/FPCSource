{ %cpu=i8086 }

{ i8086 test for correct assembly of 32-bit instructions on the i8086 target }

{ this one tests an instruction with a 32-bit const operand }

program tasm16_32_2;

{$asmmode att}
{$asmcpu 80386}

var
  lo_word, hi_word: Word;
begin
  asm
    .byte 0x66, 0x31, 0xc0  { xor eax, eax }

    { the actual instruction being tested: }
    movl $0x12345678, %eax

    .byte 0x66, 0x50  { push eax }

    popw lo_word
    popw hi_word
  end;
  if (lo_word=$5678) and (hi_word=$1234) then
    Writeln('Ok!')
  else
  begin
    Writeln('Error!');
    Halt(1);
  end;
end.
