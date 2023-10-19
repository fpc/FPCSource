{ %CPU=AARCH64 }
program tw40474e;

{ This test evaluates the correct interpretation of the BIC mnemonic.  It is
  supposed to have three operands, but the compiler mistakenly treated it as
  if it only had 2 (and an optional shifter operand) }

{ Test a = 32-bit registers, immediatre operand (is converted into equivalent machine code for AND) }

function ClearFirstAndLastBits(const Input: LongWord): LongWord; assembler; nostackframe;
asm
  BIC W0, W0, #0x80000001
end;

const
  Inputs:   array[0..3] of LongWord = (1, 2, 3, LongWord($FFFFFFFF));
  Expected: array[0..3] of LongWord = (0, 2, 2, LongWord($7FFFFFFE));
var
  Count: Integer;
  Output: LongWord;
begin
  for Count := Low(Inputs) to High(Inputs) do
    begin
      Output := ClearFirstAndLastBits(Inputs[Count]);
      if (Output <> Expected[Count]) then
        begin
          WriteLn('FAIL: BIC($', HexStr(Inputs[Count], 8), ', $80000001)... expected $', HexStr(Expected[Count], 8), ' but got $', HexStr(Output, 8));
          Halt(1);
        end;
    end;
  WriteLn('ok');
end.

