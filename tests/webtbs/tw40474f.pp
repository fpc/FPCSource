{ %CPU=AARCH64 }
program tw40474f;

{ This test evaluates the correct interpretation of the BIC mnemonic.  It is
  supposed to have three operands, but the compiler mistakenly treated it as
  if it only had 2 (and an optional shifter operand) }

{ Test a = 64-bit registers, immediatre operand (is converted into equivalent machine code for AND) }

function ClearFirstAndLastBits(const Input: QWord): QWord; assembler; nostackframe;
asm
  BIC X0, X0, #0x8000000000000001
end;

const
  Inputs:   array[0..3] of QWord = (1, 2, 3, QWord($FFFFFFFFFFFFFFFF));
  Expected: array[0..3] of QWord = (0, 2, 2, QWord($7FFFFFFFFFFFFFFE));
var
  Count: Integer;
  Output: QWord;
begin
  for Count := Low(Inputs) to High(Inputs) do
    begin
      Output := ClearFirstAndLastBits(Inputs[Count]);
      if (Output <> Expected[Count]) then
        begin
          WriteLn('FAIL: BIC($', HexStr(Inputs[Count], 16), ', $80000000000001)... expected $', HexStr(Expected[Count], 16), ' but got $', HexStr(Output, 16));
          Halt(1);
        end;
    end;
  WriteLn('ok');
end.

