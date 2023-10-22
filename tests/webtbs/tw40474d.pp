{ %CPU=AARCH64 }
program tw40474d;

{ This test evaluates the correct interpretation of the BIC mnemonic.  It is
  supposed to have three operands, but the compiler mistakenly treated it as
  if it only had 2 (and an optional shifter operand) }

{ Test b = 64-bit registers, ROR-type shift of 52 }

function ClearMaskedBits(const Input, Mask: QWord): QWord; assembler; nostackframe;
asm
  BIC X0, X0, X1, ROR #52
end;

const
  Inputs:   array[0..7] of QWord = (1,                 2,                 3,                 QWord($FFFFFFFFFFFFFFFF), 6,                 7,                 9,                 QWord($FFFFFFFFFFFFFFFF));
  Masks:    array[0..7] of QWord = ($0010000000000000, $0010000000000000, $0010000000000000, QWord($0018000000000000), $0050000000000000, $0050000000000000, $0050000000000000, QWord($FFFFFFFFFFFFFFFF));
  Expected: array[0..7] of QWord = (0,                 2,                 2,                 QWord($7FFFFFFFFFFFFFFE), 2,                 2,                 8,                 0);
var
  Count: Integer;
  Output: QWord;
begin
  for Count := Low(Inputs) to High(Inputs) do
    begin
      Output := ClearMaskedBits(Inputs[Count], Masks[Count]);
      if (Output <> Expected[Count]) then
        begin
          WriteLn('FAIL: BIC($', HexStr(Inputs[Count], 16), ', $', HexStr(Masks[Count], 16), '... expected $', HexStr(Expected[Count], 16), ' but got $', HexStr(Output, 16));
          Halt(1);
        end;
    end;
  WriteLn('ok');
end.

