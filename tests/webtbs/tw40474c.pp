{ %CPU=AARCH64 }
program tw40474c;

{ This test evaluates the correct interpretation of the BIC mnemonic.  It is
  supposed to have three operands, but the compiler mistakenly treated it as
  if it only had 2 (and an optional shifter operand) }

{ Test c = 32-bit registers, ROR-type shift of 12 }

function ClearMaskedBits(const Input, Mask: LongWord): LongWord; assembler; nostackframe;
asm
  BIC W0, W0, W1, ROR #12
end;

const
  Inputs:   array[0..7] of LongWord = (1,     2,     3,     LongWord($FFFFFFFF), 6,     7,     9,     LongWord($FFFFFFFF));
  Masks:    array[0..7] of LongWord = ($1000, $1000, $1000, LongWord($00001800), $5000, $5000, $5000, LongWord($FFFFFFFF));
  Expected: array[0..7] of LongWord = (0,     2,     2,     LongWord($7FFFFFFE), 2,     2,     8,     0);
var
  Count: Integer;
  Output: LongWord;
begin
  for Count := Low(Inputs) to High(Inputs) do
    begin
      Output := ClearMaskedBits(Inputs[Count], Masks[Count]);
      if (Output <> Expected[Count]) then
        begin
          WriteLn('FAIL: BIC($', HexStr(Inputs[Count], 8), ', $', HexStr(Masks[Count], 8), '... expected $', HexStr(Expected[Count], 8), ' but got $', HexStr(Output, 8));
          Halt(1);
        end;
    end;
  WriteLn('ok');
end.

