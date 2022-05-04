{ %OPT=-O2 }
program tw39646;

function NestedDivWithAnd(const num: UInt64): Integer;
  begin
    NestedDivWithAnd := (num div 100000000) and $FF;
  end;

function NestedModWithAnd(const num: UInt64): Integer;
  begin
    NestedModWithAnd := (num mod 100000000) and $FF;
  end;

const
  Input:     array[0..5] of UInt64  = (0, 1, 100000001, 4000000385, 5000000006, 25700000385);
  ExpectedD: array[0..5] of Integer = (0, 0, 1,         40,         50,         1);
  ExpectedM: array[0..5] of Integer = (0, 1, 1,         129,        6,          129);
var
  Failed: Boolean;
  X, Output: Integer;
begin
  WriteLn('Testing "(UInt64 div 100000000) and $FF" with implicit typecast to Integer:');
  Failed := False;
  for X := Low(Input) to High(Input) do
    begin
      Write('- Input = ', Input[X], ' ... ');
      Output := NestedDivWithAnd(Input[X]);
      if Output = ExpectedD[X] then
        WriteLn('Passed')
      else
        begin
          WriteLn('FAILED! Got ', Output, ', expected ', ExpectedD[X]);
          Failed := True;
        end;
    end;

  WriteLn(#10'Testing "(UInt64 mod 100000000) and $FF" with implicit typecast to Integer:');
  for X := Low(Input) to High(Input) do
    begin
      Write('- Input = ', Input[X], ' ... ');
      Output := NestedModWithAnd(Input[X]);
      if Output = ExpectedM[X] then
        WriteLn('Passed')
      else
        begin
          WriteLn('FAILED! Got ', Output, ', expected ', ExpectedM[X]);
          Failed := True;
        end;
    end;

  if Failed then
    Halt(1)
  else
    WriteLn('ok');
end.