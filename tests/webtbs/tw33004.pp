program Project1;

const
  Inputs: array[0..2] of QWord = (500, $4563918244F40000,QWord($8AC7230489E80000));

var X, Y: QWord; C: Integer;
begin
  for C := Low(Inputs) to High(Inputs) do
  begin
    X := Inputs[C];
    Y := X div QWord($8AC7230489E80000);
    WriteLn(X, ' div 10,000,000,000,000,000,000 = ', Y);
  end;
end.
