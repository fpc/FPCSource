program tvrec1;

type
  TTestRec = record
  case (A, B) of
  A: (I: Integer);
  B: (D: Double);
  end;

var
  rec: TTestRec;
begin
  if @rec.I=@rec.D then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
