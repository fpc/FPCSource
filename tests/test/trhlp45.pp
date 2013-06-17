program trhlp45;

{$mode delphi}

type
  TRec = record
    X: Integer;
  end;

  THelpRec = record helper for TRec
    constructor Create(AX: Integer);
  end;


{ THelpRec }

constructor THelpRec.Create(AX: Integer);
begin
  X := AX;
end;

var
  R: TRec;
begin
  R := TRec.Create(1);
  if R.X <> 1 then
    halt(1);
end.
