{ %FAIL }
{ %NORUN }
program terecs18;

{$mode delphi}

type

  { TRec }

  TRec = record
    X: Integer;
  end;

  { TRecHelper }

  TRecHelper = record helper for TRec
    constructor Create;
  end;

{ TRecHelper }

constructor TRecHelper.Create;
begin

end;

var
  R: TRec;
begin
  R := TRec.Create;
end.