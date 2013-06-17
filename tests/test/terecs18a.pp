{ %FAIL }
{ %NORUN }
program terecs18a;

{$mode delphi}

type

  { TRec }

  TRec = record
    X: Integer;
  end;

  { TRecHelper }

  TRecHelper = record helper for TRec
    constructor Create(I: Integer = 0);
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