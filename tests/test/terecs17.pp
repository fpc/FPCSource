{ %FAIL }
{ %NORUN }
program terecs17;

{$mode delphi}

type

  { TRec }

  TRec = record
    X: Integer;
    constructor Create;
  end;

{ TRec }

constructor TRec.Create;
begin

end;

var
  R: TRec;
begin
  R := TRec.Create;
end.
