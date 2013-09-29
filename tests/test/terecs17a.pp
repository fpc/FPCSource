{ %FAIL }
{ %NORUN }
program terecs17a;

{$mode delphi}

type

  { TRec }

  TRec = record
    X: Integer;
    constructor Create(I: integer = 0);
  end;

{ TRec }

constructor TRec.Create(I: integer = 0);
begin

end;

var
  R: TRec;
begin
  R := TRec.Create;
end.
