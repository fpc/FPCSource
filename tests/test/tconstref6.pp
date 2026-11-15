{ %FAIL }
{ a parameter passed by reference through [Ref] cannot be assigned to }
program tconstref6;

{$mode delphi}

type
  TSmall = record
    X: Integer;
  end;

procedure P([Ref] a: TSmall);
begin
  a.X := 9;
end;

begin
end.
