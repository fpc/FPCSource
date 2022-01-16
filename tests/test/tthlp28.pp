{ %NORUN }

program tthlp28;

{$mode delphi}

type
  TPLongIntHelper = record helper for PLongInt
    procedure Test;
  end;

procedure TPLongIntHelper.Test;
begin

end;

var
  p: PLongInt;
begin
  p.Test;
end.
