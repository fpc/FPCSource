{ %FAIL }

program tthlp27;

{$mode delphi}

type
  TLongIntHelper = record helper for LongInt
    procedure Test;
  end;

procedure TLongIntHelper.Test;
begin

end;

var
  p: PLongInt;
begin
  p.Test;
end.
