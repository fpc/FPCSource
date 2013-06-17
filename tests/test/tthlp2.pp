{ %NORUN }

{ this checks that the basic type helper syntax is parsed correctly }

program tthlp2;

{$mode delphi}{$H+}

type
  TTest = record helper for LongInt
    procedure Test;
  end;

procedure TTest.Test;
begin

end;

begin

end.

